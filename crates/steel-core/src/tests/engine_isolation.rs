// Engines made with `Engine::new` start from state built once per process (the
// boot image). Each must still be its own: nothing one does is seen by another.

use crate::rvals::SteelVal;
use crate::steel_vm::engine::Engine;

fn last(engine: &mut Engine, program: &str) -> SteelVal {
    engine
        .compile_and_run_raw_program(program.to_string())
        .unwrap_or_else(|e| panic!("{program}: {e}"))
        .pop()
        .unwrap_or(SteelVal::Void)
}

fn fails(engine: &mut Engine, program: &str) -> bool {
    engine
        .compile_and_run_raw_program(program.to_string())
        .is_err()
}

fn int(n: isize) -> SteelVal {
    SteelVal::IntV(n)
}

#[test]
fn globals_are_per_engine() {
    let mut a = Engine::new();
    let mut b = Engine::new();

    last(&mut a, "(define only-in-a 1)");
    assert!(fails(&mut b, "only-in-a"));

    last(&mut b, "(define shared-name 'b)");
    last(&mut a, "(define shared-name 'a)");
    assert_eq!(
        last(&mut b, "shared-name"),
        SteelVal::SymbolV("b".into())
    );
}

#[test]
fn redefining_a_prelude_function_stays_in_its_engine() {
    let mut a = Engine::new();
    let mut b = Engine::new();

    last(&mut a, "(define (map f xs) 'replaced)");
    assert_eq!(
        last(&mut a, "(map add1 (list 1 2))"),
        SteelVal::SymbolV("replaced".into())
    );
    assert_eq!(last(&mut b, "(apply + (map add1 (list 1 2)))"), int(5));
}

#[test]
fn syntax_rules_macros_are_per_engine() {
    let mut a = Engine::new();
    let mut b = Engine::new();

    last(
        &mut a,
        "(define-syntax only-in-a (syntax-rules () [(_ x) (+ x 1)]))",
    );
    assert_eq!(last(&mut a, "(only-in-a 1)"), int(2));
    assert!(fails(&mut b, "(only-in-a 1)"));
}

#[test]
fn procedural_macros_are_per_engine() {
    let mut a = Engine::new();
    let mut b = Engine::new();

    last(
        &mut a,
        "(define-syntax (twice stx) (syntax-case stx () [(_ e) #'(begin e e)]))",
    );
    last(&mut a, "(define n 0) (twice (set! n (+ n 1)))");
    assert_eq!(last(&mut a, "n"), int(2));

    // `twice` is no macro in b: the call fails as an unbound function
    assert!(fails(&mut b, "(define m 0) (twice (set! m (+ m 1)))"));

    // b can define its own, differently, and a keeps its own
    last(
        &mut b,
        "(define-syntax (twice stx) (syntax-case stx () [(_ e) #'(list e e e)]))",
    );
    assert_eq!(
        last(&mut b, "(length (twice 1))"),
        int(3)
    );
    last(&mut a, "(twice (set! n (+ n 1)))");
    assert_eq!(last(&mut a, "n"), int(4));
}

#[test]
fn prelude_macros_work_in_every_engine() {
    for _ in 0..3 {
        let mut engine = Engine::new();
        assert_eq!(
            last(&mut engine, "(match (list 1 2 3) [(list a b c) (+ a b c)])"),
            int(6)
        );
        assert_eq!(
            last(
                &mut engine,
                "(define/contract (f x) (->/c int? int?) (+ x 1)) (f 1)"
            ),
            int(2)
        );
    }
}

#[test]
fn mutable_state_from_the_prelude_is_per_engine() {
    let mut a = Engine::new();
    let mut b = Engine::new();

    // A box defined at the top level of the stdlib
    last(&mut a, "(set-box! #%will-executor-running 'changed-in-a)");
    assert_eq!(last(&mut b, "(unbox #%will-executor-running)"), SteelVal::BoolV(false));

    // A parameter defined by a stdlib module: a's output goes to a string port,
    // b's still to its own port
    last(&mut a, "(current-output-port (open-output-string)) (display \"in a\")");
    assert_eq!(
        last(&mut a, "(get-output-string (current-output-port))"),
        SteelVal::StringV("in a".into())
    );
    assert!(fails(&mut b, "(get-output-string (current-output-port))"));
}

#[test]
fn requires_are_per_engine() {
    let mut a = Engine::new();
    let mut b = Engine::new();
    for engine in [&mut a, &mut b] {
        engine.register_steel_module(
            "isolation/counter".to_string(),
            "(provide bump!) (define count 0) (define (bump!) (set! count (+ count 1)) count)"
                .to_string(),
        );
    }

    last(&mut a, "(require \"isolation/counter\")");
    last(&mut a, "(bump!) (bump!)");
    assert_eq!(last(&mut a, "(bump!)"), int(3));

    assert!(fails(&mut b, "(bump!)"));
    last(&mut b, "(require \"isolation/counter\")");
    assert_eq!(last(&mut b, "(bump!)"), int(1));
}

#[test]
fn hot_stdlib_code_in_each_engine_uses_its_own_values() {
    // Compiled code is specialized on the globals of the engine that compiled it;
    // two engines running the same stdlib functions until the jit compiles them
    // must each see their own.
    let program = "(define (go n acc) (if (= n 0) acc (go (- n 1) (+ acc (length (map add1 (list 1 2 3)))))))
                   (go 2000 0)";
    let mut a = Engine::new();
    let mut b = Engine::new();
    assert_eq!(last(&mut a, program), int(6000));
    last(&mut b, "(define (map f xs) (list 'one))");
    assert_eq!(last(&mut b, program), int(2000));
    assert_eq!(last(&mut a, program), int(6000));
}

#[test]
fn engines_on_another_thread_know_the_prelude_modules() {
    // The boot image is made on this thread. An engine made from it on another
    // thread gets what compiling the prelude left in the expander's per-thread
    // state, without compiling the prelude there.
    let _first = Engine::new();

    std::thread::spawn(|| {
        let mut engine = Engine::new();
        for module in crate::compiler::modules::PRELUDE_MODULES {
            assert!(
                crate::parser::expand_visitor::ThreadExpanderState::this_thread_knows(module),
                "{module}"
            );
        }
        assert_eq!(
            last(&mut engine, "(match (list 1 2) [(list a b) (+ a b)])"),
            int(3)
        );
    })
    .join()
    .unwrap();
}
