use crate::steel_vm::engine::Engine;
use crate::SteelVal;

const MODULE: &str = r#"
(provide (contract/out add (->/c fixnum? fixnum? fixnum?))
         (contract/out bad-result (->/c fixnum? string?))
         (contract/out pair-up (->/c any/c list? any/c))
         (contract/out positive (->/c (>/c 0) fixnum?)))

(define (add a b) (+ a b))
(define (bad-result a) a)
(define (pair-up a b) (cons a b))
(define (positive a) a)
"#;

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_steel_module("checked".to_owned(), MODULE.to_owned());
    engine
}

fn run(engine: &mut Engine, program: &str) -> SteelVal {
    engine.run(program.to_owned()).unwrap().pop().unwrap()
}

fn error_message(engine: &mut Engine, call: &str) -> String {
    let program = format!(
        r#"(require "checked")
           (with-handler (lambda (e) (error-object-message e)) {call})"#
    );
    match run(engine, &program) {
        SteelVal::StringV(s) => s.as_str().to_owned(),
        other => panic!("{call} returned {other} without raising"),
    }
}

#[test]
fn contracts_at_module_boundaries_still_check() {
    let mut engine = engine();
    assert_eq!(
        run(
            &mut engine,
            r#"(require "checked")
               (list (add 1 2) (pair-up 'a '(b)) (positive 3))"#
        )
        .to_string(),
        "(3 (a b) 3)"
    );

    let domain = error_message(&mut engine, r#"(add 1 "x")"#);
    assert!(
        domain.contains("domain position: 1") && domain.contains("blaming  add"),
        "{domain}"
    );
    let range = error_message(&mut engine, "(bad-result 1)");
    assert!(
        range.contains("range position") && range.contains("string?"),
        "{range}"
    );
    let list = error_message(&mut engine, "(pair-up 'a 5)");
    assert!(list.contains("list?"), "{list}");
    let refined = error_message(&mut engine, "(positive -1)");
    assert!(refined.contains("(>/c 0)"), "{refined}");
}

#[test]
fn each_contracted_function_keeps_its_own_contract() {
    let mut engine = engine();
    assert_eq!(
        run(
            &mut engine,
            r#"(require "checked")
               (map (lambda (f) (contract->string (get-contract-struct f)))
                    (list add bad-result pair-up positive))"#
        )
        .to_string(),
        r#"("(-> fixnum? fixnum? fixnum? )" "(-> fixnum? string? )" "(-> any/c list? any/c )" "(-> (>/c 0) fixnum? )")"#
    );
}

const CALLER: &str = r#"
(require "checked")
(provide use use-refined)
(define (use x) (add x 1))
(define (use-refined x) (positive x))
"#;

// A contract of only builtin type predicates is checked by a function another
// module can inline, so its calls have no `bind/c` closure left.
#[test]
fn type_contracts_are_inlined_into_callers() {
    let mut engine = engine();
    engine.register_steel_module("caller".to_owned(), CALLER.to_owned());
    let program = engine
        .emit_fully_expanded_ast_to_string(r#"(require "caller")"#, None)
        .unwrap();
    let definition = |name: &str| {
        let start = program
            .find(&format!("__%#__{name}\n"))
            .unwrap_or_else(|| panic!("no definition of {name} in {program}"));
        let end = program[start..]
            .find("(define")
            .map_or(program.len(), |i| start + i);
        program[start..end].to_owned()
    };
    let use_body = definition("use");
    assert!(use_body.contains("#%prim.fixnum?"), "{use_body}");
    let refined_body = definition("use-refined");
    assert!(!refined_body.contains("#%prim.fixnum?"), "{refined_body}");
    assert!(program.contains("add##contracted"), "{program}");
    assert!(!program.contains("positive##contracted"), "{program}");
}

const BLAMED: &str = r#"
(require "checked")
(provide inlined refined result applied)
(define (inlined x) (list 1 (add x "y")))
(define (refined x) (list 1 (positive x)))
(define (result x) (list 1 (bad-result x)))
(define (applied x) (list 1 (apply add (list x "y"))))
"#;

// A contract violation points at the call that broke it, whether the checks
// were inlined into the caller or not.
#[test]
fn violations_point_at_the_call() {
    let mut engine = engine();
    engine.register_steel_module("blamed".to_owned(), BLAMED.to_owned());
    for (call, blamed) in [
        ("(inlined 1)", "add"),
        ("(refined -1)", "positive"),
        ("(result 1)", "bad-result"),
        ("(applied 1)", "apply"),
    ] {
        let error = engine
            .run(format!("(require \"blamed\") {call}"))
            .expect_err(call);
        let span = error
            .span()
            .unwrap_or_else(|| panic!("{call} raised without a span"));
        let source = span
            .source_id
            .and_then(|id| engine.get_source(&id))
            .unwrap_or_else(|| panic!("{call} raised without a source"));
        assert_eq!(
            source.get(span.start as usize..span.end as usize),
            Some(blamed),
            "{call}: {error:?}"
        );
    }
}
