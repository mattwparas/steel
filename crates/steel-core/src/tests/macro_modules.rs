use crate::steel_vm::engine::Engine;
use crate::SteelVal;

fn last(engine: &mut Engine, program: &str) -> SteelVal {
    engine
        .run(program.to_owned())
        .unwrap()
        .last()
        .cloned()
        .unwrap()
}

fn holds(engine: &mut Engine, program: &str) {
    assert_eq!(last(engine, program), SteelVal::BoolV(true));
}

const DEFINES: &str = r#"
(provide plain mac pmac (contract/out checked (->/c int? int?)))
(define (plain x) (list 'plain x))
(define (checked x) (+ x 1))
(define (helper x) (list 'helper x))
(define-syntax mac (syntax-rules () [(_ x) (helper x)]))
(define-syntax (pmac stx) (syntax-case stx () [(_ x) #'(list 'p x)]))
"#;

const PROVIDES_AGAIN: &str = r#"
(require "macros/defines")
(provide mac pmac uses)
(define (uses) (list (mac 'b) (pmac 'b)))
"#;

const PROVIDES_FOR_SYNTAX_AGAIN: &str = r#"
(require "macros/again")
(provide mac (for-syntax pmac))
"#;

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_steel_module("macros/defines".to_owned(), DEFINES.to_owned());
    engine.register_steel_module("macros/again".to_owned(), PROVIDES_AGAIN.to_owned());
    engine.register_steel_module(
        "macros/again-for-syntax".to_owned(),
        PROVIDES_FOR_SYNTAX_AGAIN.to_owned(),
    );
    engine
}

#[test]
fn a_module_provides_the_macros_it_imports() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(require "macros/again")
(equal? (list (mac 1) (pmac 2) (uses)) '((helper 1) (p 2) ((helper b) (p b))))
"#,
    );
}

#[test]
fn macros_provided_again_through_more_modules_and_prefixes() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(require (prefix-in q: "macros/again-for-syntax"))
(equal? (list (q:mac 1) (q:pmac 2)) '((helper 1) (p 2)))
"#,
    );
}

#[test]
fn except_in_leaves_out_values_and_macros() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(require (except-in "macros/defines" plain mac))
(define (plain x) 'mine)
(define-syntax mac (syntax-rules () [(_ x) 'my-mac]))
(equal? (list (plain 1) (mac 1) (pmac 1) (checked 1)) '(mine my-mac (p 1) 2))
"#,
    );

    let error = engine
        .run(r#"(require (except-in "macros/defines" missing))"#)
        .unwrap_err();

    assert!(error.to_string().contains("isn't provided"));
}

#[test]
fn an_empty_provide_provides_nothing() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/nothing".to_owned(),
        "(provide) (define a 1)".to_owned(),
    );

    holds(&mut engine, r#"(require "macros/nothing") #t"#);
}

#[test]
fn a_module_macro_named_like_a_prelude_one_leaves_the_prelude_alone() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/hijack".to_owned(),
        r#"
(provide go inbody)
(define-syntax letrec* (syntax-rules () [(_ . r) 'hijacked]))
(define-syntax go (syntax-rules () [(_) (do ([i 0 (+ i 1)]) ((= i 3) i))]))
(define-syntax own (syntax-rules () [(_) (letrec* ([a 1]) a)]))
(define inbody (list (do ([i 0 (+ i 1)]) ((= i 3) i)) (own)))
"#
        .to_owned(),
    );

    holds(
        &mut engine,
        r#"
(require "macros/hijack")
(equal? (list inbody (go)) '((3 hijacked) 3))
"#,
    );
}

#[test]
fn a_procedural_definition_replaces_a_syntax_rules_macro() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(define-syntax foo (syntax-rules () [(_) 'rules]))
(define before (foo))
(define-syntax (foo stx) (syntax-case stx () [(_) #''procedural]))
(equal? (list before (foo)) '(rules procedural))
"#,
    );
}

#[test]
fn redefining_a_global_rebinds_what_refers_to_it() {
    let mut engine = engine();

    // Each its own program, as at the repl
    engine.run("(define x 1)").unwrap();
    engine.run("(define (get-x) x)").unwrap();
    engine.run("(define x 2)").unwrap();
    assert_eq!(last(&mut engine, "(get-x)"), SteelVal::IntV(2));

    engine.run("(begin-for-syntax (define y 1))").unwrap();
    engine
        .run(
            r#"
(begin-for-syntax (define (get-y) y))
(define-syntax (y-now stx) (syntax-case stx () [(_) (datum->syntax stx (get-y))]))
"#,
        )
        .unwrap();
    engine.run("(begin-for-syntax (define y 2))").unwrap();
    assert_eq!(last(&mut engine, "(y-now)"), SteelVal::IntV(2));
}

// The passes recurse as deep as the defines nest
#[cfg(feature = "stacker")]
#[test]
fn thousands_of_internal_defines() {
    let mut engine = engine();

    let mut program = String::from("(define (big)\n");

    for i in 0..3000 {
        program.push_str(&format!("  (define x{i} {i})\n"));
    }

    program.push_str("  (+ x0 x2999))\n(big)");

    assert_eq!(last(&mut engine, &program), SteelVal::IntV(2999));
}

#[test]
fn begin_for_syntax_defines_structs_and_values() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(begin-for-syntax
  (struct pt (a b))
  (define-values (q r) (values 1 2))
  (define (helper) (pt-a (pt (+ q r) 0))))
(define-syntax (m stx) (syntax-case stx () [(_) (datum->syntax stx (helper))]))
(m)
"#,
    );

    assert_eq!(result, SteelVal::IntV(3));
}

#[test]
fn an_error_in_what_a_macro_makes_has_a_location() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/counter".to_owned(),
        r#"
(provide (for-syntax next!))
(begin-for-syntax (define n 0) (define (next!) (set! n (+ n 1)) n))
"#
        .to_owned(),
    );

    let error = engine
        .run(
            r#"
(require (for-syntax "macros/counter"))
(define-syntax (m stx) (syntax-case stx () [(_) #'(next!)]))
(m)
"#,
        )
        .unwrap_err();

    assert!(error.span().is_some_and(|x| x.source_id().is_some()));
}

#[test]
fn sandboxed_macros_cannot_make_engines_or_load_files() {
    let mut engine = engine();

    for body in [
        "(Engine::new)",
        r#"(run! (Engine::new) "(+ 1 2)")"#,
        r#"(load "Cargo.toml")"#,
    ] {
        let error = engine
            .run(format!(
                "(define-syntax (m stx) (syntax-case stx () [(_) (begin {body} #''done)])) (m)"
            ))
            .unwrap_err();

        assert!(error.to_string().contains("sandboxed"), "{body}: {error}");
    }
}

#[test]
fn macros_imported_into_transformer_state_stay_live() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/bumps".to_owned(),
        r#"
(provide a-next (for-syntax bump!))
(begin-for-syntax
  (define n 0)
  (define (bump!) (set! n (+ n 1)) n))
(define-syntax (a-next stx)
  (syntax-case stx ()
    [(_) (bump!)]))
"#
        .to_owned(),
    );
    engine.register_steel_module(
        "macros/bumps-too".to_owned(),
        r#"
(require "macros/bumps")
(provide b-next)
(define-syntax (b-next stx)
  (syntax-case stx ()
    [(_) (* 100 (bump!))]))
"#
        .to_owned(),
    );

    // Expanded in the order the code uses them, with one instance of the shared state
    holds(
        &mut engine,
        r#"
(require "macros/bumps" "macros/bumps-too")
(equal? (list (a-next) (b-next) (a-next) (b-next)) '(1 200 3 400))
"#,
    );
}

#[test]
fn syntax_e_gives_the_names_the_code_wrote() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/kinds".to_owned(),
        r#"
(provide kind wrap)
(define-syntax (kind stx)
  (syntax-case stx ()
    [(_ k) (case (syntax-e #'k) [(add) #''plus] [(list) #''list-sym] [else #`(quote #,(syntax-e #'k))])]))
(define-syntax wrap (syntax-rules () [(_) (list (kind add) (kind list) (kind helper))]))
(define (helper) 1)
"#
        .to_owned(),
    );

    holds(
        &mut engine,
        r#"
(require "macros/kinds")
(equal? (list (wrap) (let ([add 1]) (kind add))) '((plus list-sym helper) plus))
"#,
    );
}

#[test]
fn local_syntax_case_macros_see_the_locals_where_they_are_defined() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(define (param-name x)
  (define-syntax (get stx) (syntax-case stx () [(_) #'x]))
  (let ([x 'inner]) (list x (get))))

(define (helper x) (list 'global x))
(define-syntax call-helper (syntax-rules () [(_ v) (helper v)]))
(define (shadows helper)
  (let ([helper (lambda (x) (list 'inner x))])
    (call-helper 1)))

(equal? (list (param-name 'param) (shadows 0)) '((inner param) (global 1)))
"#,
    );
}

#[test]
fn unsyntax_in_binder_positions() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(define-syntax (mk-lambda stx)
  (syntax-case stx ()
    [(_ arg body) #`(lambda (#,#'arg) body)]))
((mk-lambda q (+ q 1)) 41)
"#,
    );

    assert_eq!(result, SteelVal::IntV(42));
}

#[test]
fn transformer_code_with_optional_arguments_and_forward_references() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(define-syntax (m stx)
  (define (add a [b 10]) (+ a b (later)))
  (define (later) 100)
  (syntax-case stx ()
    [(_ v) (datum->syntax stx (add (syntax-e #'v)))]))
(m 1)
"#,
    );

    assert_eq!(result, SteelVal::IntV(111));
}

#[test]
fn a_global_defined_with_its_uses_rebinds_them_later() {
    let mut engine = engine();

    engine
        .run("(begin (define x 1) (define (get-x) x))")
        .unwrap();
    engine.run("(define x 2)").unwrap();

    assert_eq!(last(&mut engine, "(get-x)"), SteelVal::IntV(2));
}

#[test]
fn a_macro_defined_again_in_a_begin() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(begin
  (define-syntax bar (syntax-rules () [(_) 1]))
  (define a (bar))
  (define-syntax bar (syntax-rules () [(_) 2]))
  (define b (bar)))
(equal? (list a b) '(1 2))
"#,
    );
}

#[test]
fn a_top_level_macro_defined_again_is_the_new_one_from_there_on() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(define (early) (later-mac))
(define-syntax later-mac (syntax-rules () [(_) 'first]))
(define before (early))
(define-syntax later-mac (syntax-rules () [(_) 'second]))
(equal? (list before (early) (later-mac)) '(first first second))
"#,
    );
}

#[test]
fn a_module_cannot_define_a_macro_twice() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/redefined".to_owned(),
        r#"
(provide mac)
(define-syntax mac (syntax-rules () [(_) 'old]))
(define-syntax mac (syntax-rules () [(_) 'new]))
"#
        .to_owned(),
    );

    let error = engine
        .run(r#"(require "macros/redefined") (mac)"#)
        .unwrap_err();

    assert!(error.to_string().contains("Macro re defined"), "{error}");
}

const LETTERS: &str = r#"
(provide x y z)
(define x 1)
(define y 2)
(define z 3)
"#;

fn letters() -> Engine {
    let mut engine = engine();
    engine.register_steel_module("macros/letters".to_owned(), LETTERS.to_owned());
    engine
}

#[test]
fn except_in_keeps_the_spec_inside_it() {
    let mut engine = letters();

    super::assert_free_identifier(
        engine.run(r#"(require (except-in (only-in "macros/letters" x y) x)) z"#),
        "definition: z",
    );

    let mut engine = letters();
    super::assert_free_identifier(
        engine.run(r#"(require (only-in (except-in "macros/letters" x) y)) z"#),
        "definition: z",
    );

    let mut engine = letters();
    holds(
        &mut engine,
        r#"
(require (except-in (prefix-in p: "macros/letters") p:x))
(require (except-in (only-in "macros/letters" [x xx] y) y))
(equal? (list p:y p:z xx) '(2 3 1))
"#,
    );

    // Leaving everything out imports nothing
    let mut engine = letters();
    holds(
        &mut engine,
        r#"(require (except-in "macros/letters" x y z)) #t"#,
    );
}

#[test]
fn a_forward_reference_after_a_require_is_an_error() {
    let mut engine = letters();

    let error = engine
        .run(r#"(require "macros/letters") (displayln (later 0)) (define (later v) v)"#)
        .unwrap_err();

    assert!(
        error.to_string().contains("before its definition"),
        "{error}"
    );
}

#[test]
fn names_a_program_defines_dont_break_prelude_macros() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(define (when . args) 'user-when)
(equal? (list (case 1 [(1) 'one]) (when 1 2)) '(one user-when))
"#,
    );

    let mut engine = super::macro_modules::engine();
    engine.register_steel_module(
        "macros/when".to_owned(),
        r#"
(provide (for-syntax when))
(define-syntax when (syntax-rules () [(_ c e ...) 'module-when]))
"#
        .to_owned(),
    );

    holds(
        &mut engine,
        r#"
(require "macros/when")
(equal? (list (case 1 [(1) 'one]) (when 1 2)) '(one module-when))
"#,
    );
}

#[test]
fn a_local_procedural_macro_stays_in_its_function() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(define (f)
  (define-syntax foo (lambda (stx) (syntax-case stx () [(_) #''local-foo])))
  (foo))
(define first (f))
(define (foo) 'global-foo)
(equal? (list first (foo)) '(local-foo global-foo))
"#,
    );
}

#[test]
fn the_repl_redefines_from_the_old_value_and_recovers_from_failed_definitions() {
    let mut engine = engine();

    engine.run("(define counter 1)").unwrap();
    engine.run("(define counter (+ counter 1))").unwrap();
    assert_eq!(last(&mut engine, "counter"), SteelVal::IntV(2));

    assert!(engine
        .run("(begin (define (car x) 'mine) (missing-function))")
        .is_err());
    assert_eq!(last(&mut engine, "(car '(1 2))"), SteelVal::IntV(1));
}

#[test]
fn top_level_definitions_replace_procedural_macros_in_order() {
    let mut engine = engine();

    holds(
        &mut engine,
        r#"
(define-syntax (m stx) (syntax-case stx () [(_) #'1]))
(define before (m))
(define-syntax m (syntax-rules () [(_) 2]))
(equal? (list before (m)) '(1 2))
"#,
    );

    engine
        .run("(define-syntax (p stx) (syntax-case stx () [(_) #'4]))")
        .unwrap();
    engine.run("(define (p) 5)").unwrap();
    assert_eq!(last(&mut engine, "(p)"), SteelVal::IntV(5));
}

#[test]
fn a_module_cannot_define_a_macro_twice_with_either_kind() {
    let mut engine = engine();
    engine.register_steel_module(
        "macros/mixed".to_owned(),
        r#"
(provide m)
(define-syntax m (syntax-rules () [(_) 1]))
(define-syntax (m stx) (syntax-case stx () [(_) #'2]))
"#
        .to_owned(),
    );

    let error = engine.run(r#"(require "macros/mixed")"#).unwrap_err();
    assert!(error.to_string().contains("Macro re defined"), "{error}");
}

#[test]
fn a_function_inlined_at_the_repl_is_redefined() {
    let mut engine = engine();

    engine
        .run("(begin (define (sq x) (* x x)) (define (use n) (sq n)))")
        .unwrap();
    engine.run("(define (sq x) (+ x x))").unwrap();
    assert_eq!(last(&mut engine, "(use 3)"), SteelVal::IntV(6));
}

#[cfg(feature = "stacker")]
#[test]
fn deeply_nested_quoted_data() {
    let mut engine = engine();

    let program = format!(
        "(define deep '{}x{})\n#t",
        "(".repeat(1000),
        ")".repeat(1000)
    );

    assert_eq!(last(&mut engine, &program), SteelVal::BoolV(true));
}

// A long `and` or `cond` expands as deep as it is long, and the passes after recurse as deep
#[cfg(feature = "stacker")]
#[test]
fn long_and_cond_and_nested_code() {
    let mut engine = engine();

    let clauses: String = (0..1000).map(|i| format!("[(= x {i}) {i}] ")).collect();
    let lets: String = (0..1000).map(|i| format!("(let ([x{i} {i}]) ")).collect();

    let program = format!(
        "(define (all x) (and {}))\n\
         (define (pick x) (cond {clauses}[else 'none]))\n\
         (define nested {}x999{})\n\
         (list (all 1) (pick 700) (pick 5000) nested)",
        "x ".repeat(1000),
        lets,
        ")".repeat(1000),
    );

    assert_eq!(last(&mut engine, &program).to_string(), "(1 700 none 999)");
}

// A macro that never stops expanding is an error, not a stack overflow
#[cfg(feature = "stacker")]
#[test]
fn runaway_macro_expansion_is_an_error() {
    let mut engine = engine();

    let result = engine.compile_and_run_raw_program(
        "(define-syntax forever (syntax-rules () [(_) (list 'x (forever))])) (forever)",
    );

    assert!(result
        .unwrap_err()
        .to_string()
        .contains("macro expansion doesn't finish"));
}

// Using a variable at more ellipses than its pattern has is reported as that, not as some
// other error on the way
#[test]
fn too_many_ellipses_is_reported() {
    let mut engine = engine();

    let error = engine
        .run("(define-syntax bad (syntax-rules () [(_ (a ...)) (quote (a ... ...))])) (bad (1 2))")
        .unwrap_err();

    assert!(error.to_string().contains("too many ellipses"), "{error}");
}

// The symbols in a quoted dotted list are the ones the program writes, however long it is
#[test]
fn long_dotted_constants_share_their_symbols() {
    let mut engine = engine();

    let names: Vec<String> = (0..400).map(|i| format!("s{i}")).collect();
    let program = format!(
        "(define d '({} . tail))\n\
         (define (nth l n) (if (= n 0) (car l) (nth (cdr l) (- n 1))))\n\
         (define (end l) (if (pair? l) (end (cdr l)) l))\n\
         (list (eq? (nth (list-ref (list d) 0) 399) 's399) (eq? (end d) 'tail))",
        names.join(" ")
    );

    assert_eq!(last(&mut engine, &program).to_string(), "(#true #true)");
}

// A provided macro can use a private one another macro of its module defined
#[test]
fn a_provided_macro_uses_a_generated_private_one() {
    let mut engine = engine_with_helper();

    assert_eq!(
        last(
            &mut engine,
            r#"(require "generated/helper") (uses-helper 2)"#
        ),
        SteelVal::IntV(21)
    );

    let mut engine = engine_with_helper();
    super::assert_free_identifier(
        engine.run(r#"(require "generated/helper") (helper 1)"#),
        "helper",
    );
}

fn engine_with_helper() -> Engine {
    let mut engine = engine();
    engine.register_steel_module(
        "generated/helper".to_owned(),
        r#"
(provide uses-helper)
(define-syntax-rule (helper z) (* z 10))
(define-syntax uses-helper (syntax-rules () [(_ z) (+ 1 (helper z))]))
"#
        .to_owned(),
    );
    engine
}

// The modules required for syntax are instantiated once for each module being compiled, so
// the macros of different modules share them there
#[test]
fn phase1_instances_are_shared_within_a_compile() {
    let mut engine = engine();
    engine.register_steel_module(
        "shared/lib".to_owned(),
        r#"
(provide bump! make-quoted)
(define count 0)
(define (bump!) (set! count (+ count 1)) count)
(define (make-quoted x) (list 'quote x))
"#
        .to_owned(),
    );
    for (name, macro_name) in [("shared/a", "ua"), ("shared/b", "ub")] {
        engine.register_steel_module(
            name.to_owned(),
            format!(
                r#"
(require (for-syntax "shared/lib"))
(provide {macro_name})
(define-syntax ({macro_name} stx) (syntax-case stx () [(_) (datum->syntax stx (make-quoted (bump!)))]))
"#
            ),
        );
    }

    assert_eq!(
        last(
            &mut engine,
            r#"(require "shared/a" "shared/b") (list (ua) (ub))"#
        )
        .to_string(),
        "(1 2)"
    );
}

// syntax-e of a long dotted syntax list walks it without recursing on each pair
#[test]
fn syntax_e_of_a_long_dotted_list() {
    let mut engine = engine();

    let names: Vec<String> = (0..10000).map(|i| format!("s{i}")).collect();
    let program = format!(
        "(define s (syntax ({} . tail)))\n(pair? (syntax-e s))",
        names.join(" ")
    );

    assert_eq!(last(&mut engine, &program), SteelVal::BoolV(true));
}

// A define that always uses a later one as it runs is an error, also when a global has that
// name
#[test]
fn a_forward_reference_to_a_later_define_is_an_error() {
    let mut engine = engine();

    super::assert_free_identifier(
        engine.run(
            "(define b 'global) (define (f) (define (g) 1) (define a b) (define b 2) (list a (g))) (f)",
        ),
        "definition: b",
    );
}

// An only-in can't name what an except-in inside it leaves out
#[test]
fn only_in_around_except_in_reports_what_is_left_out() {
    let mut engine = letters();

    let error = engine
        .run(r#"(require (only-in (except-in "macros/letters" x) x)) x"#)
        .unwrap_err();

    assert!(error.to_string().contains("isn't included"), "{error}");
}

// The vector representations are equal, so they find each other as keys
#[test]
fn vectors_find_each_other_as_hash_keys() {
    let mut engine = engine();

    assert_eq!(
        last(
            &mut engine,
            "(list (hash-try-get (hash '#(b z) 1) (vector 'b 'z)) (hash-try-get (hash (vector 1 2) 'm) '#(1 2)))"
        )
        .to_string(),
        "(1 m)"
    );
}

// A define that always uses its own name as it runs is an error too, as in Racket, rather
// than meaning the parameter or global it shadows
#[test]
fn a_define_that_uses_itself_as_it_runs_is_an_error() {
    let mut first = engine();

    super::assert_free_identifier(
        first.run("(define x 'global) (define (f y) (define x (list 'local x)) x) (f 1)"),
        "definition: x",
    );

    let mut second = engine();
    super::assert_free_identifier(
        second.run("(define (g x) (define x (+ x 1)) x) (g 1)"),
        "definition: x",
    );
}

// A read that runs before its define, found only as the code runs, is an error rather than a
// placeholder value
#[test]
fn a_read_before_initialization_found_as_it_runs_is_an_error() {
    for program in [
        "(define (f x) (define z (if x z 1)) (list z)) (f #t)",
        "(define (f flag) (define a (if flag later 0)) (define later (list 1)) (list a later)) (f #t)",
        "(define (f a) (define y (or a x)) (define x 1) y) (f #f)",
        // A function called before the define it reads, also through another one
        "(define (f) (define g (lambda () z)) (define y (g)) (define z 5) y) (f)",
        "(define (f) (define (g) (h)) (define (h) z) (define y (g)) (define z 1) y) (f)",
        "(define (f n) (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc k)))) (define r (loop n 0)) (define k 2) r) (f 10)",
    ] {
        let mut engine = engine();
        let error = engine.run(program).unwrap_err();
        assert!(
            error.to_string().contains("cannot use before initialization"),
            "{program}: {error}"
        );
    }

    // The branch that doesn't read it runs as usual
    let mut engine = engine();
    assert_eq!(
        last(
            &mut engine,
            "(define (f x) (define z (if x z 1)) (list z)) (f #f)"
        )
        .to_string(),
        "(1)"
    );
}

// Functions called only once the defines they read have run aren't checked, and a value
// that looks like the placeholder is just a value
#[test]
fn reads_after_initialization_are_values() {
    for (program, expected) in [
        ("(define (f) (define (g) z) (define z 5) (g)) (f)", "5"),
        (
            "(define (f) (define (g) (h)) (define (h) z) (define z 1) (define y (g)) y) (f)",
            "1",
        ),
        (
            "(define (f n) (define (loop i acc) (if (= i 0) acc (loop (- i 1) (+ acc k)))) (define k 2) (loop n 0)) (f 10)",
            "20",
        ),
        (
            r##"(define (f) (define a (if #f b (string->symbol "#%uninitialized"))) (define b 1) a) (f)"##,
            "#%uninitialized",
        ),
        ("(define (f) (define y (if #f x 1)) (define x 2) y) (f)", "1"),
    ] {
        let mut engine = engine();
        assert_eq!(last(&mut engine, program).to_string(), expected, "{program}");
    }
}

// A read in the branch a literal test picks, or in a lambda applied where it's written, always
// runs, so it is found as the code compiles
#[test]
fn reads_that_always_run_are_found_as_the_code_compiles() {
    for program in [
        "(define (f) (define y (if #t x 1)) (define x 2) y) (f)",
        "(define (f) (define y ((lambda () x))) (define x 1) y) (f)",
    ] {
        let mut engine = engine();
        super::assert_free_identifier(engine.run(program), "definition: x");
    }
}

// The first of several forward references is the one reported
#[test]
fn the_first_forward_reference_is_reported() {
    let mut engine = engine();

    super::assert_free_identifier(
        engine.run(
            "(define (f) (define a b) (define b 1) (define (g) (define c d) (define d 2) c) (g)) (f)",
        ),
        "definition: b",
    );
}

// Defines that a macro makes follow the same rule as written ones
#[test]
fn defines_made_by_macros_follow_the_forward_reference_rule() {
    let mut first = engine();
    super::assert_free_identifier(
        first.run(
            "(define-syntax mk (syntax-rules () [(_ n v) (define n v)])) (define (f) (mk a b) (mk b 1) a) (f)",
        ),
        "definition: b",
    );

    let mut second = engine();
    assert_eq!(
        last(
            &mut second,
            "(define-syntax mk (syntax-rules () [(_ n v) (define n v)])) (define (f) (mk b 1) (mk a (+ b 1)) a) (f)"
        ),
        SteelVal::IntV(2)
    );
}

const EXCLUDES: &str = r#"
(provide a b mac)
(define a 1)
(define b 2)
(define-syntax mac (syntax-rules () [(_) 'mac]))
"#;

// An only-in rename is checked against its source name, and can take a name the except-in
// left out
#[test]
fn only_in_renames_around_except_in() {
    let mut first = engine();
    first.register_steel_module("excludes/x".to_owned(), EXCLUDES.to_owned());
    let error = first
        .run(r#"(require (only-in (except-in "excludes/x" b) [b bb])) bb"#)
        .unwrap_err();
    assert!(error.to_string().contains("isn't included"), "{error}");

    let mut second = engine();
    second.register_steel_module("excludes/x".to_owned(), EXCLUDES.to_owned());
    assert_eq!(
        last(
            &mut second,
            r#"(require (only-in (except-in "excludes/x" b) [a b])) b"#
        ),
        SteelVal::IntV(1)
    );
}

const DEFINES_MACROS: &str = r#"
(provide def-const-macro)
(define-syntax def-const-macro
  (syntax-rules ()
    [(_ name val) (define-syntax name (syntax-rules () [(_) val]))]))
"#;

// A macro made by an imported macro-defining macro takes the place of an imported one
#[test]
fn a_generated_macro_shadows_an_imported_one() {
    let mut engine = engine();
    engine.register_steel_module("generated/a".to_owned(), DEFINES_MACROS.to_owned());
    engine.register_steel_module(
        "generated/d".to_owned(),
        "(provide foo) (define-syntax foo (syntax-rules () [(_) 'from-d]))".to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "generated/a") (require "generated/d") (def-const-macro foo 'local) (foo)"#
        )
        .to_string(),
        "local"
    );

    // Also when the module it hides comes first
    let mut later = Engine::new();
    later.register_steel_module("generated/a".to_owned(), DEFINES_MACROS.to_owned());
    later.register_steel_module(
        "generated/d".to_owned(),
        "(provide foo) (define-syntax foo (syntax-rules () [(_) 'from-d]))".to_owned(),
    );
    assert_eq!(
        last(
            &mut later,
            r#"(require "generated/d") (require "generated/a") (def-const-macro foo 'local) (foo)"#
        )
        .to_string(),
        "local"
    );

    let mut procedural = Engine::new();
    procedural.register_steel_module("generated/a".to_owned(), DEFINES_MACROS.to_owned());
    procedural.register_steel_module(
        "generated/d".to_owned(),
        "(provide foo) (define-syntax (foo stx) (syntax-case stx () [(_) #''from-d]))".to_owned(),
    );
    assert_eq!(
        last(
            &mut procedural,
            r#"(require "generated/a" "generated/d") (def-const-macro foo 'local) (foo)"#
        )
        .to_string(),
        "local"
    );
}

// Provided macros can expand to private procedural and generated ones, also from the
// macros an exported macro-defining macro makes
#[test]
fn provided_macros_use_private_procedural_and_generated_helpers() {
    let mut engine = engine();
    engine.register_steel_module(
        "helpers/lib".to_owned(),
        r#"
(provide uses-proc uses-gen make-twice)
(define-syntax (proc-helper stx) (syntax-case stx () [(_ z) #'(* z 10)]))
(define-syntax uses-proc (syntax-rules () [(_ z) (+ 1 (proc-helper z))]))
(define-syntax def-helper
  (syntax-rules () [(_ name k) (define-syntax name (syntax-rules () [(_ z) (+ z k)]))]))
(def-helper gen-helper 1000)
(define-syntax uses-gen (syntax-rules () [(_ z) (gen-helper z)]))
(define-syntax make-twice
  (syntax-rules ()
    [(_ name) (define-syntax name (syntax-rules () [(_ z) (list (proc-helper z) (gen-helper z))]))]))
"#
        .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "helpers/lib") (make-twice tw) (list (uses-proc 2) (uses-gen 2) (tw 3))"#
        )
        .to_string(),
        "(21 1002 (30 1003))"
    );
}

// The top level's inputs share the instances of what its requires use at phase 1, as a
// Racket namespace does
#[test]
fn top_level_inputs_share_phase1_instances() {
    let mut engine = engine();
    engine.register_steel_module(
        "fresh/lib".to_owned(),
        r#"
(provide bump!)
(define count 0)
(define (bump!) (set! count (+ count 1)) count)
"#
        .to_owned(),
    );
    for (name, macro_name) in [("fresh/a", "ua"), ("fresh/b", "ub")] {
        engine.register_steel_module(
            name.to_owned(),
            format!(
                r#"
(require (for-syntax "fresh/lib"))
(provide {macro_name})
(define-syntax ({macro_name} stx) (syntax-case stx () [(_) (datum->syntax stx (bump!))]))
"#
            ),
        );
    }

    assert_eq!(
        last(&mut engine, r#"(require "fresh/a") (ua)"#),
        SteelVal::IntV(1)
    );
    assert_eq!(
        last(&mut engine, r#"(require "fresh/b") (ub)"#),
        SteelVal::IntV(2)
    );
    assert_eq!(last(&mut engine, "(ua)"), SteelVal::IntV(3));
}

// A module's own procedural macros and the ones it imports run in the order the code uses
// them, so they see each other's changes to the modules they require for syntax
#[test]
fn own_and_imported_procedural_macros_run_in_order() {
    let mut engine = engine();
    engine.register_steel_module(
        "order/counter".to_owned(),
        r#"
(provide bump! get)
(define count 0)
(define (bump!) (set! count (+ count 1)) count)
(define (get) count)
"#
        .to_owned(),
    );
    engine.register_steel_module(
        "order/bumps".to_owned(),
        r#"
(require (for-syntax "order/counter"))
(provide bumps)
(define-syntax (bumps stx) (syntax-case stx () [(_) (datum->syntax stx (bump!))]))
"#
        .to_owned(),
    );
    engine.register_steel_module(
        "order/user".to_owned(),
        r#"
(require "order/bumps" (for-syntax "order/counter"))
(provide result)
(define-syntax (reads stx) (syntax-case stx () [(_) (datum->syntax stx (get))]))
(define result (list (bumps) (bumps) (reads)))
"#
        .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "order/user") result"#).to_string(),
        "(1 2 2)"
    );

    let mut top_level = Engine::new();
    top_level.register_steel_module(
        "order/counter".to_owned(),
        "(provide bump! get) (define count 0) (define (bump!) (set! count (+ count 1)) count) (define (get) count)"
            .to_owned(),
    );
    top_level.register_steel_module(
        "order/bumps".to_owned(),
        r#"(require (for-syntax "order/counter")) (provide bumps) (define-syntax (bumps stx) (syntax-case stx () [(_) (datum->syntax stx (bump!))]))"#
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut top_level,
            r#"(require "order/bumps" (for-syntax "order/counter"))
               (define-syntax (reads stx) (syntax-case stx () [(_) (datum->syntax stx (get))]))
               (list (bumps) (bumps) (reads))"#
        )
        .to_string(),
        "(1 2 2)"
    );
}

const QUOTE_IT: &str = r#"
(provide quote-it)
(define-syntax (quote-it stx) (syntax-case stx () [(_ e) #'(quote e)]))
"#;

const DEFINES_PROCEDURAL: &str = r#"
(provide def-proc)
(define-syntax def-proc
  (syntax-rules () [(_ name v) (define-syntax (name stx) (syntax-case stx () [(_ x) #''v]))]))
"#;

// A procedural macro the program defines hides an imported one of its name: written, in a
// function, or made by a macro
#[test]
fn own_procedural_macros_hide_imported_ones() {
    let program = |body: &str| {
        let mut engine = engine();
        engine.register_steel_module("hide/quote-it".to_owned(), QUOTE_IT.to_owned());
        engine.register_steel_module("hide/gen".to_owned(), DEFINES_PROCEDURAL.to_owned());
        last(
            &mut engine,
            &format!(r#"(require "hide/quote-it" "hide/gen") {body}"#),
        )
        .to_string()
    };

    assert_eq!(
        program("(define-syntax (quote-it stx) (syntax-case stx () [(_ x) #''mine])) (quote-it a)"),
        "mine"
    );
    assert_eq!(
        program(
            "(define (f) (define-syntax (quote-it stx) (syntax-case stx () [(_ x) #''local])) (quote-it z)) (f)"
        ),
        "local"
    );
    assert_eq!(
        program("(def-proc quote-it generated) (quote-it a)"),
        "generated"
    );

    // At the top level, from the input that defines it on
    let mut engine = engine();
    engine.register_steel_module("hide/quote-it".to_owned(), QUOTE_IT.to_owned());
    assert_eq!(
        last(
            &mut engine,
            r#"(require "hide/quote-it") (quote-it before)"#
        )
        .to_string(),
        "before"
    );
    engine
        .run("(define-syntax (quote-it stx) (syntax-case stx () [(_ x) #''mine]))")
        .unwrap();
    assert_eq!(last(&mut engine, "(quote-it after)").to_string(), "mine");
}

// The names an imported macro's template writes mean what they do where it's defined, not the
// program's procedural macros of those names
#[test]
fn imported_templates_keep_their_builtins() {
    let mut engine = engine();
    engine.register_steel_module(
        "builtins/lib".to_owned(),
        r#"
(provide pm)
(define-syntax (pm stx) (syntax-case stx () [(_ e) #'(when #t (list 'lib e))]))
"#
        .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "builtins/lib")
               (define-syntax (when stx) (syntax-case stx () [(_ x ...) #''user-when]))
               (pm 2)"#
        )
        .to_string(),
        "(lib 2)"
    );
}

// A binder a procedural macro's template writes hides only what the template writes, so a
// macro of that name in the arguments is still the program's
#[test]
fn template_binders_leave_macro_arguments_alone() {
    let mut engine = engine();

    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax (pm2 stx)
                 (syntax-case stx ()
                   [(_ e) #'(let ([helper2 (lambda (x) (list 'own-local x))]) (list (helper2 1) e))]))
               (define-syntax (helper2 stx) (syntax-case stx () [(_ x) #'(list 'user-proc x)]))
               (pm2 (helper2 2))"#
        )
        .to_string(),
        "((own-local 1) (user-proc 2))"
    );
}

// A procedural macro gets its arguments as they're written, and what it makes can use imported
// syntax-rules macros, in modules too
#[test]
fn procedural_macros_and_imported_syntax_rules() {
    let lib = "(provide sq) (define-syntax sq (syntax-rules () [(_ x) 'lib-sq]))";
    let user = r#"
(require "sr/lib")
(provide results)
(define-syntax (q stx) (syntax-case stx () [(_ e) #'(quote e)]))
(define-syntax (own0 stx) (syntax-case stx () [(_) #'(sq 0)]))
(define results (list (q (sq 1)) (own0)))
"#;

    let mut engine = engine();
    engine.register_steel_module("sr/lib".to_owned(), lib.to_owned());
    engine.register_steel_module("sr/user".to_owned(), user.to_owned());
    assert_eq!(
        last(&mut engine, r#"(require "sr/user") results"#).to_string(),
        "((sq 1) lib-sq)"
    );
}

// Requiring a module again brings back the procedural macro a generated one hid
#[test]
fn requiring_again_restores_a_procedural_macro() {
    let mut engine = engine();
    engine.register_steel_module("again/quote-it".to_owned(), QUOTE_IT.to_owned());
    engine.register_steel_module(
        "again/gen".to_owned(),
        "(provide def-sr) (define-syntax def-sr (syntax-rules () [(_ name v) (define-syntax name (syntax-rules () [(_ x) 'v]))]))"
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "again/quote-it" "again/gen") (def-sr quote-it generated) (quote-it a)"#
        )
        .to_string(),
        "generated"
    );
    assert_eq!(
        last(&mut engine, r#"(require "again/quote-it") (quote-it b)"#).to_string(),
        "b"
    );
}

// A macro a macro defines can name what that macro defined, from a later input or another
// module
#[test]
fn generated_macros_name_generated_defines_later() {
    let d2 = "(define-syntax d2 (syntax-rules () [(_ name) (begin (define (helper) 42) (define-syntax name (syntax-rules () [(_) (helper)])))]))";

    let mut engine = engine();
    engine.run(format!("{d2} (d2 get-secret)")).unwrap();
    assert_eq!(last(&mut engine, "(get-secret)"), SteelVal::IntV(42));

    let mut modules = Engine::new();
    modules.register_steel_module(
        "gen/a".to_owned(),
        format!("(provide d2 gs2) {d2} (d2 gs2)"),
    );
    assert_eq!(
        last(
            &mut modules,
            r#"(require "gen/a") (d2 gs3) (list (gs2) (gs3))"#
        )
        .to_string(),
        "(42 42)"
    );
}

// Assigning a variable of a body before its define runs is an error, as in Racket, and the
// messages name it as it's written
#[test]
fn assigning_before_initialization_is_an_error() {
    for program in [
        "(define (f p) (define (g) c) (define r (if p (set! c 9) 0)) (define c 1) (list c (g))) (f #t)",
        "(define (f) (define (set-it) (set! c 5)) (define r (set-it)) (define c 1) c) (f)",
    ] {
        let mut engine = engine();
        let error = engine.run(program).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("c: assignment disallowed; cannot assign before initialization"),
            "{program}: {error}"
        );
    }

    // A name a template binds is reported as it's written
    let mut engine = engine();
    let error = engine
        .run(
            "(define-syntax mk (syntax-rules () [(_ q) (let () (define a (if q tmp 1)) (define tmp 2) a)])) (define (go q) (mk q)) (go #t)",
        )
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("tmp: undefined; cannot use before initialization"),
        "{error}"
    );
}

// A macro a module's macro defines is the module's everywhere in it, also before the form that
// defines it, as in a Racket module
#[test]
fn generated_macros_are_the_modules_everywhere() {
    let mut engine = engine();
    engine.register_steel_module(
        "everywhere/lib".to_owned(),
        "(provide sq) (define-syntax sq (syntax-rules () [(_ x) 'lib-sq]))".to_owned(),
    );
    engine.register_steel_module(
        "everywhere/gen".to_owned(),
        "(provide def-sr) (define-syntax def-sr (syntax-rules () [(_ name v) (define-syntax name (syntax-rules () [(_ x) 'v]))]))"
            .to_owned(),
    );
    engine.register_steel_module(
        "everywhere/user".to_owned(),
        r#"
(require "everywhere/lib" "everywhere/gen")
(provide results)
(define-syntax (own0 stx) (syntax-case stx () [(_) #'(sq 0)]))
(define before (own0))
(def-sr sq gen-sr)
(define results (list before (sq 1) (own0)))
"#
        .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "everywhere/user") results"#).to_string(),
        "(gen-sr gen-sr gen-sr)"
    );
}

// A module's macros can't define a macro twice, as a written one can't be
#[test]
fn a_module_defines_a_generated_macro_once() {
    let mut engine = engine();
    engine.register_steel_module(
        "twice/user".to_owned(),
        r#"
(provide x)
(define-syntax def-sr (syntax-rules () [(_ name v) (define-syntax name (syntax-rules () [(_) 'v]))]))
(def-sr m first)
(def-sr m second)
(define x (m))
"#
        .to_owned(),
    );

    let error = engine.run(r#"(require "twice/user") x"#).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("Macro re defined within the top level definition: m"),
        "{error}"
    );
}

const GEN: &str =
    "(define-syntax gen (syntax-rules () [(_ n v) (define-syntax n (syntax-rules () [(_) v]))]))";

fn module_results(name: &str, body: &str) -> String {
    let mut engine = engine();
    engine.register_steel_module(name.to_owned(), format!("(provide results)\n{GEN}\n{body}"));
    last(&mut engine, &format!("(require \"{name}\") results")).to_string()
}

#[test]
fn generated_macros_are_found_through_other_macros_and_begins() {
    // A macro that expands to a use of one that defines a macro
    assert_eq!(
        module_results(
            "gen/indirect",
            "(define-syntax m1 (syntax-rules () [(_ n) (gen n 7)]))
             (define (f) (foo))
             (m1 foo)
             (define results (f))"
        ),
        "7"
    );

    assert_eq!(
        module_results(
            "gen/begin",
            "(define (f) (foo))
             (begin (gen foo 1) (define z 2))
             (define results (list (f) z))"
        ),
        "(1 2)"
    );

    // A procedural one
    assert_eq!(
        module_results(
            "gen/procedural",
            "(define-syntax (pgen stx)
               (syntax-case stx () [(_ n v) #'(define-syntax n (syntax-rules () [(_) v]))]))
             (define (f) (foo))
             (pgen foo 3)
             (define results (list (f) (foo)))"
        ),
        "(3 3)"
    );
}

#[test]
fn macro_defining_forms_see_macros_defined_after_them() {
    assert_eq!(
        module_results(
            "gen/later",
            "(define-syntax gen2
               (syntax-rules () [(_ n u get) (begin (define-syntax n (syntax-rules () [(_) 1]))
                                                    (define (get) (u)))]))
             (gen2 a b get)
             (gen b 5)
             (define results (list (a) (get)))"
        ),
        "(1 5)"
    );
}

#[test]
fn generated_macros_in_bodies_are_local() {
    assert_eq!(
        module_results(
            "gen/bodies",
            "(define a (let () (gen foo 1) (foo)))
             (define (f) (gen foo 2) (foo))
             (define (h) (define-syntax foo (syntax-rules () [(_) 3])) (foo))
             (gen foo 4)
             (define results (list a (f) (h) (foo)))"
        ),
        "(1 2 3 4)"
    );
}

// A global, constant or not, can't be read before its define, in a file or module as at the
// top level. One read in a function only when it's called, and it's an error then.
#[test]
fn globals_are_read_only_after_their_define() {
    for program in [
        "(displayln x) (define x 10)",
        "(displayln x) (define x (list 10))",
    ] {
        let mut engine = engine();
        engine.register_steel_module("forward/m".to_owned(), program.to_owned());
        super::assert_free_identifier(engine.run(r#"(require "forward/m")"#), "x");
    }

    let mut engine = engine();
    engine.register_steel_module(
        "forward/late".to_owned(),
        r#"
(provide early late)
(define (f) x)
(define early (with-handler (lambda (e) 'not-yet) (f)))
(define x 10)
(define late (f))
"#
        .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "forward/late") (list early late)"#).to_string(),
        "(not-yet 10)"
    );
}

// Compiled code that reads a global before its define raises, as the interpreter does, here
// for functions hot enough to be compiled first
#[test]
fn compiled_reads_of_undefined_globals_raise() {
    let mut engine = engine();
    engine.register_steel_module(
        "forward/hot".to_owned(),
        r#"
(provide results)
(define (f) (list x))
(define (run n acc) (if (= n 0) acc (run (- n 1) (with-handler (lambda (e) 'err) (f)))))
(define (g n) (if (= n 0) x (g (- n 1))))
(define early (list (run 20000 0) (with-handler (lambda (e) 'err) (g 20000))))
(define x (string->number "10"))
(define results (list early (run 20000 0) (g 20000)))
"#
        .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "forward/hot") results"#).to_string(),
        "((err err) (10) 10)"
    );
}

#[test]
fn set_and_calls_of_undefined_globals_raise() {
    let mut engine = Engine::new();
    assert!(engine.run("(define c (car '()))".to_owned()).is_err());

    let message = |engine: &mut Engine, program: &str| {
        engine.run(program.to_owned()).unwrap_err().to_string()
    };

    assert!(message(&mut engine, "(define (setc) (set! c 10)) (setc)")
        .contains("c: assignment disallowed"));
    assert!(message(&mut engine, "(define (callc) (c 1)) (callc)").contains("c: undefined"));
    assert!(message(&mut engine, "(define (tailc) (c 1)) (list (tailc))").contains("c: undefined"));

    engine
        .run("(define c (lambda (x) (+ x 1)))".to_owned())
        .unwrap();
    assert_eq!(last(&mut engine, "(setc) c").to_string(), "10");
}

#[test]
fn a_required_template_means_its_modules_names() {
    let mut engine = engine();
    engine.register_steel_module(
        "lib/when".to_owned(),
        "(provide lib-when lib-cond)
         (define-syntax lib-when (syntax-rules () [(_ c e) (when c e)]))
         (define-syntax lib-cond (syntax-rules () [(_ e) (cond [e 'lib-cond-ok] [else 'no])]))"
            .to_owned(),
    );
    engine.register_steel_module(
        "user/when".to_owned(),
        "(require \"lib/when\")
         (provide results)
         (define-syntax (when stx) (syntax-case stx () [(_ c e) #''user-when]))
         (define-syntax (cond stx) (syntax-case stx () [(_ . r) #''user-cond]))
         (define results (list (lib-when #t 'lib-when-ok) (lib-cond #t)))"
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "user/when") results"#).to_string(),
        "(lib-when-ok lib-cond-ok)"
    );
}

#[test]
fn procedural_macros_bodies_define_are_theirs() {
    let mut engine = engine();
    engine.register_steel_module(
        "local/pm".to_owned(),
        "(provide results)
         (define-syntax (pm stx) (syntax-case stx () [(_ e) #'(list 'pm e)]))
         (define (k) (define-syntax (pm stx) (syntax-case stx () [(_ e) #''local-pm])) (pm 0))
         (define (j)
           (define inner
             (lambda () (define-syntax (pm stx) (syntax-case stx () [(_ e) #''inner-pm])) (pm 0)))
           (list (pm 4) (inner)))
         (define (l) (let ([v 1]) (define-syntax (pm stx) (syntax-case stx () [(_ e) #''let-pm])) (pm 6)))
         (define results (list (k) (j) (l) (pm 5)))"
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "local/pm") results"#).to_string(),
        "(local-pm ((pm 4) inner-pm) let-pm (pm 5))"
    );
}

#[test]
fn a_local_macro_a_template_names_is_hidden_from_the_user() {
    let mut engine = engine();
    engine.register_steel_module(
        "local/sq".to_owned(),
        "(provide results)
         (define-syntax (with-sq stx)
           (syntax-case stx ()
             [(_ body) #'(let-syntax ([sq (syntax-rules () [(_ x) (* x x)])]) body)]))
         (define-syntax (with-sq2 stx)
           (syntax-case stx ()
             [(_ body) #'(let () (define-syntax sq (syntax-rules () [(_ x) (* x x)])) body)]))
         (define (sq x) (list 'user-sq x))
         (define results (list (with-sq (sq 4)) (with-sq2 (sq 5))))"
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "local/sq") results"#).to_string(),
        "((user-sq 4) (user-sq 5))"
    );
}

#[test]
fn requiring_again_restores_an_imported_procedural_macro() {
    let mut engine = Engine::new();
    engine.register_steel_module(
        "lib/pm".to_owned(),
        "(provide pm) (define-syntax (pm stx) (syntax-case stx () [(_ e) #'(list 'lib-pm e)]))"
            .to_owned(),
    );

    engine.run(r#"(require "lib/pm")"#.to_owned()).unwrap();
    engine
        .run("(define-syntax (pm stx) (syntax-case stx () [(_ e) #''written-pm]))".to_owned())
        .unwrap();
    assert_eq!(last(&mut engine, "(pm 2)").to_string(), "written-pm");
    engine.run(r#"(require "lib/pm")"#.to_owned()).unwrap();
    assert_eq!(last(&mut engine, "(pm 3)").to_string(), "(lib-pm 3)");
}

#[test]
fn a_procedural_macro_defines_a_macro_in_a_body() {
    let mut engine = engine();
    engine.register_steel_module(
        "local/mk".to_owned(),
        "(provide results)
         (define-syntax (mk stx)
           (syntax-case stx () [(_ n) #'(define-syntax n (syntax-rules () [(_) 'made]))]))
         (define (h) (mk m1) (m1))
         (define (g)
           (define-syntax (mk2 stx)
             (syntax-case stx () [(_ n) #'(define-syntax n (syntax-rules () [(_) 'made2]))]))
           (mk2 m2)
           (m2))
         (define results (list (h) (g)))"
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "local/mk") results"#).to_string(),
        "(made made2)"
    );
}

#[test]
fn procedural_macros_with_several_body_forms_are_definitions() {
    // They get their arguments as written, and order like other macro definitions
    assert_eq!(
        module_results(
            "multi/raw",
            "(define-syntax two (syntax-rules () [(_ a b) (list a b)]))
             (define-syntax (kind stx)
               (define unused 1)
               (syntax-case stx (list) [(_ (list a b)) #''pre-expanded] [(_ other) #''raw]))
             (define-syntax (mk-rules stx)
               (define unused 1)
               (syntax-case stx () [(_ name v) #'(define-syntax name (syntax-rules () [(_) v]))]))
             (define x (later))
             (mk-rules later 'L)
             (define results (list (kind (two 1 2)) x))"
        ),
        "(raw L)"
    );
}

fn module_error(name: &str, body: &str) -> String {
    let mut engine = engine();
    engine.register_steel_module(name.to_owned(), format!("(provide results)\n{GEN}\n{body}"));
    engine
        .run(format!("(require \"{name}\") results"))
        .unwrap_err()
        .to_string()
}

#[test]
fn generated_macros_are_defined_once() {
    let procedural = "(define-syntax (mk-rules stx)
                        (syntax-case stx () [(_ name v) #'(define-syntax name (syntax-rules () [(_) v]))]))";

    // Twice by one form
    let twice = module_error(
        "once/same-form",
        &format!("{procedural} (begin (mk-rules a 1) (mk-rules a 2)) (define results (a))"),
    );
    assert!(
        twice.contains("Macro re defined within the top level definition: a")
            || twice.contains("duplicate binding name: a"),
        "{twice}"
    );

    // Over a written procedural macro
    assert!(module_error(
        "once/written",
        "(define-syntax (foo stx) (syntax-case stx () [(_) #'1]))
         (gen foo 42)
         (define results (foo))"
    )
    .contains("Macro re defined within the top level definition: foo"));
}

#[test]
fn errors_name_globals_as_written() {
    let mut engine = engine();
    let program = "(define-syntax mk
                     (syntax-rules ()
                       [(_ result) (begin (define (h) (tmp2 3))
                                   (define result (with-handler (lambda (e) (error-object-message e)) (h)))
                                   (define (tmp2 x) x))]))
                   (mk first-error)
                   first-error";

    assert_eq!(
        last(&mut engine, program).to_string(),
        "\"tmp2: undefined; cannot reference an identifier before its definition\""
    );
}

#[test]
fn a_call_missing_a_keyword_raises_only_when_it_runs() {
    let mut engine = engine();
    assert_eq!(
        last(
            &mut engine,
            "(define (h2 #:k k #:o [o 1]) k)
             (define (never) (h2 #:o 2))
             'ok"
        )
        .to_string(),
        "ok"
    );
}

#[test]
fn every_earlier_form_sees_a_macro_defined_later() {
    // Defined through a pattern variable, which the forms that define macros first miss
    assert_eq!(
        module_results(
            "later/patvar",
            "(define-syntax call (syntax-rules () [(_ m x ...) (m x ...)]))
             (define (f) (foo))
             (call gen foo 42)
             (define results (f))"
        ),
        "42"
    );

    // And after a form that defined one in the second pass
    assert_eq!(
        module_results(
            "later/second-pass",
            "(define-syntax call (syntax-rules () [(_ m x ...) (m x ...)]))
             (call gen bar 1)
             (define x (foo))
             (gen foo 42)
             (define results (list x (bar)))"
        ),
        "(42 1)"
    );
}

#[test]
fn deep_expansions_finish() {
    assert_eq!(
        module_results(
            "deep/or",
            "(define-syntax (pif stx) (syntax-case stx () [(_ c t e) #'(if c t e)]))
             (define-syntax my-or
               (syntax-rules () [(_) #f] [(_ a b ...) (pif (= a 99) 'found (my-or b ...))]))
             (define-syntax (count-to stx)
               (syntax-case stx () [(_ n) #`(my-or #,@(range 0 (+ 1 (syntax-e #'n))))]))
             (define results (count-to 99))"
        ),
        "found"
    );
}

#[test]
fn local_procedural_macros_are_found_by_where_they_are_named() {
    assert_eq!(
        module_results(
            "local/where",
            "(define-syntax (thelper stx) (syntax-case stx () [(_) #''top-helper]))
             (define-syntax tuse (syntax-rules () [(_) (thelper)]))
             (define (h)
               (define-syntax (thelper stx) (syntax-case stx () [(_) #''local-helper]))
               (list (tuse) (thelper)))
             (define-syntax with-hidden
               (syntax-rules ()
                 [(_ body) (let () (define-syntax (hid stx) (syntax-case stx () [(_) #''hidden]))
                                   (list (hid) body))]))
             (define-syntax (hid stx) (syntax-case stx () [(_) #''top-hid]))
             (define (k) (with-hidden (hid)))
             (define (f x)
               (define-syntax (mm stx) (syntax-case stx () [(_) #'(list 'outer x)]))
               (define (g x)
                 (define-syntax (mm2 stx) (syntax-case stx () [(_) #'(list 'mid x (mm))]))
                 (define (h x)
                   (define-syntax (mm stx) (syntax-case stx () [(_) #'(list 'inner x (mm2))]))
                   (mm))
                 (h 3))
               (g 2))
             (define results (list (h) (k) (f 1)))"
        ),
        "((top-helper local-helper) (hidden top-hid) (inner 3 (mid 2 (outer 1))))"
    );
}

#[test]
fn a_macro_procedural_output_defines_in_a_body_is_its_own() {
    assert_eq!(
        module_results(
            "local/it",
            "(define-syntax (it stx) (syntax-case stx () [(_) #''top-it]))
             (define-syntax (with-it stx)
               (syntax-case stx ()
                 [(_ v body) #'(let () (define-syntax it (syntax-rules () [(_) v])) (list (it) body))]))
             (define (f) (with-it 42 (it)))
             (define results (f))"
        ),
        "(42 top-it)"
    );
}

#[test]
fn a_librarys_own_syntax_rules_through_its_procedural_macro_mean_its_names() {
    let mut engine = engine();
    engine.register_steel_module(
        "lib/sa".to_owned(),
        "(provide pa)
         (define-syntax sa (syntax-rules () [(_ x) (when #t x)]))
         (define-syntax (pa stx) (syntax-case stx () [(_ x) #'(sa x)]))"
            .to_owned(),
    );
    engine.register_steel_module(
        "user/sa".to_owned(),
        "(require \"lib/sa\")
         (provide results)
         (define-syntax (when stx) (syntax-case stx () [(_ c e) #''user-when]))
         (define results (pa 1))"
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "user/sa") results"#).to_string(),
        "1"
    );
}

#[test]
fn what_procedural_macros_make_late_is_expanded_too() {
    assert_eq!(
        module_results(
            "late/defaults",
            "(define-syntax my-or2 (syntax-rules () [(_ a b) (if a a b)]))
             (define-syntax (pm stx) (syntax-case stx () [(_ e) #'(my-or2 #f e)]))
             (define (kf #:k [k (pm 4)]) k)
             (begin
               (define-syntax (q stx) (syntax-case stx () [(_ e) #'(my-or2 #f e)]))
               (define in-begin (q 7)))
             (define (f x)
               (define-syntax (q2 stx) (syntax-case stx () [(_ e) #'(my-or2 #f e)]))
               (q2 x))
             (define-syntax (id stx) (syntax-case stx () [(_ e) #'e]))
             (define looped (id (let loop ([i 0]) (if (< i 3) (loop (+ i 1)) i))))
             (define results (list (kf) in-begin (f 9) looped))"
        ),
        "(4 7 9 3)"
    );
}

#[test]
fn a_librarys_literals_mean_what_they_do_there() {
    let mut engine = engine();
    engine.register_steel_module(
        "lib/cond".to_owned(),
        "(provide lcond) (define-syntax lcond (syntax-rules () [(_ c e) (cond [c e] [else 'no])]))"
            .to_owned(),
    );
    engine.register_steel_module(
        "user/cond".to_owned(),
        "(require \"lib/cond\")
         (provide results)
         (define else #f)
         (define (f else) (lcond #f 3))
         (define results (list (lcond #f 3) (f #f)))"
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "user/cond") results"#).to_string(),
        "(no no)"
    );
}

#[test]
fn a_body_defines_its_macros_before_the_rest() {
    assert_eq!(
        module_results(
            "body/first",
            "(define-syntax (m stx) (syntax-case stx () [(_) #''top]))
             (define (f)
               (define (early) (m))
               (define-syntax (m stx) (syntax-case stx () [(_) #''local]))
               (list (early) (m)))
             (define results (f))"
        ),
        "(local local)"
    );

    assert!(module_error(
        "body/twice",
        "(define (f) (gen x 1) (gen x 2) (x)) (define results (f))"
    )
    .contains("duplicate binding name: x"));
}

#[test]
fn every_head_is_expanded_before_any_body() {
    assert_eq!(
        module_results(
            "heads/first",
            "(begin-for-syntax (define counter 0))
             (define-syntax (tick stx)
               (set! counter (+ counter 1))
               (syntax-case stx () [(_ e) #'e]))
             (define-syntax (count stx) (syntax-case stx () [(_) (datum->syntax stx counter)]))
             (tick (define a (count)))
             (tick (define b 5))
             (define results a)"
        ),
        "2"
    );
}

#[test]
fn a_begins_procedural_macros_share_the_modules_phase1_variables() {
    assert_eq!(
        module_results(
            "phase1/begin",
            "(begin-for-syntax (define count 0))
             (define-syntax (how-many stx) (with-syntax ([n count]) #'n))
             (define-syntax (q2 stx) (set! count (+ count 100)) (syntax-case stx () [(_ e) #'e]))
             (define a (q2 1))
             (define b (how-many))
             (begin
               (define-syntax (q stx) (set! count (+ count 1000)) (syntax-case stx () [(_ e) #'e]))
               (define c (q 7)))
             (define results (list a b c (how-many)))"
        ),
        "(1 100 7 1100)"
    );
}

#[test]
fn a_local_macros_template_means_the_procedural_macro_where_it_is_defined() {
    assert_eq!(
        module_results(
            "local/defined-where",
            "(define (k)
               (define-syntax (lhelper stx) (syntax-case stx () [(_) #''outer-l]))
               (define-syntax use2 (syntax-rules () [(_) (lhelper)]))
               (let ()
                 (define-syntax (lhelper stx) (syntax-case stx () [(_) #''inner-l]))
                 (list (use2) (lhelper))))
             (define results (k))"
        ),
        "(outer-l inner-l)"
    );
}

#[test]
fn a_templates_macros_are_defined_once_when_spliced() {
    let rules = "(define-syntax D (syntax-rules () [(_ n) (define-syntax (n stx) (syntax-case stx () [(_) #'5]))]))
                 (define-syntax E (syntax-rules () [(_ n) (define-syntax n (syntax-rules () [(_) 6]))]))";

    // A procedural one twice, then over a syntax-rules one in a begin
    for (name, uses) in [
        ("spliced/twice", "(D foo) (D foo)"),
        ("spliced/kinds", "(begin (D foo) (E foo))"),
        (
            "spliced/written",
            "(D foo) (define-syntax foo (syntax-rules () [(_) 6]))",
        ),
    ] {
        let error = module_error(name, &format!("{rules} {uses} (define results (foo))"));
        assert!(
            error.contains("Macro re defined within the top level definition: foo"),
            "{name}: {error}"
        );
    }

    // Twice in the begin a template wrote
    let error = module_error(
        "spliced/in-begin",
        "(define-syntax gen2 (syntax-rules () [(_ n) (begin (define-syntax n (syntax-rules () [(_) 1]))
                                                          (define-syntax n (syntax-rules () [(_) 2])))]))
         (gen2 b)
         (define results (b))",
    );
    assert!(error.contains("duplicate binding name: b"), "{error}");
}

#[test]
fn a_when_bodys_macro_stays_in_it() {
    assert_eq!(
        module_results(
            "spliced/when",
            "(define-syntax (m stx) (syntax-case stx () [(_) #''top]))
             (define (g)
               (define a (m))
               (when #t (define-syntax (m stx) (syntax-case stx () [(_) #''when-body])) 1)
               (list a (m)))
             (define results (g))"
        ),
        "(top top)"
    );
}

#[test]
fn a_bodys_macros_see_the_whole_body() {
    assert_eq!(
        module_results(
            "body/letrec",
            "(define-syntax (pm stx) (syntax-case stx () [(_) #''top]))
             (define (k)
               (define-syntax (pm2 stx) (syntax-case stx () [(_) #'(pm1)]))
               (define-syntax (pm1 stx) (syntax-case stx () [(_) #''pm1-local]))
               (pm2))
             (define (r)
               (define-syntax use (syntax-rules () [(_) (pm)]))
               (define-syntax (pm stx) (syntax-case stx () [(_) #''local]))
               (use))
             (define (h)
               (define-syntax use (syntax-rules () [(_) (pm)]))
               (let ()
                 (define-syntax (pm stx) (syntax-case stx () [(_) #''inner]))
                 (list (pm) (use))))
             (define (w x)
               (when x
                 (define-syntax (pm stx) (syntax-case stx () [(_) #''when-pm]))
                 (define-syntax use (syntax-rules () [(_) (pm)]))
                 (let () (define-syntax (pm stx) (syntax-case stx () [(_) #''shadow])) (list (pm) (use)))))
             (define results (list (k) (r) (h) (w #t)))"
        ),
        "(pm1-local local (inner top) (shadow when-pm))"
    );
}

#[test]
fn a_templates_procedural_helper_is_its_expansions_own() {
    assert_eq!(
        module_results(
            "helper/per-expansion",
            "(define-syntax mkfn
               (syntax-rules ()
                 [(_ n v) (begin (define-syntax (helper stx) (syntax-case stx () [(_) #'v]))
                                 (define (n) (helper)))]))
             (mkfn f1 1)
             (mkfn f2 2)
             (define results (list (f1) (f2)))"
        ),
        "(1 2)"
    );

    // Written twice, once in a begin
    let error = module_error(
        "helper/written-twice",
        "(define-syntax (m s) (syntax-case s () [(_) #'1]))
         (begin (define-syntax (m s) (syntax-case s () [(_) #'2])))
         (define results (m))",
    );
    assert!(
        error.contains("Macro re defined within the top level definition: m"),
        "{error}"
    );

    // A require in a begin is handled like any other
    let error = module_error(
        "helper/require",
        "(begin (require \"nowhere.scm\")) (define results 1)",
    );
    assert!(error.contains("nowhere.scm"), "{error}");
}

#[test]
fn clause_bodies_are_definition_contexts() {
    assert_eq!(
        module_results(
            "clauses/defines",
            "(define-syntax (pm stx) (syntax-case stx () [(_) #''top]))
             (define (f x)
               (cond
                 [(= x 1)
                  (define-syntax (pm stx) (syntax-case stx () [(_) #''cond1]))
                  (define y 2)
                  (list (pm) y)]
                 [else (pm)]))
             (define (g x) (case x [(1) (define z 3) z] [else 0]))
             (define results (list (f 1) (f 2) (g 1)))"
        ),
        "((cond1 2) top 3)"
    );
}

#[test]
fn local_macros_through_each_other() {
    assert_eq!(
        module_results(
            "local/through",
            "(define-syntax (pm stx) (syntax-case stx () [(_) #''top]))
             (define-syntax sr (syntax-rules () [(_) 'top-sr]))
             (define (f2)
               (define-syntax (pm stx) (syntax-case stx () [(_) #''f2]))
               (define-syntax use (syntax-rules () [(_) (pm)]))
               (define-syntax (lcall stx) (syntax-case stx () [(_ u) #'(u)]))
               (lcall use))
             (define (f)
               (define-syntax (pm stx) (syntax-case stx () [(_) #'(sr)]))
               (define-syntax sr (syntax-rules () [(_) 'local-sr]))
               (define (g)
                 (define-syntax sr (syntax-rules () [(_) 'g-sr]))
                 (list (sr) (pm)))
               (list (pm) (g)))
             (define (h)
               (define (g) (pm))
               (begin (define-syntax (pm stx) (syntax-case stx () [(_) #''begin-local])))
               (g))
             (define results (list (f2) (f) (h)))"
        ),
        "(f2 (local-sr (g-sr local-sr)) begin-local)"
    );
}

#[test]
fn local_procedural_macros_run_in_order_with_the_modules() {
    assert_eq!(
        module_results(
            "local/order",
            "(begin-for-syntax (define counter 0) (define (bump!) (set! counter (+ counter 1)) counter))
             (define-syntax (top-bump stx) (syntax-case stx () [(_) (with-syntax ([n (bump!)]) #'n)]))
             (define-syntax (top-pm stx) (syntax-case stx () [(_) (with-syntax ([n counter]) #'n)]))
             (define a (top-bump))
             (define (f)
               (define-syntax (pm stx) (syntax-case stx () [(_) (with-syntax ([n (* 10 (bump!))]) #'n)]))
               (pm))
             (define b (top-bump))
             (define c (top-pm))
             (define results (list a (f) b c))"
        ),
        "(1 20 3 3)"
    );
}

#[test]
fn a_name_is_a_value_or_a_macro() {
    for (name, body) in [
        (
            "once/value-first",
            "(define m 5) (define-syntax m (syntax-rules () [(_) 2]))",
        ),
        (
            "once/macro-first",
            "(define-syntax m (syntax-rules () [(_) 2])) (define m 5)",
        ),
        (
            "once/in-begin",
            "(define m 5) (begin (define-syntax (m s) (syntax-case s () [(_) #'2])))",
        ),
    ] {
        let error = module_error(name, &format!("{body} (define results 1)"));
        assert!(
            error.contains("identifier already defined as a macro: m"),
            "{name}: {error}"
        );
    }

    // A builtin's name can be defined again
    assert_eq!(
        module_results(
            "once/builtin",
            "(define-syntax vector (syntax-rules () [(_ x ...) 'mine])) (define results (vector 1))"
        ),
        "mine"
    );
}

#[test]
fn provided_procedural_macros_in_begins() {
    let mut engine = engine();
    engine.register_steel_module(
        "provides/begin".to_owned(),
        "(begin (provide pm))
         (define-syntax (pm stx) (syntax-case stx () [(_) #'3]))
         (define-syntax pmac+provide
           (syntax-rules () [(_ n v) (begin (provide n) (define-syntax (n stx) (syntax-case stx () [(_) #'v])))]))
         (pmac+provide pm2 4)"
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            "(require \"provides/begin\") (list (pm) (pm2))"
        )
        .to_string(),
        "(3 4)"
    );
}

#[test]
fn template_datum_syntax_without_pattern_variables_is_rackets() {
    assert_eq!(
        module_results(
            "datum/racket",
            "(begin-for-syntax (define n 7))
             (define-syntax make-counter
               (syntax-rules () [(_ name) (define-syntax (name s) (datum->syntax s n))]))
             (make-counter c1)
             (define results (c1))"
        ),
        "7"
    );
}

// A required module's procedural macro named alone is given the identifier
#[test]
fn an_imported_macro_used_as_an_identifier() {
    let mut engine = engine();
    engine.register_steel_module(
        "identifier/lib".to_owned(),
        r#"(provide pm rm)
           (define (helper x) (+ x 100))
           (define-syntax (pm s) (syntax-case s () [(_ a) #'(helper a)] [_ #'helper]))
           (define-syntax rm (syntax-rules () [(_ a) (helper a)]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "identifier/lib") (list (map pm '(1 2)) (pm 3) (rm 4))"#
        )
        .to_string(),
        "((101 102) 103 104)"
    );

    let error = engine
        .run(r#"(require "identifier/lib") (define (f) rm)"#.to_owned())
        .unwrap_err()
        .to_string();
    assert!(error.contains("rm: bad syntax"), "{error}");
}

// A required module's set! transformer takes a set! of its name
#[test]
fn an_imported_set_transformer() {
    let mut engine = engine();
    engine.register_steel_module(
        "identifier/counter".to_owned(),
        r#"(provide counter)
           (define store (box 0))
           (define-syntax counter
             (make-set!-transformer
              (lambda (stx)
                (syntax-case stx (set!)
                  [(set! _ v) #'(set-box! store v)]
                  [id (identifier? #'id) #'(unbox store)]))))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "identifier/counter") (set! counter 3) (define (f) (set! counter (+ counter 5)) counter) (f)"#
        )
        .to_string(),
        "8"
    );
}

// A module's macro whose template defines a procedural macro: its transformer is code of the
// module's phase 1, and what its templates write names the module's globals
#[test]
fn a_module_template_defines_a_procedural_macro() {
    let mut engine = engine();
    engine.register_steel_module(
        "phases/lib".to_owned(),
        r#"(provide def-pm def-pm2 mk)
           (begin-for-syntax (define (twice-stx d) (* 2 d)))
           (define (rt x) (list 'lib-rt x))
           (define-syntax def-pm
             (syntax-rules ()
               [(_ name) (define-syntax (name s)
                           (syntax-case s ()
                             [(_ n) (with-syntax ([v (+ 1 (syntax->datum #'n))]) #'(rt v))]))]))
           (define-syntax def-pm2
             (syntax-rules ()
               [(_ name) (define-syntax (name s)
                           (syntax-case s () [(_) (with-syntax ([v (twice-stx 21)]) #'(rt v))]))]))
           (define-syntax mk
             (syntax-rules ()
               [(_ name v) (begin
                             (define-syntax (hidden s) (syntax-case s () [(_) #'v]))
                             (define (name) (hidden)))]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "phases/lib")
               (define (rt x) 'wrong)
               (def-pm dbl) (def-pm2 dbl2) (mk a 1) (mk b 2)
               (list (dbl 21) (dbl2) (a) (b))"#
        )
        .to_string(),
        "((lib-rt 22) (lib-rt 42) 1 2)"
    );
}

// A template in a module's phase 1 code names the module's globals where another module's
// macro uses it. The module's phase 1 bindings are what a plain require brings to phase 1.
#[test]
fn a_module_phase1_template_names_its_globals() {
    let mut engine = engine();
    engine.register_steel_module(
        "phases/helper".to_owned(),
        r#"(provide (for-syntax make-call))
           (define (rt-helper x) (list 'rt x))
           (begin-for-syntax
             (define (private-scale n) (* n 10))
             (define (make-call n) (with-syntax ([v (private-scale n)]) #'(rt-helper v))))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "phases/helper")
               (define-syntax (m s) (syntax-case s () [(_ n) (make-call (syntax->datum #'n))]))
               (m 4)"#
        )
        .to_string(),
        "(rt 40)"
    );
}

// What a required module's macros write means the module's names: its identifier macros and
// set! transformers, match patterns, the syntax-rules macros its procedural macros write, and
// builtins it redefines
#[test]
fn a_module_template_names_its_own_definitions() {
    let mut engine = engine();
    engine.register_steel_module(
        "templates/lib".to_owned(),
        r#"(provide use-it fortytwo cell get-cell m-match def-rules)
           (define store (box 0))
           (define-syntax fortytwo (identifier-syntax (unbox store)))
           (define-syntax cell
             (make-set!-transformer
              (lambda (stx)
                (syntax-case stx (set!)
                  [(set! _ v) #'(set-box! store v)]
                  [id (identifier? #'id) #'(unbox store)]))))
           (define-syntax use-it (syntax-rules () [(_ v) (begin (set! cell v) (list fortytwo cell))]))
           (define (get-cell) cell)
           (define (helper x) (+ x 1))
           (define-syntax m-match
             (syntax-rules () [(_ e) (match e [(list a b) (helper (+ a b))] [_ 'none])]))
           (define (lib-helper x) (list 'lib x))
           (define-syntax (def-rules s)
             (syntax-case s ()
               [(_ name) #'(define-syntax name (syntax-rules () [(_ e) (lib-helper e)]))]))
"#
            .to_owned(),
    );
    engine.register_steel_module(
        "templates/own-list".to_owned(),
        r#"(provide m-list)
           (define (list . xs) 'lib-list)
           (define-syntax m-list (syntax-rules () [(_) (list 1)]))
           (define from-lib (m-list))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "templates/lib" "templates/own-list")
               (define store (box 'wrong))
               (define (helper x) 'wrong)
               (define (lib-helper x) 'wrong)
               (def-rules foo)
               (list (use-it 7) fortytwo (m-match (list 1 2)) (m-match 5) (foo 1) (m-list))"#
        )
        .to_string(),
        "((7 7) 7 4 none (lib 1) lib-list)"
    );
}

// An unhygienic macro's output means what it does where its use is written: in a module's
// template, the module's names
#[test]
fn datum_to_syntax_in_a_module_template() {
    let mut engine = engine();
    engine.register_steel_module(
        "templates/it".to_owned(),
        r#"(provide outer-d get-it-d)
           (define it 'module-d)
           (define-syntax (get-it-d stx) (datum->syntax stx 'it))
           (define-syntax (outer-d stx) (syntax-case stx () [(_ e) #'(list (get-it-d) e)]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "templates/it") (define it 'user) (outer-d (get-it-d))"#
        )
        .to_string(),
        "(module-d user)"
    );
}

#[test]
fn a_module_procedural_template_defines_a_procedural_macro() {
    let mut engine = engine();
    engine.register_steel_module(
        "phases/outer".to_owned(),
        r#"(provide outer)
           (define-syntax (outer stx)
             (syntax-case stx ()
               [(_ n) #'(define-syntax (n s)
                          (datum->syntax s (list 'quote (reverse (list 1 2)))))]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "phases/outer")
               (outer g)
               (g)"#
        )
        .to_string(),
        "(2 1)"
    );
}

// Literals and free-identifier=? compare bindings: a module's own global of the literal's name
// is the literal, a renamed import of it is too, and the user's own definition isn't
#[test]
fn literals_compare_bindings_across_modules() {
    let mut engine = engine();
    engine.register_steel_module(
        "literals/lib".to_owned(),
        r#"(provide mac pmac same? item)
           (define item 'item-value)
           (define-syntax mac (syntax-rules (item) [(_ item) 'literal] [(_ x) 'other]))
           (define-syntax (pmac stx)
             (syntax-case stx (item) [(_ item) #''literal] [(_ x) #''other]))
           (define-syntax (same? stx)
             (syntax-case stx ()
               [(_ a) (if (free-identifier=? #'a #'item) #''same #''different)]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require (prefix-in p: "literals/lib"))
               (list (p:mac p:item) (p:mac item) (p:pmac p:item) (p:same? p:item) (p:same? item))"#
        )
        .to_string(),
        "(literal other literal same different)"
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "literals/lib")
               (define-syntax for
                 (syntax-rules (in) [(_ x in lst) (map (lambda (x) x) lst)]))
               (define in 'mine)
               (list (for y in '(1 2)) (mac item) (same? item))"#
        )
        .to_string(),
        "((1 2) literal same)"
    );
}

// A macro that a module's template wrote, used from another module, refers to the global the
// template defined, not to the user's of the same name
#[test]
fn a_template_global_through_two_modules() {
    let mut engine = engine();
    engine.register_steel_module(
        "tglobal/a".to_owned(),
        r#"(provide outer-mk)
           (define-syntax outer-mk
             (syntax-rules ()
               [(_ name) (begin (define tmp 'a-tmp) (define-syntax name (syntax-rules () [(_) tmp])))]))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "tglobal/b".to_owned(),
        r#"(require "tglobal/a")
           (provide mk)
           (outer-mk mk)"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "tglobal/b")
               (define tmp 'main-tmp)
               (list (mk) tmp)"#
        )
        .to_string(),
        "(a-tmp main-tmp)"
    );
}

#[test]
fn provide_all_from_out_and_prefix_out() {
    let mut engine = engine();
    engine.register_steel_module(
        "provides/a".to_owned(),
        r#"(provide x f m)
           (define x 1)
           (define (f y) (+ y 1))
           (define-syntax m (syntax-rules () [(_ v) (list 'm v)]))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "provides/b".to_owned(),
        r#"(require "provides/a")
           (provide (all-from-out "provides/a") z (prefix-out a: x m))
           (define z 3)"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "provides/b")
               (list x (f 1) z (m 2) a:x (a:m 3))"#
        )
        .to_string(),
        "(1 2 3 (m 2) 1 (m 3))"
    );
}

#[test]
fn duplicate_binders_are_errors() {
    let mut engine = engine();
    let error = engine.run("(let ((x 1) (x 2)) x)").unwrap_err().to_string();
    assert!(error.contains("duplicate"), "{error}");
    let error = engine
        .run("(define-syntax m (syntax-rules () [(_ a b) (lambda (a b) a)])) ((m x x) 1 2)")
        .unwrap_err()
        .to_string();
    assert!(error.contains("duplicate"), "{error}");
}

#[test]
fn rename_in_and_rename_out() {
    let mut engine = engine();
    engine.register_steel_module(
        "renames/lib".to_owned(),
        r#"(provide kw kwv f m (rename-out [f plus-ten] [m mac]))
           (define-syntax kw (syntax-rules () [(_) 'kw-used]))
           (define kwv 1)
           (define (f x) (+ x 10))
           (define-syntax m (syntax-rules (kw kwv) [(_ kw) 'kw-literal] [(_ kwv) 'kwv-literal] [(_ x) 'other]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require (rename-in "renames/lib" [kw K] [kwv KV] [f g]))
               (list (m K) (m kw) (m KV) (g 1) (K) (plus-ten 2) (mac K))"#
        )
        .to_string(),
        "(kw-literal other kwv-literal 11 kw-used 12 kw-literal)"
    );
}

#[test]
fn struct_out_provides_a_structs_names() {
    let mut engine = engine();
    engine.register_steel_module(
        "structs/lib".to_owned(),
        r#"(provide (struct-out pt) (struct-out cell) (prefix-out q: (struct-out other)))
           (struct pt (x y) #:transparent)
           (struct cell (v) #:mutable)
           (struct other (w))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "structs/lib")
               (define c (cell 1))
               (set-cell-v! c 5)
               (list (pt? (pt 1 2)) (pt-y (pt 1 2)) (match (pt 1 2) [(pt a b) (+ a b)])
                     (cell-v c) (q:other-w (q:other 7)) (q:other? 3))"#
        )
        .to_string(),
        "(#true 2 3 5 7 #false)"
    );

    // Structs and provides that macros make, and a contract on an accessor
    engine.register_steel_module(
        "structs/made".to_owned(),
        r#"(provide (struct-out gen) (struct-out pt2) (contract/out pt2-x (->/c pt2? int?)))
           (define-syntax mk (syntax-rules () [(_ n) (struct n (a b))]))
           (mk gen)
           (define-syntax prov (syntax-rules () [(_ n) (provide (struct-out n))]))
           (struct by-macro (q))
           (prov by-macro)
           (struct pt2 (x y))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "structs/made")
               (list (gen-b (gen 1 2)) (by-macro-q (by-macro 9)) (pt2-y (pt2 "a" 3))
                     (with-handler (lambda (e) 'contract-error) (pt2-x (pt2 "a" 3))))"#
        )
        .to_string(),
        "(2 9 3 contract-error)"
    );

    // A struct's info goes with its binding: a module can re-export it, and a match on it only
    // needs the struct's name
    engine.register_steel_module(
        "structs/re".to_owned(),
        r#"(require "structs/lib") (provide (struct-out pt))"#.to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            r#"(require (only-in "structs/re" pt))
               (match (pt 1 2) [(pt a b) (+ a b)])"#
        )
        .to_string(),
        "3"
    );

    let err = engine
        .compile_and_run_raw_program(r#"(require "structs/lib") (match (pt 1 2) [(pt a) a])"#)
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("pt has 2 fields, but the pattern has 1"),
        "{err}"
    );

    engine.register_steel_module(
        "structs/bad".to_owned(),
        "(provide (struct-out nope)) (define nope 1)".to_owned(),
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "structs/bad")"#)
        .unwrap_err();
    assert!(err
        .to_string()
        .contains("struct-out expects the name of a struct"));
}

#[test]
fn a_provide_spec_macro_can_provide_several_names() {
    let mut engine = engine();
    engine.register_steel_module(
        "specs/lib".to_owned(),
        r#"(provide (both a b))
           (define-syntax both (syntax-rules () [(_ x y) (begin x (begin y))]))
           (define a 1)
           (define b 2)"#
            .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "specs/lib") (list a b)"#).to_string(),
        "(1 2)"
    );
}

#[test]
fn provide_forms_nest() {
    let mut engine = engine();
    engine.register_steel_module(
        "nest/lib".to_owned(),
        r#"(provide (except-out (all-defined-out) secret)
                    (prefix-out q: (struct-out pt) (combine-out h k))
                    (rename-out [h hh]))
           (struct pt (x y))
           (define (h) 'h)
           (define (k) 'k)
           (define secret 1)
           (define-syntax mac (syntax-rules () [(_) 'mac]))"#
            .to_owned(),
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require "nest/lib")
               (list (h) (k) (hh) (mac) (pt-x (pt 1 2)) (q:pt-y (q:pt 1 2)) (q:h) (q:k))"#
        )
        .to_string(),
        "(h k h mac 1 2 h k)"
    );
    assert!(engine.compile_and_run_raw_program("secret").is_err());
}

fn require_engine() -> Engine {
    let mut engine = engine();
    engine.register_steel_module(
        "late/values".to_owned(),
        "(provide a b) (define a 1) (define b 2)".to_owned(),
    );
    engine.register_steel_module(
        "late/macros".to_owned(),
        "(provide twice) (define-syntax twice (syntax-rules () [(_ e) (list e e)]))".to_owned(),
    );
    engine.register_steel_module(
        "late/phase1".to_owned(),
        "(provide helper) (define (helper) 'from-phase1)".to_owned(),
    );
    engine.register_steel_module(
        "late/plugin".to_owned(),
        r#"(define-syntax register-plugin
             (syntax-rules ()
               [(_ path ids ...) (begin (require (only-in path ids ...)) (provide ids ...))]))
           (register-plugin "late/values" a b)"#
            .to_owned(),
    );
    engine
}

#[test]
fn a_require_a_macro_writes_in_a_module() {
    let mut engine = require_engine();
    assert_eq!(
        last(&mut engine, r#"(require "late/plugin") (list a b)"#).to_string(),
        "(1 2)"
    );

    engine.register_steel_module(
        "late/specs".to_owned(),
        r#"(provide results)
           (define (early) (twice 0))
           (define-syntax just-a (syntax-rules () [(_ path) (only-in path a)]))
           (require (just-a "late/values"))
           (define-syntax use-macros (syntax-rules () [(_ path) (require path)]))
           (use-macros "late/macros")
           (define results (list a (twice 3) (early)))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "late/specs") results"#).to_string(),
        "(1 (3 3) (0 0))"
    );

    engine.register_steel_module(
        "late/provides-for-syntax".to_owned(),
        r#"(provide (for-syntax shout))
           (begin-for-syntax (define (shout) 'shouted))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "late/for-syntax".to_owned(),
        r#"(provide m n)
           (define-syntax fs (syntax-rules () [(_) (require (for-syntax "late/phase1"))]))
           (fs)
           (define-syntax (m stx) (datum->syntax stx (list 'quote (helper))))
           (define-syntax use-shout (syntax-rules () [(_ path) (require path)]))
           (use-shout "late/provides-for-syntax")
           (define-syntax (n stx) (datum->syntax stx (list 'quote (shout))))
           (define results (list (m) (n)))
           (provide results)"#
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            r#"(require "late/for-syntax") (list results (m) (n))"#
        )
        .to_string(),
        "((from-phase1 shouted) from-phase1 shouted)"
    );
}

#[test]
fn a_require_a_macro_writes_at_the_top_level() {
    let mut engine = require_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax use-values (syntax-rules () [(_ path) (require path)]))
               (use-values "late/values")
               (+ a b)"#
        )
        .to_string(),
        "3"
    );
    // Later programs see what it brought in
    assert_eq!(last(&mut engine, "(list a b)").to_string(), "(1 2)");

    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax use-macros (syntax-rules () [(_ path) (begin (require path))]))
               (use-macros "late/macros")
               (twice 5)"#
        )
        .to_string(),
        "(5 5)"
    );

    // Phase 1 code after it sees the instance
    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax fs (syntax-rules () [(_) (require (for-syntax "late/phase1"))]))
               (fs)
               (define-syntax (m stx) (datum->syntax stx (list 'quote (helper))))
               (m)"#
        )
        .to_string(),
        "from-phase1"
    );
}

#[test]
fn require_specs_can_be_the_programs_macros() {
    let mut engine = require_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax just-a (syntax-rules () [(_ p) (only-in p a)]))
               (require (just-a "late/values"))
               a"#
        )
        .to_string(),
        "1"
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require (nope-in "late/values"))"#)
        .unwrap_err();
    assert!(
        err.to_string().contains("nope-in: not a require sub-form"),
        "{err}"
    );
}

#[test]
fn a_circular_require_a_macro_writes_is_an_error() {
    let mut engine = engine();
    engine.register_steel_module(
        "cycle/a".to_owned(),
        r#"(require "cycle/b") (provide x) (define x 1)"#.to_owned(),
    );
    engine.register_steel_module(
        "cycle/b".to_owned(),
        r#"(define-syntax back (syntax-rules () [(_) (require "cycle/a")]))
           (back)"#
            .to_owned(),
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "cycle/a")"#)
        .unwrap_err();
    assert!(err.to_string().contains("circular"), "{err}");
}

#[test]
fn phase1_code_is_loaded_in_module_order() {
    let mut engine = require_engine();
    engine.register_steel_module(
        "order/lib".to_owned(),
        r#"(provide m results)
           (define-syntax fs (syntax-rules () [(_) (require (for-syntax "late/phase1"))]))
           (fs)
           (begin-for-syntax (define (early) (later)))
           (begin-for-syntax (define (later) (helper)))
           (define-syntax (m stx) (datum->syntax stx (list 'quote (early))))
           (define results (m))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "order/lib") (list results (m))"#).to_string(),
        "(from-phase1 from-phase1)"
    );

    engine.register_steel_module(
        "order/templates".to_owned(),
        r#"(provide results)
           (define-syntax gen
             (syntax-rules ()
               [(_ name v) (begin (begin-for-syntax (define name-val v))
                                  (define-syntax (name stx) (datum->syntax stx name-val)))]))
           (gen k1 10)
           (gen k2 20)
           (define-syntax (tripled stx) (datum->syntax stx (triple 2)))
           (begin-for-syntax (define (triple x) (* x 3)))
           (define results (list (k1) (k2) (tripled)))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "order/templates") results"#).to_string(),
        "(10 20 6)"
    );

    // Transformer code before the require doesn't see it
    engine.register_steel_module(
        "order/before".to_owned(),
        r#"(define-syntax (m stx) (datum->syntax stx (list 'quote (helper))))
           (define-syntax fs (syntax-rules () [(_) (require (for-syntax "late/phase1"))]))
           (fs)
           (m)"#
            .to_owned(),
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "order/before")"#)
        .unwrap_err();
    assert!(err.to_string().contains("helper"), "{err}");
}

#[test]
fn a_module_path_a_template_writes_binds_for_the_template() {
    let mut engine = engine();
    engine.register_steel_module(
        "hyg/m".to_owned(),
        r#"(provide foo mymac)
           (define (foo) 'm-foo)
           (define-syntax mymac (syntax-rules () [(_ x) (list 'mymac x)]))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "hyg/n".to_owned(),
        "(provide foo) (define (foo) 'n-foo)".to_owned(),
    );
    engine.register_steel_module(
        "hyg/lib".to_owned(),
        r#"(provide use-m)
           (define-syntax use-m
             (syntax-rules ()
               [(_ name) (begin (require "hyg/m") (define (name) (list (foo) (mymac 1))))]))"#
            .to_owned(),
    );
    // The template sees m's names, the macro's user doesn't
    engine.register_steel_module(
        "hyg/main".to_owned(),
        r#"(provide results)
           (require "hyg/lib")
           (require "hyg/n")
           (use-m g)
           (define results (list (g) (foo)))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "hyg/main") results"#).to_string(),
        "((m-foo (mymac 1)) n-foo)"
    );
    assert_eq!(
        last(&mut engine, r#"(require "hyg/lib") (use-m h) (h)"#).to_string(),
        "(m-foo (mymac 1))"
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "hyg/lib") (use-m k) (mymac 2)"#)
        .unwrap_err();
    assert!(err.to_string().contains("mymac"), "{err}");
}

#[test]
fn a_name_imported_from_two_modules_is_an_error() {
    let mut engine = engine();
    engine.register_steel_module(
        "twice/n".to_owned(),
        "(provide foo) (define (foo) 'n-foo)".to_owned(),
    );
    engine.register_steel_module(
        "twice/n2".to_owned(),
        "(provide foo) (define (foo) 'n2-foo)".to_owned(),
    );
    engine.register_steel_module(
        "twice/again".to_owned(),
        r#"(require "twice/n") (provide foo)"#.to_owned(),
    );
    engine.register_steel_module(
        "twice/both".to_owned(),
        r#"(require "twice/n") (require "twice/n2") (provide x) (define x (foo))"#.to_owned(),
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "twice/both")"#)
        .unwrap_err();
    assert!(err.to_string().contains("imported twice"), "{err}");

    // Fine for the same binding, or a name the module defines
    engine.register_steel_module(
        "twice/same".to_owned(),
        r#"(require "twice/n") (require "twice/again") (provide x) (define x (foo))"#.to_owned(),
    );
    engine.register_steel_module(
        "twice/own".to_owned(),
        r#"(require "twice/n") (require "twice/n2") (provide x) (define (foo) 'own) (define x (foo))"#
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            r#"(require (prefix-in s: "twice/same") (prefix-in o: "twice/own")) (list s:x o:x)"#
        )
        .to_string(),
        "(n-foo own)"
    );
}

#[test]
fn requires_are_handled_in_module_order() {
    let mut engine = engine();
    engine.register_steel_module(
        "ord/log".to_owned(),
        r#"(provide note! notes)
           (define log (box '()))
           (define (note! x) (set-box! log (cons x (unbox log))))
           (define (notes) (reverse (unbox log)))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "ord/a".to_owned(),
        r#"(require "ord/log") (provide a) (define a 1) (note! 'a)"#.to_owned(),
    );
    engine.register_steel_module(
        "ord/b".to_owned(),
        r#"(require "ord/log") (provide b) (define b 2) (note! 'b)"#.to_owned(),
    );
    // Instantiated in source order
    engine.register_steel_module(
        "ord/main".to_owned(),
        r#"(provide results)
           (require "ord/log")
           (define-syntax req (syntax-rules () [(_ p) (begin (define unused 0) (require p))]))
           (define-syntax req2 (syntax-rules () [(_ p) (req p)]))
           (req2 "ord/a")
           (require "ord/b")
           (req2 "macros/defines")
           (define results (list (notes) a b))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "ord/main") results"#).to_string(),
        "((a b) 1 2)"
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "ord/main") a"#)
        .unwrap_err();
    assert!(err.to_string().contains("a"), "{err}");
}

#[test]
fn a_late_require_of_a_required_module_at_the_top_level() {
    let mut engine = require_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax req (syntax-rules () [(_ p) (require p)]))
               (require "late/values")
               (define x a)
               (req "late/values")
               (list x b)"#
        )
        .to_string(),
        "(1 2)"
    );
}

#[test]
fn require_specs_check_what_they_are_given() {
    let mut engine = require_engine();
    let err = engine
        .compile_and_run_raw_program(r#"(require (only-in "late/values" #:a))"#)
        .unwrap_err();
    assert!(
        err.to_string().contains("only-in expects identifiers"),
        "{err}"
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require (only-in (nope "late/values") a))"#)
        .unwrap_err();
    assert!(
        err.to_string().contains("nope: not a require sub-form"),
        "{err}"
    );

    assert_eq!(
        last(
            &mut engine,
            r#"(require (only-in (for-syntax "late/phase1") helper))
               (require (prefix-in p: (for-syntax "late/phase1")))
               (begin-for-syntax (require (prefix-in q: "late/phase1")))
               (define-syntax (m stx)
                 (datum->syntax stx (list 'quote (list (helper) (p:helper) (q:helper)))))
               (m)"#
        )
        .to_string(),
        "(from-phase1 from-phase1 from-phase1)"
    );
}

#[test]
fn an_imported_value_hides_a_kernel_macro() {
    let mut engine = engine();
    engine.register_steel_module(
        "hide/lib".to_owned(),
        "(provide only-in) (define (only-in x) (list 'mine x))".to_owned(),
    );
    engine.register_steel_module(
        "hide/user".to_owned(),
        r#"(require "hide/lib") (provide x) (define x (only-in 1))"#.to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "hide/user") x"#).to_string(),
        "(mine 1)"
    );
    assert_eq!(
        last(&mut engine, r#"(require "hide/lib") (only-in 2)"#).to_string(),
        "(mine 2)"
    );
}

#[test]
fn phase1_names_of_a_failed_compile_are_forgotten() {
    let mut engine = engine();
    assert!(engine
        .run("(begin-for-syntax (define (g) (nope)))".to_owned())
        .is_err());
    let err = engine
        .run("(define-syntax (m stx) (datum->syntax stx (g)))".to_owned())
        .unwrap_err();
    assert!(err.to_string().contains("g"), "{err}");

    let err = engine
        .run(
            r#"(begin-for-syntax (define (helper x) (other x)))
               (define-syntax (m2 stx) (datum->syntax stx (helper 1)))
               (m2)
               (begin-for-syntax (define (other x) x))"#
                .to_owned(),
        )
        .unwrap_err();
    assert!(
        err.to_string()
            .contains("cannot reference an identifier before its definition"),
        "{err}"
    );
}

#[test]
fn phase1_definitions_are_known_through_begins_and_kernel_macros() {
    let mut engine = engine();
    engine.register_steel_module(
        "fwd/lib".to_owned(),
        r#"(provide results)
           (define-syntax defk
             (syntax-rules () [(_ n v) (begin (begin-for-syntax (define n v)) (define dummy 0))]))
           (begin-for-syntax (define (f) (list (a) kk)))
           (define-syntax (m stx) (datum->syntax stx (cons 'list (f))))
           (begin-for-syntax (define-values (a b) (values (lambda () 1) 2)))
           (defk kk 9)
           (define results (m))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "fwd/lib") results"#).to_string(),
        "(1 9)"
    );
}

#[test]
fn a_defmacro_defined_again_at_the_top_level_is_the_new_one() {
    let mut engine = engine();
    assert_eq!(
        last(&mut engine, "(defmacro (dd stx) 1) (dd)").to_string(),
        "1"
    );
    assert_eq!(
        last(&mut engine, "(defmacro (dd stx) 2) (dd)").to_string(),
        "2"
    );
}

#[test]
fn modules_required_for_syntax_keep_their_instance_at_the_top_level() {
    let mut engine = engine();
    engine.register_steel_module(
        "inst/lib".to_owned(),
        r#"(provide bump!)
           (define counter 0)
           (define (bump!) (set! counter (+ counter 1)) counter)"#
            .to_owned(),
    );
    engine.register_steel_module(
        "inst/x".to_owned(),
        r#"(require (for-syntax "inst/lib"))
           (provide mx)
           (define-syntax (mx stx) (datum->syntax stx (bump!)))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "inst/p".to_owned(),
        "(begin-for-syntax (define p 1))".to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "inst/x") (mx)"#).to_string(),
        "1"
    );
    assert_eq!(last(&mut engine, "(mx)").to_string(), "2");
    engine.run(r#"(require "inst/p")"#.to_owned()).unwrap();
    assert_eq!(last(&mut engine, "(mx)").to_string(), "3");
}

#[test]
fn a_phase1_name_defined_twice_in_a_module_is_an_error() {
    let mut engine = engine();
    engine.register_steel_module(
        "dup/defmacro".to_owned(),
        "(defmacro (m s) ''a) (define x (m)) (defmacro (m s) ''b)".to_owned(),
    );
    engine.register_steel_module(
        "dup/phase1".to_owned(),
        r#"(begin-for-syntax (define (h) 1))
           (define x 1)
           (begin-for-syntax (define (h) 2))"#
            .to_owned(),
    );
    for module in ["dup/defmacro", "dup/phase1"] {
        let err = engine
            .compile_and_run_raw_program(format!(r#"(require "{module}")"#))
            .unwrap_err();
        assert!(err.to_string().contains("duplicate binding name"), "{err}");
    }
}

#[test]
fn a_phase1_name_used_before_its_definition_is_undefined() {
    let mut engine = engine();
    engine.register_steel_module(
        "early/value".to_owned(),
        r#"(begin-for-syntax (define h g))
           (define x 1)
           (begin-for-syntax (define g 42))"#
            .to_owned(),
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "early/value")"#)
        .unwrap_err();
    assert!(err.to_string().contains("g: undefined"), "{err}");
}

#[test]
fn a_template_can_write_a_require_and_phase1_code_together() {
    let mut engine = require_engine();
    let program = r#"(define-syntax with-lib
                       (syntax-rules ()
                         [(_ mac) (begin (require (for-syntax "late/phase1"))
                                         (begin-for-syntax (define (h) (list 'quote (helper))))
                                         (define-syntax (mac s) (datum->syntax s (h))))]))
                     (with-lib mm)"#;
    engine.register_steel_module(
        "together/lib".to_owned(),
        format!("(provide results) {program} (define results (mm))"),
    );
    assert_eq!(
        last(&mut engine, r#"(require "together/lib") results"#).to_string(),
        "from-phase1"
    );
    assert_eq!(
        last(&mut engine, &format!("{program} (mm)")).to_string(),
        "from-phase1"
    );
}

#[test]
fn imports_and_prelude_templates_keep_a_modules_macros() {
    let mut engine = require_engine();
    engine.register_steel_module(
        "own/main".to_owned(),
        r#"(provide results)
           (require "late/values")
           (define-syntax a (syntax-rules () [(_) 7]))
           (define-syntax when (syntax-rules () [(_ . x) 'hijacked]))
           (define results (list (a) (case 1 [(1) 'one] [else 'other])))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "own/main") results"#).to_string(),
        "(7 one)"
    );
}

#[test]
fn a_required_procedural_macro_replaces_an_earlier_top_level_variable() {
    let mut engine = engine();
    engine.register_steel_module(
        "kmac/lib".to_owned(),
        "(provide kmac) (defmacro (kmac stx) ''from-macro)".to_owned(),
    );
    engine.run("(define kmac 1)".to_owned()).unwrap();
    assert_eq!(
        last(&mut engine, r#"(require "kmac/lib") (kmac)"#).to_string(),
        "from-macro"
    );
}

#[test]
fn template_imports_leave_quoted_data_and_written_names_alone() {
    let mut engine = engine();
    engine.register_steel_module(
        "tdata/m".to_owned(),
        r#"(provide foo mac)
           (define foo 1)
           (define-syntax mac (syntax-rules () [(_) 'mac]))"#
            .to_owned(),
    );
    engine.register_steel_module(
        "tdata/lib".to_owned(),
        r#"(provide use-m getfoo)
           (define-syntax use-m
             (syntax-rules ()
               [(_ g n) (begin (require (only-in "tdata/m" foo [foo n]))
                               (define (g x)
                                 (list `(foo ,foo) 'foo (case x [(foo) 'matched] [else 'no]) n)))]))
           (define-syntax def-get
             (syntax-rules ()
               [(_ name) (begin (require "tdata/m")
                                (define-syntax name (syntax-rules () [(_) (list foo (mac))])))]))
           (def-get getfoo)"#
            .to_owned(),
    );
    // A rename's name is the use's, and a provided macro still sees the template's import
    assert_eq!(
        last(
            &mut engine,
            r#"(require "tdata/lib") (use-m g bound) (list (g 'foo) bound (getfoo))"#
        )
        .to_string(),
        "(((foo 1) foo matched 1) 1 (1 mac))"
    );
}

#[test]
fn require_specs_apply_in_order() {
    let mut engine = require_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(require (rename-in (prefix-in p: "late/values") [p:a q]))
               (require (only-in "late/values" [a x] a))
               (require (except-in (rename-in "late/values" [b y]) y))
               (list q p:b x a)"#
        )
        .to_string(),
        "(1 2 1 1)"
    );
    for (spec, message) in [
        (r#"(only-in "late/values" nope)"#, "isn't included"),
        (r#"(only-in "late/values" a a)"#, "duplicate identifier"),
        (r#"(for-syntax (for-syntax "late/values"))"#, "phase 1"),
    ] {
        let err = engine
            .compile_and_run_raw_program(format!("(require {spec})"))
            .unwrap_err();
        assert!(err.to_string().contains(message), "{spec}: {err}");
    }
    let err = require_engine()
        .compile_and_run_raw_program(r#"(require (only-in "late/values")) a"#)
        .unwrap_err();
    assert!(err.to_string().contains("a"), "{err}");
}

#[test]
fn except_out_leaves_out_by_the_modules_names() {
    let mut engine = engine();
    engine.register_steel_module(
        "eo/lib".to_owned(),
        r#"(provide (except-out (prefix-out q: (combine-out a b)) a))
           (define a 1)
           (define b 2)"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "eo/lib") q:b"#).to_string(),
        "2"
    );
    engine.register_steel_module(
        "eo/bad".to_owned(),
        "(provide (except-out (all-defined-out) nope)) (define a 1)".to_owned(),
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "eo/bad")"#)
        .unwrap_err();
    assert!(err.to_string().contains("except-out"), "{err}");
}

#[test]
fn option_and_result_can_be_required_together() {
    let mut engine = engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(require "steel/option" "steel/result")
               (list (unwrap-or (Some 1) 0) (unwrap-or (None) 0) (unwrap-or (Ok 2) 0) (unwrap-or (Err 3) 0))"#
        )
        .to_string(),
        "(1 0 2 0)"
    );
}

#[test]
fn begin_for_syntax_requires_anywhere_are_for_syntax() {
    let mut engine = require_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(define-syntax r (syntax-rules () [(_) (begin-for-syntax (require "late/phase1"))]))
               (r)
               (begin (begin-for-syntax (require (prefix-in p: "late/phase1"))))
               (require (for-syntax (prefix-in q: "late/phase1") (prefix-in z: "late/phase1")))
               (define-syntax (m stx)
                 (datum->syntax stx (list 'quote (list (helper) (p:helper) (q:helper) (z:helper)))))
               (m)"#
        )
        .to_string(),
        "(from-phase1 from-phase1 from-phase1 from-phase1)"
    );
}

#[test]
fn a_require_a_syntax_case_template_writes_is_hygienic() {
    let mut engine = engine();
    engine.register_steel_module(
        "sc/m".to_owned(),
        "(provide foo) (define (foo) 'm-foo)".to_owned(),
    );
    engine.register_steel_module(
        "sc/n".to_owned(),
        "(provide foo) (define (foo) 'n-foo)".to_owned(),
    );
    engine.register_steel_module(
        "sc/lib".to_owned(),
        r#"(provide use-m req rename)
           (define-syntax (use-m stx)
             (syntax-case stx () [(_ g) #'(begin (require "sc/m") (define (g) (foo)))]))
           (define-syntax (req stx) (syntax-case stx () [(_ p) #'(require p)]))
           (define-syntax (rename stx)
             (syntax-case stx () [(_ p g) #'(begin (require (rename-in p [foo baz])) (define (g) (baz)))]))"#
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            r#"(require "sc/lib") (req "sc/n") (use-m g) (rename "sc/m" h) (list (g) (h) (foo))"#
        )
        .to_string(),
        "(m-foo m-foo n-foo)"
    );
    let err = engine.compile_and_run_raw_program("baz").unwrap_err();
    assert!(err.to_string().contains("baz"), "{err}");
}

fn transformer_engine() -> Engine {
    let mut engine = engine();
    engine.register_steel_module(
        "tf/s".to_owned(),
        "(provide str-a str-b other) (define str-a 1) (define str-b 2) (define other 3)".to_owned(),
    );
    engine.register_steel_module(
        "tf/o".to_owned(),
        "(provide other) (define other 30)".to_owned(),
    );
    engine.register_steel_module(
        "tf/lib/a".to_owned(),
        "(provide la) (define la 'la)".to_owned(),
    );
    engine.register_steel_module(
        "tf/lib/b".to_owned(),
        "(provide lb) (define lb 'lb)".to_owned(),
    );
    engine
}

#[test]
fn racket_require_forms() {
    let mut engine = transformer_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(require "steel/require")
               (require (matching-identifiers-in "^str-" "tf/s"))
               (require (prefix-in z: (subtract-in "tf/s" "tf/o")) "tf/o")
               (require (filtered-in (lambda (n) (and (regexp-match? "b$" n) (string-append "x" n)))
                                     "tf/s"))
               (require (multi-in "tf/lib" ("a" "b")))
               (list str-a str-b z:str-a other xstr-b la lb)"#
        )
        .to_string(),
        "(1 2 1 30 2 la lb)"
    );
}

#[test]
fn racket_provide_forms() {
    let mut engine = engine();
    engine.register_steel_module(
        "tf/p".to_owned(),
        r#"(require "steel/provide")
           (provide (filtered-out (lambda (n) (and (regexp-match? "^pub-" n) (substring n 4 (string-length n))))
                                  (all-defined-out))
                    (matching-identifiers-out "^api-" (prefix-out api- (combine-out g h))))
           (define pub-f 1)
           (define priv 2)
           (define g 3)
           (define h 4)"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "tf/p") (list f api-g api-h)"#).to_string(),
        "(1 3 4)"
    );
    let err = engine
        .compile_and_run_raw_program(r#"(require "tf/p") priv"#)
        .unwrap_err();
    assert!(err.to_string().contains("priv"), "{err}");
}

#[test]
fn user_require_and_provide_transformers() {
    let mut engine = transformer_engine();
    // A transformer the module defines, inside the built in specs and around them
    engine.register_steel_module(
        "tf/user".to_owned(),
        r#"(provide results (shout-out a) (prefix-out p: (shout-out b)))
           (require (for-syntax "steel/require-transform" "steel/provide-transform"))
           (define-syntax only-vars
             (make-require-transformer
              (lambda (stx)
                (syntax-case stx ()
                  [(_ path)
                   (let-values ([(vars macros) (module->exports (syntax-e #'path))])
                     (let-values ([(imports sources) (expand-import #'path)])
                       (values (filter (lambda (i) (member (import-src-sym i) (map car (cdr (car vars)))))
                                       imports)
                               sources)))]))))
           (define-syntax shout-out
             (make-provide-transformer
              (lambda (stx modes)
                (syntax-case stx ()
                  [(_ spec)
                   (map (lambda (e)
                          (export (export-local-id e)
                                  (string->symbol (string-append (symbol->string (export-out-sym e)) "!"))
                                  0 #f stx))
                        (expand-export #'spec modes))]))))
           (require (rename-in (only-vars "tf/s") [str-a A]))
           (require (prefix-in q: (except-in (only-vars "tf/s") other)))
           (define a 1)
           (define b 2)
           (define results (list A str-b q:str-a q:str-b))"#
            .to_owned(),
    );
    assert_eq!(
        last(&mut engine, r#"(require "tf/user") (list results a! p:b!)"#).to_string(),
        "((1 2 1 2) 1 2)"
    );
    let err = engine
        .compile_and_run_raw_program(
            r#"(require "steel/require")
               (require (only-in (matching-identifiers-in "^str-" "tf/s") other))"#,
        )
        .unwrap_err();
    assert!(err.to_string().contains("isn't included"), "{err}");
}

#[test]
fn transformer_specs_written_by_macros_and_for_syntax() {
    let mut engine = transformer_engine();
    assert_eq!(
        last(
            &mut engine,
            r#"(require "steel/require")
               (define-syntax use-lib
                 (syntax-rules () [(_ p) (require (matching-identifiers-in "^str-" p))]))
               (use-lib "tf/s")
               (require (for-syntax (matching-identifiers-in "b$" "tf/s")))
               (define-syntax (k stx) (datum->syntax stx str-b))
               (list str-a (k))"#
        )
        .to_string(),
        "(1 2)"
    );
}

#[test]
fn steel_require_and_provide_extras() {
    let mut engine = transformer_engine();
    engine.register_steel_module(
        "tf/vm".to_owned(),
        r#"(provide v m dm)
           (define v 1)
           (define-syntax m (syntax-rules () [(_) 2]))
           (defmacro (dm stx) 3)"#
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            r#"(require "steel/require")
               (require (prefix-in v: (values-in "tf/vm")) (prefix-in s: (macros-in "tf/vm")))
               (optional-require "tf/nowhere" [missing (lambda () 'fallback)] unset)
               (optional-require "tf/s" str-a)
               (list v:v (s:m) (s:dm) (missing) unset str-a)"#
        )
        .to_string(),
        "(1 2 3 fallback #false 1)"
    );
    for (program, name) in [
        (
            r#"(require "steel/require") (require (values-in "tf/vm")) (m)"#,
            "m",
        ),
        (
            r#"(require "steel/require") (require (macros-in "tf/vm")) v"#,
            "v",
        ),
    ] {
        let err = engine.compile_and_run_raw_program(program).unwrap_err();
        assert!(err.to_string().contains(name), "{err}");
    }
}

#[test]
fn filtered_procedures_see_the_modules_transformer_code() {
    let mut engine = transformer_engine();
    engine.register_steel_module(
        "tf/helpers".to_owned(),
        r#"(provide add-prefix) (define (add-prefix n) (string-append "h-" n))"#.to_owned(),
    );
    engine.register_steel_module(
        "tf/filtered".to_owned(),
        r#"(require "steel/require" "steel/provide")
           (require (for-syntax "tf/helpers"))
           (begin-for-syntax (define (mine n) (string-append "m-" n)))
           (require (filtered-in (lambda (n) (add-prefix n)) "tf/s"))
           (require (filtered-in (lambda (n) (mine n)) "tf/s"))
           (provide results (filtered-out (lambda (n) (mine n)) own))
           (define own 5)
           (define results (list h-str-a m-other))"#
            .to_owned(),
    );
    assert_eq!(
        last(
            &mut engine,
            r#"(require "tf/filtered") (list results m-own)"#
        )
        .to_string(),
        "((1 3) 5)"
    );
}
