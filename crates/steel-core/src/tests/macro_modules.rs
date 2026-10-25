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
        .contains("macro expansion depth reached"));
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

    assert!(
        error.to_string().contains("left out by the except-in"),
        "{error}"
    );
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
    assert!(
        error.to_string().contains("left out by the except-in"),
        "{error}"
    );

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
(define x 10)
(define results (list early (run 20000 0) (g 20000)))
"#
        .to_owned(),
    );

    assert_eq!(
        last(&mut engine, r#"(require "forward/hot") results"#).to_string(),
        "((err err) (10) 10)"
    );
}
