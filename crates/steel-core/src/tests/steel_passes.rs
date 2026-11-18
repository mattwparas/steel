//! Compiler passes written in Steel (compiler/passes/steel_passes.rs)

use crate::compiler::passes::steel_passes::PassMode;
use crate::steel_vm::engine::Engine;
use crate::SteelVal;

// Replaces the string "quiet" with "LOUD": a pass whose work shows
const SHOUT: &str = r##"
(require "#%private/steel/passes")

(define (shout stx)
  (match-syntax stx
    [(list 'quote _) stx]
    [(list parts ...) (rebuild stx (map shout parts))]
    [_ (if (equal? (syntax->datum stx) "quiet") (datum->syntax stx "LOUD") stx)]))

(register-pass! 'shout 'after-inlining 'steel (lambda (exprs context) (map shout exprs)))
"##;

fn run(engine: &mut Engine, program: &str) -> Result<SteelVal, String> {
    engine
        .run(program.to_owned())
        .map(|values| values.last().cloned().unwrap_or(SteelVal::Void))
        .map_err(|err| err.to_string())
}

fn string(value: Result<SteelVal, String>) -> String {
    match value {
        Ok(SteelVal::StringV(s)) => s.as_str().to_owned(),
        other => panic!("{other:?}"),
    }
}

#[test]
fn a_pass_changes_the_program() {
    let mut engine = Engine::new();
    engine.add_compiler_pass_module("test/shout", SHOUT);
    assert_eq!(string(run(&mut engine, r##""quiet""##)), "LOUD");
    // Quoted data isn't code
    assert_eq!(
        run(&mut engine, r##"(quote "quiet")"##)
            .unwrap()
            .to_string(),
        "\"quiet\""
    );
}

#[test]
fn settings_turn_passes_off() {
    let mut engine = Engine::new();
    engine.add_compiler_pass_module("test/shout", SHOUT);
    engine.set_compiler_pass_mode("shout", PassMode::Off);
    assert_eq!(string(run(&mut engine, r##""quiet""##)), "quiet");

    let mut engine = Engine::new();
    engine.add_compiler_pass_module("test/shout", SHOUT);
    engine.set_compiler_pass_mode("*", PassMode::Off);
    assert_eq!(string(run(&mut engine, r##""quiet""##)), "quiet");
}

#[test]
fn a_failing_pass_keeps_its_input() {
    let mut engine = Engine::new();
    engine.add_compiler_pass_module(
        "test/broken",
        r##"
        (require "#%private/steel/passes")
        (register-pass! 'raises 'after-constant-folding 'steel (lambda (exprs context) (car '())))
        (register-pass! 'not-a-program 'after-inlining 'steel (lambda (exprs context) 10))
        (register-pass! 'bad-form 'before-code-generation 'steel
          (lambda (exprs context) (list (datum->syntax (car exprs) '(if 1 2)))))
        "##,
    );
    assert_eq!(run(&mut engine, "(+ 1 2)").unwrap(), SteelVal::IntV(3));
}

#[test]
fn passes_that_fail_to_load_stay_off() {
    let mut engine = Engine::new();
    engine.add_compiler_pass_module("test/unloadable", "(this is not (defined");
    assert_eq!(run(&mut engine, "(+ 1 2)").unwrap(), SteelVal::IntV(3));
}

#[test]
fn passes_know_where_they_run() {
    let mut engine = Engine::new();
    engine.add_compiler_pass_module(
        "test/where",
        r##"
        (require "#%private/steel/passes")
        (define (where stx point)
          (match-syntax stx
            [(list 'quote _) stx]
            [(list parts ...) (rebuild stx (map (lambda (part) (where part point)) parts))]
            [_ (if (equal? (syntax->datum stx) "where") (datum->syntax stx point) stx)]))
        (register-pass! 'where 'after-analysis-passes 'steel
          (lambda (exprs context)
            (define point (symbol->string (hash-ref context 'point)))
            (map (lambda (expr) (where expr point)) exprs)))
        "##,
    );
    assert_eq!(
        string(run(&mut engine, r##""where""##)),
        "after-analysis-passes"
    );
}

#[test]
fn compare_mode() {
    // identity's Rust and Steel versions agree
    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("identity", PassMode::Compare);
    engine.set_compiler_pass_compare_fails(true);
    assert_eq!(run(&mut engine, "(+ 1 2)").unwrap(), SteelVal::IntV(3));

    // A Steel identity that doesn't
    let wrong_identity = SHOUT.replace(
        "(register-pass! 'shout 'after-inlining 'steel",
        "(register-pass! 'identity 'before-code-generation 'off",
    );

    let mut engine = Engine::new();
    engine.add_compiler_pass_module("test/wrong-identity", &wrong_identity);
    engine.set_compiler_pass_mode("identity", PassMode::Compare);
    engine.set_compiler_pass_compare_fails(true);
    let err = run(&mut engine, r##""quiet""##).unwrap_err();
    assert!(err.contains("differs from its Rust version"), "{err}");

    // Reported, not fatal, and the Rust result is what runs
    let mut engine = Engine::new();
    engine.add_compiler_pass_module("test/wrong-identity", &wrong_identity);
    engine.set_compiler_pass_mode("identity", PassMode::Compare);
    engine.set_compiler_pass_compare_fails(false);
    assert_eq!(string(run(&mut engine, r##""quiet""##)), "quiet");
}

// Applied lambdas (rest arguments too), empty lets, and both nested
const PORTED: &str = r##"
(define (g a) ((lambda (x y . z) (list x y z)) a 2 3 4))
(define (h a) (let () ((lambda () ((lambda (b) (+ a b)) 1)))))
(define (k) ((lambda (x . rest) rest) 1))
(list (g 1) (h 2) (k) ((lambda (a) a) 5) '((lambda (q) q) 6))
"##;

const PORTED_RESULT: &str = "((1 2 (3 4)) 3 () 5 ((lambda (q) q) 6))";

#[test]
fn ported_passes_match_their_rust_versions() {
    for pass in ["anonymous-lets", "empty-lets"] {
        let mut engine = Engine::new();
        engine.set_compiler_pass_mode(pass, PassMode::Compare);
        engine.set_compiler_pass_compare_fails(true);
        assert_eq!(run(&mut engine, PORTED).unwrap().to_string(), PORTED_RESULT);
    }
}

#[test]
fn ported_passes_in_steel() {
    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("anonymous-lets", PassMode::Steel);
    engine.set_compiler_pass_mode("empty-lets", PassMode::Steel);
    assert_eq!(run(&mut engine, PORTED).unwrap().to_string(), PORTED_RESULT);

    // And with every Rust version off
    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("anonymous-lets", PassMode::Off);
    engine.set_compiler_pass_mode("empty-lets", PassMode::Off);
    assert_eq!(run(&mut engine, PORTED).unwrap().to_string(), PORTED_RESULT);
}

#[test]
fn fold_primitives() {
    // The Rust constant folding gets to these first, so try the pass on syntax
    // of its own
    let mut engine = Engine::new();
    engine.add_compiler_pass_module(
        "test/fold",
        r##"
        (require "#%private/steel/passes")
        (define (fold stx datum) (syntax->datum (fold-primitives (datum->syntax stx datum))))
        (define (probe stx)
          (match-syntax stx
            [(list 'quote _) stx]
            [(list parts ...) (rebuild stx (map probe parts))]
            [_ (if (equal? (syntax->datum stx) "probe")
                   (datum->syntax stx
                                  (list 'quote (list (fold stx '(#%prim.* 6 (#%prim.+ 3 4)))
                                        (fold stx '(#%prim./ 1 0))
                                        (fold stx '(#%prim.string-append "a" "b"))
                                        (fold stx '(#%prim.+ x 1)))))
                   stx)]))
        (register-pass! 'probe 'before-code-generation 'steel (lambda (exprs context) (map probe exprs)))
        "##,
    );
    assert_eq!(
        run(&mut engine, r##""probe""##).unwrap().to_string(),
        r##"(42 (#%prim./ 1 0) (#%prim.string-append "a" "b") (#%prim.+ x 1))"##
    );
}

#[test]
fn analysis_queries() {
    let mut engine = Engine::new();
    engine.add_compiler_pass_module(
        "test/facts",
        r##"
        (require "#%private/steel/passes")
        ;; Only while a pass runs
        (define outside (with-handler (lambda (err) 'refused) (binding-info 1)))
        (define facts '())
        (define (look stx)
          (match-syntax stx
            [(list 'define name (and function (list 'lambda (list a b) (list _ x y))))
             #:when (string-contains? (symbol->string (syntax-e name)) "probe-target")
             (let ([info (function-info function)])
               (set! facts
                     (list (binding-uses a)
                           (binding-uses b)
                           (same-binding? x y)
                           (same-binding? x b)
                           (length (hash-ref info 'arguments))
                           (hash-ref info 'rest)
                           (hash-ref (binding-info x) 'kind)
                           (equal? (syntax-id a) (binding-id x))
                           outside)))]
            [(list parts ...) (for-each look parts)]
            [_ void]))
        (define (answer stx)
          (match-syntax stx
            [(list 'quote _) stx]
            [(list parts ...) (rebuild stx (map answer parts))]
            [_ (if (equal? (syntax->datum stx) "facts") (datum->syntax stx (list 'quote facts)) stx)]))
        (register-pass! 'look 'after-analysis-passes 'steel
          (lambda (exprs context) (for-each look exprs) exprs))
        (register-pass! 'answer 'before-code-generation 'steel
          (lambda (exprs context) (map answer exprs)))
        "##,
    );
    assert_eq!(
        run(&mut engine, r##"(define (probe-target a b) (+ a a)) "facts""##)
            .unwrap()
            .to_string(),
        "(2 0 #true #false 2 #false local #true refused)"
    );
}

const ORS: &str = r##"
(struct point (x y))
(define (f a b) (or a b))
(define (g l) (or (null? l) (car l)))
(define (h p q) (or (point? p) (pair? q) (car q)))
(define (k x) (or (car x) 5))
(define (m null?) (or (null? 1) 2))
(list (f #f 2) (g '()) (g '(7)) (h (point 1 2) 3) (k '(9)) (m (lambda (x) 0)))
"##;

#[test]
fn or_bindings_match_the_rust_version() {
    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("or-bindings", PassMode::Compare);
    engine.set_compiler_pass_compare_fails(true);
    assert_eq!(run(&mut engine, ORS).unwrap().to_string(), "(2 #true 7 #true 9 0)");

    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("or-bindings", PassMode::Steel);
    assert_eq!(run(&mut engine, ORS).unwrap().to_string(), "(2 #true 7 #true 9 0)");
}

#[test]
fn unused_lets() {
    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("unused-lets", PassMode::Steel);
    let program = r##"
    (define counter 0)
    (define (bump!) (set! counter (+ counter 1)) counter)
    (define (f x)
      (let ([a 1] [b (lambda (y) y)] [c x] [d (bump!)] [e 'q])
        (+ x 1)))
    (define (g x) (let ([unused (lambda () x)]) (let () (define y x) y)))
    (list (f 1) (f 2) counter (g 3))
    "##;
    assert_eq!(run(&mut engine, program).unwrap().to_string(), "(2 3 2 3)");
}

// Keyword, optional and rest arguments, apply on a known list, and loops
// over lists that read (car l) after passing (cdr l)
const REST_AND_LOOPS: &str = r##"
(define (kw a #:b [b 2] #:c [c 3]) (list a b c))
(define (opt a [b 10] [c (+ a 1)]) (list a b c))
(define (rest a . xs) (cons a xs))
(define counter 0)
(define (bump!) (set! counter (+ counter 1)) counter)
(define (use-kw x) (kw x #:c (bump!)))
(define (use-opt x) (opt x (bump!)))
(define (use-rest x) (rest x 1 (bump!)))
(define (sum l acc) (if (null? l) acc (sum (cdr l) (+ acc (car l)))))
(define (pairs l) (if (null? l) '() (let ([rest (cdr l)] [x (* 2 (car l))]) (cons x (pairs rest)))))
(define (app) (apply (lambda (a b) (+ a b)) (list 1 2)))
;; A global argument can't move (lower-rest binds it to a name first)
(define fill 9)
(define (use-global) (opt 1 fill))
(list (use-kw 1) (use-opt 2) (use-rest 3) (kw 1 #:b 5) (opt 7) (sum '(1 2 3) 0) (pairs '(1 2 3)) (app) counter (use-global))
"##;

const REST_AND_LOOPS_RESULT: &str =
    "((1 2 1) (2 2 3) (3 1 3) (1 5 3) (7 10 8) 6 (2 4 6) 3 3 (1 9 2))";

#[test]
fn lower_rest_and_hoist_car_match_their_rust_versions() {
    for pass in ["lower-rest", "hoist-car"] {
        let mut engine = Engine::new();
        engine.set_compiler_pass_mode(pass, PassMode::Compare);
        engine.set_compiler_pass_compare_fails(true);
        assert_eq!(run(&mut engine, REST_AND_LOOPS).unwrap().to_string(), REST_AND_LOOPS_RESULT, "{pass}");
    }

    let mut engine = Engine::new();
    engine.set_compiler_pass_mode("lower-rest", PassMode::Steel);
    engine.set_compiler_pass_mode("hoist-car", PassMode::Steel);
    assert_eq!(run(&mut engine, REST_AND_LOOPS).unwrap().to_string(), REST_AND_LOOPS_RESULT);
}

#[test]
fn passes_in_a_row_share_a_round_trip() {
    // The second sees what the first returned, and can still ask about it
    let mut engine = Engine::new();
    engine.add_compiler_pass_module(
        "test/chain",
        r##"
        (require "#%private/steel/passes")
        (define (swap from to)
          (define (walk stx)
            (match-syntax stx
              [(list 'quote _) stx]
              [(list parts ...) (rebuild stx (map walk parts))]
              [_ (if (equal? (syntax->datum stx) from) (datum->syntax stx to) stx)]))
          (lambda (exprs context) (map walk exprs)))
        (define (asks exprs context)
          (map (lambda (stx) (if (equal? (syntax->datum stx) "two")
                                 (datum->syntax stx (if (bound-name? 'chain-target) "three" "unknown"))
                                 stx))
               exprs))
        (register-pass! 'first 'after-inlining 'steel (swap "one" "two"))
        (register-pass! 'second 'after-inlining 'steel asks)
        "##,
    );
    assert_eq!(
        string(run(&mut engine, r##"(define chain-target 1) "one""##)),
        "three"
    );
}
