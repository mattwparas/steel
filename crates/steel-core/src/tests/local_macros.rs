use crate::steel_vm::engine::Engine;
use crate::SteelVal;

const MODULE: &str = r#"
(provide area describe with-twice)

(define (area r)
  (define-syntax square
    (syntax-rules ()
      [(_ a) (* a a)]))
  (* 3 (square r)))

(define (describe x)
  (define label 'thing)
  (define-syntax tag
    (syntax-rules ()
      [(_ v) (list label v)]))
  (tag x))

(define-syntax with-twice
  (syntax-rules ()
    [(_ v)
     (let ([t v])
       (define-syntax twice
         (syntax-rules ()
           [(_ a) (list a a)]))
       (twice t))]))
"#;

fn run(engine: &mut Engine, program: &'static str) -> String {
    let results = engine.run(program).unwrap();

    match results.last() {
        Some(SteelVal::StringV(s)) => s.as_str().to_owned(),
        other => panic!("expected a string, found {other:?}"),
    }
}

#[test]
fn local_macros_in_a_module() {
    let mut engine = Engine::new();
    engine.register_steel_module("local-macros".to_owned(), MODULE.to_owned());

    assert_eq!(
        run(
            &mut engine,
            r#"(require "local-macros")
               (to-string (list (area 2) (describe 1) (with-twice 5)))"#
        ),
        "(12 (thing 1) (5 5))"
    );
}

const GENERATED: &str = r#"
(provide scaled add-offset)

(define offset 100)

(define-syntax define-adder
  (syntax-rules ()
    [(_ name n)
     (define-syntax name
       (syntax-rules ()
         [(_ x) (list 'name (+ x n offset))]))]))

(define-adder add-offset 5)

(define (scaled factor)
  (define-syntax scale
    (syntax-rules ()
      [(_ x) (* x factor)]))
  (map (lambda (factor) (scale factor)) (list 1 2 (cadr (add-offset 0)))))
"#;

#[test]
fn a_module_macro_defined_by_a_macro() {
    let mut engine = Engine::new();
    engine.register_steel_module("generated-macros".to_owned(), GENERATED.to_owned());

    assert_eq!(
        run(
            &mut engine,
            r#"(require "generated-macros")
               (define offset 0)
               (to-string (list (scaled 3) (add-offset 1)))"#
        ),
        "((3 6 315) (add-offset 106))"
    );
}

#[test]
fn local_macros_in_evaluated_code() {
    let mut engine = Engine::new();

    assert_eq!(
        run(
            &mut engine,
            "(to-string
               (eval '(let ()
                        (define-syntax squared (syntax-rules () [(_ a) (* a a)]))
                        (squared 7))))"
        ),
        "49"
    );

    assert_eq!(
        run(
            &mut engine,
            "(to-string
               (eval '(begin
                        (define (f x)
                          (define-syntax twice (syntax-rules () [(_ a) (* 2 a)]))
                          (twice x))
                        (f 4))))"
        ),
        "8"
    );
}

#[test]
fn a_macro_from_an_evaluated_begin_stays_defined() {
    let mut engine = Engine::new();

    assert_eq!(
        run(
            &mut engine,
            "(to-string
               (eval '(begin
                        (define-syntax cubed (syntax-rules () [(_ a) (* a a a)]))
                        (cubed 2))))"
        ),
        "8"
    );

    assert_eq!(run(&mut engine, "(to-string (eval '(cubed 3)))"), "27");

    assert_eq!(
        run(
            &mut engine,
            "(to-string
               (eval-string
                 \"(begin (define-syntax fourth (syntax-rules () [(_ a) (* a a a a)])) (fourth 2))\"))"
        ),
        "16"
    );
}
