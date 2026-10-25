use crate::steel_vm::engine::Engine;
use crate::SteelVal;

fn last(engine: &mut Engine, program: &'static str) -> SteelVal {
    engine.run(program).unwrap().last().cloned().unwrap()
}

#[test]
fn module_macro_names_mean_the_module_definitions() {
    let mut engine = Engine::new();

    let module = r#"
(provide via-helper first-of)

(define (helper x) (list 'module-helper x))

(define-syntax via-helper
  (syntax-rules ()
    [(_ v) (helper v)]))

(define-syntax first-of
  (syntax-rules ()
    [(_ l) (car l)]))
"#;

    engine.register_steel_module("template-names".to_owned(), module.to_owned());

    let result = last(
        &mut engine,
        r#"
(require "template-names")

(define (f v)
  (let ([helper (lambda (x) 'local)]
        [car cdr])
    (list (helper v) (via-helper v) (first-of '(1 2)))))

(define helper 'user-global)
(define (car x) 'user-car)

(equal? (list (f 1) (via-helper 2) (first-of '(3 4)))
        '((local (module-helper 1) 1) (module-helper 2) 3))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}
