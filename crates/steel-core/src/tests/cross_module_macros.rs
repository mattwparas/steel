use crate::steel_vm::engine::Engine;
use crate::SteelVal;

const MACROS: &str = r#"
(provide (for-syntax via-helper)
         (for-syntax twice)
         (for-syntax define-pair)
         (for-syntax first-field)
         (for-syntax define-getter)
         (for-syntax old-style)
         by-name
         rules-to-case)

(define (helper x) (list 'module-helper x))

(define-syntax (private-case stx)
  (syntax-case stx ()
    [(_ v) #'(helper v)]))

(define-syntax (via-helper stx)
  (syntax-case stx ()
    [(_ v) #'(helper v)]))

(define-syntax (twice stx)
  (syntax-case stx ()
    [(_ v) #'(list (private-case v) (private-case v))]))

(define-syntax (define-pair stx)
  (syntax-case stx ()
    [(_ a b v) #'(define-values (a b) (values (helper v) v))]))

(define-syntax (first-field stx)
  (syntax-case stx ()
    [(_ x y) #'(let () (struct point (px py)) (point-px (point x y)))]))

(define-syntax (define-getter stx)
  (syntax-case stx ()
    [(_ name v) #'(define (name) (helper v))]))

(defmacro (old-style stx)
  (let ([v (cadr (syntax-e stx))])
    `(helper ,v)))

(define-syntax (by-name stx)
  (syntax-case stx ()
    [(_ v) #'(helper v)]))

(define-syntax rules-to-case
  (syntax-rules ()
    [(_ v) (private-case v)]))
"#;

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_steel_module("cross/macros".to_owned(), MACROS.to_owned());
    engine
}

fn last(engine: &mut Engine, program: &'static str) -> SteelVal {
    engine.run(program).unwrap().last().cloned().unwrap()
}

#[test]
fn procedural_macros_from_a_module_at_the_top_level() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(require "cross/macros")

(define-pair a b 2)
(define-getter get 3)

(define (shadowed helper)
  (list helper (via-helper 4) (old-style 5)))

(define helper 'user-global)

(equal? (list (via-helper 1) (twice 1) (list a b) (get) (first-field 6 7)
              (shadowed 'mine) helper (old-style 8) (by-name 9) (rules-to-case 10))
        '((module-helper 1) ((module-helper 1) (module-helper 1)) ((module-helper 2) 2)
          (module-helper 3) 6 (mine (module-helper 4) (module-helper 5)) user-global
          (module-helper 8) (module-helper 9) (module-helper 10)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn procedural_macros_from_a_module_in_another_module() {
    let mut engine = engine();

    engine.register_steel_module(
        "cross/user".to_owned(),
        r#"
(require "cross/macros")
(provide results)

(define-pair a b 2)
(define-getter get 3)

(define (shadowed helper)
  (list helper (via-helper 4) (old-style 5)))

(define helper 'user-global)

(define results
  (list (via-helper 1) (twice 1) (list a b) (get) (first-field 6 7)
        (shadowed 'mine) helper (old-style 8) (by-name 9) (rules-to-case 10)))
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "cross/user")

(equal? results
        '((module-helper 1) ((module-helper 1) (module-helper 1)) ((module-helper 2) 2)
          (module-helper 3) 6 (mine (module-helper 4) (module-helper 5)) user-global
          (module-helper 8) (module-helper 9) (module-helper 10)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn procedural_macros_through_only_in_and_prefix_in() {
    let mut engine = engine();

    engine.register_steel_module(
        "cross/selected".to_owned(),
        r#"
(require (only-in "cross/macros" twice (define-getter define-it)))
(require (prefix-in m. "cross/macros"))
(provide results)

(define-it get 3)

(define results (list (twice 1) (get) (m.via-helper 2) (m.old-style 4)))
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "cross/selected")

(equal? results
        '(((module-helper 1) (module-helper 1)) (module-helper 3) (module-helper 2)
          (module-helper 4)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn only_in_leaves_out_the_other_procedural_macros() {
    let mut engine = engine();

    engine.register_steel_module(
        "cross/excluded".to_owned(),
        r#"
(require (only-in "cross/macros" twice))
(provide result)

(define result (via-helper 1))
"#
        .to_owned(),
    );

    super::assert_free_identifier(engine.run(r#"(require "cross/excluded")"#), "via-helper");
}

#[test]
fn a_program_cannot_use_a_private_procedural_macro() {
    let mut engine = engine();

    super::assert_free_identifier(
        engine.run(r#"(require "cross/macros") (private-case 1)"#),
        "private-case",
    );
}

#[test]
fn a_local_hides_a_procedural_macro() {
    let mut engine = engine();

    let result = last(
        &mut engine,
        r#"
(require "cross/macros")

(define-syntax (local-one stx)
  (syntax-case stx ()
    [(_) #'1]))

(define (call-param twice) (twice 3))
(define (call-let) (let ([via-helper (lambda (x) 'local)]) (via-helper 4)))
(define (call-define) (define old-style (lambda (x) 'defined)) (old-style 5))
(define (call-struct struct) (struct 6))
(define (call-own local-one) (local-one 7))
(define (through-template private-case) (twice private-case))

(equal? (list (call-param (lambda (x) 'param)) (call-let) (call-define)
              (call-struct (lambda (x) 'not-a-struct)) (call-own (lambda (x) 'own))
              (through-template 8))
        '(param local defined not-a-struct own ((module-helper 8) (module-helper 8))))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const GUARDED: &str = r#"
(provide pass pass-rules)

(define-syntax (secret stx)
  (syntax-case stx ()
    [(_ v) #'(list 'secret v)]))

(define-syntax (pass stx)
  (syntax-case stx ()
    [(_ e) #'(begin e)]))

(define-syntax pass-rules
  (syntax-rules ()
    [(_ e) (begin e)]))

(define-syntax vector
  (syntax-rules ()
    [(_ x ...) 'hijacked]))
"#;

fn guarded() -> Engine {
    let mut engine = Engine::new();
    engine.register_steel_module("cross/guarded".to_owned(), GUARDED.to_owned());
    engine
}

#[test]
fn a_public_macro_does_not_open_private_ones_to_its_arguments() {
    super::assert_free_identifier(
        guarded().run(r#"(require "cross/guarded") (pass (secret 1))"#),
        "secret",
    );

    super::assert_free_identifier(
        guarded().run(r#"(require "cross/guarded") (list (pass-rules 1) (secret 2))"#),
        "secret",
    );

    let result = last(
        &mut guarded(),
        r#"
(require "cross/guarded")

(define (secret x) (list 'user x))

(equal? (list (pass (secret 1)) (pass-rules (secret 2)) (pass (vector 3)) `(4 ,(+ 2 3)))
        (list '(user 1) '(user 2) (vector 3) '(4 5)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const LIBRARY: &str = r#"
(provide lib-rules lib-case)

(define (inner x) (list 'lib-inner x))

(define-syntax lib-rules
  (syntax-rules ()
    [(_ v) (list 'lib-rules v)]))

(define-syntax (lib-case stx)
  (syntax-case stx ()
    [(_ v) #'(let ([tmp v]) (inner tmp))]))
"#;

const MIDDLE: &str = r#"
(require "cross/library")
(provide go-rules go-case go-private go-public public)

(define (inner x) (list 'middle-inner x))

(define-syntax private
  (syntax-rules ()
    [(_ v) (list 'private v)]))

(define-syntax public
  (syntax-rules ()
    [(_ v) (list 'public v)]))

(define-syntax (go-rules stx)
  (syntax-case stx ()
    [(_ v) #'(lib-rules v)]))

(define-syntax (go-case stx)
  (syntax-case stx ()
    [(_ v) #'(let ([tmp (inner v)]) (lib-case (list tmp v)))]))

(define-syntax (go-private stx)
  (syntax-case stx ()
    [(_ v) #'(private v)]))

(define-syntax (go-public stx)
  (syntax-case stx ()
    [(_ v) #'(public v)]))
"#;

#[test]
fn templates_use_the_macros_their_module_imports_and_keeps_private() {
    let mut engine = Engine::new();
    engine.register_steel_module("cross/library".to_owned(), LIBRARY.to_owned());
    engine.register_steel_module("cross/middle".to_owned(), MIDDLE.to_owned());

    let result = last(
        &mut engine,
        r#"
(require "cross/middle")

(define (inner x) 'user-inner)
(define (private x) 'user-private)
(define tmp 'user-tmp)

(equal? (list (go-rules 1) (go-case tmp) (go-private 2) (go-public 3)
              (let ([tmp 'local]) (go-case tmp)))
        '((lib-rules 1)
          (lib-inner ((middle-inner user-tmp) user-tmp))
          (private 2)
          (public 3)
          (lib-inner ((middle-inner local) local))))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

const ARITHMETIC: &str = r#"
(provide add-one count-down)

(define-syntax (add-one stx)
  (syntax-case stx ()
    [(_ v) #'(+ v 1)]))

(define-syntax (count-down stx)
  (syntax-case stx ()
    [(_ n) #'(let loop ([i n] [seen '()])
               (if (= i 0)
                   (reverse seen)
                   (loop (- i 1) (cons i seen))))]))
"#;

#[test]
fn macros_from_a_module_required_in_an_earlier_run() {
    let mut engine = Engine::new();
    engine.register_steel_module("cross/arithmetic".to_owned(), ARITHMETIC.to_owned());

    engine.run(r#"(require "cross/arithmetic")"#).unwrap();

    let result = last(
        &mut engine,
        r#"
(define (g) (add-one 9))
(equal? (list (add-one 1) (g) (count-down 3)) '(2 10 (3 2 1)))
"#,
    );

    assert_eq!(result, SteelVal::BoolV(true));
}

#[test]
fn a_user_macro_cannot_take_the_place_of_a_private_one() {
    let mut engine = Engine::new();
    engine.register_steel_module(
        "cross/rules".to_owned(),
        r#"
(provide public)
(define-syntax private (syntax-rules () [(_ v) (list 'private v)]))
(define-syntax public (syntax-rules () [(_ v) (private v)]))
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "cross/rules")
(define-syntax private (syntax-rules () [(_ v) 'user]))
(list (public 1) (private 2))
"#,
    );

    assert_eq!(result.to_string(), "((private 1) user)");
}

#[test]
fn syntax_rules_templates_use_the_macros_their_module_imports() {
    let mut engine = Engine::new();
    engine.register_steel_module("cross/library".to_owned(), LIBRARY.to_owned());
    engine.register_steel_module(
        "cross/rules-user".to_owned(),
        r#"
(require "cross/library")
(provide wraps)
(define-syntax wraps (syntax-rules () [(_ v) (list (lib-rules v) (lib-case v))]))
"#
        .to_owned(),
    );

    let result = last(
        &mut engine,
        r#"
(require "cross/rules-user")
(define-syntax lib-rules (syntax-rules () [(_ v) 'user]))
(wraps 1)
"#,
    );

    assert_eq!(result.to_string(), "((lib-rules 1) (lib-inner 1))");
}
