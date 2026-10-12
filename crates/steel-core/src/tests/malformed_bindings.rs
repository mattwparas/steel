use super::assert_script_error;

#[test]
fn a_literal_where_a_name_is_bound_is_an_error() {
    let sources = [
        // parameters
        "(define (foo 10) 10)",
        "(define (foo \"x\") 1)",
        "(define (foo #t) 1)",
        "(define (foo 'x) 1)",
        "(define (foo a 1.5 b) a)",
        "(define (foo . 5) 1)",
        "(define (foo a . \"rest\") 1)",
        "(define ((foo a) 5) a)",
        "(define ((foo 1) b) b)",
        "(lambda (10) 10)",
        "(lambda (`x) 1)",
        "(lambda (x \"s\" y) x)",
        "(lambda 5 1)",
        "(define foo (lambda (a 2) a))",
        // the name of a function
        "(define (10 a) 1)",
        "(define (\"f\" a) 1)",
        // what define names
        "(define 10 1)",
        "(define \"x\" 1)",
        "(define (f) (define 5 1) 2)",
        // what let binds
        "(let ([10 1]) 2)",
        "(let ([\"x\" 1]) 2)",
        "(let loop ([10 1]) 2)",
        "(let* ([1 2]) 3)",
        "(letrec ([1 2]) 3)",
        "(letrec ([#:k 1]) 1)",
        "(letrec* ([#:k 1]) 1)",
        "(let 1 ([x 1]) 1)",
        "(let \"s\" ([i 0]) i)",
        "(let #:k ([i 0]) i)",
        "(let ([(x y) 1]) x)",
        "(%plain-let ((1 2)) 1)",
        "(%plain-let ((#:k 2)) 1)",
        "(%plain-let ((() 2)) 1)",
        "(%plain-let (((x) 2)) 1)",
        // parameters with defaults and keywords
        "(lambda ((1 2)) 3)",
        "(define (f (1 2)) 2)",
        "(define (f #:k #:j) 2)",
        "(define (f #:k [1 2]) 2)",
        "(define (f [[a] 1]) a)",
        "(define (f [#:k 1]) 1)",
        "(define (f a #:key) a)",
        "(define (g a #:key [\"s\" 1]) a)",
        "((lambda ([\"s\" 2]) 1) 1)",
        // keywords and empty lists as names
        "(define #:k 2)",
        "(define (#:k x) 2)",
        "(define ((#:k a) b) 1)",
        "(define-values (#:k b) (values 1 2))",
        "(struct #:k (x y))",
        "(let ([x 1]) (define #:k 1) x)",
        "(define (()))",
        "(define (()) 2)",
        "(define (() x) 1)",
        "(define ((()) a) 1)",
        // names that come out of a macro
        "(define-syntax m (syntax-rules () [(_ x) (lambda (x) 1)])) (m 5)",
        "(define-syntax m (syntax-rules () [(_ x) (define x 1)])) (m 5)",
        "(define-syntax m (syntax-rules () [(_ x) (let ([x 1]) x)])) (m \"s\")",
        // what a module provides and requires
        "(provide 1)",
        "(provide (for-syntax 1))",
        "(require (only-in \"steel/option\" 1))",
    ];

    for source in sources {
        assert_script_error(source);
    }
}

#[test]
fn what_can_still_become_a_parameter_is_let_through() {
    let sources = [
        "(define (f a b) (+ a b)) (assert! (= (f 1 2) 3))",
        "(define (f . rest) rest) (assert! (equal? (f 1 2) '(1 2)))",
        "(define (f a . rest) rest) (assert! (equal? (f 1 2 3) '(2 3)))",
        "(define ((curried a) b) (+ a b)) (assert! (= ((curried 1) 2) 3))",
        "(define (f #:key [x 1]) x) (assert! (= (f) 1)) (assert! (= (f #:key 5) 5))",
        "(assert! (equal? ((lambda args args) 1 2) '(1 2)))",
        "(assert! (= ((lambda (a . rest) a) 1 2) 1))",
        "(assert! (= (let ([a 1] [b 2]) (+ a b)) 3))",
        "(assert! (= (let loop ([i 0]) (if (< i 3) (loop (+ i 1)) i)) 3))",
        "(define (f a [b 5]) (+ a b)) (assert! (= (f 1) 6))",
        "(define (f a #:b [b 2] #:c c) (list a b c)) (assert! (equal? (f 1 #:c 3) '(1 2 3)))",
        "(assert! (= (%plain-let ((x 1) (y 2)) (+ x y)) 3))",
        "(require (only-in \"steel/result\" Ok (Err MyErr))) (assert! (Ok? (Ok 1)))",
    ];

    for source in sources {
        super::assert_script(source);
    }
}

#[test]
fn a_module_providing_a_contract_for_something_that_is_not_a_name_is_an_error() {
    let provided = ["5", "(a)", "#:k"];

    for (index, name) in provided.iter().enumerate() {
        let module = format!("malformed-provide-{index}");
        let mut engine = crate::steel_vm::engine::Engine::new();
        engine.register_steel_module(
            module.clone(),
            format!("(provide (contract/out {name} any/c)) (define a 1)"),
        );

        assert!(
            engine.run(format!("(require \"{module}\")")).is_err(),
            "{name}"
        );
    }
}
