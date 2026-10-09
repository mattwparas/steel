//! Binding forms with a literal where the name goes. Each has to be an
//! error and not a panic.

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
    ];

    for source in sources {
        super::assert_script(source);
    }
}
