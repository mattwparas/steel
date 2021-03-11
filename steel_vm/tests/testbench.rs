mod helpers;
extern crate steel_vm;

use std::path::PathBuf;

use crate::steel_vm::engine::Engine;
use helpers::*;
use steel::PRELUDE;

#[test]
fn basic_test() {
    test_from_files("input_tests.rkt", "output_tests.rkt");
}

#[test]
fn module_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    evaluator
        .parse_and_execute_from_path(PathBuf::from("tests/modules/main.rkt"))
        .unwrap();
    test_line("(a 10)", &["127"], &mut evaluator);
    test_line("(b 20)", &["47"], &mut evaluator);
    evaluator.parse_and_execute("b-private").unwrap_err();
}

#[test]
fn if_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    test_line("(if #t 'a 2)", &["'a"], &mut evaluator);
    test_line("(if 'a 'b 1)", &["'b"], &mut evaluator);
    test_line(
        "(if (= 1 2) a 2)",
        &["Error: FreeIdentifier: a"],
        &mut evaluator,
    );
    test_line(
        "(if (= 1 1) a 2)",
        &["Error: FreeIdentifier: a"],
        &mut evaluator,
    );
    test_line(
        "(if (= 1 1))",
        &["Error: Parse: Parse: Syntax Error: if expects a then condition, found none"],
        &mut evaluator,
    );
    test_line(
        "(if 1 2 3 4)",
        &["Error: Parse: Parse: Syntax Error: if takes only 3 expressions"],
        &mut evaluator,
    );
}

#[test]
fn define_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("a", &["Error: FreeIdentifier: a"], e);
    test_line(
        "(define a (lambda (x) (+ x 1)) wat)",
        &["Error: Parse: Parse: Syntax Error: Define expected only one expression after the identifier"],
        e,
    );
    test_line(
        "(define a)",
        &["Error: Parse: Parse: Syntax Error: define statement expected a body, found none"],
        e,
    );
    test_line("(define a (lambda (x) (+ x 1)))", &["#<void>"], e);
    test_line("a", &["#<bytecode-closure>"], e);
    test_line("(a 2)", &["3"], e);
    test_line("(define (b a1 a2 a3) (+ a1 a2 a3))", &["#<void>"], e);
    test_line("(b 10 20 30)", &["60"], e);
    test_line("(define b 10)", &["#<void>"], e);
    test_line("b", &["10"], e);
    test_line("a1", &["Error: FreeIdentifier: a1"], e);
}

#[test]
fn lambda_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(lambda (x) 1 2)", &["#<bytecode-closure>"], e);
    test_line(
        "(lambda x 1)",
        &["Error: Parse: Parse: Syntax Error: lambda function expected a list of identifiers"],
        e,
    );
    test_line("(lambda () 1)", &["#<bytecode-closure>"], e);
    test_line(
        "(lambda () (lambda () (lambda () (lambda () 1))))",
        &["#<bytecode-closure>"],
        e,
    );
    test_line(
        "(define x (lambda (w) (lambda (x y) (lambda (z) (lambda (a) (+ w x y z a))))))",
        &["#<void>"],
        e,
    );
    test_line("((((x 1) 2 3) 4) 5)", &["15"], e);
}

#[test]
fn set_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(set! x 10)", &["Error: FreeIdentifier: x"], e);
    test_line(
        "(set! x)",
        &["Error: Parse: Parse: Arity mismatch: set! expects an identifier and an expression"],
        e,
    );
    test_line(
        "(set! x 1 2)",
        &["Error: Parse: Parse: Arity mismatch: set! expects an identifier and an expression"],
        e,
    );
    test_line(
        "(define x 100) (set! x (+ x 1)) x",
        &["#<void>", "100", "101"],
        e,
    );
    test_line(
        "(define x (lambda () (begin (define a 10) (set! a 20) a))) (x)",
        &["#<void>", "20"],
        e,
    );
    test_line(
        "(define a 1000) (define x (lambda () (begin (set! a 20)  a))) (x)",
        &["#<void>", "#<void>", "20"],
        e,
    );
}

#[test]
fn let_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(let ((x 10)) (+ x x))", &["20"], e);
    test_line("(let ((x 10) (y 20)) (+ x y))", &["30"], e);
    test_line("(let () 1)", &["1"], e);
    test_line(
        "(let ((1)) x)",
        &["Error: Parse: Parse: Syntax Error: let expected a list of variable binding pairs, found a pair with length 1"],
        e,
    );
    test_line(
        "(let ((x 1) (1)) x)",
        &["Error: Parse: Parse: Syntax Error: let expected a list of variable binding pairs, found a pair with length 1"],
        e,
    );
    test_line(
        "(let ((x 1)))",
        &["Error: Parse: Parse: Syntax Error: let expects an expression, found none"],
        e,
    );
    test_line("(let ((x 1)) 1 2 3 4)", &["4"], e);
}

#[test]
fn and_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(and #t #f)", &["#false"], e);
    test_line("(and #t #t)", &["#true"], e);
    test_line("(and a #t)", &["Error: FreeIdentifier: a"], e);
    test_line("(and #f a)", &["Error: FreeIdentifier: a"], e);
    test_line(
        "(and (= 1 1) (= 1 2) who are you)",
        &["Error: FreeIdentifier: who"],
        e,
    );
    test_line("(and (= 1 1) (= (+ 1 1) 2) (< 3 4))", &["#true"], e);
}

#[test]
fn or_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(or #t #f)", &["#true"], e);
    test_line("(or #t #t)", &["#true"], e);
    test_line("(or #f #t)", &["#true"], e);
    test_line("(or #f a)", &["Error: FreeIdentifier: a"], e);
    test_line("(or #t a)", &["Error: FreeIdentifier: a"], e);
    test_line(
        "(or #t whatever you want idk)",
        &["Error: FreeIdentifier: whatever"],
        e,
    );
    test_line("(or (> 3 4) (> 4 5) (> 5 6) (= 1 1))", &["#true"], e);
}

#[test]
fn cond_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(cond [else 10])", &["10"], e);
    test_line("(cond [#f 10] [else 20])", &["20"], e);
    test_line(
        "(cond
            [#f 1]
            [#f 2]
            [#t 25
                50]
            [else 100])",
        &["50"],
        e,
    );
}

#[test]
fn when_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(when #t 10)", &["10"], e);
    test_line("(when #f 10)", &["#<void>"], e);
}

#[test]
fn unless_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(unless #t 10)", &["#<void>"], e);
    test_line("(unless #f 10)", &["10"], e);
}

#[test]
fn thread_first_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line(
        "(->> (list 1 2 3 4)
            (map (lambda (x) (* 2 x)))
            (append (list 5 6))
            (map (lambda (x) (* 5 x)))
            (append (list 1 2 3 4)))",
        &["'(1 2 3 4 25 30 10 20 30 40)"],
        e,
    );
}

#[test]
fn thread_last_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line(
        "(-> (list 1 2 3 4)
            (drop 2)
            (append (list 5 6)))",
        &["'(3 4 5 6)"],
        e,
    );
}

#[test]
fn first_apply_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("((f> append (list 3 4)) (list 1 2))", &["'(1 2 3 4)"], e);
}

#[test]
fn last_apply_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("((l> append (list 3 4)) (list 1 2))", &["'(3 4 1 2)"], e);
}

#[test]
fn while_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(define x 0)", &["#<void>"], e);
    test_line("(while (< x 5) (set! x (+ x 1))) x", &["#<void>", "5"], e);
}

#[test]
fn map_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line(
        "(map (lambda (x) (* 2 x)) (list 1 2 3 4))",
        &["'(2 4 6 8)"],
        e,
    );
}

#[test]
fn filter_test() {
    let mut evaluator = Engine::new();
    evaluator.parse_and_execute(PRELUDE).unwrap();
    let e = &mut evaluator;
    test_line("(filter even? (list 1 2 3 4 5))", &["'(2 4)"], e);
}
