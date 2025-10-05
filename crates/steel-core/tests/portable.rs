#![allow(dead_code)]

use steel::steel_vm::engine::Engine;
use steel::SteelVal;

/// Basic regression: calling a function after a top-level error should succeed once redefined.
pub fn top_level_error_allows_redefining() {
    let mut evaluator = Engine::new();

    evaluator
        .run("(define (x) (- x 2))")
        .expect("initial definition should succeed");

    evaluator
        .run("(x)")
        .expect_err("invoking the function before redefining should fail");

    evaluator
        .run("(define (x) (- x 2))")
        .expect("redefinition after an error should succeed");
}

/// Ensure mutations of primitive procedures can be observed immediately.
pub fn redefinition_of_values_over_time() {
    let mut vm = Engine::new();

    vm.compile_and_run_raw_program("(define + -)")
        .expect("redefinition should succeed");
    let res = vm
        .compile_and_run_raw_program("(+ 10 20)")
        .expect("invocation should succeed");

    assert_eq!(res.first(), Some(&SteelVal::IntV(-10)));
}

/// Verify redefining constant evaluation functions is reflected immediately.
pub fn redefinition_of_functions_for_constant_evaluation() {
    let mut vm = Engine::new();

    vm.compile_and_run_raw_program("(define foo-bar (lambda (x y) (+ x y)))")
        .expect("initial definition should succeed");
    let res = vm
        .compile_and_run_raw_program("(foo-bar 10 20)")
        .expect("call should succeed");
    assert_eq!(res.first(), Some(&SteelVal::IntV(30)));

    vm.compile_and_run_raw_program("(define foo-bar (lambda (x y) (#%black-box) (- x y)))")
        .expect("redefinition should succeed");
    let res = vm
        .compile_and_run_raw_program("(foo-bar 10 20)")
        .expect("call after redefinition should succeed");
    assert_eq!(res.first(), Some(&SteelVal::IntV(-10)));
}

/// Small arithmetic sanity check to ensure the interpreter can evaluate simple programs.
pub fn arithmetic_smoke_test() {
    let mut vm = Engine::new();
    let result = vm
        .compile_and_run_raw_program("(+ 1 2 3 4)")
        .expect("program should run");

    assert_eq!(result.first(), Some(&SteelVal::IntV(10)));
}
