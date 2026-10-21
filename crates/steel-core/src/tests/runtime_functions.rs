use crate::rvals::{Result, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::engine::Engine;
use crate::steel_vm::register_fn::RegisterFn;
use crate::steel_vm::runtime_fn::{RegisterRuntimeFn, Runtime};

fn engine() -> Engine {
    let mut engine = Engine::new();
    engine.register_fn_with_runtime(
        "host-twice",
        |runtime: &mut Runtime, f: SteelVal, x: SteelVal| -> Result<SteelVal> {
            let once = runtime.call_function(&f, [x])?;
            runtime.call_function(&f, [once])
        },
    );
    engine.register_fn_with_runtime(
        "host-call-all",
        |runtime: &mut Runtime, fs: Vec<SteelVal>| -> Result<Vec<SteelVal>> {
            fs.iter().map(|f| runtime.call_function(f, [])).collect()
        },
    );
    // Carries on when the function fails, which also swallows a continuation
    // leaving through it
    engine.register_fn_with_runtime(
        "host-ignoring-errors",
        |runtime: &mut Runtime, f: SteelVal| -> isize {
            match runtime.call_function(&f, []) {
                Ok(_) => 1,
                Err(_) => 0,
            }
        },
    );
    // Holds `held` while `f` runs, then gives it back
    engine.register_fn_with_runtime(
        "host-hold",
        |runtime: &mut Runtime, held: SteelVal, f: SteelVal| -> Result<SteelVal> {
            runtime.call_function(&f, [])?;
            Ok(held)
        },
    );
    engine.register_fn_with_runtime(
        "host-scale",
        |runtime: &mut Runtime, f: SteelVal, n: isize| -> Result<isize> {
            match runtime.call_function(&f, [SteelVal::IntV(n)])? {
                SteelVal::IntV(m) => Ok(m * 10),
                other => crate::stop!(TypeMismatch => "expected an integer, found {}", other),
            }
        },
    );
    engine.register_fn_with_runtime(
        "host-call",
        |runtime: &mut Runtime, f: SteelVal, args: Vec<SteelVal>| -> Result<SteelVal> {
            runtime.call_function(&f, args)
        },
    );
    engine.register_fn("host-add1", |x: isize| x + 1);
    engine
}

#[test]
fn host_functions_call_back_into_steel() {
    let mut engine = engine();
    engine
        .compile_and_run_raw_program(
            r#"
            (assert! (= (host-twice (lambda (x) (* x 3)) 2) 18))
            (assert! (= (host-twice host-add1 1) 3))
            (assert! (= (host-twice add1 1) 3))
            (assert! (equal? (host-call-all (list (lambda () 1) (lambda () 'two))) '(1 two)))

            ;; Through a host function, into Steel, and through it again
            (assert! (= (host-twice (lambda (x) (host-twice (lambda (y) (+ y 1)) x)) 0) 4))

            ;; Often enough to be compiled
            (define (loop n acc)
              (if (= n 0) acc (loop (- n 1) (+ acc (host-twice (lambda (x) (+ x 1)) n)))))
            (assert! (= (loop 2000 0) (+ (* 2000 2001 1/2) 4000)))

            ;; A callable struct
            (struct adder (n) #:prop:procedure 1 #:transparent)
            (struct callable-adder (n f) #:prop:procedure 1)
            (assert! (= (host-twice (callable-adder 10 (lambda (x) (+ x 10))) 1) 21))
            "#,
        )
        .unwrap();
}

#[test]
fn errors_go_both_ways() {
    let mut engine = engine();
    engine
        .compile_and_run_raw_program(
            r#"
            (define (message e) (trim (error-object-message e)))
            (assert! (equal? (with-handler message (host-twice (lambda (x) (error "in the callback")) 1))
                             "in the callback"))
            (assert! (= (host-ignoring-errors (lambda () (error "ignored"))) 0))
            (assert! (equal? (with-handler message (host-twice 5 1))
                             "call-function: not a procedure: 5"))
            ;; The engine is fine afterwards
            (assert! (= (host-twice (lambda (x) (+ x 1)) 1) 3))
            "#,
        )
        .unwrap();
}

#[test]
fn continuations_through_host_functions() {
    let mut engine = engine();
    engine
        .compile_and_run_raw_program(
            r#"
            ;; Leaving through one
            (assert! (eq? (call/cc (lambda (k) (host-twice (lambda (x) (k 'left)) 1))) 'left))
            ;; Even when it swallows the error that carries the continuation out
            (assert! (eq? (call/cc (lambda (k) (host-ignoring-errors (lambda () (k 'left))))) 'left))
            ;; One captured in a callback can't be resumed once the host function has returned
            (define kept #f)
            (host-twice (lambda (x) (call/cc (lambda (k) (set! kept k) x))) 1)
            (assert! (equal? (with-handler (lambda (e) (trim (error-object-message e))) (kept 5))
                             "this continuation was captured under a call from native code that has returned"))
            (assert! (= (host-twice (lambda (x) (+ x 1)) 1) 3))
            "#,
        )
        .unwrap();
}

#[test]
fn what_the_host_holds_survives_a_collection_in_a_callback() {
    let mut engine = engine();
    engine
        .compile_and_run_raw_program(
            r#"
            (define (churn)
              (#%gc-collect)
              (let loop ([i 0] [acc '()])
                (if (= i 2000)
                    (length acc)
                    (loop (+ i 1) (cons (mutable-vector i i) (cons (box i) acc)))))
              (#%gc-collect))
            (define (check n)
              (when (> n 0)
                (assert! (equal? (host-hold (mutable-vector 1 2 3) churn) (mutable-vector 1 2 3)))
                (assert! (equal? (unbox (host-hold (box 'kept) churn)) 'kept))
                (check (- n 1))))
            (check 20)
            "#,
        )
        .unwrap();
}

fn runtime_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("tests/runtime");
    module.register_fn_with_runtime(
        "module-twice",
        |runtime: &mut Runtime, f: SteelVal, x: isize| -> Result<SteelVal> {
            let once = runtime.call_function(&f, [SteelVal::IntV(x)])?;
            runtime.call_function(&f, [once])
        },
    );
    module
}

#[test]
fn modules_have_runtime_functions_in_every_engine() {
    let module = runtime_module();
    let mut engines: Vec<Engine> = (0..2).map(|_| Engine::new()).collect();
    for engine in &mut engines {
        engine.register_module(module.clone());
    }
    // A different program in each, so their globals differ
    engines[0]
        .compile_and_run_raw_program("(define a 1) (define b 2)")
        .unwrap();
    for engine in &mut engines {
        engine
            .compile_and_run_raw_program(
                r#"
                (require-builtin tests/runtime)
                (assert! (= (module-twice (lambda (x) (+ x 1)) 1) 3))
                (define (loop n acc)
                  (if (= n 0) acc (loop (- n 1) (+ acc (module-twice (lambda (x) (* x 2)) 1)))))
                (assert! (= (loop 500 0) 2000))
                "#,
            )
            .unwrap();
    }
}

#[test]
fn signatures_of_runtime_functions() {
    let mut engine = engine();
    engine.register_module(runtime_module());
    // The runtime is passed by the wrapper, so callers supply the rest
    engine
        .compile_and_run_raw_program(
            r#"
            (require-builtin tests/runtime)
            (assert! (equal? (function-signature host-scale)
                             '((args Any Int) (returns Int) (raises Maybe))))
            (assert! (equal? (function-signature module-twice)
                             '((args Any Int) (returns Any) (raises Maybe))))
            "#,
        )
        .unwrap();
}

#[test]
fn builtins_and_continuations_from_host_functions() {
    let mut engine = engine();
    engine
        .compile_and_run_raw_program(
            r#"
            ;; Builtins, including ones that set up a frame for the vm
            (assert! (= (host-call apply (list + (list 1 2 3))) 6))
            (assert! (eq? (host-call string->symbol (list "abc")) 'abc))
            (assert! (= (host-call call/cc (list (lambda (k) (k 7)))) 7))
            (assert! (equal? (host-call #%prim.transduce (list (list 1 2) (mapping add1) (into-list)))
                             '(2 3)))
            (assert! (= (host-call apply (list host-call (list + (list 1 2)))) 3))

            ;; A continuation of Steel code further out leaves through the host
            (assert! (eq? (call/cc (lambda (k) (host-call k (list 'out)))) 'out))
            (assert! (= (+ 1 (call/cc (lambda (k) (host-call k (list 10))))) 11))
            ;; One captured in the same callback as the call
            (assert! (= (host-call (lambda () (+ 1 (call/cc (lambda (k) (host-call k (list 5)))))) '()) 6))
            ;; Arity errors and the like come from the callee
            (assert! (equal? (with-handler (lambda (e) 'raised) (host-call (lambda (x) x) '()))
                             'raised))

            ;; Often enough to be compiled
            (define (loop n acc)
              (if (= n 0)
                  acc
                  (loop (- n 1) (+ acc (call/cc (lambda (k) (host-call k (list 1))))))))
            (assert! (= (loop 1000 0) 1000))
            "#,
        )
        .unwrap();
}
