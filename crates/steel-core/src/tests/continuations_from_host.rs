//! A continuation called from the host carries on with what it had left to
//! do, and the host gets that computation's result.

use crate::rvals::SteelVal;
use crate::steel_vm::engine::Engine;

fn int(engine: &Engine, name: &str) -> isize {
    match engine.extract_value(name).expect("the global is there") {
        SteelVal::IntV(n) => n,
        other => panic!("{name} is {other}"),
    }
}

fn call(engine: &mut Engine, name: &str, arguments: Vec<SteelVal>) -> SteelVal {
    let function = engine.extract_value(name).expect("the global is there");
    engine
        .call_function_with_args(function, arguments)
        .unwrap_or_else(|e| panic!("calling {name}: {e}"))
}

fn refused(engine: &mut Engine, name: &str, arguments: Vec<SteelVal>) -> String {
    let function = engine.extract_value(name).expect("the global is there");
    engine
        .call_function_with_args(function, arguments)
        .expect_err("the call is refused")
        .to_string()
}

/// Both what `call/cc` hands out and the bare continuation under it.
const CAPTURES: [&str; 2] = ["call/cc", "#%prim.call/cc"];

#[test]
fn one_kept_from_a_top_level_form_runs_the_rest_of_the_form() {
    for capture in CAPTURES {
        let mut engine = Engine::new();
        engine
            .run(format!(
                r#"
                (define kept #f)
                (define times 0)
                (define result (+ 1 ({capture} (lambda (k) (set! kept k) 1))))
                (set! times (+ times 1))
                "#
            ))
            .expect("the program runs");
        assert_eq!(int(&engine, "result"), 2);
        assert_eq!(int(&engine, "times"), 1);

        call(&mut engine, "kept", vec![SteelVal::IntV(10)]);
        assert_eq!(int(&engine, "result"), 11, "{capture}");

        call(&mut engine, "kept", vec![SteelVal::IntV(20)]);
        assert_eq!(int(&engine, "result"), 21, "{capture}");

        // The engine is as usable as before
        engine.run("(define after (+ result 1))").expect("it runs");
        assert_eq!(int(&engine, "after"), 22);
    }
}

#[test]
fn the_host_gets_what_the_form_came_to() {
    for capture in CAPTURES {
        let mut engine = Engine::new();
        engine
            .run(format!(
                r#"
                (define kept #f)
                (* 2 (+ 1 ({capture} (lambda (k) (set! kept k) 1))))
                "#
            ))
            .expect("the program runs");

        let result = call(&mut engine, "kept", vec![SteelVal::IntV(10)]);
        assert_eq!(result, SteelVal::IntV(22), "{capture}");
    }
}

#[test]
fn one_kept_from_an_earlier_call_runs_the_rest_of_that_call() {
    for capture in CAPTURES {
        let mut engine = Engine::new();
        engine
            .run(format!(
                r#"
                (define kept #f)
                (define seen '())
                (define (keep-one start)
                  (let ([v (+ start ({capture} (lambda (k) (set! kept k) 1)))])
                    (set! seen (cons v seen))
                    (* 2 v)))
                (define (seen-so-far) (length seen))
                "#
            ))
            .expect("the program runs");

        assert_eq!(
            call(&mut engine, "keep-one", vec![SteelVal::IntV(100)]),
            SteelVal::IntV(202)
        );
        assert_eq!(
            call(&mut engine, "kept", vec![SteelVal::IntV(5)]),
            SteelVal::IntV(210),
            "{capture}"
        );
        assert_eq!(
            call(&mut engine, "kept", vec![SteelVal::IntV(6)]),
            SteelVal::IntV(212),
            "{capture}"
        );
        assert_eq!(call(&mut engine, "seen-so-far", vec![]), SteelVal::IntV(3));

        // And the function still works from the start
        assert_eq!(
            call(&mut engine, "keep-one", vec![SteelVal::IntV(1)]),
            SteelVal::IntV(4)
        );
    }
}

#[test]
fn a_generator_can_be_driven_from_the_host() {
    let mut engine = Engine::new();
    engine
        .run(
            r#"
            (define resume #f)
            (define (count-up limit)
              (let loop ([i 0])
                (if (= i limit)
                    'done
                    (begin
                      (call/cc (lambda (k) (set! resume (lambda () (k #t))) i))
                      (loop (+ i 1))))))
            (define (next) (resume))
            "#,
        )
        .expect("the program runs");

    // The first call runs to the end, leaving each step's continuation
    // behind it. Each resume after that runs from the last step again.
    assert_eq!(
        call(&mut engine, "count-up", vec![SteelVal::IntV(3)]).to_string(),
        "done"
    );
    for _ in 0..5 {
        assert_eq!(call(&mut engine, "next", vec![]).to_string(), "done");
    }
}

#[test]
fn an_error_in_what_is_left_comes_back_to_the_host() {
    for capture in CAPTURES {
        let mut engine = Engine::new();
        engine
            .run(format!(
                r#"
                (define kept #f)
                (define (keep-one)
                  (car ({capture} (lambda (k) (set! kept k) (list 1)))))
                (define (handled)
                  (with-handler (lambda (e) 'handled)
                                (car ({capture} (lambda (k) (set! kept k) (list 1))))))
                "#
            ))
            .expect("the program runs");

        assert_eq!(call(&mut engine, "keep-one", vec![]), SteelVal::IntV(1));
        let message = refused(&mut engine, "kept", vec![SteelVal::IntV(5)]);
        assert!(message.contains("car"), "{capture}: {message}");

        // A handler that was around the capture is around it again
        assert_eq!(call(&mut engine, "handled", vec![]), SteelVal::IntV(1));
        assert_eq!(
            call(&mut engine, "kept", vec![SteelVal::IntV(5)]).to_string(),
            "handled",
            "{capture}"
        );

        assert_eq!(call(&mut engine, "keep-one", vec![]), SteelVal::IntV(1));
    }
}

#[test]
fn one_from_a_callback_of_a_builtin_is_refused() {
    for capture in CAPTURES {
        let mut engine = Engine::new();
        engine
            .run(format!(
                r#"
                (define kept #f)
                (define (keep-one)
                  (transduce (list 1 2 3)
                             (mapping (lambda (x) ({capture} (lambda (k) (set! kept k) x))))
                             (into-list)))
                (keep-one)
                "#
            ))
            .expect("the program runs");

        let message = refused(&mut engine, "kept", vec![SteelVal::IntV(5)]);
        assert!(
            message.contains("native code that has returned"),
            "{capture}: {message}"
        );

        // Kept during a call from the host this time
        call(&mut engine, "keep-one", vec![]);
        let message = refused(&mut engine, "kept", vec![SteelVal::IntV(5)]);
        assert!(
            message.contains("native code that has returned"),
            "{capture}: {message}"
        );

        assert_eq!(call(&mut engine, "keep-one", vec![]).to_string(), "(1 2 3)");
    }
}

#[test]
fn the_wrong_number_of_arguments_is_refused() {
    let mut engine = Engine::new();
    engine
        .run(
            r#"
            (define kept #f)
            (define result (+ 1 (#%prim.call/cc (lambda (k) (set! kept k) 1))))
            "#,
        )
        .expect("the program runs");

    refused(&mut engine, "kept", vec![]);
    refused(
        &mut engine,
        "kept",
        vec![SteelVal::IntV(1), SteelVal::IntV(2)],
    );
    assert_eq!(int(&engine, "result"), 2);

    call(&mut engine, "kept", vec![SteelVal::IntV(10)]);
    assert_eq!(int(&engine, "result"), 11);
}
