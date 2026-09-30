#[cfg(test)]
mod call_cc_tests {
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterFn;

    #[test]
    fn test_async() {
        async fn test_function() -> usize {
            10 + await_within().await
        }

        async fn await_within() -> usize {
            25
        }

        let mut vm = Engine::new();

        // You can even register async finctions
        vm.register_fn("test", test_function);

        let contents = r#"
        ; *thread-queue* : list[continuation]
        (define *thread-queue* '())

        ; halt : continuation
        (define halt #f)

        ; current-continuation : -> continuation
        (define (current-continuation)
            (call/cc
                (lambda (cc)
                    (cc cc))))

        ; await : future -> value
        ; yield the current thread and loop until the value is completed
        (define (await future)
            (define output (poll! future))
            (if output
                output
                (begin
                    (yield)
                    (await future))))

        ; spawn : (-> anything) -> void
        (define (spawn thunk)
        (let ((cc (current-continuation)))
            (if (continuation? cc)
                (set! *thread-queue* (append *thread-queue* (list cc)))
                (begin
                    (thunk)
                    (quit)))))

        ; yield : value -> void
        (define (yield)
        (let ((cc (current-continuation)))
            (if (and (continuation? cc) (pair? *thread-queue*))
                (let ((next-thread (car *thread-queue*)))
                (set! *thread-queue* (append (cdr *thread-queue*) (list cc)))
                (next-thread 'resume))
                void)))

        ; quit : -> ...
        (define (quit)
        (if (pair? *thread-queue*)
            (let ((next-thread (car *thread-queue*)))
                (set! *thread-queue* (cdr *thread-queue*))
                (next-thread 'resume))
            (halt)))

        ; start-threads : -> ...
        (define (start-threads)
        (let ((cc (current-continuation)))
            (if cc
                (begin
                (set! halt (lambda () (cc #f)))
                (if (null? *thread-queue*)
                    void
                    (begin
                        (let ((next-thread (car *thread-queue*)))
                        (set! *thread-queue* (cdr *thread-queue*))
                        (next-thread 'resume)))))
                void)))


        ;; Example cooperatively threaded program
        (define counter 10)

        (define (make-thread-thunk name)
        (define (loop)
                (when (< counter 0)
                    (quit))
                (display "in thread ")
                (display name)
                (display "; counter = ")
                (display counter)
                (newline)
                (set! counter (- counter 1))
                (define output (await (test))) ;; block the execution of this thread on this future
                (display "Future done!: ")
                (display output)
                (newline)
                (yield)
                (loop))
        loop)

        (spawn (make-thread-thunk 'a))
        (spawn (make-thread-thunk 'b))
        (spawn (make-thread-thunk 'c))

        (start-threads)
        "#;

        let res = vm.compile_and_run_raw_program(contents);

        if let Err(e) = res {
            vm.raise_error(e);
            panic!();
        }

        assert_eq!(vm.extract::<isize>("counter").unwrap(), -1);
    }
}

#[cfg(test)]
mod register_fn_tests {
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterFn;

    fn external_function(arg1: usize, arg2: usize) -> usize {
        arg1 + arg2
    }

    fn option_function(arg1: Option<String>) -> Option<String> {
        arg1
    }

    fn result_function(arg1: Option<String>) -> Result<String, String> {
        if let Some(inner) = arg1 {
            Ok(inner)
        } else {
            Err("Got a none".to_string())
        }
    }

    fn add_one(arg: usize) -> usize {
        arg + 1
    }

    fn always_true(_arg: usize) -> bool {
        true
    }

    fn empty_function() {}

    #[test]
    fn test_register_fn() {
        let mut vm = Engine::new();

        // Here we can register functions
        // Any function can accept parameters that implement `FromSteelVal` and
        // return values that implement `IntoSteelVal`
        vm.register_fn("external-function", external_function);

        // See the docs for more information about `FromSteelVal` and `IntoSteelVal`
        // but we can see even functions that accept/return Option<T> or Result<T,E>
        // can be registered
        vm.register_fn("option-function", option_function);

        // Result values will map directly to errors in the VM and bubble back up
        vm.register_fn("result-function", result_function);

        // functions that return () return void
        vm.register_fn("empty-function", empty_function);

        vm.register_fn("adding-one", add_one);
        vm.register_fn("always-true", always_true);

        vm.compile_and_run_raw_program(
            r#"
        (define foo (external-function 10 25))
        (define bar (option-function "applesauce"))
        (define baz (result-function "bananas"))
        (define res (transduce (range 0 10)
                               (taking 5)
                               (into-reducer external-function 0)))
        (assert! (equal? (map adding-one '(0 1 2 3)) '(1 2 3 4)))
        (assert! (equal? (filter always-true '(0 1 2 3)) '(0 1 2 3)))
        (empty-function)
    "#,
        )
        .unwrap();

        let foo = vm.extract::<usize>("foo").unwrap();
        assert_eq!(35, foo);

        // Can also extract a value by specifying the type on the variable
        let bar: String = vm.extract("bar").unwrap();
        assert_eq!("applesauce".to_string(), bar);

        let baz: String = vm.extract("baz").unwrap();
        assert_eq!("bananas".to_string(), baz);

        let res: usize = vm.extract("res").unwrap();
        assert_eq!(10, res);
    }
}

#[cfg(test)]
mod register_type_tests {
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterFn;

    // In order to register a type with Steel,
    // it must implement Clone, Debug, and Steel
    #[derive(Clone, Debug, PartialEq)]
    pub struct ExternalStruct {
        foo: usize,
        bar: String,
        baz: f64,
    }

    impl crate::rvals::Custom for ExternalStruct {}

    impl ExternalStruct {
        pub fn new(foo: usize, bar: String, baz: f64) -> Self {
            ExternalStruct { foo, bar, baz }
        }

        // Embedding functions that take self must take by value
        pub fn method_by_value(self) -> usize {
            self.foo
        }

        // Setters should update the value and return a new instance (functional set)
        pub fn set_foo(mut self, foo: usize) -> Self {
            self.foo = foo;
            self
        }
    }
    #[test]
    fn register_type_test() {
        let mut vm = Engine::new();

        // Registering a type gives access to a predicate for the type
        vm.register_type::<ExternalStruct>("ExternalStruct?");

        // Structs in steel typically have a constructor that is the name of the struct
        vm.register_fn("ExternalStruct", ExternalStruct::new);

        // register_fn can be chained
        vm.register_fn("method-by-value", ExternalStruct::method_by_value)
            .register_fn("set-foo", ExternalStruct::set_foo);

        let external_struct = ExternalStruct::new(1, "foo".to_string(), 12.4);

        // Registering an external value is fallible if the conversion fails for some reason
        // For instance, registering an Err(T) is fallible. However, most implementation outside of manual
        // ones should not fail
        vm.register_external_value("external-struct", external_struct)
            .unwrap();

        let _ = vm
            .compile_and_run_raw_program(
                r#"
            (define new-external-struct (set-foo external-struct 100))
            (define get-output (method-by-value external-struct))
            (define second-new-external-struct (ExternalStruct 50 "bananas" 72.6))
            "last-result"
        "#,
            )
            .unwrap();

        let new_external_struct = vm.extract::<ExternalStruct>("new-external-struct").unwrap();
        assert_eq!(
            ExternalStruct::new(100, "foo".to_string(), 12.4),
            new_external_struct
        );

        // Can also extract a value by specifying the type on the variable
        let get_output: usize = vm.extract("get-output").unwrap();
        assert_eq!(1, get_output);

        let second_new_external_struct: ExternalStruct =
            vm.extract("second-new-external-struct").unwrap();
        assert_eq!(
            ExternalStruct::new(50, "bananas".to_string(), 72.6),
            second_new_external_struct
        );
    }
}

#[cfg(test)]
mod contract_tests {
    use crate::steel_vm::test_util::{assert_script, assert_script_error};

    #[test]
    fn simple_flat_contract() {
        let script = r#"
          (define/contract (test x y)
            (->/c even? even? odd?)
            (+ x y 1))

          (assert! (equal? (test 2 2) 5))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_flat_contract_domain_violation() {
        let script = r#"
          (define/contract (test x y)
            (->/c even? even? odd?)
            (+ x y 1))

          (test 1 2)
        "#;
        assert_script_error(script);
    }

    #[test]
    fn simple_higher_order_contract() {
        let script = r#"
          (define/contract (blagh func y)
            (->/c (->/c even? odd?) even? even?)
            (+ 1 (func y)))

          (assert! (equal? (blagh (lambda (x) (+ x 1)) 2) 4))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_higher_order_contract_violation() {
        let script = r#"
          (define/contract (blagh func y)
            (->/c (->/c even? odd?) even? even?)
            (+ 1 (func y)))

          (blagh (lambda (x) (+ x 2)) 2)
        "#;
        assert_script_error(script);
    }

    // TODO: This will fail on old contract implementation
    // #[test]
    // fn tail_call_mutual_recursion() {
    //     let script = r#"
    //     (define/contract (foo x)
    //       (->/c int? int?)
    //         (if (= x 100)
    //             x
    //             (bar (+ x 1))))

    //     (define/contract (bar x)
    //       (->/c int? int?)
    //         (if (= x 100)
    //             x
    //             (foo (+ x 1))))

    //     (assert! (equal? (foo 0) 100))
    //   "#;
    //     assert_script(script);
    // }

    #[test]
    fn tail_call_contract_still_works() {
        let script = r#"
          (define/contract (loop x)
            (->/c int? int?)
              (if (= x 100)
                  x
                  (loop (+ x 1))))

          (assert! (equal? (loop 0) 100))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_on_application_success() {
        let script = r#"

        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10))

        (define/contract (accept func)
            (->/c (->/c string? int?) string?)
            "cool cool cool")

        (assert! (equal? (accept (output)) "cool cool cool"))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_not_called_since_not_applied() {
        let script = r#"

        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10))

        (define/contract (accept func)
            (->/c (->/c string? string?) string?)
            "cool cool cool")

        (assert! (equal? (accept (output)) "cool cool cool"))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_on_return_does_not_happen() {
        let script = r#"

        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10))

        (define/contract (accept)
            (->/c (->/c string? string?))
            (output))

        (accept)
        "#;

        assert_script(script);
    }

    #[test]
    fn contract_application_in_map() {
        let script = r#"
        (define (test x)
            (->/c int? int?)
            (+ x 1))
        (define result (map test (list 1 2 3 4)))
        (assert! (equal? result (list 2 3 4 5)))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_application_in_filter() {
        let script = r#"
        (define (test x)
            (->/c int? boolean?)
            (even? x))
        (define result (filter test (list 1 2 3 4)))
        (assert! (equal? result (list 2 4)))
        "#;
        assert_script(script);
    }

    #[test]
    fn contract_checking_with_weaker() {
        let script = r#"
        (define/contract (output)
            (->/c (->/c string? int?))
            (lambda (x) 10.1))

        (define/contract (accept)
            (->/c (->/c string? number?))
            (output))

        ((accept) "test")
        "#;

        assert_script_error(script)
    }

    #[test]
    fn contract_checking_pre_condition_later() {
        let script = r#"
        (define/contract (output)
            (->/c (->/c list? int?))
            (lambda (x) 10.0))

        (define/contract (accept)
            (->/c (->/c string? number?))
            (output))

        ((accept) "test")
        "#;

        assert_script_error(script);
    }

    #[test]
    fn three_levels_of_contracts() {
        let script = r#"
        (define (any? x) #t)

        (define/contract (level1)
            (->/c (->/c int?))
            (lambda () 10.2))

        (define/contract (level2)
            (->/c (->/c number?))
            (level1))

        (define/contract (level3)
            (->/c (->/c any?))
            (level2))

        ((level3))
        "#;

        assert_script_error(script);
    }

    #[test]
    fn contract_with_filter() {
        let script = r#"
        (define/contract (is-even? x)
            (->/c number? boolean?)
            (even? x))

        (define res (filter is-even? (range 0 10)))

        (assert! (equal? res '(0 2 4 6 8)))
        "#;

        assert_script(script);
    }

    #[test]
    fn contract_with_map() {
        let script = r#"
        (define/contract (adding1 x)
            (->/c number? number?)
            (+ x 1))

        (define res (map adding1 (range 0 5)))

        (assert! (equal? res '(1 2 3 4 5)))
        "#;

        assert_script(script);
    }

    #[test]
    fn contract_with_reduce() {
        let script = r#"
        (define/contract (reducer accum elem)
            (->/c number? number? number?)
            (+ accum elem))

        (define res (transduce (range 0 10) (taking 5) (into-reducer + 0)))

        (assert! (equal? res 10))
        "#;
        assert_script(script);
    }

    #[test]
    fn transducers_containing_contracts() {
        let script = r#"
        (define/contract (mapper x)
            (->/c number? number?)
            (+ x 1))

        (define/contract (is-even? x)
            (->/c number? boolean?)
            (even? x))

        (define x (mapping mapper))
        (define y (filtering is-even?))
        (define z (taking 10))

        (define xyz (compose x y z))

        (define exec-list (transduce (range 0 100) xyz (into-list)))
        (define exec-vector (transduce (range 0 100) xyz (into-vector)))
        (define my-sum (transduce (range 0 100) xyz (into-reducer + 0)))

        (assert! (equal? exec-list '(2 4 6 8 10 12 14 16 18 20)))
        (assert! (equal? exec-vector (vector 2 4 6 8 10 12 14 16 18 20)))
        (assert! (equal? my-sum 110))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod find_closest_match_tests {
    use crate::steel_vm::builtin::find_closest_match;

    #[test]
    fn similar_candidate_returns_similiar_candidate() {
        assert_eq!(
            find_closest_match("make-struct", ["unrelated", "--make-struct"]),
            Some("--make-struct")
        );
    }

    #[test]
    fn no_similar_strings_returns_none() {
        assert_eq!(
            find_closest_match("make-struct", ["unrelated", "make-tuple"]),
            None
        );
    }

    #[test]
    fn multiple_similar_strings_returns_most_similar() {
        assert_eq!(
            find_closest_match(
                "make-struct",
                ["unrelated", "--make-struct", "--make-stract"]
            ),
            Some("--make-struct")
        );
    }
}

#[cfg(test)]
mod expansion_tests {
    use crate::steel_vm::engine::Engine;
    use std::path::PathBuf;

    /// A main program, compiled with `compile_and_run_raw_program`, that requires
    /// a module and has a `case` clause whose datum names a special form.
    /// Expanding the required module's macros must not parse `(lambda)` inside
    /// the unexpanded `case` as a malformed lambda.
    #[test]
    fn case_datum_naming_a_special_form_in_a_program_with_requires() {
        let engine = || {
            let mut engine = Engine::new();
            engine.register_steel_module(
                "test/case-datum".to_string(),
                "(provide x) (define x 1)".to_string(),
            );
            engine
        };
        let src = r#"(require "test/case-datum")
            (define (f e) (case e ((lambda) 1) ((define) 2) (else 3)))
            (define result (list (f 'lambda) (f 'define) (f 'other)))"#;

        let mut run = engine();
        run.compile_and_run_raw_program(src).unwrap();
        assert_eq!(run.extract_value("result").unwrap().to_string(), "(1 2 3)");

        engine()
            .emit_expanded_ast_without_optimizations(src, None)
            .unwrap();

        // Malformed code is still an error.
        let bad = r#"(require "test/case-datum") (define (g) (lambda))"#;
        assert!(engine().compile_and_run_raw_program(bad).is_err());
    }

    /// A main program using a module's own `car` and `cdr` in a loop. Hoisting
    /// car before cdr must not replace the module's `(car u)` with
    /// `(#%prim.car u)`, which rejects the module's pairs.
    #[test]
    fn a_modules_car_is_not_the_builtin_car() {
        let mut engine = Engine::new();
        engine.register_steel_module(
            "test/vpairs".to_string(),
            "(provide vcons car cdr)
             (define (vcons a b) (vector a b))
             (define (car p) (if (vector? p) (vector-ref p 0) (#%prim.car p)))
             (define (cdr p) (if (vector? p) (vector-ref p 1) (#%prim.cdr p)))"
                .to_string(),
        );
        let src = r#"(require "test/vpairs")
            (define lst (vcons 1 (vcons 2 (vcons 3 '()))))
            (define (sum l)
              (let loop ([u l] [r 0])
                (if (null? u) r (loop (cdr u) (+ r (car u))))))
            (define result (sum lst))"#;
        engine.compile_and_run_raw_program(src).unwrap();
        assert_eq!(engine.extract_value("result").unwrap().to_string(), "6");
    }

    /// Jitted `vector-ref`, `vector-set!` and `vector-length` on global vectors,
    /// closed and reassigned, leave the vectors' handle counts where they were.
    /// A leaked handle keeps the vector from ever being collected.
    #[test]
    fn jitted_vector_access_releases_the_vector() {
        let mut engine = Engine::new();
        engine
            .compile_and_run_raw_program(
                "(define G (vector 1 2 3))
                 (define H (vector 1 2 3))
                 (define L (vector 1 2 3))
                 (define S (vector 1 2 3))
                 (define (reset!) (set! S (vector 1 2 3)))
                 (define (go n acc)
                   (if (= n 0)
                       acc
                       (begin
                         (vector-set! H 1 5)
                         (go (- n 1) (+ acc (vector-ref G 1) (vector-ref S 1) (vector-length L))))))
                 (define result (go 100000 0))",
            )
            .unwrap();
        for name in ["G", "H", "L", "S"] {
            let crate::rvals::SteelVal::MutableVector(v) = engine.extract_value(name).unwrap()
            else {
                panic!("{name} is a vector")
            };
            // The global's own handle and the one extracted here.
            assert!(v.inner.weak_count() <= 3, "{name}: {}", v.inner.weak_count());
        }
    }

    /// A module's private globals that are reassigned inside functions live in
    /// boxes. Reads, writes, closures over them, locals that shadow them, and
    /// the globals that stay plain (used by a macro, functions) all behave as
    /// before.
    #[test]
    fn mutated_module_globals_in_boxes() {
        let mut engine = Engine::new();
        engine.register_steel_module(
            "test/boxed-globals".to_string(),
            "(provide bump! count exported set-exported! read-in-macro swap-fn!
                      call-fn shadow make-reader data-ref)
             (define private 0)
             (define exported 10)
             (define macro-var 100)
             (define (f x) (* x 2))
             (define data (vector 1 2 3))
             (define (bump!) (set! private (+ private 1)) private)
             (define (count) private)
             (define (set-exported! v) (set! exported v))
             (define (set-macro-var! v) (set! macro-var v))
             (define-syntax read-in-macro
               (syntax-rules () [(_) (begin (set-macro-var! (+ macro-var 1)) macro-var)]))
             (define (swap-fn!) (set! f (lambda (x) (* x 3))))
             (define (call-fn x) (f x))
             (define (shadow private) (+ private 1000))
             (define (make-reader) (lambda () private))
             (define (data-ref i) (set! data (vector 7 8 9)) (vector-ref data i))
             (set! private (+ private 5))"
                .to_string(),
        );
        let src = r#"(require "test/boxed-globals")
            (define reader (make-reader))
            (define (loop n) (when (> n 0) (bump!) (loop (- n 1))))
            (loop 10000)
            (define counted (list (count) (reader)))
            (define macro-reads (list (read-in-macro) (read-in-macro)))
            (define calls (list (call-fn 5) (begin (swap-fn!) (call-fn 5))))
            (define others (list (shadow 1) (data-ref 1)))"#;
        engine.compile_and_run_raw_program(src).unwrap();
        let value = |name: &str| engine.extract_value(name).unwrap().to_string();
        assert_eq!(value("counted"), "(10005 10005)");
        assert_eq!(value("macro-reads"), "(101 102)");
        assert_eq!(value("calls"), "(10 15)");
        assert_eq!(value("others"), "(1001 8)");
    }

    /// A reference's `builtin` flag comes from the definition it resolves to,
    /// so a program's own global named like a prelude function (`car`) can be
    /// `set!` without "cannot mutate module-required identifier".
    #[test]
    fn set_of_a_programs_own_car_is_allowed() {
        let mut engine = Engine::new();
        engine
            .compile_and_run_raw_program(
                "(define (car x) 1)
                 (set! car (lambda (x) 2))
                 (define result (car 0))",
            )
            .unwrap();
        assert_eq!(engine.extract_value("result").unwrap().to_string(), "2");

        // The real builtins are still protected.
        let mut engine = Engine::new();
        assert!(engine.compile_and_run_raw_program("(set! car 1)").is_err());
        let mut engine = Engine::new();
        assert!(engine
            .compile_and_run_raw_program(
                "(require-builtin steel/time)
                 (set! current-milliseconds 1)"
            )
            .is_err());
    }

    /// `(or (null? x) y)` may drop its binding only when `null?` is the
    /// builtin (boolean valued); a program's own `null?` can return anything.
    /// Guards `SimplifyOrBindings` against matching `null?` by name.
    #[test]
    fn a_programs_own_predicate_is_not_assumed_boolean() {
        let mut engine = Engine::new();
        engine
            .compile_and_run_raw_program(
                "(define (null? x) 'yes)
                 (define (f x) (or (null? x) 'no))
                 (define result (f 1))",
            )
            .unwrap();
        assert_eq!(engine.extract_value("result").unwrap().to_string(), "yes");
    }

    /// The unoptimized expansion (used by the language server) must turn internal
    /// defines into lets before renaming, so a reference to a later sibling
    /// define is renamed too.
    #[test]
    fn unoptimized_expansion_renames_forward_references_between_internal_defines() {
        let src = "(define (outer)
                     (define (even? n) (if (= n 0) #t (odd? (- n 1))))
                     (define (odd? n) (if (= n 0) #f (even? (- n 1))))
                     (even? 10))";
        let mut engine = Engine::new();
        let expanded = engine
            .emit_expanded_ast_without_optimizations(src, None::<PathBuf>)
            .unwrap();
        let text = expanded.iter().map(|e| e.to_string()).collect::<String>();
        // Every call is to a renamed local, so no bare `odd?` is left over.
        assert!(!text.contains("(odd? "), "{text}");
    }
}

#[cfg(test)]
mod builtin_reference_tests {
    use crate::steel_vm::engine::Engine;
    use crate::SteelVal;

    fn run(engine: &mut Engine, src: &str) -> String {
        let values = engine.compile_and_run_raw_program(src.to_string()).unwrap();
        values.last().unwrap().to_string()
    }

    #[test]
    fn program_uses_its_own_car_defined_after_the_use() {
        let mut engine = Engine::new();
        let src = "(define (f x) (car x)) (define (car x) 'mine) (f (list 1 2))";
        assert_eq!(run(&mut engine, src), "mine");
    }

    #[test]
    fn earlier_program_redefines_car() {
        let mut engine = Engine::new();
        run(&mut engine, "(define (car x) 'mine)");
        assert_eq!(run(&mut engine, "(car (list 1 2))"), "mine");
        assert_eq!(
            run(&mut engine, "((lambda (x) (car x)) (list 1 2))"),
            "mine"
        );
    }

    #[test]
    fn host_registers_a_value_named_car() {
        let mut engine = Engine::new();
        engine.register_value("car", SteelVal::IntV(10));
        assert_eq!(run(&mut engine, "(+ car 1)"), "11");
    }

    #[test]
    fn quoted_builtin_names_stay_symbols() {
        let mut engine = Engine::new();
        assert_eq!(run(&mut engine, "(car '(car +))"), "car");
        assert_eq!(
            run(&mut engine, "(symbol->string (cadr '(car +)))"),
            "\"+\""
        );
    }

    #[test]
    fn required_module_provides_car() {
        let mut engine = Engine::new();
        engine.register_steel_module(
            "test/car-module".to_string(),
            "(provide car) (define (car x) 'module-car)".to_string(),
        );
        let src = r#"(require "test/car-module") (car (list 1 2))"#;
        assert_eq!(run(&mut engine, src), "module-car");
    }

    #[test]
    fn builtins_in_a_program_become_primitive_references() {
        let mut engine = Engine::new();
        let expanded = engine
            .emit_fully_expanded_ast_to_string("(define (f x) (+ (car x) 1))", None)
            .unwrap();
        assert!(expanded.contains("#%prim.car"), "{expanded}");
        assert!(expanded.contains("#%prim.+"), "{expanded}");
    }
}

#[cfg(test)]
mod host_function_call_tests {
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterFn;

    fn engine() -> Engine {
        #[cfg(feature = "jit2")]
        crate::steel_vm::vm::jit::settle_background_compiles(
            crate::steel_vm::vm::jit::jit_background_enabled(),
        );
        let mut engine = Engine::new();
        engine.register_fn("host-inc", |x: isize| x + 1);
        engine
            .compile_and_run_raw_program(
                "(define (call-inc x) (+ 0 (host-inc x)))
                 (define (tail-inc x) (host-inc x))
                 (define (warm n acc)
                   (if (= n 0) acc (warm (- n 1) (+ acc (call-inc n) (tail-inc n)))))
                 (warm 1000 0)",
            )
            .unwrap();
        engine
    }

    fn run(engine: &mut Engine, src: &str) -> String {
        let values = engine.compile_and_run_raw_program(src.to_string()).unwrap();
        values.last().unwrap().to_string()
    }

    #[test]
    fn compiled_callers_call_the_host_function() {
        let mut engine = engine();
        assert_eq!(run(&mut engine, "(warm 1000 0)"), "1003000");
    }

    #[test]
    fn compiled_callers_see_a_host_function_set_later() {
        let mut engine = engine();
        run(&mut engine, "(set! host-inc (lambda (x) (* x 10)))");
        assert_eq!(
            run(&mut engine, "(list (call-inc 5) (tail-inc 5))"),
            "(50 50)"
        );
        run(&mut engine, "(set! host-inc car)");
        assert_eq!(
            run(
                &mut engine,
                "(list (call-inc (list 7)) (tail-inc (list 8)))"
            ),
            "(7 8)"
        );
    }

    #[test]
    fn host_function_errors_reach_the_handler() {
        let mut engine = engine();
        assert_eq!(
            run(
                &mut engine,
                "(list (with-handler (lambda (e) 'caught) (call-inc \"a\"))
                       (with-handler (lambda (e) 'caught) (tail-inc \"a\"))
                       (call-inc 1))"
            ),
            "(caught caught 2)"
        );
        assert!(engine
            .compile_and_run_raw_program("(tail-inc \"a\")")
            .is_err());
    }
}

#[cfg(test)]
mod typed_host_call_tests {
    //! Host functions called from module code, where the globals are closed,
    //! so the jit types results and arguments from their signatures.
    use crate::steel_vm::builtin::BuiltInModule;
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterFn;

    fn engine() -> Engine {
        #[cfg(feature = "jit2")]
        crate::steel_vm::vm::jit::settle_background_compiles(
            crate::steel_vm::vm::jit::jit_background_enabled(),
        );
        let mut engine = Engine::new();
        let mut module = BuiltInModule::new("host/typed");
        module.register_fn("host-inc", |x: isize| x + 1);
        module.register_fn("host-scale", |x: f64, k: isize| x * k as f64);
        module.register_fn("host-len", |s: String| s.len() as isize);
        module.register_fn("host-halve", |x: isize| -> Result<isize, String> {
            if x % 2 == 0 {
                Ok(x / 2)
            } else {
                Err("odd".to_string())
            }
        });
        engine.register_module(module);
        engine.register_steel_module(
            "typed-user".to_string(),
            "(require-builtin host/typed)
             (provide sum-incs narrowed floats halves lengths)
             (define (sum-incs n acc)
               (if (= n 0) acc (sum-incs (- n 1) (+ acc (host-inc n)))))
             ;; A normal return means x converted to an int.
             (define (narrowed x) (host-inc x) (+ x x))
             (define (floats n acc)
               (if (= n 0) acc (floats (- n 1) (+ acc (host-scale 0.5 n)))))
             (define (halves n acc)
               (if (= n 0) acc (halves (- n 2) (+ acc (host-halve n)))))
             (define (lengths xs acc)
               (if (null? xs) acc (lengths (cdr xs) (+ acc (host-len (car xs))))))"
                .to_string(),
        );
        engine
            .compile_and_run_raw_program(
                "(require \"typed-user\")
                 (define (warm k)
                   (when (> k 0)
                     (sum-incs 100 0) (narrowed k) (floats 100 0.0) (halves 100 0)
                     (lengths (list \"a\" 'bc \"def\") 0)
                     (warm (- k 1))))
                 (warm 200)",
            )
            .unwrap();
        engine
    }

    fn run(engine: &mut Engine, src: &str) -> String {
        let values = engine.compile_and_run_raw_program(src.to_string()).unwrap();
        values.last().unwrap().to_string()
    }

    #[test]
    fn typed_results_and_arguments() {
        let mut engine = engine();
        assert_eq!(
            run(
                &mut engine,
                "(list (sum-incs 1000 0) (narrowed 21) (floats 4 0.0) (halves 10 0)
                       (lengths (list \"ab\" 'cde) 0))"
            ),
            "(501500 42 5.0 15 5)"
        );
    }

    #[test]
    fn failed_host_calls_narrow_nothing() {
        let mut engine = engine();
        assert_eq!(
            run(
                &mut engine,
                "(list (with-handler (lambda (e) 'caught) (narrowed \"a\"))
                       (with-handler (lambda (e) 'caught) (narrowed 1.5))
                       (with-handler (lambda (e) 'caught) (halves 9 0))
                       (with-handler (lambda (e) 'caught) (lengths (list 1) 0))
                       (narrowed 4))"
            ),
            "(caught caught caught caught 8)"
        );
    }
}

#[cfg(test)]
mod prefixed_builtin_tests {
    use crate::steel_vm::engine::Engine;

    fn run(engine: &mut Engine, src: &str) -> String {
        let values = engine.compile_and_run_raw_program(src.to_string()).unwrap();
        values.last().unwrap().to_string()
    }

    #[test]
    fn prefixed_prelude_name_in_a_later_compile() {
        // `string-length` is steel/lists' `length` here, also in later inputs.
        let mut engine = Engine::new();
        run(&mut engine, "(require-builtin steel/lists as string-)");
        assert_eq!(run(&mut engine, "(string-length (list 1 2 3))"), "3");
    }

    #[test]
    fn modules_that_only_provide_prefixed_builtins() {
        let mut engine = Engine::new();
        engine.register_steel_module(
            "reexport-strings".to_string(),
            "(require-builtin steel/strings as s.) (provide s.string-upcase)".to_string(),
        );
        engine.register_steel_module(
            "reexport-lists".to_string(),
            "(require-builtin steel/lists as string-) (provide string-length)".to_string(),
        );
        assert_eq!(
            run(
                &mut engine,
                "(require \"reexport-strings\")
                 (require \"reexport-lists\")
                 (list (s.string-upcase \"hi\") (string-length (list 1 2 3)))"
            ),
            "(\"HI\" 3)"
        );
    }
}

#[cfg(test)]
mod exported_macro_quote_tests {
    use crate::steel_vm::engine::Engine;

    #[test]
    fn quoted_names_in_exported_macros() {
        let mut engine = Engine::new();
        engine.register_steel_module(
            "quoting-macros".to_string(),
            "(provide tick long-quote quoted-list quasi nested)
             (define (helper x) x)
             (define counter 10)
             (define-syntax tick (syntax-rules () [(_) 'helper]))
             (define-syntax long-quote (syntax-rules () [(_) (quote helper)]))
             (define-syntax quoted-list (syntax-rules () [(_) (quote (helper counter))]))
             (define-syntax quasi
               (syntax-rules () [(_) `(helper ,counter ,@(list (helper 3)))]))
             (define-syntax nested
               (syntax-rules () [(_) `(helper `(counter ,(counter ,counter)))]))"
                .to_string(),
        );
        let values = engine
            .compile_and_run_raw_program(
                "(require \"quoting-macros\")
                 (list (tick) (long-quote) (quoted-list) (quasi) (nested))",
            )
            .unwrap();
        assert_eq!(
            values.last().unwrap().to_string(),
            "(helper helper (helper counter) (helper 10 3) \
             (helper (quasiquote (counter (unquote (counter 10))))))"
        );
    }
}

#[cfg(test)]
mod set_after_compile_tests {
    //! Each `run` is a separate compile, like input at the repl. Callers are
    //! compiled before a later input `set!`s the global they call. A callee is
    //! defined in its own input, since the compiler inlines a function into
    //! callers compiled with it.
    use crate::steel_vm::engine::Engine;

    fn engine() -> Engine {
        #[cfg(feature = "jit2")]
        crate::steel_vm::vm::jit::settle_background_compiles(
            crate::steel_vm::vm::jit::jit_background_enabled(),
        );
        Engine::new()
    }

    fn run(engine: &mut Engine, src: &str) -> String {
        let values = engine.compile_and_run_raw_program(src.to_string()).unwrap();
        values.last().unwrap().to_string()
    }

    const WARM: &str = "(define (warm n) (if (= n 0) 0 (begin (g 1) (warm (- n 1)))))
                        (warm 2000)";

    #[test]
    fn tail_call_to_a_global_set_later() {
        let mut engine = engine();
        run(&mut engine, "(define (f x) (+ x 1))");
        run(&mut engine, "(define (g x) (f x))");
        run(&mut engine, WARM);
        run(&mut engine, "(set! f (lambda (x) 0))");
        assert_eq!(run(&mut engine, "(g 5)"), "0");
    }

    #[test]
    fn call_to_a_global_set_later() {
        let mut engine = engine();
        run(&mut engine, "(define (f x) (+ x 1))");
        run(&mut engine, "(define (g x) (+ 100 (f x)))");
        run(&mut engine, WARM);
        run(&mut engine, "(set! f (lambda (x) 0))");
        assert_eq!(run(&mut engine, "(g 5)"), "100");
    }

    #[test]
    fn call_to_a_variadic_global_set_later() {
        let mut engine = engine();
        run(&mut engine, "(define (f x . rest) (+ x 1))");
        run(&mut engine, "(define (g x) (+ 100 (f x)))");
        run(&mut engine, WARM);
        run(&mut engine, "(set! f (lambda (x . rest) 0))");
        assert_eq!(run(&mut engine, "(g 5)"), "100");
    }

    #[test]
    fn struct_function_set_later() {
        let mut engine = engine();
        run(&mut engine, "(struct point (x y)) (define p (point 1 2))");
        run(
            &mut engine,
            "(define (g x) (+ x (point-x p))) (define (h x) (point-x p))",
        );
        run(&mut engine, WARM);
        run(&mut engine, "(h 1) (h 1)");
        run(&mut engine, "(set! point-x (lambda (p) 40))");
        assert_eq!(run(&mut engine, "(list (g 2) (h 2))"), "(42 40)");
    }

    // Past the speculation limit the caller is compiled without the guard.
    #[test]
    fn global_set_many_times() {
        let mut engine = engine();
        run(&mut engine, "(define (f x) x)");
        run(&mut engine, "(define (g x) (+ 0 (f x)))");
        run(&mut engine, WARM);
        let mut expected = Vec::new();
        let mut src = String::from("(define results '())");
        for i in 0..150 {
            src.push_str(&format!(
                " (set! f (lambda (x) {i})) (warm 3) (set! results (cons (g 1) results))"
            ));
            expected.push(i.to_string());
        }
        run(&mut engine, &src);
        expected.reverse();
        assert_eq!(
            run(&mut engine, "results"),
            format!("({})", expected.join(" "))
        );
    }

    // A module's own calls go to its own globals, which nothing outside it can
    // `set!`. The program's `f` is a separate global.
    #[test]
    fn module_calls_are_not_changed_by_a_programs_set() {
        let mut engine = engine();
        engine.register_steel_module(
            "test/set-module".to_string(),
            "(provide f g) (define (f x) (+ x 1)) (define (g x) (f x))".to_string(),
        );
        run(&mut engine, r#"(require "test/set-module")"#);
        run(&mut engine, WARM);
        run(&mut engine, "(set! f (lambda (x) 0))");
        assert_eq!(run(&mut engine, "(list (f 1) (g 1))"), "(0 2)");
    }
}
