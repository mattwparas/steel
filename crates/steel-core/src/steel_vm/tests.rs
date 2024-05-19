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

        vm.compile_and_run_raw_program(contents).unwrap();

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
