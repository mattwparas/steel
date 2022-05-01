#[cfg(test)]
mod call_cc_tests {
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterAsyncFn;

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
        vm.register_async_fn("test", test_function);

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

        vm.run(contents).unwrap();

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

        vm.run(
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

        let baz: Result<String, String> = vm.extract("baz").unwrap();
        assert_eq!("bananas".to_string(), baz.unwrap());

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
            .run(
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

// #[cfg(test)]
// mod stream_tests {
//     use std::cell::RefCell;
//     use std::rc::Rc;

//     use crate::parser::span::Span;
//     use crate::steel_vm::evaluation_progress::EvaluationProgress;
//     use crate::steel_vm::lazy_stream::LazyStreamIter;
//     use crate::steel_vm::options::ApplyContract;
//     use crate::steel_vm::options::UseCallback;
//     use crate::values::lazy_stream::LazyStream;
//     use crate::{compiler::constants::ConstantMap, env::Env};

//     // #[test]
//     // fn test_empty_stream_creates_no_iter() {
//     //     let constants = ConstantMap::new();
//     //     let cur_inst_span = Span::new(0, 0);
//     //     let callback = EvaluationProgress::new();
//     //     let mut global_env = Env::root();
//     //     let mut mut_ref = &mut global_env;

//     //     let lazy_iter = LazyStreamIter::new(
//     //         LazyStream::new_empty_stream(),
//     //         &constants,
//     //         &cur_inst_span,
//     //         &callback,
//     //         Rc::new(RefCell::new(&mut mut_ref)),
//     //         UseCallback,
//     //         ApplyContract,
//     //     );

//     //     assert!(lazy_iter.into_iter().next().is_none());
//     // }
// }
