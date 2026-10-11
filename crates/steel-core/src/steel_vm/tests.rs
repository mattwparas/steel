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
fn gc_collect(engine: &mut crate::steel_vm::engine::Engine) {
    engine.run("(#%gc-collect)").unwrap();
}

// TODO: We should be able to simplify this a lot, but this is an annoying way to
// make sure that we don't block on blocking FFI calls.
//
// in memory blocking calls are also victim to this, but less so, because you can
// pass immutable references to them, whereas all dylib calls are mutably referenced
// by default.
#[cfg(all(test, feature = "sync", feature = "dylibs"))]
mod ffi_borrowed_custom_value_tests {
    use crate::rvals::SteelVal;
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::ffi::{FFIModule, RegisterFFIFn};
    use crate::steel_vm::tests::gc_collect;
    use abi_stable::std_types::RBox;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::mpsc::channel;
    use std::time::Duration;

    static STARTED: AtomicBool = AtomicBool::new(false);
    static RELEASE: AtomicBool = AtomicBool::new(false);

    struct Blocker;

    impl crate::rvals::Custom for Blocker {}

    impl Blocker {
        fn wait(&mut self) {
            STARTED.store(true, Ordering::SeqCst);
            while !RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
        }
    }

    #[test]
    fn collection_finishes_while_dylib_call_holds_its_argument() {
        let (done_tx, done_rx) = channel();

        std::thread::spawn(move || {
            let mut module = FFIModule::new("test/blocker");
            module
                .register_fn("make-blocker", || Blocker)
                .register_fn("blocker-wait!", Blocker::wait);

            let mut engine = Engine::new();
            engine.register_external_module(RBox::new(module)).unwrap();
            engine
                .run("(require-builtin test/blocker) (define blocker (make-blocker))")
                .unwrap();

            let blocker = engine.extract_value("blocker").unwrap();
            let SteelVal::BoxedFunction(wait) = engine.extract_value("blocker-wait!").unwrap()
            else {
                panic!("expected a boxed function");
            };

            let caller = std::thread::spawn(move || wait.func()(&[blocker]).unwrap());
            while !STARTED.load(Ordering::SeqCst) {
                std::thread::yield_now();
            }

            gc_collect(&mut engine);
            done_tx.send(()).unwrap();

            RELEASE.store(true, Ordering::SeqCst);
            caller.join().unwrap();
        });

        let finished = done_rx.recv_timeout(Duration::from_secs(20));
        RELEASE.store(true, Ordering::SeqCst);
        finished.expect("collection blocked on a value borrowed by a dylib call");
    }
}

#[cfg(all(test, feature = "sync", feature = "dylibs"))]
mod ffi_shared_receiver_tests {
    use crate::gc::shared::ShareableMut;
    use crate::rvals::{AsRefMutSteelVal, SteelVal};
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::ffi::{
        AsRefFFIVal, FFIArg, FFIBoxedDynFunction, FFIModule, FFIValue, FFIWrappedModule,
        HostRuntimeFunction, IntoFFIVal, RFn_TO, RegisterFFIFn, SHARED_RECEIVER,
    };
    use crate::steel_vm::tests::gc_collect;
    use abi_stable::sabi_trait::TD_CanDowncast;
    use abi_stable::std_types::{RArc, RBox, RBoxError, RResult, RSliceMut, RString};
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
    use std::sync::mpsc::{channel, RecvTimeoutError};
    use std::thread::JoinHandle;
    use std::time::Duration;

    struct Counter(AtomicUsize);

    impl crate::rvals::Custom for Counter {}

    impl Counter {
        fn get(&self) -> usize {
            self.0.load(Ordering::SeqCst)
        }

        fn add(&self, n: usize) -> usize {
            self.0.fetch_add(n, Ordering::SeqCst) + n
        }

        fn reset(&mut self) {
            *self.0.get_mut() = 0;
        }
    }

    fn counter_getter() -> FFIValue {
        let function = |args: RSliceMut<'static, FFIArg<'static>>| -> RResult<FFIValue, RBoxError> {
            let mut args = args.into_iter();
            match <Counter as AsRefFFIVal>::as_ref(args.next().unwrap()) {
                RResult::ROk(counter) => counter.get().into_ffi_val(),
                RResult::RErr(e) => RResult::RErr(e),
            }
        };

        FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
            name: RString::from("counter-get"),
            arity: SHARED_RECEIVER,
            function: RFn_TO::from_ptr(RArc::new(function), TD_CanDowncast),
        }))
    }

    static EXCLUSIVE_STARTED: AtomicBool = AtomicBool::new(false);
    static EXCLUSIVE_RELEASE: AtomicBool = AtomicBool::new(false);

    #[derive(Default)]
    struct Blocker {
        started: AtomicBool,
        release: AtomicBool,
    }

    impl crate::rvals::Custom for Blocker {}

    impl Blocker {
        fn wait(&self) {
            self.started.store(true, Ordering::SeqCst);
            while !self.release.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
        }

        fn started(&self) -> bool {
            self.started.load(Ordering::SeqCst)
        }

        fn release(&self) {
            self.release.store(true, Ordering::SeqCst);
        }

        fn ping(&self) -> bool {
            true
        }

        fn poke(&mut self) -> bool {
            true
        }

        fn wait_exclusive(&mut self) {
            EXCLUSIVE_STARTED.store(true, Ordering::SeqCst);
            while !EXCLUSIVE_RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
        }

        fn call_with(&self, function: HostRuntimeFunction) -> bool {
            function.call(RSliceMut::from_mut_slice(&mut [])).is_ok()
        }
    }

    struct HostOnly;

    impl crate::rvals::Custom for HostOnly {}

    fn module() -> RBox<FFIModule> {
        let mut module = FFIModule::new("test/shared");
        module
            .register_fn("make-counter", || Counter(AtomicUsize::new(0)))
            .register_fn("counter-get", Counter::get)
            .register_fn("counter-add!", Counter::add)
            .register_fn("counter-reset!", Counter::reset)
            .register_fn("counter-getter", counter_getter)
            .register_fn("make-blocker", Blocker::default)
            .register_fn("blocker-wait", Blocker::wait)
            .register_fn("blocker-started?", Blocker::started)
            .register_fn("blocker-release!", Blocker::release)
            .register_fn("blocker-ping", Blocker::ping)
            .register_fn("blocker-poke!", Blocker::poke)
            .register_fn("blocker-wait-exclusive!", Blocker::wait_exclusive)
            .register_fn("blocker-call-with", Blocker::call_with);

        RBox::new(module)
    }

    fn engine() -> Engine {
        let mut engine = Engine::new();
        engine.register_external_module(module()).unwrap();
        engine.run("(require-builtin test/shared)").unwrap();
        engine
    }

    fn engine_with_old_module() -> Engine {
        let module = FFIWrappedModule::new(module(), Some(usize::MAX))
            .unwrap()
            .build();

        let mut engine = Engine::new();
        engine.register_module(module);
        engine.run("(require-builtin test/shared)").unwrap();
        engine
    }

    fn boxed(engine: &mut Engine, name: &str) -> SteelVal {
        engine.extract_value(name).unwrap()
    }

    fn call(function: &SteelVal, args: &[SteelVal]) -> crate::rvals::Result<SteelVal> {
        let SteelVal::BoxedFunction(function) = function else {
            panic!("expected a boxed function");
        };
        function.func()(args)
    }

    fn call_on(engine: &mut Engine, name: &str, value: &SteelVal) -> bool {
        call(&boxed(engine, name), &[value.clone()]).is_ok()
    }

    fn spawn_call_on(engine: &mut Engine, name: &str, value: &SteelVal) -> JoinHandle<bool> {
        let function = boxed(engine, name);
        let value = value.clone();
        std::thread::spawn(move || call(&function, &[value]).is_ok())
    }

    fn eval(engine: &mut Engine, expr: &str) -> SteelVal {
        engine.run(expr.to_string()).unwrap().pop().unwrap()
    }

    fn with_timeout<T: Send + 'static>(test: impl FnOnce() -> T + Send + 'static) -> T {
        let (done_tx, done_rx) = channel();
        std::thread::spawn(move || done_tx.send(test()).unwrap());
        match done_rx.recv_timeout(Duration::from_secs(20)) {
            Ok(result) => result,
            Err(RecvTimeoutError::Timeout) => {
                panic!("blocked on a value borrowed over the ffi boundary")
            }
            Err(RecvTimeoutError::Disconnected) => panic!("the test thread panicked"),
        }
    }

    fn wait_until(condition: impl Fn() -> bool) {
        while !condition() {
            // Yielding let this test be more predictable than sleeping for a ms
            // for some reason
            std::thread::yield_now();
        }
    }

    fn count_errors(
        threads: usize,
        iterations: usize,
        f: impl Fn(usize) -> crate::rvals::Result<SteelVal> + Sync,
    ) -> usize {
        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..threads)
                .map(|index| {
                    let f = &f;
                    scope.spawn(move || (0..iterations).filter(|_| f(index).is_err()).count())
                })
                .collect();

            handles.into_iter().map(|x| x.join().unwrap()).sum()
        })
    }

    fn hammer(mut engine: Engine, get: SteelVal) {
        let counter = eval(&mut engine, "(make-counter)");
        let add = boxed(&mut engine, "counter-add!");

        let errors = count_errors(4, 50_000, |_| {
            call(&add, &[counter.clone(), SteelVal::IntV(1)])?;
            call(&get, &[counter.clone()])
        });

        assert_eq!(errors, 0);
        assert_eq!(call(&get, &[counter]).unwrap(), SteelVal::IntV(200_000));
    }

    // Defined as a global so the gc visits it.
    fn make_blocker(engine: &mut Engine) -> SteelVal {
        engine.run("(define blocker (make-blocker))").unwrap();
        boxed(engine, "blocker")
    }

    fn hold_shared(engine: &mut Engine, blocker: &SteelVal) -> JoinHandle<bool> {
        let holder = spawn_call_on(engine, "blocker-wait", blocker);
        let started = boxed(engine, "blocker-started?");
        wait_until(|| call(&started, &[blocker.clone()]).unwrap() == SteelVal::BoolV(true));
        holder
    }

    fn wait_for_queued_writer(blocker: &SteelVal) {
        let SteelVal::Custom(lock) = blocker else {
            panic!("expected a custom value");
        };
        wait_until(|| lock.try_read().is_err());
    }

    #[test]
    fn threads_share_a_value_through_ref_methods() {
        let mut engine = engine();
        let get = boxed(&mut engine, "counter-get");
        hammer(engine, get);
    }

    #[test]
    fn threads_share_a_value_through_the_old_module_loader() {
        let mut engine = engine_with_old_module();
        let get = boxed(&mut engine, "counter-get");
        hammer(engine, get);
    }

    #[test]
    fn threads_share_a_value_through_a_returned_function() {
        let mut engine = engine();
        let get = eval(&mut engine, "(counter-getter)");
        hammer(engine, get);
    }

    #[test]
    fn the_flag_does_not_show_in_the_arity() {
        let mut engine = engine();
        let get = boxed(&mut engine, "counter-get");
        let returned = eval(&mut engine, "(counter-getter)");

        for function in [get, returned] {
            let SteelVal::BoxedFunction(function) = function else {
                panic!("expected a boxed function");
            };
            assert_eq!(function.get_arity(), Some(0));
        }

        let FFIValue::BoxedFunction(function) = counter_getter() else {
            unreachable!()
        };
        assert_eq!(format!("{:?}", function), "#<counter-get:0>");
    }

    #[test]
    fn a_by_value_argument_still_borrows_exclusively() {
        let mut engine = engine();
        engine.run("(define c (make-counter))").unwrap();

        let error = engine.run("(counter-add! c c)").unwrap_err().to_string();
        assert!(error.contains("borrowed"), "{}", error);

        engine.run("(counter-reset! c)").unwrap();
        assert_eq!(eval(&mut engine, "(counter-get c)"), SteelVal::IntV(0));
    }

    #[test]
    fn readers_and_writers_on_one_value_never_fail() {
        let mut engine = engine();
        let counter = eval(&mut engine, "(make-counter)");
        let get = boxed(&mut engine, "counter-get");
        let add = boxed(&mut engine, "counter-add!");
        let reset = boxed(&mut engine, "counter-reset!");

        let errors = count_errors(5, 20_000, |index| match index {
            0 => call(&reset, &[counter.clone()]),
            1 => call(&add, &[counter.clone(), SteelVal::IntV(1)]),
            _ => call(&get, &[counter.clone()]),
        });

        assert_eq!(errors, 0);
    }

    #[test]
    fn an_exclusive_borrow_waits_for_shared_ones() {
        let result = with_timeout(|| {
            let mut engine = engine();
            let blocker = make_blocker(&mut engine);
            let reader = hold_shared(&mut engine, &blocker);
            let writer = spawn_call_on(&mut engine, "blocker-poke!", &blocker);
            wait_for_queued_writer(&blocker);

            let shared = call_on(&mut engine, "blocker-ping", &blocker);
            gc_collect(&mut engine);
            let early = writer.is_finished();

            call_on(&mut engine, "blocker-release!", &blocker);
            reader.join().unwrap();

            (shared, early, writer.join().unwrap())
        });

        assert_eq!(result, (true, false, true));
    }

    #[test]
    fn a_shared_borrow_waits_for_an_exclusive_one() {
        let result = with_timeout(|| {
            let mut engine = engine();
            let blocker = make_blocker(&mut engine);
            let writer = spawn_call_on(&mut engine, "blocker-wait-exclusive!", &blocker);
            wait_until(|| EXCLUSIVE_STARTED.load(Ordering::SeqCst));
            let reader = spawn_call_on(&mut engine, "blocker-ping", &blocker);

            std::thread::sleep(Duration::from_millis(100));
            gc_collect(&mut engine);
            let early = reader.is_finished();

            EXCLUSIVE_RELEASE.store(true, Ordering::SeqCst);
            writer.join().unwrap();

            (early, reader.join().unwrap())
        });

        assert_eq!(result, (false, true));
    }

    #[test]
    fn a_waiting_writer_does_not_block_shared_borrows() {
        let result = with_timeout(|| {
            let mut engine = engine();
            let blocker = make_blocker(&mut engine);
            let reader = hold_shared(&mut engine, &blocker);
            let writer = {
                let blocker = blocker.clone();
                std::thread::spawn(move || {
                    <HostOnly as AsRefMutSteelVal>::as_mut_ref(&blocker).is_ok()
                })
            };
            wait_for_queued_writer(&blocker);

            let shared = call_on(&mut engine, "blocker-ping", &blocker);
            gc_collect(&mut engine);

            call_on(&mut engine, "blocker-release!", &blocker);
            reader.join().unwrap();

            (shared, writer.join().unwrap())
        });

        assert_eq!(result, (true, false));
    }

    #[test]
    fn a_borrowed_value_can_reenter_through_the_host() {
        let result = with_timeout(|| {
            let mut engine = engine();
            eval(
                &mut engine,
                "(require-builtin steel/ffi)
                 (define b (make-blocker))
                 (list (blocker-call-with b (function->ffi-function (lambda () (blocker-ping b))))
                       (blocker-call-with b (function->ffi-function (lambda () (blocker-poke! b))))
                       (blocker-poke! b))",
            )
            .to_string()
        });

        assert_eq!(result, "(#true #false #true)");
    }
}
