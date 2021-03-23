#[cfg(test)]
mod call_cc_tests {
    use crate::steel_vm::engine::Engine;
    use crate::steel_vm::register_fn::RegisterAsyncFn;
    use crate::steel_vm::test_util::assert_script;

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

    #[test]
    fn test_generator() {
        let script = r#"
            (define (for-each func lst)
                (if (null? lst) 
                    void
                    (begin
                        (func (car lst))
                        (when (null? lst) (return! void))
                        (for-each func (cdr lst)))))

            ; (for-each (lambda (x) (displayln x)) '(1 2 3 4 5))

            ;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
            (define (generate-one-element-at-a-time lst)
            ;; Both internal functions are closures over lst

            ;; Internal variable/Function which passes the current element in a list
            ;; to its return argument (which is a continuation), or passes an end-of-list marker 
            ;; if no more elements are left. On each step the function name is 
            ;; rebound to a continuation which points back into the function body,
            ;; while return is rebound to whatever continuation the caller specifies.
            (define (control-state return)
                (for-each 
                (lambda (element)
                        (set! return (call/cc
                                        (lambda (resume-here)
                                            ;; Grab the current continuation
                                        (set! control-state resume-here)
                                        (return element))))) ;; (return element) evaluates to next return
                lst)
                (return 'you-fell-off-the-end))
            
            ;; (-> X u 'you-fell-off-the-end)
            ;; This is the actual generator, producing one item from a-list at a time.
            (define (generator)
                (call/cc control-state))

            ;; Return the generator 
            generator)

            (define generate-digit
            (generate-one-element-at-a-time '(0 1 2)))

            (assert! (equal? 0 (generate-digit))) ;; 0
            (assert! (equal? 1 (generate-digit))) ;; 1
            (assert! (equal? 2 (generate-digit))) ;; 2
            (assert! (equal? 'you-fell-off-the-end (generate-digit))) ;; you-fell-off-the-end
            "#;
        assert_script(script);
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

        vm.run(
            r#"
        (define foo (external-function 10 25))
        (define bar (option-function "applesauce"))
        (define baz (result-function "bananas"))
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

#[cfg(test)]
mod stream_tests {
    use crate::compiler::constants::ConstantMap;
    use crate::parser::span::Span;
    use crate::steel_vm::evaluation_progress::EvaluationProgress;
    use crate::steel_vm::lazy_stream::LazyStreamIter;
    use crate::steel_vm::test_util::assert_script;
    use crate::values::lazy_stream::LazyStream;

    #[test]
    fn test_empty_stream_creates_no_iter() {
        let constants = ConstantMap::new();
        let cur_inst_span = Span::new(0, 0);
        let repl = true;
        let callback = EvaluationProgress::new();
        let lazy_iter = LazyStreamIter::new(
            LazyStream::new_empty_stream(),
            &constants,
            &cur_inst_span,
            repl,
            &callback,
        );

        assert!(lazy_iter.into_iter().next().is_none());
    }

    #[test]
    fn simple_stream() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (define (stream-section n stream)
            (cond ((= n 0) '())
                  (else
                   (cons
                    (stream-car stream)
                    (stream-section
                     (- n 1)
                     (stream-cdr stream))))))

        (assert! (equal? (list 0 1 2 3 4) (stream-section 5 (integers 0))))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_stream_with_map() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (define (stream-section n stream)
            (cond ((= n 0) '())
                  (else
                   (cons
                    (stream-car stream)
                    (stream-section
                     (- n 1)
                     (stream-cdr stream))))))

        (define (map-stream func s)
            (cond
                [(stream-empty? s) s]
                [else
                    (stream-cons (func (stream-car s))
                                    (lambda ()
                                    (map-stream func (stream-cdr s))))]))

        (assert! 
            (equal? (list 10 10 10 10 10)
                    (stream-section 5 (map-stream (lambda (x) 10) (integers 0)))))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_stream_with_transducer() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (assert! 
            (equal? (list 0 1 2 3 4)
                    (execute (taking 5) (integers 0))))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_stream_with_mapping() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (assert! 
            (equal? (list 1 2 3 4 5)
                    (execute (compose 
                                (mapping (lambda (x) (+ x 1)))
                                (taking 5)) 
                                (integers 0))))
        "#;
        assert_script(script);
    }

    #[test]
    fn simple_stream_with_transduce_operation() {
        let script = r#"
        (define (stream-cdr stream)
            ((stream-cdr' stream)))

        (define (integers n)
            (stream-cons n (lambda () (integers (+ 1 n)))))

        (assert! 
            (equal? 10
                    (transduce (taking 5) + 0 (integers 0))))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod struct_integration_tests {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn test_trie_sort() {
        let script = r#"
        (struct trie (char children end-word? word-up-to))

        ;; Rename functions for the sake of compatibility
        (define empty (list))
        (define empty-trie (trie void empty #f empty))

        ;; Throw in a mediocre flatten definition
        (define (flatten lst)
        (cond ((null? lst) empty)
                ((list? lst)
                (append (flatten (car lst)) (flatten (cdr lst))))
                (else (list lst))))

        ;; contract: (listof char?) (listof tries?) integer? -> (listof trie?)
        (define (create-children char-list lst prefix-chars)
        (cond [(= (length char-list) 1)
                (handle-last-letter char-list lst prefix-chars)]
                [else ;; you are in the middle of the word
                (handle-intern-letter char-list lst prefix-chars)]))

        ;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
        (define (handle-last-letter char-list lst prefix-chars)
        (define char (first char-list))
        ; (define next-prefix (append prefix-chars (list char)))
        (define next-prefix (push-back prefix-chars char))
        (cond [(empty? lst) ;; children are empty, return list of empty children
                (list (trie char empty #t next-prefix))]
                [(< char (trie-char (first lst))) ;; less than, put it to the left
                (cons (trie char empty #t next-prefix) lst)]
                [(= char (trie-char (first lst))) ;; equal, step down a level
                (cons (trie char (trie-children (first lst)) #t next-prefix) (rest lst))]
                [else ;; move to the right
                (cons (first lst)
                    (create-children char-list (rest lst) prefix-chars))]))

        ;; contract: (listof char?) (listof trie?) integer? -> (listof trie?)
        (define (handle-intern-letter char-list lst prefix-chars)
        (define char (first char-list))
        ; (define next-prefix (append prefix-chars (list char)))
        (define next-prefix (push-back prefix-chars char))
        (cond [(empty? lst) ;; no children, pop off front and step down
                (list (trie char (create-children
                                (rest char-list) empty next-prefix) #f next-prefix))]
                [(< char (trie-char (first lst))) ;; place where it is, pop off front and go
                (cons (trie char (create-children
                                (rest char-list) empty next-prefix) #f next-prefix) lst)]
                [(= char (trie-char (first lst))) ;; equal, step down
                (cons (trie char (create-children (rest char-list) (trie-children (first lst)) next-prefix)
                            (trie-end-word? (first lst))
                            (trie-word-up-to (first lst)))
                    (rest lst))]
                [else ; move to the right
                (cons (first lst)
                    (create-children char-list (rest lst) prefix-chars))]))

        ;; contract: trie? string? integer? -> trie?
        (define (insert root-trie word)
        (define char-list (string->list word))
        (trie
        (trie-char root-trie)
        (create-children char-list (trie-children root-trie) empty)
        (trie-end-word? root-trie)
        (trie-word-up-to root-trie)))

        ; contract: trie? trie? -> boolean?
        (define (trie<? trie-node1 trie-node2)
        (< (trie-char trie-node1) (trie-char trie-node2)))


        ;; contract: trie? (listof string?) -> trie?
        (define (build-trie-from-list-of-words trie list-of-words)
            (cond
                [(= (length list-of-words) 1)
                    (insert trie (first list-of-words))]
                [else
                    (build-trie-from-list-of-words
                    (insert trie (first list-of-words))
                                 (rest list-of-words))]))

        ;; ------------------ SORTING ---------------------- ;;

        (define (trie-sort list-of-words)
        (define new-trie (build-trie-from-list-of-words empty-trie list-of-words))
        (pre-order new-trie))

        ; THIS ONE WORKS (using con and flatten)
        ;; contract: trie? -> (listof string?)
        (define (pre-order trie-node)
            (if (trie-end-word? trie-node)
                (cons (list->string (trie-word-up-to trie-node))
                      (flatten (map pre-order (trie-children trie-node))))
                (flatten (map pre-order (trie-children trie-node)))))

        (define test-list
            (list
                "suppose"
                "believe"
                "changeable"
                "absent"
                "busy"
                "float"
                "debonair"
                "throat"
                "grey"
                "use"
                "measure"
                "van"
                "thirsty"
                "notify"
                "star"))

        (define expected-list
            (list
                "absent" "believe" "busy" "changeable" "debonair" "float" "grey" "measure" "notify" "star" "suppose" "thirsty" "throat" "use" "van"))
            
        (define trie1 (build-trie-from-list-of-words empty-trie test-list))
        (assert! (equal?
                        expected-list
                        (trie-sort test-list)))
        "#;

        assert_script(script);
    }
}

#[cfg(test)]
mod transducer_tests {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn generic_transducer() {
        let script = r#"
            (define x (mapping (fn (x) x))) ;; identity
            (define y (filtering even?)) ;; get only even ones
            (define z (taking 15)) ;; take the first 15 from the range
            (define xf (compose x y z))
            (assert! 
                (equal? 
                    (transduce xf + 0 (range 0 100)) ;; => 210
                    210))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_transducer_with_different_functions() {
        let script = r#"
            (define x (mapping (fn (x) x))) ;; identity
            (define y (filtering even?)) ;; get only even ones
            (define z (taking 15)) ;; take the first 15 from the range
            (define xf (compose x y z))
            (define reduce-func (lambda (accum next) (+ accum next)))
            (assert! 
                (equal? 
                    (transduce xf reduce-func 0 (range 0 100)) ;; => 210
                    210))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_transducer_with_different_functions_and_closures() {
        let script = r#"
            (define x (mapping (fn (x) x))) ;; identity
            (define y (filtering (fn (x) (even? x)))) ;; get only even ones
            (define z (taking 15)) ;; take the first 15 from the range
            (define xf (compose x y z))
            (define reduce-func (lambda (accum next) (+ accum next)))
            (assert! 
                (equal? 
                    (transduce xf reduce-func 0 (range 0 100)) ;; => 210
                    210))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_execution() {
        let script = r#"
        (define x (mapping (fn (x) x))) ;; identity
        (define y (filtering even?)) ;; get only even ones
        (define z (taking 15)) ;; take the first 15 from the range
        (define xf (compose x y z))
        (define result
            (execute xf (range 0 100)))
        
        (define expected '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28))
        (assert! (equal? result expected))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_execution_dropping() {
        let script = r#"
        (define x (mapping (fn (x) x))) ;; identity
        (define y (filtering even?)) ;; get only even ones
        (define z (dropping 15)) ;; drop the first 15 from the range
        (define xf (compose x y z))
        (define result
            (execute xf (range 0 40)))
        
        (define expected '(30 32 34 36 38))
        (assert! (equal? result expected))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_execution_output_different_type() {
        let script = r#"
        (define x (mapping (fn (x) x))) ;; identity
        (define y (filtering even?)) ;; get only even ones
        (define z (taking 15)) ;; take the first 15 from the range
        (define xf (compose x y z))
        (define result
            (execute xf (range 0 100) 'vector))
        
        (define expected (vector 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28))
        (assert! (equal? result expected))
        "#;
        assert_script(script);
    }

    #[test]
    fn transducer_over_streams() {
        let script = r#"
        (define (integers n)
                (stream-cons n (lambda () (integers (+ 1 n)))))
        (define result (execute (taking 15) (integers 0)))
        (define expected '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
        (assert! (equal? result expected))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod stack_tests {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn stack_test_with_contract() {
        let script = r#"
        ;; destruct works like so:
        ;; (destruct (a b c) value)
        ;;  ...
        ;; (define a (car value))
        ;; (define b (car (cdr value)))
        ;; (define c (car (cdr (cdr value))))
        (define-syntax destruct
        (syntax-rules ()
            [(destruct (var) ret-value)
            (define (datum->syntax var) (car ret-value))]
            [(destruct (var1 var2 ...) ret-value)
            (begin (define (datum->syntax var1) (car ret-value))
                    (destruct (var2 ...) (cdr ret-value)))]))


        (define (any? v) #t)
        (define stack? list?)
        (define (pair? s)
        (and (list? s) (= (length s) 2)))


        (define (make-stack) '())

        (define/contract (pop stack)
        (->/c stack? pair?)
        (if (null? stack)
            '(#f '())
            (list (car stack) (cdr stack))))

        ;; value -> stack -> stack
        (define push cons)

        ;; instantiate an empty stack
        (define my-stack (make-stack))

        ;; Bind the last few values from the stack
        ;; Push the values 1, 2, 3, 4, then pop and return the value and the stack
        (destruct (pop-val new-stack)
                (->> my-stack
                    (push 1)
                    (push 2)
                    (push 3)
                    (push 4)
                    (pop)))

        pop-val ;; => 4
        new-stack ;; => '(3 2 1)
        (assert! (equal? pop-val 4))
        "#;
        assert_script(script);
    }

    #[test]
    fn stack_struct_test() {
        let script = r#"
        ;; destruct works like so:
        ;; (destruct (a b c) value)
        ;;  ...
        ;; (define a (car value)
        ;; (define b (car (cdr value)))
        ;; (define c (car (cdr (cdr value))))
        (define-syntax destruct
        (syntax-rules ()
            [(destruct (var) ret-value)
            (define (datum->syntax var) (car ret-value))]
            [(destruct (var1 var2 ...) ret-value)
            (begin (define (datum->syntax var1) (car ret-value))
                    (destruct (var2 ...) (cdr ret-value)))]))

        (define-syntax def-method
        (syntax-rules ()
            [(def-method struct-name (define/method (a this b ...) body ...))
            (define ((datum->syntax struct-name . a) this b ...)
            (unless ((datum->syntax struct-name ?) this)
                (error! (datum->syntax struct-name . a) "method takes a value of" struct-name "given" this))
            body ...)]))

        ;; impl block asserts that each function contains the struct type given as the first argument
        ;; This is why later down we use the thread first vs. the thread last given above
        ;;
        ;; However, take note that a recursive call will not work properly in this, best to be used as an interface
        ;; since it does not transform the name of the recursive call
        (define-syntax impl
        (syntax-rules ()
            [(impl struct-name (define/method (a this b ...) body ...) c ...)
            (begin (def-method struct-name (define/method (a this b ...) body ...))
                    (impl struct-name c ...))]
            [(impl struct-name (define/method (a this b ...) body ...))
            (def-method struct-name (define/method (a this b ...) body ...))]))


        (struct Stack (lst))
        (impl Stack
            ;; Change this to be something like (define/method)
            ;; as to disambiguate it from the base define
            (define/method (pop stack)
                (define contents (Stack-lst stack))
                (if (null? contents)
                    '(#f '())
                    (list (car contents) (cdr contents))))

            (define/method (push stack value)
                (define contents (Stack-lst stack))
                (Stack (cons value contents))))

        (define test-stack (Stack '()))

        (destruct (pop-val-test new-stack-test)
                (-> test-stack
                    (Stack.push 1)
                    (Stack.push 2)
                    (Stack.push 3)
                    (Stack.push 4)
                    (Stack.pop)))

        pop-val-test ;; => 4
        new-stack-test ;; => '(3 2 1)
        (assert! (equal? 4 pop-val-test))
        (assert! (equal? '(3 2 1) new-stack-test))
        "#;
        assert_script(script);
    }

    #[test]
    fn test_stack_state() {
        let script = r#"
        (define (push element)
            (lambda (stack)
                (list '() (cons element stack))))

        (define (pop)
            (lambda (stack)
                (let ((element (car stack))
                    (new-stack (cdr stack)))
                (list element new-stack))))

        (define stack-of cadr)
        (define value-of car)

        (define (>>= stack-action continuation)
            (lambda (stack)
                (let ((result (stack-action stack)))
                ((continuation (value-of result)) (stack-of result)))))

        (define (return value)
            (lambda (stack)
                (list value stack)))

        (define (run-stack computation stack)
            (computation stack))

        (define (eval-stack computation stack)
            (value-of (computation stack)))

        (define (exec-stack computation stack)
            (stack-of (computation stack)))

        (define computation-1 (>>= (push 4) (lambda (_)
                            (>>= (push 5) (lambda (_)
                            (>>= (pop)    (lambda (a)
                            (>>= (pop)    (lambda (b)
                            (return (list a b)))))))))))

        (define computation-2 (>>= (push 2) (lambda (_)
                            (>>= (push 3) (lambda (_)
                            (>>= (pop)    (lambda (a)
                            (>>= (pop)    (lambda (b)
                            (return (list a b)))))))))))

        (define (main)
            (let ((initial-stack '())
                    (composed (>>= computation-1 (lambda (a)
                            (>>= computation-2 (lambda (b)
                            (return (list a b))))))))
                (begin
                    (display "Result: ")
                    (define result (eval-stack composed initial-stack))
                    (display result)
                    (newline)
                    result)))

        (assert! (equal? '((5 4) (3 2)) (main)))
        "#;

        assert_script(script);
    }

    #[test]
    fn merge_sort_test() {
        let script = r#"
        ;;; -----------------------------------------------------------------
        ;;; Merge two lists of numbers which are already in increasing order

        (define merge-lists
        (lambda (l1 l2)
            (if (null? l1)
                l2
                (if (null? l2)
                    l1
                    (if (< (car l1) (car l2))
                        (cons (car l1) (merge-lists (cdr l1) l2))
                        (cons (car l2) (merge-lists (cdr l2) l1)))))))

        ;;; -------------------------------------------------------------------
        ;;; Given list l, output those tokens of l which are in even positions

        (define even-numbers
        (lambda (l)
            (if (null? l)
                '()
                (if (null? (cdr l))
                    '()
                    (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

        ;;; -------------------------------------------------------------------
        ;;; Given list l, output those tokens of l which are in odd positions

        (define odd-numbers
        (lambda (l)
            (if (null? l)
                '()
                (if (null? (cdr l))
                    (list (car l))
                    (cons (car l) (odd-numbers (cdr (cdr l))))))))

        ;;; ---------------------------------------------------------------------
        ;;; Use the procedures above to create a simple and efficient merge-sort

        (define merge-sort
        (lambda (l)
            (if (null? l)
                l
                (if (null? (cdr l))
                    l
                    (merge-lists
                    (merge-sort (odd-numbers l))
                    (merge-sort (even-numbers l)))))))

        (define result (merge-sort '(5 1 4 2 3)))
        (assert! (equal? result '(1 2 3 4 5)))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod apply_tests {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn basic_apply() {
        let script = r#"
        (define result (apply + (list 1 2 3 4)))
        (assert! (equal? 10 result))
        "#;
        assert_script(script);
    }

    #[test]
    fn apply_more_complex() {
        let script = r#"
        (define result (apply map (list (lambda (x) 10) (list 1 2 3 4))))
        (assert! (equal? result (list 10 10 10 10)))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod dfs_test {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn test_dfs() {
        let script = r#"
        ;; implementation of a dfs in steel

        ;; Given a node and a graph
        ;; node -> 'a
        ;; graph -> '((a b c) (b d) (e f))
        ;;
        ;; returns the neighbors
        ;; ex.
        ;; (get-neighbors 'a graph) => '(b c)
        ;;
        (define (get-neighbors node graph)
        (define found-neighbors (assoc node graph))
        (if found-neighbors
            (cdr found-neighbors)
            '()))

        (define (longest  lst)
        (foldr (λ (a b) (if (> (length a) (length b)) a b))
                '()
                lst))

        (define (reverse ls)
        (define (my-reverse-2 ls acc)
            (if (null? ls)
                acc
            (my-reverse-2 (cdr ls) (cons (car ls) acc))))
        (my-reverse-2 ls '()))

        (define (first-step curr end graph)
        (define neighbors (get-neighbors curr graph))
        (longest (map (lambda (x) (dfs x end '() '() graph)) neighbors)))


        (define (member? x los)
        (cond
            ((null? los) #f)
            ((equal? x (car los)) #t)
            (else (member? x (cdr los)))))

        ;; iteratively tries each neighbor
        ;; quits when the length is worse
        (define (try-all-neighbors neighbors best-path end new-path graph)
        (if (not (empty? neighbors))
            (let* ((next-neighbor (car neighbors))
                    (found-path (dfs next-neighbor end new-path best-path graph)))
                (if (> (length found-path) (length best-path))
                    (try-all-neighbors (cdr neighbors) found-path end new-path graph)
                    (try-all-neighbors (cdr neighbors) best-path end new-path graph)))
            best-path))

        (define (dfs curr end path best-path graph)
        (define neighbors (get-neighbors curr graph))
        (define new-path (cons curr path))
        (cond ((equal? curr end)
                (cons curr path))
                ((member? curr path)
                '())
                (neighbors
                (try-all-neighbors neighbors best-path end (cons curr path) graph))
                (else '())))

        (define (longest-path start end graph)
        (define found-path (reverse (first-step start end graph)))
        (cond ((empty? found-path)
                (if (equal? start end)
                    (list start)
                    '()))
                ((and (equal? (car found-path) start) (not (equal? start end)))
                found-path)
                (else (cons start found-path))))


        (longest-path 'a 'c '((a b) (b c))) ;; '(a b c)
        (longest-path 'a 'c '((a b) (b a c))) ;; '(a b c)
        (longest-path 'a 'c '((a d e f g b) (b a c))) ;; '(a b c)
        (longest-path 'a 'a '((a b) (b a c))) ;; '(a b a)
        (longest-path 'a 'c '((a b) (b a) (c))) ;; '()
        (longest-path 'a 'f '((a b c) (b f) (c d) (d e) (e f))) ;; '(a c d e f)
        (longest-path 'a 'f '((a b c a) (b c d) (c e a) (d e f) (e d f))) ;; '(a b c e d f)
        (longest-path 'a 'a '((a b c a) (b c d) (c e a) (d e f) (e d f))) ;; '(a b c a)
        (longest-path 'a 'a '((a b) (b c))) ;; '(a)
        (longest-path 'a 'a '((a a b) (b c))) ;; '(a a)
        (longest-path 'a 'a '((a b a) (b c))) ;; '(a a)
        (longest-path 'a 'b '((a b) (b c) (c b))) ;; '(a b)
        (longest-path 'a 'b '((a b c) (b c) (c b))) ;; '(a c b)

        (assert! (equal? (longest-path 'a 'c '((a b) (b c))) '(a b c)))
        (assert! (equal? (longest-path 'a 'c '((a b) (b a c))) '(a b c)))
        (assert! (equal? (longest-path 'a 'c '((a d e f g b) (b a c))) '(a b c)))
        (assert! (equal? (longest-path 'a 'a '((a b) (b a c))) '(a b a)))
        (assert! (equal? (longest-path 'a 'c '((a b) (b a) (c))) '()))
        (assert! (equal? (longest-path 'a 'f '((a b c) (b f) (c d) (d e) (e f))) '(a c d e f)))
        (assert! (equal? (longest-path 'a 'f '((a b c a) (b c d) (c e a) (d e f) (e d f))) '(a b c e d f)))
        (assert! (equal? (longest-path 'a 'a '((a b c a) (b c d) (c e a) (d e f) (e d f))) '(a b c a)))
        (assert! (equal? (longest-path 'a 'a '((a b) (b c))) '(a)))
        (assert! (equal? (longest-path 'a 'a '((a a b) (b c))) '(a a)))
        (assert! (equal? (longest-path 'a 'a '((a b a) (b c))) '(a a)))
        (assert! (equal? (longest-path 'a 'b '((a b) (b c) (c b))) '(a b)))
        (assert! (equal? (longest-path 'a 'b '((a b c) (b c) (c b))) '(a c b)))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod sieve_test {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn sieve_test() {
        let script = r#"
        (define (sieve n)
        (define (aux u v)
            (let ((p (car v)))
            (if (> (* p p) n)
                (rev-append u v)
                (aux (cons p u)
                (wheel '() (cdr v) (* p p) p)))))
        (aux '(2)
            (range-s '() (if (odd? n) n (- n 1)))))


        (define (wheel u v a p)
            (cond ((null? v) (reverse u))
                            ((= (car v) a) (wheel u (cdr v) (+ a p) p))
                            ((> (car v) a) (wheel u v (+ a p) p))
                            (else (wheel (cons (car v) u) (cdr v) a p))))

        (define (rev-append u v)
            (if (null? u) v (rev-append (cdr u) (cons (car u) v))))

        (define (range-s v k)
            (if (< k 3) v (range-s (cons k v) (- k 2))))

        (assert! (equal? 168 (length (sieve 1000))))
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod calculator_test {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn calculator_test() {
        let script = r#"
        (struct node (datum children))

        (define (parse expr)
        (parse-helper expr '() '()))

        (define (parse-helper expr operators operands)
        (cond ((null? expr)
                (if (null? operators)
                    (car operands)
                    (handle-op '() operators operands)))
                ((number? (car expr))
                (parse-helper (cdr expr)
                            operators
                            (cons (node (car expr) '()) operands)))
                ((list? (car expr))
                (parse-helper (cdr expr)
                            operators
                            (cons (parse (car expr)) operands)))
                (else (if (or (null? operators)
                            (> (precedence (car expr))
                                (precedence (car operators))))
                        (parse-helper (cdr expr)
                                        (cons (car expr) operators)
                                        operands)
                        (handle-op expr operators operands)))))

        (define (handle-op expr operators operands)
        (parse-helper expr
                        (cdr operators)
                        (cons (node (car operators)
                                        (list (cadr operands) (car operands)))
                            (cddr operands))))

        (define (member? x los)
        (cond
            ((null? los) #f)
            ((equal? x (car los)) #t)
            (else (member? x (cdr los)))))


        (define (precedence oper)
        (if (member? oper '(+ -))
            1
            2))


        (define (compute tree)
        (if (number? (node-datum tree))
            (node-datum tree)
            ((function-named-by (node-datum tree))
            (compute (car (node-children tree)))
            (compute (cadr (node-children tree))))))

        (define (function-named-by oper)
        (cond ((equal? oper '+) +)
                ((equal? oper '-) -)
                ((equal? oper '*) *)
                ((equal? oper '/) /)
                (else (error! "no such operator as" oper))))

        
        (assert! (equal? 10 (compute (parse '(1 + 2 + 3 + 4))))) ;; => 10
        "#;
        assert_script(script);
    }
}

#[cfg(test)]
mod read_test {
    use crate::steel_vm::test_util::assert_script;

    #[test]
    fn test_basic_read() {
        let script = r#"
            (define read-value (read "1 2 3 4 5"))
            (assert! (equal? '(1 2 3 4 5) read-value))
        "#;
        assert_script(script);
    }
}