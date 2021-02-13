#[cfg(test)]
mod stream_tests {
    use crate::evaluation_progress::EvaluationProgress;
    use crate::lazy_stream::LazyStreamIter;
    use crate::test_util::assert_script;
    use steel::lazy_stream::LazyStream;
    use steel::parser::span::Span;
    use steel::steel_compiler::constants::ConstantMap;

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
}

#[cfg(test)]
mod struct_integration_tests {
    use crate::test_util::assert_script;

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
    // use super::*;
    use crate::test_util::assert_script;

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
