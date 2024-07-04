(require-builtin steel/process)
(require-builtin steel/transducers)
(require-builtin steel/meta)

(require "tests/unit-test.scm"
         (for-syntax "tests/unit-test.scm"))

(define (check-syntax-error? name input expected . rest)
  (define (impl name input expected)
      (define error-message
        (~> (run! (Engine::new) input) Err->value error-object-message))
      (define message-assert (or (string-contains? error-message expected) `(message= ,error-message expected= ,expected)))
      (check-equal? (string-join `(,name " [compilation fails]"))  message-assert #t))
  (if (eq? name 'skip)
      (skip-compile #f)
      (impl name input expected)))

(check-syntax-error? "empty transformer"
  '(
    (define-syntax no-body
      (syntax-rules ()
                    [(_ a) ])))
  "syntax-rules requires only one pattern to one body")

(check-syntax-error? 'skip "no-spread"
  '(
    (define-syntax no-spread
      (syntax-rules ()
                    [(_ a ...) a])))
  "syntax-rules requires only one pattern to one body")

(check-syntax-error? "bad spread"
  '(
    (define-syntax bad-spread
      (syntax-rules ()
                    [(_ a ...) a ... ])))
  "syntax-rules requires only one pattern to one body")

(define-syntax catchall
  (syntax-rules ()
                [(_ a ...) '(a ...)]))

(check-equal? "catch-all, 0 args" (catchall) '())
(check-equal? "catch-all, 1 arg" (catchall a) '(a))
(check-equal? "catch-all, 2 args" (catchall a b) '(a b))

(define-syntax catchall-but-one
  (syntax-rules ()
                [(_ a ... b) b]))

(skip-compile (check-equal? "catch-all but one, 1 arg" (catchall-but-one 'x) 'x))
(skip-compile (check-equal? "catch-all but one, 2 args" (catchall-but-one x '(y)) '(y)))
(skip-compile (check-equal? "catch-all but one, 3 arg" (catchall-but-one x y '(z)) '(z)))

(check-syntax-error? "catch-all but one, 0 args"
  '(
    (define-syntax catchall-but-one
      (syntax-rules ()
                    [(_ a ... b) b]))
    (catchall-but-one))
  "unable to match case")

(define-syntax catchall-list
  (syntax-rules ()
                [(_ (a b) ...) (quote (a ...) )]))

(check-equal? "catch-all lists, 0 args" (catchall-list) '())
(check-equal? "catch-all lists, 1 arg" (catchall-list (a b)) '(a))
(check-equal? "catch-all lists, 2 args" (catchall-list (a b) (c d)) '(a c))

(define-syntax improper-rest
  (syntax-rules ()
                [(_ (a . b)) 'b]))

(skip-compile (check-equal? "improper-rest, 0 args" (improper-rest 1) '()))
(skip-compile (check-equal? "improper-rest, 1 arg" (improper-rest 1 2) '(2)))
(skip-compile (check-equal? "improper-rest, 2 args" (improper-rest 1 a (b)) '(a (b))))

;; -------------- Report ------------------

(define stats (get-test-stats))

(displayln "Passed: " (hash-ref stats 'success-count))
(displayln "Skipped compilation: " (hash-ref stats 'failed-to-compile))
(displayln "Failed: " (hash-ref stats 'failure-count))
