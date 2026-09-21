;; A call with the wrong number of arguments must raise the arity error when
;; it runs - not fail to compile, and not read a garbage slot.

(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

(define (message-of thunk)
  (with-handler (lambda (e) (error-object-message e)) (thunk)))

;; Inline rather than through a thunk: the interpreter currently panics when a
;; thunk under `with-handler` tail calls something that raises from a non-tail
;; call (a separate, pre-existing bug).
(define-syntax catching
  (syntax-rules ()
    [(_ expr) (with-handler (lambda (e) (error-object-message e)) expr)]))

(define (f a b)
  (list a b))
(define (v a b . r)
  (list a b r))

;; Small enough to be inlined at every call site. An under applied call used
;; to be inlined anyway, leaving `b` unbound: a FreeIdentifier compile error,
;; even though the bad call below is never made.
(define (never-called)
  (f 1))
(define (too-few)
  (f 1))
(define (too-many)
  (f 1 2 3))
(define (variadic-too-few)
  (v 1))

(assert-equal! "function expected 2 arguments, found 1" (message-of too-few))
(assert-equal! "function expected 2 arguments, found 3" (message-of too-many))
(assert-equal! "function expected at least 2 arguments, found 1" (message-of variadic-too-few))
(assert-equal! '(1 2) (f 1 2))
(assert-equal! '(1 2 (3 4)) (v 1 2 3 4))

;; Calling a closure only known at run time. The jitted call checked that it
;; was compiled and fixed arity, but never the argument count.
(define (tail-call k)
  (k 1))
(define (non-tail-call k)
  (cons 0 (k 1)))

(define (warm n)
  (if (= n 0)
      'done
      (begin
        (catching (tail-call f))
        (catching (non-tail-call f))
        (catching (tail-call v))
        (warm (- n 1)))))

(assert-equal! 'done (warm 2000))
(assert-equal! "function expected 2 arguments, found 1" (catching (tail-call f)))
(assert-equal! "function expected 2 arguments, found 1" (catching (non-tail-call f)))
(assert-equal! "function expected at least 2 arguments, found 1" (catching (tail-call v)))
(assert-equal! '(0 1) (non-tail-call (lambda (x) (list x))))
