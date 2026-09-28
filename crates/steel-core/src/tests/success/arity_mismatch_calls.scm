;; A call with the wrong number of arguments raises the arity error when it
;; runs. It must not fail to compile or read a garbage slot.

(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

(define (message-of thunk)
  (with-handler (lambda (e) (error-object-message e)) (thunk)))

(define-syntax catching
  (syntax-rules ()
    [(_ expr) (with-handler (lambda (e) (error-object-message e)) expr)]))

(define (f a b)
  (list a b))
(define (v a b . r)
  (list a b r))

;; Small enough to be inlined at every call site. Inlining an under applied
;; call leaves `b` unbound, which is a FreeIdentifier compile error even though
;; the bad call below is never made.
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

;; Calling a closure only known at run time. The jitted call has to check the
;; argument count as well as that the callee is compiled and fixed arity.
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

;; The no-arity call opcodes skip the arity check at run time, so the analysis
;; can only use them for calls with the right argument count. Under applied,
;; `f` would take the pending `0` below as its first argument and return
;; normally, leaving the value stack a slot off.
(define (pending-then-call)
  (cons 0 (f 1)))

(assert-equal! "function expected 2 arguments, found 1" (catching (pending-then-call)))
(assert-equal! "function expected 2 arguments, found 1" (message-of pending-then-call))
(assert-equal! "function expected 2 arguments, found 1"
               (message-of (lambda () (non-tail-call f))))
(assert-equal! "function expected 2 arguments, found 1"
               (message-of (lambda () (cons 0 (f 1)))))

;; Self calls, fixed and variadic. The jit reads the callee's arity off a
;; placeholder lambda while compiling it, which has to carry the real arity and
;; variadic flag, or fixed self calls fall off the fast path and variadic ones
;; look fixed.
(define (vsum n . xs)
  (if (= n 0) (length xs) (vsum (- n 1) n)))
(define (vcollect n . xs)
  (if (= n 0) xs (let ([r (vcollect (- n 1) n n)]) (cons n r))))
(define (self-loop k acc)
  (if (= k 0) acc (self-loop (- k 1) (+ acc (vsum 50) (length (vcollect 20))))))
(assert-equal! 46000 (self-loop 2000 0))
(assert-equal! '(3 2 1 1 1) (vcollect 3))
(define (needs-two a b . r)
  (if (= a 0) r (needs-two (- a 1))))
(assert-equal! "function expected at least 2 arguments, found 1" (catching (needs-two 5 1)))
