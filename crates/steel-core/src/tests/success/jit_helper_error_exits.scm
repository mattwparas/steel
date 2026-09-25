;; A runtime helper that raises (here `vector-ref` out of range, reached through
;; the jit's register fallback) must leave native code before anything uses its
;; result, so the error comes from `vector-ref` and not a later `+`.
(define log (vector 0 0))

(define (h v i out)
  (vector-set! out 1 (+ (vector-ref out 1) (vector-ref v i)))
  'done)

(define (message-of thunk)
  (with-handler (lambda (e) (error-object-message e)) (thunk)))

(assert! (equal? (h (vector 5 6) 0 log) 'done))
(assert! (equal? (message-of (lambda () (h (vector 5 6) 9 log)))
                 "index out of bounds, index given: 9, length of vector: 2"))
(assert! (equal? (vector-ref log 1) 5))

(define (g v i out)
  (let ([x (vector-ref v i)])
    (vector-set! out 0 (+ (vector-ref out 0) 1))
    x))

(assert! (equal? (g (vector 1 2 3) 1 log) 2))
(assert! (equal? (message-of (lambda () (g (vector 1 2 3) 10 log)))
                 "index out of bounds, index given: 10, length of vector: 3"))
(assert! (equal? (vector-ref log 0) 1))
