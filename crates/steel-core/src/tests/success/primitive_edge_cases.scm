;; Primitive bugs found by fuzzing the `args` / `raises` annotations.

(define (raises? thunk)
  (with-handler (lambda (e) #true) (thunk) #false))

;; `*` had no arm for a rational times a big rational (only the reverse order).
(define big-rational (/ 1 (expt 2 70)))
(assert! (equal? (* 1/2 big-rational) (* big-rational 1/2)))

;; `log` skipped checking its base when the argument was 1.
(assert! (raises? (lambda () (log 1 "s"))))
(assert! (raises? (lambda () (log 2 "s"))))
(assert! (equal? (log 1) 0))
(assert! (equal? (log 8 2) 3.0))

;; `string-join` panicked (todo!) on a third argument.
(assert! (equal? (string-join (list "a" "b") ", ") "a, b"))
(assert! (raises? (lambda () (string-join (list "a") "," ","))))

;; `range-vec` cast negative bounds to usize and tried to allocate the result.
(assert! (equal? (range-vec 1 4) (immutable-vector 1 2 3)))
(assert! (equal? (range-vec 3 1) (immutable-vector)))
(assert! (raises? (lambda () (range-vec -3 1))))

;; `=` compared one complex's imaginary part with the other's real part, and
;; returned #false (instead of raising) for a complex against a non-number.
(define z11 (make-rectangular 1 1))
(define z15 (make-rectangular 1 5))
(assert! (not (= z11 z15)))
(assert! (= z11 (make-rectangular 1 1)))
(assert! (raises? (lambda () (= z11 "s"))))
(assert! (raises? (lambda () (= "s" z11))))
(assert! (not (= z11 1)))
(assert! (= (make-rectangular 1 0.0) 1))

;; The bitwise operations silently skipped non-fixnum arguments after the first.
(assert! (equal? (bitwise-and 6 3) 2))
(assert! (equal? (bitwise-ior 4 1 2) 7))
(assert! (equal? (bitwise-xor 5 1) 4))
(assert! (raises? (lambda () (bitwise-and 6 "x"))))
(assert! (raises? (lambda () (bitwise-ior 1 2.0))))
(assert! (raises? (lambda () (bitwise-xor 1 (expt 2 70)))))

;; `immutable-vector-set` let index == length through to a panic.
(define v (immutable-vector 1 2))
(assert! (equal? (immutable-vector-set v 1 9) (immutable-vector 1 9)))
(assert! (raises? (lambda () (immutable-vector-set v 2 9))))
(assert! (equal? v (immutable-vector 1 2)))

;; `will-execute` unwrapped its argument.
(assert! (raises? (lambda () (will-execute 1))))
