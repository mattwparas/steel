;; Primitive edge cases for the `args` / `raises` annotations.

(define (raises? thunk)
  (with-handler (lambda (e) #true) (thunk) #false))

;; `*` handles a rational times a big rational in both orders.
(define big-rational (/ 1 (expt 2 70)))
(assert! (equal? (* 1/2 big-rational) (* big-rational 1/2)))

;; `log` checks its base even when the argument is 1.
(assert! (raises? (lambda () (log 1 "s"))))
(assert! (raises? (lambda () (log 2 "s"))))
(assert! (equal? (log 1) 0))
(assert! (equal? (log 8 2) 3.0))

;; `string-join` raises on a third argument.
(assert! (equal? (string-join (list "a" "b") ", ") "a, b"))
(assert! (raises? (lambda () (string-join (list "a") "," ","))))

;; `range-vec` raises on negative bounds.
(assert! (equal? (range-vec 1 4) (immutable-vector 1 2 3)))
(assert! (equal? (range-vec 3 1) (immutable-vector)))
(assert! (raises? (lambda () (range-vec -3 1))))

;; `=` compares complex parts pairwise, and raises for a complex against a
;; non-number.
(define z11 (make-rectangular 1 1))
(define z15 (make-rectangular 1 5))
(assert! (not (= z11 z15)))
(assert! (= z11 (make-rectangular 1 1)))
(assert! (raises? (lambda () (= z11 "s"))))
(assert! (raises? (lambda () (= "s" z11))))
(assert! (not (= z11 1)))
(assert! (= (make-rectangular 1 0.0) 1))

;; The bitwise operations check every argument is a fixnum.
(assert! (equal? (bitwise-and 6 3) 2))
(assert! (equal? (bitwise-ior 4 1 2) 7))
(assert! (equal? (bitwise-xor 5 1) 4))
(assert! (raises? (lambda () (bitwise-and 6 "x"))))
(assert! (raises? (lambda () (bitwise-ior 1 2.0))))
(assert! (raises? (lambda () (bitwise-xor 1 (expt 2 70)))))

;; `immutable-vector-set` raises when index == length.
(define v (immutable-vector 1 2))
(assert! (equal? (immutable-vector-set v 1 9) (immutable-vector 1 9)))
(assert! (raises? (lambda () (immutable-vector-set v 2 9))))
(assert! (equal? v (immutable-vector 1 2)))

;; `will-execute` raises on a non-will-executor.
(assert! (raises? (lambda () (will-execute 1))))
