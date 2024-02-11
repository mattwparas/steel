(define-syntax assert-equal!
  (syntax-rules ()
    ((_ expected actual)
     (let ((ok (equal? expected actual)))
       (when (not ok)
	 (displayln "Expected value " expected " but got " actual ".")
	 (assert! ok))))))

;; Number types
(assert! (not (equal? 10 10.0)))
(assert! (integer? 1))
(assert! (not (integer? 1.2)))
(assert! (not (integer? +inf.0)))
(assert! (rational? 1))
(assert! (rational? 1/4))
(assert! (rational? 1.2))
(assert! (not (rational? -inf.0)))
(assert! (number? 1))
(assert! (number? 1/4))
(assert! (number? 1.2))
(assert! (number? +inf.0))
(assert! (number? +nan.0))
(assert! (real? 1))
(assert! (real? 1/4))
(assert! (real? 1.2))
(assert! (real? +inf.0))
(assert! (real? -nan.0))
(assert! (not (real? 1+2i)))
(assert! (not (real? +2i)))
(assert! (not (real? 1.0+2.0i)))
(assert! (not (real? +2.0i)))
(assert! (not (real? 1.0+2i)))

;; Addition
(assert-equal! 10
	       (+ 1 2 3 4))
(assert-equal! 10.0
	       (+ 1 2 3.0 4))
(assert-equal! 7/12
	       (+ 1/4 1/3))
(assert-equal! 120.0
	       (+ 1e2 2e1))
;; Float + Rational is promoted to Float.
(assert-equal! (/ 7.0 12.0)
	       (+ 0.25 1/3))
(assert-equal! 9223372036854775808
	       (+ 9223372036854775808))
(assert-equal! 18446744073709551616
	       (+ 9223372036854775808 9223372036854775808))
(assert-equal! 27670116110564327424
	       (+ 9223372036854775808 9223372036854775808 9223372036854775808))
;; Promotion from int -> bignum, one over int max
(assert-equal! 9223372036854775808
	       (+ 1 9223372036854775807))
(assert-equal! 20000000000000000000000009/60000000000000
	       (+ 1000000000000/3 15/100000000000000))
(assert-equal! 4+6i
	       (+ 1+2i 3+4i))
(assert-equal! 4.0+6.0i
	       (+ 1.0+2.0i 3+4i))

;; Subtraction
(assert-equal! -10
	       (- 10))
(assert-equal! -10.0
	       (- 10.0))
(assert-equal! -8
	       (- 1 2 3 4))
(assert-equal! -8.0
	       (- 1 2.0 3 4))
(assert-equal! 1
	       (- -1))
(assert-equal! -1/4
	       (- 1/4))
(assert-equal! 9223372036854775808
	       (- -9223372036854775808))
(assert-equal! 9999980000000000000
	       (- 10000000000000000000 20000000000000))
(assert-equal! -1e10-2e10i
	       (- 1e10+2e10i))
(assert-equal! -2-2i
	       (- 1+2i 3+4i))

;; Multiplication
(assert-equal! 10
	       (* 2 5))
(assert-equal! 10.0
	       (* 2.0 5.0))
(assert-equal! 10.0
	       (* 100.0 0.1))
(assert-equal! 1/4
	       (* 1/8 2))
;; Promotion from int -> bignum, with multiplication
(assert-equal! 18446744073709551614
	       (* 2 9223372036854775807))
(assert-equal! 85070591730234615856620279821087277056
	       (* 9223372036854775807 9223372036854775808))
(assert-equal! -5+10i
	       (* 1+2i 3+4i))

;; Division
(assert-equal! 0.25
	       (/ 4.0))
(assert-equal! 1
	       (/ 1))
(assert-equal! 0.25
	       (/ 1 4.0))
(assert-equal! 0.04
	       (/ 2.0 5 10))
(assert-equal! 1/4
	       (/ 4))
(assert-equal! 2
	       (/ 22222222222222222222 11111111111111111111))
(assert-equal! 1/2
	       (/ 11111111111111111111 22222222222222222222))
(assert-equal! 1/2
	       (/ 11111111111111111111 22222222222222222222))
(assert-equal! 1/5-1/5i
	       (/ 1+2i))

;; Rounding
(assert-equal! 3
	       (round 3))
(assert-equal! 1
	       (round 4/3))
(assert-equal! 2
	       (round 5/3))
(assert-equal! 2.0
	       (round 2.1))
(assert-equal! 3.0
	       (round 2.6))
(assert-equal! 9223372036854775808
	       (round 9223372036854775808))

;; Comparisons
(assert! (< -10 9223372036854775808))
(assert! (< -10.1 9223372036854775808))
(assert! (< 9223372036854775808
	    9993372036854775808))
