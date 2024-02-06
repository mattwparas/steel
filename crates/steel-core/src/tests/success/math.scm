(define-syntax assert-equal!
  (syntax-rules ()
    ((_ expected actual)
     (let ((ok (equal? expected actual)))
       (when (not ok)
	 (displayln "Expected value " expected " but got " actual ".")
	 (assert! ok))))))

;; Number types
(assert! (not (equal? 10 10.0)))

;; Addition
(assert-equal! 10
	       (+ 1 2 3 4))
(assert-equal! 10.0
	       (+ 1 2 3.0 4))

;; Subtraction
(assert-equal! -10
	       (- 10))
(assert-equal! -10.0
	       (- 10.0))
(assert-equal! -8
	       (- 1 2 3 4))
(assert-equal! -8.0
	       (- 1 2.0 3 4))

;; Multiplication
(assert-equal! 10
	       (* 2 5))
(assert-equal! 10.0
	       (* 2.0 5.0))
(assert-equal! 10.0
	       (* 100.0 0.1))

;; Division
(assert-equal! 0.25
	       (/ 4.0))
(assert-equal! 1
	       (/ 1))
(assert-equal! 0.25
	       (/ 1 4.0))
(assert-equal! 0.04
	       (/ 2.0 5 10))

;; Fraction operations.
(assert-equal! 1/4
	       (/ 4))
(assert-equal! 1/4
	       (* (/ 8) 2))
(assert-equal! 7/12
	       (+ (/ 4) (/ 3)))
(assert-equal! (/ 7.0 12.0)
	       (+ 0.25 (/ 3)))
(assert-equal! -1/4
	       (- (/ 4)))
