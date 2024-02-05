(define-syntax assert-equal!
    (syntax-rules ()
      ((_ expected actual)
       (assert! (equal? expected actual)))))

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
	       (/ 4))
(assert-equal! 1
	       (/ 1))
(assert-equal! 0.25
	       (/ 1 4))
(assert-equal! 0.04
	       (/ 2 5 10))
