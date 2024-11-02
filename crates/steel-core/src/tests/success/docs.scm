;;@doc
;; This is a function that does something
(define (foo x)
  (list 10 20 30 40 x))

(assert! (equal? "This is a function that does something" (trim-end foo__doc__)))

;;@doc
;; This is a function that takes multiple arguments
(define (bar . args)
  (list 10 20 30 40 args))

(assert! (equal? (list 10 20 30 40 (list 50 60)) (bar 50 60)))
