;;@doc
;; This is a function that does something
(define (foo x)
  (list 10 20 30 40 x))

(assert! (equal? "This is a function that does something\n" foo__doc__))
