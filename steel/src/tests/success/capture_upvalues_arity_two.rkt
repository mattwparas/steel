(define test ((lambda (x) (lambda (y) (+ x y))) 10))
(assert! (equal? 25 (test 15)))