(define result (apply + (list 1 2 3 4)))
(assert! (equal? 10 result))