(define value 
    ((((lambda (x) 
        (lambda (y) 
            (lambda (z) (+ x y z)))) 
                10) 
                20) 
                30))
(assert! (equal? 60 value))