(define (test x) 
    (set! test (lambda (x) 100)) 
    (if (= x 10)
        x
        (test (+ x 1))))

(assert! (equal? (test 0) 100))