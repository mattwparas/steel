(define (stream-cdr stream)
    ((stream-cdr' stream)))

(define (integers n)
    (stream-cons n (lambda () (integers (+ 1 n)))))

(assert! 
    (equal? (list 1 2 3 4 5)
            (execute (compose 
                        (mapping (lambda (x) (+ x 1)))
                        (taking 5)) 
                        (integers 0))))