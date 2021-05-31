(define (stream-cdr stream)
    ((stream-cdr' stream)))

(define (integers n)
    (stream-cons n (lambda () (integers (+ 1 n)))))

(assert! 
    (equal? (list 0 1 2 3 4)
            (execute (taking 5) (integers 0))))