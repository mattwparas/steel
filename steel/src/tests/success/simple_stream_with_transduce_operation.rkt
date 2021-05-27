(define (stream-cdr stream)
    ((stream-cdr' stream)))

(define (integers n)
    (stream-cons n (lambda () (integers (+ 1 n)))))

(assert! 
    (equal? 10
            (transduce (taking 5) + 0 (integers 0))))