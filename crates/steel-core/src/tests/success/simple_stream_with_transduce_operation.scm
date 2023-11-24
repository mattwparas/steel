(define (stream-cdr stream)
  ((#%stream-cdr stream)))

(define (integers n)
  (stream-cons n (lambda () (integers (+ 1 n)))))

(assert! (equal? 10 (transduce (integers 0) (taking 5) (into-reducer + 0))))
