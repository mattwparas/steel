(define (ackermann m n)
  (cond [(equal? m 0) (+ n 1)]
        [(equal? n 0) (ackermann (- m 1) 1)]
        [else (ackermann (- m 1) (ackermann m (- n 1)))]))