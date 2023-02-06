(define (ackermann m n)
  (cond [(equal? m 0) (+ n 1)]
        [(equal? n 0) (ackermann (- m 1) 1)]
        [else (ackermann (- m 1) (ackermann m (- n 1)))]))

(define (loop x)
  (if (equal? x 100)
      #true
      (begin
        (ackermann 3 3)
        (loop (+ x 1)))))


(loop 50)
