(define (fib n)
  (define (loop a b n)
    (if (= n 0)
        a
        (loop b (+ a b) (- n 1))))
  (loop 0 1 n))
