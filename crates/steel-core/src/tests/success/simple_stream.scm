(define (stream-cdr stream)
  ((#%stream-cdr stream)))

(define (integers n)
  (stream-cons n (lambda () (integers (+ 1 n)))))

(define (stream-section n stream)
  (displayln stream)
  (cond
    [(= n 0) '()]
    [else (cons (stream-car stream) (stream-section (- n 1) (stream-cdr stream)))]))

(assert! (equal? (list 0 1 2 3 4) (stream-section 5 (integers 0))))
