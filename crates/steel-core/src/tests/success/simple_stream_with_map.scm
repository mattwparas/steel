(define (stream-cdr stream)
  ((#%stream-cdr stream)))

(define (integers n)
  (stream-cons n (lambda () (integers (+ 1 n)))))

(define (stream-section n stream)
  (cond
    [(= n 0) '()]
    [else (cons (stream-car stream) (stream-section (- n 1) (stream-cdr stream)))]))

(define (map-stream func s)
  (cond
    [(stream-empty? s) s]
    [else (stream-cons (func (stream-car s)) (lambda () (map-stream func (stream-cdr s))))]))

(assert! (equal? (list 10 10 10 10 10) (stream-section 5 (map-stream (lambda (x) 10) (integers 0)))))
