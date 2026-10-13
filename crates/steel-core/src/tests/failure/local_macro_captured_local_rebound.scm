(define (f y)
  (define-syntax add-y
    (syntax-rules ()
      [(_ a) (+ a y)]))
  (let ([y 1000])
    (add-y y)))
