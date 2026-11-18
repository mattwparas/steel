(define (f x)
  (if x
      (define-syntax twice
        (syntax-rules ()
          [(_ a) (* 2 a)]))
      1))
