(define (f x)
  (define-syntax twice
    (syntax-rules ()
      [(_ a) (* 2 a)]))
  (twice x))

(twice 3)
