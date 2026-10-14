(define-syntax make-pair
  (syntax-rules ()
    [(_) (define-values (a b) (values 1 2))]))
(make-pair)

(+ a b)
