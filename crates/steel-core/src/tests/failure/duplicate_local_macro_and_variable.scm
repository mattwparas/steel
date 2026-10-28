(define-syntax (mk stx) (syntax-case stx () [(_ n v) (syntax (define-syntax (n s) (syntax-case s () [(_) (syntax (quote v))])))]))
(define (f) (define x 1) (mk x 5) (x))
(f)
