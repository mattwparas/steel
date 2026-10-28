(define-syntax twice (syntax-rules () [(_ n) (begin (define n 1) (define n 2))]))
(define (f) (twice q) q)
(f)
