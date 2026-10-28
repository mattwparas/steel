(define-syntax m (syntax-rules () [(_ x) x]))
(define (f) (set! m 1))
