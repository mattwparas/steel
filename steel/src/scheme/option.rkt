(provide
    ; (contract/out o-map (->/c (t : predicate?) (Option/c t) (->/c t Option?) Option?))
    Some None
    Some? None? Option?
    (contract/out o-map (->/c Option? (->/c any/c any/c) Option?))
    Option/c)

(struct Some (value))
(struct None ())

;; Contracts for option
(define (Option/c pred)
    (make/c (fn (x) 
                (cond [(Some? x) (pred (Some-value x))]
                      [(None? x) #t]
                      [else #f])) 
            'Option/c))

(define (Option? value)
    (or (Some? value) (None? value)))

;; Map - explore dynamic dispatch with contracts?
(define (o-map option func)
    (cond [(Some? option) (Some (func (Some-value option)))]
          [(None? option) (None)]))
