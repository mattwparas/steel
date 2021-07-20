(provide
    ; (contract/out o-map (->/c (t : predicate?) (Option/c t) (->/c t Option?) Option?))
    Some None
    Some? None? Option?
    (contract/out map-option (->/c Option? (->/c any/c any/c) Option?))
    (contract/out unwrap-some (->/c Some? any/c))
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
(define (map-option option func)
    (cond [(Some? option) (Some (func (Some-value option)))]
          [(None? option) (None)]))

;; Get the inner value of the option - contract checking along the way
;; Figure out how to turn off contracts on OptLevel3 regardless - 
;; this would speed up performance a lot - also figure out how to map this to compile time options in Rust
(define (unwrap-some option)
    (Some-value option))
