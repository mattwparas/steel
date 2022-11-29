(provide
    Some None
    Some? None? Option?
    (contract/out map-option (->/c Option? (->/c any/c any/c) Option?))
    (contract/out unwrap-some (->/c Some? any/c))
    (contract/out flatten-option (->/c Option? Option?))
    (contract/out unwrap-or (->/c Option? any/c any/c))
    Option/c)

(new-make-struct Some (value) #:transparent #t)
(new-make-struct None ())

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
          [(None? option) (None)]
          [else => (error! "map-option with unknown input")]))

(define (flatten-option option)
    (if (Some? option)
        (if (Some? (Some-value option))
            (Some-value option)
            (None))
        (None)))

;; Get the inner value of the option - contract checking along the way
;; Figure out how to turn off contracts on OptLevel3 regardless - 
;; this would speed up performance a lot - also figure out how to map this to compile time options in Rust
(define unwrap-some Some-value)

;; Unwraps the given option or returns the given other value
(define (unwrap-or option other)
    (if (Some? option) (Some-value option) other))
