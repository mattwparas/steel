(struct Ok (value))
(struct Err (value))

(define (Result? value)
    (or (Ok? value) (Err? value)))

;; Contracts for Result
(define (Result/c ok-pred err-pred)
    (make/c (fn (x) 
                (cond [(Ok? x) (ok-pred (Ok-value x))]
                      [(Err? x) (err-pred (Err-value x))]
                      [else #f])) 
            'Result/c))

(define (map-option option func)
    (cond [(Some? option) (Some (func (Some-value option)))]
          [(None? option) (None)]))

;; (->/c Ok? any/c)
(define (unwrap-ok result)
    (Ok-value result))

;; (->/c Err? any/c)
(define (unwrap-err result)
    (Err-value result))

;; (->/c Result? (->/c any/c any/c) Result?)
(define (map-ok result func)
    (cond [(Ok? result) (Ok (func (Ok-value result)))]
          [(Err? result) result]))

;; (->/c Result? (->/c any/c any/c Result?))
(define (map-err result func)
    (cond [(Ok? result) result]
          [(Err? result) (Err (func (Err-value result)))]))

