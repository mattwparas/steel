(require-builtin "steel/core/result")

(provide
    Result? Ok Ok? Err Err?
    Result/c
    (contract/out unwrap-ok (->/c Ok? any/c))
    (contract/out unwrap-err (->/c Err? any/c))
    (contract/out map-ok (->/c Result? (->/c any/c any/c) Result?))
    (contract/out map-err (->/c Result? (->/c any/c any/c) Result?)))

; (struct Ok (value) #:transparent)
; (struct Err (value) #:transparent)

(define (Result? value)
    (or (Ok? value) (Err? value)))

;; Contracts for Result
(define (Result/c ok-pred err-pred)
    (make/c (fn (x) 
                (cond [(Ok? x) (ok-pred (Ok->value x))]
                      [(Err? x) (err-pred (Err->value x))]
                      [else #f])) 
            'Result/c))

;; (->/c Ok? any/c)
(define unwrap-ok Ok->value)

;; (->/c Err? any/c)
(define unwrap-err Err->value)

;; (->/c Result? (->/c any/c any/c) Result?)
(define (map-ok result func)
    (cond [(Ok? result) (Ok (func (Ok->value result)))]
          [(Err? result) result]))

;; (->/c Result? (->/c any/c any/c Result?))
(define (map-err result func)
    (cond [(Ok? result) result]
          [(Err? result) (Err (func (Err->value result)))]))

