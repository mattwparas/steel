; (steel/base)

(require-builtin "steel/core/result")

;; This should get preloaded at the top of every require, except the built ins!
(require-builtin steel/base)
(require "#%private/steel/contract"
         (for-syntax "#%private/steel/contract"))

(provide Result?
         Ok
         Ok?
         Err
         Err?
         Result/c
         ; (contract/out unwrap-ok (->/c Ok? any/c))
         unwrap-ok
         (contract/out/test unwrap-err (->/c Err? any/c))
         (contract/out/test map-ok (->/c Result? (->/c any/c any/c) Result?))
         (contract/out/test map-err (->/c Result? (->/c any/c any/c) Result?))
         unwrap-or
         ok-and-then)

; (struct Ok (value) #:transparent)
; (struct Err (value) #:transparent)

(define (Result? value)
  (or (Ok? value) (Err? value)))

;; Contracts for Result
(define (Result/c ok-pred err-pred)
  (make/c (fn (x)
              (cond
                [(Ok? x) (ok-pred (Ok->value x))]
                [(Err? x) (err-pred (Err->value x))]
                [else #f]))
          'Result/c))

;; (->/c Ok? any/c)
(define unwrap-ok Ok->value)

;; (->/c Err? any/c)
(define unwrap-err Err->value)

;; (->/c Result? (->/c any/c any/c) Result?)
(define (map-ok result func)
  (cond
    [(Ok? result) (Ok (func (Ok->value result)))]
    [(Err? result) result]))

(define (ok-and-then result func)
  (cond
    [(Ok? result) (func (Ok->value result))]
    [(Err? result) result]))

;; (->/c Result? (->/c any/c any/c Result?))
(define (map-err result func)
  (cond
    [(Ok? result) result]
    [(Err? result) (Err (func (Err->value result)))]))

(define (unwrap-or result value)
  (if (Ok? result) (Ok->value result) value))

; (define-syntax contract/out/test
;   (syntax-rules ()
;     [(contract/out/test name contract)
;      (%require-ident-spec name (bind-contract-to-function contract name 'name))]))
