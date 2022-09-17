; (define (raise-contract))

; ;; Wrap function to check preconditions and post conditions
; (define (wrap-function contract function)
;     ;; Construct a function that contains the pre conditions
;     ;; and post conditions
;     (lambda args
;         (check-arity function args)
;         (check-preconditions function contract args)
;         ()
    
;     )


; )

(make-struct FlatContract
    predicate
    name)

(make-struct FunctionContract 
    pre-conditions 
    post-condition 
    contract-attachment-location 
    parent)

(make-struct DependentPair
    argument-name
    arguments
    thunk
    thunk-name)

(make-struct DependentContract
    arg-positions
    pre-conditions
    post-condition
    contract-attachment-location
    parent)

