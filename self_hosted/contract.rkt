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


(define (split-last lst)
    (define (loop accum lst)
        (if (empty? (cdr lst))
            (list (reverse accum) (car lst))
            (loop (cons (car lst) accum) (cdr lst))))

    (loop '() lst))

(make-struct FlatContract (predicate name) #:transparent #true)

;; Alias the name for clarity
(define make-flat-contract FlatContract)

(make-struct FunctionContract 
    (pre-conditions 
    post-condition 
    contract-attachment-location 
    parent)
    #:transparent #true)

(define make-function-contract
    (lambda conditions
        (let ((split (split-last conditions)))
            (FunctionContract (first split) (second split) void void))))



(make-struct DependentPair (argument-name arguments thunk thunk-name) #:transparent #true)

(make-struct 
    DependentContract 
        (arg-positions 
         pre-conditions post-condition
         contract-attachment-location
         parent)
    #:transparent #true)

; (make-function-contract 
;     (FlatContract int? 'int?) 
;     (FlatContract int? 'int?) 
;     (FlatContract boolean? 'boolean?))


(define (apply-flat-contract flat-contract arg)
    (unless ((FlatContract-predicate flat-contract) arg)
        (error! "Contract violation: found in the application of a flat contract for" 
                (FlatContract-name flat-contract) 
                ": the given input:" arg 
                "resulted in a contract violation")))


(make-struct ContractedFunction (contract function name))

;; Call a contracted function
(define (apply-contracted-function contracted-function arguments)

    ;; Recur upwards to keep track of the 
    (define (apply-parents parent)
        (if (void? parent)
            #true
            (begin
                (apply-function-contract (ContractedFunction-contract contracted-function)
                                         (ContractedFunction-name contracted-function) 
                                         (ContractedFunction-function contracted-function) 
                                         arguments)
                
                (apply-parents (FunctionContract-parent parent)))))

    (apply-parents (FunctionContract-parent contracted-function))
    (apply-function-contract (ContractedFunction-contract contracted-function)
                             (ContractedFunction-name contracted-function)
                             (ContractedFunction-function contracted-function)
                             arguments))

(define (verify-preconditions contract arguments name)
    (transduce 
        arguments
        (zipping (FunctionContract-pre-conditions contract))
        (enumerating)
        (mapping 
            (lambda (x)
                (let ((i (first x)
                      (arg (first (second x)))
                      (contract (second (second x)))))

                    (cond [(FlatContract? contract)
                            => 
                                (displayln "Applying flat contract in pre condition")
                                arg]
                          [(FunctionContract? contract)
                            =>
                                (if (ContractedFunction? arg)
                                    
                                
                                )
                          
                          
                          ]                         
                                
                                
                                )
        
        
            ))
        (into-list)
        
        ))

(verify-preconditions 
    (make-function-contract 
        (FlatContract int? 'int?) 
        (FlatContract int? 'int?)
        (FlatContract boolean? 'boolean?))

    '(10 20)
    'test-function)

(define (apply-function-contract contract name function arguments)
    (error! "Unimplemented"))
