
(define (split-last lst)
    (define (loop accum lst)
        (if (empty? (cdr lst))
            (list (reverse accum) (car lst))
            (loop (cons (car lst) accum) (cdr lst))))

    (loop '() lst))

(make-struct FlatContract (predicate name) #:transparent #true)

;; Alias the name for clarity
(define make-flat-contract FlatContract)

(make-struct FunctionContract (pre-conditions 
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

(make-struct ContractViolation (error-message))

(define (apply-flat-contract flat-contract arg)
    (if ((FlatContract-predicate flat-contract) arg)
        #true
        (ContractViolation 
            (to-string
                "Contract violation: found in the application of a flat contract for" 
                        (FlatContract-name flat-contract) 
                        ": the given input:" arg 
                        "resulted in a contract violation"))))


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

    (apply-parents (FunctionContract-parent 
        (ContractedFunction-contract contracted-function)))
    (apply-function-contract (ContractedFunction-contract contracted-function)
                             (ContractedFunction-name contracted-function)
                             (ContractedFunction-function contracted-function)
                             arguments))


(define (verify-preconditions self-contract arguments name)
    (transduce 
        arguments
        (zipping (FunctionContract-pre-conditions self-contract))
        (enumerating)
        (mapping 
            (lambda (x)
                (let ((i (first x))
                      (arg (first (second x)))
                      (contract (second (second x))))

                    (cond [(FlatContract? contract)
                            => 
                                (displayln "Applying flat contract in pre condition")
                                (let ((result (apply-flat-contract contract arg)))
                                    (if (ContractViolation? result)
                                        (error! "This function call caused an error"
                                        "- it occured in the domain position:" i ", with the contract: " contract (ContractViolation-error-message result) ", blaming " (FunctionContract-contract-attachment-location self-contract) "(callsite)")
                                        arg))]
                          [(FunctionContract? contract)
                            =>
                                (if (ContractedFunction? arg)
                                    (let ((pre-parent (ContractedFunction-contract arg)))
                                        (let ((parent (FunctionContract
                                                            (FunctionContract-pre-conditions pre-parent)
                                                            (FunctionContract-post-condition pre-parent)
                                                            (ContractedFunction-name arg)
                                                            void)))
                                            (let ((fc (FunctionContract 
                                                            (FunctionContract-pre-conditions contract)
                                                            (FunctionContract-post-condition contract)
                                                            (ContractedFunction-name arg)
                                                            parent)))

                                                (ContractedFunction contract
                                                                    arg
                                                                    name))))
                                    (ContractedFunction contract arg name))]
                        [else => (error! "Unexpected value in pre conditions: " contract)]
                                    
                                    ))))
        (into-list)))

(verify-preconditions 
    (make-function-contract 
        (FlatContract int? 'int?) 
        (FlatContract int? 'int?)
        (FlatContract boolean? 'boolean?))

    '(10 20)
    'test-function)

(define (apply-function-contract contract name function arguments)
    ;; Check that each of the arguments abides by the 
    (verify-preconditions contract arguments name)
    (let ((output (apply function arguments))
          (contract (FunctionContract-post-condition contract)))
        (cond [(FlatContract? contract)
                => 
                (displayln "aplying flat contract in post condition")
                
                (let ((result (apply-flat-contract contract output)))
                    (if (ContractViolation? result)
                        (let ((blame-location 
                                (if (void? (FunctionContract-contract-attachment-location contract))
                                    name
                                    (FunctionContract-contract-attachment-location contract))))
                            
                            (if blame-location
                                (error! 
                                    "this function call resulted in an error - occurred in the range position of this contract: " 
                                    contract result "blaming: "
                                     blame-location)
                                (error!
                                    "this function call resulted in an error - occured in the range position of this contract: " 
                                    contract result "blaming: None - broke its own contract")))
                        output))]
                [(FunctionContract? contract)
                  => (if (ContractedFunction? output)
                            (let ((pre-parent (ContractedFunction-contract output)))
                                (let ((parent (FunctionContract
                                                    (FunctionContract-pre-conditions pre-parent)
                                                    (FunctionContract-post-condition pre-parent)
                                                    (ContractedFunction-name output)
                                                    void)))
                                    (let ((fc (FunctionContract 
                                                    (FunctionContract-pre-conditions contract)
                                                    (FunctionContract-post-condition contract)
                                                    (ContractedFunction-name output)
                                                    parent)))
                                        (ContractedFunction contract
                                                            output
                                                            name))))
                            (ContractedFunction contract output name))]
                [else => (error! "Unhandled value in post condition: " contract)])))

(define (bind-contract-to-function contract function name)
    (lambda args
        (apply-contracted-function 
            (ContractedFunction contract function name)
            args)))

(define test-function
    (bind-contract-to-function 
        (make-function-contract
            (FlatContract int? 'int?)
            (FlatContract int? 'int?)
            (FlatContract boolean? 'boolean?))
        (lambda (x y) (equal? (+ x y) 10))
        'test-function))

(test-function 10 10)