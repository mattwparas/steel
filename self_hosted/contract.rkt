(provide
    make-function-contract
    bind-contract-to-function
    FlatContract
    FlatContract-predicate
    FunctionContract)

(new-make-struct FlatContract (predicate name))

;; Alias the name for clarity
(define make-flat-contract FlatContract)

;; Contract Attachment - use this for understanding where something happened
(new-make-struct ContractAttachmentLocation (type name))

;; Function Contract - keep track of preconditions and post conditions, where the contract was attached,
;; and a pointer to the parent contract. Can probably replace parent with just a list of the parents since it can be shared
;; directly
(new-make-struct FunctionContract (pre-conditions
                                   post-condition
                                   contract-attachment-location
                                   parents))

;; (define (new-FunctionContract #:pre-conditions pre-conditions
;;                               #:post-condition post-condition
;;                               #:contract-attachment-location (contract-attachment-location void)
;;                               #:parents

;;                               ))

(new-make-struct DependentPair (argument-name arguments thunk thunk-name))

(new-make-struct DependentContract (arg-positions
                                    pre-conditions post-condition
                                    contract-attachment-location
                                    parent))

(new-make-struct ContractViolation (error-message))

(new-make-struct ContractedFunction (contract function name))

(define (contract->string contract)
  (cond [(FlatContract? contract) => (symbol->string (FlatContract-name contract))]
        [(FunctionContract? contract) =>
                                      (to-string
                                       "(->" (apply
                                              to-string
                                              (transduce (FunctionContract-pre-conditions contract)
                                                         (mapping contract->string)
                                                         (into-list)))

                                       (contract->string (FunctionContract-post-condition contract))
                                       ")")]
        [else => (error! "Unexpected value found in contract:" contract)]))


(define (split-last lst)
  (define (loop accum lst)
    (if (empty? (cdr lst))
        (list (reverse accum) (car lst))
        (loop (cons (car lst) accum) (cdr lst))))
  (loop '() lst))

(define make-function-contract
  (lambda conditions
    (let ((split (split-last conditions)))
      (FunctionContract (first split) (second split) void '()))))





(define (apply-flat-contract flat-contract arg)
  (if ((FlatContract-predicate flat-contract) arg)
      #true
      (ContractViolation
       (to-string
        "Contract violation: found in the application of a flat contract for"
        (FlatContract-name flat-contract)
        ": the given input:" arg
        "resulted in a contract violation"))))





; (define (apply-parents parent name function arguments span)
;   (if (void? parent)
;       #true
;       (begin
;         (displayln "Applying parent contract")
;         (apply-function-contract (ContractedFunction-contract parent)
;                                  name
;                                  function
;                                  arguments
;                                  span)

;         (apply-parents (FunctionContract-parent parent) name function arguments span))))

;; Call a contracted function
(define (apply-contracted-function contracted-function arguments span)

  (define span (if span span '(0 0)))

  (displayln "Applying contracted function")
;   (displayln contracted-function)
;   (displayln "")
;   (displayln "Parents:")
;   (displayln (FunctionContract-parents (ContractedFunction-contract contracted-function)))

;   (displayln contracted-function)

  (transduce 
    (FunctionContract-parents (ContractedFunction-contract contracted-function))
    (into-for-each 
        (lambda (x)
            (displayln "Checking parent contracts")
            (apply-function-contract
                (ContractedFunction-contract x)
                (ContractedFunction-name contracted-function)
                (ContractedFunction-function contracted-function)
                arguments
                span))))

;   (let ((parent (FunctionContract-parent
;                  (ContractedFunction-contract contracted-function))))
;     (when parent
;       (apply-parents
;        parent
;        (ContractedFunction-name contracted-function)
;        (ContractedFunction-function contracted-function)
;        arguments
;        span)))

  ; (apply-parents (FunctionContract-parent
  ; (ContractedFunction-contract contracted-function)))
  (apply-function-contract (ContractedFunction-contract contracted-function)
                           (ContractedFunction-name contracted-function)
                           (ContractedFunction-function contracted-function)
                           arguments
                           span))


(define (verify-preconditions self-contract arguments name span)
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
                ;  (displayln result)
                ;  (displayln (FunctionContract-contract-attachment-location self-contract))
                 (if (ContractViolation? result)
                     (error-with-span span
                                      "This function call caused an error"
                                      "- it occured in the domain position:" i ", with the contract: "
                                      (contract->string contract)
                                      (ContractViolation-error-message result)
                                      ", blaming "
                                      (ContractAttachmentLocation-name
                                       (FunctionContract-contract-attachment-location self-contract))
                                      "(callsite)")
                     arg))]
              [(FunctionContract? contract)
               =>
               (displayln "Wrapping contract")
               (if (ContractedFunction? arg)
                   (let ((pre-parent (ContractedFunction-contract arg)))
                     (let ((parent (FunctionContract
                                    (FunctionContract-pre-conditions pre-parent)
                                    (FunctionContract-post-condition pre-parent)
                                    (ContractAttachmentLocation
                                     'DOMAIN
                                     (ContractedFunction-name arg))
                                    (FunctionContract-parents pre-parent)

                                    )
                                    
                                    
                                    ))
                       (let ((fc (FunctionContract
                                  (FunctionContract-pre-conditions contract)
                                  (FunctionContract-post-condition contract)
                                  (ContractAttachmentLocation
                                   'DOMAIN
                                   (ContractedFunction-name arg))
                                  (cons parent (FunctionContract-parents parent)))))

                         (bind-contract-to-function fc arg name span))))
                   (bind-contract-to-function contract arg name span))]
              [else => (error! "Unexpected value in pre conditions: " contract)]

              ))))
   (into-list)))

; (verify-preconditions 
;     (make-function-contract 
;         (FlatContract int? 'int?) 
;         (FlatContract int? 'int?)
;         (FlatContract boolean? 'boolean?))

;     '(10 20)
;     'test-function)

(define (apply-function-contract contract name function arguments span)

  (displayln contract)

  ;; Check that each of the arguments abides by the
  (let ((validated-arguments (verify-preconditions contract arguments name span)))
    ; (displayln "Calling apply - Applying function")
    ; (displayln function)
    ; (displayln validated-arguments)
    (let ((output (apply function validated-arguments))
          (self-contract contract)
          (self-contract-attachment-location (FunctionContract-contract-attachment-location contract))
          (contract (FunctionContract-post-condition contract)))
      (cond [(FlatContract? contract)
             =>
             (displayln "applying flat contract in post condition")
             (displayln contract)
             (displayln function)

             (let ((result (apply-flat-contract contract output)))
               (displayln "OUTPUT")
               (displayln output)
               (if (ContractViolation? result)
                   (let ((blame-location
                          (if (void? self-contract-attachment-location)
                              name
                              self-contract-attachment-location)))

                    ;  (displayln self-contract-attachment-location)

                     (cond [(void? blame-location) =>
                                                   (error-with-span
                                                    span
                                                    "this function call resulted in an error - occured in the range position of this contract: "
                                                    (contract->string self-contract) (ContractViolation-error-message result) "blaming: None - broke its own contract")]

                           [else =>
                                 (error-with-span
                                  span
                                  "this function call resulted in an error - occurred in the range position of this contract: "
                                  (contract->string self-contract) (ContractViolation-error-message result) "blaming: "
                                  (ContractAttachmentLocation-name blame-location))

                                 ]

                           ;   [(equal? (ContractAttachmentLocation-type blame-location) 'DOMAIN)
                           ;     =>
                           ;     (displayln "occurred in the domain position")
                           ;     (error-with-span
                           ;         span
                           ;         "this function call resulted in an error - occurred in the range position of this contract: "
                           ;         (contract->string self-contract) (ContractViolation-error-message result) "blaming: "
                           ;         blame-location)]

                           ;   [(equal? (ContractAttachmentLocation-type blame-location) 'RANGE)
                           ;     =>
                           ;     (error-with-span
                           ;         span
                           ;         "this function call resulted in an error - occurred in the range position of this contract: "
                           ;         (contract->string self-contract) (ContractViolation-error-message result) "blaming: "
                           ;         blame-location)]

                           ;   [else => (error! "Unexpected value found when assigning blame")]


                           ))

                   output))]
            [(FunctionContract? contract)
             => 
             (displayln "Wrapping contract in post condition")
             (displayln output)
             (if (ContractedFunction? output)
                    (let ((pre-parent (ContractedFunction-contract output)))
                      (let ((parent (FunctionContract
                                     (FunctionContract-pre-conditions pre-parent)
                                     (FunctionContract-post-condition pre-parent)
                                     (ContractedFunction-name output)
                                     (FunctionContract-parents pre-parent)
                                     
                                     )))
                        (let ((fc (FunctionContract
                                   (FunctionContract-pre-conditions contract)
                                   (FunctionContract-post-condition contract)
                                   (ContractAttachmentLocation
                                    'RANGE
                                    (ContractedFunction-name output))
                                   (cons parent (FunctionContract-parents pre-parent)))))
                          (bind-contract-to-function fc output name span))))
                    (bind-contract-to-function contract output name span))]
            [else => (error! "Unhandled value in post condition: " contract)]))))

(define (bind-contract-to-function contract function name . span)
  (define post-condition (FunctionContract-post-condition contract))
;   (displayln contract)
  ; (displayln (FunctionContract-pre-conditions contract))
  ; (displayln (FunctionContract-post-condition contract))
;   (displayln name)
  (let ((updated-preconditions
         (transduce
          (FunctionContract-pre-conditions contract)
          (mapping (lambda (c)
                     (cond [(FlatContract? c) => c]
                           [(FunctionContract? c) =>
                                                  (FunctionContract
                                                   (FunctionContract-pre-conditions c)
                                                   (FunctionContract-post-condition c)
                                                   (ContractAttachmentLocation 'DOMAIN name)
                                                   (FunctionContract-parents c))]
                           [else => (error "Unexpected value found in bind-contract-to-function" c)])))
          (into-list)))

        (updated-postcondition
         (cond [(FlatContract? post-condition) => post-condition]
               [(FunctionContract? post-condition) =>
                                                   (FunctionContract
                                                    (FunctionContract-pre-conditions post-condition)
                                                    (FunctionContract-post-condition post-condition)
                                                    (ContractAttachmentLocation 'RANGE name)
                                                    (FunctionContract-parents post-condition))]
               [else => (error "Unexpected value found in bind-contract-to-function" post-condition)])))

    (displayln "Binding contract to function")
    ; (displayln updated-preconditions)
    ; (displayln updated-postcondition)

    (displayln (FunctionContract-parents contract))

    (let ((contracted-function
           (ContractedFunction
            (FunctionContract
             updated-preconditions
             updated-postcondition
             (ContractAttachmentLocation 'TOPLEVEL name)
             (FunctionContract-parents contract))
            function name)))

      (lambda args
        ; (displayln args)
        (apply-contracted-function 
            contracted-function 
            args
            (if span (car span) (current-function-span)))))))




(define test-function
    (bind-contract-to-function 
        (make-function-contract
            (FlatContract int? 'int?)
            (FlatContract int? 'int?)
            (FlatContract boolean? 'boolean?))
        (lambda (x y) (equal? (+ x y) 10))
        'test-function))

(test-function 5 5)
; (test-function "applesauce" 5)

; (test-function "hello world" 10)

; (define foo
;     (lambda (x) 
;         (if (= x 100)
;             x
;             (foo (+ x 1)))))

; (define bar
;     (lambda (x) 
;         (if (= x 100)
;             x
;             (foo (+ x 1)))))

; ; (set! foo foo)

; (set! foo
;     (bind-contract-to-function
;         (make-function-contract
;             (FlatContract int? 'int?)
;             (FlatContract int? 'int?))
;         foo
;         'foo))


; (set! bar
;     (bind-contract-to-function
;         (make-function-contract
;             (FlatContract int? 'int?)
;             (FlatContract int? 'int?))
;         bar
;         'bar))


; (define blagh 
;     (bind-contract-to-function
;         (make-function-contract
;             (make-function-contract (FlatContract even? 'even?) (FlatContract odd? 'odd?))
;             (FlatContract even? 'even?)
;             (FlatContract even? 'even?))
;         (lambda (func y) (+ 1 (func y)))
;         'blagh))

; (blagh (lambda (x) (+ x 2)) 2)


; (define (any? x) #true)
; (define level1 
;     (bind-contract-to-function
;         (make-function-contract
;             (make-function-contract (FlatContract int? 'int?)))
;         (lambda () (lambda () (displayln "---------------------------------------") 10.2))
;         'level1))

; (define level2
;     (bind-contract-to-function
;         (make-function-contract
;             (make-function-contract (FlatContract number? 'number?)))
;         (lambda () (level1))
;         'level2))

; (define level3
;     (bind-contract-to-function
;         (make-function-contract
;             (make-function-contract (FlatContract any? 'any?)))
;         (lambda () (level2))
;         'level3))

; ((level3))

(define x 0)

(define plain-function (lambda () (displayln "CALLING PLAIN FUNCTION") 10.2))

(define level1 
    (bind-contract-to-function
        (make-function-contract (make-function-contract (FlatContract number? 'number?))
                                (make-function-contract (FlatContract number? 'number?)))
    (lambda (func) func)
    'level1))

(define level2
    (bind-contract-to-function
        (make-function-contract (make-function-contract (FlatContract number? 'number?))
                                (make-function-contract (FlatContract number? 'number?)))
    (lambda (func) func)
    'level2))

((level2 (level2 (level1 plain-function))))
