(provide
 make-function-contract
 bind-contract-to-function
 FlatContract
 FlatContract-predicate
 FunctionContract
 (for-syntax ->c)
 (for-syntax define/c))

;;struct definitions
(struct FlatContract (predicate name))
;; Contract Attachment - use this for understanding where something happened
(struct ContractAttachmentLocation (type name))

;; Function Contract - keep track of preconditions and post conditions, where
;; the contract was attached, and a pointer to the parent contract. Can probably
;; replace parent with just a list of the parents since it can be shared
;; directly
(struct FunctionContract (pre-conditions
                          post-condition
                          contract-attachment-location
                          parents))


(struct DependentPair (argument-name arguments thunk thunk-name))

(struct DependentContract (arg-positions
                           pre-conditions post-condition
                           contract-attachment-location
                           parent))

;; TODO: Raise error with contract violation directly attached, if possible
;;
(struct ContractViolation (error-message))

(struct ContractedFunction (contract function name))

;; Alias the name for clarity
(define make-flat-contract FlatContract)



;;#|
;;   Testing out a multi line comment...
;; |#
(define (new-FunctionContract
         #:pre-conditions pre-conditions
         #:post-condition post-condition
         #:contract-attachment-location (contract-attachment-location void)
         ;; TODO: so this parents business isn't even practical
         ;; -> it can get removed safely, maybe revisited later
         #:parents (parents '()))
  (FunctionContract pre-conditions
                    post-condition
                    contract-attachment-location
                    parents))



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
    (%plain-let ((split (split-last conditions)))
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





;; ; (define (apply-parents parent name function arguments span)
;; ;   (if (void? parent)
;; ;       #true
;; ;       (begin
;; ;         (displayln "Applying parent contract")
;; ;         (apply-function-contract (ContractedFunction-contract parent)
;; ;                                  name
;; ;                                  function
;; ;                                  arguments
;; ;                                  span)

;; ;         (apply-parents (FunctionContract-parent parent) name function arguments span))))

;; Call a contracted function
(define (apply-contracted-function contracted-function arguments span)

  (define span (if span span '(0 0)))

  ; (displayln "Applying contracted function")
  ; (displayln (ContractedFunction-name contracted-function))
  ; (displayln arguments)
  ; (displayln "Parents:")
  ; (displayln (FunctionContract-parents (ContractedFunction-contract contracted-function)))

                                        ;   (displayln contracted-function)

  (transduce
   (FunctionContract-parents (ContractedFunction-contract contracted-function))
   (into-for-each
    (lambda (x)
      ; (displayln "Checking parent contracts")
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
  ; (displayln arguments)
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
              ;  (displayln "Applying flat contract in pre condition")
              ;  (displayln x)
              ;  (displayln arg)

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
              ;  (displayln "Wrapping contract in precondition")
              ;  (displayln arg)
               (if (ContractedFunction? arg)
                   (let ((pre-parent (ContractedFunction-contract arg)))
                     (let ((parent (new-FunctionContract
                                    #:pre-conditions (FunctionContract-pre-conditions pre-parent)
                                    #:post-condition (FunctionContract-post-condition pre-parent)
                                    #:contract-attachment-location
                                    (ContractAttachmentLocation 'DOMAIN (ContractedFunction-name arg))
                                    #:parents (FunctionContract-parents pre-parent))))
                       (let ((fc (new-FunctionContract
                                  #:pre-conditions (FunctionContract-pre-conditions contract)
                                  #:post-condition (FunctionContract-post-condition contract)
                                  #:contract-attachment-location
                                  (ContractAttachmentLocation 'DOMAIN (ContractedFunction-name arg))
                                  #:parents (cons parent (FunctionContract-parents parent)))))

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
  ; (displayln "--------------------- APPLY FUNCTION CONTRACT ------------------")

  ; (displayln contract)
  ; (displayln name)
  ; (displayln function)
  ; (displayln arguments)
  ; (displayln span)

  ;; Check that each of the arguments abides by the
  (let ((validated-arguments (verify-preconditions contract arguments name span)))
                                        ; (displayln "Calling apply - Applying function")
                                        ; (displayln function)
                                        ; (displayln validated-arguments)
                                        ; (displayln "Calling the function itself!")
    (let ((output (apply function validated-arguments))
          (self-contract contract)
          (self-contract-attachment-location (FunctionContract-contract-attachment-location contract))
          (contract (FunctionContract-post-condition contract)))
      (cond [(FlatContract? contract)
             =>
            ;  (displayln "applying flat contract in post condition")
            ;  (displayln (FlatContract-name contract))
            ;  (displayln contract)
                                        ;  (displayln function)

             (let ((result (apply-flat-contract contract output)))
               (if (ContractViolation? result)
                   (let ((blame-location
                          (if (void? self-contract-attachment-location)
                              name
                              self-contract-attachment-location)))


                     (cond [(void? blame-location) =>
                            (error-with-span
                             span
                             "this function call resulted in an error - occured in the range position of this contract: "
                             (contract->string self-contract) (ContractViolation-error-message result) "blaming: None - broke its own contract")]

                           [else =>
                                 (displayln "Inside here!")
                                 (error-with-span
                                  span
                                  "this function call resulted in an error - occurred in the range position of this contract: "
                                  (contract->string self-contract) (ContractViolation-error-message result) "blaming: "
                                  blame-location)

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
            ;  (displayln "Wrapping contract in post condition")
            ;  (displayln contract)
            ;  (displayln output)
             (if (ContractedFunction? output)
                 ;; TODO: Come back to this and understand what the heck its doing
                 ;; Figured it out -> its never actually a contracted function, because we're wrapping
                 ;; it directly in a normal function type.
                 (begin
                   (define pre-parent (ContractedFunction-contract output))
                   (define contract-attachment-location (ContractAttachmentLocation
                                                         'RANGE
                                                         (ContractedFunction-name output)))
                   (define parent (new-FunctionContract
                                   #:pre-conditions (FunctionContract-pre-conditions pre-parent)
                                   #:post-condition (FunctionContract-post-condition pre-parent)
                                   #:contract-attachment-location contract-attachment-location
                                   #:parents (FunctionContract-parents pre-parent)))
                   (define fc (new-FunctionContract
                               #:pre-conditions (FunctionContract-pre-conditions contract)
                               #:post-condition (FunctionContract-post-condition contract)
                               #:contract-attachment-location contract-attachment-location
                               #:parents (cons parent (FunctionContract-parents pre-parent))))

                   (bind-contract-to-function fc output name span))
                 (bind-contract-to-function contract output name span))]
            [else => (error! "Unhandled value in post condition: " contract)]))))

(define (bind-contract-to-function contract function name . span)
  ; (displayln "Binding contract to function")
  (define post-condition (FunctionContract-post-condition contract))
  ; (displayln post-condition)
  ; (displayln contract)
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
                           [else => (error "Unexpected value found in bind-contract-to-function preconditions: " c)])))
          (into-list)))

        (updated-postcondition
        ;  (begin 
        ;  (displayln post-condition)
         (cond [(FlatContract? post-condition) => post-condition]
               [(FunctionContract? post-condition) =>
                (FunctionContract
                 (FunctionContract-pre-conditions post-condition)
                 (FunctionContract-post-condition post-condition)
                 (ContractAttachmentLocation 'RANGE name)
                 (FunctionContract-parents post-condition))]
               [else => 
               
              ;  (displayln post-condition)
               
               (error "Unexpected value found in bind-contract-to-function post condition: " post-condition)])))

                                        ; (displayln "Binding contract to function")
                                        ; (displayln updated-preconditions)
                                        ; (displayln updated-postcondition)

                                        ; (displayln (FunctionContract-parents contract))

    (let ((contracted-function
           (ContractedFunction
            (FunctionContract
             updated-preconditions
             updated-postcondition
                                        ;  void
                                        ;  (ContractAttachmentLocation 'TOPLEVEL name)
            ;  void
             (ContractAttachmentLocation 'TOPLEVEL name)
             (FunctionContract-parents contract))
            function name)))

      (lambda args
                                        ; (displayln args)
        ; (displayln "CALLING WRAPPED FUNCTION")
        ; (displayln args)
        (apply-contracted-function
         contracted-function
         args
         (if span (car span) (current-function-span)))))))




;; ; (define test-function
;; ;     (bind-contract-to-function
;; ;         (make-function-contract
;; ;             (FlatContract int? 'int?)
;; ;             (FlatContract int? 'int?)
;; ;             (FlatContract boolean? 'boolean?))
;; ;         (lambda (x y) (equal? (+ x y) 10))
;; ;         'test-function))

;; ; (test-function 5 5)
;; ; (test-function "applesauce" 5)

;; ; (test-function "hello world" 10)

;; ; (define foo
;; ;     (lambda (x)
;; ;         (if (= x 100)
;; ;             x
;; ;             (foo (+ x 1)))))

;; ; (define bar
;; ;     (lambda (x)
;; ;         (if (= x 100)
;; ;             x
;; ;             (foo (+ x 1)))))

;; ; ; (set! foo foo)

;; ; (set! foo
;; ;     (bind-contract-to-function
;; ;         (make-function-contract
;; ;             (FlatContract int? 'int?)
;; ;             (FlatContract int? 'int?))
;; ;         foo
;; ;         'foo))


;; ; (set! bar
;; ;     (bind-contract-to-function
;; ;         (make-function-contract
;; ;             (FlatContract int? 'int?)
;; ;             (FlatContract int? 'int?))
;; ;         bar
;; ;         'bar))


; (define blagh
;      (bind-contract-to-function
;          (make-function-contract
;              (make-function-contract (FlatContract even? 'even?) (FlatContract odd? 'odd?))
;              (FlatContract even? 'even?)
;              (FlatContract even? 'even?))
;          (lambda (func y) (+ 1 (func y)))
;          'blagh))

(define (make-contract contract name)
  (cond [(FlatContract? contract) contract]
        [else => (FlatContract contract name)]))

(define-syntax ->c
  (syntax-rules ()
    [(->c r)
     (make-function-contract (make-contract r 'r))]
    [(->c a b)
     (make-function-contract (make-contract a 'a) (make-contract b 'b))]
    [(->c a b c)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c))]
    [(->c a b c d)
     (make-function-contract (make-contract a 'a) (make-contract b 'b)
                      (make-contract c 'c) (make-contract d 'd))]
    [(->c a b c d e)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c)
                      (make-contract d 'd) (make-contract e 'e))]
    [(->c a b c d e f)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c)
                      (make-contract d 'd) (make-contract e 'e) (make-contract f 'f))]
    [(->c a b c d e f g)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c)
                      (make-contract d 'd) (make-contract e 'e) (make-contract f 'f)
                      (make-contract g 'g))]
    [(->c a b c d e f g h)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c)
                      (make-contract d 'd) (make-contract e 'e) (make-contract f 'f)
                      (make-contract g 'g) (make-contract h 'h))]
    [(->c a b c d e f g h i)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c)
                      (make-contract d 'd) (make-contract e 'e) (make-contract f 'f)
                      (make-contract g 'g) (make-contract h 'h) (make-contract i 'i))]))

;; Macro for basic usage of contracts
(define-syntax define/c
  (syntax-rules ()
    [(define/c (name args ...)
       contract
       body ...)
     (begin
        (define name (lambda (args ...) body ...))
        (set! name (bind-contract-to-function contract name 'name))
        void)
    ;  (define name (bind/c contract (lambda (args ...) body ...) 'name))

     ]
    [(define/c name contract expr)
     (define name ((bind-contract-to-function
                      (make-function-contract (make-contract contract 'contract))
                      (lambda () expr))))]))

; (define/c (blagh x)
;   (->c string? string?)
;   x)

(define/c (foo x y)
  (->c even? odd? odd?)
  (+ x y))

; (foo 11 11)






;; ; (blagh (lambda (x) (+ x 2)) 2)


;; ; (define (any? x) (displayln "***** CHECKING ANY? *****") #true)

;; ; (define (int-checker? x) (displayln "***** CHECKING INT? ******") (int? x))
;; ; (define (number-checker? x) (displayln "***** CHECKING NUMBER? ******") (number? x))

;; ; (define level1
;; ;     (bind-contract-to-function
;; ;         (make-function-contract
;; ;             (make-function-contract (FlatContract number-checker? 'number-checker?)))
;; ;         (lambda () (lambda () (displayln "@@@@@@@@@@ CALLING FUNCTION @@@@@@@@@@@") 10.2))
;; ;         'level1))

;; ; (define level2
;; ;     (bind-contract-to-function
;; ;         (make-function-contract
;; ;             (make-function-contract (FlatContract int-checker? 'int-checker)))
;; ;         (lambda () (level1))
;; ;         'level2))

;; ; (define level3
;; ;     (bind-contract-to-function
;; ;         (make-function-contract
;; ;             (make-function-contract (FlatContract any? 'any?)))
;; ;         (lambda () (level2))
;; ;         'level3))

;; ; ((level3))



;; ; (define plain-function (lambda () (displayln "----CALLING PLAIN FUNCTION----") 10.2))

;; ; (define level1
;; ;     (bind-contract-to-function
;; ;         (make-function-contract (make-function-contract (FlatContract number-checker? 'number-checker?))
;; ;                                 (make-function-contract (FlatContract number-checker? 'number-checker?)))
;; ;     (lambda (func) func)
;; ;     'level1))

;; ; (define level2
;; ;     (bind-contract-to-function
;; ;         (make-function-contract (make-function-contract (FlatContract int-checker? 'int-checker?))
;; ;                                 (make-function-contract (FlatContract int-checker? 'int-checker?)))
;; ;     (lambda (func) func)
;; ;     'level2))

;; ; ((level2 (level1 (level1 (level1 plain-function)))))


;; ; (define (int-checker? x) (displayln "***** CHECKING INT? ******") (integer? x))
;; ; (define (number-checker? x) (displayln "***** CHECKING NUMBER? ******") (number? x))


;; ; (define plain-function (lambda () (displayln "----CALLING PLAIN FUNCTION-----") 10.2))
;; ; (define/contract (level1 func)
;; ;   (-> (-> number-checker?) (-> number-checker?))
;; ;   func)
