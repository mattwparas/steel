(provide make-function-contract
         make-contract
         bind-contract-to-function
         FlatContract
         FlatContract?
         FlatContract-predicate
         FlatContract-name
         FunctionContract
         FunctionContract?
         FunctionContract-pre-conditions
         FunctionContract-post-condition
         contract->string
         (for-syntax ->c)
         (for-syntax define/c))

;; struct definitions
(struct FlatContract (predicate name) #:prop:procedure 0)
;; Contract Attachment - use this for understanding where something happened
(struct ContractAttachmentLocation (type name))

;; Function Contract - keep track of preconditions and post conditions, where
;; the contract was attached, and a pointer to the parent contract. Can probably
;; replace parent with just a list of the parents since it can be shared
;; directly
(struct FunctionContract (pre-conditions post-condition contract-attachment-location parents))

(struct DependentPair (argument-name arguments thunk thunk-name))

(struct DependentContract
        (arg-positions pre-conditions post-condition contract-attachment-location parent))

;; TODO: Raise error with contract violation directly attached, if possible
;;
(struct ContractViolation (error-message))

(struct ContractedFunction (contract function name))

;; Alias the name for clarity
(define make-flat-contract FlatContract)

;;#|
;;   Testing out a multi line comment...
;; |#
(define (new-FunctionContract #:pre-conditions pre-conditions
                              #:post-condition post-condition
                              #:contract-attachment-location (contract-attachment-location void)
                              ;; TODO: so this parents business isn't even practical
                              ;; -> it can get removed safely, maybe revisited later
                              #:parents (parents '()))
  (FunctionContract pre-conditions post-condition contract-attachment-location parents))

;; Formats a contract nicely as a string
(define (contract->string contract)
  (cond
    [(FlatContract? contract)
     =>
     (symbol->string (FlatContract-name contract))]
    [(FunctionContract? contract)
     =>
     (to-string "(->"
                (apply to-string
                       (transduce (FunctionContract-pre-conditions contract)
                                  (mapping contract->string)
                                  (into-list)))
                (contract->string (FunctionContract-post-condition contract))
                ")")]
    [else
     =>
     (error! "Unexpected value found in contract:" contract)]))

;; Given a list, splits off the last argument, returns as a pair
(define (split-last lst)
  (define (loop accum lst)
    (if (empty? (cdr lst)) (list (reverse accum) (car lst)) (loop (cons (car lst) accum) (cdr lst))))
  (loop '() lst))

;;@doc
;; Creates a `FunctionContract` from the list of conditions, splitting out the
;; preconditions and the postconditions
(define make-function-contract
  (lambda conditions
    (%plain-let ((split (split-last conditions)))
                (FunctionContract (first split) (second split) void '()))))

;; Applies a flat contract to the given argument
(define (apply-flat-contract flat-contract arg)
  ; ((FlatContract-predicate flat-contract) arg)
  (if (flat-contract arg)
      #true
      (ContractViolation
       (to-string "Contract violation: found in the application of a flat contract for"
                  (FlatContract-name flat-contract)
                  ": the given input:"
                  arg
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
  ; (displayln "Passed in span: " span)
  (define span (if span span '(0 0 0)))
  (apply-function-contract (ContractedFunction-contract contracted-function)
                           (ContractedFunction-name contracted-function)
                           (ContractedFunction-function contracted-function)
                           arguments
                           span))

;;@doc
;; Verifies the arguments against the FunctionContract, and then produces
;; a new list of arguments, with any arguments wrapped in function contracts if they happen
;; to be higher order
(define (verify-preconditions self-contract arguments name span)
  (unless (equal? (length arguments) (length (FunctionContract-pre-conditions self-contract)))
    (error-with-span span
                     "Arity mismatch, function expected "
                     (length (FunctionContract-pre-conditions self-contract))
                     "Found: "
                     (length arguments)))

  (transduce
   arguments
   (zipping (FunctionContract-pre-conditions self-contract))
   (enumerating)
   (mapping
    (lambda (x)
      (let ([i (first x)] [arg (first (second x))] [contract (second (second x))])

        (cond
          [(FlatContract? contract)
           =>

           (let ([result (apply-flat-contract contract arg)])
             (if (ContractViolation? result)
                 (error-with-span span
                                  "This function call caused an error"
                                  "- it occured in the domain position:"
                                  i
                                  ", with the contract: "
                                  (contract->string contract)
                                  (ContractViolation-error-message result)
                                  ", blaming "
                                  (ContractAttachmentLocation-name
                                   (FunctionContract-contract-attachment-location self-contract))
                                  "(callsite)")
                 arg))]
          [(FunctionContract? contract)
           =>
           (if (ContractedFunction? arg)
               (let ([pre-parent (ContractedFunction-contract arg)])
                 (let ([parent (new-FunctionContract
                                #:pre-conditions (FunctionContract-pre-conditions pre-parent)
                                #:post-condition (FunctionContract-post-condition pre-parent)
                                #:contract-attachment-location
                                (ContractAttachmentLocation 'DOMAIN (ContractedFunction-name arg))
                                #:parents (FunctionContract-parents pre-parent))])
                   (let ([fc (new-FunctionContract
                              #:pre-conditions (FunctionContract-pre-conditions contract)
                              #:post-condition (FunctionContract-post-condition contract)
                              #:contract-attachment-location
                              (ContractAttachmentLocation 'DOMAIN (ContractedFunction-name arg))
                              #:parents (cons parent (FunctionContract-parents parent)))])

                     (bind-contract-to-function fc arg name span))))
               (bind-contract-to-function contract arg name span))]
          [else
           =>
           (error! "Unexpected value in pre conditions: " contract)]))))
   (into-list)))

(define (apply-function-contract contract name function arguments span)
  ;; Check that each of the arguments abides by the
  (let ([validated-arguments (verify-preconditions contract arguments name span)])

    (let ([output (with-handler (lambda (err) (raise-error err))
                                (apply function validated-arguments))]

          [self-contract contract]
          [self-contract-attachment-location (FunctionContract-contract-attachment-location contract)]
          [contract (FunctionContract-post-condition contract)])

      (cond
        [(FlatContract? contract)
         =>

         (let ([result (apply-flat-contract contract output)])
           (if (ContractViolation? result)
               (let ([blame-location (if (void? self-contract-attachment-location)
                                         name
                                         self-contract-attachment-location)])

                 (cond
                   [(void? blame-location)
                    =>
                    (error-with-span
                     span
                     "this function call resulted in an error - occured in the range position of this contract: "
                     (contract->string self-contract)
                     (ContractViolation-error-message result)
                     "blaming: None - broke its own contract")]

                   [else
                    =>
                    (error-with-span
                     span
                     "this function call resulted in an error - occurred in the range position of this contract: "
                     (contract->string self-contract)
                     (ContractViolation-error-message result)
                     "blaming: "
                     blame-location)]))

               output))]
        [(FunctionContract? contract)
         =>

         (define original-function output)

         (if (FunctionContract? (get-contract-struct output))

             ;; TODO: Come back to this and understand what the heck its doing
             ;; Figured it out -> its never actually a contracted function, because we're wrapping
             ;; it directly in a normal function type.
             (begin
               (define output (get-contract-struct output))
               (define pre-parent contract)
               (define contract-attachment-location
                 (ContractAttachmentLocation 'RANGE
                                             (ContractAttachmentLocation-name
                                              self-contract-attachment-location)))
               (define parent
                 (new-FunctionContract #:pre-conditions (FunctionContract-pre-conditions pre-parent)
                                       #:post-condition (FunctionContract-post-condition pre-parent)
                                       #:contract-attachment-location contract-attachment-location
                                       #:parents (FunctionContract-parents pre-parent)))
               (define fc
                 (new-FunctionContract #:pre-conditions (FunctionContract-pre-conditions contract)
                                       #:post-condition (FunctionContract-post-condition contract)
                                       #:contract-attachment-location contract-attachment-location
                                       #:parents (cons parent (FunctionContract-parents pre-parent))))

               (bind-contract-to-function fc original-function name span))
             (bind-contract-to-function contract output name span))]
        [else
         =>
         (error! "Unhandled value in post condition: " contract)]))))

(define (bind-contract-to-function contract function name . span)
  (define post-condition (FunctionContract-post-condition contract))

  (let ([updated-preconditions
         (transduce
          (FunctionContract-pre-conditions contract)
          (mapping (lambda (c)
                     (cond
                       [(FlatContract? c)
                        =>
                        c]
                       [(FunctionContract? c)
                        =>
                        (FunctionContract (FunctionContract-pre-conditions c)
                                          (FunctionContract-post-condition c)
                                          (ContractAttachmentLocation 'DOMAIN name)
                                          (FunctionContract-parents c))]
                       [else
                        =>
                        (error "Unexpected value found in bind-contract-to-function preconditions: "
                               c)])))
          (into-list))]

        [updated-postcondition
         (cond
           [(FlatContract? post-condition)
            =>
            post-condition]
           [(FunctionContract? post-condition)
            =>

            (FunctionContract (FunctionContract-pre-conditions post-condition)
                              (FunctionContract-post-condition post-condition)
                              (ContractAttachmentLocation 'RANGE name)
                              (FunctionContract-parents post-condition))]
           [else
            =>

            (error "Unexpected value found in bind-contract-to-function post condition: "
                   post-condition)])])

    (let ([contracted-function
           (ContractedFunction (FunctionContract updated-preconditions
                                                 updated-postcondition
                                                 ;  void
                                                 ;  (ContractAttachmentLocation 'TOPLEVEL name)
                                                 ;  void
                                                 (ContractAttachmentLocation 'TOPLEVEL name)
                                                 (if (get-contract-struct function)
                                                     (cons (get-contract-struct function)
                                                           (FunctionContract-parents contract))
                                                     (FunctionContract-parents contract)))
                               function
                               name)])

      (let ([resulting-lambda-function
             (lambda args

               (apply-contracted-function
                contracted-function
                args
                ; span
                ; (current-function-span)
                (if span (car span) (current-function-span))
                ;          (begin (displayln ("Current span: " (current-function-span)))
                ;          (current-function-span)))
                ))])
        (attach-contract-struct! resulting-lambda-function
                                 (ContractedFunction-contract contracted-function))
        resulting-lambda-function))))

(define (make-contract contract name)
  (cond
    [(FlatContract? contract) contract]
    [(FunctionContract? contract) contract]
    [else
     =>
     (FlatContract contract name)]))

(define-syntax ->c
  (syntax-rules ()
    [(->c r) (make-function-contract (make-contract r 'r))]
    [(->c a b) (make-function-contract (make-contract a 'a) (make-contract b 'b))]
    [(->c a b c)
     (make-function-contract (make-contract a 'a) (make-contract b 'b) (make-contract c 'c))]
    [(->c a b c d)
     (make-function-contract (make-contract a 'a)
                             (make-contract b 'b)
                             (make-contract c 'c)
                             (make-contract d 'd))]
    [(->c a b c d e)
     (make-function-contract (make-contract a 'a)
                             (make-contract b 'b)
                             (make-contract c 'c)
                             (make-contract d 'd)
                             (make-contract e 'e))]
    [(->c a b c d e f)
     (make-function-contract (make-contract a 'a)
                             (make-contract b 'b)
                             (make-contract c 'c)
                             (make-contract d 'd)
                             (make-contract e 'e)
                             (make-contract f 'f))]
    [(->c a b c d e f g)
     (make-function-contract (make-contract a 'a)
                             (make-contract b 'b)
                             (make-contract c 'c)
                             (make-contract d 'd)
                             (make-contract e 'e)
                             (make-contract f 'f)
                             (make-contract g 'g))]
    [(->c a b c d e f g h)
     (make-function-contract (make-contract a 'a)
                             (make-contract b 'b)
                             (make-contract c 'c)
                             (make-contract d 'd)
                             (make-contract e 'e)
                             (make-contract f 'f)
                             (make-contract g 'g)
                             (make-contract h 'h))]
    [(->c a b c d e f g h i)
     (make-function-contract (make-contract a 'a)
                             (make-contract b 'b)
                             (make-contract c 'c)
                             (make-contract d 'd)
                             (make-contract e 'e)
                             (make-contract f 'f)
                             (make-contract g 'g)
                             (make-contract h 'h)
                             (make-contract i 'i))]))

;; Macro for basic usage of contracts
(define-syntax define/c
  (syntax-rules ()
    [(define/c (name args ...)
       contract
       body ...)
     (begin
       (define name
         (lambda (args ...)
           body ...))
       (set! name (bind-contract-to-function contract name 'name))
       void)
     ;  (define name (bind/c contract (lambda (args ...) body ...) 'name))
     ]
    [(define/c name
       contract
       expr)
     (define name
       ((bind-contract-to-function (make-function-contract (make-contract contract 'contract))
                                   (lambda () expr))))]))

(provide (for-syntax contract/out/test))

(define-syntax contract/out/test
  (syntax-rules ()
    [(contract/out/test name contract)
     (%require-ident-spec name (bind-contract-to-function contract name 'name))]))
