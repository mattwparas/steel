(provide make-function/c
         make-rest-function/c
         make/c
         bind/c
         FlatContract
         FlatContract?
         FlatContract-predicate
         FlatContract-name
         FunctionContract
         FunctionContract?
         FunctionContract-pre-conditions
         FunctionContract-post-condition
         FunctionContract-rest
         CombinedPredicate?
         CombinedPredicate-kind
         CombinedPredicate-parts
         contract->string
         #%contract-result-violation
         #%contract-attach
         (for-syntax ->/c)
         (for-syntax ->*/c)
         (for-syntax define/contract))

(require-builtin steel/base)

;; struct definitions
(struct FlatContract (predicate name)
  #:prop:procedure 0
  #:printer (lambda (obj printer-function) (printer-function (contract->string obj))))
;; Contract Attachment - use this for understanding where something happened
(struct ContractAttachmentLocation (type name))

;; Function Contract - keep track of preconditions and post conditions, where
;; the contract was attached, and a pointer to the parent contract. Can probably
;; replace parent with just a list of the parents since it can be shared
;; directly. `rest` is the contract on the list of the arguments past the
;; pre-conditions, or #f when the function takes no others.
(struct FunctionContract (pre-conditions post-condition contract-attachment-location parents rest)
  #:printer (lambda (obj printer-function) (printer-function (contract->string obj))))

(struct DependentPair (argument-name arguments thunk thunk-name))

(struct DependentContract
        (arg-positions pre-conditions post-condition contract-attachment-location parent))

;; TODO: Raise error with contract violation directly attached, if possible
;;
(struct ContractViolation (error-message))

(struct ContractedFunction (contract function name))

;; Alias the name for clarity
(define make-flat-contract FlatContract)

;; Formats a contract nicely as a string
(define (contract->string contract)
  (cond
    [(FlatContract? contract) (symbol->string (FlatContract-name contract))]
    [(and (FunctionContract? contract) (FunctionContract-rest contract))
     (to-string "(->* ("
                (apply to-string
                       (transduce (FunctionContract-pre-conditions contract)
                                  (mapping contract->string)
                                  (into-list)))
                ") #:rest"
                (contract->string (FunctionContract-rest contract))
                (contract->string (FunctionContract-post-condition contract))
                ")")]
    [(FunctionContract? contract)
     (to-string "(->"
                (apply to-string
                       (transduce (FunctionContract-pre-conditions contract)
                                  (mapping contract->string)
                                  (into-list)))
                (contract->string (FunctionContract-post-condition contract))
                ")")]
    [else (error! "Unexpected value found in contract:" contract)]))

;; Given a list, splits off the last argument, returns as a pair
(define (split-last lst)
  (define (loop accum lst)
    (if (empty? (cdr lst))
        (list (reverse accum) (car lst))
        (loop (cons (car lst) accum) (cdr lst))))
  (loop '() lst))

;;@doc
;; Creates a `FunctionContract` from the list of conditions, splitting out the
;; preconditions and the postconditions
(define make-function/c
  (lambda conditions
    (let ([split (split-last conditions)])
      (FunctionContract (first split) (second split) void '() #f))))

;;@doc
;; Creates a `FunctionContract` for a function that takes the arguments in
;; `pre-conditions`, and any number after them. `rest` is checked against the
;; list of those extra arguments.
(define (make-rest-function/c pre-conditions rest post-condition)
  (FunctionContract pre-conditions post-condition void '() rest))

;; Applies a flat contract to the given argument
(define (apply-flat-contract flat-contract arg)
  (if (flat-contract arg)
      #true
      (ContractViolation
       (to-string "Contract violation: found in the application of a flat contract for"
                  (FlatContract-name flat-contract)
                  ": the given input:"
                  arg
                  "resulted in a contract violation"))))

;; Call a contracted function
(define (apply-contracted-function contracted-function arguments span)
  (define span
    (if span
        span
        '(0 0 0)))
  (apply-function-contract (ContractedFunction-contract contracted-function)
                           (ContractedFunction-name contracted-function)
                           (ContractedFunction-function contracted-function)
                           arguments
                           span))

(define (apply-contracted-function-one-arg contracted-function arg span)
  (define span
    (if span
        span
        '(0 0 0)))
  (apply-function-contract-one-arg (ContractedFunction-contract contracted-function)
                                   (ContractedFunction-name contracted-function)
                                   (ContractedFunction-function contracted-function)
                                   arg
                                   span))

(define (apply-contracted-function-two-arg contracted-function arg arg2 span)
  (define span
    (if span
        span
        '(0 0 0)))
  (apply-function-contract-two-arg (ContractedFunction-contract contracted-function)
                                   (ContractedFunction-name contracted-function)
                                   (ContractedFunction-function contracted-function)
                                   arg
                                   arg2
                                   span))

(define (apply-contracted-function-three-arg contracted-function arg arg2 arg3 span)
  (define span
    (if span
        span
        '(0 0 0)))
  (apply-function-contract-three-arg (ContractedFunction-contract contracted-function)
                                     (ContractedFunction-name contracted-function)
                                     (ContractedFunction-function contracted-function)
                                     arg
                                     arg2
                                     arg3
                                     span))

(define (test-arg contract arg span name self-contract i)
  (cond
    [(FlatContract? contract)
     ;; Apply the flat contract to the value, and check if it satisfies the contract
     (let ([result (if (contract arg)
                       #true
                       (ContractViolation
                        (to-string
                         "Contract violation: found in the application of a flat contract for"
                         (FlatContract-name contract)
                         ": the given input:"
                         arg
                         "resulted in a contract violation")))])
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
     (if (ContractedFunction? arg)
         (let ([pre-parent (ContractedFunction-contract arg)])
           (let ([parent (FunctionContract (FunctionContract-pre-conditions pre-parent)
                                           (FunctionContract-post-condition pre-parent)
                                           (ContractAttachmentLocation 'DOMAIN
                                                                       (ContractedFunction-name arg))
                                           (FunctionContract-parents pre-parent)
                                           (FunctionContract-rest pre-parent))])
             (let ([fc (FunctionContract (FunctionContract-pre-conditions contract)
                                         (FunctionContract-post-condition contract)
                                         (ContractAttachmentLocation 'DOMAIN
                                                                     (ContractedFunction-name arg))
                                         (cons parent (FunctionContract-parents parent))
                                         (FunctionContract-rest contract))])

               (bind/c fc arg name span))))
         (bind/c contract arg name span))]
    [else (error! "Unexpected value in pre conditions: " contract)]))

(define (map2 func lst1 lst2 accum i)
  (if (empty? lst1)
      ;; Reverse the accum, if it exists
      (reverse accum)

      (let ([evaluated (func (car lst1) (car lst2) i)])
        (map2 func (cdr lst1) (cdr lst2) (cons evaluated accum) (+ i 1)))))

(define (verify-preconditions-test self-contract arguments name span)
  (define pre-conditions (FunctionContract-pre-conditions self-contract))
  (define rest (FunctionContract-rest self-contract))
  (define fixed (length pre-conditions))

  (unless (if rest
              (>= (length arguments) fixed)
              (equal? (length arguments) fixed))
    (error-with-span span
                     "Arity mismatch, function expected "
                     (if rest
                         (to-string "at least" fixed)
                         fixed)
                     "Found: "
                     (length arguments)))

  (if rest
      (append (map2 (lambda (arg contract i) (test-arg contract arg span name self-contract i))
                    (take arguments fixed)
                    pre-conditions
                    '()
                    0)
              (test-arg rest (drop arguments fixed) span name self-contract fixed))
      (map2 (lambda (arg contract i) (test-arg contract arg span name self-contract i))
            arguments
            pre-conditions
            '()
            0)))

(define (check-output output contract name span)

  (let ([self-contract contract]
        [self-contract-attachment-location (FunctionContract-contract-attachment-location contract)]
        [contract (FunctionContract-post-condition contract)])

    (cond
      [(FlatContract? contract)

       (let ([result (apply-flat-contract contract output)])
         (if (ContractViolation? result)
             (let ([blame-location (if (void? self-contract-attachment-location)
                                       name
                                       self-contract-attachment-location)])

               (cond
                 [(void? blame-location)
                  (error-with-span
                   span
                   "this function call resulted in an error - occured in the range position of this contract: "
                   (contract->string self-contract)
                   (ContractViolation-error-message result)
                   "blaming: None - broke its own contract")]

                 [else
                  (error-with-span
                   span
                   "this function call resulted in an error - occurred in the range position of this contract: "
                   (contract->string self-contract)
                   (ContractViolation-error-message result)
                   "blaming: "
                   blame-location)]))

             output))]
      [(FunctionContract? contract)

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
               (FunctionContract (FunctionContract-pre-conditions pre-parent)
                                 (FunctionContract-post-condition pre-parent)
                                 contract-attachment-location
                                 (FunctionContract-parents pre-parent)
                                 (FunctionContract-rest pre-parent)))
             (define fc
               (FunctionContract (FunctionContract-pre-conditions contract)
                                 (FunctionContract-post-condition contract)
                                 contract-attachment-location
                                 (cons parent (FunctionContract-parents pre-parent))
                                 (FunctionContract-rest contract)))

             (bind/c fc original-function name span))
           (bind/c contract output name span))]
      [else (error! "Unhandled value in post condition: " contract)])))

(define (apply-function-contract contract name function arguments span)
  (define validated-arguments (verify-preconditions-test contract arguments name span))
  (define output (apply function validated-arguments))
  (check-output output contract name span))

;; TODO: Make macros for the below so we can just auto generate
;; all of the various handlers for args from 0..8 before bailing
;; to heap allocated arguments
(define (apply-function-contract-one-arg contract name function arg span)
  (define preconditions (FunctionContract-pre-conditions contract))

  ;; Apply this for one function
  (let ([output (function (test-arg (car preconditions) arg span name contract 0))])
    (check-output output contract name span)))

(define (apply-function-contract-two-arg contract name function arg arg2 span)

  (define preconditions (FunctionContract-pre-conditions contract))

  ;; Apply this for one function
  (let ([output (function (test-arg (list-ref preconditions 0) arg span name contract 0)
                          (test-arg (list-ref preconditions 1) arg2 span name contract 1))])
    (check-output output contract name span)))

(define (apply-function-contract-three-arg contract name function arg arg2 arg3 span)
  (define preconditions (FunctionContract-pre-conditions contract))

  ;; Apply this for one function
  (let ([output (function (test-arg (list-ref preconditions 0) arg span name contract 0)
                          (test-arg (list-ref preconditions 1) arg2 span name contract 1)
                          (test-arg (list-ref preconditions 2) arg3 span name contract 2))])
    (check-output output contract name span)))

;; Whether every argument and the result of a function contract are checked by
;; a flat contract, for up to three arguments
(define (flat-function-contract? contract)
  (and (not (FunctionContract-rest contract))
       (<= (length (FunctionContract-pre-conditions contract)) 3)
       (FlatContract? (FunctionContract-post-condition contract))
       (all-flat? (FunctionContract-pre-conditions contract))))

(define (all-flat? contracts)
  (or (null? contracts) (and (FlatContract? (car contracts)) (all-flat? (cdr contracts)))))

;; Calls the predicates of a function contract's flat contracts directly. When
;; one fails, the general path checks again to report it.
;; The span of the call to the function `bind/c` made. It has to be looked up
;; in the body of that function, since a helper would find the call to itself.
(define-syntax call-span
  (syntax-rules ()
    [(_ span)
     (if (empty? span)
         (current-function-span)
         (car span))]))

;; `call`, the call of the contracted function, with its result checked.
(define-syntax checked-output
  (syntax-rules ()
    [(_ post contract name span call)
     (let ([output call])
       (if (post output)
           output
           (check-output output contract name (call-span span))))]))

(define (flat-checked-function contracted-function arity span)
  (define contract (ContractedFunction-contract contracted-function))
  (define function (ContractedFunction-function contracted-function))
  (define name (ContractedFunction-name contracted-function))
  (define pre (map FlatContract-predicate (FunctionContract-pre-conditions contract)))
  (define post (FlatContract-predicate (FunctionContract-post-condition contract)))
  (cond
    [(= arity 0) (lambda () (checked-output post contract name span (function)))]
    [(= arity 1)
     (let ([p0 (car pre)])
       (lambda (arg)
         (if (p0 arg)
             (checked-output post contract name span (function arg))
             (apply-contracted-function-one-arg contracted-function arg (call-span span)))))]
    [(= arity 2)
     (let ([p0 (car pre)]
           [p1 (cadr pre)])
       (lambda (arg arg2)
         (if (and (p0 arg) (p1 arg2))
             (checked-output post contract name span (function arg arg2))
             (apply-contracted-function-two-arg contracted-function arg arg2 (call-span span)))))]
    [else
     (let ([p0 (car pre)]
           [p1 (cadr pre)]
           [p2 (caddr pre)])
       (lambda (arg arg2 arg3)
         (if (and (p0 arg) (p1 arg2) (p2 arg3))
             (checked-output post contract name span (function arg arg2 arg3))
             (apply-contracted-function-three-arg contracted-function
                                                  arg
                                                  arg2
                                                  arg3
                                                  (call-span span)))))]))

;; Reports that `output`, the result of calling the function `bind/c` made as
;; `contracted`, breaks its contract. Calls to it are inlined with the checks
;; of a contract, so it is kept out of line to keep that code small.
(define #%contract-result-violation
  (#%black-box
   (lambda (contracted output)
     (let ([contract (get-contract-struct contracted)])
       (check-output output
                     contract
                     (ContractAttachmentLocation-name (FunctionContract-contract-attachment-location
                                                       contract))
                     (current-function-span))))))

;; Gives `function` the contract of `contracted`, a function `bind/c` made.
(define (#%contract-attach function contracted)
  (attach-contract-struct! function (get-contract-struct contracted)))

(define (bind/c contract function name . span)
  (define post-condition (FunctionContract-post-condition contract))
  (define arity (length (FunctionContract-pre-conditions contract)))

  (let ([updated-preconditions
         (transduce
          (FunctionContract-pre-conditions contract)
          (mapping (lambda (c)
                     (cond
                       [(FlatContract? c) c]
                       [(FunctionContract? c)
                        (FunctionContract (FunctionContract-pre-conditions c)
                                          (FunctionContract-post-condition c)
                                          (ContractAttachmentLocation 'DOMAIN name)
                                          (FunctionContract-parents c)
                                          (FunctionContract-rest c))]
                       [else (error "Unexpected value found in bind/c preconditions: " c)])))
          (into-list))]

        [updated-postcondition
         (cond
           [(FlatContract? post-condition) post-condition]
           [(FunctionContract? post-condition)

            (FunctionContract (FunctionContract-pre-conditions post-condition)
                              (FunctionContract-post-condition post-condition)
                              (ContractAttachmentLocation 'RANGE name)
                              (FunctionContract-parents post-condition)
                              (FunctionContract-rest post-condition))]
           [else (error "Unexpected value found in bind/c post condition: " post-condition)])])

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
                                                     (FunctionContract-parents contract))
                                                 (FunctionContract-rest contract))
                               function
                               name)])

      (let ([resulting-lambda-function

             (cond

               [(flat-function-contract? (ContractedFunction-contract contracted-function))
                (flat-checked-function contracted-function arity span)]

               ;; Extra arguments go through the general path
               [(FunctionContract-rest contract)
                (lambda args
                  (apply-contracted-function contracted-function
                                             args
                                             (if (empty? span)
                                                 (current-function-span)
                                                 (car span))))]

               ;; Switch on the arity so that we can fast path (and avoid allocating)
               ;; on low enough argument counts
               [(= 0 arity)

                (lambda ()
                  (apply-contracted-function contracted-function
                                             '()
                                             (if (empty? span)
                                                 (current-function-span)
                                                 (car span))))]

               [(= 1 arity)

                (lambda (arg)
                  (apply-contracted-function-one-arg contracted-function
                                                     arg
                                                     (if (empty? span)
                                                         (current-function-span)
                                                         (car span))))]

               [(= 2 arity)

                (lambda (arg arg2)
                  (apply-contracted-function-two-arg contracted-function
                                                     arg
                                                     arg2
                                                     (if (empty? span)
                                                         (current-function-span)
                                                         (car span))))]
               [(= 3 arity)

                (lambda (arg arg2 arg3)
                  (apply-contracted-function-three-arg contracted-function
                                                       arg
                                                       arg2
                                                       arg3
                                                       (if (empty? span)
                                                           (current-function-span)
                                                           (car span))))]
               [else

                (lambda args
                  (apply-contracted-function contracted-function
                                             args
                                             (if (empty? span)
                                                 (current-function-span)
                                                 (car span))))])])
        (attach-contract-struct! resulting-lambda-function
                                 (ContractedFunction-contract contracted-function))
        resulting-lambda-function))))

(define (make/c contract name)
  (cond
    [(FlatContract? contract) contract]
    [(FunctionContract? contract) contract]
    [else (FlatContract contract name)]))

(define-syntax ->/c
  (syntax-rules ()
    [(->/c r) (make-function/c (make/c r 'r))]
    [(->/c r ...) (make-function/c (make/c r 'r) ...)]))

;;@doc
;; Syntax:
;; A contract for a function that takes the arguments in parentheses, and any
;; number of arguments after them. The `#:rest` contract is checked against
;; the list of those extra arguments.
;;
;; # Examples
;;
;; ```scheme
;; (->*/c (string?) #:rest (listof int?) string?)
;; ```
(define-syntax ->*/c
  (syntax-rules ()
    [(->*/c (pre ...) #:rest rest post)
     (make-rest-function/c (list (make/c pre 'pre) ...) (make/c rest 'rest) (make/c post 'post))]))

;;@doc
;; Syntax:
;; Macro for basic usage of contracts.
;;
;; Matches the typical syntax of define, however the result is wrapped
;; in the provided contract.
;;
;; # Examples
;;
;; ```scheme
;; (define/contract (adding1 x)
;;    (->/c number? number?)
;;    (+ x 1))
;; ```
(define-syntax define/contract
  (syntax-rules ()
    [(define/contract (name args ... . rest)
       contract
       body ...)
     (begin
       (define name
         (lambda (args ... . rest)
           body ...))
       (set! name (bind/c contract name 'name))
       void)]
    [(define/contract (name args ...)
       contract
       body ...)
     (begin
       (define name
         (lambda (args ...)
           body ...))
       (set! name (bind/c contract name 'name))
       void)
     ;  (define name (bind/c contract (lambda (args ...) body ...) 'name))
     ]
    [(define/contract name
       contract
       expr)
     (define name ((bind/c (make-function/c (make/c contract 'contract)) (lambda () expr))))]))

(provide (for-syntax contract/out/test))

(define-syntax contract/out/test
  (syntax-rules ()
    [(contract/out/test name contract) (%require-ident-spec name (bind/c contract name 'name))]))

;;;;;;;;;;;;;;;;;;;;;;;;;; Types ;;;;;;;;;;;;;;;;;;;;;;

;; The parts a combinator made a contract from, kept with the predicate that
;; checks it so the contract can be read back as a type.
(struct CombinedPredicate (kind parts check) #:prop:procedure 2)

(provide contract?
         listof
         cons/c
         hashof
         non-empty-listof
         </c
         >/c
         <=/c
         >=/c
         any/c
         and/c
         or/c
         custom/c)

(define (loop pred lst)
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (loop pred (cdr lst))]
    [else #f]))

;; Contract combinators
(define (listof pred)
  (make/c (CombinedPredicate 'listof
                             (list pred)
                             (lambda (lst)
                               (cond
                                 [(null? lst) #t]
                                 [(list? lst) (loop pred lst)]
                                 [else #f])))
          (list 'listof (contract-or-procedure-name pred))))

(define (cons/c car-pred cdr-pred)
  (make/c (CombinedPredicate 'cons/c
                             (list car-pred cdr-pred)
                             (λ (pair) (and (pair? pair) (car-pred (car pair)) (cdr-pred (cdr pair)))))
          (list 'cons/c (contract-or-procedure-name car-pred) (contract-or-procedure-name cdr-pred))))

(define (hashof key-pred value-pred)
  (make/c
   (CombinedPredicate 'hashof
                      (list key-pred value-pred)
                      (lambda (hashmap)
                        ;; For hashof - we want to assert that both all of the keys and all of the values abide by
                        ;; a specific predicate
                        (and (hash? hashmap)
                             ((listof key-pred) (hash-keys->list hashmap))
                             ((listof value-pred) (hash-values->list hashmap)))))
   (list 'hashof (contract-or-procedure-name key-pred) (contract-or-procedure-name value-pred))))

(define (contract-or-procedure-name x)
  (cond
    [(FlatContract? x) (FlatContract-name x)]
    [(FunctionContract? x) (string->symbol (contract->string x))]
    [else
     (let ([lookup (function-name x)])
       (if (string? lookup)
           (string->symbol lookup)
           '#<function>))]))

;; Like listof, however requires that the list is non empty as well
(define (non-empty-listof pred)
  (make/c (CombinedPredicate 'non-empty-listof
                             (list pred)
                             (lambda (lst)
                               (cond
                                 [(null? lst) #f]
                                 [(list? lst) (loop pred lst)]
                                 [else #f])))
          (list 'non-empty-listof (contract-or-procedure-name pred))))

;; Contracts for <
(define (</c n)
  (make/c (fn (x) (< x n)) (list '</c n)))

;; Contracts for >
(define (>/c n)
  (make/c (fn (x) (> x n)) (list '>/c n)))

;; Contracts for <=
(define (<=/c n)
  (make/c (fn (x) (<= x n)) (list '<=/c n)))

;; Contracts for >=
(define (>=/c n)
  (make/c (fn (x) (>= x n)) (list '>=/c n)))

;; Satisfies any single value
(define any/c (make/c (fn (_) #t) 'any/c))

;; produces a function compatible with contract definitions
(define (and/c x y)
  (make/c (CombinedPredicate 'and/c (list x y) (lambda (z) (and (x z) (y z))))
          (list 'and/c (contract-or-procedure-name x) (contract-or-procedure-name y))))

;; produces a function compatible with contract definitions
(define (or/c x y)
  (make/c (CombinedPredicate 'or/c (list x y) (lambda (z) (or (x z) (y z))))
          (list 'or/c (contract-or-procedure-name x) (contract-or-procedure-name y))))

;;@doc
;; A contract for the values of the host type named `name`, the name its values
;; report, like `"app::Counter"`.
(define (custom/c name)
  (make/c (CombinedPredicate 'custom
                             (list name)
                             (lambda (value) (equal? (#%custom-type-name value) name)))
          (list 'custom/c name)))

(define combinators (hashset listof non-empty-listof </c >/c <=/c >=/c and/c or/c custom/c))

(define (contract? predicate-or-contract)
  (cond
    [(FlatContract? predicate-or-contract) #t]
    [(FunctionContract? predicate-or-contract) #t]
    [(function? predicate-or-contract)
     (or (equal? (arity? predicate-or-contract) 1)
         (hashset-contains? combinators predicate-or-contract))]
    [else #f]))

;; TODO: Come back to this - I think I need to be very specific about the then-contract and else-contract
;; and how they get applied
; (define (if/c predicate then-contract else-contract)
;     (make/c

;         (lambda (x) (if (predicate x)
;                         (then-contract predicate)
;                         (else-contract predicate)))
;         (list 'if/c then-contract else-contract)))

; (define (type/c type contract)
;     (and/c

; )

; (define (int/c ))
