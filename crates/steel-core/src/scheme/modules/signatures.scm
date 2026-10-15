(provide signature->contract
         contract->signature)

;; Each type of a signature, with the predicate that holds for exactly its
;; values and that predicate's name.
(define type-predicates
  (list (list 'Any any/c 'any/c)
        (list 'Bool boolean? 'boolean?)
        (list 'Int fixnum? 'fixnum?)
        (list 'Float float? 'float?)
        (list 'Number number? 'number?)
        (list 'Char char? 'char?)
        (list 'String string? 'string?)
        (list 'Symbol symbol? 'symbol?)
        (list 'List list? 'list?)
        (list 'Pair improper-pair? 'improper-pair?)
        (list 'Vector immutable-vector? 'immutable-vector?)
        (list 'MutableVector mutable-vector? 'mutable-vector?)
        (list 'HashMap hash? 'hash?)
        (list 'HashSet set? 'set?)
        (list 'ByteVector bytes? 'bytes?)
        (list 'Void void? 'void?)
        (list 'FlatVector flat-vector? 'flat-vector?)
        (list 'Procedure function? 'function?)))

(define (find-type-predicate matches?)
  (let loop ([entries type-predicates])
    (cond
      [(null? entries) #f]
      [(matches? (car entries)) (car entries)]
      [else (loop (cdr entries))])))

;; The contract for the values of a type written as in a signature
(define (type->contract type)
  (cond
    [(symbol? type)
     (let ([entry (find-type-predicate (lambda (entry) (equal? (car entry) type)))])
       (if entry
           (make/c (cadr entry) (caddr entry))
           (error "signature->contract: unknown type" type)))]
    [(and (pair? type) (equal? (car type) 'or))
     (let loop ([members (cdr type)])
       (if (null? (cdr members))
           (type->contract (car members))
           (or/c (type->contract (car members)) (loop (cdr members)))))]
    [(and (pair? type) (equal? (car type) 'listof)) (listof (type->contract (cadr type)))]
    [(and (pair? type) (equal? (car type) 'hashof))
     (hashof (type->contract (cadr type)) (type->contract (caddr type)))]
    [(and (pair? type) (equal? (car type) 'custom)) (custom/c (cadr type))]
    [else (error "signature->contract: unknown type" type)]))

(define (clause name clauses)
  (let ([found (assoc name clauses)]) (if found (cdr found) #f)))

;;@doc
;; The contract a function with this signature satisfies, given the signature
;; as `function-signature` returns it. A signature without argument types has
;; no contract, so the result is #false.
;;
;; # Examples
;;
;; ```scheme
;; (signature->contract (function-signature string-length)) ;; => (-> string? fixnum?)
;; ```
(define (signature->contract signature)
  (define args (and signature (clause 'args signature)))
  (define returns (and signature (clause 'returns signature)))
  (cond
    [(not args) #f]
    [else
     (define fixed
       (filter (lambda (arg) (not (and (pair? arg) (equal? (car arg) 'rest)))) args))
     (define rest
       (let ([found (filter (lambda (arg) (and (pair? arg) (equal? (car arg) 'rest))) args)])
         (if (null? found)
             #f
             (cadr (car found)))))
     (define post
       (type->contract (if returns
                           (car returns)
                           'Any)))
     (define pre (map type->contract fixed))
     (if rest
         (make-rest-function/c pre (listof (type->contract rest)) post)
         (apply make-function/c (append pre (list post))))]))

;; The type in a signature for the values a contract accepts, or #false when
;; the contract says more than a type can.
(define (contract->type contract)
  (define predicate
    (if (FlatContract? contract)
        (FlatContract-predicate contract)
        contract))
  (define entry
    (find-type-predicate (lambda (entry)
                           (or (eq? (cadr entry) contract) (eq? (cadr entry) predicate)))))
  (cond
    [entry (car entry)]
    [(CombinedPredicate? predicate)
     (let ([parts (CombinedPredicate-parts predicate)])
       (case (CombinedPredicate-kind predicate)
         [(listof)
          (let ([elements (contract->type (car parts))]) (and elements (list 'listof elements)))]
         [(hashof)
          (let ([keys (contract->type (car parts))]
                [values (contract->type (cadr parts))])
            (and keys values (list 'hashof keys values)))]
         [(or/c)
          (let ([left (contract->type (car parts))]
                [right (contract->type (cadr parts))])
            (and left right (cons 'or (append (union-members left) (union-members right)))))]
         [(custom) (list 'custom (car parts))]
         [else #f]))]
    [else #f]))

(define (union-members type)
  (if (and (pair? type) (equal? (car type) 'or))
      (cdr type)
      (list type)))

;;@doc
;; The signature, in the form `function-signature` returns, of a function
;; contract that only says what type each argument and the result has. A
;; contract that says more, like `(>/c 0)` or a function argument with its own
;; contract, has no signature, so the result is #false.
;;
;; # Examples
;;
;; ```scheme
;; (contract->signature (->/c string? fixnum?))
;; ;; => ((args String) (returns Int) (raises Maybe))
;; ```
(define (contract->signature contract)
  (cond
    [(not (FunctionContract? contract)) #f]
    [else
     (define fixed (map contract->type (FunctionContract-pre-conditions contract)))
     (define rest-contract (FunctionContract-rest contract))
     (define rest
       (and rest-contract
            (let ([type (contract->type rest-contract)])
              (cond
                [(equal? type 'List) 'Any]
                [(and (pair? type) (equal? (car type) 'listof)) (cadr type)]
                [else #f]))))
     (define returns (contract->type (FunctionContract-post-condition contract)))
     (cond
       [(member #f fixed) #f]
       [(and rest-contract (not rest)) #f]
       ;; A signature's result is a single type, not a union
       [(or (not returns) (and (pair? returns) (equal? (car returns) 'or))) #f]
       [else
        (list (cons 'args
                    (if rest
                        (append fixed (list (list 'rest rest)))
                        fixed))
              (list 'returns returns)
              (list 'raises 'Maybe))])]))
