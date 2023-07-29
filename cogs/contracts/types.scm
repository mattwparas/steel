(require "contract.scm"
         (for-syntax "contract.scm"))

(provide contract?
         listof
         non-empty-listof
         </c
         >/c
         <=/c
         >=/c
         any/c
         and/c
         or/c)

(define (loop pred lst)
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (loop pred (cdr lst))]
    [else #f]))

;; Contract combinators
(define (listof pred)
  (make-contract (lambda (lst)
                   (cond
                     [(null? lst) #t]
                     [(list? lst) (loop pred lst)]
                     [else #f]))
                 (list 'listof (contract-or-procedure-name pred))))

(define (contract-or-procedure-name x)
  (cond
    [(FlatContract? x) (FlatContract-name x)]
    [(FunctionContract? x) (string->symbol (contract->string x))]
    [else
     (let ([lookup (function-name x)]) (if (string? lookup) (string->symbol lookup) '#<function>))]))

;; Like listof, however requires that the list is non empty as well
(define (non-empty-listof pred)
  (make-contract (lambda (lst)
                   (cond
                     [(null? lst)
                      (displayln "getting here?")
                      #f]
                     [(list? lst) (loop pred lst)]
                     [else #f]))
                 (list 'non-empty-listof (contract-or-procedure-name pred))))

;; Contracts for <
(define (</c n)
  (make-contract (fn (x) (< x n)) (list '</c n)))

;; Contracts for >
(define (>/c n)
  (make-contract (fn (x) (> x n)) (list '>/c n)))

;; Contracts for <=
(define (<=/c n)
  (make-contract (fn (x) (<= x n)) (list '<=/c n)))

;; Contracts for >=
(define (>=/c n)
  (make-contract (fn (x) (>= x n)) (list '>=/c n)))

;; Satisfies any single value
(define (any/c x)
  (make-contract (fn (x) #t) 'any/c))

;; produces a function compatible with contract definitions
(define (and/c x y)
  (make-contract (lambda (z) (and (x z) (y z))) (list 'and/c x y)))

;; produces a function compatible with contract definitions
(define (or/c x y)
  (make-contract (lambda (z) (or (x z) (y z))) (list 'or/c x y)))

(define combinators (hashset listof non-empty-listof </c >/c <=/c >=/c any/c and/c or/c))

(define (contract? predicate-or-contract)
  (cond
    [(function? predicate-or-contract)
     (or (equal? (arity? predicate-or-contract) 1)
         (hashset-contains? combinators predicate-or-contract))]
    [else
     =>
     #f]))

;; TODO: Come back to this - I think I need to be very specific about the then-contract and else-contract
;; and how they get applied
; (define (if/c predicate then-contract else-contract)
;     (make-contract
;         (lambda (x) (if (predicate x)
;                         (then-contract predicate)
;                         (else-contract predicate)))
;         (list 'if/c then-contract else-contract)))

; (define (type/c type contract)
;     (and/c

; )

; (define (int/c ))
