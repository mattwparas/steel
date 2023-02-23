(require "contract.scm" (for-syntax "contract.scm"))

(provide listof </c >/c <=/c >=/c any/c and/c or/c)

(define (loop pred lst)
    (cond [(null? lst) #t]
            [(pred (car lst)) (loop pred (cdr lst))]
            [else #f]))

;; Contract combinators
(define (listof pred)
        (lambda (lst)
            (cond [(null? lst) #t]
                  [(list? lst)
                    (loop pred lst)]
                  [else #f])))

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

;; TODO: Come back to this - I think I need to be very specific about the then-contract and else-contract
;; and how they get applied
(define (if/c predicate then-contract else-contract)
    (make-contract 
        (lambda (x) (if (predicate x) 
                        (then-contract predicate) 
                        (else-contract predicate)))
        (list 'if/c then-contract else-contract)))
