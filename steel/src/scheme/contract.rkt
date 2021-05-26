;; Contract combinators
(define (listof pred)
        (lambda (lst)
            (define (loop lst)
                (cond [(null? lst) #t]
                      [(pred (car lst)) (loop (cdr lst))]
                      [else #f]))
            (cond [(null? lst) #t]
                  [(list? lst)
                    (loop lst)]
                  [else #f])))

;; Contracts for <
(define (</c n)
    (make/c (fn (x) (< x n)) '</c))

;; Contracts for >
(define (>/c n)
    (make/c (fn (x) (> x n)) '>/c))

;; Contracts for <=
(define (<=/c n)
    (make/c (fn (x) (<= x n)) '<=/c))

;; Contracts for >=
(define (>=/c n)
    (make/c (fn (x) (>= x n)) '>=/c))

;; Satisfies any single value
(define (any/c x)
    (make/c (fn (x) #t) 'any/c))

;; produces a function compatible with contract definitions
(define (and/c x y)
    (lambda (z) (and (x z) (y z))))

;; produces a function compatible with contract definitions
(define (or/c x y)
    (lambda (z) (or (x z) (y z))))