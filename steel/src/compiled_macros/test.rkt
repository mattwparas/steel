(define-syntax module
    (syntax-rules (provide gen-defines contract/out) 
        [(module name (provide ids ...) funcs ...)
         (begin
            (define (datum->syntax name) 
                ((lambda () funcs ... 
                (module provide ids ...))))
            (module gen-defines name ids ...))]
        
        ;; in the contract case, ignore the contract in the hash
        [(module provide (contract/out name contract)) (hash 'name name)]
        ;; Normal case
        [(module provide name) (hash 'name name)]

        ;; in the contract case, ignore the contract in the hash
        [(module provide (contract/out name contract) rest ...)
         (hash-insert (module provide rest ...) 'name name)]

        ;; Normal case
        [(module provide name rest ...)
         (hash-insert (module provide rest ...) 'name name)]

        ;; Module contract provides
        [(module gen-defines mod (contract/out name contract))
         (define (datum->syntax name) (bind/c contract (hash-get mod 'name)))]
        [(module gen-defines mod (contract/out name contract) rest ...)
         (begin (define (datum->syntax name) (bind/c contract (hash-get mod 'name)))
            (module gen-defines mod rest ...))]

        ;; Normal provides
        [(module gen-defines mod name) (define (datum->syntax name) (hash-get mod 'name))]
        [(module gen-defines mod name rest ...)
         (begin (define (datum->syntax name) (hash-get mod 'name))
            (module gen-defines mod rest ...))]))