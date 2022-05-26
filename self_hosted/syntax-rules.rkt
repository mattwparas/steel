

(make-struct SyntaxRules (name syntaxes patterns) #:transparent #true)

(define (check-first expression symbol)
    (equal? (first expression) symbol))

(define (define-syntax? expression) (check-first expression 'define-syntax))

(define (syntax-rules? lst) (check-first lst 'syntax-rules))

(define (syntax-rules->struct name lst)
    (SyntaxRules name (second lst) (third lst)))

(syntax-rules->struct 'or
    '(syntax-rules ()
        [(or) #f]
        [(or x) x]
        [(or x y) (let ([z x])
                    (if z z y))]
        [(or x y ...) (or x (or y ...))]))