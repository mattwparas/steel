(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((quasiquote ((unquote x) xs ...))          (cons x (quasiquote (xs ...))))
    ((quasiquote ((unquote-splicing x)))        (append (list x) '()))
    ((quasiquote ((unquote-splicing x) xs ...)) (append x (quasiquote (xs ...))))
    ((quasiquote (unquote x))                 x)
    ((quasiquote (x))                          '(x))
    ((quasiquote (x xs ...))                   (cons (quasiquote x) (quasiquote (xs ...))))
    ((quasiquote x)                           'x)))


(define struct-name 'Applesauce)
(define fields '(a b c))

;; TODO: This actually expands using the reader macro from the parser which expands fancy defines
;;
`(define ,struct-name (lambda ,fields (vector __magic_struct_symbol__ (quote ,struct-name) ,@fields)))

(quasiquote (define (unquote struct-name)
           (lambda (unquote fields)
             (vector __magic_struct_symbol__
                     (quote (unquote struct-name))
                     (unquote-splicing fields)))))


;; TODO: Go _back_ to an implementation that just uses S-Expressions and thats it for internal
;; representation. Don't have special AST implementation other than in the core.
;; Another solution that will get me part of the way there, is treating quasiquote like quote in the
;; parser
