;; a name a macro template introduces is renamed for hygiene, but quoting it
;; gives the symbol as it was originally
(define-syntax quote-local
  (syntax-rules ()
    [(_) (let ([y 1]) (list y 'y (quote y)))]))
(assert! (equal? (quote-local) '(1 y y)))

;; a pattern variable inside a quote is still replaced by the argument
(define-syntax quote-arg
  (syntax-rules ()
    [(_ x) (list 'x (quote x) '(x a))]))
(assert! (equal? (quote-arg hello) '(hello hello (hello a))))

;; inside quasiquote only the unquoted parts are code
(define-syntax quasi
  (syntax-rules ()
    [(_ x) (let ([y 2]) `(y ,y x ,x (nested ,@(list y x))))]))
(assert! (equal? (let ([x 7]) (quasi x)) '(y 2 x 7 (nested 2 7))))

;; the renamed binder does not capture the caller's variable of the same name
(define-syntax bind-and-quote
  (syntax-rules ()
    [(_ v) (let ([tmp v]) (list 'tmp tmp))]))
(assert! (equal? (let ([tmp 5]) (bind-and-quote tmp)) '(tmp 5)))
