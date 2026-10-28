(define-syntax (mk stx)
  (syntax-case stx () [(_ n v) #'(define-syntax (n s) (syntax-case s () [(_) #''v]))]))

;; A body's procedural macro hides a parameter of its name
(define (param-hidden x)
  (define-syntax (x s) (syntax-case s () [(_) #''macro]))
  (x))
(assert! (equal? (param-hidden 1) 'macro))
(define (generated-hides-param list) (mk list 7) (list))
(assert! (equal? (generated-hides-param 1) 7))
(assert! (equal? (list 1 2) '(1 2)))

;; An inner scope's variable hides it
(define (inner-variable)
  (define-syntax (m s) (syntax-case s () [(_ x) #''macro]))
  (let ([m (lambda (x) 'variable)]) (m 1)))
(assert! (equal? (inner-variable) 'variable))

;; The same name in sibling and nested bodies, and one a macro introduced, isn't a duplicate
(define (siblings)
  (define (a) (define-syntax (e s) (syntax-case s () [(_) #''a])) (e))
  (define (b) (define-syntax (e s) (syntax-case s () [(_) #''b])) (e))
  (define-syntax (e s) (syntax-case s () [(_) #''outer]))
  (list (a) (b) (e) (let () (define-syntax (e s) (syntax-case s () [(_) #''inner])) (e))))
(assert! (equal? (siblings) '(a b outer inner)))
(define-syntax (with-hidden stx)
  (syntax-case stx () [(_ v) #'(begin (define-syntax (e s) (syntax-case s () [(_) #''hidden])) v)]))
(define (introduced)
  (define-syntax (e s) (syntax-case s () [(_) #''mine]))
  (with-hidden 1)
  (e))
(assert! (equal? (introduced) 'mine))

;; A curried define of a macro's name defines a function
(define ((cond x) y) (list 'c x y))
(assert! (equal? ((cond 1) 2) '(c 1 2)))
(define (((add a) b) c) (+ a b c))
(assert! (equal? (((add 1) 2) 3) 6))
(define ((with-rest a) . more) (list a more))
(assert! (equal? ((with-rest 1) 2 3) '(1 (2 3))))
(define (curried-in-a-body)
  (define ((when x) y) (list 'w x y))
  ((when 1) 2))
(assert! (equal? (curried-in-a-body) '(w 1 2)))

;; A define-syntax in quoted output is data
(define-syntax (quoted-definition stx)
  (syntax-case stx () [(_) #'(list '(define-syntax (foo x) x) `(define-syntax bar ,(+ 1 2)))]))
(assert! (equal? (quoted-definition) '((define-syntax (foo x) x) (define-syntax bar 3))))
