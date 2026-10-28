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

;; A body's macro used through another of its macros' templates still hides a parameter,
;; and a template's name isn't captured by a use site's local
(define (through-rules m)
  (define-syntax (m s) (syntax-case s () [(_) #''macro]))
  (define-syntax call-m (syntax-rules () [(_) (m)]))
  (call-m))
(assert! (equal? (through-rules 1) 'macro))
(define (through-procedural m)
  (define-syntax (m s) (syntax-case s () [(_) #''macro]))
  (define-syntax (call-m s) (syntax-case s () [(_) #'(m)]))
  (call-m))
(assert! (equal? (through-procedural 1) 'macro))
(define (template-name-is-hygienic)
  (define-syntax (m s) (syntax-case s () [(_) #''macro]))
  (define-syntax (call-m s) (syntax-case s () [(_) #'(m)]))
  (let ([m (lambda () 'var)]) (call-m)))
(assert! (equal? (template-name-is-hygienic) 'macro))

;; A body's define hides a global macro from functions defined before it
(define-syntax sibling (syntax-rules () [(_ x) 'macro]))
(define (sibling-hidden)
  (define (use) (sibling 1))
  (define (sibling a) (list 'fn a))
  (use))
(assert! (equal? (sibling-hidden) '(fn 1)))

;; A body's define hides a macro a body macro's template names, where that macro is defined
(define-syntax (global-m stx) (syntax-case stx () [(_) #''macro]))
(define (body-define-hides)
  (define (global-m) 'body)
  (define-syntax (call s) (syntax-case s () [(_) #'(global-m)]))
  (call))
(assert! (equal? (body-define-hides) 'body))

;; A template's set! of a variable where its macro is defined, used where a macro has the name
(define (template-set)
  (define m 0)
  (define-syntax (g s) (syntax-case s () [(_) #'(begin (set! m 1) m)]))
  (define (k) (define-syntax (m s) (syntax-case s () [(_) #''mac])) (list (g) (m)))
  (k))
(assert! (equal? (template-set) '(1 mac)))

;; Defines in an expression's begins aren't the body's
(define (expression-begins c)
  (if c (begin (define y 1) y) (begin (define y 2) y)))
(assert! (equal? (list (expression-begins #t) (expression-begins #f)) '(1 2)))

;; A function's macro's template names a syntax-rules macro, not a use site's local
(define-syntax rules-gr (syntax-rules () [(_) 'macro]))
(define (template-names-rules)
  (define-syntax (inner s) (syntax-case s () [(_) #'(rules-gr)]))
  (let ([rules-gr (lambda () 'let)]) (inner)))
(assert! (equal? (template-names-rules) 'macro))
(define (template-names-local-rules)
  (define-syntax m (syntax-rules () [(_) 'rules]))
  (define-syntax (call-m s) (syntax-case s () [(_) #'(m)]))
  (list (let ([m (lambda () 'let)]) (call-m)) (let () (define (m) 'inner) (call-m))))
(assert! (equal? (template-names-local-rules) '(rules rules)))

;; And a body's define, which a use site's binder doesn't capture
(define (template-names-body-define)
  (define-syntax (gz s) (syntax-case s () [(_) #'z]))
  (define z 'body)
  (list (let ([z 'inner]) (gz)) (gz)))
(assert! (equal? (template-names-body-define) '(body body)))

;; Match's code doesn't use the locals where it's used
(define (match-under-shadowed-primitives)
  (let ([car 1] [cdr 2] [null? 3] [not 4] [equal? 5] [reverse 6] [list? 7])
    (list (match (list 1 2) [(list x y) (+ x y)])
          (match '(1 (2 3)) [(list a (list b c)) (+ a b c)])
          (match (list 1 2 3) [(list x ... y) (list x y)]))))
(assert! (equal? (match-under-shadowed-primitives) '(3 6 ((1 2) 3))))
