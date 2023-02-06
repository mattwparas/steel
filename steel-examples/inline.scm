;; Constant propagation
;; Need to take an argument and check if its a constant value
;; If it is a constant value, substitute the value in the source code


;; (define (test)
;;   (+ 1 2 3 4 (+ 5 6 7 8 (+ 9 10 11 12))))


;; (define (blagh)
;;   (test))


;; (define (collect-global-context))

;; gotta fix this in the prelude
(define (reduce op z lst)
  (cond ((null? lst) z)    ; just pass it in as another argument
    (else (reduce op
                  (op z (car lst))  ; NB! `z` as first arg
                  (cdr lst)))))

(define *program*
  '((define test (lambda () (+ 1 2 3 4 (+ 4 5 6 7 8) (+ 9 10 11 12))))
    (define blagh (lambda () (test)))
    (define foo (lambda () (blagh)))
    (define bar foo)
    (define function-with-args (lambda (x y) (+ x y)))
    (define add1 (lambda (x) (+ x 1)))))


;; if its a definition, returns the pair of (name, body)
(define (define-expr? expr)
  (if (and (list? expr)
           (equal? 3 (length expr))
           (equal? (car expr) 'define))
      (cdr expr)
      #f))

;; takes an expression, and if its a definition
;; puts the pair into the map
(define (insert-define-var env expr)
  (let ((p (define-expr? expr)))
    (if p
        (hash-insert env (car p) (cadr p))
        env)))

;; This gives the top level definitions
(define initial-env
  (reduce insert-define-var (hash) *program*))

(define (lookup sym)
  (if (hash-contains? initial-env sym)
      (hash-get initial-env sym)
      #f))

;; Heres the deal:
;; In order to remove the closure, form and instead just replace the body as it is
;; We need to assert that the argument to the function is only used once

;; For now, just assume that the function has one argument
(define (count-argument-usages sym expr)
  (cond [(equal? sym expr) 1]
        [(list? expr)
         (apply + (map (lambda (x) (count-argument-usages sym x)) expr))]
        [else 0]))

;; Any true
(define (any lst)
  (if (null? lst)
      #f
      (if (car lst)
          #t
          (any (cdr lst)))))


;; This is also not quite correct
;; This should take into account aliasing
(define (recursive? expr)
  (define ctx (cadr expr))
  (define (recursive?* expr)
    (cond
      [(list? expr)
       (if (equal? (car expr) ctx)
           #t
           (any (map recursive?* (cdr expr))))]
      [else #f]))
  (recursive?* expr))


(recursive? '(define loop
               (lambda () (loop))))




;; We also need to know that the body of the function in which we're inlining has no sideeffects that we could be goofing up evaluation order w/
;; (count-argument-usages 'x '(+ x (+ x y)))

;; (define (perform-native-substitution))


;; (define (can-perform-beta-reduction func)
;;   )


;; initial-env

;; ignore the fact that they could have been shadowed... for now
;; (define (inline-expr expr))

;; We want to inline a given expr, outside of the
(define (naive-inline expr)
  (cond [(symbol? expr)
         (define bound-expr (lookup expr))
         (if bound-expr
             bound-expr
             expr)]
        [(list? expr) (map naive-inline expr)]
        [else expr]))

(naive-inline 'bar)
(naive-inline '(blagh))
(naive-inline '(add1 10))



;; Argument Substitution below
;; -----------------------------------------

(define (lambda-expression-application? expr)
  (if (and (>= (length expr) 2)
           (list? (car expr))
           (equal? (car (car expr)) 'lambda))
      (car (cdr (car expr)))
      '()))

(define (atom? expr)
  (or (number? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)))

(define (constant? expr)
  (or (number? expr)
      (symbol? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)))


;; hash -> (listof sym?) -> (listof any?) -> hash
(define (bind-args env list-of-symbols args)
  (if (or (null? list-of-symbols) (null? args))
      env
      (bind-args
       (hash-insert env
                    (car list-of-symbols) (car args))
       (cdr list-of-symbols)
       (cdr args))))

;; this is just an alias here
(define get-application-args cdr)

;; naive substitution of constant arguments
(define (naive-subst* expr env)
  ;; (displayln expr)
  ;; (displayln (symbol? expr))
  (cond [(null? expr) expr]
        [(atom? expr) expr]
        [(symbol? expr)
         ;; (displayln "inside symbol case with: ")
         ;; (displayln expr)
         ;; (displayln (hash-contains? env expr))
         ;; (displayln env)
         (cond
           [(hash-contains? env expr)
            (define expr-value (hash-get env expr))
            ;; (displayln expr-value)
            (if (constant? expr-value)
                (begin
                  ;; (mark-changed)
                  expr-value)
                expr)]
           [else expr])]
        [(lambda-expression-application? expr)
         ;; (displayln "Found an expression")
         ;; (displayln expr)
         (define list-of-bindings (lambda-expression-application? expr))
         (define list-of-args (get-application-args expr))
         (define new-env (bind-args env list-of-bindings list-of-args))
         ;; (displayln "lambda evaluation")
         ;; (displayln new-env)
         ;; (displayln (caddr (car expr)))
         ;; TODO check if you can remove the constant value
         ;; in the argument position
         (cons
          (list
           'lambda
           (car (cdr (car expr)))
           (naive-subst* (caddr (car expr)) new-env))
          (cdr expr))]
        [else
         (map (lambda (x) (naive-subst* x env)) expr)]))

(define (naive-subst expr)
  ;; (init-changed)
  (naive-subst* expr (hash)))


;; (naive-subst
;;  (naive-inline '(add1 10)))

;; (naive-subst '((lambda (x) (* x x)) y))
