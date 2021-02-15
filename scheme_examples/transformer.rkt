

(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))))


(define (lambda-no-args? e)
  (and (equal? (car e) 'lambda)
       (null? (cadr e))))

;; assumes the body is one expression
(define (get-body-fn l)
  (last l))

(define (empty-function-application? e)
  (and (equal? (length e) 1)
       (lambda-no-args? (car e))))


(lambda-no-args? '(lambda () 10))
(lambda-no-args? '(lambda (x) 10))


(empty-function-application? '((lambda () 10)))
(empty-function-application? '((lambda (x) 10) 2))

;; take the code and transform it
(define (transform e)
  (caddr (car e)))

(transform '((lambda () 10)))

(define (transform-empty-function-application e)
  (if (empty-function-application? e)
      (transform e)
      e))


(transform-empty-function-application '((lambda () 10))) ;; 10
(transform-empty-function-application '((lambda (x) 10) 2)) ;; stays the same


(define (lambda-application? e)
  (and (not (null? e))
       (list? (car e))
       (equal? (caar e) 'lambda)))



;; '(+ 1 2 (+ 3 4) (+ 5 6))

;; `(+ 1 2 ,(+ 3 4) (+ 5 6))

(define (constant? expr)
  (or (number? expr)
      (symbol? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)))

(define (constant-not-symbols? expr)
  (or (number? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)))

(define (all func lst)
  (if (null? lst)
      #t
      (if (func (car lst))
          (all func (cdr lst))
          #f)))

(define (match-func sym)
  (cond [(equal? '+ sym) +]
        [(equal? '- sym) -]
        [(equal? '* sym) *]
        [(equal? '/ sym) /]
        [(equal? 'string-append sym) string-append]
        [(equal? 'equal? sym) equal?]
        [else #f]))

(define (match-if expr)
  (equal? (car expr) 'if))

;; provide functions which can be run at compile time?
;; Or just try to run all of them?

;; this is a generic recursive evaluation
;; it's not quite done yet
(define (constant-fold expr)
  (cond
    [(constant? expr) expr]
    [(match-func (car expr))
     (define res (map constant-fold expr))
     (if (all constant-not-symbols? (cdr res))
         (apply (match-func (car expr)) (cdr res))
         res)]
    [(match-if expr)
     (define condition (constant-fold (car (cdr expr))))
     ;; HACK / TODO
     ;; Find how to differentiate something like a constant expression '(1 2 3 4) which
     ;; evaluates to a list, versus something like '(lambda (x) 10)
     (if (constant? condition)
         (if condition
             (constant-fold (caddr expr))
             (constant-fold (cadddr expr)))
         (list
          'if
          condition
          (constant-fold (caddr expr))
          (constant-fold (cadddr expr))))]
    [else (map constant-fold expr)]))

    ;; [(all atom? expr)
    ;;  (apply
    ;;   (match-func (car expr))
    ;;   (map traverse (cdr expr)))]


;; (define (all-atom expr)
;;   (all atom? expr))

(constant-fold
 '(list 1 2 3 4
        (+ 1 2 3 4
           (+ 4 5
              (+ 7 8 9 10)))))

(constant-fold
 '(if #t 10 20))

(constant-fold
 '(if '(1 2 3 4) 20 40))

(constant-fold
 '(if 25 10 20))

(constant-fold
 '(if #t
      (lambda (x) 10)
      25))

(constant-fold
 '(lambda (x) (* x x)))


;; TODO make bytecode serializable
;; compile the bytecode for the pipeline


(define (atom? expr)
  (or (number? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)))

(struct MFunction (bindings body))

;; Meta Circular Evaluator
(define (meval expr env)
  (cond [(null? expr) (error! "empty expression")]
        [(atom? expr) expr]
        [(symbol? expr) (lookup env expr)]
        [(equal? (car expr) 'if)
         (if (meval (cadr expr) env)
             (meval (caddr expr) env)
             (meval (cadddr expr) env))]
        [(equal? (car expr) 'lambda)
         (MFunction (cadr expr) (caddr expr))]
        [else ;; hopefully function application?
         (define res (map (lambda (x) (meval x env)) expr))
         (cond
           [(function? (car res))
            (apply (car res) (cdr res))]
           [(MFunction? (car res))
            ;; bind the arguments to the function arguments
            (define new-env
              (bind-args env
                         (MFunction-bindings (car res))
                         (cdr res)))
            (meval (MFunction-body (car res))
                   new-env)])]))


(define (lookup env expr)
  ;; (displayln "looking up: " expr)
  (hash-get env expr))

;; hash -> (listof sym?) -> (listof any?) -> hash
(define (bind-args env list-of-symbols args)
  (if (or (null? list-of-symbols) (null? args))
      env
      (bind-args
       (hash-insert env
                    (car list-of-symbols) (car args))
       (cdr list-of-symbols)
       (cdr args))))

;; bind the functions from my initial environment
;; into the scheme
(define initial-env
  (hash '+ +
        '- -
        '* *
        '/ /
        'list list
        'equal? equal?))


(meval '(list 1 2 3 4) initial-env)
(meval '((lambda (x) 10)) initial-env)
(meval '(if (equal? (+ 1 2 3) 6)
            ((lambda (x) (* x x)) 25)
            420) initial-env)

;; remove unused variables from closures
;; as in '((lambda (x) 30) 25)
;;
;; This should go to -> ((lambda () 30) 25)
;; Which should then map to 25

(define program
  '(if (equal? (+ 1 2 3) 6)
       ((lambda (x) (+ x x)) 25)
       420))


(constant-fold program)


(define (m-atom? expr)
  (or (number? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)
      (symbol? expr)))

;; does a naive walk of the tree to remove
;; the empty function applications
(define (walk expr)
  (cond [(m-atom? expr)
         expr]
        [(empty-function-application? expr)
         (define body
           (transform-empty-function-application expr))
         (walk body)]
        [else
         (map walk expr)]))

(constant-fold
 (walk '(if #t
           ((lambda () 10))
           #f)))



;; try to see if a value is constant by looking in the env and seeing if the closest related
;; definition is a constant, if so substitute the value there and continue

;; returns the bindings for a lambda expression application
(define (lambda-expression-application? expr)
  (if (and (>= (length expr) 2)
           (list? (car expr))
           (equal? (car (car expr)) 'lambda))
      (car (cdr (car expr)))
      '()))

;; this is just an alias here
(define get-application-args cdr)

;; naive substitution of constant arguments
(define (naive-subst* expr env)
  ;; (displayln expr)
  ;; (displayln (symbol? expr))
  (cond [(null? expr) (error! "empty expression")]
        [(atom? expr) expr]
        [(symbol? expr)
         ;; (displayln "inside symbol case with: ")
         ;; (displayln expr)
         ;; (displayln (hash-contains? env expr))
         ;; (displayln env)
         (cond
           [(hash-contains? env expr)
            (define expr-value (lookup env expr))
            (displayln expr-value)
            (if (constant? expr-value)
                expr-value
                expr)]
           [else expr])]
        [(lambda-expression-application? expr)
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
  (naive-subst* expr (hash)))


(constant-fold
 (naive-subst
  (constant-fold
   '((lambda (x) (* x x)) (+ 1 2 3 (+ 1 2 3))))))

(naive-subst '((lambda (x) x) 10))


(define (lambda-application? e)
  (and (not (null? e))
       (list? (car e))
       (equal? (caar e) 'lambda)))

(define args cadr)



;; (define (walk2 expr)
;;   (cond [(m-atom? expr) expr]
;;         [(lambda-application? expr)
;;          (define body (walk2 (get-body-fn (car expr))))
;;          (if (constant? body)
;;              body
;;              (cons
;;               (list 'lambda (args (car expr)) body)
;;               (cdr expr)))]
;;         [else (map walk2 expr)]))



(define-syntax case
  (syntax-rules (else =>)
    [(case arg [else => e1 ...])
     (begin e1 ...)]
    [(case arg [e1 => e2 ...])
     (when (e1 arg) e2 ...)]
    [(case arg [e1 => e2 ...] c1 ...)
     (if (e1 arg)
         (begin e2 ...)
         (case arg c1 ...))]))

(define (walk2 expr)
  (case expr
    [m-atom? => expr]
    [lambda-application? =>
                         (define body (walk2 (get-body-fn (car expr))))
                         (if (constant? body)
                             body
                             (cons
                              (list 'lambda (args (car expr)) body)
                              (cdr expr)))]
    [else => (map walk2 expr)]))


;; (define (pattern-match x)
;;   (case x
;;     [number? =>
;;              (displayln "Found a number!")
;;              (displayln "second statement")]
;;     [function? =>
;;                (displayln "found a function")
;;                (displayln "found some other noise")]
;;     [else => (displayln "else case!")]))


(walk2 '((lambda (x) 10) 2))
(walk2 '((lambda () 10)))

(-> '((lambda (x) (* x x)) (+ 1 2 3 (+ 1 2 3)))
    (constant-fold)
    (naive-subst)
    (constant-fold)
    (walk2)) ;; -> 144


(define *changed* (box #f))
(define (init-changed) (set-box! *changed* #f))
(define (mark-changed) (set-box! *changed* #t))
(define (changed?) (unbox *changed*))


(define *box* (box 0))
(while (not (changed?))
  (if (equal? (unbox *box*) 10)
      (begin
        (displayln "hello")
        (mark-changed)
        (displayln *box*))
      (begin
        (set-box! *box* (+ 1 (unbox *box*)))
        (displayln *box*))))

*box*

;; default arguments need to get handled internally?
;; replace all callsites with these
;; (define (test-function  a (b 10) (c 25))
;;   (+ a b c))

;; -> have to figure out the aliasing rules to see if we can always do this
;; this could also be handled fundamentally - the optional arguments always get
;; put in on the lambda and could be handled that way
;; (define (test func arg1)
;;   (func arg1))

;; ;; default values are going to be handled earlier
;; (test-function)

;; (test-function 10)

;; (test-function 10 20)

;; optional arguments to function




;; (define-pass)
;; pass -> takes an expr, returns an expr
;; could require optional state, could not

;; (walk2
;;  (constant-fold
;;   (naive-subst
;;    (constant-fold
;;     '((lambda (x) (* x x)) (+ 1 2 3 (+ 1 2 3)))))))
