;; Actually instead put here a hashmap
;; that is exposed by the root enviroment
;;
;; then keep track of the bindings to see if its a thing
;; All that needs to happen is to keep track of bindings
;; at every point
;; if a symbol has been shadowed, then we move on
;; later, we can provide functionality to expose const functions

(define (all func lst)
  (if (null? lst)
      #t
      (if (func (car lst))
          (all func (cdr lst))
          #f)))


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

;; See if this maps to a constant function that exists
(define (match-func sym bound-vars)
  (if (and (hash-contains? *env* sym)
           (not (hashset-contains? bound-vars sym)))
      (hash-get *env* sym)
      #f))

;; determines if that function actually was bound by the current expression
;; match the
(define (bound? sym)
  (hash-contains? *env* sym))

;; either outputs the symbol used for the definition
;; or false otherwise
(define (define-expr? expr)
  (if (and (list? expr)
           (equal? 3 (length expr))
           (equal? (car expr) 'define))
      (cadr expr)
      #f))

(define (lambda-expression-application? expr)
  (if (and (>= (length expr) 2)
           (list? (car expr))
           (equal? (car (car expr)) 'lambda))
      (car (cdr (car expr)))
      '()))


;; TODO go back through and actually fix the prelude
;; reduce is broken there
(define (reduce op z lst)
  (cond ((null? lst) z)    ; just pass it in as another argument
    (else (reduce op
                  (op z (car lst))  ; NB! `z` as first arg
                  (cdr lst)))))

;; returns the updated bound-variable set
(define (bind-args-to-env env args)
  (reduce hashset-insert env args))

(define bind-arg-to-env hashset-insert)

(define get-body-lambda-expression cddr)

(define (match-if expr)
  (equal? (car expr) 'if))

;; (define (match-define expr)
;;   (equal? (car expr) 'define))

(define *changed* (box #f))
(define (init-changed) (set-box! *changed* #f))
(define (mark-changed) (set-box! *changed* #t))
(define (changed?) (unbox *changed*))

;; might have to return 2 values each time, the expr and the set
;; this way I can just pass them around everywhere
;; select the first element of the return each time
;; select the second element whenever we want the second one
(define (constant-fold expr bound-vars)
  ;; (displayln expr)
  (cond
    ;; if its a constant, just return a pair of the expr and bound vars
    [(constant? expr) (list expr bound-vars)]
    ;; if its a definition, continue on the body
    ;; return the new bound vars
    [(define-expr? expr)
     ;; (displayln "inside here")
     (define new-bound-vars (bind-arg-to-env bound-vars (define-expr? expr)))
     (list
      (list 'define (cadr expr) (car (constant-fold (caddr expr) bound-vars)))
      new-bound-vars)]
    ;; if its a function application
    ;; bind the args to whatever is there
    ;; return a pair of the bound vars
    [(lambda-expression-application? expr)
     (define new-bound-vars
       (bind-args-to-env bound-vars (lambda-expression-application? expr)))
     (list
      ;; constant fold the args w/ the new bound variables
      (cons (append (list 'lambda (lambda-expression-application? expr)) ;; args
                    (map (lambda (x)
                           (car (constant-fold x new-bound-vars)))
                         (get-body-lambda-expression (car expr))))
            ;; constant fold the arguments to the function
            (map (lambda (x)
                   (car (constant-fold x bound-vars)))
                 (cdr expr)))
      bound-vars)]
    ;; if we have an if statement
    ;; go ahead and remove the stuff
    [(match-if expr)
     (define condition (car (constant-fold (car (cdr expr)) bound-vars)))
     ;; HACK / TODO
     ;; Find how to differentiate something like a constant expression '(1 2 3 4) which
     ;; evaluates to a list, versus something like '(lambda (x) 10)
     ;; TODO double check that this check is correct - constant? vs constant-not-symbols?
     (list
      (if (constant-not-symbols? condition)
          (cond
            [condition
             (mark-changed)
             (car (constant-fold (caddr expr) bound-vars))]
            [else
             (mark-changed)
             (car (constant-fold (cadddr expr) bound-vars))])
          (list
           'if
           condition
           (car (constant-fold (caddr expr) bound-vars))
           (car (constant-fold (cadddr expr) bound-vars))))
      bound-vars)]

    ;; now if it actually is a function application
    ;; get the constants associated with the
    [(match-func (car expr) bound-vars)
     (define res (map (lambda (x)
                        (car (constant-fold x bound-vars))) expr))
     (if (all constant-not-symbols? (cdr res))
         (begin
           (mark-changed)
           (list (apply (match-func (car expr) bound-vars) (cdr res)) bound-vars))
         (list res bound-vars))]
    [else
     ;; (displayln "else case")
     ;; (displayln expr)
     (list
      (map (lambda (x)
                 (car (constant-fold x bound-vars))) expr)
      bound-vars)]))


;; (define (CF expr)
;;   ;; (init-changed)
;;   (car (constant-fold expr (hashset))))


;; (CF
;;  '(list 1 2 3 4
;;         (+ 1 2 3 4
;;            (+ 4 5
;;               (+ 7 8 9 10)))))

;; (CF
;;  '(if #t 10 20))

;; (CF
;;  '(if '(1 2 3 4) 20 40))

;; (CF
;;  '(if 25 10 20))

;; (CF
;;  '(if #t
;;       (lambda (x) 10)
;;       25))

;; (CF
;;  '(lambda (x) (* x x)))

(define (atom? expr)
  (or (number? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)))

(define lookup hash-get)

;; this is just an alias here
(define get-application-args cdr)

;; hash -> (listof sym?) -> (listof any?) -> hash
(define (bind-args env list-of-symbols args)
  (if (or (null? list-of-symbols) (null? args))
      env
      (bind-args
       (hash-insert env
                    (car list-of-symbols) (car args))
       (cdr list-of-symbols)
       (cdr args))))

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
            (define expr-value (lookup env expr))
            ;; (displayln expr-value)
            (if (constant? expr-value)
                (begin
                  (mark-changed)
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


;; (naive-subst '((lambda (x) x) 10))
;; (changed?)


(define (lambda-application? e)
  (and (not (null? e))
       (list? (car e))
       (equal? (caar e) 'lambda)))

(define (m-atom? expr)
  (or (number? expr)
      (string? expr)
      (integer? expr)
      (boolean? expr)
      (null? expr)
      (symbol? expr)))

;; (define (get-body-fn l)
;;   (last l))

(define get-body-exprs cddr)
(define args cadr)


(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))))



(define (lambda-removal expr)
  (cond [(m-atom? expr) expr]
        [(lambda-application? expr)
         (define bodies (map lambda-removal (get-body-exprs (car expr))))
         ;; TODO
         ;; actually need to handle the case like
         ;; (lambda (x) (list 1 2 3) 100)
         ;; (list 1 2 3) should be removed
         (if (all constant-not-symbols? bodies)
             (begin
               (mark-changed)
               (last bodies))
             (cons
              (append
               (list 'lambda (args (car expr)))
               bodies)
              (cdr expr)))]
        [else (map lambda-removal expr)]))


;; (lambda-removal '((lambda (x) 10) 2))
;; (lambda-removal '((lambda () 10 20 30 40 50 60)))



;; (-> '((lambda (x) (* x x)) (+ 1 2 3 (+ 1 2 3)))
;;     (CF)
;;     (naive-subst)
;;     (CF)
;;     (lambda-removal))


;; (CF '((lambda (x) (* x x)) (+ 1 2 3 (+ 1 2 3))))

;; (define *program* '((list 1 2 3)))

(define (insert-define-var bound-vars expr)
  (let ((v (define-expr? expr)))
    (if v
        (hashset-insert bound-vars v)
        bound-vars)))


;; (define *program*
;;   '((define make-stack (lambda () (quote ())))
;;     (define pop (lambda (stack) (if (null? stack) (quote (#false (quote ()))) (list (car stack) (cdr stack)))))
;;     (define push cons)
;;     (define my-stack (make-stack))
;;     (begin (define pop-val (car (pop ((lambda (x) (push 4 x))
;;                                       ((lambda (x) (push 3 x)) ((lambda (x) (push 2 x))
;;                                                                 ((lambda (x) (push 1 x)) my-stack)))))))
;;            (define new-stack (car (cdr (pop ((lambda (x) (push 4 x)) ((lambda (x) (push 3 x))
;;                                                                           ((lambda (x) (push 2 x))
;;                                                                            ((lambda (x) (push 1 x)) my-stack)))))))))
;;     pop-val
;;     new-stack))


;; if begin statement, lift the defines up to be flattened at the top
;; e.g.
;; (begin (define x 10) (define y 20))
;; should be converted into
;; (define x 10)
;; (define y 20)
;; TODO/HACK
(define (lift-defines-out-of-begin begin-expr)
  (if (all define-expr? (cdr begin-expr))
      (cdr begin-expr)
      (begin-expr)))

(define (begin-expr? expr)
  (and (list? expr)
       (equal? 'begin (car expr))))


(define (flatten-to-define lst)
  (cond ((null? lst) '())
        ((define-expr? lst)
         (list lst))
        ((list? lst)
         (append (flatten-to-define (car lst))
                 (flatten-to-define (cdr lst))))
        (else (list lst))))

(define (lift-defines-out-of-begin begin-expr)
  (define (helper expr)
    (cond
      [(not (list? expr)) expr]
      [(null? expr) expr]
      [(define-expr? expr)
       expr]
      [(begin-expr? expr)
       (map lift-defines-out-of-begin (cdr expr))]
      [else expr]))
  (flatten-to-define (helper begin-expr)))


(define (lift-defines-out-of-begin-2 expr)
  (filter (lambda (x) (not (equal? x 'begin))) (flatten-to-define expr)))


;; TODO
(define (flatten-top-level-defines lst-of-exprs)
  (define (loop input output)
    (cond [(null? input) output]
          [(begin-expr? (car input))
           (loop (cdr input) (append output (lift-defines-out-of-begin (car input))))]
          [else (loop (cdr input) (append output (list (car input))))]))
  (loop lst-of-exprs '()))


;; (flatten-top-level-defines *program*)

;; ----- main thing kinda starts here ----

(define *flattened-program* (flatten-top-level-defines *program*))

(define initial-bound-vars
  (reduce insert-define-var (hashset) *flattened-program*))

(define (CF expr)
  (car (constant-fold expr initial-bound-vars)))


(define (optimize! expr)
  (init-changed)
  (define output
    (-> expr
        (CF)
        (naive-subst)
        (lambda-removal)))
  (if (changed?)
      (begin
        (displayln "program changed!")
        (optimize! output))
      output))


(define (pprint lst)
  (if (null? lst)
      '()
      (begin
        (displayln (car lst))
        (pprint (cdr lst)))))

; (map optimize! *flattened-program*)

(displayln "-------------- BEFORE --------------")
; (displayln *flattened-program*)
(pprint *flattened-program*)
(define *result* (map optimize! *flattened-program*))
(displayln "-------------- AFTER ---------------")
; (displayln *result*)
(pprint *result*)
*result*

;; ----------------------------
