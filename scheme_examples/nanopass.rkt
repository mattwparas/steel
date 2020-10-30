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


;; See if this maps to a constant function that exists
(define (match-func sym)
  (if (hash-contains? *env* sym)
      (hash-get *env* sym)
      #f))

;; determines if that function actually was bound by the current expression
;; match the
(define (bound? sym)
  (hash-contains? *env* sym))

;; either outputs the symbol used for the definition
;; or false otherwise
(define (define-expr? expr)
  (if (and (equal? 3 (length expr))
           (equal? (car expr) 'define))
      (cdr expr)
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

;; might have to return 2 values each time, the expr and the set
;; this way I can just pass them around everywhere
;; select the first element of the return each time
;; select the second element whenever we want the second one
(define (constant-fold expr bound-vars)
  (cond
    [(constant? expr) (list expr bound-vars)]
    [(define-expr? expr)
     (define new-bound-vars (bind-arg-to-env bound-vars (define-expr? expr)))
     (list (car (constant-fold expr new-bound-vars))
           (new-bound-vars))]
    [(lambda-expression-application? expr)
     (define new-bound-vars (bind-args-to-env bound-vars (lambda-expression-application? expr)))
     (list
      (append (list
               'lambda
               (lambda-expression-application? expr)) ;; args
              (car (constant-fold
                    (get-body-lambda-expression expr)
                    new-bound-vars)))
      new-bound-vars)]
    [(match-if expr)


     ]


    )


  )



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
