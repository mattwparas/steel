;; These _don't_ need to be provided for syntax.
;; However in the mean time, this will work.
(provide match
         match-define)

;; ------------------- match functions ----------------------

;;
(define (collect-until-last-p input collected)
  (if (null? (cdr input))
      (list (car input) collected)
      (collect-until-last-p (cdr input) (cons (car input) collected))))

(define (collect-until-last input)
  (collect-until-last-p input '()))

(define (quoted? x)
  (and (list? x) (not (null? x)) (equal? (car x) 'quote)))

(begin-for-syntax

  (define *gensym-counter* 0)
  (define (gensym)
    (set! *gensym-counter* (+ 1 *gensym-counter*))
    (string->symbol (string-append "##gensym" (to-string *gensym-counter*))))

  (define (gensym-ident identifier)
    (concat-symbols (gensym) 'match identifier))

  (define (compile-cons-to-list pattern depth)
    (cond
      [(and (list? pattern)
            (not (null? pattern))
            (= (length pattern) 3)
            (equal? (car pattern) 'cons)
            (list? (caddr pattern))
            (equal? (caaddr pattern) 'append))

       (define first (cadr pattern))
       (define first-depth (if (and (list? first) (not (quoted? first))) 0 (+ 1 depth)))

       (define rest (cadr (caddr pattern)))

       (if (= depth 0)
           (cons 'list
                 (cons (compile-cons-to-list (cadr pattern) first-depth)
                       (cons (compile-cons-to-list rest (+ 1 depth)) '(...))))

           (cons (compile-cons-to-list (cadr pattern) first-depth)
                 (cons (compile-cons-to-list rest depth) '(...))))]

      [(and (list? pattern)
            (not (null? pattern))
            (= (length pattern) 3)
            (equal? (car pattern) 'cons)
            (equal? (caddr pattern) '(quote ())))

       (define first (cadr pattern))
       (define first-depth (if (and (list? first) (not (quoted? first))) 0 (+ 1 depth)))

       (if (= depth 0)
           (cons 'list (cons (compile-cons-to-list (cadr pattern) first-depth) '()))
           (cons (compile-cons-to-list (cadr pattern) first-depth) '()))]

      [(and (list? pattern)
            (not (null? pattern))
            (= (length pattern) 3)
            (equal? (car pattern) 'cons)
            (list? (caddr pattern))
            (equal? (caaddr pattern) 'cons))

       (define first (cadr pattern))
       (define rest (caddr pattern))

       (define first-depth (if (and (list? first) (not (quoted? first))) 0 (+ 1 depth)))
       (define rest-depth (if (and (list? rest) (not (quoted? rest))) 0 (+ 1 depth)))

       ; (displayln "GETTING HERE with" first " and " rest " " first-depth " " rest-depth " " depth)

       (if (= depth 0)
           (cons 'list
                 (cons (compile-cons-to-list (cadr pattern) first-depth)
                       (compile-cons-to-list (caddr pattern) (+ 1 depth))))

           (cons (compile-cons-to-list (cadr pattern) first-depth)
                 (compile-cons-to-list (caddr pattern) depth)))]

      [else pattern]))

  (define var? symbol?)

  (define (ignore? x)
    (equal? x '_))

  (define (quoted? x)
    (and (list? x) (not (null? x)) (equal? (car x) 'quote)))

  (define (many? x)
    (and
     (symbol? x)
     (let ([str (symbol->string x)])
       ; (and (starts-with? str "?")
       (ends-with?
        str
        "...") ;; TODO: Convert this to a separate symbol, something like "a ..." rather than "a..."
       ; )
       )))

  (define (starts-with-many? pat)
    (and (>= (length pat) 2) (equal? (cadr pat) '...)))

  (define (equal-or-insert hm key value)
    (define existing-value (hash-try-get hm key))
    (if existing-value (if (equal? existing-value value) hm #f) (hash-insert hm key value)))

  (define (number->symbol n)
    (~> n number->string string->symbol))

  ;; TODO: Insert a check to remove the `list` part from the pattern if the cdr-depth is 0?
  (define (match-p-syntax pattern
                          input
                          final-body-expr
                          depth
                          bound-vars
                          check-var?
                          cdr-depth
                          introduced-identifiers)

    (cond
      [(quoted? pattern) `(and (equal? ,pattern ,input) ,final-body-expr)]
      [(and (list? pattern) (not (null? pattern)) (= cdr-depth 0))
       (cond
         [(equal? (car pattern) 'list)
          `(if (list? ,input)
               ;; Recur after dropping the list
               ,(match-p-syntax (cdr pattern)
                                input
                                final-body-expr
                                depth
                                bound-vars
                                check-var?
                                (+ 1 cdr-depth)
                                introduced-identifiers)
               #f)]

         [else (error "list pattern must start with `list - found " (car pattern))])]

      [(and (list? pattern) (not (null? pattern)) (starts-with-many? pattern))
       (if (null? (cddr pattern))
           (begin
             (vector-push! introduced-identifiers (car pattern))

             `(let ([,(car pattern) ,input]) ,final-body-expr))
           (begin

             (vector-push! introduced-identifiers (car (cdr pattern)))
             (vector-push! introduced-identifiers (car pattern))

             `(let ([collected (collect-until-last ,input)])
                ,(if (null? (cdddr pattern))
                     `(let ([,(car (cdr pattern)) (car collected)]
                            [,(car pattern) (reverse (car (cdr collected)))])

                        ,final-body-expr)

                     #f))))]

      ;; If the pattern is to be ignored, just return the body - the automatically match
      [(ignore? pattern) final-body-expr]

      ;; If this is a free variable, bind against it.
      ;; Note: We currently don't have the ability to check if this is a free variable
      ;; within the context of the macro expansion
      [(var? pattern)

       (if check-var?
           `(if (equal? ,pattern ,input) ,final-body-expr #f)
           (begin
             ;; Keep track of the introduced identifiers
             (vector-push! introduced-identifiers pattern)

             `(let ([,pattern ,input]) ,final-body-expr)))]

      ;; If the pattern is the same, just return whether they match
      [(atom? pattern) `(and (equal? ,pattern ,input) ,final-body-expr)]

      ;; If there is no pattern, just return whether the pattern and input match
      [(null? pattern) `(and (null? ,input) ,final-body-expr)]

      ;; TODO: Not sure how we can even get here?
      ; (displayln "getting here!")
      [(null? input) #f]

      [else

       (define cdr-input-depth (gensym-ident (concat-symbols 'cdr-input (number->symbol depth))))
       (define car-input-depth
         (gensym-ident (concat-symbols 'car-input (number->symbol (+ 1 depth)))))

       ;; If the pattern is an atom, then we're going to bind the pattern here!
       (define car-pattern-is-atom? (atom? (car pattern)))
       (define should-check-var?
         (and car-pattern-is-atom? (hashset-contains? bound-vars (car pattern))))

       (define remaining
         (match-p-syntax
          (cdr pattern)
          cdr-input-depth
          final-body-expr
          depth
          (if car-pattern-is-atom? (hashset-insert bound-vars (car pattern)) bound-vars)
          should-check-var?
          ;; Increment the cdr depth since we're traversing across the list
          (+ 1 cdr-depth)
          introduced-identifiers))

       (if remaining

           `(if (not (null? ,input))
                ;; Save our spot in the recursion so we don't have to recompute a bunch
                ;; of stuff
                (let ([,cdr-input-depth (cdr ,input)] [,car-input-depth (car ,input)])
                  ,(match-p-syntax
                    (car pattern)
                    car-input-depth
                    remaining
                    (+ 1 depth)
                    (if car-pattern-is-atom? (hashset-insert bound-vars (car pattern)) bound-vars)
                    should-check-var?
                    0
                    introduced-identifiers))
                #f)

           #f)]))

  (define (go-match pattern input final-body-expr introduced-identifiers)
    (define compile-pattern (compile-cons-to-list pattern 0))
    (match-p-syntax compile-pattern input final-body-expr 0 (hashset) #f 0 introduced-identifiers)))

(defmacro (single-match-define expression)
          (define unwrapped (syntax-e expression))
          (define variable (syntax->datum (second unwrapped)))
          (define pattern (syntax->datum (third unwrapped)))
          (define body (list-ref unwrapped 3))
          (define introduced-identifiers (mutable-vector))
          (define res (go-match pattern variable body introduced-identifiers))
          ;; I _think_ this drains the values from the vector into the list?
          (define list-identifiers (reverse (mutable-vector->list introduced-identifiers)))
          (define temp (gensym))
          (define final-expr
            `(define-values (,@list-identifiers)
               (let ([,temp ,(go-match pattern variable `(list ,@list-identifiers) (mutable-vector))])
                 (if (not (equal? #f))
                     ,temp
                     (error-with-span (quote ,(syntax-span (third unwrapped)))
                                      "Unable to match the given expression: "
                                      ,variable
                                      "to any of the patterns")))))
          ; (displayln (syntax-span expression))
          ; (displayln final-expr)
          (syntax/loc final-expr
            (syntax-span expression)))

;; Match a single pattern
(defmacro (single-match expression)
          (define unwrapped (syntax-e expression))
          ;; Unwrapping entirely, not what we want! We want to
          ;; wrap it back up with the span of the original definition!
          (define variable (syntax->datum (second unwrapped)))
          (define pattern (syntax->datum (third unwrapped)))
          (define body (list-ref unwrapped 3))
          ;; Keep track of all of the identifiers that this
          ;; expression introduces
          ;; TODO: Keep one top level around and clear each time. Then
          ;; we won't keep around any garbage
          (define introduced-identifiers (mutable-vector))
          (define res (go-match pattern variable body introduced-identifiers))
          (syntax/loc res
            (syntax-span expression)))

;; ----------------- match! syntax --------------------

;; TODO add case for the remaining - when there is no else case given
;; and it should just error out
(define-syntax match-dispatch
  (syntax-rules (else)
    ;; Explicitly giving an else case
    [(match-dispatch expr [else e0 e1 ...])
     (begin
       e0
       e1 ...)]
    ;; Generic recursive case
    [(match-dispatch expr [p1 e2 ...] c0 c1 ...)
     (let ([match? (single-match expr
                                 p1
                                 (begin
                                   e2 ...))])
       (if (not (equal? #f match?))
           match?

           (match-dispatch expr c0 c1 ...)))]
    ;; When there isn't an else case given, the last case
    ;; Should include a failure mode
    [(match-dispatch expr (p1 e2 ...))
     (let ([match? (single-match expr
                                 p1
                                 (begin
                                   e2 ...))])
       (if (not (equal? #f match?))
           match?
           (error! "Unable to match expression: " expr " to any of the given patterns")))]))

(define-syntax match-define
  (syntax-rules ()
    [(match-define pattern expr)
     (let ([evald-expr expr]) (single-match-define evald-expr pattern 'empty-body))]))

(define-syntax match
  (syntax-rules ()
    [(match expr
       pat)
     (let ([evald-expr expr]) (match-dispatch evald-expr pat))]
    [(match expr
       pat
       pats ...)
     (let ([evald-expr expr]) (match-dispatch evald-expr pat pats ...))]))

; (match (list 10 20 30 40 50)
;   [(?x 20 ?y 40 ?z) (+ ?x ?y ?z)])

;; --------------------- match! tests ------------------------

; (displayln "--------------------- match! tests ----------------------")
