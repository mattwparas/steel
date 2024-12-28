(provide __module__)
(define __module__ 'match-tests)

(define (test name input expected)
  (assert! (equal? input expected))
  (if (equal? input expected)
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display "OK")
        (newline))
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display "FAILED")
        (newline)
        (display "    Expected: ")
        (display expected)
        (display ", Found ")
        (displayln input))))

; ;; a => 1
; ;; x => '(2 3 4 5)
(test "Basic ellipses matching works"
      (match '(1 2 3 4 5)
        [(list a x ...) x])
      (list 2 3 4 5))

(test "Ellipses matches until next value"
      (match (list 1 2 3 4 5)
        [(list first rest ... last) rest])
      (list 2 3 4))

(test "match list of constants"
      (match (list 1 2 3 4 5)
        [(list 1 2 3 4 5) 'case1])
      'case1)

(test "match binds free vars"
      (match (list 1 2 3 4 5)
        [(list x 2 y 4 z) (+ x y z)])
      (+ 1 3 5))

(test "match binds to first matching case"
      (match (list 1 2 3 4 5)
        [?x 'case1]
        [(list ?a ?b ?c ?d ?e) 'case2])
      'case1)

(test "match constant"
      (match 10
        [10 'case1])
      'case1)

(test "takes else case when nothing matches"
      (match (list 1 (list 2 (list 3)))
        [(list x y z) 'case1]
        [24 'case2]
        [else 'case3])
      'case3)

(test "Empty list matches empty list"
      (match '()
        [() 'found-empty-list!]
        [(list x xs...) 'found-list!])
      'found-empty-list!)

(test "match nested list"
      (match (list (list 1 2) 3 (list 4 (list 5 6)))
        [(list (list a 2) b (list c (list 5 6))) (+ a b c)])
      (+ 1 3 4))

(test "wildcards work"
      (match (list 1 2 3 4 5)
        [(list 1 2 _ _ a) a])
      5)

;; Generic optimization passes?
(test "pattern matching against anonymous function application"
      (match '((lambda (x) 10) 20)

        [(list (list 'lambda (list var) body) arg) body])
      10)

(define (quoted? x)
  (and (list? x) (not (null? x)) (equal? (car x) 'quote)))

(define (constant? x)
  (or (number? x) (quoted? x)))

(define (identify-sequential-maps expr)
  (match expr
    ;; Matching against quasiquotes, should help with real pattern matching?
    [`(map ,func1 (map ,func2 ,lst))
     `(map (lambda (x)

             (,func2 (,func1 x)))
           ,lst)]
    [_ expr]))

(identify-sequential-maps '(map add1 (range 0 100)))
(identify-sequential-maps '(map add1 (map sub1 (range 0 100))))

(define my-expr
  '(define (foo-bar x)
     ((lambda (y) 100) x)))

(define (remove-constant-anonymous-functions expr)
  (match expr
    [`((lambda (,var) ,body) ,@args) (if (constant? body) body expr)]
    [(list args ...) (map remove-constant-anonymous-functions args)]
    [_ expr]))

; Nano pass framework for rewriting and incrementally lowering!
(remove-constant-anonymous-functions my-expr)

(define type-table
  (hash '#%prim.cons
        '((any list?) list?)
        '#%prim.car
        '((list?) any)
        '#%prim.cdr
        '((list?) any)
        '#%prim.+
        '((number? number?) number?)))

(define primitive-table
  (hash '#%prim.list?
        'list?
        '#%prim.number?
        'number?
        '#%prim.int?
        'int?
        '#%prim.integer?
        'int?
        '#%prim.string?
        'string?))

;; Lower primitives to unsafe variants, by checking if
;; there are redundant checks for the variable. This can also be done at
;; the contract level, assuming those checks are visible to the optimizer
(define (check-type-info raw-expr type-info)

  (match-syntax
   raw-expr
   ;; Do this generically for every type?
   ;; For primitive checks, we can do this
   [`(if (,primitive-type-check ,variable) ,then-expr ,else-expr)
    ; #:when (equal? (syntax-e ifp) 'if)
    ;; Check the type?
    (define primitive-type-check-type (hash-try-get primitive-table (syntax-e primitive-type-check)))
    (define maybe-type (check-type-info variable type-info))
    ; (displayln "maybe-type" maybe-type)
    ; (displayln "primitive-type-check-type" primitive-type-check-type)
    ;; Check that the expected types are the same - if we've concretely found that they won't
    ;; line up, we should suggest that the then branch is unreachable.
    (when (and maybe-type
               primitive-type-check-type
               (not (equal? 'any maybe-type))
               (not (equal? primitive-type-check-type maybe-type)))
      (displayln "Warning: It appears that the then branch is unreachable"))
    ;; Unify these types, otherwise lift to None
    (let ([then-expr-type (check-type-info
                           then-expr
                           (hash-insert type-info (syntax-e variable) primitive-type-check-type))]
          [else-expr-type (check-type-info else-expr type-info)])

      (if (equal? then-expr-type else-expr-type)

          then-expr-type

          ;; Just promote to the any type
          'any))
    ; (list (check-type-info then-expr
    ;                        (hash-insert type-info (syntax-e variable) primitive-type-check-type))
    ;       (check-type-info else-expr type-info))
    ]
   ;; Function application, check that the variable found in `known-variable`
   ;; matches the expected type from known-variable
   [(list application args ...)
    (define maybe-type (check-type-info application type-info))
    (define args-types (map (lambda (e) (check-type-info e type-info)) args))
    (match maybe-type
      [(list args ret-val)

       (unless (equal? args args-types)
         (error-with-span (syntax-span application)
                          "Type mismatch! expected "
                          args
                          " found "
                          args-types))

       ret-val]

      [any any])
    ;; Application
    ; (define maybe-type (hash-try-get type-info (syntax-e known-variable)))
    ; (define maybe-signature (hash-try-get type-info (syntax-e function-application)))
    ; (when (and maybe-type maybe-signature (not (equal? maybe-type (car maybe-signature))))
    ;   (error-with-span (syntax-span function-application)
    ;                    "Type mismatch! expected "
    ;                    (car maybe-signature)
    ;                    " found "
    ;                    maybe-type))
    ;; This should then return the
    ; (displayln maybe-signature)
    ; (list (check-type-info function-application type-info)
    ;       (check-type-info known-variable type-info))
    ]
   ;; If this doesn't match any of our other forms, recur
   [(list other ...) (map (lambda (e) (check-type-info e type-info)) other)]
   ;; We've bottomed out, just return the collected type information
   [other (or (hash-try-get type-info (syntax-e other)) 'any)]))

(define my-expr2
  (quasisyntax (define loop
                 (lambda (maybe-list)
                   (if (#%prim.list? maybe-list) (#%prim.car maybe-list) (+ maybe-list 10))))))

;; type-check...
(check-type-info my-expr2 type-table)

(define (tile-null-cdr-checks expr)

  (match-syntax expr
                ;; Function call merging optimizations
                [`(#%prim.null? (#%prim.cdr ,expr))
                 ;; Map the span of this object to the span
                 ;; of the incoming one.
                 (syntax/loc (list (syntax/loc '#%prim.cdr-null?
                                     (syntax-span expr))
                                   expr)
                   (syntax-span expr))]
                [(list other ...)
                 (syntax/loc (map tile-null-cdr-checks other)
                   (syntax-span expr))]
                [other other]))

(define test-expr
  (quasisyntax
   (define foo
     (lambda (x)
       (if (#%prim.null? (#%prim.cdr x)) (displayln "EMPTY CDR") (displayln "NOT EMPTY CDR"))))))

(define res (tile-null-cdr-checks test-expr))

(define-syntax define/lint
  (syntax-rules ()

    [(_ (name expr) pat ...)

     (define name
       (lambda (expr)

         ;;
         (match-syntax expr pat ... [other other])

         (match-syntax expr
                       [(list other ...)
                        (syntax/loc (map name other)
                          (syntax-span expr))]
                       [other other])))]))

;; define/lint
(define/lint (null-cdr-check expr)
             [`(#%prim.null? (#%prim.cdr ,expr))
              (displayln "Consider turning this into #%prim.cdr-null?")])

;; This works!
(define cdr-check-test
  (quasisyntax (define foo
                 (lambda (x)
                   (if (#%prim.null? (#%prim.cdr (#%prim.null? (#%prim.cdr x))))
                       (displayln "EMPTY CDR")
                       (displayln "NOT EMPTY CDR"))))))

(null-cdr-check cdr-check-test)

(define (flatten-cons expr)
  (match-syntax expr
                [`(#%prim.cons ,x (#%prim.list ,@expr))
                 (syntax/loc (cons (syntax/loc '#%prim.list
                                     (syntax-span (car expr)))
                                   (cons x expr))

                   (syntax-span (car expr)))]
                [(list other ...)
                 (syntax/loc (map flatten-cons other)
                   (syntax-span expr))]
                [other other]))

(flatten-cons
 (quasisyntax
  (define foo
    (#%prim.cons 10 (#%prim.cons 20 (#%prim.cons 30 (#%prim.cons 40 (#%prim.list 10 20))))))))
