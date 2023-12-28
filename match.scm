;; Sets of useful ideas for things
;; #lang racket

(provide (for-syntax match))

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

  ;; Tells me if this is a free variable (for now)
  ; (define (var? x)
  ;   (and (symbol? x) (starts-with? (symbol->string x) "?")))

  (define (compile-cons-to-list pattern depth)

    (cond

      [(and (list? pattern)
            (not (null? pattern))
            (= (length pattern) 3)
            (equal? (car pattern) 'cons)
            (equal? (caddr pattern) '(quote ())))

       (define first (cadr pattern))
       (define first-depth (if (and (list? first) (not (quoted? first))) 0 (+ 1 depth)))

       ; (displayln "GETTING HERE" first " " first-depth " " depth)

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
  (define (match-p-syntax pattern input final-body-expr depth bound-vars check-var? cdr-depth)

    ; (displayln pattern)
    ; (displayln "Cdr depth: " cdr-depth)

    (cond

      [(quoted? pattern) `(and (equal? ,pattern ,input) ,final-body-expr)]

      [(and (list? pattern) (not (null? pattern)) (= cdr-depth 0))

       ; (displayln "GETTING HERE: " pattern)

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
                                (+ 1 cdr-depth))
               #f)]

         [else (error "list pattern must start with `list - found " (car pattern))])

       ; (if (or (equal? (car pattern) 'list) (equal? (car pattern) 'cons))

       ;     `(if (list? ,input)

       ;          ;; Recur after dropping the list
       ;          ,(match-p-syntax (cdr pattern)
       ;                           input
       ;                           final-body-expr
       ;                           depth
       ;                           bound-vars
       ;                           check-var?
       ;                           (+ 1 cdr-depth))
       ;          #f)

       ;     (error "list pattern must start with `list - found " (car pattern)))
       ]

      [(and (list? pattern) (not (null? pattern)) (starts-with-many? pattern))

       ; (displayln "GETTING HERE " pattern)

       ; (let ([pattern (if (and (= cdr-depth 0) (not (equal? (car pattern) 'list)))
       ;                    (error "Expected `list` to start list pattern")
       ;                    (if (equal? (car pattern) 'list) (cdr pattern) pattern))])

       (if (null? (cddr pattern))

           `(let ([,(car pattern) ,input]) ,final-body-expr)

           `(let ([collected (collect-until-last ,input)])
              ,(if (null? (cdddr pattern))

                   `(let ([,(car (cdr pattern)) (car collected)]
                          [,(car pattern) (reverse (car (cdr collected)))])

                      ,final-body-expr)

                   #f)))
       ; )
       ]

      ;; If the pattern is to be ignored, just return the body - the automatically match
      [(ignore? pattern) final-body-expr]

      ;; If this is a free variable, bind against it.
      ;; Note: We currently don't have the ability to check if this is a free variable
      ;; within the context of the macro expansion
      [(var? pattern)

       (if check-var?
           `(if (equal? ,pattern ,input) ,final-body-expr #f)

           `(let ([,pattern ,input])

              ,final-body-expr))]

      ;; If the pattern is the same, just return whether they match
      [(atom? pattern) `(and (equal? ,pattern ,input) ,final-body-expr)]

      ;; If there is no pattern, just return whether the pattern and input match
      [(null? pattern) `(and (null? ,input) ,final-body-expr)]

      ;; TODO: Not sure how we can even get here?
      ; (displayln "getting here!")
      [(null? input) #f]

      [else

       (define cdr-input-depth (concat-symbols 'cdr-input (number->symbol depth)))
       (define car-input-depth (concat-symbols 'car-input (number->symbol (+ 1 depth))))

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
          (+ 1 cdr-depth)))

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
                    0))
                #f)

           #f)]))

  (define (go-match pattern input final-body-expr)

    ; (displayln pattern)

    (define compile-pattern (compile-cons-to-list pattern 0))

    ; (displayln compile-pattern)

    (match-p-syntax compile-pattern input final-body-expr 0 (hashset) #f 0)))

;; Match a single pattern
(defmacro (single-match expression)
          (define unwrapped (syntax-e expression))
          ;; Unwrapping entirely, not what we want! We want to
          ;; wrap it back up with the span of the original definition!
          (define variable (syntax->datum (second unwrapped)))
          (define pattern (syntax->datum (third unwrapped)))
          (define body (list-ref unwrapped 3))
          (define res (go-match pattern variable body))
          ; (displayln res)
          ; (displayln unwrapped)
          ; (displayln "Variable span: " (syntax-span (second unwrapped)))
          (syntax/loc res
            (syntax-span expression)))

;; Takes the input, generates bindings. But instead of collecting the bindings, we're
;; instead going to generate the instructions that we want to do
; (displayln (go-match '(10 20 (?a) ?b...)
;                      'black-box
;                      '(begin
;                         (displayln "FOUND VALUE: " ?a ?b...)
;                         (+ ?a (sum ?b...)))))

(define black-box (list 10 20 30 40 50))

; (single-match black-box (10 20 ?a ?b...) (displayln "Found value: " ?a ?b...))

; (define black-box (list 10 20))

;; ---------------- tests --------------------

;; Pretty print for testing purposes
;; Throw in the assert as well for testing
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

; ; (displayln "--------------------- match tests ----------------------")

; ;; Matches a pattern explicitly
; (test "Simple match"
;       (match '?x
;         '(1 2 3 4))
;       (hash '?x '(1 2 3 4)))

; ;; If the pattern match fails, return false
; (test "Pattern match fails returns false"
;       (match '(10 2 ?z 5)
;         '(1 2 3 4))
;       #f)

; ;; If the pattern fails because we didn't match exactly, bail
; (test "Pattern fails because constants don't match exactly"
;       (match '(1 2 3 4 5)
;         '(1 2 3 4))
;       #f)

; ;; Should fail
; (test "Lengths unequal fails"
;       (match '(?x ?y ?z 4 5)
;         '(1 2 3 4))
;       #f)

; ;; Should succeed with x y z bound to 1 2 3
; (test "Successful pattern match on simple list"
;       (match '(?x ?y ?z 4 5)
;         '(1 2 3 4 5))
;       (hash '?x 1 '?y 2 '?z 3))

; ;; Should succed with x y z bound to 1 2 3
; (test "Nested patterns match"
;       (match '(?x (?y ?z))
;         '(1 (2 3)))
;       (hash '?x 1 '?y 2 '?z 3))

; ;; Also should work
; (test "Deep nested pattern"
;       (match '(?x (?y (?z (?applesauce ?bananas))))
;         '(1 (2 (3 (4 5)))))
;       (hash '?x 1 '?y 2 '?z 3 '?applesauce 4 '?bananas 5))

; ;; Also should work
; (test "Deep nested pattern with list matching"
;       (match '(?x (?y (?z (?applesauce ?bananas))))
;         '(1 (2 (3 (4 (1 2 3 4 5))))))
;       (hash '?x 1 '?y 2 '?z 3 '?applesauce 4 '?bananas '(1 2 3 4 5)))

; ;; Match the bindings
; (test "Pattern variables once bound retain their value"
;       (match '(?x ?y ?x)
;         '(1 2 1))
;       (hash '?x 1 '?y 2))

; ;; Should fail since x doesn't match what was there at first
; (test "Matching fails when variable has two different values"
; (match (list 1 2 1)
;   [(?x ?y ?x) (+ ?x ?y)])
; #f)

; ;; Shouldn't fail, should ignore whatever is in the second position
; (test "Wildcard ignores the matching at that position"
;       (match '(?x _ 3)
;         '(1 (1 2 3) 3))
;       (hash '?x 1))

; ;; a => 1
; ;; x => '(2 3 4 5)
(test "Basic ellipses matching works"
      (match '(1 2 3 4 5)
        [(list a x ...) x])
      (list 2 3 4 5))

; (test "Ellipses matches to empty list"
;       (match '(?first ?rest...)
;         '(1))
;       (hash '?first 1 '?rest... '()))

(test "Ellipses matches until next value"
      (match (list 1 2 3 4 5)
        [(list first rest ... last) rest])
      (list 2 3 4))

;         '(?first ?rest... ?last)
;   '(1 2 3 4 5))
; (hash '?first 1 '?rest... '(2 3 4) '?last 5))

; ; TODO this should error out as illegal pattern
; (test "Ellipses does not match multiple characters at the end"
;       (match '(?first ?rest... ?second-last ?last)
;         '(1 2 3 4 5 6))
;       #f
;       ; (hash '?first 1 '?rest... '(2 3 4) '?last 5 '?last 6)
;       )

; (test "Ellipses with nested pattern"
;       (match '(?x (?y ?z (?foo ?bar...)))
;         '(1 (2 3 (4 5 6 7 8 9 10))))
;       (hash '?x 1 '?y 2 '?z 3 '?foo 4 '?bar... '(5 6 7 8 9 10)))

; (test "Empty pattern matches empty list"
;       (match '()
;         '())
;       (hash))

; (test "Empty pattern fails on non empty list"
;       (match '()
;         '(1 2 3))
;       #f)

; (test "Single variable with empty list"
;       (match '?x
;         '())
;       (hash '?x '()))

; (test "Constant matches constant"
;       (match (list 1 2 3)
;         [list
;          1
;          2
;          3])
;       (hash))

; (test "List pattern does not match constant"
;       (match (list 1 2 3 4 5)
;         10)
;       #f)

; (test "Constant pattern does not match list"
;       (match 10
;         [list
;          1
;          2
;          3
;          4
;          5])
;       #f)

; (test "Wildcard passes"
;       (match '_
;         [list
;          1
;          2
;          3
;          4
;          5])
;       (hash))

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

; (test "Custom map implementation succeeds"
;       ((lambda ()
;          (define (budget-map func lst)
;            (define (loop lst accum)
;              (match! lst (() accum) ((?x ?xs...) (loop ?xs... (cons (func ?x) accum)))))
;            (reverse (loop lst '())))
;          (budget-map (fn (x) (+ x 1)) (list 1 2 3 4 5))))
;       '(2 3 4 5 6))

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

(define (constant? x)
  (or (number? x) (quoted? x)))

(define (remove-constant-anonymous-functions expr)

  (match expr

    ; [(list (list 'lambda (list var) body) arg) (if (constant? body) body expr)]

    [`((lambda (,var) ,body) ,arg) (if (constant? body) body expr)]

    [(list args...) (map remove-constant-anonymous-functions args...)]

    [_ expr]))

(define my-expr

  '(define (foo-bar x)
     ((lambda (y) 100) x)))

;; Nano pass framework for rewriting and incrementally lowering!
(remove-constant-anonymous-functions my-expr)

(define (identify-sequential-maps expr)

  (match expr
    ; [(list 'map func1 (list 'map func2 lst))
    ;  `(map (lambda (x)

    ;          (,func2 (,func1 x)))
    ;        ,lst)]

    ;; Matching against quasiquotes, should help with real pattern matching?
    [`(map ,func1 (map ,func2 ,lst))
     `(map (lambda (x)

             (,func2 (,func1 x)))
           ,lst)]
    [_ expr]))

(identify-sequential-maps '(map add1 (range 0 100)))
(identify-sequential-maps '(map add1 (map sub1 (range 0 100))))

; (match (list 10 20 30 40)
;   [(list 10 20 30 40) (error "uh oh!")])

; (match '(a b c)
; [`(a ,b ,c) (list b c)])

; (define evald-expr '(a b c))

; (if (list? evald-expr)
;     (if (not (null? evald-expr))
;         (let ([cdr-input0 (cdr evald-expr)] [car-input1 (car evald-expr)])
;           (displayln cdr-input0 car-input1)
;           (and (equal? (quote a) car-input1)
;                (and (null? cdr-input0)
;                     #<syntax:1..39
;                     '(begin
;                        (list b c))
;                     >)))
;         #false)
;     #false)

;; Ambiguous matches will take the first one that matches
; (match (list (list 1 2) 3 (list 4 5))
;   [() (displayln "Empty pattern!")]
;   [(?x) (displayln ?x)]
;   [(?x ?y) (displayln (+ ?x ?y))]
;   [((?x ?y) ?z (?foo ?bar)) (displayln (list ?x ?y ?z ?foo ?bar))]
;   [(?x ?y ?z...) (displayln ?z...)]
;   [else (displayln "didn't match!")])

; ;; (define (budget-map func lst)
; ;;   (define (loop lst accum)
; ;;     (match! lst
; ;;             (() accum)
; ;;             ((?x ?xs...)
; ;;              (loop ?xs...
; ;;                    (cons (func ?x) accum)))))
; ;;   (reverse (loop lst '())))

; ;; (displayln
; ;;  (budget-map (fn (x) (+ x 1))
; ;;             (list 1 2 3 4 5))) ;; => '(2 3 4 5 6)

; ;; (match! (list 1 2 3 4)
; ;;         ((?x ?y) (displayln "Shouldn't get here!"))
; ;;         ((?x 2 ?y 4)
; ;;          (displayln ?x)
; ;;          (displayln ?y)))

; ;; TODO fix bug where using (quote <expr>) instead of '<expr>
; ;; leads to a parsing error
; ;; (define-syntax deck
; ;;   (syntax-rules ()
; ;;     [(deck (var1))
; ;;         (begin
; ;;         (display "Found a one element list ")
; ;;         (displayln '(var1)))]
; ;;     [(deck (var1 var2 ...))
; ;;         (begin
; ;;         (display "Popping off value: ")
; ;;         (displayln 'var1)
; ;;         (deck (var2 ...)))]
; ;;     [(deck var)
; ;;      (begin
; ;;        (display "Found a single expr ")
; ;;        (displayln 'var))]))

; ;; (deck (?x ?y ?z))
