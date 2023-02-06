;; Sets of useful ideas for things
;; #lang racket

;; ------------------- match functions ----------------------

;; Tells me if this is a free variable (for now)
(define (var? x)
  (and (symbol? x)
       (starts-with? (symbol->string x) "?")))

(define (ignore? x)
  (equal? x '_))

(define (many? x)
  (and (symbol? x)
       (let ((str (symbol->string x)))
         (and (starts-with? str "?")
              (ends-with? str "...")))))


(define (equal-or-insert hm key value)
  (define existing-value (hash-try-get hm key))
  (if existing-value
      (if (equal? existing-value value)
          hm
          #f
          ;; (error! "Duplicate bindings found for variable: " key)
          )
      (hash-insert hm key value)))

(define (collect-until-last-p input collected)
  (if (null? (cdr input))
      (list (car input) collected)
      (collect-until-last-p (cdr input) (cons (car input) collected))))


(define (collect-until-last input)
  (collect-until-last-p input '()))


;; Bindings or #false if there is not a match
(define (match-p pattern input bindings)
  (cond
    ;; If its a struct, immediately do the conversion
    ;; TODO bug arity check is not happening here
    [(and (list? pattern) (struct? input)) (match-p pattern (struct->list input) bindings)]
    [(and (list? pattern)
          (not (null? pattern))
          (many? (car pattern)))
     (if (null? (cdr pattern))
         (equal-or-insert bindings (car pattern) input)
         (let ((collected (collect-until-last input)))
           (define remainder (car collected))
           (define collected-list (reverse (car (cdr collected))))
           (if (null? (cdr (cdr pattern)))
               (let ((remainder-bound
                      (equal-or-insert bindings
                                       (car (cdr pattern)) remainder)))
                 (equal-or-insert remainder-bound (car pattern) collected-list))
               #f)))]
    [(var? pattern) (equal-or-insert bindings pattern input)]
    [(ignore? pattern) bindings]
    [(atom? pattern) (if (equal? pattern input) bindings #f)]
    [(null? pattern) (if (null? input) bindings #f)]
    [(null? input) #f]
    [(and (list? pattern) (not (list? input))) #f]
    [else
     (define remaining (match-p (cdr pattern) (cdr input) bindings))
     (if remaining
         (match-p (car pattern) (car input) remaining)
         #f)]))

(define (match pattern input)
  (match-p pattern input (hash)))


;; ---------------- tests --------------------

;; Pretty print for testing purposes
(define (test name input expected)
  (if (equal? input expected)
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display-color "OK" 'green)
        (newline))
      (begin
        (display "> ")
        (display name)
        (display " ... ")
        (display-color "FAILED" 'red)
        (newline)
        (display "    Expected: ")
        (display expected)
        (display ", Found ")
        (displayln input))))

(displayln "--------------------- match tests ----------------------")

;; Matches a pattern explicitly
(test "Simple match" 
      (match '?x '(1 2 3 4))
      (hash '?x '(1 2 3 4)))

;; If the pattern match fails, return false
(test "Pattern match fails returns false"
      (match '(10 2 ?z 5) '(1 2 3 4))
      #f)

;; If the pattern fails because we didn't match exactly, bail
(test "Pattern fails because constants don't match exactly"
      (match '(1 2 3 4 5) '(1 2 3 4))
      #f)

;; Should fail
(test "Lengths unequal fails"
      (match '(?x ?y ?z 4 5) '(1 2 3 4))
      #f)

;; Should succeed with x y z bound to 1 2 3
(test "Successful pattern match on simple list"
      (match '(?x ?y ?z 4 5) '(1 2 3 4 5))
      (hash '?x 1 '?y 2 '?z 3))

;; Should succed with x y z bound to 1 2 3
(test "Nested patterns match"
      (match '(?x (?y ?z)) '(1 '(2 3)))
      (hash '?x 1 '?y 2 '?z 3))

;; Also should work
(test "Deep nested pattern"
      (match '(?x (?y (?z (?applesauce ?bananas))))
        '(1 (2 (3 (4 5)))))
      (hash '?x 1 '?y 2 '?z 3 '?applesauce 4 '?bananas 5))


;; Also should work
(test "Deep nested pattern with list matching"
      (match '(?x (?y (?z (?applesauce ?bananas))))
        '(1 (2 (3 (4 (1 2 3 4 5))))))
      (hash '?x 1 '?y 2 '?z 3 '?applesauce 4 '?bananas '(1 2 3 4 5)))


;; Match the bindings
(test "Pattern variables once bound retain their value"
      (match '(?x ?y ?x) '(1 2 1))
      (hash '?x 1 '?y 2))

;; Should fail since x doesn't match what was there at first
(test "Matching fails when variable has two different values"
      (match '(?x ?y ?x) '(1 2 3))
      #f)

;; Shouldn't fail, should ignore whatever is in the second position
(test "Wildcard ignores the matching at that position"
      (match '(?x _ 3) '(1 (1 2 3) 3))
      (hash '?x 1))

;; a => 1
;; x => '(2 3 4 5)
(test "Basic ellipses matching works"
      (match '(?a ?x...) '(1 2 3 4 5))
      (hash '?a 1 '?x... '(2 3 4 5)))


(test "Ellipses matches to empty list"
      (match '(?first ?rest...) '(1))
      (hash '?first 1 '?rest... '()))


(test "Ellipses matches until next value"
      (match '(?first ?rest... ?last) '(1 2 3 4 5))
      (hash '?first 1 '?rest... '(2 3 4) '?last 5))

; TODO this should error out as illegal pattern
(test "Ellipses does not match multiple characters at the end"
      (match '(?first ?rest... ?second-last ?last) '(1 2 3 4 5 6))
      #f
      ; (hash '?first 1 '?rest... '(2 3 4) '?last 5 '?last 6)
      )

(test "Ellipses with nested pattern"
      (match '(?x (?y ?z (?foo ?bar...))) '(1 (2 3 (4 5 6 7 8 9 10))))
      (hash '?x 1 '?y 2 '?z 3 '?foo 4 '?bar... '(5 6 7 8 9 10)))


(test "Empty pattern matches empty list"
      (match '() '())
      (hash))

(test "Empty pattern fails on non empty list"
      (match '() '(1 2 3))
      #f)

(test "Single variable with empty list"
      (match '?x '())
      (hash '?x '()))

(test "Constant matches constant"
      (match (list 1 2 3) (list 1 2 3))
      (hash))

(test "List pattern does not match constant"
      (match (list 1 2 3 4 5) 10)
      #f)

(test "Constant pattern does not match list"
      (match 10 (list 1 2 3 4 5))
      #f)

(test "Wildcard passes"
      (match '_ (list 1 2 3 4 5))
      (hash))


;; ----------------- match! syntax --------------------


(define-syntax syntax->pattern
  (syntax-rules ()
    [(syntax->pattern (var1))
     '(var1)]
    [(syntax->pattern (var1 var2 ...))
       (cons 'var1 (syntax->pattern (var2 ...)))]
    [(syntax->pattern var)
     'var]))


(define-syntax syntax-pattern->lets
  (syntax-rules ()
    [(syntax-pattern->lets ((var1)) bindings body)
     (syntax-pattern->lets (var1) bindings body)]
    [(syntax-pattern->lets ((var1 ...) rest ...) bindings body)
     (syntax-pattern->lets (var1 ...) bindings
                           (syntax-pattern->lets (rest ...) bindings body))]
    [(syntax-pattern->lets (var1) bindings body)
     ;; TODO
     (syntax-const-if var1 body
                      (let ((var1 (hash-try-get bindings 'var1)))
                        body))]
    [(syntax-pattern->lets (var1 var2 ...) bindings body)
     (syntax-const-if var1
                      (syntax-pattern->lets (var2 ...) bindings body)
                      (let ((var1 (hash-try-get bindings 'var1)))
                        (syntax-pattern->lets (var2 ...) bindings body)))]
    [(syntax-pattern->lets () bindings body)
     body]
    [(syntax-pattern->lets var bindings body)
     (syntax-const-if var body
                      (let ((var (hash-try-get bindings 'var)))
                        body))]))



;; TODO add case for the remaining - when there is no else case given
;; and it should just error out
(define-syntax match-dispatch
  (syntax-rules (else)
    ;; Explicitly giving an else case
    [(match-dispatch expr [else e1 ...])
     (begin e1 ...)]
    ;; Generic recursive case
    [(match-dispatch expr [p1 e2 ...] c1 ...)
     (let ((match? (match (syntax->pattern p1) expr)))
       (if match?
           (syntax-pattern->lets p1 match? (begin e2 ...))
           (match-dispatch expr c1 ...)))]
    ;; When there isn't an else case given, the last case
    ;; Should include a failure mode
    [(match-dispatch expr (p1 e2 ...))
     (let ((match? (match (syntax->pattern p1) expr)))
       (if match?
           (syntax-pattern->lets p1 match? (begin e2 ...))
           (error! "Unable to match expression: " expr " to any of the given patterns")))]))


(define-syntax match!
  (syntax-rules ()
    [(match expr pat)
     (let ((evald-expr expr))
       (match-dispatch evald-expr pat))]
    [(match expr pats ...)
     (let ((evald-expr expr))
       (match-dispatch evald-expr pats ...))]))


;; --------------------- match! tests ------------------------

(displayln "--------------------- match! tests ----------------------")

(test "Matches constant patterns"
      (match! (list 1 2 3 4 5)
              ((1 2 3 4 5) 'case1))
      'case1)


(test "Matches patterns with constants mixed in"
      (match! (list 1 2 3 4 5)
              ((?x 2 ?y 4 ?z) (+ ?x ?y ?z)))
      (+ 1 3 5))

(test "Dispatches on first of multiple matching patterns"
      (match! (list 1 2 3 4 5)
              (?x 'case1)
              ((?a ?b ?c ? d ?e) 'case2))
      'case1)

(test "Constants match"
      (match! 10
              (10 'case1))
      'case1)

(test "Successfully takes the else case on no match"
      (match! (list 1 (list 2 (list 3)))
              ((?x ?y ?z) 'case1)
              (24 'case2)
              (else 'case3))
      'case3)


(test "Custom map implementation succeeds"
      ((lambda ()
         (define (budget-map func lst)
           (define (loop lst accum)
             (match! lst
                     (() accum)
                     ((?x ?xs...)
                      (loop ?xs...
                            (cons (func ?x) accum)))))
           (reverse (loop lst '())))
         (budget-map (fn (x) (+ x 1)) (list 1 2 3 4 5))))
      '(2 3 4 5 6))

(test "Empty list matches empty list"
      (match! '()
              (() 'found-empty-list!)
              ((?x ?xs...) 'found-list!))
      'found-empty-list!)

(test "Nested patterns match with bindings"
      (match! (list (list 1 2) 3 (list 4 (list 5 6)))
              (((?a 2) ?b (?c (5 6))) (+ ?a ?b ?c)))
      (+ 1 3 4))


(define-syntax begin-scope
  (syntax-rules ()
    ((begin-scope body ...)
     ((fn () body ...)))))

(test "Struct matching"
      (begin-scope
         (struct Apples (a b c))
         (match! (Apples 10 30 50)
                 ((Apples ?x ?y ?z) (+ ?x ?y ?z))))
      (+ 10 30 50))


(test "Nested struct matching"
      (begin-scope
        (struct Apples (a b c))
        (match! (Apples (Apples (Apples 10 30 50) 1 2) 3 4)
                ((Apples (Apples (Apples ?x ?y ?z) _ _) _ _)
                 (+ ?x ?y ?z))))
      (+ 10 30 50))


(struct Add (l r))
(struct Sub (l r))
(struct Num (n))
(struct String (s))

(define (calculate expr)
  (match! expr
          ((Num ?n) ?n)
          ((Sub ?l ?r) (- (calculate ?l) (calculate ?r)))
          ((Add ?l ?r) (+ (calculate ?l) (calculate ?r)))))

;; TODO provide stack trace on errors for debug builds
;; collecting stack trace should just be:
;; Go to function stack: Print out last executing functions with their spans
;; Map spans to file + call site - print out underneath the function information
;; Will help with errors inside functions, the callstack will tell me where things actually
;; went wrong


;; (displayln (calculate (Add (Add (Num 10) (Num 25)) (Sub (Num 100) (Num 300)))))

; (struct Some (type inner))
; (struct None (type))


; (struct tuple (inner))
; (displayln (t

;; TODO multi arity functions - how do implement those? just turn everything inside into a list? maybe?

; (displayln (apply list '(1 2 3 4 5 6)))


; (struct Tuple (inner))

; (define-syntax tuple
;   (syntax-rules ()
;     [(tuple arg ...) (Tuple (vector arg ...))]))



; (displayln (tuple 1 2 3 4 5))




; (define-syntax case-lambda
;   (syntax-rules ()
;     ((case-lambda)
;      (lambda args
;        (error "CASE-LAMBDA without any clauses.")))
;     ((case-lambda 
;       (?a1 ?e1 ...) 
;       ?clause1 ...)
;      (lambda args
;        (let ((l (length args)))
;          (case-lambda "CLAUSE" args l 
;            (?a1 ?e1 ...)
;            ?clause1 ...))))
;     ((case-lambda "CLAUSE" ?args ?l 
;       ((?a1 ...) ?e1 ...) 
;       ?clause1 ...)
;      (if (= ?l (length '(?a1 ...)))
;          (apply (lambda (?a1 ...) ?e1 ...) ?args)
;          (case-lambda "CLAUSE" ?args ?l 
;            ?clause1 ...)))
;     ((case-lambda "CLAUSE" ?args ?l
;       ((?a1 . ?ar) ?e1 ...) 
;       ?clause1 ...)
;      (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
;        ?clause1 ...))
;     ((case-lambda "CLAUSE" ?args ?l 
;       (?a1 ?e1 ...)
;       ?clause1 ...)
;      (let ((?a1 ?args))
;        ?e1 ...))
;     ((case-lambda "CLAUSE" ?args ?l)
;      (error "Wrong number of arguments to CASE-LAMBDA."))
;     ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
;       ?clause1 ...)
;      (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
;       ?clause1 ...))
;     ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
;       ?clause1 ...)
;      (if (>= ?l ?k)
;          (apply (lambda ?al ?e1 ...) ?args)
;          (case-lambda "CLAUSE" ?args ?l 
;            ?clause1 ...)))))
  
; (define plus
;       (case-lambda 
;             (() 0)
;             ((x) x)
;             ((x y) (+ x y))
;             ((x y z) (+ (+ x y) z))
;             (args (apply + args))))

; (plus)                     
; (plus 1)                   
; (plus 1 2 3)


;; (test "Matching duplicate names fails"
;;       (match! (list 1 2 3 4 5)
;;               ((?x ?x ?x ?x ?x) 'should-fail))
;;       #f)


;; match struct
;; given a value, destruct it into each variables positions

;; (struct Apple ())
;; (struct Banana ())
;; (struct Fruit ())
;; (struct Burger ())

;; (displayln (struct-ref (Apple) 2))


;; (define (which-struct? s)
;;   (cond [(Apple? s) 'Apple]
;;         [(Banana? s) 'Banana]
;;         [(Fruit? s) 'Fruit]
;;         [else 'Unknown]))

;; (displayln (which-struct? (Apple)))
;; (displayln (which-struct? (Banana)))
;; (displayln (which-struct? (Fruit)))
;; (displayln (which-struct? (Burger)))

;; When its a struct, we want to pop off the first pattern
;; Keywords inside quotes expressions do not work properly
; (displayln (match (quote (Apple y z)) '(x y z)))


;; (displayln (match '(?x y z) (list 10 'y 'z)))


; '(Apple 1 2 3)

;; (struct Applesauce (a b c))

;; (define (struct-pattern->indices pat)
;;       (define (loop pat accum seed)
;;             (if (null? pat)
;;                   accum
;;                   (let ((next-index (+ 1 seed)))
;;                         (loop (cdr pat) (cons next-index accum) next-index))))
;;       (reverse (loop (cdr pat) '() -1)))

;; (displayln (struct-pattern->indices '(Applesauce ?x ?y z)))
;; (displayln (struct-pattern->indices '(Unit-Struct)))


;; (displayln (struct->list (Applesauce 10 45 90)))

;; (displayln
;;  (match '(Applesauce ?x ?y ?z)
;;    (struct->list (Applesauce (Applesauce 10 20 30) 45 90))))


;; (displayln
;;  (match! (Applesauce (Applesauce 10 20 30) 2 3)
;;          ((Applesauce (Applesauce ?x ?y ?z) 2 3) (+ ?x ?y ?z))
;;          (else 'no-match)))

;; (define-syntax match-struct
;;   (syntax-rules ()
;;     [(match-struct struct-name pattern expr)
;;      (if ((datum->syntax struct-name?) expr)
;;          (match (cdr pattern) (struct->list expr)))]))


; (define (match-struct pattern struct bindings)
      


; )


;; (define (match-wrapper pattern expr
