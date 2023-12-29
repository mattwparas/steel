;; Sets of useful ideas for things
;; #lang racket

;; ------------------- match functions ----------------------

;; Tells me if this is a free variable (for now)
(define (var? x)
  (and (symbol? x) (starts-with? (symbol->string x) "?")))

(define (ignore? x)
  (equal? x '_))

(define (many? x)
  (and (symbol? x)
       (let ([str (symbol->string x)]) (and (starts-with? str "?") (ends-with? str "...")))))

(define (equal-or-insert hm key value)
  (define existing-value (hash-try-get hm key))
  (if existing-value (if (equal? existing-value value) hm #f) (hash-insert hm key value)))

(define (collect-until-last-p input collected)
  (if (null? (cdr input))
      (list (car input) collected)
      (collect-until-last-p (cdr input) (cons (car input) collected))))

(define (collect-until-last input)
  (collect-until-last-p input '()))

;; Bindings or #false if there is not a match
(define (match-p pattern input bindings)
  (cond
    [(and (list? pattern) (not (null? pattern)) (many? (car pattern)))
     (if (null? (cdr pattern))
         (equal-or-insert bindings (car pattern) input)
         (let ([collected (collect-until-last input)])
           (define remainder (car collected))
           (define collected-list (reverse (car (cdr collected))))
           (if (null? (cdr (cdr pattern)))
               (let ([remainder-bound (equal-or-insert bindings (car (cdr pattern)) remainder)])
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
     (if remaining (match-p (car pattern) (car input) remaining) #f)]))

(define (match-b pattern input)
  (match-p pattern input (hash)))

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

(displayln "--------------------- match tests ----------------------")

;; Matches a pattern explicitly
(test "Simple match" (match-b '?x '(1 2 3 4)) (hash '?x '(1 2 3 4)))

;; If the pattern match fails, return false
(test "Pattern match fails returns false" (match-b '(10 2 ?z 5) '(1 2 3 4)) #f)

;; If the pattern fails because we didn't match exactly, bail
(test "Pattern fails because constants don't match exactly" (match-b '(1 2 3 4 5) '(1 2 3 4)) #f)

;; Should fail
(test "Lengths unequal fails" (match-b '(?x ?y ?z 4 5) '(1 2 3 4)) #f)

;; Should succeed with x y z bound to 1 2 3
(test "Successful pattern match on simple list"
      (match-b '(?x ?y ?z 4 5) '(1 2 3 4 5))
      (hash '?x 1 '?y 2 '?z 3))

;; Should succed with x y z bound to 1 2 3
(test "Nested patterns match" (match-b '(?x (?y ?z)) '(1 (2 3))) (hash '?x 1 '?y 2 '?z 3))

;; Also should work
(test "Deep nested pattern"
      (match-b '(?x (?y (?z (?applesauce ?bananas)))) '(1 (2 (3 (4 5)))))
      (hash '?x 1 '?y 2 '?z 3 '?applesauce 4 '?bananas 5))

;; Also should work
(test "Deep nested pattern with list matching"
      (match-b '(?x (?y (?z (?applesauce ?bananas)))) '(1 (2 (3 (4 (1 2 3 4 5))))))
      (hash '?x 1 '?y 2 '?z 3 '?applesauce 4 '?bananas '(1 2 3 4 5)))

;; Match the bindings
(test "Pattern variables once bound retain their value"
      (match-b '(?x ?y ?x) '(1 2 1))
      (hash '?x 1 '?y 2))

;; Should fail since x doesn't match what was there at first
(test "Matching fails when variable has two different values" (match-b '(?x ?y ?x) '(1 2 3)) #f)

;; Shouldn't fail, should ignore whatever is in the second position
(test "Wildcard ignores the matching at that position"
      (match-b '(?x _ 3) '(1 (1 2 3) 3))
      (hash '?x 1))

;; a => 1
;; x => '(2 3 4 5)
(test "Basic ellipses matching works"
      (match-b '(?a ?x...) '(1 2 3 4 5))
      (hash '?a 1 '?x... '(2 3 4 5)))

(test "Ellipses matches to empty list"
      (match-b '(?first ?rest...) '(1))
      (hash '?first 1 '?rest... '()))

(test "Ellipses matches until next value"
      (match-b '(?first ?rest... ?last) '(1 2 3 4 5))
      (hash '?first 1 '?rest... '(2 3 4) '?last 5))

; TODO this should error out as illegal pattern
(test "Ellipses does not match multiple characters at the end"
      (match-b '(?first ?rest... ?second-last ?last) '(1 2 3 4 5 6))
      #f
      ; (hash '?first 1 '?rest... '(2 3 4) '?last 5 '?last 6)
      )

(test "Ellipses with nested pattern"
      (match-b '(?x (?y ?z (?foo ?bar...))) '(1 (2 3 (4 5 6 7 8 9 10))))
      (hash '?x 1 '?y 2 '?z 3 '?foo 4 '?bar... '(5 6 7 8 9 10)))

(test "Empty pattern matches empty list" (match-b '() '()) (hash))

(test "Empty pattern fails on non empty list" (match-b '() '(1 2 3)) #f)

(test "Single variable with empty list" (match-b '?x '()) (hash '?x '()))

(test "Constant matches constant" (match-b (list 1 2 3) [list 1 2 3]) (hash))

(test "List pattern does not match constant" (match-b (list 1 2 3 4 5) 10) #f)

(test "Constant pattern does not match list" (match-b 10 [list 1 2 3 4 5]) #f)

(test "Wildcard passes" (match-b '_ [list 1 2 3 4 5]) (hash))

;; ----------------- match! syntax --------------------

(define-syntax syntax->pattern
  (syntax-rules ()
    [(syntax->pattern (var1)) '(var1)]
    [(syntax->pattern (var1 var2 ...)) (cons 'var1 (syntax->pattern (var2 ...)))]
    [(syntax->pattern var) 'var]))

(define-syntax syntax-pattern->lets
  (syntax-rules ()
    [(syntax-pattern->lets ((var1)) bindings body) (syntax-pattern->lets (var1) bindings body)]
    [(syntax-pattern->lets ((var1 ...) rest ...) bindings body)
     (syntax-pattern->lets (var1 ...) bindings (syntax-pattern->lets (rest ...) bindings body))]
    [(syntax-pattern->lets (var1) bindings body)
     ;; TODO
     (syntax-const-if var1 body (let ([var1 (hash-try-get bindings 'var1)]) body))]
    [(syntax-pattern->lets (var1 var2 ...) bindings body)
     (syntax-const-if var1
                      (syntax-pattern->lets (var2 ...) bindings body)
                      (let ([var1 (hash-try-get bindings 'var1)])
                        (syntax-pattern->lets (var2 ...) bindings body)))]
    [(syntax-pattern->lets () bindings body) body]
    [(syntax-pattern->lets var bindings body)
     (syntax-const-if var body (let ([var (hash-try-get bindings 'var)]) body))]))

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
     (let ([match? (match-b (syntax->pattern p1) expr)])
       (if match?
           (syntax-pattern->lets p1
                                 match?
                                 (begin
                                   e2 ...))
           (match-dispatch expr c0 c1 ...)))]
    ;; When there isn't an else case given, the last case
    ;; Should include a failure mode
    [(match-dispatch expr (p1 e2 ...))
     (let ([match? (match-b (syntax->pattern p1) expr)])
       (if match?
           (syntax-pattern->lets p1
                                 match?
                                 (begin
                                   e2 ...))
           (error! "Unable to match expression: " expr " to any of the given patterns")))]))

(define-syntax match!
  (syntax-rules ()
    [(match! expr pat) (let ([evald-expr expr]) (match-dispatch evald-expr pat))]
    [(match! expr pat pats ...) (let ([evald-expr expr]) (match-dispatch evald-expr pat pats ...))]))

;; --------------------- match! tests ------------------------

(displayln "--------------------- match! tests ----------------------")

(test "Matches constant patterns" (match! (list 1 2 3 4 5) ((1 2 3 4 5) 'case1)) 'case1)

(test "Matches patterns with constants mixed in"
      (match! (list 1 2 3 4 5) ((?x 2 ?y 4 ?z) (+ ?x ?y ?z)))
      (+ 1 3 5))

(test "Dispatches on first of multiple matching patterns"
      (match! (list 1 2 3 4 5) (?x 'case1) ((?a ?b ?c ? d ?e) 'case2))
      'case1)

(test "Constants match" (match! 10 (10 'case1)) 'case1)

(test "Successfully takes the else case on no match"
      (match! (list 1 (list 2 (list 3))) ((?x ?y ?z) 'case1) (24 'case2) (else 'case3))
      'case3)

(test "Custom map implementation succeeds"
      ((lambda ()
         (define (budget-map func lst)
           (define (loop lst accum)
             (match! lst (() accum) ((?x ?xs...) (loop ?xs... (cons (func ?x) accum)))))
           (reverse (loop lst '())))
         (budget-map (fn (x) (+ x 1)) (list 1 2 3 4 5))))
      '(2 3 4 5 6))

(test "Empty list matches empty list"
      (match! '() (() 'found-empty-list!) ((?x ?xs...) 'found-list!))
      'found-empty-list!)

(test "Nested patterns match with bindings"
      (match! (list (list 1 2) 3 (list 4 (list 5 6))) (((?a 2) ?b (?c (5 6))) (+ ?a ?b ?c)))
      (+ 1 3 4))

;; ;; Ambiguous matches will take the first one that matches
;; (match! (list (list 1 2) 3 (list 4 5))
;;         (() (displayln "Empty pattern!"))
;;         ((?x) (displayln ?x))
;;         ((?x ?y) (displayln (+ ?x ?y)))
;;         (((?x ?y) ?z (?foo ?bar)) (displayln (list ?x ?y ?z ?foo ?bar)))
;;         ((?x ?y ?z...) (displayln ?z...))
;;         (else (displayln "didn't match!")))

;; (define (budget-map func lst)
;;   (define (loop lst accum)
;;     (match! lst
;;             (() accum)
;;             ((?x ?xs...)
;;              (loop ?xs...
;;                    (cons (func ?x) accum)))))
;;   (reverse (loop lst '())))

;; (displayln
;;  (budget-map (fn (x) (+ x 1))
;;             (list 1 2 3 4 5))) ;; => '(2 3 4 5 6)

;; (match! (list 1 2 3 4)
;;         ((?x ?y) (displayln "Shouldn't get here!"))
;;         ((?x 2 ?y 4)
;;          (displayln ?x)
;;          (displayln ?y)))

;; TODO fix bug where using (quote <expr>) instead of '<expr>
;; leads to a parsing error
;; (define-syntax deck
;;   (syntax-rules ()
;;     [(deck (var1))
;;         (begin
;;         (display "Found a one element list ")
;;         (displayln '(var1)))]
;;     [(deck (var1 var2 ...))
;;         (begin
;;         (display "Popping off value: ")
;;         (displayln 'var1)
;;         (deck (var2 ...)))]
;;     [(deck var)
;;      (begin
;;        (display "Found a single expr ")
;;        (displayln 'var))]))

;; (deck (?x ?y ?z))
