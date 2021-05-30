;; Sets of useful ideas for things
;; #lang racket

;(define-syntax match
;  (syntax-rules (list)
;    [(match list x) x]))

;; Do some pattern matching destructuring with this
;; Pull in the let destruct macro and use that destructuring for lists
;; (displayln (match list 10))

(define-syntax destruct
  (syntax-rules (list)
    [(destruct ((list var) ret-value) body ...)
     (let ((ret ret-value))
       (destruct-list ((var) ret) body ...))]
    [(destruct ((list var1 var2 ...) ret-value) body ...)
     (let ((ret ret-value))
       (destruct-list ((var1 var2 ...) ret) body ...))]))

;; destruct a list
;; maybe write a generic match library?
(define-syntax destruct-list
  (syntax-rules (..)
    [(destruct-list ((var ..) ret-value) body ...)
     (let ((var ret-value))
       (begin body ...))]
    ;; This should throw an error if there are remaining values in the list that
    ;; the pattern matching isn't complete
    [(destruct-list ((var) ret-value) body ...)
     (let ((var (car ret-value)))
       (begin body ...))]
    [(destruct-list ((var1 var2 ...) ret-value) body ...)
     (let ((var1 (car ret-value)))
       (destruct-list ((var2 ...) (cdr ret-value))
                      body ...))]))

;; <var> without the <..> means bind the value there
; (destruct ((list a b c) '(1 2 3))
;           (displayln a)
;           (displayln b)
;           (displayln c))

;; <var> .. means bind the rest of the list to that parameter
; (destruct ((list a b c ..) '(1 2 3 4 5))
;           (displayln a)
;           (displayln b)
;           (displayln c))


(define-syntax match-exact
  (syntax-rules ()
    [(match-exact 1)
     10]
    [(match-exact 2)
     20]
    [(match-exact 'test)
     30]))


; (displayln (match-exact 'test))

;; evaluates body if expr is a list
;; errors otherwise
(define-syntax match-list
  (syntax-rules ()
    [(match-list expr body)     
     (let ((exp expr))
       (if (list? exp)
           body
           (error! "match expected a list, found: " exp)))]))

; (match-list (list 1 2 3 4 5) (displayln "Found a list!"))


;; (define (matcher pattern expr)
;;   (cond
;;     [(list? pattern)
;;      (displayln "found a list pattern")]
;;     [(symbol? pattern) (displayln "found a symbol pattern")]
;;     [else (displayln "Could not match")]))


; (matcher '(list 1 2 3) (list 1 2 3))

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
          #f)
      (hash-insert hm key value)))

(define (collect-until-last-p input collected)
  (if (null? (cdr input))
      (list (car input) collected)
      (collect-until-last-p (cdr input) (cons (car input) collected))))


(define (collect-until-last input)
  (collect-until-last-p input '()))


;; Bindings or #false if there is not a match
(define (match-p pattern input bindings)
  (cond [(and (list? pattern)
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

(define (match pattern input)
  (match-p pattern input (hash)))

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

;; Should trim the ...
;; Off of the many symbols to ease with use
(define (match-expr expr)
  (let ((match? (match '(?x) expr)))
    (if match?
        (let ((x (hash-get match? '?x)))
          (display "Found x: ")
          (displayln x))
        (let ((match? (match '(?x ?xs...) expr)))
           (if match?
               (let ((x (hash-get match? '?x))
                     (xs (hash-get match? '?xs...)))
                 (display "Found x: ")
                 (displayln x)
                 (display "Found xs: ")
                 (displayln xs))
               (error! "Unable to match expression: " expr " to any of the given patterns"))))))



(match-expr (list 1 2 3 4 5))




;; Something like
;; -- Pattern -> Match, return bindings on success or nothing on failure
;; -- Take output, use to bind variables in an expression
;; -- Otherwise, emit failure or bail out to the else case
;; -- TODO figure out how to do real time bindings of variables in map to syntax
;; -- TODO make macros only overwrite the span for expressions from the macro, not passed
;;    in ones. This makes error reporting pretty bad for things that aren't the actual syntax of the macro itself
;;    The easiest way to do this would be to rewrite the spans of _only_ the syntax in the macro at the callsite
;;    before rewriting the span of _all_ things
