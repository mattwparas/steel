; `to-string` joins values with a space
(displayln (to-string "I am" 500 "years old"))

; `define` declares a variable
;
; Steel's operators are like functions
;
; The first argument is `1`. The second argument is `2`
(define three (+ 1 2))

(define actual (to-string "1 + 2 =" three))
(define expected "1 + 2 = 3")

; Functions that end with a question mark `?` are predicates.
; They return a boolean: `#true` or `#false`
;
; The `eq?` predicate returns `#true` if the 2 arguments are
; the same, and `#false` otherwise
;
; The `assert!` function throws an error if its argument is `#false`
(assert! (eq? 10 10))

; But eq? tests for pointer-equality. `actual` and `expected` have the same value:
(displayln actual) ; prints: "1 + 2 = 3"
(displayln expected) ; prints: "1 + 2 = 3"

(assert! (not (eq? actual expected)))

; equal? tests for value-equality, so will be `true` here
(assert! (equal? actual expected))

; Boolean operators
(assert! (and #true #true))
(assert! (or #true #false))
(assert! (not #false))

; This math expression is equivalent to 1 + 3 + (7 - 4) + 7
(assert! (eq? (+ 1 3 (- 7 4 (* 100)) 7) 14))
