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
; equal, and `#false` otherwise
(define result (eq? (actual expected)))

; Functions that end with an exclamation mark `!` are macros.
;
; Macros expand to code.
;
; The `assert!` macro throws an error if its argument is `#false`
(assert! result)

; Boolean operators
(assert! (and #true #true))
(assert! (or #true #false))
(assert! (not #false))
