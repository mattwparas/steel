; `to-string` joins values with a space
(displayln (to-string "I am" 500 "years old"))

; `define` declares a variable
;
; Steel's operators are like functions
;
; The first argument is `1`. The second argument is `2`
(define three (+ 1 2))

; functions that end with an exlamation mark `!` are macros.
;
; The `assert!` macro throws an error if its argument is `#false`
;
; Macros do not evaluate to anything.
; Rather, they expand to more code
;
; `eq?`: functions that return a boolean:
; `#true` (alias `#t`) or `#false` (alias `#f`)
; end with a question mark `?
(assert! (eq? (to-string "1 + 2 =" three "1 + 2 = 3")))

; boolean operators
(assert! (and #t #t))
(assert! (or #t #f))
(assert! (not #f))
