;; Symbols made by native functions are the same objects as the ones written
;; in the program, so eq?, memq and case find them.

(assert! (eq? (concat-symbols 'fo 'o) 'foo))
(assert! (eq? (string->symbol "foo") 'foo))
(assert! (eq? (case (concat-symbols 'a 'b) [(ab) 'yes] [else 'no]) 'yes))

(define json (string->jsexpr "{\"outer\": {\"inner\": [1]}}"))
(define outer (car (hash-keys->list json)))
(assert! (eq? outer 'outer))
(assert! (eq? (car (hash-keys->list (hash-ref json 'outer))) 'inner))

(assert! (eq? (car (car (function-signature string-length))) 'args))
(assert! (eq? (car (cadr (function-signature string-length))) 'returns))
