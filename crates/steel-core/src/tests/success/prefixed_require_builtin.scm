(require-builtin steel/strings as s.)
(require-builtin steel/lists as l.)

;; a prelude module required with a prefix binds its values under the prefix
(define (len x)
  (s.string-length x))
(assert! (equal? (len "abc") 3))
(assert! (equal? (l.car (list 1 2)) 1))
(assert! (equal? (s.string-upcase "hi") "HI"))

;; as does a macro that uses them
(define-syntax first-len
  (syntax-rules ()
    [(_ xs) (s.string-length (l.car xs))]))
(assert! (equal? (first-len (list "abcd" "e")) 4))

;; the unprefixed names are still the builtins
(assert! (equal? (string-length "ab") 2))
(assert! (equal? (car (list 5)) 5))
