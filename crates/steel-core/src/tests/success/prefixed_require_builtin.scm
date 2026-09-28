(require-builtin steel/strings as s.)
(require-builtin steel/lists as l.)

(define (len x)
  (s.string-length x))
(assert! (equal? (len "abc") 3))
(assert! (equal? (l.car (list 1 2)) 1))
(assert! (equal? (s.string-upcase "hi") "HI"))

(define-syntax first-len
  (syntax-rules ()
    [(_ xs) (s.string-length (l.car xs))]))
(assert! (equal? (first-len (list "abcd" "e")) 4))

(assert! (equal? (string-length "ab") 2))
(assert! (equal? (car (list 5)) 5))
