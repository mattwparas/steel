(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

(assert-equal! (apply list 1 2 '(3 4)) '(1 2 3 4))
; matrix transposition
(assert-equal! (apply map list '((1 2 3) (4 5 6))) '((1 4) (2 5) (3 6)))
