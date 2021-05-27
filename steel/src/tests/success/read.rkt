(define read-value (read "1 2 3 4 5"))
(assert! (equal? '(1 2 3 4 5) read-value))