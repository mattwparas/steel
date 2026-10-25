;; A let whose body folds to a constant keeps the arguments that aren't constant
(assert! (equal? (let ((a car) (b '())) b) '()))
(assert! (equal? (let ((a cdr) (b '(1 2))) b) '(1 2)))
(assert! (equal? ((lambda (a b) b) list '(x)) '(x)))
(assert! (equal? (let ((a (list 1)) (b "s")) b) "s"))
