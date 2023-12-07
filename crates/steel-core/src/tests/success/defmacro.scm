(begin-for-syntax
  (define (my-function x)
    (displayln x)))

(defmacro (leave-alone x) (cadr (syntax-e x)))

(leave-alone (define foo-bar 100))

(defmacro (print-value x) (my-function x) (cadr (syntax-e x)))

(print-value (define baz 100))

(assert! (equal? foo-bar 100))
(assert! (equal? baz 100))
