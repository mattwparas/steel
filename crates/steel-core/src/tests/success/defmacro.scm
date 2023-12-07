(defmacro (leave-alone x) (cadr (syntax-e x)))

(leave-alone (define foo-bar 100))

(assert! (equal? foo-bar 100))
