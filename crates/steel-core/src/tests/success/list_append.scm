(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "expected value" expected "but got" actual)))

;; empty
(assert-equal! (append) '())
;; proper lists
(assert-equal! (append '(x) '(y) '(z)) '(x y z))
(assert-equal! (append '(a) '() '(b c d)) '(a b c d))
(assert-equal! (append '(a (b)) '((c))) '(a (b) (c)))
;; improper lists
(assert-equal! (append 'a) 'a)
(assert-equal! (append '() 'a) 'a)
(assert-equal! (append '(a b) '(c . d)) '(a b c . d))
(assert-equal! (append '(a b) '() '(c) 'd) '(a b c . d))
