(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 3
(assert! (equal? 3 (a-plus-abs-b 1 2)))
(assert! (equal? 3 (a-plus-abs-b 1 -2)))
