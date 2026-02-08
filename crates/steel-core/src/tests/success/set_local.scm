(define (foo a)
  (set! a 100)
  a)

(assert! (equal? (foo 50) 100))
