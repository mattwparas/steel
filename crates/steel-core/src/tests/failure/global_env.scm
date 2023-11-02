(define (foo x)
  (#%black-box)
  (list x 20 30 40))

(define list (void))

(foo 10)
