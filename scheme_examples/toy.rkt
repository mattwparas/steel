;; Sets of useful ideas for things
(define (test-contract x)
  (lambda (y)
    (equal? (+ x y) 10)))


(define/contract (foo x)
  (->/c (test-contract 4) any/c)
  (+ x 10))


(displayln (foo 6))
