(define (foo x)
  (vector 10 20 30 40 x))

;; Closure should get serialized and sent across the thread
(Ok->value (thread-join! (spawn-thread! (lambda () (displayln (vector-ref (foo 100) 4))))))
