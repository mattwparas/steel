(define (foo x)
  (vector 10 20 30 40 x))

;; TODO: This actually won't work now - displayln references (current-output-port)
;; which is a parameter, which allocates a mutable variable. So for this to work:
;; 1. Certain kinds of ports need to be transferrable across threads
;; 2. Mutable variables need to be transferrable across threads, by being allocated
;;    in the target threads heap
;; 3. Parameters somehow need to be "reinitialized" to have a default value. This should
;;    be doable by having some kind of thread initialization function (like how thread locals work)

;; Closure should get serialized and sent across the thread
; (thread-join! (spawn-thread! (lambda () (stdout-simple-displayln (vector-ref (foo 100) 4)))))
