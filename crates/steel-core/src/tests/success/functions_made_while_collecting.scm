;; Wrapping a closure as a host function makes a thread of its own for it to
;; run on. One thread makes them while another collects, and neither may
;; wait on the other for good.
(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(define collecting (spawn-native-thread (lambda () (collect 1000) 'done)))

(define (make n last)
  (if (= n 0) last (make (- n 1) (#%closure->boxed-function (lambda (x) x)))))
(assert! (function? (make 1000 #f)))
(assert! (eq? (thread-join! collecting) 'done))
