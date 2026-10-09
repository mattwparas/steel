;; A weak box whose value is still held never reads as empty, whatever a
;; collection on another thread is in the middle of.
(define held (box 42))
(define weak (make-weak-box held))

(define (collect n)
  (when (> n 0)
    (#%gc-collect)
    (collect (- n 1))))
(define collecting (spawn-native-thread (lambda () (collect 300))))

(define (read n empties)
  (if (= n 0) empties (read (- n 1) (if (weak-box-value weak) empties (+ empties 1)))))
(assert! (= (read 1000000 0) 0))
(thread-join! collecting)
(assert! (= (unbox (weak-box-value weak)) 42))
