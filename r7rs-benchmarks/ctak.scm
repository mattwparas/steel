;; TODO: This takes _way_ too long. Most likely need to optimize how call/cc works internally.

(define (ctak x y z)
  (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (#%prim.call-with-current-continuation
       (lambda (k)
         (ctak-aux k
                   (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k (- x 1) y z)))
                   (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k (- y 1) z x)))
                   (#%prim.call-with-current-continuation (lambda (k) (ctak-aux k (- z 1) x y))))))))

; (ctak 27 20 11)
(displayln (ctak 32 16 8))

; 1
; 32
; 16
; 8
; 9
