;; A box's value is released as soon as its last handle drops, rather than
;; waiting for the collector (see `Drop for HeapRef`).

(define (assert-equal! expected actual)
  (unless (equal? expected actual)
    (error "Expected value" expected "but got" actual)))

;; A cycle through one box: a set! captured variable holding a closure that
;; captures it. Only the collector can free it, and emptying the slot drops
;; the closure, which drops the last handle to the box whose lock the
;; collector is holding. That must not wait on the lock.
(define (make-cycle k)
  (define self #f)
  (set! self (lambda () (list k self)))
  self)
(define (make-cycles n)
  (when (> n 0)
    (make-cycle n)
    (make-cycles (- n 1))))
(make-cycles 20000)
(#%gc-collect)
(make-cycles 20000)
(#%gc-collect)

;; Long chains through boxes, mutable vectors and mutable structs are released
;; without recursing once per link.
(struct node (val next) #:mutable)
(define (box-chain n)
  (let loop ([i 0] [acc '()])
    (if (= i n) acc (loop (+ i 1) (box acc)))))
(define (vector-chain n)
  (let loop ([i 0] [acc '()])
    (if (= i n) acc (loop (+ i 1) (mutable-vector i acc)))))
(define (struct-chain n)
  (let loop ([i 0] [acc '()])
    (if (= i n) acc (loop (+ i 1) (node i acc)))))
(define (mixed-chain n)
  (let loop ([i 0] [acc '()])
    (if (= i n) acc (loop (+ i 1) (list (box (mutable-vector (node i acc))))))))
(define (build-and-drop make)
  (let ([chain (make 200000)])
    (set! chain #f)
    'dropped))
(assert-equal! 'dropped (build-and-drop box-chain))
(assert-equal! 'dropped (build-and-drop vector-chain))
(assert-equal! 'dropped (build-and-drop struct-chain))
(assert-equal! 'dropped (build-and-drop mixed-chain))

;; A value read out of a temporary box or vector stays valid after the
;; container's last handle is gone, including once the read is jitted.
(define (mk k) (list k (+ k 1) (+ k 2)))
(define (churn)
  (let loop ([i 0] [acc '()])
    (if (= i 20) acc (loop (+ i 1) (cons (mk i) acc)))))
(define (from-box k) (let ([v (unbox (box (mk k)))]) (churn) (apply + v)))
(define (tail-unbox k) (unbox (box (mk k))))
(define (from-tail-unbox k) (let ([v (tail-unbox k)]) (churn) (apply + v)))
(define (from-struct k) (let ([v (node-val (node (mk k) '()))]) (churn) (apply + v)))
(define (from-vector k) (let ([v (vector-ref (mutable-vector (mk k) 0) 0)]) (churn) (apply + v)))
(define (check f)
  (let loop ([k 0])
    (when (< k 3000)
      (assert-equal! (+ (* 3 k) 3) (f k))
      (loop (+ k 1)))))
(check from-box)
(check from-tail-unbox)
(check from-struct)
(check from-vector)
