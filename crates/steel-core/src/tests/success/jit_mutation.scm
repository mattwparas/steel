;; Boxes, mutable vectors, and set! on locals and globals from jitted code.
(define g 0)
(define (bump-global n) (if (= n 0) g (begin (set! g (+ g 1)) (bump-global (- n 1)))))
(assert! (equal? (bump-global 1000) 1000))

(define b (box 10))
(define (bump-box n) (if (= n 0) (unbox b) (begin (set-box! b (+ (unbox b) 1)) (bump-box (- n 1)))))
(assert! (equal? (bump-box 500) 510))

(define v (make-vector 8 0))
(define (fill v n) (if (= n 0) v (begin (vector-set! v (- n 1) (* n n)) (fill v (- n 1)))))
(define (swap! v i j)
  (let ([t (vector-ref v i)])
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)
    v))
(define (sum-vec v i acc) (if (= i (vector-length v)) acc (sum-vec v (+ i 1) (+ acc (vector-ref v i)))))

(fill v 8)
(assert! (equal? (vector-ref v 3) 16))
(swap! v 0 7)
(assert! (equal? (vector-ref v 0) 64))
(assert! (equal? (vector-ref v 7) 1))
(assert! (equal? (sum-vec v 0 0) 204))
