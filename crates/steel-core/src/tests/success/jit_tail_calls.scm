;; Self tail calls, mutual recursion, and tail calls through a variable.
(define (count-down n acc) (if (= n 0) acc (count-down (- n 1) (+ acc 1))))
(define (even2? n) (if (= n 0) #t (odd2? (- n 1))))
(define (odd2? n) (if (= n 0) #f (even2? (- n 1))))
(define (apply-tail f n) (f n))
(define (loop-through f n acc) (if (= n 0) acc (loop-through f (- n 1) (f acc))))
(define (acc-list n out) (if (= n 0) (length out) (acc-list (- n 1) (cons n out))))

(assert! (equal? (count-down 100000 0) 100000))
(assert! (equal? (list (even2? 10001) (odd2? 10001)) '(#false #true)))
(assert! (equal? (apply-tail (lambda (n) (count-down n 0)) 5000) 5000))
(assert! (equal? (loop-through (lambda (x) (+ x 2)) 10000 0) 20000))
(assert! (equal? (acc-list 20000 '()) 20000))
