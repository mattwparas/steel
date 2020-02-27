1
(+ 1 1)
#f
())
(define (fac n) (if (= n 1) 1 (* n (fac (- n 1)))))
(fac 5)
(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(fib 10)
(define (fib-helper a b n) (if (= n 0) a (fib-helper b (+ a b) (- n 1))))
(define (fib n) (fib-helper 0 1 n))
(fib 60)