(define/contract (fib n) 
    (->/c integer? integer?)
    (if (<= n 2) 
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

(fib 20)