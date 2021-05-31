(define x (mapping (fn (x) x))) ;; identity
(define y (filtering even?)) ;; get only even ones
(define z (taking 15)) ;; take the first 15 from the range
(define xf (compose x y z))
(assert! 
    (equal? 
        (transduce xf + 0 (range 0 100)) ;; => 210
        210))