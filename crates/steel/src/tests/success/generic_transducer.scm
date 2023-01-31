(define x (mapping (fn (x) x))) ;; identity
(define y (filtering even?)) ;; get only even ones
(define z (taking 15)) ;; take the first 15 from the range
(define xf (compose x y z))
(assert! 
    (equal? 
        (transduce (range 0 100) xf (into-reducer + 0)) ;; => 210
        210))