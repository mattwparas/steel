(define x (mapping (fn (x) x))) ;; identity
(define y (filtering even?)) ;; get only even ones
(define z (taking 15)) ;; take the first 15 from the range
(define xf (compose x y z))
(define result
    (transduce (range 0 100) xf (into-vector)))

(define expected (vector 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28))
(assert! (equal? result expected))