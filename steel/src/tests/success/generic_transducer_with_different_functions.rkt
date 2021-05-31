(define x (mapping (fn (x) x))) ;; identity
(define y (filtering even?)) ;; get only even ones
(define z (taking 15)) ;; take the first 15 from the range
(define xf (compose x y z))
(define reduce-func (lambda (accum next) (+ accum next)))
(assert! 
    (equal? 
        (transduce xf reduce-func 0 (range 0 100)) ;; => 210
        210))