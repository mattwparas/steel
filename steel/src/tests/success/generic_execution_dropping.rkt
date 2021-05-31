(define x (mapping (fn (x) x))) ;; identity
(define y (filtering even?)) ;; get only even ones
(define z (dropping 15)) ;; drop the first 15 from the range
(define xf (compose x y z))
(define result
    (execute xf (range 0 40)))

(define expected '(30 32 34 36 38))
(assert! (equal? result expected))