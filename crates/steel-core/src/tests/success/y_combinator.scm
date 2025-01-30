(define Y (lambda (f) ((lambda (x) (x x)) (lambda (x) (f (lambda (y) ((x x) y)))))))

;; head-recursive factorial
(define fac ; fac = (Y f) = (f      (lambda a (apply (Y f) a)))
  (Y (lambda (r) ;     = (lambda (x) ... (r     (- x 1)) ... )
       (lambda (x) ;        where   r    = (lambda a (apply (Y f) a))
         (if (< x 2) ;               (r ... ) == ((Y f) ... )
             1 ;     == (lambda (x) ... (fac  (- x 1)) ... )
             (* x (r (- x 1))))))))

; double-recursive Fibonacci
(define fib
  (Y (lambda (f)
       (lambda (x)
         (if (< x 2)
             x
             (+ (f (- x 1)) (f (- x 2))))))))

(assert! (equal? 720 (dbg! (fac 6))))
(assert! (equal? 233 (dbg! (fib 13))))
