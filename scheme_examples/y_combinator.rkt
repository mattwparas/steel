; #lang racket

(define Y 
  (lambda (f)
    ((lambda (x) (x x))
      (lambda (x) (f (lambda (y) ((x x) y)))))))

;; head-recursive factorial
(define fac                ; fac = (Y f) = (f      (lambda a (apply (Y f) a))) 
  (Y (lambda (r)           ;     = (lambda (x) ... (r     (- x 1)) ... )
       (lambda (x)         ;        where   r    = (lambda a (apply (Y f) a))
         (if (< x 2)       ;               (r ... ) == ((Y f) ... )
             1             ;     == (lambda (x) ... (fac  (- x 1)) ... )
             (* x (r (- x 1))))))))
 
 
; double-recursive Fibonacci
(define fib
  (Y (lambda (f)
       (lambda (x)
         (if (< x 2)
             x
             (+ (f (- x 1)) (f (- x 2))))))))



; (display (fac 6))
; (newline)
 
; (display (fib 13))
; (newline)


(define (loop x)
  ; (displayln "looping")
  (test-let ((output (vector 1 2 3 4 5)))
    (test-let ((output2 (vector 1 2 3 4 5)))
      (if (= x 100)
          output2
          (loop (+ x 1))))))


(define (loop x)
  (test-let ((output (vector 1 2 3 4 5))
             (output2 (vector 1 2 3 4 5)))
      (if (= x 1000)
          output2
          (loop (+ x 1)))))


(define (test x)
  (test-let ((y 20))
    (lambda (z) (+ x y z))))