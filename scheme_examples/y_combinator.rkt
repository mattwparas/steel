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


(display (fac 6))
(newline)
 
(display (fib 13))
(newline)


; 0    SCLOSURE : 18
; 1    NDEFS : 1
; 2    FILLLOCALUPVALUE : 0
; 3    SCLOSURE : 10
; 4    NDEFS : 1
; 5    FILLLOCALUPVALUE : 0
; 6    READLOCAL : 0
; 7    READUPVALUE : 0
; 8    READUPVALUE : 0
; 9    FUNC : 1
; 10    FUNC : 1
; 11    POP : 1
; 12    CLOSEUPVALUE : 0
; 13    ECLOSURE : 1
; 14    READUPVALUE : 0
; 15    TAILCALL : 1
; 16    POP : 1
; 17    CLOSEUPVALUE : 1
; 18    ECLOSURE : 1
; 19    SCLOSURE : 7
; 20    NDEFS : 0
; 21    READLOCAL : 0
; 22    READLOCAL : 0
; 23    FUNC : 1
; 24    POP : 1
; 25    CLOSEUPVALUE : 0
; 26    ECLOSURE : 1
; 27    FUNC : 1
; 28    POP : 1
; 29    CLOSEUPVALUE : 1
