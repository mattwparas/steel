;;; DERIV -- Symbolic derivation.

; (import (scheme base)
;         (scheme cxr)
;         (scheme read)
;         (scheme write)
;         (scheme time))

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(require "common.scm")

(define (deriv a)
  (cond
    [(not (pair? a)) (if (eq? a 'x) 1 0)]
    [(eq? (car a) '+) (cons '+ (map deriv (cdr a)))]
    [(eq? (car a) '-) (cons '- (map deriv (cdr a)))]
    [(eq? (car a) '*) (list '* a (cons '+ (map (lambda (a) (list '/ (deriv a) a)) (cdr a))))]
    [(eq? (car a) '/)
     (list '-
           (list '/ (deriv (cadr a)) (caddr a))
           (list '/ (cadr a) (list '* (caddr a) (caddr a) (deriv (caddr a)))))]
    [else (error #f "No derivation method available")]))

(define (run-benchmark)
  (let* ([count (read)]
         [input1 (read)]
         [output (read)]
         [s (number->string count)]
         [name "deriv"])
    (run-r7rs-benchmark (string-append name ":" s)
                        count
                        (lambda () (deriv (hide count input1)))
                        (lambda (result) (equal? result output)))))
