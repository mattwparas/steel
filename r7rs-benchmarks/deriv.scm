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
    ;; TODO: Add a pass from the reader to intern the result! That way it matches
    ;; the expected behavior
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

    (displayln count input1 output)

    (run-r7rs-benchmark (string-append name ":" s)
                        count
                        (lambda () (deriv (hide count input1)))
                        (lambda (result) (equal? result output)))))

(with-input-from-file "r7rs-benchmarks/inputs/deriv.input" run-benchmark)

; 10000000

; (+ (* 3 x x) (* a x x) (* b x) 5)

; (+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
;    (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
;    (* (* b x) (+ (/ 0 b) (/ 1 x)))
;    0)

; (displayln (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))
