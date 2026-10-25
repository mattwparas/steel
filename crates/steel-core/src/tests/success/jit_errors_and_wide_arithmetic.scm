;; Arithmetic with many arguments, and errors from car and cdr, in functions
;; called often enough to be compiled.

(define (add5 a b c d e) (+ a b c d e))
(define (sub4 a b c d) (- a b c d))
(define (mul4 a b c d) (* a b c d))
(define (sum-loop n acc)
  (if (= n 0) acc (sum-loop (- n 1) (+ acc (add5 1 2 3 4 5) (sub4 10 1 1 1) (mul4 1 1 1 2)))))
(assert! (= (add5 1 2 3 4 5) 15))
(assert! (= (sum-loop 1000 0) 24000))

(define (first x) (car x))
(define (rest x) (cdr x))
(define (second x) (car (cdr x)))
(define (handled n)
  (if (= n 0)
      'done
      (begin
        (assert! (eq? (with-handler (lambda (e) 'car) (first '())) 'car))
        (assert! (eq? (with-handler (lambda (e) 'cdr) (rest 5)) 'cdr))
        (assert! (eq? (with-handler (lambda (e) 'second) (second (list 1))) 'second))
        (assert! (= (first (list 7)) 7))
        (handled (- n 1)))))
(assert! (eq? (handled 500) 'done))
(assert! (eq? (with-handler (lambda (e) 'top) (car '())) 'top))
