;; Closures that capture, escape, and mutate what they captured.
(define (counter)
  (define n 0)
  (lambda () (set! n (+ n 1)) n))

(define c1 (counter))
(define c2 (counter))

(assert! (equal? (list (c1) (c1) (c2) (c1)) '(1 2 1 3)))

(define (adders n) (map (lambda (i) (lambda (x) (+ x i))) (range 0 n)))
(assert! (equal? (map (lambda (f) (f 100)) (adders 5)) '(100 101 102 103 104)))

(define (make-pair-fns a b)
  (list (lambda () a) (lambda () b) (lambda (v) (set! a v) a)))

(define fns (make-pair-fns 'x 'y))
(assert! (equal? (list ((car fns)) ((cadr fns)) ((caddr fns) 'z) ((car fns))) '(x y z z)))

(define (nested x) (lambda (y) (lambda (z) (list x y z))))
(assert! (equal? (((nested 1) 2) 3) '(1 2 3)))
