;; Indirect calls at a spread of arities, with branches among the arguments.
;; https://github.com/mattwparas/steel/issues/680 lived here - a branch part way
;; through an argument list left the jit's two arms disagreeing about the stack.
(define (id x) x)

(define (call0 f) (f))
(define (call1 f a) (f a))
(define (call2 f a b) (f a b))
(define (call3 f a b c) (f a b c))

(define (trailing a b c) (list a b c (if c (id a) 0)))
(define (leading a b c) (list (if c (id a) 0) a b c))
(define (middle a b c) (list a (if c (id b) 1) b (and c (id a))))

(assert! (equal? (call1 (lambda (x) (trailing x "y" 'z)) 1) '(1 "y" z 1)))
(assert! (equal? (call1 (lambda (x) (leading x "y" 'z)) 2) '(2 2 "y" z)))
(assert! (equal? (call1 (lambda (x) (middle x "y" 'z)) 3) '(3 "y" "y" 3)))
(assert! (equal? (call0 (lambda () (trailing 'p 'q 'r))) '(p q r p)))
(assert! (equal? (call2 (lambda (a b) (trailing a b 'r)) 'p 'q) '(p q r p)))
(assert! (equal? (call3 (lambda (a b c) (leading a b c)) 'p 'q 'r) '(p p q r)))
