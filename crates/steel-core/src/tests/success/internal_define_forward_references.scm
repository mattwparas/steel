;; Functions in a body that refer to each other, next to a define that only might refer to a
;; later one, still see each other
(define (mutual flag)
  (define (ev n) (if (= n 0) #t (od (- n 1))))
  (define (od n) (if (= n 0) #f (ev (- n 1))))
  (define a (if flag later 0))
  (define later 1)
  (list a (ev 4)))
(assert! (equal? (mutual #f) '(0 #t)))

;; A reference to a later define in a branch that doesn't run is fine
(define (branch flag)
  (define (g) 1)
  (define a (if flag b (g)))
  (define b 2)
  a)
(assert! (equal? (branch #f) 1))

;; A function with rest or optional arguments that captures a later define gets its
;; arguments right
(define (rest-capture)
  (define t (lambda args (list later args)))
  (define later 7)
  (t 1))
(assert! (equal? (rest-capture) '(7 (1))))
(define (optional-capture)
  (define (g [x later]) x)
  (define later 8)
  (g))
(assert! (equal? (optional-capture) 8))

;; A define can refer to itself where that only runs later, as in a function or a branch
;; that isn't taken
(define (self-in-branch x) (define z (if x z 1)) z)
(assert! (equal? (self-in-branch #f) 1))
(define (self-in-lambda) (define y (lambda () y)) (procedure? (y)))
(assert! (self-in-lambda))
