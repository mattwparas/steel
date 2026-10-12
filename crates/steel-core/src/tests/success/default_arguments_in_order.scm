;; A default can use the arguments written before it
(define (after-required a [b (+ a 1)] [c (+ b 10)])
  (list a b c))
(assert! (equal? (after-required 1) '(1 2 12)))
(assert! (equal? (after-required 1 5) '(1 5 15)))
(assert! (equal? (after-required 1 5 7) '(1 5 7)))

(define (all-optional [a 1] [b a] [c (list a b)])
  c)
(assert! (equal? (all-optional) '(1 1)))
(assert! (equal? (all-optional 2) '(2 2)))
(assert! (equal? (all-optional 2 3) '(2 3)))
(assert! (equal? (all-optional 2 3 4) 4))

(define (keyword-first a #:k [k (* a 2)] [c (+ k 1)])
  (list a k c))
(assert! (equal? (keyword-first 1) '(1 2 3)))
(assert! (equal? (keyword-first 1 #:k 5) '(1 5 6)))
(assert! (equal? (keyword-first 1 9 #:k 5) '(1 5 9)))

(define (keyword-last [b 1] #:k [k (+ b 1)])
  (list b k))
(assert! (equal? (keyword-last) '(1 2)))
(assert! (equal? (keyword-last 5) '(5 6)))
(assert! (equal? (keyword-last 5 #:k 0) '(5 0)))

(define (with-rest a [b (* a 2)] . rest)
  (list a b rest))
(assert! (equal? (with-rest 1) '(1 2 ())))
(assert! (equal? (with-rest 1 5 6 7) '(1 5 (6 7))))

;; Called through a value
(define (call f . args)
  (apply f args))
(assert! (equal? (call after-required 1) '(1 2 12)))
(assert! (equal? (call keyword-first 1 #:k 5) '(1 5 6)))
(assert! (equal? (map (lambda (x) (after-required x)) '(1 2)) '((1 2 12) (2 3 13))))

;; Defaults that don't use each other
(define (independent a [b 2] #:k [k 3])
  (list a b k))
(assert! (equal? (independent 1) '(1 2 3)))
(assert! (equal? (independent 1 5) '(1 5 3)))
(assert! (equal? (independent 1 #:k 9) '(1 2 9)))

(define (in-a-loop n acc)
  (if (= n 0) acc (in-a-loop (- n 1) (+ acc (car (after-required n)) (cadr (all-optional n n))))))
(assert! (= (in-a-loop 3000 0) 9003000))
