;; A quoted #f is false
(assert! (= (if '#f 1 2) 2))
(define (branch) (if '#f 1 2))
(assert! (= (branch) 2))
(assert! (= (if '#t 1 2) 1))

;; = takes one or more numbers
(assert! (= 1))
(assert! (= 1 1 1))
(assert! (not (= 1 1 2)))
(assert! (= 1 1.0 1))
(assert! (eq? (with-handler (lambda (e) 'err) (= 1 2 "a")) 'err))
(define (all-same a b c) (= a b c))
(assert! (all-same 2 2 2))
(assert! (not (all-same 2 2 3)))
(assert! (apply = (list 4 4 4 4)))
