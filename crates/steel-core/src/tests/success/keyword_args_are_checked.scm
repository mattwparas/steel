;; A keyword the function doesn't take, or one given twice, is an error, as in Racket
(define (message thunk)
  (with-handler (lambda (e) (error-object-message e)) (thunk)))

(define (f x #:foo [foo 1] #:bar [bar 2])
  (list x foo bar))

(assert! (equal? (f 0 #:foo 10) '(0 10 2)))
(assert! (equal? (f 0 #:bar 5 #:foo 6) '(0 6 5)))
(assert! (equal? (message (lambda () (f 0 #:fooo 10)))
                 "f: procedure does not expect an argument with given keyword: #:fooo"))
(assert! (equal? (message (lambda () (f 0 #:foo 1 #:foo 2)))
                 "f: duplicate keyword in arguments: #:foo"))

;; Through a variable, so the call isn't known when compiling
(define g (if (> (length (list 1 2)) 1) f car))
(assert! (equal? (message (lambda () (g 0 #:baz 1)))
                 "f: procedure does not expect an argument with given keyword: #:baz"))

(define (opt x [y 2])
  (list x y))
(assert! (equal? (opt 1) '(1 2)))
(assert! (equal? (message (lambda () (opt 1 #:y 3)))
                 "opt: procedure does not expect an argument with given keyword: #:y"))

(define (with-rest #:k [k 0] . rest)
  (list k rest))
(assert! (equal? (with-rest #:k 1 2 3) '(1 (2 3))))
(assert! (equal? (message (lambda () (with-rest #:q 1 2)))
                 "with-rest: procedure does not expect an argument with given keyword: #:q"))
