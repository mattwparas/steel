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

;; A quoted keyword is a plain symbol, so it's a positional argument
(define (two a [b 'none] #:k [k 0])
  (list a b k))
(assert! (equal? (two 1 '#:k) '(1 #:k 0)))
(assert! (equal? (two '#:k 2 #:k 3) '(#:k 2 3)))
(assert! (not (equal? (car (list #:k)) '#:k)))
(define (first-of . xs) (car xs))
(assert! (symbol? (first-of '#:k)))

;; Keyword arguments aren't part of the rest list, as in Racket
(define (keys-and-rest #:k [k 0] . rest)
  (list k rest))
(assert! (equal? (keys-and-rest 1 #:k 2 3) '(2 (1 3))))
(assert! (equal? (keys-and-rest 1 '#:k 3) '(0 (1 #:k 3))))

;; Through apply, the keyword is still a keyword
(define (call f . args)
  (apply f args))
(assert! (equal? (call two 1 #:k 5) '(1 none 5)))
(assert! (equal? (message (lambda () (call two 1 #:kk 5)))
                 "two: procedure does not expect an argument with given keyword: #:kk"))
