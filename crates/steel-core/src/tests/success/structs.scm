(struct Foo (a b c))

(assert! (equal? (Foo 10 20 30) (Foo 10 20 30)))

(assert! (Foo? (Foo 10 20 30)))
(assert! (not (Foo? 10)))

(define my-foo (Foo 10 20 30))

(assert! (equal? (Foo-a my-foo) 10))
(assert! (equal? (Foo-b my-foo) 20))
(assert! (equal? (Foo-c my-foo) 30))

(struct Bar (a b c) #:transparent)

(assert! (equal? (trim (to-string (Bar 10 20 30))) "(Bar 10 20 30)"))

(struct Baz (a) #:transparent #:mutable)

(define my-baz (Baz 10))

(assert! (equal? (Baz-a my-baz) 10))

(set-Baz-a! my-baz 50)

(assert! (equal? (Baz-a my-baz)))

(struct Callable (func) #:transparent #:prop:procedure 0)

(define callable (Callable (lambda (x) (+ x 10))))

(assert! (equal? (callable 100) 110))
