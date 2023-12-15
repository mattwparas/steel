(provide Applesauce
         bananas
         foo-bar-baz
         new-identifier
         one-more-identifier
         another-identifier
         Applesauce-foo
         Applesauce-bar
         Applesauce-baz
         thing-should-not-escape

         my-fun-contracted-function)

(define (bananas)
  (error "Hello world"))

(define (foo-bar-baz)
  10)

(define/contract (my-fun-contracted-function x y)
  (->/c int? int? int?)
  (+ x y))

(define new-identifier 100)

(define one-more-identifier 'foo-bar-baz)
(define another-identifier 100)

(define-syntax thing-should-not-escape
  (syntax-rules ()
    [(thing-should-not-escape x) (thing-should-not-escape2 x)]))

(define-syntax thing-should-not-escape2
  (syntax-rules ()
    [(thing-should-not-escape x) x]))

;; This should be provided!
(struct Applesauce (foo bar baz))
