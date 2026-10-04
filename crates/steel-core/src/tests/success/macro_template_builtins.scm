;; A prelude macro's template names builtins. Definitions of those names where
;; the macro is used don't change what it expands to.
(define (cons a b)
  'my-cons)
(define (append . xs)
  'my-append)
(define (list . xs)
  'my-list)
(define (length x)
  'my-length)
(define (apply f . xs)
  'my-apply)

(define (splice x ys)
  `(1 ,x ,@ys 4))

(define dispatch
  (case-lambda
    [(a) (#%prim.list 'one a)]
    [(a b) (#%prim.list 'two a b)]
    [(a . rest) (#%prim.list 'many a rest)]))

(assert! (equal? (splice 9 (#%prim.list 2 3)) (#%prim.list 1 9 2 3 4)))
(assert! (equal? (dispatch 1) (#%prim.list 'one 1)))
(assert! (equal? (dispatch 1 2) (#%prim.list 'two 1 2)))
(assert! (equal? (dispatch 1 2 3) (#%prim.list 'many 1 (#%prim.list 2 3))))

;; The definitions still apply to the code that names them directly.
(assert! (equal? (cons 1 2) 'my-cons))
(assert! (equal? (list 1 2) 'my-list))
(assert! (equal? (length '(1 2)) 'my-length))

;; So does a local, including one handed to the macro.
(define (local-splice cons)
  `(,cons ,@(#%prim.list cons)))
(assert! (equal? (local-splice 5) (#%prim.list 5 5)))
