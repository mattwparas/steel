;; A procedural macro gets its arguments as they're written, before any macro in them expands
(define-syntax (as-written stx) (syntax-case stx () [(_ e) #''e]))
(assert! (equal? (as-written (when #t 1)) '(when #t 1)))

;; match reads quasiquote patterns itself
(define (shape x)
  (match x
    [`(map ,f ,xs) (list 'map f xs)]
    [`(add ,a ,@rest) (list 'add a rest)]
    [`(nested (,a ,b)) (list a b)]
    [_ 'other]))
(assert! (equal? (shape '(map car lst)) '(map car lst)))
(assert! (equal? (shape '(add 1 2 3)) '(add 1 (2 3))))
(assert! (equal? (shape '(nested (x y))) '(x y)))
(assert! (equal? (shape '(map car)) 'other))

;; unquote and the like in quoted data are symbols
(assert! (equal? (car '(unquote x)) 'unquote))
(assert! (equal? (length '(unquote-splicing x)) 2))
(assert! (equal? '(quasiquote (unquote x)) (list 'quasiquote (list 'unquote 'x))))
