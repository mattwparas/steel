(define-syntax be-like-begin
  (syntax-rules ()
    [(be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         [(name expr (... ...))
          (begin
            expr
            (... ...))]))]))

(be-like-begin sequence)
(assert! (= (sequence 1 2 3 4) 4))
(assert! (equal? (sequence 'a (list 1 2)) '(1 2)))

(define-syntax quote-with-dots
  (syntax-rules ()
    [(_ x) (quote (... (x ...)))]))
(assert! (equal? (quote-with-dots hello) '(hello ...)))

(define-syntax just-dots
  (syntax-rules ()
    [(_) (quote (... ...))]))
(assert! (eq? (just-dots) '...))

(define-syntax still-escaped
  (syntax-rules ()
    [(_) (quote (... (... ...)))]))
(assert! (equal? (still-escaped) '(... ...)))

(define-syntax make-listers
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-syntax name
         (syntax-rules ()
           [(name item (... ...)) (list 'name item (... ...))])) ...)]))
(make-listers first-lister second-lister)
(assert! (equal? (first-lister 1 2) '(first-lister 1 2)))
(assert! (equal? (second-lister) '(second-lister)))

(define-syntax my-list
  (syntax-rules :::
    []
    [(_ x :::) (list x :::)]))
(assert! (equal? (my-list 1 2 3) '(1 2 3)))

(define-syntax vec-of
  (syntax-rules <dots>
    []
    [(_ x <dots>) (vector x <dots>)]))
(assert! (equal? (vec-of 'a 'b) (vector 'a 'b)))

(define-syntax define-lister
  (syntax-rules :::
    []
    [(_ name prefix :::)
     (define-syntax name
       (syntax-rules ()
         [(_ x ...) (list prefix ::: x ...)]))]))
(define-lister nums 0 0)
(assert! (equal? (nums 1 2 3) '(0 0 1 2 3)))

(define-syntax quote-dots
  (syntax-rules :::
    []
    [(_ x) (quote (x ...))]))
(assert! (equal? (quote-dots hi) '(hi ...)))

(define-syntax escaped-custom
  (syntax-rules :::
    []
    [(_) (quote (::: :::))]))
(assert! (eq? (escaped-custom) ':::))
