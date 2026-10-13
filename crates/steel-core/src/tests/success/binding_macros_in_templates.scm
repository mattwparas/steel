(define-syntax two-values
  (syntax-rules ()
    [(_ x)
     (let ()
       (define-values (first-value second-value) (values x 2))
       (list first-value second-value))]))
(assert! (equal? (two-values 1) '(1 2)))

(define-syntax boxed
  (syntax-rules ()
    [(_ v)
     (let ()
       (struct pt (x y) #:mutable)
       (define p (pt v 2))
       (set-pt-y! p 5)
       (list (pt? p) (pt-x p) (pt-y p)))]))

;; The names of the struct and of a field are also variables where it is used
(define (uses-boxed x pt)
  (list (boxed x) pt))
(assert! (equal? (uses-boxed 1 'outer) '((#t 1 5) outer)))

(define-syntax define-point
  (syntax-rules ()
    [(_ name)
     (struct name (x y))]))
(define-point point)
(assert! (= (point-y (point 3 4)) 4))

(match-define (list top-a (list top-b top-c)) '(1 (2 3)))
(assert! (equal? (list top-a top-b top-c) '(1 2 3)))

(define (defines-in-a-body x)
  (define offset 10)
  (match-define (list a b) x)
  (+ a b offset))
(assert! (= (defines-in-a-body '(1 2)) 13))

(define (defines-in-a-let x)
  (let ([offset 10])
    (match-define (list a b) x)
    (+ a b offset)))
(assert! (= (defines-in-a-let '(1 2)) 13))

(define-syntax sum-of-pair
  (syntax-rules ()
    [(_ v)
     (let ()
       (match-define (list a b) v)
       (+ a b))]))

(define (uses-sum-of-pair a)
  (list (sum-of-pair a) a))
(assert! (equal? (uses-sum-of-pair '(1 2)) '(3 (1 2))))

;; The variables can come from where the macro is used
(define-syntax bind-pair
  (syntax-rules ()
    [(_ p q v) (match-define (list p q) v)]))

(define (uses-bind-pair v)
  (bind-pair one two v)
  (list two one))
(assert! (equal? (uses-bind-pair '(1 2)) '(2 1)))

(define (in-a-local-macro acc)
  (define-syntax split
    (syntax-rules ()
      [(_ v)
       (let ()
         (match-define (list acc rest) v)
         (list acc rest))]))
  (list (split '(1 2)) acc))
(assert! (equal? (in-a-local-macro 10) '((1 2) 10)))
