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

(define (uses-boxed x pt)
  (list (boxed x) pt))
(assert! (equal? (uses-boxed 1 'outer) '((#t 1 5) outer)))

(define-syntax define-point
  (syntax-rules ()
    [(_ name) (struct name (x y))]))
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

(define-syntax template-values
  (syntax-rules ()
    [(_ get)
     (begin
       (define-values (list hidden) (values 1 2))
       (define (get)
         (+ list hidden)))]))
(template-values first-sum)
(template-values second-sum)
(assert! (= (first-sum) 3))
(assert! (= (second-sum) 3))
(assert! (equal? (list 1 2) '(1 2)))

(define-syntax template-struct
  (syntax-rules ()
    [(_ get)
     (begin
       (struct hidden-point (x y))
       (define (get)
         (hidden-point-x (hidden-point 7 8))))]))
(template-struct hidden-x)
(assert! (= (hidden-x) 7))

(define hidden-point 10)
(define shared 5)
(define-syntax template-and-use-site
  (syntax-rules ()
    [(_ get)
     (begin
       (define-values (shared other) (values 1 2))
       (define (get)
         (list shared other hidden-point)))]))
(template-and-use-site get-all)
(assert! (equal? (get-all) '(1 2 10)))
(assert! (= shared 5))

(define-syntax template-match
  (syntax-rules ()
    [(_ get)
     (begin
       (define-values (pair extra) (values '(1 2) 2))
       (define (get)
         (match pair
           [(list u v) (+ u v extra)])))]))
(template-match matched-sum)
(assert! (= (matched-sum) 5))

(define-syntax define-with-hidden
  (syntax-rules ()
    [(_ name v) (define-values (name hidden) (values v 0))]))
(define-with-hidden visible 42)
(assert! (= visible 42))

(define-syntax define-and-use
  (syntax-rules ()
    [(_ name)
     (begin
       (define-syntax plus-one
         (syntax-rules ()
           [(_ v) (+ v 1)]))
       (define name (plus-one 1)))]))
(define-and-use two)
(assert! (= two 2))

(define-syntax define-counter
  (syntax-rules ()
    [(_ name)
     (begin
       (define-syntax count-args
         (syntax-rules ()
           [(_) 0]
           [(_ x . rest) (+ 1 (count-args . rest))]))
       (define (name)
         (count-args a b c)))]))
(define-counter three)
(assert! (= (three) 3))

(define-syntax top-first
  (syntax-rules ()
    [(_ l) (car l)]))
(define (top-first-of lst)
  (let ([car cdr]) (top-first lst)))
(assert! (equal? (top-first-of (list 1 2)) 1))

(defmacro (gensym-struct stx)
          (let* ([name (gensym)]
                 [getter (string->symbol (string-append (symbol->string name) "-a"))])
            `(begin
               (struct ,name (a))
               (define gensym-struct-field (,getter (,name 5))))))
(gensym-struct)
(assert! (= gensym-struct-field 5))

(define-syntax define-doubler
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
         [(_ v) (* v 2)]))]))
(define-syntax define-ten
  (syntax-rules ()
    [(_ name)
     (begin
       (define-doubler double)
       (define name (double 5)))]))
(define-ten ten)
(assert! (= ten 10))
(define (local-ten)
  (define-ten ten)
  ten)
(assert! (= (local-ten) 10))

(define-syntax define-constant-fn
  (syntax-rules ()
    [(_ name n)
     (begin
       (define-syntax constant
         (syntax-rules ()
           [(_) n]))
       (define (name)
         (constant)))]))
(define-constant-fn one 1)
(define-constant-fn two-fn 2)
(assert! (equal? (list (one) (two-fn)) (list 1 2)))
