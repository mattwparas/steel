(define (in-function x)
  (define-syntax twice
    (syntax-rules ()
      [(_ a) (* 2 a)]))
  (twice x))
(assert! (= (in-function 4) 8))

(define in-lambda
  (lambda (a)
    (define-syntax negated
      (syntax-rules ()
        [(_ v) (- v)]))
    (negated a)))
(assert! (= (in-lambda 3) -3))

(assert! (= (let ()
              (define-syntax add
                (syntax-rules ()
                  [(_ a b) (+ a b)]))
              (add 1 3))
            4))

(assert! (= (let ([y 5])
              (define-syntax plus-y
                (syntax-rules ()
                  [(_ a) (+ a y)]))
              (plus-y 1))
            6))

(define (uses-parameters i acc)
  (define-syntax step
    (syntax-rules ()
      [(_) (+ acc i)]))
  (list acc (step)))
(assert! (equal? (uses-parameters 1 2) '(2 3)))

(define (calls-itself n)
  (define (loop i acc)
    (define-syntax next
      (syntax-rules ()
        [(_) (loop (+ i 1) (+ acc i))]))
    (if (= i n)
        acc
        (next)))
  (loop 0 0))
(assert! (= (calls-itself 5) 10))

(define (in-named-let n)
  (let loop ([i 0]
             [acc 0])
    (define-syntax next
      (syntax-rules ()
        [(_) (loop (+ i 1) (+ acc i))]))
    (if (= i n)
        acc
        (next))))
(assert! (= (in-named-let 5) 10))

(define (used-early x)
  (define (helper)
    (doubled x))
  (define-syntax doubled
    (syntax-rules ()
      [(_ a) (* 2 a)]))
  (helper))
(assert! (= (used-early 5) 10))

(define (refers-to-later-define x)
  (define-syntax via-helper
    (syntax-rules ()
      [(_ a) (helper a)]))
  (define (helper v)
    (list 'helped v))
  (via-helper x))
(assert! (equal? (refers-to-later-define 1) '(helped 1)))

(define (nested lst)
  (define-syntax choose
    (syntax-rules ()
      [(_ c t e)
       (cond
         [c t]
         [else e])]))
  (let* ([n (length lst)])
    (define-syntax bump
      (syntax-rules ()
        [(_ v) (+ v n)]))
    (choose (> n 1) (bump 10) (bump 100))))
(assert! (= (nested '(1 2 3)) 13))
(assert! (= (nested '(1)) 101))

(define (in-closures x)
  (define-syntax add1*
    (syntax-rules ()
      [(_ v) (+ v 1)]))
  (define (inner y)
    (add1* (add1* y)))
  (map (lambda (z) (add1* (inner z))) (list x x)))
(assert! (equal? (in-closures 1) '(4 4)))

(define (in-a-begin x)
  (begin
    (define-syntax doubled
      (syntax-rules ()
        [(_ a) (* 2 a)])))
  (when (> x 0)
    (doubled x)))
(assert! (= (in-a-begin 4) 8))

(define in-case-lambda
  (case-lambda
    [(a)
     (define-syntax one
       (syntax-rules ()
         [(_ v) (list 'one v)]))
     (one a)]
    [(a b)
     (define-syntax two
       (syntax-rules ()
         [(_ v w) (list 'two v w)]))
     (two a b)]))
(assert! (equal? (list (in-case-lambda 1) (in-case-lambda 1 2)) '((one 1) (two 1 2))))

(define (with-default #:scale [scale 2] x)
  (define-syntax scaled
    (syntax-rules ()
      [(_ a) (* scale a)]))
  (scaled x))
(assert! (equal? (list (with-default 3) (with-default 3 #:scale 10)) '(6 30)))

(define (recursive n)
  (define-syntax first-true
    (syntax-rules ()
      [(_) #f]
      [(_ a) a]
      [(_ a b ...)
       (let ([t a])
         (if t
             t
             (first-true b ...)))]))
  (first-true #f #f n))
(assert! (= (recursive 9) 9))

(define (with-literal x)
  (define-syntax tag
    (syntax-rules (as)
      [(_ v as name) (list 'name v)]))
  (tag x as thing))
(assert! (equal? (with-literal 1) '(thing 1)))

(define (one-uses-another)
  (define-syntax pairs
    (syntax-rules ()
      [(_ (a b) ...) (list (cons a b) ...)]))
  (define-syntax both
    (syntax-rules ()
      [(_ x) (pairs (x 1) (x 2))]))
  (both 'k))
(assert! (equal? (one-uses-another) '((k . 1) (k . 2))))

(define (hygienic t)
  (define-syntax add-through-tmp
    (syntax-rules ()
      [(_ a b) (let ([tmp a]) (+ tmp b))]))
  (let ([tmp 100]) (add-through-tmp t tmp)))
(assert! (= (hygienic 1) 101))

(define (hidden-by-a-variable)
  (define-syntax m
    (syntax-rules ()
      [(_) 'macro]))
  (list (m) (let ([m (lambda () 'variable)]) (m))))
(assert! (equal? (hidden-by-a-variable) '(macro variable)))

(define-syntax pick
  (syntax-rules ()
    [(_ a) (list 'global a)]))
(define (hides-a-global x)
  (define-syntax pick
    (syntax-rules ()
      [(_ a) (list 'local a)]))
  (pick x))
(assert! (equal? (hides-a-global 1) '(local 1)))
(assert! (equal? (pick 1) '(global 1)))

(define-syntax with-twice
  (syntax-rules ()
    [(_ v)
     (let ([t v])
       (define-syntax twice
         (syntax-rules ()
           [(_ a) (list a a)]))
       (twice t))]))
(assert! (equal? (with-twice 5) '(5 5)))

(begin
  (define-syntax squared
    (syntax-rules ()
      [(_ a) (* a a)]))
  (define nine (squared 3)))
(assert! (equal? (list nine (squared 4)) '(9 16)))

(define (in-a-when x)
  (when x
    (define-syntax tagged
      (syntax-rules ()
        [(_ a) (list 'tagged a)]))
    (tagged x)))
(assert! (equal? (in-a-when 1) '(tagged 1)))

(when #t
  (define-syntax at-the-top
    (syntax-rules ()
      [(_ a) (list 'top a)]))
  (assert! (equal? (at-the-top 1) '(top 1))))

(define-syntax define-constant-macro
  (syntax-rules ()
    [(_ name)
     (define-syntax name
       (syntax-rules ()
         [(_) 100]))]))
(define-constant-macro hundred)
(assert! (= (hundred) 100))

(define (rebound-by-let acc)
  (define-syntax add-acc
    (syntax-rules ()
      [(_ x) (+ x acc)]))
  (let ([acc 1000]) (list acc (add-acc acc))))
(assert! (equal? (rebound-by-let 10) '(1000 1010)))

(define (rebound-by-parameter acc)
  (define-syntax add-acc
    (syntax-rules ()
      [(_ x) (+ x acc)]))
  (define (inner acc)
    (add-acc acc))
  (inner 1000))
(assert! (= (rebound-by-parameter 10) 1010))

(define (assigned-through-a-loop acc)
  (define-syntax bump!
    (syntax-rules ()
      [(_) (set! acc (+ acc 1))]))
  (let loop ([acc 0]
             [i 0])
    (when (< i 3)
      (bump!)
      (loop (+ acc 100) (+ i 1))))
  acc)
(assert! (= (assigned-through-a-loop 10) 13))

(define (rebound-define)
  (define total 5)
  (define-syntax show
    (syntax-rules ()
      [(_ x) (list 'total total x)]))
  (map (lambda (total) (show total)) '(1 2)))
(assert! (equal? (rebound-define) '((total 5 1) (total 5 2))))

(define (nested-captures acc)
  (define-syntax outer
    (syntax-rules ()
      [(_) acc]))
  (let ([acc 2])
    (define-syntax inner
      (syntax-rules ()
        [(_) (list acc (outer))]))
    (let ([acc 3]) (list acc (inner) (outer)))))
(assert! (equal? (nested-captures 1) '(3 (2 1) 1)))

(define (rebound-by-match acc)
  (define-syntax add-acc
    (syntax-rules ()
      [(_ x) (+ x acc)]))
  (match '(1000 2000)
    [(list acc other) (list acc (add-acc other))]))
(assert! (equal? (rebound-by-match 10) '(1000 2010)))

(define-syntax (call-it stx)
  (syntax-case stx ()
    [(_ m x) #'(m x)]))

(define (through-a-procedural-macro y)
  (define-syntax twice
    (syntax-rules ()
      [(_ x) (* 2 x)]))
  (call-it twice y))
(assert! (= (through-a-procedural-macro 21) 42))

(define-syntax define-adder
  (syntax-rules ()
    [(_ name n)
     (define-syntax name
       (syntax-rules ()
         [(_ x) (list 'name (+ x n))]))]))
(define-adder add5 5)
(assert! (equal? (add5 1) '(add5 6)))

(define-syntax define-pair-of-macros
  (syntax-rules ()
    [(_ first second)
     (begin
       (define-syntax first
         (syntax-rules ()
           [(_ x) (car x)]))
       (define-syntax second
         (syntax-rules ()
           [(_ x) (first (cdr x))])))]))
(define-pair-of-macros my-first my-second)
(assert! (equal? (list (my-first '(1 2)) (my-second '(1 2))) '(1 2)))

(define (template-binds-the-name acc)
  (define-syntax doubled
    (syntax-rules ()
      [(_ x) (let ([acc (* 2 x)]) (+ acc 1))]))
  (define-syntax both
    (syntax-rules ()
      [(_ x) (let ([old acc]) (let ([acc x]) (list old acc)))]))
  (define-syntax defined
    (syntax-rules ()
      [(_)
       (let ()
         (define acc 5)
         acc)]))
  (list (doubled acc) (both 7) (defined) acc))
(assert! (equal? (template-binds-the-name 10) '(21 (10 7) 5 10)))

(define-syntax two-values
  (syntax-rules ()
    [(_ x)
     (let ()
       (define-values (acc other) (values x 2))
       (list acc other))]))

(define (template-defines-values acc)
  (define-syntax local-two-values
    (syntax-rules ()
      [(_ x)
       (begin
         (define-values (acc other) (values x 2))
         (list acc other))]))
  (list (two-values acc) (let () (local-two-values 50)) acc))
(assert! (equal? (template-defines-values 10) '((10 2) (50 2) 10)))

(define-syntax (name-of stx)
  (syntax-case stx ()
    [(_ x) (symbol->string (syntax->datum #'x))]))

(define-syntax (name-and-value stx)
  (syntax-case stx ()
    [(_ x) #`(list #,(symbol->string (syntax-e #'x)) x)]))

(define (named-by-a-procedural-macro acc)
  (define-syntax describe
    (syntax-rules ()
      [(_) (list (name-of acc) (name-and-value acc))]))
  (let ([acc 100]) (describe)))
(assert! (equal? (named-by-a-procedural-macro 10) '("acc" ("acc" 10))))

(define (template-matches a)
  (define-syntax sum-of
    (syntax-rules ()
      [(_ v)
       (match v
         [(list a b) (+ a b)])]))
  (list (sum-of '(1 2)) a))
(assert! (equal? (template-matches 10) '(3 10)))

(define (first-of lst)
  (define-syntax first*
    (syntax-rules ()
      [(_ l) (car l)]))
  (let ([car cdr]) (first* lst)))
(assert! (equal? (first-of (list 1 2)) 1))

(define (quoted-vector x)
  (define-syntax show
    (syntax-rules ()
      [(_) (quote #(x (x)))]))
  (show))
(assert! (equal? (quoted-vector 1) (vector 'x (list 'x))))
