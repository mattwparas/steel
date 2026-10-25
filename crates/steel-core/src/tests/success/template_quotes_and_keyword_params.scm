;; Quoted data in a nested template keeps its names
(define-syntax outer
  (syntax-rules ()
    [(_ m) (begin (define x 1) (define-syntax m (syntax-rules () [(_) (list 'x `(x ,x) (quote (x y)))])))]))
(outer m)
(assert! (equal? (m) '(x (x 1) (x y))))

(define-syntax outer2
  (syntax-rules ()
    [(_ m) (let ([x 1]) (let-syntax ([m (syntax-rules () [(_) (list 'x (symbol->string 'x) x)])]) (m)))]))
(assert! (equal? (outer2 m) '(x "x" 1)))

;; Keyword and optional parameters a template writes are its own binders
(define-syntax keyword-default
  (syntax-rules () [(_ e) (let ([y 5]) (define (h #:y [y 1]) y) (list (h) (h #:y e) y))]))
(assert! (equal? (keyword-default 9) '(1 9 5)))

(define d 'global-d)
(define-syntax default-sees-local
  (syntax-rules () [(_ e) (let ([d 5]) (define (h #:k [k d]) k) (h))]))
(assert! (= (default-sees-local 0) 5))

(define-syntax keyword-lambda
  (syntax-rules () [(_ e) (let ([y 5]) ((lambda (#:y [y 1]) y) #:y e))]))
(assert! (= (keyword-lambda 9) 9))

;; A keyword lambda applied where it's written, inside a let
(assert! (= (let ([y 5]) ((lambda (#:y [y 1]) y) #:y 9)) 9))
(assert! (= (let ([y 5]) ((lambda (#:y [y 1]) y))) 1))
