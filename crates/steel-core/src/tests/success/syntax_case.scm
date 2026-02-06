(define-syntax (hello stx)
  (define foo #'10)
  (define bar #'20)
  (syntax-case stx ()
    [(_ name place)
     (begin
       (when (identifier? #'name)
         (displayln "Found identifier for name:" #'name))
       (when (identifier? #'place)
         (displayln "Found identifier for place:" #'place))
       (with-syntax ([baz #'10])
         #`(list name place #,foo #,bar baz)))]))

(assert! (equal? (hello 500 1000) '(500 1000 10 20 10)))

(define test #`(40 50 60))
(define res #`(list 10 20 30 #,@test))

(assert! (equal? (map syntax-e (syntax-e res)) '(list 10 20 30 40 50 60)))

(define-syntax (loop x)
  (syntax-case x ()
    [(k e ...)
     (with-syntax ([break #'k])
       #'(call-with-current-continuation (lambda (break)
                                           (let f ()
                                             e
                                             ...
                                             (f)))))]))

(define (func)
  (displayln "Hello world!"))

(define (test-compile)
  (loop (func)))

(define-syntax loop2
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax ([break #'k])
         #'(call-with-current-continuation (lambda (break)
                                             (let f ()
                                               e
                                               ...
                                               (f)))))])))

(define (test-compile2)
  (loop2 (func)))

(define-syntax structure-test
  (lambda (stx)
    (syntax-case stx ()
      [(kw (a b c))
       (let ([datum (syntax->datum (syntax (a b c)))])
         (if (and (pair? datum) (eq? (car datum) 'a) (eq? (cadr datum) 'b) (eq? (caddr datum) 'c))
             (syntax (quote correct))
             (syntax (quote wrong))))])))

(assert! (equal? (structure-test (a b c)) 'correct))

(define-syntax make-macro
  (syntax-rules (foo bar baz)
    [(_ name)
     (define-syntax name
       (syntax-rules (foo bar baz)
         [(_) 100]))]))

(define-syntax make-macro2
  (syntax-rules ()
    [(_ name)
     (define-syntax (name stx)
       (syntax-case stx (foo bar baz)
         [(_) 10]))]))

(make-macro foo)
(make-macro2 bar)

(assert! (equal? (foo) 100))
(assert! (equal? (bar) 10))
