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
