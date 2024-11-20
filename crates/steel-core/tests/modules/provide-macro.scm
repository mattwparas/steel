(define-syntax foo
  (syntax-rules ()
    [(_ names ...)
     (begin
       (begin
         (provide names)
         (define names 10)) ...)]))

(foo bar baz)

(define-syntax foocon
  (syntax-rules ()
    [(_ names ...)
     (begin
       (begin
         (provide (contract/out names (->/c any/c int?)))
         (define (names _)
           10)) ...)]))

(foocon barcon bazcon)
