(define-syntax foo
  (syntax-rules ()
    [(_ names ...)
     (begin
       (begin
         (provide names)
         (define names 10)) ...)]))

(foo bar baz)
