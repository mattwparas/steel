(define-syntax define-and-use
  (syntax-rules ()
    [(_ name)
     (begin
       (define-syntax plus-one
         (syntax-rules ()
           [(_ v) (+ v 1)]))
       (define name (plus-one 1)))]))

(define-and-use two)

(plus-one 10)
