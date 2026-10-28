(define-syntax (defa s) (syntax-case s () [(_) (datum->syntax s (list 'begin (list 'define 'a 1) (list 'define 'a 2)))]))
(define (g) (defa) a)
