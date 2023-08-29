; (provide (for-syntax lambda))

; (define-syntax lambda
;   (syntax-rules ()
;     [(lambda () expr) (#%plain-lambda () expr)]
;     [(lambda ()
;        expr
;        exprs ...)
;      (#%plain-lambda () expr exprs ...)]
;     [(lambda (x) expr) (#%plain-lambda (x) expr)]
;     [(lambda (x)
;        expr
;        exprs ...)
;      (#%plain-lambda (x) expr exprs ...)]
;     [(lambda (x xs ...)
;        expr
;        exprs ...)
;      (#%plain-lambda (x xs ...) expr exprs ...)]
;     [(lambda x
;        expr
;        exprs ...)
;      (#%plain-lambda x expr exprs ...)]))
