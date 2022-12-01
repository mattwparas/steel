(provide
    list
    cons
    range
    empty
    car
    first
    cdr
    rest
    list-ref
    apply)


(define %list list)
(define list %list)

(define %cons cons)
(define cons %cons)

(define %range range)
(define range %range)

(define %empty? empty)
(define empty %empty)

(define %car car)
(define car %car)

(define %first first)
(define first %first)

(define %cdr cdr)
(define cdr %cdr)

(define %rest rest)
(define rest %rest)

(define %list-ref list-ref)
(define list-ref %list-ref)

(define %apply apply)
(define apply %apply)