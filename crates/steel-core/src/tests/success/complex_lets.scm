; (define (test)
;     (let ((apples (begin (displayln 10) (displayln 20) 10))
;             (bananas (begin (displayln 10) (displayln 20) 30))
;             (foo (begin (displayln 10) (displayln 20) 40)))
;         (+ apples bananas foo)))

    
(define (test)
    (let ((apples (begin (stdout-simple-displayln 10) (stdout-simple-displayln 20) 10))
            (bananas (begin (stdout-simple-displayln 10) (stdout-simple-displayln 20) 30))
            (foo (begin (stdout-simple-displayln 10) (stdout-simple-displayln 20) 40)))
        (+ apples bananas foo)))

(set! test test)

(assert! (equal? (test) 80))
