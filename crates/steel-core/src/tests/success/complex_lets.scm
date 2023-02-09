(define (test)
    (let ((apples (begin (displayln 10) (displayln 20) 10))
            (bananas (begin (displayln 10) (displayln 20) 30))
            (foo (begin (displayln 10) (displayln 20) 40)))
        (+ apples bananas foo)))

(assert! (equal? (test) 80))