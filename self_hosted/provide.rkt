; #lang racket

(provide get-apple mutate-apple)

(define apple (lambda () 10))

(define (get-apple) apple)

(define (mutate-apple)
    (set! apple (lambda () 100)))

; (make-struct Applesauce (a b c))
