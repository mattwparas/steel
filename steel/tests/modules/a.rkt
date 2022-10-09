(require "b.rkt")

(provide a update! fetch-value)

(define *volatile* 0)

(define (a x) (a-private) (b 100))
(define (a-private) "hi this is a private function")
(define (update!) 
    (set! *volatile* (+ *volatile* 1)))

(define (fetch-value) *volatile*)