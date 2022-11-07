(require "b.rkt")

(provide a)

; (define *volatile* 0)

(define (a x) (a-private) (b 100))
(define (a-private) "hi this is a private function")
; (define (update!) 
;     (set! *volatile* (+ *volatile* 1)))

; (define (fetch-value) *volatile*)

; (provide capture-func)

; (define (capture-func x)
;     (lambda (y) (+ x y)))