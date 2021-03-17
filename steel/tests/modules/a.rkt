(require "b.rkt")

(provide a)

(define (a x) (a-private) (b 100))
(define (a-private) "hi this is a private function")