(provide hello-world
         (for-syntax dummy)) 

(define (hello-world) (displayln "hello-world!"))

(define-syntax dummy
  (syntax-rules ()
    [(dummy a b ...)
     (if a (begin b ...) void)]))

(define-syntax dummy2
  (syntax-rules ()
    [(dummy2 a b ...)
     (if a (begin b ...) void)]))