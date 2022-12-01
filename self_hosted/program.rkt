; (define foo 
;         (lambda (x) 
;             (+ (if #t x 20) 
;                 x 
;                 (begin 10 20 30 
;                     (set! x 30)))))


(define hello-world (lambda () (displayln "hello world!")))

(hello-world)