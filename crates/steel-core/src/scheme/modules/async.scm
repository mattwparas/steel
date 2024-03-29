(provide await
         register-async-executor!)

(define (chosen-async-executor x _)
  (local-executor/block-on x))

(define using-async-runtime? #f)

(define (register-async-executor! executor-func)
  (set! using-async-runtime? #t)
  (set! chosen-async-executor executor-func))

(define the-empty-cont #f)
;; Set up this to just swallow everything
(call/cc (lambda (k) (set! the-empty-cont k)))

(define (await future)
  (if using-async-runtime?
      (call/cc (lambda (k)
                 (chosen-async-executor future k)
                 (the-empty-cont k)))

      (local-executor/block-on future)))
