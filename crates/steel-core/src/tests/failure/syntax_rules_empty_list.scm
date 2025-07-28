(define-syntax make-args
  (syntax-rules ()
    [() (displayln ...)]))

(make-args #:arg test)
