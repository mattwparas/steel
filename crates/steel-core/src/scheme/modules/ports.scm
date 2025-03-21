(require-builtin steel/base)

(provide call-with-port
         call-with-output-file
         call-with-input-file)

(define (call-with-port port proc)
  (let ([ret (proc port)])
    (close-port port)
    ret))

(define (call-with-output-file file proc)
  (call-with-port (open-output-file file) proc))

(define (call-with-input-file file proc)
  (call-with-port (open-input-file file) proc))
