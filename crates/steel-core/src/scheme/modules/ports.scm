(require-builtin steel/base)
(require "#%private/steel/control")

(provide call-with-port
         call-with-output-file
         call-with-input-file
         call-with-output-string
         with-output-to-string)

(define (call-with-port port proc)
  (let ([ret (proc port)])
    (close-port port)
    ret))

(define (call-with-output-file file proc)
  (call-with-port (open-output-file file) proc))

(define (call-with-input-file file proc)
  (call-with-port (open-input-file file) proc))

(define (call-with-output-string proc)
  (define output-string (open-output-string))
  (proc output-string)
  (get-output-string output-string))

(define (with-output-to-string proc)
  (call-with-output-string (lambda (p)
                             (parameterize ([current-output-port p])
                               (proc)))))
