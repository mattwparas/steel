(require-builtin steel/base)
(require "#%private/steel/control")

(provide call-with-port
         call-with-output-file
         call-with-input-file
         call-with-output-string
         call-with-input-string
         with-output-to-string
         with-input-from-string)

;;@doc
;; Calls the given *proc* with the *port*.
;; If *proc* returns, then the port will be closed and the return value of *proc* returned.
;;
;; (call-with-port port proc) -> any/c
;;
;; - port : port?
;; - proc : procedure?
(define (call-with-port port proc)
  (let ([ret (proc port)])
    (close-port port)
    ret))

;;@doc
;; Calls the given *proc* with an output port obtained opening *file*.
;; If *proc* returns, then the temporary port will be closed and the return value of *proc* returned.
;;
;; (call-with-output-file file proc) -> any/c
;;
;; - file : string?
;; - proc : procedure?
(define (call-with-output-file file proc)
  (call-with-port (open-output-file file) proc))

;;@doc
;; Calls the given *proc* with an input port obtained opening *file*.
;; If *proc* returns, then the temporary port will be closed and the return value of *proc* returned.
;;
;; (call-with-input-file file proc) -> any/c
;;
;; - file : string?
;; - proc : procedure?
(define (call-with-input-file file proc)
  (call-with-port (open-input-file file) proc))

;;@doc
;; Calls the given *proc* with an output string port created with `open-output-string`.
;; If *proc* returns, then the content from the string port will be returned.
;;
;; (call-with-output-string proc) -> string?
;;
;; - proc : procedure?
(define (call-with-output-string proc)
  (define output-string (open-output-string))
  (proc output-string)
  (get-output-string output-string))

;;@doc
;; Calls the given *proc* with an input string port created by opening the given *string* with `open-input-string`.
;; If *proc* returns, then the return value of *proc* returned.
;;
;; (call-with-input-string proc) -> any/c
;;
;; - proc : procedure?
(define (call-with-input-string string proc)
  (call-with-port (open-input-string string) proc))

(define (with-output-to-string proc)
  (call-with-output-string (lambda (port)
                             (parameterize ([current-output-port port])
                               (proc)))))

(define (with-input-from-string string proc)
  (call-with-input-string string
                          (lambda (port)
                            (parameterize ([current-input-port port])
                              (proc)))))
