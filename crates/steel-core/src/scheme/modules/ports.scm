(require-builtin steel/base)
(require "#%private/steel/control")

(provide call-with-port
         call-with-output-file
         call-with-input-file
         with-output-to-file
         with-input-from-file
         call-with-output-string
         with-output-to-string)

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
;; Calls the given *proc* with an output port obtained opening *file*.
;; If *proc* returns, then the temporary port will be closed and the return value of *proc* returned.
;;
;; (call-with-output-file file proc) -> any/c
;;
;; - file : string?
;; - proc : procedure?
(define (call-with-input-file file proc)
  (call-with-port (open-input-file file) proc))

;;@doc
;; Similar to `call-with-output-file`, but installs the newly opened port as the `current-output-port` instead of passing it as an argument.
;; If *thunk* returns, then the temporary port will be closed and the return value of *thunk* returned.
;;
;; (with-output-to-file file proc) > any/c
;;
;; - file : string?
;; - thunk : procedure?
(define (with-output-to-file file thunk)
  (call-with-output-file file
                         (lambda (port)
                           (parameterize ([current-output-port port])
                             (thunk)))))

;;@doc
;; Similar to `call-with-input-file`, but installs the newly opened port as the `current-input-port` instead of passing it as an argument.
;; If *thunk* returns, then the temporary port will be closed and the return value of *thunk* returned.
;;
;; (with-input-from-file file proc) > any/c
;;
;; - file : string?
;; - thunk : procedure?
(define (with-input-from-file file thunk)
  (call-with-input-file file
                           (lambda (port)
                             (parameterize ([current-input-port port])
                               (thunk)))))

(define (call-with-output-string proc)
  (define output-string (open-output-string))
  (proc output-string)
  (get-output-string output-string))

(define (with-output-to-string proc)
  (call-with-output-string (lambda (p)
                             (parameterize ([current-output-port p])
                               (proc)))))
