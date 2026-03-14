(require-builtin steel/base)
(require "#%private/steel/control")

(provide read-port-to-string
         port->string
         read-port-to-bytes
         port->bytes
         read-line
         read-byte
         read-u8
         peek-byte
         peek-u8
         read-bytes
         read-bytevector
         read-bytes-into-buf
         write-byte
         write-u8
         write-bytes
         write-bytevector
         read-char
         peek-char
         call-with-port
         call-with-output-file
         call-with-input-file
         with-output-to-file
         with-input-from-file
         call-with-output-string
         call-with-input-string
         with-output-to-string
         with-input-from-string)

;;@doc
;; Reads the entire content of an input port into a string.
;;
;; (read-port-to-string [port]) -> string?
;;
;; * [port] : input-port? = (current-input-port)
(define read-port-to-string
  (case-lambda
    [() (#%read-port-to-string (current-input-port))]
    [(port) (#%read-port-to-string port)]))

;;@doc
;; Alias of `read-port-to-string`.
(define port->string read-port-to-string)

;;@doc
;; Reads the entire content of an input port into a byte vector.
;;
;; (read-port-to-bytes [port]) -> string?
;;
;; * [port] : input-port? = (current-input-port)
(define read-port-to-bytes
  (case-lambda
    [() (#%read-port-to-bytes (current-input-port))]
    [(port) (#%read-port-to-bytes port)]))

;;@doc
;; Alias of `read-port-to-bytes`.
(define port->bytes read-port-to-bytes)

;;@doc
;; Reads a line from an input port.
;;
;; (read-line [port]) -> string?
;;
;; * port : input-port? = (current-input-port)
(define read-line
  (case-lambda
    [() (#%read-line (current-input-port))]
    [(port) (#%read-line port)]))

;;@doc
;; Reads a single byte from an input port.
;;
;; (read-byte [port]) -> byte?
;;
;; * port : input-port? = (current-input-port)
(define read-byte
  (case-lambda
    [() (#%read-byte (current-input-port))]
    [(port) (#%read-byte port)]))

;;@doc
;; Alias of `read-byte`.
(define read-u8 read-byte)

;;@doc
;; Peeks the next byte from an input port.
;;
;; (peek-byte [port]) -> byte?
;;
;; * port : input-port? = (current-input-port)
(define peek-byte
  (case-lambda
    [() (#%peek-byte (current-input-port))]
    [(port) (#%peek-byte port)]))

;;@doc
;; Alias of `peek-byte`.
(define peek-u8 peek-byte)

;;@doc
;; Reads bytes from an input port.
;;
;; (read-bytes amt [port]) -> bytes?
;;
;; * amt : (and positive? int?)
;; * port : input-port? = (current-input-port)
(define read-bytes
  (case-lambda
    [(amt) (#%read-bytes amt (current-input-port))]
    [(amt port) (#%read-bytes amt port)]))

;;@doc
;; Alias of `read-bytes`.
(define read-bytevector read-bytes)

;;@doc
;; Reads bytes from an input port into a given buffer.
;;
;; (read-bytes-into-buf buf amt [port]) -> int?
;;
;; * buf : bytes?
;; * amt : (and positive? int?)
;; * port : input-port? = (current-input-port)
(define read-bytes-into-buf
  (case-lambda
    [(buf amt) (#%read-bytes-into-buf buf amt (current-input-port))]
    [(buf amt port) (#%read-bytes-into-buf buf amt port)]))

;;@doc
;; Writes a single byte to an output port.
;;
;; (write-byte b [port])
;;
;; * b : byte?
;; * port : output-port? = (current-output-port)
(define write-byte
  (case-lambda
    [(byte) (#%write-byte byte (current-output-port))]
    [(byte port) (#%write-byte byte port)]))

;;@doc
;; Alias of `write-byte`.
(define write-u8 write-byte)

;;@doc
;; Writes the contents of a bytevector into an output port.
;;
;; (write-bytes buf [port])
;;
;; * buf : bytes?
;; * port : output-port? = (current-output-port)
(define write-bytes
  (case-lambda
    [(bytes) (#%write-bytes bytes (current-output-port))]
    [(bytes port) (#%write-bytes bytes port)]))

;;@doc
;; Alias of `write-bytes`.
(define write-bytevector write-bytes)

;;@doc
;; Reads the next character from an input port.
;;
;; (read-char [port]) -> char?
;;
;; * port : input-port? = (current-input-port)
(define read-char
  (case-lambda
    [() (#%read-char (current-input-port))]
    [(port) (#%read-char port)]))

;;@doc
;; Peeks the next character from an input port.
;;
;; (peek-char [port]) -> char?
;;
;; * port : input-port? = (current-input-port)
(define peek-char
  (case-lambda
    [() (#%peek-char (current-input-port))]
    [(port) (#%peek-char port)]))

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
(define (call-with-output-file file proc #:exists [exists 'error])
  (call-with-port (open-output-file file #:exists exists) proc))

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
;; Similar to `call-with-output-file`, but installs the newly opened port as the `current-output-port` instead of passing it as an argument.
;; If *thunk* returns, then the temporary port will be closed and the return value of *thunk* returned.
;;
;; (with-output-to-file file proc) > any/c
;;
;; - file : string?
;; - thunk : procedure?
(define (with-output-to-file file thunk #:exists [exists 'error])
  (call-with-output-file file
                         (lambda (port)
                           (parameterize ([current-output-port port])
                             (thunk)))
                         #:exists exists))

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

;;@doc
;; Similar to `call-with-output-string`, but installs the newly opened port as the `current-input-port` instead of passing it as an argument.
;; If *thunk* returns, then the content from the string port will be returned.
;;
;; (with-output-to-string thunk) -> string?
;;
;; - thunk : procedure?
(define (with-output-to-string thunk)
  (call-with-output-string (lambda (port)
                             (parameterize ([current-output-port port])
                               (thunk)))))

;;@doc
;; Similar to `call-with-output-string`, but installs the newly opened port as the `current-input-port` instead of passing it as an argument.
;; If *thunk* returns, then the return value of *thunk* returned.
;;
;; (with-input-from-string string thunk) -> any/c
;;
;; - string : string?
;; - thunk : procedure?
(define (with-input-from-string string thunk)
  (call-with-input-string string
                          (lambda (port)
                            (parameterize ([current-input-port port])
                              (thunk)))))
