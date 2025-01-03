(require-builtin steel/tcp)

; (define listener (tcp-listen "0.0.0.0:8080"))

; (define input-stream (tcp-accept listener))

; (define reader-port (tcp-stream-buffered-reader input-stream))
; (define writer-port (tcp-stream-writer input-stream))

; (displayln (eval (read reader-port)))

;; Loop until its disconnected?
;; Accept user input... until not

(define (repl)
  (define listener (tcp-listen "0.0.0.0:8080"))
  ;; Accept the stream
  (define input-stream (tcp-accept listener))

  (displayln "Accepted connection")

  ; (define reader-port (tcp-stream-buffered-reader input-stream))
  (define reader-port (tcp-stream-buffered-reader input-stream))
  (define writer-port (tcp-stream-writer input-stream))

  ;; Read until... newline?
  (define buffer (bytevector))

  ;; Continue to accept connections until this one disconnects
  (define (repl-loop)
    ;; Assume, that for now, we are comfortable with the fact
    ;; that stdout / etc will get printed from the
    (let ([expr (read reader-port)])

      (unless (eof-object? expr)
        ;; Eval... could error. The error should
        ;; be reported to the client repl, somehow.
        ;;
        ;; It is probably possible to just serialize the eventual
        ;; error message directly, and send that over. That way
        ;; the stack trace is maintained?
        (define result (with-handler (lambda (err) (to-string err)) (eval expr)))

        ;; TODO: Merge this into one display call.
        ;; It all has to come through in one fell swoop.
        (define output-string (open-output-string))
        (display result output-string)
        (define formatted (get-output-string output-string))

        (display "#%start-repl#%" writer-port)
        ;; Don't send back a void, just have it be the length of 0
        (display (if (void? result) 0 (string-length formatted)) writer-port)
        (newline writer-port)
        (display formatted writer-port)

        (repl-loop))))

  ;; Set up the repl to also grab std out
  (parameterize ([current-output-port writer-port])
    (repl-loop)))

(repl)
