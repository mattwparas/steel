(require-builtin steel/tcp)
(require "steel/sync")

(provide repl)

(define (repl [port 8080] [thread-pool-size 2])
  (define listener (tcp-listen (string-append "0.0.0.0:" (int->string port))))
  (define tp (make-thread-pool thread-pool-size))

  (while
   #t
   ;; Accept the stream
   (define input-stream (tcp-accept listener))
   (submit-task
    tp
    (lambda ()
      ;; TODO: Set up dedicated logging stream, flushes on its own
      ;; background thread?
      (define reader-port (tcp-stream-buffered-reader input-stream))
      (define writer-port (tcp-stream-writer input-stream))
      ;; Continue to accept connections until this one disconnects
      (define (repl-loop)
        ;; Assume, that for now, we are comfortable with the fact
        ;; that stdout / etc will get printed from the
        (let ([expr (read reader-port)])

          (unless (eof-object? expr)
            ;; It is probably possible to just serialize the eventual
            ;; error message directly, and send that over. That way
            ;; the stack trace is maintained?
            (define result (with-handler (lambda (err) (to-string err)) (eval expr)))

            ;; TODO: Merge this into one display call.
            ;; It all has to come through in one fell swoop, such that
            ;; return values are atomic on the output stream.
            (define output-string (open-output-string))
            (display result output-string)
            (define formatted (get-output-string output-string))

            ;; Don't send back a void, just have it be the length of 0
            (display
             (string-append "#%start-repl#%" (int->string (string-length formatted)) "\n" formatted))

            (repl-loop))))
      ;; Set up the repl to also grab std out
      (parameterize ([current-output-port writer-port])
        (repl-loop))))))

;; Serve with a thread pool
(repl)
