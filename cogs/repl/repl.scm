(require-builtin steel/tcp)
(require "steel/sync")

(provide repl-serve)

;; Thread id -> TCP reader. Remove when finished.
(define #%repl-manager (hash))

(define (#%mark-connected tcp-stream)
  (set! #%repl-manager (hash-insert #%repl-manager (current-thread-id) tcp-stream)))

(define (#%set-thread-closed!)
  (define connection (hash-get #%repl-manager (current-thread-id)))
  ;; Close it all down
  (tcp-shutdown! connection)
  (set! #%repl-manager (hash-remove #%repl-manager (current-thread-id)))

  ; (display "...Shutting down thread" (stdout))
  ; (newline (stdout))
  )

(define (#%shutdown?)
  (not (hash-contains? #%repl-manager (current-thread-id))))

(define (quit)
  ;; Attempt to set this thread closed no matter what.
  (with-handler (lambda (_) void) (#%set-thread-closed!)))

(define (repl-serve [port 8080] [thread-pool-size 2])
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

            ;; Close the thread.
            (when (#%shutdown?)
              (close-output-port writer-port)
              (close-input-port reader-port)
              (return! #t))

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

      (#%mark-connected input-stream)

      ;; Set up the repl to also grab std out
      (parameterize ([current-output-port writer-port])
        (repl-loop))

      (displayln "Closing connection.")))))

; (unless (get-test-mode)
;   (repl-serve))
