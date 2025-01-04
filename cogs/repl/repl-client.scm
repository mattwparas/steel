(require-builtin steel/tcp)
(require-builtin #%private/steel/readline)

(require "steel/sync")

(define channels (channels/new))
(define sender (channels-sender channels))
(define receiver (channels-receiver channels))

;; After every input if we should shutdown.
(define shutdown (channels/new))
(define shutdown-sender (channels-sender shutdown))
(define shutdown-receiver (channels-receiver shutdown))

(define MAGIC-START (string->bytes "%start-repl#%"))
(define MAGIC-START-LENGTH (bytes-length MAGIC-START))

(define (read-size buffer port)
  (define next (read-byte port))
  (if (equal? next #x0A)
      (begin
        (string->int (bytes->string/utf8 buffer)))
      (begin
        (bytes-push! buffer next)
        (read-size buffer port))))

;; Handle user input, forward, dump out things happening, continue.
(define (repl-loop [host "0.0.0.0"] [port 8080])
  (define stream (tcp-connect (string-append host ":" (int->string port))))
  (define reader (tcp-stream-reader stream))
  (define writer (tcp-stream-writer stream))

  ;; Print out the startup message for the repl, and then
  ;; we'll enter the event loop waiting for input.
  (#%repl-display-startup)
  (define rl (#%create-repl))

  (define buffer (bytevector))

  (define (loop)
    (define next (read-char reader))
    (cond
      [(equal? next #\#)
       (let ([maybe-magic (read-bytes MAGIC-START-LENGTH reader)])
         (if (equal? maybe-magic MAGIC-START)
             (let ([size (read-size buffer reader)])

               (bytes-clear! buffer)

               ;; Read the next value
               (define next-value (read-bytes size reader))
               (define value-as-string (bytes->string/utf8 next-value))

               (unless (equal? "#<void>" value-as-string)
                 (display "=> ")
                 (display value-as-string)
                 (newline))

               (channel/send sender #t))
             ;; Next should be the length, until the next newline
             (begin
               (write-char next (current-output-port))
               (display (bytes->string/utf8 maybe-magic)))))]

      [(eof-object? next)
       (displayln "Connection closed.")
       (return! void)]

      [else (write-char next (current-output-port))])

    ;; Remote repl... add bindings to readline?
    ;; That could help?
    ; (write-char (read-char reader) (current-output-port))

    (loop))

  (define (driver)
    (with-handler (lambda (err)
                    (displayln err)
                    (channel/send shutdown-sender #t))
                  (loop)))

  (spawn-native-thread driver)

  ;; Read input, send on the stream
  (define (input-loop)
    (define input (#%read-line rl))
    (#%repl-add-history-entry rl input)

    ;; Show the error, go again
    (with-handler (lambda (err) (displayln err))
                  (write (read (open-input-string input)) writer)
                  (newline writer)
                  ;; Selection. Wait on either an acknowledgement, or a shutdown?
                  (define result (receivers-select receiver shutdown-receiver))
                  (case result
                    [(0) (input-loop)]
                    [(1) (displayln "Shutting down")]
                    [else void])))
  (input-loop))

(define (main)
  ;; Fetch the args, check if there is a host provided.
  ;; If not, default to the loop back host.
  (define args (command-line))

  (match (drop args 2)
    [(list) (repl-loop)]
    [(list "--port" port) (repl-loop "0.0.0.0" (string->int port))]
    [(list "--host" host) (repl-loop host)]
    [(list "--host" host "--port" port) (repl-loop host (string->int port))]))

(main)
