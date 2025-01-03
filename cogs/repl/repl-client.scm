(require-builtin steel/tcp)
(require-builtin #%private/steel/readline)

(require "steel/sync")

; (define display-mode #f)

(define channels (channels/new))

(define sender (channels-sender channels))
(define receiver (channels-receiver channels))

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
(define (repl-loop)

  (define stream (tcp-connect "0.0.0.0:8080"))
  (define reader (tcp-stream-reader stream))
  (define writer (tcp-stream-writer stream))

  (define rl (#%create-repl))

  (define buffer (bytevector))

  (define (loop)

    (define next (read-char reader))

    (if (equal? next #\#)
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
                (display (bytes->string/utf8 maybe-magic)))))

        (write-char next (current-output-port)))

    ;; Remote repl... add bindings to readline?
    ;; That could help?
    ; (write-char (read-char reader) (current-output-port))

    (loop))

  (define (driver)
    (with-handler (lambda (err) (displayln err)) (loop)))

  (spawn-native-thread driver)

  ;; Read input, send on the stream
  (define (input-loop)
    ;; If previous expression is still running:

    ;; Do the thing
    (define input (#%read-line rl))

    ;; Show the error, go again
    (with-handler (lambda (err)
                    (displayln err)
                    (input-loop))
                  (write (read (open-input-string input)) writer)
                  (newline writer)
                  (channel/recv receiver)
                  ;; Wait for acknowledgement?
                  (input-loop)))

  (input-loop))

(repl-loop)
