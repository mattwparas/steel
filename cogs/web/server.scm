(require-builtin steel/tcp)
(require-builtin steel/http)
(require-builtin steel/time)

(require "steel/sync")

(require "steel/logging/log.scm")

;;; Web server

(define (send-as-json port hashmap)
  (define response-json (string->bytes (value->jsexpr-string hashmap)))
  (define size (bytes-length response-json))
  (write-bytes (string->bytes "HTTP/1.1 200 OK\r\n") port)
  (write-bytes (string->bytes (to-string "Content-length:" size "\r\n")) port)
  (write-bytes (string->bytes "Content-Type: application/json\r\n") port)
  (write-bytes (string->bytes "\r\n") port)
  (write-bytes response-json port)
  (write-bytes (string->bytes "\r\n\r\n") port)
  (flush-output-port port))

(define (read-bytes-exact-into-buf port buf count)
  (unless (= count 0)
    (define byte (read-byte port))
    (when (byte? byte)
      (bytes-push! buf byte)
      (read-bytes-exact-into-buf port buf (sub1 count)))))

;; Vec of bytevectors, return cleared bytevector to the pool
(define (buffer-pool)
  (error "TODO"))

(define (parse-keep-alive keep-alive-string)
  (apply hash
         (map trim
              (flatten (map (lambda (p) (split-once p "=")) (split-once keep-alive-string ","))))))

(define EMPTY-KEEP-ALIVE (hash))

;; Router -> function that takes a path and returns a function

(define (read-body-into-buffer content-length output-port buffer)
  (cond
    [(> content-length 0)
     (read-bytes-exact-into-buf output-port buffer content-length)
     (bytes->string/utf8 buffer)]
    [else #f]))

;; Router should be a function that takes a
;; method and a path, and returns a function
;; that takes the path, the body, and returns
;; a json or something

(define (serve addr thread-pool-size)
  ; (->/c string? int? void? (->/c string? string? (->/c string? string? hash?)))
  (define listener (tcp-listen addr))
  (define tp (make-thread-pool thread-pool-size))

  (log/info! "Listening on 8080")

  (while
   #t
   (define input-stream-and-addr (tcp-accept-with-addr listener))
   (define input-stream (car input-stream-and-addr))
   (define addr (cdr input-stream-and-addr))
   (submit-task
    tp
    (lambda ()
      (define now (instant/now))
      (define output-port (tcp-stream-reader input-stream))
      (define writer-port (tcp-stream-writer input-stream))
      ;; Lets just start with 100 bytes first, and then see what happens?
      (define buffer (bytevector))
      (define (loop)
        (define byte (read-byte output-port))
        (when (byte? byte)
          (begin
            (bytes-push! buffer byte)

            ;; If http-parse is not a bool
            (let ([http-request (http-parse-request buffer)])
              (if http-request
                  (begin
                    (define headers (http-request-headers http-request))
                    (define content-length
                      (string->int (bytes->string/utf8 (or (hash-try-get headers "Content-Length")
                                                           #(0)))))
                    (define content-type (bytes->string/utf8 (hash-try-get headers "Content-Type")))

                    ;; Get the connection kind. If its
                    (define keep-alive-connection?
                      (if (hash-try-get headers "Connection")
                          (equal? (bytes->string/utf8 (hash-get headers "Connection")) "keep-alive")
                          #f))

                    (define keep-alive-params
                      (if (hash-try-get headers "Keep-Alive")
                          (~> (hash-get headers "Keep-Alive") bytes->string/utf8 parse-keep-alive)
                          EMPTY-KEEP-ALIVE))

                    (log/info! addr
                               (http-request-method http-request)
                               (http-request-path http-request)
                               (http-request-version http-request)
                               content-type
                               keep-alive-params)

                    ;; Wipe out the buffer, since we've already parsed out the headers.
                    (bytes-clear! buffer)

                    (define body (read-body-into-buffer content-length output-port buffer))

                    (when (and body (equal? content-type "application/json"))
                      (send-as-json writer-port (string->jsexpr (bytes->string/utf8 buffer))))

                    (flush-output-port writer-port)

                    (displayln "finished sending response: "
                               (duration->string (instant/elapsed now))))

                  (loop))))))
      (loop)))))

;; Actually start the server
(serve "0.0.0.0:8080" 10)
