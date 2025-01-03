;; TODO: Incorporate rustls?
(require "libs/steel-rustls/rustls.scm")
(require-builtin steel/http)

(provide request/get)

(define APPLICATION-JSON "application/json")
(define APPLICATION-FORM-URL-ENCODED "application/x-www-form-urlencoded")

(define (parse-url-into-parts url)
  (~> (trim-start-matches url "https://") (split-once "/")))

(define (read-bytes-exact-into-buf port buf count)
  (unless (= count 0)
    (define byte (read-byte port))
    (when (byte? byte)
      (bytes-push! buf byte)
      (read-bytes-exact-into-buf port buf (sub1 count)))))

(define (read-body-into-buffer->string content-length output-port buffer)
  (cond
    [(> content-length 0)
     (read-bytes-exact-into-buf output-port buffer content-length)
     (bytes->string/utf8 buffer)]
    [else #f]))

(define (read-body-into-buffer content-length output-port buffer)
  (cond
    [(> content-length 0)
     (read-bytes-exact-into-buf output-port buffer content-length)
     buffer]
    [else #f]))

(define (request/get url #:headers [headers '()] #:body [body void])
  (define url-parts (parse-url-into-parts url))
  (define host (car url-parts))
  (define remaining-url (string-append "/" (cadr url-parts)))

  (define tls-socket (tcp-connect (string-append host ":443")))

  ;; TODO: This is an issue - causes a panic
  ; (define client-connection (client-connection "www.rust-lang.org"))
  (define connection (client-connection host))

  (define stream (tls-stream connection tls-socket))

  (define port (tls-writer stream))
  (define reader (tls-reader stream))

  (write-bytes (string->bytes (string-append "GET " remaining-url " HTTP/1.1\r\n")) port)
  (write-bytes (string->bytes (string-append "Host: " host "\r\n")) port)

  ;; Go through the headers, add them here
  (for-each (lambda (header) (write-bytes (string->bytes header) port)) headers)

  (write-bytes (string->bytes "accept: application/json\r\n") port)
  (write-bytes (string->bytes "\r\n") port)
  (flush-output-port port)

  (define buffer (bytevector))
  (define (loop)
    (define byte (read-byte reader))
    (when (byte? byte)
      (begin
        (bytes-push! buffer byte)

        ;; If http-parse is not a bool
        (let ([http-request (http-parse-response buffer)])
          (if http-request
              (begin
                (define headers (http-response-headers http-request))
                (define content-length
                  (string->int (bytes->string/utf8 (or (hash-try-get headers "Content-Length")
                                                       #(0)))))
                (define content-type (bytes->string/utf8 (hash-try-get headers "Content-Type")))

                ;; Wipe out the buffer, since we've already parsed out the headers.
                (bytes-clear! buffer)

                ;; Check the content type:
                (define body (read-body-into-buffer content-length reader buffer))

                (cond
                  [(equal? content-type "application/json")
                   (string->jsexpr (bytes->string/utf8 body))]
                  [else body]))

              (loop))))))
  (loop))
