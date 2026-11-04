(require "client.scm")
(require "url.scm")

(provide tls-connector
         https-connector
         gzip-decode)

(define *loaded* (box #f))

(define (load-rustls!)
  (unless (unbox *loaded*)
    (eval '(#%require-dylib "libsteel_rustls"
                            (only-in client-connection
                                     tcp-connect
                                     tcp-set-read-timeout!
                                     tls-stream
                                     tls-reader
                                     tls-writer
                                     gz-decode)))
    (set-box! *loaded* #t)))

(define (rustls name)
  (load-rustls!)
  (eval name))

(define (tls-ports host address read-timeout)
  (define socket ((rustls 'tcp-connect) address))
  ((rustls 'tcp-set-read-timeout!) socket read-timeout)
  (define connection ((rustls 'client-connection) host))
  (define stream ((rustls 'tls-stream) connection socket))
  (list ((rustls 'tls-reader) stream) ((rustls 'tls-writer) stream)))

;;@doc
;; Opens a tls connection to the host of a url. Needs the `steel-rustls`
;; dylib.
(define (tls-connector url #:read-timeout [read-timeout #f])
  (define host (url-host url))
  (define ports (tls-ports host (string-append host ":" (int->string (url-port url))) read-timeout))
  (make-transport (car ports)
                  (list-ref ports 1)
                  #:close (lambda ()
                            (with-handler (lambda (err) void) (close-port (car ports)))
                            (with-handler (lambda (err) void) (close-port (list-ref ports 1))))))

;;@doc
;; Builds a connector for both `https` and `http`
;;
;; ```scheme
;; (parameterize ([current-http-connector https-connector])
;;   (response-text (http-get "https://example.com")))
;; ```
(define (https-connector url #:read-timeout [read-timeout #f])
  (if (equal? (url-scheme url) "https")
      (tls-connector url #:read-timeout read-timeout)
      (tcp-connector url #:read-timeout read-timeout)))

;;@doc
;; Decompresses a gzipped body. Needs the `steel-rustls` dylib.
(define (gzip-decode body)
  (read-port-to-bytes ((rustls 'gz-decode) body)))
