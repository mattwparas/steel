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

;; The dylib's names are looked up one at a time rather than by building an
;; expression around the arguments, since a bytevector does not survive being
;; written into one.
(define (rustls name)
  (load-rustls!)
  (eval name))

(define (tls-ports host address read-timeout)
  (define socket ((rustls 'tcp-connect) address))
  ;; The deadline is set on the socket before the tls stream is built on it, so
  ;; it covers the handshake as well as later reads.
  ((rustls 'tcp-set-read-timeout!) socket read-timeout)
  (define connection ((rustls 'client-connection) host))
  (define stream ((rustls 'tls-stream) connection socket))
  (list ((rustls 'tls-reader) stream) ((rustls 'tls-writer) stream)))

;;@doc
;; Opens a tls connection to the host of *url*. The handshake happens on the
;; first read or write, so a certificate that does not verify is reported from
;; there rather than from here.
;;
;; With a *read-timeout* in milliseconds, a read that waits that long raises.
;; The timeout covers the handshake as well.
;;
;; Requires the `steel-rustls` dylib.
(define (tls-connector url #:read-timeout [read-timeout #f])
  (define host (url-host url))
  (define ports (tls-ports host (string-append host ":" (int->string (url-port url))) read-timeout))
  (make-transport (car ports)
                  (list-ref ports 1)
                  #:close (lambda ()
                            (with-handler (lambda (err) void) (close-port (car ports)))
                            (with-handler (lambda (err) void) (close-port (list-ref ports 1))))))

;;@doc
;; A connector for both schemes: `https` over tls, `http` over plain tcp.
;; Parameterize `current-http-connector` with it to follow a redirect from one
;; to the other.
;;
;; ```scheme
;; (parameterize ([current-http-connector https-connector])
;;   (response-text (http-get "https://example.com")))
;; ```
;;
;; A *read-timeout* in milliseconds is passed to whichever of the two is used.
(define (https-connector url #:read-timeout [read-timeout #f])
  (if (equal? (url-scheme url) "https")
      (tls-connector url #:read-timeout read-timeout)
      (tcp-connector url #:read-timeout read-timeout)))

;;@doc
;; Decompresses a gzipped body. The client sends `Accept-Encoding: identity`
;; unless told otherwise, so this is only needed when it has been.
;;
;; Requires the `steel-rustls` dylib.
(define (gzip-decode body)
  (read-port-to-bytes ((rustls 'gz-decode) body)))
