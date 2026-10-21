(require "client.scm")
(require "url.scm")

(provide tls-connector
         https-connector
         gzip-decode)

;; Tls lives in the `steel-rustls` dylib rather than in the interpreter, so it
;; is loaded on the first https request rather than when this module is
;; required, and requiring this file does not need the dylib installed.
;;
;; To install it:
;;
;;   cargo install --path crates/cargo-steel-lib
;;   cd libs/steel-rustls && cargo steel-lib

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

;; The dylib's own names are pulled out one at a time, rather than by building
;; an expression around the arguments, because a bytevector does not survive
;; being written back into one.
(define (rustls name)
  (load-rustls!)
  (eval name))

(define (tls-ports host address read-timeout)
  (define socket ((rustls 'tcp-connect) address))
  ;; The deadline goes on the socket before the tls stream is built on top of
  ;; it, so that it covers the handshake as well as the reads after it.
  ((rustls 'tcp-set-read-timeout!) socket read-timeout)
  (define connection ((rustls 'client-connection) host))
  (define stream ((rustls 'tls-stream) connection socket))
  (list ((rustls 'tls-reader) stream) ((rustls 'tls-writer) stream)))

;;@doc
;; Opens a tls connection to the host of *url*. The handshake happens on the
;; first read or write, so a certificate that does not check out is reported
;; from there rather than from here.
;;
;; With a *read-timeout* in milliseconds, a read that waits that long raises.
;; The deadline covers the handshake too, so a server that accepts the
;; connection and never answers the ClientHello is given up on.
;;
;; Needs the `steel-rustls` dylib; see the comment at the top of this file.
(define (tls-connector url #:read-timeout [read-timeout #f])
  (define host (url-host url))
  (define ports (tls-ports host (string-append host ":" (int->string (url-port url))) read-timeout))
  (make-transport (car ports)
                  (list-ref ports 1)
                  #:close (lambda ()
                            (with-handler (lambda (err) void) (close-port (car ports)))
                            (with-handler (lambda (err) void) (close-port (list-ref ports 1))))))

;;@doc
;; A connector that speaks both schemes: `https` over tls, `http` over plain
;; tcp. Parameterize `current-http-connector` with it to let the client follow
;; a redirect from one to the other.
;;
;; ```scheme
;; (parameterize ([current-http-connector https-connector])
;;   (response-text (http-get "https://example.com")))
;; ```
;;
;; A *read-timeout* in milliseconds is handed to whichever of the two it ends
;; up using.
(define (https-connector url #:read-timeout [read-timeout #f])
  (if (equal? (url-scheme url) "https")
      (tls-connector url #:read-timeout read-timeout)
      (tcp-connector url #:read-timeout read-timeout)))

;;@doc
;; Decompresses a gzipped body. The client asks for `Accept-Encoding: identity`
;; unless told otherwise, so this is only needed when it has been.
;;
;; Needs the `steel-rustls` dylib; see the comment at the top of this file.
(define (gzip-decode body)
  (read-port-to-bytes ((rustls 'gz-decode) body)))
