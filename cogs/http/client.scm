(require-builtin steel/tcp)
(require "url.scm")
(require "headers.scm")
(require "message.scm")

(provide *http-version*
         *user-agent*
         *max-header-lines*
         *max-line-length*
         request?
         request-method
         request-url
         request-headers
         request-body
         request-version
         request-keep-alive?
         request-target
         request-header
         request-header*
         make-request
         response?
         response-version
         response-status
         response-reason
         response-headers
         response-body
         response-ok?
         response-text
         response-header
         response-header*
         response-keep-alive?
         make-response
         reason-phrase
         transport?
         transport-reader
         transport-writer
         make-transport
         tcp-connector
         current-http-connector
         connection?
         connection-transport
         connection-origin
         http-connect
         http-close!
         call-with-http-connection
         http-write-request!
         http-read-response
         http-exchange
         http-send
         http-get
         http-head
         http-post
         http-put
         http-patch
         http-delete
         http-options)

;;@doc
;; The value sent in the `User-Agent` header.
(define *user-agent* "steel-http/0.1.0")

;; A pair of ports and a thunk to close them. Nothing above this names a
;; socket, so `current-http-connector` can supply tls or a fixture.
(struct transport (reader writer close) #:transparent)

;;@doc
;; Builds a transport out of an input port, an output port, and the thunk that
;; closes them.
(define (make-transport reader writer #:close [close (lambda () void)])
  (transport reader writer close))

(define (connect-address url)
  (define host (url-host url))
  (string-append (if (string-contains? host ":")
                     (string-append "[" host "]")
                     host)
                 ":"
                 (int->string (url-port url))))

;;@doc
;; Opens a plain tcp connection to the host of `url`. This is the default
;; connector, and it handles `http` only.
;;
;; With a `read-timeout` in milliseconds, a read that waits that long raises.
;; The timeout applies to a single read and not to the whole response.
(define (tcp-connector url #:read-timeout [read-timeout #f])
  (unless (equal? (url-scheme url) "http")
    (error "http: the tcp connector only speaks http, not"
           (url-scheme url)
           "- require steel/http/tls.scm and parameterize current-http-connector for https"))
  (define stream (tcp-connect (connect-address url)))
  (tcp-stream-set-read-timeout! stream read-timeout)
  (transport (tcp-stream-buffered-reader stream)
             (tcp-stream-writer stream)
             ;; Shutting down a connection the far end already closed raises.
             (lambda () (with-handler (lambda (err) void) (tcp-shutdown! stream)))))

;;@doc
;; The procedure the client uses to reach a host. It takes a url and returns a
;; `transport?`. Parameterize it for tls, for a fixture, or to give the default
;; connector a read timeout:
;;
;; ```scheme
;; (parameterize ([current-http-connector
;;                 (lambda (url) (tcp-connector url #:read-timeout 5000))])
;;   (http-get "http://127.0.0.1:8080/"))
;; ```
(define current-http-connector (make-parameter tcp-connector))

(struct connection (transport origin) #:transparent)

;;@doc
;; Opens a connection to the origin of `target`. The connection stays open
;; until `http-close!` and can carry more than one exchange.
(define (http-connect target)
  (define url (parse-url target))
  (unless (url-port url)
    (error "http: no port, and no default port for the scheme:" (url->string url)))
  (connection ((current-http-connector) url) (url-origin url)))

;;@doc
;; Closes a connection.
(define (http-close! connection)
  ((transport-close (connection-transport connection))))

;;@doc
;; Opens a connection to `target`, calls `proc` with it, and closes it
;; afterwards, whether or not `proc` raised.
(define (call-with-http-connection target proc)
  (define connection (http-connect target))
  (dynamic-wind (lambda () void) (lambda () (proc connection)) (lambda () (http-close! connection))))

;; These methods send `Content-Length: 0` for an empty body; an omitted length
;; can read as a truncated request.
(define (expects-body? method)
  (member method (list "POST" "PUT" "PATCH")))

(define (wire-headers request keep-alive?)
  (define url (request-url request))
  (define body (request-body request))
  (define headers
    (~> (request-headers request)
        (header-default "Host" (url-authority url))
        (header-default "User-Agent" *user-agent*)
        (header-default "Accept" "*/*")
        ;; Nothing here decompresses a response, so ask for an uncompressed
        ;; one.
        (header-default "Accept-Encoding" "identity")
        (header-default "Connection" (if keep-alive? "keep-alive" "close"))))
  (when (header-contains? headers "Transfer-Encoding")
    (error
     "http: this client sends a body with a Content-Length, not chunked - take Transfer-Encoding off the request"))
  (cond
    [body (header-default headers "Content-Length" (int->string (bytes-length body)))]
    [(expects-body? (request-method request)) (header-default headers "Content-Length" "0")]
    [else headers]))

;;@doc
;; Writes `request` over `connection`. The head and the body go out in one
;; write.
(define (http-write-request! connection request #:keep-alive [keep-alive #f])
  (define head
    (string-append (request-method request)
                   " "
                   (url-target (request-url request))
                   " "
                   *http-version*
                   "\r\n"
                   (headers->string (wire-headers request keep-alive))
                   "\r\n"))
  (define body (request-body request))
  (define writer (transport-writer (connection-transport connection)))
  (if body
      (write-bytes (bytes-append (string->bytes head) body) writer)
      (display head writer))
  (flush-output-port writer))

(define (parse-status-line line)
  (define after-version (split-once line " "))
  (unless (list? after-version)
    (error "http: a response did not start with a status line:" line))
  (define after-code (split-once (list-ref after-version 1) " "))
  (define code
    (string->number (trim (if (list? after-code)
                              (car after-code)
                              (list-ref after-version 1)))))
  (unless (known-version? (car after-version))
    (error "http: a response spoke" (car after-version) "- this client speaks HTTP/1.1"))
  (unless (and (int? code) (>= code 100) (< code 1000))
    (error "http: a response had no status code:" line))
  (list (car after-version)
        code
        (if (list? after-code)
            (list-ref after-code 1)
            "")))

;; A 1xx other than a 101 is interim; the real response follows. Counted, since
;; an endless stream of them would block the read.
(define *max-interim-responses* 8)

(define (read-status-line-past-interim reader [interim 0])
  (define line (read-wire-line reader))
  (when (eof-object? line)
    (error "http: the connection closed before the response"))
  (define status-line (parse-status-line line))
  (define status (list-ref status-line 1))
  (cond
    [(and (>= status 100) (< status 200) (not (equal? status 101)))
     (when (>= interim *max-interim-responses*)
       (error "http: more than" *max-interim-responses* "interim responses, giving up"))
     (read-wire-headers reader)
     (read-status-line-past-interim reader (+ interim 1))]
    [else status-line]))

(define (bodyless-status? status method)
  (or (equal? method "HEAD")
      (equal? status 204)
      (equal? status 304)
      (and (>= status 100) (< status 200))))

(define (body-mode headers status method)
  (define encoding (header-ref headers "Transfer-Encoding"))
  (define declared (content-length headers))
  (cond
    [(bodyless-status? status method) 'none]
    [(and encoding (token-member? encoding "chunked"))
     (when declared
       (error "http: a response carried both Transfer-Encoding: chunked and Content-Length"))
     'chunked]
    [encoding (error "http: unsupported Transfer-Encoding:" encoding)]
    [declared declared]
    [else 'until-close]))

;;@doc
;; Reads one response off `reader`. `method` is needed because the response to
;; a `HEAD` carries the headers of a body it does not send.
;;
;; The body is always a bytevector, empty for a response that cannot carry one.
;; Trailers that follow a chunked body are added to the headers.
(define (http-read-response reader #:method [method "GET"])
  (define status-line (read-status-line-past-interim reader))
  (define status (list-ref status-line 1))
  (define headers (read-wire-headers reader))
  (define mode (body-mode headers status (method->string method)))
  (define body-and-trailers
    (cond
      [(equal? mode 'none) (cons (bytes) '())]
      [(equal? mode 'chunked) (read-chunked reader)]
      [(equal? mode 'until-close) (cons (read-until-close reader) '())]
      [else (cons (read-exactly reader mode) '())]))
  (response (car status-line)
            status
            (list-ref status-line 2)
            (append headers (cdr body-and-trailers))
            (car body-and-trailers)))

;;@doc
;; Writes `request` over `connection` and reads the response back.
;;
;; Pass `#:keep-alive #true` to ask the server to keep the connection open for
;; another exchange, and check `response-keep-alive?` before using it again.
(define (http-exchange connection request #:keep-alive [keep-alive #f])
  (http-write-request! connection request #:keep-alive keep-alive)
  (http-read-response (transport-reader (connection-transport connection))
                      #:method (request-method request)))

(define (redirect-status? status)
  (member status (list 301 302 303 307 308)))

;; 301, 302 and 303 become a GET with no body; 307 and 308 repeat the same
;; request.
(define (redirect-request sent received)
  (define location (response-header received "Location"))
  (cond
    [(not location) #f]
    [else
     (let* ([from (request-url sent)]
            [to (url-join from (trim location))]
            [same-origin? (equal? (url-origin from) (url-origin to))]
            [carried (if same-origin?
                         (request-headers sent)
                         ;; Credentials and `Host` belong to the origin they
                         ;; were written for, so they are not carried across a
                         ;; redirect.
                         (~> (request-headers sent)
                             (header-remove "Host")
                             (header-remove "Authorization")
                             (header-remove "Cookie")))])
       (if (and (member (response-status received) (list 301 302 303))
                (not (equal? (request-method sent) "HEAD")))
           (request "GET"
                    to
                    (~> carried
                        (header-remove "Content-Type")
                        (header-remove "Content-Length")
                        (header-remove "Transfer-Encoding"))
                    #f
                    (request-version sent))
           (request (request-method sent) to carried (request-body sent) (request-version sent))))]))

;;@doc
;; Opens a connection, sends one request, reads the response, and closes the
;; connection.
;;
;; Redirects are followed up to `follow` times; `#:follow 0` returns the 3xx.
;; The other keywords are those of `make-request`.
(define (http-send method
                   target
                   #:headers [headers '()]
                   #:body [body #f]
                   #:form [form #f]
                   #:query [query #f]
                   #:auth [auth #f]
                   #:follow [follow 5])
  (when (< follow 0)
    (error "http: a redirect count cannot be negative:" follow))
  (let loop ([request (make-request method
                                    target
                                    #:headers headers
                                    #:body body
                                    #:form form
                                    #:query query
                                    #:auth auth)]
             [left follow])
    (define response
      (call-with-http-connection (request-url request)
                                 (lambda (connection) (http-exchange connection request))))
    (cond
      [(not (redirect-status? (response-status response))) response]
      [(equal? left 0)
       (when (> follow 0)
         (error "http: more than"
                follow
                "redirects, giving up at"
                (url->string (request-url request))))
       response]
      [else
       (let ([next (redirect-request request response)])
         (if next
             (loop next (- left 1))
             response))])))

;;@doc
;; Sends a `GET`. Takes the keywords of `http-send`.
(define (http-get target . options)
  (apply http-send "GET" target options))

;;@doc
;; Sends a `HEAD`, which returns headers and an empty body. Takes the keywords
;; of `http-send`.
(define (http-head target . options)
  (apply http-send "HEAD" target options))

;;@doc
;; Sends a `POST`. Takes the keywords of `http-send`, including `#:form`.
(define (http-post target . options)
  (apply http-send "POST" target options))

;;@doc
;; Sends a `PUT`. Takes the keywords of `http-send`.
(define (http-put target . options)
  (apply http-send "PUT" target options))

;;@doc
;; Sends a `PATCH`. Takes the keywords of `http-send`.
(define (http-patch target . options)
  (apply http-send "PATCH" target options))

;;@doc
;; Sends a `DELETE`. Takes the keywords of `http-send`.
(define (http-delete target . options)
  (apply http-send "DELETE" target options))

;;@doc
;; Sends an `OPTIONS`. Takes the keywords of `http-send`.
(define (http-options target . options)
  (apply http-send "OPTIONS" target options))
