(require-builtin steel/tcp)
(require "url.scm")
(require "headers.scm")
(require "message.scm")

(provide *server-name*
         *max-body-bytes*
         server?
         server-listener
         server-address
         http-listen
         http-read-request
         http-write-response!
         http-serve-connection
         http-serve-one
         http-serve)

;;@doc
;; The value sent in the `Server` header.
(define *server-name* "steel-http/0.1.0")

;;@doc
;; The largest request body this server reads. A longer one is answered with a
;; 413.
(define *max-body-bytes* 8388608)

(struct server (listener address read-timeout) #:transparent)

;;@doc
;; Binds a listener. A port of `0` takes a free port from the os, which
;; `server-address` then reports. A `read-timeout` in milliseconds is set on
;; every connection accepted.
(define (http-listen address #:read-timeout [read-timeout #f])
  (define listener (tcp-listen address))
  (server listener (tcp-listener-local-addr listener) read-timeout))

(define (request-line-parts line)
  ;; A request line is three fields separated by single spaces; anything else
  ;; is an error.
  (define parts (split-many line " "))
  (if (equal? (length parts) 3) parts #f))

;; The target may be origin form (`/a?b`), absolute form (`http://host/a`, as
;; sent to a proxy), or `*` for an `OPTIONS` about the server itself.
(define (target->url target host)
  (cond
    [(equal? target "*") (make-url (or host "") #:path "*")]
    [(string-contains? target "://") (parse-url target)]
    [(not (starts-with? target "/")) #f]
    [host (parse-url (string-append "http://" host target))]
    [else
     ;; An HTTP/1.0 request may arrive with no `Host`, leaving the path with no
     ;; authority.
     (let ([split (split-once target "?")])
       (if (list? split)
           (make-url "" #:path (car split) #:query (list-ref split 1))
           (make-url "" #:path target)))]))

(define (request-body-mode headers)
  (define encoding (header-ref headers "Transfer-Encoding"))
  (define declared (content-length headers))
  (cond
    [(and encoding (token-member? encoding "chunked"))
     (if declared
         (problem-report 400 "a request carried both Transfer-Encoding: chunked and Content-Length")
         'chunked)]
    [encoding (problem-report 501 (string-append "unsupported Transfer-Encoding: " encoding))]
    [(and declared (> declared *max-body-bytes*))
     (problem-report 413
                     (string-append "a request declared a body of " (int->string declared) " bytes"))]
    ;; No framing means no body; reading until close would leave nothing to
    ;; answer on.
    [declared declared]
    [else 'none]))

;; A raise cannot carry a value through `with-handler`, and re-raising inside a
;; handler panics the interpreter, so this is returned.
(struct problem-report (status text) #:transparent)

;; Anything the shared reader raises (a bad header line, a bad chunk size, a
;; body cut short) becomes a 400.
(define (or-bad-request thunk)
  (with-handler (lambda (err) (problem-report 400 (trim (to-string (error-object-message err)))))
                (thunk)))

(define (expects-continue? headers)
  (define expectation (header-ref headers "Expect"))
  (and expectation (token-member? expectation "100-continue")))

(define (read-request-line reader)
  ;; An empty line before the request line is a stray crlf from the previous
  ;; request.
  (let loop ([skipped 0])
    (define line (read-wire-line reader))
    (cond
      [(eof-object? line) line]
      [(and (equal? line "") (< skipped 2)) (loop (+ skipped 1))]
      [else line])))

;; Returns the eof object when the connection closed between requests, and
;; otherwise a `request?` or a `problem-report?`.
(define (read-request reader writer)
  (define line (read-request-line reader))
  (cond
    [(eof-object? line) line]
    [else
     (let ([parts (request-line-parts line)])
       (cond
         [(not parts) (problem-report 400 "not a request line")]
         [(not (known-version? (list-ref parts 2)))
          (problem-report 505 (string-append "this server speaks HTTP/1.1, not " (list-ref parts 2)))]
         [else (read-request-headers reader writer parts)]))]))

(define (read-request-headers reader writer parts)
  (define version (list-ref parts 2))
  (define headers (or-bad-request (lambda () (read-wire-headers reader))))
  (cond
    [(problem-report? headers) headers]
    [else
     (let ([hosts (header-ref* headers "Host")])
       (cond
         ;; Two `Host` headers would leave the next hop to decide which one
         ;; counts.
         [(> (length hosts) 1) (problem-report 400 "a request carried more than one Host header")]
         [(and (equal? version "HTTP/1.1") (empty? hosts))
          (problem-report 400 "an HTTP/1.1 request without a Host header")]
         [else
          (let ([url (or-bad-request (lambda ()
                                       (target->url (list-ref parts 1)
                                                    (if (empty? hosts)
                                                        #f
                                                        (car hosts)))))])
            (cond
              [(or (problem-report? url) (not url))
               (problem-report 400 "not a usable request target")]
              [else (read-request-body reader writer parts headers url)]))]))]))

(define (read-request-body reader writer parts headers url)
  (define mode (or-bad-request (lambda () (request-body-mode headers))))
  (cond
    [(problem-report? mode) mode]
    [else
     ;; With `Expect: 100-continue` the interim response has to be written
     ;; before the body is read.
     (when (and writer (expects-continue? headers))
       (with-handler (lambda (err) void)
                     (begin
                       (display (string-append *http-version* " 100 Continue\r\n\r\n") writer)
                       (flush-output-port writer))))
     (let ([body-and-trailers (or-bad-request (lambda ()
                                                (cond
                                                  [(equal? mode 'none) (cons #f '())]
                                                  [(equal? mode 'chunked)
                                                   (read-chunked reader #:limit *max-body-bytes*)]
                                                  [else (cons (read-exactly reader mode) '())])))])
       (cond
         [(problem-report? body-and-trailers) body-and-trailers]
         [(equal? body-and-trailers 'too-large)
          (problem-report 413 "a chunked request body grew past what this server reads")]
         [else
          (request (string-upcase (car parts))
                   url
                   (append headers (cdr body-and-trailers))
                   (car body-and-trailers)
                   (list-ref parts 2))]))]))

;;@doc
;; Reads one request off `reader`. Returns the eof object when the connection
;; closed cleanly between requests; a request this server cannot read raises.
;;
;; The body is a bytevector, or `#false` for a request with no framing.
;; Trailers that follow a chunked body are added to the headers. Give
;; `continue` the writer of the same connection to answer an
;; `Expect: 100-continue`.
(define (http-read-request reader #:continue [continue #f])
  (define outcome (read-request reader continue))
  (cond
    [(eof-object? outcome) outcome]
    [(problem-report? outcome)
     (error "http:" (problem-report-text outcome) "- would answer" (problem-report-status outcome))]
    [else outcome]))

;; A 1xx, a 204 and a 304 carry no body, and no `Content-Length` either.
(define (bodyless-status? status)
  (or (equal? status 204) (equal? status 304) (and (>= status 100) (< status 200))))

(define (wire-headers received keep-alive?)
  (define body (response-body received))
  (define headers
    (~> (response-headers received)
        (header-default "Server" *server-name*)
        (header-default "Connection" (if keep-alive? "keep-alive" "close"))))
  (when (header-contains? headers "Transfer-Encoding")
    (error
     "http: this server sends a body with a Content-Length, not chunked - take Transfer-Encoding off the response"))
  (if (bodyless-status? (response-status received))
      (header-remove headers "Content-Length")
      ;; A HEAD response carries the body's length but not the body.
      (header-default headers "Content-Length" (int->string (bytes-length body)))))

;; The head and the body as bytes. An unframable response is rejected here,
;; before anything is written.
(define (response->wire received keep-alive method)
  (define status (response-status received))
  (define reason (response-reason received))
  (cons (string-append *http-version*
                       " "
                       (int->string status)
                       (if (equal? reason "")
                           ""
                           (string-append " " reason))
                       "\r\n"
                       (headers->string (wire-headers received keep-alive))
                       "\r\n")
        (if (or (bodyless-status? status) (equal? (method->string method) "HEAD"))
            (bytes)
            (response-body received))))

(define (write-wire! writer wire)
  (if (equal? (bytes-length (cdr wire)) 0)
      (display (car wire) writer)
      (write-bytes (bytes-append (string->bytes (car wire)) (cdr wire)) writer))
  (flush-output-port writer))

;;@doc
;; Writes `response` over `writer`. The head and the body go out in one write.
;; `method` is the method being answered: the response to a `HEAD` carries the
;; headers of a body it does not send, and a 1xx, a 204 and a 304 send no body
;; for any method.
(define (http-write-response! writer received #:keep-alive [keep-alive #f] #:method [method "GET"])
  (write-wire! writer (response->wire received keep-alive method)))

(define (answer-to handler sent)
  (define answered
    (with-handler (lambda (err)
                    ;; What the handler raised is not sent to the client.
                    (make-response 500))
                  (handler sent)))
  (if (framable? answered (request-method sent))
      answered
      (make-response 500)))

;; An unframable response - one carrying `Transfer-Encoding`, or built with the
;; bare constructor and no body - becomes a 500 here. `http-write-response!`
;; still raises.
(define (framable? answered method)
  (and (response? answered)
       (with-handler (lambda (err) #f)
                     (begin
                       (response->wire answered #f method)
                       #t))))

;; The body is the reason phrase alone, so nothing read off the wire is sent
;; back. `http-read-request` raises with the detail.
(define (problem-response report)
  (make-response (problem-report-status report)
                 #:headers (list (cons "Content-Type" "text/plain; charset=utf-8"))
                 #:body (string-append (reason-phrase (problem-report-status report)) "\n")))

;; A closed client ends the connection, not the loop serving it.
(define (write-answer writer answered keep-alive? method)
  (with-handler (lambda (err) #f)
                (begin
                  (write-wire! writer (response->wire answered keep-alive? method))
                  #t)))

;;@doc
;; Serves requests on one connection until it ends, and returns how many it
;; served. `handler` is called with a `request?` and returns a `response?`; any
;; other value, or a raise, becomes a 500. A request this server cannot read is
;; answered with a 4xx or a 5xx and ends the connection. The connection is held
;; open for another request when both ends allow it, and `#:keep-alive #false`
;; serves exactly one request.
(define (http-serve-connection reader writer handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (let loop ([served 0])
    (cond
      [(and limit (>= served limit)) served]
      [else
       (let ([outcome (read-request reader writer)])
         (cond
           [(eof-object? outcome) served]
           [(problem-report? outcome)
            ;; An unreadable request leaves the stream at an unknown point, so
            ;; the connection ends after this answer.
            (if (write-answer writer (problem-response outcome) #f "GET")
                (+ served 1)
                served)]
           [else
            (let* ([sent outcome]
                   [answered (answer-to handler sent)]
                   [again? (and keep-alive
                                (request-keep-alive? sent)
                                (response-keep-alive? answered)
                                (not (and limit (>= (+ served 1) limit))))])
              (cond
                [(not (write-answer writer answered again? (request-method sent))) served]
                [again? (loop (+ served 1))]
                [else (+ served 1)]))]))])))

(define (connection-ports server stream)
  (when (server-read-timeout server)
    (tcp-stream-set-read-timeout! stream (server-read-timeout server)))
  (cons (tcp-stream-buffered-reader stream) (tcp-stream-writer stream)))

;;@doc
;; Waits for one connection, serves requests on it, and closes it. Returns how
;; many requests it served.
(define (http-serve-one server handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (define stream (tcp-accept (server-listener server)))
  (define ports (connection-ports server stream))
  (dynamic-wind
   (lambda () void)
   (lambda ()
     (http-serve-connection (car ports) (cdr ports) handler #:keep-alive keep-alive #:limit limit))
   ;; A client that has already closed makes this raise.
   (lambda () (with-handler (lambda (err) void) (tcp-shutdown! stream)))))

;;@doc
;; Serves connections one after another, and returns how many it served.
;; Connections are served in sequence, so a slow request delays the next one.
;; Pass `#:connections` to stop after that many.
;;
;; ```scheme
;; (define server (http-listen "127.0.0.1:0"))
;; (http-serve server (lambda (sent) (make-response 200 #:body "hello")))
;; ```
(define (http-serve server
                    handler
                    #:keep-alive [keep-alive #t]
                    #:connections [connections #f]
                    #:requests [requests #f])
  (let loop ([served 0])
    (cond
      [(and connections (>= served connections)) served]
      [else
       (http-serve-one server handler #:keep-alive keep-alive #:limit requests)
       (loop (+ served 1))])))
