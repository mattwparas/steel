(require-builtin steel/tcp)
(require "url.scm")
(require "headers.scm")
(require "message.scm")

(provide *server-name*
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
;; What this server calls itself in the `Server` header.
(define *server-name* "steel-http/0.1.0")

;; ------------------------------------------------------------------ listening

(struct server (listener address read-timeout) #:transparent)

;;@doc
;; Binds a listener. A port of `0` asks the os for a free one, which
;; `server-address` then reports. A *read-timeout* in milliseconds is put on
;; every connection accepted, so a client that says nothing is given up on.
(define (http-listen address #:read-timeout [read-timeout #f])
  (define listener (tcp-listen address))
  (server listener (tcp-listener-local-addr listener) read-timeout))

;; ------------------------------------------------------------------- requests

(define (request-line-parts line)
  ;; A request line is three fields with one space between them; anything else
  ;; is refused rather than guessed at.
  (define parts (split-many line " "))
  (if (equal? (length parts) 3) parts #f))

(define (known-version? version)
  (or (equal? version "HTTP/1.1") (equal? version "HTTP/1.0")))

;; The target may be origin form (`/a?b`), absolute form (`http://host/a`, what
;; a proxy is sent), or `*` for an `OPTIONS` about the server itself.
(define (target->url target host)
  (cond
    [(equal? target "*") (make-url (or host "") #:path "*")]
    [(string-contains? target "://") (parse-url target)]
    [(not (starts-with? target "/")) #f]
    [host (parse-url (string-append "http://" host target))]
    [else
     ;; An HTTP/1.0 request may arrive with no Host at all, leaving a path with
     ;; no authority to go under it.
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
    ;; No framing at all means no body: reading until the connection closed
    ;; instead would leave nothing to answer on.
    [declared declared]
    [else 'none]))

;; What this server answers with when a request cannot be read. A raise cannot
;; carry a value through `with-handler`, and re-raising inside a handler panics
;; the interpreter, so a problem travels as a return value.
(struct problem-report (status text) #:transparent)

;; Anything the shared reader raises - an unparseable header line, a chunk size
;; that is not a number, a body cut short - is a request this server cannot read.
(define (or-bad-request thunk)
  (with-handler (lambda (err) (problem-report 400 (trim (to-string (error-object-message err)))))
                (thunk)))

(define (expects-continue? headers)
  (define expectation (header-ref headers "Expect"))
  (and expectation (token-member? expectation "100-continue")))

(define (read-request-line reader)
  ;; An empty line before the request line is a stray crlf left by the previous
  ;; request, and is skipped rather than treated as a request.
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
         ;; Two `Host` headers leave the next hop to decide which one counts.
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
     ;; `Expect: 100-continue` has the client waiting for a go ahead, which has
     ;; to go out before the body is read.
     (when (and writer (expects-continue? headers))
       (display (string-append *http-version* " 100 Continue\r\n\r\n") writer)
       (flush-output-port writer))
     (let ([body-and-trailers (or-bad-request (lambda ()
                                                (cond
                                                  [(equal? mode 'none) (cons #f '())]
                                                  [(equal? mode 'chunked) (read-chunked reader)]
                                                  [else (cons (read-exactly reader mode) '())])))])
       (cond
         [(problem-report? body-and-trailers) body-and-trailers]
         [else
          (request (string-upcase (car parts))
                   url
                   (append headers (cdr body-and-trailers))
                   (car body-and-trailers)
                   (list-ref parts 2))]))]))

;;@doc
;; Reads one request off *reader*. Returns the eof object when the connection
;; closed cleanly between requests, so a read loop can tell that from a request
;; this server cannot read, which raises.
;;
;; The body is a bytevector, or `#false` for a request with no framing.
;; Trailers that follow a chunked body are added to the headers. Give
;; *continue* the writer of the same connection to answer an
;; `Expect: 100-continue`.
(define (http-read-request reader #:continue [continue #f])
  (define outcome (read-request reader continue))
  (cond
    [(eof-object? outcome) outcome]
    [(problem-report? outcome)
     (error "http:" (problem-report-text outcome) "- would answer" (problem-report-status outcome))]
    [else outcome]))

;; ------------------------------------------------------------------ responses

;; A 1xx, a 204 and a 304 carry no body, so they go out with no length either:
;; a `Content-Length: 0` on a 304 contradicts the body it stands in for.
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
      ;; The length is the body's even for a HEAD, whose answer carries the
      ;; headers of the body it does not send.
      (header-default headers "Content-Length" (int->string (bytes-length body)))))

;;@doc
;; Writes *response* out over *writer*. The head and the body go out in one
;; write. *method* is the method being answered: the answer to a `HEAD` carries
;; the headers of a body it does not send, and a 1xx, a 204 and a 304 send no
;; body whatever the method.
(define (http-write-response! writer received #:keep-alive [keep-alive #f] #:method [method "GET"])
  (define status (response-status received))
  (define reason (response-reason received))
  (define head
    (string-append *http-version*
                   " "
                   (int->string status)
                   (if (equal? reason "")
                       ""
                       (string-append " " reason))
                   "\r\n"
                   (headers->string (wire-headers received keep-alive))
                   "\r\n"))
  (define body
    (if (or (bodyless-status? status) (equal? (method->string method) "HEAD"))
        (bytes)
        (response-body received)))
  (if (equal? (bytes-length body) 0)
      (display head writer)
      (write-bytes (bytes-append (string->bytes head) body) writer))
  (flush-output-port writer))

;; ------------------------------------------------------------------- serving

(define (answer-to handler sent)
  (define answered
    (with-handler (lambda (err)
                    ;; What the handler raised stays on this side of the wire.
                    (make-response 500))
                  (handler sent)))
  (if (response? answered)
      answered
      (make-response 500)))

;; The body is the reason phrase and nothing else; a detail read off the wire
;; would be going straight back out again. `http-read-request` raises with the
;; detail, for a caller reading a request itself.
(define (problem-response report)
  (make-response (problem-report-status report)
                 #:headers (list (cons "Content-Type" "text/plain; charset=utf-8"))
                 #:body (string-append (reason-phrase (problem-report-status report)) "\n")))

;;@doc
;; Serves requests on one connection until it ends, and returns how many it
;; served. *handler* is called with a `request?` and returns a `response?`;
;; anything else, or a raise, becomes a 500. A request this server cannot read
;; is answered with a 4xx or a 5xx and ends the connection. The connection is
;; held open for another request when both ends are willing, and `#:keep-alive
;; #false` serves exactly one whatever the request asked for.
(define (http-serve-connection reader writer handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (let loop ([served 0])
    (cond
      [(and limit (>= served limit)) served]
      [else
       (let ([outcome (read-request reader writer)])
         (cond
           [(eof-object? outcome) served]
           [(problem-report? outcome)
            ;; A request that could not be read leaves the connection at an
            ;; unknown place in the stream, so this is the last answer on it.
            (http-write-response! writer (problem-response outcome))
            (+ served 1)]
           [else
            (let* ([sent outcome]
                   [answered (answer-to handler sent)]
                   [again? (and keep-alive
                                (request-keep-alive? sent)
                                (response-keep-alive? answered)
                                (not (and limit (>= (+ served 1) limit))))])
              (http-write-response! writer
                                    answered
                                    #:keep-alive again?
                                    #:method (request-method sent))
              (if again?
                  (loop (+ served 1))
                  (+ served 1)))]))])))

(define (connection-ports server stream)
  (when (server-read-timeout server)
    (tcp-stream-set-read-timeout! stream (server-read-timeout server)))
  (cons (tcp-stream-buffered-reader stream) (tcp-stream-writer stream)))

;;@doc
;; Waits for one connection, serves requests on it, and hangs up. Returns how
;; many requests it served.
(define (http-serve-one server handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (define stream (tcp-accept (server-listener server)))
  (define ports (connection-ports server stream))
  (dynamic-wind
   (lambda () void)
   (lambda ()
     (http-serve-connection (car ports) (cdr ports) handler #:keep-alive keep-alive #:limit limit))
   ;; A client that has already hung up makes this raise.
   (lambda () (with-handler (lambda (err) void) (tcp-shutdown! stream)))))

;;@doc
;; Serves connections one after another, and returns how many it served. A slow
;; request holds up the next one. Pass `#:connections` to stop after that many.
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
