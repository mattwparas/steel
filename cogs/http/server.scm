(require-builtin steel/tcp)
(require "chars.scm")
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
;; The value sent in the `Server` header
(define *server-name* "steel-http/0.1.0")

;;@doc
;; The largest request body this server reads
(define *max-body-bytes* 8388608)

(struct server (listener address read-timeout) #:transparent)

;; TODO: hacking around a bug where re-raising inside handler panics
(struct problem-report (status text) #:transparent)

;;@doc
;; Binds a listener. A port of `0` takes a free port from the os.
(define (http-listen address #:read-timeout [read-timeout #f])
  (unless (or (not read-timeout) (and (int? read-timeout) (> read-timeout 0)))
    (error "http: a read timeout is a positive number of milliseconds or #false, got:" read-timeout))
  (define listener (tcp-listen address))
  (server listener (tcp-listener-local-addr listener) read-timeout))

(define (request-line-parts line)
  (define parts (split-many line " "))
  (if (equal? (length parts) 3) parts #f))

(define (host-char? char)
  (not (string-contains? "/?#@" char)))

(define (target->url target host)
  (cond
    ;; The host is prepended to the target, so it must only be an authority.
    ;; Otherwise it could change the path.
    [(and host (not (every-char? host host-char?))) #f]
    [(equal? target "*") (make-url (or host "") #:path "*")]
    [(string-contains? target "://") (parse-url target)]
    [(not (starts-with? target "/")) #f]
    [host (parse-url (string-append "http://" host target))]
    [else
     (let ([split (split-once target "?")])
       (if (list? split)
           (make-url "" #:path (car split) #:query (list-ref split 1))
           (make-url "" #:path target)))]))

(define (request-body-mode headers)
  ;; If the encodings are anything but exactly chunked, the sender and this
  ;; server do not agree on where the body ends
  (define encodings (header-tokens headers "Transfer-Encoding"))
  (define declared (content-length headers))
  (cond
    [(equal? encodings '("chunked"))
     (if declared
         (problem-report 400 "a request carried both Transfer-Encoding: chunked and Content-Length")
         'chunked)]
    [(not (empty? encodings))
     (problem-report 501
                     (string-append "unsupported Transfer-Encoding: " (string-join encodings ", ")))]
    [(and declared (> declared *max-body-bytes*))
     (problem-report 413
                     (string-append "a request declared a body of " (int->string declared) " bytes"))]
    [declared declared]
    [else 'none]))

(define (or-bad-request thunk)
  (with-handler (lambda (err)
                  (problem-report (if (would-block-error? err) 408 400)
                                  (trim (to-string (error-object-message err)))))
                (thunk)))

(define (expects-continue? headers)
  (if (member "100-continue" (header-tokens headers "Expect")) #t #f))

(define (read-request-line reader)
  (let loop ([skipped 0])
    (define line (read-wire-line reader))
    (cond
      [(eof-object? line) line]
      [(and (equal? line "") (< skipped 2)) (loop (+ skipped 1))]
      [else line])))

(define (read-request reader writer)
  (define line (or-bad-request (lambda () (read-request-line reader))))
  (cond
    [(eof-object? line) line]
    [(problem-report? line) line]
    [else
     (let ([parts (request-line-parts line)])
       (cond
         [(not parts) (problem-report 400 "not a request line")]
         [(not (token? (car parts))) (problem-report 400 "not a usable method")]
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
                   (append headers (usable-trailers (cdr body-and-trailers)))
                   (car body-and-trailers)
                   (list-ref parts 2))]))]))

(define (http-read-request reader #:continue [continue #f])
  (define outcome (read-request reader continue))
  (cond
    [(eof-object? outcome) outcome]
    [(problem-report? outcome)
     (error "http:" (problem-report-text outcome) "- would answer" (problem-report-status outcome))]
    [else outcome]))

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
      (header-default headers "Content-Length" (int->string (bytes-length body)))))

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

(define (http-write-response! writer received #:keep-alive [keep-alive #f] #:method [method "GET"])
  (write-wire! writer (response->wire received keep-alive method)))

(define (answer-to handler sent)
  (define answered (with-handler (lambda (err) (make-response 500)) (handler sent)))
  (if (framable? answered (request-method sent))
      answered
      (make-response 500)))

(define (framable? answered method)
  (and (response? answered)
       (with-handler (lambda (err) #f)
                     (begin
                       (response->wire answered #f method)
                       #t))))

(define (problem-response report)
  (make-response (problem-report-status report)
                 #:headers (list (cons "Content-Type" "text/plain; charset=utf-8"))
                 #:body (string-append (reason-phrase (problem-report-status report)) "\n")))

(define (write-answer writer answered keep-alive? method)
  (with-handler (lambda (err) #f)
                (begin
                  (write-wire! writer (response->wire answered keep-alive? method))
                  #t)))

;; TODO: Add thread pool support here
;;@doc
;; Serves requests on one connection until it ends, and returns how many it
;; served. `handler` takes a `request?` and returns a `response?`; a raise, or
;; any other value, becomes a 500.
;;
;; (http-serve-connection reader writer handler [#:keep-alive keep-alive]
;;                        [#:limit limit]) -> int?
;;
;; * reader : input-port?
;; * writer : output-port?
;; * handler : procedure?
;; * keep-alive : bool? = #true - hold the connection open for another request
;; * limit : (or/c int? #f) = #f - stop after this many requests
(define (http-serve-connection reader writer handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (let loop ([served 0])
    (cond
      [(and limit (>= served limit)) served]
      [else
       (let ([outcome (read-request reader writer)])
         (cond
           [(eof-object? outcome) served]
           [(problem-report? outcome)
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
;; Waits for one connection, serves it, and closes it.
(define (http-serve-one server handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (define stream (tcp-accept (server-listener server)))
  (define ports (connection-ports server stream))
  (dynamic-wind
   (lambda () void)
   (lambda ()
     (http-serve-connection (car ports) (cdr ports) handler #:keep-alive keep-alive #:limit limit))
   (lambda () (with-handler (lambda (err) void) (tcp-shutdown! stream)))))

;; TODO: Add threads here
;;@doc
;; # Examples
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
