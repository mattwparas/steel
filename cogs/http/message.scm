(require "url.scm")
(require "headers.scm")
(require "base64.scm")

(provide *http-version*
         *max-header-lines*
         *max-line-length*
         known-version?
         request
         response
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
         method->string
         token-member?
         content-length
         read-wire-line
         read-wire-headers
         read-exactly
         read-until-close
         read-chunked)

;;@doc
;; The version written on every request line and status line.
(define *http-version* "HTTP/1.1")

;;@doc
;; The maximum number of header lines in a message. Reading a message with more
;; than this raises.
(define *max-header-lines* 200)

;;@doc
;; The maximum length of one line of a head. Reading a longer line raises.
(define *max-line-length* 8192)

(struct request (method url headers body version) #:transparent)

(define (method->string method)
  (define text
    (cond
      [(string? method) (string-upcase method)]
      [(symbol? method) (string-upcase (symbol->string method))]
      [else (error "http: a method has to be a string or a symbol, got:" method)]))
  (unless (token? text)
    (error "http: not a usable method:" text))
  text)

(define (body->bytes body)
  (cond
    [(not body) #f]
    [(string? body) (string->bytes body)]
    [(bytes? body) body]
    [else (error "http: a body has to be a string or a bytevector, got:" body)]))

(define (basic-credentials auth)
  (cond
    [(string? auth) auth]
    [(and (pair? auth) (not (list? auth))) (string-append (car auth) ":" (cdr auth))]
    [(and (list? auth) (equal? (length auth) 2)) (string-append (car auth) ":" (list-ref auth 1))]
    [else
     (error "http: expected a \"user:password\" string or a (user . password) pair, got:" auth)]))

(define (authorization headers url auth)
  (cond
    [auth
     (header-set headers
                 "Authorization"
                 (string-append "Basic " (base64-encode (basic-credentials auth))))]
    ;; Credentials in the url belong in the `Authorization` header.
    [(url-userinfo url)
     (header-set headers
                 "Authorization"
                 (string-append "Basic " (base64-encode (url-decode (url-userinfo url)))))]
    [else headers]))

;;@doc
;; Builds a request. Either `body` or `form` may be given, not both: `form` is
;; an association list, which is url encoded and sent as
;; `application/x-www-form-urlencoded`. `query` replaces the query of `target`,
;; and `auth` becomes a basic `Authorization` header.
(define (make-request method
                      target
                      #:headers [headers '()]
                      #:body [body #f]
                      #:form [form #f]
                      #:query [query #f]
                      #:auth [auth #f]
                      #:version [version *http-version*])
  (when (and body form)
    (error "http: give a request either a body or a form, not both"))
  (define url
    (let ([parsed (parse-url target)])
      (if query
          (url-with-query parsed query)
          parsed)))
  (define given (normalize-headers headers))
  (define with-content-type
    (if form
        (header-default given "Content-Type" "application/x-www-form-urlencoded")
        given))
  (request (method->string method)
           url
           (authorization with-content-type url auth)
           (body->bytes (if form
                            (alist->query form)
                            body))
           version))

;;@doc
;; The target for the request line: the path, followed by the query when there
;; is one.
(define (request-target sent)
  (url-target (request-url sent)))

;;@doc
;; The first value of a request header, or `default` when it is absent.
(define (request-header sent name [default #f])
  (header-ref (request-headers sent) name default))

;;@doc
;; Every value of a request header.
(define (request-header* sent name)
  (header-ref* (request-headers sent) name))

;;@doc
;; Whether the request allows another exchange on the connection. An HTTP/1.1
;; request keeps the connection open unless it sends `Connection: close`; an
;; HTTP/1.0 request closes it unless it sends `Connection: keep-alive`.
(define (request-keep-alive? sent)
  (define asked (request-header sent "Connection"))
  (cond
    [(and asked (token-member? asked "close")) #f]
    [(equal? (request-version sent) "HTTP/1.1") #t]
    [asked (token-member? asked "keep-alive")]
    [else #f]))

(struct response (version status reason headers body) #:transparent)

;;@doc
;; Whether the status is a 2xx.
(define (response-ok? received)
  (and (>= (response-status received) 200) (< (response-status received) 300)))

;;@doc
;; The body decoded as utf8. Raises when the body is not utf8; use
;; `response-body` when it may not be text.
(define (response-text received)
  (bytes->string/utf8 (response-body received)))

;;@doc
;; The first value of a response header, or `default` when it is absent.
(define (response-header received name [default #f])
  (header-ref (response-headers received) name default))

;;@doc
;; Every value of a response header.
(define (response-header* received name)
  (header-ref* (response-headers received) name))

;;@doc
;; Whether the response allows another exchange on the connection.
(define (response-keep-alive? received)
  (define asked (response-header received "Connection"))
  (cond
    [(and asked (token-member? asked "close")) #f]
    [(equal? (response-version received) "HTTP/1.1") #t]
    [asked (token-member? asked "keep-alive")]
    [else #f]))

;; The phrases from RFC 9110. A code that is not listed gets an empty phrase.
(define *reasons*
  (list (cons 100 "Continue")
        (cons 101 "Switching Protocols")
        (cons 200 "OK")
        (cons 201 "Created")
        (cons 202 "Accepted")
        (cons 204 "No Content")
        (cons 206 "Partial Content")
        (cons 301 "Moved Permanently")
        (cons 302 "Found")
        (cons 303 "See Other")
        (cons 304 "Not Modified")
        (cons 307 "Temporary Redirect")
        (cons 308 "Permanent Redirect")
        (cons 400 "Bad Request")
        (cons 401 "Unauthorized")
        (cons 403 "Forbidden")
        (cons 404 "Not Found")
        (cons 405 "Method Not Allowed")
        (cons 406 "Not Acceptable")
        (cons 408 "Request Timeout")
        (cons 409 "Conflict")
        (cons 411 "Length Required")
        (cons 413 "Content Too Large")
        (cons 414 "URI Too Long")
        (cons 415 "Unsupported Media Type")
        (cons 418 "I'm a teapot")
        (cons 422 "Unprocessable Content")
        (cons 426 "Upgrade Required")
        (cons 429 "Too Many Requests")
        (cons 431 "Request Header Fields Too Large")
        (cons 500 "Internal Server Error")
        (cons 501 "Not Implemented")
        (cons 502 "Bad Gateway")
        (cons 503 "Service Unavailable")
        (cons 504 "Gateway Timeout")
        (cons 505 "HTTP Version Not Supported")))

;;@doc
;; The reason phrase for `status`, or `""` for a code with no well known
;; phrase.
(define (reason-phrase status)
  (define found (assv status *reasons*))
  (if found
      (cdr found)
      ""))

;;@doc
;; Builds a response. The reason phrase is filled in from `status` when it is
;; not given, and the body may be a string or a bytevector.
(define (make-response status
                       #:reason [reason #f]
                       #:headers [headers '()]
                       #:body [body #f]
                       #:version [version *http-version*])
  (unless (and (int? status) (>= status 100) (< status 1000))
    (error "http: not a usable status code:" status))
  (response version
            status
            (or reason (reason-phrase status))
            (normalize-headers headers)
            (or (body->bytes body) (bytes))))

(define (strip-cr line)
  ;; A tcp reader returns the `\r` of a crlf and a string port does not.
  (if (ends-with? line "\r")
      (substring line 0 (- (string-length line) 1))
      line))

(define (would-block-error)
  (error "http: this read would block - the port is non blocking, or its read timeout ran out"))

(define (line-so-far chars)
  (strip-cr (list->string (reverse chars))))

;; `read-line` has no limit, so lines are read a character at a time.
(define (read-wire-line reader)
  (let loop ([chars '()]
             [length 0])
    (define char (read-char reader))
    (cond
      [(would-block-object? char) (would-block-error)]
      [(eof-object? char)
       (if (empty? chars)
           char
           (line-so-far chars))]
      [(char=? char #\newline) (line-so-far chars)]
      [(>= length *max-line-length*)
       (error "http: a line of the head was longer than" *max-line-length* "characters")]
      [else (loop (cons char chars) (+ length 1))])))

;;@doc
;; Whether `version` is a version this library supports.
(define (known-version? version)
  (or (equal? version "HTTP/1.1") (equal? version "HTTP/1.0")))

(define (token-member? value token)
  (if (member token (map (lambda (part) (string-downcase (trim part))) (split-many value ",")))
      #t
      #f))

(define (continuation-line? line)
  (or (starts-with? line " ") (starts-with? line "\t")))

(define (read-wire-headers reader)
  (let loop ([headers '()]
             [lines 0])
    (when (>= lines *max-header-lines*)
      (error "http: a message had more than" *max-header-lines* "header lines"))
    (define line (read-wire-line reader))
    (cond
      [(eof-object? line) (error "http: the connection closed in the middle of the headers")]
      [(equal? line "") (reverse headers)]
      ;; An obs-fold continuation belongs to the header above it.
      [(and (continuation-line? line) (not (empty? headers)))
       (loop (cons (cons (car (car headers)) (string-append (cdr (car headers)) " " (trim line)))
                   (cdr headers))
             (+ lines 1))]
      [else
       (let ([header (parse-header-line line)])
         (unless header
           (error "http: not a header line:" line))
         (loop (cons header headers) (+ lines 1)))])))

(define (content-length headers)
  ;; One `Content-Length: 11, 11` and two `Content-Length: 11` headers mean the
  ;; same thing. Values that disagree are an error.
  (define parts
    (apply append
           (map (lambda (value) (map trim (split-many value ",")))
                (header-ref* headers "Content-Length"))))
  (cond
    [(empty? parts) #f]
    [else
     (let ([lengths (map (lambda (part) (string->number part)) parts)])
       (for-each (lambda (each)
                   (unless (and (int? each) (>= each 0))
                     (error "http: not a usable Content-Length:"
                            (header-ref headers "Content-Length"))))
                 lengths)
       (for-each (lambda (each)
                   (unless (equal? each (car lengths))
                     (error "http: a message carried Content-Length headers that disagree:" parts)))
                 lengths)
       (car lengths))]))

;; `read-bytes` allocates the whole amount up front, so a declared length is
;; read a chunk at a time.
(define *read-chunk-bytes* 65536)

(define (read-exactly reader amount)
  (let loop ([chunks '()]
             [left amount])
    (cond
      [(equal? left 0) (apply bytes-append (reverse chunks))]
      [else
       (let ([chunk (read-bytes (min left *read-chunk-bytes*) reader)])
         (when (eof-object? chunk)
           (error "http: the connection closed with" left "bytes of the body still to come"))
         (when (would-block-object? chunk)
           (would-block-error))
         (when (equal? (bytes-length chunk) 0)
           (error "http: the port stopped handing out bytes with" left "left to read"))
         (loop (cons chunk chunks) (- left (bytes-length chunk))))])))

(define (read-until-close reader)
  (define body (read-port-to-bytes reader))
  (if (eof-object? body)
      (bytes)
      body))

(define (parse-chunk-size line)
  ;; A chunk size may carry extensions, as in `1f;name=value`.
  (define size (string->number (trim (car (split-many line ";"))) 16))
  (unless (and (int? size) (>= size 0))
    (error "http: not a usable chunk size:" line))
  size)

;;@doc
;; Reads a chunked body off `reader`, and returns it with the trailer headers
;; that followed it. With a `limit`, a body longer than it returns
;; `'too-large`, and nothing further can be read from the connection.
(define (read-chunked reader #:limit [limit #f])
  (let loop ([chunks '()]
             [read-so-far 0])
    (define line (read-wire-line reader))
    (when (eof-object? line)
      (error "http: the connection closed before a chunk size"))
    (define size (parse-chunk-size line))
    (cond
      [(and limit (> (+ read-so-far size) limit)) 'too-large]
      [(equal? size 0) (cons (apply bytes-append (reverse chunks)) (read-wire-headers reader))]
      [else
       ;; `let*`, since the chunk has to be read before the crlf that follows
       ;; it.
       (let* ([chunk (read-exactly reader size)]
              [terminator (read-wire-line reader)])
         (unless (equal? terminator "")
           (error "http: a chunk did not end in a crlf:" terminator))
         (loop (cons chunk chunks) (+ read-so-far size)))])))
