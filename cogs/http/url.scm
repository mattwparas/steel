(provide url?
         url-scheme
         url-userinfo
         url-host
         url-port
         url-path
         url-query
         url-fragment
         make-url
         parse-url
         url->string
         url-authority
         url-origin
         url-target
         url-with-query
         url-join
         scheme-default-port
         url-encode
         url-decode
         alist->query
         query->alist)

(struct url (scheme userinfo host port path query fragment) #:transparent)

;;@doc
;; The port a scheme is served on when a url does not say, or `#false` for a
;; scheme with no default.
(define (scheme-default-port scheme)
  (cond
    [(equal? scheme "http") 80]
    [(equal? scheme "https") 443]
    [else #f]))

;;@doc
;; Builds a url out of its parts. The port defaults to the one belonging to
;; *scheme*.
(define (make-url host
                  #:scheme [scheme "http"]
                  #:userinfo [userinfo #f]
                  #:port [port #f]
                  #:path [path "/"]
                  #:query [query #f]
                  #:fragment [fragment #f])
  (checked-url scheme
               userinfo
               host
               (or port (scheme-default-port scheme))
               (if (equal? path "") "/" path)
               query
               fragment))

;; A space or a newline in a path would end the request line and let the
;; caller write their own headers after it, so nothing unencoded gets through.
(define (check-url-part what str)
  (when str
    (let loop ([at 0])
      (when (< at (string-length str))
        (let ([code (char->integer (string-ref str at))])
          (when (or (<= code 32) (equal? code 127))
            (error "url: an unescaped control character or space in the"
                   what
                   "of a url - use url-encode:"
                   str))
          (loop (+ at 1)))))))

(define (checked-url scheme userinfo host port path query fragment)
  (check-url-part "host" host)
  (check-url-part "userinfo" userinfo)
  (check-url-part "path" path)
  (check-url-part "query" query)
  (check-url-part "fragment" fragment)
  (url scheme userinfo host port path query fragment))

(define (char-between? char low high)
  (and (char>=? char low) (char<=? char high)))

(define (ascii-letter? char)
  (or (char-between? char #\a #\z) (char-between? char #\A #\Z)))

(define (index-of-any str chars)
  (define len (string-length str))
  (let loop ([at 0])
    (cond
      [(>= at len) #f]
      [(member (string-ref str at) chars) at]
      [else (loop (+ at 1))])))

(define (last-index-of str char)
  (let loop ([at (- (string-length str) 1)])
    (cond
      [(< at 0) #f]
      [(char=? (string-ref str at) char) at]
      [else (loop (- at 1))])))

(define (drop-prefix str at)
  (substring str at (string-length str)))

;; A scheme is a letter followed by letters, digits, `+`, `-` and `.`. Anything
;; else in front of a `://` belongs to the path.
(define (scheme-name? str)
  (and (> (string-length str) 0)
       (ascii-letter? (string-ref str 0))
       (let loop ([at 1])
         (or (>= at (string-length str))
             (let ([char (string-ref str at)])
               (and (or (ascii-letter? char)
                        (char-digit? char)
                        (char=? char #\+)
                        (char=? char #\-)
                        (char=? char #\.))
                    (loop (+ at 1))))))))

(define (split-host-and-port authority)
  (cond
    ;; An ipv6 literal keeps its brackets out of the host, so that the host is
    ;; what goes in the `Host` header and what gets looked up.
    [(starts-with? authority "[")
     (let ([close (index-of-any authority (list #\]))])
       (unless close
         (error "url: an ipv6 literal was never closed:" authority))
       (cons (substring authority 1 close)
             (port-after (drop-prefix authority (+ close 1)) authority)))]
    [else
     (let ([colon (last-index-of authority #\:)])
       (if colon
           (cons (substring authority 0 colon) (port-after (drop-prefix authority colon) authority))
           (cons authority #f)))]))

(define (port-after rest authority)
  (cond
    [(equal? rest "") #f]
    [(not (starts-with? rest ":")) (error "url: unexpected text after the host:" authority)]
    [else
     (let ([port (string->number (drop-prefix rest 1))])
       (unless (and (int? port) (> port 0) (< port 65536))
         (error "url: not a valid port:" authority))
       port)]))

;;@doc
;; Parses a url. Already parsed urls are handed back as they are.
;;
;; A url with no scheme is taken to be `http`, and an empty path is taken to be
;; `/`. Percent escapes are left exactly as they were given.
(define (parse-url input)
  (if (url? input)
      input
      (parse-url-string input)))

(define (parse-url-string str)
  (define split (split-once str "://"))
  (define scheme
    (if (and (list? split) (scheme-name? (car split)))
        (string-downcase (car split))
        #f))
  (define rest
    (if scheme
        (list-ref split 1)
        str))
  (define authority-end (or (index-of-any rest (list #\/ #\? #\#)) (string-length rest)))
  (define authority (substring rest 0 authority-end))
  (define tail (drop-prefix rest authority-end))
  (define at (last-index-of authority #\@))
  (define host-and-port
    (split-host-and-port (if at
                             (drop-prefix authority (+ at 1))
                             authority)))
  (define hash-at (index-of-any tail (list #\#)))
  (define before-fragment
    (if hash-at
        (substring tail 0 hash-at)
        tail))
  (define question-at (index-of-any before-fragment (list #\?)))
  (define path
    (if question-at
        (substring before-fragment 0 question-at)
        before-fragment))

  (when (equal? (car host-and-port) "")
    (error "url: no host in:" str))

  (checked-url (or scheme "http")
               (if at
                   (substring authority 0 at)
                   #f)
               (car host-and-port)
               (or (cdr host-and-port) (scheme-default-port (or scheme "http")))
               (if (equal? path "") "/" path)
               (if question-at
                   (drop-prefix before-fragment (+ question-at 1))
                   #f)
               (if hash-at
                   (drop-prefix tail (+ hash-at 1))
                   #f)))

(define (bracketed-host host)
  (if (and (string-contains? host ":") (not (starts-with? host "[")))
      (string-append "[" host "]")
      host))

;;@doc
;; The host and port of a url, as it belongs in a `Host` header. The port is
;; left out when it is the default for the scheme.
(define (url-authority url)
  (define host (bracketed-host (url-host url)))
  (if (or (not (url-port url)) (equal? (url-port url) (scheme-default-port (url-scheme url))))
      host
      (string-append host ":" (int->string (url-port url)))))

;;@doc
;; The scheme, host and port of a url. Two urls with the same origin can share
;; a connection.
(define (url-origin url)
  (string-append (url-scheme url) "://" (url-authority url)))

;;@doc
;; The request target of a url, which is the path and the query. This is what
;; goes on the request line.
(define (url-target url)
  (if (url-query url)
      (string-append (url-path url) "?" (url-query url))
      (url-path url)))

;;@doc
;; Writes a url back out as a string.
(define (url->string url)
  (string-append (url-scheme url)
                 "://"
                 (if (url-userinfo url)
                     (string-append (url-userinfo url) "@")
                     "")
                 (url-authority url)
                 (url-target url)
                 (if (url-fragment url)
                     (string-append "#" (url-fragment url))
                     "")))

;;@doc
;; The same url with a different query, given either as a string or as an
;; association list to encode.
(define (url-with-query url query)
  (make-url (url-host url)
            #:scheme (url-scheme url)
            #:userinfo (url-userinfo url)
            #:port (url-port url)
            #:path (url-path url)
            #:query (cond
                      [(not query) #f]
                      [(string? query) query]
                      [else (alist->query query)])
            #:fragment (url-fragment url)))

;; `/a/./b/../c` -> `/a/c`, the way a client has to read a `Location` before it
;; can ask for it.
(define (remove-dot-segments path)
  (define absolute? (starts-with? path "/"))
  (define segments (split-many path "/"))
  (define kept
    (let loop ([rest segments]
               [out '()])
      (cond
        [(empty? rest) (reverse out)]
        [(or (equal? (car rest) "") (equal? (car rest) ".")) (loop (cdr rest) out)]
        [(equal? (car rest) "..")
         (loop (cdr rest)
               (if (empty? out)
                   out
                   (cdr out)))]
        [else (loop (cdr rest) (cons (car rest) out))])))
  (define directory?
    (and (not (empty? segments))
         (let ([last-segment (list-ref segments (- (length segments) 1))])
           (or (equal? last-segment "") (equal? last-segment ".") (equal? last-segment "..")))))
  (string-append (if absolute? "/" "")
                 (string-join kept "/")
                 (if (and directory? (not (empty? kept))) "/" "")))

(define (merge-paths base reference)
  (define cut (last-index-of base #\/))
  (remove-dot-segments (string-append (if cut
                                          (substring base 0 (+ cut 1))
                                          "/")
                                      reference)))

;;@doc
;; Resolves *reference* against *base*, the way a `Location` header has to be
;; read. The reference may be a full url, a `//host/path`, an absolute path, a
;; relative path, a bare query or a bare fragment.
(define (url-join base reference)
  (define from (parse-url base))
  (cond
    [(url? reference) reference]
    [(list? (split-once reference "://")) (parse-url reference)]
    [(starts-with? reference "//") (parse-url (string-append (url-scheme from) ":" reference))]
    [(equal? reference "") from]
    [(starts-with? reference "#")
     (make-url (url-host from)
               #:scheme (url-scheme from)
               #:userinfo (url-userinfo from)
               #:port (url-port from)
               #:path (url-path from)
               #:query (url-query from)
               #:fragment (drop-prefix reference 1))]
    [else
     (let* ([hash-at (index-of-any reference (list #\#))]
            [fragment (if hash-at
                          (drop-prefix reference (+ hash-at 1))
                          #f)]
            [before-fragment (if hash-at
                                 (substring reference 0 hash-at)
                                 reference)]
            [question-at (index-of-any before-fragment (list #\?))]
            [query (if question-at
                       (drop-prefix before-fragment (+ question-at 1))
                       #f)]
            [path (if question-at
                      (substring before-fragment 0 question-at)
                      before-fragment)])
       (make-url (url-host from)
                 #:scheme (url-scheme from)
                 #:userinfo (url-userinfo from)
                 #:port (url-port from)
                 #:path (cond
                          [(equal? path "") (url-path from)]
                          [(starts-with? path "/") (remove-dot-segments path)]
                          [else (merge-paths (url-path from) path)])
                 #:query (if (and (equal? path "") (not query))
                             (url-query from)
                             query)
                 #:fragment fragment))]))

(define *hex-digits* "0123456789ABCDEF")

(define (byte->hex byte)
  (string #\%
          (string-ref *hex-digits* (quotient byte 16))
          (string-ref *hex-digits* (modulo byte 16))))

(define (unreserved-byte? byte)
  (or (and (>= byte 48) (<= byte 57))
      (and (>= byte 65) (<= byte 90))
      (and (>= byte 97) (<= byte 122))
      (= byte 45)
      (= byte 46)
      (= byte 95)
      (= byte 126)))

;;@doc
;; Percent encodes a string. Letters, digits and `-._~` are left alone, every
;; other byte becomes `%XX`. Characters in *safe* are left alone too, which is
;; what keeps the `/` in a path.
(define (url-encode str #:safe [safe ""])
  (define out (open-output-string))
  (define safe-bytes (bytes->list (string->bytes safe)))
  (for-each (lambda (byte)
              (if (or (unreserved-byte? byte) (member byte safe-bytes))
                  (display (bytes->string/utf8 (bytes byte)) out)
                  (display (byte->hex byte) out)))
            (bytes->list (string->bytes str)))
  (get-output-string out))

(define (hex-digit-value char)
  (cond
    [(char-between? char #\0 #\9) (- (char->integer char) 48)]
    [(char-between? char #\a #\f) (- (char->integer char) 87)]
    [(char-between? char #\A #\F) (- (char->integer char) 55)]
    [else #f]))

;;@doc
;; Undoes `url-encode`. A `+` becomes a space, since that is what form encoding
;; does, and a `%` that is not followed by two hex digits is left as it is.
(define (url-decode str)
  (define len (string-length str))
  (define out (bytes))
  (define (push-char char)
    (for-each (lambda (byte) (bytes-push! out byte)) (bytes->list (string->bytes (string char)))))
  (let loop ([at 0])
    (cond
      [(>= at len) (bytes->string/utf8 out)]
      [else
       (let ([char (string-ref str at)])
         (cond
           [(char=? char #\+)
            (bytes-push! out 32)
            (loop (+ at 1))]
           [(and (char=? char #\%)
                 (<= (+ at 3) len)
                 (hex-digit-value (string-ref str (+ at 1)))
                 (hex-digit-value (string-ref str (+ at 2))))
            (bytes-push! out
                         (+ (* 16 (hex-digit-value (string-ref str (+ at 1))))
                            (hex-digit-value (string-ref str (+ at 2)))))
            (loop (+ at 3))]
           [else
            (push-char char)
            (loop (+ at 1))]))])))

(define (query-part->string value)
  (cond
    [(string? value) value]
    [(symbol? value) (symbol->string value)]
    [(number? value) (number->string value)]
    [(equal? value #t) "true"]
    [(equal? value #f) "false"]
    [else (error "url: a query value has to be a string, symbol, number or boolean, got:" value)]))

;;@doc
;; Encodes an association list as a query string. Keys and values are percent
;; encoded, and a `#false` value means a key with no `=`.
;;
;; ```scheme
;; (alist->query '(("q" . "a b") ("page" . 2))) ;; => "q=a%20b&page=2"
;; ```
(define (alist->query alist)
  (string-join (map (lambda (pair)
                      (define key (url-encode (query-part->string (car pair))))
                      (if (equal? (cdr pair) #f)
                          key
                          (string-append key "=" (url-encode (query-part->string (cdr pair))))))
                    alist)
               "&"))

;;@doc
;; Decodes a query string into an association list. A key with no `=` comes
;; back with an empty value, and repeated keys are all kept, in order.
(define (query->alist query)
  (if (not query)
      '()
      (map (lambda (part)
             (define split (split-once part "="))
             (if (list? split)
                 (cons (url-decode (car split)) (url-decode (list-ref split 1)))
                 (cons (url-decode part) "")))
           (filter (lambda (part) (not (equal? part ""))) (split-many query "&")))))
