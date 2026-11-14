(require "chars.scm")

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
;; The default port for `scheme`, or `#false` when it has none
(define (scheme-default-port scheme)
  (cond
    [(equal? scheme "http") 80]
    [(equal? scheme "https") 443]
    [else #f]))

;;@doc
;; Builds a url out of its parts, with the scheme's default port.
;;
;; (make-url host [#:scheme scheme] [#:userinfo userinfo] [#:port port]
;;           [#:path path] [#:query query] [#:fragment fragment]) -> url?
;;
;; * host : string?
;; * scheme : string? = "http"
;; * userinfo : (or/c string? #f) = #f
;; * port : (or/c int? #f) = #f
;; * path : string? = "/"
;; * query : (or/c string? #f) = #f
;; * fragment : (or/c string? #f) = #f
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

(define (scheme-char? char)
  (or (ascii-letter? char) (char-digit? char) (char=? char #\+) (char=? char #\-) (char=? char #\.)))

(define (scheme-name? str)
  (and (not (equal? str ""))
       (ascii-letter? (string-ref str 0))
       (every-char? (drop-prefix str 1) scheme-char?)))

(define (leading-scheme str)
  (define split (split-once str "://"))
  (and (list? split) (scheme-name? (car split)) (string-downcase (car split))))

(define (all-digits? str)
  (and (not (equal? str "")) (every-char? str char-digit?)))

;; A scheme that is not followed by `//`, like `mailto:`. A colon followed by
;; only digits is a port, so `localhost:8080` is a host and not a scheme.
(define (opaque-scheme str)
  (define cut (index-of-any str (list #\: #\/ #\? #\#)))
  (and cut
       (char=? (string-ref str cut) #\:)
       (scheme-name? (substring str 0 cut))
       (let* ([rest (drop-prefix str (+ cut 1))]
              [end (or (index-of-any rest (list #\/ #\? #\#)) (string-length rest))])
         (not (all-digits? (substring rest 0 end))))
       (substring str 0 cut)))

(define (split-host-and-port authority)
  (cond
    [(starts-with? authority "[")
     (let ([close (index-of-any authority (list #\]))])
       (unless close
         (error "url: an ipv6 literal was never closed:" authority))
       (cons (substring authority 1 close)
             (port-after (drop-prefix authority (+ close 1)) authority)))]
    [else
     (let ([colon (last-index-of authority #\:)])
       (if colon
           (let ([host (substring authority 0 colon)])
             (when (string-contains? host #\:)
               (error "url: an unbracketed host cannot contain a colon:" authority))
             (cons host (port-after (drop-prefix authority colon) authority)))
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
;; Parses a url, leaving percent escapes as given. A url that is already parsed is
;; returned unchanged.
(define (parse-url input)
  (if (url? input)
      input
      (parse-url-string input)))

(define (parse-url-string str)
  (define scheme (leading-scheme str))
  (unless scheme
    (let ([opaque (opaque-scheme str)])
      (when opaque
        (error "url: this understands scheme://host urls, not the scheme:" opaque))))
  (define rest
    (if scheme
        (drop-prefix str (+ (string-length scheme) 3))
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

(define (url-authority url)
  (define host (bracketed-host (url-host url)))
  (if (or (not (url-port url)) (equal? (url-port url) (scheme-default-port (url-scheme url))))
      host
      (string-append host ":" (int->string (url-port url)))))

(define (url-origin url)
  (string-append (url-scheme url) "://" (url-authority url)))

(define (url-target url)
  (if (url-query url)
      (string-append (url-path url) "?" (url-query url))
      (url-path url)))

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

(define (remove-dot-segments path)
  (define absolute? (starts-with? path "/"))
  (define segments
    (split-many (if absolute?
                    (drop-prefix path 1)
                    path)
                "/"))
  ;; Empty segments are kept, so `/a//b` keeps both slashes. A trailing dot
  ;; segment is replaced with an empty segment, so `/a/.` becomes `/a/`.
  (define kept
    (let loop ([rest segments]
               [out '()])
      (cond
        [(empty? rest) (reverse out)]
        [(or (equal? (car rest) ".") (equal? (car rest) ".."))
         (let ([above (if (and (equal? (car rest) "..") (not (empty? out)))
                          (cdr out)
                          out)])
           (loop (cdr rest)
                 (if (empty? (cdr rest))
                     (cons "" above)
                     above)))]
        [else (loop (cdr rest) (cons (car rest) out))])))
  (string-append (if absolute? "/" "") (string-join kept "/")))

(define (merge-paths base reference)
  (define cut (last-index-of base #\/))
  (remove-dot-segments (string-append (if cut
                                          (substring base 0 (+ cut 1))
                                          "/")
                                      reference)))

(define (url-join base reference)
  (define from (parse-url base))
  (cond
    [(url? reference) reference]
    [(leading-scheme reference) (parse-url reference)]
    [(starts-with? reference "//") (parse-url (string-append (url-scheme from) ":" reference))]
    [(opaque-scheme reference)
     (error "url: a reference with a scheme has to be a scheme://host url:" reference)]
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

(define (url-encode str #:safe [safe ""])
  (define out (open-output-string))
  (define safe-bytes (filter (lambda (byte) (< byte 128)) (bytes->list (string->bytes safe))))
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
;; Decodes percent escapes. With `#:form`, a `+` decodes as a space, the way a
;; form encoded query writes one.
;;
;; (url-decode str [#:form form]) -> string?
;;
;; * str : string?
;; * form : bool? = #true
(define (url-decode str #:form [form #t])
  (define out (bytes))
  (define (push-char char)
    (for-each (lambda (byte) (bytes-push! out byte)) (bytes->list (string->bytes (string char)))))
  (let loop ([rest (string->list str)])
    (cond
      [(empty? rest) (bytes->string/utf8 out)]
      [(and form (char=? (car rest) #\+))
       (bytes-push! out 32)
       (loop (cdr rest))]
      [else
       (let* ([after (cdr rest)]
              [high (and (char=? (car rest) #\%) (pair? after) (hex-digit-value (car after)))]
              [low (and high (pair? (cdr after)) (hex-digit-value (car (cdr after))))])
         (cond
           [low
            (bytes-push! out (+ (* 16 high) low))
            (loop (cdr (cdr after)))]
           [else
            (push-char (car rest))
            (loop after)]))])))

(define (query-part->string value)
  (cond
    [(string? value) value]
    [(symbol? value) (symbol->string value)]
    [(number? value) (number->string value)]
    [(equal? value #t) "true"]
    [(equal? value #f) "false"]
    [else (error "url: a query value has to be a string, symbol, number or boolean, got:" value)]))

;;@doc
;; Encodes an alist as a query string, where a `#false` value means no `=`.
;;
;; # Examples
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

(define (url-decode-or-raw part)
  (with-handler (lambda (err) part) (url-decode part)))

(define (query->alist query)
  (if (not query)
      '()
      (map (lambda (part)
             (define split (split-once part "="))
             (if (list? split)
                 (cons (url-decode-or-raw (car split)) (url-decode-or-raw (list-ref split 1)))
                 (cons (url-decode-or-raw part) "")))
           (filter (lambda (part) (not (equal? part ""))) (split-many query "&")))))
