(require-builtin steel/tcp)
(require "url.scm")
(require "headers.scm")
(require "base64.scm")
(require "client.scm")
(require "message.scm")
(require "server.scm")
(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm"))

(define __http-module 'http-module)

(provide __http-module)

(define (fixture-connector replies requests)
  (lambda (url)
    (when (empty? (unbox replies))
      (error "fixture: the client opened more connections than there are replies"))
    (define reply (car (unbox replies)))
    (define writer (open-output-string))
    (set-box! replies (cdr (unbox replies)))
    (set-box! requests (append (unbox requests) (list (cons (url->string url) writer))))
    (make-transport (open-input-string reply) writer)))

(define (exchange replies thunk)
  (define requests (box '()))
  (define response
    (parameterize ([current-http-connector (fixture-connector (box replies) requests)])
      (thunk)))
  (cons response
        (map (lambda (request) (cons (car request) (get-output-string (cdr request))))
             (unbox requests))))

(define (reply-to replies thunk)
  (car (exchange replies thunk)))

(define (requests-of replies thunk)
  (cdr (exchange replies thunk)))

(define (request-text requests at)
  (cdr (list-ref requests at)))

(define (request-line text)
  (car (split-many text "\r\n")))

(define (socket-connector reply #:reader [reader tcp-stream-buffered-reader])
  (lambda (url)
    (define listener (tcp-listen "127.0.0.1:0"))
    (define client (tcp-connect (tcp-listener-local-addr listener)))
    (define server (tcp-accept listener))
    (display reply (tcp-stream-writer server))
    (make-transport (reader client)
                    (tcp-stream-writer client)
                    #:close (lambda ()
                              (with-handler (lambda (err) void) (tcp-shutdown! client))
                              (with-handler (lambda (err) void) (tcp-shutdown! server))
                              listener))))

(define (over-socket reply thunk #:reader [reader tcp-stream-buffered-reader])
  (parameterize ([current-http-connector (socket-connector reply #:reader reader)])
    (thunk)))

(define *ok* "HTTP/1.1 200 OK\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nhello world")

(define *chunked*
  (string-append "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nTrailer: X-Sum\r\n\r\n"
                 "5\r\nhello\r\n"
                 "6;note=x\r\n world\r\n"
                 "0\r\nX-Sum: 11\r\n\r\n"))

(define (redirect-to location)
  (string-append "HTTP/1.1 302 Found\r\nLocation: " location "\r\nContent-Length: 0\r\n\r\n"))

(test-module
 "http-url-tests"
 (check-equal? "parse-url fills in scheme, port and path for a bare host"
               (list "http" "example.com" 80 "/")
               (let ([url (parse-url "example.com")])
                 (list (url-scheme url) (url-host url) (url-port url) (url-path url))))
 (check-equal? "parse-url reads every part of a url"
               (list "https" "user:pw" "example.com" 8443 "/a/b" "x=1&y=2" "frag")
               (let ([url (parse-url "https://user:pw@example.com:8443/a/b?x=1&y=2#frag")])
                 (list (url-scheme url)
                       (url-userinfo url)
                       (url-host url)
                       (url-port url)
                       (url-path url)
                       (url-query url)
                       (url-fragment url))))
 (check-equal? "https default port" 443 (url-port (parse-url "https://example.com")))
 (check-equal? "ipv6 host without brackets, authority with brackets"
               (list "::1" "[::1]:8080")
               (let ([url (parse-url "http://[::1]:8080/x")])
                 (list (url-host url) (url-authority url))))
 (check-equal? "default port is left out of the authority"
               "example.com"
               (url-authority (parse-url "http://example.com:80/x")))
 (check-equal? "request target is path and query" "/a?b=1" (url-target (parse-url "http://x/a?b=1")))
 (check-equal? "origin is scheme and authority"
               "http://x:81"
               (url-origin (parse-url "http://x:81/a")))
 (check-equal? "url->string round trip"
               "http://a:81/b/c?d#e"
               (url->string (parse-url "http://a:81/b/c?d#e")))
 (check-equal? "parse-url on a parsed url"
               "http://a/b"
               (url->string (parse-url (parse-url "http://a/b"))))
 (check-err? "url with no host errors" (parse-url "http:///just-a-path") 'error)
 (check-err? "non-numeric port errors" (parse-url "http://a:http/") 'error)
 (check-equal? "url-encode escapes reserved characters"
               "a%20b%2Fc%3Fd%3D%C3%A9"
               (url-encode "a b/c?d=é"))
 (check-equal? "url-encode keeps slashes in a path" "a%20b/c" (url-encode "a b/c" #:safe "/"))
 (check-equal? "url-decode handles escapes and form plus" "a b/c dé" (url-decode "a%20b%2Fc+d%C3%A9"))
 (check-equal? "url-decode leaves an invalid percent alone" "100% %zz" (url-decode "100%+%zz"))
 (check-equal? "encode query from association list"
               "q=a%20b&page=2&flag"
               (alist->query (list (cons "q" "a b") (cons "page" 2) (cons "flag" #f))))
 (check-equal? "decode query to association list"
               (list (cons "q" "a b") (cons "page" "2") (cons "flag" ""))
               (query->alist "q=a%20b&page=2&flag"))
 (check-equal? "empty query decodes to no pairs" '() (query->alist #f))
 (check-equal? "invalid utf8 escapes are left encoded" (list (cons "a" "%FF")) (query->alist "a=%FF"))
 (check-equal? "resolve absolute url"
               "http://z/q"
               (url->string (url-join "http://a/b/c" "http://z/q")))
 (check-equal? "resolve absolute path" "http://a/d" (url->string (url-join "http://a/b/c?x=1" "/d")))
 (check-equal? "resolve relative path" "http://a/b/d/e" (url->string (url-join "http://a/b/c" "d/e")))
 (check-equal? "resolve dot segments"
               "http://a/x"
               (url->string (url-join "http://a/b/c/d" "../../x")))
 (check-equal? "resolve keeps empty path segments"
               (list "http://a/proxy/http://example.com/x" "http://a/a//b" "http://a/b/g//")
               (map (lambda (reference) (url->string (url-join "http://a/b/c" reference)))
                    (list "/proxy/http://example.com/x" "/a//b" "g//h/..")))
 (check-equal? "resolve scheme relative reference"
               "https://z/q"
               (url->string (url-join "https://a/b" "//z/q")))
 (check-equal? "resolve fragment only reference"
               "http://a/b?x=1#top"
               (url->string (url-join "http://a/b?x=1" "#top")))
 (check-equal? "resolve query only reference"
               "http://a/b?y=2"
               (url->string (url-join "http://a/b?x=1" "?y=2")))
 (check-equal? "resolve relative reference with a url in its query"
               "http://a/login?next=https://b/x"
               (url->string (url-join "http://a/b/c" "/login?next=https://b/x")))
 (check-equal? "url-encode escapes non-ascii characters" "%C3%A9" (url-encode "é" #:safe "é"))
 (check-equal? "url-decode keeps plus when not form encoded" "a+b c" (url-decode "a+b%20c" #:form #f))
 (check-equal? "resolve empty reference drops the fragment"
               "http://a/b?x=1"
               (url->string (url-join "http://a/b?x=1#top" "")))
 (check-equal? "resolve reference with colon before a port"
               (list "localhost" 8080)
               (let ([url (parse-url "localhost:8080/x")]) (list (url-host url) (url-port url))))
 (check-err? "resolve reference with unsupported scheme errors"
             (url-join "http://a/b" "mailto:someone@example.com")
             'error)
 (check-err? "url with unsupported scheme errors" (parse-url "mailto:someone@example.com") 'error)
 (check-err? "host with colon outside brackets errors" (parse-url "http://a:1:2/") 'error))

(test-module
 "http-headers-tests"
 (check-equal? "headers from pairs or two element lists"
               (list (cons "A" "1") (cons "B" "2"))
               (normalize-headers (list (cons "A" 1) (list "B" "2"))))
 (check-equal? "header lookup is case insensitive"
               "text/plain"
               (header-ref (normalize-headers (list (cons "Content-Type" "text/plain")))
                           "content-type"))
 (check-equal? "header lookup with default" "none" (header-ref '() "Content-Type" "none"))
 (check-equal? "repeated header keeps every value"
               (list "a" "b")
               (header-ref* (header-add (header-add '() "Set-Cookie" "a") "Set-Cookie" "b")
                            "set-cookie"))
 (check-equal? "header-set replaces the old value"
               (list "new")
               (header-ref* (header-set (header-add '() "A" "old") "a" "new") "A"))
 (check-equal? "header-default does not overwrite"
               "mine"
               (header-ref (header-default (header-add '() "A" "mine") "a" "theirs") "A"))
 (check-equal? "header-remove removes every value"
               '()
               (header-ref* (header-remove (header-add '() "A" "1") "a") "A"))
 (check-equal? "headers written as crlf lines"
               "A: 1\r\nB: 2\r\n"
               (headers->string (list (cons "A" "1") (cons "B" "2"))))
 (check-equal? "header line splits on first colon"
               (cons "Location" "http://x/y")
               (parse-header-line "Location: http://x/y"))
 (check-equal? "header line without colon" #f (parse-header-line "nocolon"))
 (check-err? "newline in header value errors"
             (normalize-headers (list (cons "X" "a\r\nY: b")))
             'error)
 (check-err? "colon in header name errors" (normalize-headers (list (cons "X:Y" "a"))) 'error)
 (check-err? "space in header name errors" (normalize-headers (list (cons "X Y" "a"))) 'error)
 (check-equal? "token punctuation in header name"
               "X-Not_A.Token!: 1\r\n"
               (headers->string (header-add '() "X-Not_A.Token!" 1))))

(test-module
 "http-base64-tests"
 (check-equal? "base64 encode empty string" "" (base64-encode ""))
 (check-equal? "base64 encode one byte" "Zg==" (base64-encode "f"))
 (check-equal? "base64 encode two bytes" "Zm8=" (base64-encode "fo"))
 (check-equal? "base64 encode three bytes" "Zm9v" (base64-encode "foo"))
 (check-equal? "base64 encode credentials" "dXNlcjpwYXNzd29yZA==" (base64-encode "user:password"))
 (check-equal? "base64 round trip"
               (bytes 0 255 128)
               (base64-decode (base64-encode (bytes 0 255 128))))
 (check-equal? "base64 decode ignores whitespace" (bytes 102 111 111) (base64-decode "Zm\n9v"))
 (check-err? "base64 decode invalid digit errors" (base64-decode "a*b") 'error)
 (check-err? "base64 decode digit after padding errors" (base64-decode "QQ=A") 'error)
 (check-equal? "base64 round trip, many groups"
               #t
               (let ([long (apply string-append (map (lambda (n) "0123456789") (range 0 30)))])
                 (equal? long (bytes->string/utf8 (base64-decode (base64-encode long)))))))

(test-module
 "http-response-tests"
 (check-equal?
  "status line parts"
  (list "HTTP/1.1" 200 "OK")
  (let ([response (reply-to (list *ok*) (lambda () (http-get "http://x/")))])
    (list (response-version response) (response-status response) (response-reason response))))
 (check-equal? "response body with Content-Length"
               "hello world"
               (response-text (reply-to (list *ok*) (lambda () (http-get "http://x/")))))
 (check-equal? "response-ok? on 2xx"
               #t
               (response-ok? (reply-to (list *ok*) (lambda () (http-get "http://x/")))))
 (check-equal? "response-ok? on 404"
               #f
               (response-ok? (reply-to (list "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n")
                                       (lambda () (http-get "http://x/")))))
 (check-equal? "response header lookup is case insensitive"
               "text/plain"
               (response-header (reply-to (list *ok*) (lambda () (http-get "http://x/")))
                                "content-type"))
 (check-equal?
  "status code must be three digits"
  (list #t #t)
  (map (lambda (code)
         (with-handler
          (lambda (err) #t)
          (begin
            (http-read-response
             (open-input-string (string-append "HTTP/1.1 " code " OK\r\nContent-Length: 0\r\n\r\n")))
            #f)))
       (list "#xc8" "+200")))
 (check-equal? "response without reason phrase"
               (list 200 "")
               (let ([response (reply-to (list "HTTP/1.1 200\r\nContent-Length: 0\r\n\r\n")
                                         (lambda () (http-get "http://x/")))])
                 (list (response-status response) (response-reason response))))
 (check-equal? "chunked response body"
               "hello world"
               (response-text (reply-to (list *chunked*) (lambda () (http-get "http://x/")))))
 (check-equal? "chunked trailers are added to headers"
               "11"
               (response-header (reply-to (list *chunked*) (lambda () (http-get "http://x/")))
                                "x-sum"))
 (check-equal?
  "trailers do not override head fields"
  (list #f #f "t")
  (let ([response
         (reply-to
          (list (string-append "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                               "0\r\nLocation: http://y/\r\nSet-Cookie: a=b\r\nX-Trailer: t\r\n\r\n"))
          (lambda () (http-get "http://x/")))])
    (list (response-header response "Location")
          (response-header response "Set-Cookie")
          (response-header response "X-Trailer"))))
 (check-equal? "response body without length reads to eof"
               "no length here"
               (response-text (reply-to (list "HTTP/1.1 200 OK\r\n\r\nno length here")
                                        (lambda () (http-get "http://x/")))))
 (check-equal? "HEAD response keeps Content-Length, no body"
               (list "99" 0)
               (let ([response (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 99\r\n\r\n")
                                         (lambda () (http-head "http://x/")))])
                 (list (response-header response "content-length")
                       (bytes-length (response-body response)))))
 (check-equal? "204 response has no body"
               (list 204 0)
               (let ([response (reply-to (list "HTTP/1.1 204 No Content\r\n\r\n")
                                         (lambda () (http-get "http://x/")))])
                 (list (response-status response) (bytes-length (response-body response)))))
 (check-equal?
  "304 response has no body"
  0
  (bytes-length (response-body
                 (reply-to (list "HTTP/1.1 304 Not Modified\r\nContent-Length: 7\r\n\r\n")
                           (lambda () (http-get "http://x/"))))))
 (check-equal?
  "folded header line"
  "one two"
  (response-header
   (reply-to (list "HTTP/1.1 200 OK\r\nX-Long: one\r\n  two\r\nContent-Length: 0\r\n\r\n")
             (lambda () (http-get "http://x/")))
   "x-long"))
 (check-equal? "repeated matching Content-Length"
               "ab"
               (response-text (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 2, 2\r\n\r\nab")
                                        (lambda () (http-get "http://x/")))))
 (check-equal? "http/1.1 response defaults to keep-alive"
               #t
               (response-keep-alive? (reply-to (list *ok*) (lambda () (http-get "http://x/")))))
 (check-equal? "response with Connection: close"
               #f
               (response-keep-alive?
                (reply-to (list "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 0\r\n\r\n")
                          (lambda () (http-get "http://x/")))))
 (check-equal? "Connection: close in a second header line"
               #f
               (response-keep-alive? (reply-to (list (string-append "HTTP/1.1 200 OK\r\n"
                                                                    "Connection: keep-alive\r\n"
                                                                    "Connection: close\r\n"
                                                                    "Content-Length: 0\r\n\r\n"))
                                               (lambda () (http-get "http://x/")))))
 (check-err? "conflicting Content-Length errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Length: 3\r\n\r\nab")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err?
  "chunked with Content-Length errors"
  (reply-to
   (list "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n")
   (lambda () (http-get "http://x/")))
  'error)
 (check-err? "unknown Transfer-Encoding errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\nxx")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "chunked plus another Transfer-Encoding errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip, chunked\r\n\r\n0\r\n\r\n")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "Transfer-Encoding split over two header lines"
             (reply-to (list (string-append "HTTP/1.1 200 OK\r\n"
                                            "Transfer-Encoding: chunked\r\n"
                                            "Transfer-Encoding: gzip\r\n\r\n"
                                            "5\r\nhello\r\n0\r\n\r\n"))
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "truncated response body errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\nshort")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "invalid status line errors"
             (reply-to (list "not http at all\r\n\r\n") (lambda () (http-get "http://x/")))
             'error)
 (check-err? "unterminated head errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nA: b") (lambda () (http-get "http://x/")))
             'error)
 (check-err? "non-numeric chunk size errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err?
  "signed chunk size errors"
  (reply-to (list "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n+5\r\nhello\r\n0\r\n\r\n")
            (lambda () (http-get "http://x/")))
  'error)
 (check-err? "signed Content-Length errors"
             (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: +2\r\n\r\nab")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "unterminated header line errors"
             (reply-to (list (string-append "HTTP/1.1 200 OK\r\nX: "
                                            (apply string-append
                                                   (map (lambda (n) "0123456789")
                                                        (range 0 (quotient *max-line-length* 5))))))
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "unsupported http version errors"
             (reply-to (list "HTTP/2 200 OK\r\nContent-Length: 0\r\n\r\n")
                       (lambda () (http-get "http://x/")))
             'error)
 (check-equal?
  "large response body"
  (list 70000 #t)
  (let* ([body (apply string-append (map (lambda (n) "0123456789") (range 0 7000)))]
         [text (response-text (reply-to (list (string-append "HTTP/1.1 200 OK\r\nContent-Length: "
                                                             (int->string (string-length body))
                                                             "\r\n\r\n"
                                                             body))
                                        (lambda () (http-get "http://x/"))))])
    (list (string-length text) (equal? text body))))
 (check-err? "too many header lines errors"
             (reply-to (list (string-append "HTTP/1.1 200 OK\r\n"
                                            (headers->string (map (lambda (n)
                                                                    (cons "X" (int->string n)))
                                                                  (range 0 (+ *max-header-lines* 1))))
                                            "\r\n"))
                       (lambda () (http-get "http://x/")))
             'error)
 (check-err? "too many interim responses errors"
             (reply-to (list (apply string-append
                                    (map (lambda (n) "HTTP/1.1 100 Continue\r\n\r\n") (range 0 20))))
                       (lambda () (http-get "http://x/")))
             'error))

(test-module
 "http-request-tests"
 (check-equal?
  "request line has method, target and version"
  "GET /a?b=1 HTTP/1.1"
  (request-line
   (request-text (requests-of (list *ok*) (lambda () (http-get "http://example.com/a?b=1"))) 0)))
 (check-equal? "Host header from url"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://example.com/a")))
                              0)
                "Host: example.com\r\n"))
 (check-equal?
  "Host header keeps a non default port"
  #t
  (string-contains?
   (request-text (requests-of (list *ok*) (lambda () (http-get "http://example.com:8080/a"))) 0)
   "Host: example.com:8080\r\n"))
 (check-equal? "default User-Agent header"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://x/"))) 0)
                "User-Agent: steel-http/0.1.0\r\n"))
 (check-equal? "caller header overrides default"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*)
                                           (lambda ()
                                             (http-get "http://x/"
                                                       #:headers (list (cons "User-Agent" "mine")))))
                              0)
                "User-Agent: mine\r\n"))
 (check-equal?
  "body sets Content-Length"
  #t
  (string-contains?
   (request-text (requests-of (list *ok*) (lambda () (http-post "http://x/" #:body "payload"))) 0)
   "Content-Length: 7\r\n"))
 (check-equal? "post without body sends Content-Length: 0"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-post "http://x/"))) 0)
                "Content-Length: 0\r\n"))
 (check-equal?
  "body follows the head"
  #t
  (ends-with?
   (request-text (requests-of (list *ok*) (lambda () (http-post "http://x/" #:body "payload"))) 0)
   "\r\n\r\npayload"))
 (check-equal? "form body is url encoded with Content-Type"
               #t
               (let ([text (request-text (requests-of (list *ok*)
                                                      (lambda ()
                                                        (http-post "http://x/"
                                                                   #:form (list (cons "q" "a b")
                                                                                (cons "n" 2)))))
                                         0)])
                 (and (string-contains? text "Content-Type: application/x-www-form-urlencoded\r\n")
                      (ends-with? text "\r\n\r\nq=a%20b&n=2"))))
 (check-equal? "query keyword replaces url query"
               "GET /p?a=b%20c HTTP/1.1"
               (request-line (request-text (requests-of (list *ok*)
                                                        (lambda ()
                                                          (http-get "http://x/p?old=1"
                                                                    #:query (list (cons "a" "b c")))))
                                           0)))
 (check-equal? "basic auth from credentials"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*)
                                           (lambda ()
                                             (http-get "http://x/" #:auth (cons "user" "password"))))
                              0)
                "Authorization: Basic dXNlcjpwYXNzd29yZA==\r\n"))
 (check-equal? "basic auth from url credentials"
               #t
               (let ([text (request-text (requests-of (list *ok*)
                                                      (lambda ()
                                                        (http-get "http://user:password@x/p")))
                                         0)])
                 (and (string-contains? text "Authorization: Basic dXNlcjpwYXNzd29yZA==\r\n")
                      (equal? (request-line text) "GET /p HTTP/1.1"))))
 (check-equal? "plus in url credentials is not decoded"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://user:pa+ss@x/p")))
                              0)
                "Authorization: Basic dXNlcjpwYStzcw==\r\n"))
 (check-equal? "default Accept-Encoding"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://x/"))) 0)
                "Accept-Encoding: identity\r\n"))
 (check-equal? "one shot request sends Connection: close"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://x/"))) 0)
                "Connection: close\r\n"))
 (check-err? "body and form together errors"
             (make-request "POST" "http://x/" #:body "a" #:form (list (cons "b" "c")))
             'error)
 (check-err? "body that is not a string or bytes errors"
             (make-request "POST" "http://x/" #:body 42)
             'error))

(test-module
 "http-redirect-tests"
 (check-equal? "redirect with relative location"
               (list "http://a/first" "http://a/next")
               (map car
                    (requests-of (list (redirect-to "/next")
                                       "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
                                 (lambda () (http-get "http://a/first")))))
 (check-equal? "redirect returns the last response body"
               "ok"
               (response-text (reply-to (list (redirect-to "/next")
                                              "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
                                        (lambda () (http-get "http://a/first")))))
 (check-equal?
  "302 changes post to get without body"
  "GET /next HTTP/1.1"
  (request-line (request-text (requests-of (list (redirect-to "/next")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda () (http-post "http://a/first" #:body "data")))
                              1)))
 (check-equal?
  "307 repeats the request"
  #t
  (let ([text (request-text
               (requests-of (list (string-append "HTTP/1.1 307 Temporary Redirect\r\n"
                                                 "Location: /again\r\nContent-Length: 0\r\n\r\n")
                                  "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                            (lambda () (http-post "http://a/first" #:body "data")))
               1)])
    (and (equal? (request-line text) "POST /again HTTP/1.1") (ends-with? text "\r\n\r\ndata"))))
 (check-equal?
  "redirect keeps HEAD method"
  "HEAD /next HTTP/1.1"
  (request-line (request-text (requests-of (list (redirect-to "/next")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda () (http-head "http://a/first")))
                              1)))
 (check-equal? "credentials kept on same origin redirect"
               #t
               (string-contains?
                (request-text (requests-of (list (redirect-to "/next")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda ()
                                             (http-get "http://a/first" #:auth (cons "u" "p"))))
                              1)
                "Authorization: "))
 (check-equal? "credentials dropped on cross origin redirect"
               #f
               (string-contains?
                (request-text (requests-of (list (redirect-to "http://b/there")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda ()
                                             (http-get "http://a/first" #:auth (cons "u" "p"))))
                              1)
                "Authorization: "))
 (check-equal? "Host header updated on cross origin redirect"
               #t
               (string-contains?
                (request-text (requests-of (list (redirect-to "http://b/there")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda () (http-get "http://a/first")))
                              1)
                "Host: b\r\n"))
 (check-equal? "follow disabled returns the 3xx"
               (list 302 "/next")
               (let ([response (reply-to (list (redirect-to "/next"))
                                         (lambda () (http-get "http://a/" #:follow 0)))])
                 (list (response-status response) (response-header response "location"))))
 (check-equal? "3xx without location is returned"
               301
               (response-status (reply-to (list "HTTP/1.1 301 Moved\r\nContent-Length: 0\r\n\r\n")
                                          (lambda () (http-get "http://a/")))))
 (check-err? "too many redirects errors"
             (reply-to (list (redirect-to "/1") (redirect-to "/2"))
                       (lambda () (http-get "http://a/" #:follow 1)))
             'error))

(test-module
 "http-socket-tests"
 (check-equal? "get over tcp"
               "hello world"
               (response-text (over-socket *ok* (lambda () (http-get "http://127.0.0.1/a")))))
 (check-equal? "chunked body over tcp"
               "hello world"
               (response-text (over-socket *chunked* (lambda () (http-get "http://127.0.0.1/a")))))
 (check-equal? "post over tcp"
               200
               (response-status
                (over-socket *ok* (lambda () (http-post "http://127.0.0.1/a" #:body "payload")))))
 (check-equal? "HEAD over tcp"
               0
               (bytes-length (response-body
                              (over-socket *ok* (lambda () (http-head "http://127.0.0.1/a"))))))
 (check-equal? "large body over tcp"
               (list 20000 #t)
               (let* ([body (apply string-append (map (lambda (each) "0123456789") (range 0 2000)))]
                      [response (over-socket (string-append "HTTP/1.1 200 OK\r\nContent-Length: "
                                                            (int->string (string-length body))
                                                            "\r\n\r\n"
                                                            body)
                                             (lambda () (http-get "http://127.0.0.1/big")))])
                 (list (bytes-length (response-body response))
                       (equal? (response-text response) body))))
 (check-equal? "response read from unbuffered reader"
               "hello world"
               (response-text (over-socket *ok*
                                           (lambda () (http-get "http://127.0.0.1/a"))
                                           #:reader tcp-stream-reader)))
 (check-equal? "body without length over tcp"
               "until the end"
               (let* ([listener (tcp-listen "127.0.0.1:0")]
                      [client (tcp-connect (tcp-listener-local-addr listener))]
                      [server (tcp-accept listener)])
                 (display "HTTP/1.1 200 OK\r\n\r\nuntil the end" (tcp-stream-writer server))
                 (tcp-shutdown! server)
                 (response-text (http-read-response (tcp-stream-buffered-reader client)))))
 (check-equal?
  "response with a connector read timeout"
  "hello world"
  (let ([listener (tcp-listen "127.0.0.1:0")]
        [far-end (box #f)])
    (parameterize ([current-http-connector (lambda (url)
                                             (define sent (tcp-connector url #:read-timeout 10000))
                                             (set-box! far-end (tcp-accept listener))
                                             (display *ok* (tcp-stream-writer (unbox far-end)))
                                             sent)])
      (response-text (http-get (string-append "http://" (tcp-listener-local-addr listener) "/a"))))))
 (check-equal? "connector read timeout"
               #t
               (let* ([listener (tcp-listen "127.0.0.1:0")]
                      [target (string-append "http://" (tcp-listener-local-addr listener) "/a")]
                      [sent (tcp-connector (parse-url target) #:read-timeout 50)]
                      [far-end (tcp-accept listener)]
                      [outcome (with-handler (lambda (err)
                                               (string-contains? (error-object-message err)
                                                                 "would block"))
                                             (begin
                                               (http-read-response (transport-reader sent))
                                               'returned))])
                 (if far-end outcome outcome)))
 (check-equal?
  "two requests on one connection over tcp"
  (list "one" "two")
  (let ()
    (define listener (tcp-listen "127.0.0.1:0"))
    (define client (tcp-connect (tcp-listener-local-addr listener)))
    (define server (tcp-accept listener))
    (display (string-append "HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\none"
                            "HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\ntwo")
             (tcp-stream-writer server))
    (define connection
      (parameterize ([current-http-connector (lambda (url)
                                               (make-transport (tcp-stream-buffered-reader client)
                                                               (tcp-stream-writer client)
                                                               #:close
                                                               (lambda ()
                                                                 (with-handler (lambda (err) void)
                                                                               (tcp-shutdown! server))
                                                                 listener)))])
        (http-connect "http://127.0.0.1/")))
    (list (response-text
           (http-exchange connection (make-request "GET" "http://127.0.0.1/one") #:keep-alive #t))
          (response-text
           (http-exchange connection (make-request "GET" "http://127.0.0.1/two") #:keep-alive #t))))))

(test-module
 "http-framing-tests"
 (check-err? "newline in path errors" (parse-url "http://x/a\r\nX-Evil: 1") 'error)
 (check-err? "newline in query errors" (parse-url "http://x/a?b=1\nX: 2") 'error)
 (check-err? "space in host errors" (parse-url "http://ev il/a") 'error)
 (check-err? "space in path errors" (parse-url "http://x/a b") 'error)
 (check-equal? "escaped space in path" "/a%20b" (url-target (parse-url "http://x/a%20b")))
 (check-err? "method containing a request line errors"
             (http-send "GET /evil HTTP/1.1\r\nX" "http://x/")
             'error)
 (check-equal? "method as symbol"
               "DELETE /x HTTP/1.1"
               (request-line (request-text (requests-of (list *ok*)
                                                        (lambda () (http-send 'delete "http://x/x")))
                                           0)))
 (check-err? "request with invalid framing headers errors"
             (reply-to (list *ok*)
                       (lambda ()
                         (http-post "http://x/"
                                    #:body "a"
                                    #:headers (list (cons "Transfer-Encoding" "chunked")))))
             'error)
 (check-err? "negative redirect count errors" (http-get "http://x/" #:follow -1) 'error)
 (check-err? "method that is not a token errors"
             (make-request (string-append "GET" (string (integer->char 127))) "http://x/")
             'error)
 (check-equal?
  "100 Continue is skipped"
  (list 200 "ok")
  (let ([response (reply-to (list (string-append "HTTP/1.1 100 Continue\r\n\r\n"
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"))
                            (lambda () (http-get "http://x/")))])
    (list (response-status response) (response-text response))))
 (check-equal? "103 early hints are skipped"
               200
               (response-status
                (reply-to (list (string-append "HTTP/1.1 103 Early Hints\r\nLink: </s.css>\r\n\r\n"
                                               "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"))
                          (lambda () (http-get "http://x/")))))
 (check-equal? "101 is returned as the response"
               101
               (response-status
                (reply-to (list "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n\r\n")
                          (lambda () (http-get "http://x/"))))))

(define (over-ports text handler #:keep-alive [keep-alive #t] #:limit [limit #f])
  (define writer (open-output-string))
  (define served
    (http-serve-connection (open-input-string text)
                           writer
                           handler
                           #:keep-alive keep-alive
                           #:limit limit))
  (cons served (get-output-string writer)))

(define (answer text handler)
  (cdr (over-ports text handler)))

(define (answer-status text handler)
  (car (split-many (answer text handler) "\r\n")))

(define (hello sent)
  (make-response 200 #:body (string-append "hello " (request-target sent))))

(define *get* "GET /a HTTP/1.1\r\nHost: x\r\n\r\n")

(define (round-trip request handler)
  (define server (http-listen "127.0.0.1:0"))
  (define connection (http-connect (string-append "http://" (server-address server) "/")))
  (http-write-request! connection request)
  (http-serve-one server handler #:limit 1)
  (http-read-response (transport-reader (connection-transport connection))
                      #:method (request-method request)))

(test-module
 "http-server-request-tests"
 (check-equal?
  "request has method, target and version"
  (list "GET" "/a?b=1" "HTTP/1.1")
  (let ([sent (http-read-request (open-input-string "GET /a?b=1 HTTP/1.1\r\nHost: x\r\n\r\n"))])
    (list (request-method sent) (request-target sent) (request-version sent))))
 (check-equal?
  "request header lookup"
  "text/plain"
  (request-header
   (http-read-request (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\nAccept: text/plain\r\n\r\n"))
   "Accept"))
 (check-equal? "request url host comes from Host header"
               "x"
               (url-host (request-url (http-read-request (open-input-string *get*)))))
 (check-equal? "request without body has #false body"
               #f
               (request-body (http-read-request (open-input-string *get*))))
 (check-equal? "request body with Content-Length"
               "hello"
               (bytes->string/utf8
                (request-body (http-read-request
                               (open-input-string
                                "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\nhello")))))
 (check-equal?
  "chunked request body with trailers"
  (list "hello world" "t")
  (let ([sent (http-read-request
               (open-input-string
                (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                               "Transfer-Encoding: chunked\r\n\r\n"
                               "5\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n")))])
    (list (bytes->string/utf8 (request-body sent)) (request-header sent "X-Trailer"))))
 (check-equal? "chunked plus another Transfer-Encoding is a 501"
               "HTTP/1.1 501 Not Implemented"
               (answer-status (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                             "Transfer-Encoding: gzip, chunked\r\n\r\n"
                                             "0\r\n\r\n")
                              hello))
 (check-equal? "request Transfer-Encoding split over two header lines"
               "HTTP/1.1 501 Not Implemented"
               (answer-status (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                             "Transfer-Encoding: chunked\r\n"
                                             "Transfer-Encoding: gzip\r\n\r\n"
                                             "0\r\n\r\n")
                              hello))
 (check-equal? "eof between requests is not an error"
               #t
               (eof-object? (http-read-request (open-input-string ""))))
 (check-equal? "absolute request target keeps its own host"
               "other"
               (url-host (request-url (http-read-request
                                       (open-input-string
                                        "GET http://other/a HTTP/1.1\r\nHost: x\r\n\r\n")))))
 (check-equal? "OPTIONS * request target"
               "*"
               (request-target (http-read-request
                                (open-input-string "OPTIONS * HTTP/1.1\r\nHost: x\r\n\r\n"))))
 (check-equal? "HTTP/1.0 request without Host"
               (list "/a" "HTTP/1.0")
               (let ([sent (http-read-request (open-input-string "GET /a HTTP/1.0\r\n\r\n"))])
                 (list (request-target sent) (request-version sent))))
 (check-equal?
  "folded request header line"
  "one two"
  (request-header
   (http-read-request (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\nX-Long: one\r\n two\r\n\r\n"))
   "X-Long"))
 (check-err? "unreadable request errors"
             (http-read-request (open-input-string "nonsense\r\n\r\n"))
             'error)
 (check-equal? "leading crlf before request line is skipped"
               "/a"
               (request-target (http-read-request (open-input-string (string-append "\r\n" *get*)))))
 (check-equal?
  "Expect: 100-continue is answered before the body"
  "HTTP/1.1 100 Continue\r\n\r\n"
  (let ([writer (open-output-string)])
    (http-read-request
     (open-input-string (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                       "Expect: 100-continue\r\nContent-Length: 2\r\n\r\nhi"))
     #:continue writer)
    (get-output-string writer)))
 (check-equal? "keep-alive default for 1.1 and 1.0 requests"
               (list #t #f)
               (list (request-keep-alive? (http-read-request (open-input-string *get*)))
                     (request-keep-alive?
                      (http-read-request (open-input-string "GET /a HTTP/1.0\r\n\r\n")))))
 (check-equal?
  "Connection header overrides the keep-alive default"
  (list #t #f)
  (list (request-keep-alive?
         (http-read-request (open-input-string "GET /a HTTP/1.0\r\nConnection: keep-alive\r\n\r\n")))
        (request-keep-alive?
         (http-read-request
          (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")))))
 (check-equal? "Connection split over two header lines"
               #f
               (request-keep-alive?
                (http-read-request (open-input-string (string-append "GET /a HTTP/1.1\r\nHost: x\r\n"
                                                                     "Connection: keep-alive\r\n"
                                                                     "Connection: close\r\n\r\n"))))))

(test-module
 "http-server-answer-tests"
 (check-equal? "response has status line, Content-Length and body"
               (string-append "HTTP/1.1 200 OK\r\n"
                              "Server: steel-http/0.1.0\r\n"
                              "Connection: keep-alive\r\n"
                              "Content-Length: 8\r\n"
                              "\r\n"
                              "hello /a")
               (answer *get* hello))
 (check-equal? "default reason phrase"
               "HTTP/1.1 404 Not Found"
               (answer-status *get* (lambda (sent) (make-response 404))))
 (check-equal? "newline in reason phrase is a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) (make-response 200 #:reason "OK\r\nX: y"))))
 (check-equal? "custom reason phrase"
               "HTTP/1.1 404 Nothing Here"
               (answer-status *get* (lambda (sent) (make-response 404 #:reason "Nothing Here"))))
 (check-equal? "status code without a known reason phrase"
               "HTTP/1.1 599"
               (answer-status *get* (lambda (sent) (make-response 599))))
 (check-equal? "handler headers are sent"
               #t
               (string-contains?
                (answer *get*
                        (lambda (sent) (make-response 200 #:headers (list (cons "X-Mine" "yes")))))
                "X-Mine: yes"))
 (check-equal? "handler overrides Server header"
               #t
               (string-contains?
                (answer *get*
                        (lambda (sent) (make-response 200 #:headers (list (cons "Server" "mine")))))
                "Server: mine"))
 (check-equal? "HEAD response has Content-Length, no body"
               (string-append "HTTP/1.1 200 OK\r\n"
                              "Server: steel-http/0.1.0\r\n"
                              "Connection: keep-alive\r\n"
                              "Content-Length: 5\r\n"
                              "\r\n")
               (answer "HEAD /a HTTP/1.1\r\nHost: x\r\n\r\n"
                       (lambda (sent) (make-response 200 #:body "hello"))))
 (check-equal? "204 response has no body or Content-Length"
               (string-append "HTTP/1.1 204 No Content\r\n"
                              "Server: steel-http/0.1.0\r\n"
                              "Connection: keep-alive\r\n"
                              "\r\n")
               (answer *get* (lambda (sent) (make-response 204 #:body "ignored"))))
 (check-equal? "304 response has no Content-Length"
               #f
               (string-contains? (answer *get* (lambda (sent) (make-response 304))) "Content-Length"))
 (check-equal? "response with Transfer-Encoding is a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get*
                              (lambda (sent)
                                (make-response 200
                                               #:headers (list (cons "Transfer-Encoding" "chunked"))
                                               #:body "x"))))
 (check-err? "http-write-response! with Transfer-Encoding errors"
             (http-write-response!
              (open-output-string)
              (make-response 200 #:headers (list (cons "Transfer-Encoding" "chunked")) #:body "x"))
             'error)
 (check-err? "invalid status code errors" (make-response 99) 'error)
 (check-equal?
  "bytevector response body"
  "\r\n\r\nhello"
  (let ([text (answer *get* (lambda (sent) (make-response 200 #:body (string->bytes "hello"))))])
    (substring text (- (string-length text) 9) (string-length text)))))

(test-module
 "http-server-handler-tests"
 (check-equal? "handler error is a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) (error "a handler blew up"))))
 (check-equal? "handler error message is not sent"
               #f
               (string-contains? (answer *get* (lambda (sent) (error "/etc/passwd is right here")))
                                 "passwd"))
 (check-equal? "non-response handler result is a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) 'not-a-response)))
 (check-equal? "handler called once per request"
               2
               (let ([calls (box 0)])
                 (over-ports (string-append *get* *get*)
                             (lambda (sent)
                               (set-box! calls (+ (unbox calls) 1))
                               (make-response 200)))
                 (unbox calls)))
 (check-equal? "write failure ends the connection"
               0
               (let ([writer (open-output-string)])
                 (close-output-port writer)
                 (http-serve-connection (open-input-string *get*) writer hello)))
 (check-equal? "response with #false body is a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) (response "HTTP/1.1" 200 "OK" '() #f)))))

(test-module
 "http-server-framing-tests"
 (check-equal? "invalid request line is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "nonsense\r\n\r\n" hello))
 (check-equal? "request line with extra space is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a b HTTP/1.1\r\nHost: x\r\n\r\n" hello))
 (check-equal? "unsupported http version is a 505"
               "HTTP/1.1 505 HTTP Version Not Supported"
               (answer-status "GET /a HTTP/2.0\r\nHost: x\r\n\r\n" hello))
 (check-equal? "HTTP/1.1 request without Host is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\n\r\n" hello))
 (check-equal? "two Host headers is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\nHost: x\r\nHost: y\r\n\r\n" hello))
 (check-equal? "chunked with Content-Length is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status
                (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                               "Transfer-Encoding: chunked\r\nContent-Length: 5\r\n\r\n0\r\n\r\n")
                hello))
 (check-equal? "conflicting Content-Length is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                             "Content-Length: 5\r\nContent-Length: 6\r\n\r\nhello")
                              hello))
 (check-equal? "unsupported Transfer-Encoding is a 501"
               "HTTP/1.1 501 Not Implemented"
               (answer-status "POST /a HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: gzip\r\n\r\n" hello))
 (check-equal? "Content-Length over the limit is a 413"
               "HTTP/1.1 413 Content Too Large"
               (answer-status (string-append "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: "
                                             (int->string (+ *max-body-bytes* 1))
                                             "\r\n\r\n")
                              hello))
 (check-equal? "chunked body over the limit stops reading"
               'too-large
               (read-chunked (open-input-string "4\r\nabcd\r\n0\r\n\r\n") #:limit 2))
 (check-equal? "header line over the limit is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status (string-append "GET /a HTTP/1.1\r\nHost: x\r\nX: "
                                             (apply string-append
                                                    (map (lambda (n) "0123456789")
                                                         (range 0 (quotient *max-line-length* 5))))
                                             "\r\n\r\n")
                              hello))
 (check-equal? "request line over the limit is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status (string-append "GET /"
                                             (apply string-append
                                                    (map (lambda (n) "0123456789")
                                                         (range 0 (quotient *max-line-length* 5))))
                                             " HTTP/1.1\r\nHost: x\r\n\r\n")
                              hello))
 (check-equal? "silent client does not block the server"
               1
               (let* ([server (http-listen "127.0.0.1:0" #:read-timeout 50)]
                      [quiet (tcp-connect (server-address server))])
                 (http-serve-one server hello #:limit 1)))
 (check-equal? "silent client gets a 408"
               "HTTP/1.1 408 Request Timeout"
               (let* ([server (http-listen "127.0.0.1:0" #:read-timeout 50)]
                      [quiet (tcp-connect (server-address server))])
                 (http-serve-one server hello #:limit 1)
                 (trim (read-line (tcp-stream-buffered-reader quiet)))))
 (check-equal? "partial request head gets a 408"
               "HTTP/1.1 408 Request Timeout"
               (let* ([server (http-listen "127.0.0.1:0" #:read-timeout 50)]
                      [slow (tcp-connect (server-address server))]
                      [writer (tcp-stream-writer slow)])
                 (write-string "GET /a HTTP/1.1\r\nHost: x\r\n" writer)
                 (flush-output-port writer)
                 (http-serve-one server hello #:limit 1)
                 (trim (read-line (tcp-stream-buffered-reader slow)))))
 (check-equal? "invalid read timeout errors"
               (list #t #t)
               (map (lambda (timeout)
                      (with-handler (lambda (err) #t)
                                    (begin
                                      (http-listen "127.0.0.1:0" #:read-timeout timeout)
                                      #f)))
                    (list 0 "soon")))
 (check-equal? "bare cr or nul in header value is a 400"
               (list "HTTP/1.1 400 Bad Request" "HTTP/1.1 400 Bad Request")
               (list (answer-status "GET /a HTTP/1.1\r\nHost: x\r\nX: a\rb\r\n\r\n" hello)
                     (answer-status "GET /a HTTP/1.1\r\nHost: x\r\nX: a\0b\r\n\r\n" hello)))
 (check-equal? "truncated request body is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nshort" hello))
 (check-equal? "invalid header line is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\nHost: x\r\nnot a header\r\n\r\n" hello))
 (check-equal? "response to an unreadable request sends Connection: close"
               #t
               (string-contains? (answer "nonsense\r\n\r\n" hello) "Connection: close"))
 (check-equal? "space before header colon is a 400"
               (list 1 "HTTP/1.1 400 Bad Request")
               (let ([served (over-ports (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                                        "Content-Length : 5\r\n\r\n"
                                                        "helloGET /b HTTP/1.1\r\nHost: x\r\n\r\n")
                                         hello)])
                 (list (car served) (car (split-many (cdr served) "\r\n")))))
 (check-equal? "method that is not a token is a 400"
               (list "HTTP/1.1 400 Bad Request" "HTTP/1.1 400 Bad Request")
               (list (answer-status " /a HTTP/1.1\r\nHost: x\r\n\r\n" hello)
                     (answer-status "G(T /a HTTP/1.1\r\nHost: x\r\n\r\n" hello)))
 (check-equal? "Host with more than an authority is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\nHost: x/b?c\r\n\r\n" hello))
 (check-equal?
  "trailers do not change framing or Host"
  (list '("x") '() '("t"))
  (let ([sent (http-read-request
               (open-input-string
                (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                               "Transfer-Encoding: chunked\r\n\r\n"
                               "0\r\nHost: y\r\ncontent-length: 9\r\nX-Trailer: t\r\n\r\n")))])
    (list (request-header* sent "Host")
          (request-header* sent "Content-Length")
          (request-header* sent "X-Trailer"))))
 (check-equal? "invalid request target is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET notapath HTTP/1.1\r\nHost: x\r\n\r\n" hello))
 (check-equal? "request without framing headers has no body"
               (list 1 #f)
               (let ([seen (box 'unset)])
                 (list (car (over-ports "POST /a HTTP/1.1\r\nHost: x\r\n\r\n"
                                        (lambda (sent)
                                          (set-box! seen (request-body sent))
                                          (make-response 200))))
                       (unbox seen)))))

(test-module
 "http-server-keep-alive-tests"
 (check-equal? "two requests on one connection"
               (list 2 #t #t "hello /two")
               (let* ([text (string-append "GET /one HTTP/1.1\r\nHost: x\r\n\r\n"
                                           "GET /two HTTP/1.1\r\nHost: x\r\n\r\n")]
                      [out (over-ports text hello)]
                      [back (cdr out)])
                 (list (car out)
                       (string-contains? back "hello /one")
                       (string-contains? back "hello /two")
                       (substring back (- (string-length back) 10) (string-length back)))))
 (check-equal?
  "request with Connection: close"
  1
  (car (over-ports (string-append "GET /a HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
                                  "GET /b HTTP/1.1\r\nHost: x\r\n\r\n")
                   hello)))
 (check-equal? "handler sets Connection: close"
               1
               (car (over-ports (string-append *get* *get*)
                                (lambda (sent)
                                  (make-response 200 #:headers (list (cons "Connection" "close")))))))
 (check-equal? "keep-alive off serves one request"
               (list 1 #t)
               (let ([out (over-ports (string-append *get* *get*) hello #:keep-alive #f)])
                 (list (car out) (string-contains? (cdr out) "Connection: close"))))
 (check-equal? "request limit" 2 (car (over-ports (string-append *get* *get* *get*) hello #:limit 2)))
 (check-equal? "last response under a limit sends Connection: close"
               #t
               (let ([out (over-ports (string-append *get* *get*) hello #:limit 1)])
                 (string-contains? (cdr out) "Connection: close")))
 (check-equal? "empty connection serves nothing" (cons 0 "") (over-ports "" hello)))

(test-module
 "http-server-socket-tests"
 (check-equal? "request and response over tcp"
               (list 200 "hello /a")
               (let ([received (round-trip (make-request "GET" "http://x/a") hello)])
                 (list (response-status received) (response-text received))))
 (check-equal? "request body over tcp"
               (list 201 "payload")
               (let ([received (round-trip (make-request "POST" "http://x/a" #:body "payload")
                                           (lambda (sent)
                                             (make-response 201 #:body (request-body sent))))])
                 (list (response-status received) (response-text received))))
 (check-equal? "HEAD over tcp has no body"
               (list 200 0 "5")
               (let ([received (round-trip (make-request "HEAD" "http://x/a")
                                           (lambda (sent) (make-response 200 #:body "hello")))])
                 (list (response-status received)
                       (bytes-length (response-body received))
                       (response-header received "Content-Length"))))
 (check-equal? "large request body over tcp"
               (list 20000 #t)
               (let* ([body (apply string-append (map (lambda (each) "0123456789") (range 0 2000)))]
                      [received (round-trip (make-request "GET" "http://x/big")
                                            (lambda (sent) (make-response 200 #:body body)))])
                 (list (bytes-length (response-body received))
                       (equal? (response-text received) body))))
 (check-equal? "server-address has the bound port"
               #t
               (string-contains? (server-address (http-listen "127.0.0.1:0")) "127.0.0.1:"))
 (check-equal? "handler error over tcp is a 500"
               500
               (response-status (round-trip (make-request "GET" "http://x/a")
                                            (lambda (sent) (error "blew up"))))))
