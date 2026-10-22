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

;; ---------------------------------------------------------------- fixtures

;; Answers every connection with the next canned reply, and keeps what the
;; client wrote so that a test can look at the request it sent.
(define (fixture-connector replies requests)
  (lambda (url)
    (when (empty? (unbox replies))
      (error "fixture: the client opened more connections than there are replies"))
    (define reply (car (unbox replies)))
    (define writer (open-output-string))
    (set-box! replies (cdr (unbox replies)))
    (set-box! requests (append (unbox requests) (list (cons (url->string url) writer))))
    (make-transport (open-input-string reply) writer)))

;; Runs *thunk* against *replies* and returns (response . requests), where each
;; request is (url . text).
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

;; The same, over a loopback socket: the far end writes the whole reply before
;; the near end is handed over, so one thread can play both parts.
(define (socket-connector reply)
  (lambda (url)
    (define listener (tcp-listen "127.0.0.1:0"))
    (define client (tcp-connect (tcp-listener-local-addr listener)))
    (define server (tcp-accept listener))
    (display reply (tcp-stream-writer server))
    ;; The close thunk is what holds on to the far end: nothing else refers to
    ;; it once this returns, and a collected socket hangs up on the client in
    ;; the middle of its request.
    (make-transport (tcp-stream-buffered-reader client)
                    (tcp-stream-writer client)
                    #:close (lambda ()
                              (with-handler (lambda (err) void) (tcp-shutdown! client))
                              (with-handler (lambda (err) void) (tcp-shutdown! server))
                              listener))))

(define (over-socket reply thunk)
  (parameterize ([current-http-connector (socket-connector reply)])
    (thunk)))

(define *ok* "HTTP/1.1 200 OK\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nhello world")

(define *chunked*
  (string-append "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nTrailer: X-Sum\r\n\r\n"
                 "5\r\nhello\r\n"
                 "6;note=x\r\n world\r\n"
                 "0\r\nX-Sum: 11\r\n\r\n"))

(define (redirect-to location)
  (string-append "HTTP/1.1 302 Found\r\nLocation: " location "\r\nContent-Length: 0\r\n\r\n"))

;; --------------------------------------------------------------------- urls

(test-module
 "http-url-tests"
 (check-equal? "a bare host gets a scheme, a port and a path"
               (list "http" "example.com" 80 "/")
               (let ([url (parse-url "example.com")])
                 (list (url-scheme url) (url-host url) (url-port url) (url-path url))))
 (check-equal? "every part of a url comes back out"
               (list "https" "user:pw" "example.com" 8443 "/a/b" "x=1&y=2" "frag")
               (let ([url (parse-url "https://user:pw@example.com:8443/a/b?x=1&y=2#frag")])
                 (list (url-scheme url)
                       (url-userinfo url)
                       (url-host url)
                       (url-port url)
                       (url-path url)
                       (url-query url)
                       (url-fragment url))))
 (check-equal? "https knows its port" 443 (url-port (parse-url "https://example.com")))
 (check-equal? "an ipv6 host loses its brackets, an authority keeps them"
               (list "::1" "[::1]:8080")
               (let ([url (parse-url "http://[::1]:8080/x")])
                 (list (url-host url) (url-authority url))))
 (check-equal? "a default port stays out of the authority"
               "example.com"
               (url-authority (parse-url "http://example.com:80/x")))
 (check-equal? "the request target is the path and the query"
               "/a?b=1"
               (url-target (parse-url "http://x/a?b=1")))
 (check-equal? "an origin is the scheme and the authority"
               "http://x:81"
               (url-origin (parse-url "http://x:81/a")))
 (check-equal? "a url survives a round trip"
               "http://a:81/b/c?d#e"
               (url->string (parse-url "http://a:81/b/c?d#e")))
 (check-equal? "a parsed url parses to itself"
               "http://a/b"
               (url->string (parse-url (parse-url "http://a/b"))))
 (check-err? "a url with no host is refused" (parse-url "http:///just-a-path") 'raises)
 (check-err? "a port that is not a number is refused" (parse-url "http://a:http/") 'raises)
 (check-equal? "reserved characters are escaped" "a%20b%2Fc%3Fd%3D%C3%A9" (url-encode "a b/c?d=é"))
 (check-equal? "a path keeps its slashes when asked" "a%20b/c" (url-encode "a b/c" #:safe "/"))
 (check-equal? "escapes, and a form's plus, decode" "a b/c dé" (url-decode "a%20b%2Fc+d%C3%A9"))
 (check-equal? "a percent that is not an escape is left alone" "100% %zz" (url-decode "100%+%zz"))
 (check-equal? "a query is encoded from an association list"
               "q=a%20b&page=2&flag"
               (alist->query (list (cons "q" "a b") (cons "page" 2) (cons "flag" #f))))
 (check-equal? "a query decodes back to an association list"
               (list (cons "q" "a b") (cons "page" "2") (cons "flag" ""))
               (query->alist "q=a%20b&page=2&flag"))
 (check-equal? "an empty query has no pairs" '() (query->alist #f))
 (check-equal? "a part whose escapes are not utf8 is left as it was written"
               (list (cons "a" "%FF"))
               (query->alist "a=%FF"))
 (check-equal? "a full url replaces the one it was found on"
               "http://z/q"
               (url->string (url-join "http://a/b/c" "http://z/q")))
 (check-equal? "an absolute path keeps the origin"
               "http://a/d"
               (url->string (url-join "http://a/b/c?x=1" "/d")))
 (check-equal? "a relative path hangs off the directory"
               "http://a/b/d/e"
               (url->string (url-join "http://a/b/c" "d/e")))
 (check-equal? "dot segments are resolved"
               "http://a/x"
               (url->string (url-join "http://a/b/c/d" "../../x")))
 (check-equal? "a scheme relative reference keeps the scheme"
               "https://z/q"
               (url->string (url-join "https://a/b" "//z/q")))
 (check-equal? "a bare fragment keeps the query"
               "http://a/b?x=1#top"
               (url->string (url-join "http://a/b?x=1" "#top")))
 (check-equal? "a bare query replaces the query"
               "http://a/b?y=2"
               (url->string (url-join "http://a/b?x=1" "?y=2")))
 (check-equal? "a reference is relative even with a url in its query"
               "http://a/login?next=https://b/x"
               (url->string (url-join "http://a/b/c" "/login?next=https://b/x")))
 (check-equal? "a non ascii safe character is encoded anyway" "%C3%A9" (url-encode "é" #:safe "é")))

;; ------------------------------------------------------------------ headers

(test-module
 "http-headers-tests"
 (check-equal? "headers come from pairs or from two element lists"
               (list (cons "A" "1") (cons "B" "2"))
               (normalize-headers (list (cons "A" 1) (list "B" "2"))))
 (check-equal? "a name is looked up without its case"
               "text/plain"
               (header-ref (normalize-headers (list (cons "Content-Type" "text/plain")))
                           "content-type"))
 (check-equal? "a missing name answers with the default"
               "none"
               (header-ref '() "Content-Type" "none"))
 (check-equal? "a repeated name keeps every value"
               (list "a" "b")
               (header-ref* (header-add (header-add '() "Set-Cookie" "a") "Set-Cookie" "b")
                            "set-cookie"))
 (check-equal? "setting a name takes the old value away"
               (list "new")
               (header-ref* (header-set (header-add '() "A" "old") "a" "new") "A"))
 (check-equal? "a default does not overwrite"
               "mine"
               (header-ref (header-default (header-add '() "A" "mine") "a" "theirs") "A"))
 (check-equal? "removing a name takes every value"
               '()
               (header-ref* (header-remove (header-add '() "A" "1") "a") "A"))
 (check-equal? "headers are written as crlf lines"
               "A: 1\r\nB: 2\r\n"
               (headers->string (list (cons "A" "1") (cons "B" "2"))))
 (check-equal? "a header line splits on the first colon"
               (cons "Location" "http://x/y")
               (parse-header-line "Location: http://x/y"))
 (check-equal? "a line with no colon is not a header" #f (parse-header-line "nocolon"))
 (check-err? "a newline in a value is refused"
             (normalize-headers (list (cons "X" "a\r\nY: b")))
             'raises)
 (check-err? "a colon in a name is refused" (normalize-headers (list (cons "X:Y" "a"))) 'raises)
 (check-err? "a space in a name is refused" (normalize-headers (list (cons "X Y" "a"))) 'raises)
 (check-equal? "the punctuation a token allows is kept"
               "X-Not_A.Token!: 1\r\n"
               (headers->string (header-add '() "X-Not_A.Token!" 1))))

;; ------------------------------------------------------------------- base64

(test-module
 "http-base64-tests"
 (check-equal? "the empty string encodes to nothing" "" (base64-encode ""))
 (check-equal? "one byte pads twice" "Zg==" (base64-encode "f"))
 (check-equal? "two bytes pad once" "Zm8=" (base64-encode "fo"))
 (check-equal? "three bytes do not pad" "Zm9v" (base64-encode "foo"))
 (check-equal? "credentials encode the way a server expects"
               "dXNlcjpwYXNzd29yZA=="
               (base64-encode "user:password"))
 (check-equal? "bytes survive a round trip"
               (bytes 0 255 128)
               (base64-decode (base64-encode (bytes 0 255 128))))
 (check-equal? "whitespace in the middle is ignored" (bytes 102 111 111) (base64-decode "Zm\n9v"))
 (check-err? "a digit outside the alphabet is refused" (base64-decode "a*b") 'raises)
 (check-err? "a digit after the padding is refused" (base64-decode "QQ=A") 'raises)
 (check-equal? "a payload of many groups survives a round trip"
               #t
               (let ([long (apply string-append (map (lambda (n) "0123456789") (range 0 30)))])
                 (equal? long (bytes->string/utf8 (base64-decode (base64-encode long)))))))

;; ---------------------------------------------------------- reading a reply

(test-module
 "http-response-tests"
 (check-equal?
  "a status line is read into its three parts"
  (list "HTTP/1.1" 200 "OK")
  (let ([response (reply-to (list *ok*) (lambda () (http-get "http://x/")))])
    (list (response-version response) (response-status response) (response-reason response))))
 (check-equal? "a body of a known length is read whole"
               "hello world"
               (response-text (reply-to (list *ok*) (lambda () (http-get "http://x/")))))
 (check-equal? "a 2xx is ok"
               #t
               (response-ok? (reply-to (list *ok*) (lambda () (http-get "http://x/")))))
 (check-equal? "a 404 is not ok"
               #f
               (response-ok? (reply-to (list "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n")
                                       (lambda () (http-get "http://x/")))))
 (check-equal? "a header is read without its case"
               "text/plain"
               (response-header (reply-to (list *ok*) (lambda () (http-get "http://x/")))
                                "content-type"))
 (check-equal? "a reason phrase may be missing"
               (list 200 "")
               (let ([response (reply-to (list "HTTP/1.1 200\r\nContent-Length: 0\r\n\r\n")
                                         (lambda () (http-get "http://x/")))])
                 (list (response-status response) (response-reason response))))
 (check-equal? "a chunked body is put back together"
               "hello world"
               (response-text (reply-to (list *chunked*) (lambda () (http-get "http://x/")))))
 (check-equal? "a trailer joins the headers"
               "11"
               (response-header (reply-to (list *chunked*) (lambda () (http-get "http://x/")))
                                "x-sum"))
 (check-equal? "a body with no length is read until the connection closes"
               "no length here"
               (response-text (reply-to (list "HTTP/1.1 200 OK\r\n\r\nno length here")
                                        (lambda () (http-get "http://x/")))))
 (check-equal? "the answer to a HEAD keeps the length and drops the body"
               (list "99" 0)
               (let ([response (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 99\r\n\r\n")
                                         (lambda () (http-head "http://x/")))])
                 (list (response-header response "content-length")
                       (bytes-length (response-body response)))))
 (check-equal? "a 204 has no body"
               (list 204 0)
               (let ([response (reply-to (list "HTTP/1.1 204 No Content\r\n\r\n")
                                         (lambda () (http-get "http://x/")))])
                 (list (response-status response) (bytes-length (response-body response)))))
 (check-equal?
  "a 304 has no body"
  0
  (bytes-length (response-body
                 (reply-to (list "HTTP/1.1 304 Not Modified\r\nContent-Length: 7\r\n\r\n")
                           (lambda () (http-get "http://x/"))))))
 (check-equal?
  "a folded header line belongs to the header above it"
  "one two"
  (response-header
   (reply-to (list "HTTP/1.1 200 OK\r\nX-Long: one\r\n  two\r\nContent-Length: 0\r\n\r\n")
             (lambda () (http-get "http://x/")))
   "x-long"))
 (check-equal? "two lengths that agree are one length"
               "ab"
               (response-text (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 2, 2\r\n\r\nab")
                                        (lambda () (http-get "http://x/")))))
 (check-equal? "http/1.1 keeps the connection unless it says otherwise"
               #t
               (response-keep-alive? (reply-to (list *ok*) (lambda () (http-get "http://x/")))))
 (check-equal? "a Connection: close is believed"
               #f
               (response-keep-alive?
                (reply-to (list "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 0\r\n\r\n")
                          (lambda () (http-get "http://x/")))))
 (check-err? "two lengths that disagree are refused, rather than guessed at"
             (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nContent-Length: 3\r\n\r\nab")
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-err?
  "a chunked body with a length is refused"
  (reply-to
   (list "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n")
   (lambda () (http-get "http://x/")))
  'raises)
 (check-err? "an unknown transfer encoding is refused"
             (reply-to (list "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\nxx")
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "a body cut short is refused"
             (reply-to (list "HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\nshort")
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "something that is not a status line is refused"
             (reply-to (list "not http at all\r\n\r\n") (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "a head that never ends is refused"
             (reply-to (list "HTTP/1.1 200 OK\r\nA: b") (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "a chunk size that is not a number is refused"
             (reply-to (list "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n")
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "a header line with no end to it is refused"
             (reply-to (list (string-append "HTTP/1.1 200 OK\r\nX: "
                                            (apply string-append
                                                   (map (lambda (n) "0123456789")
                                                        (range 0 (quotient *max-line-length* 5))))))
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "a version this client does not speak is refused"
             (reply-to (list "HTTP/2 200 OK\r\nContent-Length: 0\r\n\r\n")
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-equal?
  "a body longer than one read comes back whole"
  (list 70000 #t)
  (let* ([body (apply string-append (map (lambda (n) "0123456789") (range 0 7000)))]
         [text (response-text (reply-to (list (string-append "HTTP/1.1 200 OK\r\nContent-Length: "
                                                             (int->string (string-length body))
                                                             "\r\n\r\n"
                                                             body))
                                        (lambda () (http-get "http://x/"))))])
    (list (string-length text) (equal? text body))))
 (check-err? "more header lines than are allowed is refused"
             (reply-to (list (string-append "HTTP/1.1 200 OK\r\n"
                                            (headers->string (map (lambda (n)
                                                                    (cons "X" (int->string n)))
                                                                  (range 0 (+ *max-header-lines* 1))))
                                            "\r\n"))
                       (lambda () (http-get "http://x/")))
             'raises)
 (check-err? "interim responses that never end are refused"
             (reply-to (list (apply string-append
                                    (map (lambda (n) "HTTP/1.1 100 Continue\r\n\r\n") (range 0 20))))
                       (lambda () (http-get "http://x/")))
             'raises))

;; -------------------------------------------------------- writing a request

(test-module
 "http-request-tests"
 (check-equal?
  "the request line carries the method, the target and the version"
  "GET /a?b=1 HTTP/1.1"
  (request-line
   (request-text (requests-of (list *ok*) (lambda () (http-get "http://example.com/a?b=1"))) 0)))
 (check-equal? "the host header comes from the url"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://example.com/a")))
                              0)
                "Host: example.com\r\n"))
 (check-equal?
  "a non default port stays in the host header"
  #t
  (string-contains?
   (request-text (requests-of (list *ok*) (lambda () (http-get "http://example.com:8080/a"))) 0)
   "Host: example.com:8080\r\n"))
 (check-equal? "the client names itself"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://x/"))) 0)
                "User-Agent: steel-http/0.1.0\r\n"))
 (check-equal? "the caller's own header wins over the default"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*)
                                           (lambda ()
                                             (http-get "http://x/"
                                                       #:headers (list (cons "User-Agent" "mine")))))
                              0)
                "User-Agent: mine\r\n"))
 (check-equal?
  "a body gets a length"
  #t
  (string-contains?
   (request-text (requests-of (list *ok*) (lambda () (http-post "http://x/" #:body "payload"))) 0)
   "Content-Length: 7\r\n"))
 (check-equal? "a post with no body still says so"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-post "http://x/"))) 0)
                "Content-Length: 0\r\n"))
 (check-equal?
  "a body is sent after the head"
  #t
  (ends-with?
   (request-text (requests-of (list *ok*) (lambda () (http-post "http://x/" #:body "payload"))) 0)
   "\r\n\r\npayload"))
 (check-equal? "a form is url encoded and typed"
               #t
               (let ([text (request-text (requests-of (list *ok*)
                                                      (lambda ()
                                                        (http-post "http://x/"
                                                                   #:form (list (cons "q" "a b")
                                                                                (cons "n" 2)))))
                                         0)])
                 (and (string-contains? text "Content-Type: application/x-www-form-urlencoded\r\n")
                      (ends-with? text "\r\n\r\nq=a%20b&n=2"))))
 (check-equal? "a query keyword replaces the query"
               "GET /p?a=b%20c HTTP/1.1"
               (request-line (request-text (requests-of (list *ok*)
                                                        (lambda ()
                                                          (http-get "http://x/p?old=1"
                                                                    #:query (list (cons "a" "b c")))))
                                           0)))
 (check-equal? "credentials become a basic authorization header"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*)
                                           (lambda ()
                                             (http-get "http://x/" #:auth (cons "user" "password"))))
                              0)
                "Authorization: Basic dXNlcjpwYXNzd29yZA==\r\n"))
 (check-equal? "credentials in the url become a header, not part of the request line"
               #t
               (let ([text (request-text (requests-of (list *ok*)
                                                      (lambda ()
                                                        (http-get "http://user:password@x/p")))
                                         0)])
                 (and (string-contains? text "Authorization: Basic dXNlcjpwYXNzd29yZA==\r\n")
                      (equal? (request-line text) "GET /p HTTP/1.1"))))
 (check-equal? "a request asks for no encoding it cannot undo"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://x/"))) 0)
                "Accept-Encoding: identity\r\n"))
 (check-equal? "a one shot request asks to be hung up on"
               #t
               (string-contains?
                (request-text (requests-of (list *ok*) (lambda () (http-get "http://x/"))) 0)
                "Connection: close\r\n"))
 (check-err? "a body and a form together are refused"
             (make-request "POST" "http://x/" #:body "a" #:form (list (cons "b" "c")))
             'raises)
 (check-err? "a body that is not text or bytes is refused"
             (make-request "POST" "http://x/" #:body 42)
             'raises))

;; ---------------------------------------------------------------- redirects

(test-module
 "http-redirect-tests"
 (check-equal? "a relative location is resolved and followed"
               (list "http://a/first" "http://a/next")
               (map car
                    (requests-of (list (redirect-to "/next")
                                       "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
                                 (lambda () (http-get "http://a/first")))))
 (check-equal? "the body of the last hop is the answer"
               "ok"
               (response-text (reply-to (list (redirect-to "/next")
                                              "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
                                        (lambda () (http-get "http://a/first")))))
 (check-equal?
  "a 302 turns a post into a get without a body"
  "GET /next HTTP/1.1"
  (request-line (request-text (requests-of (list (redirect-to "/next")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda () (http-post "http://a/first" #:body "data")))
                              1)))
 (check-equal?
  "a 307 asks again exactly as before"
  #t
  (let ([text (request-text
               (requests-of (list (string-append "HTTP/1.1 307 Temporary Redirect\r\n"
                                                 "Location: /again\r\nContent-Length: 0\r\n\r\n")
                                  "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                            (lambda () (http-post "http://a/first" #:body "data")))
               1)])
    (and (equal? (request-line text) "POST /again HTTP/1.1") (ends-with? text "\r\n\r\ndata"))))
 (check-equal?
  "a head stays a head"
  "HEAD /next HTTP/1.1"
  (request-line (request-text (requests-of (list (redirect-to "/next")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda () (http-head "http://a/first")))
                              1)))
 (check-equal? "credentials follow a redirect within the same origin"
               #t
               (string-contains?
                (request-text (requests-of (list (redirect-to "/next")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda ()
                                             (http-get "http://a/first" #:auth (cons "u" "p"))))
                              1)
                "Authorization: "))
 (check-equal? "credentials do not follow a redirect to another origin"
               #f
               (string-contains?
                (request-text (requests-of (list (redirect-to "http://b/there")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda ()
                                             (http-get "http://a/first" #:auth (cons "u" "p"))))
                              1)
                "Authorization: "))
 (check-equal? "the host header is rewritten for the new origin"
               #t
               (string-contains?
                (request-text (requests-of (list (redirect-to "http://b/there")
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
                                           (lambda () (http-get "http://a/first")))
                              1)
                "Host: b\r\n"))
 (check-equal? "asking not to follow hands back the 3xx"
               (list 302 "/next")
               (let ([response (reply-to (list (redirect-to "/next"))
                                         (lambda () (http-get "http://a/" #:follow 0)))])
                 (list (response-status response) (response-header response "location"))))
 (check-equal? "a 3xx with no location is the answer"
               301
               (response-status (reply-to (list "HTTP/1.1 301 Moved\r\nContent-Length: 0\r\n\r\n")
                                          (lambda () (http-get "http://a/")))))
 (check-err? "a redirect that never lands is refused"
             (reply-to (list (redirect-to "/1") (redirect-to "/2"))
                       (lambda () (http-get "http://a/" #:follow 1)))
             'raises))

;; ------------------------------------------------------------- over a socket

(test-module
 "http-socket-tests"
 (check-equal? "a request and a reply cross a real socket"
               "hello world"
               (response-text (over-socket *ok* (lambda () (http-get "http://127.0.0.1/a")))))
 (check-equal? "a chunked body crosses a real socket"
               "hello world"
               (response-text (over-socket *chunked* (lambda () (http-get "http://127.0.0.1/a")))))
 (check-equal? "a post crosses a real socket"
               200
               (response-status
                (over-socket *ok* (lambda () (http-post "http://127.0.0.1/a" #:body "payload")))))
 (check-equal? "the answer to a HEAD crosses a real socket"
               0
               (bytes-length (response-body
                              (over-socket *ok* (lambda () (http-head "http://127.0.0.1/a"))))))
 (check-equal? "a body larger than one read arrives whole"
               (list 20000 #t)
               (let* ([body (apply string-append (map (lambda (each) "0123456789") (range 0 2000)))]
                      [response (over-socket (string-append "HTTP/1.1 200 OK\r\nContent-Length: "
                                                            (int->string (string-length body))
                                                            "\r\n\r\n"
                                                            body)
                                             (lambda () (http-get "http://127.0.0.1/big")))])
                 (list (bytes-length (response-body response))
                       (equal? (response-text response) body))))
 (check-equal? "an unbuffered reader reads a response too"
               "hello world"
               (parameterize ([current-http-connector
                               (lambda (url)
                                 (define listener (tcp-listen "127.0.0.1:0"))
                                 (define client (tcp-connect (tcp-listener-local-addr listener)))
                                 (define server (tcp-accept listener))
                                 (display *ok* (tcp-stream-writer server))
                                 (make-transport (tcp-stream-reader client)
                                                 (tcp-stream-writer client)))])
                 (response-text (http-get "http://127.0.0.1/a"))))
 (check-equal? "a body with no length ends when the peer hangs up"
               "until the end"
               (let* ([listener (tcp-listen "127.0.0.1:0")]
                      [client (tcp-connect (tcp-listener-local-addr listener))]
                      [server (tcp-accept listener)])
                 (display "HTTP/1.1 200 OK\r\n\r\nuntil the end" (tcp-stream-writer server))
                 (tcp-shutdown! server)
                 (response-text (http-read-response (tcp-stream-buffered-reader client)))))
 (check-equal?
  "a reply still arrives when the connector is given a read timeout"
  "hello world"
  (let ([listener (tcp-listen "127.0.0.1:0")]
        [far-end (box #f)])
    (parameterize ([current-http-connector (lambda (url)
                                             (define sent (tcp-connector url #:read-timeout 10000))
                                             ;; The far end is kept in a box: a collected socket
                                             ;; hangs up on the client in the middle of its request.
                                             (set-box! far-end (tcp-accept listener))
                                             (display *ok* (tcp-stream-writer (unbox far-end)))
                                             sent)])
      (response-text (http-get (string-append "http://" (tcp-listener-local-addr listener) "/a"))))))
 (check-equal? "a read waits no longer than the timeout the connector was given"
               #t
               (let* ([listener (tcp-listen "127.0.0.1:0")]
                      [target (string-append "http://" (tcp-listener-local-addr listener) "/a")]
                      [sent (tcp-connector (parse-url target) #:read-timeout 50)]
                      ;; The far end says nothing and is held on to, so the read
                      ;; gives up on its own rather than on a hang up
                      [far-end (tcp-accept listener)]
                      [outcome (with-handler (lambda (err)
                                               (string-contains? (error-object-message err)
                                                                 "would block"))
                                             (begin
                                               (http-read-response (transport-reader sent))
                                               'returned))])
                 (if far-end outcome outcome)))
 (check-equal?
  "one connection carries two exchanges"
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

;; ------------------------------------------- what the client refuses to send

(test-module
 "http-framing-tests"
 (check-err? "a newline in a path is refused, rather than written into the request line"
             (parse-url "http://x/a\r\nX-Evil: 1")
             'raises)
 (check-err? "a newline in a query is refused" (parse-url "http://x/a?b=1\nX: 2") 'raises)
 (check-err? "a space in a host is refused" (parse-url "http://ev il/a") 'raises)
 (check-err? "a space in a path is refused, since url-encode is right there"
             (parse-url "http://x/a b")
             'raises)
 (check-equal? "an escaped space is a path like any other"
               "/a%20b"
               (url-target (parse-url "http://x/a%20b")))
 (check-err? "a method with a request line in it is refused"
             (http-send "GET /evil HTTP/1.1\r\nX" "http://x/")
             'raises)
 (check-equal? "a method may be a symbol"
               "DELETE /x HTTP/1.1"
               (request-line (request-text (requests-of (list *ok*)
                                                        (lambda () (http-send 'delete "http://x/x")))
                                           0)))
 (check-err? "a request this client cannot frame is refused rather than sent broken"
             (reply-to (list *ok*)
                       (lambda ()
                         (http-post "http://x/"
                                    #:body "a"
                                    #:headers (list (cons "Transfer-Encoding" "chunked")))))
             'raises)
 (check-err? "a negative redirect count is refused" (http-get "http://x/" #:follow -1) 'raises)
 (check-err? "a method that is not a token is refused"
             (make-request (string-append "GET" (string (integer->char 127))) "http://x/")
             'raises)
 (check-equal?
  "a 100 Continue is stepped over"
  (list 200 "ok")
  (let ([response (reply-to (list (string-append "HTTP/1.1 100 Continue\r\n\r\n"
                                                 "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"))
                            (lambda () (http-get "http://x/")))])
    (list (response-status response) (response-text response))))
 (check-equal? "early hints are stepped over, headers and all"
               200
               (response-status
                (reply-to (list (string-append "HTTP/1.1 103 Early Hints\r\nLink: </s.css>\r\n\r\n"
                                               "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"))
                          (lambda () (http-get "http://x/")))))
 (check-equal? "a 101 is the answer, since the connection is no longer http"
               101
               (response-status
                (reply-to (list "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n\r\n")
                          (lambda () (http-get "http://x/"))))))

;; ------------------------------------------------------------ the server side

;; Serves *text* as if it had arrived on a connection, and returns
;; (requests-served . what-went-back).
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

;; The status line of what went back, so that a test can name a code without
;; matching the whole message.
(define (answer-status text handler)
  (car (split-many (answer text handler) "\r\n")))

(define (hello sent)
  (make-response 200 #:body (string-append "hello " (request-target sent))))

(define *get* "GET /a HTTP/1.1\r\nHost: x\r\n\r\n")

;; One thread, no canned replies: the client writes a request over a real
;; socket, the server answers it, and the client reads the answer back. The
;; `#:limit 1` keeps the server from waiting for a second request on a
;; connection the client is holding open.
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
  "a request arrives with its method, target and version"
  (list "GET" "/a?b=1" "HTTP/1.1")
  (let ([sent (http-read-request (open-input-string "GET /a?b=1 HTTP/1.1\r\nHost: x\r\n\r\n"))])
    (list (request-method sent) (request-target sent) (request-version sent))))
 (check-equal?
  "a header arrives on the request"
  "text/plain"
  (request-header
   (http-read-request (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\nAccept: text/plain\r\n\r\n"))
   "Accept"))
 (check-equal? "the host of the request line is the host of the url"
               "x"
               (url-host (request-url (http-read-request (open-input-string *get*)))))
 (check-equal? "a request with no body has none, rather than an empty one"
               #f
               (request-body (http-read-request (open-input-string *get*))))
 (check-equal? "a body of the declared length arrives whole"
               "hello"
               (bytes->string/utf8
                (request-body (http-read-request
                               (open-input-string
                                "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\nhello")))))
 (check-equal?
  "a chunked body arrives whole, trailers and all"
  (list "hello world" "t")
  (let ([sent (http-read-request
               (open-input-string
                (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                               "Transfer-Encoding: chunked\r\n\r\n"
                               "5\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n")))])
    (list (bytes->string/utf8 (request-body sent)) (request-header sent "X-Trailer"))))
 (check-equal? "a connection that closes between requests is the end of them, not an error"
               #t
               (eof-object? (http-read-request (open-input-string ""))))
 (check-equal? "an absolute target, which is what a proxy is asked for, keeps its own host"
               "other"
               (url-host (request-url (http-read-request
                                       (open-input-string
                                        "GET http://other/a HTTP/1.1\r\nHost: x\r\n\r\n")))))
 (check-equal? "an OPTIONS may ask about the server itself"
               "*"
               (request-target (http-read-request
                                (open-input-string "OPTIONS * HTTP/1.1\r\nHost: x\r\n\r\n"))))
 (check-equal? "an HTTP/1.0 request may arrive with no Host at all"
               (list "/a" "HTTP/1.0")
               (let ([sent (http-read-request (open-input-string "GET /a HTTP/1.0\r\n\r\n"))])
                 (list (request-target sent) (request-version sent))))
 (check-equal?
  "an obs-fold continuation belongs to the header above it"
  "one two"
  (request-header
   (http-read-request (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\nX-Long: one\r\n two\r\n\r\n"))
   "X-Long"))
 (check-err? "a request this server cannot read raises, rather than coming back empty"
             (http-read-request (open-input-string "nonsense\r\n\r\n"))
             'raises)
 (check-equal? "a stray crlf before the request line is skipped, not read as a request"
               "/a"
               (request-target (http-read-request (open-input-string (string-append "\r\n" *get*)))))
 (check-equal?
  "an Expect gets its go ahead before the body is read"
  "HTTP/1.1 100 Continue\r\n\r\n"
  (let ([writer (open-output-string)])
    (http-read-request
     (open-input-string (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                       "Expect: 100-continue\r\nContent-Length: 2\r\n\r\nhi"))
     #:continue writer)
    (get-output-string writer)))
 (check-equal? "a 1.1 request holds the connection open, a 1.0 one does not"
               (list #t #f)
               (list (request-keep-alive? (http-read-request (open-input-string *get*)))
                     (request-keep-alive?
                      (http-read-request (open-input-string "GET /a HTTP/1.0\r\n\r\n")))))
 (check-equal?
  "either end may ask to hold a 1.0 connection open, or to drop a 1.1 one"
  (list #t #f)
  (list (request-keep-alive?
         (http-read-request (open-input-string "GET /a HTTP/1.0\r\nConnection: keep-alive\r\n\r\n")))
        (request-keep-alive?
         (http-read-request
          (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"))))))

(test-module
 "http-server-answer-tests"
 (check-equal? "an answer carries the status line, the length and the body"
               (string-append "HTTP/1.1 200 OK\r\n"
                              "Server: steel-http/0.1.0\r\n"
                              "Connection: keep-alive\r\n"
                              "Content-Length: 8\r\n"
                              "\r\n"
                              "hello /a")
               (answer *get* hello))
 (check-equal? "the reason phrase comes from the code when it is not given"
               "HTTP/1.1 404 Not Found"
               (answer-status *get* (lambda (sent) (make-response 404))))
 (check-equal? "a reason phrase may be given instead"
               "HTTP/1.1 404 Nothing Here"
               (answer-status *get* (lambda (sent) (make-response 404 #:reason "Nothing Here"))))
 (check-equal? "a code with no well known phrase goes out without one"
               "HTTP/1.1 599"
               (answer-status *get* (lambda (sent) (make-response 599))))
 (check-equal? "the headers a handler sets are the ones that go out"
               #t
               (string-contains?
                (answer *get*
                        (lambda (sent) (make-response 200 #:headers (list (cons "X-Mine" "yes")))))
                "X-Mine: yes"))
 (check-equal? "a handler may name the Server header itself"
               #t
               (string-contains?
                (answer *get*
                        (lambda (sent) (make-response 200 #:headers (list (cons "Server" "mine")))))
                "Server: mine"))
 (check-equal? "the answer to a HEAD carries the length of a body it does not send"
               (string-append "HTTP/1.1 200 OK\r\n"
                              "Server: steel-http/0.1.0\r\n"
                              "Connection: keep-alive\r\n"
                              "Content-Length: 5\r\n"
                              "\r\n")
               (answer "HEAD /a HTTP/1.1\r\nHost: x\r\n\r\n"
                       (lambda (sent) (make-response 200 #:body "hello"))))
 (check-equal? "a 204 sends neither a body nor a length for one"
               (string-append "HTTP/1.1 204 No Content\r\n"
                              "Server: steel-http/0.1.0\r\n"
                              "Connection: keep-alive\r\n"
                              "\r\n")
               (answer *get* (lambda (sent) (make-response 204 #:body "ignored"))))
 (check-equal? "a 304 sends no length either, since it stands in for a body it is not sending"
               #f
               (string-contains? (answer *get* (lambda (sent) (make-response 304))) "Content-Length"))
 (check-equal? "an answer this server cannot frame is a 500, rather than sent broken"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get*
                              (lambda (sent)
                                (make-response 200
                                               #:headers (list (cons "Transfer-Encoding" "chunked"))
                                               #:body "x"))))
 (check-err? "writing one directly is still refused"
             (http-write-response!
              (open-output-string)
              (make-response 200 #:headers (list (cons "Transfer-Encoding" "chunked")) #:body "x"))
             'raises)
 (check-err? "a status code that is not one is refused" (make-response 99) 'raises)
 (check-equal?
  "a bytevector body goes out as it is"
  "\r\n\r\nhello"
  (let ([text (answer *get* (lambda (sent) (make-response 200 #:body (string->bytes "hello"))))])
    (substring text (- (string-length text) 9) (string-length text)))))

(test-module
 "http-server-handler-tests"
 (check-equal? "a handler that raises becomes a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) (error "a handler blew up"))))
 (check-equal? "what the handler raised stays on this side of the wire"
               #f
               (string-contains? (answer *get* (lambda (sent) (error "/etc/passwd is right here")))
                                 "passwd"))
 (check-equal? "a handler that answers with something that is not a response becomes a 500"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) 'not-a-response)))
 (check-equal? "the handler is called once per request"
               2
               (let ([calls (box 0)])
                 (over-ports (string-append *get* *get*)
                             (lambda (sent)
                               (set-box! calls (+ (unbox calls) 1))
                               (make-response 200)))
                 (unbox calls)))
 (check-equal? "an answer that cannot be written ends the connection rather than raising"
               0
               (let ([writer (open-output-string)])
                 (close-output-port writer)
                 (http-serve-connection (open-input-string *get*) writer hello)))
 (check-equal? "a response this server cannot frame is a 500, not the end of the loop"
               "HTTP/1.1 500 Internal Server Error"
               (answer-status *get* (lambda (sent) (response "HTTP/1.1" 200 "OK" '() #f)))))

(test-module
 "http-server-framing-tests"
 (check-equal? "a request line that is not one is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "nonsense\r\n\r\n" hello))
 (check-equal? "a request line with a space too many is a 400, rather than guessed at"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a b HTTP/1.1\r\nHost: x\r\n\r\n" hello))
 (check-equal? "a version this server does not speak is a 505"
               "HTTP/1.1 505 HTTP Version Not Supported"
               (answer-status "GET /a HTTP/2.0\r\nHost: x\r\n\r\n" hello))
 (check-equal? "an HTTP/1.1 request without a Host is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\n\r\n" hello))
 (check-equal? "two Host headers are a 400, rather than left to the next hop to choose"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\nHost: x\r\nHost: y\r\n\r\n" hello))
 (check-equal? "a chunked body with a Content-Length beside it is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status
                (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                               "Transfer-Encoding: chunked\r\nContent-Length: 5\r\n\r\n0\r\n\r\n")
                hello))
 (check-equal? "Content-Length headers that disagree are a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status (string-append "POST /a HTTP/1.1\r\nHost: x\r\n"
                                             "Content-Length: 5\r\nContent-Length: 6\r\n\r\nhello")
                              hello))
 (check-equal? "a transfer encoding this server cannot read is a 501"
               "HTTP/1.1 501 Not Implemented"
               (answer-status "POST /a HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: gzip\r\n\r\n" hello))
 (check-equal? "a body longer than this server reads is a 413, before it is read"
               "HTTP/1.1 413 Content Too Large"
               (answer-status (string-append "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: "
                                             (int->string (+ *max-body-bytes* 1))
                                             "\r\n\r\n")
                              hello))
 (check-equal? "a chunked body longer than it is allowed stops rather than being read whole"
               'too-large
               (read-chunked (open-input-string "4\r\nabcd\r\n0\r\n\r\n") #:limit 2))
 (check-equal? "a line of the head longer than it is allowed is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status (string-append "GET /a HTTP/1.1\r\nHost: x\r\nX: "
                                             (apply string-append
                                                    (map (lambda (n) "0123456789")
                                                         (range 0 (quotient *max-line-length* 5))))
                                             "\r\n\r\n")
                              hello))
 (check-equal? "a body that stops early is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "POST /a HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nshort" hello))
 (check-equal? "a header line that is not one is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET /a HTTP/1.1\r\nHost: x\r\nnot a header\r\n\r\n" hello))
 (check-equal? "a request this server cannot read is the last one on the connection"
               #t
               (string-contains? (answer "nonsense\r\n\r\n" hello) "Connection: close"))
 (check-equal? "a target that is neither a path nor a url is a 400"
               "HTTP/1.1 400 Bad Request"
               (answer-status "GET notapath HTTP/1.1\r\nHost: x\r\n\r\n" hello))
 (check-equal? "a body with no framing at all is no body, rather than a wait for one"
               (list 1 #f)
               (let ([seen (box 'unset)])
                 (list (car (over-ports "POST /a HTTP/1.1\r\nHost: x\r\n\r\n"
                                        (lambda (sent)
                                          (set-box! seen (request-body sent))
                                          (make-response 200))))
                       (unbox seen)))))

(test-module
 "http-server-keep-alive-tests"
 (check-equal? "two requests on one connection are both answered, in order"
               (list 2 #t #t "hello /two")
               (let* ([text (string-append "GET /one HTTP/1.1\r\nHost: x\r\n\r\n"
                                           "GET /two HTTP/1.1\r\nHost: x\r\n\r\n")]
                      [out (over-ports text hello)]
                      [back (cdr out)])
                 (list (car out)
                       (string-contains? back "hello /one")
                       (string-contains? back "hello /two")
                       ;; the second answer is the one that ends the stream
                       (substring back (- (string-length back) 10) (string-length back)))))
 (check-equal?
  "a request that asks to close the connection is the last one on it"
  1
  (car (over-ports (string-append "GET /a HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
                                  "GET /b HTTP/1.1\r\nHost: x\r\n\r\n")
                   hello)))
 (check-equal? "a handler may close the connection itself"
               1
               (car (over-ports (string-append *get* *get*)
                                (lambda (sent)
                                  (make-response 200 #:headers (list (cons "Connection" "close")))))))
 (check-equal? "keep-alive off serves one request whatever the request asked for"
               (list 1 #t)
               (let ([out (over-ports (string-append *get* *get*) hello #:keep-alive #f)])
                 (list (car out) (string-contains? (cdr out) "Connection: close"))))
 (check-equal? "a limit stops the connection after that many requests"
               2
               (car (over-ports (string-append *get* *get* *get*) hello #:limit 2)))
 (check-equal? "the last answer under a limit says the connection is closing"
               #t
               (let ([out (over-ports (string-append *get* *get*) hello #:limit 1)])
                 (string-contains? (cdr out) "Connection: close")))
 (check-equal? "a connection with nothing on it serves nothing" (cons 0 "") (over-ports "" hello)))

(test-module
 "http-server-socket-tests"
 (check-equal? "a request and an answer cross a real socket"
               (list 200 "hello /a")
               (let ([received (round-trip (make-request "GET" "http://x/a") hello)])
                 (list (response-status received) (response-text received))))
 (check-equal? "a body crosses a real socket"
               (list 201 "payload")
               (let ([received (round-trip (make-request "POST" "http://x/a" #:body "payload")
                                           (lambda (sent)
                                             (make-response 201 #:body (request-body sent))))])
                 (list (response-status received) (response-text received))))
 (check-equal? "the answer to a HEAD crosses a real socket with no body"
               (list 200 0 "5")
               (let ([received (round-trip (make-request "HEAD" "http://x/a")
                                           (lambda (sent) (make-response 200 #:body "hello")))])
                 (list (response-status received)
                       (bytes-length (response-body received))
                       (response-header received "Content-Length"))))
 (check-equal? "a body larger than one read crosses whole"
               (list 20000 #t)
               (let* ([body (apply string-append (map (lambda (each) "0123456789") (range 0 2000)))]
                      [received (round-trip (make-request "GET" "http://x/big")
                                            (lambda (sent) (make-response 200 #:body body)))])
                 (list (bytes-length (response-body received))
                       (equal? (response-text received) body))))
 (check-equal? "a server reports the port the os picked for it"
               #t
               (string-contains? (server-address (http-listen "127.0.0.1:0")) "127.0.0.1:"))
 (check-equal? "the answer a handler never gave is a 500 over a socket too"
               500
               (response-status (round-trip (make-request "GET" "http://x/a")
                                            (lambda (sent) (error "blew up"))))))
