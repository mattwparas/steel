# steel/http

An HTTP/1.1 client and server, written in Steel over `steel/tcp`.

```scheme
(require "steel/http/client.scm")

(define response (http-get "http://example.com/"))

(response-status response) ;; => 200
(response-header response "content-type") ;; => "text/html; charset=UTF-8"
(response-text response) ;; => "<!doctype html>..."
```

`http-get`, `http-head`, `http-post`, `http-put`, `http-patch`, `http-delete` and
`http-options` each open a connection, send one request, read the response and
hang up. They take the keywords of `http-send`:

```scheme
(http-get "http://api.example.com/search"
          #:query (list (cons "q" "a phrase") (cons "page" 2))
          #:headers (list (cons "Accept" "application/json"))
          #:auth (cons "user" "password"))

(http-post "http://example.com/form" #:form (list (cons "name" "value")))

(http-post "http://example.com/upload" #:body (bytes 1 2 3))

;; #:follow is how many redirects to go through; 0 hands the 3xx back instead
(http-get "http://example.com/old" #:follow 0)
```

A response is a struct: `response-version`, `response-status`, `response-reason`,
`response-headers`, `response-body` (always a bytevector), plus `response-ok?`,
`response-text`, `response-header`, `response-header*` and
`response-keep-alive?`.

## One connection, several requests

```scheme
(call-with-http-connection "http://example.com"
  (lambda (connection)
    (define first (http-exchange connection (make-request "GET" "http://example.com/a") #:keep-alive #t))
    (if (response-keep-alive? first)
        (http-exchange connection (make-request "GET" "http://example.com/b") #:keep-alive #t)
        first)))
```

`http-read-response` reads a response straight off any input port, which is
what to reach for when something else owns the socket.

## https

Tls is not in the interpreter, so it comes from the `steel-rustls` dylib:

```sh
cargo install --path crates/cargo-steel-lib
cd libs/steel-rustls && cargo steel-lib
```

The dylib is loaded on the first https request rather than when the module is
required, so requiring `tls.scm` does not need it installed.

```scheme
(require "steel/http/client.scm")
(require "steel/http/tls.scm")

(parameterize ([current-http-connector https-connector])
  (response-text (http-get "https://example.com/")))
```

`https-connector` speaks both schemes, so a redirect from one to the other is
followed, and it takes the same `#:read-timeout` as `tcp-connector`:

```scheme
(parameterize ([current-http-connector
                (lambda (url) (https-connector url #:read-timeout 5000))])
  (response-text (http-get "https://example.com/")))
```

The deadline is set on the socket before the tls stream is built on it, so it
covers the handshake as well. Since rustls shakes hands on the first write, a
server that accepts the connection and then never answers the ClientHello is
reported from the write rather than from the read: `unable to write string to
port: operation would block`.

The same dylib provides `gzip-decode`, for when you asked for
`Accept-Encoding: gzip`.

## The server

```scheme
(require "steel/http/server.scm")

(define server (http-listen "127.0.0.1:8080"))

(http-serve server
            (lambda (sent)
              (if (equal? (request-target sent) "/")
                  (make-response 200 #:body "hello world")
                  (make-response 404 #:body "no such thing"))))
```

A handler takes a `request?` and returns a `response?`. Anything else it
returns, or anything it raises, becomes a 500, and what it raised stays on this
side of the wire.

A port of `0` asks the os for a free one and `server-address` reports which,
so a test can bind a server without picking a port:

```scheme
(define server (http-listen "127.0.0.1:0"))
(server-address server) ;; => "127.0.0.1:54321"
```

A request is read as far as the spec says it should be: `Content-Length` and
chunked bodies with their trailers, obs-fold continuation lines, absolute
request targets (what a proxy is asked for), `OPTIONS *`, an HTTP/1.0 request
with no `Host`, and `Expect: 100-continue`, which gets its go ahead written out
before the body is read. Keep-alive follows the version, and a connection is
held open only when the request, the answer and `#:keep-alive` all agree.

A request this server cannot read is answered and the connection ends, because
a half read request leaves the stream at an unknown place: 400 for a request
line or a body that will not parse, 505 for a version other than HTTP/1.1 or
1.0, 501 for a transfer encoding other than chunked. Two `Content-Length`
headers that disagree, a chunked body with a `Content-Length` beside it and two
`Host` headers are each a 400 rather than a guess.

`http-serve-connection` is the part underneath, and it works on any pair of
ports, which is how most of the tests run:

```scheme
(define writer (open-output-string))
(http-serve-connection (open-input-string "GET /a HTTP/1.1\r\nHost: x\r\n\r\n")
                       writer
                       (lambda (sent) (make-response 200 #:body "hi")))
(get-output-string writer) ;; => "HTTP/1.1 200 OK\r\n..."
```

`http-serve` takes `#:connections` to stop after that many, `#:requests` to
bound one connection, and `http-listen` takes the `#:read-timeout` that
`tcp-connector` does, which it puts on every connection it accepts.

### One request at a time

`http-serve` accepts a connection, serves it, hangs up, and only then looks at
the next one. There is no thread per connection, because handing an accepted
socket to another thread does not work on every build of Steel: two ports that
cross into two spawned threads can collapse into one, so one of them receives
both writes and the other receives none, and the port they collapse onto can be
stdout. `tests/port_threads.rs` in the interpreter has that on its own, with no
http in it. A listener may cross into one thread and accept there, which is
enough to run a server in the background of a program that is doing something
else:

```scheme
(define server (http-listen "127.0.0.1:0"))
(spawn-native-thread (lambda () (http-serve server handler)))
```

Where that port bug is present, a client and a server in the same process
exchange a couple of requests and then stall, so the tests that put the two
together drive them by hand in one thread and the real end to end check runs
them as two processes.

## Putting something else underneath

`current-http-connector` is handed a url and returns a `transport?`, which is an
input port, an output port and a thunk that hangs up. Anything that can produce
two ports can go there - tls, a unix socket, or a canned reply in a test:

```scheme
(parameterize ([current-http-connector
                (lambda (url)
                  (make-transport (open-input-string "HTTP/1.1 204 No Content\r\n\r\n")
                                  (open-output-string)))])
  (response-status (http-get "http://wherever/")))
```

## The other modules

- `message.scm` - what the client and the server share: the `request?` and
  `response?` structs with `make-request` and `make-response`, and the reading
  of a head and a body off a port
- `url.scm` - `parse-url`, `url->string`, `url-join`, `url-encode`,
  `url-decode`, `alist->query`, `query->alist`
- `headers.scm` - case insensitive header lists: `header-ref`, `header-ref*`,
  `header-set`, `header-add`, `header-default`, `header-remove`
- `base64.scm` - `base64-encode`, `base64-decode`

Requiring `client.scm` or `server.scm` brings the message module with it, so a
request a server read can be sent by the client and a response the client read
can be served - they are the same two structs.

## What it does not do

- **No timeout unless one is asked for.** By default a server that accepts a
  connection and then says nothing holds the calling thread. `tcp-connector`
  takes a `#:read-timeout` in milliseconds to put a stop to that:

  ```scheme
  (parameterize ([current-http-connector
                  (lambda (url) (tcp-connector url #:read-timeout 5000))])
    (http-get "http://127.0.0.1:8080/"))
  ```

  A read that reaches the deadline raises out of `http-get`, so the thread comes
  back. The deadline covers a single wait rather than the whole response, so a
  server that keeps trickling bytes in can still outlast it, and there is no
  connect timeout at all. `https-connector` and `tls-connector` take the same
  keyword.
- **No cookie jar, no proxies, no connection pool.** `Set-Cookie` is readable
  with `response-header*`, and a connection can be reused by hand.
- **No compression** unless the `steel-rustls` dylib is installed; requests ask
  for `Accept-Encoding: identity` so that a response is never one the client
  cannot read.
- **Sends a body with a `Content-Length`,** never chunked, on both sides.
  Chunked *bodies* are read, trailers and all.
- **No routing, no static files, no TLS on the server side.** A handler is a
  procedure, and dispatch is whatever `cond` you write in it.
- **The server reads a bounded request, the client an unbounded response.** A
  request is held to `*max-header-lines*` lines of `*max-line-length*`
  characters each and a body of `*max-body-bytes*`, which is answered with a
  413 rather than read. A response is as long as the server says it is, since
  that is a server the caller picked.
