; (require-builtin dylib/steel/webserver)
(require-builtin steel/sqlite)
(require "steel/result")

(#%require-dylib "libsteel_webserver"
                 (only-in start-server!
                          setup-channels
                          receiver/recv
                          thread/join
                          request-path
                          request-type
                          sender/send
                          request-body
                          Response-Ok
                          Response-Err))

(define connection (connection/open-in-memory))

(connection/execute!
 connection
 "CREATE TABLE person (
        id   INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        data TEXT
    )"
 '())

(connection/prepare-and-execute!
 connection
 "INSERT INTO person (name, data) VALUES (?1, ?2)"
 (list (list "Steven" "likes to eat") (list "Alex" "likes biking") (list "Matt" "likes running")))

(define (get-people)
  (map (lambda (x) (hash "name" (list-ref x 1) "data" (list-ref x 2)))
       (unwrap-ok
        (connection/prepare-and-query! connection "SELECT id, name, data FROM person" '()))))

(define (add-person person-hash)
  (connection/prepare-and-execute! connection
                                   "INSERT INTO person (name, data) VALUES (?1, ?2)"
                                   (list (list (hash-get person-hash 'name)
                                               (hash-get person-hash 'data)))))

(define vm-receiver void)
(define vm-sender void)
(define command-channel void)

;; We don't need the extra garbage laying around, so we'll just go ahead
;; and unroot these channels here
(let ([channels (setup-channels)])
  (set! vm-sender (first channels))
  (set! vm-receiver (second channels))
  (set! command-channel (third channels)))

(define (hello-world-handler)
  (value->jsexpr-string (hash 'hello "world")))

(define (people-handler)
  (value->jsexpr-string (get-people)))

(define (add-person-handler person-hash)
  (displayln person-hash)
  (add-person person-hash)
  (value->jsexpr-string person-hash))

(define *routes* (hash "hello/world" hello-world-handler "people" people-handler))

(define *post-routes* (hash "add" add-person-handler))

(define POST 0)
(define GET 1)

(define (handle-request req)
  (define type (request-type req))
  ; (displayln "Path: " (request-path req))
  (cond
    [(= GET type)
     =>
     ((hash-get *routes* (request-path req)))]

    [(= POST type)
     =>
     ((hash-get *post-routes* (request-path req)) (request-body req))]
    [else
     =>
     (displayln "Unknown request type!: " type)]))

(struct WorkerThread (sender thread-handle))

;;@doc
;; Spawn a worker thread
(define (spawn-worker-thread)
  (spawn-thread! (lambda ()
                   ;; Loop and handle requests until we're done!
                   (event-loop)
                   (stdout-simple-displayln "Shutting down thread: " (thread::current/id)))))

;; Setup n threads to send on, round robin?
(define (event-loop)
  (with-handler
   (lambda (err)
     (report-error! err)
     (sender/send vm-sender (Response-Err (to-string err))))
   (->> (receiver/recv vm-receiver) (handle-request) (Response-Ok) (sender/send vm-sender)))

  (event-loop))

(displayln "Routes: " *routes*)

(start-server! command-channel '("/*route") 3000)

;; Try to run this on multiple threads...
(map thread-join! (map (lambda (_) (spawn-worker-thread)) (range 0 10)))
