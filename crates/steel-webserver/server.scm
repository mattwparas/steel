(require-builtin dylib/steel/webserver)
(require-builtin steel/sqlite)
(require "steel/result")


(define connection (unwrap-ok (connection/open-in-memory)))

(connection/execute! connection
    "CREATE TABLE person (
        id   INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        data TEXT
    )" '())

(connection/prepare-and-execute! connection
    "INSERT INTO person (name, data) VALUES (?1, ?2)"
    (list (list "Steven" "likes to eat")
          (list "Alex" "likes biking")
          (list "Matt" "likes running")))

(define (get-people)
    (map (lambda (x)
            (hash "name" (list-ref x 1)
                  "data" (list-ref x 2)))
         (unwrap-ok (connection/prepare-and-query! connection "SELECT id, name, data FROM person" '()))))


(define vm-receiver void)
(define vm-sender void)
(define command-channel void)

;; We don't need the extra garbage laying around, so we'll just go ahead
;; and unroot these channels here
(let ((channels (setup-channels)))
    (set! vm-sender (first channels))
    (set! vm-receiver (second channels))
    (set! command-channel (third channels)))


(define (hello-world-handler)
    (value->jsexpr-string 
            (hash 'hello "world")))

(define (people-handler)
    (value->jsexpr-string (get-people)))

(define *routes*
    (hash "hello/world" hello-world-handler
          "people" people-handler)
)

(define (handle-request path)
    ((hash-get *routes* path))


    ; (displayln path)
    ; "hello world"
    )

(define (loop)
    (->> (receiver/recv vm-receiver)
            (handle-request)
            (sender/send vm-sender))

    (loop))


(start-server! command-channel)

(loop)