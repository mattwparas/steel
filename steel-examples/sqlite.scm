(require-builtin steel/sqlite)
(require "steel/result")

(require "steel/time/time.scm"
         (for-syntax "steel/time/time.scm"))

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

(time! (connection/prepare-and-query! connection "SELECT id, name, data FROM person" '()))
