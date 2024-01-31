(#%require-dylib "libsteel_sqlite"
                 (only-in open-in-memory
                          prepare
                          execute
                          query
                          begin/transaction
                          transaction/finish
                          transaction/commit
                          transaction/try-commit
                          transaction/rollback
                          transaction/try-finish
                          SqliteConnection?
                          SqliteTransaction?
                          SqlitePreparedStatement?
                          open))

(provide SqliteConnection?
         SqliteTransaction?
         SqlitePreparedStatement?
         open-in-memory
         open
         (contract/out prepare (->/c SqliteConnection? string? any/c))
         (contract/out execute (->/c SqlitePreparedStatement? list? any/c))
         (contract/out query (->/c SqlitePreparedStatement? list? list?))
         (contract/out begin/transaction (->/c SqliteConnection? SqliteTransaction?))
         (contract/out transaction/finish (->/c SqliteTransaction? any/c))
         (contract/out transaction/commit (->/c SqliteTransaction? any/c))
         (contract/out transaction/rollback (->/c SqliteTransaction? any/c))
         (contract/out transaction/try-finish (->/c SqliteTransaction? any/c))
         (contract/out run-transaction
                       (->/c SqliteConnection? (->/c SqliteTransaction? any/c) any/c)))

; (define connection (open-in-memory))

; (let ([prepared-statement
;        (prepare
;         connection
;         "CREATE TABLE IF NOT EXISTS person (
;         id   INTEGER PRIMARY KEY,
;         name TEXT NOT NULL,
;         data TEXT
;     )")])

;   (execute prepared-statement '()))

; (define insert-statement (prepare connection "INSERT INTO person (name, data) VALUES (?1, ?2)"))

; (define (insert-data _)
;   (execute
;    insert-statement
;    (list (list "Steven" "likes to eat") (list "Alex" "likes biking") (list "Matt" "likes running"))))

; ; ;; Takes about 0.5 seconds, which seems pretty acceptable!
; ; ; (transduce (range 0 100000) (into-for-each insert-data))

; (define read-statement (prepare connection "Select id, name, data FROM person LIMIT 100"))

; (transduce (range 0 100000) (into-for-each (lambda (_) (query read-statement '()))))

;;@doc
;; Run the thunk with the transaction, catching any exceptions
;; and closes the transaction on the way out. By default, this will attempt
;; to commit the transaction if the body of the function runs successfully.
;;
;; If there are any exceptions, the transaction will be aborted.
;;
;; (->/c SqliteConnection? (->/c SqliteTransaction? any/c) any/c)
(define (run-transaction connection thunk)
  (define transaction (begin/transaction connection))
  (dynamic-wind (lambda () void)
                (lambda ()
                  (thunk transaction)
                  (transaction/try-commit transaction))
                (lambda () (transaction/try-finish transaction))))

; (define read-statement (prepare connection "Select id, name, data FROM person"))

; (displayln (query read-statement '()))

; (connection/execute!
;  connection
; "CREATE TABLE person (
;        id   INTEGER PRIMARY KEY,
;        name TEXT NOT NULL,
;        data TEXT
;    )"
;  '())

; (connection/prepare-and-execute!
;  connection
;  "INSERT INTO person (name, data) VALUES (?1, ?2)"
;  (list (list "Steven" "likes to eat") (list "Alex" "likes biking") (list "Matt" "likes running")))

; (time! (connection/prepare-and-query! connection "SELECT id, name, data FROM person" '()))
