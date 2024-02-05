(#%require-dylib "libsteel_sqlite"
                 (only-in sqlite/open-in-memory
                          sqlite/prepare
                          sqlite/execute
                          sqlite/query
                          sqlite/begin/transaction
                          sqlite/transaction/finish
                          sqlite/transaction/commit
                          sqlite/transaction/try-commit
                          sqlite/transaction/rollback
                          sqlite/transaction/try-finish
                          sqlite/SqliteConnection?
                          sqlite/SqliteTransaction?
                          sqlite/SqlitePreparedStatement?
                          sqlite/open))

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

;;@doc
;; Prepares a sqlite statement for further use.
(define prepare sqlite/prepare)

;;@doc
;; Opens an in-memory sqlite database
(define open-in-memory sqlite/open-in-memory)

;;@doc
;; Execute a sqlite statement with a list of parameters, without returning any rows.
(define execute sqlite/execute)

;;@doc
;; Run a sqlite statement with a list of parameters, returning the rows found.
(define query sqlite/query)

;;@doc
;; Start a sqlite transaction
(define begin/transaction sqlite/begin/transaction)

;;@doc
;; Mark a transaction as finished. This will default to the behavior specified
;; for when the transaction goes out of scope, which in this case would be to
;; roll back.
(define transaction/finish sqlite/transaction/finish)

;;@doc
;; Commit a transaction. If the transaction has already been committed, this will
;; raise an exception.
(define transaction/commit sqlite/transaction/commit)

;;@doc
;; Attempts to commit a transaction. If the transaction has already been finished, this
;; will do nothing.
(define transaction/try-commit sqlite/transaction/try-commit)

;;@doc
;; Roll back a transaction.
(define transaction/rollback sqlite/transaction/rollback)

;;@doc
;; Attempt to finish a transaction. If the transaction has been already been finished,
;; this will do nothing.
(define transaction/try-finish sqlite/transaction/try-finish)

;;@doc
;; Test if the value is a `SqliteConnection`.
(define SqliteConnection? sqlite/SqliteConnection?)

;;@doc
;; Test if the value is a `SqliteTransaction`
(define SqliteTransaction? sqlite/SqliteTransaction?)

;;@doc
;; Test if the value is a `SqlitePreparedStatement`
(define SqlitePreparedStatement? sqlite/SqlitePreparedStatement?)

;;@doc
;; Open a sqlite transaction against a given path.
(define open sqlite/open)

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
