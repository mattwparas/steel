(define package-name 'steel/repl)
(define version "0.1.0")

;; Core library, requires no dependencies
(define dependencies '())

;; Entrypoint in this case is a client that can connect
;; to a repl server?
(define entrypoint '(#:name "repl-connect" #:path "repl-client.scm"))
