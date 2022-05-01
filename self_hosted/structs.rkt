
(provide
 struct-name
 struct-transparent?
 struct-printer)

(define/contract (struct-name s)
  (->/c custom-struct? symbol?)
  (mut-vector-ref s 1))

(define/contract (struct-transparent? s)
  (->/c custom-struct? boolean?)
  (hash-try-get (mut-vector-ref s 2) ':transparent))

(define/contract (struct-printer s)
  (->/c custom-struct? (or/c function? boolean?))
  (hash-try-get (mut-vector-ref s 2) ':printer))

;; Get documentation for a function on demand - query for a contract
