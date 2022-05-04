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

;; TODO: Make the core environment built up over the course of the macros
;; wrap core to make sure that the core things don't get overwritten
;; And put each of the modules in their own modules, exposing the syntax correctly as well