(provide println)


(define/contract (struct-name s)
  (->/c custom-struct? symbol?)
  (mut-vector-ref s 1))

(define/contract (struct-transparent? s)
  (->/c custom-struct? boolean?)
  (hash-try-get (mut-vector-ref s 2) ':transparent))

(define (println x)
  (displayln 
    (if (custom-struct? x)
      (if (struct-transparent? x)
          (cons (struct-name x) (transduce x (into-list)))
          (string-append "#<"
                        (string-append
                          (symbol->string (struct-name x))
                        ">")))
      x)))