



(define/contract (struct-name s)
  (->/c custom-struct? symbol?)
  (mut-vector-ref s 1))


; (define (displayln . rest)
;   (transduce rest
;              (into-for-each (lambda (x) (display " "))))
;   (newline))

; (displayln "hello" "world" "this" "is" "multiple" "values")


(define (println x)
  (displayln 
    (if (custom-struct? x)
      (string-append "#<"
                     (string-append
                      (symbol->string (struct-name x))
                     ">"))
      x)))


(struct Applesauce (a b c))

(println (Applesauce 1 2 3))
