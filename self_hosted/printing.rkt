



(define/contract (struct-name s)
  (->/c custom-struct? symbol?)
  (mut-vector-ref s 1))


(define (displayln x . rest)
  (display x)
  (transduce rest
             (into-for-each (lambda (x) (display " ")
                              (display x))))
  (newline))

(displayln "hello" "world" "this" "is" "multiple" "values")


(define (println x)
  (if (custom-struct? x)
      (string-append "<"
                     (string-append
                      (symbol->string (struct-name x))
                     ">"))
      (displayln x)))


(make-struct Applesauce (a b c))

(println (Applesauce 1 2 3))
