



(define (json-parse json-str)
  (chars json-str))




(chars json-str) ;; => iterator over the characters of the stream


(define (parse-atom-value atom)
  (cond [(equal? "true") #true]
        [(equal? "false") #false]
        [(list? )]))
