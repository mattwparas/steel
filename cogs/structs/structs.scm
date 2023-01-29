

(define (json->struct struct-descriptor struct-constructor json)
    (define fields (hash-get struct-descriptor '#:fields))
    (apply struct-constructor (map (lambda (field) (hash-try-get json field)) fields)))

(struct Applesauce (a b c) #:transparent)

(json->struct ___Applesauce-options___ Applesauce (hash 'a 10 'b 20 'c 30))