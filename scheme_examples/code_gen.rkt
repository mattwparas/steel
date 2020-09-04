


(define-syntax hm-help
  (syntax-rules (=>)
    [(hm-help hm key => value rest ...)
     (hm-help (hash-insert hm key value) rest ...)]
    [(hm-help hm key => value)
     (hash-insert hm key value)]))


(define-syntax hashmap!
  (syntax-rules (=>)
    [(hashmap! key => value rest ...)
     (hm-help (hash) key => value rest ...)]
    [(hashmap! key => value)
     (hash-insert (hash) key value)]
    [(hashmap!)
     (hash)]))

(hash
 'a 25
 'b 30
 'c 440
 'applesauce 225
 'blargh 335)

(hashmap!
 'a => 25
 'b => 30
 'c => 440
 'applesauce => 225
 'blargh => 335)


(define x (mapping (fn (x) x))) ;; identity
(define y (filtering even?)) ;; get only even ones
(define z (taking 15)) ;; take the first 15 from the range
(define xf (compose x y z))
(transduce xf + 0 (range 0 100)) ;; => 120




(define-syntax cond
  (syntax-rules (else)
    [(cond [else e1 ...])
     (begin e1 ...)]
    [(cond [e1 e2 ...])
     (when e1 e2 ...)]
    [(cond [e1 e2 ...] c1 ...)
     (if e1
         (begin e2 ...)
         (cond c1 ...))]))
