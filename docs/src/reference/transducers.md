# Transducers

Inspired by clojure's transducers, `Steel` has a similar object that is somewhere half way in between transducers and iterators. Consider the following:

```scheme

(mapping (lambda (x) (+ x 1))) ;; => <#iterator>
(filtering even?) ;; => <#iterator>
(taking 15) ;; => <#iterator>

(compose 
    (mapping add1)
    (filtering odd?)
    (taking 15)) ;; => <#iterator>
```

Each of these expressions emit an `<#iterator>` object, which means they're compatible with `transduce`. Transduce takes a collection that can be iterated (`list`, `vector`, or `stream`), a transducer (i.e. `<#iterator>`) and a reducer, which dictates how the element are combined into a result value.

```scheme
;; Accepts lists
(transduce (mapping (lambda (x) (+ x 1))) (list 1 2 3 4 5) (into-list)) ;; => '(2 3 4 5 6)

;; Accepts vectors
(transduce (vector 1 2 3 4 5) (mapping (lambda (x) (+ x 1))) (into-vector)) ;; '#(2 3 4 5 6)

;; Even accepts streams!
(define (integers n)
    (stream-cons n (lambda () (integers (+ 1 n)))))

(transduce (integers 0) (taking 5) (into-list)) ;; => '(0 1 2 3 4)
```

Compose just combines the iterator functions and lets us avoid intermediate allocation. The composition works left to right - it chains each value through the functions and then accumulates into the output type. See the following:

```scheme
(define xf 
    (compose 
        (mapping add1)
        (filtering odd?)
        (taking 5)))

(transduce (range 0 100) xf (into-list)) ;; => '(1 3 5 7 9)
```
