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

Each of these expressions emit an `<#iterator>` object, which means they're compatible with `execute` and `transduce`. Execute takes a transducer (i.e. `<#iterator>`) and a collection that can be iterated (`list`, `vector`, or `stream`) and applies the transducer.

```scheme
;; Accepts lists
(execute (mapping (lambda (x) (+ x 1))) (list 1 2 3 4 5)) ;; => '(2 3 4 5 6)

;; Accepts vectors
(execute (mapping (lambda (x) (+ x 1))) (vector 1 2 3 4 5)) ;; '#(2 3 4 5 6)

;; Even accepts streams!
(define (integers n)
    (stream-cons n (lambda () (integers (+ 1 n)))))

(execute (taking 5) (integers 0)) ;; => '(0 1 2 3 4)
```

Transduce is just `reduce` with more bells and whistles and works similarly:

```scheme
;; (-> transducer reducing-function initial-value iterable)
(transduce (mapping (lambda (x) (+ x 1))) + 0 (list 0 1 2 3)) ;; => 10
```

Compose just combines the iterator functions and lets us avoid intermediate allocation. The composition works left to right - it chains each value through the functions and then accumulates into the output type. See the following:

```scheme
(define xf 
    (compose 
        (mapping add1)
        (filtering odd?)
        (taking 5)))

(execute xf (range 0 100)) ;; => '(1 3 5 7 9)
```

By default, execute outputs to the same type that was passed in. In other words, if you `execute` a `list`, it will return a `list`. However, if you so choose, you can pass in a symbol specifying the output type of your choosing like so:

```scheme
(define xf 
    (compose 
        (mapping add1)
        (filtering odd?)
        (taking 5)))

;; Takes a list and returns a vector
(execute xf (range 0 100) 'vector) ;; => '#(1 3 5 7 9)

;; Takes a vector and returns a list
(execute xf (vector 0 1 2 3 4 5 6 7 8 9 10) 'list) ;; => '(1 3 5 7 9)
```