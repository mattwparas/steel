# steel/transducers
### **compose**
Compose multiple iterators into one iterator

(compose . iters) -> iterator?

#### Examples
```scheme
(compose
(mapping (λ (x) (+ x 1)))
(filtering odd?)
(taking 15))
```
### **dropping**
Creates a taking iterator combinator

(dropping integer?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3 4 5) (dropping 3) (into-list)) ;; => '(4 5)
```
### **enumerating**
Create an enumerating iterator

(enumerating) -> iterator?

#### Examples
```scheme
(transduce (list 1 3 5) (enumerating) (into-list)) ;; => '((0 1) (1 3) (2 5))
```
### **extending**
Create an extending iterator

(extending iterable) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (extending (list 4 5 6 7)) (into-list)) ;; => '(1 2 3 4 5 6 7)
```
### **filtering**
Creates a filtering iterator

(filtering proc?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3 4) (filtering even?) (into-list)) ;; => '(2 4)
```
### **flat-mapping**
Creates a flat-mapping iterator

(flat-mapping proc?) -> iterator

#### Examples
```scheme
(transduce (list 1 2 3) (flat-mapping (λ (x) (range 0 x))) (into-list)) ;; => '(0 0 1 0 1 2)
```
### **flattening**
Creates a flattening iterator that etc

(flattening) -> iterator?

#### Examples
```scheme
(transduce (list '(1 2) '(3 4) '(5 6)) (flattening) (into-list)) ;; => '(1 2 3 4 5 6)
```
### **interleaving**
Create an interleaving iterator

(interleaving any/c) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (interleaving (list 4 5 6)) (into-list)) ;; => '(1 4 2 5 3 6)
```
### **mapping**
Create a mapping iterator

(mapping proc?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (mapping (λ (x) (+ x 1))) (into-list)) ;; => '(2 3 4)
```
### **taking**
Creates a taking iterator combinator

(taking number?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3 4 5) (taking 3) (into-list)) ;; => '(1 2 3)
```
### **zipping**
Create a zipping iterator

(zipping any/c) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (zipping (list 4 5 6 7)) (into-list)) ;; => '((1 4) (2 5) (3 6))
```
### **into-count**
### **into-for-each**
### **into-hashmap**
### **into-hashset**
### **into-last**
### **into-list**
### **into-max**
### **into-min**
### **into-nth**
### **into-product**
### **into-reducer**
### **into-string**
### **into-sum**
### **into-vector**
