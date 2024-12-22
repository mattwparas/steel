# steel/sets
### **hashset**
Constructs a new hash set

#### Examples
```scheme
(hashset 10 20 30 40)
```
### **hashset->immutable-vector**
Creates an immutable vector from this hashset. The order of the vector is not guaranteed.

#### Examples
```scheme
(hashset->immutable-vector (hashset 10 20 30)) ;; => '#(10 20 30)
(hashset->immutable-vector (hashset 10 20 30)) ;; => '#(20 10 30)
```
### **hashset->list**
Creates a list from this hashset. The order of the list is not guaranteed.

#### Examples
```scheme
(hashset->list (hashset 10 20 30)) ;; => '(10 20 30)
(hashset->list (hashset 10 20 30)) ;; => '(20 10 30)
```
### **hashset->vector**
Creates a mutable vector from this hashset. The order of the vector is not guaranteed.

#### Examples
```scheme
(hashset->vector (hashset 10 20 30)) ;; => '#(10 20 30)
(hashset->vector (hashset 10 20 30)) ;; => '#(20 10 30)
```
### **hashset-clear**
Clears the hashset and returns the passed in hashset.
This first checks if there are no other references to this hashset,
and if there aren't, clears that allocation. Given that there are
functional updates, this is only relevant if there are no more
references to a given hashset, and you want to reuse its allocation.
### **hashset-contains?**
Test if the hashset contains a given element.

#### Examples
```scheme
(hashset-contains? (hashset 10 20) 10) ;; => #true
(hashset-contains? (hashset 10 20) "foo") ;; => #false
```
### **hashset-insert**
Insert a new element into the hashset. Returns a hashset.

#### Examples
```scheme
(define hs (hashset 10 20 30))
(define updated (hashset-insert hs 40))
(equal? hs (hashset 10 20 30)) ;; => #true
(equal? updated (hashset 10 20 30 40)) ;; => #true
```
### **hashset-length**
Get the number of elements in the hashset

#### Examples
```scheme
(hashset-length (hashset 10 20 30)) ;; => 3
```
### **hashset-subset?**
Check if the left set is a subset of the right set

#### Examples
```scheme
(hashset-subset? (hash 10) (hashset 10 20)) ;; => #true
(hashset-subset? (hash 100) (hashset 30)) ;; => #false
```
### **list->hashset**
Convert the given list into a hashset.

#### Examples
```scheme
(list 10 20 30) ;; => (hashset 10 20 30)
```
