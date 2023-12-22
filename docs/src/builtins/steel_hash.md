# steel/hash
### **hash-contains?**
Checks whether the given map contains the given key. Key must be hashable.

(hash-contains? map key) -> bool?

* map : hash?
* key : hashable?

#### Example

```scheme
> (hash-contains? (hash 'a 10 'b 20) 'a) ;; => #true
> (hash-contains? (hash 'a 10 'b 20) 'not-there) ;; => #false
```
### **hash-insert**
Returns a new hashmap with the additional key value pair added. Performs a functional update,
so the old hash map is still accessible.

(hash-insert map key val) -> hash?

* map : hash?
* key : any/c
* val : any/c

#### Examples
```scheme
> (hash-insert (hash 'a 10 'b 20) 'c 30)

=> #<hashmap {
'a: 10,
'b: 20,
'c: 30
}>
```
### **hash-keys->list**
Returns the keys of the given hash map as a list.

```scheme
(hash-keys->list map) -> (listof hashable?)
```

* map : hash?

#### Examples

```scheme
> (hash-keys->list? (hash 'a 'b 20)) ;; => '(a b)
```
### **hash-length**
Returns the number of key value pairs in the map

(hash-length map) -> (and positive? int?)

* map : hash?

#### Examples

```scheme
> (hash-length (hash 'a 10 'b 20)) ;; => 2
```
### **hash-ref**
Gets the `key` from the given `map`. Raises an error if the key does not exist. `hash-get` is an alias for this.

(hash-ref map key) -> any/c

* map : hash?
* key : any/c

#### Examples
```scheme
> (hash-ref (hash 'a 10 'b 20) 'b) ;; => 20
```
### **hash-try-get**
Gets the `key` from the given `map`. Returns #false if the key does not exist.

(hash-try-get map key) -> (or any/c #false)

* map : hash?
* key : any/c

#### Examples

```scheme
> (hash-try-get (hash 'a 10 'b 20) 'b) ;; => 20
> (hash-try-get (hash 'a 10 'b 20) 'does-not-exist) ;; => #false
```
### **%keyword-hash**
### **hash-clear**
### **hash-empty?**
### **hash-get**
### **hash-keys->vector**
### **hash-union**
### **hash-values->vector**
