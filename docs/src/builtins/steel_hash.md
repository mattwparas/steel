# steel/hash
### **hash**
Creates an immutable hash table with each given `key` mapped to the following `val`.
Each key must have a val, so the total number of arguments must be even.


(hash key val ...) -> hash?

key : hashable?
val : any/c

Note: the keys must be hashable.

#### Examples
```scheme
> (hash 'a 10 'b 20)",
r#"=> #<hashmap {
'a: 10,
'b: 20,
}>"#,
```
### **hash-clear**
Clears the entries out of the existing hashmap.
Will attempt to reuse the existing memory if there are no other references
to the hashmap.

(hash-clear h) -> hash?

h: hash?

#### Examples
```scheme
> (hash-clear (hash 'a 10 'b 20))
=> '#hash()
```
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
### **hash-empty?**
Checks whether the hash map is empty

(hash-empty? m) -> bool?

m: hash?

#### Examples
```scheme
> (hash-empty? (hash 'a 10)) ;; => #f
> (hash-emptY? (hash)) ;; => #true
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
### **hash-keys->vector**
Returns the keys of the given hash map as an immutable vector

(hash-keys->vector map) -> (vectorof any/c)?

map: hash?

#### Examples
```scheme
> (hash-keys->vector (hash 'a 10 'b 20)),
=> ['a 'b]",
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
### **hash-union**
Constructs the union of two hashmaps, keeping the values
in the left map when the keys exist in both maps.

Will reuse memory where possible.

(hash-union l r) -> hash?

#### Examples
```scheme
> (hash-union (hash 'a 10) (hash 'b 20)) ;; => '#hash((a . 10) (b . 20))
```
### **hash-values->list**
Returns the values of the given hash map as a list

(hash-values->list? map) -> (listof any/c)?

map: hash?

#### Examples
```scheme
> (hash-values->list? (hash 'a 10 'b 20)),
=> '(10 20)",
```
### **hash-values->vector**
Returns the values of the given hash map as an immutable vector

(hash-values->vector map) -> (vectorof any/c)?

map: hash?

#### Examples
```scheme
> (hash-keys->vector (hash 'a 10 'b 20)),
=> [10 10]",
```
### **%keyword-hash**
### **hash-get**
