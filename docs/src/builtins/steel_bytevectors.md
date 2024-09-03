# steel/bytevectors
### **byte?**
Returns `#t` if the given value is a byte, meaning an exact
integer between 0 and 255 inclusive, `#f` otherwise.

#### Examples
```scheme
(byte? 65) ;; => #t
(byte? 0) ;; => #t
(byte? 256) ;; => #f
(byte? 100000) ;; => #f
(byte? -1) ;; => #f
```
### **bytes**
Returns a new mutable vector with each byte as the given arguments.
Each argument must satisfy the `byte?` predicate, meaning it is an exact
integer range from 0 - 255 (inclusive)

(bytes b ...)

* b : byte?


#### Examples
```scheme
(bytes 65 112 112 108 101)
```
### **bytes->list**
Converts the bytevector to the equivalent list representation.

#### Examples
```scheme
(bytes->list (bytes 0 1 2 3 4 5)) ;; => '(0 1 2 3 4 5)
```
### **bytes->string/utf8**
Decodes a string from a bytevector containing valid UTF-8.

(bytes->string/utf8 buf [start] [end]) -> string?

* buf : bytes?
* start: int? = 0
* end: int? = (bytes-length buf)

#### Examples
```scheme
(bytes->string/utf8 (bytes #xe5 #x8d #x83 #xe8 #x91 #x89)) ;; => "千葉"
```
### **bytes-append**
Append multiple byte vectors into a new bytevector.

#### Examples
```scheme
(bytes-append #u8(0 1 2) #u8(3 4 5)) ;; => #u8(#x00 #x01 #x02 #x03 #x04 #x05)

(bytes-append #u8(0) #u8(1) #u8() #u8(2)) ;; => #u8(#x00 #x01 #x02)
```
### **bytes-length**
Returns the length of the given byte vector

#### Examples
```scheme
(bytes-length (bytes 1 2 3 4 5)) ;; => 5
```
### **bytes-ref**
Fetches the byte at the given index within the bytevector.
If the index is out of bounds, this will error.

(bytes-ref vector index)

* vector : bytes?
* index: (and exact? int?)

#### Examples
```scheme
(bytes-ref (bytes 0 1 2 3 4 5) 3) ;; => 4
(bytes-ref (bytes) 10) ;; error
```
### **bytes-set!**
Sets the byte at the given index to the given byte. Will error
if the index is out of bounds.

(bytes-set! vector index byte)

* vector : bytes?
* index: (and exact? int?)
* byte: byte?

#### Examples
```scheme
(define my-bytes (bytes 0 1 2 3 4 5))
(bytes-set! my-bytes 0 100)
(bytes-ref my-bytes 0) ;; => 100
```
### **bytes?**
Returns `#t` if this value is a bytevector

#### Examples
```scheme
(bytes? (bytes 0 1 2)) ;; => #t
(bytes? (list 10 20 30)) ;; => #f
```
### **bytevector**
Returns a new mutable vector with each byte as the given arguments.
Each argument must satisfy the `byte?` predicate, meaning it is an exact
integer range from 0 - 255 (inclusive)

(bytevector b ...)

* b : byte?


#### Examples
```scheme
(bytevector 65 112 112 108 101)
```
### **bytevector-copy**
Creates a copy of a bytevector.

(bytevector-copy vector [start end]) -> bytes?

* vector : bytes?
* start: int? = 0
* end: int? = (bytes-length vector)

#### Examples

```scheme
(define vec (bytes 1 2 3 4 5))

(bytevector-copy vec) ;; => (bytes 1 2 3 4 5)
(bytevector-copy vec 1 3) ;; => (bytes 2 3)
```
### **list->bytes**
Converts the list of bytes to the equivalent bytevector representation.
The list must contain _only_ values which satisfy the `byte?` predicate,
otherwise this function will error.

#### Examples
```scheme
(list->bytes (list 0 1 2 3 4 5)) ;; => (bytes 0 1 2 3 4 5)
```
### **make-bytes**
Creates a bytevector given a length and a default value.

(make-bytes len default) -> bytes?

* len : int?
* default : byte?

#### Examples
```scheme
(make-bytes 6 42) ;; => (bytes 42 42 42 42 42)
```
### **utf8->string**
Alias of `bytes->string/utf8`.
