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
### **bytes->string/utf-8**
Alias of `bytes->string/utf8`.
### **bytes->string/utf8**
Decodes a string from a bytevector containing valid UTF-8. Raises if the
bytes are not valid UTF-8, unless `err-char` is given to stand in for them.
Reading the same bytes from a port substitutes the replacement character
rather than raising.

(bytes->string/utf8 buf [err-char] [start] [end]) -> string?

* buf : bytes?
* err-char : (or/c char? #f) = #f
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
### **bytes-clear!**
Removes all bytes from the given bytevector, leaving it empty.

(bytes-clear! vector)

* vector : bytes?

#### Examples
```scheme
(define my-bytes (bytes 0 1 2 3))
(bytes-clear! my-bytes)
my-bytes ;; => #u8()
```
### **bytes-copy**
Creates a copy of a bytevector.

(bytevector-copy vector [start end]) -> bytes?

* vector : bytes?
* start: int? = 0
* end: int? = (bytes-length vector)

#### Examples

```scheme
(define vec (bytes 1 2 3 4 5))

(bytes-copy vec) ;; => (bytes 1 2 3 4 5)
(bytes-copy vec 1 3) ;; => (bytes 2 3)
```
### **bytes-copy!**
Copies the bytes of `from` into `to`, starting at index `at`. The two can be the same
bytevector, and the ranges are allowed to overlap.

(bytevector-copy! to at from [start end])

* to : bytes?
* at : int?
* from : bytes?
* start : int? = 0
* end : int? = (bytes-length from)

#### Examples

```scheme
(define vec (bytes 0 1 2 3 4))

(bytevector-copy! vec 1 (bytes 10 20)) ;; vec => (bytes 0 10 20 3 4)
(bytevector-copy! vec 0 vec 3 5) ;; vec => (bytes 3 4 20 3 4)
```
### **bytes-length**
Returns the length of the given byte vector

#### Examples
```scheme
(bytes-length (bytes 1 2 3 4 5)) ;; => 5
```
### **bytes-push!**
Appends a byte to the end of the given bytevector, growing it in place.

(bytes-push! vector byte)

* vector : bytes?
* byte : byte?

#### Examples
```scheme
(define my-bytes (bytes 0 1 2))
(bytes-push! my-bytes 3)
my-bytes ;; =>  #u8(#x00 #x01 #x02 #x03)
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
### **bytevector-append**
Alias of `bytes-append`.
### **bytevector-copy**
Alias of `bytes-copy`.
### **bytevector-copy!**
Alias of `bytes-copy!`.
### **bytevector-f32-ref**
Reads a 32-bit floating point number from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 4 bytes starting at offset
`index * 4`; if that range is out of bounds, this will error.

(bytevector-f32-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 4 0))
(bytevector-f32-set! vec 0 1.5)
(bytevector-f32-ref vec 0) ;; => 1.5
```
### **bytevector-f32-set!**
Writes a 32-bit floating point number into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 4 bytes starting at offset `index * 4`;
will error if the index is out of bounds.

(bytevector-f32-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : real?

#### Examples
```scheme
(define vec (make-bytes 4 0))
(bytevector-f32-set! vec 0 1.5)
(bytevector-f32-ref vec 0) ;; => 1.5
```
### **bytevector-f64-ref**
Reads a 64-bit floating point number from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 8 bytes starting at offset
`index * 8`; if that range is out of bounds, this will error.

(bytevector-f64-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 8 0))
(bytevector-f64-set! vec 0 1.5)
(bytevector-f64-ref vec 0) ;; => 1.5
```
### **bytevector-f64-set!**
Writes a 64-bit floating point number into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 8 bytes starting at offset `index * 8`;
will error if the index is out of bounds.

(bytevector-f64-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : real?

#### Examples
```scheme
(define vec (make-bytes 8 0))
(bytevector-f64-set! vec 0 1.5)
(bytevector-f64-ref vec 0) ;; => 1.5
```
### **bytevector-length**
Alias of `bytes-length`.
### **bytevector-s16-ref**
Reads a 16-bit signed (two's complement) integer from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 2 bytes starting at offset
`index * 2`; if that range is out of bounds, this will error.

(bytevector-s16-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 2 0))
(bytevector-s16-set! vec 0 -1000)
(bytevector-s16-ref vec 0) ;; => -1000
```
### **bytevector-s16-set!**
Writes a 16-bit signed (two's complement) integer into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 2 bytes starting at offset `index * 2`;
will error if the index is out of bounds.

(bytevector-s16-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 2 0))
(bytevector-s16-set! vec 0 -1000)
(bytevector-s16-ref vec 0) ;; => -1000
```
### **bytevector-s32-ref**
Reads a 32-bit signed (two's complement) integer from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 4 bytes starting at offset
`index * 4`; if that range is out of bounds, this will error.

(bytevector-s32-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 4 0))
(bytevector-s32-set! vec 0 -100000)
(bytevector-s32-ref vec 0) ;; => -100000
```
### **bytevector-s32-set!**
Writes a 32-bit signed (two's complement) integer into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 4 bytes starting at offset `index * 4`;
will error if the index is out of bounds.

(bytevector-s32-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 4 0))
(bytevector-s32-set! vec 0 -100000)
(bytevector-s32-ref vec 0) ;; => -100000
```
### **bytevector-s64-ref**
Reads a 64-bit signed (two's complement) integer from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 8 bytes starting at offset
`index * 8`; if that range is out of bounds, this will error.

(bytevector-s64-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 8 0))
(bytevector-s64-set! vec 0 -1000000)
(bytevector-s64-ref vec 0) ;; => -1000000
```
### **bytevector-s64-set!**
Writes a 64-bit signed (two's complement) integer into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 8 bytes starting at offset `index * 8`;
will error if the index is out of bounds.

(bytevector-s64-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 8 0))
(bytevector-s64-set! vec 0 -1000000)
(bytevector-s64-ref vec 0) ;; => -1000000
```
### **bytevector-s8-ref**
Reads an 8-bit signed integer from the bytevector at the
given index. Unlike the wider variants, the index addresses a single byte
directly (offset `index`). If the index is out of bounds, this will error.

(bytevector-s8-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 1 0))
(bytevector-s8-set! vec 0 -100)
(bytevector-s8-ref vec 0) ;; => -100
```
### **bytevector-s8-set!**
Writes an 8-bit signed integer into the bytevector at the
given index. Unlike the wider variants, the index addresses a single byte
directly (offset `index`). Will error if the index is out of bounds.

(bytevector-s8-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 1 0))
(bytevector-s8-set! vec 0 -100)
(bytevector-s8-ref vec 0) ;; => -100
```
### **bytevector-u16-ref**
Reads a 16-bit unsigned integer from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 2 bytes starting at offset
`index * 2`; if that range is out of bounds, this will error.

(bytevector-u16-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 2 0))
(bytevector-u16-set! vec 0 1000)
(bytevector-u16-ref vec 0) ;; => 1000
```
### **bytevector-u16-set!**
Writes a 16-bit unsigned integer into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 2 bytes starting at offset `index * 2`;
will error if the index is out of bounds.

(bytevector-u16-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 2 0))
(bytevector-u16-set! vec 0 1000)
(bytevector-u16-ref vec 0) ;; => 1000
```
### **bytevector-u32-ref**
Reads a 32-bit unsigned integer from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 4 bytes starting at offset
`index * 4`; if that range is out of bounds, this will error.

(bytevector-u32-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 4 0))
(bytevector-u32-set! vec 0 100000)
(bytevector-u32-ref vec 0) ;; => 100000
```
### **bytevector-u32-set!**
Writes a 32-bit unsigned integer into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 4 bytes starting at offset `index * 4`;
will error if the index is out of bounds.

(bytevector-u32-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 4 0))
(bytevector-u32-set! vec 0 100000)
(bytevector-u32-ref vec 0) ;; => 100000
```
### **bytevector-u64-ref**
Reads a 64-bit unsigned integer from the bytevector at the given element
index, interpreting the bytes using the platform's native byte order
(endianness). The value occupies the 8 bytes starting at offset
`index * 8`; if that range is out of bounds, this will error.

(bytevector-u64-ref vector index)

* vector : bytes?
* index : (and exact? int?)

#### Examples
```scheme
(define vec (make-bytes 8 0))
(bytevector-u64-set! vec 0 1000000)
(bytevector-u64-ref vec 0) ;; => 1000000
```
### **bytevector-u64-set!**
Writes a 64-bit unsigned integer into the bytevector at the given element
index, storing the bytes using the platform's native byte order (endianness).
The value occupies the 8 bytes starting at offset `index * 8`;
will error if the index is out of bounds.

(bytevector-u64-set! vector index value)

* vector : bytes?
* index : (and exact? int?)
* value : int?

#### Examples
```scheme
(define vec (make-bytes 8 0))
(bytevector-u64-set! vec 0 1000000)
(bytevector-u64-ref vec 0) ;; => 1000000
```
### **bytevector-u8-ref**
Alias of `bytes-ref`.
### **bytevector-u8-set!**
Alias of `bytes-set!`.
### **bytevector?**
Alias of `bytes?`.
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
### **make-bytevector**
Alias of `make-bytes`.
### **utf8->string**
Alias of `bytes->string/utf8`.
