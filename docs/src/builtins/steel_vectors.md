# steel/vectors
### **immutable-vector**
Constructs an immutable vector from the given arguments.

(immutable-vector . vals) -> vector?

* vals : any? - The values to store in the immutable vector.

#### Examples
```scheme
> (define V (immutable-vector 1 2 3)) ;;
> V ;; => '#(1 2 3)
```
### **make-vector**
Creates a mutable vector of a given size, optionally initialized with a specified value.

(make-vector size [value]) -> vector?

* size : integer? - The number of elements in the vector (must be non-negative).
* value : any? - The value to fill the vector with (defaults to `0` if omitted).

#### Examples
```scheme
> (make-vector 3) ;; => '#(0 0 0)
> (make-vector 3 42) ;; => '#(42 42 42)
```
### **mut-vec-len**
Returns the length of a mutable vector.

(mut-vec-len vec) -> integer?

* vec : vector? - The mutable vector to retrieve the length of.

#### Examples
```scheme
> (define A (mutable-vector 1 2 3 4 5)) ;;
> (mut-vec-len A) ;; => 5
```
### **mut-vector-ref**
Retrieves the value at a specified index in a mutable vector.

(mut-vector-ref vec index) -> any?

* vec : vector? - The mutable vector from which to retrieve a value.
* index : integer? - The position in `vec` to access (must be within bounds).

#### Examples
```scheme
> (define A (mutable-vector 10 20 30)) ;;
> (mut-vector-ref A 1) ;; => 20
```
### **mutable-vector**
Constructs a new mutable vector from the provided arguments.

(mutable-vector . args) -> vector?

* args : any? - Elements to initialize the mutable vector.

#### Examples
```scheme
> (mutable-vector 1 2 3) ;; => '#(1 2 3)
```
### **mutable-vector->clear**
Removes all elements from a mutable vector.

(mutable-vector->clear vec) -> void?

* vec : vector?

#### Examples
```scheme
> (define A (vector 1 2 3 4)) ;;
> (mutable-vector->clear A) ;;
> A ;; => '#()
```
### **mutable-vector->list**
Converts a mutable vector into a list, optionally over a specified range.

(mutable-vector->list vec [start end]) -> list?

* vec : vector? - The mutable vector to convert.
* start : integer? - The starting index of the range (defaults to `0`).
* end : integer? - The exclusive ending index of the range (defaults to the length of `vec`).

#### Examples
```scheme
> (define A (mutable-vector 1 2 3 4 5)) ;;
> (mutable-vector->list A) ;; => '(1 2 3 4 5)
> (mutable-vector->list A 1 4) ;; => '(2 3 4)
```
### **mutable-vector->string**
Converts a vector of characters into a string.

(mutable-vector->string vec) -> string?

* vec : vector? - (must contain only characters)

#### Examples
```scheme
> (define A (vector #\H #\e #\l #\l #\o))
> (mutable-vector->string A) ;; => "Hello"
```
### **mutable-vector-pop!**
Removes and returns the last element of the vector.

(mutable-vector-pop! vec) -> any?

* vec : vector? - the vector to modify

#### Examples
```scheme
> (define A (vector 1 2 3))
> (mutable-vector-pop! A) ;; => 3
> A ;; => '#(1 2)
```
### **null?**
Checks if the given list or vector is empty.

(null? obj) -> boolean?

* obj : (or/c list? vector?) - The list or vector to check.

#### Examples
```scheme
> (null? (vector)) ;; => #t
> (null? (immutable-vector 1 2 3)) ;; => #f
> (null? '()) ;; => #t
> (null? '(1 2 3)) ;; => #f
```
### **pop-front**
Returns the first element of the given vector.

(pop-front vec) -> any?

* vec : immutable-vector? - The vector from which the first element will be returned.

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (pop-front A) ;; => 1
```
### **push**
Appends an element to the given vector.

(push elem vec) -> immutable-vector?

* elem : any - The element to append.
* vec : immutable-vector? - The vector to which the element will be appended.

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (push 4 A) ;; => '#(1 2 3 4)
```
### **push-front**
Prepends an element to the given vector.

(push-front elem vec) -> immutable-vector?

* elem : any - The element to prepend.
* vec : immutable-vector? - The vector to which the element will be prepended.

#### Examples
```scheme
> (define A (immutable-vector 2 3 4)) ;;
> (push-front 1 A) ;; => '#(1 2 3 4)
```
### **range-vec**
Constructs a vector containing a range of integers from `start` to `end` (exclusive).

(range-vec start end) -> immutable-vector?

* start : integer? - The starting value of the range (inclusive).
* end : integer? - The ending value of the range (exclusive).

#### Examples
```scheme
> (range-vec 1 5) ;; => '#(1 2 3 4)
```
### **vec-append**
Combines the given vectors.

(vec-append . vecs) -> immutable-vector?

* vecs : immutable-vector? - The vectors to combine.

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (define B (immutable-vector 4 5)) ;;
> (vec-append A B) ;; => '#(1 2 3 4 5)
```
### **vec-rest**
Returns a new vector with the first element removed.

(vec-rest vec) -> immutable-vector?

* vec : immutable-vector? - The vector from which the first element will be removed.

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (vec-rest A) ;; => '#(2 3)
```
### **vector**
Constructs a new mutable vector from the provided arguments.

(vector . args) -> vector?

* args : any? - Elements to initialize the mutable vector.

#### Examples
```scheme
> (vector 1 2 3) ;; => '#(1 2 3)
```
### **vector-append!**
Appends the contents of one mutable vector to another.

(vector-append! vec1 vec2) -> void?

* vec1 : vector? - The mutable vector to which elements will be appended.
* vec2 : vector? - The mutable vector whose elements will be appended to `vec1`.

#### Examples
```scheme
> (define A (mutable-vector 1 2)) ;;
> (define B (mutable-vector 3 4)) ;;
> (vector-append! A B) ;;
> A ;; => '#(1 2 3 4)
> B ;; => '#()
```
### **vector-copy!**
Copies a range of elements from a source vector into a destination mutable vector.
Overwrites elements in `dest`, starting at `dest-start`, with elements from `src`
within the range `[src-start, src-end)`.

(vector-copy! dest dest-start src [src-start src-end]) -> void?

* dest : vector? - The destination mutable vector.
* dest-start : integer? - The starting index in the destination vector.
* src : vector? - The source vector.
* src-start : integer? - The starting index in the source vector (defaults to `0`).
* src-end : integer? - The exclusive ending index in the source vector (defaults to the length of `src`).


#### Examples
```scheme
> (define A (mutable-vector 1 2 3 4 5)) ;;
> (define B (mutable-vector 10 20 30 40 50)) ;;
> (vector-copy! B 1 A 2 4) ;;
> B ;; => '#(10 3 4 40 50)
```
### **vector-fill!**
Fills a mutable vector with a specified value over a given range.

(vector-fill! vec value [start end]) -> void?

* vec : vector? - The mutable vector to modify.
* value : any? - The value to fill the vector with.
* start : integer? - The starting index of the fill range (defaults to `0`).
* end : integer? - The exclusive ending index of the fill range (defaults to the length of `vec`).

#### Examples
```scheme
> (define A (mutable-vector 1 2 3 4 5)) ;;
> (vector-fill! A 9 1 4) ;;
> A ;; => '#(1 9 9 9 5)
```
### **vector-length**
Returns the length of the given vector.

(vector-length vec) -> integer?

* vec : vector? - The vector whose length is to be determined.

#### Examples
```scheme
> (define V (immutable-vector 1 2 3 4)) ;;
> (vector-length V) ;; => 4
```
### **vector-ref**
Retrieves the value at a specified index in an immutable or mutable vector.

(vector-ref vec index) -> any?

* vec : vector? - The vector from which to retrieve a value.
* index : integer? - The position in `vec` to access (must be within bounds).

#### Examples
```scheme
> (define A (immutable-vector 10 20 30)) ;;
> (vector-ref A 1) ;; => 20
> (define B (mutable-vector 5 15 25)) ;;
> (vector-ref B 2) ;; => 25
```
### **vector-set!**
Sets the value at a specified index in a mutable vector.

(vector-set! vec index value) -> void?

* vec : vector? - The mutable vector to modify.
* index : integer? - The position in `vec` to update (must be within bounds).
* value : any? - The new value to store at `index`.

#### Examples
```scheme
> (define A (mutable-vector 1 2 3)) ;;
> (vector-set! A 1 42) ;;
> A ;; => '#(1 42 3)
```
### **vector-swap!**
Swaps the value at a specified indices in a mutable vector.

(vector-set! vec index value) -> void?

* vec : vector? - The mutable vector to modify.
* index : integer? - The position in `vec` to update (must be within bounds).
* value : any? - The new value to store at `index`.

#### Examples
```scheme
> (define A (mutable-vector 1 2 3)) ;;
> (vector-set! A 1 42) ;;
> A ;; => '#(1 42 3)
```
### **vector-push!**
