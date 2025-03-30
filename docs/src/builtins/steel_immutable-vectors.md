# steel/immutable-vectors
### **immutable-vector**
Constructs an immutable vector from the given arguments.

(immutable-vector . vals) -> vector?

* vals : any? - The values to store in the immutable vector.

#### Examples
```scheme
> (define V (immutable-vector 1 2 3)) ;;
> V ;; => '#(1 2 3)
```
### **immutable-vector->list**
Converts an immutable vector into a list.

(immutable-vector->list vec) -> list?

* vec : immutable-vector?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (immutable-vector->list A) ;; => '(1 2 3)
```
### **immutable-vector->string**
Converts an immutable vector of characters into a string.

(immutable-vector->string vec) -> string?

* vec : immutable-vector?

#### Examples
```scheme
> (define A (immutable-vector #\a #\b #\c)) ;;
> (immutable-vector->string A) ;; => "abc"
```
### **immutable-vector-append**
Returns the combination of the given immutable vectors.

(immutable-vector-append . vecs) -> immutable-vector?

* vecs : immutable-vector? - The vectors to combine.

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (define B (immutable-vector 4 5 6)) ;;
> (immutable-vector-append A B) ;; => '#(1 2 3 4 5 6)
```
### **immutable-vector-copy**
Returns a new copy of the given immutable vector.

(immutable-vector-copy vec) -> immutable-vector?

* vec : immutable-vector?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (immutable-vector-copy A) ;; => '#(1 2 3)
```
### **immutable-vector-drop**
Returns a new vector with the first `n` elements removed from the original vector.

(immutable-vector-drop vec n) -> immutable-vector?

* vec : immutable-vector?
* n : integer?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3 4)) ;;
> (immutable-vector-drop A 2) ;; => '#(3 4)
```
### **immutable-vector-push**
Pushes a value to the back of the vector, returning a new vector.

(immutable-vector-push vec val) -> immutable-vector?

* vec : immutable-vector?
* val : any?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (immutable-vector-push A 5) ;; => '#(1 2 3 5)
```
### **immutable-vector-rest**
Returns the vector with the first value removed.

(immutable-vector-rest vec) -> immutable-vector?

* vec : immutable-vector?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (immutable-vector-rest A) ;; => '#(2 3)
```
### **immutable-vector-set**
Returns a new vector with the specified index updated to the given value.

(immutable-vector-set vec index val) -> immutable-vector?

* vec : immutable-vector?
* index : integer?
* val : any?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (immutable-vector-set A 0 5) ;; => '#(5 2 3)
```
### **immutable-vector-take**
Returns a new vector containing only the first `n` elements of the original vector.

(immutable-vector-take vec n) -> immutable-vector?

* vec : immutable-vector?
* n : integer?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3 4)) ;;
> (immutable-vector-take A 2) ;; => '#(1 2)
```
### **make-immutable-vector**
Creates an immutable vector of a given length, filled with an optional value.
(which defaults to '#<void>')

(make-immutable-vector len [val]) -> immutable-vector?

* len : integer?
* val : any? - defaults to #<void>

#### Examples
```scheme
> (make-immutable-vector 3 5) ;; => '#(5 5 5)
> (make-immutable-vector 2) ;; => '#(#<void> #<void>)
```
### **vector->string**
Converts a vector of characters into a string.

(vector->string vec) -> string?

* vec : vector?

#### Examples
```scheme
> (define A (vector #\a #\b #\c)) ;;
> (vector->string A) ;; => "abc"
```
### **vector-copy**
Returns a new copy of the given vector.

(vector-copy vec) -> vector?

* vec : vector?

#### Examples
```scheme
> (define A (vector 1 2 3)) ;;
> (vector-copy A) ;; => '#(1 2 3)
```
### **vector-push-front**
Pushes a value to the front of the vector, returning a new vector.

(vector-push-front vec val) -> immutable-vector?

* vec : immutable-vector?
* val : any?

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (vector-push-front A 5) ;; => '#(5 1 2 3)
```
### **vector-append**
### **vector-immutable**
