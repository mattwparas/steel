# steel/base
### **\***
Multiplies the given numbers.

(* . nums) -> number?

* nums : number? - The numbers to multiply. Can have any number of arguments including zero.

#### Examples
```scheme
> (* 5 3) ;; => 15
> (* 10 3 2) ;; => 60
> (*) ;; => 1
```
### **+**
Adds the given numbers.

(+ . nums) -> number?

* nums : number? - The numbers to add. Can have any number of arguments including zero.

#### Examples
```scheme
> (+ 5 3) ;; => 8
> (+ 10 3 2) ;; => 15
> (+) ;; => 0
```
### **-**
Subtracts the given numbers.

(- . nums) -> number?

* nums : number? - The numbers to subtract. Must have at least one number.

#### Examples
```scheme
> (- 5 3) ;; => 2
> (- 10 3 2) ;; => 5
> (- -5) ;; => 5
```
### **/**
Divides the given numbers.

(/ . nums) -> number?

* nums : number? - The numbers to divide. Must have at least one number.

#### Examples
```scheme
> (/ 10 2) ;; => 5
> (/ 10 2 2.0) ;; => 2.5
> (/ 1 3.0) ;; => 0.3333333333333333
> (/ 1 3) ;; => 1/3
```
### **<**
Compares real numbers to check if any number is less than the subsequent.

(< x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (< 1) ;; => #t
> (< 3 2) ;; => #f
> (< 2 3) ;; => #t
> (< 3/2 1.5) ;; => #f
> (< 2.5 3/2) ;; => #t
> (< 2 5/2 3) ;; #t
```
### **<=**
Compares real numbers to check if any number is less than or equal than the subsequent.

(<= x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (<= 1) ;; => #t
> (<= 3 2) ;; => #f
> (<= 2 3) ;; => #t
> (<= 3/2 1.5) ;; => #t
> (<= 2.5 3/2) ;; => #f
> (<= 2 6/2 3) ;; #t
```
### **>**
Compares real numbers to check if any number is greater than the subsequent.

(> x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (> 1) ;; => #t
> (> 3 2) ;; => #t
> (> 1 1) ;; => #f
> (> 3/2 1.5) ;; => #f
> (> 3/2 1.4) ;; => #t
> (> 3 4/2 1) ;; #t
```
### **>=**
Compares real numbers to check if any number is greater than or equal than the subsequent.

(>= x . rest) -> bool?

* x : real? - The first real number to compare.
* rest : real? - The rest of the numbers to compare.

#### Examples
```scheme
> (>= 1) ;; => #t
> (>= 3 2) ;; => #t
> (>= 2 3) ;; => #f
> (>= 3/2 1.5) ;; => #t
> (>= 3/2 1.4) ;; => #t
> (>= 2 4/2 1) ;; #t
```
### **abs**
Computes the absolute value of the given number.

(abs number) -> number?

* number : number? - The number to compute the absolute value of.

#### Examples
```scheme
> (abs 42) ;; => 42
> (abs -42) ;; => 42
> (abs 0) ;; => 0
```
### **acos**
Returns the arccosine, or inverse cosine, of a value; output is in radians.

(acos n) -> number?

* n : number? - The input value is the cosine of the angle you want and must be from -1 to 1.

#### Examples
```scheme
> (acos -1) ;; => 3.141592653589793
> (acos 0) ;; => 1.5707963267948966
> (acos 0.5) ;; => 1.0471975511965976
> (acos 2) ;; => +nan.0
```
### **append**
Appends the given lists together. If provided with no lists, will return the empty list.

(append lst ...)

lst : list?

#### Examples
```scheme
> (append (list 1 2) (list 3 4)) ;; => '(1 2 3 4)
> (append) ;; => '()
```
### **apply**
Applies the given `function` with arguments as the contents of the `list`.

(apply function lst) -> any?

* function : function?
* list: list?

#### Examples
```scheme
> (apply + (list 1 2 3 4)) ;; => 10
> (apply list (list 1 2 3 4)) ;; => '(1 2 3 4)
```
### **arithmetic-shift**
Performs a bitwise arithmetic shift using the given 2 numbers

(arithmetic-shift n m) -> integer?

* n : integer? - The number to shift.
* m : integer? - The number by which to shift.

#### Examples
```scheme
> (arithmetic-shift 10 1) ;; => 20
> (arithmetic-shift 20 1) ;; => 40
> (arithmetic-shift 40 -2) ;; => 10
```
### **asin**
Returns the arcsine, or inverse sine, of a value; output is in radians.

(asin n) -> number?

* n : number? - The input value is the sine of the angle you want and must be from -1 to 1.

#### Examples
```scheme
> (asin -1) ;; => -1.5707963267948966
> (asin 0) ;; => 0
> (asin 0.5) ;; => 0.5235987755982988
> (asin 2) ;; => +nan.0
```
### **atan**
Returns the arctangent, or inverse tangent, of a value; output is in radians.

(atan n) -> number?

* n : number? - The input value is the tangent of the angle you want.

#### Examples
```scheme
> (atan -1) ;; => -0.7853981633974483
> (atan 0) ;; => 0
> (atan 0.5) ;; => 0.46364760900080615
> (atan 2) ;; => 1.1071487177940906
```
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
### **canonicalize-path**
Returns canonical path with all components normalized

(canonicalize-path path) -> string?

* path : (string?) - The path to canonicalize

#### Examples
```scheme
> (canonicalize-path "logs") ;; => "/Users/me/Desktop/programming/logs"
> (canonicalize-path "logs/today.json") ;; => "/Users/me/Desktop/programming/logs/today.json"
```
### **car**
Returns the first element of the list l.

(car l) -> any/c

* l : list?

#### Examples

```scheme
> (car '(1 2)) ;; => 1
> (car (cons 2 3)) ;; => 2
```
### **cdr**
Returns the rest of the list. Will raise an error if the list is empty.

(cdr l) -> list?

* l : list?

#### Examples
```scheme
> (cdr (list 10 20 30)) ;; => '(20 30)
> (cdr (list 10)) ;; => '()
> (cdr '())
error[E11]: Generic
┌─ :1:2
│
1 │ (cdr '())
│  ^^^ cdr expects a non empty list
```
### **ceiling**
Rounds the given number up to the nearest integer not less than it.

(ceiling number) -> integer?

* number : number? - The number to round up.

#### Examples
```scheme
> (ceiling 42) ;; => 42
> (ceiling 42.1) ;; => 43
> (ceiling -42.1) ;; => -42
```
### **change-current-directory!**
Change the current working directory

(change-current-directory! path) -> void?

* path : (string?) - The directory to switch to

#### Examples
```scheme
> (change-current-directory! "logs") ;;
> (change-current-directory! "..") ;;
```
### **char->integer**
Returns the Unicode codepoint of a given character.

(char->integer char?) -> integer?
### **char->number**
Attemps to convert the character into a decimal digit,
and returns `#f` on failure.
### **char-digit?**
Returns `#t` if the character is a decimal digit.
### **char-downcase**
Returns the lower case version of a character, if defined by Unicode,
or the same character otherwise.
### **char-upcase**
Returns the upper case version of a character, if defined by Unicode,
or the same character otherwise.
### **char-whitespace?**
Returns `#t` if the character is a whitespace character.
### **char<=?**
Compares characters according to their codepoints, in a "less-than-or-equal" fashion.

(char<=? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
### **char<?**
Compares characters according to their codepoints, in a "less-than" fashion.

(char<? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
### **char=?**
Checks if all characters are equal.

Requires that all inputs are characters, and will otherwise raise an error.

(char=? char1 char2 ...) -> bool?

* char1 : char?
* char2 : char?
### **char>=?**
Compares characters according to their codepoints, in a "greater-than-or-equal" fashion.

(char>=? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
### **char>?**
Compares characters according to their codepoints, in a "greater-than" fashion.

(char>? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
### **close-input-port**
Close an input port. If the port is a file, the file will be closed.

(close-port input-port?) -> void
### **close-output-port**
Close an output port. If the port is a file, the file will be closed.

(close-port output-port?) -> void
### **close-port**
Close a port. If the port is a file, the file will be closed.

(close-port port?) -> void
### **command-line**
Returns the command line passed to this process,
including the command name as first argument.
### **complex?**
Checks if the given value is a complex number

(complex? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (complex? 3+4i) ;; => #t
> (complex? 42) ;; => #t
> (complex? "hello") ;; => #f
```
### **cons**
Returns a newly allocated list whose first element is `a` and second element is `d`.

(cons a d) -> list?

* a : any/c
* d : any/c

#### Examples
```scheme
> (cons 1 2) ;; => '(1 . 2)
> (cons 1 '()) ;; => '(1)
```
### **copy-directory-recursively!**
Recursively copies the contents of the source directory to the destination

(copy-directory-recursively! source destination) -> void?

* source : (string?) - The directory to copy.
* destination : (string?) - The destination directory into which to copy.

#### Examples
```scheme
> (copy-directory-recursively! "logs" "backup") ;;
```
### **cos**
Returns the cosine value of the input angle, measured in radians.

(cos n) -> number?

* n : number? - The input angle, in radians.

#### Examples
```scheme
> (cos 0) ;; => 1
> (cos 1) ;; => 0.5403023058681398
> (cos 2.0) ;; => -0.4161468365471424
> (cos 3.14) ;; => -0.9999987317275395
```
### **create-directory!**
Creates the directory

(create-directory! dir) -> void?

* dir : (string?) - The directory name to create.

#### Examples
```scheme
> (create-directory! "logs") ;;
```
### **current-directory**
Outputs the current working directory as a string

(current-directory) -> string?

#### Examples
```scheme
> (current-directory) ;; => "/Users/me/Desktop/programming"
```
### **current-inexact-milliseconds**
Returns the number of milliseconds since the Unix epoch as an inexact number.

(current-inexact-milliseconds) -> inexact?
### **current-milliseconds**
Returns the number of milliseconds since the Unix epoch as an integer.

(current-milliseconds) -> int?
### **current-second**
Returns the number of seconds since the Unix epoch as an integer.

(current-second) -> int?
### **delete-directory!**
Deletes the directory

(delete-directory! dir) -> void?

* dir : (string?) - The directory name to delete.

#### Examples
```scheme
> (delete-directory! "logs") ;;
```
### **delete-file!**
Deletes the file

(delete-file! path) -> void?

* path : (string?) - The file to delete

#### Examples
```scheme
> (delete-file! "logs/today.json") ;;
```
### **denominator**
Retrieves the denominator of the given rational number.

(denominator number) -> integer?

* number : number? - The rational number to retrieve the denominator from.

#### Examples
```scheme
> (denominator 1/2) ;; => 2
> (denominator 3/4) ;; => 4
> (denominator 4) ;; => 1
```
### **disconnected-channel-object?**
Returns `#t` if the value is an disconnected-channel object.

(eof-object? any/c) -> bool?
### **duration->string**
Returns a string representation of a duration

(duration->string dur)

* dur : duration?
### **empty-channel-object?**
Returns `#t` if the value is an empty-channel object.

(empty-channel-object? any/c) -> bool?
### **empty?**
Checks if the list is empty

(empty? lst) -> bool?

* lst: list?

#### Examples

```scheme
> (empty? (list 1 2 3 4 5)) ;; => #false
> (empty? '()) ;; => #true
```
### **ends-with?**
Checks if the input string ends with a given suffix

(ends-with? input pattern) -> bool?

input : string?
pattern: string?

#### Examples

```scheme
> (ends-with? "foobar" "foo") ;; => #false
> (ends-with? "foobar" "bar") ;; => #true
```
### **eof-object**
Returns an EOF object.

(eof-object) -> eof-object?
### **eof-object?**
Returns `#t` if the value is an EOF object.

(eof-object? any/c) -> bool?
### **error-object-message**
Returns the message of an error object.

(error-object-message error?) -> string?
### **even?**
Checks if the given number is even

(even? n) -> bool?

* n : number? - The number to check for evenness.

#### Examples
```scheme
> (even? 2) ;; => #true
> (even? 3) ;; => #false
> (even? 4.0) ;; => #true
```
### **exact**
Returns an exact representation of the input number, coerces an inexact number to an exact form.

(exact n) -> number?

* n : number? - The value to check for exactness.

#### Examples
```scheme
> (exact 5.0) ;; => 5
> (exact 5/3) ;; => 5/3
> (exact 2) ;; => 2
```
### **exact->inexact**
Converts an exact number to an inexact number.

(exact->inexact num) -> number?

* num : number? - The number to convert from exact to inexact.

#### Examples
```scheme
> (exact->inexact 10) ;; => 10
> (exact->inexact 1/2) ;; => 0.5
> (exact->inexact 1+2i) ;; => 1+2i
```
### **exact-integer-sqrt**
Computes the integer square root of the given non-negative integer.

(exact-integer-sqrt number) -> (integer? integer?)

* number : (and/c integer? positive?) - The non-negative integer to compute the square root for.

#### Examples
```scheme
> (exact-integer-sqrt 25) ;; => (5 0)
> (exact-integer-sqrt 35) ;; => (5 10)
```
### **exact-integer?**
Checks if the given value is an exact integer

(exact-integer? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (exact-integer? 42) ;; => #t
> (exact-integer? -42) ;; => #t
> (exact-integer? 4.0) ;; => #f
```
### **exact?**
Checks if the given value is exact.

(exact? val) -> boolean?

* val : any - The value to check for exactness.

#### Examples
```scheme
> (exact? 42) ;; => #t
> (exact? 3.14) ;; => #f
> (exact? "hello") ;; => #f
```
### **exp**
Returns Euler’s number raised to the power of z.

(exp z) -> number?

* z : number? - The number to raise e to the power of.

#### Examples
```scheme
> (exp 0) ;; => 1
> (exp 2) ;; => 7.38905609893065
> (exp 1.5) ;; => 4.4816890703380645
```
### **expt**
Raises the left operand to the power of the right operand.

(expt base exponent) -> number?

* base : number? - The base number.
* exponent : number? - The exponent to raise the base to.

#### Examples
```scheme
> (expt 2 3) ;; => 8
> (expt 2.0 0.5) ;; => 1.4142135623730951
> (expt 9 0.5) ;; => 3
```
### **f+**
Sums all given floats

(f+ nums) -> number?

* nums : float? - The floats to sum up.

#### Examples
```scheme
> (f+ 5.5) ;; => 5.5
> (f+ 1.1 2.2) ;; => 3.3
> (f+ 3.3 3.3 3.3) ;; => 9.9
```
### **file-name**
Gets the filename for a given path

(file-name path) -> string?

* path : (string?) - The path to check

#### Examples
```scheme
> (file-name "logs") ;; => "logs"
> (file-name "logs/today.json") ;; => "today.json"
```
### **finite?**
Returns `#t` if the given number is finite.

(finite? number) -> boolean?

* number : number? - The number to check for finiteness.

#### Examples
```scheme
> (finite? 42) ;; => #t
> (finite? 0.1) ;; => #t
> (finite? +inf.0) ;; => #f
> (finite? -inf.0) ;; => #f
> (finite? +nan.0) ;; => #f
```
### **first**
Returns the first element of the list l.

(first l) -> any/c

* l : list?

#### Examples

```scheme
> (first '(1 2)) ;; => 1
> (first (cons 2 3)) ;; => 2
```
### **float?**
Checks if the given value is a floating-point number

(float? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (float? 42) ;; => #f
> (float? 3.14) ;; => #t
> (float? #t) ;; => #f
```
### **floor**
Computes the largest integer less than or equal to the given number.

(floor number) -> number?

* number : number? - The number to compute the floor for.

#### Examples
```scheme
> (floor 3.14) ;; => 3
> (floor 4.99) ;; => 4
> (floor -2.5) ;; => -3
```
### **get-output-bytevector**
Extracts the contents from a port created with `open-output-bytevector`.

(get-output-bytevector port?) -> bytes?
### **get-output-string**
Extracts the string contents from a port created with `open-output-string`.

(get-output-string port?) -> string?
### **get-tls**
Get the value out of the thread local storage slot.
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
### **hash-remove**
Returns a new hashmap with the given key removed. Performs a functional
update, so the old hash map is still available with the original key value pair.

(hash-remove map key) -> hash?

* map : hash?
* key : any/c

#### Examples
```scheme
> (hash-remove (hash 'a 10 'b 20) 'a)

=> '#hash(('b . 20))
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
### **imag-part**
Returns the imaginary part of a number

(imag-part number) -> number?

#### Examples
```scheme
> (imag-part 3+4i) ;; => 4
> (imag-part 42) ;; => 0
```
### **immutable-vector**
Constructs an immutable vector from the given arguments.

(immutable-vector . vals) -> vector?

* vals : any? - The values to store in the immutable vector.

#### Examples
```scheme
> (define V (immutable-vector 1 2 3)) ;;
> V ;; => '#(1 2 3)
```
### **inexact->exact**
Converts an inexact number to an exact number.

(inexact->exact num) -> number?

* num : number? - The number to convert from inexact to exact.

#### Examples
```scheme
> (inexact->exact 10.0) ;; => 10
> (inexact->exact 1.5) ;; => 3/2
> (inexact->exact 1.5+2.5i) ;; => 3/2+5/2i
```
### **inexact?**
Checks if the given value is inexact.

(inexact? val) -> boolean?

* val : any - The value to check for inexactness.

#### Examples
```scheme
> (inexact? 42) ;; => #f
> (inexact? 3.14) ;; => #t
```
### **infinite?**
Returns `#t` if the given number is infinite.

(infinite? number) -> boolean?

* number : number? - The number to check for infiniteness.

#### Examples
```scheme
> (infinite? 42) ;; => #f
> (infinite? -nan.0) ;; => #f
> (infinite? +inf.0) ;; => #t
```
### **input-port?**
Checks if a given value is an input port

(input-port? any/c) -> bool?

#### Examples

```scheme
> (input-port? (stdin)) ;; => #true
> (input-port? "foo") ;; => #false
```
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
```
### **int?**
Checks if the given value is an integer, an alias for `integer?`

(int? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (int? 42) ;; => #t
> (int? 3.14) ;; => #f
> (int? "hello") ;; => #f
```
### **integer->char**
Returns the character corresponding to a given Unicode codepoint.

(integer->char integer?) -> char?
### **integer?**
Checks if the given value is an integer, an alias for `int?`

(integer? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (integer? 42) ;; => #t
> (integer? 3.14) ;; => #f
> (integer? "hello") ;; => #f
```
### **is-dir?**
Checks if a path is a directory

(is-dir? path) -> bool?

* path : (string?) - The path to check

#### Examples
```scheme
> (is-dir? "logs") ;; => #true
> (is-dir? "logs/today.json") ;; => #false
```
### **is-file?**
Checks if a path is a file

(is-file? path) -> bool?

* path : (string?) - The path to check

#### Examples
```scheme
> (is-file? "logs") ;; => #false
> (is-file? "logs/today.json") ;; => #true
```
### **last**
Returns the last element in the list. Takes time proportional to the length of the list.

(last l) -> any/c

* l : list?

#### Examples
```scheme
> (list (list 1 2 3 4)) ;; => 4
```
### **length**
Returns the length of the list.

(length l) -> int?

* l : list?

#### Examples

```scheme
> (length (list 10 20 30)) ;; => 3
```
### **list**
Returns a newly allocated list containing the vs as its elements.

(list v ...) -> list?

* v : any/c

#### Examples

```scheme
> (list 1 2 3 4 5) ;; => '(1 2 3 4 5)
> (list (list 1 2) (list 3 4)) ;; => '((1 2) (3 4))
```
### **list->bytes**
Converts the list of bytes to the equivalent bytevector representation.
The list must contain _only_ values which satisfy the `byte?` predicate,
otherwise this function will error.

#### Examples
```scheme
(list->bytes (list 0 1 2 3 4 5)) ;; => (bytes 0 1 2 3 4 5)
```
### **list->hashset**
Convert the given list into a hashset.

#### Examples
```scheme
(list 10 20 30) ;; => (hashset 10 20 30)
```
### **list-ref**
Returns the value located at the given index. Will raise an error if you try to index out of bounds.

Note: Runs in time proportional to the length of the list, however lists in Steel are implemented in such a fashion that the
time complexity is O(n/64). Meaning, for small lists this can be constant.

(list-ref lst index) -> list?

* lst : list?
* index : (and/c int? positive?)

#### Examples
```scheme
> (list-ref (list 1 2 3 4) 2) ;; => 3
> (list-ref (range 0 100) 42) ;; => 42"
> (list-ref (list 1 2 3 4) 10)
error[E11]: Generic
┌─ :1:2
│
1 │ (list-ref (list 1 2 3 4) 10)
│  ^^^^^^^^ out of bounds index in list-ref - list length: 4, index: 10
```
### **local-time/now!**
Returns the local time in the format given by the input string (using `chrono::Local::format`).

(local-time/now! fmt) -> string?

* fmt : string?
### **lock-acquire!**
Lock the given mutex. Note, this is most likely used as a building block
with the `lock!` function.
### **lock-release!**
Unlock the given mutex.
### **log**
Computes the natural logarithm of the given number.

(log number [base]) -> number?

* number : number? - The number to compute the logarithm for.
* base : number? - The base of the logarithm. If not provided, defaults to Euler's number (e).

#### Examples
```scheme
> (log 10) ;; => 2.302585092994046
> (log 100 10) ;; => 2
> (log 27 3) ;; => 3
```
### **magnitude**
Computes the magnitude of the given number.

(magnitude number) -> number?

* number : number? - The number to compute the magnitude for.

#### Examples
```scheme
> (magnitude 3+4i) ;; => 5
> (magnitude 5) ;; => 5
> (magnitude -5) ;; => 5
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
### **make-string**
Creates a string of a given length, filled with an optional character
(which defaults to `#\0`).

(make-string len [char]) -> string?

* len : int?
* char : char? = #\0
### **make-tls**
Creates a thread local storage slot. These slots are static, and will _not_ be reclaimed.

When spawning a new thread, the value inside will be shared into that slot, however
future updates to the slot will be local to that thread.
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
### **modulo**
Returns the euclidean remainder of the division of the first number by the second
This differs from the remainder operator when using negative numbers.

(modulo n m) -> integer?

* n : integer?
* m : integer?

#### Examples
```scheme
> (modulo 10 3) ;; => 1
> (modulo -10 3) ;; => 2
> (modulo 10 -3) ;; => -2
> (module -10 -3) ;; => -1
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
### **mutex**
Construct a new mutex
### **nan?**
Returns `#t` if the real number is Nan.

(nan? value) -> boolean?

* value : real? - The value to check

```scheme
(nan? +nan.0) => #t
(nan? 100000) => #f
```
### **negative?**
Checks if the given real number is negative.

(negative? num) -> boolean?

* num : real? - The real number to check for negativity.

#### Examples
```scheme
> (negative? 0) ;; => #f
> (negative? 1) ;; => #f
> (negative? -1) ;; => #t
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
### **number->string**
Converts the given number to a string.
### **number?**
Checks if the given value is a number

(number? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (number? 42) ;; => #t
> (number? "hello") ;; => #f
> (number? 'symbol) ;; => #f
```
### **numerator**
Retrieves the numerator of the given rational number.

(numerator number) -> number?

* number : number? - The rational number to retrieve the numerator from.

#### Examples
```scheme
> (numerator 3/4) ;; => 3
> (numerator 5/2) ;; => 5
> (numerator -2) ;; => -2
```
### **odd?**
Checks if the given number is odd

(odd? n) -> bool?

* n : number? - The number to check for oddness.

#### Examples
```scheme
> (odd? 2) ;; => #false
> (odd? 3) ;; => #true
> (odd? 5.0) ;; => #true
```
### **open-input-bytevector**
Creates an input port from a bytevector, that will return the bytevector contents.

(open-input-bytevector bytes?) -> input-port?
### **open-input-file**
Takes a filename `path` referring to an existing file and returns an input port. Raises an error
if the file does not exist

(open-input-file string?) -> input-port?

#### Examples
```scheme
> (open-input-file "foo-bar.txt") ;; => #<port>
> (open-input-file "file-does-not-exist.txt")
error[E08]: Io
┌─ :1:2
│
1 │ (open-input-file "foo-bar.txt")
│  ^^^^^^^^^^^^^^^ No such file or directory (os error 2)
```
### **open-input-string**
Creates an input port from a string, that will return the string contents.

(open-input-string string?) -> input-port?
### **open-output-bytevector**
Creates an output port that accumulates what is written into a bytevector.
These bytes can be recovered calling `get-output-bytevector`.

(open-output-bytevector) -> output-port?

#### Examples
```scheme
(define out (open-output-bytevector))


(write-byte 30 out)
(write-byte 250 out)

(get-output-bytevector out) ;; => (bytes 30 250)
```
### **open-output-file**
Takes a filename `path` referring to a file to be created and returns an output port.

(open-output-file string?) -> output-port?

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<port>
```
### **open-output-string**
Creates an output port that accumulates what is written into a string.
This string can be recovered calling `get-output-string`.

(open-output-string) -> output-port?

#### Examples
```scheme
(define out (open-output-string))


(write-char "α" out)
(write-char "ω" out)

(get-output-string out) ;; => "αω"
```
### **output-port?**
Checks if a given value is an output port

(output-port? any/c) -> bool?

#### Examples

```scheme
> (define output (open-output-file "foo.txt"))
> (output-port? output) ;; => #true
```
### **pair?**
Checks if the given value can be treated as a pair.

(pair? any/c) -> bool?

#### Examples

```scheme
> (pair? '(10 20)) ;; => #true
> (pair? '(10)) ;; => #true
> (pair? '()) ;; => #false
```
### **parent-name**
Gets the parent directory name for a given path

(parent-name path) -> string?

* path : (string?) - The path to check

#### Examples
```scheme
> (parent-name "logs") ;; => ""
> (parent-name "logs/today.json") ;; => "logs"
```
### **path->extension**
Gets the extension from a path

(path->extension path) -> (or/c string? void?)

* path : (string?) - The path to check

#### Examples
```scheme
> (path->extension "logs") ;; => void
> (path->extension "logs/today.json") ;; => ".json"
```
### **path-exists?**
Checks if a path exists

(path-exists? path) -> bool?

* path : (string?) - The path to check

#### Examples
```scheme
> (path-exists? "logs") ;; => #true
> (path-exists? "backup/logs") ;; => #false
```
### **peek-byte**
Peeks the next byte from an input port.

(peek-byte [port]) -> byte?

* port : input-port? = (current-input-port)
### **pop-front**
Returns the first element of the given vector.

(pop-front vec) -> any?

* vec : immutable-vector? - The vector from which the first element will be returned.

#### Examples
```scheme
> (define A (immutable-vector 1 2 3)) ;;
> (pop-front A) ;; => 1
```
### **positive?**
Checks if the given real number is positive.

(positive? num) -> boolean?

* num : real? - The real number to check for positivity.

#### Examples
```scheme
> (positive? 0) ;; => #f
> (positive? 1) ;; => #t
> (positive? -1) ;; => #f
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
### **quotient**
Returns quotient of dividing numerator by denomintator.

(quotient numerator denominator) -> integer?

* numerator : integer? - The numerator.
* denominator : integer? - The denominator.

#### Examples
```scheme
> (quotient 11 2) ;; => 5
> (quotient 10 2) ;; => 5
> (quotient -10 2) ;; => -5
```
### **range**
Returns a newly allocated list of the elements in the range [n, m) or [0, m) when n is not given.

(range m)   -> (listof int?)
(range n m) -> (listof int?)

* n : int?
* m : int?

```scheme
> (range 4) ;; => '(0 1 2 3)
> (range 4 10) ;; => '(4 5 6 7 8 9)
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
### **rational?**
Returns #t if obj is a rational number, #f otherwise.
Rational numbers are numbers that can be expressed as the quotient of two numbers.
For example, 3/4, -5/2, 0.25, and 0 are rational numbers.

(rational? value) -> bool?

* value : any - The value to check

Examples:
```scheme
> (rational? (/ 0.0)) ;; => #f
> (rational? 3.5) ;; => #t
> (rational? 6/10) ;; => #t
> (rational? +nan.0) ;; => #f
```
### **read-byte**
Reads a single byte from an input port.

(read-byte [port]) -> byte?

* port : input-port? = (current-input-port)
### **read-bytes**
Reads bytes from an input port.

(read-bytes amt [port]) -> bytes?

* amt : (and positive? int?)
* port : input-port? = (current-input-port)
### **read-bytes-into-buf**
Reads bytes from an input port into a given buffer.

(read-bytes-into-buf buf amt [port]) -> bytes?

* buf : bytes?
* amt : (and positive? int?)
* port : input-port? = (current-input-port)
### **read-char**
Reads the next character from an input port.

(read-char [port]) -> char?

* port : input-port? = (current-input-port)
### **read-dir**
Returns the contents of the directory as a list

(read-dir path) -> list?

* path : (string?) - The path to check

#### Examples
```scheme
> (read-dir "logs") ;; => '("logs/today.json" "logs/yesterday.json")
> (read-dir "empty_dir") ;; => '()
```
### **read-port-to-string**
Takes a port and reads the entire content into a string

(read-port-to-string port) -> string?

* port : input-port?
### **real-part**
Returns the real part of a number

(real-part number) -> number?

#### Examples
```scheme
> (real-part 3+4i) ;; => 3
> (real-part 42) ;; => 42
```
### **real?**
Checks if the given value is a real number

(real? value) -> boolean?

* value : any - The value to check

#### Examples
```scheme
> (real? 42) ;; => #t
> (real? 3+4i) ;; => #f
> (real? "hello") ;; => #f
```
### **receivers-select**
Blocks until one of the channels passed in is ready to receive.
Returns the index of the channel arguments passed in which is ready.

Using this directly is not recommended.
### **remainder**
Returns the arithmetic remainder of the division of the first number by the second.
This differs from the modulo operator when using negative numbers.

(remainder n m) -> integer?

* n : integer?
* m : integer?

#### Examples
```scheme
> (remainder 10 3) ;; => 1
> (remainder -10 3) ;; => -1
> (remainder 10 -3) ;; => 1
> (remainder -10 -3) ;; => -1
```
### **rest**
Returns the rest of the list. Will raise an error if the list is empty.

(rest l) -> list?

* l : list?

#### Examples
```scheme
> (rest (list 10 20 30)) ;; => '(20 30)
> (rest (list 10)) ;; => '()
> (rest (list 10))
error[E11]: Generic
┌─ :1:2
│
1 │ (rest '())
│  ^^^^ rest expects a non empty list
```
### **reverse**
Returns a list that has the same elements as `lst`, but in reverse order.
This function takes time proportional to the length of `lst`.

(reverse lst) -> list?

* l : list?

#### Examples
```scheme
> (reverse (list 1 2 3 4)) ;; '(4 3 2 1)
```
### **round**
Rounds the given number to the nearest integer.

(round number) -> number?

* number : number? - The number to round.

#### Examples
```scheme
> (round 3.14) ;; => 3
> (round 4.6) ;; => 5
> (round -2.5) ;; => -3
```
### **second**
Get the second element of the list. Raises an error if the list does not have an element in the second position.

(second l) -> any/c

* l : list?

#### Examples

```scheme
> (second '(1 2 3)) ;; => 2
> (second '())
error[E11]: Generic
┌─ :1:2
│
1 │ (second '())
│  ^^^^^^ second: index out of bounds - list did not have an element in the second position: []
### **set-tls!**
Set the value in the the thread local storage. Only this thread will see the updates associated
with this TLS.
### **sin**
Returns the sine value of the input angle, measured in radians.

(sin n) -> number?

* n : number? - The input angle, in radians.

#### Examples
```scheme
> (sin 0) ;; => 0
> (sin 1) ;; => 0.8414709848078965
> (sin 2.0) ;; => 0.9092974268256817
> (sin 3.14) ;; => 0.0015926529164868282
```
### **spawn-native-thread**
Spawns the given `func` on another thread. It is required that the arity of the
given function be 0. If the arity of the given function cannot be checked until runtime,
the thread will be spawned and the function will fail to execute.

#### Examples

```scheme
(define thread (spawn-native-thread (lambda () (displayln "Hello world!"))))
```
### **split-many**
Splits a string given a separator pattern into a list of strings.

(split-many str pat) -> (listof string?)

* str : string?
* pat : string?

#### Examples
```scheme
(split-many "foo,bar,baz" ",") ;; => '("foo" "bar" "baz")
(split-many "foo|bar|" "|") ;; => '("foo" "bar" "")
(split-many "" "&") ;; => '("")
```
### **split-once**
Splits a string given a separator at most once, yielding
a list with at most 2 elements.

(split-once str pat) -> string?

* str : string?
* pat : string?

#### Examples
```scheme
(split-once "foo,bar,baz" ",") ;; => '("foo" "bar,baz")
(split-once "foo|bar|" "|") ;; => '("foo" "bar|")
(split-once "" "&") ;; => '("")
```
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
```
### **sqrt**
Computes the square root of the given number.

(sqrt number) -> number?

* number : number? - The number to compute the square root for.

#### Examples
```scheme
> (sqrt 4) ;; => 2
> (sqrt 2) ;; => 1.4142135623730951
> (sqrt -1) ;; => 0+1i
```
### **square**
Computes the square of the given number.

(square number) -> number?

* number : number? - The number to square.

#### Examples
```scheme
> (square 5) ;; => 25
> (square -3) ;; => 9
> (square 2.5) ;; => 6.25
```
### **starts-with?**
Checks if the input string starts with a prefix

(starts-with? input pattern) -> bool?

* input : string?
* pattern: string?

#### Examples

```scheme
> (starts-with? "foobar" "foo") ;; => #true
> (starts-with? "foobar" "bar") ;; => #false
```
### **stdin**
Gets the port handle to stdin

(stdin) -> input-port?

#### Examples

```scheme
> (stdin) ;; => #<port>
```
### **string**
Constructs a string from the given characters
### **string->bytes**
Encodes a string as UTF-8 into a bytevector.

(string->bytes string?) -> bytes?

#### Examples
```scheme
(string->bytes "Apple") ;; => (bytes 65 112 112 108 101)
```
### **string->int**
Converts a string into an int. Raises an error if the string cannot be converted to an integer.

(string->int string?) -> int?

#### Examples

```scheme
> (string->int "100") ;; => 10
> (string->int "not-an-int") ;; error
```
### **string->jsexpr**
Deserializes a JSON string into a Steel value.

(string->jsexpr json) -> any/c

* json : string?

#### Examples
```scheme
(string->jsexpr "{\"foo\": [3]}") ;; => '#hash((foo . (3)))
```
### **string->list**
Converts a string into a list of characters.

(string->list s [start] [end]) -> (listof char?)

* s : string?
* start : int? = 0
* end : int?

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
```
### **string->lower**
Creates a new lowercased version of the input string

(string->lower string?) -> string?

#### Examples

```scheme
> (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
```
### **string->number**
Converts the given string to a number, with an optional radix.
On failure, it returns `#f`

(string->number digits [radix]) -> (or/c number? boolean?)

* digits : string?
* radix : number?
### **string->symbol**
Converts a string into a symbol.

(string->symbol string?) -> symbol?

#### Examples

```scheme
> (string->symbol "FooBar") ;; => 'FooBar
```
### **string->upper**
Creates a new uppercased version of the input string

(string->upper string?) -> string?

#### Examples

```scheme
> (string->upper "lower") ;; => "LOWER"
```
### **string->utf8**
Alias of `string->bytes`.
### **string->vector**
Returns a vector containing the characters of a given string

(string->vector string?) -> vector?

#### Examples
```scheme
(string->vector "hello") ;; => '#(#\h #\e #\l #\l #\o)
```
### **string-append**
Concatenates all of the given strings into one

(string-append strs...) -> string?

* strs ... : string?

#### Examples
```scheme
> (string-append) ;; => ""
> (string-append "foo" "bar") ;; => "foobar"
```
### **string-ci<=?**
Compares strings lexicographically (as in"less-than-or-equal"),
in a case insensitive fashion.

(string-ci<=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string-ci<?**
Compares strings lexicographically (as in"less-than"),
in a case insensitive fashion.

(string-ci<? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string-ci=?**
Compares strings for equality, in a case insensitive fashion.
### **string-ci>=?**
Compares strings lexicographically (as in"greater-than-or-equal"),
in a case insensitive fashion.

(string-ci>=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string-ci>?**
Compares strings lexicographically (as in"greater-than"),
in a case insensitive fashion.

(string-ci>? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string-contains?**
Searches a string to check if it contains the second argument.

(string-contains? string? string?) -> bool?

#### Examples
```scheme
(string-contains? "hello" "lo") ;;=> #t
(string-contains? "hello" "world") ;;=> #f
```
### **string-join**
Joins the given list of strings, with an optional separator.

(string-join strings [sep]) -> string?

* strings : (listof string?)
* sep : string? = ""

#### Examples
```scheme
(string-join '("a" "b" "c")) ;; => "abc"
(string-join '("one" "two" "three") ", ") ;; => "one, two, three"
```
### **string-length**
Get the length of the given string in UTF-8 bytes.

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
> (string-length "✅") ;; => 3
> (string-length "🤖") ;; => 4
```
### **string-ref**
Extracts the nth character out of a given string.

(string-ref str n) -> char?

* str : string?
* n : int?
### **string-replace**
Replaces all occurrences of a pattern into the given string

(string-replace str from to) -> string?

* str : string?
* from : string?
* to : string?

#### Examples
```scheme
(string-replace "hello world" "o" "@") ;; => "hell@ w@rld"
```
### **string<=?**
Compares strings lexicographically (as in"less-than-equal-to").

(string<=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string<?**
Compares strings lexicographically (as in"less-than").

(string<? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string=?**
Compares strings for equality.

(string=? string1 string2 ...) -> bool?

* string1 : string?
* string2 : string?
### **string>=?**
Compares strings lexicographically (as in"greater-than-or-equal").

(string>=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **string>?**
Compares strings lexicographically (as in"greater-than").

(string>? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
### **substring**
Creates a substring slicing the characters between two indices.

(substring str start end) -> string?

* str: string?
* start : int?
* end : int?

#### Examples
```scheme
(substring "hello" 1 4) ;; => "ell"
(substring "hello" 10 15) ;; => error
```
### **take**
Returns the first n elements of the list l as a new list.

(take l n) -> list?

* l : list?
* n : (and/c positive? int?)

#### Examples

```scheme
> (take '(1 2 3 4) 2) ;; => '(0 1)
> (take (range 0 10) 4) ;; => '(0 1 2 3)
```
### **tan**
Returns the tangent value of the input angle, measured in radians.

(tan n) -> number?

* n : number? - The input angle, in radians.

#### Examples
```scheme
> (tan 0) ;; => 0
> (tan 1) ;; => 1.557407724654902
> (tan 2.0) ;; => -2.185039863261519
> (tan 3.14) ;; => -0.0015926549364072232
```
### **third**
Get the third element of the list. Raises an error if the list does not have an element in the third position.

(third l) -> any/c

* l : list?

#### Examples
```scheme
> (third '(1 2 3)) ;; => 3
> (third '())
error[E11]: Generic
┌─ :1:2
│
1 │ (third '())
│  ^^^^^^ third: index out of bounds - list did not have an element in the second position: []
```
### **thread-finished?**
Check if the given thread is finished running.
### **thread-interrupt**
Interrupts the thread. Note, this will _not_ interrupt any native code
that is potentially running in the thread, and will attempt to block
at the next bytecode instruction that is running.
### **thread-join!**
Block until this thread finishes.
### **thread-resume**
Resume a suspended thread. This does nothing if the thread is already joined.
### **thread-suspend**
Suspend the thread. Note, this will _not_ interrupt any native code that is
potentially running in the thread, and will attempt to block at the next
bytecode instruction that is running.
### **time/sleep-ms**
Sleeps the thread for a given number of milliseconds.

(time/sleep-ms ms)

* ms : int?
### **to-string**
Concatenates all of the inputs to their string representation, separated by spaces.

(to-string xs ...)

* xs : any/c

#### Examples
```scheme
> (to-string 10) ;; => "10"
> (to-string 10 20) ;; => "10 20"
> (to-string "hello" "world") ;; => "hello world"
```
### **trim**
Returns a new string with the leading and trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo"
```
### **trim-end**
Returns a new string with the trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "   foo"
```
### **trim-end-matches**
Returns a new string with the given `pat` repeatedly removed from the end
of the string

```scheme
(trim-end-matches string? string?) -> string?
```

#### Examples
```scheme
> (trim-end-matches "123foo1bar123123" "123") ;; => "123foo1bar"
```
### **trim-start**
Returns a new string with the leading whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo     "
```
### **trim-start-matches**
Returns a new string with the given `pat` repeatedly removed from the start
of the string

```scheme
(trim-start-matches string? string?) -> string?
```

#### Examples
```scheme
> (trim-start-matches "123foo1bar123123" "123") ;; => "foo1bar123123"
```
### **utf8->string**
Alias of `bytes->string/utf8`.
### **value->jsexpr-string**
Serializes a Steel value into a string.

(value->jsexpr-string any/c) -> string?

#### Examples
```scheme
(value->jsexpr-string `(,(hash "foo" #t))) ;; => "[{\"foo\":true}]"
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
### **void**
The void value, returned by many forms with side effects, such as `define`.
### **would-block-object?**
Returns `#t` if the value is an EOF object.

(eof-object? any/c) -> bool?
### **write-byte**
Writes a single byte to an output port.

(write-byte b [port])

* b : byte?
* port : output-port? = (current-output-port)
### **write-bytes**
Writes the contents of a bytevector into an output port.

(write-bytes buf [port])

* buf : bytes?
* port : output-port? = (current-output-port)
### **zero?**
Checks if the given real number is zero.

(zero? num) -> boolean?

* num : real? - The number to check for zero.

#### Examples
```scheme
> (zero? 0) ;; => #t
> (zero? 0.0) ;; => #t
> (zero? 0.1) ;; => #f
```
### **%#interner-memory-usage**
### **%iterator?**
### **%keyword-hash**
### **=**
### **Engine::add-module**
### **Engine::clone**
### **Engine::modules->list**
### **Engine::new**
### **Engine::raise_error**
### **Err**
### **Err->value**
### **Err?**
### **None**
### **None?**
### **Ok**
### **Ok->value**
### **Ok?**
### **Some**
### **Some->value**
### **Some?**
### **TypeId?**
### **active-object-count**
### **arity?**
### **assert!**
### **atom?**
### **attach-contract-struct!**
### **block-on**
### **bool?**
### **boolean?**
### **box**
### **box-strong**
### **breakpoint!**
### **bytes-clear!**
### **bytes-push!**
### **call-with-current-continuation**
### **call-with-exception-handler**
### **call/cc**
### **cdr-null?**
### **channel->recv**
### **channel->send**
### **channel->try-recv**
### **channel/recv**
### **channel/send**
### **channel/try-recv**
### **channels-receiver**
### **channels-sender**
### **channels/new**
### **char?**
### **child-stderr**
### **child-stdin**
### **child-stdout**
### **command**
### **compose**
### **concat-symbols**
### **continuation?**
### **current-function-span**
### **current-os!**
### **current-thread-id**
### **dropping**
### **duration->seconds**
### **duration-since**
### **emit-expanded**
### **empty-stream**
### **enumerating**
### **env-var**
### **eq?**
### **equal?**
### **eqv?**
### **error-object?**
### **error-with-span**
### **eval**
### **eval!**
### **eval-string**
### **expand!**
### **extending**
### **feature-dylib-build?**
### **filtering**
### **flat-mapping**
### **flattening**
### **flush-output-port**
### **function-name**
### **function?**
### **future?**
### **get-contract-struct**
### **get-test-mode**
### **hash-get**
### **hash?**
### **immutable-vector?**
### **inspect**
### **instant/elapsed**
### **instant/now**
### **interleaving**
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
### **iter-next!**
### **join!**
### **kill**
### **list->string**
### **list->vector**
### **list-chunks**
### **list-drop**
### **list-tail**
### **list?**
### **load**
### **load-expanded**
### **local-executor/block-on**
### **make-channels**
### **make-struct-type**
### **mapping**
### **maybe-get-env-var**
### **memory-address**
### **multi-arity?**
### **mutable-vector?**
### **naive-current-date-local**
### **naive-date-and-hms**
### **naive-date-day**
### **naive-date-month**
### **naive-date-year**
### **naive-date-ymd**
### **not**
### **plist-get**
### **plist-get-kwarg**
### **plist-get-positional-arg**
### **plist-get-positional-arg-list**
### **plist-try-get**
### **plist-try-get-positional-arg**
### **plist-validate-args**
### **poll!**
### **port?**
### **procedure?**
### **push-back**
### **raise-error**
### **raise-error-with-span**
### **read!**
### **read-line-from-port**
### **read-to-string**
### **run!**
### **set-box!**
### **set-current-dir!**
### **set-env-var!**
### **set-piped-stdout!**
### **set-strong-box!**
### **set-test-mode!**
### **set?**
### **span-file-id**
### **spawn-process**
### **spawn-thread!**
### **stdout**
### **stdout-simple-displayln**
### **steel-home-location**
### **stream-car**
### **stream-cons**
### **stream-empty?**
### **string?**
### **struct->list**
### **struct?**
### **symbol->string**
### **symbol=?**
### **symbol?**
### **syntax->datum**
### **syntax-e**
### **syntax-loc**
### **syntax-originating-file**
### **syntax-span**
### **syntax/loc**
### **syntax?**
### **taking**
### **thread/available-parallelism**
### **thread::current/id**
### **transduce**
### **try-list-ref**
### **unbox**
### **unbox-strong**
### **value->iterator**
### **value->string**
### **vector-push!**
### **vector?**
### **void?**
### **wait**
### **wait->stdout**
### **which**
### **would-block**
### **write-line!**
### **zipping**
