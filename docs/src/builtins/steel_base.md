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
### **angle**
Computes the angle `θ` of a complex number `z` where `z = r * (cos θ + i sin θ)` and `r` is the magnitude.

(angle number) -> number?

- number : number?
### **append**
Appends the given lists together. If provided with no lists, will return
the empty list.

If the last element is not a list, an improper list will be returned

(append lst ...) -> list?  
(append lst ... v) -> any/c

* lst : list?
* v : any/c

#### Examples
```scheme
> (append (list 1 2) (list 3 4)) ;; => '(1 2 3 4)
> (append) ;; => '()
> (append (list 1 2) (cons 3 4)) ;; => '(1 2 3 . 4)
> (append '() 'a) ;; => 'a
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
### **bitwise-and**
Performs a bitwise and using the given numbers

(bitwise-and n ...) -> integer?

* n : integer?

#### Examples
```scheme
> (bitwise-and 1 2) ;; => 0
> (bitwise-and -32 -1) ;; => -32
```
### **bitwise-ior**
Performs a bitwise ior using the given numbers

(bitwise-ior n ...) -> integer?

* n : integer?

#### Examples
```scheme
> (bitwise-ior 1 2) ;; => 3
> (bitwise-ior -32 1) ;; => -31
```
### **bitwise-not**
Performs a bitwise not using the given numbers

(bitwise-not n ...) -> integer?

* n : integer?

#### Examples
```scheme
> (bitwise-not 5) ;; => -6
> (bitwise-not -1) ;; => 0
```
### **bitwise-xor**
Performs a bitwise xor using the given numbers

(bitwise-xor n ...) -> integer?

* n : integer?

#### Examples
```scheme
> (bitwise-xor 1 5) ;; => 4
> (bitwise-xor -32 -1) ;; => 31
```
### **bool?**
Alias for `boolean?`. Returns true if the value is a boolean.

(bool? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (bool? #false)
#true

> (bool? "hi")
#false
```
### **boolean?**
Returns true if the value is a boolean (`#true` or `#false`).

(boolean? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (boolean? #true)
#true

> (boolean? #false)
#true

> (boolean? 0)
#false
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
Returns canonical path with all components normalized.

(canonicalize-path path) -> string?

* path : string? - The path to canonicalize.

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

* number : real? - The number to round up.

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

#### Examples

```scheme
> (char->integer #\a) ;; => 97
> (char->integer #\λ) ;; => 955
```
### **char->number**
Attemps to convert the character into an ascii decimal digit,
and returns `#f` on failure.

(char->number char?) -> (or/c number? bool?)

#### Examples

```scheme
> (char->number #\4) ;; => 4
> (char->number #\a) ;; => #f
> (char->number #\٣) ;; => #f
```
### **char-ci<=?**
Returns `#t` if the characters are monotonically non-decreasing according to their codepoints,
in a case-insensitive fashion (as if char-foldcase was applied to the arguments).

(char-ci<=? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char-ci<=? #\a #\b) ;; => #t
 > (char-ci<=? #\a #\B) ;; => #t
 > (char-ci<=? #\a #\B #\c) ;; => #t
 > (char-ci<=? #\a #\B #\b) ;; => #t
 ```
### **char-ci<?**
Returns `#t` if the characters are monotonically increasing according to their codepoints,
in a case-insensitive fashion (as if char-foldcase was applied to the arguments).

(char-ci<? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char-ci<? #\a #\b) ;; => #t
 > (char-ci<? #\a #\B) ;; => #t
 > (char-ci<? #\a #\B #\c) ;; => #t
 > (char-ci<? #\a #\B #\b) ;; => #f
 ```
### **char-ci=?**
Checks if all characters are equal, in a case-insensitive fashion
(i.e. as if char-foldcase was applied to the arguments).

Requires that all inputs are characters, and will otherwise raise an error.

(char-ci=? char1 char2 ...) -> bool?

* char1 : char?
* char2 : char?

#### Examples

```scheme
> (char-ci=? #\s #\S) ;; => #t
> (char-ci=? #\ß #\ẞ) ;; => #t
> (char-ci=? #\σ #\Σ #\ς) ;; => #t
```
### **char-ci>=?**
Returns `#t` if the characters are monotonically non-increasing according to their codepoints,
in a case-insensitive fashion (as if char-foldcase was applied to the arguments).

(char-ci>=? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char-ci>? #\b #\a) ;; => #t
 > (char-ci>? #\B #\a) ;; => #t
 > (char-ci>? #\c #\B #\a) ;; => #t
 > (char-ci>? #\c #\B #\b) ;; => #t
 ```
### **char-ci>?**
Returns `#t` if the characters are monotonically decreasing according to their codepoints,
in a case-insensitive fashion (as if char-foldcase was applied to the arguments).

(char-ci>? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char-ci>? #\b #\a) ;; => #t
 > (char-ci>? #\B #\a) ;; => #t
 > (char-ci>? #\c #\B #\a) ;; => #t
 > (char-ci>? #\c #\B #\b) ;; => #f
 ```
### **char-digit?**
Returns `#t` if the character is an ascii decimal digit.

(char-digit? char?) -> bool?

#### Examples

```scheme
> (char-digit? #\4) ;; => #t
> (char-digit? #\a) ;; => #f
> (char-digit? #\٣) ;; => #f
> (char-digit? #\①) ;; => #f
```
### **char-downcase**
Returns the lower case version of a character, if defined by Unicode,
or the same character otherwise.

(char-downcase char?) -> char?

#### Examples

```scheme
> (char-downcase #\U) ;; => #\u
> (char-downcase #\d) ;; => #\d
> (char-downcase #\ẞ) ;; => #\ß
```
### **char-foldcase**
Apply simple unicode case-folding to a char

(char-foldcase char?) -> char?

#### Examples

```scheme
> (char-foldcase #\A) ;; => #\a
> (char-foldcase #\c) ;; => #\c
> (char-foldcase #\ς) ;; => #\σ
```
### **char-upcase**
Returns the upper case version of a character, if defined by Unicode,
or the same character otherwise.

(char-upcase char?) -> char?

#### Examples

```scheme
> (char-upcase #\d) ;; => #\D
> (char-upcase #\U) ;; => #\U
> (char-upcase #\ß) ;; => #\ß
```
### **char-whitespace?**
Returns `#t` if the character is a whitespace character.

#### Example

```scheme
> (char-whitespace? #\space) ;; => #t
> (char-whitespace? #\newline) ;; => #t
; nbsp character
> (char-whitespace? #\xA0) ;; => #t
> (char-whitespace? #\越) ;; => #f
```
### **char<=?**
Returns `#t` if the characters are monotonically non-decreasing according to their codepoints.

(char<=? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char<=? #\a #\b) ;; => #t
 > (char<=? #\a #\B) ;; => #f
 > (char<=? #\a #\b #\c) ;; => #t
 > (char<=? #\a #\b #\b) ;; => #t
 ```
### **char<?**
Returns `#t` if the characters are monotonically increasing according to their codepoints.

(char<? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char<? #\a #\b) ;; => #t
 > (char<? #\a #\b #\c) ;; => #t
 > (char<? #\a #\b #\b) ;; => #f
 ```
### **char=?**
Checks if all characters are equal.

Requires that all inputs are characters, and will otherwise raise an error.

(char=? char1 char2 ...) -> bool?

* char1 : char?
* char2 : char?

#### Examples

```scheme
> (char=? #\a #\a) ;; => #t
> (char=? #\a #\b) ;; => #f
> (char=? #\a #\A) ;; => #f
```
### **char>=?**
Returns `#t` if the characters are monotonically non-increasing according to their codepoints.

(char>=? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char>=? #\b #\a) ;; => #t
 > (char>=? #\c #\b #\a) ;; => #t
 > (char>=? #\c #\b #\b) ;; => #t
 ```
### **char>?**
Returns `#t` if the characters are monotonically decreasing according to their codepoints.

(char>? char1 char2 ... ) -> bool?
* char1 : char?
* char2 : char?
 # Examples

 ```scheme
 > (char>? #\b #\a) ;; => #t
 > (char>? #\c #\b #\a) ;; => #t
 > (char>? #\c #\b #\b) ;; => #f
 ```
### **close-input-port**
Close an input port. If the port is a file, the file will be closed.

(close-input-port input-port?) -> void
### **close-output-port**
Close an output port. If the port is a file, the file will be closed.

(close-output-port output-port?) -> void
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
### **concat-symbols**
Concatenates zero or more symbols into a new symbol.

(concat-symbols sym1 sym2 …) -> symbol?

* `sym1` : symbol? — the first symbol to append
* `sym2` : symbol? — the next symbol to append, and so on

#### Examples
```scheme
> (concat-symbols 'he 'llo)
=> 'hello
> (concat-symbols)
=> '
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
Recursively copies the contents of the source directory to the destination.

(copy-directory-recursively! source destination) -> void?

* source : string? - The directory to copy.
* destination : string? - The destination directory into which to copy.

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
### **dropping**
Creates a taking iterator combinator

(dropping integer?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3 4 5) (dropping 3) (into-list)) ;; => '(4 5)
```
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

* input : string?
* pattern: string?

#### Examples

```scheme
> (ends-with? "foobar" "foo") ;; => #false
> (ends-with? "foobar" "bar") ;; => #true
```
### **enumerating**
Create an enumerating iterator

(enumerating) -> iterator?

#### Examples
```scheme
(transduce (list 1 3 5) (enumerating) (into-list)) ;; => '((0 1) (1 3) (2 5))
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
### **euclidean-quotient**
Returns the quotient of a euclidean integer division of a given numerator *n*
by a given denominator *m*.

(euclidean-quotient n m) -> integer?

* n : integer?
* m : integer?

#### Examples

```scheme
> (euclidean-quotient 5 2) ;; => 2
> (euclidean-quotient -5 2) ;; => -3
> (euclidean-quotient 5 -2) ;; => -2
> (euclidean-quotient -5 -2) ;; => 3
```
### **euclidean-remainder**
Returns the arithmetic remainder of a euclidean integer division of a given
numerator *n* by a given denominator *m*.

The return value of this procedure is always positive.

(euclidean-remainder n m) -> integer?

* n : integer?
* m : integer?

#### Examples

```scheme
> (euclidean-remainder 5 2) ;; => 1
> (euclidean-remainder -5 2) ;; => 1
> (euclidean-remainder 5 -2) ;; => 1
> (euclidean-remainder -5 -2) ;; => 1
```
### **euclidean/**
Simultaneously returns the quotient and the arithmetic remainder of a euclidean
integer division of a given numerator *n* by a given denominator *m*.

Equivalent to `(values (euclidean-quotient n m) (euclidean-remainder n m))`,
but may be computed more efficiently.

(euclidean/ n m) -> (integer? integer?)

* n : integer?
* m : integer?

#### Examples

```scheme
> (euclidean/ 5 2) ;; => (2 1)
> (euclidean/ -5 2) ;; => (-3 1)
> (euclidean/ 5 -2) ;; => (-2 1)
> (euclidean/ -5 -2) ;; => (3 1)
```
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
Converts a number to an exact number.

(exact num) -> number?

* num : number? - The value to convert to exact.

#### Examples
```scheme
> (exact 10.0) ;; => 10
> (exact 1.5) ;; => 3/2
> (exact 1.5+2.5i) ;; => 3/2+5/2i
```
### **exact->inexact**
Alias of `inexact`.
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
### **extending**
Create an extending iterator

(extending iterable) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (extending (list 4 5 6 7)) (into-list)) ;; => '(1 2 3 4 5 6 7)
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
### **file-metadata**
Access the file metadata for a given path
### **file-name**
Gets the filename for a given path.

(file-name path) -> string?

* path : string? - The path from which to get the file name.

#### Examples
```scheme
> (file-name "logs") ;; => "logs"
> (file-name "logs/today.json") ;; => "today.json"
```
### **filtering**
Creates a filtering iterator

(filtering proc?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3 4) (filtering even?) (into-list)) ;; => '(2 4)
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
Rounds the given number down to the nearest integer not larger than it.

(floor number) -> number?

* number : real? - The number to compute the floor for.

#### Examples
```scheme
> (floor 3.14) ;; => 3
> (floor 4.99) ;; => 4
> (floor -2.5) ;; => -3
```
### **floor-quotient**
Returns the quotient of a floored integer division of a given numerator *n*
by a given denominator *m*.

Equivalent to `(values (floor-quotient n m) (floor-remainder n m))`, but
may be computed more efficiently.

(floor-quotient n m) -> integer?

* n : integer?
* m : integer?

#### Examples

```scheme
> (floor-quotient 5 2) ;; => 2
> (floor-quotient -5 2) ;; => -3
> (floor-quotient 5 -2) ;; => -3
> (floor-quotient -5 -2) ;; => 2
```
### **floor-remainder**
Returns the arithmetic remainder of a floored integer division of a given
numerator *n* by a given denominator *m*.

The return value of this procedure has the same sign as the denominator.

(floor-remainder n m) -> integer?

* n : integer?
* m : integer?

#### Examples

```scheme
> (floor-remainder 5 2) ;; => 1
> (floor-remainder -5 2) ;; => 1
> (floor-remainder 5 -2) ;; => -1
> (floor-remainder -5 -2) ;; => -1
```
### **floor/**
Simultaneously returns the quotient and the arithmetic remainder of a floored
integer division of a given numerator *n* by a given denominator *m*.

(floor/ n m) -> (integer? integer?)

* n : integer?
* m : integer?

#### Examples

```scheme
> (floor/ 5 2) ;; => (2 1)
> (floor/ -5 2) ;; => (-3 1)
> (floor/ 5 -2) ;; => (-3 -1)
> (floor/ -5 -2) ;; => (2 -1)
```
### **fs-metadata-accessed**
Get the last accessed time from the file metadata
### **fs-metadata-created**
Get the created time from the file metadata
### **fs-metadata-is-dir?**
Check if this metadata is from a directory
### **fs-metadata-is-file?**
Check if this metadata is from a file
### **fs-metadata-is-symlink?**
Check if this metadata is from a symlink
### **fs-metadata-len**
Get the length of the file in bytes
### **fs-metadata-modified**
Get the last modified time from the file metadata
### **fs-metadata?**
Checks if this value is a #<Metadata>
### **function?**
Returns true if the value is a function or callable.

(function? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (function? (lambda (x) x))
#true

> (function? map)
#true

> (function? 42)
#false
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

* key : any/c
* val : any/c

Note: the keys must be hashable.

#### Examples
```scheme
> (hash 'a 10 'b 20)
=> '#hash((a . 10) (b . 20))
```
### **hash->list**
Returns a list of the key-value pairs of a given hash map.

(hash->list map) -> (listof (cons/c any/c any/c))

* map : hash?

#### Examples

```scheme
> (hash->list (hash 'a 10 'b 20)) ;; => '((a . 10) (b . 20))
```
### **hash->vector**
Returns a list of the key-value pairs of a given hash map.

(hash->vector map) -> (vectorof (cons/c any/c any/c))

* map : hash?

#### Examples

```scheme
> (hash->vector (hash 'a 10 'b 20)) ;; => '#((a . 10) (b . 20))
```
### **hash-clear**
Clears the entries out of the existing hashmap.
Will attempt to reuse the existing memory if there are no other references
to the hashmap.

(hash-clear map) -> hash?

* map : hash?

#### Examples
```scheme
> (hash-clear (hash 'a 10 'b 20))
=> '#hash()
```
### **hash-code**
Gets the hash code for the given value;

(hash-code v) -> integer?

* v : any/c

#### Examples
```scheme
(hash-code 10) ;; => 16689870864682149525
(hash-code "hello world") ;; => 12361891819228967546
```
### **hash-contains?**
Checks whether the given map contains the given key. Key must be hashable.

(hash-contains? map key) -> bool?

* map : hash?
* key : any/c

#### Example

```scheme
> (hash-contains? (hash 'a 10 'b 20) 'a) ;; => #true
> (hash-contains? (hash 'a 10 'b 20) 'not-there) ;; => #false
```
### **hash-empty?**
Checks whether the hash map is empty

(hash-empty? map) -> bool?

* map : hash?

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
=> '#hash((a . 10) (b . 20) (c . 30))
```
### **hash-keys->list**
Returns the keys of the given hash map as a list.

(hash-keys->list map) -> (listof any/c)

* map : hash?

#### Examples

```scheme
> (hash-keys->list (hash 'a 10 'b 20))
=> '(a b)
```
### **hash-keys->vector**
Returns the keys of the given hash map as an immutable vector

(hash-keys->vector map) -> (vectorof any/c)

* map: hash?

#### Examples
```scheme
> (hash-keys->vector (hash 'a 10 'b 20))
=> '#(a b)
```
### **hash-length**
Returns the number of key value pairs in the map

(hash-length map) -> (and/c positive? int?)

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

(hash-values->list map) -> (listof any/c)

* map : hash?

#### Examples

```scheme
> (hash-values->list (hash 'a 10 'b 20))
=> '(10 20)
```
### **hash-values->vector**
Returns the values of the given hash map as an immutable vector

(hash-values->vector map) -> (vectorof any/c)?

* map : hash?

#### Examples

```scheme
> (hash-values->vector (hash 'a 10 'b 20))
=> '#(10 20)
```
### **hash?**
Returns true if the value is a hash map.

(hash? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (hash? (hash 'a 10 'b 20))
#true

> (hash? '(a b c))
#false
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
### **hashset-difference**
Finds the difference between the two hash sets.

#### Examples
```scheme
(hashset-difference (hashset 10 20 30) (hashset 20 30 40)) ;; => (hashset 40 10)
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
### **hashset-intersection**
Finds the intersection between the two hash sets.

#### Examples
```scheme
(hashset-intersection (hashset 10 20) (hashset 20)) ;; => (hashset 10)
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
### **hashset-union**
Finds the union between the two hash sets.

#### Examples
```scheme
(hashset-union (hashset 10) (hashset 20)) ;; => (hashset 10 20)
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
### **inexact**
Converts a number to an inexact number.

(inexact num) -> number?

* num : number? - The number to convert from exact to inexact.

#### Examples
```scheme
> (inexact 10) ;; => 10
> (inexact 1/2) ;; => 0.5
> (inexact 1+2i) ;; => 1+2i
```
### **inexact->exact**
Alias of `exact`.
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

Use of this procedure is discouraged in favor of the more powerful and more
portable `(number->string)` procedure.

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

#### Examples

```scheme
> (integer->char #x61) ;; => #\a
> (integer->char 955) ;; => #\λ
```
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
### **interleaving**
Create an interleaving iterator

(interleaving any/c) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (interleaving (list 4 5 6)) (into-list)) ;; => '(1 4 2 5 3 6)
```
### **is-dir?**
Checks if a path is a directory.

(is-dir? path) -> bool?

* path : string? - The path to check.

#### Examples
```scheme
> (is-dir? "logs") ;; => #true
> (is-dir? "logs/today.json") ;; => #false
```
### **is-file?**
Checks if a path is a file.

(is-file? path) -> bool?

* path : string? - The path to check.

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
### **list->string**
Convert a list of charecters to a string.

(list->string lst) -> string?

* lst : (listof char?)

#### Examples
```scheme
(list->string '(#\a #\b #\c)) ;; => "abc"
```
### **list-drop**
Remove a certain number of elements (`n`) from the front of `lst`.

(list-drop lst n) -> any/c

* lst : list?
* n : int?

#### Examples
```scheme
> (list-drop '(1 2 3 4 5) 2) ;; => '(3 4 5)
> (list-drop '() 3) ;; => '()
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
> (list-ref (range 0 100) 42) ;; => 42
> (list-ref (list 1 2 3 4) 10)
error[E11]: Generic
  ┌─ :1:2
  │
1 │ (list-ref (list 1 2 3 4) 10)
  │  ^^^^^^^^ out of bounds index in list-ref - list length: 4, index: 10
```
### **list-tail**
Same as `list-drop`, except raise an error if `n` is greater than the length of `lst`.

(list-tail lst n) -> any/c

* lst : list?
* n : int?

#### Examples
```scheme
> (list-tail '(1 2 3 4 5) 2) ;; => '(3 4 5)
> (list-tail '() 3)
error[E11]: Generic
  ┌─ :1:2
  │
1 │ (list-tail '() 3)
  │  ^^^^^^^^^ list-tail expects at least 3
                    elements in the list, found: 0
```
### **list?**
Returns true if the value is a list.

(list? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (list? '(1 2 3))
#true

> (list? "not-a-list")
#false
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
### **make-polar**
Make a complex number out of a magnitude `r` and an angle `θ`, so that the result is `r * (cos θ + i sin θ)`

(make-polar r θ) -> number?

- r : real?
- theta : real?
### **make-rectangular**
Create a complex number with `re` as the real part and `im` as the imaginary part.

(make-rectangular re im) -> number?

- re : real?
- im : real?
### **make-string**
Creates a string of a given length, filled with an optional character
(which defaults to `#\0`).

(make-string len [char]) -> string?

* len : int?
* char : char? = #\0

#### Examples

```scheme
> (make-string 5 #\a) ;; => "aaaaa"
> (make-string 5) ;; => "\0\0\0\0\0"
```
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
### **make-weak-box**
Allocates a weak box.

A weak box is similar to a box, but when the garbage collector can prove
that the value of a weak box is only reachable through weak references,
the weak box value will always return #false.

In other words, a weak box does not keep the value contained alive through
a gc collection.
### **mapping**
Create a mapping iterator

(mapping proc?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (mapping (λ (x) (+ x 1))) (into-list)) ;; => '(2 3 4)
```
### **member**
Return the first tail of the list, where the car is `equal?` to the given obj.
Returns `#f`, if no element is found.

(member obj lst) -> (or/c list? #f)

* obj : any/c
* lst : list?

```scheme
(member #\c '(#\a #\b #\c #\d #\e)) ;; => '(#\c #\d #\e)
(member 5 '(0 1 2 3 4)) ;; => #f
```
### **memq**
Return the first tail of the list, where the car is `eq?` to the given obj.
Returns `#f`, if no element is found.

This procedure is equivalent to `member`, but using `eq?` instead of `equal?`.

(memq obj lst) -> (or/c list? #f)

* obj : any/c
* lst : list?

```scheme
(memq #\c '(#\a #\b #\c #\d #\e)) ;; => '(#\c #\d #\e)
(memq 5 '(0 1 2 3 4)) ;; => #f
```
### **modulo**
Returns the arithmetic remainder of a floored integer division of a given
numerator *n* by a given denominator *m*.

The return value of this procedure has the same sign as the denominator.

This procedure is an alias of `floor-remainder`.

(modulo n m) -> integer?

* n : integer?
* m : integer?

#### Examples

```scheme
> (modulo 5 2) ;; => 1
> (modulo -5 2) ;; => 1
> (modulo 5 -2) ;; => -1
> (modulo -5 -2) ;; => -1
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
### **not**
Returns true if the given value is exactly `#false`.

(not value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (not #false)
#true

> (not #true)
#false

> (not "hello")
#false
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
Converts the given number to a string, with an optional radix.

Returns an error, if the value given is not a number.

(number->string number? [radix]) -> string?

* radix: number?

```scheme
> (number->string 10) ;; => "10"
> (number->string 1.0) ;; => "1.0"
> (number->string 1/2) ;; => "1.0"
> (number->string 1+2i) ;; => "1+2i"
> (number->string 255 16) ;; => "ff"
> (number->string 1/2 2) ;; => "1/10"
```
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
> (open-input-file "foo-bar.txt") ;; => #<input-port:foo-bar.txt>
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
> (open-output-file "foo-bar.txt") ;; => #<output-port:foo-bar.txt>
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
Gets the parent directory name for a given path.

(parent-name path) -> string?

* path : string? - The path from which to get the parent.

#### Examples
```scheme
> (parent-name "logs") ;; => ""
> (parent-name "logs/today.json") ;; => "logs"
```
### **path->extension**
Gets the extension from a path.

(path->extension path) -> (or/c string? void?)

* path : string? - The path from which to get the extension.

#### Examples
```scheme
> (path->extension "logs") ;; => void
> (path->extension "logs/today.json") ;; => ".json"
```
### **path-exists?**
Checks if a path exists.

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
### **peek-char**
Peeks the next character from an input port.

(peek-char [port]) -> char?

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
Returns the quotient of a truncated integer division of a given numerator *n*
by a given denominator *m*.

This procedure is an alias of `truncate-quotient`.

(quotient n m) -> integer?

* n : integer? - The numerator.
* m : integer? - The denominator.

#### Examples

```scheme
> (quotient 5 2) ;; => 2
> (quotient -5 2) ;; => -2
> (quotient 5 -2) ;; => -2
> (quotient -5 -2) ;; => 2
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

(read-bytes-into-buf buf amt [port]) -> int?

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

* path : string? - The path to the directory.

#### Examples
```scheme
> (read-dir "logs") ;; => '("logs/today.json" "logs/yesterday.json")
> (read-dir "empty_dir") ;; => '()
```
### **read-dir-entry-file-name**
Returns the file name from a given read-dir-entry.
### **read-dir-entry-is-dir?**
Checks whether the read dir entry is a directory.

(read-dir-entry-is-dir? value) -> bool?

* value : read-dir-iter-entry?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define next (read-dir-iter-next! my-iter))

(read-dir-entry-path) ;; => "src/lib.rs"
(read-dir-entry-is-dir? next) ;; #false - because this is a file

```
### **read-dir-entry-is-file?**
Checks whether the read dir entry is a file.

(read-dir-entry-is-dir? value) -> bool?

* value : read-dir-iter-entry?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define next (read-dir-iter-next! my-iter))

(read-dir-entry-path) ;; => "src/lib.rs"
(read-dir-entry-is-dir? next) ;; #true - because this is a file

```
### **read-dir-entry-is-symlink?**
Checks whether the read dir entry is a symlink.
### **read-dir-entry-metadata**
Extract the file metadata from the #<DirEntry>
### **read-dir-entry-path**
Returns the path from a given read-dir-entry.
### **read-dir-iter**
Creates an iterator over the contents of the given directory.
The given path must be a directory.

(read-dir-iter dir) -> #<ReadDir>

* dir : (is-dir?) - the directory to iterate over

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(read-dir-iter-next! my-iter) ;; => #<DirEntry> src/lib.rs
(read-dir-iter-next! my-iter) ;; => #<DirEntry> src/main.rs
(read-dir-iter-next! my-iter) ;; => #false
```
### **read-dir-iter-entry?**
Checks whether the given value is a #<DirEntry>

(read-dir-iter-entry? value) -> bool?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define next (read-dir-iter-next! my-iter))
(read-dir-iter-entry? next) ;; => #true
```
### **read-dir-iter-next!**
Reads one entry from the iterator. Reads a `ReadDir` struct.

(read-dir-iter-next! read-dir-iter) -> #<DirEntry>

* dir : (read-dir-iter?) - the directory to iterate over

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(define nex-entry (read-dir-iter-next! my-iter)) ;; => #<DirEntry> src/lib.rs
(read-dir-entry-is-dir? next-entry) ;; => #false
(read-dir-entry-is-file? next-entry) ;; => #true
(read-dir-entry-file-name) ;; => "lib.rs"
```
### **read-dir-iter?**
Checks whether the given value is a #<ReadDir>

(read-dir-iter? value) -> bool?

#### Examples
```scheme
(define my-iter (read-dir-iter "src"))
(read-dir-iter? my-iter) ;; => #true
(read-dir-iter "not an iter") ;; => #false
```
### **read-line**
Reads a line from an input port.

(read-line [port]) -> string?

* port : input-port? = (current-input-port)
### **read-line-from-port**
Reads a line from the given port, including the '\n' at the end.

Use of this procedure is discouraged in favor of the (read-line) procedure,
which is included in the scheme spec and therefore more portable.

(read-line-from-port port?) -> string?
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
Returns the arithmetic remainder of a truncated integer division of a given
numerator *n* by a given denominator *m*.

The return value of this procedure has the same sign as the numerator.

This procedure is an alias of `truncate-remainder`.

(remainder n m) -> integer?

* n : integer?
* m : integer?

#### Examples

```scheme
> (remainder 5 2) ;; => 1
> (remainder -5 2) ;; => -1
> (remainder 5 -2) ;; => 1
> (remainder -5 -2) ;; => -1
```
### **rest**
Returns the rest of the list. Will raise an error if the list is empty.

(rest l) -> list?

* l : list?

#### Examples
```scheme
> (rest (list 10 20 30)) ;; => '(20 30)
> (rest (list 10)) ;; => '()
> (rest '())
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

* lst : list?

#### Examples
```scheme
> (reverse (list 1 2 3 4)) ;; '(4 3 2 1)
```
### **round**
Rounds the given number to the nearest integer, rounding half-way cases to
the even number.

(round number) -> number?

* number : real? - The number to round.

#### Examples
```scheme
> (round 3.14) ;; => 3
> (round 4.6) ;; => 5
> (round 2.5) ;; => 2
> (round 3.5) ;; => 4
> (round -2.5) ;; => -2
```
### **second**
Get the second element of the list. Raises an error if the list does not have an element in the second position.

(second l) -> any/c

* l : list?

#### Examples

```scheme
> (second '(1 2 3)) ;; => 2
error[E11]: Generic
  ┌─ :1:2
  │
1 │ (second '())
  │  ^^^^^^ second: index out of bounds - list did not have an element in the second position: []
```
### **set-tls!**
Set the value in the the thread local storage. Only this thread will see the updates associated
with this TLS.
### **set?**
Returns true if the value is a hash set.

(set? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (set? (hashset 10 20 30 40))
#true

> (set? "abc")
#false
```
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

```scheme
(spawn-native-thread func)
```

func : (-> any?) ;; Function with no arguments, returns anything

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
(split-whitespace "one\t \ttwo\nthree") ;; '("one" "two" "three")
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
> (stdin) ;; => #<input-port:stdin>
```
### **string**
Constructs a string from the given characters

(string . char?) -> string?

#### Examples

```scheme
> (string #\h #\e #\l #\l #\o) ;; => "hello"
> (string #\λ) ;; => "λ"
> (string) ;; => ""
```
### **string->bytes**
Encodes a string as UTF-8 into a bytevector.

(string->bytes str [start] [end]) -> bytes?

* str : string?
* start : int? = 0
* end : int? = (string-length str)

#### Examples

```scheme
(string->bytes "Apple") ;; => #u8(#x41 #x70 #x70 #x6C #x65)
(string->bytes "αβγ") ;; => #u8(#xCE #xB1 #xCE #xB2 #xCE #xB3)
(string->bytes "one two three" 4 7) ;; => #u8(#x74 #x77 #x6F)
```
### **string->int**
Converts a string into an int. Raises an error if the string cannot be converted to an integer.

Use of this procedure is discouraged in favor of the more powerful and more
portable `(string->number)` procedure.

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

(string->list str [start] [end]) -> (listof char?)

* str : string?
* start : int? = 0
* end : int? = (string-length str)

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
> (string->list "one two three" 4 7) ;; => '(#\t #\w #\o)
```
### **string->lower**
Alias of `string-downcase`.
### **string->number**
Converts the given string to a number, with an optional radix.
On failure, it returns `#f`

(string->number digits [radix]) -> (or/c number? boolean?)

* digits : string?
* radix : number?

#### Examples

```scheme
> (string->number "10") ;; => 10
> (string->number "1.0") ;; => 1.0
> (string->number "1/2") ;; => 1/2
> (string->number "1+2i") ;; => 1+2i
> (string->number "ff") ;; => #f
> (string->number "ff" 16) ;; => 255
> (string->number "1/10" 2) ;; => 1/2
> (string->number "not-a-number") ;; => #f
```
### **string->symbol**
Returns an interned symbol from the given string.

(string->symbol string?) -> symbol?

#### Examples

```scheme
> (string->symbol "abc") ;; => 'abc
> (string->symbol "pea pod") ;; => '|pea pod|
```
### **string->uninterned-symbol**
Return an uninterned symbol from the given string.

(string->uninterned-symbol string?) -> symbol?

#### Examples

```scheme
(string->uninterned-symbol "abc") ;; => 'abc
(string->uninterned-symbol "pea pod") ;; => '|pea pod|
```
### **string->upper**
Alias of `string-upcase`.
### **string->utf8**
Alias of `string->bytes`.
### **string->vector**
Returns a vector containing the characters of a given string

(string->vector s [start] [end]) -> vector?

* str : string?
* start : int? = 0
* end : int? = (string-length str)

#### Examples

```scheme
(string->vector "hello") ;; => '#(#\h #\e #\l #\l #\o)
(string->vector "one two three" 4 7) ;; => '#(#\t #\w #\o)
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
 # Examples

 ```scheme
 > (string-ci<=? "a" "b") ;; => #t
 > (string-ci<=? "a" "B") ;; => #t
 > (string-ci<=? "a" "B" "c") ;; => #t
 > (string-ci<=? "a" "B" "b") ;; => #t
 > (string-ci<=? "Straßburg" "STRASSE" "straßenbahn") ;; => #t
 ```
### **string-ci<?**
Compares strings lexicographically (as in"less-than"),
in a case insensitive fashion.

(string-ci<? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string-ci<? "a" "b") ;; => #t
 > (string-ci<? "a" "B") ;; => #t
 > (string-ci<? "a" "B" "c") ;; => #t
 > (string-ci<? "a" "B" "b") ;; => #f
 > (string-ci<? "Straßburg" "STRASSE" "straßenbahn") ;; => #t
 ```
### **string-ci=?**
Compares strings for equality, in a case insensitive fashion.

(string-ci=? string? string? ...) -> bool?

#### Examples

```scheme
> (string-ci=? "hEllO WorLd" "HELLO worlD") ;; => #t
> (string-ci=? "Straße" "STRASSE" "strasse" "STRAẞE") ;; => #t
> (string-ci=? "ὈΔΥΣΣΕΎΣ" "ὀδυσσεύς" "ὀδυσσεύσ") ;; => #t
```
### **string-ci>=?**
Compares strings lexicographically (as in"greater-than-or-equal"),
in a case insensitive fashion.

(string-ci>=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string-ci>=? "b" "a") ;; => #t
 > (string-ci>=? "B" "a") ;; => #t
 > (string-ci>=? "c" "B" "a") ;; => #t
 > (string-ci>=? "c" "B" "b") ;; => #f
 > (string-ci>=? "straßenbahn" "STRASSE" "Straßburg") ;; => #t
 ```
### **string-ci>?**
Compares strings lexicographically (as in"greater-than"),
in a case insensitive fashion.

(string-ci>? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string-ci>? "b" "a") ;; => #t
 > (string-ci>? "B" "a") ;; => #t
 > (string-ci>? "c" "B" "a") ;; => #t
 > (string-ci>? "c" "B" "b") ;; => #f
 > (string-ci>? "straßenbahn" "STRASSE" "Straßburg") ;; => #t
 ```
### **string-contains?**
Searches a string to check if it contains the second argument.

(string-contains? string? string?) -> bool?

#### Examples
```scheme
(string-contains? "hello" "lo") ;;=> #t
(string-contains? "hello" "world") ;;=> #f
```
### **string-downcase**
Creates a new lowercased version of the input string

(string-downcase string?) -> string?

#### Examples

```scheme
> (string-downcase "sPonGeBoB tExT") ;; => "spongebob text"
> (string-downcase "ὈΔΥΣΣΕΎΣ") ;; => "ὀδυσσεύς"
> (string-downcase "STRAẞE") ;; => "straße"
```
### **string-foldcase**
Applies full unicode case-folding to the input string

(string-foldcase string?) -> string?

#### Examples

```scheme
> (string-foldcase "Straße") ;; => "strasse"
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
Get the number of characters in the string.

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
> (string-length "αβγ") ;; => 3
> (string-length "✅") ;; => 1
```
### **string-ref**
Extracts the nth character out of a given string, starting at 0.

(string-ref str n) -> char?

* str : string?
* n : int?

```scheme
(string-ref "one" 1) ;; => #\n
(string-ref "αβγ" 1) ;; => #\β
```
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
### **string-upcase**
Creates a new uppercased version of the input string

(string-upcase string?) -> string?

#### Examples

```scheme
> (string-upcase "lower") ;; => "LOWER"
> (string-upcase "straße") ;; => "STRASSE"
```
### **string<=?**
Compares strings lexicographically (as in"less-than-equal-to").

(string<=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string<=? "a" "b") ;; => #t
 > (string<=? "a" "b" "c") ;; => #t
 > (string<=? "a" "b" "b") ;; => #t
 ```
### **string<?**
Compares strings lexicographically (as in"less-than").

(string<? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string<? "a" "b") ;; => #t
 > (string<? "a" "b" "c") ;; => #t
 > (string<? "a" "b" "b") ;; => #f
 ```
### **string=?**
Compares strings for equality.

(string=? string1 string2 ...) -> bool?

* string1 : string?
* string2 : string?

#### Examples

```scheme
> (string=? "hello" "hello") ;; => #t
> (string=? "hello" "HELLO") ;; => #f
```
### **string>=?**
Compares strings lexicographically (as in"greater-than-or-equal").

(string>=? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string>=? "b" "a") ;; => #t
 > (string>=? "c" "b" "a") ;; => #t
 > (string>=? "c" "b" "b") ;; => #t
 ```
### **string>?**
Compares strings lexicographically (as in"greater-than").

(string>? s1 s2 ... ) -> bool?
* s1 : string?
* s2 : string?
 # Examples

 ```scheme
 > (string>? "b" "a") ;; => #t
 > (string>? "c" "b" "a") ;; => #t
 > (string>? "c" "b" "b") ;; => #f
 ```
### **string?**
Returns true if the value is a string.

(string? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (string? "hello")
#true

> (string? 'foo)
#false
```
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
### **symbol->string**
Converts a symbol or quoted list into its string representation.

(symbol->string sym) -> string?

* `sym` : symbol? | list? — a symbol or quoted list to convert

#### Examples
```scheme
> (symbol->string 'foo)
"foo"

> (symbol->string '(a b c))
"(a b c)"

> (symbol->string 123)
Error: symbol->string expected a symbol, found 123
```
### **symbol=?**
Compares one or more symbols for pointer‐identity equality.

(symbol=? sym1 sym2 …) -> bool?

* `sym1` : symbol? — the first symbol to compare
* `sym2` : symbol? — the next symbol to compare, and so on

Returns `#t` if all provided symbols share the same memory pointer,
`#f` otherwise. At least one argument is required.

#### Examples
```scheme
> (define a 'foo)
> (define b 'foo)
> (symbol=? a b)
=> #t
> (symbol=? 'a 'b)
=> #f
> (symbol=? 'x 'x 'x)
=> #t
```
### **symbol?**
Returns true if the value is a symbol.

(symbol? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (symbol? 'hello)
#true

> (symbol? "hello")
#false
```
### **system-time-duration-since**
Gets the duration between two system times.

(system-time-duration-since time earlier)
### **system-time/now**
Returns the current `SystemTime`.

(system-time/now) -> SystemTime?
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
### **taking**
Creates a taking iterator combinator

(taking number?) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3 4 5) (taking 3) (into-list)) ;; => '(1 2 3)
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
  │  ^^^^^ third: Index out of bounds - list did not have an element in the second position: []
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
### **truncate**
Rounds the given number to the nearest integer, whose absolute value is not
larger than it.

(truncate number) -> integer?

* number : real? - The number to truncate.

#### Examples

```scheme
> (truncate 42) ;; => 42
> (truncate 42.1) ;; => 42
> (truncate -42.1) ;; => -42
```
### **truncate-quotient**
Returns the quotient of a truncated integer division of a given numerator *n*
by a given denominator *m*.

(truncate-quotient n m) -> integer?

* n : integer? - The numerator.
* m : integer? - The denominator.

#### Examples

```scheme
> (truncate-quotient 5 2) ;; => 2
> (truncate-quotient -5 2) ;; => -2
> (truncate-quotient 5 -2) ;; => -2
> (truncate-quotient -5 -2) ;; => 2
```
### **truncate-remainder**
Returns the arithmetic remainder of a truncated integer division of a given
numerator *n* by a given denominator *m*.

The return value of this procedure has the same sign as the numerator.

(truncate-remainder n m) -> integer?

* n : integer? - The numerator.
* m : integer? - The denominator.

#### Examples

```scheme
> (truncate-remainder 5 2) ;; => 1
> (truncate-remainder -5 2) ;; => -1
> (truncate-remainder 5 -2) ;; => 1
> (truncate-remainder -5 -2) ;; => -1
```
### **truncate/**
Simultaneously returns the quotient and the arithmetic remainder of a truncated
integer division of a given numerator *n* by a given denominator *m*.

Equivalent to `(values (truncate-quotient n m) (truncate-remainder n m))`,
but may be computed more efficiently.

(truncate/ n m) -> (integer? integer?)

* n : integer?
* m : integer?

#### Examples

```scheme
> (truncate/ 5 2) ;; => (2 1)
> (truncate/ -5 2) ;; => (-2 -1)
> (truncate/ 5 -2) ;; => (-2 1)
> (truncate/ -5 -2) ;; => (2 -1)
```
### **utf8->string**
Alias of `bytes->string/utf8`.
### **utf8-length**
Get the length of the string in UTF-8 bytes.

(utf8-length string?) -> int?

#### Examples

```scheme
> (utf8-length "apples") ;; => 6
> (utf8-length "αβγ") ;; => 6
> (utf8-length "✅") ;; => 3
```
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
### **vector-swap!**
Swaps the value of the specified indices in a mutable vector.

(vector-swap! vec a b) -> void?

* vec : vector? - The mutable vector to modify.
* a : integer? - The first index of `vec` to swap with `b` (must be within bounds).
* b : integer? - The first index of `vec` to swap with `a` (must be within bounds).

#### Examples
```scheme
> (define A (mutable-vector 1 2 3)) ;;
> (vector-swap! A 0 1) ;;
> A ;; => '#(2 1 3)
```
### **vector?**
Returns true if the value is a vector (mutable or immutable).

(vector? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (vector? #(1 2 3))
#true

> (vector? 'foo)
#false
```
### **void**
The void value, returned by many forms with side effects, such as `define`.
### **void?**
Returns true if the value is `void`.

(void? value) -> boolean?

* `value` : any — the value to test

#### Examples
```scheme
> (void? void)
#true

> (void? 42)
#false
```
### **weak-box-value**
Returns the value contained in the weak box.
If the garbage collector has proven that the previous content
value of weak-box was reachable only through a weak reference,
then default-value (which defaults to #f) is returned.

```scheme
(define value (make-weak-box 10))
(weak-box-value value) ;; => 10
(set! value #f) ;; Wipe out the previous value
(#%gc-collect)
(weak-box-value value) ;; => #false
```
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
### **zipping**
Create a zipping iterator

(zipping any/c) -> iterator?

#### Examples
```scheme
(transduce (list 1 2 3) (zipping (list 4 5 6 7)) (into-list)) ;; => '((1 4) (2 5) (3 6))
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
### **arity-object->list**
### **arity?**
### **assert!**
### **atom?**
### **attach-contract-struct!**
### **block-on**
### **box**
### **box-strong**
### **breakpoint!**
### **bytes-clear!**
### **bytes-push!**
### **call-with-current-continuation**
### **call-with-exception-handler**
### **call/cc**
### **callstack-hydrate-names**
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
### **continuation?**
### **current-function-span**
### **current-module**
### **current-os!**
### **current-thread-id**
### **dump-profiler**
### **duration->micros**
### **duration->millis**
### **duration->nanos**
### **duration->seconds**
### **duration-since**
### **emit-expanded**
### **empty-stream**
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
### **feature-dylib-build?**
### **flush-output-port**
### **function-arity**
### **function-name**
### **future?**
### **futures-join-all**
### **get-contract-struct**
### **get-test-mode**
### **glob**
### **glob-iter-next!**
### **hash-get**
### **immutable-vector?**
### **inspect**
### **instant/elapsed**
### **instant/now**
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
### **list->vector**
### **list-chunks**
### **list-contains**
### **load**
### **load-expanded**
### **local-executor/block-on**
### **make-callstack-profiler**
### **make-channels**
### **make-struct-type**
### **make-will-executor**
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
### **path->string**
### **path-separator**
### **platform-dll-extension!**
### **platform-dll-prefix!**
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
### **read-to-string**
### **run!**
### **set-box!**
### **set-current-dir!**
### **set-env-var!**
### **set-piped-stdout!**
### **set-strong-box!**
### **set-test-mode!**
### **span-file-id**
### **spawn-process**
### **stdout**
### **stdout-simple-displayln**
### **steel-home-location**
### **stream-car**
### **stream-cons**
### **stream-empty?**
### **string-push**
### **struct->list**
### **struct?**
### **syntax->datum**
### **syntax-e**
### **syntax-loc**
### **syntax-originating-file**
### **syntax-span**
### **syntax/loc**
### **syntax?**
### **system-time<=**
### **system-time<?**
### **system-time>=**
### **system-time>?**
### **target-arch!**
### **thread/available-parallelism**
### **thread::current/id**
### **transduce**
### **try-list-ref**
### **unbox**
### **unbox-strong**
### **value->iterator**
### **value->string**
### **vector-push!**
### **wait**
### **wait->stdout**
### **which**
### **will-execute**
### **will-register**
### **would-block**
### **write-line!**
