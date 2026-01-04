
# steel/core/option
# steel/core/result
# steel/identity
# steel/hash
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
> (hash-get (hash 'a 10 'b 20) 'b) ;; => 20
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
# steel/random
# steel/io
# steel/equality
# steel/ord
# steel/json
# steel/sets
# steel/time
### **steel/time**


#### steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

# steel/vectors
# steel/ports
### **read-port-to-string**
Takes a port and reads the entire content into a string

(read-port-to-string port) -> string?

* port : input-port?
### **output-port?**
Checks if a given value is an output port

(output-port? any/c) -> bool?

#### Examples

```scheme
> (define output (open-output-file "foo.txt"))
> (output-port? output) ;; => #true
```
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
### **stdin**
Gets the port handle to stdin

(stdin) -> input-port?

#### Examples

```scheme
> (stdin) ;; => #<input-port:stdin>
```
### **input-port?**
Checks if a given value is an input port

(input-port? any/c) -> bool?

#### Examples

```scheme
> (input-port? (stdin)) ;; => #true
> (input-port? "foo") ;; => #false
```
### **open-output-file**
Takes a filename `path` referring to a file to be created and returns an output port.

(open-output-file string?) -> output-port?

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<output-port:foo-bar.txt>
```
# steel/strings
### **starts-with?**
Checks if the input string starts with a prefix

(starts-with? input pattern) -> bool?

input : string?
pattern: string?

#### Examples

```scheme
> (starts-with? "foobar" "foo") ;; => #true
> (starts-with? "foobar" "bar") ;; => #false
```
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
```
### **trim-end**
Returns a new string with the trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "   foo"
```
### **string->int**
Converts a string into an int. Raises an error if the string cannot be converted to an integer.

(string->int string?) -> int?

#### Examples

```scheme
> (string->int "100") ;; => 10
> (string->int "not-an-int") ;; error
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
### **string-length**
Get the length of the given string

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
```
### **trim-start**
Returns a new string with the leading whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo     "
```
### **string->upper**
Creates a new uppercased version of the input string

(string->upper string?) -> string?

#### Examples

```scheme
> (string->upper "lower") ;; => "LOWER"
```
### **trim**
Returns a new string with the leading and trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo"
```
### **string->list**
Converts a string into a list of characters.

(string->list string?) -> (listof char?)

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
```
### **string->symbol**
Converts a string into a symbol.

(string->symbol string?) -> symbol?

#### Examples

```scheme
> (string->symbol "FooBar") ;; => 'FooBar
```
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
```
### **string-append**
Concatenates all of the given strings into one

(string-append strs...) -> string?

* strs ... : string?

#### Examples
```scheme
> (string-append) ;; => ""
> (string-append "foo" "bar") ;; => "foobar"
### **string->lower**
Creates a new lowercased version of the input string

(string->lower string?) -> string?

#### Examples

```scheme
> (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
```
### **to-string**
Concatenatives all of the inputs to their string representation, separated by spaces.

(to-string xs ...)

* xs : any/c

#### Examples
```scheme
> (to-string 10) ;; => "10"
> (to-string "hello" "world") ;; => "hello world"
```
# steel/filesystem
# steel/syntax
# steel/transducers
# steel/symbols
# steel/strings/colors
# steel/meta
# steel/base
### **read-port-to-string**
Takes a port and reads the entire content into a string

(read-port-to-string port) -> string?

* port : input-port?
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
### **to-string**
Concatenatives all of the inputs to their string representation, separated by spaces.

(to-string xs ...)

* xs : any/c

#### Examples
```scheme
> (to-string 10) ;; => "10"
> (to-string "hello" "world") ;; => "hello world"
```
### **output-port?**
Checks if a given value is an output port

(output-port? any/c) -> bool?

#### Examples

```scheme
> (define output (open-output-file "foo.txt"))
> (output-port? output) ;; => #true
```
### **open-output-file**
Takes a filename `path` referring to a file to be created and returns an output port.

(open-output-file string?) -> output-port?

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<output-port:foo-bar.txt>
```
### **range**
Returns a newly allocated list of the elements in the range (n, m]

(range n m) -> (listof int?)

* n : int?
* m : int?

```scheme
> (range 0 10) ;; => '(0 1 2 3 4 5 6 7 8 9)
```
### **string-length**
Get the length of the given string

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
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
### **hash-ref**
Gets the `key` from the given `map`. Raises an error if the key does not exist. `hash-get` is an alias for this.

(hash-ref map key) -> any/c

* map : hash?
* key : any/c

#### Examples
```scheme
> (hash-get (hash 'a 10 'b 20) 'b) ;; => 20
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
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
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
### **pair?**
Checks if the given value can be treated as a pair.
Note - there are no improper lists in steel, so any list with at least one element
is considered a pair.

(pair? any/c) -> bool?

#### Examples

```scheme
> (pair? '(10 20)) ;; => #true
> (pair? '(10)) ;; => #true
> (pair? '()) ;; => #false
```
### **steel/lists**
#### steel/lists

Lists in Steel have an interface that matches those of classic schemes or lisps.
At face value, they appear to be implemented as cons cells - however, under the hood
they are actually implemented as unrolled linked lists.

This means that for most practical purposes, interaction with lists is the same.
That being said, there are no improper lists, meaning, pairs are actually just lists of two elements.

Indexing into a list also takes O(n/64) - which means you'll get constant time indexing on small lists.

```scheme
(list 10 20 30 40) ;; => '(10 20 30 40)
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
### **hash-length**
Returns the number of key value pairs in the map

(hash-length map) -> (and positive? int?)

* map : hash?

#### Examples

```scheme
> (hash-length (hash 'a 10 'b 20)) ;; => 2
```
### **string->upper**
Creates a new uppercased version of the input string

(string->upper string?) -> string?

#### Examples

```scheme
> (string->upper "lower") ;; => "LOWER"
```
### **trim**
Returns a new string with the leading and trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo"
```
### **starts-with?**
Checks if the input string starts with a prefix

(starts-with? input pattern) -> bool?

input : string?
pattern: string?

#### Examples

```scheme
> (starts-with? "foobar" "foo") ;; => #true
> (starts-with? "foobar" "bar") ;; => #false
```
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
### **input-port?**
Checks if a given value is an input port

(input-port? any/c) -> bool?

#### Examples

```scheme
> (input-port? (stdin)) ;; => #true
> (input-port? "foo") ;; => #false
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
### **string-append**
Concatenates all of the given strings into one

(string-append strs...) -> string?

* strs ... : string?

#### Examples
```scheme
> (string-append) ;; => ""
> (string-append "foo" "bar") ;; => "foobar"
### **steel/time**


#### steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

### **trim-end**
Returns a new string with the trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "   foo"
```
### **string->list**
Converts a string into a list of characters.

(string->list string?) -> (listof char?)

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
```
### **empty?**
Checks if the list is empty

(empty? lst) -> bool?

* lst: list?

#### Examples

```scheme
> (empty? (list 1 2 3 4 5)) ;; => #false
> (empty? '()) ;; => #true
```
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
```
### **trim-start**
Returns a new string with the leading whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo     "
```
### **stdin**
Gets the port handle to stdin

(stdin) -> input-port?

#### Examples

```scheme
> (stdin) ;; => #<input-port:stdin>
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
### **string->lower**
Creates a new lowercased version of the input string

(string->lower string?) -> string?

#### Examples

```scheme
> (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
```
### **string->int**
Converts a string into an int. Raises an error if the string cannot be converted to an integer.

(string->int string?) -> int?

#### Examples

```scheme
> (string->int "100") ;; => 10
> (string->int "not-an-int") ;; error
```
### **string->symbol**
Converts a string into a symbol.

(string->symbol string?) -> symbol?

#### Examples

```scheme
> (string->symbol "FooBar") ;; => 'FooBar
```
# steel/process
# steel/numbers
# steel/streams
# steel/constants
# steel/threads
# steel/lists
### **steel/lists**
#### steel/lists

Lists in Steel have an interface that matches those of classic schemes or lisps.
At face value, they appear to be implemented as cons cells - however, under the hood
they are actually implemented as unrolled linked lists.

This means that for most practical purposes, interaction with lists is the same.
That being said, there are no improper lists, meaning, pairs are actually just lists of two elements.

Indexing into a list also takes O(n/64) - which means you'll get constant time indexing on small lists.

```scheme
(list 10 20 30 40) ;; => '(10 20 30 40)
```
### **pair?**
Checks if the given value can be treated as a pair.
Note - there are no improper lists in steel, so any list with at least one element
is considered a pair.

(pair? any/c) -> bool?

#### Examples

```scheme
> (pair? '(10 20)) ;; => #true
> (pair? '(10)) ;; => #true
> (pair? '()) ;; => #false
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
### **empty?**
Checks if the list is empty

(empty? lst) -> bool?

* lst: list?

#### Examples

```scheme
> (empty? (list 1 2 3 4 5)) ;; => #false
> (empty? '()) ;; => #true
```
### **range**
Returns a newly allocated list of the elements in the range (n, m]

(range n m) -> (listof int?)

* n : int?
* m : int?

```scheme
> (range 0 10) ;; => '(0 1 2 3 4 5 6 7 8 9)
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
### **list**
Returns a newly allocated list containing the vs as its elements.

(list v ...) -> list?

* v : any/c

#### Examples

```scheme
> (list 1 2 3 4 5) ;; => '(1 2 3 4 5)
> (list (list 1 2) (list 3 4)) ;; => '((1 2) (3 4))
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
# steel/contracts
