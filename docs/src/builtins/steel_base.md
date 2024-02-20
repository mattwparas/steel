# steel/base
### **abs**
Returns the absolute value of the given input
### **byte?**
Returns #t if the given value is a byte, meaning an exact
integer between 0 and 255 inclusive, #f otherwise.

#### Examples
```scheme
(byte? 65) ;; => #t
(byte? 0) ;; => #t
(byte? 256) ;; => #f
(byte? 100000) ;; => #f
(byte? -1) ;; => #f
```
### **bytes**
Returns a new mutable vector vectors with each byte as the given arguments.
Each argument must satisfy the `byte?` predicate, meaning it is an exact
integer range from 0 - 255 (inclusive)

(bytes b ...)

b : byte?


#### Example
```scheme
(bytes 65 112 112 108 101)
```
### **bytes->list**
Converts the bytevector to the equivalent list representation.

#### Examples
```scheme
(bytes->list (bytes 0 1 2 3 4 5)) ;; => '(0 1 2 3 4 5)
```
### **bytes-append**
Append two byte vectors into a new bytevector.

#### Examples
```scheme
(bytes-append (bytes 0 1 2) (bytes 3 4 5)) ;; (bytes 0 1 2 3 4 5)
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

vector : bytes?
index: (and exact? int?)

#### Examples
```scheme
(bytes-ref (bytes 0 1 2 3 4 5) 3) ;; => 4
(bytes-ref (bytes) 10) ;; error
```
### **bytes-set!**
Sets the byte at the given index to the given byte. Will error
if the index is out of bounds.

(bytes-set! vector index byte)

vector : bytes?
index: (and exact? int?)
byte: byte?

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
Returns a new mutable vector vectors with each byte as the given arguments.
Each argument must satisfy the `byte?` predicate, meaning it is an exact
integer range from 0 - 255 (inclusive)

(bytevector b ...)

b : byte?


#### Example
```scheme
(bytevector 65 112 112 108 101)
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
### **char=?**
Checks if two characters are equal

Requires that the two inputs are both characters, and will otherwise
raise an error.
### **copy-directory-recursively!**
Recursively copies the directory from source to destination
### **create-directory!**
Creates the directory
### **current-directory**
Check the current working directory
### **delete-directory!**
Deletes the directory
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
### **exp**
Returns Euler's number raised to the power of z.
### **file-name**
Gets the filename for a given path
### **first**
Returns the first element of the list l.

(first l) -> any/c

* l : list?

#### Examples

```scheme
> (first '(1 2)) ;; => 1
> (first (cons 2 3)) ;; => 2
```
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
### **is-dir?**
Checks if a path is a directory
### **is-file?**
Checks if a path is a file
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
### **number->string**
Converts the given number to a string
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
### **open-output-file**
Takes a filename `path` referring to a file to be created and returns an output port.

(open-output-file string?) -> output-port?

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<port>
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
Note - there are no improper lists in steel, so any list with at least one element
is considered a pair.

(pair? any/c) -> bool?

#### Examples

```scheme
> (pair? '(10 20)) ;; => #true
> (pair? '(10)) ;; => #true
> (pair? '()) ;; => #false
```
### **path->extension**
Gets the extension from a path
### **path-exists?**
Checks if a path exists
### **range**
Returns a newly allocated list of the elements in the range (n, m]

(range n m) -> (listof int?)

* n : int?
* m : int?

```scheme
> (range 0 10) ;; => '(0 1 2 3 4 5 6 7 8 9)
```
### **rational?**
Returns #t if obj is a rational number, #f otherwise.
Rational numbers are numbers that can be expressed as the quotient of two numbers.
For example, 3/4, -5/2, 0.25, and 0 are rational numbers, while

(rational? value) -> bool?

Examples:
```scheme
(rational? (/ 0.0)) ⇒ #f
(rational? 3.5)     ⇒ #t
(rational? 6/10)    ⇒ #t
(rational? 6/3)     ⇒ #t
```
### **read-dir**
Returns the contents of the directory as a list
### **read-port-to-string**
Takes a port and reads the entire content into a string

(read-port-to-string port) -> string?

* port : input-port?
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
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
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
Converts the given string to a bytevector

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
### **string->list**
Converts a string into a list of characters.

(string->list string?) -> (listof char?)

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
Converts the given string to a number
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
### **string-append**
Concatenates all of the given strings into one

(string-append strs...) -> string?

* strs ... : string?

#### Examples
```scheme
> (string-append) ;; => ""
> (string-append "foo" "bar") ;; => "foobar"
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
### **to-string**
Concatenatives all of the inputs to their string representation, separated by spaces.

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
### **#%black-box**
### **#%box**
### **#%debug-syntax->exprkind**
### **#%default-input-port**
### **#%default-output-port**
### **#%environment-length**
### **#%function-ptr-table**
### **#%function-ptr-table-add**
### **#%function-ptr-table-get**
### **#%get-dylib**
### **#%iterator-finished**
### **#%native-fn-ptr-doc**
### **#%private-cycle-collector**
### **#%private-cycle-collector-get**
### **#%private-cycle-collector-values**
### **#%private-struct?**
### **#%set-box!**
### **#%stream-cdr**
### **#%struct-property-ref**
### **#%struct-update**
### **#%syntax/raw**
### **#%unbox**
### **#%vtable-update-entry!**
### **%iterator?**
### **%keyword-hash**
### *****
### **+**
### **-**
### **/**
### **<**
### **<=**
### **=**
### **>**
### **>=**
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
### **arithmetic-shift**
### **arity?**
### **assert!**
### **atom?**
### **attach-contract-struct!**
### **block-on**
### **bool?**
### **boolean?**
### **box**
### **breakpoint!**
### **bytevector-copy**
### **call-with-current-continuation**
### **call-with-exception-handler**
### **call/cc**
### **cdr-null?**
### **ceiling**
### **channel->recv**
### **channel->send**
### **channel->try-recv**
### **char->number**
### **char-digit?**
### **char-upcase**
### **char-whitespace?**
### **char?**
### **child-stdin**
### **child-stdout**
### **command**
### **complex?**
### **compose**
### **concat-symbols**
### **continuation?**
### **current-function-span**
### **current-inexact-milliseconds**
### **current-milliseconds**
### **current-os!**
### **current-second**
### **denominator**
### **dropping**
### **duration->seconds**
### **duration->string**
### **duration-since**
### **empty-stream**
### **enumerating**
### **env-var**
### **eq?**
### **equal?**
### **eqv?**
### **error-with-span**
### **eval!**
### **even?**
### **exact->inexact**
### **exact-integer?**
### **exact?**
### **expand!**
### **expt**
### **extending**
### **f+**
### **filtering**
### **finite?**
### **flat-mapping**
### **flattening**
### **float?**
### **floor**
### **flush-output-port**
### **function-name**
### **function?**
### **future?**
### **get-contract-struct**
### **get-output-string**
### **get-test-mode**
### **hash-get**
### **hash?**
### **hashset**
### **hashset->list**
### **hashset->vector**
### **hashset-clear**
### **hashset-contains?**
### **hashset-insert**
### **hashset-length**
### **hashset-subset?**
### **inexact->exact**
### **inexact?**
### **infinite?**
### **inspect-bytecode**
### **instant/elapsed**
### **instant/now**
### **int?**
### **integer?**
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
### **list->hashset**
### **list->string**
### **list-tail**
### **list?**
### **local-time/now!**
### **log**
### **make-bytes**
### **make-channels**
### **make-string**
### **make-struct-type**
### **make-vector**
### **mapping**
### **maybe-get-env-var**
### **memory-address**
### **multi-arity?**
### **mut-vec-len**
### **mut-vector-ref**
### **mutable-vector**
### **mutable-vector->list**
### **mutable-vector?**
### **not**
### **null?**
### **number?**
### **numerator**
### **odd?**
### **open-output-string**
### **poll!**
### **pop-front**
### **procedure?**
### **push**
### **push-back**
### **push-front**
### **quotient**
### **raise-error**
### **raise-error-with-span**
### **range-vec**
### **raw-write**
### **raw-write-char**
### **raw-write-string**
### **read!**
### **read-line-from-port**
### **read-to-string**
### **real?**
### **round**
### **run!**
### **set-current-dir!**
### **set-env-var!**
### **set-piped-stdout!**
### **set-strong-box!**
### **set-test-mode!**
### **set?**
### **spawn-process**
### **spawn-thread!**
### **split-many**
### **split-once**
### **square**
### **stdout**
### **stdout-simple-displayln**
### **stream-car**
### **stream-cons**
### **stream-empty?**
### **string->jsexpr**
### **string-ci<=?**
### **string-ci<?**
### **string-ci=?**
### **string-ci>=?**
### **string-ci>?**
### **string-ref**
### **string-replace**
### **string<=?**
### **string<?**
### **string=?**
### **string>=?**
### **string>?**
### **string?**
### **struct->list**
### **struct?**
### **substring**
### **symbol->string**
### **symbol?**
### **syntax->datum**
### **syntax-e**
### **syntax-loc**
### **syntax-span**
### **syntax/loc**
### **syntax?**
### **taking**
### **thread-finished?**
### **thread-join!**
### **thread::current/id**
### **time/sleep-ms**
### **transduce**
### **try-list-ref**
### **unbox-strong**
### **value->iterator**
### **value->jsexpr-string**
### **value->string**
### **vec-append**
### **vec-rest**
### **vector**
### **vector-append!**
### **vector-length**
### **vector-push!**
### **vector-ref**
### **vector-set!**
### **vector?**
### **void**
### **void?**
### **wait**
### **wait->stdout**
### **which**
### **write-line!**
### **zipping**
