# steel/base
### **string-append**
Concatenates all of the given strings into one

(string-append strs...) -> string?

* strs ... : string?

#### Examples
```scheme
> (string-append) ;; => ""
> (string-append "foo" "bar") ;; => "foobar"
### **is-file?**
Checks if a path is a file
### **string->lower**
Creates a new lowercased version of the input string

(string->lower string?) -> string?

#### Examples

```scheme
> (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
```
### **string->number**
Converts the given string to a number
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
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
```
### **steel/strings**
#### steel/strings

Strings in Steel are immutable, fixed length arrays of characters. They are heap allocated,
and are implemented under the hood as referenced counted rust `Strings`.
### **stdin**
Gets the port handle to stdin

(stdin) -> input-port?

#### Examples

```scheme
> (stdin) ;; => #<port>
```
### **number->string**
Converts the given number to a string
### **copy-directory-recursively!**
Recursively copies the directory from source to destination
### **empty?**
Checks if the list is empty

(empty? lst) -> bool?

* lst: list?

#### Examples

```scheme
> (empty? (list 1 2 3 4 5)) ;; => #false
> (empty? '()) ;; => #true
```
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
```
### **output-port?**
Checks if a given value is an output port

(output-port? any/c) -> bool?

#### Examples

```scheme
> (define output (open-output-file "foo.txt"))
> (output-port? output) ;; => #true
```
### **path->extension**
Gets the extension from a path
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
### **trim-start**
Returns a new string with the leading whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo     "
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
### **exp**
Returns Euler's number raised to the power of z.
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
### **hash-length**
Returns the number of key value pairs in the map

(hash-length map) -> (and positive? int?)

* map : hash?

#### Examples

```scheme
> (hash-length (hash 'a 10 'b 20)) ;; => 2
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
### **car**
Returns the first element of the list l.

(car l) -> any/c

* l : list?

#### Examples

```scheme
> (car '(1 2)) ;; => 1
> (car (cons 2 3)) ;; => 2
```
### **current-directory**
Check the current working directory
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
### **range**
Returns a newly allocated list of the elements in the range (n, m]

(range n m) -> (listof int?)

* n : int?
* m : int?

```scheme
> (range 0 10) ;; => '(0 1 2 3 4 5 6 7 8 9)
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
### **delete-directory!**
Deletes the directory
### **first**
Returns the first element of the list l.

(first l) -> any/c

* l : list?

#### Examples

```scheme
> (first '(1 2)) ;; => 1
> (first (cons 2 3)) ;; => 2
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
### **read-dir**
Returns the contents of the directory as a list
### **hash-ref**
Gets the `key` from the given `map`. Raises an error if the key does not exist. `hash-get` is an alias for this.

(hash-ref map key) -> any/c

* map : hash?
* key : any/c

#### Examples
```scheme
> (hash-ref (hash 'a 10 'b 20) 'b) ;; => 20
```
### **length**
Returns the length of the list.

(length l) -> int?

* l : list?

#### Examples

```scheme
> (length (list 10 20 30)) ;; => 3
```
### **read-port-to-string**
Takes a port and reads the entire content into a string

(read-port-to-string port) -> string?

* port : input-port?
### **open-output-file**
Takes a filename `path` referring to a file to be created and returns an output port.

(open-output-file string?) -> output-port?

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<port>
```
### **input-port?**
Checks if a given value is an input port

(input-port? any/c) -> bool?

#### Examples

```scheme
> (input-port? (stdin)) ;; => #true
> (input-port? "foo") ;; => #false
```
### **steel/time**


#### steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
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
### **to-string**
Concatenatives all of the inputs to their string representation, separated by spaces.

(to-string xs ...)

* xs : any/c

#### Examples
```scheme
> (to-string 10) ;; => "10"
> (to-string "hello" "world") ;; => "hello world"
```
### **trim-end**
Returns a new string with the trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "   foo"
```
### **string-length**
Get the length of the given string

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
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
### **path-exists?**
Checks if a path exists
### **file-name**
Gets the filename for a given path
### **string->upper**
Creates a new uppercased version of the input string

(string->upper string?) -> string?

#### Examples

```scheme
> (string->upper "lower") ;; => "LOWER"
```
### **abs**
Returns the absolute value of the given input
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
### **trim**
Returns a new string with the leading and trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo"
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
### **create-directory!**
Creates the directory
### **string->list**
Converts a string into a list of characters.

(string->list string?) -> (listof char?)

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
```
### **char=?**
Checks if two characters are equal

Requires that the two inputs are both characters, and will otherwise
raise an error.
### **is-dir?**
Checks if a path is a directory
### **string**
Constructs a string from the given characters
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
### **steel/filesystem**
#### steel/filesystem

Filesystem functions, mostly just thin wrappers around the `std::fs` functions in
the Rust std library.
### **number?**
### **into-max**
### **>=**
### **syntax-e**
### **-**
### **push**
### **set-current-dir!**
### **expand!**
### **<**
### **%keyword-hash**
### **into-count**
### **value->string**
### **mapping**
### **Engine::new**
### **into-sum**
### **dropping**
### **channel->try-recv**
### **hashset-insert**
### **#%black-box**
### **function?**
### **set-box!**
### **None?**
### **current-function-span**
### **block-on**
### **void**
### **#%struct-property-ref**
### **into-min**
### **vector**
### **vector-set!**
### **=**
### **substring**
### **thread::current/id**
### **current-second**
### **future?**
### **syntax/loc**
### **Engine::modules->list**
### **concat-symbols**
### **Some**
### **string-ref**
### **thread-finished?**
### **string-ci>?**
### **push-back**
### **split-many**
### **hashset-contains?**
### **call-with-exception-handler**
### **raw-write-char**
### **set-env-var!**
### **hashset-subset?**
### **#%syntax/raw**
### **/**
### **string>=?**
### **set-piped-stdout!**
### **current-milliseconds**
### **which**
### **get-contract-struct**
### **#%private-struct?**
### **run!**
### **vec-append**
### **integer?**
### **into-hashset**
### **open-output-string**
### **current-os!**
### **make-vector**
### **raise-error**
### **hashset->list**
### **try-list-ref**
### **arithmetic-shift**
### **child-stdin**
### **even?**
### **into-list**
### **eqv?**
### **char-digit?**
### **read!**
### **#%iterator-finished**
### **#%private-cycle-collector-values**
### **vector-ref**
### **env-var**
### **set-test-mode!**
### **spawn-process**
### **spawn-thread!**
### **string?**
### **exact->inexact**
### **mutable-vector->list**
### **#%default-output-port**
### **mutable-vector?**
### **symbol->string**
### **string-ci<?**
### **function-name**
### **duration->seconds**
### **call-with-current-continuation**
### **string<=?**
### **get-output-string**
### **join!**
### **list->string**
### **make-struct-type**
### **assert!**
### **child-stdout**
### **expt**
### **mut-vector-ref**
### **hash?**
### **symbol?**
### **box**
### **raw-write-string**
### **continuation?**
### **hash-keys->vector**
### **list?**
### **list-tail**
### **string-ci=?**
### **filtering**
### **#%unbox**
### **into-for-each**
### **compose**
### **Err**
### **transduce**
### **read-line-from-port**
### **multi-arity?**
### **maybe-get-env-var**
### **duration-since**
### **inspect-bytecode**
### **char-upcase**
### **hashset->vector**
### **char->number**
### **poll!**
### **flat-mapping**
### **atom?**
### **Ok**
### **bool?**
### **syntax?**
### **wait**
### **#%stream-cdr**
### **command**
### **stream-car**
### **wait->stdout**
### **hash-clear**
### **#%function-ptr-table-get**
### **int?**
### **raw-write**
### **log**
### **range-vec**
### **make-channels**
### **float?**
### **active-object-count**
### **hashset-length**
### **local-time/now!**
### **call/cc**
### **Ok?**
### **enumerating**
### **vector?**
### **memory-address**
### **get-test-mode**
### **breakpoint!**
### **write-line!**
### **string>?**
### **hashset-clear**
### **list->hashset**
### **#%private-cycle-collector-get**
### **into-vector**
### **mut-vec-len**
### **<=**
### **vec-rest**
### **flush-output-port**
### **#%vtable-update-entry!**
### **error-with-span**
### **value->iterator**
### **mutable-vector**
### **split-once**
### **attach-contract-struct!**
### **not**
### **procedure?**
### **syntax-loc**
### **string-ci>=?**
### **#%default-input-port**
### **Err->value**
### **char?**
### **hash-union**
### **#%set-box!**
### **hash-values->vector**
### **quotient**
### **Some?**
### **into-hashmap**
### **#%box**
### **interleaving**
### **Engine::raise_error**
### **raise-error-with-span**
### **current-inexact-milliseconds**
### **None**
### **hash-get**
### **thread-join!**
### **char-whitespace?**
### **string=?**
### **hash-empty?**
### **unbox**
### **#%function-ptr-table**
### **arity?**
### **empty-stream**
### **string->jsexpr**
### **eq?**
### **iter-next!**
### **Engine::clone**
### **string<?**
### **flattening**
### **Some->value**
### **+**
### **channel->send**
### **vector-length**
### **vector-push!**
### **stdout-simple-displayln**
### **TypeId?**
### **hashset**
### **into-last**
### **taking**
### **channel->recv**
### **#%function-ptr-table-add**
### **stream-empty?**
### **into-reducer**
### **instant/elapsed**
### **push-front**
### **Err?**
### **syntax->datum**
### **%iterator?**
### **>**
### **vector-append!**
### **zipping**
### **read-to-string**
### **into-string**
### **pop-front**
### **odd?**
### **boolean?**
### **into-nth**
### **#%private-cycle-collector**
### **make-string**
### **time/sleep-ms**
### **equal?**
### **string-ci<=?**
### **round**
### **null?**
### **Engine::add-module**
### **set?**
### *****
### **struct?**
### **duration->string**
### **extending**
### **stream-cons**
### **stdout**
### **Ok->value**
### **value->jsexpr-string**
### **into-product**
### **eval!**
### **void?**
### **instant/now**
### **f+**
