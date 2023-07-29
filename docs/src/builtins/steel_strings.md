# steel/strings
### **trim-start**
Returns a new string with the leading whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo     "
```
### **string-append**
Concatenates all of the given strings into one

(string-append strs...) -> string?

* strs ... : string?

#### Examples
```scheme
> (string-append) ;; => ""
> (string-append "foo" "bar") ;; => "foobar"
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
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
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
```
### **trim**
Returns a new string with the leading and trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo"
```
### **string->symbol**
Converts a string into a symbol.

(string->symbol string?) -> symbol?

#### Examples

```scheme
> (string->symbol "FooBar") ;; => 'FooBar
```
### **string->list**
Converts a string into a list of characters.

(string->list string?) -> (listof char?)

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
```
### **string->upper**
Creates a new uppercased version of the input string

(string->upper string?) -> string?

#### Examples

```scheme
> (string->upper "lower") ;; => "LOWER"
```
### **string->int**
Converts a string into an int. Raises an error if the string cannot be converted to an integer.

(string->int string?) -> int?

#### Examples

```scheme
> (string->int "100") ;; => 10
> (string->int "not-an-int") ;; error
```
### **string-length**
Get the length of the given string

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
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
### **string->lower**
Creates a new lowercased version of the input string

(string->lower string?) -> string?

#### Examples

```scheme
> (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
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
