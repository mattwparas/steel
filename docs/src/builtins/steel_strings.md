# steel/strings
#### steel/strings

Strings in Steel are immutable, fixed length arrays of characters. They are heap allocated,
and are implemented under the hood as referenced counted rust `Strings`.
### **char=?**
Checks if two characters are equal

Requires that the two inputs are both characters, and will otherwise
raise an error.
### **string->int**
Converts a string into an int. Raises an error if the string cannot be converted to an integer.

(string->int string?) -> int?

#### Examples

```scheme
> (string->int "100") ;; => 10
> (string->int "not-an-int") ;; error
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
### **trim**
Returns a new string with the leading and trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo"
```
### **string-length**
Get the length of the given string

(string-length string?) -> int?

#### Examples

```scheme
> (string-length "apples") ;; => 6
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
### **number->string**
Converts the given number to a string
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
### **string**
Constructs a string from the given characters
### **string->lower**
Creates a new lowercased version of the input string

(string->lower string?) -> string?

#### Examples

```scheme
> (string->lower "sPonGeBoB tExT") ;; => "spongebob text"
```
### **trim-end**
Returns a new string with the trailing whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "   foo"
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
### **string->list**
Converts a string into a list of characters.

(string->list string?) -> (listof char?)

#### Examples

```scheme
> (string->list "hello") ;; => '(#\h #\e #\l #\l #\o)
```
### **trim-start**
Returns a new string with the leading whitespace removed.

(trim string?) -> string?

#### Examples

```scheme
> (trim "   foo     ") ;; => "foo     "
```
### **string->number**
Converts the given string to a number
### **split-whitespace**
Returns a list of strings from the original string split on the whitespace

(split-whitespace string?) -> (listof string?)

#### Examples

```scheme
(split-whitespace "apples bananas fruits veggies") ;; '("apples" "bananas" "fruits" "veggies")
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
### **char->number**
### **string>?**
### **string=?**
### **string-ci<=?**
### **split-once**
### **char-whitespace?**
### **char-digit?**
### **char-upcase**
### **substring**
### **split-many**
### **make-string**
### **string>=?**
### **string-ci<?**
### **string-ci>=?**
### **string<=?**
### **string-ci>?**
### **string<?**
### **string-ref**
### **string-ci=?**
