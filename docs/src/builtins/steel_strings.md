# steel/strings
Strings in Steel are immutable, fixed length arrays of characters. They are heap allocated, and
are implemented under the hood as referenced counted Rust `Strings`. Rust `Strings` are stored
as UTF-8 encoded bytes.
### **char=?**
Checks if two characters are equal

Requires that the two inputs are both characters, and will otherwise
raise an error.
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
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
```
### **make-string**
Creates a string of a given length, filled with an optional character
(which defaults to `#\0`).

(make-string len [char]) -> string?

* len : int?
* char : char? = #\0
### **number->string**
Converts the given number to a string
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
### **string**
Constructs a string from the given characters
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
Converts the given string to a number, with an optional radix.
On failure, it returns `f`

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
Compares two strings lexicographically (as in "less-than-or-equal"),
### **string-ci<?**
Compares two strings lexicographically (as in "less-than"),
in a case insensitive fashion.
### **string-ci=?**
Compares two strings for equality, in a case insensitive fashion.
### **string-ci>=?**
Compares two strings lexicographically (as in "greater-than-or-equal"),
in a case insensitive fashion.
### **string-ci>?**
Compares two strings lexicographically (as in "greater-than"),
in a case-insensitive fashion.
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

(string-ref str n)

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
Compares two strings lexicographically (as in "less-than-or-equal").
### **string<?**
Compares two strings lexicographically (as in "less-than").
### **string=?**
Compares two strings for equality.
### **string>=?**
Compares two strings lexicographically (as in "greater-than-or-equal").
### **string>?**
Compares two strings lexicographically (as in "greater-than").
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
### **char->number**
### **char-digit?**
### **char-upcase**
### **char-whitespace?**
