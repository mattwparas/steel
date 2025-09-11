# steel/strings
Strings in Steel are immutable, fixed length arrays of characters. They are heap allocated, and
are implemented under the hood as referenced counted Rust `Strings`. Rust `Strings` are stored
as UTF-8 encoded bytes.
### **char->integer**
Returns the Unicode codepoint of a given character.

(char->integer char?) -> integer?
### **char->number**
Attemps to convert the character into a decimal digit,
and returns `#f` on failure.
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
Returns `#t` if the character is a decimal digit.
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
### **int->string**
Converts an integer into a string.

(int->string int?) -> string?

#### Examples

```scheme
> (int->string 10) ;; => "10"
```
### **integer->char**
Returns the character corresponding to a given Unicode codepoint.

(integer->char integer?) -> char?
### **make-string**
Creates a string of a given length, filled with an optional character
(which defaults to `#\0`).

(make-string len [char]) -> string?

* len : int?
* char : char? = #\0
### **number->string**
Converts the given number to a string.
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
Alias of `string-downcase`.
### **string->number**
Converts the given string to a number, with an optional radix.
On failure, it returns `#f`

(string->number digits [radix]) -> (or/c number? boolean?)

* digits : string?
* radix : number?
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
### **utf8-length**
Get the length of the string in UTF-8 bytes.

(utf8-length string?) -> int?

#### Examples

```scheme
> (utf8-length "apples") ;; => 6
> (utf8-length "αβγ") ;; => 6
> (utf8-length "✅") ;; => 3
```
