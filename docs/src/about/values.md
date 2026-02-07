# Values

Steel values include numbers, characters, booleans, strings, byte vectors, and symbols.


## Numbers

Steel has support for the numeric tower, and as a result, many different kinds of numbers
and their conversions between each other are supported. They can be written as follows:

```scheme
1 ;; i64
3.14 ;; f64 
1/2 ;; 32 bit rational 
6.02e+23 ;; f64
1+2i ;; Imaginary
9999999999999999999999 ;; Big num (heap allocated integer)
```

Operations should be supported between the various types transparently, and if not should be otherwise documented. Anything unexpected is most likely a bug and should be reported.

## Strings

Strings are written between double quotes much like other programming languages:

```scheme
"Hello world!"
```

Typical rules apply with respect to backslash and escaping, where the next character will be interpreted literally.
For example, backslashes will escape double quotes within strings:

```scheme
"hello \"world\" foobar" ;; The double quotes are interpreted literally
```

Unicode is supported for strings as well, as well as within symbols.

```scheme
"😂😂💯"
```

## Symbols

Symbols are identifiers which are quoted, meaning they are identifiers which include a leading `'``. It may be unclear what that means exactly now, and will be covered in future sections:

```scheme
'symbol ;; Symbol
```

## Characters

Unlike other languages which use single quotes for characters (since quotes are used for other things), characters in steel
are prefixed with `#\`, like so:

```scheme
#\a
#\b
#\newline ;; for '\n'
#\NEWLINE
#\space ;; for ' '
```

## Booleans

`#true` is used to represent True, and `#false` is used to represent False. These can also be written as `#t` and `#f`.

All values in steel except `#false` evaluate to `#true`, so when testing a conditional, like `if`:

```scheme
(if 10
    "found true"
    "found false") ;; This evaluates to "found true", since 10 is truthy

(if #f "found true" "found false") ;; This evaluates to "found false" since #f is explicitly #false
```

## Byte vectors (or byte strings)

Byte vectors are vectors (or strings) of bytes, where each element is a `u8`. Bytevector literals can be written using
the prefix `#u8`:

```scheme
#u8(1 2 3 4 5)
```
