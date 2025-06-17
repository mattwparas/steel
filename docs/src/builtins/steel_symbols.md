# steel/symbols
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
