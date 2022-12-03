# Functions


### keyword?

```scheme

(keyword? v) -> boolean?

v: any/c

```

### mutable-vector

```scheme

(mutable-vector args ...) -> mutable-vector?

```

### struct

Macro for creating a new struct, in the form of:
`(struct <struct-name> (fields ...) options ...)`
The options can consist of the following:

Single variable options (those which their presence indicates `#true`)
- `#:mutable`
- `#:transparent`

Other options must be presented as key value pairs, and will get stored
in the struct instance. They will also be bound to the variable
`___<struct-name>-options___` in the same lexical environment where the
struct was defined. For example:

```scheme
λ > (struct Applesauce (a b c) #:mutable #:transparent #:unrecognized-option 1234)
λ > ___Applesauce-options___
=> #<hashmap {
    '#:fields: '(a b c),
    '#:mutable: #false,
    '#:transparent: #false,
    '#:unrecognized-option: 1234,
}> 
```

By default, structs are immutable, which means setter functions will not
be generated. Also by default, structs are not transparent, which means
printing them will result in an opaque struct that does not list the fields