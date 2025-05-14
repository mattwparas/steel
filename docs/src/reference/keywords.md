# Keywords

Keyword arguments are specified by `#:` syntax:

```scheme
(define (add #:first first #:second second)
  (+ first second))
```

Optional keywords can also be specified by `[arg-id val]`,

```scheme
(define (add #:first [first 0] #:second [second 0])
  (+ first second))
```

Here is another example with a variadic definition.

```scheme
(define (add #:first [first 0] . args)
  (apply + (cons first args)))
```
```

