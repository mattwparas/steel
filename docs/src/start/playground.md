# Online Playground

The Steel Playground allows you to try Steel out directly from your browser. Visit the Steel Playground at:

<https://mattwparas.github.io/steel-playground/>

The Steel Playground's environment is as follows:

- Builtin modules are supported and are automatically imported.
- Dylibs are not supported.

## Output

The output prints the results of all expressions. Additional
information can be printed out with the `display` and `displayln`
functions.

## Bytecode

Bytecode renders the Bytecode that Steel generates from the Steel
code. The Bytecode is a low level representation of the code that is
executed by the Steel interpretter.

## Raw AST

Raw AST exposes the parsed AST. This expands some macros. For example:

```scheme
(define (foo bar)
  (+ bar bar))

(define baz '(1 2 3))
```

is actually shorthand for

```scheme
(define foo
  (λ (bar)
    (+ bar bar)))

(define baz (quote 1 2 3))
```

## Expanded AST

This is similar to the Raw AST but provides more detailed information.
