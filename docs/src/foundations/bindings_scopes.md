# Bindings and Scopes

### `define`

To define a new value, use the form `(define symbol value)`:

```scheme
λ > (define a 1337)
λ > a
=> 1337
```

Here, we've created a _binding_ between the symbol `a` and a number literal. Consequently, further
evaluation of `a` yields `1337`. It's worth noting that regardless of whether `a` is
bound and used across different expressions (in separate interpreter entries), it still retains its
value.

Another example:

```scheme
λ > (define c (+ 1000 337))
λ > c
=> 1337
```

Although evaluation rules differ for `define` (symbol `c` is not evaluated, otherwise it would
result in an error and make no sense), the _value_ part follows standard rules.

We also introduce a shorthand for function definitions:

```scheme
(define (function-name args ...) body ...)
```

Is equivalent to:

```scheme
(define function-name (lambda (args ...) body ...))
```

### `let` and local scope `define`

Steel has two ways to define local variables, `let` bindings, and the
other is local scope `define`.

#### `let`

`let` is used as follows: `(let ((symbol-a value-a) (symbol-b value-b) ...) body)`.

```scheme
λ > (let ((a (* 25 4))
          (b 3))
      (* a b))
=> 300
λ > a
=> 1337
```

Three points to note here:
1. We're dealing with an altered evaluation: the `let` symbol has no function bound to it, and in
nested forms, only certain things get evaluated. This is the effect macros give us.
2. We've defined two symbols and bound values to them, which have been evaluated to these values in
the latest expression inside the `let` expression.
3. `a` became 100, but only within the last expression nested in `let`, because it introduced a
narrower _scope_. However, it didn't change the previous binding of `a`, but it shadowed it. And as
you can probably guess, `b` is not defined beyond `let`.

#### Local scope `define`

For the sake of example, we'll use `let` once again to define a local scope. However, we won't
define any symbols with it but use `define` within the _body_ of `let`:

```scheme
λ > (let ()
      (define a (+ 100 100))
      (define b (+ 2 3))
      (* a b))
=> 1000
λ > a
error[E02]: FreeIdentifier
  ┌─ :1:1
  │
1 │ a
  │ ^ a

λ > b
error[E02]: FreeIdentifier
  ┌─ :1:1
  │
1 │ b
  │ ^ b
```

`define` inside of a local scope could be used instead of `let` bindings,
and it won't alter the outer scope.

