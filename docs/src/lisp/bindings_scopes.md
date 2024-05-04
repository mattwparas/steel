# Bindings and Scopes

How do we define symbols? And more importantly, how do we assign values to them? Let's address these
questions now.

```lisp
λ > a
```

Congratulations, we've defined our first symbol.

```
error[E02]: FreeIdentifier
  ┌─ :1:1
  │
1 │ a
  │ ^ a
```

Well, not so fast.

As mentioned earlier in the book, the interpreter evaluates S-expressions that it encounters, and
the symbol `a` is no exception. Since symbol evaluation involves substituting it with a bound value,
and there is none bound to it, we get an error.

Let's change that.

### `define`

This is how we bind values to symbols, using `(define symbol value)`:

```
λ > (define a 1337)
λ > a
=> 1337
```

Here, we've created a _binding_ between the symbol `a` and a number literal. Consequently, further
evaluation of `a` yields the elite integer. It's worth noting that regardless of whether `a` is
bound and used across different expressions (in separate interpreter entries), it still retains its
value. Why? Because the Steel session we're in introduces a _scope_, a context used in evaluation to
resolve symbol bindings.

Another example:

```
λ > (define c (+ 1000 337))
λ > c
=> 1337
```

Although evaluation rules differ for `define` (symbol `c` is not evaluated, otherwise it would
result in an error and make no sense), the _value_ part follows standard rules.

### `let` and local scope `define`

If `define` seems reminiscent of global variables to you, you're not alone. While it's suitable for
defining function bindings (more on that later) or constants, there must be a better way for local
bindings.

Steel has two ways to do it: one is a Lisp classic, `let` bindings, and another one is
Steel-specific local scope `define`.

#### `let`

Allow me to introduce `let`, a term familiar from our host language, Rust, with the same meaning.

`let` is used as follows: `(let ((symbol-a value-a) (symbol-b value-b) ...) body)`.

```lisp
λ > (let ((a (* 25 4))
          (b 3))
      (* a b))
=> 300
λ > a
=> 1337
```

Three points to note here:
1. We're dealing with an altered evaluation: `let` has no function bound to it, and in nested forms,
only certain things get evaluated. This is the effect macros give us.
2. We've defined two symbols and bound values to them, which have been evaluated to these values in
the latest expression inside the `let` expression.
3. `a` became 100, but only within the last expression nested in `let`, because it introduced a
narrower _scope_. However, it didn't change the previous binding of `a`, but it shadowed it. And as
you can probably guess, `b` is not defined beyond `let`.

#### Local scope `define`

For the sake of example, we'll use `let` once again to define a local scope. However, we won't
define any symbols with it but use `define` within the _body_ of `let` (this shall work with
other ways to define a local scope, but we don't know about them yet, so this one should work just
enough):

```lisp
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

As we can see, in Steel, `define` inside of a local scope could be used instead of `let` bindings,
and it won't alter the outer scope.

