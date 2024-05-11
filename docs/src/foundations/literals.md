# Literals

Literals are evaluated to themselves: numbers and string literals.

Let's fire up a Steel interpreter
(or use [the online one](https://mattwparas.github.io/steel-playground/dev/))
to take a look at some examples.

In the following code examples, lambda precedes user input, and `=>` indicates the evaluation result
returned from the interpreter.

```scheme
λ > 5
=> 5

λ > -1337
=> -1337

λ > 1.5
=> 1.5

λ > 0
=> 0
```

As we can see, an input text read as a sexp in a variant of an atom made of a number literal is
evaluated to itself. What about string literals?

```scheme
λ > "hello"
=> "hello"

λ > "steel"
=> "steel"
```

Nothing unexpected happens, as for characters:

```scheme
λ > #\y
=> #\y
```

While not very fascinating by itself, it starts to become useful when built upon, as we're about to
see next.
