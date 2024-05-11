# Function Calls

We're familiar with two variants of S-expressions: atoms and expressions. We've already seen how
numbers and string literals, which are examples of atoms, are evaluated. Now, let's delve into the
evaluation of expressions.

Let's revisit the previous expression example:

```scheme
(+ 1 3 (- 7 4) 7)
```

Skipping the reader and macro expansion steps mentioned in the
[evaluation chapter](evaluation.html), the interpreter will evaluate this expression in the
following steps:

**Step 1: Evaluate all items**

1. The _symbol_ `+` evaluates to the value behind it, in this case, the value of the addition
function:

```scheme
λ > +
=> #<function:+>
```

More details on _symbols_ will follow in the [symbols chapter](symbols.html).

2. As mentioned earlier, numbers evaluate to themselves.
3. A nested expression is evaluated, similar to the current top-level one.

At this point, the expression is represented as follows (not proper input, but a human-readable
representation of the process):

```scheme
(#<function:+> 1 3 (- 7 4) 7)
```

**Step 2: Evaluate all items, level + 1**

Here, we address the nested expression.

1. The _symbol_ `-` also evaluates to a subtraction function value, just like the previous case
   with addition.
2. Numbers evaluate to themselves, again.
3. Function application occurs.

Expression evaluation follows a scheme where the first (0th, or more aptly, "the head") value
represents a function value. This results in a function call familiar to us. With scheme, we have the
freedom to instruct the interpreter not to evaluate it and leave it as is, but we'll address this
case later in the book.

Thus, the expression `(- 7 4)` transitions from `(#<function:-> 7 4)` to `3` due to function application.

**Step 3: Returning to level 0**

After reducing the nested expression, we have:

```scheme
(#<function:+> 1 3 3 7)
```

The evaluation of an expression eventually results in a function application unless instructed
otherwise. Here, there's no exception:

```scheme
=> 14
```
