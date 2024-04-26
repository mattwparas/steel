# Symbols

We've previously learned that `+` is not merely a function, but rather a _symbol_ representing a
function. But what exactly is a _symbol_?

In Lisp, a symbol is a fundamental data type—a sequence of characters, distinct from a string. In
Lisp, entities are named using symbols rather than strings because symbols serve as identifiers and
are evaluated differently.

An analogy from Rust can shed light on the concept of symbols. Consider the question "what is `a`
in `let a = 2;`" from a metaprogramming perspective. In this context, `a` can be understood as an
identifier. Similarly, when invoking a declarative macro like `hello!(b)` with a macro definition
of `macro_rules! hello { ($x:ident) => {} }`, it won't raise an error if `b` is not defined. In this
scenario, `b` acts akin to a _symbol_, the one we don't evaluate to any value behind it, but it
still exists and usable, just on a different level.

Symbols can reference values depending on the context in which they are evaluated. However, in some
cases, symbols are used as themselves and may be compared directly. For example, when defining
a domain-specific language (DSL), certain symbols may act as "operators" or "keywords" without
having a value associated with them. Instead, they impart meaning within the code that processes
the DSL. We'll delve deeper into this topic later in the discussion on macros because, by default,
symbols are evaluated, which may not always align with our intended usage, especially if we assign
them a "keyword" meaning.

Next, we'll explore how to assign values to symbols to give them something to evaluate to.
