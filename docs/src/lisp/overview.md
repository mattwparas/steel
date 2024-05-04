# Lisp

The foundational ideas of Lisp are straightforward and easily combined, resulting in a minimalistic
syntax that may appear excessive at first glance. While the abundance of parentheses can seem
off-putting and archaic, Lisp expressions are actually nested lists and atoms, making it a recursive
data structure. This inherent structure enables easy manipulation, allowing for straightforward
metaprogramming and direct AST manipulations with proper editor support.

Now, let's welcome an S-expression (or `sexp` for short):

```lisp
(+ 1 3 (- 7 4) 7)
```

As mentioned earlier, its structure is recursive; this particular one is comprised of a top-level
_expression_ and nested _atoms_, as well as one nested _expression_. Such a construction can be
manipulated in various ways before being evaluated. Although the list of syntactic rules will
be minimally supplemented later in this book, it already provides a stable foundation to fit the
language in any domain area.
