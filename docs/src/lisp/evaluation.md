# Evaluation

Please welcome, an S-expression (or `sexp` for short):

```lisp
(+ 1 3 (- 7 4) 7)
```

Its structure is recursive; this particular one is made of a top-level _expression_ and nested
_atoms_, as well as one nested _expression_.

A key to understanding any Lisp is grasping the idea of how expressions should be evaluated.
Roughly, the steps are:

1. Reader: Code as a string becomes a sexp, either an atom or an expression. Later, we'll discuss
that there are forms that users can input, but they look different until processed by the reader
into a proper sexp. It will be explicitly mentioned in this book if that's the case. Until then, our
string inputs will match the string representation of a resulting sexp.
2. Macro expansion: Some sexps may become other sexps.
3. Eval: The final sexp is evaluated.

In subsequent chapters, we'll demystify all of it. For now, steps 1 and 2 will be transparent.
