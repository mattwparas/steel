# Macros

Steel contains a limited form of the `syntax-rules` that scheme provides. These macros build on the small primary language constructs that exist. Consider the following:

```scheme
(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or x) x]
    [(or x y) (let ([z x])
                (if z z y))]
    [(or x y ...) (or x (or y ...))]))

(or #f #f #t)
```

This will actually expand into something like this

```scheme
((λ (__z)
     (if __z __z ((λ (__z) (if __z __z #t)) #f)))
   #f)
```

These macros allow for a simple extension of Steel to however you see fit - defining macros in terms of the syntax rules format is fairly straightforward.