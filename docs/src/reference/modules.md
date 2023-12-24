# Modules

In order to support a growing codebase, Steel has module support for projects spanning multiple files. Steel files can `provide` values (with contracts attached) and `require` modules from other files:

```scheme
;; main.scm
(require "provide.scm")

(even->odd 10)


;; provide.scm
(provide 
    (contract/out even->odd (->/c even? odd?))
    no-contract
    flat-value)

(define (even->odd x) 
    (+ x 1))

(define (accept-number x) (+ x 10))

(define (no-contract) "cool cool cool")
(define flat-value 15)

(displayln "Calling even->odd with some bad inputs but its okay")
(displayln (even->odd 1))
```

Here we can see if we were to run `main` that it would include the contents of `provide`, and only provided values would be accessible from `main`. The contract is attached at the contract boundary, so inside the `provide` module, you can violate the contract, but outside the module the contract will be applied.

A few notes on modules:
* Cyclical dependencies are not allowed
* Modules will be only compiled once and used across multiple files. If `A` requires `B` and `C`, and `B` requires `C`, `C` will be compiled once and shared between `A` and `B`. 
* Modules will be recompiled when changed, and any dependent files will also be recompiled as necessary

## Modifiers for requiring

```scheme

(require (prefix-in export. ;; prefix-in will prefix all of the bound identifiers with the given prefix
                    (only-in "export.scm" ;; only-in will only introduce the identifiers listed.
                             thing-should-not-escape
                             Applesauce
                             bananas
                             new-identifier
                             my-fun-contracted-function
                             contract-out-test)))
```

If no modifiers are given, `require`-ing a module will introduce all of the provided values into the top level scope.

## Providing and requiring macros;

Macros defined using `define-syntax` can be handled just like normal values:

```scheme
;; main.scm
(require "provide.scm")

(foo-bar x) ;; Will expand into (displayln "Hello world")

;; provide.scm
(provide foo-bar)

(define-syntax foo-bar 
    (syntax-rules ()
        [(foo-bar x) (displayln "Hello world!")]))

```

The module system will take care to keep the namespaces separate - any macros that expand
into macros that exist in the originating module will be expanded, but those will not be available to
the requiring module. In addition, macros can expand into private values (those that are not provided), and the
will still be inaccessible from the requiring module.

