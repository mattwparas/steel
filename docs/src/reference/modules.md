# Modules

In order to support a growing codebase, Steel has module support for projects spanning multiple files. Steel files can `provide` values (with contracts attached) and `require` modules from other files:

```scheme
;; main.stl
(require "provide.stl")

(even->odd 10)


;; provide.stl
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