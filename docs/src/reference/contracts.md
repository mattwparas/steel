# Contracts

Inspired by Racket's higher order contracts, `Steel` implements\* higher order contracts to enable design by contract, made easy with a `define/contract` macro for easier ergonomics. Racket makes use of a concept known as _blame_ which seeks to identify the violating party - `Steel` does not quite have fully fleshed out blame but that is a work in progress. Here are some examples:

```scheme
;; Simple flat contracts
(define/contract (test x y)
    (->/c even? even? odd?)
    (+ x y 1))

(test 2 2) ;; => 5

(define/contract (test-violation x y)
    (->/c even? even? odd?)
    (+ x y 1))

(test-violation 1 2) ;; contract violation

```

Contracts are implemented as _values_, so they are bound to functions. This enables the use of contract checking on functions themselves since functions can be passed around:

```scheme
;; Higher order contracts, check on application
(define/contract (higher-order func y)
    (->/c (->/c even? odd?) even? even?)
    (+ 1 (func y)))

(higher-order (lambda (x) (+ x 1)) 2) ;; => 4

(define/contract (higher-order-violation func y)
    (->/c (->/c even? odd?) even? even?)
    (+ 1 (func y)))

(higher-order-violation (lambda (x) (+ x 2)) 2) ;; contract violation
```

Contracts on functions do not get checked until they are applied, so a function returning a _contracted_ function won't cause a violation until that function is actually used:

```scheme
;; More higher order contracts, get checked on application
(define/contract (output)
    (->/c (->/c string? int?))
    (lambda (x) 10))

(define/contract (accept func)
    (->/c (->/c string? int?) string?)
    "cool cool cool")

(accept (output)) ;; => "cool cool cool"

;; different contracts on the argument
(define/contract (accept-violation func)
    (->/c (->/c string? string?) string?)
    (func "applesauce")
    "cool cool cool")

(accept-violation (output)) ;; contract violation

;; generates a function
(define/contract (generate-closure)
    (->/c (->/c string? int?))
    (lambda (x) 10))

;; calls generate-closure which should result in a contract violation
(define/contract (accept-violation)
    (->/c (->/c string? string?))
    (generate-closure))

((accept-violation) "test") ;; contract violation
```

Perhaps a more nuanced case:

```scheme
(define/contract (output)
    (->/c (->/c string? int?))
    (lambda (x) 10.2))

(define/contract (accept)
    (->/c (->/c string? number?))
    (output))


((accept) "test") ;; contract violation 10.2 satisfies number? but _not_ int?
```