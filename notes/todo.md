### Defines in expression position

Define should not be allowed in an expression context, right now this is legal:
```scheme
(when (vector 1 2 3) (define x 10))
```

This shouldn't be allowed, otherwise we're gonna run into a weird state.

### Modules

Modules are pretty busted as well. Because modules get shared at the top level, their requires also seem to get shared at the top level as well, for instance, see `steel/tests/modules/main.rkt` - b shows up at the top level even if b is not required. This is not what we want, it should have its own dedicated namespace thats not polluted.
	- Modules _should_ be able to live on their own. We create one top level one, and each sub module should be able to access it independently, with proper namespacing. If every module just mangles their definitions with a module prefix, we should be in a good spot I think.

The transformation should look like this:
```scheme
;; b-module.rkt
(provide b)

(define (b x) (+ x 27))
(define (c x) (+ x 350))
(define (b-private) "hi this is a private function change this part")
```

```scheme
(provide b-module.rkt-b)

(define b-module.rkt-b x (+ x 27))
(define b-module.rkt-c x (+ x 350))
(define (b-module.rkt-b-private) "hi this is a prvate function change this part")
```

Because then, from another module, we can require it like so:
```scheme
(require "b-module.rkt")

(b 10)
```

Would expand to:

```scheme
(define b-module.rkt-b x (+ x 27))
(define b-module.rkt-c x (+ x 350))
(define (b-module.rkt-b-private) "hi this is a prvate function change this part")

;; This creates a namespace that we can interact with
(define __module-b-module.rkt 
		(hash 'b b-module.rkt-b))

;; Refresh the module definition in this namespace
(define b (hash-get 'b b-module.rkt-b))
```

Now, lets say this is a module that exports stuff, something like this:

```scheme
;; a-module.rkt

(require "b-module.rkt")
(provide a)

(define a (b 10))
```

Then requiring `a-module.rkt` would look like this:

```scheme
(require "a-module.rkt")
```

Expands to:

```scheme
;; Load in b-module
(define b-module.rkt-b x (+ x 27))
(define b-module.rkt-c x (+ x 350))
(define (b-module.rkt-b-private) "hi this is a private function change this part")

;; This creates a namespace that we can interact with
(define __module-b-module.rkt 
		(hash 'b b-module.rkt-b))

;; Refresh the module definition in this namespace
(define a-module.rkt-b (hash-get 'b b-module.rkt-b))

;; Mangled definition in the a module
(define a-module.rkt-a (a-module.rkt-b 10))
(define __module-a-module.rkt (hash 'a a-module.rkt-a))

;; Set up the namespace for the top level module, wherever this is then
(define a (hash-get 'a a-module.rkt-a))

;; Do whatever we want with a...
```

### Transformations at the module boundary

Now, we also want to provide the ability to apply transformations at the module level, take for instance something like this- Define should not be allowed in an expression context, right now this is legal:
```scheme
(when (vector 1 2 3) (define x 10))
```

This shouldn't be allowed, otherwise we're gonna run into a weird state.

- Modules are pretty busted as well. Because modules get shared at the top level, their requires also seem to get shared at the top level as well, for instance, see `steel/tests/modules/main.rkt` - b shows up at the top level even if b is not required. This is not what we want, it should have its own dedicated namespace thats not polluted.
	- Modules _should_ be able to live on their own. We create one top level one, and each sub module should be able to access it independently, with proper namespacing. If every module just mangles their definitions with a module prefix, we should be in a good spot I think.

The transformation should look like this:
```scheme
;; b-module.rkt
(provide b)

(define (b x) (+ x 27))
(define (c x) (+ x 350))
(define (b-private) "hi this is a private function change this part")
```

```scheme
(provide b-module.rkt-b)

(define b-module.rkt-b x (+ x 27))
(define b-module.rkt-c x (+ x 350))
(define (b-module.rkt-b-private) "hi this is a prvate function change this part")
```

Because then, from another module, we can require it like so:
```scheme
(require "b-module.rkt")

(b 10)
```

Would expand to:

```scheme
(define b-module.rkt-b x (+ x 27))
(define b-module.rkt-c x (+ x 350))
(define (b-module.rkt-b-private) "hi this is a prvate function change this part")

;; This creates a namespace that we can interact with
(define __module-b-module.rkt 
		(hash 'b b-module.rkt-b))

;; Refresh the module definition in this namespace
(define b (hash-get 'b b-module.rkt-b))
```

Now, lets say this is a module that exports stuff, something like this:

```scheme
;; a-module.rkt

(require "b-module.rkt")
(provide a)

(define a (b 10))
```

Then requiring `a-module.rkt` would look like this:

```scheme
(require "a-module.rkt")
```

Expands to:

```scheme
;; Load in b-module
(define b-module.rkt-b x (+ x 27))
(define b-module.rkt-c x (+ x 350))
(define (b-module.rkt-b-private) "hi this is a private function change this part")

;; This creates a namespace that we can interact with
(define __module-b-module.rkt 
		(hash 'b b-module.rkt-b))

;; Refresh the module definition in this namespace
(define a-module.rkt-b (hash-get 'b b-module.rkt-b))

;; Mangled definition in the a module
(define a-module.rkt-a (a-module.rkt-b 10))
(define __module-a-module.rkt (hash 'a a-module.rkt-a))

;; Set up the namespace for the top level module, wherever this is then
(define a (hash-get 'a a-module.rkt-a))

;; Do whatever we want with a...
```

### Transformations at the module boundary

Now, we also want to provide the ability to apply transformations at the module level, take for instance something like this:

```scheme
(provide  
	(contract-out [add-2 (->/c integer? integer? integer?)]))

(define (add-2 x y) (+ x y))
```

We want this to actually apply the contract transformation explicitly at the module boundary - that way only modules that explicitly import this will see the contract applied here.
