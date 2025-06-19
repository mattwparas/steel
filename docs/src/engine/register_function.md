# Registering functions


There are a few different ways that functions can be registered against Steel.
The most user friendly way is to use the `RegisterFn` trait, which exposes a `register_fn` function that will coerce functions into the
proper function representation according to the `IntoSteelVal` and `FromSteelVal` traits. An example of this:

```rust,noplaypen
use steel_vm::engine::Engine;
use steel_vm::register_fn::RegisterFn;

fn external_function(arg1: usize, arg2: usize) -> usize {
    arg1 + arg2
}

fn option_function(arg1: Option<String>) -> Option<String> {
    arg1
}

fn result_function(arg1: Option<String>) -> Result<String, String> {
    if let Some(inner) = arg1 {
        Ok(inner)
    } else {
        Err("Got a none".to_string())
    }
}

pub fn main() {
    let mut vm = Engine::new();

    // Here we can register functions
    // Any function can accept parameters that implement `FromSteelVal` and
    // return values that implement `IntoSteelVal`
    vm.register_fn("external-function", external_function);

    // See the docs for more information about `FromSteelVal` and `IntoSteelVal`
    // but we can see even functions that accept/return Option<T> or Result<T,E>
    // can be registered
    vm.register_fn("option-function", option_function);

    // Result values will map directly to errors in the VM and bubble back up
    vm.register_fn("result-function", result_function);
}
```

## IntoSteelVal and FromSteelVal

Types that implement `IntoSteelVal` and `FromSteelVal` and be returned and passed into rust functions, respectively. Take the following for example:

```rust

fn foo(value: isize) -> String {
    ...
}
    
```

This means that steel values will attempt to be coerced to the type in the function signature, and the value that this function returns will then attempt to be coerced into a that Steel understands.

There are some special case conversions that happen specifically:

### `IntoSteelVal`

* Vec<T> -> Steel list
* HashMap<K, V> -> Steel hashmap
* HashSet<T> -> Steel hashset
* Result<T, E> -> if `Ok(T)` then T else `(error E)`
* Option<T> -> if `Some(T)` then T else `#false`


## Defining builtin modules

Organizing built in functions into modules is generally recommended. To do this, use the `BuiltInModule` struct, which can be registered against the engine.

```rust,noplaypen

let mut vm = Engine::new();
let mut module = BuiltInModule::new("foo/bar");

module.register_fn("baz", || println!("baz"));

vm.register_module(module);

```

To use this from the steel side, you can use the special form `require-builtin`:

```scheme
(require-builtin foo/bar)

(baz) ;; Prints "baz"
```

To include a module with a prefix, you can use the form:

`(require-builtin <module> as <prefix>)`

And all imported identifiers will have that prefix. Continuing the example above:


```scheme
(require-builtin foo/bar as quux.)

(quux.baz) ;; Prints "baz"
```


## Registering steel modules directly from Rust

You might want to bundle some steel code directly as a module that can be `require`d from other steel code, this can be done like so:

```rust
let mut vm = Engine::new();

vm.register_steel_module(
    "foo/bar.scm".to_string(),
    r#"
(provide baz)

(define (baz) (displayln "baz"))
    "#.to_string(),
);

```

And then from the steel side:

```scheme
(require "foo/bar.scm")

(baz) ;; Prints "baz"
```
