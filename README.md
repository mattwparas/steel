# Steel

![Actions Status](https://github.com/mattwparas/steel/workflows/Build/badge.svg) [![Coverage Status](https://coveralls.io/repos/github/mattwparas/steel/badge.svg?branch=master)](https://coveralls.io/github/mattwparas/steel?branch=master)

An embedded scheme interpreter in Rust.

## Getting Started

This github repository is a client that uses the `steel`, `steel_vm`, `steel_derive`, and `steel_repl` crates. To get started using a repl with the crates, make sure you first have rust installed.

Then, clone the repo and run the following command:

```bash
cargo run
```

This will launch a REPL instance that looks something like this:

<p align="center">
  <img src="images/repl.gif" width="100%">
</p>

## About

`Steel` is an embedded scheme interpreter. Inspired largely by Racket and Clojure, the language seeks to be ergonomic scheme variant helpful for embedding in applications, or to be used on its own with high performance functions implemented in Rust. The language implementation itself contains a fairly powerful macro system based on the `syntax-rules` style and a bytecode virtual machine.

## Features

* Limited `syntax-rules` style macros are supported
* Easy integration with Rust functions and structs
* Easily call a script from rust or via a separate file
* Few dependencies
* Efficient - common functions and data structures are optimized for performance (`map`, `filter`, etc)
* Higher order Contracts
* Built in immutable data structures include:
  * lists
  * vectors
  * hashmaps
  * hashsets

## Contracts

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

\* Very much a work in progress

## Transducers

Inspired by clojure's transducers, `Steel` has a similar object that is somewhere half way in between transducers and iterators. Consider the following:

```scheme

(mapping (lambda (x) (+ x 1))) ;; => <#iterator>
(filtering even?) ;; => <#iterator>
(taking 15) ;; => <#iterator>

(compose 
    (mapping add1)
    (filtering odd?)
    (taking 15)) ;; => <#iterator>
```

Each of these expressions emit an `<#iterator>` object, which means they're compatible with `execute` and `transduce`. Execute takes a transducer (i.e. `<#iterator>`) and a collection that can be iterated (`list`, `vector`, or `stream`) and applies the transducer.

```scheme
;; Accepts lists
(execute (mapping (lambda (x) (+ x 1))) (list 1 2 3 4 5)) ;; => '(2 3 4 5 6)

;; Accepts vectors
(execute (mapping (lambda (x) (+ x 1))) (vector 1 2 3 4 5)) ;; '#(2 3 4 5 6)

;; Even accepts streams!
(define (integers n)
    (stream-cons n (lambda () (integers (+ 1 n)))))

(execute (taking 5) (integers 0)) ;; => '(0 1 2 3 4)
```

Transduce is just `reduce` with more bells and whistles and works similarly:

```scheme
;; (-> transducer reducing-function initial-value iterable)
(transduce (mapping (lambda (x) (+ x 1))) + 0 (list 0 1 2 3)) ;; => 10
```

Compose just combines the iterator functions and lets us avoid intermediate allocation. The composition works left to right - it chains each value through the functions and then accumulates into the output type. See the following:

```scheme
(define xf 
    (compose 
        (mapping add1)
        (filtering odd?)
        (taking 5)))

(execute xf (range 0 100)) ;; => '(1 3 5 7 9)
```

By default, execute outputs to the same type that was passed in. In other words, if you `execute` a `list`, it will return a `list`. However, if you so choose, you can pass in a symbol specifying the output type of your choosing like so:

```scheme
(define xf 
    (compose 
        (mapping add1)
        (filtering odd?)
        (taking 5)))

;; Takes a list and returns a vector
(execute xf (range 0 100) 'vector) ;; => '#(1 3 5 7 9)

;; Takes a vector and returns a list
(execute xf (vector 0 1 2 3 4 5 6 7 8 9 10) 'list) ;; => '(1 3 5 7 9)
```

## Syntax Choices

`Steel` is mildly opinionated in that there a few ways to define variables and functions. These choices are fairly arbitrary except for the shorthand function syntax, which I borrowed from Racket. `defn` and `fn` were really encouraged by me wanting to type less characters.

```scheme

;; All of the following are equivalent
(define (foo x) (+ x 1))
(define foo (lambda (x) (+ x 1)))
(defn (foo x) (+ x 1))
(defn foo (lambda (x) (+ x 1)))

;; All of the following are equivalent
(lambda (x) (+ x 1))
(λ (x) (+ x 1))
(fn (x) (+ x 1))
```

## Examples of embedding Rust values in the virtual machine

Rust is easily embeddable due to the use of some helpful attribute macros

```rust
[steel]
pub struct MyStruct {
    pub field: usize,
    pub stays_the_same: usize,
    pub name: String,
}

#[steel]
pub struct CoolTest {
    pub val: f64,
}

#[steel]
pub struct UnnamedFields(pub usize);

#[steel]
pub struct Foo {
    pub f: UnnamedFields,
}

pub fn build_interpreter_and_modify() {
    // Construct interpreter with 3 custom structs
    // each has now getters, setters, a predicate and constructor
    let mut interpreter = build_engine! {
        MyStruct,
        CoolTest,
        Foo
    };

    // define value outside of interpreter to embed
    let test = UnnamedFields(100);
    // embed the value
    interpreter.register_value("unnamed", test.new_steel_val());

    // write a quick script
    let script = "
    (define cool-test (CoolTest 100))
    (define return-val (set-CoolTest-val! cool-test 200))
    (define foo-test (Foo unnamed))
    ";

    // get the values back out
    if let Ok(_) = interpreter.parse_and_execute_without_optimizations(script) {
        let ret_val = CoolTest::try_from(interpreter.extract_value("return-val").unwrap()).unwrap();
        println!("{:?}", ret_val); // Should be "CoolTest { val: 200.0 }"
        let ret_val2 =
            UnnamedFields::try_from(interpreter.extract_value("unnamed").unwrap()).unwrap();
        println!("{:?}", ret_val2); // Should be "UnnamedFields(100)"
        let ret_val3 = Foo::try_from(interpreter.extract_value("foo-test").unwrap()).unwrap();
        println!("{:?}", ret_val3); // Should be Foo { f: UnnamedFields(100) }
    };
}
```

## Attribute Macros

The `steel_derive` crate contains a number of procedural macros designed to make your life easier while using `Steel`. The macros are as follows:

* `#[steel]`
* `#[function]`

The `#[steel]` attribute operates on structs currently (enums are not yet supported). It derives the `CustomType` and `StructFunctions` trait, which allows for embedding inside the interpreter with constructors, predicates, getters, and setters, automatically defined. For example, the follow code snippet:

```rust
#[steel]
pub struct Foo {
    pub bar: usize
}
```

Would result in bindings for the following scheme functions:

```scheme
Foo
Foo?
set-Foo-bar!
Foo-bar
```

Example usage:

```scheme
(define my-foo (Foo 10)) ;; #<void>
(Foo? my-foo) ;; #t
(set-Foo-bar! my-foo 25) ;; 10
(Foo-bar my-foo) ;; 25
```

The `#[function]` attribute macro operates on functions. It _transforms_ the function from a normal rust function into a function that matches the form used inside the `Steel` interpreter. Functions inside the `Steel` interpreter have the following signature:

```rust
fn(&[Gc<SteelVal>]) -> Result<Gc<SteelVal>>
```

This macro attempts to remove a great deal of the boilerplate with respect to transferring values in and out of the semantics of the interpreter. However, this means that a function tagged with the `#[function]` attribute **_cannot_** be used as a standard Rust function with the original signature. For a rough idea of what this function does, let's look at a function and its resultant expansion:

Example function:

```rust
#[function]
pub fn multiple_types(val: u64) -> u64 {
    val + 25
}
```

Expands to:

```rust
pub fn multiple_types(args: &[Gc<SteelVal>]) -> Result<Gc<SteelVal>, SteelErr>
{
    pub fn multiple_types(val: u64) -> u64 { val + 25 }
    if args.len () != 1usize {
        steel::stop!(ArityMismatch => format!("{} expected {} arguments, got {}", stringify!(multiple_types), 1usize.to_string (), args.len()))
    }
    let res = multiple_types(unwrap!((*(args [0usize])).clone(), u64)?);
    Ok(Gc::new(SteelVal::try_from(res)?))
}
```

The macro operates by defining a wrapper function arounds the original definition. The original definition shadows the wrapper, which allows us to call the original function with some boilerplate for going in and out of `SteelVals`.

## VM Macro

So now that we've defined some structs and functions, how do we get them into the interpreter? There is a helpful interpreter macro that is given to build and embed the functions into the interpreter (to then pass into the repl). Here is an example of the macro usage:

```rust
build_vm! {
    Structs => {
        MyStruct,
        CoolTest,
        Foo,
        MutexWrapper
    }
    Functions => {
        "add-cool-tests" => add_cool_tests,
        "multiple-types" => multiple_types,
        "new-mutex-wrapper" => new_mutex_wrapper
    }
}
```

This builds a mutable interpreter with all of the relevant bindings for the structs (getters, setters, constructors and predicates), and all of the functions that are given with the relevant bindings.

You can launch a repl by passing the result of `build_vm!` into `repl_base`, as follows:

```rust
repl_base(build_engine!{...})
```

From here, these would be valid calls:

```scheme
> (define cool-test-1 (CoolTest 1))
> (define cool-test-2 (CoolTest 2))
> (define cool-test-3 (add-cool-tests cool-test-1 cool-test-2))
> (CoolTest-val cool-test-3)
3
> (multiple-types 25)
50
```



## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
