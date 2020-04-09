# steel

![Actions Status](https://github.com/mattwparas/steel/workflows/Build/badge.svg)

An embedded scheme interpreter in Rust.

## Getting Started

This github repository is a client that uses the `steel` and `steel_derive` crates. To get started using a repl with the crates, make sure you first have rust installed.

Then, clone the repo and run the following command:

```bash
cargo run
```

This will launch a REPL instance that looks something like this:

<p align="center">
  <img src="images/repl.gif" width="70%">
</p>

## Examples

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
    let mut interpreter = build_interpreter! {
        MyStruct,
        CoolTest,
        Foo
    };

    // define value outside of interpreter to embed
    let test = UnnamedFields(100);
    // embed the value
    interpreter.insert_binding("unnamed", test.new_steel_val());

    // write a quick script
    let script = "
    (define cool-test (CoolTest 100))
    (define return-val (set-CoolTest-val! cool-test 200))
    (define foo-test (Foo unnamed))
    ";

    // get the values back out
    if let Ok(_) = interpreter.evaluate(script) {
        let ret_val = unwrap!(interpreter.extract_value("return-val").unwrap(), CoolTest).unwrap();
        println!("{:?}", ret_val); // Should be "CoolTest { val: 200.0 }"
        let ret_val2 =
            unwrap!(interpreter.extract_value("unnamed").unwrap(), UnnamedFields).unwrap();
        println!("{:?}", ret_val2); // Should be "UnnamedFields(100)"
        let ret_val3 = unwrap!(interpreter.extract_value("foo-test").unwrap(), Foo).unwrap();
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
fn(Vec<Rc<SteelVal>>) -> Result<Rc<SteelVal>>
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
pub fn multiple_types (args: Vec<Rc<SteelVal>>) ->
Result<Rc<SteelVal>, SteelErr>
{
    pub fn multiple_types(val: u64) -> u64 { val + 25 }
    let res = multiple_types(unwrap!((*(args [0usize])).clone(), u64)?);
    Ok(Rc::new(SteelVal::try_from(res)?))
}
```

The macro operates by defining a wrapper function arounds the original definition. The original definition shadows the wrapper, which allows us to call the original function with some boilerplate for going in and out of `SteelVals`.

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
