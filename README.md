# steel

![Actions Status](https://github.com/mattwparas/steel/workflows/Rust/badge.svg)
(https://github.com/mattwparas/steel/actions)

An embedded scheme interpreter in Rust.

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
