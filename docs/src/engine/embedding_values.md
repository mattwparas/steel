# Embedding values

Rust values, types, and functions are easily embedded into Steel. Using the `register_fn` call, you can embed functions easily:

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

    vm.run(
        r#"
        (define foo (external-function 10 25))
        (define bar (option-function "applesauce"))
        (define baz (result-function "bananas"))
    "#,
    )
    .unwrap();

    let foo = vm.extract::<usize>("foo").unwrap();
    println!("foo: {}", foo);
    assert_eq!(35, foo);

    // Can also extract a value by specifying the type on the variable
    let bar: String = vm.extract("bar").unwrap();
    println!("bar: {}", bar);
    assert_eq!("applesauce".to_string(), bar);

    let baz: String = vm.extract("baz").unwrap();
    println!("baz: {}", baz);
    assert_eq!("bananas".to_string(), baz);
}
```

We can also embed structs themselves:

```rust,noplaypen
use steel_vm::engine::Engine;
use steel_vm::register_fn::RegisterFn;

use steel_derive::Steel;

// In order to register a type with Steel,
// it must implement Clone, Debug, and Steel
#[derive(Clone, Debug, Steel, PartialEq)]
pub struct ExternalStruct {
    foo: usize,
    bar: String,
    baz: f64,
}

impl ExternalStruct {
    pub fn new(foo: usize, bar: String, baz: f64) -> Self {
        ExternalStruct { foo, bar, baz }
    }

    // Embedding functions that take self must take by value
    pub fn method_by_value(self) -> usize {
        self.foo
    }

    // Setters should update the value and return a new instance (functional set)
    pub fn set_foo(mut self, foo: usize) -> Self {
        self.foo = foo;
        self
    }
}

pub fn main() {
    let mut vm = Engine::new();

    // Registering a type gives access to a predicate for the type
    vm.register_type::<ExternalStruct>("ExternalStruct?");

    // Structs in steel typically have a constructor that is the name of the struct
    vm.register_fn("ExternalStruct", ExternalStruct::new);

    // register_fn can be chained
    vm.register_fn("method-by-value", ExternalStruct::method_by_value)
        .register_fn("set-foo", ExternalStruct::set_foo);

    let external_struct = ExternalStruct::new(1, "foo".to_string(), 12.4);

    // Registering an external value is fallible if the conversion fails for some reason
    // For instance, registering an Err(T) is fallible. However, most implementation outside of manual
    // ones should not fail
    vm.register_external_value("external-struct", external_struct)
        .unwrap();

    let output = vm
        .run(
            r#"
            (define new-external-struct (set-foo external-struct 100))
            (define get-output (method-by-value external-struct))
            (define second-new-external-struct (ExternalStruct 50 "bananas" 72.6))
            "last-result"
        "#,
        )
        .unwrap();

    let new_external_struct = vm.extract::<ExternalStruct>("new-external-struct").unwrap();
    println!("new_external_struct: {:?}", new_external_struct);
    assert_eq!(
        ExternalStruct::new(100, "foo".to_string(), 12.4),
        new_external_struct
    );

    // Can also extract a value by specifying the type on the variable
    let get_output: usize = vm.extract("get-output").unwrap();
    println!("get_output: {}", get_output);
    assert_eq!(1, get_output);

    let second_new_external_struct: ExternalStruct =
        vm.extract("second-new-external-struct").unwrap();
    println!(
        "second_new_external_struct: {:?}",
        second_new_external_struct
    );
    assert_eq!(
        ExternalStruct::new(50, "bananas".to_string(), 72.6),
        second_new_external_struct
    );

    // We also get the output of the VM as the value of every expression run
    // we can inspect the results just by printing like so
    println!("{:?}", output);
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
