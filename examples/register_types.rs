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
