use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;

use steel::steel_vm::register_fn::RegisterAsyncFn;

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

async fn test_function() -> usize {
    10
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

    // You can even register async finctions
    vm.register_async_fn("test", test_function);

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

    let baz: Result<String, String> = vm.extract("baz").unwrap();
    println!("baz: {}", baz.clone().unwrap());
    assert_eq!("bananas".to_string(), baz.unwrap());
}
