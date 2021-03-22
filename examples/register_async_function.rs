use steel::steel_vm::engine::Engine;

use steel::steel_vm::register_fn::RegisterAsyncFn;

async fn test_function() -> usize {
    println!("Inside async function!");
    10 + await_within().await
}

async fn await_within() -> usize {
    25
}

pub fn main() {
    let mut vm = Engine::new();

    // You can even register async finctions
    vm.register_async_fn("test", test_function);

    let contents = include_str!("scripts/async.rkt");

    let res = vm.parse_and_execute_without_optimizations(&contents);

    if let Err(e) = res {
        e.emit_result("async.rkt", &contents);
    }

    let contents = include_str!("scripts/async-threads.rkt");

    let res = vm.parse_and_execute_without_optimizations(&contents);

    if let Err(e) = res {
        e.emit_result("async-threads.rkt", &contents);
    }
}
