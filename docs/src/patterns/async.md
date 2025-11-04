# Async

Steel does not (currently) expose an async `run` function from the engine API. That being said,
you can still register async functions and cooperate with a runtime:


```rust

use std::{sync::Arc, time::Duration};

use steel::{
    rvals::SteelVal,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

pub async fn test() -> usize {
    tokio::time::sleep(Duration::from_secs(1)).await;
    10
}

fn main() {
    // Set up a shared runtime
    let main_runtime = Arc::new(
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap(),
    );

    let mut module = BuiltInModule::new("my/async/functions");

    let runtime = main_runtime.clone();

    module
        .register_fn("call-test", test)
        .register_fn("await", move |value: SteelVal| {
            if let SteelVal::FutureV(f) = value {
                let shared = f.unwrap().into_shared();
                runtime.block_on(shared)
            } else {
                Ok(value)
            }
        });

    let mut engine = steel::steel_vm::engine::Engine::new();

    engine.register_module(module);

    main_runtime
        .block_on(async {
            std::thread::spawn(|| steel_repl::run_repl(engine))
                .join()
                .unwrap()
        })
        .unwrap()
}

```

And using the launched repl:

```
λ > (require-builtin my/async/functions)
λ > (define res1 (call-test))
λ > (define res2 (call-test))
λ > (define combined (futures-join-all res1 res2))
λ > (await combined)
=> '#(10 10)
```
