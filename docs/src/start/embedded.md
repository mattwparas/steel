# Using Steel as an embedded scripting engine

Steel can be used as a scripting language within a Rust program. You can achieve
this by creating a Steel `Engine` object. The `Engine` object allows you to
execute Scheme code and interact with it from your Rust program. For more
details about `Engine`, see the [Engine API](../engine/engine.html).

## Steel Engine

The Steel Virtual Machine is provided by the `steel-core` trait.

```toml
[dependencies]
steel-core = { git="https://github.com/mattwparas/steel.git", branch = "master" }
```

The following example runs a few expressions in a Steel `Engine` and asserts
that the results are as expected.

```rust,noplaypen
use steel::steel_vm::engine::Engine;
use steel::SteelVal;

fn main() {
    let mut steel_engine = Engine::new();
    let answer = steel_engine.run(
        (r#"
      (+ 1 2 3 4)
      (+ 5 6 7 8)
    "#),
    );
    assert_eq!(answer, vec![SteelVal::IntV(10), SteelVal::IntV(26)])
}
```

### Engine::new

Creates a new engine. The `Engine` is used to run Steel Scheme code. Note that
the `Engine` is not `Send` or `Sync`. This means it is bound to the current
thread and cannot be shared or sent across other threads.

```rust,noplaypen
let mut steel_engine = Engine::new();
```

### Engine::run

Runs a Steel expression and returns the result as a `Vec<SteelVal>`. If any
error occurs, then `Err(SteelErr)` is returned.

```rust,noplaypen
let mut steel_engine = Engine::new();
assert_eq!(steel_engine.run("(+ 1 1)"), Ok(vec![SteelVal::IntV(2)]));
assert!(steel_engine.run("(+ 1 undefined-identifier)").is_err());
```

## Embedding The Steel REPL

Repl functionality is provided by the `steel-repl` crate.

```toml
[dependencies]
steel-repl = { git="https://github.com/mattwparas/steel.git", branch = "master" }
```

### run_repl

`run_repl` runs the Steel repl until an IO error is encountered or the user
exits the repl. The repl may be exited by:

- Running the `(quit)` Steel Scheme function.
- Pressing either `ctrl+c` or `ctrl+d` within the repl.

```rust,noplaypen
let steel_engine = Engine::new();
steel_repl::run_repl(steel_engine).unwrap();
```
