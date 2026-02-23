# Context variables

When embedding steel inside of another application, it is common to need to reference some global context. For example, lets say
we're embedding steel within a game. One easy way to do this is to enter a point where you pass a reference
to the game context to steel, and call a function with it.

First, we'll need to make our game context available to the engine, so we'll implement the `CustomReference` trait.

```rust,noplaypen
pub struct GameContext<'a> {
  // Game stuff
}

impl<'a> CustomReference for GameContext<'a> {}
steel::custom_reference!(GameContext<'a>);
```

Now, to actually let Steel code interact with the context, we need to take a reference to the game context and provide it to steel:

```rust,noplaypen
fn call_function_in_engine(
    engine: &mut Engine,
    ctx: &mut GameContext<'_>,
    name: &str,
    func_args: Vec<SteelVal>,
) {
    engine
        .with_mut_reference(ctx)
        .consume_once(|engine, args| {
            engine.update_value("*ctx*", args.into_iter().next().unwrap());
            engine.call_function_by_name_with_args(name, func_args)
        })
        .unwrap();
}
```

Notice though that `*ctx*` has been updated, but we haven't registered it yet. This example assumes that `*ctx*` is some
globally available value, so when we create our `Engine` instance, we'll register it there first:

```rust,noplaypen
pub static CTX: &'static str = "*ctx*";

pub fn register_primitives(engine: &mut Engine) {
    engine.register_value(CTX, SteelVal::Void);
}
```


At this point though we don't have any functions. Let's add a function on the game context and register it:

```rust,noplaypen
impl<'a> GameContext<'a> {
  fn add_hex(&mut self) {
    // Something
  }
}

pub fn register_primitives(engine: &mut Engine) {
    module
        .register_fn("add-hex", GameContext::add_hex);
}

```

Now, from the steel side, unfortunately we'd have to call it like this:

```scheme
(add-hex *ctx*)
```

And for _every_ function that expects this context, we'd have to provide it. So what we're going to do instead
is register the function such that some context variable is implicitly provided:

```rust
pub fn register_primitives(engine: &mut Engine) {
    module
        .register_fn_with_ctx(CTX, "add-hex", GameContext::add_hex);
}
```

Now, all we have to do is make sure to update `*ctx*` before we enter a new context, to refresh
the variable with the updated reference, and we can call the function without needing to pass it in:

```scheme
(add-hex)
```
