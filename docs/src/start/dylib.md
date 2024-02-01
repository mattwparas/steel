# Making a Steel Module in Rust

Sometimes you may want to create a Steel module by calling Rust. This can be
useful to:

- Optimize a performance critical codepath with Rust.
- Take advantage of Rust's rich ecosystem.

Whatever the case, Steel provides facilities to create reusable modules based on
Rust code. This process involves compiling Rust into a `dylib`.

## Guide

In this guide, we'll make a `dylib` that will wrap the `sys-info` crate. There
are roughly 3 steps:

1. Create a new Rust library of type "cdylib".
1. Define a module and register types and functions to it.
1. Build the Steel crate and install it to `$STEEL_HOME/native`.
1. Use the library from Steel with `#%require-dylib`.

### Creating a cdylib library

To start, create a new library using `cargo`:

```
$ cargo new --lib steel-sys-info
```

This should create a directory structure as follows:

```
├── Cargo.toml
├── src
│   └── lib.rs
```

We'll want to make this a `cdylib` library for Steel, so we'll perform the following adjustments in `Cargo.toml`:

1. Set the `crate-type` to `"cdylib"`.
1. Include `steel-core` with the `dylibs` feature as a dependency.
1. Include `abi_stable` as a dependency. This is required by some `steel-core`
   macros.

```toml
[package]
name = "steel-sys-info"
version.workspace = true
edition = "2021"

[lib]
name = "steel_sys_info"
crate-type = ["cdylib"]

[dependencies]
# I'm running this example based on the `steel-sys-info` library found in the steel repo. If you're
# running this on your own, use whichever steel version you'd like to target and pin to that.
steel-core = { workspace = true, features = ["dylibs"] }
abi_stable = "0.11.1"
sys-info = "0.9.1"
```

This means that when we run `cargo build` we'll produce a shared library (`.so`
file). The shared library can be loaded into other programs, Steel in our case.

## Creating a module

For the purposes of this example, we'll create a module that wraps the `MemInfo`
struct, and expose the information there. Since we'll be implementing traits
that are defined inside the `steel` crate, we'll need to create a struct to wrap
the `sys_info::MemInfo` struct:

```rust,noplaypen
struct MemoryInfo {
    info: sys_info::MemInfo,
}

impl MemoryInfo {
    fn total(&self) -> isize {
        self.info.total as isize
    }

    fn avail(&self) -> isize {
        self.info.avail as isize
    }

    fn free(&self) -> isize {
        self.info.free as isize
    }

    fn buffers(&self) -> isize {
        self.info.buffers as isize
    }

    fn cached(&self) -> isize {
        self.info.cached as isize
    }

    fn swap_total(&self) -> isize {
        self.info.swap_total as isize
    }

    fn swap_free(&self) -> isize {
        self.info.swap_free as isize
    }
}
```

Now that we've done that, we can expose this to steel by implementing the
`Custom` type for the struct, and declaring an `FFIModule`:

```rust,noplaypen
// Using ABI Stable types is very important
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

impl Custom for MemoryInfo {}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/sys-info");

    module.register_fn("mem-info", || MemoryInfo {
        info: sys_info::mem_info().unwrap(),
    });

    module
        .register_fn("MemoryInfo-total", MemoryInfo::total)
        .register_fn("MemoryInfo-avail", MemoryInfo::avail)
        .register_fn("MemoryInfo-free", MemoryInfo::free)
        .register_fn("MemoryInfo-buffers", MemoryInfo::buffers)
        .register_fn("MemoryInfo-cached", MemoryInfo::cached)
        .register_fn("MemoryInfo-swap-total", MemoryInfo::swap_total)
        .register_fn("MemoryInfo-swap-free", MemoryInfo::swap_free);

    module
}
```

The `register_fn` API will perform all of the necessary coercions necessary to
make this as safe as possible. At the end of the day, this is FFI and we are
loading shared libraries, so there is some unsafe Rust code, however steel uses
the underlying `abi_stable` library in order to make interactions with the
shared library as safe as possible.


### Installing the library

To install the dylib in a location where the `steel` interpreter will find it,
from the root of the library just run:

```
$ cargo steel-lib
```

This will build the crate, and copy the resulting dylib to `$STEEL_HOME/native`.

### Using the library from Steel

To load the library, use the syntax `#%require-dylib` - This operates similary
to a standard `require`, in that all of the modifiers you're used to using work,
such as `only-in` and `prefix-in`. However, the argument is no longer the path
to the library, but rather the name of the library without the extension. By
default, the library will be named the `[lib]` name used in the toml, prefixed
with `lib`.

```scheme
(#%require-dylib "libsteel_sys_info"
                 (only-in mem-info
                          MemoryInfo-total
                          MemoryInfo-avail
                          MemoryInfo-free
                          MemoryInfo-buffers
                          MemoryInfo-cached
                          MemoryInfo-swap-total
                          MemoryInfo-swap-free))

(provide current-memory-usage memory-usage-as-percentage)

(define (current-memory-usage #:memory-info (memory-info (mem-info)))
  (- (MemoryInfo-total memory-info) (MemoryInfo-free memory-info) (MemoryInfo-cached memory-info)))

(define (memory-usage-as-percentage #:memory-info (memory-info (mem-info)))
  (/ (current-memory-usage #:memory-info memory-info) (MemoryInfo-total memory-info)))

```

This can then be installed as a library itself on the machine, and required just
like any other library, using a `cog.scm` file for the manifest.
