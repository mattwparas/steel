# Steel

<div align="center">
    <img width="150px" src="images/styled.png">
</div>

<div align="center">

An embeddable and extensible scheme dialect built in Rust.

![Actions Status](https://github.com/mattwparas/steel/workflows/Build/badge.svg)
![Actions Status](https://github.com/mattwparas/steel/workflows/Docker%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/mattwparas/steel/badge.svg?branch=master)](https://coveralls.io/github/mattwparas/steel?branch=master)
[![Discord Chat](https://img.shields.io/discord/1152443024715034675.svg?logo=discord&label=discord)](https://discord.gg/WwFRXdN6HU)
[![Matrix Chat](https://img.shields.io/matrix/steel:matrix.org?logo=element&label=matrix)](https://matrix.to/#/#steel:matrix.org)

<a href="https://mattwparas.github.io/steel-playground/dev">
    <b>Try it on the Playground</b>
</a>
·
<a href="https://mattwparas.github.io/steel/book">
    <b>Read the Steel book (WIP)</b>
</a>

</div>

## Getting Started

This github repository contains a cli interpreter. To try it out on the online playground, go to the [Steel playground](https://mattwparas.github.io/steel-playground/dev). To get started using a repl with the crates, make sure you first have rust installed.

Then, clone the repo and run the following command:

```bash
cargo run
```

This will launch a REPL instance that looks something like this:

<p align="center">
  <img src="images/repl.gif" width="100%">
</p>

### Full install

If you'd like to install everything, just run the following command:

```bash
cargo xtask install
```

This will install:

- The steel interpreter, `steel`
- The package manager, `forge`,
- The dylib installer, `cargo-steel-lib` (also available via the interpreter)
- The steel language server
- The standard library, found under the `cogs` directory

### Packages

If you would like to customize the location of installed packages, please set the `STEEL_HOME` environment variable. Steel currently follows XDG if present, and otherwise assumes the default of `$HOME/.steel` if the `STEEL_HOME` environment variable is not already set.

## About

`Steel` is an embeddable scheme interpreter, with a standalone cli included as well. Inspired largely by Racket, the language seeks to be ergonomic scheme variant helpful for embedding in applications, or to be used on its own with high performance functions implemented in Rust. The language implementation itself contains a fairly powerful macro system based on the `syntax-rules` style and a bytecode virtual machine. At the moment, it is mostly compliant with R5RS, only missing `let-syntax` support. Support for R7Rs is underway.

> **Warning**
> The API is relatively stable, however it may change at any time while pre 1.0. Care will be taken to keep things backwards compatible where possible.

## Features

- R5RS support
- Modules, using `require` and `provide` much like Racket.
- `syntax-rules` and `syntax-case` macros.
- Easy integration with native Rust functions and structs, either through embedding or via FFI.
- Higher order Contracts
- Built in immutable data structures, including:
  - lists
  - vectors
  - hashmaps
  - hashsets

For more details, see the [book](https://mattwparas.github.io/steel/book)

## License

Licensed under either of

- Apache License, Version 2.0
  ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license
  ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

See [CONTRIBUTING.md](./CONTRIBUTING.md).
