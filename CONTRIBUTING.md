# Contributing to Steel

## Getting Started

The following will clone Steel from the primary repository and run the test
suite. You should have previously setup a [recent rust tool
chain](https://www.rust-lang.org/tools/install).

```bash
git clone https://github.com/mattwparas/steel.git &&
cd steel &&
cargo xtask install &&
cargo test --all
```

This will install:

* The steel interpreter, `steel`
* The dylib installer, `cargo-steel-lib` (also available via the interpreter)
* The steel language server
* The standard library, found under the `cogs` directory

## Commit message style

Steel does not employ a strict commit message style or convention. Try to
follow best practices by keeping the first line concise and descriptive,
otherwise use your best judgement.

## Submitting Patches

IMPORTANT: By submitting a patch, you agree that your work will be licensed
under the license used by the project.

Follow common advice when submitting a patch:

- Keep the patch focused.
- Include tests.
- Work in a separate branch, not `master`.
- Before working on "big ideas", open an issue to discuss approaches or
  requirements to limit wasted effort.

## Generating documentation

Run the following command to update the documentation in `docs/` based on the code in `crates/`:

```sh
cargo xtask docgen
```
