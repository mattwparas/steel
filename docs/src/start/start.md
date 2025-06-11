# Getting Started

## Playground

If you want to start exploring Steel you can use the [Steel
Playground](https://mattwparas.github.io/steel-playground/dev)

## Local Install options

A local install provides

1. The Steel interpreter, `steel`
1. The Steel dynamic library installer, `cargo-steel-lib`
1. The Steel Language Server, `steel-language-server`
1. The Steel libraries under the `cogs` directory
1. The Steel package manager `forge`


### Using `cargo`'s `--git` option

For a completely managed install through `cargo` use the following command

```
$ cargo install --git https://github.com/mattwparas/steel.git steel-interpreter forge steel-language-server cargo-steel-lib
```

### Manually cloning the Steel repository

You will need to have [Rust](https://www.rust-lang.org/tools/install)
installed on your system.

1. Clone the [Steel repository](https://github.com/mattwparas/steel)
1. From the cloned repository's root folder execute `cargo xtask install`.


## Steel CLI

Once you have Steel installed you can start the interpreter in your
terminal by issuing the command `steel`[^path-note].

[^path-note]: Typically the binary will be installed in `.cargo/bin`
under your user's home directory. Steel libraries by default reside in
`.steel/cogs` under your user's home directory.


Steel's CLI provides some other options.

```
$steel --help

Steel repl and command line interface

Usage: steel [DEFAULT_FILE] [ARGUMENTS]... [COMMAND]

Commands:
  bytecode     Output a debug display of the fully transformed bytecode [aliases: b]
  ast          Print a debug display of the fully expanded AST [aliases: a]
  interactive  Enter the repl with the given file loaded [aliases: r]
  test         Tests the module - only tests modules which provide values [aliases: t]
  doc          Generate the documentation from source code [aliases: d]
  compile      Compile. (Experimental) [aliases: c]
  dylib        Build a dylib from the root of this directory [aliases: dl]
  completions  Generate Shell completions

Arguments:
  [DEFAULT_FILE]  The existence of this argument indicates whether we want to run the repl, or interpret this file
  [ARGUMENTS]...  Arguments to the input file

Options:
  -h, --help     Print help
  -V, --version  Print version
```

Each sub-command has its own `--help` option that provides more
information on the sub-commands use.


### Shell autocompletions for `steel`

The `steel` CLI can generate the necessary code to provide CLI
autocompletion for supported shells. For example for Bash

```
$ steel completions bash > steel.bash
```

Will generate the file `steel.bash` with all the Bash related code
that adds autocompletion features to the `steel` CLI in Bash. Consult
your shell's documentation on how to install and permanently have
these autocompletions available.
