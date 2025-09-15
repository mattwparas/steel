# Using Steel on its own

The `steel` executable contains a number of options:

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

The most straightforward usage would be to provide an entrypoint to the interpreter like so:

```scheme
;; hello-world.scm
(displayln "Hello world!")
```

```shell
steel hello-world.scm
Hello world!
```
