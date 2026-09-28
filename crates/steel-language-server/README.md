# Steel language server

## Installation

From the directory of this crate, run:

```
cargo install --path .
```

This will install the `steel-language-server` to your path.

Configuration in helix can be done by adding the following to your `languages.toml` file:

```toml
[[language]]
name = "scheme"
language-servers = [ "steel-language-server" ]

[language-server.steel-language-server]
command = "steel-language-server"
args = []
```

Configuration in neovim can be done by adding the following to your configuration

```lua
configs.steel = {
    default_config = {
        cmd = { "steel-language-server" },
        filetypes = { "scheme" },
        root_dir = function(_)
            -- We can run on single file, so root dir doesn't really matter.
            require("lspconfig.util").root_pattern "*"
        end,
        single_file_support = true,
        docs = {
            description = [[
Steel language server.

Install from: https://github.com/mattwparas/steel/tree/master/crates/steel-language-server

Check the repo for more configuration.
]],
            default_config = {}
        }
    }
}
```

Now you can call

```lua
require'lspconfig'.steel.setup {
  -- Your usual configuration (handlers, capabilities, etc.) goes here.
}
```

VSCode, and emacs installation instructions coming soon.

## Configuration

If you're embedding steel within a host application, it is possible the language server does not have knowledge
of the globals given by the host application. To make these available to the language server, create a configuration file
for the language server under the path specified by the env var `STEEL_LSP_HOME`.

For example, I have set my `STEEL_LSP_HOME` to `~/.config/steel-lsp/` and added the file `globals.scm`:

```scheme

(define keymaps (#%module "helix/core/keymaps"))

(define (register-values module values)
  (map (lambda (ident) (#%module-add module (symbol->string ident) void)) values))

(register-values keymaps
                 '(helix-current-keymap *buffer-or-extension-keybindings*
                                        *reverse-buffer-map*
                                        helix-merge-keybindings
                                        helix-string->keymap
                                        *global-keybinding-map*
                                        helix-deep-copy-keymap))

(define typable-commands (#%module "helix/core/typable"))
(define static-commands (#%module "helix/core/static"))
(define editor (#%module "helix/core/editor"))

(register-values typable-commands '())

(#%ignore-unused-identifier "_")

keymaps

```

The following rules apply: Any global value that is defined as a builtin module, will be interpreted by the LSP and used, or evaluated as a top level env var. So either defining `keymaps` or evaluating like so on the last line of the example will get the module to show up.

### Signatures of host functions

Instead of `void`, a module entry can carry the function's signature, so the language server can check
calls to it (wrong argument types, wrong number of arguments) and show the types in hints:

```scheme
(define demo (#%module "host/demo"))

(#%module-add demo "host-add" (#%signature (args Int Int) (returns Int)))
(#%module-add demo "host-shout" (#%signature (args (or String Symbol)) (returns String)))
(#%module-add demo "host-log" (#%signature (args String (rest Any)) (returns Void) (raises BadArgs)))
```

The names are the `ValueType` variants the `steel_derive` function macros use (`Int`, `Float`, `Number`,
`String`, `Symbol`, `List`, `Pair`, `Vector`, `MutableVector`, `HashMap`, `HashSet`, `Procedure`, `Any`, ...).
A union is `(or List Pair)`, and `(rest T)` gives the type of every argument past the fixed ones.
A value of a host type (one implementing `Custom`) is written with its full Rust type name, as
`core::any::type_name` gives it:

```scheme
(#%module-add demo "make-counter" (#%signature (args) (returns (custom "my_app::Counter"))))
(#%module-add demo "counter-value" (#%signature (args (custom "my_app::Counter")) (returns Int)))
```

The language server trusts these: a signature claiming a type the function does not actually require will
produce wrong errors.

### Generating the file

Rather than writing this by hand, the host application can generate it from the functions it registers:

```rust
std::fs::write(
    lsp_home.join("host.scm"),
    engine.emit_lsp_definitions(),
)?;
```

`emit_lsp_definitions` describes every module the engine has beyond a stock one. Functions registered with
`register_fn` get signatures read from their Rust argument and return types; other values are listed with
`void`. A function registered with `register_fn_with_ctx` is listed without its context argument, since
callers never pass it. Functions registered directly on the engine rather than in a module are not included.
Regenerate the file when the registered functions change.

Any symbol added with `#%ignore-unused-identifier` will escape that symbol from being shown as an unused identifier.

Any global registered with `#%register-global` will be ignored as a free identifier as well.

