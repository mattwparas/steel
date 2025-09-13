# Stdlib

Unlike the steel builtins, some steel procedures are not written in rust and are
instead written directly in scheme.

This means that these procedures cannot be required via
`(require-builtin <module name>)`, but have to be required via the normal `require`
procedure, as in `(require "<module name>")`.

Additionally, steel defines a so-called "prelude", a list of modules that steel
loads into the context of the engine automatically. this list currently includes
`#%private/steel/stdlib`, `#%private/steel/control`, `#%private/steel/contract`,
`#%private/steel/print`, `#%private/steel/ports`, `#%private/steel/reader`,
`#%private/steel/match`, `#%private/steel/control` and `#%private/steel/contract`.
