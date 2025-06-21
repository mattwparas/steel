# Features

Steel supports the R5Rs spec, and implements a number of modern features, including:

* Modules, using `require` and `provide` much like Racket
* `syntax-rules` and `syntax-case` macros. These macros can be exported and required much like any other symbol, and respect hygiene.
* Has a sophisticated FFI layer, where Rust is a first class citizen. Unlike most languages where C is the choice for FFI,
Steel wraps Rust using a stable abi layer and prefers Rust.
* Almost all of the built in data structures are immutable by default, including lists, vectors, hashmaps, and hashsets.
* A language server and a package manager.

Embedding the interpreter into a host application has many advantages, namely that native rust structs and functions can
be easily passed to the interpreter to use. This means you can expose a set of functions to script your application, and
steel can use them efficiently. When you would like users to be able to extend your applications with native libraries not
bundled directly with the application, then FFI should be preferred.

