# Name Pending

## Plan

* An embedded scheme interpreter in Rust
  * Rust has some features that make implementing a scheme interesting, such as iterators, traits, and lifetimes.
  * There should be an API that users can interface with to embed rust function inside the scheme.
  * Difficulties will include: 
    * How to handle lexing/parsing/return values, designing a clean API that users can interface with.
    * Quote/Quasiquote/Unquote semantics
    * Lifetimes/Memory management - Lexical scope and global environments can make things tricky

## Features

* Must haves:
  * Some arbitrary subset of R5S5, which includes:
    * Functions and variable definitions
    * Lexical scope
    * Let expressions
    * Begin expressions
    * Partial Tail-call Optimization
  * REPL/Interpreter
  * Trait definitions that allow arbitrary rust functions to be embedded in the scheme
* Nice to have, bit of a reach
  * Macros
  * Quote/Quasiquote/Unquote
  * Structs
  
## Use Cases
* Scripting
* Rust has better concurrency, safe memory management, pattern matching
  * Write this stuff in Rust, leverage it in the scheme for expressivity
      * Building multiple TCP sockets to listen for client connections to perform tasks at the same time
      * Desire for pattern matching and fast runtime
      * Connecting scheme flexibility to a powerful systems backend language
      
## Example Trait Definitions
```rust
trait RucketFunc {
    // Static method signature; `Self` refers to the implementor type.
    fn new_func(name: &'static str) -> fn(&[&RucketVal]) -> Result<RucketVal, RucketErr>;
}

impl From<T> to RucketVal {...}
impl From<RucketVal> to T {...}
```
