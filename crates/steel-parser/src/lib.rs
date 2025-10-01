#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(any(feature = "std", feature = "no_std_ast"))]
pub mod ast;
#[cfg(any(feature = "std", feature = "no_std_interner"))]
pub mod interner;
#[cfg(any(feature = "std", feature = "no_std_lexer"))]
pub mod lexer;
#[cfg(any(feature = "std", feature = "no_std_parser"))]
pub mod parser;
#[cfg(any(feature = "std", feature = "no_std_span"))]
pub mod span;
#[cfg(any(feature = "std", feature = "no_std_tokens"))]
pub mod tokens;
#[cfg(any(feature = "std", feature = "no_std_visitors"))]
pub mod visitors;
