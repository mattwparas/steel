#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

#[cfg(not(feature = "std"))]
pub mod pretty;

pub mod ast;
pub mod interner;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod tokens;
pub mod visitors;
