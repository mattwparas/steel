#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod ast;
#[doc(hidden)]
pub mod compare;
pub mod interner;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod tokens;
pub mod visitors;
