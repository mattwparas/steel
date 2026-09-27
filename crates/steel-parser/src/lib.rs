#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod ast;
// Only for the integration tests and the fuzz targets
#[doc(hidden)]
pub mod differential;
pub mod interner;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod tokens;
pub mod visitors;
