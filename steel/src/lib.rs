extern crate im_rc;

// #[macro_use]
// extern crate lazy_static;

#[macro_use]
pub mod env;
// pub mod evaluator;
#[macro_use]
// pub mod interpreter;
pub mod parser;
pub mod primitives;
// #[macro_use]
// pub mod repl;
#[macro_use]
pub mod rerrs;
pub mod rvals;
pub mod stdlib;
#[macro_use]
pub mod expander;
// #[macro_use]
// pub mod compiler;
pub mod engine;
pub mod gc;
pub mod json_vals;
pub mod lazy_stream;
pub mod port;
pub mod structs;
pub mod vm;

#[cfg(test)]
mod test_util;

pub use self::{
    gc::Gc,
    rerrs::SteelErr,
    rvals::{CustomType, SteelVal, StructFunctions},
    stdlib::PRELUDE,
};
