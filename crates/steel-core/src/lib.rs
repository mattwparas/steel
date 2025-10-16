#![cfg_attr(not(feature = "std"), no_std)]

#[allow(unused_extern_crates)]
extern crate alloc; // Required for heap-backed types when std is disabled

extern crate im_rc;
pub mod collections;
pub mod os_strings;
pub mod sync;
pub mod time;
#[macro_use]
mod env;
#[macro_use]
pub mod core;
pub mod compiler;
pub mod primitives;
#[macro_use]
pub mod rerrs;
pub mod rvals;
pub mod stdlib;
#[macro_use]
pub mod gc;
mod containers;
mod conversions;

// #[cfg(feature = "jit")]
// pub mod jit;
pub mod parser;
pub mod steel_vm;

#[cfg(test)]
mod tests;
pub(crate) mod values;

pub use self::{rerrs::SteelErr, rvals::SteelVal, stdlib::PRELUDE};
pub use crate::collections::{HashMap, HashSet, Vector};
pub use im_lists::list::List;
pub use primitives::UnRecoverableResult;
pub use steel_derive::steel_quote;
pub use values::LambdaMetadataTable;
pub use values::RootToken;
pub use values::RootedSteelVal;
