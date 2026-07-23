#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(all(feature = "jit2", feature = "allocator-api2"))]
compile_error!(
    "jit2 and allocator-api2 cannot be enabled together: jit2's compiled super-instructions \
     are raw `fn(&mut VmCore)` pointers with no allocator-generic representation (see \
     ALLOCATOR_SPEC.md)"
);

extern crate alloc;
extern crate im_rc;
#[macro_use]
mod env;
#[macro_use]
pub mod core;
pub mod compiler;
pub mod primitives;
pub mod time;
#[macro_use]
pub mod rerrs;
pub mod rvals;
pub mod stdlib;
#[macro_use]
pub mod gc;
mod containers;
mod conversions;

pub mod parser;
pub mod steel_vm;

#[cfg(test)]
mod tests;
pub(crate) mod values;

pub use self::{rerrs::SteelErr, rvals::SteelVal, stdlib::PRELUDE};
pub use crate::values::{HashMap, HashSet, Vector};
pub use im_lists::list::List;
pub use primitives::UnRecoverableResult;
pub use steel_derive::steel_quote;
pub use values::serde::{
    register_deserializer, register_serializer, steel_deserialize, steel_serialize,
};
pub use values::LambdaMetadataTable;
pub use values::RootToken;
pub use values::RootedSteelVal;

#[cfg(feature = "jit2")]
pub mod jit2;
