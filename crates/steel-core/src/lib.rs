#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

#[cfg(feature = "std")]
extern crate im_rc;

#[cfg(all(feature = "no_std", not(feature = "std")))]
mod getrandom_custom;
#[cfg(not(feature = "std"))]
mod minimal;
#[cfg(any(feature = "std", feature = "no_std_env"))]
#[macro_use]
mod env;
#[macro_use]
pub mod core;
#[cfg(any(feature = "std", feature = "no_std_compiler"))]
pub mod compiler;
#[cfg(any(feature = "std", feature = "no_std_primitives"))]
pub mod primitives;
#[cfg(any(feature = "std", feature = "no_std_rerrs"))]
#[macro_use]
pub mod rerrs;
#[cfg(any(feature = "std", feature = "no_std_rvals"))]
pub mod rvals;
#[cfg(any(feature = "std", feature = "no_std_stdlib"))]
pub mod stdlib;
#[cfg(any(feature = "std", feature = "no_std_gc"))]
#[macro_use]
pub mod gc;
#[cfg(any(feature = "std", feature = "no_std_containers"))]
mod containers;
#[cfg(any(feature = "std", feature = "no_std_conversions"))]
mod conversions;

// #[cfg(feature = "jit")]
// pub mod jit;
#[cfg(any(feature = "std", feature = "no_std_rvals"))]
pub mod parser;
#[cfg(any(feature = "std", feature = "no_std_steel_vm"))]
pub mod steel_vm;

#[cfg(all(any(feature = "std", feature = "no_std_values"), test))]
mod tests;

#[cfg(feature = "std")]
pub(crate) mod values;

#[cfg(any(feature = "std", feature = "no_std_stdlib"))]
pub use self::stdlib::PRELUDE;
#[cfg(all(not(feature = "std"), not(feature = "no_std_stdlib")))]
pub use minimal::PRELUDE;

#[cfg(all(not(feature = "std"), feature = "no_std_rerrs"))]
pub use minimal::SteelErr;
#[cfg(feature = "std")]
pub use rerrs::SteelErr;

#[cfg(all(not(feature = "std"), feature = "no_std_rvals"))]
pub use minimal::SteelVal;
#[cfg(feature = "std")]
pub use rvals::SteelVal;

#[cfg(feature = "std")]
pub use crate::values::{HashMap, HashSet, Vector};
#[cfg(not(feature = "std"))]
pub use minimal::{HashMap, HashSet};

#[cfg(feature = "std")]
pub use im_lists::list::List;

#[cfg(any(feature = "std", feature = "no_std_primitives"))]
pub use primitives::UnRecoverableResult;

pub use steel_derive::steel_quote;

#[cfg(any(feature = "std", feature = "no_std_values"))]
pub use values::LambdaMetadataTable;

#[cfg(any(feature = "std", feature = "no_std_values"))]
pub use values::RootToken;

#[cfg(any(feature = "std", feature = "no_std_values"))]
pub use values::RootedSteelVal;

#[cfg(all(not(feature = "std"), not(feature = "no_std_values")))]
pub use minimal::{LambdaMetadataTable, RootToken, RootedSteelVal};
