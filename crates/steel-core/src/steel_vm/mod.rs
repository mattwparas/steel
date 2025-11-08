pub mod builtin;
pub mod cache;
pub(crate) mod const_evaluation;
pub mod contract_checker;
#[cfg(feature = "dylibs")]
pub mod dylib;
pub mod engine;
#[cfg(feature = "dylibs")]
pub mod ffi;
mod lazy_stream;
mod meta;
pub mod primitives;

#[cfg(feature = "op-code-profiling")]
mod profiling;

pub mod register_fn;
#[cfg(all(test, feature = "std"))]
mod test_util;
#[cfg(all(test, feature = "std"))]
mod tests;
pub(crate) mod transducers;
pub(crate) mod vm;

pub use vm::ThreadStateController;
#[cfg(feature = "sync")]
pub use vm::{mutex_lock, mutex_unlock};
