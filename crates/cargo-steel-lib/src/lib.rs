#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(feature = "std")]
mod std_impl;
#[cfg(feature = "std")]
pub use std_impl::*;

#[cfg(not(feature = "std"))]
mod minimal;
#[cfg(not(feature = "std"))]
pub use minimal::*;
