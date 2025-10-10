#![allow(dead_code)]

use alloc::string::String;
pub use hashbrown::{HashMap, HashSet};
pub type DefaultHashBuilder = hashbrown::hash_map::DefaultHashBuilder;
pub type HashMapConsumingIter<K, V> = hashbrown::hash_map::IntoIter<K, V>;
pub type HashSetConsumingIter<T> = hashbrown::hash_set::IntoIter<T>;
pub type List<T> = alloc::vec::Vec<T>;

/// Basic error type available when `std` is not enabled.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SteelErr {
    message: &'static str,
}

impl SteelErr {
    pub const fn new(message: &'static str) -> Self {
        Self { message }
    }

    /// Returns a static message describing the error.
    pub const fn message(&self) -> &'static str {
        self.message
    }
}

/// Minimal value type available in `no_std` builds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SteelVal {
    Unsupported,
}

pub const PRELUDE: &str = "";

#[derive(Default)]
pub struct LambdaMetadataTable;

#[derive(Default)]
pub struct RootToken;

pub type RootedSteelVal = SteelVal;

/// Helper that mirrors the `SteelErr::new` naming in std builds for missing APIs.
pub fn unsupported_feature(feature: &'static str) -> SteelErr {
    SteelErr::new(feature)
}

/// Lightweight formatter used by consumers that only need textual errors.
pub fn format_error(err: &SteelErr) -> String {
    String::from(err.message())
}
