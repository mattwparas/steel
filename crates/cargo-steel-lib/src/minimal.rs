use alloc::{boxed::Box, string::String, vec::Vec};
use core::fmt::{self, Debug, Display};

/// Minimal error trait used when `std` is unavailable.
pub trait Error: Debug + Display {}
impl<T: Debug + Display> Error for T {}

/// Placeholder path buffer type for `no_std` builds.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PathBuf;

/// `no_std` build does not support resolving a Steel home directory.
pub fn steel_home() -> Option<PathBuf> {
    None
}

#[derive(Debug)]
struct UnsupportedError;

impl Display for UnsupportedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("cargo-steel-lib requires the `std` feature enabled")
    }
}

/// Stubbed implementation that always reports lack of support in `no_std` mode.
pub fn run(_args: Vec<String>, _env_vars: Vec<(String, String)>) -> Result<bool, Box<dyn Error>> {
    Err(Box::new(UnsupportedError))
}
