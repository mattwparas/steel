#![allow(dead_code)]

use alloc::borrow::Cow;
use alloc::string::String;

#[cfg(feature = "std")]
use std::path::{Path, PathBuf};
#[cfg(feature = "std")]
use std::{borrow::Borrow, ops::Deref};

#[cfg(feature = "std")]
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct OwnedPath(PathBuf);

#[cfg(not(feature = "std"))]
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct OwnedPath(String);

#[cfg(feature = "std")]
impl OwnedPath {
    #[inline]
    pub fn new() -> Self {
        Self(PathBuf::new())
    }

    #[inline]
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    #[inline]
    pub fn push(&mut self, segment: impl AsRef<Path>) {
        self.0.push(segment);
    }

    #[inline]
    pub fn join(&self, segment: impl AsRef<Path>) -> Self {
        Self(self.0.join(segment))
    }

    #[inline]
    pub fn into_path(self) -> PathBuf {
        self.0
    }

    #[inline]
    pub fn into_string(self) -> String {
        self.0
            .into_os_string()
            .into_string()
            .unwrap_or_else(|os| os.to_string_lossy().into_owned())
    }

    #[inline]
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        self.0.to_string_lossy()
    }
}

#[cfg(feature = "std")]
impl From<PathBuf> for OwnedPath {
    fn from(value: PathBuf) -> Self {
        Self(value)
    }
}

#[cfg(feature = "std")]
impl From<OwnedPath> for PathBuf {
    fn from(value: OwnedPath) -> Self {
        value.0
    }
}

#[cfg(feature = "std")]
impl From<&Path> for OwnedPath {
    fn from(value: &Path) -> Self {
        Self(value.to_path_buf())
    }
}

#[cfg(feature = "std")]
impl AsRef<Path> for OwnedPath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

#[cfg(feature = "std")]
impl Deref for OwnedPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "std")]
impl Borrow<Path> for OwnedPath {
    fn borrow(&self) -> &Path {
        &self.0
    }
}

#[cfg(feature = "std")]
impl From<String> for OwnedPath {
    fn from(value: String) -> Self {
        Self(PathBuf::from(value))
    }
}

#[cfg(feature = "std")]
impl From<&str> for OwnedPath {
    fn from(value: &str) -> Self {
        Self(PathBuf::from(value))
    }
}

#[cfg(feature = "std")]
impl core::fmt::Debug for OwnedPath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.0, f)
    }
}

#[cfg(feature = "std")]
impl core::fmt::Display for OwnedPath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0.to_string_lossy())
    }
}

#[cfg(not(feature = "std"))]
impl OwnedPath {
    #[inline]
    pub fn new() -> Self {
        Self(String::new())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]
    pub fn push(&mut self, segment: &str) {
        if !self.0.is_empty() && !self.0.ends_with('/') {
            self.0.push('/');
        }
        self.0.push_str(segment);
    }

    #[inline]
    pub fn join(&self, segment: &str) -> Self {
        let mut next = self.clone();
        next.push(segment);
        next
    }

    #[inline]
    pub fn into_string(self) -> String {
        self.0
    }

    #[inline]
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.0)
    }
}

#[cfg(not(feature = "std"))]
impl From<String> for OwnedPath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

#[cfg(not(feature = "std"))]
impl From<&str> for OwnedPath {
    fn from(value: &str) -> Self {
        Self(value.to_owned())
    }
}

#[cfg(not(feature = "std"))]
impl core::fmt::Debug for OwnedPath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.0, f)
    }
}

#[cfg(not(feature = "std"))]
impl core::fmt::Display for OwnedPath {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}
