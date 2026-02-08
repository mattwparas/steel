#![allow(dead_code)]

#[cfg(not(feature = "std"))]
use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::borrow::ToOwned;
#[cfg(not(feature = "std"))]
use alloc::string::String;

#[cfg(not(feature = "std"))]
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[cfg(feature = "std")]
pub use std::ffi::OsStr;

#[cfg(not(feature = "std"))]
pub type OsStr = str;

#[cfg(feature = "std")]
pub const MAIN_SEPARATOR_STR: &str = std::path::MAIN_SEPARATOR_STR;

#[cfg(not(feature = "std"))]
pub const MAIN_SEPARATOR_STR: &str = "/";

#[cfg(feature = "std")]
pub type PathBuf = std::path::PathBuf;

#[cfg(not(feature = "std"))]
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct PathBuf(String);

#[cfg(not(feature = "std"))]
impl PathBuf {
    #[inline]
    pub fn new() -> Self {
        Self(String::new())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]
    pub fn as_os_str(&self) -> &OsStr {
        self.0.as_str()
    }

    #[inline]
    pub fn as_path(&self) -> &str {
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
    pub fn exists(&self) -> bool {
        false
    }

    #[inline]
    pub fn is_file(&self) -> bool {
        false
    }

    #[inline]
    pub fn is_dir(&self) -> bool {
        false
    }

    #[inline]
    pub fn pop(&mut self) -> bool {
        if self.0.is_empty() {
            return false;
        }

        let trimmed_len = self.0.trim_end_matches('/').len();
        self.0.truncate(trimmed_len);

        if self.0.is_empty() {
            return false;
        }

        if let Some(idx) = self.0.rfind('/') {
            self.0.truncate(idx);
        } else {
            self.0.clear();
        }

        true
    }

    #[inline]
    pub fn set_extension(&mut self, extension: &str) -> bool {
        if self.0.is_empty() {
            return false;
        }

        let trimmed_len = self.0.trim_end_matches('/').len();
        self.0.truncate(trimmed_len);

        if self.0.is_empty() {
            return false;
        }

        let (prefix, file) = match self.0.rsplit_once('/') {
            Some((prefix, file)) => (Some(prefix.to_owned()), file),
            None => (None, self.0.as_str()),
        };

        if file.is_empty() {
            return false;
        }

        let stem = match file.rfind('.') {
            Some(idx) => &file[..idx],
            None => file,
        };

        let mut new_file = stem.to_owned();
        if !extension.is_empty() {
            if !new_file.is_empty() {
                new_file.push('.');
            }
            new_file.push_str(extension);
        }

        self.0.clear();
        if let Some(prefix) = prefix {
            if !prefix.is_empty() {
                self.0.push_str(&prefix);
                if !self.0.ends_with('/') {
                    self.0.push('/');
                }
            }
        }

        self.0.push_str(&new_file);
        true
    }

    #[inline]
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        Cow::Borrowed(&self.0)
    }

    #[inline]
    pub fn to_str(&self) -> Option<&str> {
        Some(&self.0)
    }

    #[inline]
    pub fn to_path_buf(&self) -> Self {
        self.clone()
    }
}

#[cfg(not(feature = "std"))]
impl From<String> for PathBuf {
    fn from(value: String) -> Self {
        Self(value)
    }
}

#[cfg(not(feature = "std"))]
impl From<&str> for PathBuf {
    fn from(value: &str) -> Self {
        Self(value.to_owned())
    }
}

#[cfg(not(feature = "std"))]
impl From<&PathBuf> for PathBuf {
    fn from(value: &PathBuf) -> Self {
        value.clone()
    }
}

#[cfg(not(feature = "std"))]
impl core::fmt::Debug for PathBuf {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.0, f)
    }
}

#[cfg(not(feature = "std"))]
impl core::fmt::Display for PathBuf {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(not(feature = "std"))]
impl Serialize for PathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0)
    }
}

#[cfg(not(feature = "std"))]
impl<'de> Deserialize<'de> for PathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Ok(Self(value))
    }
}

#[cfg(not(feature = "std"))]
impl From<PathBuf> for String {
    fn from(value: PathBuf) -> Self {
        value.0
    }
}

#[cfg(not(feature = "std"))]
impl From<&PathBuf> for String {
    fn from(value: &PathBuf) -> Self {
        value.0.clone()
    }
}

#[cfg(not(feature = "std"))]
#[cfg(not(feature = "std"))]
impl core::str::FromStr for PathBuf {
    type Err = core::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s))
    }
}

pub type OwnedPath = PathBuf;
