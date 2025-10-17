#![allow(dead_code)]

use alloc::borrow::Cow;
use alloc::string::String;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
    pub fn to_path_buf(&self) -> PathBuf {
        self.0.clone()
    }

    #[inline]
    pub fn exists(&self) -> bool {
        self.0.exists()
    }

    #[inline]
    pub fn is_file(&self) -> bool {
        self.0.is_file()
    }

    #[inline]
    pub fn is_dir(&self) -> bool {
        self.0.is_dir()
    }

    #[inline]
    pub fn pop(&mut self) -> bool {
        self.0.pop()
    }

    #[inline]
    pub fn set_extension(&mut self, extension: impl AsRef<std::ffi::OsStr>) -> bool {
        self.0.set_extension(extension)
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
impl Borrow<PathBuf> for OwnedPath {
    fn borrow(&self) -> &PathBuf {
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

#[cfg(feature = "std")]
impl Serialize for OwnedPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.to_string_lossy().as_ref())
    }
}

#[cfg(feature = "std")]
impl<'de> Deserialize<'de> for OwnedPath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Ok(Self(PathBuf::from(value)))
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
            Some((prefix, file)) => (Some(prefix), file),
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
                self.0.push_str(prefix);
                if !self.0.ends_with('/') {
                    self.0.push('/');
                }
            }
        }

        self.0.push_str(&new_file);
        true
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

#[cfg(not(feature = "std"))]
impl Serialize for OwnedPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0)
    }
}

#[cfg(not(feature = "std"))]
impl<'de> Deserialize<'de> for OwnedPath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Ok(Self(value))
    }
}
