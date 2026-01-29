#![allow(dead_code)]

use alloc::borrow::Cow;
#[cfg(not(feature = "std"))]
use alloc::borrow::ToOwned;
use alloc::string::String;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[cfg(feature = "std")]
pub use std::ffi::OsStr;
#[cfg(feature = "std")]
pub const MAIN_SEPARATOR_STR: &str = std::path::MAIN_SEPARATOR_STR;
#[cfg(feature = "std")]
use std::{borrow::Borrow, ops::Deref};

#[cfg(feature = "std")]
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct PathBuf(std::path::PathBuf);

#[cfg(not(feature = "std"))]
pub type OsStr = str;

#[cfg(not(feature = "std"))]
#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct PathBuf(String);

#[cfg(feature = "std")]
impl PathBuf {
    #[inline]
    pub fn into_parser_path(self) -> std::path::PathBuf {
        self.into()
    }

    #[inline]
    pub fn to_parser_path(&self) -> std::path::PathBuf {
        self.clone().into()
    }

    #[inline]
    pub fn new() -> Self {
        Self(std::path::PathBuf::new())
    }

    #[inline]
    pub fn as_path(&self) -> &std::path::Path {
        &self.0
    }

    #[inline]
    pub fn push(&mut self, segment: impl AsRef<std::path::Path>) {
        self.0.push(segment);
    }

    #[inline]
    pub fn join(&self, segment: impl AsRef<std::path::Path>) -> Self {
        Self(self.0.join(segment))
    }

    #[inline]
    pub fn to_path_buf(&self) -> std::path::PathBuf {
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
    pub fn set_extension(&mut self, extension: impl AsRef<OsStr>) -> bool {
        self.0.set_extension(extension)
    }

    #[inline]
    pub fn into_path(self) -> std::path::PathBuf {
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
impl From<std::path::PathBuf> for PathBuf {
    fn from(value: std::path::PathBuf) -> Self {
        Self(value)
    }
}

#[cfg(feature = "std")]
impl From<&std::path::PathBuf> for PathBuf {
    fn from(value: &std::path::PathBuf) -> Self {
        Self(value.clone())
    }
}

#[cfg(feature = "std")]
impl From<&PathBuf> for PathBuf {
    fn from(value: &PathBuf) -> Self {
        value.clone()
    }
}

#[cfg(feature = "std")]
impl From<PathBuf> for std::path::PathBuf {
    fn from(value: PathBuf) -> Self {
        value.0
    }
}

#[cfg(feature = "std")]
impl From<&std::path::Path> for PathBuf {
    fn from(value: &std::path::Path) -> Self {
        Self(value.to_path_buf())
    }
}

#[cfg(feature = "std")]
impl From<String> for PathBuf {
    fn from(value: String) -> Self {
        Self(std::path::PathBuf::from(value))
    }
}

#[cfg(feature = "std")]
impl From<&str> for PathBuf {
    fn from(value: &str) -> Self {
        Self(std::path::PathBuf::from(value))
    }
}

#[cfg(feature = "std")]
impl AsRef<std::path::Path> for PathBuf {
    fn as_ref(&self) -> &std::path::Path {
        &self.0
    }
}

#[cfg(feature = "std")]
impl Deref for PathBuf {
    type Target = std::path::Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "std")]
impl Borrow<std::path::Path> for PathBuf {
    fn borrow(&self) -> &std::path::Path {
        &self.0
    }
}

#[cfg(feature = "std")]
impl Borrow<std::path::PathBuf> for PathBuf {
    fn borrow(&self) -> &std::path::PathBuf {
        &self.0
    }
}

#[cfg(feature = "std")]
impl<'a> core::iter::FromIterator<std::path::Component<'a>> for PathBuf {
    fn from_iter<I: IntoIterator<Item = std::path::Component<'a>>>(iter: I) -> Self {
        let mut path = std::path::PathBuf::new();
        for component in iter {
            path.push(component.as_os_str());
        }
        Self(path)
    }
}

#[cfg(feature = "std")]
impl core::fmt::Debug for PathBuf {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(&self.0, f)
    }
}

#[cfg(feature = "std")]
impl core::fmt::Display for PathBuf {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0.to_string_lossy())
    }
}

#[cfg(feature = "std")]
impl Serialize for PathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.to_string_lossy().as_ref())
    }
}

#[cfg(feature = "std")]
impl<'de> Deserialize<'de> for PathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Ok(Self(std::path::PathBuf::from(value)))
    }
}

#[cfg(not(feature = "std"))]
impl PathBuf {
    #[inline]
    pub fn into_parser_path(self) -> String {
        self.0
    }

    #[inline]
    pub fn to_parser_path(&self) -> String {
        self.0.clone()
    }

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
    pub fn into_string(self) -> String {
        self.0
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

impl core::str::FromStr for PathBuf {
    type Err = core::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s))
    }
}

pub type OwnedPath = PathBuf;
