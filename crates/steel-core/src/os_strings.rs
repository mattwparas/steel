#![allow(dead_code)]

#[cfg(feature = "std")]
pub use std::ffi::{OsStr, OsString};

#[cfg(not(feature = "std"))]
mod no_std {
    use alloc::borrow::Borrow;
    use alloc::string::String;
    use core::ops::Deref;

    pub type OsStr = str;

    #[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct OsString(pub String);

    impl OsString {
        #[inline]
        pub fn new() -> Self {
            Self(String::new())
        }

        #[inline]
        pub fn into_string(self) -> String {
            self.0
        }

        #[inline]
        pub fn as_str(&self) -> &str {
            &self.0
        }
    }

    impl From<String> for OsString {
        #[inline]
        fn from(value: String) -> Self {
            Self(value)
        }
    }

    impl From<&str> for OsString {
        #[inline]
        fn from(value: &str) -> Self {
            Self(value.to_owned())
        }
    }

    impl From<OsString> for String {
        #[inline]
        fn from(value: OsString) -> Self {
            value.0
        }
    }

    impl Deref for OsString {
        type Target = str;

        #[inline]
        fn deref(&self) -> &Self::Target {
            self.as_str()
        }
    }

    impl AsRef<OsStr> for OsString {
        #[inline]
        fn as_ref(&self) -> &OsStr {
            self.as_str()
        }
    }

    impl AsRef<OsStr> for String {
        #[inline]
        fn as_ref(&self) -> &OsStr {
            self.as_str()
        }
    }

    impl AsRef<OsStr> for str {
        #[inline]
        fn as_ref(&self) -> &OsStr {
            self
        }
    }

    impl Borrow<OsStr> for OsString {
        #[inline]
        fn borrow(&self) -> &OsStr {
            self.as_str()
        }
    }
}

#[cfg(not(feature = "std"))]
pub use no_std::{OsStr, OsString};
