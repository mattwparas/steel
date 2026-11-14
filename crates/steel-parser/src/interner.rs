use alloc::string::String;
use compact_str::CompactString;
use core::fmt;
use serde::{Deserialize, Serialize};

#[cfg(feature = "std")]
pub use once_cell::sync::Lazy;
#[cfg(not(feature = "std"))]
pub use spin::Lazy;

#[cfg(feature = "std")]
pub use with_std::*;

// TODO: Serialize and Deserialize should resolve() -> Otherwise we're in for deep trouble
// trying to serialize and deserialize this
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct InternedString(Key);

impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.resolve().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for InternedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let key = <&str>::deserialize(deserializer)?;

        Ok(InternedString::from(key))
    }
}

impl InternedString {
    pub fn from_static(ident: &'static str) -> Self {
        Self(intern_static(ident))
    }

    pub fn from_string(ident: String) -> Self {
        Self(intern(&ident))
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(ident: &str) -> Self {
        Self(intern(ident))
    }

    pub fn new(key: usize) -> Self {
        Self(key_from_usize(key))
    }

    pub fn try_get(ident: &str) -> Option<InternedString> {
        lookup(ident).map(InternedString)
    }

    #[doc(hidden)]
    pub fn as_u32(self) -> u32 {
        key_into_usize(self.0) as u32
    }

    pub fn resolve(&self) -> &str {
        resolve(&self.0)
    }
}

impl From<CompactString> for InternedString {
    fn from(value: CompactString) -> Self {
        Self::from_str(&value)
    }
}

impl From<&str> for InternedString {
    fn from(ident: &str) -> Self {
        Self::from_str(ident)
    }
}

impl From<String> for InternedString {
    fn from(ident: String) -> Self {
        Self::from_string(ident)
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.resolve())
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.resolve())
    }
}

// impl Serialize for InternedString {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         serializer.serialize_str(self.resolve())
//     }
// }

#[cfg(feature = "std")]
use with_std::{intern, intern_static, key_from_usize, key_into_usize, lookup, resolve, Key};
#[cfg(not(feature = "std"))]
use without_std::{intern, intern_static, key_from_usize, key_into_usize, lookup, resolve, Key};

#[cfg(feature = "std")]
mod with_std {
    use super::InternedString;
    use alloc::sync::Arc;
    use lasso::{Key as _, Spur, ThreadedRodeo};
    use once_cell::sync::OnceCell;
    use rustc_hash::FxBuildHasher;

    pub(super) type Key = Spur;

    static INTERNER: OnceCell<Arc<ThreadedRodeo<Spur, FxBuildHasher>>> = OnceCell::new();

    fn interner() -> &'static Arc<ThreadedRodeo<Spur, FxBuildHasher>> {
        INTERNER.get_or_init(|| Arc::new(ThreadedRodeo::with_hasher(FxBuildHasher)))
    }

    pub(super) fn intern(ident: &str) -> Spur {
        interner().get_or_intern(ident)
    }

    pub(super) fn intern_static(ident: &'static str) -> Spur {
        interner().get_or_intern_static(ident)
    }

    pub(super) fn lookup(ident: &str) -> Option<Spur> {
        INTERNER.get().unwrap().get(ident)
    }

    pub(super) fn resolve(key: &Spur) -> &str {
        INTERNER.get().unwrap().resolve(key)
    }

    pub(super) fn key_from_usize(key: usize) -> Spur {
        Spur::try_from_usize(key).unwrap()
    }

    pub(super) fn key_into_usize(key: Spur) -> usize {
        key.into_usize()
    }

    impl InternedString {
        pub fn get(self) -> Spur {
            self.0
        }
    }

    impl From<Spur> for InternedString {
        fn from(spur: Spur) -> Self {
            Self(spur)
        }
    }

    pub fn interned_current_memory_usage() -> usize {
        INTERNER.get().unwrap().current_memory_usage()
    }

    pub fn take_interner() -> Arc<ThreadedRodeo<Spur, FxBuildHasher>> {
        Arc::clone(INTERNER.get().unwrap())
    }

    pub fn initialize_with(
        interner: Arc<ThreadedRodeo<Spur, FxBuildHasher>>,
    ) -> Result<(), Arc<ThreadedRodeo<Spur, FxBuildHasher>>> {
        INTERNER.set(interner)
    }

    pub fn get_interner() -> Option<&'static Arc<ThreadedRodeo<Spur, FxBuildHasher>>> {
        INTERNER.get()
    }

    pub fn add_interner(interner: Arc<ThreadedRodeo>) {
        let guard = INTERNER.get().unwrap();

        for key in interner.strings() {
            guard.get_or_intern(key);
        }
    }

    #[test]
    fn test_initialization() {
        let key = interner().get_or_intern_static("hello world");

        let resolved_string = INTERNER.get().unwrap().resolve(&key);

        println!("resolved string: {resolved_string:?}");
    }
}

#[cfg(not(feature = "std"))]
mod without_std {
    use alloc::{boxed::Box, vec::Vec};
    use core::num::NonZeroU32;
    use hashbrown::HashMap;
    use rustc_hash::FxBuildHasher;
    use spin::Mutex;

    pub(super) type Key = NonZeroU32;

    struct Interner {
        keys: HashMap<&'static str, Key, FxBuildHasher>,
        strings: Vec<&'static str>,
    }

    static INTERNER: Mutex<Interner> = Mutex::new(Interner {
        keys: HashMap::with_hasher(FxBuildHasher),
        strings: Vec::new(),
    });

    // Strings are leaked, since the interner is never dropped. This lets
    // `resolve` return a `&'static str` without holding the lock.
    fn intern_with(ident: &str, keep: impl FnOnce() -> &'static str) -> Key {
        let mut interner = INTERNER.lock();
        if let Some(key) = interner.keys.get(ident) {
            return *key;
        }

        let ident = keep();
        interner.strings.push(ident);
        let key = key_from_usize(interner.strings.len() - 1);
        interner.keys.insert(ident, key);
        key
    }

    pub(super) fn intern(ident: &str) -> Key {
        intern_with(ident, || Box::leak(Box::from(ident)))
    }

    pub(super) fn intern_static(ident: &'static str) -> Key {
        intern_with(ident, || ident)
    }

    pub(super) fn lookup(ident: &str) -> Option<Key> {
        INTERNER.lock().keys.get(ident).copied()
    }

    pub(super) fn resolve(key: &Key) -> &'static str {
        INTERNER.lock().strings[key_into_usize(*key)]
    }

    pub(super) fn key_from_usize(key: usize) -> Key {
        u32::try_from(key)
            .ok()
            .and_then(|key| NonZeroU32::new(key.wrapping_add(1)))
            .unwrap()
    }

    pub(super) fn key_into_usize(key: Key) -> usize {
        key.get() as usize - 1
    }
}
