use lasso::Key;
use lasso::Spur;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use std::{fmt, sync::Arc};

// #[derive(Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
// pub enum MaybeInternedString {
//     Interned(InternedString),
//     Uninterned(String),
// }

/// An interned string
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[repr(transparent)]
pub struct InternedString(Spur);

impl InternedString {
    pub fn from_static(ident: &'static str) -> Self {
        Self(
            INTERNER
                .get_or_init(|| Arc::new(ThreadedRodeo::new()))
                .get_or_intern_static(ident),
        )
    }

    pub fn from_string(ident: String) -> Self {
        Self(
            INTERNER
                .get_or_init(|| Arc::new(ThreadedRodeo::new()))
                .get_or_intern(ident),
        )
    }

    pub fn new(key: usize) -> Self {
        Self(Spur::try_from_usize(key).unwrap())
    }

    pub fn get(self) -> Spur {
        self.0
    }

    pub fn try_get(ident: &str) -> Option<InternedString> {
        INTERNER.get().unwrap().get(ident).map(InternedString)
    }

    #[doc(hidden)]
    pub fn as_u32(self) -> u32 {
        self.get().into_usize() as u32
    }

    pub fn resolve(&self) -> &str {
        resolve(&self.0)
    }
}

impl From<&str> for InternedString {
    fn from(ident: &str) -> Self {
        Self(
            INTERNER
                .get_or_init(|| Arc::new(ThreadedRodeo::new()))
                .get_or_intern(ident),
        )
    }
}

impl From<String> for InternedString {
    fn from(ident: String) -> Self {
        Self::from_string(ident)
    }
}

impl From<Spur> for InternedString {
    fn from(spur: Spur) -> Self {
        Self(spur)
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get().into_usize())
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.resolve())
    }
}

impl From<InternedString> for SteelVal {
    fn from(value: InternedString) -> Self {
        SteelVal::StringV(value.into())
    }
}

impl From<InternedString> for SteelString {
    fn from(value: InternedString) -> Self {
        value.resolve().into()
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

// impl Dese

use lasso::ThreadedRodeo;
// use std::sync::OnceLock;

use crate::{rvals::SteelString, SteelVal};

static INTERNER: OnceCell<Arc<ThreadedRodeo>> = OnceCell::new();

pub fn take_interner() -> Arc<ThreadedRodeo> {
    // INTERNER.take().unwrap()
    Arc::clone(INTERNER.get().unwrap())
}

pub fn initialize_with(interner: Arc<ThreadedRodeo>) -> Result<(), Arc<ThreadedRodeo>> {
    INTERNER.set(interner)
}

pub fn get_interner() -> Option<&'static Arc<ThreadedRodeo>> {
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
    INTERNER.get_or_init(|| Arc::new(ThreadedRodeo::new()));
    let key = INTERNER.get().unwrap().get_or_intern_static("hello world");

    let resolved_string = INTERNER.get().unwrap().resolve(&key);

    println!("resolved string: {resolved_string:?}");
}

// fn intern(key: String) -> Spur {
//     INTERNER.get().unwrap().get_or_intern(key)
// }

// fn intern_static(key: &'static str) -> Spur {
//     INTERNER.get().unwrap().get_or_intern_static(key)
// }

fn resolve(key: &Spur) -> &str {
    INTERNER.get().unwrap().resolve(key)
}
