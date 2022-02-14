use lasso::{Key, Rodeo};

use core::{
    cell::RefCell,
    fmt::{Debug, Display},
};
use lasso::{Capacity, Spur};
use std::rc::Rc;

use std::fmt;

/// An interned string
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct InternedString(Spur);

impl InternedString {
    pub fn new(key: usize) -> Self {
        Self(Spur::try_from_usize(key).unwrap())
    }

    pub fn get(self) -> Spur {
        self.0
    }

    #[doc(hidden)]
    pub fn as_u32(self) -> u32 {
        self.get().into_usize() as u32
    }

    pub fn resolve(&self) -> &str {
        resolve(&self.0)
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

use lasso::ThreadedRodeo;
use once_cell::sync::OnceCell;

static INTERNER: OnceCell<ThreadedRodeo> = OnceCell::new();

#[test]
fn test_the_initializaiton() {
    INTERNER.get_or_init(|| ThreadedRodeo::new());
    let key = INTERNER.get().unwrap().get_or_intern_static("hello world");

    let resolved_string = INTERNER.get().unwrap().resolve(&key);

    println!("resolved string: {:?}", resolved_string);
}

pub fn intern(key: String) -> Spur {
    INTERNER.get().unwrap().get_or_intern(key)
}

pub fn intern_static(key: &'static str) -> Spur {
    INTERNER.get().unwrap().get_or_intern_static(key)
}

pub fn resolve<'a>(key: &'a Spur) -> &'a str {
    INTERNER.get().unwrap().resolve(&key)
}
