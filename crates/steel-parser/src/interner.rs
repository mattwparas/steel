#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
use alloc::string::String;
#[cfg(feature = "std")]
use alloc::sync::Arc;
use compact_str::CompactString;
#[cfg(not(feature = "std"))]
use core::cell::UnsafeCell;
use core::fmt;
use lasso::Key;
use lasso::Spur;
#[cfg(not(feature = "std"))]
use once_cell::race::OnceBox;
#[cfg(feature = "std")]
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};

// TODO: Serialize and Deserialize should resolve() -> Otherwise we're in for deep trouble
// trying to serialize and deserialize this
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct InternedString(Spur);

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
        let spur = {
            #[cfg(feature = "std")]
            {
                interner().get_or_intern_static(ident)
            }

            #[cfg(not(feature = "std"))]
            {
                interner().with_mut(|interner| interner.get_or_intern_static(ident))
            }
        };

        Self(spur)
    }

    pub fn from_string(ident: String) -> Self {
        let spur = {
            #[cfg(feature = "std")]
            {
                interner().get_or_intern(ident)
            }

            #[cfg(not(feature = "std"))]
            {
                interner().with_mut(|interner| interner.get_or_intern(ident))
            }
        };

        Self(spur)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(ident: &str) -> Self {
        let spur = {
            #[cfg(feature = "std")]
            {
                interner().get_or_intern(ident)
            }

            #[cfg(not(feature = "std"))]
            {
                interner().with_mut(|interner| interner.get_or_intern(ident))
            }
        };

        Self(spur)
    }

    pub fn new(key: usize) -> Self {
        Self(Spur::try_from_usize(key).unwrap())
    }

    pub fn get(self) -> Spur {
        self.0
    }

    pub fn try_get(ident: &str) -> Option<InternedString> {
        #[cfg(feature = "std")]
        {
            return INTERNER
                .get()
                .and_then(|interner| interner.get(ident).map(InternedString));
        }

        #[cfg(not(feature = "std"))]
        {
            INTERNER.get().and_then(|interner| {
                interner.with(|inner| inner.get(ident).map(InternedString))
            })
        }
    }

    #[doc(hidden)]
    pub fn as_u32(self) -> u32 {
        self.get().into_usize() as u32
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

// impl Serialize for InternedString {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         serializer.serialize_str(self.resolve())
//     }
// }

#[cfg(not(feature = "std"))]
use lasso::Rodeo;
#[cfg(feature = "std")]
use lasso::ThreadedRodeo;

#[cfg(feature = "std")]
type GlobalInterner = ThreadedRodeo<Spur>;
#[cfg(not(feature = "std"))]
type GlobalInterner = Rodeo<Spur>;

#[cfg(feature = "std")]
type InternerHandle = Arc<GlobalInterner>;
#[cfg(not(feature = "std"))]
type InternerHandle = &'static StaticInterner;

#[cfg(feature = "std")]
static INTERNER: OnceCell<InternerHandle> = OnceCell::new();
#[cfg(not(feature = "std"))]
static INTERNER: OnceBox<StaticInterner> = OnceBox::new();

#[cfg(feature = "std")]
fn interner() -> &'static InternerHandle {
    INTERNER.get_or_init(|| Arc::new(GlobalInterner::new()))
}

#[cfg(not(feature = "std"))]
fn interner() -> InternerHandle {
    INTERNER.get_or_init(|| Box::new(StaticInterner::new()))
}

#[cfg(not(feature = "std"))]
pub struct StaticInterner(UnsafeCell<GlobalInterner>);

#[cfg(not(feature = "std"))]
impl StaticInterner {
    fn new() -> Self {
        Self(UnsafeCell::new(GlobalInterner::new()))
    }

    fn with<'a, R>(&'a self, f: impl FnOnce(&'a GlobalInterner) -> R) -> R {
        let interner = unsafe { &*self.0.get() };
        f(interner)
    }

    fn with_mut<'a, R>(&'a self, f: impl FnOnce(&'a mut GlobalInterner) -> R) -> R {
        let interner = unsafe { &mut *self.0.get() };
        f(interner)
    }
}

#[cfg(not(feature = "std"))]
unsafe impl Sync for StaticInterner {}

pub fn interned_current_memory_usage() -> usize {
    #[cfg(feature = "std")]
    {
        return interner().current_memory_usage();
    }

    #[cfg(not(feature = "std"))]
    {
        interner().with(|interner| interner.current_memory_usage())
    }
}

#[cfg(feature = "std")]
pub fn take_interner() -> InternerHandle {
    interner().clone()
}

#[cfg(not(feature = "std"))]
pub fn take_interner() -> InternerHandle {
    interner()
}

#[cfg(feature = "std")]
pub fn initialize_with(interner: InternerHandle) -> Result<(), InternerHandle> {
    INTERNER.set(interner)
}

#[cfg(not(feature = "std"))]
pub fn initialize_with(interner: InternerHandle) -> Result<(), InternerHandle> {
    if INTERNER.get().is_some() {
        Err(interner)
    } else {
        INTERNER
            .set(Box::new(StaticInterner::new()))
            .map_err(|_| interner)?;

        add_interner(interner);
        Ok(())
    }
}

#[cfg(feature = "std")]
pub fn get_interner() -> Option<InternerHandle> {
    INTERNER.get().cloned()
}

#[cfg(not(feature = "std"))]
pub fn get_interner() -> Option<InternerHandle> {
    INTERNER.get().map(|interner| interner as InternerHandle)
}

#[cfg(feature = "std")]
pub fn add_interner(interner: InternerHandle) {
    let guard = take_interner();

    for key in interner.strings() {
        guard.get_or_intern(key);
    }
}

#[cfg(not(feature = "std"))]
pub fn add_interner(interner: InternerHandle) {
    let destination = take_interner();

    interner.with(|source| {
        destination.with_mut(|target| {
            for key in source.strings() {
                target.get_or_intern(key);
            }
        })
    });
}

#[test]
fn test_initialization() {
    let interner = take_interner();
    let key = interner.get_or_intern_static("hello world");

    let resolved_string = interner.resolve(&key);

    println!("resolved string: {resolved_string:?}");
}

fn resolve(key: &Spur) -> &str {
    #[cfg(feature = "std")]
    {
        return interner().resolve(key);
    }

    #[cfg(not(feature = "std"))]
    {
        interner().with(|interner| interner.resolve(key))
    }
}
