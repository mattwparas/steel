use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::stop;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{ffi::OsStr, fmt};
use std::{ops::Deref, rc::Weak};

pub static OBJECT_COUNT: AtomicUsize = AtomicUsize::new(0);
pub(crate) static MAXIMUM_OBJECTS: usize = 50000;

// TODO: Consider triomphe for a drop in replacement of Arc

/// Used for automatic detection of ref cycle
pub enum MaybeWeak<T: Clone> {
    StrongRef(Gc<T>),
    WeakRef(Gc<T>),
}

/// This is simply a newtype around the `Rc` type
/// When enabled, this allows for complete sandboxing of data types
/// It does not expose the full functionality of the `Rc` type
/// but it does allow for some
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Gc<T: ?Sized>(Rc<T>);

/// Newtype around the `Weak` type.
/// Enables the detection of reference cycles in mutable memory locations
pub struct WeakGc<T>(Weak<T>);

impl fmt::Display for Gc<SteelVal> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Gc<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn get_object_count() -> usize {
    OBJECT_COUNT.fetch_add(0, Ordering::SeqCst)
}

impl<T: Clone> Gc<T> {
    /// Deep clone the object to remove it from the GC
    pub fn unwrap(&self) -> T {
        (*self.0).clone()
    }

    pub fn make_mut(&mut self) -> &mut T {
        Rc::make_mut(&mut self.0)
    }
}

impl<T> Gc<T> {
    // in order to fully sandbox, I have to check the memory limit
    pub fn new(val: T) -> Gc<T> {
        // OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        Gc(Rc::new(val))
    }

    pub fn try_new(val: T) -> Result<Gc<T>, SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        if mem > MAXIMUM_OBJECTS {
            stop!(Generic => "ran out of memory!")
        }
        Ok(Gc(Rc::new(val)))
    }

    pub fn checked_allocate(allocations: usize) -> Result<(), SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(0, Ordering::SeqCst);
        if mem + allocations > MAXIMUM_OBJECTS {
            stop!(Generic => "allocation would exceed maximum allowed memory")
        }
        Ok(())
    }

    pub fn downgrade(this: &Self) -> Weak<T> {
        Rc::downgrade(&this.0)
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        Rc::get_mut(&mut self.0)
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.0, &other.0)
    }

    pub fn as_ptr(&self) -> *const T {
        Rc::as_ptr(&self.0)
    }

    pub fn try_unwrap_(self) -> Result<T, Gc<T>> {
        Rc::try_unwrap(self.0).map_err(|x| Gc(x))
    }

    // this does not match the original semantics of Rc::try_unwrap
    // in order to match this, we would need some unsafe rust
    // instead, I take a _slight_ performance hit in order to
    // match the original functionality, and the specific use case
    // for me, which is unwinding lists in the drop for SteelVal
    // pub fn try_unwrap(this: Self) -> Result<T, SteelErr> {
    //     let inner = Rc::clone(&this.0);
    //     drop(this);
    //     Rc::try_unwrap(inner)
    //         .map_err(|_| SteelErr::new(ErrorKind::Generic, "value still has reference".to_string()))
    //     // .map(|x| {
    //     //     OBJECT_COUNT.fetch_sub(1, Ordering::SeqCst);
    //     //     x
    //     // })
    // }

    pub fn check_memory() -> Result<usize, SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(0, Ordering::SeqCst);
        if mem > MAXIMUM_OBJECTS {
            stop!(Generic => "ran out of memory!")
        }
        Ok(mem)
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}

// impl<T> Drop for Gc<T> {
//     fn drop(&mut self) {
//         // println!("Strong count: {}", Rc::strong_count(&self.0));

//         // if Rc::strong_count(&self.0) == 1 {
//         //     OBJECT_COUNT.fetch_sub(1, Ordering::SeqCst);
//         // }
//     }
// }

impl<T> Clone for Gc<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Gc(Rc::clone(&self.0))
    }
}

impl AsRef<OsStr> for Gc<String> {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref().as_ref()
    }
}

impl From<&str> for Gc<String> {
    fn from(val: &str) -> Self {
        Gc::new(val.to_string())
    }
}

impl From<String> for Gc<String> {
    fn from(val: String) -> Self {
        Gc::new(val)
    }
}

impl From<&String> for Gc<String> {
    fn from(val: &String) -> Self {
        Gc::new(val.clone())
    }
}

impl AsRef<str> for Gc<String> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[cfg(feature = "unsafe-internals")]
mod unsafe_roots {

    use crate::SteelVal;

    use super::Gc;
    use std::ptr::NonNull;

    pub struct Rooted<T> {
        value: NonNull<T>,
    }

    impl<T> Rooted<T> {
        fn from_rc(value: &Gc<T>) -> Self {
            Rooted {
                value: NonNull::new(Gc::as_ptr(value) as _).expect("Given pointer was null!"),
            }
        }
    }

    #[test]
    fn test_rooting() {
        let root = Gc::new(SteelVal::ListV(im_lists::list![]));

        let rooted_reference = Rooted::from_rc(&root);

        println!("{:?}", unsafe { rooted_reference.value.as_ref() });
    }
}
