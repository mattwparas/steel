use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::stop;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub(crate) static OBJECT_COUNT: AtomicUsize = AtomicUsize::new(0);

/// This is simply a newtype around the `Rc` type
/// When enabled, this allows for complete sandboxing of data types
/// It does not expose the full functionality of the `Rc` type
/// but it does allow for some
#[derive(PartialEq, Eq, Debug, Hash)]
pub struct Gc<T: Clone>(Rc<T>);

impl fmt::Display for Gc<SteelVal> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Clone> Gc<T> {
    // in order to fully sandbox, I have to check the memory limit
    pub fn new(val: T) -> Gc<T> {
        OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        Gc(Rc::new(val))
    }

    pub fn try_new(val: T) -> Result<Gc<T>, SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        if mem > crate::vm::MAXIMUM_OBJECTS {
            stop!(Generic => "ran out of memory!")
        }
        Ok(Gc(Rc::new(val)))
    }

    pub fn checked_allocate(allocations: usize) -> Result<(), SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(0, Ordering::SeqCst);
        if mem + allocations > crate::vm::MAXIMUM_OBJECTS {
            stop!(Generic => "allocation would exceed maximum allowed memory")
        }
        Ok(())
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        Rc::get_mut(&mut self.0)
    }

    pub fn make_mut(&mut self) -> &mut T {
        Rc::make_mut(&mut self.0)
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.0, &other.0)
    }

    /// Deep clone the object to remove it from the GC
    pub fn unwrap(&self) -> T {
        (*self.0).clone()
    }

    // this does not match the original semantics of Rc::try_unwrap
    // in order to match this, we would need some unsafe rust
    // instead, I take a _slight_ performance hit in order to
    // match the original functionality, and the specific use case
    // for me, which is unwinding lists in the drop for SteelVal
    pub fn try_unwrap(this: Self) -> Result<T, SteelErr> {
        let inner = Rc::clone(&this.0);
        drop(this);
        Rc::try_unwrap(inner)
            .map_err(|_| SteelErr::Generic("value still has reference".to_string(), None))
            .and_then(|x| {
                OBJECT_COUNT.fetch_sub(1, Ordering::SeqCst);
                Ok(x)
            })
    }

    pub fn check_memory() -> Result<usize, SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(0, Ordering::SeqCst);
        if mem > crate::vm::MAXIMUM_OBJECTS {
            stop!(Generic => "ran out of memory!")
        }
        Ok(mem)
    }
}

impl<T: Clone> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T: Clone> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<T: Clone> Drop for Gc<T> {
    fn drop(&mut self) {
        if Rc::strong_count(&self.0) == 1 {
            OBJECT_COUNT.fetch_sub(1, Ordering::SeqCst);
        }
    }
}

impl<T: Clone> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc(Rc::clone(&self.0))
    }
}
