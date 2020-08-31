use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

pub(crate) static OBJECT_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(PartialEq, Eq, Debug)]
pub struct Gc<T>(Rc<T>);

impl fmt::Display for Gc<SteelVal> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> Gc<T> {
    pub fn new(val: T) -> Gc<T> {
        OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        Gc(Rc::new(val))
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        Rc::get_mut(&mut self.0)
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.0, &other.0)
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

impl<T> Drop for Gc<T> {
    fn drop(&mut self) {
        if Rc::strong_count(&self.0) == 1 {
            OBJECT_COUNT.fetch_sub(1, Ordering::SeqCst);
        }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc(Rc::clone(&self.0))
    }
}
