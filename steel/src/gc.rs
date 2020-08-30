use std::rc::Rc;

use std::ops::Deref;

// thread_local! {
//     // pub static VOID: Rc<SteelVal> = Rc::new(SteelVal::Void);
//     // pub static TRUE: Rc<SteelVal> = Rc::new(SteelVal::BoolV(true));
//     pub static OBJECT_COUNT: Rc<SteelVal> = Rc::new(SteelVal::BoolV(false));
// }

use std::sync::atomic::{AtomicUsize, Ordering};

static OBJECT_COUNT: AtomicUsize = AtomicUsize::new(0);

// CALL_COUNT.fetch_add(1, Ordering::SeqCst);

// #[derive(Clone)]
pub struct Gc<T>(Rc<T>);

impl<T> Gc<T> {
    pub fn new(val: T) -> Gc<T> {
        OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        Gc(Rc::new(val))
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
        OBJECT_COUNT.fetch_sub(1, Ordering::SeqCst);
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc(Rc::clone(&self.0))
    }
}
