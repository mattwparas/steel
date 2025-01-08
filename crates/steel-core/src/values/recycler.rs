use std::cell::RefCell;
use std::default::Default;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicUsize, Ordering};

use smallvec::SmallVec;

use crate::SteelVal;

pub trait Recyclable {
    fn put(self);
    fn get() -> Self;
    fn get_with_capacity(capacity: usize) -> Self;
}

pub struct Recycle<T: Recyclable + Default> {
    t: T,
}

impl<T: Recyclable + Default> Recycle<T> {
    pub fn new() -> Self {
        Recycle { t: T::get() }
    }

    pub fn new_with_capacity(capacity: usize) -> Self {
        Recycle {
            t: T::get_with_capacity(capacity),
        }
    }
}

impl<T: Recyclable + Default> Drop for Recycle<T> {
    fn drop(&mut self) {
        T::put(std::mem::take(&mut self.t))
    }
}

impl<T: Recyclable + Default> Deref for Recycle<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.t
    }
}

impl<T: Recyclable + Default + 'static> DerefMut for Recycle<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.t
    }
}

impl<T: Recyclable + Clone + Default> Clone for Recycle<T> {
    fn clone(&self) -> Self {
        Recycle { t: self.t.clone() }
    }
}

impl<T: Recyclable + std::fmt::Debug + Default> std::fmt::Debug for Recycle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Recycle").field("t", &self.t).finish()
    }
}

impl<T: Recyclable + std::hash::Hash + Default> std::hash::Hash for Recycle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.t.hash(state);
    }
}

static RECYCLE_LIMIT: AtomicUsize = AtomicUsize::new(128);

fn set_recycle_limit(value: usize) {
    RECYCLE_LIMIT.store(value, Ordering::Relaxed);
}

macro_rules! impl_recyclable {
    ($tl:ident, $t:ty) => {
        impl_recyclable!($tl, $t, Default::default(), Self::with_capacity);
    };
    ($tl:ident, $t:ty, $constructor:expr, $constructor_capacity:expr) => {
        thread_local! {
            static $tl: RefCell<Vec<$t>> = RefCell::new(Vec::new())
        }

        impl Recyclable for $t {
            #[cfg(feature = "recycle")]
            fn put(mut self) {
                let _ = $tl.try_with(|p| {
                    let p = p.try_borrow_mut();

                    if let Ok(mut p) = p {
                        // This _should_ be cleared, but it seems it is not!
                        // debug_assert!(self.is_empty());
                        self.clear();
                        if p.len() < RECYCLE_LIMIT.load(Ordering::Relaxed) {
                            p.push(self);
                        }
                    }
                });
            }

            #[cfg(not(feature = "recycle"))]
            fn put(self) {}

            fn get() -> Self {
                #[cfg(feature = "recycle")]
                {
                    $tl.with(|p| {
                        let mut p = p.borrow_mut();
                        p.pop()
                    })
                    .unwrap_or($constructor)
                }

                #[cfg(not(feature = "recycle"))]
                {
                    Self::new()
                }
            }

            fn get_with_capacity(capacity: usize) -> Self {
                #[cfg(feature = "recycle")]
                {
                    $tl.with(|p| {
                        let mut p = p.borrow_mut();
                        p.pop()
                    })
                    .unwrap_or(($constructor_capacity)(capacity))
                }

                #[cfg(not(feature = "recycle"))]
                {
                    Self::with_capacity(capacity)
                }
            }
        }
    };
}

impl_recyclable!(TL_V_STEELVAL, Vec<SteelVal>);
impl_recyclable!(TL_SV_STEELVAL, SmallVec<[SteelVal; 4]>);
