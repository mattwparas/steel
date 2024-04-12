use std::cell::RefCell;
use std::default::Default;
use std::ops::{Deref, DerefMut};

use smallvec::SmallVec;

use crate::SteelVal;

pub trait Recyclable {
    fn put(self);
    fn get() -> Self;
}

pub struct Recycle<T: Recyclable> {
    t: Option<T>,
}

impl<T: Recyclable> Recycle<T> {
    pub fn new() -> Self {
        Recycle { t: Some(T::get()) }
    }
}

impl<T: Recyclable> Drop for Recycle<T> {
    fn drop(&mut self) {
        if let Some(t) = self.t.take() {
            T::put(t)
        }
    }
}

impl<T: Recyclable> Deref for Recycle<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.t.as_ref().unwrap()
    }
}

impl<T: Recyclable + 'static> DerefMut for Recycle<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.t.as_mut().unwrap()
    }
}

impl<T: Recyclable + Clone> Clone for Recycle<T> {
    fn clone(&self) -> Self {
        Recycle { t: self.t.clone() }
    }
}

impl<T: Recyclable + std::fmt::Debug> std::fmt::Debug for Recycle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Recycle").field("t", &self.t).finish()
    }
}

impl<T: Recyclable + std::hash::Hash> std::hash::Hash for Recycle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.t.hash(state);
    }
}

macro_rules! impl_recyclable {
    ($tl:ident, $t:ty) => {
        impl_recyclable!($tl, $t, Default::default());
    };
    ($tl:ident, $t:ty, $constructor:expr) => {
        thread_local! {
            static $tl: RefCell<Vec<$t>> = RefCell::new(Vec::new())
        }

        impl Recyclable for $t {
            fn put(mut self) {
                $tl.with(|p| {
                    let p = p.try_borrow_mut();

                    if let Ok(mut p) = p {
                        // This _should_ be cleared, but it seems it is not!
                        // debug_assert!(self.is_empty());
                        self.clear();
                        if p.len() < 128 {
                            p.push(self);
                        }
                    }
                })
            }

            fn get() -> Self {
                $tl.with(|p| {
                    let mut p = p.borrow_mut();
                    p.pop()
                })
                .unwrap_or($constructor)
            }
        }
    };
}

impl_recyclable!(TL_V_STEELVAL, Vec<SteelVal>);
impl_recyclable!(TL_SV_STEELVAL, SmallVec<[SteelVal; 4]>);
