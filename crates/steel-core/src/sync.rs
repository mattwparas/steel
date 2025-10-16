#![allow(dead_code)]

// Crate-local synchronization primitives that abstract over `std::sync`
// and provide lightweight alternatives when `std` is not available.

#[cfg(feature = "std")]
pub use std::sync::{LockResult, Mutex, MutexGuard, PoisonError, TryLockError, TryLockResult};

#[cfg(not(feature = "std"))]
mod no_std_mutex {
    use core::ops::{Deref, DerefMut};

    #[derive(Debug)]
    pub struct PoisonError;

    #[derive(Debug)]
    pub enum TryLockError {
        WouldBlock,
    }

    pub type LockResult<T> = Result<T, PoisonError>;
    pub type TryLockResult<T> = Result<T, TryLockError>;

    #[cfg(feature = "sync")]
    mod spin_impl {
        use super::{LockResult, PoisonError, TryLockError, TryLockResult};
        use core::cell::UnsafeCell;
        use core::hint::spin_loop;
        use core::ops::{Deref, DerefMut};
        use core::sync::atomic::{AtomicBool, Ordering};

        pub struct Mutex<T: ?Sized> {
            locked: AtomicBool,
            value: UnsafeCell<T>,
        }

        pub struct MutexGuard<'a, T: ?Sized> {
            mutex: &'a Mutex<T>,
        }

        impl<T> Mutex<T> {
            pub const fn new(value: T) -> Self
            where
                T: Sized,
            {
                Self {
                    locked: AtomicBool::new(false),
                    value: UnsafeCell::new(value),
                }
            }
        }

        impl<T: ?Sized> Mutex<T> {
            #[inline]
            pub fn lock(&self) -> LockResult<MutexGuard<'_, T>> {
                while self
                    .locked
                    .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
                    .is_err()
                {
                    spin_loop();
                }

                Ok(MutexGuard { mutex: self })
            }

            #[inline]
            pub fn try_lock(&self) -> TryLockResult<MutexGuard<'_, T>> {
                if self
                    .locked
                    .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
                    .is_ok()
                {
                    Ok(MutexGuard { mutex: self })
                } else {
                    Err(TryLockError::WouldBlock)
                }
            }

            #[inline]
            pub fn into_inner(self) -> LockResult<T>
            where
                T: Sized,
            {
                Ok(self.value.into_inner())
            }
        }

        unsafe impl<T: Send + ?Sized> Send for Mutex<T> {}
        unsafe impl<T: Send + ?Sized> Sync for Mutex<T> {}

        impl<'a, T: ?Sized> Deref for MutexGuard<'a, T> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                unsafe { &*self.mutex.value.get() }
            }
        }

        impl<'a, T: ?Sized> DerefMut for MutexGuard<'a, T> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                unsafe { &mut *self.mutex.value.get() }
            }
        }

        impl<'a, T: ?Sized> Drop for MutexGuard<'a, T> {
            fn drop(&mut self) {
                self.mutex.locked.store(false, Ordering::Release);
            }
        }

        pub use self::{Mutex as RawMutex, MutexGuard as RawMutexGuard};
    }

    #[cfg(feature = "sync")]
    pub use spin_impl::{Mutex, MutexGuard};

    #[cfg(not(feature = "sync"))]
    mod cell_impl {
        use super::{LockResult, PoisonError, TryLockError, TryLockResult};
        use core::cell::{RefCell, RefMut};
        use core::ops::{Deref, DerefMut};

        pub struct Mutex<T: ?Sized> {
            value: RefCell<T>,
        }

        pub struct MutexGuard<'a, T: ?Sized> {
            guard: RefMut<'a, T>,
        }

        impl<T> Mutex<T> {
            pub const fn new(value: T) -> Self
            where
                T: Sized,
            {
                Self {
                    value: RefCell::new(value),
                }
            }
        }

        impl<T: ?Sized> Mutex<T> {
            #[inline]
            pub fn lock(&self) -> LockResult<MutexGuard<'_, T>> {
                Ok(MutexGuard {
                    guard: self.value.borrow_mut(),
                })
            }

            #[inline]
            pub fn try_lock(&self) -> TryLockResult<MutexGuard<'_, T>> {
                match self.value.try_borrow_mut() {
                    Ok(guard) => Ok(MutexGuard { guard }),
                    Err(_) => Err(TryLockError::WouldBlock),
                }
            }

            #[inline]
            pub fn into_inner(self) -> LockResult<T>
            where
                T: Sized,
            {
                Ok(self.value.into_inner())
            }
        }

        impl<'a, T: ?Sized> Deref for MutexGuard<'a, T> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                &self.guard
            }
        }

        impl<'a, T: ?Sized> DerefMut for MutexGuard<'a, T> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.guard
            }
        }

        pub use self::{Mutex as RawMutex, MutexGuard as RawMutexGuard};
    }

    #[cfg(not(feature = "sync"))]
    pub use cell_impl::{Mutex, MutexGuard};
}

#[cfg(not(feature = "std"))]
pub use no_std_mutex::{LockResult, Mutex, MutexGuard, PoisonError, TryLockError, TryLockResult};
