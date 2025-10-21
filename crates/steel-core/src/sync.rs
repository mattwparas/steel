#![allow(dead_code)]

// Crate-local synchronization primitives that abstract over shared-state primitives.

#[cfg(feature = "std")]
mod std_sync {
    use core::fmt;
    use core::ops::{Deref, DerefMut};
    use parking_lot::{
        lock_api::ArcMutexGuard as RawArcMutexGuard,
        MappedRwLockReadGuard as RawMappedRwLockReadGuard,
        MappedRwLockWriteGuard as RawMappedRwLockWriteGuard, Mutex as RawMutex,
        MutexGuard as RawMutexGuard, RwLock as RawRwLock, RwLockReadGuard as RawRwLockReadGuard,
        RwLockWriteGuard as RawRwLockWriteGuard,
    };

    #[derive(Debug)]
    pub struct PoisonError;

    #[derive(Debug)]
    pub enum TryLockError {
        WouldBlock,
    }

    pub type LockResult<T> = Result<T, PoisonError>;
    pub type TryLockResult<T> = Result<T, TryLockError>;

    pub struct Mutex<T: ?Sized> {
        inner: RawMutex<T>,
    }

    impl<T: ?Sized + fmt::Debug> fmt::Debug for Mutex<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.inner.fmt(f)
        }
    }

    pub struct MutexGuard<'a, T: ?Sized> {
        guard: RawMutexGuard<'a, T>,
    }

    impl<T> Mutex<T> {
        pub const fn new(value: T) -> Self {
            Self {
                inner: RawMutex::new(value),
            }
        }

        #[inline]
        pub fn into_inner(self) -> LockResult<T>
        where
            T: Sized,
        {
            Ok(self.inner.into_inner())
        }
    }

    impl<T: ?Sized> Mutex<T> {
        #[inline]
        pub fn lock(&self) -> LockResult<MutexGuard<'_, T>> {
            Ok(MutexGuard {
                guard: self.inner.lock(),
            })
        }

        #[inline]
        pub fn try_lock(&self) -> TryLockResult<MutexGuard<'_, T>> {
            self.inner
                .try_lock()
                .map(|guard| MutexGuard { guard })
                .ok_or(TryLockError::WouldBlock)
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

    impl<T: Default> Default for Mutex<T> {
        fn default() -> Self {
            Self::new(T::default())
        }
    }

    pub struct RwLock<T: ?Sized> {
        inner: RawRwLock<T>,
    }

    impl<T: ?Sized + fmt::Debug> fmt::Debug for RwLock<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.inner.fmt(f)
        }
    }

    pub struct RwLockReadGuard<'a, T: ?Sized> {
        guard: RawRwLockReadGuard<'a, T>,
    }

    pub struct RwLockWriteGuard<'a, T: ?Sized> {
        guard: RawRwLockWriteGuard<'a, T>,
    }

    pub type MappedRwLockReadGuard<'a, T> = RawMappedRwLockReadGuard<'a, T>;
    pub type MappedRwLockWriteGuard<'a, T> = RawMappedRwLockWriteGuard<'a, T>;

    impl<T> RwLock<T> {
        pub const fn new(value: T) -> Self {
            Self {
                inner: RawRwLock::new(value),
            }
        }
    }

    impl<T: Default> Default for RwLock<T> {
        fn default() -> Self {
            Self::new(T::default())
        }
    }

    impl<T: ?Sized> RwLock<T> {
        #[inline]
        pub fn read(&self) -> RwLockReadGuard<'_, T> {
            RwLockReadGuard {
                guard: self.inner.read(),
            }
        }

        #[inline]
        pub fn write(&self) -> RwLockWriteGuard<'_, T> {
            RwLockWriteGuard {
                guard: self.inner.write(),
            }
        }

        #[inline]
        pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<'_, T>> {
            self.inner
                .try_read()
                .map(|guard| RwLockReadGuard { guard })
                .ok_or(TryLockError::WouldBlock)
        }

        #[inline]
        pub fn try_write(&self) -> TryLockResult<RwLockWriteGuard<'_, T>> {
            self.inner
                .try_write()
                .map(|guard| RwLockWriteGuard { guard })
                .ok_or(TryLockError::WouldBlock)
        }

        #[inline]
        pub fn into_inner(self) -> LockResult<T>
        where
            T: Sized,
        {
            Ok(self.inner.into_inner())
        }
    }

    impl<'a, T: ?Sized> Deref for RwLockReadGuard<'a, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.guard
        }
    }

    impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for RwLockReadGuard<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&*self.guard, f)
        }
    }

    impl<'a, T: ?Sized + fmt::Display> fmt::Display for RwLockReadGuard<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(&*self.guard, f)
        }
    }

    impl<'a, T: ?Sized> RwLockReadGuard<'a, T> {
        #[inline]
        pub fn map<U: ?Sized, F>(this: Self, f: F) -> MappedRwLockReadGuard<'a, U>
        where
            F: for<'b> FnOnce(&'b T) -> &'b U,
        {
            let guard = this.guard;
            RawRwLockReadGuard::map(guard, f)
        }
    }

    impl<'a, T: ?Sized> Deref for RwLockWriteGuard<'a, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.guard
        }
    }

    impl<'a, T: ?Sized> DerefMut for RwLockWriteGuard<'a, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.guard
        }
    }

    impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for RwLockWriteGuard<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&*self.guard, f)
        }
    }

    impl<'a, T: ?Sized> RwLockWriteGuard<'a, T> {
        #[inline]
        pub fn map<U: ?Sized, F>(this: Self, f: F) -> MappedRwLockWriteGuard<'a, U>
        where
            F: for<'b> FnOnce(&'b mut T) -> &'b mut U,
        {
            let guard = this.guard;
            RawRwLockWriteGuard::map(guard, f)
        }
    }

    pub type ArcMutex<T> = parking_lot::Mutex<T>;
    pub type ArcMutexGuard<T> = RawArcMutexGuard<parking_lot::RawMutex, T>;
}

#[cfg(not(feature = "std"))]
mod no_std_sync {
    use alloc::sync::Arc;
    use core::cell::{Ref, RefCell, RefMut};
    use core::fmt;
    use core::ops::{Deref, DerefMut};

    #[derive(Debug)]
    pub struct PoisonError;

    #[derive(Debug)]
    pub enum TryLockError {
        WouldBlock,
    }

    #[derive(Debug)]
    pub struct AccessError;

    pub type LockResult<T> = Result<T, PoisonError>;
    pub type TryLockResult<T> = Result<T, TryLockError>;

    pub struct Mutex<T: ?Sized> {
        value: RefCell<T>,
    }

    unsafe impl<T: ?Sized> Send for Mutex<T> {}
    unsafe impl<T: ?Sized> Sync for Mutex<T> {}

    impl<T: ?Sized + fmt::Debug> fmt::Debug for Mutex<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.value.fmt(f)
        }
    }

    pub struct MutexGuard<'a, T: ?Sized> {
        guard: RefMut<'a, T>,
    }

    pub struct ArcMutex<T: ?Sized> {
        inner: Mutex<T>,
    }

    impl<T: ?Sized + fmt::Debug> fmt::Debug for ArcMutex<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.inner.fmt(f)
        }
    }

    pub struct ArcMutexGuard<'a, T: ?Sized> {
        guard: MutexGuard<'a, T>,
    }

    pub struct ThreadLocal<T: 'static> {
        init: fn() -> T,
        storage: Mutex<Option<T>>,
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

        #[inline]
        pub fn into_inner(self) -> LockResult<T>
        where
            T: Sized,
        {
            Ok(self.value.into_inner())
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

    impl<T: Default> Default for Mutex<T> {
        fn default() -> Self {
            Self::new(T::default())
        }
    }

    impl<T> ArcMutex<T> {
        pub const fn new(value: T) -> Self
        where
            T: Sized,
        {
            Self {
                inner: Mutex::new(value),
            }
        }
    }

    impl<T: Default> Default for ArcMutex<T> {
        fn default() -> Self {
            Self::new(T::default())
        }
    }

    impl<T: ?Sized> ArcMutex<T> {
        #[inline]
        pub fn lock(&self) -> LockResult<MutexGuard<'_, T>> {
            self.inner.lock()
        }

        #[inline]
        pub fn try_lock(&self) -> TryLockResult<MutexGuard<'_, T>> {
            self.inner.try_lock()
        }

        #[inline]
        pub fn lock_arc(self: &Arc<Self>) -> ArcMutexGuard<'_, T> {
            ArcMutexGuard {
                guard: self
                    .inner
                    .lock()
                    .expect("lock_arc failed due to poisoned mutex"),
            }
        }

        #[inline]
        pub fn try_lock_arc(self: &Arc<Self>) -> TryLockResult<ArcMutexGuard<'_, T>> {
            self.inner.try_lock().map(|guard| ArcMutexGuard { guard })
        }

        #[inline]
        pub fn into_inner(self) -> LockResult<T>
        where
            T: Sized,
        {
            self.inner.into_inner()
        }
    }

    impl<'a, T: ?Sized> Deref for ArcMutexGuard<'a, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &*self.guard
        }
    }

    impl<'a, T: ?Sized> DerefMut for ArcMutexGuard<'a, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut *self.guard
        }
    }

    pub struct RwLock<T: ?Sized> {
        value: RefCell<T>,
    }

    pub struct RwLockReadGuard<'a, T: ?Sized> {
        inner: Ref<'a, T>,
    }

    pub struct RwLockWriteGuard<'a, T: ?Sized> {
        inner: RefMut<'a, T>,
    }

    impl<'a, T: ?Sized + fmt::Display> fmt::Display for RwLockReadGuard<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(&*self.inner, f)
        }
    }

    pub type MappedRwLockReadGuard<'a, T> = Ref<'a, T>;
    pub type MappedRwLockWriteGuard<'a, T> = RefMut<'a, T>;

    impl<T> RwLock<T> {
        pub const fn new(value: T) -> Self
        where
            T: Sized,
        {
            Self {
                value: RefCell::new(value),
            }
        }
    }

    impl<T: ?Sized + fmt::Debug> fmt::Debug for RwLock<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.value.fmt(f)
        }
    }

    impl<T: Default> Default for RwLock<T> {
        fn default() -> Self {
            Self::new(T::default())
        }
    }

    impl<T: ?Sized> RwLock<T> {
        #[inline]
        pub fn read(&self) -> RwLockReadGuard<'_, T> {
            RwLockReadGuard {
                inner: self.value.borrow(),
            }
        }

        #[inline]
        pub fn write(&self) -> RwLockWriteGuard<'_, T> {
            RwLockWriteGuard {
                inner: self.value.borrow_mut(),
            }
        }

        #[inline]
        pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<'_, T>> {
            match self.value.try_borrow() {
                Ok(inner) => Ok(RwLockReadGuard { inner }),
                Err(_) => Err(TryLockError::WouldBlock),
            }
        }

        #[inline]
        pub fn try_write(&self) -> TryLockResult<RwLockWriteGuard<'_, T>> {
            match self.value.try_borrow_mut() {
                Ok(inner) => Ok(RwLockWriteGuard { inner }),
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

    impl<'a, T: ?Sized> Deref for RwLockReadGuard<'a, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }

    impl<'a, T: ?Sized> RwLockReadGuard<'a, T> {
        #[inline]
        pub fn map<U: ?Sized, F>(this: Self, f: F) -> MappedRwLockReadGuard<'a, U>
        where
            F: for<'b> FnOnce(&'b T) -> &'b U,
        {
            Ref::map(this.inner, f)
        }
    }

    impl<'a, T: ?Sized> Deref for RwLockWriteGuard<'a, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }

    impl<'a, T: ?Sized> DerefMut for RwLockWriteGuard<'a, T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.inner
        }
    }

    impl<T: 'static> ThreadLocal<T> {
        pub const fn new(init: fn() -> T) -> Self {
            Self {
                init,
                storage: Mutex::new(None),
            }
        }

        pub fn with<R>(&self, f: impl FnOnce(&T) -> R) -> R {
            let mut guard = match self.storage.lock() {
                Ok(guard) => guard,
                Err(_) => panic!("thread-local mutex poisoned"),
            };

            let value = guard.get_or_insert_with(self.init);
            f(value)
        }

        pub fn try_with<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, AccessError> {
            Ok(self.with(f))
        }
    }

    impl<'a, T: ?Sized> RwLockWriteGuard<'a, T> {
        #[inline]
        pub fn map<U: ?Sized, F>(this: Self, f: F) -> MappedRwLockWriteGuard<'a, U>
        where
            F: for<'b> FnOnce(&'b mut T) -> &'b mut U,
        {
            RefMut::map(this.inner, f)
        }
    }

    impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for RwLockReadGuard<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&*self.inner, f)
        }
    }

    impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for RwLockWriteGuard<'a, T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&*self.inner, f)
        }
    }
}

#[cfg(feature = "std")]
pub use std_sync::{
    ArcMutex, ArcMutexGuard, LockResult, MappedRwLockReadGuard, MappedRwLockWriteGuard, Mutex,
    MutexGuard, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard, TryLockError,
    TryLockResult,
};

#[cfg(not(feature = "std"))]
pub use no_std_sync::{
    AccessError, ArcMutex, ArcMutexGuard, LockResult, MappedRwLockReadGuard,
    MappedRwLockWriteGuard, Mutex, MutexGuard, PoisonError, RwLock, RwLockReadGuard,
    RwLockWriteGuard, ThreadLocal, TryLockError, TryLockResult,
};

#[cfg(feature = "std")]
pub use std::thread_local;

#[cfg(not(feature = "std"))]
#[macro_export]
macro_rules! thread_local {
    () => {};
    ($(#[$attr:meta])* $vis:vis static $name:ident : $ty:ty = const $($rest:tt)+; $($tail:tt)*) => {
        $(#[$attr])* $vis static $name: $crate::sync::ThreadLocal<$ty> =
            $crate::sync::ThreadLocal::new(|| const { $($rest)+ });
        $crate::thread_local!($($tail)*);
    };
    ($(#[$attr:meta])* $vis:vis static $name:ident : $ty:ty = $value:expr; $($tail:tt)*) => {
        $(#[$attr])* $vis static $name: $crate::sync::ThreadLocal<$ty> =
            $crate::sync::ThreadLocal::new(|| $value);
        $crate::thread_local!($($tail)*);
    };
}
