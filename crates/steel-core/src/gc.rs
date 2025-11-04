use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::stop;
#[cfg_attr(not(feature = "sync"), allow(unused_imports))]
use crate::sync::RwLock;
#[allow(unused_imports)]
use alloc::{boxed::Box, format, string::String, vec::Vec};
#[allow(unused_imports)]
use alloc::string::ToString;

#[cfg(not(feature = "sync"))]
use core::cell::RefCell;

#[cfg(feature = "std")]
use crate::os_strings::OsStr;
use core::fmt;
use core::fmt::Pointer;
use core::ops::Deref;
use core::sync::atomic::{AtomicUsize, Ordering};

pub static OBJECT_COUNT: AtomicUsize = AtomicUsize::new(0);
pub(crate) static MAXIMUM_OBJECTS: usize = 50000;

pub use shared::{GcMut, MutContainer, ShareableMut, Shared, SharedMut};
pub use unsafe_erased_pointers::is_reference_type;

pub mod shared {
    #[cfg(not(feature = "sync"))]
    use alloc::rc::Weak;
    #[allow(unused_imports)]
    use alloc::{boxed::Box, rc::Rc, string::String, vec::Vec};
    use core::cell::{BorrowError, BorrowMutError, Ref, RefCell, RefMut};
    use core::ops::{Deref, DerefMut};

    #[cfg_attr(not(feature = "sync"), allow(unused_imports))]
    use crate::sync::{
        MappedRwLockReadGuard, MappedRwLockWriteGuard, Mutex, MutexGuard, RwLock, RwLockReadGuard,
        RwLockWriteGuard, TryLockResult,
    };
    use alloc::sync::Arc;

    use super::Gc;

    #[cfg(not(feature = "sync"))]
    pub type Shared<T> = Rc<T>;

    #[cfg(not(feature = "sync"))]
    pub type SharedMut<T> = Rc<RefCell<T>>;

    #[cfg(not(feature = "sync"))]
    pub type WeakSharedMut<T> = Weak<RefCell<T>>;

    #[cfg(not(feature = "sync"))]
    pub type WeakShared<T> = Weak<T>;

    #[cfg(not(feature = "sync"))]
    pub type MutContainer<T> = RefCell<T>;

    #[cfg(not(feature = "sync"))]
    pub type GcMut<T> = Gc<RefCell<T>>;

    #[cfg(feature = "sync")]
    pub type StandardShared<T> = alloc::sync::Arc<T>;

    #[cfg(feature = "sync")]
    pub type StandardSharedMut<T> = alloc::sync::Arc<RwLock<T>>;

    #[cfg(not(feature = "sync"))]
    pub type StandardShared<T> = alloc::rc::Rc<T>;

    #[cfg(not(feature = "sync"))]
    pub type StandardSharedMut<T> = alloc::rc::Rc<RefCell<T>>;

    #[cfg(all(feature = "sync", not(feature = "triomphe")))]
    pub type Shared<T> = Arc<T>;
    #[cfg(all(feature = "sync", not(feature = "triomphe")))]
    pub type SharedMut<T> = Arc<RwLock<T>>;

    #[cfg(all(feature = "sync", feature = "triomphe"))]
    pub type Shared<T> = triomphe::Arc<T>;
    #[cfg(all(feature = "sync", feature = "triomphe"))]
    pub type SharedMut<T> = triomphe::Arc<RwLock<T>>;

    #[cfg(feature = "sync")]
    pub type GcMut<T> = Gc<RwLock<T>>;

    #[cfg(feature = "sync")]
    pub type WeakSharedMut<T> = alloc::sync::Weak<RwLock<T>>;

    #[cfg(feature = "sync")]
    pub type WeakShared<T> = alloc::sync::Weak<T>;

    #[cfg(feature = "sync")]
    pub type MutContainer<T> = RwLock<T>;

    pub trait MutableContainer<T> {
        fn consume(self) -> T;
    }

    impl<T> MutableContainer<T> for RefCell<T> {
        fn consume(self) -> T {
            self.into_inner()
        }
    }

    impl<T> MutableContainer<T> for RwLock<T> {
        fn consume(self) -> T {
            self.into_inner()
                .expect("RwLock::into_inner should not fail in non-poisoning contexts")
        }
    }

    #[cfg(not(feature = "sync"))]
    pub type ScopedReadContainer<'a, T> = Ref<'a, T>;
    #[cfg(not(feature = "sync"))]
    pub type ScopedWriteContainer<'a, T> = RefMut<'a, T>;

    #[cfg(feature = "sync")]
    pub type ScopedReadContainer<'a, T> = RwLockReadGuard<'a, T>;
    #[cfg(feature = "sync")]
    pub type ScopedWriteContainer<'a, T> = RwLockWriteGuard<'a, T>;

    #[cfg(not(feature = "sync"))]
    pub type MappedScopedReadContainer<'a, T> = Ref<'a, T>;
    #[cfg(not(feature = "sync"))]
    pub type MappedScopedWriteContainer<'a, T> = RefMut<'a, T>;

    #[cfg(feature = "sync")]
    pub type MappedScopedReadContainer<'a, T> = MappedRwLockReadGuard<'a, T>;
    #[cfg(feature = "sync")]
    pub type MappedScopedWriteContainer<'a, T> = MappedRwLockWriteGuard<'a, T>;

    pub trait ShareableMut<T>: Clone {
        type ShareableRead<'a>: Deref<Target = T>
        where
            Self: 'a;
        type ShareableWrite<'a>: DerefMut<Target = T>
        where
            Self: 'a;

        type TryReadResult<'a>
        where
            Self: 'a;
        type TryWriteResult<'a>
        where
            Self: 'a;

        /// Obtain a scoped guard for reading
        fn read<'a>(&'a self) -> Self::ShareableRead<'a>;
        /// Obtain a scoped guard for writing
        fn write<'a>(&'a self) -> Self::ShareableWrite<'a>;

        /// Fallibly obtain a scoped guard for reading
        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a>;

        /// Fallibly obtain a scoped guard for writing
        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a>;
    }

    impl<T> ShareableMut<T> for Rc<RefCell<T>> {
        type ShareableRead<'a>
            = Ref<'a, T>
        where
            T: 'a;
        type ShareableWrite<'a>
            = RefMut<'a, T>
        where
            T: 'a;
        type TryReadResult<'a>
            = Result<Ref<'a, T>, BorrowError>
        where
            T: 'a;
        type TryWriteResult<'a>
            = Result<RefMut<'a, T>, BorrowMutError>
        where
            T: 'a;

        fn read<'a>(&'a self) -> Self::ShareableRead<'a> {
            Rc::deref(self).borrow()
        }

        fn write<'a>(&'a self) -> Self::ShareableWrite<'a> {
            Rc::deref(self).borrow_mut()
        }

        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a> {
            Rc::deref(self).try_borrow_mut()
        }

        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a> {
            Rc::deref(self).try_borrow()
        }
    }

    impl<T> ShareableMut<T> for Gc<RefCell<T>> {
        type ShareableRead<'a>
            = Ref<'a, T>
        where
            T: 'a;
        type ShareableWrite<'a>
            = RefMut<'a, T>
        where
            T: 'a;
        type TryReadResult<'a>
            = Result<Ref<'a, T>, BorrowError>
        where
            T: 'a;
        type TryWriteResult<'a>
            = Result<RefMut<'a, T>, BorrowMutError>
        where
            T: 'a;

        fn read<'a>(&'a self) -> Self::ShareableRead<'a> {
            Gc::deref(self).borrow()
        }

        fn write<'a>(&'a self) -> Self::ShareableWrite<'a> {
            Gc::deref(self).borrow_mut()
        }

        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a> {
            Gc::deref(self).try_borrow_mut()
        }

        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a> {
            Gc::deref(self).try_borrow()
        }
    }

    impl<T> ShareableMut<T> for Arc<RwLock<T>> {
        type ShareableRead<'a>
            = RwLockReadGuard<'a, T>
        where
            T: 'a;
        type ShareableWrite<'a>
            = RwLockWriteGuard<'a, T>
        where
            T: 'a;
        // type TryReadResult<'a>
        // = TryLockResult<RwLockReadGuard<'a, T>> where T: 'a;
        type TryReadResult<'a>
            = Result<RwLockReadGuard<'a, T>, ()>
        where
            T: 'a;
        // type TryWriteResult<'a>
        // = TryLockResult<RwLockWriteGuard<'a, T>> where T: 'a;
        type TryWriteResult<'a>
            = Result<RwLockWriteGuard<'a, T>, ()>
        where
            T: 'a;

        fn read<'a>(&'a self) -> Self::ShareableRead<'a> {
            Arc::deref(self).read()
            // .expect("Read lock should not be poisoned")
        }

        fn write<'a>(&'a self) -> Self::ShareableWrite<'a> {
            Arc::deref(self).write()
            // .expect("Write lock should not be poisoned")
        }

        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a> {
            match Arc::deref(self).try_read() {
                Ok(v) => Ok(v),
                Err(_) => Err(()),
            }
        }

        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a> {
            match Arc::deref(self).try_write() {
                Ok(v) => Ok(v),
                Err(_) => Err(()),
            }
        }
    }

    impl<T> ShareableMut<T> for Gc<RwLock<T>> {
        type ShareableRead<'a>
            = RwLockReadGuard<'a, T>
        where
            T: 'a;
        type ShareableWrite<'a>
            = RwLockWriteGuard<'a, T>
        where
            T: 'a;
        // type TryReadResult<'a>
        // = TryLockResult<RwLockReadGuard<'a, T>> where T: 'a;
        // type TryWriteResult<'a>
        // = TryLockResult<RwLockWriteGuard<'a, T>> where T: 'a;
        type TryReadResult<'a>
            = Result<RwLockReadGuard<'a, T>, ()>
        where
            T: 'a;
        type TryWriteResult<'a>
            = Result<RwLockWriteGuard<'a, T>, ()>
        where
            T: 'a;

        fn read<'a>(&'a self) -> Self::ShareableRead<'a> {
            Gc::deref(self).read()
            // .expect("Read lock should not be poisoned")
        }

        fn write<'a>(&'a self) -> Self::ShareableWrite<'a> {
            Gc::deref(self).write()
            // .expect("Write lock should not be poisoned")
        }

        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a> {
            match Gc::deref(self).try_read() {
                Ok(v) => Ok(v),
                Err(_) => Err(()),
            }
        }

        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a> {
            match Gc::deref(self).try_write() {
                Ok(v) => Ok(v),
                Err(_) => Err(()),
            }
        }
    }

    impl<T> ShareableMut<T> for Arc<Mutex<T>> {
        type ShareableRead<'a>
            = MutexGuard<'a, T>
        where
            T: 'a;
        type ShareableWrite<'a>
            = MutexGuard<'a, T>
        where
            T: 'a;
        type TryReadResult<'a>
            = TryLockResult<MutexGuard<'a, T>>
        where
            T: 'a;
        type TryWriteResult<'a>
            = TryLockResult<MutexGuard<'a, T>>
        where
            T: 'a;

        fn read<'a>(&'a self) -> Self::ShareableRead<'a> {
            Arc::deref(self)
                .lock()
                .expect("Mutex should not be poisoned")
        }

        fn write<'a>(&'a self) -> Self::ShareableWrite<'a> {
            Arc::deref(self)
                .lock()
                .expect("Mutex should not be poisoned")
        }

        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a> {
            Arc::deref(self).try_lock()
        }

        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a> {
            Arc::deref(self).try_lock()
        }
    }

    impl<T> ShareableMut<T> for Gc<Mutex<T>> {
        type ShareableRead<'a>
            = MutexGuard<'a, T>
        where
            T: 'a;
        type ShareableWrite<'a>
            = MutexGuard<'a, T>
        where
            T: 'a;
        type TryReadResult<'a>
            = TryLockResult<MutexGuard<'a, T>>
        where
            T: 'a;
        type TryWriteResult<'a>
            = TryLockResult<MutexGuard<'a, T>>
        where
            T: 'a;

        fn read<'a>(&'a self) -> Self::ShareableRead<'a> {
            Gc::deref(self)
                .lock()
                .expect("Mutex should not be poisoned")
        }

        fn write<'a>(&'a self) -> Self::ShareableWrite<'a> {
            Gc::deref(self)
                .lock()
                .expect("Mutex should not be poisoned")
        }

        fn try_read<'a>(&'a self) -> Self::TryReadResult<'a> {
            Gc::deref(self).try_lock()
        }

        fn try_write<'a>(&'a self) -> Self::TryWriteResult<'a> {
            Gc::deref(self).try_lock()
        }
    }
}

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
pub struct Gc<T: ?Sized>(pub(crate) Shared<T>);

impl<T: ?Sized> Pointer for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

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
        Shared::make_mut(&mut self.0)
    }
}

impl<T> Gc<T> {
    pub fn new(val: T) -> Gc<T> {
        // OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        Gc(Shared::new(val))
    }

    pub fn new_mut(val: T) -> GcMut<T> {
        #[cfg(not(feature = "sync"))]
        {
            Gc::new(RefCell::new(val))
        }

        #[cfg(feature = "sync")]
        Gc::new(RwLock::new(val))
    }

    pub fn try_new(val: T) -> Result<Gc<T>, SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(1, Ordering::SeqCst);
        if mem > MAXIMUM_OBJECTS {
            stop!(Generic => "ran out of memory!")
        }
        Ok(Gc(Shared::new(val)))
    }

    pub fn checked_allocate(allocations: usize) -> Result<(), SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(0, Ordering::SeqCst);
        if mem + allocations > MAXIMUM_OBJECTS {
            stop!(Generic => "allocation would exceed maximum allowed memory")
        }
        Ok(())
    }

    pub fn try_unwrap(self) -> Result<T, Gc<T>> {
        Shared::try_unwrap(self.0).map_err(|x| Gc(x))
    }

    pub fn check_memory() -> Result<usize, SteelErr> {
        let mem: usize = OBJECT_COUNT.fetch_add(0, Ordering::SeqCst);
        if mem > MAXIMUM_OBJECTS {
            stop!(Generic => "ran out of memory!")
        }
        Ok(mem)
    }
}

impl<T: ?Sized> Gc<T> {
    // in order to fully sandbox, I have to check the memory limit

    pub fn get_mut(&mut self) -> Option<&mut T> {
        Shared::get_mut(&mut self.0)
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Shared::ptr_eq(&this.0, &other.0)
    }

    pub fn as_ptr(&self) -> *const T {
        Shared::as_ptr(&self.0)
    }

    pub fn into_raw(self) -> *const T {
        Shared::into_raw(self.0)
    }

    pub unsafe fn from_raw(this: *const T) -> Self {
        Self(Shared::from_raw(this))
    }

    pub fn strong_count(this: &Self) -> usize {
        Shared::strong_count(&this.0)
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Gc(Shared::clone(&self.0))
    }
}

#[cfg(feature = "std")]
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
pub mod unsafe_roots {

    use super::Gc;
    use core::ptr::NonNull;

    #[derive(Clone)]
    pub enum MaybeRooted<T> {
        Rooted(Rooted<T>),
        Reference(Gc<T>),
    }

    impl<T: core::fmt::Debug> core::fmt::Debug for MaybeRooted<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                Self::Rooted(v) => write!(f, "{:?}", unsafe { v.value.as_ref() }),
                Self::Reference(v) => write!(f, "{:?}", v),
            }
        }
    }

    impl<T> MaybeRooted<T> {
        pub fn from_root(value: &Gc<T>) -> Self {
            Self::Rooted(Rooted::from_ref(value))
        }
    }

    impl<T> AsRef<T> for MaybeRooted<T> {
        fn as_ref(&self) -> &T {
            match self {
                Self::Rooted(v) => unsafe { v.value.as_ref() },
                Self::Reference(v) => &v,
            }
        }
    }

    impl<T> core::ops::Deref for MaybeRooted<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            match self {
                Self::Rooted(v) => unsafe { v.value.as_ref() },
                Self::Reference(v) => &v,
            }
        }
    }

    #[derive(Clone)]
    pub struct Rooted<T> {
        value: NonNull<T>,
    }

    impl<T> Rooted<T> {
        pub fn from_ref(value: &Gc<T>) -> Self {
            Rooted {
                value: NonNull::new(Gc::as_ptr(value) as _).expect("Given pointer was null!"),
            }
        }
    }

    #[test]
    fn test_rooting() {
        use crate::SteelVal;

        let root = Gc::new(SteelVal::ListV(im_lists::list![]));

        let rooted_reference = Rooted::from_ref(&root);

        println!("{:?}", unsafe { rooted_reference.value.as_ref() });
    }

    #[test]
    fn recover_original_gc() {
        use crate::SteelVal;

        let root = Gc::new(SteelVal::ListV(im_lists::list![]));

        let rooted_reference = Rooted::from_ref(&root);

        let recovered = unsafe { rooted_reference.value.as_ref() };

        println!("{:?}", recovered);
    }
}

// #[cfg(feature = "unsafe-internals")]
#[allow(unused)]
pub mod unsafe_erased_pointers {
    /*
    Warning - here be dragons. Definitely a lot of unsafe things here, and when used incorrectly
    can lead to undefined behavior.
    */

    use alloc::{
        boxed::Box,
        format,
        rc::{Rc, Weak},
        string::String,
        sync::Arc,
        vec::Vec,
    };
    use core::sync::atomic::AtomicBool;
    use core::{
        any::Any,
        cell::{Cell, RefCell},
        marker::PhantomData,
    };

    use crate::gc::shared::{
        MappedScopedReadContainer, MappedScopedWriteContainer, ScopedReadContainer,
        ScopedWriteContainer, StandardSharedMut,
    };
    use crate::steel_vm::engine::EngineId;
    use crate::sync::Mutex;
    use once_cell::sync::Lazy;

    use crate::rvals::cycles::IterativeDropHandler;
    use crate::rvals::{AsRefSteelValFromRef, MaybeSendSyncStatic};
    use crate::{rerrs::ErrorKind, rvals::AsRefMutSteelValFromRef, SteelErr, SteelVal};

    use super::shared::{
        MutContainer, ShareableMut as _, StandardShared, WeakShared, WeakSharedMut,
    };
    use super::{Gc, Shared};

    // TODO: This needs to be exanded to n args, probably like 8 with a macro
    pub struct MutableReferenceArena<A, B> {
        _phantom1: PhantomData<A>,
        _phantom2: PhantomData<B>,
    }

    impl<A, B> MutableReferenceArena<A, B> {
        pub fn new() -> Self {
            Self {
                _phantom1: PhantomData,
                _phantom2: PhantomData,
            }
        }

        pub fn retain_reference(
            &mut self,
            original_a: &mut A,
            original_b: &mut B,
            mut thunk: impl FnMut(
                BorrowedObject<A>,
                BorrowedObject<B>,
            ) -> crate::rvals::Result<SteelVal>,
        ) -> crate::rvals::Result<SteelVal> {
            let erased = original_a as *mut _;

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let borrowed = BorrowedObject::new(weak_ptr);

            let erased2 = original_b as *mut _;
            let wrapped2 = StandardShared::new(MutContainer::new(erased2));
            let weak_ptr2 = StandardShared::downgrade(&wrapped2);

            let borrowed2 = BorrowedObject::new(weak_ptr2);

            thunk(borrowed, borrowed2)
        }
    }

    pub struct MutableReferenceNursery<T> {
        _phantom: PhantomData<T>, // pointers: Vec<Rc<RefCell<*mut T>>>,
    }

    impl<T> MutableReferenceNursery<T> {
        pub fn new() -> Self {
            Self {
                _phantom: PhantomData, // pointers: Vec::new(),
            }
        }

        pub fn retain_readonly_reference(
            &mut self,
            original: &T,
            mut thunk: impl FnMut(ReadOnlyBorrowedObject<T>) -> crate::rvals::Result<SteelVal>,
        ) -> crate::rvals::Result<SteelVal> {
            let erased = original as *const _;

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, Arc::new(Mutex::new(0)));

            thunk(borrowed)
        }

        pub fn retain_reference(
            &mut self,
            original: &mut T,
            mut thunk: impl FnMut(BorrowedObject<T>) -> crate::rvals::Result<SteelVal>,
        ) -> crate::rvals::Result<SteelVal> {
            let erased = original as *mut _;

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let borrowed = BorrowedObject::new(weak_ptr);

            thunk(borrowed)
        }

        // pub(crate) fn wrap_mut_borrow_object(original: &mut T) -> BorrowedObject<T> {
        //     let erased = original as *mut _;

        //     // Wrap the original mutable pointer in an object that respects borrowing
        //     // rules for runtime borrow checking
        //     let wrapped = Rc::new(RefCell::new(erased));
        //     let weak_ptr = Rc::downgrade(&wrapped);

        //     let borrowed = BorrowedObject { ptr: weak_ptr };

        //     borrowed
        // }
    }

    // TODO: Re-evaluate the necessity of this trait. Is it possible to overload it all into one? That could
    // help disambiguate the function call sites.
    pub trait CustomReference {
        fn walk(&self) {}
    }

    /// Warning - you should not implement this trait yourself. Use the `custom_reference` macro instead.
    /// Implementing this incorrectly could lead to undefined behavior.
    pub unsafe trait ReferenceMarker<'a> {
        type Static: ?Sized + 'static;
    }

    pub fn type_id<'a, T>() -> core::any::TypeId
    where
        T: ReferenceMarker<'a>,
        T::Static: Any,
    {
        core::any::TypeId::of::<T::Static>()
    }

    #[macro_export]
    macro_rules! custom_reference {
        ($t: ident) => {
            unsafe impl<'a> $crate::gc::unsafe_erased_pointers::ReferenceMarker<'a> for $t {
                type Static = $t;
            }
        };

        ($t: ident<'a>) => {
            unsafe impl<'a> $crate::gc::unsafe_erased_pointers::ReferenceMarker<'a> for $t<'a> {
                type Static = $t<'static>;
            }
        };
    }

    // Only works if this is explicitly, a reference type?
    impl CustomReference for &str {}

    pub trait ReferenceCustomType {
        fn as_any_ref(&self) -> &dyn Any;
        fn as_any_ref_mut(&mut self) -> &mut dyn Any;
        fn name(&self) -> &str {
            core::any::type_name::<Self>()
        }
        fn display(&self) -> core::result::Result<String, core::fmt::Error> {
            Ok(format!("#<{}>", self.name()))
        }
        fn visit(&self) {}
        fn drop_mut(&mut self, drop_handler: &mut IterativeDropHandler) {}
    }

    impl<T: CustomReference + 'static> ReferenceCustomType for T {
        fn as_any_ref(&self) -> &dyn Any {
            self as &dyn Any
        }
        fn as_any_ref_mut(&mut self) -> &mut dyn Any {
            self as &mut dyn Any
        }
        fn display(&self) -> core::result::Result<String, core::fmt::Error> {
            Ok(format!("#<{}>", self.name()))
        }
        fn visit(&self) {
            self.walk()
        }
    }

    // impl<T: ReferenceCustomType + 'static> IntoSteelVal for T {
    //     fn into_steelval(self) -> crate::rvals::Result<SteelVal> {
    //         // Ok(self.new_steel_val())
    //         Ok(SteelVal::Custom(Rc::new(RefCell::new(Box::new(self)))))
    //     }
    // }

    /// This is for objects that are references FROM an already borrowed object.
    /// In order to make this safe - we create a scoped nursery which ties the lifetime
    /// of the object to the parent object, and all of those get dropped when the parent lifetime
    /// ends. This will also mean that allocating references repeatedly will continuously
    /// fill space in the nursery (at the moment). Once the original reference is done being borrowed,
    /// we purge the entire nursery. This could cause issues - however for now as a proof
    /// of concept, we will just treat the nursery as an allocation pool. Gc cycles should be able
    /// to be run over this pool - just check the amount of weak pointer allocations to each allocation
    /// and drop those from the vec.
    pub(crate) struct TemporaryObject<T> {
        pub(crate) ptr: StandardShared<MutContainer<*mut T>>,
    }

    #[cfg(feature = "sync")]
    unsafe impl<T> Send for TemporaryObject<T> {}
    #[cfg(feature = "sync")]
    unsafe impl<T> Sync for TemporaryObject<T> {}

    // TODO: Probably combine this and the above
    pub(crate) struct ReadOnlyTemporaryObject<T> {
        pub(crate) ptr: StandardShared<MutContainer<*const T>>,
    }

    #[cfg(feature = "sync")]
    unsafe impl<T> Send for ReadOnlyTemporaryObject<T> {}
    #[cfg(feature = "sync")]
    unsafe impl<T> Sync for ReadOnlyTemporaryObject<T> {}

    // Not a reference explicitly. This might contain a reference, but on its own it is a
    // value type. This should probably only deal with immutable references for now.
    pub(crate) struct Temporary<T> {
        pub(crate) ptr: StandardShared<T>,
    }

    // #[cfg(feature = "sync")]
    // unsafe impl<T> Send for Temporary<T> {}
    // #[cfg(feature = "sync")]
    // unsafe impl<T> Sync for Temporary<T> {}

    impl<T> CustomReference for Temporary<T> {}
    impl<T> CustomReference for TemporaryObject<T> {}
    impl<T> CustomReference for ReadOnlyTemporaryObject<T> {}

    impl<T: MaybeSendSyncStatic> TemporaryObject<T> {
        pub(crate) fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: StandardShared::new(self),
            }
        }
    }

    impl<T: MaybeSendSyncStatic> ReadOnlyTemporaryObject<T> {
        pub(crate) fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: StandardShared::new(self),
            }
        }
    }

    impl<T: MaybeSendSyncStatic> Temporary<T> {
        pub(crate) fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: StandardShared::new(self),
            }
        }
    }

    pub struct ReadOnlyTemporary<T> {
        pub(crate) ptr: WeakShared<T>,
    }

    impl<T> CustomReference for ReadOnlyTemporary<T> {}

    impl<T> Clone for ReadOnlyTemporary<T> {
        fn clone(&self) -> Self {
            Self {
                ptr: WeakShared::clone(&self.ptr),
            }
        }
    }

    impl<T: MaybeSendSyncStatic> ReadOnlyTemporary<T> {
        pub(crate) fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: StandardShared::new(self),
            }
        }
    }

    pub struct ReadOnlyBorrowedObject<T> {
        pub(crate) ptr: WeakSharedMut<*const T>,
        pub(crate) parent_borrow_count: Arc<Mutex<BorrowFlag>>,
    }

    impl<T> CustomReference for ReadOnlyBorrowedObject<T> {}

    impl<T> ReadOnlyBorrowedObject<T> {
        pub fn new(
            ptr: WeakSharedMut<*const T>,
            parent_borrow_count: Arc<Mutex<BorrowFlag>>,
        ) -> Self {
            Self {
                ptr,
                parent_borrow_count,
            }
        }
    }

    impl<T> Drop for ReadOnlyBorrowedObject<T> {
        fn drop(&mut self) {
            let mut guard = self.parent_borrow_count.lock().unwrap();
            *guard -= 1;
        }
    }

    impl<T> Clone for ReadOnlyBorrowedObject<T> {
        fn clone(&self) -> Self {
            Self {
                ptr: WeakShared::clone(&self.ptr),
                parent_borrow_count: Arc::clone(&self.parent_borrow_count),
            }
        }
    }

    impl<T: 'static> ReadOnlyBorrowedObject<T> {
        pub(crate) fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: StandardShared::new(self),
            }
        }
    }

    #[cfg(feature = "sync")]
    unsafe impl<T> Send for ReadOnlyBorrowedObject<T> {}
    #[cfg(feature = "sync")]
    unsafe impl<T> Sync for ReadOnlyBorrowedObject<T> {}

    type BorrowFlag = isize;
    const UNUSED: BorrowFlag = 0;

    #[inline(always)]
    fn is_writing(x: BorrowFlag) -> bool {
        x < UNUSED
    }

    #[inline(always)]
    fn is_reading(x: BorrowFlag) -> bool {
        x > UNUSED
    }

    pub struct BorrowedObject<T> {
        pub(crate) ptr: WeakSharedMut<*mut T>,
        pub(crate) parent_borrow_flag: Arc<AtomicBool>,
        pub(crate) child_borrow_flag: Arc<AtomicBool>,
        pub(crate) borrow_count: Arc<Mutex<BorrowFlag>>,
    }

    impl<T> Drop for BorrowedObject<T> {
        fn drop(&mut self) {
            // We're not borrowing anymore, so we can do this
            self.parent_borrow_flag
                .store(false, core::sync::atomic::Ordering::SeqCst);
        }
    }

    pub(crate) fn increment_borrow_flag(value: &Arc<Mutex<BorrowFlag>>) {
        let mut guard = value.lock().unwrap();
        *guard += 1;
    }

    impl<T> BorrowedObject<T> {
        pub(crate) fn new(ptr: WeakSharedMut<*mut T>) -> Self {
            Self {
                ptr,
                parent_borrow_flag: Arc::new(AtomicBool::new(false)),
                child_borrow_flag: Arc::new(AtomicBool::new(false)),
                borrow_count: Arc::new(Mutex::new(0)),
            }
        }

        pub(crate) fn with_parent_flag(mut self, parent_borrow_flag: Arc<AtomicBool>) -> Self {
            self.parent_borrow_flag = parent_borrow_flag;

            self
        }
    }

    impl SteelVal {
        pub(crate) fn get_borrow_flag_if_borrowed_object<T: AsRefMutSteelValFromRef + 'static>(
            &self,
        ) -> crate::rvals::Result<Arc<AtomicBool>> {
            if let SteelVal::Reference(v) = self {
                let res = v.inner.as_any_ref();

                if res.is::<BorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<BorrowedObject<T>>().unwrap();

                    Ok(Arc::clone(&borrowed_object.child_borrow_flag))
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        self,
                        core::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    self,
                    core::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }

        pub(crate) fn get_borrow_count_if_borrowed_object<T: AsRefMutSteelValFromRef + 'static>(
            &self,
        ) -> crate::rvals::Result<Arc<Mutex<BorrowFlag>>> {
            if let SteelVal::Reference(v) = self {
                let res = v.inner.as_any_ref();

                if res.is::<BorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<BorrowedObject<T>>().unwrap();

                    Ok(Arc::clone(&borrowed_object.borrow_count))
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        self,
                        core::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    self,
                    core::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }
    }

    impl<T> CustomReference for BorrowedObject<T> {}

    impl<T> Clone for BorrowedObject<T> {
        fn clone(&self) -> Self {
            Self {
                ptr: WeakShared::clone(&self.ptr),
                parent_borrow_flag: Arc::clone(&self.parent_borrow_flag),
                child_borrow_flag: Arc::clone(&self.child_borrow_flag),
                borrow_count: Arc::clone(&self.borrow_count),
            }
        }
    }

    impl<T: 'static> BorrowedObject<T> {
        pub(crate) fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: StandardShared::new(self),
            }
        }
    }

    #[cfg(feature = "sync")]
    unsafe impl<T> Send for BorrowedObject<T> {}
    #[cfg(feature = "sync")]
    unsafe impl<T> Sync for BorrowedObject<T> {}

    pub(crate) trait Opaque {}

    // impl<T> Opaque for Shared<MutContainer<T>> {}
    impl<T> Opaque for StandardShared<MutContainer<T>> {}

    // TODO: Use this to chain multiple references together. The engine should be able to accept something
    // `with_reference` and then have the value be scoped to that lifetime.

    // #[cfg(not(feature = "sync"))]
    #[derive(Clone, Default)]
    pub(crate) struct OpaqueReferenceNursery {
        // memory: Rc<RefCell<Vec<OpaqueReference<'static>>>>,
        memory: Shared<MutContainer<Vec<Box<dyn Opaque>>>>,
        weak_values: Shared<MutContainer<Vec<OpaqueReference<'static>>>>,
    }

    // #[cfg(feature = "sync")]
    // #[derive(Clone, Default)]
    // pub(crate) struct OpaqueReferenceNursery {
    //     // memory: Rc<RefCell<Vec<OpaqueReference<'static>>>>,
    //     memory: Shared<MutContainer<Vec<Box<dyn Opaque>>>>,
    //     weak_values: Shared<MutContainer<Vec<OpaqueReference<'static>>>>,
    // }

    // #[cfg(feature = "sync")]
    // unsafe impl Sync for OpaqueReferenceNursery {}
    // #[cfg(feature = "sync")]
    // unsafe impl Send for OpaqueReferenceNursery {}

    // #[cfg(feature = "sync")]
    // static STATIC_NURSERY: Lazy<std::sync::Mutex<HashMap<EngineId, OpaqueReferenceNursery>>> =
    //     Lazy::new(|| std::sync::Mutex::new(HashMap::new()));

    // #[cfg(feature = "sync")]
    // static NURSERY: Lazy<OpaqueReferenceNursery> = Lazy::new(|| OpaqueReferenceNursery::new());

    // TODO: This needs to be like, engine-id -> nursery?

    // #[cfg(not(feature = "sync"))]

    // TODO: Think about if this makes sense, of it we need to move to something that isn't
    // thread local. I _think_ this makes complete sense, given that the main thread of execution
    // is not going to be moved across threads. Nothing ever references this?
    thread_local! {
        static NURSERY: OpaqueReferenceNursery = OpaqueReferenceNursery::new();
    }

    impl OpaqueReferenceNursery {
        const DEFAULT_CAPACITY: usize = 8;

        fn new() -> Self {
            Self {
                memory: Shared::new(MutContainer::new(Vec::with_capacity(
                    Self::DEFAULT_CAPACITY,
                ))),
                weak_values: Shared::new(MutContainer::new(Vec::with_capacity(
                    Self::DEFAULT_CAPACITY,
                ))),
            }
        }

        pub fn tie_lifetime<'a, T>(_: &'a mut T) -> NurseryAccessToken<'a> {
            NurseryAccessToken {
                phantom: PhantomData,
            }
        }

        // pub(crate) fn allocate_ro_object<T: 'static>

        // Safety - these absolutely need to be the same type.
        pub(crate) fn allocate_rw_object<'a, T: 'a, EXT: 'static>(obj: &mut T) {
            let erased = obj as *mut _;

            let erased = unsafe { core::mem::transmute::<*mut T, *mut EXT>(erased) };

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let borrowed = BorrowedObject::new(weak_ptr);

            // #[cfg(not(feature = "sync"))]
            // {
            NURSERY.with(|x| x.memory.write().push(Box::new(wrapped)));
            NURSERY.with(|x| x.weak_values.write().push(borrowed.into_opaque_reference()));
            // }

            // #[cfg(feature = "sync")]
            // {
            //     if let Some(nursery) = STATIC_NURSERY.lock().unwrap().get(&id) {
            //         nursery.memory.write().push(Box::new(wrapped));
            //         nursery
            //             .weak_values
            //             .write()
            //             .push(borrowed.into_opaque_reference());
            //     } else {
            //         let mut nursery = OpaqueReferenceNursery::new();
            //         nursery.memory.write().push(Box::new(wrapped));
            //         nursery
            //             .weak_values
            //             .write()
            //             .push(borrowed.into_opaque_reference());
            //     }
            // }
        }

        pub(crate) fn allocate_ro_object<'a, T: 'a, EXT: 'static>(obj: &T) {
            let erased = obj as *const _;

            let erased = unsafe { core::mem::transmute::<*const T, *const EXT>(erased) };

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = StandardShared::new(MutContainer::new(erased));
            let weak_ptr = StandardShared::downgrade(&wrapped);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, Arc::new(Mutex::new(0)));
            // let borrowed = ReadOnlyBorrowedObject::new(weak_ptr);

            // #[cfg(feature = "sync")]
            // {
            //     if let Some(nursery) = STATIC_NURSERY.lock().unwrap().get(&id) {
            //         nursery.memory.write().push(Box::new(wrapped));
            //         nursery
            //             .weak_values
            //             .write()
            //             .push(borrowed.into_opaque_reference());
            //     } else {
            //         let mut nursery = OpaqueReferenceNursery::new();
            //         nursery.memory.write().push(Box::new(wrapped));
            //         nursery
            //             .weak_values
            //             .write()
            //             .push(borrowed.into_opaque_reference());
            //     }
            // }

            // #[cfg(not(feature = "sync"))]
            // {
            NURSERY.with(|x| x.memory.write().push(Box::new(wrapped)));

            // TODO: Consider doing the transmute in the into_opaque_reference function,
            // to extend the lifetime, but nowhere else. Could save passing in the double types.
            NURSERY.with(|x| x.weak_values.write().push(borrowed.into_opaque_reference()));
            // }
        }

        pub(crate) fn allocate(obj: OpaqueReference<'static>) {
            NURSERY.with(|x| x.weak_values.write().push(obj));
        }

        pub(crate) fn free_all() {
            NURSERY.with(|x| x.memory.write().clear());
            NURSERY.with(|x| x.weak_values.write().clear());
        }

        pub(crate) fn drain_weak_references_to_steelvals() -> Vec<SteelVal> {
            let res = NURSERY.with(|x| {
                x.weak_values
                    .write()
                    .drain(..)
                    .map(Gc::new)
                    .map(SteelVal::Reference)
                    .collect()
            });

            // NURSERY.with(|x| x.memory.borrow_mut().clear());

            res
        }
    }

    pub struct NurseryAccessToken<'a> {
        phantom: PhantomData<&'a ()>,
    }

    impl<'a> NurseryAccessToken<'a> {
        pub fn allocate(obj: OpaqueReference<'static>) {
            OpaqueReferenceNursery::allocate(obj)
        }
    }

    impl<'a> Drop for NurseryAccessToken<'a> {
        fn drop(&mut self) {
            OpaqueReferenceNursery::free_all()
        }
    }

    // impl<T> BorrowedObject {
    //     pub fn borrow_mut()
    // }

    #[derive(Clone)]
    pub struct OpaqueReference<'a> {
        #[cfg(not(feature = "sync"))]
        inner: Shared<dyn ReferenceCustomType + 'a>,
        #[cfg(feature = "sync")]
        inner: StandardShared<dyn ReferenceCustomType + 'a + Send + Sync>,
    }

    impl OpaqueReference<'static> {
        pub(crate) fn format(&self) -> core::result::Result<String, core::fmt::Error> {
            self.display()
        }

        pub(crate) fn drop_mut(&mut self, drop_handler: &mut IterativeDropHandler) {
            if let Some(inner) = StandardShared::get_mut(&mut self.inner) {
                inner.drop_mut(drop_handler);
            }
        }
    }

    impl CustomReference for OpaqueReference<'static> {}

    pub(crate) struct TemporaryMutableView<T> {
        view: StandardSharedMut<*mut T>,
    }

    impl<T> TemporaryMutableView<T> {
        pub(crate) fn as_mut(&mut self) -> MappedScopedWriteContainer<'_, T> {
            ScopedWriteContainer::map(self.view.write(), |x| unsafe { &mut (**x) })
        }
    }

    pub(crate) enum TemporaryReadonlyView<T> {
        Standard(StandardSharedMut<*const T>),
        Slim(StandardShared<T>),
    }

    impl<T> TemporaryReadonlyView<T> {
        pub(crate) fn as_ro(&self) -> &T {
            match self {
                TemporaryReadonlyView::Standard(rw_lock) => unsafe { &(**rw_lock.read()) },
                TemporaryReadonlyView::Slim(x) => x,
            }
        }
    }

    impl<T: ReferenceCustomType + 'static> AsRefMutSteelValFromRef for T {
        fn as_mut_ref_from_ref(val: &SteelVal) -> crate::rvals::Result<TemporaryMutableView<T>> {
            if let SteelVal::Reference(v) = val {
                let res = v.inner.as_any_ref();

                if res.is::<BorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<BorrowedObject<T>>().unwrap();

                    if *borrowed_object.borrow_count.lock().unwrap() > 0
                        || borrowed_object
                            .child_borrow_flag
                            .load(core::sync::atomic::Ordering::SeqCst)
                    {
                        stop!(Generic => "Value is already borrowed!")
                    }

                    let guard = borrowed_object.ptr.upgrade().ok_or_else(
                        throw!(Generic => "opaque reference pointer dropped before use!"),
                    )?;

                    Ok(TemporaryMutableView { view: guard })
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        val,
                        core::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    val,
                    core::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }
    }

    impl<T: ReferenceCustomType + 'static> AsRefSteelValFromRef for T {
        fn as_ref_from_ref(val: &SteelVal) -> crate::rvals::Result<TemporaryReadonlyView<T>> {
            if let SteelVal::Reference(v) = val {
                let res = v.inner.as_any_ref();

                // TODO: Flatten these conversions - we're doing 2 checks when we only need to do one.
                if res.is::<ReadOnlyBorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<ReadOnlyBorrowedObject<T>>().unwrap();

                    let guard = borrowed_object.ptr.upgrade().ok_or_else(
                        throw!(Generic => "opaque reference pointer dropped before use!"),
                    )?;

                    Ok(TemporaryReadonlyView::Standard(guard))
                } else if res.is::<ReadOnlyTemporary<T>>() {
                    let borrowed_object = res.downcast_ref::<ReadOnlyTemporary<T>>().unwrap();

                    let guard = borrowed_object.ptr.upgrade().ok_or_else(
                        throw!(Generic => "opaque reference pointer dropped before use!"),
                    )?;

                    Ok(TemporaryReadonlyView::Slim(guard))
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        val,
                        core::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    val,
                    core::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }
    }

    pub fn is_reference_type<T: ReferenceCustomType + 'static>(value: &SteelVal) -> bool {
        if let SteelVal::Reference(v) = value {
            let res = v.inner.as_any_ref();
            if res.is::<ReadOnlyBorrowedObject<T>>() {
                true
            } else if res.is::<ReadOnlyTemporary<T>>() {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    #[test]
    fn test() {
        #[derive(Debug)]
        struct FooBar {
            baz: String,
        }

        struct Baz<'a> {
            foo_bar: &'a mut FooBar,
        }

        impl FooBar {
            fn append_str(&mut self, suffix: &str) {
                self.baz.push_str(suffix);
            }
        }
        let mut nursery = MutableReferenceNursery::new();

        let mut object = FooBar {
            baz: "hello world!".into(),
        };

        let mut baz = Baz {
            foo_bar: &mut object,
        };

        // HAS to be around the whole time
        nursery
            .retain_reference(&mut baz, |erased| unsafe {
                let guard = erased.ptr.upgrade().unwrap();

                let ref_mut: &mut Baz = &mut *(*guard.write());

                ref_mut.foo_bar.append_str("bananas");

                Ok(SteelVal::Void)
            })
            .unwrap();

        object.append_str("foobar");

        dbg!(object);
    }
}
