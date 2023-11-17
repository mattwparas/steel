use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::stop;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{ffi::OsStr, fmt};
use std::{ops::Deref, rc::Weak};

pub static OBJECT_COUNT: AtomicUsize = AtomicUsize::new(0);
pub(crate) static MAXIMUM_OBJECTS: usize = 50000;

// TODO: Make these available to be
// type Shared<T> = std::rc::Rc<T>;
// type SharedMut<T> = std::rc::Rc<std::cell::RefCell<T>>;

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
pub struct Gc<T: ?Sized>(pub(crate) Rc<T>);

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

    pub fn try_unwrap(self) -> Result<T, Gc<T>> {
        Rc::try_unwrap(self.0).map_err(|x| Gc(x))
    }

    pub fn strong_count(this: &Self) -> usize {
        Rc::strong_count(&this.0)
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
pub mod unsafe_roots {

    use super::Gc;
    use std::ptr::NonNull;

    #[derive(Clone)]
    pub enum MaybeRooted<T> {
        Rooted(Rooted<T>),
        Reference(Gc<T>),
    }

    impl<T: std::fmt::Debug> std::fmt::Debug for MaybeRooted<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

    impl<T> std::ops::Deref for MaybeRooted<T> {
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

    use std::cell::Cell;
    use std::rc::{Rc, Weak};
    use std::{any::Any, cell::RefCell, marker::PhantomData};

    use crate::rvals::cycles::IterativeDropHandler;
    use crate::rvals::AsRefSteelValFromRef;
    use crate::{rerrs::ErrorKind, rvals::AsRefMutSteelValFromRef, SteelErr, SteelVal};

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
            let wrapped = Rc::new(RefCell::new(erased));
            let weak_ptr = Rc::downgrade(&wrapped);

            let borrowed = BorrowedObject::new(weak_ptr);

            let erased2 = original_b as *mut _;
            let wrapped2 = Rc::new(RefCell::new(erased2));
            let weak_ptr2 = Rc::downgrade(&wrapped2);

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
            let wrapped = Rc::new(RefCell::new(erased));
            let weak_ptr = Rc::downgrade(&wrapped);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, Rc::new(Cell::new(0)));

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
            let wrapped = Rc::new(RefCell::new(erased));
            let weak_ptr = Rc::downgrade(&wrapped);

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

    pub fn type_id<'a, T>() -> std::any::TypeId
    where
        T: ReferenceMarker<'a>,
        T::Static: Any,
    {
        std::any::TypeId::of::<T::Static>()
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
            std::any::type_name::<Self>()
        }
        fn display(&self) -> std::result::Result<String, std::fmt::Error> {
            Ok(format!("#<{}>", self.name().to_string()))
        }
        fn visit(&self) {}
        fn drop_mut(&mut self, drop_handler: &mut IterativeDropHandler) {}
    }

    impl<'a, T: CustomReference + 'static> ReferenceCustomType for T {
        fn as_any_ref(&self) -> &dyn Any {
            self as &dyn Any
        }
        fn as_any_ref_mut(&mut self) -> &mut dyn Any {
            self as &mut dyn Any
        }
        fn display(&self) -> std::result::Result<String, std::fmt::Error> {
            // if let Some(formatted) = self.fmt() {
            //     formatted
            // } else {
            Ok(format!("#<{}>", self.name().to_string()))
            // }
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
        pub(crate) ptr: Rc<RefCell<*mut T>>,
    }

    // TODO: Probably combine this and the above
    pub(crate) struct ReadOnlyTemporaryObject<T> {
        pub(crate) ptr: Rc<RefCell<*const T>>,
    }

    // Not a reference explicitly. This might contain a reference, but on its own it is a
    // value type. This should probably only deal with immutable references for now.
    pub(crate) struct Temporary<T> {
        pub(crate) ptr: Rc<T>,
    }

    impl<T> CustomReference for Temporary<T> {}
    impl<T> CustomReference for TemporaryObject<T> {}
    impl<T> CustomReference for ReadOnlyTemporaryObject<T> {}

    impl<T: 'static> TemporaryObject<T> {
        pub fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: Rc::new(self),
            }
        }
    }

    impl<T: 'static> ReadOnlyTemporaryObject<T> {
        pub fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: Rc::new(self),
            }
        }
    }

    impl<T: 'static> Temporary<T> {
        pub fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: Rc::new(self),
            }
        }
    }

    pub struct ReadOnlyTemporary<T> {
        pub(crate) ptr: Weak<T>,
    }

    impl<T> CustomReference for ReadOnlyTemporary<T> {}

    impl<T> Clone for ReadOnlyTemporary<T> {
        fn clone(&self) -> Self {
            Self {
                ptr: Weak::clone(&self.ptr),
            }
        }
    }

    impl<T: 'static> ReadOnlyTemporary<T> {
        pub fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: Rc::new(self),
            }
        }
    }

    pub struct ReadOnlyBorrowedObject<T> {
        pub(crate) ptr: Weak<RefCell<*const T>>,
        pub(crate) parent_borrow_count: Rc<Cell<BorrowFlag>>,
    }

    impl<T> CustomReference for ReadOnlyBorrowedObject<T> {}

    impl<T> ReadOnlyBorrowedObject<T> {
        pub fn new(
            ptr: Weak<RefCell<*const T>>,
            parent_borrow_count: Rc<Cell<BorrowFlag>>,
        ) -> Self {
            Self {
                ptr,
                parent_borrow_count,
            }
        }
    }

    impl<T> Drop for ReadOnlyBorrowedObject<T> {
        fn drop(&mut self) {
            self.parent_borrow_count
                .set(self.parent_borrow_count.get() - 1);
        }
    }

    impl<T> Clone for ReadOnlyBorrowedObject<T> {
        fn clone(&self) -> Self {
            Self {
                ptr: Weak::clone(&self.ptr),
                parent_borrow_count: Rc::clone(&self.parent_borrow_count),
            }
        }
    }

    impl<T: 'static> ReadOnlyBorrowedObject<T> {
        pub fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: Rc::new(self),
            }
        }
    }

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
        pub(crate) ptr: Weak<RefCell<*mut T>>,
        // TODO: This might need to just be a direct reference to the parent?
        pub(crate) parent_borrow_flag: Rc<Cell<bool>>,
        pub(crate) child_borrow_flag: Rc<Cell<bool>>,
        // TODO:
        // This really should be the way to do things...
        pub(crate) borrow_count: Rc<Cell<BorrowFlag>>,
    }

    impl<T> Drop for BorrowedObject<T> {
        fn drop(&mut self) {
            // We're not borrowing anymore, so we can do this
            self.parent_borrow_flag.set(false);
        }
    }

    impl<T> BorrowedObject<T> {
        pub fn new(ptr: Weak<RefCell<*mut T>>) -> Self {
            Self {
                ptr,
                parent_borrow_flag: Rc::new(Cell::new(false)),
                child_borrow_flag: Rc::new(Cell::new(false)),
                borrow_count: Rc::new(Cell::new(0)),
            }
        }

        pub fn with_parent_flag(mut self, parent_borrow_flag: Rc<Cell<bool>>) -> Self {
            self.parent_borrow_flag = parent_borrow_flag;

            self
        }
    }

    impl SteelVal {
        pub(crate) fn get_borrow_flag_if_borrowed_object<T: AsRefMutSteelValFromRef + 'static>(
            &self,
        ) -> crate::rvals::Result<Rc<Cell<bool>>> {
            if let SteelVal::Reference(v) = self {
                let res = v.inner.as_any_ref();

                if res.is::<BorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<BorrowedObject<T>>().unwrap();

                    Ok(Rc::clone(&borrowed_object.child_borrow_flag))
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        self,
                        std::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    self,
                    std::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }

        pub(crate) fn get_borrow_count_if_borrowed_object<T: AsRefMutSteelValFromRef + 'static>(
            &self,
        ) -> crate::rvals::Result<Rc<Cell<BorrowFlag>>> {
            if let SteelVal::Reference(v) = self {
                let res = v.inner.as_any_ref();

                if res.is::<BorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<BorrowedObject<T>>().unwrap();

                    Ok(Rc::clone(&borrowed_object.borrow_count))
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        self,
                        std::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    self,
                    std::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }
    }

    impl<T> CustomReference for BorrowedObject<T> {}

    impl<'a, T> Clone for BorrowedObject<T> {
        fn clone(&self) -> Self {
            Self {
                ptr: Weak::clone(&self.ptr),
                parent_borrow_flag: Rc::clone(&self.parent_borrow_flag),
                child_borrow_flag: Rc::clone(&self.child_borrow_flag),
                borrow_count: Rc::clone(&self.borrow_count),
            }
        }
    }

    impl<T: 'static> BorrowedObject<T> {
        pub fn into_opaque_reference<'a>(self) -> OpaqueReference<'a> {
            OpaqueReference {
                inner: Rc::new(self),
            }
        }
    }

    pub(crate) trait Opaque {}

    impl<T> Opaque for Rc<RefCell<T>> {}

    // TODO: Use this to chain multiple references together. The engine should be able to accept something
    // `with_reference` and then have the value be scoped to that lifetime.
    #[derive(Clone, Default)]
    pub(crate) struct OpaqueReferenceNursery {
        // memory: Rc<RefCell<Vec<OpaqueReference<'static>>>>,
        memory: Rc<RefCell<Vec<Box<dyn Opaque>>>>,
        weak_values: Rc<RefCell<Vec<OpaqueReference<'static>>>>,
    }

    thread_local! {
        static NURSERY: OpaqueReferenceNursery = OpaqueReferenceNursery::new();
    }

    impl OpaqueReferenceNursery {
        const DEFAULT_CAPACITY: usize = 8;

        fn new() -> Self {
            Self {
                memory: Rc::new(RefCell::new(Vec::with_capacity(Self::DEFAULT_CAPACITY))),
                weak_values: Rc::new(RefCell::new(Vec::with_capacity(Self::DEFAULT_CAPACITY))),
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

            let erased = unsafe { std::mem::transmute::<*mut T, *mut EXT>(erased) };

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = Rc::new(RefCell::new(erased));
            let weak_ptr = Rc::downgrade(&wrapped);

            let borrowed = BorrowedObject::new(weak_ptr);

            NURSERY.with(|x| x.memory.borrow_mut().push(Box::new(wrapped)));
            NURSERY.with(|x| {
                x.weak_values
                    .borrow_mut()
                    .push(borrowed.into_opaque_reference())
            });
        }

        pub(crate) fn allocate_ro_object<'a, T: 'a, EXT: 'static>(obj: &T) {
            let erased = obj as *const _;

            let erased = unsafe { std::mem::transmute::<*const T, *const EXT>(erased) };

            // Wrap the original mutable pointer in an object that respects borrowing
            // rules for runtime borrow checking
            let wrapped = Rc::new(RefCell::new(erased));
            let weak_ptr = Rc::downgrade(&wrapped);

            let borrowed = ReadOnlyBorrowedObject::new(weak_ptr, Rc::new(Cell::new(0)));

            NURSERY.with(|x| x.memory.borrow_mut().push(Box::new(wrapped)));

            // TODO: Consider doing the transmute in the into_opaque_reference function,
            // to extend the lifetime, but nowhere else. Could save passing in the double types.
            NURSERY.with(|x| {
                x.weak_values
                    .borrow_mut()
                    .push(borrowed.into_opaque_reference())
            });
        }

        pub(crate) fn allocate(obj: OpaqueReference<'static>) {
            NURSERY.with(|x| x.weak_values.borrow_mut().push(obj));
        }

        pub(crate) fn free_all() {
            NURSERY.with(|x| x.memory.borrow_mut().clear());
            NURSERY.with(|x| x.weak_values.borrow_mut().clear());
        }

        pub(crate) fn drain_weak_references_to_steelvals() -> Vec<SteelVal> {
            let res = NURSERY.with(|x| {
                x.weak_values
                    .borrow_mut()
                    .drain(..)
                    .map(Rc::new)
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
        inner: Rc<dyn ReferenceCustomType + 'a>,
    }

    impl OpaqueReference<'static> {
        pub fn format(&self) -> std::result::Result<String, std::fmt::Error> {
            self.display()
        }

        pub fn drop_mut(&mut self, drop_handler: &mut IterativeDropHandler) {
            if let Some(inner) = Rc::get_mut(&mut self.inner) {
                inner.drop_mut(drop_handler);
            }
        }
    }

    impl CustomReference for OpaqueReference<'static> {}

    // TODO: Combine this and the next into 1 trait
    impl<T: ReferenceCustomType + 'static> AsRefMutSteelValFromRef for T {
        fn as_mut_ref_from_ref<'a>(val: &'a SteelVal) -> crate::rvals::Result<&'a mut T> {
            if let SteelVal::Reference(v) = val {
                let res = v.inner.as_any_ref();

                if res.is::<BorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<BorrowedObject<T>>().unwrap();

                    if borrowed_object.borrow_count.get() > 0
                        || borrowed_object.child_borrow_flag.get()
                    {
                        stop!(Generic => "Value is already borrowed!")
                    }

                    let guard = borrowed_object.ptr.upgrade().ok_or_else(
                        throw!(Generic => "opaque reference pointer dropped before use!"),
                    );

                    return guard.map(|x| unsafe { &mut *(*x.borrow_mut()) });
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        val,
                        std::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    val,
                    std::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
        }
    }

    impl<T: ReferenceCustomType + 'static> AsRefSteelValFromRef for T {
        fn as_ref_from_ref<'a>(val: &'a SteelVal) -> crate::rvals::Result<&'a T> {
            if let SteelVal::Reference(v) = val {
                let res = v.inner.as_any_ref();

                // TODO: Flatten these conversions - we're doing 2 checks when we only need to do one.
                if res.is::<ReadOnlyBorrowedObject<T>>() {
                    let borrowed_object = res.downcast_ref::<ReadOnlyBorrowedObject<T>>().unwrap();

                    // return Ok(borrowed_object.clone());

                    let guard = borrowed_object.ptr.upgrade().ok_or_else(
                        throw!(Generic => "opaque reference pointer dropped before use!"),
                    );

                    return guard.map(|x| unsafe { &*(*x.borrow()) });
                } else if res.is::<ReadOnlyTemporary<T>>() {
                    let borrowed_object = res.downcast_ref::<ReadOnlyTemporary<T>>().unwrap();

                    // return Ok(borrowed_object.clone());

                    let guard = borrowed_object.ptr.upgrade().ok_or_else(
                        throw!(Generic => "opaque reference pointer dropped before use!"),
                    );

                    // TODO: @Matt -> We really do not want to have this here
                    // The way to fix it is to have a separate trait, with a return type
                    // more akin to a an owned borrow of some kind (like Ref from borrow() on refcell)
                    // This is super suspect but we'll move on for now
                    return guard.map(|x| unsafe { &*(Rc::as_ptr(&x)) });
                } else {
                    let error_message = format!(
                        "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                        val,
                        std::any::type_name::<Self>()
                    );
                    Err(SteelErr::new(ErrorKind::ConversionError, error_message))
                }
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    val,
                    std::any::type_name::<Self>()
                );

                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
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
            baz: "hello world!".to_string(),
        };

        let mut baz = Baz {
            foo_bar: &mut object,
        };

        // HAS to be around the whole time
        nursery
            .retain_reference(&mut baz, |erased| unsafe {
                let guard = erased.ptr.upgrade().unwrap();

                let ref_mut: &mut Baz = &mut *(*guard.borrow_mut());

                ref_mut.foo_bar.append_str("bananas");

                Ok(SteelVal::Void)
            })
            .unwrap();

        object.append_str("foobar");

        dbg!(object);
    }
}
