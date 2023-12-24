use std::{
    borrow::Cow,
    cell::{Ref, RefCell, RefMut},
    marker::PhantomData,
    rc::Rc,
    sync::{Arc, Mutex},
};

use crate::{
    gc::{unsafe_erased_pointers::OpaqueReference, Gc},
    rerrs::ErrorKind,
    rvals::{
        as_underlying_type, Custom, CustomType, FutureResult, IntoSteelVal, Result, SRef,
        SerializableSteelVal, SteelHashMap, SteelVal,
    },
    values::functions::{BoxedDynFunction, StaticOrRcStr},
    SteelErr,
};

use abi_stable::{
    std_types::{RBoxError, RCowStr, RHashMap, RResult, RSlice, RStr, RString, RVec, Tuple2},
    StableAbi,
};
use futures_util::FutureExt;

pub use async_ffi::{FfiFuture, FutureExt as FfiFutureExt};

#[macro_export]
macro_rules! ffi_try {
    ($result:expr) => {
        match $result {
            RResult::ROk(v) => v,
            RResult::RErr(e) => return RResult::RErr(e),
        }
    };
}

/// This creates an external module this is _mostly_ safe.
#[repr(C)]
#[derive(StableAbi, Debug)]
pub struct FFIModule {
    // Name of the module - let this be set by the dylib
    name: RString,
    values: RHashMap<RString, FFIValue>,
    docs: RBox<FFIInternalDocumentation>,
}

impl FFIModule {
    pub fn new<T: Into<RString>>(name: T) -> Self {
        Self {
            name: name.into(),
            values: RHashMap::default(),
            docs: RBox::new(FFIInternalDocumentation::new()),
        }
    }

    pub fn register_value<T: Into<FFIValue>>(&mut self, name: &str, value: T) -> &mut Self {
        self.values.insert(RString::from(name), value.into());
        self
    }
}

// TODO: Wrap FFI Objects with this trait, have the object we pass around just be a normal one?
// Making sure that we are not referencing static things (or memory accessed in the other one?)

#[repr(C)]
#[derive(Clone)]
pub struct OpaqueFFIValue {
    pub name: RString,
    pub inner: Gc<RefCell<Box<dyn CustomType>>>,
}

// #[repr(C)]
// pub struct SendOpaqueFFIValue {
//     pub name: RString,
//     pub inner: RefCell<Box<dyn CustomType + Send>>,
// }

// impl Custom for SendOpaqueFFIValue {}

impl Custom for OpaqueFFIValue {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<{}>", self.name)))
    }

    // TODO: This is most likely, not correct. We're blindly taking the struct and now making
    // it thread safe.
    fn into_serializable_steelval(&mut self) -> Option<crate::rvals::SerializableSteelVal> {
        self.inner.borrow_mut().as_serializable_steelval()
    }
}

// Unfortunately we'll have to wrap the incoming reference in multiple layers of indirection
#[repr(C)]
#[derive(Clone)]
pub struct OpaqueFFIReference {
    pub name: RString,
    pub inner: OpaqueReference<'static>,
}

// Probably need to merge these together?
impl Custom for OpaqueFFIReference {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<{}>", self.name)))
    }
}

use crate::steel_vm::register_fn::SendSyncStatic;

pub trait RegisterFFIFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &str, func: FN) -> &mut Self;
}

pub trait IntoFFIVal: Sized {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError>;
}

// Probably can do a blanket impl here?
pub trait FromFFIVal: Sized {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError>;
}

fn ffi_error(message: Cow<'static, str>) -> RBoxError {
    let error: Box<dyn std::error::Error + Send + Sync> = message.into();

    RBoxError::from_box(error)
}

// FFI Conversions for primitives
macro_rules! conversion_error {
    ($name:tt, $val:expr) => {
        RResult::RErr(ffi_error(
            format!(
                "Value unable to be converted to {}: {:?}",
                stringify!($name),
                $val
            )
            .into(),
        ))
    };
}

impl<T: IntoFFIVal, E: IntoFFIVal> IntoFFIVal for std::result::Result<T, E> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        match self {
            Ok(v) => v.into_ffi_val(),
            Err(e) => {
                let error: Box<dyn std::error::Error + Send + Sync> =
                    format!("{:?}", ffi_try!(e.into_ffi_val())).into();

                RResult::RErr(RBoxError::from_box(error))
            }
        }
    }
}

impl<T: IntoFFIVal> IntoFFIVal for Option<T> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        match self {
            Some(value) => value.into_ffi_val(),
            None => RResult::ROk(FFIValue::BoolV(false)),
        }
    }
}

impl FromFFIVal for bool {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::BoolV(b) = val {
            RResult::ROk(b)
        } else {
            conversion_error!(boolean, val)
        }
    }
}

impl From<bool> for FFIValue {
    fn from(value: bool) -> Self {
        FFIValue::BoolV(value)
    }
}

// Blanket implementation
impl<T: Into<FFIValue>> IntoFFIVal for T {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        RResult::ROk(self.into())
    }
}

// use async_ffi::FutureExt as FFIFutureExt;
// impl<F: Future<Output = FFIValue>> IntoFFIVal for F {
//     fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
//         RResult::ROk(FFIValue::Future {
//             fut: self.into_ffi(),
//         })
//     }
// }

// impl<T: IntoFFIVal, F: Future<Output = T>> From<F> for FFIValue {
//     fn from(value: F) -> FFIValue {
//         todo!()
//     }
// }

impl From<FfiFuture<RResult<FFIValue, RBoxError>>> for FFIValue {
    fn from(value: FfiFuture<RResult<FFIValue, RBoxError>>) -> Self {
        FFIValue::Future { fut: value }
    }
}

impl FromFFIVal for String {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::StringV(s) = val {
            RResult::ROk(s.into_string())
        } else {
            conversion_error!(string, val)
        }
    }
}

impl From<String> for FFIValue {
    fn from(value: String) -> Self {
        FFIValue::StringV(RString::from(value))
    }
}

impl FromFFIVal for f64 {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::NumV(n) = val {
            RResult::ROk(n)
        } else {
            conversion_error!(f64, val)
        }
    }
}

impl From<f64> for FFIValue {
    fn from(value: f64) -> Self {
        FFIValue::NumV(value)
    }
}

// This is super spooky...
impl FromFFIVal for usize {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::IntV(i) = val {
            RResult::ROk(i as usize)
        } else {
            conversion_error!(isize, val)
        }
    }
}

impl FromFFIVal for isize {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::IntV(i) = val {
            RResult::ROk(i)
        } else {
            conversion_error!(isize, val)
        }
    }
}

impl From<isize> for FFIValue {
    fn from(value: isize) -> Self {
        FFIValue::IntV(value)
    }
}

impl FromFFIVal for () {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::Void = val {
            RResult::ROk(())
        } else {
            conversion_error!((), val)
        }
    }
}

impl From<()> for FFIValue {
    fn from(_: ()) -> Self {
        FFIValue::Void
    }
}

impl<T: FromFFIVal> FromFFIVal for Vec<T> {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::Vector(v) = val {
            let mut collected = Vec::with_capacity(v.len());

            for value in v {
                match T::from_ffi_val(value) {
                    RResult::ROk(v) => collected.push(v),
                    RResult::RErr(r) => return RResult::RErr(r),
                }
            }

            RResult::ROk(collected)
        } else {
            conversion_error!(Vec, val)
        }
    }
}

// TODO: I think we can get rid of the unnecessary cloning... maybe?
impl<T: Custom + Clone + 'static> FromFFIVal for T {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::Custom { custom } = &val {
            if let Some(underlying) = as_underlying_type::<T>(custom.inner.borrow().as_ref()) {
                return RResult::ROk(underlying.clone());
            }
        }
        conversion_error!(Opaque, val)
    }
}

// Convert this directly to the type we want on the way out
impl<T: CustomType + 'static> From<T> for FFIValue {
    fn from(value: T) -> Self {
        let name = RString::from(value.name());

        FFIValue::Custom {
            custom: OpaqueFFIValue {
                name,
                inner: Gc::new(RefCell::new(Box::new(value))),
            },
        }
    }
}

pub struct Wrapper<ARGS>(PhantomData<ARGS>);
pub struct WrapperRef<ARGS>(PhantomData<ARGS>);
pub struct WrapperMut<ARGS>(PhantomData<ARGS>);
pub struct WrapperMutRef<ARGS>(PhantomData<ARGS>);

pub trait AsRefFFIVal: Sized {
    type Nursery: Default;

    fn as_ref<'b, 'a: 'b>(
        val: &'a FFIValue,
        _nursery: &'a mut Self::Nursery,
    ) -> RResult<SRef<'b, Self>, RBoxError>;

    fn as_mut_ref<'b, 'a: 'b>(val: &'a FFIValue) -> RResult<RefMut<'b, Self>, RBoxError>;
}

impl<T: CustomType + 'static> AsRefFFIVal for T {
    type Nursery = ();

    fn as_ref<'b, 'a: 'b>(
        val: &'a FFIValue,
        _nursery: &'a mut Self::Nursery,
    ) -> RResult<SRef<'b, Self>, RBoxError> {
        if let FFIValue::Custom { custom } = val {
            let res = Ref::map(custom.inner.borrow(), |x| x.as_any_ref());

            if res.is::<T>() {
                return RResult::ROk(SRef::Owned(Ref::map(res, |x| {
                    x.downcast_ref::<T>().unwrap()
                })));
            }
        }

        return conversion_error!(OpaqueFFIValue, val);
    }

    fn as_mut_ref<'b, 'a: 'b>(val: &'a FFIValue) -> RResult<RefMut<'b, Self>, RBoxError> {
        if let FFIValue::Custom { custom } = val {
            let res = RefMut::map(custom.inner.borrow_mut(), |x| x.as_any_ref_mut());

            if res.is::<T>() {
                return RResult::ROk(RefMut::map(res, |x| x.downcast_mut::<T>().unwrap()));
            }
        }

        return conversion_error!(OpaqueFFIValue, val);
    }
}

impl From<RVec<FFIValue>> for FFIValue {
    fn from(value: RVec<FFIValue>) -> Self {
        FFIValue::Vector(value)
    }
}

impl<T: IntoFFIVal> IntoFFIVal for Vec<T> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        let mut output = RVec::with_capacity(self.len());

        for value in self {
            output.push(ffi_try!(value.into_ffi_val()));
        }

        RResult::ROk(FFIValue::Vector(output))
    }
}

impl IntoFFIVal for RResult<FFIValue, RBoxError> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        self
    }
}

// Implementing FFI value conversion layers...
impl<RET: IntoFFIVal, SELF: AsRefFFIVal, FN: Fn(&SELF) -> RET + SendSyncStatic>
    RegisterFFIFn<FN, WrapperRef<(SELF,)>, RET> for FFIModule
{
    fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
        let f = move |args: RVec<FFIValue>| -> RResult<FFIValue, RBoxError> {
            if args.len() != 1 {
                let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                let rbox = RBoxError::from_box(error);

                return RResult::RErr(rbox);
            }

            let mut arg_iter = args.into_iter();
            let rarg1 = arg_iter.next().unwrap();
            let mut nursery = <SELF::Nursery>::default();

            let arg1 = ffi_try!(<SELF>::as_ref(&rarg1, &mut nursery));

            let res = func(&arg1);

            res.into_ffi_val()
        };

        self.register_value(
            name,
            FFIValue::BoxedFunction(FFIBoxedDynFunction {
                name: RString::from(name),
                arity: 0,
                function: Arc::new(f),
            }),
        );

        self
    }
}

macro_rules! impl_register_fn_ffi {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            $($param: FromFFIVal,)*
            FN: Fn($($param),*) -> RET + SendSyncStatic,
            RET: IntoFFIVal
        > RegisterFFIFn<FN, Wrapper<($($param,)*)>, RET> for FFIModule {
            fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
                let f = move |args: RVec<FFIValue>| -> RResult<FFIValue, RBoxError> {
                    if args.len() != $arg_count {
                        let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                        let rbox = RBoxError::from_box(error);

                        return RResult::RErr(rbox);
                    }

                    let mut arg_iter = args.into_iter();
                    let res = func($(ffi_try!(<$param>::from_ffi_val(arg_iter.next().unwrap())),)*);

                    // let res = func($({
                    //     ffi_try!(<$param>::from_ffi_val());
                    // },)*);

                    res.into_ffi_val()
                };

                self.register_value(
                    name,
                    FFIValue::BoxedFunction(FFIBoxedDynFunction {
                        name: RString::from(name),
                        arity: 0,
                        function: Arc::new(f),
                    }),
                );

                self
            }
        }


        impl<RET: IntoFFIVal, SELF: AsRefFFIVal, $($param: FromFFIVal,)* FN: Fn(&mut SELF, $($param),*) -> RET + SendSyncStatic>
            RegisterFFIFn<FN, WrapperMutRef<(SELF, $($param,)*)>, RET> for FFIModule
        {
            fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
                let f = move |args: RVec<FFIValue>| -> RResult<FFIValue, RBoxError> {
                    if  args.len() != $arg_count + 1 {
                        let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                        let rbox = RBoxError::from_box(error);

                        return RResult::RErr(rbox);
                    }

                    let mut arg_iter = args.into_iter();
                    let rarg1 = arg_iter.next().unwrap();

                    let mut arg1 = ffi_try!(<SELF>::as_mut_ref(&rarg1));

                    let res = func(&mut arg1, $(ffi_try!(<$param>::from_ffi_val(arg_iter.next().unwrap())),)*);

                    res.into_ffi_val()
                };

                self.register_value(
                    name,
                    FFIValue::BoxedFunction(FFIBoxedDynFunction {
                        name: RString::from(name),
                        arity: 0,
                        function: Arc::new(f),
                    }),
                );

                self
            }
        }


        impl<RET: IntoFFIVal, SELF: AsRefFFIVal, $($param: FromFFIVal,)* FN: Fn(&SELF, $($param),*) -> RET + SendSyncStatic>
            RegisterFFIFn<FN, WrapperRef<(SELF, $($param,)*)>, RET> for FFIModule
        {
            fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
                let f = move |args: RVec<FFIValue>| -> RResult<FFIValue, RBoxError> {
                    if  args.len() != $arg_count + 1 {
                        let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                        let rbox = RBoxError::from_box(error);

                        return RResult::RErr(rbox);
                    }

                    let mut arg_iter = args.into_iter();
                    let rarg1 = arg_iter.next().unwrap();

                    let mut arg1 = ffi_try!(<SELF>::as_mut_ref(&rarg1));

                    let res = func(&mut arg1, $(ffi_try!(<$param>::from_ffi_val(arg_iter.next().unwrap())),)*);

                    res.into_ffi_val()
                };

                self.register_value(
                    name,
                    FFIValue::BoxedFunction(FFIBoxedDynFunction {
                        name: RString::from(name),
                        arity: 0,
                        function: Arc::new(f),
                    }),
                );

                self
            }
        }
    }
}

impl_register_fn_ffi!(1 => A:0);
impl_register_fn_ffi!(2 => A:0, B:1);
impl_register_fn_ffi!(3 => A:0, B:1, C:2);
impl_register_fn_ffi!(4 => A:0, B:1, C:2, D:3);
impl_register_fn_ffi!(5 => A:0, B:1, C:2, D:3, E:4);
impl_register_fn_ffi!(6 => A:0, B:1, C:2, D:3, E:4, F:5);
impl_register_fn_ffi!(7 => A:0, B:1, C:2, D:3, E:4, F:5, G:6);
impl_register_fn_ffi!(8 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7);
impl_register_fn_ffi!(9 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8);
impl_register_fn_ffi!(10 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9);
impl_register_fn_ffi!(11 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10);
impl_register_fn_ffi!(12 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11);
impl_register_fn_ffi!(13 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12);
impl_register_fn_ffi!(14 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13);
impl_register_fn_ffi!(15 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 13, O: 14);
impl_register_fn_ffi!(16 => A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M: 12, N: 14, O: 14, P: 15);

// Looks like we can dispatch on the &mut SELF vs &SELF if the Wrapper type is different!
impl<RET: IntoFFIVal, SELF: AsRefFFIVal, FN: Fn(&mut SELF) -> RET + SendSyncStatic>
    RegisterFFIFn<FN, WrapperMut<(SELF,)>, RET> for FFIModule
{
    fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
        let f = move |args: RVec<FFIValue>| -> RResult<FFIValue, RBoxError> {
            if args.len() != 1 {
                let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                let rbox = RBoxError::from_box(error);

                return RResult::RErr(rbox);
            }

            let mut arg_iter = args.into_iter();
            let rarg1 = arg_iter.next().unwrap();

            let mut arg1 = ffi_try!(<SELF>::as_mut_ref(&rarg1));

            let res = func(&mut arg1);

            res.into_ffi_val()
        };

        self.register_value(
            name,
            FFIValue::BoxedFunction(FFIBoxedDynFunction {
                name: RString::from(name),
                arity: 0,
                function: Arc::new(f),
            }),
        );

        self
    }
}

// Implementing FFI value conversion layers...
impl<RET: IntoFFIVal, FN: Fn() -> RET + SendSyncStatic> RegisterFFIFn<FN, Wrapper<()>, RET>
    for FFIModule
{
    fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
        let f = move |args: RVec<FFIValue>| -> RResult<FFIValue, RBoxError> {
            if !args.is_empty() {
                let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                let rbox = RBoxError::from_box(error);

                return RResult::RErr(rbox);
            }

            let res = func();

            res.into_ffi_val()
        };

        self.register_value(
            name,
            FFIValue::BoxedFunction(FFIBoxedDynFunction {
                name: RString::from(name),
                arity: 0,
                function: Arc::new(f),
            }),
        );

        self
    }
}

pub use abi_stable::std_types::RBox;
pub use abi_stable::{export_root_module, prefix_type::PrefixTypeTrait, sabi_extern_fn};

use super::builtin::BuiltInModule;

#[macro_export]
macro_rules! declare_module {
    ($module_function:expr) => {
        #[::steel::steel_vm::ffi::export_root_module]
        pub fn get_library() -> ::steel::steel_vm::dylib::GenerateModule_Ref {
            use ::steel::steel_vm::ffi::PrefixTypeTrait;
            ::steel::steel_vm::dylib::GenerateModule { generate_module }.leak_into_prefix()
        }

        #[::steel::steel_vm::ffi::sabi_extern_fn]
        fn generate_module() -> ::steel::steel_vm::ffi::RBox<FFIModule> {
            ::steel::steel_vm::ffi::RBox::new($module_function())
        }
    };
}

/// Values that are safe to cross the FFI Boundary.
#[repr(C)]
#[derive(StableAbi)]
pub enum FFIValue {
    BoxedFunction(FFIBoxedDynFunction),
    BoolV(bool),
    NumV(f64),
    IntV(isize),
    Void,
    StringV(RString),
    Vector(RVec<FFIValue>),
    CharV {
        #[sabi(unsafe_opaque_field)]
        c: char,
    },
    // TODO: This is super dangerous, BUT it could work... restricting CustomType to have
    // the StableAbi is a bit of a deal breaker in terms of its application. With certain restrictions on the
    // generation of dylibs, this might be totally fine.
    Custom {
        #[sabi(unsafe_opaque_field)]
        custom: OpaqueFFIValue,
    },
    HashMap(RHashMap<FFIValue, FFIValue>),
    Future {
        #[sabi(unsafe_opaque_field)]
        fut: FfiFuture<RResult<FFIValue, RBoxError>>,
    }, // Future(Pin<RBox<dyn Future<Output = RResult<FFIValue, RBoxError>>>>),
}

impl std::hash::Hash for FFIValue {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl PartialEq for FFIValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::NumV(l), Self::NumV(r)) => l == r,
            (Self::BoolV(l), Self::BoolV(r)) => l == r,
            (Self::IntV(l), Self::IntV(r)) => l == r,
            (Self::StringV(l), Self::StringV(r)) => l == r,
            (Self::CharV { c: l }, Self::CharV { c: r }) => l == r,
            (Self::Void, Self::Void) => true,
            (Self::Vector(l), Self::Vector(r)) => l == r,
            (Self::HashMap(l), Self::HashMap(r)) => l == r,
            (_, _) => false,
        }
    }
}

impl Eq for FFIValue {}

impl std::fmt::Debug for FFIValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FFIValue::BoxedFunction(func) => write!(f, "{:?}", func),
            FFIValue::Custom { .. } => write!(f, "#<OpaqueFFIValue>"),
            FFIValue::BoolV(b) => write!(f, "{:?}", b),
            FFIValue::NumV(n) => write!(f, "{:?}", n),
            FFIValue::IntV(i) => write!(f, "{:?}", i),
            FFIValue::CharV { c } => write!(f, "{}", c),
            FFIValue::Void => write!(f, "#<void>"),
            FFIValue::StringV(s) => write!(f, "{}", s),
            FFIValue::Vector(v) => write!(f, "{:?}", v),
            FFIValue::HashMap(h) => write!(f, "{:?}", h),
            FFIValue::Future { .. } => write!(f, "#<future>"),
        }
    }
}

impl FFIValue {
    pub fn as_steelval(&self) -> Result<SteelVal> {
        match self {
            Self::BoxedFunction(b) => {
                Ok(SteelVal::BoxedFunction(Rc::new(b.as_boxed_dyn_function())))
            }
            Self::BoolV(b) => Ok(SteelVal::BoolV(*b)),
            Self::NumV(n) => Ok(SteelVal::NumV(*n)),
            Self::IntV(i) => Ok(SteelVal::IntV(*i)),
            Self::CharV { c } => Ok(SteelVal::CharV(*c)),
            Self::Void => Ok(SteelVal::Void),
            // TODO: I think this might clone the string, its also a little suspect
            Self::StringV(s) => Ok(SteelVal::StringV(s.to_string().into())),
            Self::Vector(v) => v
                .into_iter()
                .map(|x| x.as_steelval())
                .collect::<Result<_>>()
                .map(SteelVal::ListV),

            Self::HashMap(h) => h
                .into_iter()
                .map(|kv| {
                    let k = kv.0;
                    let v = kv.1;

                    let k = k.as_steelval()?;
                    let v = v.as_steelval()?;

                    Ok((k, v))
                })
                .collect::<Result<im_rc::HashMap<_, _>>>()
                .map(Gc::new)
                .map(SteelHashMap::from)
                .map(SteelVal::HashMapV),
            // FFIValue::Future { fut } => Ok(SteelVal::FutureV(Gc::new(Sharedfut.map(|x| {
            //     match x {
            //         RResult::ROk(v) => {
            //             v.
            //         }
            //         RResult::RErr(_) => {
            //             todo!()
            //         }
            //     }
            // })))
            _v => {
                todo!("Missing enum variant not accounted for during FFIValue -> SteelVal conversion!")
            }
        }
    }
}

impl IntoSteelVal for FFIValue {
    fn into_steelval(self) -> Result<SteelVal> {
        match self {
            Self::BoxedFunction(b) => Ok(SteelVal::BoxedFunction(Rc::new(b.into()))),
            Self::Custom { custom } => {
                Ok(SteelVal::Custom(Gc::new(RefCell::new(Box::new(custom)))))
            }
            Self::BoolV(b) => Ok(SteelVal::BoolV(b)),
            Self::NumV(n) => Ok(SteelVal::NumV(n)),
            Self::IntV(i) => Ok(SteelVal::IntV(i)),
            Self::CharV { c } => Ok(SteelVal::CharV(c)),
            Self::Void => Ok(SteelVal::Void),
            // TODO: I think this might clone the string, its also a little suspect
            Self::StringV(s) => Ok(SteelVal::StringV(s.into_string().into())),
            Self::Vector(v) => v
                .into_iter()
                .map(|x| x.into_steelval())
                .collect::<Result<_>>()
                .map(SteelVal::ListV),

            Self::HashMap(h) => h
                .into_iter()
                .map(|kv| {
                    let k = kv.0;
                    let v = kv.1;

                    let k = k.into_steelval()?;
                    let v = v.into_steelval()?;

                    Ok((k, v))
                })
                .collect::<Result<im_rc::HashMap<_, _>>>()
                .map(Gc::new)
                .map(SteelHashMap::from)
                .map(SteelVal::HashMapV),

            // Attempt to move this across the FFI Boundary... We'll see how successful it is.
            FFIValue::Future { fut } => Ok(SteelVal::FutureV(Gc::new(FutureResult::new(
                Box::pin(async {
                    fut.map(|x| match x {
                        RResult::ROk(v) => v.into_steelval(),
                        RResult::RErr(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
                    })
                    .await
                }),
            )))),
        }
    }
}

#[repr(C)]
#[derive(StableAbi, Clone)]
pub struct FFIBoxedDynFunction {
    pub name: RString,
    pub arity: usize,
    // TODO: See if theres a better option here
    #[sabi(unsafe_opaque_field)]
    pub function:
        Arc<dyn Fn(RVec<FFIValue>) -> RResult<FFIValue, RBoxError> + Send + Sync + 'static>,
}

impl std::fmt::Debug for FFIBoxedDynFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<{}:{}:{:p}>", self.name, self.arity, self.function)
    }
}

pub fn as_ffi_value(value: &SteelVal) -> Result<FFIValue> {
    match value {
        SteelVal::BoolV(b) => Ok(FFIValue::BoolV(*b)),
        SteelVal::IntV(i) => Ok(FFIValue::IntV(*i)),
        SteelVal::NumV(n) => Ok(FFIValue::NumV(*n)),
        SteelVal::CharV(c) => Ok(FFIValue::CharV { c: *c }),
        SteelVal::Void => Ok(FFIValue::Void),
        // We can really only look at values that were made from the FFI boundary.
        SteelVal::Custom(c) => {
            if let Some(c) = as_underlying_type::<OpaqueFFIValue>(c.borrow().as_ref()) {
                Ok(FFIValue::Custom { custom: c.clone() })
            } else {
                // TODO: Re-enable this, so that the check is back in
                // place. Otherwise, this could be dangerous?

                Ok(FFIValue::Custom {
                    custom: OpaqueFFIValue {
                        name: RString::from(c.borrow().name()),
                        inner: c.clone(),
                    },
                })

                // stop!(TypeMismatch => "This opaque type did not originate from an FFI boundary, and thus cannot be passed across it: {:?}", value);
            }
        }
        SteelVal::HashMapV(h) => h
            .iter()
            .map(|(key, value)| {
                let key = as_ffi_value(key)?;
                let value = as_ffi_value(value)?;

                Ok((key, value))
            })
            .collect::<Result<_>>()
            .map(FFIValue::HashMap),
        SteelVal::ListV(l) => l
            .into_iter()
            .map(as_ffi_value)
            .collect::<Result<_>>()
            .map(FFIValue::Vector),

        SteelVal::StringV(s) => Ok(FFIValue::StringV(s.as_str().into())),

        _ => {
            stop!(TypeMismatch => "Cannot proceed with the conversion from steelval to FFI Value. This will only succeed for a subset of values deemed as FFI-safe-enough: {:?}", value)
        }
    }
}

impl FFIBoxedDynFunction {
    fn as_boxed_dyn_function(&self) -> BoxedDynFunction {
        let name = self.name.to_string();

        let cloned_function = Arc::clone(&self.function);

        let function = move |args: &[SteelVal]| -> crate::rvals::Result<SteelVal> {
            // Convert the arguments to the FFI types before passing through
            let args = args
                .into_iter()
                .map(as_ffi_value)
                .collect::<Result<RVec<_>>>()?;

            // Don't use RSlice - use a vec, so that we can consume the values (don't have to clone)
            let result = (cloned_function)(args);

            match result {
                RResult::ROk(output) => output.into_steelval(),
                RResult::RErr(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
            }
        };

        BoxedDynFunction {
            name: Some(StaticOrRcStr::Owned(Arc::new(name))),
            arity: Some(self.arity),
            function: Arc::new(function),
        }
    }
}

// Wrap the function that we get from FFI into another instance.
impl From<FFIBoxedDynFunction> for BoxedDynFunction {
    fn from(value: FFIBoxedDynFunction) -> Self {
        let name = value.name.into_string();

        let function = move |args: &[SteelVal]| -> crate::rvals::Result<SteelVal> {
            // Convert the arguments to the FFI types before passing through
            let args = args
                .into_iter()
                .map(as_ffi_value)
                .collect::<Result<RVec<_>>>()?;

            let result = (value.function)(args);

            match result {
                RResult::ROk(output) => output.into_steelval(),
                RResult::RErr(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
            }
        };

        BoxedDynFunction {
            name: Some(StaticOrRcStr::Owned(Arc::new(name))),
            arity: Some(value.arity),
            function: Arc::new(function),
        }
    }
}

thread_local! {
    pub static FFI_MODULES: Arc<Mutex<Vec<RBox<FFIModule>>>> = Arc::new(Mutex::new(Vec::new()));
}

pub struct FFIWrappedModule {
    raw_module: RBox<FFIModule>,
    converted_module: BuiltInModule,
}

impl FFIWrappedModule {
    pub fn new(raw_module: RBox<FFIModule>) -> Result<Self> {
        let mut converted_module = BuiltInModule::new((&raw_module.name).to_string());

        for tuple in raw_module.values.iter() {
            let key = tuple.0;
            let value = tuple.1;

            converted_module.register_value(&key, value.as_steelval()?);
        }

        Ok(Self {
            raw_module,
            converted_module,
        })
    }

    pub fn build(self) -> BuiltInModule {
        // Extend the lifetime of this
        FFI_MODULES.with(|x| x.lock().unwrap().push(self.raw_module));

        self.converted_module
    }
}

#[repr(C)]
#[derive(Clone, Debug, StableAbi)]
pub struct FFIInternalDocumentation {
    definitions: RHashMap<RCowStr<'static>, FFIDocumentation<'static>>,
}

impl FFIInternalDocumentation {
    pub fn new() -> Self {
        Self {
            definitions: RHashMap::new(),
        }
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<RCowStr<'static>>,
        description: FFIDocumentation<'static>,
    ) {
        self.definitions.insert(definition.into(), description);
    }
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub enum FFIDocumentation<'a> {
    Function(FFIDocTemplate<'a>),
    Module(FFIModuleDoc<'a>),
    Value(FFIValueDoc<'a>),
    Markdown(FFIMarkdownDoc<'a>),
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIDocTemplate<'a> {
    pub signature: RStr<'a>,
    pub params: RSlice<'a, RStr<'a>>,
    pub description: RStr<'a>,
    pub examples: RSlice<'a, Tuple2<RStr<'a>, RStr<'a>>>,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIModuleDoc<'a> {
    pub name: RStr<'a>,
    pub description: RStr<'a>,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIValueDoc<'a> {
    pub name: RStr<'a>,
    pub description: RStr<'a>,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIMarkdownDoc<'a>(pub RStr<'a>);

impl<'a> From<FFIDocTemplate<'a>> for FFIDocumentation<'a> {
    fn from(val: FFIDocTemplate<'a>) -> Self {
        FFIDocumentation::Function(val)
    }
}

impl<'a> From<FFIModuleDoc<'a>> for FFIDocumentation<'a> {
    fn from(val: FFIModuleDoc<'a>) -> Self {
        FFIDocumentation::Module(val)
    }
}

impl<'a> From<FFIValueDoc<'a>> for FFIDocumentation<'a> {
    fn from(val: FFIValueDoc<'a>) -> Self {
        FFIDocumentation::Value(val)
    }
}

impl<'a> From<FFIMarkdownDoc<'a>> for FFIDocumentation<'a> {
    fn from(val: FFIMarkdownDoc<'a>) -> Self {
        FFIDocumentation::Markdown(val)
    }
}
