#![allow(non_local_definitions)]

use std::{
    borrow::Cow,
    io::BufReader,
    marker::PhantomData,
    sync::{Arc, Mutex},
};

use crate::{
    gc::{
        shared::{ScopedWriteContainer, ShareableMut},
        Gc,
    },
    rerrs::ErrorKind,
    rvals::{
        as_underlying_type_mut, Custom, CustomType, FutureResult, IntoSteelVal,
        MaybeSendSyncStatic, Result, SteelByteVector, SteelHashMap, SteelVal,
    },
    values::{
        functions::{BoxedDynFunction, StaticOrRcStr},
        port::SteelPort,
        SteelPortRepr,
    },
    SteelErr,
};

use abi_stable::{
    sabi_trait::TD_CanDowncast,
    std_types::{
        RArc, RBoxError, RCowStr, RHashMap, RResult, RSlice, RSliceMut, RStr, RString, RVec, Tuple2,
    },
    DynTrait, RMut, StableAbi,
};
use futures_util::FutureExt;

use crate::values::HashMap;
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

    pub fn register_type<T: CustomType + 'static>(
        &mut self,
        predicate_name: &'static str,
    ) -> &mut Self {
        self.register_fn(predicate_name, |value: FFIArg| {
            if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = value {
                as_underlying_ffi_type::<T>(custom.get_mut()).is_some()
            } else {
                false
            }
        })
    }

    pub fn bindings(&self) -> Vec<RString> {
        self.values.keys().cloned().collect()
    }

    pub fn emit_package<W: std::io::Write>(
        &self,
        name: &str,
        writer: &mut W,
    ) -> std::result::Result<(), std::io::Error> {
        let mut bindings = self.bindings();

        bindings.sort();

        writeln!(writer, r#"(#%require-dylib "{}" (only-in"#, name)?;

        for key in &bindings {
            writeln!(writer, "    {}", key)?;
        }

        writeln!(writer, "))")?;

        writeln!(writer, "(provide ")?;
        for key in &bindings {
            writeln!(writer, "    {}", key)?;
        }

        writeln!(writer, ")")?;

        Ok(())
    }

    pub fn emit_package_to_file(
        &self,
        name: &str,
        path: impl AsRef<std::path::Path>,
    ) -> std::io::Result<()> {
        let mut file = std::fs::File::create(path)?;

        self.emit_package(name, &mut file)
    }
}

#[abi_stable::sabi_trait]
pub trait RFn<'a, In, Out>: Sync + Send {
    fn call(&self, input: In) -> Out;
}
impl<'a, In: StableAbi + 'a, Out: StableAbi, F: Fn(In) -> Out + Send + Sync> RFn<'a, In, Out>
    for F
{
    fn call(&self, input: In) -> Out {
        (self)(input)
    }
}

#[cfg(feature = "sync")]
#[abi_stable::sabi_trait]
pub trait OpaqueObject: Send + Sync {
    fn obj_ffi_fmt(&self) -> RString;
}

#[cfg(not(feature = "sync"))]
#[abi_stable::sabi_trait]
pub trait OpaqueObject {
    fn obj_ffi_fmt(&self) -> RString;
}

// Blanket implement this for all things that implement Custom!
impl<T: Custom + MaybeSendSyncStatic> OpaqueObject for T {
    fn obj_ffi_fmt(&self) -> RString {
        self.fmt_ffi()
            .unwrap_or_else(|| format!("#<OpaqueFFIValue:{}>", std::any::type_name::<T>()).into())
    }
}

// TODO: Swap this implementation with the above.
#[repr(C)]
#[derive(StableAbi)]
pub struct OpaqueFFIValueReturn {
    pub inner: OpaqueObject_TO<'static, RBox<()>>,
}

impl Custom for OpaqueFFIValueReturn {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(self.inner.obj_ffi_fmt().into_string()))
    }
}

use crate::steel_vm::register_fn::SendSyncStatic;

pub trait RegisterFFIFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &str, func: FN) -> &mut Self;
}

pub trait IntoFFIVal: Sized {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError>;
}

pub trait IntoFFIArg: Sized {
    fn into_ffi_arg(&self) -> RResult<FFIArg, RBoxError>;
}

pub trait FromFFIArg<'a>: Sized {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError>;
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

impl<T: IntoFFIVal, E: IntoFFIVal + std::fmt::Debug> IntoFFIVal for std::result::Result<T, E> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        match self {
            Ok(v) => v.into_ffi_val(),
            Err(e) => {
                let error: Box<dyn std::error::Error + Send + Sync> = format!("{:?}", e).into();

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

impl<'a> FromFFIArg<'a> for bool {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        if let FFIArg::BoolV(b) = val {
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
        FFIValue::Future {
            fut: SyncFfiFuture { fut: value },
        }
    }
}

impl FromFFIVal for RString {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        if let FFIValue::StringV(s) = val {
            RResult::ROk(s)
        } else {
            conversion_error!(string, val)
        }
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

impl<'a> FromFFIArg<'a> for String {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        match val {
            FFIArg::StringV(s) => RResult::ROk(s.into_string()),
            FFIArg::StringRef(s) => RResult::ROk(s.to_string()),
            _ => conversion_error!(string, val),
        }
    }
}

impl<'a> FromFFIArg<'a> for &'a str {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        match val {
            FFIArg::StringRef(s) => RResult::ROk(s.as_str()),
            _ => conversion_error!(string, val),
        }
    }
}

impl<'a> FromFFIArg<'a> for RMut<'a, RString> {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        match val {
            FFIArg::StringMutRef(s) => RResult::ROk(s.string),
            _ => conversion_error!(string, val),
        }
    }
}

impl<'a> FromFFIArg<'a> for HostRuntimeFunction {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        match val {
            FFIArg::HostFunction(h) => RResult::ROk(h),
            _ => conversion_error!(runtime_function, val),
        }
    }
}

impl<'a> FromFFIArg<'a> for RSliceMut<'a, FFIValue> {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        if let FFIArg::VectorRef(v) = val {
            RResult::ROk(v.vec)
        } else {
            conversion_error!(vec_slice, val)
        }
    }
}

impl<'a> FromFFIArg<'a> for RVec<u8> {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        if let FFIArg::ByteVector(b) = val {
            RResult::ROk(b)
        } else {
            conversion_error!(bytevector, val)
        }
    }
}

impl<'a, T: Custom + Clone + 'static> FromFFIArg<'a> for T {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        let lifted = unsafe { std::mem::transmute::<FFIArg<'a>, FFIArg<'static>>(val) };
        match lifted {
            FFIArg::Custom { mut custom } => {
                let inner = as_underlying_ffi_type::<T>(&mut custom.inner);

                match inner {
                    Some(v) => RResult::ROk(v.clone()),
                    None => conversion_error!(custom, "#<opaque>"),
                }
            }

            FFIArg::CustomRef(mut custom) => {
                let inner = as_underlying_ffi_type::<T>(custom.custom.get_mut());

                match inner {
                    Some(v) => RResult::ROk(v.clone()),
                    None => conversion_error!(custom, "#<opaque>"),
                }
            }

            _ => conversion_error!(custom, lifted),
        }
    }
}

// TODO: Handle this properly for overflow
impl From<usize> for FFIValue {
    fn from(value: usize) -> Self {
        FFIValue::IntV(value as isize)
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

impl<'a> FromFFIArg<'a> for usize {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        if let FFIArg::IntV(i) = val {
            if i < 0 {
                conversion_error!(usize, val)
            } else {
                RResult::ROk(i as usize)
            }
        } else {
            conversion_error!(usize, val)
        }
    }
}

impl<'a> FromFFIArg<'a> for char {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        if let FFIArg::CharV { c } = val {
            RResult::ROk(c)
        } else {
            conversion_error!(usize, val)
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

impl<'a, T: FromFFIArg<'a>> FromFFIArg<'a> for Option<T> {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        match val {
            FFIArg::BoolV(false) => RResult::ROk(None),
            anything => T::from_ffi_arg(anything).map(Some),
        }
    }
}

impl<'a, T: FromFFIArg<'a>> FromFFIArg<'a> for Vec<T> {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        if let FFIArg::Vector(v) = val {
            let mut collected = Vec::with_capacity(v.len());

            for value in v {
                match T::from_ffi_arg(value) {
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

impl FromFFIVal for FFIValue {
    fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
        RResult::ROk(val)
    }
}

// impl<T: FromFFIVal, E: FromFFIVal> FromFFIVal for std::result::Result<T, E> {
//     fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
//         todo!()
//     }
// }

impl<'a> FromFFIArg<'a> for FFIArg<'a> {
    fn from_ffi_arg(val: FFIArg<'a>) -> RResult<Self, RBoxError> {
        RResult::ROk(val)
    }
}

// TODO: I think we can get rid of the unnecessary cloning... maybe?
// impl<T: Custom + Clone + 'static> FromFFIVal for T {
//     fn from_ffi_val(val: FFIValue) -> RResult<Self, RBoxError> {
//         if let FFIValue::Custom { custom } = &val {
//             if let Some(underlying) = as_underlying_type::<T>(custom.inner.borrow().as_ref()) {
//                 return RResult::ROk(underlying.clone());
//             }
//         }
//         conversion_error!(Opaque, val)
//     }
// }

// Convert this directly to the type we want on the way out
impl<T: OpaqueObject + MaybeSendSyncStatic> From<T> for FFIValue {
    fn from(value: T) -> Self {
        FFIValue::Custom {
            custom: OpaqueFFIValueReturn {
                inner: OpaqueObject_TO::from_value(value, TD_CanDowncast),
            },
        }
    }
}

pub struct Wrapper<ARGS>(PhantomData<ARGS>);
pub struct WrapperRef<ARGS>(PhantomData<ARGS>);
pub struct WrapperMut<ARGS>(PhantomData<ARGS>);
pub struct WrapperMutRef<ARGS>(PhantomData<ARGS>);

// pub trait AsRefFFIVal: Sized {
//     type Nursery: Default;

//     fn as_ref<'b, 'a: 'b>(
//         val: &'a FFIValue,
//         _nursery: &'a mut Self::Nursery,
//     ) -> RResult<SRef<'b, Self>, RBoxError>;

//     fn as_mut_ref<'b, 'a: 'b>(val: &'a FFIValue) -> RResult<RefMut<'b, Self>, RBoxError>;
// }

pub trait AsRefFFIVal: Sized {
    fn as_ref<'b, 'a: 'b, 'c>(val: &'a FFIArg<'c>) -> RResult<&'b Self, RBoxError>;
    fn as_mut_ref<'b, 'a: 'b, 'c>(val: &'a mut FFIArg<'c>) -> RResult<&'b mut Self, RBoxError>;
}

// impl<T: CustomType + 'static> AsRefFFIVal for T {
//     type Nursery = ();

//     fn as_ref<'b, 'a: 'b>(
//         val: &'a FFIValue,
//         _nursery: &'a mut Self::Nursery,
//     ) -> RResult<SRef<'b, Self>, RBoxError> {
//         if let FFIValue::Custom { custom } = val {
//             let res = Ref::map(custom.inner.borrow(), |x| x.as_any_ref());

//             if res.is::<T>() {
//                 return RResult::ROk(SRef::Owned(Ref::map(res, |x| {
//                     x.downcast_ref::<T>().unwrap()
//                 })));
//             }
//         }

//         return conversion_error!(OpaqueFFIValue, val);
//     }

//     fn as_mut_ref<'b, 'a: 'b>(val: &'a FFIValue) -> RResult<RefMut<'b, Self>, RBoxError> {
//         if let FFIValue::Custom { custom } = val {
//             let res = RefMut::map(custom.inner.borrow_mut(), |x| x.as_any_ref_mut());

//             if res.is::<T>() {
//                 return RResult::ROk(RefMut::map(res, |x| x.downcast_mut::<T>().unwrap()));
//             }
//         }

//         return conversion_error!(OpaqueFFIValue, val);
//     }
// }

impl<T: OpaqueObject + 'static> AsRefFFIVal for T {
    fn as_ref<'b, 'a: 'b, 'c>(val: &'a FFIArg<'c>) -> RResult<&'b Self, RBoxError> {
        if let FFIArg::CustomRef(CustomRef { custom, .. }) = val {
            let downcast = custom.get().obj.downcast_as::<T>();

            match downcast {
                Ok(v) => return RResult::ROk(v),
                Err(e) => return conversion_error!(OpaqueFFIValue, e),
            }
        }

        return conversion_error!(OpaqueFFIValue, val);
    }

    fn as_mut_ref<'b, 'a: 'b, 'c>(val: &'a mut FFIArg<'c>) -> RResult<&'b mut Self, RBoxError> {
        if let FFIArg::CustomRef(CustomRef { custom, .. }) = val {
            // let res = RefMut::map(custom.inner.borrow_mut(), |x| x.as_any_ref_mut());

            // if res.is::<T>() {
            //     return RResult::ROk(RefMut::map(res, |x| x.downcast_mut::<T>().unwrap()));
            // }

            // todo!()

            // RArc::get_mut()

            let downcast = custom.get_mut().obj.downcast_as_mut::<T>();

            match downcast {
                Ok(v) => return RResult::ROk(v),
                Err(e) => {
                    return RResult::RErr(ffi_error(
                        format!("Value unable to be converted to Opaque: {:?}", e).into(),
                    ));
                }
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

impl<T: IntoFFIVal, V: IntoFFIVal> IntoFFIVal for std::collections::HashMap<T, V> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        let mut output = RHashMap::with_capacity(self.len());

        for (key, value) in self {
            output.insert(ffi_try!(key.into_ffi_val()), ffi_try!(value.into_ffi_val()));
        }

        RResult::ROk(FFIValue::HashMap(output))
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
        let f = move |args: RSliceMut<'static, FFIArg<'static>>| -> RResult<FFIValue, RBoxError> {
            if args.len() != 1 {
                let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                let rbox = RBoxError::from_box(error);

                return RResult::RErr(rbox);
            }

            let mut arg_iter = args.into_iter();
            let rarg1 = arg_iter.next().unwrap();

            let arg1 = ffi_try!(<SELF>::as_ref(&rarg1));

            let res = func(&arg1);

            res.into_ffi_val()
        };

        self.register_value(
            name,
            FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
                name: RString::from(name),
                arity: 0,
                function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
            })),
        );

        self
    }
}

macro_rules! impl_register_fn_ffi {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            $($param: FromFFIArg<'static>,)*
            FN: Fn($($param),*) -> RET + SendSyncStatic,
            RET: IntoFFIVal
        > RegisterFFIFn<FN, Wrapper<($($param,)*)>, RET> for FFIModule {
            fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
                let f = move |args: RSliceMut<'static, FFIArg<'static>>| -> RResult<FFIValue, RBoxError> {
                    if args.len() != $arg_count {
                        let error: Box<dyn std::error::Error + Send + Sync> = format!("arity mismatch: expected: {}, found: {}", $arg_count, args.len()).into();

                        let rbox = RBoxError::from_box(error);

                        return RResult::RErr(rbox);
                    }

                    let mut arg_iter = args.into_iter();
                    let res = func($(ffi_try!(<$param>::from_ffi_arg(std::mem::replace(arg_iter.next().unwrap(), FFIArg::Void))),)*);

                    // let res = func($({
                    //     ffi_try!(<$param>::from_ffi_val());
                    // },)*);

                    res.into_ffi_val()
                };

                self.register_value(
                    name,
                    FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
                        name: RString::from(name),
                        arity: 0,
                        // function: Arc::new(f),
                        function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
                    })),
                );

                self
            }
        }


        impl<RET: IntoFFIVal, SELF: AsRefFFIVal, $($param: FromFFIArg<'static>,)* FN: Fn(&mut SELF, $($param),*) -> RET + SendSyncStatic>
            RegisterFFIFn<FN, WrapperMutRef<(SELF, $($param,)*)>, RET> for FFIModule
        {
            fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
                let f = move |args: RSliceMut<'static, FFIArg<'static>>| -> RResult<FFIValue, RBoxError> {
                    if  args.len() != $arg_count + 1 {
                        let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                        let rbox = RBoxError::from_box(error);

                        return RResult::RErr(rbox);
                    }

                    let mut arg_iter = args.into_iter();
                    let mut rarg1 = arg_iter.next().unwrap();

                    let mut arg1 = ffi_try!(<SELF>::as_mut_ref(&mut rarg1));

                    let res = func(&mut arg1, $(ffi_try!(<$param>::from_ffi_arg(std::mem::replace(arg_iter.next().unwrap(), FFIArg::Void))),)*);

                    res.into_ffi_val()
                };

                self.register_value(
                    name,
                    FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
                        name: RString::from(name),
                        arity: 0,
                        // function: Arc::new(f),
                        function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
                    })),
                );

                self
            }
        }


        impl<RET: IntoFFIVal, SELF: AsRefFFIVal, $($param: FromFFIArg<'static>,)* FN: Fn(&SELF, $($param),*) -> RET + SendSyncStatic>
            RegisterFFIFn<FN, WrapperRef<(SELF, $($param,)*)>, RET> for FFIModule
        {
            fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
                let f = move |args: RSliceMut<'static, FFIArg<'static>>| -> RResult<FFIValue, RBoxError> {
                    if args.len() != $arg_count + 1 {
                        let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                        let rbox = RBoxError::from_box(error);

                        return RResult::RErr(rbox);
                    }

                    let mut arg_iter = args.into_iter();
                    let mut rarg1 = arg_iter.next().unwrap();

                    let mut arg1 = ffi_try!(<SELF>::as_mut_ref(&mut rarg1));

                    let res = func(&mut arg1, $(ffi_try!(<$param>::from_ffi_arg(std::mem::replace(arg_iter.next().unwrap(), FFIArg::Void))),)*);

                    res.into_ffi_val()
                };

                self.register_value(
                    name,
                    FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
                        name: RString::from(name),
                        arity: 0,
                        // function: Arc::new(f),
                        function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
                    })),
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
        let f = move |args: RSliceMut<'static, FFIArg>| -> RResult<FFIValue, RBoxError> {
            if args.len() != 1 {
                let error: Box<dyn std::error::Error + Send + Sync> =
                    format!("Arity mismatch, expected: 1, found: {}", args.len()).into();

                let rbox = RBoxError::from_box(error);

                return RResult::RErr(rbox);
            }

            let mut arg_iter = args.into_iter();
            let mut rarg1 = arg_iter.next().unwrap();

            let mut arg1 = ffi_try!(<SELF>::as_mut_ref(&mut rarg1));

            let res = func(&mut arg1);

            res.into_ffi_val()
        };

        self.register_value(
            name,
            FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
                name: RString::from(name),
                arity: 0,
                // function: Arc::new(f),
                function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
            })),
        );

        self
    }
}

// Implementing FFI value conversion layers...
impl<RET: IntoFFIVal, FN: Fn() -> RET + SendSyncStatic> RegisterFFIFn<FN, Wrapper<()>, RET>
    for FFIModule
{
    fn register_fn(&mut self, name: &str, func: FN) -> &mut Self {
        let f = move |args: RSliceMut<'static, FFIArg>| -> RResult<FFIValue, RBoxError> {
            if !args.is_empty() {
                // TODO: Fix the error message here if possible. Might require cloning the string?
                let error: Box<dyn std::error::Error + Send + Sync> = "arity mismatch".into();

                let rbox = RBoxError::from_box(error);

                return RResult::RErr(rbox);
            }

            let res = func();

            res.into_ffi_val()
        };

        self.register_value(
            name,
            FFIValue::BoxedFunction(RBox::new(FFIBoxedDynFunction {
                name: RString::from(name),
                arity: 0,
                // function: Arc::new(f),
                function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
            })),
        );

        self
    }
}

pub use abi_stable::std_types::RBox;
pub use abi_stable::{export_root_module, prefix_type::PrefixTypeTrait, sabi_extern_fn};

use super::{builtin::BuiltInModule, register_fn::RegisterFn, vm::VmCore};

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

#[repr(C)]
#[derive(StableAbi)]
#[sabi(impl_InterfaceType(Sync, Send, IoWrite))]
pub struct WriterInterface;

#[repr(C)]
#[derive(StableAbi)]
#[sabi(impl_InterfaceType(Sync, Send, IoRead))]
pub struct ReaderInterface;

#[repr(C)]
#[derive(StableAbi)]
pub struct DynWriter {
    pub writer: DynTrait<'static, RBox<()>, WriterInterface>,
}

impl DynWriter {
    fn into_port(self) -> SteelPortRepr {
        SteelPortRepr::DynWriter(Arc::new(Mutex::new(self.writer)))
    }
}

#[repr(C)]
#[derive(StableAbi)]
pub struct DynReader {
    pub reader: DynTrait<'static, RBox<()>, ReaderInterface>,
}

impl DynReader {
    fn into_port(self) -> SteelPortRepr {
        SteelPortRepr::DynReader(BufReader::new(Box::new(self.reader)))
    }
}

impl IntoFFIVal for DynWriter {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        RResult::ROk(FFIValue::DynWriter(self))
    }
}

impl IntoFFIVal for DynReader {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        RResult::ROk(FFIValue::DynReader(self))
    }
}

#[repr(C)]
#[derive(StableAbi)]
pub struct MutableString {
    pub string: RString,
}

impl Custom for MutableString {}

#[repr(C)]
#[derive(StableAbi)]
struct FFIVector {
    vec: RVec<FFIValue>,
}

impl Custom for FFIVector {}

pub fn as_underlying_ffi_type<'a, T: 'static>(
    obj: &'a mut OpaqueObject_TO<'static, RBox<()>>,
) -> Option<&'a mut T> {
    obj.obj.downcast_as_mut().ok()
}

pub fn is_opaque_type<T: 'static>(val: FFIArg) -> bool {
    if let FFIArg::CustomRef(CustomRef { custom, .. }) = val {
        return as_underlying_ffi_type::<T>(custom.into_mut()).is_some();
    } else {
        false
    }
}

#[repr(C)]
#[derive(StableAbi)]
pub struct CustomRef<'a> {
    pub custom: RMut<'a, OpaqueObject_TO<'static, RBox<()>>>,
    // TODO: Make this private
    #[sabi(unsafe_opaque_field)]
    guard: ScopedWriteContainer<'a, Box<dyn CustomType>>,
}

#[repr(C)]
#[derive(StableAbi)]
pub struct VectorRef<'a> {
    pub vec: RSliceMut<'a, FFIValue>,
    #[sabi(unsafe_opaque_field)]
    guard: ScopedWriteContainer<'a, Box<dyn CustomType>>,
}

#[repr(C)]
#[derive(StableAbi)]
pub struct StringMutRef<'a> {
    string: RMut<'a, RString>,
    #[sabi(unsafe_opaque_field)]
    guard: ScopedWriteContainer<'a, Box<dyn CustomType>>,
}

// TODO:
// Values that are safe to cross the FFI Boundary as arguments from
// `SteelVal`s. This means the values can be borrowed without
// copying in certain situations, assuming we can do that optimization.
#[repr(C)]
#[derive(StableAbi)]
pub enum FFIArg<'a> {
    StringRef(RStr<'a>),
    BoolV(bool),
    NumV(f64),
    IntV(isize),
    Void,
    StringV(RString),
    StringMutRef(StringMutRef<'a>),
    Vector(RVec<FFIArg<'a>>),
    VectorRef(VectorRef<'a>),
    CharV {
        #[sabi(unsafe_opaque_field)]
        c: char,
    },
    Custom {
        custom: OpaqueFFIValueReturn,
    },
    CustomRef(CustomRef<'a>),
    HashMap(RHashMap<FFIArg<'a>, FFIArg<'a>>),
    Future {
        #[sabi(unsafe_opaque_field)]
        fut: FfiFuture<RResult<FFIArg<'a>, RBoxError>>,
    },
    HostFunction(HostRuntimeFunction),
    ByteVector(RVec<u8>),
}

impl<'a> std::default::Default for FFIArg<'a> {
    fn default() -> Self {
        FFIArg::Void
    }
}

impl<'a> std::hash::Hash for FFIArg<'a> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl<'a> PartialEq for FFIArg<'a> {
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

impl<'a> Eq for FFIArg<'a> {}

#[steel_derive::define_module(name = "steel/ffi")]
pub fn ffi_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ffi");

    module
        .register_native_fn_definition(NEW_FFI_VECTOR_DEFINITION)
        .register_fn("ffi-vector-ref", |vec: &mut FFIVector, index: usize| {
            let value = vec.vec.get(index);
            match value {
                Some(value) => FFIValue::try_clone(value),
                None => None,
            }
        })
        .register_fn("mutable-string", || MutableString {
            string: RString::new(),
        });

    #[cfg(feature = "sync")]
    module.register_native_fn_definition(FUNCTION_TO_FFI_FUNCTION_DEFINITION);

    module
}

#[steel_derive::native(name = "ffi-vector", arity = "AtLeast(0)")]
pub fn new_ffi_vector(args: &[SteelVal]) -> Result<SteelVal> {
    FFIVector {
        vec: args
            .into_iter()
            .map(|x| as_ffi_value(x))
            .collect::<Result<_>>()?,
    }
    .into_steelval()
}

/// Values that are safe to cross the FFI Boundary.
#[repr(C)]
#[derive(StableAbi)]
pub enum FFIValue {
    BoxedFunction(RBox<FFIBoxedDynFunction>),
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
    Custom {
        custom: OpaqueFFIValueReturn,
    },
    HashMap(RHashMap<FFIValue, FFIValue>),
    Future {
        fut: SyncFfiFuture,
    },
    ByteVector(RVec<u8>),
    DynWriter(DynWriter),
    DynReader(DynReader),
    VectorToVector(RVec<FFIValue>),
    VectorToMutableVector(RVec<FFIValue>),
}

#[repr(C)]
#[derive(StableAbi)]
pub struct SyncFfiFuture {
    // TODO: @Matt - Just wrap this in a mutex, then we should be
    // good to go on this!
    fut: FfiFuture<RResult<FFIValue, RBoxError>>,
}

// TODO: Don't let this slip through the cracks
unsafe impl Sync for SyncFfiFuture {}

impl FFIValue {
    pub fn try_clone(&self) -> Option<FFIValue> {
        match self {
            FFIValue::BoolV(b) => Some(FFIValue::BoolV(*b)),
            FFIValue::NumV(n) => Some(FFIValue::NumV(*n)),
            FFIValue::IntV(i) => Some(FFIValue::IntV(*i)),
            FFIValue::Void => Some(FFIValue::Void),
            FFIValue::StringV(s) => Some(FFIValue::StringV(s.clone())),
            FFIValue::CharV { c } => Some(FFIValue::CharV { c: *c }),
            _ => None,
        }
    }
}

impl std::hash::Hash for FFIValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            FFIValue::BoolV(b) => b.hash(state),
            FFIValue::IntV(i) => i.hash(state),
            FFIValue::Void => 0.hash(state),
            FFIValue::StringV(s) => s.hash(state),
            FFIValue::CharV { c } => c.hash(state),
            _ => panic!("Cannot hash this value: {:?}", self),
        }
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
            FFIValue::ByteVector(b) => write!(f, "{:?}", b),
            FFIValue::DynWriter(_) => write!(f, "#<ffi-writer>"),
            FFIValue::DynReader(_) => write!(f, "#<ffi-reader>"),
            FFIValue::VectorToVector(v) => write!(f, "{:?}", v),
            FFIValue::VectorToMutableVector(v) => write!(f, "{:?}", v),
        }
    }
}

impl<'a> std::fmt::Debug for FFIArg<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Custom { .. } => write!(f, "#<OpaqueFFIValue>"),
            Self::CustomRef { .. } => write!(f, "#<OpaqueFFIValue>"),
            Self::BoolV(b) => write!(f, "{:?}", b),
            Self::NumV(n) => write!(f, "{:?}", n),
            Self::IntV(i) => write!(f, "{:?}", i),
            Self::CharV { c } => write!(f, "{}", c),
            Self::Void => write!(f, "#<void>"),
            Self::StringV(s) => write!(f, "{}", s),
            Self::StringRef(s) => write!(f, "{}", s),
            Self::Vector(v) => write!(f, "{:?}", v),
            Self::HashMap(h) => write!(f, "{:?}", h),
            Self::Future { .. } => write!(f, "#<future>"),
            _ => todo!(),
        }
    }
}

impl FFIValue {
    pub fn as_steelval(&self) -> Result<SteelVal> {
        match self {
            Self::BoxedFunction(b) => {
                Ok(SteelVal::BoxedFunction(Gc::new(b.as_boxed_dyn_function())))
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
                .collect::<Result<HashMap<_, _>>>()
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
            v => {
                todo!("Missing enum variant not accounted for during FFIValue -> SteelVal conversion!: {:?}", v);
            }
        }
    }
}

impl IntoSteelVal for FFIValue {
    fn into_steelval(self) -> Result<SteelVal> {
        match self {
            Self::BoxedFunction(b) => {
                Ok(SteelVal::BoxedFunction(Gc::new(RBox::into_inner(b).into())))
            }
            Self::Custom { custom } => Ok(SteelVal::Custom(Gc::new_mut(Box::new(custom)))),
            Self::BoolV(b) => Ok(SteelVal::BoolV(b)),
            Self::NumV(n) => Ok(SteelVal::NumV(n)),
            Self::IntV(i) => Ok(SteelVal::IntV(i)),
            Self::CharV { c } => Ok(SteelVal::CharV(c)),
            Self::Void => Ok(SteelVal::Void),
            // TODO: I think this might clone the string, its also a little suspect
            Self::StringV(s) => Ok(SteelVal::StringV(s.into_string().into())),

            // Vectors... can probably turn directly into vectors?
            Self::Vector(v) => v
                .into_iter()
                .map(|x| x.into_steelval())
                .collect::<Result<_>>()
                .map(SteelVal::ListV),

            Self::VectorToVector(v) => v
                .into_iter()
                .map(|x| x.into_steelval())
                .collect::<Result<_>>()
                .map(SteelVal::ListV),

            Self::VectorToMutableVector(v) => v
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
                .collect::<Result<HashMap<_, _>>>()
                .map(Gc::new)
                .map(SteelHashMap::from)
                .map(SteelVal::HashMapV),

            // Attempt to move this across the FFI Boundary... We'll see how successful it is.
            FFIValue::Future { fut } => Ok(SteelVal::FutureV(Gc::new(FutureResult::new(
                Box::pin(async {
                    fut.fut
                        .map(|x| match x {
                            RResult::ROk(v) => v.into_steelval(),
                            RResult::RErr(e) => {
                                Err(SteelErr::new(ErrorKind::Generic, e.to_string()))
                            }
                        })
                        .await
                }),
            )))),

            Self::ByteVector(b) => Ok(SteelVal::ByteVector(SteelByteVector::new(b.into()))),

            Self::DynWriter(d) => Ok(SteelVal::PortV(SteelPort {
                port: Gc::new_mut(d.into_port()),
            })),

            Self::DynReader(d) => Ok(SteelVal::PortV(SteelPort {
                port: Gc::new_mut(d.into_port()),
            })),
        }
    }
}

#[repr(C)]
#[derive(StableAbi)]
pub struct FFIBoxedDynFunction {
    pub name: RString,
    pub arity: usize,
    pub function: RFn_TO<
        'static,
        'static,
        RArc<()>,
        RSliceMut<'static, FFIArg<'static>>,
        RResult<FFIValue, RBoxError>,
    >,
}

impl Clone for FFIBoxedDynFunction {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            arity: self.arity.clone(),
            function: RFn_TO::from_sabi(self.function.obj.shallow_clone()),
        }
    }
}

impl std::fmt::Debug for FFIBoxedDynFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<{}:{}>", self.name, self.arity)
    }
}

fn into_ffi_value(value: SteelVal) -> Result<FFIValue> {
    match value {
        SteelVal::BoolV(b) => Ok(FFIValue::BoolV(b)),
        SteelVal::IntV(i) => Ok(FFIValue::IntV(i)),
        SteelVal::NumV(n) => Ok(FFIValue::NumV(n)),
        SteelVal::CharV(c) => Ok(FFIValue::CharV { c }),
        SteelVal::Void => Ok(FFIValue::Void),
        SteelVal::HashMapV(h) => {
            h.0.unwrap()
                .into_iter()
                .map(|(key, value)| {
                    let key = into_ffi_value(key)?;
                    let value = into_ffi_value(value)?;

                    Ok((key, value))
                })
                .collect::<Result<_>>()
                .map(FFIValue::HashMap)
        }
        SteelVal::ListV(l) => l
            .into_iter()
            .map(into_ffi_value)
            .collect::<Result<_>>()
            .map(FFIValue::Vector),

        // TODO:
        // Don't copy the string unless we have to!
        SteelVal::StringV(s) => Ok(FFIValue::StringV(s.as_str().into())),

        SteelVal::Custom(c) => {
            let mut guard = c.write();

            if let Some(c) = as_underlying_type_mut::<OpaqueFFIValueReturn>(guard.as_mut()) {
                #[repr(C)]
                #[derive(StableAbi)]
                struct DummyOpaqueObject;
                impl Custom for DummyOpaqueObject {}

                let existing = std::mem::replace(
                    c,
                    OpaqueFFIValueReturn {
                        inner: OpaqueObject_TO::from_value(DummyOpaqueObject, TD_CanDowncast),
                    },
                );

                Ok(FFIValue::Custom { custom: existing })
            } else {
                stop!(TypeMismatch => "Cannot proceed with the conversion from steelval to FFI Value. This value did not originate from across an FFI boundary")
            }
        }

        _ => {
            stop!(TypeMismatch => "Cannot proceed with the conversion from steelval to FFI Value. This will only succeed for a subset of values deemed as FFI-safe-enough: {:?}", value)
        }
    }
}

pub fn as_ffi_value(value: &SteelVal) -> Result<FFIValue> {
    match value {
        SteelVal::BoolV(b) => Ok(FFIValue::BoolV(*b)),
        SteelVal::IntV(i) => Ok(FFIValue::IntV(*i)),
        SteelVal::NumV(n) => Ok(FFIValue::NumV(*n)),
        SteelVal::CharV(c) => Ok(FFIValue::CharV { c: *c }),
        SteelVal::Void => Ok(FFIValue::Void),
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

        // TODO:
        // Don't copy the string unless we have to!
        SteelVal::StringV(s) => Ok(FFIValue::StringV(s.as_str().into())),

        _ => {
            stop!(TypeMismatch => "Cannot proceed with the conversion from steelval to FFI Value. This will only succeed for a subset of values deemed as FFI-safe-enough: {:?}", value)
        }
    }
}

#[repr(C)]
#[derive(StableAbi)]
pub struct HostRuntimeFunction {
    function: RFn_TO<
        'static,
        'static,
        RArc<()>,
        RSliceMut<'static, FFIValue>,
        RResult<FFIValue, RBoxError>,
    >,
}

#[cfg(feature = "sync")]
#[steel_derive::context(name = "function->ffi-function", arity = "Exact(1)")]
pub fn function_to_ffi_function(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    fn function_to_ffi_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
        if args.len() != 1 {
            stop!(ArityMismatch => "function->ffi-function expects one arg, found: {}", args.len());
        }

        let function = args[0].clone();

        if function.is_function() {
            let function = ctx.steel_function_to_rust_function(function);

            let foreign_fn_ptr = HostRuntimeFunction::from_dyn_function(function);

            // Convert the foreign fn ptr into something that we can use
            foreign_fn_ptr.into_steelval()
        } else {
            stop!(TypeMismatch => "function->ffi-function expected a function, found: {}", function);
        }
    }

    Some(function_to_ffi_impl(ctx, args))
}

impl HostRuntimeFunction {
    fn from_dyn_function(
        func: Box<dyn Fn(&mut [SteelVal]) -> Result<SteelVal> + Send + Sync + 'static>,
    ) -> Self {
        let f = move |args: RSliceMut<'static, FFIValue>| -> RResult<FFIValue, RBoxError> {
            let other_args = args
                .into_iter()
                .map(|x| std::mem::replace(x, FFIValue::Void).into_steelval())
                .collect::<Result<smallvec::SmallVec<[SteelVal; 16]>>>();

            match other_args {
                Ok(mut args) => {
                    let res = (func)(&mut args);

                    match res.and_then(|x| into_ffi_value(x)) {
                        Ok(value) => RResult::ROk(value),
                        Err(e) => RResult::RErr(RBoxError::new(e)),
                    }
                }
                Err(e) => RResult::RErr(RBoxError::new(e)),
            }
        };

        Self {
            function: RFn_TO::from_ptr(RArc::new(f), TD_CanDowncast),
        }
    }

    pub fn call(&self, mut args: RSliceMut<FFIValue>) -> RResult<FFIValue, RBoxError> {
        let lifted_slice = unsafe {
            std::mem::transmute::<&mut [FFIValue], &'static mut [FFIValue]>(args.as_mut_slice())
        };

        self.function.call(RSliceMut::from_mut_slice(lifted_slice))
    }
}

impl Custom for HostRuntimeFunction {}

fn as_ffi_argument(value: &SteelVal) -> Result<FFIArg<'_>> {
    match value {
        SteelVal::BoolV(b) => Ok(FFIArg::BoolV(*b)),
        SteelVal::IntV(i) => Ok(FFIArg::IntV(*i)),
        SteelVal::NumV(n) => Ok(FFIArg::NumV(*n)),
        SteelVal::CharV(c) => Ok(FFIArg::CharV { c: *c }),
        SteelVal::Void => Ok(FFIArg::Void),

        // TODO: Find a way to not have to copy the whole byte vector
        SteelVal::ByteVector(b) => Ok(FFIArg::ByteVector(b.vec.read().iter().copied().collect())),

        // We can really only look at values that were made from the FFI boundary.
        SteelVal::Custom(c) => {
            // let mut guard = if let Ok(guard) = RefCell::try_borrow_mut(c) {
            let mut guard = if let Ok(guard) = c.try_write() {
                guard
            } else {
                stop!(Generic => "value cannot be borrowed mutably twice over the ffi boundary: {:?}", value)
            };

            if let Some(c) = as_underlying_type_mut::<OpaqueFFIValueReturn>(guard.as_mut()) {
                // SAFETY:
                // This should only be called internally, and the scope of the lifetime should be limited
                // to macro generated code.
                // TODO: I think it is possible that if the same value is used multiple times,
                // we could be borrowing this value more than once. We need a stickier way
                // to borrow since the RefCell cannot be passed along?
                unsafe {
                    Ok(FFIArg::CustomRef(CustomRef {
                        custom: RMut::from_raw(&mut c.inner as *mut _),
                        guard,
                    }))
                }
            } else if let Some(c) = as_underlying_type_mut::<FFIVector>(guard.as_mut()) {
                unsafe {
                    let mut_ptr = &mut c.vec as *mut RVec<FFIValue>;

                    // Passing the same one _should_ panic
                    Ok(FFIArg::VectorRef(VectorRef {
                        vec: (*mut_ptr).as_mut_rslice(),
                        guard,
                    }))
                }
            } else if let Some(c) = as_underlying_type_mut::<MutableString>(guard.as_mut()) {
                unsafe {
                    let mut_ptr = &mut c.string as *mut RString;

                    // Passing the same one _shoudl_ panic
                    Ok(FFIArg::StringMutRef(StringMutRef {
                        string: RMut::from_raw(mut_ptr),
                        guard,
                    }))
                }
            } else if let Some(c) = as_underlying_type_mut::<HostRuntimeFunction>(guard.as_mut()) {
                // Just pass the function by value
                Ok(FFIArg::HostFunction(HostRuntimeFunction {
                    function: RFn_TO::from_sabi(c.function.obj.shallow_clone()),
                }))
            } else {
                stop!(TypeMismatch => "This opaque type did not originate from an FFI boundary, and thus cannot be passed across it: {:?}", value);
            }
        }
        SteelVal::HashMapV(h) => h
            .iter()
            .map(|(key, value)| {
                let key = as_ffi_argument(key)?;
                let value = as_ffi_argument(value)?;

                Ok((key, value))
            })
            .collect::<Result<_>>()
            .map(FFIArg::HashMap),
        SteelVal::ListV(l) => l
            .into_iter()
            .map(as_ffi_argument)
            .collect::<Result<_>>()
            .map(FFIArg::Vector),

        // TODO:
        // Don't copy the string unless we have to!
        // SteelVal::StringV(s) => Ok(FFIValue::StringV(s.as_str().into())),
        SteelVal::StringV(s) => Ok(FFIArg::StringRef(RStr::from_str(s.as_str()))),

        _ => {
            stop!(TypeMismatch => "Cannot proceed with the conversion from steelval to FFI Value. This will only succeed for a subset of values deemed as FFI-safe-enough: {:?}", value)
        }
    }
}

impl FFIBoxedDynFunction {
    fn as_boxed_dyn_function(&self) -> BoxedDynFunction {
        let name = self.name.to_string();

        let this = self.clone();

        let function = move |args: &[SteelVal]| -> crate::rvals::Result<SteelVal> {
            let cloned_function = RFn_TO::from_sabi(this.function.obj.shallow_clone());

            let args = unsafe { std::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            // Attempt collecting and passing as an rslice?
            let mut other_args = args
                .into_iter()
                .map(as_ffi_argument)
                .collect::<Result<smallvec::SmallVec<[FFIArg<'_>; 16]>>>()?;

            let lifted_slice = unsafe {
                std::mem::transmute::<&mut [FFIArg], &'static mut [FFIArg]>(
                    other_args.as_mut_slice(),
                )
            };

            // Get the slice
            let rslice = RSliceMut::from_mut_slice(lifted_slice);

            let result = cloned_function.call(rslice);

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
        let name = value.name.clone().into_string();
        let arity = value.arity;

        let function = move |args: &[SteelVal]| -> crate::rvals::Result<SteelVal> {
            let args = unsafe { std::mem::transmute::<&[SteelVal], &'static [SteelVal]>(args) };

            let mut other_args = args
                .into_iter()
                .map(as_ffi_argument)
                .collect::<Result<smallvec::SmallVec<[FFIArg<'_>; 16]>>>()?;

            let lifted_slice = unsafe {
                std::mem::transmute::<&mut [FFIArg], &'static mut [FFIArg]>(
                    other_args.as_mut_slice(),
                )
            };

            // Get the slice
            let rslice = RSliceMut::from_mut_slice(lifted_slice);

            let result = value.function.call(rslice);

            match result {
                RResult::ROk(output) => output.into_steelval(),
                RResult::RErr(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
            }
        };

        BoxedDynFunction {
            name: Some(StaticOrRcStr::Owned(Arc::new(name))),
            arity: Some(arity),
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
    pub fn new(mut raw_module: RBox<FFIModule>) -> Result<Self> {
        let mut converted_module = BuiltInModule::new((&raw_module.name).to_string());

        for tuple in std::mem::take(&mut raw_module.values).into_iter() {
            let key = tuple.0;
            let value = tuple.1;

            converted_module.register_value(&key, value.into_steelval()?);
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
