use std::{
    borrow::Cow,
    cell::{Ref, RefCell, RefMut},
    marker::PhantomData,
    rc::Rc,
    sync::{Arc, Mutex},
};

use crate::{
    containers::RegisterValue,
    gc::{unsafe_erased_pointers::OpaqueReference, Gc},
    parser::{ast::ExprKind, interner::InternedString, parser::SyntaxObject, tokens::TokenType},
    rerrs::ErrorKind,
    rvals::{
        as_underlying_type, Custom, CustomType, FromSteelVal, FunctionSignature, IntoSteelVal,
        Result, SRef, SteelVal,
    },
    values::functions::{BoxedDynFunction, StaticOrRcStr},
    SteelErr,
};
use im_rc::{HashMap, OrdMap};

use abi_stable::{
    sabi_trait,
    std_types::{
        RArc, RBoxError, RCow, RCowStr, RHashMap, RResult, RSlice, RStr, RString, RVec, Tuple2,
    },
    StableAbi,
};

#[macro_export]
macro_rules! ffi_try {
    ($result:expr) => {
        match $result {
            RResult::ROk(v) => v,
            RResult::RErr(e) => return RResult::RErr(e),
        }
    };
}

/// A module to be consumed by the Steel Engine for later on demand access by scripts
/// to refresh the primitives that are being used. For instance, the VM should have support
/// for a primitive like so, where "kernel" has functions like `list`, `cons`, `car`, and `cdr`:
///
/// ```scheme
/// (require-builtin "kernel")
/// ```
///
/// This would then expand into a series of function calls like so:
/// ```scheme
/// (begin
///     (define list (hash-get *--unreadable-module-name--kernel*))
///     (define cons (hash-get *--unreadable-module-name--kernel*))
///     (define car (hash-get *--unreadable-module-name--kernel*))
///     (define cdr (hash-get *--unreadable-module-name--kernel*)))
/// ```
/// So for now, its just a hashmap that will get embedded smartly, accompanied with a macro that will assist
/// with reserving the proper slots in the interner
///
/// TODO: @Matt - We run the risk of running into memory leaks here when exposing external mutable
/// structs. This should be more properly documented.
#[derive(Clone, Debug)]
#[repr(C)]
pub struct BuiltInModule {
    pub(crate) name: Rc<str>,
    values: HashMap<Arc<str>, SteelVal>,
    docs: Box<InternalDocumentation>,
    version: &'static str,
    // Add the metadata separate from the pointer, keeps the pointer slim
    fn_ptr_table: HashMap<*const FunctionSignature, FunctionSignatureMetadata>,
}

/// This creates an external module this is _mostly_ safe.
#[repr(C)]
#[derive(StableAbi, Clone, Debug)]
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

    // pub fn register_value(&mut self, name: RString, value: FFIValue) -> &mut Self {
    //     self.values.insert(name, value);
    //     self
    // }
}

// TODO: Wrap FFI Objects with this trait, have the object we pass around just be a normal one?
// Making sure that we are not referencing static things (or memory accessed in the other one?)

#[repr(C)]
#[derive(Clone)]
pub struct OpaqueFFIValue {
    pub name: RString,
    pub inner: Gc<RefCell<Box<dyn CustomType>>>,
}

impl Custom for OpaqueFFIValue {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<{}>", self.name)))
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
            format!("Value unable to be converted to $name: {:?}", $val).into(),
        ))
    };
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

impl<T: IntoFFIVal> IntoFFIVal for Vec<T> {
    fn into_ffi_val(self) -> RResult<FFIValue, RBoxError> {
        let mut output = RVec::with_capacity(self.len());

        for value in self {
            output.push(ffi_try!(value.into_ffi_val()));
        }

        RResult::ROk(FFIValue::Vector(output))
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

#[macro_export]
macro_rules! declare_module {
    ($module_function:expr) => {
        #[::steel::steel_vm::builtin::export_root_module]
        pub fn get_library() -> ::steel::steel_vm::dylib::GenerateModule_Ref {
            use ::steel::steel_vm::builtin::PrefixTypeTrait;
            ::steel::steel_vm::dylib::GenerateModule { generate_module }.leak_into_prefix()
        }

        #[::steel::steel_vm::builtin::sabi_extern_fn]
        fn generate_module() -> ::steel::steel_vm::builtin::RBox<FFIModule> {
            ::steel::steel_vm::builtin::RBox::new($module_function())
        }
    };
}

/// Values that are safe to cross the FFI Boundary.
#[repr(C)]
#[derive(StableAbi, Clone)]
pub enum FFIValue {
    BoxedFunction(FFIBoxedDynFunction),
    BoolV(bool),
    NumV(f64),
    IntV(isize),
    Void,
    // Make this be
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
    }, // HashMap(RHashMap<FFIValue, FFIValue>),
    // TODO: Delete this FFI enum variant. It just doesn't make sense to allow
    Reference {
        #[sabi(unsafe_opaque_field)]
        custom: OpaqueFFIReference,
    },
}

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
            FFIValue::Reference { .. } => write!(f, "#<OpaqueReference>"),
        }
    }
}

impl FFIValue {
    pub fn as_steelval(&self) -> Result<SteelVal> {
        match self {
            Self::BoxedFunction(b) => {
                Ok(SteelVal::BoxedFunction(Rc::new(b.as_boxed_dyn_function())))
            }
            // Self::Custom { custom } => Ok(SteelVal::Custom(custom)),
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

            Self::Reference { custom } => Ok(SteelVal::Reference(custom.inner.clone())),

            // Self::HashMap(h) => h
            //     .into_iter()
            //     .map(|kv| {
            //         let k = kv.0;
            //         let v = kv.1;

            //         let k = k.into_steelval()?;
            //         let v = v.into_steelval()?;

            //         Ok((k, v))
            //     })
            //     .collect::<Result<im_rc::HashMap<_, _>>>()
            //     .map(Gc::new)
            //     .map(SteelVal::HashMapV),
            // Self::HashMap(s) => Ok(SteelVal::HashMap)
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

            // Self::HashMap(h) => h
            //     .into_iter()
            //     .map(|kv| {
            //         let k = kv.0;
            //         let v = kv.1;

            //         let k = k.into_steelval()?;
            //         let v = v.into_steelval()?;

            //         Ok((k, v))
            //     })
            //     .collect::<Result<im_rc::HashMap<_, _>>>()
            //     .map(Gc::new)
            //     .map(SteelVal::HashMapV),
            // Self::HashMap(s) => Ok(SteelVal::HashMap)
            _v => {
                todo!("Missing enum variant not accounted for during FFIValue -> SteelVal conversion!")
            }
        }
    }
}

/// Not my favorite, but this can help with the lack of closures, until I figure out the
/// function pointer abstraction.
// #[sabi_trait]
// trait FFIFunction {
//     fn call(&self, args: RSlice<FFIValue>) -> RResult<FFIValue, RBoxError>;
// }

#[repr(C)]
#[derive(StableAbi, Clone)]
// #[sabi(unsafe_opaque_fields)]
pub struct FFIBoxedDynFunction {
    pub name: RString,
    pub arity: usize,
    // TODO: See if theres a better option here
    #[sabi(unsafe_opaque_field)]
    pub function:
        Arc<dyn Fn(RVec<FFIValue>) -> RResult<FFIValue, RBoxError> + Send + Sync + 'static>,
    // function: FFIFunction_TO<'static, RBox<()>>,
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
                stop!(TypeMismatch => "This opaque type did not originate from an FFI boundary, and thus cannot be passed across it: {:?}", value);
            }
        }
        SteelVal::Reference(r) => Ok(FFIValue::Reference {
            custom: OpaqueFFIReference {
                name: RString::from("OpaqueReference"),
                inner: r.clone(),
            },
        }),
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

#[derive(Clone, Debug)]
pub struct ExternalModule {
    module: Rc<RefCell<*mut BuiltInModule>>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// Probably need something more interesting than just an integer for the arity
pub struct FunctionSignatureMetadata {
    name: &'static str,
    arity: Arity,
}

impl FunctionSignatureMetadata {
    pub fn new(name: &'static str, arity: Arity) -> Self {
        Self { name, arity }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arity {
    Exact(usize),
    AtLeast(usize),
    AtMost(usize),
    Range(usize),
}

impl Custom for FunctionSignatureMetadata {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", self)))
    }
}

impl Custom for BuiltInModule {}

impl RegisterValue for BuiltInModule {
    fn register_value_inner(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.values.insert(name.into(), value);
        self
    }
}

lazy_static::lazy_static! {
    pub static ref MODULE_GET: InternedString = "%module-get%".into();
    pub static ref VOID: InternedString = "void".into();
}

impl BuiltInModule {
    pub fn new(name: String) -> Self {
        Self {
            name: name.into(),
            values: HashMap::new(),
            docs: Box::new(InternalDocumentation::new()),
            version: env!("CARGO_PKG_VERSION"),
            fn_ptr_table: HashMap::new(),
        }
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name.into();
    }

    pub fn register_native_fn(
        &mut self,
        name: &'static str,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        arity: Arity,
    ) -> &mut Self {
        // Just automatically add it to the function pointer table to help out with searching
        self.add_to_fn_ptr_table(func, FunctionSignatureMetadata::new(name, arity));
        self.register_value(name, SteelVal::FuncV(func))
    }

    pub fn check_compatibility(self: &BuiltInModule) -> bool {
        // self.version == env!("CARGO_PKG_VERSION")
        true
    }

    pub fn contains(&self, ident: &str) -> bool {
        self.values.contains_key(ident)
    }

    pub(crate) fn add_to_fn_ptr_table(
        &mut self,
        value: FunctionSignature,
        data: FunctionSignatureMetadata,
    ) -> &mut Self {
        self.fn_ptr_table
            .insert(value as *const FunctionSignature, data);

        self
    }

    pub fn search(&self, value: SteelVal) -> Option<FunctionSignatureMetadata> {
        // println!("{:?}", (*value as *const FunctionSignature) as usize);

        // println!("{:#?}", self.fn_ptr_table);

        if let SteelVal::FuncV(f) = value {
            self.fn_ptr_table
                .get(&(f as *const FunctionSignature))
                .cloned()
            // None
        } else {
            None
        }
    }

    pub fn bound_identifiers(&self) -> im_lists::list::List<SteelVal> {
        self.values
            .keys()
            .map(|x| SteelVal::StringV(x.to_string().into()))
            .collect()
    }

    pub fn with_module(mut self, module: BuiltInModule) -> Self {
        // self.values = self.values.union(module.values);

        self.values.extend(module.values.into_iter());

        self.docs.definitions.extend(module.docs.definitions);
        self
    }

    pub fn register_type<T: FromSteelVal + IntoSteelVal>(
        &mut self,
        predicate_name: &'static str,
    ) -> &mut Self {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => format!("{} expected 1 argument, got {}", predicate_name, args.len()));
            }

            assert!(args.len() == 1);

            Ok(SteelVal::BoolV(T::from_steelval(&args[0]).is_ok()))
        };

        self.register_value(
            predicate_name,
            SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new(
                Arc::new(f),
                Some(predicate_name),
                Some(1),
            ))),
        )
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: impl Into<Documentation<'static>>,
    ) {
        self.docs.register_doc(definition, description.into());
    }

    pub fn get_doc(&self, definition: String) {
        if let Some(value) = self.docs.get(&definition) {
            println!("{value}")
        }
    }

    pub(crate) fn unreadable_name(&self) -> String {
        "%-builtin-module-".to_string() + &self.name
    }

    /// Add a value to the module namespace. This value can be any legal SteelVal, or if you're explicitly attempting
    /// to compile an program for later use and don't currently have access to the functions in memory, use `SteelVal::Void`
    pub fn register_value(&mut self, name: &str, value: SteelVal) -> &mut Self {
        self.register_value_inner(name, value)
    }

    pub fn register_value_with_doc(
        &mut self,
        name: &'static str,
        value: SteelVal,
        doc: DocTemplate<'static>,
    ) -> &mut Self {
        self.values.insert(name.into(), value);
        self.register_doc(Cow::from(name), doc);
        self
    }

    // This _will_ panic given an incorrect value. This will be tied together by macros only allowing legal entries
    pub fn get(&self, name: String) -> SteelVal {
        self.values.get(name.as_str()).unwrap().clone()
    }

    /// This does the boot strapping for bundling modules
    /// Rather than expose a native hash-get, the built in module above should expose a raw
    /// function to fetch a dependency. It will be a packaged #<BuiltInModule> with only a function to
    /// fetch a function given its registered name. For instance:
    ///
    /// (##module-get## ##unreadable-module-name-core-lists## 'list)
    ///
    /// This puts the onus on the expansion of a primitive on the compiler, but now the language
    /// is bootstrapped via syntax-rules and the kernel macro expansion, and other dependencies
    /// are included via the usual macro expansion.
    ///
    /// In this way its always possible to refresh the native functions (and they don't disappear),
    /// and bundles of functions from third parties don't get included immediately into the global namespace.
    /// Scripts can choose to include these modules directly, or opt to not, and are not as risk of clobbering their
    /// global namespace.
    pub fn to_syntax(&self, prefix: Option<&str>) -> ExprKind {
        let module_name = self.unreadable_name();

        let mut defines = self
            .values
            .keys()
            .map(|x| {
                // TODO: Consider a custom delimeter as well
                // If we have a prefix, put the prefix at the front and append x
                // Otherwise, just default to using the provided name
                let name = prefix
                    .map(|pre| pre.to_string() + x)
                    .unwrap_or_else(|| x.to_string());

                ExprKind::Define(Box::new(crate::parser::ast::Define::new(
                    // TODO: Add the custom prefix here
                    // Handling a more complex case of qualifying imports
                    ExprKind::atom(name),
                    ExprKind::List(crate::parser::ast::List::new(vec![
                        ExprKind::atom(*MODULE_GET),
                        ExprKind::atom(module_name.clone()),
                        ExprKind::Quote(Box::new(crate::parser::ast::Quote::new(
                            ExprKind::atom(x.to_string()),
                            SyntaxObject::default(TokenType::Quote),
                        ))),
                    ])),
                    SyntaxObject::default(TokenType::Define),
                )))
            })
            .collect::<Vec<_>>();

        defines.push(ExprKind::List(crate::parser::ast::List::new(vec![
            ExprKind::atom(*MODULE_GET),
            ExprKind::atom("%-builtin-module-".to_string() + "steel/constants"),
            ExprKind::Quote(Box::new(crate::parser::ast::Quote::new(
                ExprKind::atom(*VOID),
                SyntaxObject::default(TokenType::Quote),
            ))),
        ])));

        ExprKind::Begin(crate::parser::ast::Begin::new(
            defines,
            SyntaxObject::default(TokenType::Begin),
        ))
    }
}

/// Documentation representation
#[derive(Clone, Debug)]
pub struct InternalDocumentation {
    definitions: im_rc::HashMap<Cow<'static, str>, Documentation<'static>>,
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

impl InternalDocumentation {
    pub fn new() -> Self {
        Self {
            definitions: im_rc::HashMap::new(),
        }
    }

    pub fn register_doc(
        &mut self,
        definition: impl Into<Cow<'static, str>>,
        description: Documentation<'static>,
    ) {
        self.definitions.insert(definition.into(), description);
    }

    pub fn get(&self, definition: &str) -> Option<&Documentation<'static>> {
        self.definitions.get(definition)
    }
}

// pub(crate) const LAST_DOC: &'static str = r#"

// (last l) -> any/c

//     l : list?

// Returns the last element in the list.
// Takes time proportional to the length of the list.

// Example:
//     > (last (list 1 2 3 4))
//     4

// "#;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub enum FFIDocumentation<'a> {
    Function(FFIDocTemplate<'a>),
    Module(FFIModuleDoc<'a>),
    Value(FFIValueDoc<'a>),
    Markdown(FFIMarkdownDoc<'a>),
}

// TODO: We're gonna need an FFI safe variant of this as well
#[derive(Debug, Clone, PartialEq)]
pub enum Documentation<'a> {
    Function(DocTemplate<'a>),
    Module(ModuleDoc<'a>),
    Value(ValueDoc<'a>),
    Markdown(MarkdownDoc<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocTemplate<'a> {
    pub signature: &'a str,
    pub params: &'a [&'a str],
    pub description: &'a str,
    pub examples: &'a [(&'a str, &'a str)],
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIDocTemplate<'a> {
    pub signature: RStr<'a>,
    pub params: RSlice<'a, RStr<'a>>,
    pub description: RStr<'a>,
    pub examples: RSlice<'a, Tuple2<RStr<'a>, RStr<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDoc<'a> {
    pub name: &'a str,
    pub description: &'a str,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIModuleDoc<'a> {
    pub name: RStr<'a>,
    pub description: RStr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueDoc<'a> {
    pub name: &'a str,
    pub description: &'a str,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIValueDoc<'a> {
    pub name: RStr<'a>,
    pub description: RStr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MarkdownDoc<'a>(pub &'a str);

#[repr(C)]
#[derive(Debug, Clone, PartialEq, StableAbi)]
pub struct FFIMarkdownDoc<'a>(pub RStr<'a>);

impl<'a> std::fmt::Display for MarkdownDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", termimad::text(self.0))
    }
}

impl<'a> std::fmt::Display for Documentation<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Documentation::Function(d) => write!(f, "{d}"),
            Documentation::Module(d) => write!(f, "{d}"),
            Documentation::Value(d) => write!(f, "{d}"),
            Documentation::Markdown(d) => write!(f, "{d}"),
        }
    }
}

impl<'a> std::fmt::Display for DocTemplate<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        writeln!(f, "{}", self.signature)?;
        writeln!(f)?;

        for param in self.params {
            writeln!(f, "   {param}")?
        }

        writeln!(f)?;
        writeln!(f, "{}", self.description)?;
        writeln!(f)?;
        if !self.examples.is_empty() {
            writeln!(f, "Examples:")?;
            for (example, output) in self.examples {
                writeln!(f, "   {example}")?;
                writeln!(f, "   {output}")?;
            }
        }

        Ok(())
    }
}

impl<'a> std::fmt::Display for ValueDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        writeln!(f, "{}", self.name)?;
        writeln!(f)?;
        writeln!(f, "{}", self.description)
    }
}

impl<'a> std::fmt::Display for ModuleDoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        writeln!(f, "{}", self.name)?;
        writeln!(f)?;
        writeln!(f, "{}", self.description)
    }
}

impl<'a> From<DocTemplate<'a>> for Documentation<'a> {
    fn from(val: DocTemplate<'a>) -> Self {
        Documentation::Function(val)
    }
}

impl<'a> From<ModuleDoc<'a>> for Documentation<'a> {
    fn from(val: ModuleDoc<'a>) -> Self {
        Documentation::Module(val)
    }
}

impl<'a> From<ValueDoc<'a>> for Documentation<'a> {
    fn from(val: ValueDoc<'a>) -> Self {
        Documentation::Value(val)
    }
}

impl<'a> From<MarkdownDoc<'a>> for Documentation<'a> {
    fn from(val: MarkdownDoc<'a>) -> Self {
        Documentation::Markdown(val)
    }
}

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
