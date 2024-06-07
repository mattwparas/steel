pub mod cycles;

use crate::{
    gc::{unsafe_erased_pointers::OpaqueReference, Gc},
    parser::{
        ast::{self, Atom, ExprKind},
        parser::SyntaxObject,
        span::Span,
        tokens::TokenType,
    },
    primitives::numbers::realp,
    rerrs::{ErrorKind, SteelErr},
    steel_vm::vm::{threads::closure_into_serializable, BuiltInSignature, Continuation},
    values::port::SteelPort,
    values::{
        closed::{Heap, HeapRef, MarkAndSweepContext},
        functions::ByteCodeLambda,
        lazy_stream::LazyStream,
        port::SendablePort,
        structs::SerializableUserDefinedStruct,
        transducers::{Reducer, Transducer},
        SteelPortRepr,
    },
    values::{functions::BoxedDynFunction, structs::UserDefinedStruct},
};
use std::vec::IntoIter;
use std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell, RefMut},
    cmp::Ordering,
    convert::TryInto,
    fmt,
    future::Future,
    hash::{Hash, Hasher},
    io::Write,
    ops::Deref,
    pin::Pin,
    rc::Rc,
    result,
    sync::{Arc, Mutex},
    task::Context,
};

// TODO
#[macro_export]
macro_rules! list {
    () => { $crate::rvals::SteelVal::ListV(
        im_lists::list![]
    ) };

    ( $($x:expr),* ) => {{
        $crate::rvals::SteelVal::ListV(vec![$(
            $crate::rvals::IntoSteelVal::into_steelval($x).unwrap()
        ), *].into())
    }};

    ( $($x:expr ,)* ) => {{
        $crate::rvals::SteelVal::ListV(im_lists::list![$(
            $crate::rvals::IntoSteelVal::into_steelval($x).unwrap()
        )*])
    }};
}

use SteelVal::*;

use im_rc::{HashMap, Vector};

use futures_task::noop_waker_ref;
use futures_util::future::Shared;
use futures_util::FutureExt;

use crate::values::lists::List;
use num::{BigInt, BigRational, Rational32, Signed, ToPrimitive, Zero};
use steel_parser::tokens::{IntLiteral, RealLiteral};

use self::cycles::{CycleDetector, IterativeDropHandler};

pub type RcRefSteelVal = Rc<RefCell<SteelVal>>;
pub fn new_rc_ref_cell(x: SteelVal) -> RcRefSteelVal {
    Rc::new(RefCell::new(x))
}

pub type Result<T> = result::Result<T, SteelErr>;
pub type FunctionSignature = fn(&[SteelVal]) -> Result<SteelVal>;
pub type MutFunctionSignature = fn(&mut [SteelVal]) -> Result<SteelVal>;
pub type BoxedFunctionSignature = Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>>;
pub type BoxedAsyncFunctionSignature = Box<Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>>;
pub type AsyncSignature = fn(&[SteelVal]) -> FutureResult;

pub type BoxedFutureResult = Pin<Box<dyn Future<Output = Result<SteelVal>>>>;

#[derive(Clone)]
pub struct FutureResult(Shared<BoxedFutureResult>);

impl FutureResult {
    pub fn new(fut: BoxedFutureResult) -> Self {
        // FutureResult()
        FutureResult(fut.shared())
    }

    pub fn into_shared(self) -> Shared<BoxedFutureResult> {
        self.0
    }
}

// This is an attempt to one off poll a future
// This should enable us to use embedded async functions
// Will require using call/cc w/ a thread queue in steel, however it should be possible
pub(crate) fn poll_future(mut fut: Shared<BoxedFutureResult>) -> Option<Result<SteelVal>> {
    // If the future has already been awaited (by somebody) get that value instead
    if let Some(output) = fut.peek() {
        return Some(output.clone());
    }

    // Otherwise, go ahead and poll the value to see if its ready
    // The context is going to exist exclusively in Steel, hidden behind an `await`
    let waker = noop_waker_ref();
    let context = &mut Context::from_waker(waker);

    // Polling requires a pinned future - TODO make sure this is correct
    let mut_fut = Pin::new(&mut fut);

    match Future::poll(mut_fut, context) {
        std::task::Poll::Ready(r) => Some(r),
        std::task::Poll::Pending => None,
    }
}

/// Attempt to cast this custom type down to the underlying type
pub fn as_underlying_type<T: 'static>(value: &dyn CustomType) -> Option<&T> {
    value.as_any_ref().downcast_ref::<T>()
}

pub fn as_underlying_type_mut<T: 'static>(value: &mut dyn CustomType) -> Option<&mut T> {
    value.as_any_ref_mut().downcast_mut::<T>()
}

pub trait Custom: private::Sealed {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        None
    }

    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        None
    }

    fn as_iterator(&self) -> Option<Box<dyn Iterator<Item = SteelVal>>> {
        None
    }

    fn gc_drop_mut(&mut self, _drop_handler: &mut IterativeDropHandler) {}

    fn gc_visit_children(&self, _context: &mut MarkAndSweepContext) {}

    fn visit_equality(&self, _visitor: &mut cycles::EqualityVisitor) {}

    fn equality_hint(&self, _other: &dyn CustomType) -> bool {
        true
    }

    fn equality_hint_general(&self, _other: &SteelVal) -> bool {
        false
    }
}

pub trait CustomType {
    // fn box_clone(&self) -> Box<dyn CustomType>;
    // fn as_any(&self) -> Box<dyn Any>;
    fn as_any_ref(&self) -> &dyn Any;
    fn as_any_ref_mut(&mut self) -> &mut dyn Any;
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }
    fn inner_type_id(&self) -> TypeId;
    // fn new_steel_val(&self) -> SteelVal;
    fn display(&self) -> std::result::Result<String, std::fmt::Error> {
        Ok(format!("#<{}>", self.name().to_string()))
    }

    fn as_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        None
    }

    // Implement visit for anything that holds steel values
    fn drop_mut(&mut self, _drop_handler: &mut IterativeDropHandler) {}

    fn visit_children(&self, _context: &mut MarkAndSweepContext) {}
    fn visit_children_for_equality(&self, _visitor: &mut cycles::EqualityVisitor) {}

    fn check_equality_hint(&self, _other: &dyn CustomType) -> bool {
        true
    }

    fn check_equality_hint_general(&self, _other: &SteelVal) -> bool {
        false
    }
}

impl<T: Custom + 'static> CustomType for T {
    fn as_any_ref(&self) -> &dyn Any {
        self as &dyn Any
    }
    fn as_any_ref_mut(&mut self) -> &mut dyn Any {
        self as &mut dyn Any
    }
    fn inner_type_id(&self) -> TypeId {
        std::any::TypeId::of::<Self>()
    }
    fn display(&self) -> std::result::Result<String, std::fmt::Error> {
        if let Some(formatted) = self.fmt() {
            formatted
        } else {
            Ok(format!("#<{}>", self.name().to_string()))
        }
    }

    fn as_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        <T as Custom>::into_serializable_steelval(self)
    }

    fn drop_mut(&mut self, drop_handler: &mut IterativeDropHandler) {
        self.gc_drop_mut(drop_handler)
    }

    fn visit_children(&self, context: &mut MarkAndSweepContext) {
        self.gc_visit_children(context)
    }

    // TODO: Equality visitor
    fn visit_children_for_equality(&self, visitor: &mut cycles::EqualityVisitor) {
        self.visit_equality(visitor)
    }

    fn check_equality_hint(&self, other: &dyn CustomType) -> bool {
        self.equality_hint(other)
    }

    fn check_equality_hint_general(&self, other: &SteelVal) -> bool {
        self.equality_hint_general(other)
    }
}

impl<T: CustomType + 'static> IntoSteelVal for T {
    fn into_steelval(self) -> Result<SteelVal> {
        // Ok(self.new_steel_val())
        Ok(SteelVal::Custom(Gc::new(RefCell::new(Box::new(self)))))
    }
}

pub trait IntoSerializableSteelVal {
    fn into_serializable_steelval(val: &SteelVal) -> Result<SerializableSteelVal>;
}

impl<T: CustomType + Clone + Send + Sync + 'static> IntoSerializableSteelVal for T {
    fn into_serializable_steelval(val: &SteelVal) -> Result<SerializableSteelVal> {
        if let SteelVal::Custom(v) = val {
            // let left_type = v.borrow().as_any_ref();
            // TODO: @Matt - dylibs cause issues here, as the underlying type ids are different
            // across workspaces and builds
            let left = v.borrow().as_any_ref().downcast_ref::<T>().cloned();
            let _lifted = left.ok_or_else(|| {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {:?}, did not match the given type: {}",
                    val,
                    std::any::type_name::<Self>()
                );
                SteelErr::new(ErrorKind::ConversionError, error_message)
            });

            todo!()
        } else {
            let error_message = format!(
                "Type Mismatch: Type of SteelVal: {:?} did not match the given type, expecting opaque struct: {}",
                val,
                std::any::type_name::<Self>()
            );

            Err(SteelErr::new(ErrorKind::ConversionError, error_message))
        }
    }
}

// TODO: Marshalling out of the type could also try to yoink from a native steel struct.
// If possible, we can try to line the constructor up with the fields
impl<T: CustomType + Clone + 'static> FromSteelVal for T {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::Custom(v) = val {
            // let left_type = v.borrow().as_any_ref();
            // TODO: @Matt - dylibs cause issues here, as the underlying type ids are different
            // across workspaces and builds
            let left = v.borrow().as_any_ref().downcast_ref::<T>().cloned();
            left.ok_or_else(|| {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {:?}, did not match the given type: {}",
                    val,
                    std::any::type_name::<Self>()
                );
                SteelErr::new(ErrorKind::ConversionError, error_message)
            })
        } else {
            let error_message = format!(
                "Type Mismatch: Type of SteelVal: {:?} did not match the given type, expecting opaque struct: {}",
                val,
                std::any::type_name::<Self>()
            );

            Err(SteelErr::new(ErrorKind::ConversionError, error_message))
        }
    }
}

/// The entry point for turning values into SteelVals
/// The is implemented for most primitives and collections
/// You can also manually implement this for any type, or can optionally
/// get this implementation for a custom struct by using the custom
/// steel derive.
pub trait IntoSteelVal: Sized {
    fn into_steelval(self) -> Result<SteelVal>;
}

/// The exit point for turning SteelVals into outside world values
/// This is implement for most primitives and collections
/// You can also manually implement this for any type, or can optionally
/// get this implementation for a custom struct by using the custom
/// steel derive.
pub trait FromSteelVal: Sized {
    fn from_steelval(val: &SteelVal) -> Result<Self>;
}

pub trait PrimitiveAsRef<'a>: Sized {
    fn primitive_as_ref(val: &'a SteelVal) -> Result<Self>;
    fn maybe_primitive_as_ref(val: &'a SteelVal) -> Option<Self>;
}

pub trait PrimitiveAsRefMut<'a>: Sized {
    fn primitive_as_ref(val: &'a mut SteelVal) -> Result<Self>;
    fn maybe_primitive_as_ref(val: &'a mut SteelVal) -> Option<Self>;
}

pub struct RestArgsIter<'a, T>(
    pub std::iter::Map<std::slice::Iter<'a, SteelVal>, fn(&'a SteelVal) -> Result<T>>,
);

impl<'a, T: PrimitiveAsRef<'a> + 'a> RestArgsIter<'a, T> {
    pub fn new(
        args: std::iter::Map<std::slice::Iter<'a, SteelVal>, fn(&'a SteelVal) -> Result<T>>,
    ) -> Self {
        RestArgsIter(args)
    }

    pub fn from_slice(args: &'a [SteelVal]) -> Result<Self> {
        Ok(RestArgsIter(args.iter().map(T::primitive_as_ref)))
    }
}

impl<'a, T> Iterator for RestArgsIter<'a, T> {
    type Item = Result<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub struct RestArgs<T: FromSteelVal>(pub Vec<T>);

impl<T: FromSteelVal> RestArgs<T> {
    pub fn new(args: Vec<T>) -> Self {
        RestArgs(args)
    }

    pub fn from_slice(args: &[SteelVal]) -> Result<Self> {
        args.iter()
            .map(|x| T::from_steelval(x))
            .collect::<Result<Vec<_>>>()
            .map(RestArgs)
    }
}

impl<T: FromSteelVal> std::ops::Deref for RestArgs<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

mod private {

    use std::any::Any;

    pub trait Sealed {}

    impl<T: Any> Sealed for T {}
}

// pub trait DowncastSteelval: private::Sealed {
//     type Output;
//     fn downcast(&self) -> Result<&Self::Output>;
// }

// impl DowncastSteelval for SteelVal {
//     type Output = Box<dyn CustomType>;

//     fn downcast(&self) -> Result<&Self::Output> {
//         todo!()
//     }
// }

pub enum SRef<'b, T: ?Sized + 'b> {
    Temporary(&'b T),
    Owned(Ref<'b, T>),
    // ExistingBorrow(S),
}

impl<'b, T: ?Sized + 'b> Deref for SRef<'b, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        match self {
            SRef::Temporary(inner) => inner,
            SRef::Owned(inner) => inner,
            // SRef::ExistingBorrow(inner) =>
        }
    }
}

// Can you take a steel val and execute operations on it by reference
pub trait AsRefSteelVal: Sized {
    type Nursery: Default;

    fn as_ref<'b, 'a: 'b>(
        val: &'a SteelVal,
        _nursery: &'a mut Self::Nursery,
    ) -> Result<SRef<'b, Self>>;
}

pub trait AsSlice<T> {
    fn as_slice_repr(&self) -> &[T];
}

impl<T> AsSlice<T> for Vec<T> {
    fn as_slice_repr(&self) -> &[T] {
        self.as_slice()
    }
}

// TODO: Try to incorporate these all into one trait if possible
pub trait AsRefSteelValFromUnsized<T>: Sized {
    type Output: AsSlice<T>;

    fn as_ref_from_unsized(val: &SteelVal) -> Result<Self::Output>;
}

pub trait AsRefMutSteelVal: Sized {
    fn as_mut_ref<'b, 'a: 'b>(val: &'a SteelVal) -> Result<RefMut<'b, Self>>;
}

pub trait AsRefMutSteelValFromRef: Sized {
    fn as_mut_ref_from_ref<'a>(val: &'a SteelVal) -> crate::rvals::Result<&'a mut Self>;
}

pub trait AsRefSteelValFromRef: Sized {
    fn as_ref_from_ref<'a>(val: &'a SteelVal) -> crate::rvals::Result<&'a Self>;
}

// impl AsRefSteelVal for List<SteelVal> {
//     type Nursery = ();

//     fn as_ref<'b, 'a: 'b>(val: &'a SteelVal, _nursery: &mut ()) -> Result<SRef<'b, Self>> {
//         if let SteelVal::ListV(l) = val {
//             Ok(SRef::Temporary(l))
//         } else {
//             stop!(TypeMismatch => "Value cannot be referenced as a list")
//         }
//     }
// }

impl AsRefSteelVal for UserDefinedStruct {
    type Nursery = ();

    fn as_ref<'b, 'a: 'b>(val: &'a SteelVal, _nursery: &mut ()) -> Result<SRef<'b, Self>> {
        if let SteelVal::CustomStruct(l) = val {
            Ok(SRef::Temporary(l))
        } else {
            stop!(TypeMismatch => "Value cannot be referenced as a list")
        }
    }
}

// impl AsRefSteelVal for FunctionSignature {
//     fn as_ref<'b, 'a: 'b>(val: &'a SteelVal) -> Result<SRef<'b, Self>> {
//         if let SteelVal::FuncV(f) = val {
//             Ok(SRef::Temporary(f))
//         } else {
//             stop!(TypeMismatch => "Value cannot be referenced as a primitive function!")
//         }
//     }
// }

impl<T: CustomType + 'static> AsRefSteelVal for T {
    type Nursery = ();

    fn as_ref<'b, 'a: 'b>(
        val: &'a SteelVal,
        _nursery: &mut Self::Nursery,
    ) -> Result<SRef<'b, Self>> {
        // todo!()

        if let SteelVal::Custom(v) = val {
            let res = Ref::map(v.borrow(), |x| x.as_any_ref());

            if res.is::<T>() {
                Ok(SRef::Owned(Ref::map(res, |x| {
                    x.downcast_ref::<T>().unwrap()
                })))
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    val,
                    std::any::type_name::<Self>()
                );
                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
            // res
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

impl<T: CustomType + 'static> AsRefMutSteelVal for T {
    fn as_mut_ref<'b, 'a: 'b>(val: &'a SteelVal) -> Result<RefMut<'b, Self>> {
        // todo!()

        if let SteelVal::Custom(v) = val {
            let res = RefMut::map(v.borrow_mut(), |x| x.as_any_ref_mut());

            if res.is::<T>() {
                Ok(RefMut::map(res, |x| x.downcast_mut::<T>().unwrap()))
            } else {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal: {} did not match the given type: {}",
                    val,
                    std::any::type_name::<Self>()
                );
                Err(SteelErr::new(ErrorKind::ConversionError, error_message))
            }
            // res
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

impl ast::TryFromSteelValVisitorForExprKind {
    pub fn visit_syntax_object(&mut self, value: &Syntax) -> Result<ExprKind> {
        let span = value.span;
        // dbg!(&span);
        // let source = self.source.clone();
        match &value.syntax {
            // Mutual recursion case
            SyntaxObject(s) => self.visit_syntax_object(s),
            BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::BooleanLiteral(*x),
                span,
            )))),
            NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                RealLiteral::Float(*x).into(),
                span,
            )))),
            IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                RealLiteral::Int(IntLiteral::Small(*x)).into(),
                span,
            )))),
            VectorV(lst) => {
                let items: Result<Vec<ExprKind>> = lst.iter().map(|x| self.visit(x)).collect();
                Ok(ExprKind::List(crate::parser::ast::List::new(items?)))
            }
            StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::StringLiteral(x.to_string()),
                span,
            )))),
            SymbolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::Identifier(x.as_str().into()),
                span,
            )))),

            ListV(l) => {
                // Rooted - things operate as normal
                if self.qq_depth == 0 {
                    let maybe_special_form = l.first().and_then(|x| {
                        x.as_symbol()
                            .or_else(|| x.as_syntax_object().and_then(|x| x.syntax.as_symbol()))
                    });

                    match maybe_special_form {
                        Some(x) if x.as_str() == "quote" => {
                            if self.quoted {
                                let items: std::result::Result<Vec<ExprKind>, _> =
                                    l.iter().map(|x| self.visit(x)).collect();

                                return Ok(ExprKind::List(ast::List::new(items?)));
                            }

                            self.quoted = true;

                            let return_value = l
                                .into_iter()
                                .map(|x| self.visit(x))
                                .collect::<std::result::Result<Vec<_>, _>>()?
                                .try_into()?;

                            self.quoted = false;

                            return Ok(return_value);
                        } // "quasiquote" => {
                        //     self.qq_depth += 1;
                        // }
                        // None => {
                        // return Ok(ExprKind::empty());
                        // }
                        _ => {}
                    }
                }

                Ok(l.into_iter()
                    .map(|x| self.visit(x))
                    .collect::<std::result::Result<Vec<_>, _>>()?
                    .try_into()?)
            }

            CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::CharacterLiteral(*x),
                span,
            )))),
            _ => stop!(ConversionError => "unable to convert {:?} to expression", &value.syntax),
        }
    }
}

// TODO: Replace this with RawSyntaxObject<SteelVal>

#[derive(Debug, Clone)]
pub struct Syntax {
    pub(crate) raw: Option<SteelVal>,
    pub(crate) syntax: SteelVal,
    span: Span,
}

impl Syntax {
    pub fn new(syntax: SteelVal, span: Span) -> Syntax {
        // dbg!(&syntax);

        Self {
            raw: None,
            syntax,
            span,
        }
    }

    pub fn proto(raw: SteelVal, syntax: SteelVal, span: Span) -> Syntax {
        Self {
            raw: Some(raw),
            syntax,
            span,
        }
    }

    pub fn syntax_e(&self) -> SteelVal {
        self.syntax.clone()
    }

    pub fn new_with_source(syntax: SteelVal, span: Span) -> Syntax {
        Self {
            raw: None,
            syntax,
            span,
        }
    }

    pub fn syntax_loc(&self) -> Span {
        self.span
    }

    pub fn syntax_datum(&self) -> SteelVal {
        self.raw.clone().unwrap()
    }

    pub(crate) fn steelval_to_exprkind(value: &SteelVal) -> Result<ExprKind> {
        match value {
            // Mutual recursion case
            SyntaxObject(s) => s.to_exprkind(),
            BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::BooleanLiteral(*x),
            )))),
            NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                RealLiteral::Float(*x).into(),
            )))),
            IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                RealLiteral::Int(IntLiteral::Small(*x)).into(),
            )))),
            VectorV(lst) => {
                let items: Result<Vec<ExprKind>> =
                    lst.iter().map(Self::steelval_to_exprkind).collect();
                Ok(ExprKind::List(crate::parser::ast::List::new(items?)))
            }
            StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::StringLiteral(x.to_string()),
            )))),
            // LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            // MacroV(_) => Err("Can't convert from Macro to expression!"),
            SymbolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::Identifier(x.as_str().into()),
            )))),
            ListV(l) => {
                let items: Result<Vec<ExprKind>> =
                    l.iter().map(Self::steelval_to_exprkind).collect();

                Ok(ExprKind::List(crate::parser::ast::List::new(items?)))
            }
            CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::default(
                TokenType::CharacterLiteral(*x),
            )))),
            _ => stop!(ConversionError => "unable to convert {:?} to expression", value),
        }
    }

    // TODO: match on self.syntax. If its itself a syntax object, then just recur on that until we bottom out
    // Otherwise, reconstruct the ExprKind and replace the span and source information into the representation
    pub fn to_exprkind(&self) -> Result<ExprKind> {
        let span = self.span;
        // let source = self.source.clone();
        match &self.syntax {
            // Mutual recursion case
            SyntaxObject(s) => s.to_exprkind(),
            BoolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::BooleanLiteral(*x),
                span,
            )))),
            NumV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                RealLiteral::Float(*x).into(),
                span,
            )))),
            IntV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                RealLiteral::Int(IntLiteral::Small(*x)).into(),
                span,
            )))),
            VectorV(lst) => {
                let items: Result<Vec<ExprKind>> =
                    lst.iter().map(Self::steelval_to_exprkind).collect();
                Ok(ExprKind::List(crate::parser::ast::List::new(items?)))
            }
            StringV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::StringLiteral(x.to_string()),
                span,
            )))),
            // LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            // MacroV(_) => Err("Can't convert from Macro to expression!"),
            SymbolV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::Identifier(x.as_str().into()),
                span,
            )))),
            ListV(l) => {
                let items: Result<Vec<ExprKind>> =
                    l.iter().map(Self::steelval_to_exprkind).collect();

                Ok(ExprKind::List(crate::parser::ast::List::new(items?)))
            }
            CharV(x) => Ok(ExprKind::Atom(Atom::new(SyntaxObject::new(
                TokenType::CharacterLiteral(*x),
                span,
            )))),
            _ => stop!(ConversionError => "unable to convert {:?} to expression", &self.syntax),
        }
    }
}

impl IntoSteelVal for Syntax {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(SteelVal::SyntaxObject(Gc::new(self)))
    }
}

impl AsRefSteelVal for Syntax {
    type Nursery = ();

    fn as_ref<'b, 'a: 'b>(
        val: &'a SteelVal,
        _nursery: &'a mut Self::Nursery,
    ) -> Result<SRef<'b, Self>> {
        if let SteelVal::SyntaxObject(s) = val {
            Ok(SRef::Temporary(s))
        } else {
            stop!(TypeMismatch => "Value cannot be referenced as a syntax object: {}", val)
        }
    }
}

impl From<Syntax> for SteelVal {
    fn from(val: Syntax) -> Self {
        SteelVal::SyntaxObject(Gc::new(val))
    }
}

// TODO:
// This needs to be a method on the runtime: in order to properly support
// threads
// Tracking issue here: https://github.com/mattwparas/steel/issues/98

// Values which can be sent to another thread.
// If it cannot be sent to another thread, then we'll error out on conversion.
// TODO: Add boxed dyn functions to this.
// #[derive(PartialEq)]
pub enum SerializableSteelVal {
    Closure(crate::values::functions::SerializedLambda),
    BoolV(bool),
    NumV(f64),
    IntV(isize),
    CharV(char),
    Void,
    StringV(String),
    FuncV(FunctionSignature),
    MutFunc(MutFunctionSignature),
    HashMapV(Vec<(SerializableSteelVal, SerializableSteelVal)>),
    ListV(Vec<SerializableSteelVal>),
    VectorV(Vec<SerializableSteelVal>),
    BoxedDynFunction(BoxedDynFunction),
    BuiltIn(BuiltInSignature),
    SymbolV(String),
    Custom(Box<dyn CustomType + Send>),
    CustomStruct(SerializableUserDefinedStruct),
    // Attempt to reuse the storage if possible
    HeapAllocated(usize),
    Port(SendablePort),
}

pub enum SerializedHeapRef {
    Serialized(Option<SerializableSteelVal>),
    Closed(HeapRef<SteelVal>),
}

pub struct HeapSerializer<'a> {
    pub heap: &'a mut Heap,
    pub fake_heap: &'a mut std::collections::HashMap<usize, SerializedHeapRef>,
    // After the conversion, we go back through, and patch the values from the fake heap
    // in to each of the values listed here - otherwise, we'll miss cycles
    pub values_to_fill_in: &'a mut std::collections::HashMap<usize, HeapRef<SteelVal>>,

    // Cache the functions that get built
    pub built_functions: &'a mut std::collections::HashMap<usize, Gc<ByteCodeLambda>>,
}

// Once crossed over the line, convert BACK into a SteelVal
// This should be infallible.
pub fn from_serializable_value(ctx: &mut HeapSerializer, val: SerializableSteelVal) -> SteelVal {
    match val {
        SerializableSteelVal::Closure(c) => {
            if c.captures.is_empty() {
                if let Some(already_made) = ctx.built_functions.get(&c.id) {
                    SteelVal::Closure(already_made.clone())
                } else {
                    let id = c.id;
                    let value = Gc::new(ByteCodeLambda::from_serialized(ctx, c));

                    // Save those as well
                    // Probably need to just do this for all
                    ctx.built_functions.insert(id, value.clone());
                    SteelVal::Closure(value)
                }
            } else {
                SteelVal::Closure(Gc::new(ByteCodeLambda::from_serialized(ctx, c)))
            }
        }
        SerializableSteelVal::BoolV(b) => SteelVal::BoolV(b),
        SerializableSteelVal::NumV(n) => SteelVal::NumV(n),
        SerializableSteelVal::IntV(i) => SteelVal::IntV(i),
        SerializableSteelVal::CharV(c) => SteelVal::CharV(c),
        SerializableSteelVal::Void => SteelVal::Void,
        SerializableSteelVal::StringV(s) => SteelVal::StringV(s.into()),
        SerializableSteelVal::FuncV(f) => SteelVal::FuncV(f),
        SerializableSteelVal::MutFunc(f) => SteelVal::MutFunc(f),
        SerializableSteelVal::HashMapV(h) => SteelVal::HashMapV(
            Gc::new(
                h.into_iter()
                    .map(|(k, v)| {
                        (
                            from_serializable_value(ctx, k),
                            from_serializable_value(ctx, v),
                        )
                    })
                    .collect::<HashMap<_, _>>(),
            )
            .into(),
        ),
        SerializableSteelVal::ListV(v) => SteelVal::ListV(
            v.into_iter()
                .map(|x| from_serializable_value(ctx, x))
                .collect(),
        ),
        SerializableSteelVal::VectorV(v) => SteelVal::VectorV(SteelVector(Gc::new(
            v.into_iter()
                .map(|x| from_serializable_value(ctx, x))
                .collect(),
        ))),
        SerializableSteelVal::BoxedDynFunction(f) => SteelVal::BoxedFunction(Rc::new(f)),
        SerializableSteelVal::BuiltIn(f) => SteelVal::BuiltIn(f),
        SerializableSteelVal::SymbolV(s) => SteelVal::SymbolV(s.into()),
        SerializableSteelVal::Custom(b) => SteelVal::Custom(Gc::new(RefCell::new(b))),
        SerializableSteelVal::CustomStruct(s) => {
            SteelVal::CustomStruct(Gc::new(UserDefinedStruct {
                fields: {
                    let fields = s
                        .fields
                        .into_iter()
                        .map(|x| from_serializable_value(ctx, x));

                    let mut recycle: crate::values::recycler::Recycle<Vec<_>> =
                        crate::values::recycler::Recycle::new();

                    recycle.extend(fields);

                    recycle
                },
                type_descriptor: s.type_descriptor,
            }))
        }
        SerializableSteelVal::Port(p) => SteelVal::PortV(SteelPort::from_sendable_port(p)),
        SerializableSteelVal::HeapAllocated(v) => {
            // todo!()

            if let Some(mut guard) = ctx.fake_heap.get_mut(&v) {
                match &mut guard {
                    SerializedHeapRef::Serialized(value) => {
                        let value = std::mem::take(value);

                        if let Some(value) = value {
                            let value = from_serializable_value(ctx, value);
                            let allocation = ctx.heap.allocate_without_collection(value);

                            ctx.fake_heap
                                .insert(v, SerializedHeapRef::Closed(allocation.clone()));

                            SteelVal::HeapAllocated(allocation)
                        } else {
                            // println!("If we're getting here - it means the value from the heap has already
                            // been converting. if so, we should do something...");

                            let fake_allocation =
                                ctx.heap.allocate_without_collection(SteelVal::Void);

                            ctx.values_to_fill_in.insert(v, fake_allocation.clone());

                            SteelVal::HeapAllocated(fake_allocation)
                        }
                    }

                    SerializedHeapRef::Closed(c) => SteelVal::HeapAllocated(c.clone()),
                }
            } else {
                // Shouldn't silently fail here, but we will... for now

                let allocation = ctx.heap.allocate_without_collection(SteelVal::Void);

                ctx.fake_heap
                    .insert(v, SerializedHeapRef::Closed(allocation.clone()));

                SteelVal::HeapAllocated(allocation)
            }
        }
    }
}

// The serializable value needs to refer to the original heap -
// that way can reference the original stuff easily.

// TODO: Use the cycle detector instead
pub fn into_serializable_value(
    val: SteelVal,
    serialized_heap: &mut std::collections::HashMap<usize, SerializableSteelVal>,
    visited: &mut std::collections::HashSet<usize>,
) -> Result<SerializableSteelVal> {
    // dbg!(&serialized_heap);

    match val {
        SteelVal::Closure(c) => closure_into_serializable(&c, serialized_heap, visited)
            .map(SerializableSteelVal::Closure),
        SteelVal::BoolV(b) => Ok(SerializableSteelVal::BoolV(b)),
        SteelVal::NumV(n) => Ok(SerializableSteelVal::NumV(n)),
        SteelVal::IntV(n) => Ok(SerializableSteelVal::IntV(n)),
        SteelVal::CharV(c) => Ok(SerializableSteelVal::CharV(c)),
        SteelVal::Void => Ok(SerializableSteelVal::Void),
        SteelVal::StringV(s) => Ok(SerializableSteelVal::StringV(s.to_string())),
        SteelVal::FuncV(f) => Ok(SerializableSteelVal::FuncV(f)),
        SteelVal::ListV(l) => Ok(SerializableSteelVal::ListV(
            l.into_iter()
                .map(|x| into_serializable_value(x, serialized_heap, visited))
                .collect::<Result<_>>()?,
        )),
        SteelVal::BoxedFunction(f) => Ok(SerializableSteelVal::BoxedDynFunction((*f).clone())),
        SteelVal::BuiltIn(f) => Ok(SerializableSteelVal::BuiltIn(f)),
        SteelVal::SymbolV(s) => Ok(SerializableSteelVal::SymbolV(s.to_string())),
        SteelVal::MutFunc(f) => Ok(SerializableSteelVal::MutFunc(f)),
        SteelVal::HashMapV(v) => Ok(SerializableSteelVal::HashMapV(
            v.0.unwrap()
                .into_iter()
                .map(|(k, v)| {
                    let kprime = into_serializable_value(k, serialized_heap, visited)?;
                    let vprime = into_serializable_value(v, serialized_heap, visited)?;

                    Ok((kprime, vprime))
                })
                .collect::<Result<_>>()?,
        )),

        SteelVal::Custom(c) => {
            if let Some(output) = c.borrow_mut().as_serializable_steelval() {
                Ok(output)
            } else {
                stop!(Generic => "Custom type not allowed to be moved across threads!")
            }
        }

        SteelVal::CustomStruct(s) => Ok(SerializableSteelVal::CustomStruct(
            SerializableUserDefinedStruct {
                fields: s
                    .fields
                    .iter()
                    .cloned()
                    .map(|x| into_serializable_value(x, serialized_heap, visited))
                    .collect::<Result<Vec<_>>>()?,
                type_descriptor: s.type_descriptor,
            },
        )),

        SteelVal::PortV(p) => SendablePort::from_port(p).map(SerializableSteelVal::Port),

        // If there is a cycle, this could cause problems?
        SteelVal::HeapAllocated(h) => {
            // We should pick it up on the way back the recursion
            if visited.contains(&h.as_ptr_usize())
                && !serialized_heap.contains_key(&h.as_ptr_usize())
            {
                // println!("Already visited: {}", h.as_ptr_usize());

                Ok(SerializableSteelVal::HeapAllocated(h.as_ptr_usize()))
            } else {
                visited.insert(h.as_ptr_usize());

                if serialized_heap.contains_key(&h.as_ptr_usize()) {
                    // println!("Already exists in map: {}", h.as_ptr_usize());

                    Ok(SerializableSteelVal::HeapAllocated(h.as_ptr_usize()))
                } else {
                    // println!("Trying to insert: {} @ {}", h.get(), h.as_ptr_usize());

                    let value = into_serializable_value(h.get(), serialized_heap, visited);

                    let value = match value {
                        Ok(v) => v,
                        Err(e) => {
                            // println!("{}", e);
                            return Err(e);
                        }
                    };

                    serialized_heap.insert(h.as_ptr_usize(), value);

                    // println!("Inserting: {}", h.as_ptr_usize());

                    Ok(SerializableSteelVal::HeapAllocated(h.as_ptr_usize()))
                }
            }
        }

        illegal => stop!(Generic => "Type not allowed to be moved across threads!: {}", illegal),
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SteelMutableVector(pub(crate) Gc<RefCell<Vec<SteelVal>>>);

#[derive(Clone, PartialEq, Eq)]
pub struct SteelVector(pub(crate) Gc<im_rc::Vector<SteelVal>>);

impl Deref for SteelVector {
    type Target = im_rc::Vector<SteelVal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Gc<im_rc::Vector<SteelVal>>> for SteelVector {
    fn from(value: Gc<im_rc::Vector<SteelVal>>) -> Self {
        SteelVector(value)
    }
}

#[derive(Clone, PartialEq)]
pub struct SteelHashMap(pub(crate) Gc<HashMap<SteelVal, SteelVal>>);

impl Deref for SteelHashMap {
    type Target = HashMap<SteelVal, SteelVal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Gc<HashMap<SteelVal, SteelVal>>> for SteelHashMap {
    fn from(value: Gc<HashMap<SteelVal, SteelVal>>) -> Self {
        SteelHashMap(value)
    }
}

#[derive(Clone, PartialEq)]
pub struct SteelHashSet(pub(crate) Gc<im_rc::HashSet<SteelVal>>);

impl Deref for SteelHashSet {
    type Target = im_rc::HashSet<SteelVal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Gc<im_rc::HashSet<SteelVal>>> for SteelHashSet {
    fn from(value: Gc<im_rc::HashSet<SteelVal>>) -> Self {
        SteelHashSet(value)
    }
}

pub enum TypeKind {
    Any,
    Bool,
    Num,
    Int,
    Char,
    Vector(Box<TypeKind>),
    Void,
    String,
    Function,
    HashMap(Box<TypeKind>, Box<TypeKind>),
    HashSet(Box<TypeKind>),
    List(Box<TypeKind>),
}

// TODO: Make this repr(transparent)
// to work correctly with FFI

/// A value as represented in the runtime.
#[derive(Clone)]
pub enum SteelVal {
    /// Represents a bytecode closure.
    Closure(Gc<ByteCodeLambda>),
    /// Represents a boolean value.
    BoolV(bool),
    /// Represents a number, currently only f64 numbers are supported.
    NumV(f64),
    /// Represents an integer.
    IntV(isize),
    /// Represents a rational number.
    Rational(Rational32),
    /// Represents a character type
    CharV(char),
    /// Vectors are represented as `im_rc::Vector`'s, which are immutable
    /// data structures
    VectorV(SteelVector),
    /// Void return value
    Void,
    /// Represents strings
    StringV(SteelString),
    /// Represents built in rust functions
    FuncV(FunctionSignature),
    /// Represents a symbol, internally represented as `String`s
    SymbolV(SteelString),
    /// Container for a type that implements the `Custom Type` trait. (trait object)
    Custom(Gc<RefCell<Box<dyn CustomType>>>),
    // Embedded HashMap
    HashMapV(SteelHashMap),
    // Embedded HashSet
    HashSetV(SteelHashSet),
    /// Represents a scheme-only struct
    CustomStruct(Gc<UserDefinedStruct>),
    /// Represents a port object
    PortV(SteelPort),
    /// Generic iterator wrapper
    IterV(Gc<Transducer>),
    /// Reducers
    ReducerV(Gc<Reducer>),
    /// Async Function wrapper
    FutureFunc(BoxedAsyncFunctionSignature),
    // Boxed Future Result
    FutureV(Gc<FutureResult>),
    // A stream of `SteelVal`.
    StreamV(Gc<LazyStream>),
    /// Custom closure
    BoxedFunction(Rc<BoxedDynFunction>),
    // Continuation
    ContinuationFunction(Continuation),
    // Function Pointer
    // #[cfg(feature = "jit")]
    // CompiledFunction(Box<JitFunctionPointer>),
    // List
    ListV(crate::values::lists::List<SteelVal>),
    // Holds a pair that contains 2 `SteelVal`.
    Pair(Gc<crate::values::lists::Pair>),
    // Mutable functions
    MutFunc(MutFunctionSignature),
    // Built in functions
    BuiltIn(BuiltInSignature),
    // Mutable vector
    MutableVector(HeapRef<Vec<SteelVal>>),
    // This should delegate to the underlying iterator - can allow for faster raw iteration if possible
    // Should allow for polling just a raw "next" on underlying elements
    BoxedIterator(Gc<RefCell<OpaqueIterator>>),
    // Contains a syntax object.
    SyntaxObject(Gc<Syntax>),
    // Mutable storage, with Gc backing
    // Boxed(HeapRef),
    Boxed(Gc<RefCell<SteelVal>>),
    // Holds a SteelVal on the heap.
    HeapAllocated(HeapRef<SteelVal>),
    // TODO: This itself, needs to be boxed unfortunately.
    Reference(Rc<OpaqueReference<'static>>),
    // Like IntV but supports larger values.
    BigNum(Gc<BigInt>),
    // Like Rational but supports larger numerators and denominators.
    BigRational(Gc<BigRational>),
    // A complex number.
    Complex(Gc<SteelComplex>),
    // Byte vectors
    ByteVector(SteelByteVector),
}

#[derive(Clone, PartialEq, Eq)]
pub struct SteelByteVector {
    // TODO: Consider using Box<[u8]>
    pub(crate) vec: Gc<RefCell<Vec<u8>>>,
}

impl SteelByteVector {
    pub fn new(vec: Vec<u8>) -> Self {
        Self {
            vec: Gc::new(RefCell::new(vec)),
        }
    }
}

/// Contains a complex number.
///
/// TODO: Optimize the contents of complex value. Holding `SteelVal` makes it easier to use existing
/// operations but a more specialized representation may be faster.
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct SteelComplex {
    /// The real part of the complex number.
    pub re: SteelVal,
    /// The imaginary part of the complex number.
    pub im: SteelVal,
}

impl SteelComplex {
    pub fn new(real: SteelVal, imaginary: SteelVal) -> SteelComplex {
        SteelComplex {
            re: real,
            im: imaginary,
        }
    }

    /// Returns `true` if the imaginary part is negative.
    fn imaginary_is_negative(&self) -> bool {
        match &self.im {
            NumV(x) => x.is_negative(),
            IntV(x) => x.is_negative(),
            Rational(x) => x.is_negative(),
            BigNum(x) => x.is_negative(),
            SteelVal::BigRational(x) => x.is_negative(),
            _ => unreachable!(),
        }
    }
}

impl IntoSteelVal for SteelComplex {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(match self.im {
            NumV(n) if n.is_zero() => self.re,
            IntV(0) => self.re,
            _ => SteelVal::Complex(Gc::new(self)),
        })
    }
}

impl fmt::Display for SteelComplex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.imaginary_is_negative() {
            write!(f, "{re}{im}i", re = self.re, im = self.im)
        } else {
            write!(f, "{re}+{im}i", re = self.re, im = self.im)
        }
    }
}

impl SteelVal {
    pub fn new_dyn_writer_port(port: impl Write + Send + Sync + 'static) -> SteelVal {
        SteelVal::PortV(SteelPort {
            port: Rc::new(RefCell::new(SteelPortRepr::DynWriter(Arc::new(
                Mutex::new(port),
            )))),
        })
    }

    pub fn anonymous_boxed_function(
        function: std::sync::Arc<
            dyn Fn(&[SteelVal]) -> crate::rvals::Result<SteelVal> + Send + Sync + 'static,
        >,
    ) -> SteelVal {
        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction {
            function,
            name: None,
            arity: None,
        }))
    }

    pub fn as_box(&self) -> Option<HeapRef<SteelVal>> {
        if let SteelVal::HeapAllocated(heap_ref) = self {
            Some(heap_ref.clone())
        } else {
            None
        }
    }

    pub fn as_box_to_inner(&self) -> Option<SteelVal> {
        self.as_box().map(|x| x.get())
    }

    pub fn as_ptr_usize(&self) -> Option<usize> {
        match self {
            // Closure(_) => todo!(),
            // BoolV(_) => todo!(),
            // NumV(_) => todo!(),
            // IntV(_) => todo!(),
            // CharV(_) => todo!(),
            // VectorV(_) => todo!(),
            // Void => todo!(),
            // StringV(_) => todo!(),
            // FuncV(_) => todo!(),
            // SymbolV(_) => todo!(),
            // SteelVal::Custom(_) => todo!(),
            // HashMapV(_) => todo!(),
            // HashSetV(_) => todo!(),
            CustomStruct(c) => Some(c.as_ptr() as usize),
            // PortV(_) => todo!(),
            // IterV(_) => todo!(),
            // ReducerV(_) => todo!(),
            // FutureFunc(_) => todo!(),
            // FutureV(_) => todo!(),
            // StreamV(_) => todo!(),
            // BoxedFunction(_) => todo!(),
            // ContinuationFunction(_) => todo!(),
            ListV(l) => Some(l.as_ptr_usize()),
            // MutFunc(_) => todo!(),
            // BuiltIn(_) => todo!(),
            // MutableVector(_) => todo!(),
            // BoxedIterator(_) => todo!(),
            // SteelVal::SyntaxObject(_) => todo!(),
            // Boxed(_) => todo!(),
            HeapAllocated(h) => Some(h.as_ptr_usize()),
            // Reference(_) => todo!(),
            // BigNum(_) => todo!(),
            _ => None,
        }
    }

    // pub(crate) fn children_mut<'a>(&'a mut self) -> impl IntoIterator<Item = SteelVal> {
    //     match self {
    //         Self::CustomStruct(inner) => {
    //             if let Some(inner) = inner.get_mut() {
    //                 std::mem::take(&mut inner.borrow_mut().fields)
    //             } else {
    //                 std::iter::empty()
    //             }
    //         }
    //         _ => todo!(),
    //     }
    // }
}

// TODO: Consider unboxed value types, for optimized usages when compiling segments of code.
// If we can infer the types from the concrete functions used, we don't need to have unboxed values -> We also
// can use concrete forms of the underlying functions as well.
// #[derive(Clone)]
// pub enum UnboxedSteelVal {
//     /// Represents a boolean value
//     BoolV(bool),
//     /// Represents a number, currently only f64 numbers are supported
//     NumV(f64),
//     /// Represents an integer
//     IntV(isize),
//     /// Represents a character type
//     CharV(char),
//     /// Vectors are represented as `im_rc::Vector`'s, which are immutable
//     /// data structures
//     VectorV(Vector<SteelVal>),
//     /// Void return value
//     Void,
//     /// Represents strings
//     StringV(SteelString),
//     /// Represents built in rust functions
//     FuncV(FunctionSignature),
//     /// Represents a symbol, internally represented as `String`s
//     SymbolV(SteelString),
//     /// Container for a type that implements the `Custom Type` trait. (trait object)
//     Custom(Gc<RefCell<Box<dyn CustomType>>>),
//     // Embedded HashMap
//     HashMapV(HashMap<SteelVal, SteelVal>),
//     // Embedded HashSet
//     HashSetV(HashSet<SteelVal>),
//     /// Represents a scheme-only struct
//     // StructV(Gc<SteelStruct>),
//     /// Alternative implementation of a scheme-only struct
//     CustomStruct(Gc<RefCell<UserDefinedStruct>>),
//     // Represents a special rust closure
//     // StructClosureV(Box<SteelStruct>, StructClosureSignature),
//     // StructClosureV(Box<StructClosure>),
//     /// Represents a port object
//     PortV(SteelPort),
//     /// Represents a bytecode closure
//     Closure(Gc<ByteCodeLambda>),
//     /// Generic iterator wrapper
//     IterV(Gc<Transducer>),
//     /// Reducers
//     ReducerV(Gc<Reducer>),
//     // Reducer(Reducer)
//     // Generic IntoIter wrapper
//     // Promise(Gc<SteelVal>),
//     /// Async Function wrapper
//     FutureFunc(BoxedAsyncFunctionSignature),
//     // Boxed Future Result
//     FutureV(Gc<FutureResult>),

//     StreamV(Gc<LazyStream>),
//     // Break the cycle somehow
//     // EvaluationEnv(Weak<RefCell<Env>>),
//     /// Contract
//     Contract(Gc<ContractType>),
//     /// Contracted Function
//     ContractedFunction(Gc<ContractedFunction>),
//     /// Custom closure
//     BoxedFunction(BoxedFunctionSignature),
//     // Continuation
//     ContinuationFunction(Gc<Continuation>),
//     // List
//     ListV(List<SteelVal>),
//     // Mutable functions
//     MutFunc(MutFunctionSignature),
//     // Built in functions
//     BuiltIn(BuiltInSignature),
//     // Mutable vector
//     MutableVector(Gc<RefCell<Vec<SteelVal>>>),
//     // This should delegate to the underlying iterator - can allow for faster raw iteration if possible
//     // Should allow for polling just a raw "next" on underlying elements
//     BoxedIterator(Gc<RefCell<BuiltInDataStructureIterator>>),

//     SyntaxObject(Gc<Syntax>),

//     // Mutable storage, with Gc backing
//     Boxed(HeapRef),
// }

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct SteelString(Rc<String>);

impl Deref for SteelString {
    type Target = Rc<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<&str> for SteelString {
    fn from(val: &str) -> Self {
        SteelString(Rc::new(val.to_string()))
    }
}

impl From<&String> for SteelString {
    fn from(val: &String) -> Self {
        SteelString(Rc::new(val.to_owned()))
    }
}

impl From<String> for SteelString {
    fn from(val: String) -> Self {
        SteelString(Rc::new(val))
    }
}

impl From<Rc<String>> for SteelString {
    fn from(val: Rc<String>) -> Self {
        SteelString(val)
    }
}

impl From<SteelString> for Rc<String> {
    fn from(value: SteelString) -> Self {
        value.0
    }
}

impl std::fmt::Display for SteelString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for SteelString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

// Check that steel values aren't growing without us knowing
const _ASSERT_SMALL: () = assert!(std::mem::size_of::<SteelVal>() <= 16);

#[test]
fn check_size_of_steelval() {
    assert_eq!(std::mem::size_of::<SteelVal>(), 16);
}

pub struct Chunks {
    remaining: IntoIter<char>,
}

impl Chunks {
    fn new(s: SteelString) -> Self {
        Chunks {
            remaining: s.chars().collect::<Vec<_>>().into_iter(),
        }
    }
}

pub struct OpaqueIterator {
    pub(crate) root: SteelVal,
    iterator: BuiltInDataStructureIterator,
}

impl Custom for OpaqueIterator {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<iterator>")))
    }
}

// TODO: Convert this to just a generic custom type. This does not have to be
// a special enum variant.
pub enum BuiltInDataStructureIterator {
    List(crate::values::lists::ConsumingIterator<SteelVal>),
    Vector(im_rc::vector::ConsumingIter<SteelVal>),
    Set(im_rc::hashset::ConsumingIter<SteelVal>),
    Map(im_rc::hashmap::ConsumingIter<(SteelVal, SteelVal)>),
    String(Chunks),
    Opaque(Box<dyn Iterator<Item = SteelVal>>),
}

impl BuiltInDataStructureIterator {
    pub fn into_boxed_iterator(self, value: SteelVal) -> SteelVal {
        SteelVal::BoxedIterator(Gc::new(RefCell::new(OpaqueIterator {
            root: value,
            iterator: self,
        })))
    }
}

impl BuiltInDataStructureIterator {
    pub fn from_iterator<T: IntoSteelVal, S: IntoIterator<Item = T> + 'static>(value: S) -> Self {
        Self::Opaque(Box::new(
            value
                .into_iter()
                .map(|x| x.into_steelval().expect("This shouldn't fail!")),
        ))
    }
}

impl Iterator for BuiltInDataStructureIterator {
    type Item = SteelVal;

    fn next(&mut self) -> Option<SteelVal> {
        match self {
            Self::List(l) => l.next(),
            Self::Vector(v) => v.next(),
            Self::String(s) => s.remaining.next().map(SteelVal::CharV),
            Self::Set(s) => s.next(),
            Self::Map(s) => s.next().map(|x| SteelVal::ListV(vec![x.0, x.1].into())),
            Self::Opaque(s) => s.next(),
        }
    }
}

pub fn value_into_iterator(val: SteelVal) -> Option<SteelVal> {
    let root = val.clone();
    match val {
        SteelVal::ListV(l) => Some(BuiltInDataStructureIterator::List(l.into_iter())),
        SteelVal::VectorV(v) => Some(BuiltInDataStructureIterator::Vector(
            (*v).clone().into_iter(),
        )),
        SteelVal::StringV(s) => Some(BuiltInDataStructureIterator::String(Chunks::new(s))),
        SteelVal::HashSetV(s) => Some(BuiltInDataStructureIterator::Set((*s).clone().into_iter())),
        SteelVal::HashMapV(m) => Some(BuiltInDataStructureIterator::Map((*m).clone().into_iter())),
        _ => None,
    }
    .map(|iterator| BuiltInDataStructureIterator::into_boxed_iterator(iterator, root))
}

thread_local! {
    pub static ITERATOR_FINISHED: SteelVal = SteelVal::SymbolV("done".into());
}

pub fn iterator_next(args: &[SteelVal]) -> Result<SteelVal> {
    match &args[0] {
        SteelVal::BoxedIterator(b) => match b.borrow_mut().iterator.next() {
            Some(v) => Ok(v),
            None => Ok(ITERATOR_FINISHED.with(|x| x.clone())),
        },
        _ => stop!(TypeMismatch => "Unexpected argument"),
    }
}

impl SteelVal {
    pub fn boxed(value: SteelVal) -> SteelVal {
        SteelVal::Boxed(Gc::new(RefCell::new(value)))
    }

    pub(crate) fn ptr_eq(&self, other: &SteelVal) -> bool {
        match (self, other) {
            // Integers are a special case of ptr eq -> if integers are equal? they are also eq?
            (IntV(l), IntV(r)) => l == r,
            (NumV(l), NumV(r)) => l == r,
            (BoolV(l), BoolV(r)) => l == r,
            (VectorV(l), VectorV(r)) => Gc::ptr_eq(&l.0, &r.0),
            (Void, Void) => true,
            (StringV(l), StringV(r)) => Rc::ptr_eq(l, r),
            (FuncV(l), FuncV(r)) => *l as usize == *r as usize,
            (SymbolV(l), SymbolV(r)) => Rc::ptr_eq(l, r),
            (SteelVal::Custom(l), SteelVal::Custom(r)) => Gc::ptr_eq(l, r),
            (HashMapV(l), HashMapV(r)) => Gc::ptr_eq(&l.0, &r.0),
            (HashSetV(l), HashSetV(r)) => Gc::ptr_eq(&l.0, &r.0),
            (PortV(l), PortV(r)) => Rc::ptr_eq(&l.port, &r.port),
            (Closure(l), Closure(r)) => Gc::ptr_eq(l, r),
            (IterV(l), IterV(r)) => Gc::ptr_eq(l, r),
            (ReducerV(l), ReducerV(r)) => Gc::ptr_eq(l, r),
            #[allow(clippy::vtable_address_comparisons)]
            (FutureFunc(l), FutureFunc(r)) => Rc::ptr_eq(l, r),
            (FutureV(l), FutureV(r)) => Gc::ptr_eq(l, r),
            (StreamV(l), StreamV(r)) => Gc::ptr_eq(l, r),
            (BoxedFunction(l), BoxedFunction(r)) => Rc::ptr_eq(l, r),
            (ContinuationFunction(l), ContinuationFunction(r)) => Continuation::ptr_eq(l, r),
            (ListV(l), ListV(r)) => {
                l.ptr_eq(r) || l.storage_ptr_eq(r) || l.is_empty() && r.is_empty()
            }
            (MutFunc(l), MutFunc(r)) => *l as usize == *r as usize,
            (BuiltIn(l), BuiltIn(r)) => *l as usize == *r as usize,
            (MutableVector(l), MutableVector(r)) => HeapRef::ptr_eq(l, r),
            (BigNum(l), BigNum(r)) => Gc::ptr_eq(l, r),
            (ByteVector(l), ByteVector(r)) => Gc::ptr_eq(&l.vec, &r.vec),
            (_, _) => false,
        }
    }
}

impl Hash for SteelVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BoolV(b) => b.hash(state),
            NumV(n) => n.to_string().hash(state),
            IntV(i) => i.hash(state),
            Rational(f) => f.hash(state),
            BigNum(n) => n.hash(state),
            BigRational(f) => f.hash(state),
            Complex(x) => x.hash(state),
            CharV(c) => c.hash(state),
            ListV(l) => l.hash(state),
            CustomStruct(s) => s.hash(state),
            // Pair(cell) => {
            //     cell.hash(state);
            // }
            VectorV(v) => v.hash(state),
            v @ Void => v.hash(state),
            StringV(s) => s.hash(state),
            FuncV(s) => (*s as *const FunctionSignature).hash(state),
            // LambdaV(_) => unimplemented!(),
            // MacroV(_) => unimplemented!(),
            SymbolV(sym) => {
                "symbol".hash(state);
                sym.hash(state);
                // format!("symbol: {}")
            }
            Custom(_) => unimplemented!(),
            // StructClosureV(_) => unimplemented!(),
            PortV(_) => unimplemented!(),
            Closure(b) => b.hash(state),
            HashMapV(hm) => hm.hash(state),
            IterV(s) => s.hash(state),
            HashSetV(hs) => hs.hash(state),
            SyntaxObject(s) => s.raw.hash(state),
            Pair(p) => (&**p).hash(state),
            _ => {
                unimplemented!("Attempted to has unsupported value: {self:?}")
            }
        }
    }
}

impl SteelVal {
    #[inline(always)]
    pub fn is_truthy(&self) -> bool {
        match &self {
            SteelVal::BoolV(false) => false,
            _ => true,
        }
    }

    #[inline(always)]
    pub fn is_future(&self) -> bool {
        matches!(self, SteelVal::FutureV(_))
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            BoolV(_)
                | IntV(_)
                | CharV(_)
                // | Pair(_)
                | VectorV(_)
                | StringV(_)
                | SymbolV(_)
                | HashMapV(_)
                | Closure(_)
                | ListV(_)
                | FuncV(_)
                | CustomStruct(_)
        )
    }

    pub fn is_function(&self) -> bool {
        matches!(
            self,
            BoxedFunction(_)
                | Closure(_)
                | FuncV(_)
                // | ContractedFunction(_)
                | BuiltIn(_)
                | MutFunc(_)
        )
    }

    // pub fn is_contract(&self) -> bool {
    //     matches!(self, Contract(_))
    // }

    pub fn empty_hashmap() -> SteelVal {
        SteelVal::HashMapV(Gc::new(HashMap::new()).into())
    }
}

impl SteelVal {
    // pub fn res_iterator

    pub fn list_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&List<SteelVal>, E> {
        match self {
            Self::ListV(v) => Ok(v),
            _ => Err(err()),
        }
    }

    pub fn list(&self) -> Option<&List<SteelVal>> {
        match self {
            Self::ListV(l) => Some(l),
            _ => None,
        }
    }

    pub fn pair(&self) -> Option<&Gc<crate::values::lists::Pair>> {
        match self {
            Self::Pair(p) => Some(p),
            _ => None,
        }
    }

    pub fn bool_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<bool, E> {
        match self {
            Self::BoolV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    pub fn int_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<isize, E> {
        match self {
            Self::IntV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    pub fn num_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<f64, E> {
        match self {
            Self::NumV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    pub fn char_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<char, E> {
        match self {
            Self::CharV(v) => Ok(*v),
            _ => Err(err()),
        }
    }

    /// Vector does copy on the value to return
    pub fn vector_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<Vector<SteelVal>, E> {
        match self {
            Self::VectorV(v) => Ok(v.0.unwrap()),
            _ => Err(err()),
        }
    }

    pub fn void_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<(), E> {
        match self {
            Self::Void => Ok(()),
            _ => Err(err()),
        }
    }

    pub fn string_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&str, E> {
        match self {
            Self::StringV(v) => Ok(v),
            _ => Err(err()),
        }
    }

    pub fn func_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&FunctionSignature, E> {
        match self {
            Self::FuncV(v) => Ok(v),
            _ => Err(err()),
        }
    }

    pub fn boxed_func_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&BoxedDynFunction, E> {
        match self {
            Self::BoxedFunction(v) => Ok(v),
            _ => Err(err()),
        }
    }

    // pub fn contract_or_else<E, F: FnOnce() -> E>(
    //     &self,
    //     err: F,
    // ) -> std::result::Result<Gc<ContractType>, E> {
    //     match self {
    //         Self::Contract(c) => Ok(c.clone()),
    //         _ => Err(err()),
    //     }
    // }

    pub fn closure_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<Gc<ByteCodeLambda>, E> {
        match self {
            Self::Closure(c) => Ok(c.clone()),
            _ => Err(err()),
        }
    }

    pub fn symbol_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&str, E> {
        match self {
            Self::SymbolV(v) => Ok(v),
            _ => Err(err()),
        }
    }

    pub fn clone_symbol_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<String, E> {
        match self {
            Self::SymbolV(v) => Ok(v.to_string()),
            _ => Err(err()),
        }
    }

    pub fn as_isize(&self) -> Option<isize> {
        match self {
            Self::IntV(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        self.as_isize()
            .and_then(|x| if x >= 0 { Some(x as usize) } else { None })
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::BoolV(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_future(&self) -> Option<Shared<BoxedFutureResult>> {
        match self {
            Self::FutureV(v) => Some(v.clone().unwrap().into_shared()),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&SteelString> {
        match self {
            Self::StringV(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<&SteelString> {
        match self {
            Self::SymbolV(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_syntax_object(&self) -> Option<&Syntax> {
        match self {
            Self::SyntaxObject(s) => Some(s),
            _ => None,
        }
    }

    // pub fn custom_or_else<E, F: FnOnce() -> E>(
    //     &self,
    //     err: F,
    // ) -> std::result::Result<&Box<dyn CustomType>, E> {
    //     match self {
    //         Self::Custom(v) => Ok(&v),
    //         _ => Err(err()),
    //     }
    // }

    // pub fn struct_or_else<E, F: FnOnce() -> E>(
    //     &self,
    //     err: F,
    // ) -> std::result::Result<&SteelStruct, E> {
    //     match self {
    //         Self::StructV(v) => Ok(v),
    //         _ => Err(err()),
    //     }
    // }

    pub fn closure_arity(&self) -> Option<usize> {
        if let SteelVal::Closure(c) = self {
            Some(c.arity())
        } else {
            None
        }
    }
}

impl SteelVal {
    pub const INT_ZERO: SteelVal = SteelVal::IntV(0);
    pub const INT_ONE: SteelVal = SteelVal::IntV(1);
    pub const INT_TWO: SteelVal = SteelVal::IntV(2);
}

impl Eq for SteelVal {}

fn integer_float_equality(int: isize, float: f64) -> bool {
    let converted = float as isize;

    if float == converted as f64 {
        int == converted
    } else {
        false
    }
}

fn bignum_float_equality(bigint: &Gc<num::BigInt>, float: f64) -> bool {
    if float.fract() == 0.0 {
        if let Some(promoted) = bigint.to_f64() {
            promoted == float
        } else {
            false
        }
    } else {
        false
    }
}

#[steel_derive::function(name = "=", constant = true)]
pub fn number_equality(left: &SteelVal, right: &SteelVal) -> Result<SteelVal> {
    let result = match (left, right) {
        (IntV(l), IntV(r)) => l == r,
        (NumV(l), NumV(r)) => l == r,
        (IntV(l), NumV(r)) | (NumV(r), IntV(l)) => integer_float_equality(*l, *r),
        (Rational(l), Rational(r)) => l == r,
        (Rational(l), NumV(r)) | (NumV(r), Rational(l)) => l.to_f64().unwrap() == *r,
        (BigNum(l), BigNum(r)) => l == r,
        (BigNum(l), NumV(r)) | (NumV(r), BigNum(l)) => bignum_float_equality(l, *r),
        (BigRational(l), BigRational(r)) => l == r,
        (BigRational(l), NumV(r)) | (NumV(r), BigRational(l)) => l.to_f64().unwrap() == *r,
        // The below should be impossible as integers/bignums freely convert into each
        // other. Similar for int/bignum/rational/bigrational.
        (Rational(_), IntV(_))
        | (IntV(_), Rational(_))
        | (Rational(_), BigNum(_))
        | (BigNum(_), Rational(_))
        | (Rational(_), BigRational(_))
        | (BigRational(_), Rational(_)) => false,
        (BigRational(_), IntV(_))
        | (IntV(_), BigRational(_))
        | (BigRational(_), BigNum(_))
        | (BigNum(_), BigRational(_)) => false,
        (IntV(_), BigNum(_)) | (BigNum(_), IntV(_)) => false,
        (Complex(x), Complex(y)) => {
            number_equality(&x.re, &y.re)? == BoolV(true)
                && number_equality(&x.im, &y.re)? == BoolV(true)
        }
        (Complex(_), _) | (_, Complex(_)) => false,
        _ => stop!(TypeMismatch => "= expects two numbers, found: {:?} and {:?}", left, right),
    };
    Ok(BoolV(result))
}

fn partial_cmp_f64(l: &impl ToPrimitive, r: &impl ToPrimitive) -> Option<Ordering> {
    l.to_f64()?.partial_cmp(&r.to_f64()?)
}

// TODO add tests
impl PartialOrd for SteelVal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // TODO: Attempt to avoid converting to f64 for cases below as it may lead to precision loss
        // at tiny and large values.
        match (self, other) {
            (IntV(l), IntV(r)) => l.partial_cmp(r),
            (IntV(l), NumV(r)) => partial_cmp_f64(l, r),
            (IntV(l), Rational(r)) => partial_cmp_f64(l, r),
            (IntV(l), BigRational(r)) => partial_cmp_f64(l, r.as_ref()),
            (IntV(l), BigNum(r)) => BigInt::from(*l).partial_cmp(r),
            (NumV(l), IntV(r)) => partial_cmp_f64(l, r),
            (NumV(l), NumV(r)) => l.partial_cmp(r),
            (NumV(l), Rational(r)) => partial_cmp_f64(l, r),
            (NumV(l), BigRational(r)) => partial_cmp_f64(l, r.as_ref()),
            (NumV(l), BigNum(r)) => partial_cmp_f64(l, r.as_ref()),
            (Rational(l), Rational(r)) => l.partial_cmp(&r),
            (Rational(l), IntV(r)) => partial_cmp_f64(l, r),
            (Rational(l), NumV(r)) => l.to_f64()?.partial_cmp(&r),
            (Rational(l), BigRational(r)) => partial_cmp_f64(l, r.as_ref()),
            (Rational(l), BigNum(r)) => l.to_f64()?.partial_cmp(&r.to_f64()?),
            (BigNum(l), IntV(r)) => l.as_ref().partial_cmp(&BigInt::from(*r)),
            (BigNum(l), NumV(r)) => l.to_f64()?.partial_cmp(r),
            (BigNum(l), BigNum(r)) => l.as_ref().partial_cmp(r.as_ref()),
            (BigNum(l), Rational(r)) => partial_cmp_f64(l.as_ref(), r),
            (BigNum(l), BigRational(r)) => partial_cmp_f64(l.as_ref(), r.as_ref()),
            (BigRational(l), BigRational(r)) => l.as_ref().partial_cmp(r.as_ref()),
            (BigRational(l), IntV(r)) => partial_cmp_f64(l.as_ref(), r),
            (BigRational(l), NumV(r)) => partial_cmp_f64(l.as_ref(), r),
            (BigRational(l), Rational(r)) => partial_cmp_f64(l.as_ref(), r),
            (BigRational(l), BigNum(r)) => partial_cmp_f64(l.as_ref(), r.as_ref()),
            (StringV(s), StringV(o)) => s.partial_cmp(o),
            (CharV(l), CharV(r)) => l.partial_cmp(r),
            (l, r) => {
                // All real numbers (not complex) should have order defined.
                debug_assert!(
                    !(realp(l) && realp(r)),
                    "Numbers {l:?} and {r:?} should implement partial_cmp"
                );
                // Unimplemented for other types
                None
            }
        }
    }
}

impl fmt::Display for SteelVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // at the top level, print a ' if we are
        // trying to print a symbol or list
        match self {
            SymbolV(_) | ListV(_) => write!(f, "'")?,
            VectorV(_) => write!(f, "'#")?,
            _ => (),
        };

        CycleDetector::detect_and_display_cycles(self, f)
        // display_helper(self, f)
    }
}

impl fmt::Debug for SteelVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // at the top level, print a ' if we are
        // trying to print a symbol or list
        match self {
            SymbolV(_) | ListV(_) => write!(f, "'")?,
            VectorV(_) => write!(f, "'#")?,
            _ => (),
        };
        // display_helper(self, f)

        CycleDetector::detect_and_display_cycles(self, f)
    }
}

#[cfg(test)]
mod or_else_tests {

    use super::*;
    use im_rc::vector;

    #[test]
    fn bool_or_else_test_good() {
        let input = SteelVal::BoolV(true);
        assert_eq!(input.bool_or_else(throw!(Generic => "test")).unwrap(), true);
    }

    #[test]
    fn bool_or_else_test_bad() {
        let input = SteelVal::CharV('f');
        assert!(input.bool_or_else(throw!(Generic => "test")).is_err());
    }

    #[test]
    fn num_or_else_test_good() {
        let input = SteelVal::NumV(10.0);
        assert_eq!(input.num_or_else(throw!(Generic => "test")).unwrap(), 10.0);
    }

    #[test]
    fn num_or_else_test_bad() {
        let input = SteelVal::CharV('f');
        assert!(input.num_or_else(throw!(Generic => "test")).is_err());
    }

    #[test]
    fn char_or_else_test_good() {
        let input = SteelVal::CharV('f');
        assert_eq!(input.char_or_else(throw!(Generic => "test")).unwrap(), 'f');
    }

    #[test]
    fn char_or_else_test_bad() {
        let input = SteelVal::NumV(10.0);
        assert!(input.char_or_else(throw!(Generic => "test")).is_err());
    }

    #[test]
    fn vector_or_else_test_good() {
        let input: SteelVal = vector![SteelVal::IntV(1)].into();
        assert_eq!(
            input.vector_or_else(throw!(Generic => "test")).unwrap(),
            vector![SteelVal::IntV(1)]
        );
    }

    #[test]
    fn vector_or_else_bad() {
        let input = SteelVal::CharV('f');
        assert!(input.vector_or_else(throw!(Generic => "test")).is_err());
    }

    #[test]
    fn void_or_else_test_good() {
        let input = SteelVal::Void;
        assert_eq!(input.void_or_else(throw!(Generic => "test")).unwrap(), ())
    }

    #[test]
    fn void_or_else_test_bad() {
        let input = SteelVal::StringV("foo".into());
        assert!(input.void_or_else(throw!(Generic => "test")).is_err());
    }

    #[test]
    fn string_or_else_test_good() {
        let input = SteelVal::StringV("foo".into());
        assert_eq!(
            input.string_or_else(throw!(Generic => "test")).unwrap(),
            "foo".to_string()
        );
    }

    #[test]
    fn string_or_else_test_bad() {
        let input = SteelVal::Void;
        assert!(input.string_or_else(throw!(Generic => "test")).is_err())
    }

    #[test]
    fn symbol_or_else_test_good() {
        let input = SteelVal::SymbolV("foo".into());
        assert_eq!(
            input.symbol_or_else(throw!(Generic => "test")).unwrap(),
            "foo".to_string()
        );
    }

    #[test]
    fn symbol_or_else_test_bad() {
        let input = SteelVal::Void;
        assert!(input.symbol_or_else(throw!(Generic => "test")).is_err())
    }

    #[test]
    fn num_and_char_are_not_ordered() {
        assert_eq!(SteelVal::IntV(0).partial_cmp(&SteelVal::CharV('0')), None);
        assert_eq!(SteelVal::NumV(0.0).partial_cmp(&SteelVal::CharV('0')), None);
        assert_eq!(
            SteelVal::BigNum(Gc::new(BigInt::default())).partial_cmp(&SteelVal::CharV('0')),
            None
        );
    }

    #[test]
    fn number_cmp() {
        let less_cases = [
            (SteelVal::IntV(-10), SteelVal::IntV(1)),
            (
                SteelVal::IntV(-10),
                SteelVal::BigNum(Gc::new(BigInt::from(1))),
            ),
            (SteelVal::NumV(-10.0), SteelVal::IntV(1)),
            (SteelVal::IntV(-10), SteelVal::NumV(1.0)),
            (
                SteelVal::BigNum(Gc::new(BigInt::from(-10))),
                SteelVal::BigNum(Gc::new(BigInt::from(1))),
            ),
            (
                SteelVal::NumV(-10.0),
                SteelVal::BigNum(Gc::new(BigInt::from(1))),
            ),
        ];
        for (l, r) in less_cases {
            assert_eq!(l.partial_cmp(&r), Some(Ordering::Less));
            assert_eq!(r.partial_cmp(&l), Some(Ordering::Greater));
        }
        let equal_cases = [
            SteelVal::IntV(-10),
            SteelVal::NumV(-10.0),
            SteelVal::BigNum(Gc::new(BigInt::from(-10))),
            // Added to test that the number is equal even if it points to a different object.
            SteelVal::BigNum(Gc::new(BigInt::from(-10))),
        ]
        .into_iter();
        for (l, r) in equal_cases.clone().zip(equal_cases.clone()) {
            assert_eq!(l.partial_cmp(&r), Some(Ordering::Equal));
        }
    }
}
