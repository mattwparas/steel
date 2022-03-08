use crate::{
    core::instructions::DenseInstruction,
    gc::Gc,
    rerrs::{ErrorKind, SteelErr},
    steel_vm::vm::{BuiltInSignature, Continuation},
    values::port::SteelPort,
    values::{
        contracts::{ContractType, ContractedFunction},
        functions::ByteCodeLambda,
        lazy_stream::LazyStream,
        transducers::{Reducer, Transducer},
    },
    values::{structs::SteelStruct, upvalue::UpValue},
};

#[cfg(feature = "jit")]
use crate::jit::sig::JitFunctionPointer;

use std::{
    any::Any,
    cell::RefCell,
    cmp::Ordering,
    fmt,
    fmt::Write,
    future::Future,
    hash::{Hash, Hasher},
    pin::Pin,
    rc::Rc,
    result,
    task::Context,
};

// TODO
#[macro_export]
macro_rules! list {
    () => { $crate::rvals::SteelVal::ListV(
        im_lists::list![]
    ) };

    ( $($x:expr),* ) => {{
        $crate::rvals::SteelVal::ListV(im_lists::list![$(
            $crate::rvals::IntoSteelVal::into_steelval($x).unwrap()
        ), *])
    }};

    ( $($x:expr ,)* ) => {{
        $crate::rvals::SteeVal::ListV(im_lists::list![$(
            $crate::rvals::IntoSteelVal::into_steelval($x).unwrap()
        )*])
    }};
}

use SteelVal::*;

use im_rc::{HashMap, HashSet, Vector};

use futures::FutureExt;
use futures::{future::Shared, task::noop_waker_ref};

use im_lists::list::List;

pub type RcRefSteelVal = Rc<RefCell<SteelVal>>;
pub fn new_rc_ref_cell(x: SteelVal) -> RcRefSteelVal {
    Rc::new(RefCell::new(x))
}

pub type Result<T> = result::Result<T, SteelErr>;
pub type FunctionSignature = fn(&[SteelVal]) -> Result<SteelVal>;
pub type MutFunctionSignature = fn(&mut [SteelVal]) -> Result<SteelVal>;
// pub type FunctionSignature = fn(&[SteelVal]) -> Result<SteelVal>;
pub type StructClosureSignature = fn(&[SteelVal], &SteelStruct) -> Result<SteelVal>;
pub type BoxedFunctionSignature = Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>;

pub type BoxedAsyncFunctionSignature = Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>;

// Do something like this:
// vector of async functions
// then for a wait group, make a closure that looks something like this:
// async move vec<functioncalls> |_| {
//    let values = Vec::new();
//    for func in vec {
//         values.push(func(args).await)
//    }
//    values
// }

// pub type BoxedFutureResult = Shared<Output = Result<Gc<SteelVal>>>;
pub type AsyncSignature = fn(&[SteelVal]) -> FutureResult;

pub type BoxedFutureResult = Pin<Box<dyn Future<Output = Result<SteelVal>>>>;

// Pin<Box<dyn Future<Output = T> + 'a + Send>>;

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
    let context = &mut Context::from_waker(&*waker);

    // Polling requires a pinned future - TODO make sure this is correct
    let mut_fut = Pin::new(&mut fut);

    match Future::poll(mut_fut, context) {
        std::task::Poll::Ready(r) => Some(r),
        std::task::Poll::Pending => None,
    }
}

pub trait Custom {}

pub trait CustomType {
    fn box_clone(&self) -> Box<dyn CustomType>;
    fn as_any(&self) -> Box<dyn Any>;
    fn name(&self) -> String {
        (std::any::type_name::<Self>()).to_string()
    }
    fn new_steel_val(&self) -> SteelVal;
    fn display(&self) -> std::result::Result<String, std::fmt::Error>;
}

impl Clone for Box<dyn CustomType> {
    fn clone(&self) -> Box<dyn CustomType> {
        self.box_clone()
    }
}

impl From<Box<dyn CustomType>> for SteelVal {
    fn from(val: Box<dyn CustomType>) -> SteelVal {
        val.new_steel_val()
    }
}

impl<T: Custom + Clone + 'static + std::fmt::Debug> CustomType for T {
    fn box_clone(&self) -> Box<dyn CustomType> {
        Box::new((*self).clone())
    }
    fn as_any(&self) -> Box<dyn Any> {
        Box::new((*self).clone())
    }
    fn new_steel_val(&self) -> SteelVal {
        SteelVal::Custom(Gc::new(Box::new(self.clone())))
    }
    fn display(&self) -> std::result::Result<String, std::fmt::Error> {
        let mut buf = String::new();
        write!(buf, "{:?}", &self)?;
        Ok(buf)
    }
}

impl<T: CustomType> IntoSteelVal for T {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(self.new_steel_val())
    }
}

impl<T: CustomType + Clone + 'static> FromSteelVal for T {
    fn from_steelval(val: SteelVal) -> Result<Self> {
        if let SteelVal::Custom(v) = val {
            let left_type = v.as_any();
            let left: Option<T> = left_type.downcast_ref::<T>().cloned();
            left.ok_or_else(|| {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal did not match the given type: {}",
                    std::any::type_name::<Self>()
                );
                SteelErr::new(ErrorKind::ConversionError, error_message)
            })
        } else {
            let error_message = format!(
                "Type Mismatch: Type of SteelVal did not match the given type: {}",
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
    fn from_steelval(val: SteelVal) -> Result<Self>;
}

#[derive(Clone)]
pub enum SteelVal {
    /// Represents a boolean value
    BoolV(bool),
    /// Represents a number, currently only f64 numbers are supported
    NumV(f64),
    /// Represents an integer
    IntV(isize),
    /// Represents a character type
    CharV(char),
    /// Represents a cons cell
    /// cons, cdr, optional parent pointer
    // Pair(Gc<SteelVal>, Option<Gc<SteelVal>>),
    // Pair(Gc<ConsCell>),
    /// Vectors are represented as `im_rc::Vector`'s, which are immutable
    /// data structures
    VectorV(Gc<Vector<SteelVal>>), // TODO wrap in GC
    /// Void return value
    Void,
    /// Represents strings
    StringV(Gc<String>),
    /// Represents built in rust functions
    FuncV(FunctionSignature),
    /// Represents Steel Lambda functions or closures defined inside the environment
    // LambdaV(SteelLambda),
    /// Represents built in macros,
    // MacroV(SteelMacro),
    /// Represents a symbol, internally represented as `String`s
    SymbolV(Gc<String>),
    /// Container for a type that implements the `Custom Type` trait. (trait object)
    Custom(Gc<Box<dyn CustomType>>),
    // Embedded HashMap
    HashMapV(Gc<HashMap<SteelVal, SteelVal>>), // TODO wrap in GC
    // Embedded HashSet
    HashSetV(Gc<HashSet<SteelVal>>), // TODO wrap in GC
    /// Represents a scheme-only struct
    StructV(Gc<SteelStruct>),
    // Represents a special rust closure
    // StructClosureV(Box<SteelStruct>, StructClosureSignature),
    // StructClosureV(Box<StructClosure>),
    /// Represents a port object
    PortV(Gc<SteelPort>),
    /// Represents a bytecode closure
    Closure(Gc<ByteCodeLambda>),
    /// Generic iterator wrapper
    IterV(Gc<Transducer>),
    /// Reducers
    ReducerV(Gc<Reducer>),
    // Reducer(Reducer)
    // Generic IntoIter wrapper
    // Promise(Gc<SteelVal>),
    /// Async Function wrapper
    FutureFunc(BoxedAsyncFunctionSignature),
    // Boxed Future Result
    FutureV(Gc<FutureResult>),
    // Mutable Box
    // Functions that want to operate by reference must move the value into a mutable box
    // This deep clones the value but then the value can be mutably snatched
    // MutableBox(Gc<RefCell<SteelVal>>),
    StreamV(Gc<LazyStream>),
    // Break the cycle somehow
    // EvaluationEnv(Weak<RefCell<Env>>),
    /// Mutable box - lets you put a value in there and change what it points to
    BoxV(Gc<RefCell<SteelVal>>),
    /// Contract
    Contract(Gc<ContractType>),
    /// Contracted Function
    ContractedFunction(Gc<ContractedFunction>),
    /// Custom closure
    BoxedFunction(BoxedFunctionSignature),
    // Continuation
    ContinuationFunction(Gc<Continuation>),
    // Function Pointer
    #[cfg(feature = "jit")]
    CompiledFunction(JitFunctionPointer),
    // List
    ListV(List<SteelVal>),
    // Mutable functions
    MutFunc(MutFunctionSignature),
    // Built in functions
    BuiltIn(BuiltInSignature),
    // Mutable vector
    MutableVector(Gc<RefCell<Vec<SteelVal>>>),
}

// TODO come back to this for the constant map

// impl Serialize for SteelVal {
//     fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         match self {
//             SteelVal::BoolV(b) => serializer.serialize_newtype_variant("SteelVal", 0, "BoolV", b),
//             SteelVal::NumV(n) => serializer.serialize_newtype_variant("SteelVal", 1, "NumV", n),
//             SteelVal::IntV(n) => serializer.serialize_newtype_variant("SteelVal", 2, "IntV", n),
//             SteelVal::CharV(c) => serializer.serialize_newtype_variant("SteelVal", 3, "CharV", c),
//             SteelVal::StringV(s) => {
//                 serializer.serialize_newtype_variant("SteelVal", 7, "StringV", s)
//             }
//             SteelVal::Pair(car, cdr) => {
//                 let mut state = serializer.serialize_tuple_variant("SteelVal", 4, "Pair", 2)?;
//                 state.serialize_field(car)?;
//                 state.serialize_field(cdr)?;
//                 state.end()
//             }
//             _ => panic!("Cannot serialize enum variant: {}", self),
//         }
//     }
// }

impl Hash for SteelVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BoolV(b) => b.hash(state),
            NumV(_) => {
                unimplemented!();
            }
            IntV(i) => i.hash(state),
            CharV(c) => c.hash(state),
            // Pair(cell) => {
            //     cell.hash(state);
            // }
            VectorV(v) => v.hash(state),
            Void => {
                unimplemented!();
            }
            StringV(s) => s.hash(state),
            FuncV(_) => unimplemented!(),
            // LambdaV(_) => unimplemented!(),
            // MacroV(_) => unimplemented!(),
            SymbolV(sym) => {
                "symbol".hash(state);
                sym.hash(state);
                // format!("symbol: {}")
            }
            Custom(_) => unimplemented!(),
            StructV(_) => unimplemented!(),
            // StructClosureV(_) => unimplemented!(),
            PortV(_) => unimplemented!(),
            Closure(b) => b.hash(state),
            HashMapV(hm) => hm.hash(state),
            IterV(_) => unimplemented!(),
            HashSetV(hs) => hs.hash(state),
            _ => unimplemented!(),
            // Promise(_) => unimplemented!(),
        }
    }
}

impl SteelVal {
    pub fn is_truthy(&self) -> bool {
        match &self {
            SteelVal::BoolV(false) => false,
            SteelVal::Void => false,
            SteelVal::VectorV(v) => !v.is_empty(),
            _ => true,
        }
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
        )
    }

    pub fn is_function(&self) -> bool {
        matches!(
            self,
            BoxedFunction(_) | Closure(_) | FuncV(_) | ContractedFunction(_)
        )
    }

    pub fn is_contract(&self) -> bool {
        matches!(self, Contract(_))
    }
}

impl SteelVal {
    // pub fn res_iterator

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
            Self::VectorV(v) => Ok(v.unwrap()),
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
            Self::StringV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn func_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&FunctionSignature, E> {
        match self {
            Self::FuncV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn boxed_func_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&BoxedFunctionSignature, E> {
        match self {
            Self::BoxedFunction(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn contract_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<Gc<ContractType>, E> {
        match self {
            Self::Contract(c) => Ok(c.clone()),
            _ => Err(err()),
        }
    }

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
            Self::SymbolV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn clone_symbol_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<String, E> {
        match self {
            Self::SymbolV(v) => Ok(v.unwrap()),
            _ => Err(err()),
        }
    }

    pub fn custom_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&Box<dyn CustomType>, E> {
        match self {
            Self::Custom(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn struct_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&SteelStruct, E> {
        match self {
            Self::StructV(v) => Ok(v),
            _ => Err(err()),
        }
    }

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

// TODO add tests
impl PartialEq for SteelVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Void, Void) => true,
            (BoolV(l), BoolV(r)) => l == r,
            // (NumV(l), NumV(r)) => l == r,
            (IntV(l), IntV(r)) => l == r,
            // (NumV(l), IntV(r)) => *l == *r as f64,
            // (IntV(l), NumV(r)) => *l as f64 == *r,
            (StringV(l), StringV(r)) => l == r,
            (VectorV(l), VectorV(r)) => l == r,
            (SymbolV(l), SymbolV(r)) => l == r,
            (CharV(l), CharV(r)) => l == r,
            // (Pair(_), Pair(_)) => collect_pair_into_vector(self) == collect_pair_into_vector(other),
            (HashSetV(l), HashSetV(r)) => l == r,
            (HashMapV(l), HashMapV(r)) => l == r,
            (StructV(l), StructV(r)) => l == r,
            (Closure(l), Closure(r)) => l == r,
            (ContractedFunction(l), ContractedFunction(r)) => l == r,
            (Contract(l), Contract(r)) => l == r,
            (IterV(l), IterV(r)) => l == r,
            (ListV(l), ListV(r)) => l == r,
            //TODO
            (_, _) => false, // (l, r) => {
                             //     let left = unwrap!(l, usize);
                             //     let right = unwrap!(r, usize);
                             //     match (left, right) {
                             //         (Ok(l), Ok(r)) => l == r,
                             //         (_, _) => false,
                             //     }
                             // }
        }
    }
}

// TODO add tests
impl PartialOrd for SteelVal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (NumV(n), NumV(o)) => n.partial_cmp(o),
            (StringV(s), StringV(o)) => s.partial_cmp(o),
            (CharV(l), CharV(r)) => l.partial_cmp(r),
            (IntV(l), IntV(r)) => l.partial_cmp(r),
            _ => None, // unimplemented for other types
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
        display_helper(self, f)
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
        display_helper(self, f)
    }
}

/// this function recursively prints lists without prepending the `'`
/// at the beginning
fn display_helper(val: &SteelVal, f: &mut fmt::Formatter) -> fmt::Result {
    match val {
        BoolV(b) => write!(f, "#{}", b),
        NumV(x) => write!(f, "{:?}", x),
        IntV(x) => write!(f, "{}", x),
        StringV(s) => write!(f, "\"{}\"", s),
        CharV(c) => write!(f, "#\\{}", c),
        FuncV(_) => write!(f, "#<function>"),
        // LambdaV(_) => write!(f, "#<lambda-function>"),
        // LambdaV(l) => write!(f, "#<{}>", l.pretty_print_closure()),
        // MacroV(_) => write!(f, "#<macro>"),
        Void => write!(f, "#<void>"),
        SymbolV(s) => write!(f, "{}", s),
        VectorV(lst) => {
            let mut iter = lst.iter();
            write!(f, "(")?;
            if let Some(last) = iter.next_back() {
                for item in iter {
                    display_helper(item, f)?;
                    write!(f, " ")?;
                }
                display_helper(last, f)?;
            }
            write!(f, ")")
        }
        Custom(x) => write!(f, "#<{}>", x.display()?),
        // Pair(_) => {
        //     let v = collect_pair_into_vector(val);
        //     display_helper(&v, f)
        // }
        StructV(s) => write!(f, "#<{}>", s.pretty_print()), // TODO
        // StructClosureV(_) => write!(f, "#<struct-constructor>"),
        PortV(_) => write!(f, "#<port>"),
        Closure(_) => write!(f, "#<bytecode-closure>"),
        HashMapV(hm) => write!(f, "#<hashmap {:#?}>", hm),
        IterV(_) => write!(f, "#<iterator>"),
        HashSetV(hs) => write!(f, "#<hashset {:?}>", hs),
        FutureFunc(_) => write!(f, "#<future-func>"),
        FutureV(_) => write!(f, "#<future>"),
        // Promise(_) => write!(f, "#<promise>"),
        StreamV(_) => write!(f, "#<stream>"),
        BoxV(b) => write!(f, "#<box {:?}>", b.borrow()),
        Contract(c) => write!(f, "{}", c.to_string()),
        ContractedFunction(_) => write!(f, "#<contracted-function>"),
        BoxedFunction(_) => write!(f, "#<function>"),
        ContinuationFunction(_) => write!(f, "#<continuation>"),
        #[cfg(feature = "jit")]
        CompiledFunction(_) => write!(f, "#<compiled-function>"),
        ListV(l) => {
            write!(f, "(")?;

            let mut iter = l.iter().peekable();

            while let Some(item) = iter.next() {
                display_helper(item, f)?;
                if iter.peek().is_some() {
                    write!(f, " ")?
                }
            }

            // for item in l.iter().pe

            // for item in l {
            //     display_helper(item, f)?;
            //     write!(f, " ")?;
            // }
            write!(f, ")")
        }
        // write!(f, "#<list {:?}>", l),
        MutFunc(_) => write!(f, "#<function>"),
        BuiltIn(_) => write!(f, "#<function>"),
        ReducerV(_) => write!(f, "#<reducer>"),
        MutableVector(v) => write!(f, "{:?}", v.as_ref().borrow()),
    }
}

// pub(crate) fn collect_pair_into_vector(p: &SteelVal) -> SteelVal {
//     VectorV(Gc::new(SteelVal::iter(p.clone()).collect::<Vector<_>>()))
// }

#[cfg(test)]
mod or_else_tests {

    use super::*;
    use crate::rerrs::ErrorKind;
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
        let input = SteelVal::VectorV(Gc::new(vector![SteelVal::IntV(1)]));
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
}
