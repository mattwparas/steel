use crate::env::Env;
use crate::expander::SteelMacro;
use crate::parser::tokens::TokenType::*;
use crate::parser::Expr;
use crate::parser::SyntaxObject;
use crate::port::SteelPort;
use crate::rerrs::SteelErr;
// use std::any::Any;
use std::any::Any;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;
use std::rc::Weak;
use SteelVal::*;

use im_rc::{HashMap, HashSet, Vector};
use std::convert::TryFrom;
use std::result;

use crate::structs::SteelStruct;

use crate::vm::DenseInstruction;
use crate::vm::EvaluationProgress;

// use std::mem;

use crate::gc::Gc;

use std::hash::{Hash, Hasher};

// use std::ops::Deref;

use crate::parser::span::Span;
use crate::vm::ConstantTable;

use crate::vm::inline_iter::{
    inline_filter_result_iter, inline_map_result_iter, inline_reduce_iter,
};
// use itertools::Itertools;
// pub use constants::ConstantTable;

// use std::collections::HashMap;

// use std::io::Read;
// use std::io::Write;

use crate::primitives::ListOperations;

// use futures::future::FutureExt;
use futures::FutureExt;
// use futures::Sha
use futures::future::Shared;
use std::future::Future;
use std::pin::Pin;

use crate::lazy_stream::LazyStream;
use crate::lazy_stream::LazyStreamIter;

pub type RcRefSteelVal = Rc<RefCell<SteelVal>>;
pub fn new_rc_ref_cell(x: SteelVal) -> RcRefSteelVal {
    Rc::new(RefCell::new(x))
}

pub type Result<T> = result::Result<T, SteelErr>;
pub type FunctionSignature = fn(&[Gc<SteelVal>]) -> Result<Gc<SteelVal>>;
pub type StructClosureSignature = fn(Vec<Gc<SteelVal>>, &SteelStruct) -> Result<Gc<SteelVal>>;

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
pub type AsyncSignature = fn(&[Gc<SteelVal>]) -> FutureResult;

pub type BoxedFutureResult = Pin<Box<dyn Future<Output = Result<Gc<SteelVal>>>>>;

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

// async fn join_futures(args: &[AsyncSignature]) -> Vec<Result<Gc<SteelVal>>> {
//     futures::future::join_all(args.into_iter().map(|x| x(&[])))
// }

/*
The alternative is providing some way to just throw an entire process onto another thread via deep cloning...
This might be slower ultimately but it might make sense for some shallow functions

especially since creating the root is honestly pretty cheap... function pointers are not terribly expensive

create a temporary sendable env that can be moved across to another thread
this just spawns a new instance of the VM by duplicating it, moving it to the transient thread, and then rebuilds the instance
to continue operation

there is some overhead here but I think it might be worth it?
*/

// async fn embedded_nonblocking() -> SteelVal {
//     unimplemented!()
// }

// fn embedded_nonblocking_2() -> impl Future<Output = SteelVal> {
//     unimplemented!();
// }

// Box<Fn(i32) -> i32>

pub trait StructFunctions {
    fn generate_bindings() -> Vec<(String, SteelVal)>;
}

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

// macro_rules! ok_val {
//     ($variant:ty, $value:expr) => {
//         Ok(Rc::new(SteelVal::$variant($value)));
//     };
// }

/// Unwraps the `SteelVal::Custom` with the given type. The type must implement the `CustomType` trait.
/// If the type does not match, then
/// the macro returns a `SteelErr::ConverstionError`. If the type does match, return the
/// underlying value.
///
/// # Example
/// ```rust
///
///
///
/// ```
///
#[macro_export]
macro_rules! unwrap {
    ($x:expr, $body:ty) => {{
        if let crate::rvals::SteelVal::Custom(ref v) = $x {
            let left_type = (*v).as_any();
            let left = left_type.downcast_ref::<$body>();
            left.map(|x| x.clone()).ok_or_else(|| {
                let error_message = format!(
                    "Type Mismatch: Type of SteelVal did not match the given type: {}",
                    stringify!($body)
                );
                crate::rerrs::SteelErr::ConversionError(error_message, None)
            })
        } else {
            let error_message = format!(
                "Type Mismatch: Type of SteelVal did not match the given type: {}",
                stringify!($body)
            );
            Err(crate::rerrs::SteelErr::ConversionError(error_message, None))
        }
    }};
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
    Pair(Gc<SteelVal>, Option<Gc<SteelVal>>),
    /// Vectors are represented as `im_rc::Vector`'s, which are immutable
    /// data structures
    VectorV(Vector<Gc<SteelVal>>),
    /// Void return value
    Void,
    /// Represents strings
    StringV(String),
    /// Represents built in rust functions
    FuncV(FunctionSignature),
    /// Represents Steel Lambda functions or closures defined inside the environment
    LambdaV(SteelLambda),
    /// Represents built in macros,
    MacroV(SteelMacro),
    /// Represents a symbol, internally represented as `String`s
    SymbolV(String),
    /// Container for a type that implements the `Custom Type` trait. (trait object)
    Custom(Box<dyn CustomType>),
    // Embedded HashMap
    HashMapV(HashMap<Gc<SteelVal>, Gc<SteelVal>>),
    // Embedded HashSet
    HashSetV(HashSet<Gc<SteelVal>>),
    /// Represents a scheme-only struct
    StructV(SteelStruct),
    /// Represents a special rust closure
    StructClosureV(SteelStruct, StructClosureSignature),
    /// Represents a port object
    PortV(SteelPort),
    /// Represents a bytecode closure
    Closure(ByteCodeLambda),
    /// Generic iterator wrapper?
    IterV(Transducer),
    // Generic IntoIter wrapper?
    // Promise(Gc<SteelVal>),
    /// Async Function wrapper
    FutureFunc(AsyncSignature),
    // Boxed Future Result
    FutureV(FutureResult),
    // Mutable Box
    // Functions that want to operate by reference must move the value into a mutable box
    // This deep clones the value but then the value can be mutably snatched
    // MutableBox(Gc<RefCell<SteelVal>>),
    StreamV(LazyStream),
    // Break the cycle somehow
    // EvaluationEnv(Weak<RefCell<Env>>),
    /// Mutable box - lets you put a value in there and change what it points to
    BoxV(RefCell<Gc<SteelVal>>),
}

pub struct SIterator(Box<dyn IntoIterator<IntoIter = Iter, Item = Result<Gc<SteelVal>>>>);

// pub struct

// pub trait Transduce {
//     fn run() -> Gc<SteelVal>;
// }

enum CollectionType {
    List,
    Vector,
}

// Make a transducer actually contain an option to a rooted value, otherwise
// it is a source agnostic transformer on the (eventual) input
#[derive(Clone)]
pub struct Transducer {
    // root: Gc<SteelVal>,
    pub(crate) ops: Vector<Transducers>,
}

impl Transducer {
    pub fn new() -> Self {
        Transducer { ops: Vector::new() }
    }

    pub fn append(&mut self, other: Self) {
        self.ops.append(other.ops)
    }

    pub fn push(&mut self, t: Transducers) {
        self.ops.push_back(t);
    }

    // This runs through the iterators  in sequence in the transducer
    // we want to then finish with a reducer
    // TODO see transduce vs educe
    // TODO change the return type to match the given input type
    // optionally add an argument to select the return type manually
    pub fn run<CT: ConstantTable>(
        &self,
        root: Gc<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
        collection_type: Option<Gc<SteelVal>>,
    ) -> Result<Gc<SteelVal>> {
        // if let Some(collection_type) = collection_type {
        //     match collection_type.as_ref() {}
        // }

        let output_type = match root.as_ref() {
            SteelVal::VectorV(_) => CollectionType::Vector,
            _ => CollectionType::List,
        };

        let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> = match root.as_ref() {
            SteelVal::VectorV(v) => Box::new(v.into_iter().map(|x| Ok(Gc::clone(x)))),
            SteelVal::Pair(_, _) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.clone(),
                constants,
                cur_inst_span,
                repl,
                callback,
            )),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in &self.ops {
            my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        }

        if let Some(collection_type) = collection_type {
            if let SteelVal::SymbolV(n) = collection_type.as_ref() {
                match n.as_ref() {
                    "list" => ListOperations::built_in_list_normal_iter(my_iter),
                    "vector" => crate::primitives::VectorOperations::vec_construct_iter(my_iter),
                    _ => stop!(Generic => "Cannot collect into an undefined type"),
                }
            } else {
                stop!(Generic => "execute takes a symbol")
            }
        } else {
            match output_type {
                CollectionType::List => ListOperations::built_in_list_normal_iter(my_iter),
                CollectionType::Vector => {
                    crate::primitives::VectorOperations::vec_construct_iter(my_iter)
                }
            }
            // match root.as_ref()
            // unimplemented!()
        }

        // match root.as_ref() {
        //     SteelVal::VectorV(v) => {
        //         let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> =
        //             Box::new(v.into_iter().map(|x| Ok(Gc::clone(x))));
        //         for t in &self.ops {
        //             my_iter =
        //                 t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        //         }

        //         crate::primitives::VectorOperations::vec_construct_iter(my_iter)
        //     }
        //     SteelVal::Pair(_, _) => {
        //         let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> =
        //             Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x)));
        //         for t in &self.ops {
        //             my_iter =
        //                 t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        //         }

        //         crate::primitives::VectorOperations::vec_construct_iter(my_iter)
        //     }
        //     SteelVal::StreamV(lazy_stream) => {
        //         let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> =
        //             Box::new(LazyStreamIter::new(
        //                 lazy_stream.clone(),
        //                 constants,
        //                 cur_inst_span,
        //                 repl,
        //                 callback,
        //             ));

        //         for t in &self.ops {
        //             my_iter =
        //                 t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        //         }

        //         crate::primitives::VectorOperations::vec_construct_iter(my_iter)
        //     }
        //     _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        // }
    }

    pub fn transduce<CT: ConstantTable>(
        &self,
        root: Gc<SteelVal>,
        initial_value: Gc<SteelVal>,
        reducer: Gc<SteelVal>,
        constants: &CT,
        cur_inst_span: &Span,
        repl: bool,
        callback: &EvaluationProgress,
    ) -> Result<Gc<SteelVal>> {
        let mut my_iter: Box<dyn Iterator<Item = Result<Gc<SteelVal>>>> = match root.as_ref() {
            SteelVal::VectorV(v) => Box::new(v.into_iter().map(|x| Ok(Gc::clone(x)))),
            SteelVal::Pair(_, _) => Box::new(SteelVal::iter(root).into_iter().map(|x| Ok(x))),
            SteelVal::StreamV(lazy_stream) => Box::new(LazyStreamIter::new(
                lazy_stream.clone(),
                constants,
                cur_inst_span,
                repl,
                callback,
            )),
            _ => stop!(TypeMismatch => "Iterators not yet implemented for this type"),
        };

        for t in &self.ops {
            my_iter = t.into_transducer(my_iter, constants, cur_inst_span, repl, callback)?;
        }

        inline_reduce_iter(
            my_iter,
            initial_value,
            reducer,
            constants,
            cur_inst_span,
            repl,
            callback,
        )
    }
}

#[derive(Clone)]
pub enum Transducers {
    Map(Gc<SteelVal>),    // function
    Filter(Gc<SteelVal>), // function
    Take(Gc<SteelVal>),   // integer
}

impl Transducers {
    pub fn into_transducer<
        'global,
        I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
        CT: ConstantTable,
    >(
        &self,
        iter: I,
        // stack_func: Gc<SteelVal>,
        constants: &'global CT,
        cur_inst_span: &'global Span,
        repl: bool,
        callback: &'global EvaluationProgress,
    ) -> Result<Box<dyn Iterator<Item = Result<Gc<SteelVal>>> + 'global>> {
        match self {
            Transducers::Map(func) => Ok(Box::new(inline_map_result_iter(
                iter,
                Gc::clone(func),
                constants,
                cur_inst_span,
                repl,
                callback,
            ))),
            Transducers::Filter(func) => Ok(Box::new(inline_filter_result_iter(
                iter,
                Gc::clone(func),
                constants,
                cur_inst_span,
                repl,
                callback,
            ))),
            Transducers::Take(num) => {
                if let SteelVal::IntV(num) = num.as_ref() {
                    if *num < 0 {
                        stop!(ContractViolation => "take transducer must have a position number"; *cur_inst_span)
                    }
                    Ok(Box::new(iter.take(*num as usize)))
                } else {
                    stop!(TypeMismatch => "take transducer takes an integer"; *cur_inst_span)
                }
            }
        }
    }
}

impl Hash for SteelVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BoolV(b) => b.hash(state),
            NumV(_) => {
                unimplemented!();
            }
            IntV(i) => i.hash(state),
            CharV(c) => c.hash(state),
            Pair(car, cdr) => {
                car.hash(state);
                cdr.hash(state);
            }
            VectorV(v) => v.hash(state),
            Void => {
                unimplemented!();
            }
            StringV(s) => s.hash(state),
            FuncV(_) => unimplemented!(),
            LambdaV(_) => unimplemented!(),
            MacroV(_) => unimplemented!(),
            SymbolV(sym) => {
                "symbol".hash(state);
                sym.hash(state);
                // format!("symbol: {}")
            }
            Custom(_) => unimplemented!(),
            StructV(_) => unimplemented!(),
            StructClosureV(_, _) => unimplemented!(),
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

pub struct Iter(Option<Gc<SteelVal>>);

// pub struct IntoIter(Option<Gc<SteelVal>>);

impl SteelVal {
    pub fn iter(_self: Gc<SteelVal>) -> Iter {
        Iter(Some(_self))
    }

    // pub fn into_iter(_self: Gc<SteelVal>) -> Iter {
    //     IntoIter(Some(_self))
    // }

    pub fn is_truthy(&self) -> bool {
        match &self {
            SteelVal::BoolV(false) => false,
            SteelVal::VectorV(v) => {
                if v.is_empty() {
                    false
                } else {
                    true
                }
            }
            _ => true,
        }
    }

    pub fn is_hashable(&self) -> bool {
        match self {
            BoolV(_)
            | IntV(_)
            | CharV(_)
            | Pair(_, _)
            | VectorV(_)
            | StringV(_)
            | SymbolV(_)
            | HashMapV(_)
            | Closure(_) => true,
            _ => false,
        }
    }
}

impl Iterator for Iter {
    type Item = Gc<SteelVal>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(_self) = &self.0 {
            match _self.as_ref() {
                SteelVal::Pair(car, cdr) => {
                    match (car, cdr) {
                        (first, Some(rest)) => {
                            let ret_val = Some(Gc::clone(&first));
                            self.0 = Some(Gc::clone(&rest));
                            ret_val
                        }
                        (first, None) => {
                            let ret_val = Some(Gc::clone(&first));
                            self.0 = None;
                            ret_val
                        } // _ => None,
                    }
                }
                SteelVal::VectorV(v) => {
                    if v.is_empty() {
                        None
                    } else {
                        let ret_val = Some(Gc::clone(&_self));
                        self.0 = None;
                        ret_val
                    }
                }
                _ => {
                    let ret_val = Some(Gc::clone(&_self));
                    self.0 = None;
                    ret_val
                }
            }
        } else {
            None
        }
    }
}

impl SteelVal {
    pub fn bool_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<bool, E> {
        match self {
            Self::BoolV(v) => Ok(*v),
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
    ) -> std::result::Result<Vector<Gc<SteelVal>>, E> {
        match self {
            Self::VectorV(v) => Ok(v.clone()),
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

    pub fn struct_func_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<(&SteelStruct, &StructClosureSignature), E> {
        match self {
            Self::StructClosureV(steel_struct, struct_closure) => {
                Ok((steel_struct, struct_closure))
            }
            _ => Err(err()),
        }
    }

    pub fn lambda_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&SteelLambda, E> {
        match self {
            Self::LambdaV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn macro_or_else<E, F: FnOnce() -> E>(
        &self,
        err: F,
    ) -> std::result::Result<&SteelMacro, E> {
        match self {
            Self::MacroV(v) => Ok(&v),
            _ => Err(err()),
        }
    }

    pub fn symbol_or_else<E, F: FnOnce() -> E>(&self, err: F) -> std::result::Result<&str, E> {
        match self {
            Self::SymbolV(v) => Ok(&v),
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

impl Drop for SteelVal {
    // don't want to blow the stack with destructors,
    // but also don't want to walk the whole list.
    // So walk the list until we find a non-uniquely owned item
    fn drop(&mut self) {
        let mut curr = match *self {
            Pair(_, ref mut next) => next.take(),
            _ => return,
        };
        loop {
            match curr {
                Some(r) => match Gc::try_unwrap(r) {
                    Ok(Pair(_, ref mut next)) => curr = next.take(),
                    _ => return,
                },
                _ => return,
            }
        }
    }
}

// sometimes you want to just
// return an expression
impl TryFrom<Expr> for SteelVal {
    type Error = SteelErr;
    fn try_from(e: Expr) -> std::result::Result<Self, Self::Error> {
        // let span = || {
        //     e.span()
        // }
        match &e {
            Expr::Atom(a) => match &a.ty {
                OpenParen => Err(SteelErr::UnexpectedToken("(".to_string(), Some(e.span()))),
                CloseParen => Err(SteelErr::UnexpectedToken(")".to_string(), Some(e.span()))),
                QuoteTick => Err(SteelErr::UnexpectedToken("'".to_string(), Some(e.span()))),
                Unquote => Err(SteelErr::UnexpectedToken(",".to_string(), Some(e.span()))),
                UnquoteSplice => Err(SteelErr::UnexpectedToken(",@".to_string(), Some(e.span()))),
                QuasiQuote => Err(SteelErr::UnexpectedToken("`".to_string(), Some(e.span()))),
                Hash => Err(SteelErr::UnexpectedToken("#".to_string(), Some(e.span()))),
                BooleanLiteral(x) => Ok(BoolV(*x)),
                Identifier(x) => Ok(SymbolV(x.clone())),
                NumberLiteral(x) => Ok(NumV(*x)),
                IntegerLiteral(x) => Ok(IntV(*x)),
                StringLiteral(x) => Ok(StringV(x.clone())),
                CharacterLiteral(x) => Ok(CharV(*x)),
                Error => Err(SteelErr::UnexpectedToken(
                    "error".to_string(),
                    Some(e.span()),
                )),
                Comment => Err(SteelErr::UnexpectedToken(
                    "comment".to_string(),
                    Some(e.span()),
                )),
            },
            Expr::VectorVal(lst) => {
                let items: std::result::Result<Vec<Gc<Self>>, Self::Error> = lst
                    .iter()
                    .map(|x| Self::try_from(x.clone()).map(Gc::new))
                    .collect();

                ListOperations::built_in_list_func()(&items?).map(|x| (*x).clone())
                // Ok(VectorV(items?))

                // let items: std::result::Result<Vector<Self>, Self::Error> =
                //     lst.iter().map(|x| Self::try_from(x.clone())).collect();
                // Ok(VectorV(items?))
            }
        }
    }
}

/// Sometimes you want to execute a list
/// as if it was an expression
impl TryFrom<&SteelVal> for Expr {
    type Error = &'static str;
    fn try_from(r: &SteelVal) -> result::Result<Self, Self::Error> {
        match r {
            BoolV(x) => Ok(Expr::Atom(SyntaxObject::default(BooleanLiteral(*x)))),
            NumV(x) => Ok(Expr::Atom(SyntaxObject::default(NumberLiteral(*x)))),
            IntV(x) => Ok(Expr::Atom(SyntaxObject::default(IntegerLiteral(*x)))),
            VectorV(lst) => {
                let items: result::Result<Vec<Self>, Self::Error> = lst
                    .into_iter()
                    .map(|x| Self::try_from(x.as_ref()))
                    .collect();
                Ok(Expr::VectorVal(items?))
            }
            Void => Err("Can't convert from Void to expression!"),
            StringV(x) => Ok(Expr::Atom(SyntaxObject::default(StringLiteral(x.clone())))),
            FuncV(_) => Err("Can't convert from Function to expression!"),
            LambdaV(_) => Err("Can't convert from Lambda to expression!"),
            MacroV(_) => Err("Can't convert from Macro to expression!"),
            SymbolV(x) => Ok(Expr::Atom(SyntaxObject::default(Identifier(x.clone())))),
            Custom(_) => Err("Can't convert from Custom Type to expression!"),
            // Pair(_, _) => Err("Can't convert from pair"), // TODO
            Pair(_, _) => {
                if let VectorV(ref lst) = collect_pair_into_vector(r) {
                    let items: result::Result<Vec<Self>, Self::Error> = lst
                        .into_iter()
                        .map(|x| Self::try_from(x.as_ref()))
                        .collect();
                    Ok(Expr::VectorVal(items?))
                } else {
                    Err("Couldn't convert from list to expression")
                }
            }
            CharV(x) => Ok(Expr::Atom(SyntaxObject::default(CharacterLiteral(*x)))),
            StructV(_) => Err("Can't convert from Struct to expression!"),
            StructClosureV(_, _) => Err("Can't convert from struct-function to expression!"),
            PortV(_) => Err("Can't convert from port to expression!"),
            Closure(_) => Err("Can't convert from bytecode closure to expression"),
            HashMapV(_) => Err("Can't convert from hashmap to expression!"),
            HashSetV(_) => Err("Can't convert from hashset to expression!"),
            IterV(_) => Err("Can't convert from iterator to expression!"),
            FutureFunc(_) => Err("Can't convert from future function to expression!"),
            FutureV(_) => Err("Can't convert future to expression!"),
            // Promise(_) => Err("Can't convert from promise to expression!"),
            StreamV(_) => Err("Can't convert from stream to expression!"),
            BoxV(_) => Err("Can't convert from box to expression!"),
        }
    }
}

impl Eq for SteelVal {}

// TODO add tests
impl PartialEq for SteelVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BoolV(l), BoolV(r)) => l == r,
            // (NumV(l), NumV(r)) => l == r,
            (IntV(l), IntV(r)) => l == r,
            // (NumV(l), IntV(r)) => *l == *r as f64,
            // (IntV(l), NumV(r)) => *l as f64 == *r,
            (StringV(l), StringV(r)) => l == r,
            (VectorV(l), VectorV(r)) => l == r,
            (SymbolV(l), SymbolV(r)) => l == r,
            (CharV(l), CharV(r)) => l == r,
            (Pair(_, _), Pair(_, _)) => {
                collect_pair_into_vector(self) == collect_pair_into_vector(other)
            }
            (HashSetV(l), HashSetV(r)) => l == r,
            (HashMapV(l), HashMapV(r)) => l == r,
            (StructV(l), StructV(r)) => l == r,
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

#[derive(Clone)]
pub struct ByteCodeLambda {
    /// body of the function with identifiers yet to be bound
    body_exp: Rc<[DenseInstruction]>,
    /// parent environment that created this Lambda.
    /// the actual environment with correct bindings is built at runtime
    /// once the function is called
    // parent_env: Option<Rc<RefCell<Env>>>,
    /// parent subenvironment that created this lambda.
    /// the actual environment gets upgraded at runtime if needed
    sub_expression_env: Weak<RefCell<Env>>,
    // bytecode instruction body
    // body_byte: Vec<Instruction>,
    offset: usize,
    arity: usize,
    ndef_body: usize,
}

impl Hash for ByteCodeLambda {
    fn hash<H: Hasher>(&self, state: &mut H) {
        &self.body_exp.as_ptr().hash(state);
        &self.sub_expression_env.as_ptr().hash(state);
    }
}

impl ByteCodeLambda {
    pub fn new(
        body_exp: Vec<DenseInstruction>,
        // parent_env: Option<Rc<RefCell<Env>>>,
        sub_expression_env: Weak<RefCell<Env>>,
        offset: usize,
        arity: usize,
        ndef_body: usize,
    ) -> ByteCodeLambda {
        ByteCodeLambda {
            body_exp: Rc::from(body_exp.into_boxed_slice()),
            // parent_env,
            sub_expression_env,
            offset,
            arity,
            ndef_body,
        }
    }

    // pub fn params_exp(&self) -> &[String] {
    //     &self.params_exp
    // }

    pub fn body_exp(&self) -> Rc<[DenseInstruction]> {
        Rc::clone(&self.body_exp)
    }

    // pub fn parent_env(&self) -> Option<&Rc<RefCell<Env>>> {
    //     self.parent_env.as_ref()
    // }

    pub fn sub_expression_env(&self) -> &Weak<RefCell<Env>> {
        &self.sub_expression_env
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn ndef_body(&self) -> usize {
        self.ndef_body
    }
}

#[derive(Clone)]
/// struct representing data required to describe a scheme function
pub struct SteelLambda {
    /// symbols representing the arguments to the function
    params_exp: Vec<String>,
    /// body of the function with identifiers yet to be bound
    body_exp: Expr,
    /// parent environment that created this Lambda.
    /// the actual environment with correct bindings is built at runtime
    /// once the function is called
    parent_env: Option<Rc<RefCell<Env>>>,
    /// parent subenvironment that created this lambda.
    /// the actual environment gets upgraded at runtime if needed
    sub_expression_env: Option<Weak<RefCell<Env>>>,
}
impl SteelLambda {
    pub fn new(
        params_exp: Vec<String>,
        body_exp: Expr,
        parent_env: Option<Rc<RefCell<Env>>>,
        sub_expression_env: Option<Weak<RefCell<Env>>>,
    ) -> SteelLambda {
        SteelLambda {
            params_exp,
            body_exp,
            parent_env,
            sub_expression_env,
        }
    }
    /// symbols representing the arguments to the function
    pub fn params_exp(&self) -> &[String] {
        &self.params_exp
    }
    /// body of the function with identifiers yet to be bound
    pub fn body_exp(&self) -> &Expr {
        &self.body_exp
    }

    pub fn pretty_print_closure(&self) -> String {
        let params = self.params_exp().join(" ");
        format!(
            "(lambda ({}) {})",
            params.to_string(),
            self.body_exp().to_string()
        )
    }

    /// parent environment that created this Lambda.
    ///
    /// The actual environment with correct bindings is built at runtime
    /// once the function is called
    pub fn parent_env(&self) -> Option<&Rc<RefCell<Env>>> {
        self.parent_env.as_ref()
    }

    pub fn sub_expression_env(&self) -> Option<&Weak<RefCell<Env>>> {
        self.sub_expression_env.as_ref()
    }
}

impl fmt::Display for SteelVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // at the top level, print a ' if we are
        // trying to print a symbol or list
        match self {
            SymbolV(_) | Pair(_, _) => write!(f, "'")?,
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
            SymbolV(_) | Pair(_, _) => write!(f, "'")?,
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
        LambdaV(l) => write!(f, "#<{}>", l.pretty_print_closure()),
        MacroV(_) => write!(f, "#<macro>"),
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
        Pair(_, _) => {
            let v = collect_pair_into_vector(val);
            display_helper(&v, f)
        }
        StructV(s) => write!(f, "#<{}>", s.pretty_print()), // TODO
        StructClosureV(_, _) => write!(f, "#<struct-constructor>"),
        PortV(_) => write!(f, "#<port>"),
        Closure(_) => write!(f, "#<bytecode-closure>"),
        HashMapV(hm) => write!(f, "#<hashmap {:?}>", hm),
        IterV(_) => write!(f, "#<iterator>"),
        HashSetV(hs) => write!(f, "#<hashset {:?}>", hs),
        FutureFunc(_) => write!(f, "#<future-func>"),
        FutureV(_) => write!(f, "#<future>"),
        // Promise(_) => write!(f, "#<promise>"),
        StreamV(_) => write!(f, "#<stream>"),
        BoxV(b) => write!(f, "#<box {:?}>", b.borrow()),
    }
}

fn collect_pair_into_vector(mut p: &SteelVal) -> SteelVal {
    let mut lst = Vector::new();

    loop {
        if let Pair(cons, cdr) = p {
            lst.push_back(Gc::clone(cons));
            match cdr.as_ref() {
                Some(rest) => match rest.as_ref() {
                    Pair(_, _) => p = rest,
                    _ => {
                        lst.push_back(Gc::clone(rest));
                        return VectorV(lst);
                    }
                },
                None => {
                    return VectorV(lst);
                }
            }
        }
    }
}

#[test]
fn display_test() {
    use crate::parser::tokens::TokenType;
    use im_rc::vector;
    assert_eq!(SteelVal::BoolV(false).to_string(), "#false");
    assert_eq!(SteelVal::NumV(1.0).to_string(), "1.0");
    assert_eq!(
        SteelVal::FuncV(|_args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            Ok(Gc::new(SteelVal::VectorV(vector![])))
        })
        .to_string(),
        "#<function>"
    );
    assert_eq!(
        SteelVal::LambdaV(SteelLambda::new(
            vec!["arg1".to_owned()],
            Expr::Atom(SyntaxObject::default(TokenType::NumberLiteral(1.0))),
            Some(Rc::new(RefCell::new(crate::env::Env::default_env()))),
            None
        ))
        .to_string(),
        "#<(lambda (arg1) 1.0)>"
    );
    assert_eq!(SteelVal::SymbolV("foo".to_string()).to_string(), "'foo");
}

#[test]
fn display_list_test() {
    use crate::parser::tokens::TokenType;
    use im_rc::vector;
    assert_eq!(VectorV(vector![]).to_string(), "'#()");
    assert_eq!(
        VectorV(
            vector![
                BoolV(false),
                NumV(1.0),
                LambdaV(SteelLambda::new(
                    vec!["arg1".to_owned()],
                    Expr::Atom(SyntaxObject::default(TokenType::NumberLiteral(1.0))),
                    Some(Rc::new(RefCell::new(crate::env::Env::default_env()))),
                    None
                ))
            ]
            .into_iter()
            .map(Gc::new)
            .collect()
        )
        .to_string(),
        "\'#(#false 1.0 #<(lambda (arg1) 1.0)>)"
    );
    assert_eq!(
        VectorV(
            vector![
                VectorV(
                    vector![
                        NumV(1.0),
                        VectorV(
                            vector![NumV(2.0), NumV(3.0)]
                                .into_iter()
                                .map(Gc::new)
                                .collect()
                        )
                    ]
                    .into_iter()
                    .map(Gc::new)
                    .collect()
                ),
                VectorV(
                    vector![NumV(4.0), NumV(5.0)]
                        .into_iter()
                        .map(Gc::new)
                        .collect()
                ),
                NumV(6.0),
                VectorV(vector![NumV(7.0)].into_iter().map(Gc::new).collect())
            ]
            .into_iter()
            .map(Gc::new)
            .collect()
        )
        .to_string(),
        "'#((1.0 (2.0 3.0)) (4.0 5.0) 6.0 (7.0))"
    );
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
        let input = SteelVal::VectorV(
            vector![SteelVal::IntV(1)]
                .into_iter()
                .map(Gc::new)
                .collect(),
        );
        assert_eq!(
            input.vector_or_else(throw!(Generic => "test")).unwrap(),
            vector![SteelVal::IntV(1)]
                .into_iter()
                .map(Gc::new)
                .collect()
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
        let input = SteelVal::StringV("foo".to_string());
        assert!(input.void_or_else(throw!(Generic => "test")).is_err());
    }

    #[test]
    fn string_or_else_test_good() {
        let input = SteelVal::StringV("foo".to_string());
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
        let input = SteelVal::SymbolV("foo".to_string());
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

#[cfg(test)]
mod transducer_tests {
    // use super::*;
    use crate::test_util::assert_script;

    #[test]
    fn generic_transducer() {
        let script = r#"
            (define x (mapping (fn (x) x))) ;; identity
            (define y (filtering even?)) ;; get only even ones
            (define z (taking 15)) ;; take the first 15 from the range
            (define xf (compose x y z))
            (assert! 
                (equal? 
                    (transduce xf + 0 (range 0 100)) ;; => 210
                    210))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_execution() {
        let script = r#"
        (define x (mapping (fn (x) x))) ;; identity
        (define y (filtering even?)) ;; get only even ones
        (define z (taking 15)) ;; take the first 15 from the range
        (define xf (compose x y z))
        (define result
            (execute xf (range 0 100)))
        
        (define expected '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28))
        (assert! (equal? result expected))
        "#;
        assert_script(script);
    }

    #[test]
    fn generic_exeuction_output_different_type() {
        let script = r#"
        (define x (mapping (fn (x) x))) ;; identity
        (define y (filtering even?)) ;; get only even ones
        (define z (taking 15)) ;; take the first 15 from the range
        (define xf (compose x y z))
        (define result
            (execute xf (range 0 100) 'vector))
        
        (define expected (vector 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28))
        (assert! (equal? result expected))
        "#;
        assert_script(script);
    }
}
