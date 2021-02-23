use crate::{
    contracts::{ContractType, ContractedFunction},
    core::instructions::DenseInstruction,
    env::Env,
    gc::Gc,
    lazy_stream::LazyStream,
    // parser::{tokens::TokenType::*, Expr, SyntaxObject},
    port::SteelPort,
    // primitives::ListOperations,
    rerrs::SteelErr,
    structs::SteelStruct,
};

use std::{
    any::Any,
    cell::RefCell,
    cmp::Ordering,
    // convert::TryFrom,
    fmt,
    future::Future,
    hash::{Hash, Hasher},
    pin::Pin,
    rc::{Rc, Weak},
    result,
};

// use std::any::Any;
use SteelVal::*;

use im_rc::{HashMap, HashSet, Vector};

use futures::future::Shared;
use futures::FutureExt;

pub type RcRefSteelVal = Rc<RefCell<SteelVal>>;
pub fn new_rc_ref_cell(x: SteelVal) -> RcRefSteelVal {
    Rc::new(RefCell::new(x))
}

pub type Result<T> = result::Result<T, SteelErr>;
pub type FunctionSignature = fn(&[Gc<SteelVal>]) -> Result<Gc<SteelVal>>;
// pub type FunctionSignature = fn(&[SteelVal]) -> Result<SteelVal>;
pub type StructClosureSignature = fn(Vec<Gc<SteelVal>>, &SteelStruct) -> Result<Gc<SteelVal>>;

// This would mean we would have to rewrite literally everything to not return Gc'd values,
// but it would also make linked lists like impossible to use
// Because the internals of the linked list wouldn't be as easy to use with easy shared usage
pub type PossibleOtherFunctionSignature = fn(&[SteelVal]) -> Result<SteelVal>;

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
    Custom(Box<dyn CustomType>),
    // Embedded HashMap
    HashMapV(HashMap<Gc<SteelVal>, Gc<SteelVal>>),
    // Embedded HashSet
    HashSetV(HashSet<Gc<SteelVal>>),
    /// Represents a scheme-only struct
    StructV(Box<SteelStruct>),
    /// Represents a special rust closure
    StructClosureV(Box<SteelStruct>, StructClosureSignature),
    /// Represents a port object
    PortV(Gc<SteelPort>),
    /// Represents a bytecode closure
    Closure(Gc<ByteCodeLambda>),
    /// Generic iterator wrapper
    IterV(Gc<Transducer>),
    // Generic IntoIter wrapper
    // Promise(Gc<SteelVal>),
    /// Async Function wrapper
    FutureFunc(AsyncSignature),
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
    BoxV(RefCell<Gc<SteelVal>>),
    /// Contract
    Contract(Gc<ContractType>),
    /// Contracted Function
    ContractedFunction(Gc<ContractedFunction>),
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

pub struct SIterator(Box<dyn IntoIterator<IntoIter = Iter, Item = Result<Gc<SteelVal>>>>);

pub enum CollectionType {
    List,
    Vector,
}

// Make a transducer actually contain an option to a rooted value, otherwise
// it is a source agnostic transformer on the (eventual) input
#[derive(Clone)]
pub struct Transducer {
    // root: Gc<SteelVal>,
    pub ops: Vec<Gc<Transducers>>,
}

impl Transducer {
    pub fn new() -> Self {
        Transducer { ops: Vec::new() }
    }

    pub fn append(&mut self, mut other: Self) {
        self.ops.append(&mut other.ops)
    }

    pub fn push(&mut self, t: Gc<Transducers>) {
        self.ops.push(t);
    }
}

#[derive(Clone)]
pub enum Transducers {
    Map(Gc<SteelVal>),    // function
    Filter(Gc<SteelVal>), // function
    Take(Gc<SteelVal>),   // integer
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
            // LambdaV(_) => unimplemented!(),
            // MacroV(_) => unimplemented!(),
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

impl SteelVal {
    pub fn iter(_self: Gc<SteelVal>) -> Iter {
        Iter(Some(_self))
    }

    pub fn is_truthy(&self) -> bool {
        match &self {
            SteelVal::BoolV(false) => false,
            SteelVal::VectorV(v) => v.is_empty(),
            _ => true,
        }
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            BoolV(_)
                | IntV(_)
                | CharV(_)
                | Pair(_, _)
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
            StructClosureV(_, _) | Closure(_) | FuncV(_) | ContractedFunction(_)
        )
    }

    pub fn is_contract(&self) -> bool {
        matches!(self, Contract(_))
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

impl PartialEq for ByteCodeLambda {
    fn eq(&self, other: &Self) -> bool {
        self.body_exp == other.body_exp && self.arity == other.arity
    }
}

impl Hash for ByteCodeLambda {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.body_exp.as_ptr().hash(state);
        self.sub_expression_env.as_ptr().hash(state);
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

    pub fn body_exp(&self) -> Rc<[DenseInstruction]> {
        Rc::clone(&self.body_exp)
    }

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
        Pair(_, _) => {
            let v = collect_pair_into_vector(val);
            display_helper(&v, f)
        }
        StructV(s) => write!(f, "#<{}>", s.pretty_print()), // TODO
        StructClosureV(_, _) => write!(f, "#<struct-constructor>"),
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
        Contract(_) => write!(f, "#<contract>"),
        ContractedFunction(_) => write!(f, "#<contracted-function>"),
    }
}

pub(crate) fn collect_pair_into_vector(mut p: &SteelVal) -> SteelVal {
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

#[cfg(test)]
mod display_test {
    // use super::*;
    // use im_rc::vector;
    // use steel::parser::tokens::TokenType;

    // #[test]
    // fn display_test() {
    //     assert_eq!(SteelVal::BoolV(false).to_string(), "#false");
    //     assert_eq!(SteelVal::NumV(1.0).to_string(), "1.0");
    //     assert_eq!(
    //         SteelVal::FuncV(|_args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
    //             Ok(Gc::new(SteelVal::VectorV(vector![])))
    //         })
    //         .to_string(),
    //         "#<function>"
    //     );
    //     assert_eq!(
    //         SteelVal::LambdaV(SteelLambda::new(
    //             vec!["arg1".to_owned()],
    //             Expr::Atom(SyntaxObject::default(TokenType::NumberLiteral(1.0))),
    //             Some(Rc::new(RefCell::new(crate::env::Env::default_env()))),
    //             None
    //         ))
    //         .to_string(),
    //         "#<(lambda (arg1) 1.0)>"
    //     );
    //     assert_eq!(SteelVal::SymbolV("foo".to_string()).to_string(), "'foo");
    // }

    // #[test]
    // fn display_list_test() {
    //     assert_eq!(VectorV(vector![]).to_string(), "'#()");
    //     assert_eq!(
    //         VectorV(
    //             vector![
    //                 BoolV(false),
    //                 NumV(1.0),
    //                 LambdaV(SteelLambda::new(
    //                     vec!["arg1".to_owned()],
    //                     Expr::Atom(SyntaxObject::default(TokenType::NumberLiteral(1.0))),
    //                     Some(Rc::new(RefCell::new(crate::env::Env::default_env()))),
    //                     None
    //                 ))
    //             ]
    //             .into_iter()
    //             .map(Gc::new)
    //             .collect()
    //         )
    //         .to_string(),
    //         "\'#(#false 1.0 #<(lambda (arg1) 1.0)>)"
    //     );
    //     assert_eq!(
    //         VectorV(
    //             vector![
    //                 VectorV(
    //                     vector![
    //                         NumV(1.0),
    //                         VectorV(
    //                             vector![NumV(2.0), NumV(3.0)]
    //                                 .into_iter()
    //                                 .map(Gc::new)
    //                                 .collect()
    //                         )
    //                     ]
    //                     .into_iter()
    //                     .map(Gc::new)
    //                     .collect()
    //                 ),
    //                 VectorV(
    //                     vector![NumV(4.0), NumV(5.0)]
    //                         .into_iter()
    //                         .map(Gc::new)
    //                         .collect()
    //                 ),
    //                 NumV(6.0),
    //                 VectorV(vector![NumV(7.0)].into_iter().map(Gc::new).collect())
    //             ]
    //             .into_iter()
    //             .map(Gc::new)
    //             .collect()
    //         )
    //         .to_string(),
    //         "'#((1.0 (2.0 3.0)) (4.0 5.0) 6.0 (7.0))"
    //     );
    // }
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
