//! What kind of value something is, as far as optimizations are concerned.
//!
//! Shared by everything that describes values statically: native function
//! definitions carry their return type as one of these (filled in by the
//! `steel_derive` function macros), and the jit turns it into its own
//! `InferredType` - which additionally tracks how it is holding a value
//! (unboxed in a register or not), something this deliberately leaves out.
//!
//! A type here is a promise the optimizer relies on, so it has to hold for
//! every value the function can return, not just the common ones. In
//! particular [`ValueType::Int`] means a fixnum: a function that can return a
//! bignum is [`ValueType::Number`].

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum ValueType {
    /// Nothing known.
    #[default]
    Any,
    Bool,
    /// A fixnum (`SteelVal::IntV`) - never a bignum.
    Int,
    /// A float (`SteelVal::NumV`).
    Float,
    /// Some number: fixnum, bignum, float, rational or complex.
    Number,
    Char,
    String,
    Symbol,
    /// A proper list, `'()` included.
    List,
    /// A pair that may not be a proper list.
    Pair,
    /// An immutable vector.
    Vector,
    /// A mutable vector.
    MutableVector,
    /// An immutable hash map (`SteelVal::HashMapV`).
    HashMap,
    /// An immutable hash set (`SteelVal::HashSetV`).
    HashSet,
    /// A bytevector (`SteelVal::ByteVector`).
    ByteVector,
    Void,
}

impl ValueType {
    /// Whether `value` is of this type.
    pub fn matches(&self, value: &crate::SteelVal) -> bool {
        use crate::SteelVal::*;
        match self {
            ValueType::Any => true,
            ValueType::Bool => matches!(value, BoolV(_)),
            ValueType::Int => matches!(value, IntV(_)),
            ValueType::Float => matches!(value, NumV(_)),
            ValueType::Number => matches!(
                value,
                IntV(_) | NumV(_) | BigNum(_) | Rational(_) | BigRational(_) | Complex(_)
            ),
            ValueType::Char => matches!(value, CharV(_)),
            ValueType::String => matches!(value, StringV(_)),
            ValueType::Symbol => matches!(value, SymbolV(_)),
            ValueType::List => matches!(value, ListV(_)),
            ValueType::Pair => matches!(value, Pair(_)),
            ValueType::Vector => matches!(value, VectorV(_)),
            ValueType::MutableVector => matches!(value, MutableVector(_)),
            ValueType::HashMap => matches!(value, HashMapV(_)),
            ValueType::HashSet => matches!(value, HashSetV(_)),
            ValueType::ByteVector => matches!(value, ByteVector(_)),
            ValueType::Void => matches!(value, Void),
        }
    }

    /// The spelling used by `returns = "..."` in the function macros.
    pub fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "any" => Self::Any,
            "bool" => Self::Bool,
            "int" => Self::Int,
            "float" => Self::Float,
            "number" => Self::Number,
            "char" => Self::Char,
            "string" => Self::String,
            "symbol" => Self::Symbol,
            "list" => Self::List,
            "pair" => Self::Pair,
            "vector" => Self::Vector,
            "mutable-vector" => Self::MutableVector,
            "hashmap" => Self::HashMap,
            "hashset" => Self::HashSet,
            "bytevector" => Self::ByteVector,
            "void" => Self::Void,
            _ => return None,
        })
    }
}

/// The argument types of a native function, from the macros' `args = "..."`.
///
/// The promise is about successful calls: if a call with the right number of
/// arguments returns normally, argument `i` had type `fixed[i]`, and every
/// argument past the fixed ones had type `rest`. That is what lets an analysis
/// learn the type of a variable from a call it was passed to. It says nothing
/// about which values are accepted - see [`Raises`] for that.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ArgTypes {
    pub fixed: &'static [ValueType],
    /// The type of every argument past `fixed`, for variadic functions (and
    /// the optional arguments of `Range`/`AtMost` ones).
    pub rest: Option<ValueType>,
}

impl ArgTypes {
    /// The declared type of argument `index`, if there is one.
    pub fn get(&self, index: usize) -> Option<ValueType> {
        self.fixed.get(index).copied().or(self.rest)
    }

    /// How `args` compare with the declaration.
    pub fn check(&self, args: &[crate::SteelVal]) -> ArgMatch {
        if args.len() < self.fixed.len() || (self.rest.is_none() && args.len() > self.fixed.len()) {
            return ArgMatch::BadCount;
        }
        for (index, arg) in args.iter().enumerate() {
            let declared = self.get(index).unwrap_or(ValueType::Any);
            if !declared.matches(arg) {
                return ArgMatch::BadType {
                    index,
                    declared,
                    found: arg.to_string(),
                };
            }
        }
        ArgMatch::Match
    }
}

/// See [`ArgTypes::check`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgMatch {
    Match,
    BadCount,
    BadType {
        index: usize,
        declared: ValueType,
        found: String,
    },
}

/// When a native function can raise an error, from the macros' `raises = "..."`.
/// Calls with the wrong number of arguments always may.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum Raises {
    /// Nothing known.
    #[default]
    Maybe,
    /// Only when some argument is not of its declared type (see [`ArgTypes`]):
    /// once the argument types are known, the call cannot fail.
    BadArgs,
    /// Never.
    Never,
}

impl Raises {
    /// The spelling used by `raises = "..."` in the function macros.
    pub fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "maybe" => Self::Maybe,
            "bad-args" => Self::BadArgs,
            "never" => Self::Never,
            _ => return None,
        })
    }
}

/// What a native function hands back, for `check_return`.
pub trait ReturnedValue {
    fn returned_value(&self) -> Option<&crate::SteelVal>;
    /// Whether this is an error (as opposed to a value, or a context function
    /// handing control to the vm).
    fn is_error(&self) -> bool;
}

impl ReturnedValue for crate::SteelVal {
    fn returned_value(&self) -> Option<&crate::SteelVal> {
        Some(self)
    }
    fn is_error(&self) -> bool {
        false
    }
}

impl<E> ReturnedValue for Result<crate::SteelVal, E> {
    fn returned_value(&self) -> Option<&crate::SteelVal> {
        self.as_ref().ok()
    }
    fn is_error(&self) -> bool {
        self.is_err()
    }
}

impl<E> ReturnedValue for Option<Result<crate::SteelVal, E>> {
    fn returned_value(&self) -> Option<&crate::SteelVal> {
        self.as_ref().and_then(|result| result.as_ref().ok())
    }
    fn is_error(&self) -> bool {
        matches!(self, Some(Err(_)))
    }
}

/// In debug builds the function macros call this on the result of every
/// function with an explicit `returns = "..."`: the declared type is a promise
/// the jit optimizes on, and this is what checks it, across the whole test
/// suite.
#[track_caller]
pub fn check_return<R: ReturnedValue>(returned: &R, declared: ValueType, name: &str) {
    if let Some(value) = returned.returned_value() {
        assert!(
            declared.matches(value),
            "`{name}` is declared to return {declared:?}, but returned {value}"
        );
    }
}

/// In debug builds the function macros call this around every function with
/// an explicit `args = "..."` or `raises = "..."`: `before` is
/// [`ArgTypes::check`] of the arguments as they were passed in (the function
/// may move out of them). Checks the promises [`ArgTypes`] and [`Raises`] make.
#[track_caller]
pub fn check_call<R: ReturnedValue>(returned: &R, before: &ArgMatch, raises: Raises, name: &str) {
    if returned.returned_value().is_some() {
        if let ArgMatch::BadType {
            index,
            declared,
            found,
        } = before
        {
            panic!(
                "`{name}` declares argument {index} as {declared:?}, but returned normally \
                 when passed {found}"
            );
        }
    }
    if returned.is_error() {
        match raises {
            Raises::Maybe => {}
            Raises::BadArgs => assert!(
                *before != ArgMatch::Match,
                "`{name}` is declared to raise only on bad arguments, but raised with \
                 arguments of the declared types"
            ),
            Raises::Never => assert!(
                *before == ArgMatch::BadCount,
                "`{name}` is declared never to raise, but raised"
            ),
        }
    }
}
