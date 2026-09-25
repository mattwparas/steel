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

/// What a native function hands back, for `check_return`.
pub trait ReturnedValue {
    fn returned_value(&self) -> Option<&crate::SteelVal>;
}

impl ReturnedValue for crate::SteelVal {
    fn returned_value(&self) -> Option<&crate::SteelVal> {
        Some(self)
    }
}

impl<E> ReturnedValue for Result<crate::SteelVal, E> {
    fn returned_value(&self) -> Option<&crate::SteelVal> {
        self.as_ref().ok()
    }
}

impl<E> ReturnedValue for Option<Result<crate::SteelVal, E>> {
    fn returned_value(&self) -> Option<&crate::SteelVal> {
        self.as_ref().and_then(|result| result.as_ref().ok())
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
