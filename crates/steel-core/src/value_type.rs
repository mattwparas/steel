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
    Void,
}

impl ValueType {
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
            "void" => Self::Void,
            _ => return None,
        })
    }
}
