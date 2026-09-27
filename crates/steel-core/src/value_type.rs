//! What kind of value something is, as far as optimizations are concerned.
//!
//! Native function definitions carry their return type as one of these (filled
//! in by the `steel_derive` function macros), and the jit turns it into its own
//! `InferredType`, which also tracks whether a value is unboxed in a register.
//!
//! The optimizer relies on these types, so a type has to hold for every value
//! the function can return. [`ValueType::Int`] means a fixnum. A function that
//! can return a bignum is [`ValueType::Number`].

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum ValueType {
    /// Nothing known.
    #[default]
    Any,
    Bool,
    /// A fixnum (`SteelVal::IntV`). Never a bignum.
    Int,
    /// A float (`SteelVal::NumV`).
    Float,
    /// Any number, whether fixnum, bignum, float, rational or complex.
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
    /// A flat vector (`SteelVal::FlatVector`).
    FlatVector,
    /// A closure, native function or continuation. Not a struct with a
    /// procedure property.
    Procedure,
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
            ValueType::FlatVector => matches!(value, FlatVector(_)),
            ValueType::Procedure => matches!(
                value,
                Closure(_)
                    | FuncV(_)
                    | MutFunc(_)
                    | BuiltIn(_)
                    | BoxedFunction(_)
                    | ContinuationFunction(_)
            ),
        }
    }

    /// The name used for this type in messages, like `mutable-vector`.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Any => "any",
            Self::Bool => "bool",
            Self::Int => "int",
            Self::Float => "float",
            Self::Number => "number",
            Self::Char => "char",
            Self::String => "string",
            Self::Symbol => "symbol",
            Self::List => "list",
            Self::Pair => "pair",
            Self::Vector => "vector",
            Self::MutableVector => "mutable-vector",
            Self::HashMap => "hashmap",
            Self::HashSet => "hashset",
            Self::ByteVector => "bytevector",
            Self::Void => "void",
            Self::FlatVector => "flat-vector",
            Self::Procedure => "procedure",
        }
    }
}

/// The argument types of a native function, from the macros' `args(..)`.
///
/// If a call with the right number of arguments returns normally, argument `i`
/// had type `fixed[i]`, and every argument past the fixed ones had type `rest`.
/// An analysis can use this to learn the type of a variable from a call it was
/// passed to. It says nothing about which values are accepted; see [`Raises`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ArgTypes {
    pub fixed: &'static [ArgType],
    /// The type of every argument past `fixed`, for variadic functions (and
    /// the optional arguments of `Range`/`AtMost` ones).
    pub rest: Option<ArgType>,
}

/// One argument's declared type: a value of any of these types. Written
/// `List | Pair` in `args(..)`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ArgType(pub &'static [ValueType]);

impl ArgType {
    pub fn matches(&self, value: &crate::SteelVal) -> bool {
        self.0.iter().any(|ty| ty.matches(value))
    }

    /// The type, when it is a single one and not a union.
    pub fn single(&self) -> Option<ValueType> {
        match self.0 {
            [ty] => Some(*ty),
            _ => None,
        }
    }
}

impl core::fmt::Display for ArgType {
    /// Like `list|pair`.
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for (i, ty) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str("|")?;
            }
            f.write_str(ty.name())?;
        }
        Ok(())
    }
}

impl core::fmt::Debug for ArgType {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for (i, ty) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str("|")?;
            }
            write!(f, "{ty:?}")?;
        }
        Ok(())
    }
}

impl ArgTypes {
    /// The declared type of argument `index`, if there is one.
    pub fn get(&self, index: usize) -> Option<ArgType> {
        self.fixed.get(index).copied().or(self.rest)
    }

    /// How `args` compare with the declaration.
    pub fn check(&self, args: &[crate::SteelVal]) -> ArgMatch {
        if args.len() < self.fixed.len() || (self.rest.is_none() && args.len() > self.fixed.len()) {
            return ArgMatch::BadCount;
        }
        for (index, arg) in args.iter().enumerate() {
            let Some(declared) = self.get(index) else {
                continue;
            };
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
        declared: ArgType,
        found: String,
    },
}

/// When a native function can raise an error, from the macros' `raises = ..`.
/// Calls with the wrong number of arguments always may.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum Raises {
    /// Nothing known.
    #[default]
    Maybe,
    /// Only when some argument is not of its declared type (see [`ArgTypes`]).
    /// Once the argument types are known, the call cannot fail.
    BadArgs,
    /// Never.
    Never,
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
/// function with an explicit `returns = ..`, to check the declared type the
/// jit optimizes on.
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
/// an explicit `args(..)` or `raises = ..`, to check the promises
/// [`ArgTypes`] and [`Raises`] make. `before` is [`ArgTypes::check`] of the
/// arguments as they were passed in (the function may move out of them).
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

/// A set of possible kinds of value, which the type inference pass
/// (`compiler::passes::types`) computes for an expression. Unlike [`ValueType`]
/// it is a union, so `int | string` is expressible. It also adds kinds
/// [`ValueType`] has no name for: procedures, the struct types a program
/// defines (each its own kind, see [`TypeSet::structure`]), and `OTHER` for
/// everything else (ports, structs of types the analysis did not see, custom
/// types, ...).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeSet(u64);

impl TypeSet {
    pub const BOTTOM: TypeSet = TypeSet(0);
    pub const BOOL: TypeSet = TypeSet(1 << 0);
    /// A fixnum.
    pub const INT: TypeSet = TypeSet(1 << 1);
    pub const FLOAT: TypeSet = TypeSet(1 << 2);
    /// A bignum, rational, big rational or complex number.
    pub const OTHER_NUMBER: TypeSet = TypeSet(1 << 3);
    pub const CHAR: TypeSet = TypeSet(1 << 4);
    pub const STRING: TypeSet = TypeSet(1 << 5);
    pub const SYMBOL: TypeSet = TypeSet(1 << 6);
    /// A proper list, `'()` included.
    pub const LIST: TypeSet = TypeSet(1 << 7);
    /// An improper pair.
    pub const PAIR: TypeSet = TypeSet(1 << 8);
    pub const VECTOR: TypeSet = TypeSet(1 << 9);
    pub const MUTABLE_VECTOR: TypeSet = TypeSet(1 << 10);
    pub const HASHMAP: TypeSet = TypeSet(1 << 11);
    pub const HASHSET: TypeSet = TypeSet(1 << 12);
    pub const BYTEVECTOR: TypeSet = TypeSet(1 << 13);
    pub const VOID: TypeSet = TypeSet(1 << 14);
    /// A closure or a native function.
    pub const PROCEDURE: TypeSet = TypeSet(1 << 15);
    /// Anything else.
    pub const OTHER: TypeSet = TypeSet(1 << 16);

    pub const NUMBER: TypeSet = TypeSet(Self::INT.0 | Self::FLOAT.0 | Self::OTHER_NUMBER.0);
    /// Every kind, struct types included.
    pub const TOP: TypeSet = TypeSet(u64::MAX);
    /// The kinds other than struct types.
    const BASE: TypeSet = TypeSet((1 << 17) - 1);
    /// Every struct type kind.
    pub const STRUCTS: TypeSet = TypeSet(!Self::BASE.0);
    /// How many struct types of a program get kinds of their own.
    pub const MAX_STRUCTS: usize = 64 - 17;

    const NAMES: [(TypeSet, &'static str); 17] = [
        (Self::BOOL, "bool"),
        (Self::INT, "int"),
        (Self::FLOAT, "float"),
        (Self::OTHER_NUMBER, "number"),
        (Self::CHAR, "char"),
        (Self::STRING, "string"),
        (Self::SYMBOL, "symbol"),
        (Self::LIST, "list"),
        (Self::PAIR, "pair"),
        (Self::VECTOR, "vector"),
        (Self::MUTABLE_VECTOR, "mutable-vector"),
        (Self::HASHMAP, "hashmap"),
        (Self::HASHSET, "hashset"),
        (Self::BYTEVECTOR, "bytevector"),
        (Self::VOID, "void"),
        (Self::PROCEDURE, "procedure"),
        (Self::OTHER, "other"),
    ];

    pub fn from_value_type(ty: ValueType) -> TypeSet {
        match ty {
            ValueType::Any => Self::TOP,
            ValueType::Bool => Self::BOOL,
            ValueType::Int => Self::INT,
            ValueType::Float => Self::FLOAT,
            ValueType::Number => Self::NUMBER,
            ValueType::Char => Self::CHAR,
            ValueType::String => Self::STRING,
            ValueType::Symbol => Self::SYMBOL,
            ValueType::List => Self::LIST,
            ValueType::Pair => Self::PAIR,
            ValueType::Vector => Self::VECTOR,
            ValueType::MutableVector => Self::MUTABLE_VECTOR,
            ValueType::HashMap => Self::HASHMAP,
            ValueType::HashSet => Self::HASHSET,
            ValueType::ByteVector => Self::BYTEVECTOR,
            ValueType::Void => Self::VOID,
            // Flat vectors have no kind of their own.
            ValueType::FlatVector => Self::OTHER,
            ValueType::Procedure => Self::PROCEDURE,
        }
    }

    /// Every kind any of the union's types can be.
    pub fn from_arg_type(ty: ArgType) -> TypeSet {
        ty.0.iter().fold(Self::BOTTOM, |set, ty| {
            set.union(Self::from_value_type(*ty))
        })
    }

    /// The narrowest [`ValueType`] covering every kind in this set.
    pub fn to_value_type(self) -> ValueType {
        const EXACT: [(TypeSet, ValueType); 14] = [
            (TypeSet::BOOL, ValueType::Bool),
            (TypeSet::INT, ValueType::Int),
            (TypeSet::FLOAT, ValueType::Float),
            (TypeSet::CHAR, ValueType::Char),
            (TypeSet::STRING, ValueType::String),
            (TypeSet::SYMBOL, ValueType::Symbol),
            (TypeSet::LIST, ValueType::List),
            (TypeSet::PAIR, ValueType::Pair),
            (TypeSet::VECTOR, ValueType::Vector),
            (TypeSet::MUTABLE_VECTOR, ValueType::MutableVector),
            (TypeSet::HASHMAP, ValueType::HashMap),
            (TypeSet::HASHSET, ValueType::HashSet),
            (TypeSet::BYTEVECTOR, ValueType::ByteVector),
            (TypeSet::VOID, ValueType::Void),
        ];
        if let Some((_, ty)) = EXACT.iter().find(|(set, _)| *set == self) {
            return *ty;
        }
        if !self.is_bottom() && Self::NUMBER.contains(self) {
            return ValueType::Number;
        }
        ValueType::Any
    }

    /// The kind of `value`, as a single-kind set.
    pub fn of(value: &crate::SteelVal) -> TypeSet {
        use crate::SteelVal::*;
        match value {
            BoolV(_) => Self::BOOL,
            IntV(_) => Self::INT,
            NumV(_) => Self::FLOAT,
            BigNum(_) | Rational(_) | BigRational(_) | Complex(_) => Self::OTHER_NUMBER,
            CharV(_) => Self::CHAR,
            StringV(_) => Self::STRING,
            SymbolV(_) => Self::SYMBOL,
            ListV(_) => Self::LIST,
            Pair(_) => Self::PAIR,
            VectorV(_) => Self::VECTOR,
            MutableVector(_) => Self::MUTABLE_VECTOR,
            HashMapV(_) => Self::HASHMAP,
            HashSetV(_) => Self::HASHSET,
            ByteVector(_) => Self::BYTEVECTOR,
            Void => Self::VOID,
            Closure(_)
            | FuncV(_)
            | MutFunc(_)
            | BuiltIn(_)
            | BoxedFunction(_)
            | ContinuationFunction(_) => Self::PROCEDURE,
            _ => Self::OTHER,
        }
    }

    /// The kind of the program's `index`th struct type, counting from 0 up to
    /// [`Self::MAX_STRUCTS`].
    pub fn structure(index: usize) -> TypeSet {
        assert!(index < Self::MAX_STRUCTS);
        TypeSet(1 << (17 + index))
    }

    /// The struct types in this set, by index.
    pub fn struct_indices(self) -> impl Iterator<Item = usize> {
        let structs = self.intersect(Self::STRUCTS).0 >> 17;
        (0..Self::MAX_STRUCTS).filter(move |i| structs & (1 << i) != 0)
    }

    /// Whether `value` is of a kind in this set.
    pub fn matches(self, value: &crate::SteelVal) -> bool {
        self.contains(Self::of(value))
    }

    /// The raw bits, e.g. to pass the set to scheme code.
    pub fn bits(self) -> u64 {
        self.0
    }

    pub fn from_bits(bits: u64) -> TypeSet {
        TypeSet(bits)
    }

    pub const fn union(self, other: TypeSet) -> TypeSet {
        TypeSet(self.0 | other.0)
    }

    pub const fn intersect(self, other: TypeSet) -> TypeSet {
        TypeSet(self.0 & other.0)
    }

    pub const fn without(self, other: TypeSet) -> TypeSet {
        TypeSet(self.0 & !other.0)
    }

    pub fn is_bottom(self) -> bool {
        self.0 == 0
    }

    pub fn is_top(self) -> bool {
        self == Self::TOP
    }

    /// Whether every kind in `other` is in this set.
    pub fn contains(self, other: TypeSet) -> bool {
        self.0 & other.0 == other.0
    }
}

impl TypeSet {
    /// `int`, `string | list`, `number` (every numeric kind), `any` (everything)
    /// or `never` (the expression cannot produce a value). Struct types are
    /// shown by their names in `structs`, the program's struct types by index.
    pub fn show(self, structs: &[String]) -> String {
        if self.is_top() {
            return "any".to_string();
        }
        if self.is_bottom() {
            return "never".to_string();
        }
        // Struct kinds past the program's own are never produced on their own,
        // only as part of `any`, so they only count as a group.
        let named = (0..structs.len().min(Self::MAX_STRUCTS))
            .fold(Self::BOTTOM, |set, index| set.union(Self::structure(index)));
        let unnamed = Self::STRUCTS.without(named);
        let has_unnamed = !unnamed.is_bottom() && self.contains(unnamed);
        if has_unnamed {
            let missing = Self::BASE.union(named).without(self);
            if missing.0.count_ones() <= 3 {
                // Mostly everything, so say what it is not.
                return format!("any except {}", missing.show(structs));
            }
        }
        let mut rest = self.without(Self::STRUCTS);
        let mut parts = Vec::new();
        if rest.contains(Self::NUMBER) {
            parts.push("number".to_string());
            rest = rest.without(Self::NUMBER);
        }
        for (kind, name) in Self::NAMES {
            if rest.contains(kind) {
                parts.push(if kind == Self::OTHER_NUMBER {
                    "bignum/rational/complex".to_string()
                } else {
                    name.to_string()
                });
            }
        }
        for index in self.intersect(named).struct_indices() {
            if !parts.contains(&structs[index]) {
                parts.push(structs[index].clone());
            }
        }
        if has_unnamed {
            parts.push("struct".to_string());
        }
        parts.join(" | ")
    }
}

impl core::fmt::Display for TypeSet {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(&self.show(&[]))
    }
}

impl core::fmt::Debug for TypeSet {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}
