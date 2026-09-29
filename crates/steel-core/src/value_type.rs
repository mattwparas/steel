//! What kind of value something is, as far as optimizations are concerned.
//!
//! Native function definitions carry their return type as one of these (filled
//! in by the `steel_derive` function macros), and the jit turns it into its own
//! `InferredType`, which also tracks whether a value is unboxed in a register.
//!
//! The optimizer relies on these types, so a type has to hold for every value
//! the function can return. [`ValueType::Int`] means a fixnum. A function that
//! can return a bignum is [`ValueType::Number`].

use crate::gc::shared::ShareableMut;

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
    /// A value of a host type (`SteelVal::Custom`), by the type's name.
    Custom(CustomTypeName),
}

/// The name of a host type: `core::any::type_name` of it, which is also what
/// `CustomType::name` reports for its values.
#[derive(Clone, Copy)]
pub enum CustomTypeName {
    /// `type_name::<T>`, usable in a constant.
    Of(fn() -> &'static str),
    /// A name read from data, like a signature file.
    Named(&'static str),
}

impl CustomTypeName {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Of(name) => name(),
            Self::Named(name) => name,
        }
    }

    /// `name` as a [`CustomTypeName`]. Each distinct name is stored once.
    pub fn named(name: &str) -> CustomTypeName {
        CustomTypeName::Named(CUSTOM_TYPES.lock().name(name))
    }

    /// The name without its module path: `Counter` for `app::state::Counter`,
    /// `Vec<Counter>` for `alloc::vec::Vec<app::Counter>`.
    pub fn short(&self) -> String {
        let name = self.name();
        let mut out = String::new();
        let mut segment = String::new();
        for c in name.chars() {
            if c.is_alphanumeric() || c == '_' || c == ':' {
                segment.push(c);
            } else {
                out.push_str(segment.rsplit("::").next().unwrap_or(""));
                segment.clear();
                out.push(c);
            }
        }
        out.push_str(segment.rsplit("::").next().unwrap_or(""));
        out
    }
}

impl PartialEq for CustomTypeName {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Eq for CustomTypeName {}

impl core::hash::Hash for CustomTypeName {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.name().hash(state)
    }
}

impl PartialOrd for CustomTypeName {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CustomTypeName {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.name().cmp(other.name())
    }
}

impl core::fmt::Debug for CustomTypeName {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(self.name())
    }
}

impl ValueType {
    /// Every variant but [`ValueType::Custom`].
    pub const ALL: [ValueType; 18] = [
        ValueType::Any,
        ValueType::Bool,
        ValueType::Int,
        ValueType::Float,
        ValueType::Number,
        ValueType::Char,
        ValueType::String,
        ValueType::Symbol,
        ValueType::List,
        ValueType::Pair,
        ValueType::Vector,
        ValueType::MutableVector,
        ValueType::HashMap,
        ValueType::HashSet,
        ValueType::ByteVector,
        ValueType::Void,
        ValueType::FlatVector,
        ValueType::Procedure,
    ];

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
            ValueType::Custom(name) => {
                matches!(value, Custom(custom) if custom.read().name() == name.name())
            }
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
            Self::Custom(name) => name.name(),
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
    /// Anything.
    pub const ANY: ArgType = ArgType(&[ValueType::Any]);

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
            match ty {
                ValueType::Custom(name) => f.write_str(&name.short())?,
                ty => f.write_str(ty.name())?,
            }
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
/// [`ValueType`] has no name for: the struct types a program
/// defines (each its own kind, see [`TypeSet::structure`]), and `OTHER` for
/// everything else (ports, structs of types the analysis did not see, custom
/// types, ...).
///
/// The builtin kinds are a fixed set of bits. A program can define any number
/// of struct types, so those are an interned set of ids instead.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeSet {
    base: u32,
    nominals: NominalSet,
}

impl TypeSet {
    pub const BOTTOM: TypeSet = TypeSet::base(0);
    pub const BOOL: TypeSet = TypeSet::base(1 << 0);
    /// A fixnum.
    pub const INT: TypeSet = TypeSet::base(1 << 1);
    pub const FLOAT: TypeSet = TypeSet::base(1 << 2);
    /// A bignum, rational, big rational or complex number.
    pub const OTHER_NUMBER: TypeSet = TypeSet::base(1 << 3);
    pub const CHAR: TypeSet = TypeSet::base(1 << 4);
    pub const STRING: TypeSet = TypeSet::base(1 << 5);
    pub const SYMBOL: TypeSet = TypeSet::base(1 << 6);
    /// A proper list, `'()` included.
    pub const LIST: TypeSet = TypeSet::base(1 << 7);
    /// An improper pair.
    pub const PAIR: TypeSet = TypeSet::base(1 << 8);
    pub const VECTOR: TypeSet = TypeSet::base(1 << 9);
    pub const MUTABLE_VECTOR: TypeSet = TypeSet::base(1 << 10);
    pub const HASHMAP: TypeSet = TypeSet::base(1 << 11);
    pub const HASHSET: TypeSet = TypeSet::base(1 << 12);
    pub const BYTEVECTOR: TypeSet = TypeSet::base(1 << 13);
    pub const VOID: TypeSet = TypeSet::base(1 << 14);
    /// A closure, native function or continuation.
    pub const PROCEDURE: TypeSet = TypeSet::base(1 << 15);
    /// Anything else.
    pub const OTHER: TypeSet = TypeSet::base(1 << 16);

    pub const NUMBER: TypeSet =
        TypeSet::base(Self::INT.base | Self::FLOAT.base | Self::OTHER_NUMBER.base);
    /// Every kind, struct types included.
    pub const TOP: TypeSet = TypeSet {
        base: BASE_BITS,
        nominals: NominalSet::ALL,
    };
    /// Every struct type kind.
    pub const STRUCTS: TypeSet = TypeSet {
        base: 0,
        nominals: NominalSet::ALL,
    };

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

    const fn base(bits: u32) -> TypeSet {
        TypeSet {
            base: bits,
            nominals: NominalSet::NONE,
        }
    }

    /// The union of builtin kinds, usable in constants.
    pub const fn with(self, other: TypeSet) -> TypeSet {
        TypeSet {
            base: self.base | other.base,
            nominals: self.nominals,
        }
    }

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
            ValueType::Custom(name) => Self::custom(name.name()),
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
            Custom(custom) => Self::custom(custom.read().name()),
            _ => Self::OTHER,
        }
    }

    /// The kind of the program's `index`th struct type.
    pub fn structure(index: usize) -> TypeSet {
        TypeSet {
            base: 0,
            nominals: NominalSet::of(vec![index as u32], false),
        }
    }

    /// Whether the program's `index`th struct type is in this set.
    pub fn has_struct(self, index: usize) -> bool {
        self.nominals.has(index as u32)
    }

    /// The kind of the host type named `name` (see [`CustomTypeName`]).
    pub fn custom(name: &str) -> TypeSet {
        let id = CUSTOM_TYPES.lock().id(name);
        TypeSet {
            base: 0,
            nominals: NominalSet::of(vec![CUSTOM | id], false),
        }
    }

    /// Whether `value` is of a kind in this set.
    pub fn matches(self, value: &crate::SteelVal) -> bool {
        self.contains(Self::of(value))
    }

    /// The raw bits, e.g. to pass the set to scheme code. Only meaningful in
    /// this process.
    pub fn bits(self) -> u64 {
        (self.nominals.0 as u64) << 32 | self.base as u64
    }

    pub fn from_bits(bits: u64) -> TypeSet {
        TypeSet {
            base: bits as u32,
            nominals: NominalSet((bits >> 32) as u32),
        }
    }

    pub fn union(self, other: TypeSet) -> TypeSet {
        TypeSet {
            base: self.base | other.base,
            nominals: self.nominals.union(other.nominals),
        }
    }

    pub fn intersect(self, other: TypeSet) -> TypeSet {
        TypeSet {
            base: self.base & other.base,
            nominals: self.nominals.intersect(other.nominals),
        }
    }

    pub fn without(self, other: TypeSet) -> TypeSet {
        TypeSet {
            base: self.base & !other.base,
            nominals: self.nominals.without(other.nominals),
        }
    }

    pub fn is_bottom(self) -> bool {
        self.base == 0 && self.nominals == NominalSet::NONE
    }

    pub fn is_top(self) -> bool {
        self == Self::TOP
    }

    /// Whether every kind in `other` is in this set.
    pub fn contains(self, other: TypeSet) -> bool {
        self.base & other.base == other.base && self.nominals.contains(other.nominals)
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
        let listed = self.nominals.members();
        // Struct types past the program's own and host types never named in
        // it are never produced on their own, only as part of `any`, so they
        // only count as a group.
        let complemented = self.nominals.complemented();
        if complemented {
            let mut missing: Vec<u32> = (0..structs.len() as u32)
                .filter(|index| !self.nominals.has(*index))
                .collect();
            missing.extend(listed.iter().filter(|id| **id & CUSTOM != 0));
            let missing_base = BASE_BITS & !self.base;
            if missing_base.count_ones() as usize + missing.len() <= 3 {
                // Mostly everything, so say what it is not.
                let missing = TypeSet {
                    base: missing_base,
                    nominals: NominalSet::of(missing, false),
                };
                return format!("any except {}", missing.show(structs));
            }
        }
        let mut rest = TypeSet::base(self.base);
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
        for (index, name) in structs.iter().enumerate() {
            if self.has_struct(index) && !parts.contains(name) {
                parts.push(name.clone());
            }
        }
        if !complemented {
            for id in listed.iter().filter(|id| **id & CUSTOM != 0) {
                let name = CUSTOM_TYPES.lock().names[(id & !CUSTOM) as usize];
                parts.push(CustomTypeName::Named(name).short());
            }
        }
        let unnamed_struct = listed
            .iter()
            .any(|id| id & CUSTOM == 0 && *id as usize >= structs.len());
        if complemented || unnamed_struct {
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

/// Every builtin kind.
const BASE_BITS: u32 = (1 << 17) - 1;

/// Tags the nominal id of a host type. Struct ids are a program's own indices.
const CUSTOM: u32 = 1 << 30;

/// The host types named so far, by name. A name is kept for as long as the
/// process runs, so each is stored once.
static CUSTOM_TYPES: once_cell::sync::Lazy<parking_lot::Mutex<CustomTypes>> =
    once_cell::sync::Lazy::new(Default::default);

#[derive(Default)]
struct CustomTypes {
    ids: rustc_hash::FxHashMap<&'static str, u32>,
    names: Vec<&'static str>,
}

impl CustomTypes {
    fn id(&mut self, name: &str) -> u32 {
        if let Some(id) = self.ids.get(name) {
            return *id;
        }
        let id = self.names.len() as u32;
        assert!(id < CUSTOM, "too many host types");
        let name: &'static str = Box::leak(name.to_string().into_boxed_str());
        self.names.push(name);
        self.ids.insert(name, id);
        id
    }

    fn name(&mut self, name: &str) -> &'static str {
        let id = self.id(name);
        self.names[id as usize]
    }
}

/// A set of nominal type ids (struct types and host types), interned so a
/// [`TypeSet`] stays small and `Copy` and equal sets compare equal. The low
/// bits index [`NOMINAL_SETS`]. The high bit marks the complement: every
/// nominal type except those listed, which is what `any` and the else branch
/// of a struct predicate need.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct NominalSet(u32);

const COMPLEMENT: u32 = 1 << 31;

/// Every set of nominal ids ever made, each once, sorted. Struct ids are a
/// program's own indices, which start at 0, so the table stays small.
static NOMINAL_SETS: once_cell::sync::Lazy<parking_lot::Mutex<NominalSets>> =
    once_cell::sync::Lazy::new(|| {
        let empty: alloc::sync::Arc<[u32]> = alloc::sync::Arc::from(Vec::new());
        let mut ids = rustc_hash::FxHashMap::default();
        ids.insert(empty.clone(), 0);
        parking_lot::Mutex::new(NominalSets {
            sets: vec![empty],
            ids,
        })
    });

struct NominalSets {
    sets: Vec<alloc::sync::Arc<[u32]>>,
    ids: rustc_hash::FxHashMap<alloc::sync::Arc<[u32]>, u32>,
}

impl NominalSets {
    fn intern(&mut self, members: Vec<u32>) -> u32 {
        if let Some(id) = self.ids.get(members.as_slice()) {
            return *id;
        }
        let id = self.sets.len() as u32;
        assert!(id < COMPLEMENT, "too many distinct sets of struct types");
        let members: alloc::sync::Arc<[u32]> = alloc::sync::Arc::from(members);
        self.sets.push(members.clone());
        self.ids.insert(members, id);
        id
    }
}

impl NominalSet {
    const NONE: NominalSet = NominalSet(0);
    const ALL: NominalSet = NominalSet(COMPLEMENT);

    /// `members`, or every struct type but them.
    fn of(mut members: Vec<u32>, complemented: bool) -> NominalSet {
        members.sort_unstable();
        members.dedup();
        let id = NOMINAL_SETS.lock().intern(members);
        NominalSet(if complemented { id | COMPLEMENT } else { id })
    }

    fn complemented(self) -> bool {
        self.0 & COMPLEMENT != 0
    }

    fn complement(self) -> NominalSet {
        NominalSet(self.0 ^ COMPLEMENT)
    }

    /// The ids listed, which are excluded when complemented.
    fn members(self) -> alloc::sync::Arc<[u32]> {
        NOMINAL_SETS.lock().sets[(self.0 & !COMPLEMENT) as usize].clone()
    }

    fn has(self, index: u32) -> bool {
        if self == NominalSet::NONE {
            return false;
        }
        if self == NominalSet::ALL {
            return true;
        }
        self.members().binary_search(&index).is_ok() != self.complemented()
    }

    fn union(self, other: NominalSet) -> NominalSet {
        if self == other || other == NominalSet::NONE || self == NominalSet::ALL {
            return self;
        }
        if self == NominalSet::NONE || other == NominalSet::ALL {
            return other;
        }
        let mut table = NOMINAL_SETS.lock();
        let a = table.sets[(self.0 & !COMPLEMENT) as usize].clone();
        let b = table.sets[(other.0 & !COMPLEMENT) as usize].clone();
        let (members, complemented) = match (self.complemented(), other.complemented()) {
            (false, false) => (merge(&a, &b, |x, y| x || y), false),
            // Not A, or B: not (A without B).
            (true, false) => (merge(&a, &b, |x, y| x && !y), true),
            (false, true) => (merge(&a, &b, |x, y| y && !x), true),
            // Not A, or not B: not (A and B).
            (true, true) => (merge(&a, &b, |x, y| x && y), true),
        };
        let id = table.intern(members);
        NominalSet(if complemented { id | COMPLEMENT } else { id })
    }

    fn intersect(self, other: NominalSet) -> NominalSet {
        self.complement().union(other.complement()).complement()
    }

    fn without(self, other: NominalSet) -> NominalSet {
        self.intersect(other.complement())
    }

    fn contains(self, other: NominalSet) -> bool {
        other.without(self) == NominalSet::NONE
    }
}

/// The ids of two sorted lists for which `keep(in a, in b)` holds, sorted.
fn merge(a: &[u32], b: &[u32], keep: impl Fn(bool, bool) -> bool) -> Vec<u32> {
    let mut out = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() || j < b.len() {
        let (id, in_a, in_b) = match (a.get(i), b.get(j)) {
            (Some(x), Some(y)) if x == y => (*x, true, true),
            (Some(x), Some(y)) if x < y => (*x, true, false),
            (Some(x), None) => (*x, true, false),
            (_, Some(y)) => (*y, false, true),
            (None, None) => unreachable!(),
        };
        i += in_a as usize;
        j += in_b as usize;
        if keep(in_a, in_b) {
            out.push(id);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A set of struct ids as the finite ids it holds out of 0..8, with ids 8
    /// and up standing in for every other struct type.
    fn model(set: TypeSet) -> [bool; 12] {
        core::array::from_fn(|i| set.has_struct(i))
    }

    #[test]
    fn struct_sets_behave_like_sets() {
        let mut seed = 0x2545f4914f6cdd1du64;
        let mut next = move || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            seed
        };
        let mut random = |next: &mut dyn FnMut() -> u64| {
            let members: Vec<u32> = (0..8).filter(|_| next() % 3 == 0).collect();
            let set = members.iter().fold(TypeSet::BOTTOM, |s, i| {
                s.union(TypeSet::structure(*i as usize))
            });
            if next() % 2 == 0 {
                TypeSet::STRUCTS.without(set)
            } else {
                set
            }
        };
        for _ in 0..2000 {
            let (a, b) = (random(&mut next), random(&mut next));
            let (ma, mb) = (model(a), model(b));
            let each = |f: fn(bool, bool) -> bool| -> [bool; 12] {
                core::array::from_fn(|i| f(ma[i], mb[i]))
            };
            assert_eq!(model(a.union(b)), each(|x, y| x || y));
            assert_eq!(model(a.intersect(b)), each(|x, y| x && y));
            assert_eq!(model(a.without(b)), each(|x, y| x && !y));
            assert_eq!(a.contains(b), (0..12).all(|i| !mb[i] || ma[i]));
            // Equal sets are the same value.
            assert_eq!(a.union(b), b.union(a));
            assert_eq!(a.union(b).without(b), a.without(b));
        }
        assert!(TypeSet::TOP.contains(TypeSet::structure(1_000_000)));
        assert!(TypeSet::structure(0)
            .without(TypeSet::structure(0))
            .is_bottom());
    }
}
