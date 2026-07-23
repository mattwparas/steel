use crate::gc::Gc;
use crate::rvals::{Result, SteelValGeneric};
use crate::SteelVal;

use crate::core::utils::{arity_check, declare_const_ref_functions};

type Alloc = crate::gc::Global;

// Make a transducer actually contain an option to a rooted value, otherwise
// it is a source agnostic transformer on the (eventual) input
pub struct Transducer<A: crate::gc::Allocator + Clone + Send + Sync + 'static = Alloc> {
    // root: Gc<SteelVal>,
    pub ops: Vec<Transducers<A>>,
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Clone for Transducer<A> {
    fn clone(&self) -> Self {
        Transducer {
            ops: self.ops.clone(),
        }
    }
}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> PartialEq for Transducer<A> {
    fn eq(&self, other: &Self) -> bool {
        self.ops == other.ops
    }
}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::hash::Hash for Transducer<A> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.ops.hash(state);
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Transducer<A> {
    pub fn new() -> Self {
        Transducer { ops: Vec::new() }
    }

    pub fn append(&mut self, mut other: Self) {
        self.ops.append(&mut other.ops)
    }

    pub fn push(&mut self, t: Transducers<A>) {
        self.ops.push(t);
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Default for Transducer<A> {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Transducers<A: crate::gc::Allocator + Clone + Send + Sync + 'static = Alloc> {
    Map(SteelValGeneric<A>),          // function
    Filter(SteelValGeneric<A>),       // function
    Take(SteelValGeneric<A>),         // integer
    Drop(SteelValGeneric<A>),         // integer
    FlatMap(SteelValGeneric<A>),      // function
    Flatten,                          // Takes nothing
    Window(SteelValGeneric<A>),       // integer
    TakeWhile(SteelValGeneric<A>),    // function
    DropWhile(SteelValGeneric<A>),    // function
    Extend(SteelValGeneric<A>),       // Collection
    Cycle,                            // Continue forever
    Enumerating,                      // turns (a b c) into ((0 a) (1 b) (2 c))
    Zipping(SteelValGeneric<A>), // Combine with another iterator, either a Collection or a Transducer
    Interleaving(SteelValGeneric<A>), // Interleave with another interator, either a Collection or a Transducer

    // Optimized versions:
    // Map pair will automatically expand the list into a pair.
    MapPair(SteelValGeneric<A>),
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Clone for Transducers<A> {
    fn clone(&self) -> Self {
        match self {
            Self::Map(v) => Self::Map(v.clone()),
            Self::Filter(v) => Self::Filter(v.clone()),
            Self::Take(v) => Self::Take(v.clone()),
            Self::Drop(v) => Self::Drop(v.clone()),
            Self::FlatMap(v) => Self::FlatMap(v.clone()),
            Self::Flatten => Self::Flatten,
            Self::Window(v) => Self::Window(v.clone()),
            Self::TakeWhile(v) => Self::TakeWhile(v.clone()),
            Self::DropWhile(v) => Self::DropWhile(v.clone()),
            Self::Extend(v) => Self::Extend(v.clone()),
            Self::Cycle => Self::Cycle,
            Self::Enumerating => Self::Enumerating,
            Self::Zipping(v) => Self::Zipping(v.clone()),
            Self::Interleaving(v) => Self::Interleaving(v.clone()),
            Self::MapPair(v) => Self::MapPair(v.clone()),
        }
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> PartialEq for Transducers<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Map(a), Self::Map(b)) => a == b,
            (Self::Filter(a), Self::Filter(b)) => a == b,
            (Self::Take(a), Self::Take(b)) => a == b,
            (Self::Drop(a), Self::Drop(b)) => a == b,
            (Self::FlatMap(a), Self::FlatMap(b)) => a == b,
            (Self::Flatten, Self::Flatten) => true,
            (Self::Window(a), Self::Window(b)) => a == b,
            (Self::TakeWhile(a), Self::TakeWhile(b)) => a == b,
            (Self::DropWhile(a), Self::DropWhile(b)) => a == b,
            (Self::Extend(a), Self::Extend(b)) => a == b,
            (Self::Cycle, Self::Cycle) => true,
            (Self::Enumerating, Self::Enumerating) => true,
            (Self::Zipping(a), Self::Zipping(b)) => a == b,
            (Self::Interleaving(a), Self::Interleaving(b)) => a == b,
            (Self::MapPair(a), Self::MapPair(b)) => a == b,
            _ => false,
        }
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::hash::Hash for Transducers<A> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::Map(v)
            | Self::Filter(v)
            | Self::Take(v)
            | Self::Drop(v)
            | Self::FlatMap(v)
            | Self::Window(v)
            | Self::TakeWhile(v)
            | Self::DropWhile(v)
            | Self::Extend(v)
            | Self::Zipping(v)
            | Self::Interleaving(v)
            | Self::MapPair(v) => v.hash(state),
            Self::Flatten | Self::Cycle | Self::Enumerating => {}
        }
    }
}

// This should just describe how a sequence of values can be reduced
// assert that the function passed in has an arity of 2
// and the initival
pub struct ReducerFunc<A: crate::gc::Allocator + Clone + Send + Sync + 'static = Alloc> {
    pub(crate) initial_value: SteelValGeneric<A>,
    pub(crate) function: SteelValGeneric<A>,
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Clone for ReducerFunc<A> {
    fn clone(&self) -> Self {
        ReducerFunc {
            initial_value: self.initial_value.clone(),
            function: self.function.clone(),
        }
    }
}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::fmt::Debug for ReducerFunc<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ReducerFunc")
            .field("initial_value", &self.initial_value)
            .field("function", &self.function)
            .finish()
    }
}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> PartialEq for ReducerFunc<A> {
    fn eq(&self, other: &Self) -> bool {
        self.initial_value == other.initial_value && self.function == other.function
    }
}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Eq for ReducerFunc<A> {}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::hash::Hash for ReducerFunc<A> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.initial_value.hash(state);
        self.function.hash(state);
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> ReducerFunc<A> {
    fn new(initial_value: SteelValGeneric<A>, function: SteelValGeneric<A>) -> Self {
        ReducerFunc {
            initial_value,
            function,
        }
    }
}

// Defines how to collect a function
// defaults to the same input type?

pub enum Reducer<A: crate::gc::Allocator + Clone + Send + Sync + 'static = Alloc> {
    // Sum the sequence
    Sum,
    // Multiply the sequence
    Multiply,
    // Find the Max of the sequence
    Max,
    // Find the min of the sequence
    Min,
    // Count the elements in the sequence
    Count,
    // Give the nth elements
    Nth(usize),
    // Collect into a list
    List,
    // Collect into a vector
    Vector,
    // Collect into a hash map
    HashMap,
    // Collect into a hash set
    HashSet,
    // Collect into a string
    String,
    // Consumes the iterator, giving the last value
    Last,
    // For-each -> calls a function for each value in the sequence
    ForEach(SteelValGeneric<A>),
    // Collect according to the function
    Generic(ReducerFunc<A>),
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Clone for Reducer<A> {
    fn clone(&self) -> Self {
        match self {
            Self::Sum => Self::Sum,
            Self::Multiply => Self::Multiply,
            Self::Max => Self::Max,
            Self::Min => Self::Min,
            Self::Count => Self::Count,
            Self::Nth(n) => Self::Nth(*n),
            Self::List => Self::List,
            Self::Vector => Self::Vector,
            Self::HashMap => Self::HashMap,
            Self::HashSet => Self::HashSet,
            Self::String => Self::String,
            Self::Last => Self::Last,
            Self::ForEach(v) => Self::ForEach(v.clone()),
            Self::Generic(rf) => Self::Generic(rf.clone()),
        }
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::fmt::Debug for Reducer<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Sum => write!(f, "Sum"),
            Self::Multiply => write!(f, "Multiply"),
            Self::Max => write!(f, "Max"),
            Self::Min => write!(f, "Min"),
            Self::Count => write!(f, "Count"),
            Self::Nth(n) => write!(f, "Nth({n})"),
            Self::List => write!(f, "List"),
            Self::Vector => write!(f, "Vector"),
            Self::HashMap => write!(f, "HashMap"),
            Self::HashSet => write!(f, "HashSet"),
            Self::String => write!(f, "String"),
            Self::Last => write!(f, "Last"),
            Self::ForEach(v) => write!(f, "ForEach({v:?})"),
            Self::Generic(rf) => write!(f, "Generic({rf:?})"),
        }
    }
}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> PartialEq for Reducer<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Sum, Self::Sum)
            | (Self::Multiply, Self::Multiply)
            | (Self::Max, Self::Max)
            | (Self::Min, Self::Min)
            | (Self::Count, Self::Count)
            | (Self::List, Self::List)
            | (Self::Vector, Self::Vector)
            | (Self::HashMap, Self::HashMap)
            | (Self::HashSet, Self::HashSet)
            | (Self::String, Self::String)
            | (Self::Last, Self::Last) => true,
            (Self::Nth(a), Self::Nth(b)) => a == b,
            (Self::ForEach(a), Self::ForEach(b)) => a == b,
            (Self::Generic(a), Self::Generic(b)) => a == b,
            _ => false,
        }
    }
}
impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> Eq for Reducer<A> {}

impl<A: crate::gc::Allocator + Clone + Send + Sync + 'static> core::hash::Hash for Reducer<A> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::Nth(n) => n.hash(state),
            Self::ForEach(v) => v.hash(state),
            Self::Generic(rf) => rf.hash(state),
            _ => {}
        }
    }
}

macro_rules! into_collection {
    ($($name:tt => $collection:tt),* $(,)? ) => {
        $ (
            fn $name(args: &[SteelVal]) -> Result<SteelVal> {
                arity_check!($name, args, 0);
                Ok(SteelVal::ReducerV(Gc::new(Reducer::$collection)))
            }
        ) *
    }
}

declare_const_ref_functions! {
    INTO_SUM => into_sum,
    INTO_PRODUCT => into_multiply,
    INTO_MAX => into_max,
    INTO_MIN => into_min,
    INTO_COUNT => into_count,
    INTO_LIST => into_list,
    INTO_VECTOR => into_vector,
    INTO_HASHMAP => into_hashmap,
    INTO_HASHSET => into_hashset,
    INTO_STRING => into_string,
    INTO_LAST => into_last,
    FOR_EACH => for_each,
    REDUCER => generic,
    NTH => nth,
}

into_collection! {
    into_sum => Sum,
    into_multiply => Multiply,
    into_max => Max,
    into_min => Min,
    into_count => Count,
    into_list => List,
    into_vector => Vector,
    into_hashmap => HashMap,
    into_hashset => HashSet,
    into_string => String,
    into_last => Last
}

fn for_each(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(for_each, args, 1);
    let function = args[0].clone();
    Ok(SteelVal::ReducerV(Gc::new(Reducer::ForEach(function))))
}

fn generic(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(reducer, args, 2);
    let function = args[0].clone();
    let initial_value = args[1].clone();
    Ok(SteelVal::ReducerV(Gc::new(Reducer::Generic(
        ReducerFunc::new(initial_value, function),
    ))))
}

fn nth(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(nth, args, 1);

    let number = args[0].clone();

    if let SteelVal::IntV(n) = number {
        if n < 0 {
            stop!(TypeMismatch => format!("nth expected a (postive) integer, found: {number}"));
        }
        Ok(SteelVal::ReducerV(Gc::new(Reducer::Nth(n as usize))))
    } else {
        stop!(TypeMismatch => format!("nth expected a (postive) integer, found: {number}"))
    }
}

// Reducer functions
