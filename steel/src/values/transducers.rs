use crate::SteelVal;

pub enum CollectionType {
    List,
    Vector,
}

// Make a transducer actually contain an option to a rooted value, otherwise
// it is a source agnostic transformer on the (eventual) input
#[derive(Clone, PartialEq)]
pub struct Transducer {
    // root: Gc<SteelVal>,
    pub ops: Vec<Transducers>,
}

impl Transducer {
    pub fn new() -> Self {
        Transducer { ops: Vec::new() }
    }

    pub fn append(&mut self, mut other: Self) {
        self.ops.append(&mut other.ops)
    }

    pub fn push(&mut self, t: Transducers) {
        self.ops.push(t);
    }
}

#[derive(Clone, PartialEq)]
pub enum Transducers {
    Map(SteelVal),       // function
    Filter(SteelVal),    // function
    Take(SteelVal),      // integer
    Drop(SteelVal),      // integer
    FlatMap(SteelVal),   // function
    Flatten,             // Takes nothing
    Window(SteelVal),    // integer
    TakeWhile(SteelVal), // function
    DropWhile(SteelVal), // function
    Extend(SteelVal),    // Collection
}

// This should just describe how a sequence of values can be reduced
// assert that the function passed in has an arity of 2
// and the initival
pub struct ReducerFunc {
    initial_value: SteelVal,
    function: SteelVal,
}

pub enum Reducer {
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
    Nth,
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
    ForEach(SteelVal),
    // Collect according to the function
    Generic(ReducerFunc),
}

// How to collect into a type
// Have a reducer for every collection - optionally, otherwise it just defaults
// to the same type
// for-each is just transducing but without the collection
// pub enum Reducers {
//     // Typical function
//     Generic(SteelVal),
//     // Specific collection type
// }
