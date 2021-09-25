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
