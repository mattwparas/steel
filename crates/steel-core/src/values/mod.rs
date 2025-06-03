#[allow(dead_code)]
pub(crate) mod closed;
pub(crate) mod contracts;
pub(crate) mod functions;
pub(crate) mod json_vals;
pub(crate) mod lazy_stream;
pub(crate) mod lists;
pub(crate) mod port;
pub(crate) mod recycler;
pub(crate) mod structs;
pub(crate) mod transducers;

pub use functions::LambdaMetadataTable;

pub use closed::RootToken;
pub use closed::RootedSteelVal;
pub use port::SteelPortRepr;

pub use im_shims::{
    HashMap, HashMapConsumingIter, HashSet, HashSetConsumingIter, Vector, VectorConsumingIter,
};

#[cfg(not(feature = "sync"))]
mod im_shims {
    use std::hash::RandomState;

    pub type Vector<T> = im_rc::Vector<T>;
    pub type HashMap<K, V, S = RandomState> = im_rc::HashMap<K, V, S>;
    pub type HashSet<K, S = RandomState> = im_rc::HashSet<K, S>;

    pub type VectorConsumingIter<T> = im_rc::vector::ConsumingIter<T>;
    pub type HashSetConsumingIter<T> = im_rc::hashset::ConsumingIter<T>;
    pub type HashMapConsumingIter<K, V> = im_rc::hashmap::ConsumingIter<(K, V)>;
}

#[cfg(feature = "sync")]
mod im_shims {
    use std::hash::RandomState;

    pub type Vector<T> = im::Vector<T>;
    pub type HashMap<K, V, S = RandomState> = im::HashMap<K, V, S>;
    pub type HashSet<K, S = RandomState> = im::HashSet<K, S>;

    pub type VectorConsumingIter<T> = im::vector::ConsumingIter<T>;
    pub type HashSetConsumingIter<T> = im::hashset::ConsumingIter<T>;
    pub type HashMapConsumingIter<K, V> = im::hashmap::ConsumingIter<(K, V)>;
}
