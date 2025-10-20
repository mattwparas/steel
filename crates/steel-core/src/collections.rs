#![cfg_attr(not(feature = "std"), allow(unused_imports))]

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use fxhash::FxBuildHasher;

#[cfg(not(feature = "std"))]
mod imp {
    use alloc::vec::Vec;

    pub type Vector<T> = Vec<T>;
    pub type HashMap<K, V, S = FxBuildHasher> = hashbrown::HashMap<K, V, S>;
    pub type HashSet<K, S = FxBuildHasher> = hashbrown::HashSet<K, S>;

    pub type VectorConsumingIter<T> = alloc::vec::IntoIter<T>;
    pub type HashSetConsumingIter<T> = hashbrown::hash_set::IntoIter<T>;
    pub type HashMapConsumingIter<K, V> = hashbrown::hash_map::IntoIter<K, V>;
}

#[cfg(all(feature = "std", not(feature = "sync")))]
mod imp {
    use std::hash::RandomState;

    pub type Vector<T> = im_rc::Vector<T>;
    pub type HashMap<K, V, S = RandomState> = im_rc::HashMap<K, V, S>;
    pub type HashSet<K, S = RandomState> = im_rc::HashSet<K, S>;

    pub type VectorConsumingIter<T> = im_rc::vector::ConsumingIter<T>;
    pub type HashSetConsumingIter<T> = im_rc::hashset::ConsumingIter<T>;
    pub type HashMapConsumingIter<K, V> = im_rc::hashmap::ConsumingIter<(K, V)>;
}

#[cfg(all(feature = "std", feature = "sync", not(feature = "imbl")))]
mod imp {
    use std::hash::RandomState;

    pub type Vector<T> = im::Vector<T>;
    pub type HashMap<K, V, S = RandomState> = im::HashMap<K, V, S>;
    pub type HashSet<K, S = RandomState> = im::HashSet<K, S>;

    pub type VectorConsumingIter<T> = im::vector::ConsumingIter<T>;
    pub type HashSetConsumingIter<T> = im::hashset::ConsumingIter<T>;
    pub type HashMapConsumingIter<K, V> = im::hashmap::ConsumingIter<(K, V)>;
}

#[cfg(all(feature = "std", feature = "sync", feature = "imbl"))]
mod imp {
    use std::hash::RandomState;

    use imbl::shared_ptr::DefaultSharedPtr;

    pub type Vector<T> = imbl::Vector<T>;
    pub type HashMap<K, V, S = RandomState> = imbl::GenericHashMap<K, V, S, DefaultSharedPtr>;
    pub type HashSet<K, S = RandomState> = imbl::GenericHashSet<K, S, DefaultSharedPtr>;

    pub type VectorConsumingIter<T> = imbl::vector::ConsumingIter<T, DefaultSharedPtr>;
    pub type HashSetConsumingIter<T> = imbl::hashset::ConsumingIter<T, DefaultSharedPtr>;
    pub type HashMapConsumingIter<K, V> = imbl::hashmap::ConsumingIter<(K, V), DefaultSharedPtr>;
}

pub use imp::{
    HashMap, HashMapConsumingIter, HashSet, HashSetConsumingIter, Vector, VectorConsumingIter,
};

pub type MutableHashMap<K, V> = hashbrown::HashMap<K, V, FxBuildHasher>;
pub type MutableHashSet<T> = hashbrown::HashSet<T, FxBuildHasher>;

pub type DrainHashSet<T> = hashbrown::HashSet<T, FxBuildHasher>;

#[cfg(not(feature = "std"))]
pub use hashbrown::hash_map;
#[cfg(feature = "std")]
pub use std::collections::hash_map;
