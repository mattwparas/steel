#![cfg_attr(not(feature = "std"), allow(unused_imports))]

use fxhash::FxBuildHasher;

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use core::hash::{BuildHasher, Hash};
#[cfg(not(feature = "std"))]
mod imp {
    use super::FxBuildHasher;
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

#[cfg(not(feature = "std"))]
pub trait HashMapExt<K, V> {
    fn update(&self, key: K, value: V) -> Self;
    fn union(&self, other: &Self) -> Self;
}

#[cfg(not(feature = "std"))]
impl<K, V, S> HashMapExt<K, V> for hashbrown::HashMap<K, V, S>
where
    K: Eq + Hash + Clone,
    V: Clone,
    S: BuildHasher + Clone + Default,
{
    fn update(&self, key: K, value: V) -> Self {
        let mut new_map = self.clone();
        new_map.insert(key, value);
        new_map
    }

    fn union(&self, other: &Self) -> Self {
        if other.is_empty() {
            return self.clone();
        }

        if self.is_empty() {
            return other.clone();
        }

        let mut new_map = self.clone();
        for (key, value) in other.iter() {
            new_map.insert(key.clone(), value.clone());
        }
        new_map
    }
}

pub type MutableHashMap<K, V> = hashbrown::HashMap<K, V, FxBuildHasher>;
pub type MutableHashSet<T> = hashbrown::HashSet<T, FxBuildHasher>;

pub type DrainHashSet<T> = hashbrown::HashSet<T, FxBuildHasher>;

#[cfg(not(feature = "std"))]
pub trait HashSetExt<T> {
    fn update(&self, value: T) -> Self;
    fn union(&self, other: &Self) -> Self;
    fn intersection(&self, other: &Self) -> Self;
    fn difference(&self, other: &Self) -> Self;
}

#[cfg(not(feature = "std"))]
impl<T, S> HashSetExt<T> for hashbrown::HashSet<T, S>
where
    T: Eq + Hash + Clone,
    S: BuildHasher + Clone + Default,
{
    fn update(&self, value: T) -> Self {
        let mut new_set = self.clone();
        new_set.insert(value);
        new_set
    }

    fn union(&self, other: &Self) -> Self {
        if other.is_empty() {
            return self.clone();
        }

        if self.is_empty() {
            return other.clone();
        }

        let mut new_set = self.clone();
        for value in other.iter() {
            new_set.insert(value.clone());
        }
        new_set
    }

    fn intersection(&self, other: &Self) -> Self {
        if self.is_empty() || other.is_empty() {
            return Self::default();
        }

        let (smaller, larger) = if self.len() <= other.len() {
            (self, other)
        } else {
            (other, self)
        };

        let mut new_set = Self::default();
        for value in smaller.iter() {
            if larger.contains(value) {
                new_set.insert(value.clone());
            }
        }
        new_set
    }

    fn difference(&self, other: &Self) -> Self {
        if self.is_empty() {
            return Self::default();
        }

        if other.is_empty() {
            return self.clone();
        }

        let mut new_set = Self::default();
        for value in self.iter() {
            if !other.contains(value) {
                new_set.insert(value.clone());
            }
        }
        new_set
    }
}

#[cfg(not(feature = "std"))]
pub use hashbrown::hash_map;
#[cfg(feature = "std")]
pub use std::collections::hash_map;
