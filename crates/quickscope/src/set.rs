use std::{collections::{hash_map::RandomState}, hash::BuildHasher, hash::Hash, borrow::Borrow};

use crate::ScopeMap;

/// A layered hash set for representing the scopes of variables.
#[derive(Clone)]
pub struct ScopeSet<T, S: BuildHasher = RandomState> {
  map: ScopeMap<T, (), S>
}

impl<T, S: Default + BuildHasher> Default for ScopeSet<T, S> {
  /// Creates a new `ScopeSet` with the default configuration.
  #[inline]
  fn default() -> Self {
    Self {
      map: Default::default()
    }
  }
}

impl<T> ScopeSet<T, RandomState> {
  /// Creates an empty `ScopeSet` with a default hasher and capacity. 
  #[inline]
  pub fn new() -> Self {
    Default::default()
  }

  /// Creates an empty `ScopeSet` with a default hasher and the specified capacity.
  #[inline]
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      map: ScopeMap::with_capacity(capacity)
    }
  }
}

impl<T, S: BuildHasher> ScopeSet<T, S> {
  /// Creates an empty `ScopeSet` with the specified hasher and a default capacity.
  #[inline]
  pub fn with_hasher(hash_builder: S) -> Self {
    Self {
      map: ScopeMap::with_hasher(hash_builder)
    }
  }

  /// Creates an empty `ScopeSet` with the specified capacity and hasher.
  #[inline]
  pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> Self {
    Self {
      map: ScopeMap::with_capacity_and_hasher(capacity, hash_builder)
    }
  }
}

impl<T, S: BuildHasher> ScopeSet<T, S> {

  /// Returns `true` is the set is empty.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.map.is_empty()
  }

  /// Gets the number of elements the set can hold without reallocating.
  #[inline]
  pub fn capacity(&self) -> usize {
    self.map.capacity()
  }

  /// Gets the number of unique keys in the set.
  #[inline]
  pub fn len(&self) -> usize {
    self.map.len()
  }

  /// Gets the number of layers in the set.
  #[inline]
  pub fn depth(&self) -> usize {
    self.map.depth()
  }

  /// Adds a new, empty layer.
  ///
  /// Computes in **O(1)** time.
  #[inline]
  pub fn push_layer(&mut self) {
    self.map.push_layer()
  }

  /// Removes the topmost layer (if it isn't the bottom layer) and all associated keys.
  /// Returns `true` if the layer was removed.
  ///
  /// Computes in **O(n)** time in relation to the number of keys stored in the removed layer.
  #[inline]
  pub fn pop_layer(&mut self) -> bool {
    self.map.pop_layer()
  }
}

impl<T: Eq + Hash, S: BuildHasher> ScopeSet<T, S> {
  /// Removes all entries and additional layers. 
  #[inline]
  pub fn clear_all(&mut self) {
    self.map.clear_all()
  }

  /// Removes all keys in the topmost layer.
  #[inline]
  pub fn clear_top(&mut self) {
    self.map.clear_top()
  }

  /// Adds the specified key to the topmost layer.
  #[inline]
  pub fn define(&mut self, key: T) {
    self.map.define(key, ());
  }

  /// Adds the specified key to the layer `min_depth` layers below the top layer. Saturates to base layer.
  #[inline]
  pub fn define_parent(&mut self, key: T, min_depth: usize) {
    self.map.define_parent(key, (), min_depth);
  }

  /// Removes the specified key from the topmost layer and returns it.
  #[inline]
  pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> bool
  where
    T: Borrow<Q>,
    Q: Eq + Hash,
  {
    self.map.remove(key).is_some()
  }

  /// Returns `true` if any layer contains the specified key.
  ///
  /// Computes in **O(1)** time.
  #[inline]
  pub fn contains<Q: ?Sized>(&self, key: &Q) -> bool
  where
    T: Borrow<Q>,
    Q: Eq + Hash,
  {
    self.map.contains_key(key)
  }

  /// Returns `true` if the topmost layer contains the specified key.
  //
  /// Computes in **O(1)** time.
  #[inline]
  pub fn contains_at_top<Q: ?Sized>(&self, key: &Q) -> bool 
  where
    T: Borrow<Q>,
    Q: Eq + Hash,
  {
    self.map.contains_key_at_top(key)
  }

  /// Gets the depth of the specified key (i.e. how many layers down from the top that the key first appears).
  /// A depth of 0 refers to the top layer.
  ///
  /// Returns `None` if the key does not exist.
  ///
  /// Computes in **O(n)** time (worst-case) in relation to layer count.
  #[inline]
  pub fn depth_of<Q: ?Sized>(&self, key: &Q) -> Option<usize> 
  where
    T: Borrow<Q>,
    Q: Eq + Hash,
  {
    self.map.depth_of(key)
  }

  /// Gets the depth of the specified key (i.e. how many layers down from the top that the key first appears), 
  /// but ignores a maximum number of layers from the top equal to `min_depth`.
  /// Saturates to base layer. 
  ///
  /// Returns `None` if the key does not exist.
  ///
  /// Computes in **O(n)** time (worst-case) in relation to layer count.
  #[inline]
  pub fn depth_of_parent<Q: ?Sized>(&self, key: &Q, min_depth: usize) -> Option<usize> 
  where
    T: Borrow<Q>,
    Q: Eq + Hash,
  {
    self.map.get_parent_depth(key, min_depth).map(|(_, d)| d)
  }

  /// Gets the height of the specified key (i.e. how many layers up from the bottom that the key last appears).
  /// A height of 0 refers to the bottom layer.
  ///
  /// Returns `None` if the key does not exist.
  ///
  /// Computes in **O(n)** time (worst-case) in relation to layer count.
  #[inline]
  pub fn height_of<Q: ?Sized>(&self, key: &Q) -> Option<usize> 
  where
  T: Borrow<Q>,
  Q: Eq + Hash,
  {
    self.map.height_of(key)
  }
  
  /// Gets the height of the specified key (i.e. how many layers up from the bottom that the key last appears),
  /// but ignores a maximum number of layers from the top equal to `min_depth`.
  /// A height of 0 refers to the bottom layer.
  /// Saturates to base layer.
  ///
  /// Returns `None` if the key does not exist.
  ///
  /// Computes in **O(n)** time (worst-case) in relation to layer count.
  #[inline]
  pub fn height_of_parent<Q: ?Sized>(&self, key: &Q, min_depth: usize) -> Option<usize> 
  where
    T: Borrow<Q>,
    Q: Eq + Hash,
  {
    self.map.get_parent_height(key, min_depth).map(|(_, h)| h)
  }

  /// Iterates over the keys in arbitrary order.
  ///
  /// The iterator element type is `&'a T`.
  #[inline]
  pub fn iter(&self) -> impl Iterator<Item = &T> {
    self.map.keys()
  }

  /// Iterates over the top-level keys in arbitrary order.
  ///
  /// The iterator element type is `&'a T`.
  #[inline]
  pub fn iter_top(&self) -> impl Iterator<Item = &T> {
    self.map.keys_top()
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use std::collections::HashSet;

  #[test]
  fn set_init() {
    let set: ScopeSet<String> = ScopeSet::new();
    assert_eq!(0, set.len());
    assert_eq!(1, set.depth());
    assert!(set.is_empty());
  }

  #[test]
  fn set_default() {
    let set: ScopeSet<String> = Default::default();
    assert_eq!(0, set.len());
    assert_eq!(1, set.depth());
    assert!(set.is_empty());
  }

  #[test]
  fn set_capacity() {
    let set: ScopeSet<String> = ScopeSet::with_capacity(32);
    assert_eq!(32, set.capacity());
  }

  #[test]
  fn set_define() {
    let mut set = ScopeSet::new();
    set.define("foo");
    assert_eq!(1, set.len());
  }

  #[test]
  fn set_define_parent() {
    let mut set = ScopeSet::new();
    set.push_layer();
    set.define_parent("foo", 1);
    assert_eq!(Some(1), set.depth_of("foo"));
    assert!(set.contains("foo"));
  }

  #[test]
  fn set_delete() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.remove("foo");
    assert!(!set.contains("foo"));
  }

  #[test]
  fn set_pop_to_delete() {
    let mut set = ScopeSet::new();
    set.push_layer();
    set.define("foo");
    assert!(set.contains("foo"));
    set.pop_layer();
    assert!(!set.contains("foo"));
  }

  #[test]
  fn set_layer_count() {
    let mut set: ScopeSet<String> = Default::default();
    set.push_layer();
    assert_eq!(2, set.depth());
    set.pop_layer();
    assert_eq!(1, set.depth());
  }

  #[test]
  fn set_try_pop_first_layer() {
    let mut set: ScopeSet<String> = Default::default();
    assert_eq!(false, set.pop_layer());
    assert_eq!(1, set.depth());
  }

  #[test]
  fn set_contains() {
    let mut set = ScopeSet::new();
    set.define("foo");
    assert!(set.contains("foo"));
  }

  #[test]
  fn set_contains_none() {
    let mut set = ScopeSet::new();
    set.define("foo");
    assert!(!set.contains("bar"));
  }

  #[test]
  fn set_contains_multi_layer() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.define("bar");
    assert!(set.contains("foo"));
    assert!(set.contains("bar"));
  }

  #[test]
  fn set_depth_of() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.define("bar");
    assert_eq!(Some(1), set.depth_of("foo"));
    assert_eq!(Some(0), set.depth_of("bar"));
    assert_eq!(None, set.depth_of("baz"));
  }

  #[test]
  fn set_depth_of_parent() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.push_layer();
    set.define("foo");
    assert_eq!(Some(0), set.depth_of_parent("foo", 0));
    assert_eq!(Some(2), set.depth_of_parent("foo", 1));
    assert_eq!(Some(2), set.depth_of_parent("foo", 2));
  }

  #[test]
  fn set_height_of() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.define("bar");
    assert_eq!(Some(0), set.height_of("foo"));
    assert_eq!(Some(1), set.height_of("bar"));
    assert_eq!(None, set.height_of("baz"));
  }

  #[test]
  fn set_height_of_parent() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.push_layer();
    set.define("foo");
    assert_eq!(Some(2), set.height_of_parent("foo", 0));
    assert_eq!(Some(0), set.height_of_parent("foo", 1));
    assert_eq!(Some(0), set.height_of_parent("foo", 2));
  }

  #[test]
  fn set_iter() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.define("bar");
    set.push_layer();
    set.define("baz");

    let expected_keys: HashSet<&str> = ["foo", "bar", "baz"].iter().cloned().collect();
    let actual_keys: HashSet<&str> = set.iter().cloned().collect();
    assert_eq!(expected_keys, actual_keys);
  }

  #[test]
  fn set_iter_top() {
    let mut set = ScopeSet::new();
    set.define("foo");
    set.push_layer();
    set.define("bar");
    set.push_layer();
    set.define("baz");
    set.define("qux");

    let expected_keys: HashSet<&str> = ["baz", "qux"].iter().cloned().collect();
    let actual_keys: HashSet<&str> = set.iter_top().cloned().collect();
    assert_eq!(expected_keys, actual_keys);
  }
}