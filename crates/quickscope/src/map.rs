use std::{
    borrow::Borrow,
    collections::{hash_map::RandomState, HashSet},
    hash::{BuildHasher, Hash},
    ops::Index,
};

use indexmap::IndexMap;
use smallvec::SmallVec;

type ScopeMapValueStack<V> = SmallVec<[V; 1]>;

#[inline(always)]
fn invert_index(index: usize, n: usize) -> usize {
    if index >= n {
        0
    } else {
        n - index - 1
    }
}

#[derive(Clone)]
struct Var<T> {
    value: T,
    layer: usize,
}

/// A layered hash map for representing scoped variables and their values.
#[derive(Clone)]
pub struct ScopeMap<K, V, S: BuildHasher = RandomState> {
    /// Stores a value stack for each variable.
    ///
    /// The bottom of a variable's stack corresponds to the lowest layer on which the variable appears.
    map: IndexMap<K, ScopeMapValueStack<Var<V>>, S>,
    /// Stores the layers of the stack.
    ///
    /// Each layer contains map indices indicating which variables are created or updated in that layer.
    layers: Vec<HashSet<usize>>,
    /// The number of currently empty variable stacks.
    ///
    /// Used internally to accurately calculate the number of active variables.
    empty_key_count: usize,

    reused_layers: Vec<HashSet<usize>>,
}

impl<K, V, S: Default + BuildHasher> Default for ScopeMap<K, V, S> {
    /// Creates a new `ScopeMap` with the default configuration.
    #[inline]
    fn default() -> Self {
        Self::with_hasher(Default::default())
    }
}

impl<K, Q: ?Sized, V, S> Index<&Q> for ScopeMap<K, V, S>
where
    K: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash,
    S: BuildHasher,
{
    type Output = V;

    /// Returns a reference to the value associated with the provided key.
    ///
    /// # Panics
    ///
    /// Panics if the key does not exist in the `ScopeMap`.
    #[inline]
    fn index(&self, index: &Q) -> &Self::Output {
        self.get(index).expect("key not found in map")
    }
}

impl<K, V> ScopeMap<K, V, RandomState> {
    /// Creates an empty `ScopeMap` with a default hasher and capacity.
    #[inline]
    pub fn new() -> ScopeMap<K, V, RandomState> {
        Self {
            map: Default::default(),
            layers: vec![Default::default()],
            empty_key_count: 0,
            reused_layers: Vec::new(),
        }
    }

    /// Creates an empty `ScopeMap` with a default hasher and the specified capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> ScopeMap<K, V, RandomState> {
        Self::with_capacity_and_hasher(capacity, Default::default())
    }
}

impl<K, V, S: BuildHasher> ScopeMap<K, V, S> {
    /// Creates an empty `ScopeMap` with the specified hasher and a default capacity.
    #[inline]
    pub fn with_hasher(hash_builder: S) -> Self {
        Self {
            map: IndexMap::with_hasher(hash_builder),
            layers: vec![Default::default()],
            empty_key_count: 0,
            reused_layers: Vec::new(),
        }
    }

    /// Creates an empty `ScopeMap` with the specified hasher and capacity.
    #[inline]
    pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> Self {
        Self {
            map: IndexMap::with_capacity_and_hasher(capacity, hash_builder),
            layers: vec![Default::default()],
            empty_key_count: 0,
            reused_layers: Vec::new(),
        }
    }

    /// Gets the number of elements the map can hold without reallocating.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.map.capacity()
    }

    /// Returns `true` if the map is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Gets the number of unique keys in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.map.len() - self.empty_key_count
    }

    /// Gets the number of layers in the map.
    #[inline]
    pub fn depth(&self) -> usize {
        self.layers.len()
    }
}

impl<K, V, S> ScopeMap<K, V, S>
where
    S: BuildHasher,
{
    /// Adds a new, empty layer.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn push_layer(&mut self) {
        self.layers
            .push(self.reused_layers.pop().unwrap_or_default())
    }

    /// Removes the topmost layer (if it isn't the bottom layer) and all associated keys/values.
    /// Returns `true` if a layer was removed.
    ///
    /// Computes in **O(n)** time in relation to the number of keys stored in the removed layer.
    #[inline]
    pub fn pop_layer(&mut self) -> bool {
        // Don't allow the base layer to be popped
        if self.layers.len() > 1 {
            let mut last_layer = self.layers.pop().unwrap();

            // Pop the keys found in the removed layer
            for stack_index in last_layer.drain() {
                if let Some((_key, stack)) = self.map.get_index_mut(stack_index) {
                    let stack_just_emptied = stack.pop().is_some() && stack.is_empty();
                    if stack_just_emptied {
                        self.empty_key_count += 1;
                    }
                }
            }

            self.reused_layers.push(last_layer);

            return true;
        }
        false
    }
}

impl<K, V, S> ScopeMap<K, V, S>
where
    K: Clone,
    S: BuildHasher,
{
    pub fn drain_layer(&mut self) -> SmallVec<[(K, V); 4]> {
        let mut results = SmallVec::new();
        // Don't allow the base layer to be popped
        if self.layers.len() > 1 {
            // Pop the keys found in the removed layer
            for stack_index in self.layers.pop().unwrap() {
                // if let Some((_key, stack)) = self.map.get_index_mut(stack_index) {
                if let Some((key, stack)) = self.map.get_index_mut(stack_index) {
                    let last = stack.pop();
                    let stack_just_emptied = last.is_some() && stack.is_empty();
                    if stack_just_emptied {
                        self.empty_key_count += 1;
                    }

                    if let Some(last) = last {
                        results.push((key.clone(), last.value));
                    }
                }
            }
            return results;
        }
        results
    }
}

impl<K: Eq + Hash, V, S: BuildHasher> ScopeMap<K, V, S> {
    /// Returns `true` if the map contains the specified key in any layer.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn contains_key<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some(stack) = self.map.get(key) {
            !stack.is_empty()
        } else {
            false
        }
    }

    /// Returns `true` if the map contains the specified key at the top layer.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn contains_key_at_top<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.map
            .get_index_of(key)
            .map_or(false, |i| self.layers.last().unwrap().contains(&i))
    }

    /// Gets a reference to the topmost value associated with a key.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.map.get(key).and_then(|v| v.last().map(|v| &v.value))
    }

    /// Gets an iterator over references to all the values associated with a key, starting with the topmost and going down.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn get_all<Q: ?Sized>(&self, key: &Q) -> Option<impl Iterator<Item = &V>>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.map
            .get(key)
            .map(|stack| stack.iter().rev().map(|v| &v.value))
    }

    /// Gets a mutable reference to the topmost value associated with a key.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.map
            .get_mut(key)
            .and_then(|v| v.last_mut().map(|v| &mut v.value))
    }

    /// Gets an iterator over mutable references to all the values associated with a key, starting with the topmost and going down.
    ///
    /// Computes in **O(1)** time.
    #[inline]
    pub fn get_all_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<impl Iterator<Item = &mut V>>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        self.map
            .get_mut(key)
            .map(|stack| stack.iter_mut().rev().map(|v| &mut v.value))
    }

    /// Gets a reference to a value `min_depth` layers below the topmost value associated with a key.
    /// Saturates to base layer.
    ///
    /// Computes in **O(n)** time (worst-case) in relation to `min_depth`.
    #[inline]
    pub fn get_parent<Q: ?Sized>(&self, key: &Q, min_depth: usize) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((var_index, _key, stack)) = self.map.get_full(key) {
            // If the skip count exceeds the stack size, it shouldn't matter because take() is self-truncating
            let stack_skip_count = self
                .layers
                .iter()
                .rev()
                .take(min_depth)
                .filter(|layer| layer.contains(&var_index))
                .count();
            return stack.iter().rev().nth(stack_skip_count).map(|v| &v.value);
        }
        None
    }

    /// Gets a reference to the value associated with a key at least `min_depth` layers below the topmost layer, as well as its associated depth.
    /// Saturates to base layer.
    ///
    /// Computes in **O(n)** time (worst-case) in relation to `min_depth`.
    #[inline]
    pub fn get_parent_depth<Q: ?Sized>(&self, key: &Q, min_depth: usize) -> Option<(&V, usize)>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((var_index, _key, stack)) = self.map.get_full(key) {
            // If the skip count exceeds the stack size, it shouldn't matter because take() is self-truncating
            let stack_skip_count = self
                .layers
                .iter()
                .rev()
                .take(min_depth)
                .filter(|layer| layer.contains(&var_index))
                .count();
            return stack
                .iter()
                .rev()
                .nth(stack_skip_count)
                .map(|v| (&v.value, invert_index(v.layer, self.depth())));
        }
        None
    }

    /// Gets a reference to the value associated with a key at least `min_depth` layers below the topmost layer, as well as its associated height.
    /// Saturates to base layer.
    ///
    /// Computes in **O(n)** time (worst-case) in relation to `min_depth`.
    #[inline]
    pub fn get_parent_height<Q: ?Sized>(&self, key: &Q, min_depth: usize) -> Option<(&V, usize)>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((var_index, _key, stack)) = self.map.get_full(key) {
            // If the skip count exceeds the stack size, it shouldn't matter because take() is self-truncating
            let stack_skip_count = self
                .layers
                .iter()
                .rev()
                .take(min_depth)
                .filter(|layer| layer.contains(&var_index))
                .count();
            return stack
                .iter()
                .rev()
                .nth(stack_skip_count)
                .map(|v| (&v.value, v.layer));
        }
        None
    }

    /// Gets an iterator over references to all values `min_depth` layers below the topmost value associated with a key.
    /// Saturates to base layer.
    ///
    /// Computes in **O(n)** time (worst-case) in relation to `min_depth`.
    #[inline]
    pub fn get_parents<Q: ?Sized>(
        &self,
        key: &Q,
        min_depth: usize,
    ) -> Option<impl Iterator<Item = &V>>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((var_index, _key, stack)) = self.map.get_full(key) {
            // If the skip count exceeds the stack size, it shouldn't matter because take() is self-truncating
            let stack_skip_count = self
                .layers
                .iter()
                .rev()
                .take(min_depth)
                .filter(|layer| layer.contains(&var_index))
                .count();
            return Some(stack.iter().rev().skip(stack_skip_count).map(|v| &v.value));
        }
        None
    }

    /// Gets a mutable reference to a value `min_depth` layers below the topmost value associated with a key.
    /// Saturates to base layer.
    ///
    /// Computes in **O(n)** time (worst-case) in relation to `min_depth`.
    #[inline]
    pub fn get_parent_mut<Q: ?Sized>(&mut self, key: &Q, min_depth: usize) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((var_index, _key, stack)) = self.map.get_full_mut(key) {
            // If the skip count exceeds the stack size, it shouldn't matter because take() is self-truncating
            let stack_skip_count = self
                .layers
                .iter()
                .rev()
                .take(min_depth)
                .filter(|layer| layer.contains(&var_index))
                .count();
            return stack
                .iter_mut()
                .rev()
                .nth(stack_skip_count)
                .map(|v| &mut v.value);
        }
        None
    }

    /// Gets an iterator over mutable references to all values `min_depth` layers below the topmost value associated with a key.
    /// Saturates to base layer.
    ///
    /// Computes in **O(n)** time (worst-case) in relation to `min_depth`.
    #[inline]
    pub fn get_parents_mut<Q: ?Sized>(
        &mut self,
        key: &Q,
        min_depth: usize,
    ) -> Option<impl Iterator<Item = &mut V>>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((var_index, _key, stack)) = self.map.get_full_mut(key) {
            // If the skip count exceeds the stack size, it shouldn't matter because take() is self-truncating
            let stack_skip_count = self
                .layers
                .iter()
                .rev()
                .take(min_depth)
                .filter(|layer| layer.contains(&var_index))
                .count();
            return Some(
                stack
                    .iter_mut()
                    .rev()
                    .skip(stack_skip_count)
                    .map(|v| &mut v.value),
            );
        }
        None
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
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some(index) = self.map.get_index_of(key) {
            for (depth, layer) in self.layers.iter().rev().enumerate() {
                if layer.contains(&index) {
                    return Some(depth);
                }
            }
        }
        None
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
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some(index) = self.map.get_index_of(key) {
            for (height, layer) in self.layers.iter().enumerate().rev() {
                if layer.contains(&index) {
                    return Some(height);
                }
            }
        }
        None
    }

    /// Adds the specified entry to the topmost layer.
    #[inline]
    pub fn define(&mut self, key: K, value: V) {
        let height = self.depth();
        let entry = self.map.entry(key);
        let var_index = entry.index();
        let is_stack_new = matches!(entry, indexmap::map::Entry::Vacant(..));
        let stack = entry.or_insert_with(Default::default);
        let is_new_in_layer = self.layers.last_mut().unwrap().insert(var_index);
        let was_stack_empty = stack.is_empty();

        if is_new_in_layer {
            stack.push(Var {
                value,
                layer: height - 1,
            });
            if was_stack_empty && !is_stack_new {
                self.empty_key_count -= 1;
            }
        } else {
            stack.last_mut().unwrap().value = value;
        }
    }

    /// Adds the specified entry in the layer `min_depth` layers from the top. Saturates to base layer.
    #[inline]
    pub fn define_parent(&mut self, key: K, value: V, min_depth: usize) {
        let height = self.depth();
        let entry = self.map.entry(key);
        let stack_index = entry.index();
        let is_stack_new = matches!(entry, indexmap::map::Entry::Vacant(..));
        let stack = entry.or_insert_with(Default::default);
        let is_new_in_layer = self
            .layers
            .iter_mut()
            .nth_back(min_depth.min(height - 1))
            .unwrap()
            .insert(stack_index);
        let was_stack_empty = stack.is_empty();

        let stack_skip_count = self
            .layers
            .iter()
            .rev()
            .take(min_depth)
            .filter(|layer| layer.contains(&stack_index))
            .count();

        let index_in_stack = invert_index(stack_skip_count, stack.len());

        if is_new_in_layer {
            // If the key is new in this layer, we need to insert the value into the key's stack
            stack.insert(
                index_in_stack,
                Var {
                    value,
                    layer: height.saturating_sub(min_depth + 1),
                },
            );

            if was_stack_empty && !is_stack_new {
                self.empty_key_count -= 1;
            }
        } else {
            // If the key is already in the layer, just replace the value
            stack[index_in_stack].value = value;
        }
    }

    /// Removes the entry with the specified key from the topmost layer and returns its value.
    #[inline]
    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash,
    {
        if let Some((index, _key, stack)) = self.map.get_full_mut(key) {
            if self.layers.last_mut().unwrap().remove(&index) {
                let taken = stack.pop();
                let stack_just_emptied = taken.is_some() && stack.is_empty();
                if stack_just_emptied {
                    self.empty_key_count += 1;
                }
                return taken.map(|v| v.value);
            }
        }
        None
    }

    /// Removes all entries in the topmost layer.
    #[inline]
    pub fn clear_top(&mut self) {
        for stack_index in self.layers.last_mut().unwrap().drain() {
            let stack = self.map.get_index_mut(stack_index).unwrap().1;
            let stack_just_emptied = stack.pop().is_some() && stack.is_empty();
            if stack_just_emptied {
                self.empty_key_count += 1;
            }
        }
    }

    /// Removes all elements and additional layers.
    #[inline]
    pub fn clear_all(&mut self) {
        self.map.clear();
        self.layers.clear();
        self.layers.push(Default::default());
        self.empty_key_count = 0;
    }

    /// Iterates over all key-value pairs in arbitrary order.
    ///
    /// The iterator element type is `(&'a K, &'a V)`.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&'_ K, &'_ V)> {
        self.map
            .iter()
            .filter_map(|(key, stack)| stack.last().map(|var| (key, &var.value)))
    }

    #[inline]
    pub fn iter_without_bottom(&self) -> impl Iterator<Item = (&'_ K, &'_ V)> {
        self.layers.iter().skip(1).flat_map(move |x| {
            x.iter().filter_map(move |i| {
                self.map
                    .get_index(*i)
                    .map(|(key, stack)| (key, &stack.last().unwrap().value))
            })
        })
    }

    /// Iterates over all key-value pairs in the topmost layer in arbitrary order.
    ///
    /// The iterator element type is `(&'a K, &'a V)`.
    #[inline]
    pub fn iter_top(&self) -> impl Iterator<Item = (&'_ K, &'_ V)> {
        self.layers.last().unwrap().iter().filter_map(move |i| {
            self.map
                .get_index(*i)
                .map(|(key, stack)| (key, &stack.last().unwrap().value))
        })
    }

    /// Iterates over all key-value pairs in arbitrary order, allowing mutation of the values.
    ///
    /// The iterator element type is `(&'a K, &'a mut V)`.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&'_ K, &'_ mut V)> {
        self.map
            .iter_mut()
            .filter_map(|(key, stack)| stack.last_mut().map(|var| (key, &mut var.value)))
    }

    /// Iterates over all keys in arbitrary order.
    ///
    /// The iterator element type is `&'a K`.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &'_ K> {
        self.map
            .iter()
            .filter(|(_, stack)| !stack.is_empty())
            .map(|(key, _)| key)
    }

    /// Iterates over all keys in the topmost layer in arbitrary order.
    ///
    /// The iterator element type is `&'a K`.
    #[inline]
    pub fn keys_top(&self) -> impl Iterator<Item = &'_ K> {
        self.layers
            .last()
            .unwrap()
            .iter()
            .map(move |i| self.map.get_index(*i).unwrap().0)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn map_init() {
        let map: ScopeMap<String, i32> = ScopeMap::new();
        assert_eq!(0, map.len());
        assert_eq!(1, map.depth());
        assert!(map.is_empty());
    }

    #[test]
    fn map_default() {
        let map: ScopeMap<String, i32> = Default::default();
        assert_eq!(0, map.len());
        assert_eq!(1, map.depth());
        assert!(map.is_empty());
    }

    #[test]
    fn map_capacity() {
        let map: ScopeMap<String, i32> = ScopeMap::with_capacity(32);
        assert_eq!(32, map.capacity());
    }

    #[test]
    fn map_define() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        assert_eq!(1, map.len());
        assert_eq!(Some(&123), map.get("foo"));
    }

    #[test]
    fn map_define_parent() {
        let mut map = ScopeMap::new();
        map.push_layer();
        map.define_parent("foo", 123, 1);
        assert_eq!(Some(1), map.depth_of("foo"));
        assert_eq!(Some(&123), map.get_parent("foo", 1));
        assert_eq!(None, map.get_parent("foo", 2));
    }

    #[test]
    fn map_define_parent_after_child() {
        let mut map = ScopeMap::new();
        map.push_layer();
        map.define("foo", 456);
        map.define_parent("foo", 123, 1);
        assert_eq!(Some(&456), map.get("foo"));
        assert_eq!(Some(&123), map.get_parent("foo", 1));
        map.pop_layer();
        assert_eq!(Some(&123), map.get("foo"));
    }

    #[test]
    fn map_define_parent_saturated() {
        let mut map = ScopeMap::new();
        map.push_layer();
        map.define_parent("foo", 123, 3);
        assert_eq!(Some(1), map.depth_of("foo"));
        assert_eq!(Some(&123), map.get("foo"));
    }

    #[test]
    fn map_remove() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        let removed = map.remove("foo");
        assert_eq!(0, map.len());
        assert_eq!(None, map.get("foo"));
        assert_eq!(Some(123), removed);
        assert!(!map.contains_key("foo"));
    }

    #[test]
    fn map_layer_count() {
        let mut map: ScopeMap<String, i32> = Default::default();
        map.push_layer();
        assert_eq!(2, map.depth());
        map.pop_layer();
        assert_eq!(1, map.depth());
    }

    #[test]
    fn map_try_pop_first_layer() {
        let mut map: ScopeMap<String, i32> = Default::default();
        assert_eq!(false, map.pop_layer());
        assert_eq!(1, map.depth());
    }

    #[test]
    fn map_get_none() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        assert_eq!(None, map.get("bar"));
    }

    #[test]
    fn map_get_multi_layer() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("bar", 456);
        assert_eq!(Some(&123), map.get("foo"));
        assert_eq!(Some(&456), map.get("bar"));
    }

    #[test]
    fn map_get_parent() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("foo", 456);
        assert_eq!(Some(&456), map.get_parent("foo", 0));
        assert_eq!(Some(&123), map.get_parent("foo", 1));
    }

    #[test]
    fn map_get_parent_none() {
        let mut map = ScopeMap::new();
        map.push_layer();
        map.define("foo", 123);
        assert_eq!(None, map.get_parent("foo", 1));
    }

    #[test]
    fn map_get_parent_depth() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.push_layer();
        map.define("foo", 456);
        assert_eq!(Some((&456, 0)), map.get_parent_depth("foo", 0));
        assert_eq!(Some((&123, 2)), map.get_parent_depth("foo", 1));
        assert_eq!(Some((&123, 2)), map.get_parent_depth("foo", 2));
    }

    #[test]
    fn map_get_parent_height() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.push_layer();
        map.define("foo", 456);
        assert_eq!(Some((&456, 2)), map.get_parent_height("foo", 0));
        assert_eq!(Some((&123, 0)), map.get_parent_height("foo", 1));
        assert_eq!(Some((&123, 0)), map.get_parent_height("foo", 2));
    }

    #[test]
    fn map_get_all() {
        let mut map = ScopeMap::new();
        map.define("foo", 1);
        map.push_layer();
        map.define("foo", 2);
        map.push_layer();
        map.define("foo", 3);
        let values = map
            .get_all("foo")
            .map(|values| values.cloned().collect::<Vec<i32>>());
        assert_eq!(Some(vec![3, 2, 1]), values);
    }

    #[test]
    fn map_get_parents() {
        let mut map = ScopeMap::new();
        map.define("foo", 1);
        map.push_layer();
        map.define("foo", 2);
        map.push_layer();
        map.define("foo", 3);
        let values = map
            .get_parents("foo", 1)
            .map(|values| values.cloned().collect::<Vec<i32>>());
        assert_eq!(Some(vec![2, 1]), values);
    }

    #[test]
    fn map_define_override() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("foo", 456);
        assert_eq!(Some(&456), map.get("foo"));
    }

    #[test]
    fn map_delete_override() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("foo", 456);
        map.remove("foo");
        assert_eq!(Some(&123), map.get("foo"));
    }

    #[test]
    fn map_pop_override() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("foo", 456);
        map.pop_layer();
        assert_eq!(Some(&123), map.get("foo"));
    }

    #[test]
    fn map_get_mut() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        if let Some(foo) = map.get_mut("foo") {
            *foo = 456;
        }
        assert_eq!(Some(&456), map.get("foo"));
    }

    #[test]
    fn map_contains_key() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        assert!(map.contains_key("foo"));
    }

    #[test]
    fn map_not_contains_key() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        assert!(!map.contains_key("bar"));
    }

    #[test]
    fn map_depth_of() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("bar", 456);
        assert_eq!(Some(1), map.depth_of("foo"));
        assert_eq!(Some(0), map.depth_of("bar"));
        assert_eq!(None, map.depth_of("baz"));
    }

    #[test]
    fn map_height_of() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("bar", 456);
        assert_eq!(Some(0), map.height_of("foo"));
        assert_eq!(Some(1), map.height_of("bar"));
        assert_eq!(None, map.height_of("baz"));
    }

    #[test]
    fn map_keys() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("bar", 456);
        map.push_layer();
        map.define("baz", 789);

        let expected_keys: HashSet<&str> = ["foo", "bar", "baz"].iter().cloned().collect();
        let actual_keys: HashSet<&str> = map.keys().cloned().collect();
        assert_eq!(expected_keys, actual_keys);
    }

    #[test]
    fn map_keys_top() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.push_layer();
        map.define("bar", 456);
        map.push_layer();
        map.define("baz", 789);
        map.define("qux", 999);

        let expected_keys: HashSet<&str> = ["baz", "qux"].iter().cloned().collect();
        let actual_keys: HashSet<&str> = map.keys_top().cloned().collect();
        assert_eq!(expected_keys, actual_keys);
    }

    #[test]
    fn map_iter() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.define("bar", 123);
        map.define("baz", 123);
        map.push_layer();
        map.define("bar", 456);
        map.push_layer();
        map.define("baz", 789);

        let expected_keys: HashSet<(&str, i32)> = [("foo", 123), ("bar", 456), ("baz", 789)]
            .iter()
            .cloned()
            .collect();
        let actual_keys: HashSet<(&str, i32)> = map.iter().map(|(key, val)| (*key, *val)).collect();
        assert_eq!(expected_keys, actual_keys);
    }

    #[test]
    fn map_iter_top() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.define("bar", 123);
        map.define("baz", 123);
        map.push_layer();
        map.define("bar", 456);
        map.push_layer();
        map.define("baz", 789);
        map.define("qux", 999);

        let expected_keys: HashSet<(&str, i32)> =
            [("baz", 789), ("qux", 999)].iter().cloned().collect();
        let actual_keys: HashSet<(&str, i32)> =
            map.iter_top().map(|(key, val)| (*key, *val)).collect();
        assert_eq!(expected_keys, actual_keys);
    }

    #[test]
    fn map_iter_mut() {
        let mut map = ScopeMap::new();
        map.define("foo", 123);
        map.define("bar", 123);
        map.define("baz", 123);
        map.push_layer();
        map.define("bar", 456);
        map.push_layer();
        map.define("baz", 789);

        for (_k, v) in map.iter_mut() {
            *v = 999;
        }

        let expected_keys: HashSet<(&str, i32)> = [("foo", 999), ("bar", 999), ("baz", 999)]
            .iter()
            .cloned()
            .collect();
        let actual_keys: HashSet<(&str, i32)> = map.iter().map(|(key, val)| (*key, *val)).collect();
        assert_eq!(expected_keys, actual_keys);
    }
}
