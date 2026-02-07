use crate::throw;
use crate::{parser::interner::InternedString, rvals::Result};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// At the REPL or within an application, a script might be repeatedly
/// loaded. In this situation, the behavior that Steel picks is that
/// values that are shadowed are still there. Right now - we just leave
/// those values around, and they are unable to be reclaimed.
///
/// Instead what we should do is if a script is executed, we should
/// check if any values are shadowed directly. If they are shadowed,
/// we should add them to a candidate list of indices that we could
/// potentially mark as unreachable, freeing them for future use.
///
/// In order to mark things as reachable, we will need to run a GC
/// pass explicitly to check for the values that might be unreachable.
///
/// Since these are known to potentially be unreachable, this is our
/// candidate set to drop. Reachability in this case, is just checking
/// if the global variable is referenced by any instructions. That offset
/// into the global namespace is the only way that globals get referenced.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct FreeList {
    // Slots which are potentially unreachable.
    pub(crate) shadowed_slots: Vec<usize>,

    // Slots which represent lambda lifted functions.
    pub(crate) lambda_lifted: Vec<usize>,

    // Once the above slots have been deemed definitely unreachable,
    // we put them here. This is now the candidate set of variables
    // to fill in from. We don't have to slot at the back, we can
    // slot in over the things that have filled up otherwise.
    pub(crate) free_list: Vec<usize>,

    pub(crate) threshold: usize,

    pub(crate) multiplier: usize,

    pub(crate) epoch: usize,

    pub(crate) recently_freed: HashSet<usize>,
}

impl FreeList {
    pub fn add_shadowed(&mut self, val: usize) {
        self.shadowed_slots.push(val);
    }

    pub fn pop_next_free(&mut self) -> Option<usize> {
        self.free_list.pop().map(|x| {
            self.recently_freed.insert(x);
            x
        })
    }

    pub fn clear_recently_freed(&mut self) {
        self.recently_freed.clear();
    }

    pub fn shadowed_count(&self) -> usize {
        self.shadowed_slots.len()
    }

    pub fn should_collect(&self) -> bool {
        self.shadowed_count() > self.threshold
    }

    pub fn increment_generation(&mut self) {
        if self.epoch == 4 {
            self.threshold = 100;
            self.epoch = 1;
        } else {
            self.threshold *= self.multiplier;
            self.epoch += 1;
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolMap {
    values: Vec<InternedString>,
    map: FxHashMap<InternedString, usize>,
    // TODO:
    // This definitely does not need to be this way, and also it
    // can be locked during the entire duration of the compilation
    // process
    pub(crate) free_list: FreeList,
}

impl Default for SymbolMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolMap {
    pub fn new() -> Self {
        SymbolMap {
            values: Vec::new(),
            map: FxHashMap::default(),
            free_list: FreeList {
                threshold: 100,
                multiplier: 2,
                epoch: 1,
                ..Default::default()
            },
        }
    }

    pub fn values(&self) -> &Vec<InternedString> {
        &self.values
    }

    pub fn map(&self) -> &FxHashMap<InternedString, usize> {
        &self.map
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn roll_back(&mut self, index: usize) {
        for value in self.values.drain(index..) {
            self.map.remove(&value);
        }
    }

    pub fn contains(&mut self, ident: InternedString) -> bool {
        self.map.contains_key(&ident)
    }

    pub fn add(&mut self, ident: &InternedString) -> usize {
        // Check the free list for the next value. If the free list has an open slot
        // then we should take that. Otherwise, just insert it at the end.
        let idx = self
            .free_list
            .pop_next_free()
            .unwrap_or_else(|| self.values.len());

        {
            let resolved = ident.resolve();

            if resolved.starts_with("##__lifted_pure_function")
                || resolved.starts_with("##lambda-lifting")
            {
                self.free_list.lambda_lifted.push(idx);
            }
        }

        let prev = self.map.insert(*ident, idx);

        // There was something there previously.
        // And now that we're overriding it, go ahead and put
        // it in the shadowed_slots.
        if let Some(prev) = prev {
            self.free_list.add_shadowed(prev);
        }

        // If this is going in at the end, then we just slot it in there.
        // Otherwise, we need to overwrite the existing global slot.
        if idx == self.values.len() {
            // Add the values so we can do a backwards resolution
            self.values.push(*ident);
        } else {
            self.values[idx] = *ident;
        }

        idx
    }

    // fallible
    pub fn get(&self, ident: &InternedString) -> Result<usize> {
        self.map
            .get(ident)
            .copied()
            .ok_or_else(throw!(FreeIdentifier => "Cannot reference an identifier before its definition: {}", ident.resolve()))
    }
}
