use crate::throw;
use crate::{parser::interner::InternedString, rvals::Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct SymbolMap {
    values: Vec<InternedString>,
    map: HashMap<InternedString, usize>,
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
            map: HashMap::new(),
        }
    }

    pub fn values(&self) -> &Vec<InternedString> {
        &self.values
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn roll_back(&mut self, index: usize) {
        for value in self.values.drain(index..) {
            self.map.remove(&value);
        }
    }

    pub fn add(&mut self, ident: &InternedString) -> usize {
        let idx = self.values.len();

        self.map.insert(*ident, idx);

        // Add the values so we can do a backwards resolution
        self.values.push(*ident);

        idx
    }

    // fallible
    pub fn get(&self, ident: &InternedString) -> Result<usize> {
        self.map
            .get(ident)
            .copied()
            .ok_or_else(throw!(FreeIdentifier => ident.resolve()))
    }
}
