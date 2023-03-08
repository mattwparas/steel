use crate::throw;
use crate::{parser::interner::InternedString, rvals::Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct SymbolMap {
    values: Vec<InternedString>,
    // TODO don't do this - don't expose this API
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

    pub fn len(&self) -> usize {
        self.values.len()
    }

    // pub fn add(&mut self, ident: &str) -> usize {
    //     let idx = self.values.len();
    //     self.values.push(ident.to_string());
    //     // println!("`add`: {} @ {}", ident, idx);
    //     idx
    // }

    pub fn roll_back(&mut self, index: usize) {
        for value in self.values.drain(index..) {
            self.map.remove(&value);
        }
    }

    pub fn get_or_add(&mut self, ident: &InternedString) -> usize {
        // let rev_iter = self.values.iter().enumerate().rev();

        // for (idx, val) in rev_iter {
        //     if val == ident {
        //         return idx;
        //     }
        // }

        // let idx = self.values.len();
        // self.values.push(ident.to_string());

        // idx

        if let Some(idx) = self.map.get(ident) {
            // if ident == "b" {
            //     println!("Fetching from map: {}", idx);
            // }

            *idx
        } else {
            let idx = self.map.len();

            // if ident == "b" {
            //     println!("Adding to map: {}", idx);
            // }

            self.map.insert(*ident, idx);

            // Add the values so we can do a backwards resolution
            self.values.push(*ident);

            idx
        }
    }

    // fallible
    pub fn get(&self, ident: &InternedString) -> Result<usize> {
        // let rev_iter = self.values.iter().enumerate().rev();

        // for (idx, val) in rev_iter {
        //     // println!("{}", idx);
        //     if val == ident {
        //         return Ok(idx);
        //     }
        // }
        // let e = ident.to_string();
        // // TODO come back to this - this is causing things to be renamed to 0 regardless
        // stop!(FreeIdentifier => e)

        self.map
            .get(ident)
            .copied()
            .ok_or_else(throw!(FreeIdentifier => ident.resolve()))
    }
}
