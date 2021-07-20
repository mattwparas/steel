use std::collections::HashMap;

use crate::{
    parser::ast::ExprKind,
    rvals::{Result, SteelVal},
};

// TODO
pub const fn _new_void() -> SteelVal {
    SteelVal::Void
}

// TODO
pub const fn _new_true() -> SteelVal {
    SteelVal::BoolV(true)
}

// TODO
pub const fn _new_false() -> SteelVal {
    SteelVal::BoolV(false)
}

#[derive(Debug)]
pub struct Env {
    pub(crate) bindings_vec: Vec<SteelVal>,
    pub(crate) ast_map: HashMap<usize, ExprKind>,
}

pub trait MacroEnv {
    fn validate_identifier(&self, name: &str) -> bool;
}

impl Env {
    pub fn extract(&self, idx: usize) -> Option<SteelVal> {
        self.bindings_vec.get(idx).cloned()
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings_vec: Vec::new(),
            ast_map: HashMap::new(),
        }
    }

    pub(crate) fn print_diagnostics(&self) {
        for (idx, value) in self.bindings_vec.iter().enumerate() {
            if let SteelVal::Closure(b) = value {
                let count = b.call_count();
                if count > 0 {
                    println!("Function: {} - Count: {}", idx, b.call_count());
                }
            }
        }
    }

    // Appends the values from the map into the other
    pub(crate) fn add_hashmap(&mut self, map: HashMap<usize, ExprKind>) {
        for (key, value) in map {
            self.ast_map.insert(key, value);
        }
    }

    pub(crate) fn get_expr(&mut self, idx: usize) -> Option<&ExprKind> {
        self.ast_map.get(&idx)
    }

    /// Search starting from the current environment
    /// for `idx`, looking through the parent chain in order.
    ///
    /// if found, return that value
    ///
    /// Otherwise, error with `FreeIdentifier`
    // #[inline]
    pub fn repl_lookup_idx(&self, idx: usize) -> Result<SteelVal> {
        Ok(self.bindings_vec[idx].clone())
    }

    #[inline]
    pub fn repl_define_idx(&mut self, idx: usize, val: SteelVal) {
        if idx < self.bindings_vec.len() {
            self.bindings_vec[idx] = val;
        } else {
            self.bindings_vec.push(val);
            assert_eq!(self.bindings_vec.len() - 1, idx);
        }
    }

    pub fn repl_set_idx(&mut self, idx: usize, val: SteelVal) -> Result<SteelVal> {
        let output = self.bindings_vec[idx].clone();
        self.bindings_vec[idx] = val;
        Ok(output)
    }

    #[inline]
    pub fn add_root_value(&mut self, idx: usize, val: SteelVal) {
        // self.bindings_map.insert(idx, val);
        self.repl_define_idx(idx, val);
    }
}
