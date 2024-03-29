use crate::rvals::{Result, SteelVal};

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Env {
    pub(crate) bindings_vec: Vec<SteelVal>,
}

impl Env {
    pub fn extract(&self, idx: usize) -> Option<SteelVal> {
        self.bindings_vec.get(idx).cloned()
    }

    pub fn len(&self) -> usize {
        self.bindings_vec.len()
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings_vec: Vec::with_capacity(1024),
        }
    }

    // pub fn len(&self) -> usize {
    //     self.bindings_vec.len()
    // }

    #[cfg(feature = "dynamic")]
    pub(crate) fn _print_diagnostics(&self) {
        for (idx, value) in self.bindings_vec.iter().enumerate() {
            if let SteelVal::Closure(b) = value {
                let count = b.call_count();
                if count > 0 {
                    println!("Function: {} - Count: {}", idx, b.call_count());
                }
            }
        }
    }

    #[inline(always)]
    pub fn repl_lookup_idx(&self, idx: usize) -> SteelVal {
        self.bindings_vec[idx].clone()
    }

    /// Get the value located at that index
    pub fn _repl_get_idx(&self, idx: usize) -> &SteelVal {
        &self.bindings_vec[idx]
    }

    #[inline]
    pub fn repl_define_idx(&mut self, idx: usize, val: SteelVal) {
        if idx < self.bindings_vec.len() {
            self.bindings_vec[idx] = val;
        } else {
            if idx > self.bindings_vec.len() {
                // TODO: This seems suspect. Try to understand
                // what is happening here. This would be that values
                // are getting interned to be at a global offset in the
                // wrong order, which seems to be fine in general,
                // assuming that the values then get actually updated
                // to the correct values.
                for _ in 0..(idx - self.bindings_vec.len()) {
                    self.bindings_vec.push(SteelVal::Void);
                }
            }

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

    pub fn roots(&self) -> impl Iterator<Item = &SteelVal> {
        self.bindings_vec.iter()
    }
}
