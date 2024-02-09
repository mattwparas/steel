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
        {
            let ref this = self.bindings_vec[idx];
            match this {
                SteelVal::Closure(f0) => SteelVal::Closure(f0.clone()),
                SteelVal::BoolV(f0) => SteelVal::BoolV(f0.clone()),
                SteelVal::NumV(f0) => SteelVal::NumV(f0.clone()),
                SteelVal::IntV(f0) => SteelVal::IntV(f0.clone()),
                SteelVal::CharV(f0) => SteelVal::CharV(f0.clone()),
                SteelVal::VectorV(f0) => SteelVal::VectorV(f0.clone()),
                SteelVal::Void => SteelVal::Void,
                SteelVal::StringV(f0) => SteelVal::StringV(f0.clone()),
                SteelVal::FuncV(f0) => SteelVal::FuncV(f0.clone()),
                SteelVal::SymbolV(f0) => SteelVal::SymbolV(f0.clone()),
                SteelVal::Custom(f0) => SteelVal::Custom(f0.clone()),
                SteelVal::HashMapV(f0) => SteelVal::HashMapV(f0.clone()),
                SteelVal::HashSetV(f0) => SteelVal::HashSetV(f0.clone()),
                SteelVal::CustomStruct(f0) => SteelVal::CustomStruct(f0.clone()),
                SteelVal::PortV(f0) => SteelVal::PortV(f0.clone()),
                SteelVal::IterV(f0) => SteelVal::IterV(f0.clone()),
                SteelVal::ReducerV(f0) => SteelVal::ReducerV(f0.clone()),
                SteelVal::FutureFunc(f0) => SteelVal::FutureFunc(f0.clone()),
                SteelVal::FutureV(f0) => SteelVal::FutureV(f0.clone()),
                SteelVal::StreamV(f0) => SteelVal::StreamV(f0.clone()),
                SteelVal::BoxedFunction(f0) => SteelVal::BoxedFunction(f0.clone()),
                SteelVal::ContinuationFunction(f0) => SteelVal::ContinuationFunction(f0.clone()),
                SteelVal::ListV(f0) => SteelVal::ListV(f0.clone()),
                SteelVal::Pair(f0) => SteelVal::Pair(f0.clone()),
                SteelVal::MutFunc(f0) => SteelVal::MutFunc(f0.clone()),
                SteelVal::BuiltIn(f0) => SteelVal::BuiltIn(f0.clone()),
                SteelVal::MutableVector(f0) => SteelVal::MutableVector(f0.clone()),
                SteelVal::BoxedIterator(f0) => SteelVal::BoxedIterator(f0.clone()),
                SteelVal::SyntaxObject(f0) => SteelVal::SyntaxObject(f0.clone()),
                SteelVal::Boxed(f0) => SteelVal::Boxed(f0.clone()),
                SteelVal::HeapAllocated(f0) => SteelVal::HeapAllocated(f0.clone()),
                SteelVal::Reference(f0) => SteelVal::Reference(f0.clone()),
                SteelVal::BigNum(f0) => SteelVal::BigNum(f0.clone()),
            }
        }
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
