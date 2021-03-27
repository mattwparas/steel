use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::Result;
use crate::stop;
use crate::values::structs::StructFuncBuilder;

#[derive(Debug, PartialEq)]
pub struct SymbolMap(Vec<String>);

impl SymbolMap {
    pub fn new() -> Self {
        SymbolMap(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add(&mut self, ident: &str) -> usize {
        let idx = self.0.len();
        self.0.push(ident.to_string());
        // println!("`add`: {} @ {}", ident, idx);
        idx
    }

    pub fn copy_underlying_vec(&self) -> Vec<String> {
        self.0.clone()
    }

    pub fn get_or_add(&mut self, ident: &str) -> (usize, bool) {
        let rev_iter = self.0.iter().enumerate().rev();

        for (idx, val) in rev_iter {
            // println!("{}", idx);
            if val == ident {
                return (idx, false);
            }
        }

        let idx = self.0.len();
        self.0.push(ident.to_string());

        // println!("`get_or_add`: {} @ {}", ident, idx);
        // println!("Adding {} with index {}", ident, idx);
        // println!("{:?}", self.0);

        (idx, true)
    }

    // fallible
    pub fn get(&self, ident: &str) -> Result<usize> {
        let rev_iter = self.0.iter().enumerate().rev();

        for (idx, val) in rev_iter {
            // println!("{}", idx);
            if val == ident {
                return Ok(idx);
            }
        }
        let e = ident.to_string();
        // TODO come back to this
        // stop!(FreeIdentifier => e)
        Ok(0)
    }

    pub fn roll_back(&mut self, idx: usize) {
        // println!("Rolling back to: {}", idx);
        self.0.truncate(idx);

        // unimplemented!()
    }

    pub fn contains(&self, _ident: &str) -> bool {
        // self.seen.contains_key(ident)
        unimplemented!()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn insert_struct_function_names<'a>(
        &mut self,
        struct_builder: &'a StructFuncBuilder,
    ) -> Vec<usize> {
        let mut indices = Vec::new();

        // Constructor
        indices.push(self.add(&struct_builder.name));
        // Predicate
        indices.push(self.add(format!("{}?", &struct_builder.name).as_str()));
        for field in &struct_builder.fields {
            // Getter
            indices.push(self.add(format!("{}-{}", &struct_builder.name, field).as_str()));
            // Setter
            indices.push(self.add(format!("set-{}-{}!", &struct_builder.name, field).as_str()));
        }

        indices
    }
}
