use crate::rerrs::SteelErr;
use crate::rvals::Result;

#[derive(Debug, PartialEq)]
pub struct SymbolMap(Vec<String>);

impl SymbolMap {
    pub fn new() -> Self {
        SymbolMap(Vec::new())
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
    pub fn get(&mut self, ident: &str) -> Result<usize> {
        let rev_iter = self.0.iter().enumerate().rev();

        for (idx, val) in rev_iter {
            // println!("{}", idx);
            if val == ident {
                return Ok(idx);
            }
        }
        let e = format!("{}", ident);
        stop!(FreeIdentifier => e)
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
}
