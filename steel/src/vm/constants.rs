use crate::rvals::SteelVal;
use std::rc::Rc;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ConstantMap(Vec<Rc<SteelVal>>);

impl ConstantMap {
    pub fn new() -> ConstantMap {
        ConstantMap(Vec::new())
    }
}

impl ConstantTable for ConstantMap {
    fn add(&mut self, val: Rc<SteelVal>) -> usize {
        let idx = self.0.len();
        self.0.push(val);
        idx
    }

    // Fallible
    fn get(&self, idx: usize) -> Rc<SteelVal> {
        Rc::clone(&self.0[idx])
    }

    fn try_get(&self, idx: usize) -> Option<Rc<SteelVal>> {
        self.0.get(idx).map(Rc::clone)
    }

    fn add_or_get(&mut self, val: Rc<SteelVal>) -> usize {
        // unimplemented!()
        if let Some(idx) = self.0.iter().position(|x| Rc::clone(x) == val) {
            idx
        } else {
            self.add(val)
        }
    }
}

pub trait ConstantTable {
    fn add(&mut self, val: Rc<SteelVal>) -> usize;
    fn get(&self, idx: usize) -> Rc<SteelVal>;
    fn try_get(&self, idx: usize) -> Option<Rc<SteelVal>>;
    fn add_or_get(&mut self, val: Rc<SteelVal>) -> usize;
}
