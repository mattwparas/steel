use crate::rvals::SteelVal;
// use std::rc::Rc;
use crate::gc::Gc;

#[derive(Debug, PartialEq)]
pub struct ConstantMap(Vec<Gc<SteelVal>>);

impl ConstantMap {
    pub fn new() -> ConstantMap {
        ConstantMap(Vec::new())
    }
}

impl ConstantTable for ConstantMap {
    fn add(&mut self, val: Gc<SteelVal>) -> usize {
        let idx = self.0.len();
        self.0.push(val);
        idx
    }

    // Fallible
    fn get(&self, idx: usize) -> Gc<SteelVal> {
        Gc::clone(&self.0[idx])
    }

    fn try_get(&self, idx: usize) -> Option<Gc<SteelVal>> {
        self.0.get(idx).map(Gc::clone)
    }

    fn add_or_get(&mut self, val: Gc<SteelVal>) -> usize {
        // unimplemented!()
        if let Some(idx) = self.0.iter().position(|x| Gc::clone(x) == val) {
            idx
        } else {
            self.add(val)
        }
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn roll_back(&mut self, idx: usize) {
        self.0.truncate(idx);
    }

    #[cfg(test)]
    fn clear(&mut self) {
        self.0.clear()
    }
}

pub trait ConstantTable {
    fn add(&mut self, val: Gc<SteelVal>) -> usize;
    fn get(&self, idx: usize) -> Gc<SteelVal>;
    fn try_get(&self, idx: usize) -> Option<Gc<SteelVal>>;
    fn add_or_get(&mut self, val: Gc<SteelVal>) -> usize;
    fn len(&self) -> usize;
    fn roll_back(&mut self, idx: usize);
    fn is_empty(&self) -> bool;

    #[cfg(test)]
    fn clear(&mut self);
}

#[cfg(test)]
pub mod constant_table_tests {
    use super::*;

    #[test]
    fn run_tests_constant_map() {
        let mut instance = ConstantMap::new();
        test_add(&mut instance);

        let mut instance = ConstantMap::new();
        test_get(&mut instance);
    }

    fn test_add<CT: ConstantTable>(instance: &mut CT) {
        assert_eq!(instance.len(), 0);
        let val1 = Gc::new(SteelVal::BoolV(true));
        let val2 = Gc::new(SteelVal::BoolV(false));
        assert_eq!(instance.add(val1), 0);
        assert_eq!(instance.add(val2), 1);
    }

    fn test_get<CT: ConstantTable>(instance: &mut CT) {
        assert_eq!(instance.len(), 0);
        let val1 = Gc::new(SteelVal::BoolV(true));
        let val2 = Gc::new(SteelVal::BoolV(false));
        assert_eq!(instance.add(val1), 0);
        assert_eq!(instance.add(val2), 1);

        assert_eq!(instance.get(0), Gc::new(SteelVal::BoolV(true)));
        assert_eq!(instance.get(1), Gc::new(SteelVal::BoolV(false)));
    }
}
