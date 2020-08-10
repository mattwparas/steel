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

    fn len(&self) -> usize {
        self.0.len()
    }

    #[cfg(test)]
    fn clear(&mut self) {
        self.0.clear()
    }
}

pub trait ConstantTable {
    fn add(&mut self, val: Rc<SteelVal>) -> usize;
    fn get(&self, idx: usize) -> Rc<SteelVal>;
    fn try_get(&self, idx: usize) -> Option<Rc<SteelVal>>;
    fn add_or_get(&mut self, val: Rc<SteelVal>) -> usize;
    fn len(&self) -> usize;

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
        let val1 = Rc::new(SteelVal::BoolV(true));
        let val2 = Rc::new(SteelVal::BoolV(false));
        assert_eq!(instance.add(val1), 0);
        assert_eq!(instance.add(val2), 1);
    }

    fn test_get<CT: ConstantTable>(instance: &mut CT) {
        assert_eq!(instance.len(), 0);
        let val1 = Rc::new(SteelVal::BoolV(true));
        let val2 = Rc::new(SteelVal::BoolV(false));
        assert_eq!(instance.add(val1), 0);
        assert_eq!(instance.add(val2), 1);

        assert_eq!(instance.get(0), Rc::new(SteelVal::BoolV(true)));
        assert_eq!(instance.get(1), Rc::new(SteelVal::BoolV(false)));
    }
}
