use crate::rvals::{Result, SteelVal};

use crate::parser::{
    ast::ExprKind,
    parser::{ParseError, Parser},
};

use std::{cell::RefCell, convert::TryFrom, rc::Rc};

// TODO add the serializing and deserializing for constants
// use serde::{Deserialize, Serialize};

// Shared constant map - for repeated in memory execution of a program, this is going to share the same
// underlying representation.
#[derive(Debug, PartialEq)]
pub struct ConstantMap(Rc<RefCell<Vec<SteelVal>>>);

// #[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
// struct ConstantExprMap {
//     map: Vec<Expr>,
// }
impl Default for ConstantMap {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for ConstantMap {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl ConstantMap {
    pub fn new() -> ConstantMap {
        ConstantMap(Rc::new(RefCell::new(Vec::new())))
    }

    // There might be a better way of doing this - but provide this as an option
    // in the event we want a deep clone of the constant map
    pub fn deep_clone(&self) -> ConstantMap {
        ConstantMap(Rc::new(RefCell::new(
            self.0.borrow().iter().cloned().collect(),
        )))
    }

    pub fn from_vec(vec: Vec<SteelVal>) -> ConstantMap {
        ConstantMap(Rc::new(RefCell::new(vec)))
    }

    fn to_constant_expr_map(&self) -> Vec<String> {
        let result: std::result::Result<Vec<_>, _> =
            self.0.borrow().iter().map(ExprKind::try_from).collect();

        result.unwrap().into_iter().map(|x| x.to_string()).collect()
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>> {
        // let vector_val = self.to_constant_expr_map()?;

        let str_vector = self.to_constant_expr_map();
        let result = bincode::serialize(&str_vector);

        Ok(result.unwrap())
    }

    pub fn from_bytes(encoded: &[u8]) -> Result<ConstantMap> {
        let str_vector: Vec<String> = bincode::deserialize(encoded).unwrap();

        str_vector
            .into_iter()
            .map(|x| {
                // Parse the input
                let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
                    Parser::new(&x, None).collect();
                let parsed = parsed?;

                Ok(SteelVal::try_from(parsed[0].clone()).unwrap())
            })
            .collect::<Result<Vec<_>>>()
            .map(|x| ConstantMap(Rc::new(RefCell::new(x))))
    }

    // pub fn from_bytes(encoded: &[u8]) -> ConstantMap {
    //     bincode::deserialize(encoded).unwrap()
    // }
}

impl ConstantMap {
    pub fn add(&mut self, val: SteelVal) -> usize {
        let idx = self.len();
        self.0.borrow_mut().push(val);
        idx
    }

    // Fallible
    #[inline(always)]
    pub fn get(&self, idx: usize) -> SteelVal {
        self.0.borrow()[idx].clone()
    }

    pub fn try_get(&self, idx: usize) -> Option<SteelVal> {
        self.0.borrow().get(idx).cloned()
    }

    pub fn add_or_get(&mut self, val: SteelVal) -> usize {
        let idx = { self.0.borrow_mut().iter().position(|x| x == &val) };

        if let Some(idx) = idx {
            idx
        } else {
            self.add(val)
        }
    }

    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.borrow().is_empty()
    }

    pub fn roll_back(&mut self, idx: usize) {
        self.0.borrow_mut().truncate(idx);
    }

    #[cfg(test)]
    pub fn clear(&mut self) {
        self.0.borrow_mut().clear()
    }
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

    fn test_add(instance: &mut ConstantMap) {
        assert_eq!(instance.len(), 0);
        let val1 = SteelVal::BoolV(true);
        let val2 = SteelVal::BoolV(false);
        assert_eq!(instance.add(val1), 0);
        assert_eq!(instance.add(val2), 1);
    }

    fn test_get(instance: &mut ConstantMap) {
        assert_eq!(instance.len(), 0);
        let val1 = SteelVal::BoolV(true);
        let val2 = SteelVal::BoolV(false);
        assert_eq!(instance.add(val1), 0);
        assert_eq!(instance.add(val2), 1);

        assert_eq!(instance.get(0), SteelVal::BoolV(true));
        assert_eq!(instance.get(1), SteelVal::BoolV(false));
    }
}
