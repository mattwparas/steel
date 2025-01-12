use crate::gc::shared::{MutContainer, ShareableMut};
use crate::gc::{Shared, SharedMut};
use crate::parser::tryfrom_visitor::TryFromExprKindForSteelVal;
use crate::rvals::{into_serializable_value, Result, SerializableSteelVal, SteelVal};

use crate::parser::{
    ast::ExprKind,
    parser::{ParseError, Parser},
};

use std::collections::HashMap;
use std::sync::Arc;

use arc_swap::ArcSwap;
// TODO add the serializing and deserializing for constants
use serde::{Deserialize, Serialize};
use steel_parser::parser::{lower_entire_ast, SourceId};

// Shared constant map - for repeated in memory execution of a program, this is going to share the same
// underlying representation.
#[derive(Debug)]
pub struct ConstantMap {
    map: SharedMut<HashMap<SteelVal, usize>>,
    values: SharedMut<Vec<SteelVal>>,
    // TODO: Flush to these values after a compilation. - maybe have two of them to
    reified_values: Arc<ArcSwap<Vec<SteelVal>>>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializableConstantMap(Vec<u8>);

impl Default for ConstantMap {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for ConstantMap {
    fn clone(&self) -> Self {
        Self {
            values: Shared::clone(&self.values),
            map: Shared::clone(&self.map),
            reified_values: Arc::clone(&self.reified_values),
        }
    }
}

impl ConstantMap {
    pub fn new() -> ConstantMap {
        ConstantMap {
            values: Shared::new(MutContainer::new(Vec::new())),
            map: Shared::new(MutContainer::new(HashMap::new())),
            // Does this help at all?
            reified_values: Arc::new(ArcSwap::from_pointee(Vec::new())),
        }
    }

    pub fn flush(&self) {
        let values = self.values.read().clone();
        self.reified_values.store(Arc::new(values));
    }

    pub fn deep_clone(&self) -> ConstantMap {
        Self {
            map: Shared::new(MutContainer::new(
                self.map
                    .read()
                    .iter()
                    .map(|x| (x.0.clone(), x.1.clone()))
                    .collect(),
            )),
            values: Shared::new(MutContainer::new(
                self.values.read().iter().cloned().collect(),
            )),
            reified_values: Arc::new(ArcSwap::from_pointee(
                self.values.read().iter().cloned().collect(),
            )),
        }
    }

    pub(crate) fn into_serializable_map(self) -> SerializableConstantMap {
        SerializableConstantMap(self.to_bytes().unwrap())
    }

    pub fn to_serializable_vec(
        &self,
        serializer: &mut std::collections::HashMap<usize, SerializableSteelVal>,
        visited: &mut std::collections::HashSet<usize>,
    ) -> Vec<SerializableSteelVal> {
        self.values
            .read()
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, serializer, visited))
            .collect::<Result<_>>()
            .unwrap()
    }

    pub fn from_vec(vec: Vec<SteelVal>) -> ConstantMap {
        ConstantMap {
            map: Shared::new(MutContainer::new(
                vec.clone()
                    .into_iter()
                    .enumerate()
                    .map(|x| (x.1, x.0))
                    .collect(),
            )),
            values: Shared::new(MutContainer::new(vec)),
            reified_values: todo!(),
        }
    }

    fn to_constant_expr_map(&self) -> Vec<String> {
        self.values
            .read()
            .iter()
            .map(|x| match x {
                SteelVal::StringV(s) => {
                    format!("{:?}", s)
                }
                SteelVal::CharV(c) => {
                    format!("#\\{c}")
                }
                _ => x.to_string(),
            })
            .collect()
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>> {
        let str_vector = self.to_constant_expr_map();
        // println!("{:?}", str_vector);
        let result = bincode::serialize(&str_vector);

        Ok(result.unwrap())
    }

    pub fn from_serialized(map: SerializableConstantMap) -> Result<Self> {
        Self::from_bytes(&map.0)
    }

    pub fn from_bytes(encoded: &[u8]) -> Result<ConstantMap> {
        let str_vector: Vec<String> = bincode::deserialize(encoded).unwrap();

        str_vector
            .into_iter()
            .map(|x| {
                // Parse the input
                let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
                    Parser::new_flat(&x, SourceId::none()).collect();
                let mut parsed = parsed?;

                lower_entire_ast(&mut parsed[0])?;

                // println!("{}", &parsed[0]);

                TryFromExprKindForSteelVal::try_from_expr_kind(parsed[0].clone())

                // Ok(SteelVal::try_from(parsed[0].clone()).unwrap())
            })
            .collect::<Result<Vec<_>>>()
            .map(Self::from_vec)
    }

    // pub fn from_bytes(encoded: &[u8]) -> ConstantMap {
    //     bincode::deserialize(encoded).unwrap()
    // }
}

impl ConstantMap {
    pub fn add(&mut self, val: SteelVal) -> usize {
        let idx = self.len();
        self.values.write().push(val.clone());

        // TODO: Consider just storing the hash code, not the actual value.
        self.map.write().insert(val, idx);

        idx
    }

    // Fallible
    #[inline(always)]
    pub fn get(&self, idx: usize) -> SteelVal {
        self.values.read()[idx].clone()
    }

    pub fn get_value(&self, idx: usize) -> SteelVal {
        self.reified_values.load()[idx].clone()
    }

    pub fn try_get(&self, idx: usize) -> Option<SteelVal> {
        self.values.read().get(idx).cloned()
    }

    // Replace with existing constants if they already exist
    fn walk_constants(&mut self, val: &SteelVal) -> Option<SteelVal> {
        match val {
            SteelVal::ListV(l) => Some(SteelVal::ListV(
                l.iter()
                    .map(|value| {
                        let idx = self.add_or_get(value.clone());

                        self.get(idx)
                    })
                    .collect(),
            )),
            _ => None,
        }
    }

    // This is certainly not what we want. This time complexity is
    // questionable
    pub fn add_or_get(&mut self, mut val: SteelVal) -> usize {
        if let SteelVal::ListV(_) = &val {
            if let Some(new_list) = self.walk_constants(&val) {
                val = new_list;
            };
        }

        let idx = self.map.write().get(&val).copied();

        if let Some(idx) = idx {
            idx
        } else {
            self.add(val)
        }
    }

    pub fn len(&self) -> usize {
        self.values.read().len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.read().is_empty()
    }

    pub fn roll_back(&mut self, idx: usize) {
        self.values.write().truncate(idx);
    }

    #[cfg(test)]
    pub fn clear(&mut self) {
        self.values.write().clear()
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
