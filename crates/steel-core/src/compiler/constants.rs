use crate::gc::shared::MutContainer;

// #[cfg(not(feature = "triomphe"))]
// use crate::gc::shared::ShareableMut;

#[cfg(not(feature = "sync"))]
use crate::gc::shared::ShareableMut;

use crate::gc::{Gc, Shared, SharedMut};
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{
    into_serializable_value, Result, SerializableSteelVal, SerializationContext, SteelByteVector,
    SteelComplex, SteelVal,
};
use crate::values::lists::Pair;

use alloc::sync::Arc;

use arc_swap::ArcSwap;
use num_bigint::BigInt;
use num_rational::{BigRational, Rational32};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

// Shared constant map - for repeated in memory execution of a program, this is going to share the same
// underlying representation.
#[derive(Debug)]
pub struct ConstantMap {
    map: SharedMut<FxHashMap<SteelVal, usize>>,
    values: SharedMut<Vec<SteelVal>>,
    // TODO: Flush to these values after a compilation. - maybe have two of them to
    reified_values: Arc<ArcSwap<Vec<SteelVal>>>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializableConstantMap(Vec<u8>);

#[derive(Serialize, Deserialize)]
enum SerializedConstant {
    Bool(bool),
    Int(isize),
    Float(f64),
    Rational(Rational32),
    BigInt(BigInt),
    BigRational(BigRational),
    Complex(Box<SerializedConstant>, Box<SerializedConstant>),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<SerializedConstant>),
    Pair(Box<SerializedConstant>, Box<SerializedConstant>),
    Vector(Vec<SerializedConstant>),
    ByteVector(Vec<u8>),
    Void,
}

impl SerializedConstant {
    fn from_value(value: &SteelVal) -> Result<SerializedConstant> {
        match value {
            SteelVal::BoolV(b) => Ok(SerializedConstant::Bool(*b)),
            SteelVal::IntV(i) => Ok(SerializedConstant::Int(*i)),
            SteelVal::NumV(n) => Ok(SerializedConstant::Float(*n)),
            SteelVal::Rational(r) => Ok(SerializedConstant::Rational(*r)),
            SteelVal::BigNum(b) => Ok(SerializedConstant::BigInt(b.as_ref().clone())),
            SteelVal::BigRational(r) => Ok(SerializedConstant::BigRational(r.as_ref().clone())),
            SteelVal::Complex(c) => Ok(SerializedConstant::Complex(
                Box::new(SerializedConstant::from_value(&c.re)?),
                Box::new(SerializedConstant::from_value(&c.im)?),
            )),
            SteelVal::CharV(c) => Ok(SerializedConstant::Char(*c)),
            SteelVal::StringV(s) => Ok(SerializedConstant::String(s.as_str().to_owned())),
            SteelVal::SymbolV(s) => Ok(SerializedConstant::Symbol(s.as_str().to_owned())),
            SteelVal::ListV(values) => Ok(SerializedConstant::List(
                values
                    .iter()
                    .map(SerializedConstant::from_value)
                    .collect::<Result<_>>()?,
            )),
            SteelVal::Pair(pair) => Ok(SerializedConstant::Pair(
                Box::new(SerializedConstant::from_value(&pair.car())?),
                Box::new(SerializedConstant::from_value(&pair.cdr())?),
            )),
            SteelVal::VectorV(values) => Ok(SerializedConstant::Vector(
                values
                    .iter()
                    .map(SerializedConstant::from_value)
                    .collect::<Result<_>>()?,
            )),
            SteelVal::ByteVector(bytes) => {
                Ok(SerializedConstant::ByteVector(bytes.vec.read().clone()))
            }
            SteelVal::Void => Ok(SerializedConstant::Void),
            unsupported => Err(SteelErr::new(
                ErrorKind::Generic,
                format!("constant map serialization does not support value: {unsupported}"),
            )),
        }
    }

    fn into_value(self) -> SteelVal {
        match self {
            SerializedConstant::Bool(b) => SteelVal::BoolV(b),
            SerializedConstant::Int(i) => SteelVal::IntV(i),
            SerializedConstant::Float(n) => SteelVal::NumV(n),
            SerializedConstant::Rational(r) => SteelVal::Rational(r),
            SerializedConstant::BigInt(b) => SteelVal::BigNum(Gc::new(b)),
            SerializedConstant::BigRational(r) => SteelVal::BigRational(Gc::new(r)),
            SerializedConstant::Complex(re, im) => {
                SteelVal::Complex(Gc::new(SteelComplex::new(re.into_value(), im.into_value())))
            }
            SerializedConstant::Char(c) => SteelVal::CharV(c),
            SerializedConstant::String(s) => SteelVal::StringV(s.into()),
            SerializedConstant::Symbol(s) => SteelVal::SymbolV(s.into()),
            SerializedConstant::List(values) => SteelVal::ListV(
                values
                    .into_iter()
                    .map(SerializedConstant::into_value)
                    .collect(),
            ),
            SerializedConstant::Pair(car, cdr) => {
                Pair::cons(car.into_value(), cdr.into_value()).into()
            }
            SerializedConstant::Vector(values) => SteelVal::VectorV(
                Gc::new(
                    values
                        .into_iter()
                        .map(SerializedConstant::into_value)
                        .collect::<crate::values::Vector<_>>(),
                )
                .into(),
            ),
            SerializedConstant::ByteVector(bytes) => {
                SteelVal::ByteVector(SteelByteVector::new(bytes))
            }
            SerializedConstant::Void => SteelVal::Void,
        }
    }
}

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
            map: Shared::new(MutContainer::new(FxHashMap::default())),
            // Does this help at all?
            reified_values: Arc::new(ArcSwap::from_pointee(Vec::new())),
        }
    }

    pub(crate) fn shares_storage_with(&self, other: &ConstantMap) -> bool {
        Shared::ptr_eq(&self.values, &other.values)
    }

    pub fn flush(&self) {
        let values = self.values.read();
        if values.len() != self.reified_values.load().len() {
            self.reified_values.store(Arc::new(values.clone()));
        }
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

    pub(crate) fn into_serializable_map(self) -> Result<SerializableConstantMap> {
        Ok(SerializableConstantMap(self.to_bytes()?))
    }

    pub fn to_serializable_vec(&self, ctx: &mut SerializationContext) -> Vec<SerializableSteelVal> {
        self.values
            .read()
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, ctx))
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
            values: Shared::new(MutContainer::new(vec.clone())),
            reified_values: Arc::new(ArcSwap::from_pointee(vec)),
        }
    }

    pub fn to_bytes(&self) -> Result<Vec<u8>> {
        let constants = self
            .values
            .read()
            .iter()
            .map(SerializedConstant::from_value)
            .collect::<Result<Vec<_>>>()?;

        bincode::serialize(&constants).map_err(|e| {
            SteelErr::new(
                ErrorKind::Generic,
                format!("unable to serialize the constant map: {e}"),
            )
        })
    }

    pub fn from_serialized(map: SerializableConstantMap) -> Result<Self> {
        Self::from_bytes(&map.0)
    }

    pub fn from_bytes(encoded: &[u8]) -> Result<ConstantMap> {
        let constants: Vec<SerializedConstant> = bincode::deserialize(encoded).map_err(|e| {
            SteelErr::new(
                ErrorKind::Generic,
                format!("unable to deserialize the constant map: {e}"),
            )
        })?;

        Ok(Self::from_vec(
            constants
                .into_iter()
                .map(SerializedConstant::into_value)
                .collect(),
        ))
    }
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
        // Just check if the values are the same. Otherwise, push down to the proper one?

        // if let Some(value) = self.local_values.get(idx) {
        //     return value.clone();
        // } else {
        //     self.local_values = self.reified_values.load().to_vec();
        //     self.local_values[idx].clone()
        // }

        self.values.read()[idx].clone()
    }

    pub fn get_value(&self, idx: usize) -> SteelVal {
        self.reified_values.load()[idx].clone()
        // self.values.read()[idx].clone()
    }

    pub fn get_map<T>(&self, idx: usize, func: impl FnOnce(&SteelVal) -> T) -> T {
        // let value = &self.values.read()[idx];
        let value = &self.reified_values.load()[idx];
        func(value)
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
            SteelVal::VectorV(v) => Some(SteelVal::VectorV(
                v.iter()
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
        if let SteelVal::ListV(_) | SteelVal::VectorV(_) = &val {
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

    use crate::gc::Gc;
    use crate::rvals::SteelByteVector;
    use crate::values::lists::Pair;
    use num_bigint::BigInt;
    use num_rational::{BigRational, Rational32};

    fn list_of(values: Vec<SteelVal>) -> SteelVal {
        SteelVal::ListV(values.into())
    }

    fn symbol(name: &str) -> SteelVal {
        SteelVal::SymbolV(name.into())
    }

    fn representative_constants() -> Vec<SteelVal> {
        vec![
            symbol("plain-symbol"),
            list_of(vec![symbol("quote"), symbol("inner-symbol")]),
            list_of(vec![
                symbol("a"),
                list_of(vec![
                    symbol("quote"),
                    list_of(vec![symbol("b"), SteelVal::IntV(1)]),
                ]),
                SteelVal::StringV("nested".into()),
            ]),
            list_of(vec![
                symbol("let"),
                list_of(vec![list_of(vec![symbol("x"), SteelVal::IntV(1)])]),
                symbol("x"),
            ]),
            list_of(vec![
                symbol("%plain-let"),
                list_of(vec![list_of(vec![symbol("y"), SteelVal::CharV('y')])]),
                symbol("y"),
            ]),
            SteelVal::StringV("line one\nline \"two\" \\ λ \t".into()),
            SteelVal::CharV('\n'),
            SteelVal::CharV(' '),
            SteelVal::CharV('λ'),
            SteelVal::CharV('\\'),
            SteelVal::IntV(-42),
            SteelVal::IntV(isize::MAX),
            SteelVal::NumV(2.5),
            SteelVal::NumV(-0.0),
            SteelVal::BoolV(true),
            SteelVal::BoolV(false),
            SteelVal::Rational(Rational32::new(1, 3)),
            SteelVal::BigNum(Gc::new(BigInt::from(isize::MAX) * 16)),
            SteelVal::BigRational(Gc::new(BigRational::new(
                BigInt::from(isize::MAX) * 4,
                BigInt::from(3),
            ))),
            SteelVal::ByteVector(SteelByteVector::new(vec![0, 1, 254, 255])),
            SteelVal::VectorV(
                Gc::new(
                    vec![symbol("vec-elem"), SteelVal::IntV(7)]
                        .into_iter()
                        .collect::<crate::values::Vector<_>>(),
                )
                .into(),
            ),
            SteelVal::Pair(Gc::new(Pair::cons(symbol("car-part"), symbol("cdr-part")))),
            SteelVal::Void,
        ]
    }

    #[test]
    fn round_trip_preserves_constants() {
        let constants = representative_constants();
        let map = ConstantMap::from_vec(constants.clone());

        let bytes = map.to_bytes().unwrap();
        let restored = ConstantMap::from_bytes(&bytes).unwrap();

        assert_eq!(restored.len(), constants.len());

        for (index, expected) in constants.iter().enumerate() {
            assert_eq!(
                restored.try_get(index).unwrap(),
                *expected,
                "constant {index} did not survive the round trip"
            );
        }
    }

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
