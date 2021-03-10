use crate::{
    primitives::ListOperations,
    rerrs::ErrorKind,
    rvals::{FromSteelVal, IntoSteelVal, Result},
    Gc, SteelErr, SteelVal,
};
use std::collections::{HashMap, HashSet};

// Vectors
impl<T: IntoSteelVal> IntoSteelVal for Vec<T> {
    fn into_steelval(self) -> Result<SteelVal> {
        let vec_vals: Result<Vec<SteelVal>> = self.into_iter().map(|x| x.into_steelval()).collect();

        match vec_vals {
            Ok(l) => ListOperations::built_in_list_func_flat(&l),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert vector of values to SteelVal list".to_string(),
            )),
        }
    }
}

impl<T: FromSteelVal> FromSteelVal for Vec<T> {
    fn from_steelval(val: SteelVal) -> Result<Self> {
        match val {
            SteelVal::Pair(_) => {
                let result_vec_vals: Result<Self> = SteelVal::iter(val.clone())
                    .into_iter()
                    .map(FromSteelVal::from_steelval)
                    .collect();

                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            }
            SteelVal::VectorV(v) => {
                let result_vec_vals: Result<Self> = v
                    .iter()
                    .map(|x| FromSteelVal::from_steelval(x.clone()))
                    .collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            } // TODO
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal list to Vector of values".to_string(),
            )),
        }
    }
}

// HashMap
impl<K: IntoSteelVal, V: IntoSteelVal> IntoSteelVal for HashMap<K, V> {
    fn into_steelval(mut self) -> Result<SteelVal> {
        let mut hm = im_rc::HashMap::new();
        for (key, val) in self.drain() {
            hm.insert(key.into_steelval()?, val.into_steelval()?);
        }
        Ok(SteelVal::HashMapV(Gc::new(hm)))
    }
}

impl<K: FromSteelVal + Eq + std::hash::Hash, V: FromSteelVal> FromSteelVal for HashMap<K, V> {
    fn from_steelval(val: SteelVal) -> Result<Self> {
        // todo!()
        if let SteelVal::HashMapV(hm) = val {
            let mut h = HashMap::new();
            for (key, value) in hm.unwrap().into_iter() {
                h.insert(K::from_steelval(key)?, V::from_steelval(value)?);
            }
            Ok(h)
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal to HashMap".to_string(),
            ))
        }
    }
}

// BTreeMap
// impl<K: IntoSteelVal, V: IntoSteelVal> IntoSteelVal for BTreeMap<K, V> {
//     fn into_steelval(self) -> Result<SteelVal> {
//         let mut hm = im_rc::HashMap::new();
//         for (key, val) in self.drain() {
//             hm.insert(key.into_steelval()?, val.into_steelval()?);
//         }
//         Ok(SteelVal::HashMapV(Gc::new(hm)))
//     }
// }

// impl<K: FromSteelVal, V: FromSteelVal> FromSteelVal for BTreeMap<K, V> {
//     fn from_steelval(val: SteelVal) -> Result<Self> {
//         todo!()
//     }
// }

// HashSet
impl<K: IntoSteelVal> IntoSteelVal for HashSet<K> {
    fn into_steelval(mut self) -> Result<SteelVal> {
        let mut hs = im_rc::HashSet::new();
        for value in self.drain() {
            hs.insert(value.into_steelval()?);
        }
        Ok(SteelVal::HashSetV(Gc::new(hs)))
    }
}

impl<K: FromSteelVal + Eq + std::hash::Hash> FromSteelVal for HashSet<K> {
    fn from_steelval(val: SteelVal) -> Result<Self> {
        if let SteelVal::HashSetV(hs) = val {
            let mut h = HashSet::new();
            for k in hs.unwrap().into_iter() {
                h.insert(K::from_steelval(k)?);
            }
            Ok(h)
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal to HashSet".to_string(),
            ))
        }
    }
}

// BTreeSet
// impl<K: IntoSteelVal> IntoSteelVal for BTreeSet<K> {
//     fn into_steelval(self) -> Result<SteelVal> {
//         todo!()
//     }
// }

// impl<K: FromSteelVal> FromSteelVal for BTreeSet<K> {
//     fn from_steelval(val: SteelVal) -> Result<Self> {
//         todo!()
//     }
// }
