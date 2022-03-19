use im_lists::list::List;

use crate::{
    gc::Gc,
    rerrs::ErrorKind,
    rvals::{FromSteelVal, IntoSteelVal, Result},
    SteelErr, SteelVal,
};
use std::collections::{HashMap, HashSet};

impl IntoSteelVal for SteelVal {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(self)
    }
}

impl<T: IntoSteelVal + Clone> IntoSteelVal for List<T> {
    fn into_steelval(self) -> Result<SteelVal> {
        self.into_iter()
            .map(|x| x.into_steelval())
            .collect::<Result<List<_>>>()
            .map(SteelVal::ListV)
    }
}

// Vectors
impl<T: IntoSteelVal> IntoSteelVal for Vec<T> {
    fn into_steelval(self) -> Result<SteelVal> {
        let vec_vals: Result<Vec<SteelVal>> = self.into_iter().map(|x| x.into_steelval()).collect();

        match vec_vals {
            Ok(l) => Ok(SteelVal::ListV(l.into())),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert vector of values to SteelVal list".to_string(),
            )),
        }
    }
}

impl<T: FromSteelVal> FromSteelVal for Vec<T> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        match val {
            SteelVal::ListV(l) => {
                let result_vec_vals: Result<Self> = l
                    .into_iter()
                    .map(|x| FromSteelVal::from_steelval(x))
                    .collect();

                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            }
            // SteelVal::Pair(_) => {
            //     let result_vec_vals: Result<Self> = SteelVal::iter(val.clone())
            //         .into_iter()
            //         .map(FromSteelVal::from_steelval)
            //         .collect();

            //     match result_vec_vals {
            //         Ok(x) => Ok(x),
            //         _ => Err(SteelErr::new(
            //             ErrorKind::ConversionError,
            //             "Could not convert SteelVal list to Vector of values".to_string(),
            //         )),
            //     }
            // }
            SteelVal::VectorV(v) => {
                let result_vec_vals: Result<Self> =
                    v.iter().map(FromSteelVal::from_steelval).collect();
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
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        // todo!()
        if let SteelVal::HashMapV(hm) = val {
            let mut h = HashMap::new();
            for (key, value) in hm.unwrap().into_iter() {
                h.insert(K::from_steelval(&key)?, V::from_steelval(&value)?);
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
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::HashSetV(hs) = val {
            let mut h = HashSet::new();
            for k in hs.unwrap().into_iter() {
                h.insert(K::from_steelval(&k)?);
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

#[cfg(test)]
mod conversion_tests {
    use super::*;
    use im_lists::list;
    use im_rc::vector;

    #[test]
    fn vec_into_list() {
        let input_vec = vec![1, 2];
        // let expected = SteelVal::Pair(Gc::new(ConsCell::new(
        //     SteelVal::IntV(1),
        //     Some(Gc::new(ConsCell::new(SteelVal::IntV(2), None))),
        // )));

        let expected = list![SteelVal::IntV(1), SteelVal::IntV(2)].into();

        assert_eq!(input_vec.into_steelval().unwrap(), expected)
    }

    #[test]
    fn vec_from_list() {
        let input_list = SteelVal::ListV(list![SteelVal::IntV(1), SteelVal::IntV(2)]);

        let expected = vec![1, 2];
        let result = <Vec<i32>>::from_steelval(&input_list).unwrap();

        assert_eq!(result, expected)
    }

    #[test]
    fn vec_from_vector() {
        let input_vector =
            SteelVal::VectorV(Gc::new(vector![SteelVal::IntV(1), SteelVal::IntV(2)]));

        let expected = vec![1, 2];
        let result = <Vec<i32>>::from_steelval(&input_vector).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn vec_from_steelval_error() {
        let input = SteelVal::IntV(2);
        assert!(<Vec<i32>>::from_steelval(&input).is_err());
    }

    #[test]
    fn hashmap_into_steelval() {
        let mut input = HashMap::new();
        input.insert("foo".to_string(), "bar".to_string());
        input.insert("foo2".to_string(), "bar2".to_string());

        let expected = SteelVal::HashMapV(Gc::new(im_rc::hashmap! {
            SteelVal::StringV("foo".into()) => SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()) => SteelVal::StringV("bar2".into())
        }));

        assert_eq!(input.into_steelval().unwrap(), expected);
    }

    #[test]
    fn hashmap_from_steelval_hashmap() {
        let input = SteelVal::HashMapV(Gc::new(im_rc::hashmap! {
            SteelVal::StringV("foo".into()) => SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()) => SteelVal::StringV("bar2".into())
        }));

        let mut expected = HashMap::new();
        expected.insert("foo".to_string(), "bar".to_string());
        expected.insert("foo2".to_string(), "bar2".to_string());

        assert_eq!(
            <HashMap<String, String>>::from_steelval(&input).unwrap(),
            expected
        );
    }

    #[test]
    fn hashset_into_steelval() {
        let mut input = HashSet::new();
        input.insert("foo".to_string());
        input.insert("bar".to_string());

        let expected = SteelVal::HashSetV(Gc::new(im_rc::hashset! {
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        }));

        assert_eq!(input.into_steelval().unwrap(), expected);
    }

    #[test]
    fn hashset_from_steelval_hashset() {
        let input = SteelVal::HashSetV(Gc::new(im_rc::hashset! {
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        }));

        let mut expected = HashSet::new();
        expected.insert("foo".to_string());
        expected.insert("bar".to_string());

        assert_eq!(<HashSet<String>>::from_steelval(&input).unwrap(), expected);
    }
}
