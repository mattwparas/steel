use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::values::lists::List;

use crate::{
    gc::Gc,
    rerrs::ErrorKind,
    rvals::{AsRefSteelValFromUnsized, FromSteelVal, IntoSteelVal, Result},
    SteelErr, SteelVal,
};
use std::borrow::Cow;

#[cfg(feature = "anyhow")]
mod anyhow_conversion {
    use crate::{rvals::IntoSteelVal, SteelVal};

    impl IntoSteelVal for anyhow::Error {
        fn into_steelval(self) -> crate::rvals::Result<crate::SteelVal> {
            Ok(SteelVal::StringV(format!("{:#?}", self).into()))
        }
    }
}

impl IntoSteelVal for SteelVal {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(self)
    }
}

// impl IntoSteelVal for Result<SteelVal> {
//     fn into_steelval(self) -> Result<SteelVal> {
//         self
//     }
// }

impl FromSteelVal for Gc<im_rc::HashMap<SteelVal, SteelVal, FxBuildHasher>> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::HashMapV(hm) = val {
            Ok(hm.0.clone())
        } else {
            stop!(TypeMismatch => "Unable to convert Steelval to HashMap, found: {}", val);
        }
    }
}

// impl IntoSteelVal for str {
//     fn into_steelval(self) -> Result<SteelVal> {
//         Ok(SteelVal::StringV(self.to_string().into()))
//     }
// }

// impl<T: IntoSteelVal + Clone> IntoSteelVal for Cow<'_, T> {
//     fn into_steelval(self) -> Result<SteelVal> {
//         match self {
//             Cow::Borrowed(b) => b.clone().into_steelval(),
//             Cow::Owned(o) => o.into_steelval(),
//         }
//     }
// }

impl IntoSteelVal for Cow<'_, str> {
    fn into_steelval(self) -> Result<SteelVal> {
        match self {
            Cow::Borrowed(b) => b.into_steelval(),
            Cow::Owned(o) => o.into_steelval(),
        }
    }
}

impl FromSteelVal for Cow<'_, str> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::StringV(s) = val {
            Ok(Cow::Owned(s.to_string()))
        } else {
            stop!(TypeMismatch => "expected string, found {:?}", val)
        }
    }
}

impl<T: IntoSteelVal + Clone> IntoSteelVal for &[T] {
    fn into_steelval(self) -> Result<SteelVal> {
        self.iter()
            .map(|x| x.clone().into_steelval())
            .collect::<Result<List<_>>>()
            .map(SteelVal::ListV)
    }
}

// TODO: @Matt - figure out how to get this to actually return the right value. At the moment,
// it seems I can't get this to return the correct value
impl<T: FromSteelVal + Clone> AsRefSteelValFromUnsized<T> for T {
    type Output = Vec<T>;

    fn as_ref_from_unsized(val: &SteelVal) -> Result<Self::Output> {
        if let SteelVal::ListV(v) = val {
            v.iter()
                .map(|x| T::from_steelval(x))
                .collect::<Result<Vec<_>>>()
        } else {
            stop!(TypeMismatch => "expected list, found: {:?}", val);
        }
    }
}

impl<A: IntoSteelVal, B: IntoSteelVal> IntoSteelVal for (A, B) {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(SteelVal::ListV(
            vec![self.0.into_steelval()?, self.1.into_steelval()?].into(),
        ))
    }
}

// Vectors should translate into vectors in rust
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

impl<T: FromSteelVal> FromSteelVal for Box<[T]> {
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

impl FromSteelVal for Box<str> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::StringV(s) = val {
            Ok(s.as_str().into())
        } else {
            stop!(TypeMismatch => "Unable to convert {} into Box<str>", val);
        }
    }
}

// HashMap
impl<K: IntoSteelVal, V: IntoSteelVal> IntoSteelVal for FxHashMap<K, V> {
    fn into_steelval(mut self) -> Result<SteelVal> {
        let mut hm = im_rc::HashMap::<SteelVal, SteelVal, FxBuildHasher>::default();
        for (key, val) in self.drain() {
            hm.insert(key.into_steelval()?, val.into_steelval()?);
        }
        Ok(SteelVal::HashMapV(Gc::new(hm).into()))
    }
}

impl<K: FromSteelVal + Eq + std::hash::Hash, V: FromSteelVal> FromSteelVal for FxHashMap<K, V> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        // todo!()
        if let SteelVal::HashMapV(hm) = val {
            let mut h = FxHashMap::default();
            for (key, value) in hm.0.unwrap().into_iter() {
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

impl<A: FromSteelVal, B: FromSteelVal> FromSteelVal for (A, B) {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::ListV(l) = val {
            if l.len() != 2 {
                return Err(SteelErr::new(ErrorKind::ConversionError, format!("Could not convert steelval to (A, B): {:?} - list was not length of 2, found length: {}", val, l.len())));
            }

            Ok((
                A::from_steelval(l.get(0).unwrap())?,
                B::from_steelval(l.get(1).unwrap())?,
            ))
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                format!("Could not convert SteelVal to (A, B): {:?}", val),
            ))
        }
    }
}

// HashSet
impl<K: IntoSteelVal> IntoSteelVal for FxHashSet<K> {
    fn into_steelval(mut self) -> Result<SteelVal> {
        let mut hs = im_rc::HashSet::new();
        for value in self.drain() {
            hs.insert(value.into_steelval()?);
        }
        Ok(SteelVal::HashSetV(Gc::new(hs).into()))
    }
}

impl<K: FromSteelVal + Eq + std::hash::Hash> FromSteelVal for FxHashSet<K> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::HashSetV(hs) = val {
            let mut h = FxHashSet::default();
            for k in hs.0.unwrap().into_iter() {
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

    use im_rc::vector;

    #[test]
    fn vec_into_list() {
        let input_vec = vec![1, 2];
        // let expected = SteelVal::Pair(Gc::new(ConsCell::new(
        //     SteelVal::IntV(1),
        //     Some(Gc::new(ConsCell::new(SteelVal::IntV(2), None))),
        // )));

        let expected = SteelVal::ListV(vec![SteelVal::IntV(1), SteelVal::IntV(2)].into());

        assert_eq!(input_vec.into_steelval().unwrap(), expected)
    }

    #[test]
    fn vec_from_list() {
        let input_list = SteelVal::ListV(vec![SteelVal::IntV(1), SteelVal::IntV(2)].into());

        let expected = vec![1, 2];
        let result = <Vec<i32>>::from_steelval(&input_list).unwrap();

        assert_eq!(result, expected)
    }

    #[test]
    fn vec_from_vector() {
        let input_vector = vector![SteelVal::IntV(1), SteelVal::IntV(2)].into();

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
        let mut input = FxHashMap::default();
        input.insert("foo".to_string(), "bar".to_string());
        input.insert("foo2".to_string(), "bar2".to_string());

        let expected = SteelVal::HashMapV(
            Gc::new(im_rc::HashMap::<_, _, FxBuildHasher>::from(vec![
                (
                    SteelVal::StringV("foo".into()),
                    SteelVal::StringV("bar".into()),
                ),
                (
                    SteelVal::StringV("foo2".into()),
                    SteelVal::StringV("bar2".into()),
                ),
            ]))
            .into(),
        );

        assert_eq!(input.into_steelval().unwrap(), expected);
    }

    #[test]
    fn hashmap_from_steelval_hashmap() {
        let input = SteelVal::HashMapV(
            Gc::new(im_rc::HashMap::<_, _, FxBuildHasher>::from(vec![
                (
                    SteelVal::StringV("foo".into()),
                    SteelVal::StringV("bar".into()),
                ),
                (
                    SteelVal::StringV("foo2".into()),
                    SteelVal::StringV("bar2".into()),
                ),
            ]))
            .into(),
        );

        let mut expected = FxHashMap::default();
        expected.insert("foo".to_string(), "bar".to_string());
        expected.insert("foo2".to_string(), "bar2".to_string());

        assert_eq!(
            <FxHashMap<String, String>>::from_steelval(&input).unwrap(),
            expected
        );
    }

    #[test]
    fn hashset_into_steelval() {
        let mut input = FxHashSet::default();
        input.insert("foo".to_string());
        input.insert("bar".to_string());

        let expected = SteelVal::HashSetV(
            Gc::new(im_rc::hashset![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
            ])
            .into(),
        );

        assert_eq!(input.into_steelval().unwrap(), expected);
    }

    #[test]
    fn hashset_from_steelval_hashset() {
        let input = SteelVal::HashSetV(
            Gc::new(im_rc::hashset![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
            ])
            .into(),
        );

        let mut expected = FxHashSet::default();
        expected.insert("foo".to_string());
        expected.insert("bar".to_string());

        assert_eq!(
            <FxHashSet<String>>::from_steelval(&input).unwrap(),
            expected
        );
    }
}
