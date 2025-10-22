use crate::values::lists::List;
#[cfg(test)]
use alloc::string::String;
use alloc::{borrow::Cow, boxed::Box, format, vec, vec::Vec};

use crate::collections::{
    HashMap as ImmutableHashMap, HashSet as ImmutableHashSet, MutableHashMap, MutableHashSet,
};
use crate::{
    gc::Gc,
    rerrs::ErrorKind,
    rvals::{AsRefSteelValFromUnsized, FromSteelVal, IntoSteelVal, Result},
    SteelErr, SteelVal,
};

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

impl FromSteelVal for Gc<ImmutableHashMap<SteelVal, SteelVal>> {
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
                "Could not convert vector of values to SteelVal list".into(),
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
                        "Could not convert SteelVal list to Vector of values".into(),
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
            //             "Could not convert SteelVal list to Vector of values".into(),
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
                        "Could not convert SteelVal list to Vector of values".into(),
                    )),
                }
            } // TODO
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal list to Vector of values".into(),
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
                        "Could not convert SteelVal list to Vector of values".into(),
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
                        "Could not convert SteelVal list to Vector of values".into(),
                    )),
                }
            } // TODO
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal list to Vector of values".into(),
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
impl<K: IntoSteelVal, V: IntoSteelVal> IntoSteelVal for MutableHashMap<K, V> {
    fn into_steelval(self) -> Result<SteelVal> {
        let mut hm = ImmutableHashMap::new();
        for (key, val) in self.into_iter() {
            hm.insert(key.into_steelval()?, val.into_steelval()?);
        }
        Ok(SteelVal::HashMapV(Gc::new(hm).into()))
    }
}

impl<K: FromSteelVal + Eq + core::hash::Hash, V: FromSteelVal> FromSteelVal
    for MutableHashMap<K, V>
{
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        // todo!()
        if let SteelVal::HashMapV(hm) = val {
            let mut h = MutableHashMap::default();
            for (key, value) in hm.0.unwrap().into_iter() {
                h.insert(K::from_steelval(&key)?, V::from_steelval(&value)?);
            }
            Ok(h)
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal to HashMap".into(),
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

#[cfg(feature = "std")]
impl<K, V> IntoSteelVal for std::collections::HashMap<K, V>
where
    K: IntoSteelVal + std::hash::Hash + Eq,
    V: IntoSteelVal,
{
    fn into_steelval(self) -> Result<SteelVal> {
        let mutable: MutableHashMap<K, V> = self.into_iter().collect();
        mutable.into_steelval()
    }
}

#[cfg(feature = "std")]
impl<K, V> FromSteelVal for std::collections::HashMap<K, V>
where
    K: FromSteelVal + Eq + std::hash::Hash,
    V: FromSteelVal,
{
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        let map: MutableHashMap<K, V> = <MutableHashMap<K, V> as FromSteelVal>::from_steelval(val)?;
        Ok(map.into_iter().collect())
    }
}

// HashSet
impl<K: IntoSteelVal> IntoSteelVal for MutableHashSet<K> {
    fn into_steelval(self) -> Result<SteelVal> {
        let mut hs = ImmutableHashSet::new();
        for value in self.into_iter() {
            hs.insert(value.into_steelval()?);
        }
        Ok(SteelVal::HashSetV(Gc::new(hs).into()))
    }
}

#[cfg(feature = "std")]
impl<K> IntoSteelVal for std::collections::HashSet<K>
where
    K: IntoSteelVal + std::hash::Hash + Eq,
{
    fn into_steelval(self) -> Result<SteelVal> {
        let mutable: MutableHashSet<K> = self.into_iter().collect();
        mutable.into_steelval()
    }
}

#[cfg(feature = "std")]
impl<K> FromSteelVal for std::collections::HashSet<K>
where
    K: FromSteelVal + Eq + std::hash::Hash,
{
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        let set: MutableHashSet<K> = <MutableHashSet<K> as FromSteelVal>::from_steelval(val)?;
        Ok(set.into_iter().collect())
    }
}

impl<K: FromSteelVal + Eq + core::hash::Hash> FromSteelVal for MutableHashSet<K> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::HashSetV(hs) = val {
            let mut h = MutableHashSet::default();
            for k in hs.0.unwrap().into_iter() {
                h.insert(K::from_steelval(&k)?);
            }
            Ok(h)
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal to HashSet".into(),
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

    #[cfg(feature = "std")]
    use std::collections::{HashMap, HashSet};

    #[cfg(not(feature = "sync"))]
    use im_rc::vector;

    #[cfg(not(feature = "sync"))]
    use im_rc::hashmap;

    #[cfg(not(feature = "sync"))]
    use im_rc::hashset;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::vector;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::hashmap;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::hashset;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::vector;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::hashmap;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::hashset;

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

    #[cfg(feature = "std")]
    #[test]
    fn hashmap_into_steelval() {
        let mut input: HashMap<String, String> = HashMap::new();
        input.insert(String::from("foo"), String::from("bar"));
        input.insert(String::from("foo2"), String::from("bar2"));

        let expected = SteelVal::HashMapV(
            Gc::new(hashmap! {
                SteelVal::StringV("foo".into()) => SteelVal::StringV("bar".into()),
                SteelVal::StringV("foo2".into()) => SteelVal::StringV("bar2".into())
            })
            .into(),
        );

        assert_eq!(input.into_steelval().unwrap(), expected);
    }

    #[cfg(feature = "std")]
    #[test]
    fn hashmap_from_steelval_hashmap() {
        let input = SteelVal::HashMapV(
            Gc::new(hashmap! {
                SteelVal::StringV("foo".into()) => SteelVal::StringV("bar".into()),
                SteelVal::StringV("foo2".into()) => SteelVal::StringV("bar2".into())
            })
            .into(),
        );

        let mut expected: HashMap<String, String> = HashMap::new();
        expected.insert(String::from("foo"), String::from("bar"));
        expected.insert(String::from("foo2"), String::from("bar2"));

        assert_eq!(
            <HashMap<String, String>>::from_steelval(&input).unwrap(),
            expected
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn hashset_into_steelval() {
        let mut input: HashSet<String> = HashSet::new();
        input.insert(String::from("foo"));
        input.insert(String::from("bar"));

        let expected = SteelVal::HashSetV(
            Gc::new(hashset! {
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into())
            })
            .into(),
        );

        assert_eq!(input.into_steelval().unwrap(), expected);
    }

    #[cfg(feature = "std")]
    #[test]
    fn hashset_from_steelval_hashset() {
        let input = SteelVal::HashSetV(
            Gc::new(hashset! {
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into())
            })
            .into(),
        );

        let mut expected: HashSet<String> = HashSet::new();
        expected.insert(String::from("foo"));
        expected.insert(String::from("bar"));

        assert_eq!(<HashSet<String>>::from_steelval(&input).unwrap(), expected);
    }
}
