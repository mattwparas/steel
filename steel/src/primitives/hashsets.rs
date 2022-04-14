use crate::gc::Gc;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use im_lists::list::List;
use im_rc::HashSet;

use crate::primitives::VectorOperations;

pub struct HashSetOperations {}

impl HashSetOperations {
    pub fn hs_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let mut hs = HashSet::new();

            for key in args {
                if key.is_hashable() {
                    hs.insert(key.clone());
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
            }

            Ok(SteelVal::HashSetV(Gc::new(hs)))
        })
    }

    pub fn hs_length() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hs-length takes 1 argument")
            }

            let hashmap = &args[0];

            if let SteelVal::HashSetV(hm) = hashmap {
                Ok(SteelVal::IntV(hm.len() as isize))
            } else {
                stop!(TypeMismatch => "hs-length takes a hashmap")
            }
        })
    }

    pub fn hs_insert() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "set insert takes 2 arguments")
            }

            let hashset = &args[0];
            let key = &args[1];

            if let SteelVal::HashSetV(hs) = hashset {
                let mut hs = hs.unwrap();
                if key.is_hashable() {
                    hs.insert(key.clone());
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
                Ok(SteelVal::HashSetV(Gc::new(hs)))
            } else {
                stop!(TypeMismatch => "set insert takes a set")
            }
        })
    }

    pub fn hs_contains() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "set-contains? get takes 2 arguments")
            }

            let hashset = &args[0];
            let key = &args[1];

            if let SteelVal::HashSetV(hm) = hashset {
                if key.is_hashable() {
                    Ok(SteelVal::BoolV(hm.contains(key)))
                } else {
                    stop!(TypeMismatch => "hash key not hashable!: {}", key);
                }
            } else {
                stop!(TypeMismatch => "set-contains? takes a hashmap")
            }
        })
    }

    pub fn is_subset() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "hash-subset? takes 2 arguments")
            }

            let left = &args[0];
            let right = &args[1];

            if let SteelVal::HashSetV(left) = left {
                if let SteelVal::HashSetV(right) = right {
                    Ok(SteelVal::BoolV(left.is_subset(right.as_ref())))
                } else {
                    stop!(TypeMismatch => "hash-subset? takes a hashset")
                }
            } else {
                stop!(TypeMismatch => "hash-subset? takes a hashset")
            }
        })
    }

    // keys as list
    pub fn keys_to_list() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-keys->list takes 1 argument")
            }

            let hashset = &args[0];

            if let SteelVal::HashSetV(hs) = hashset {
                Ok(SteelVal::ListV(
                    hs.iter().cloned().collect::<List<SteelVal>>(),
                ))
            } else {
                stop!(TypeMismatch => "hm-keys->list takes a hashmap")
            }
        })
    }

    // keys as vectors
    pub fn keys_to_vector() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-keys->vector takes 1 argument")
            }

            let hashset = &args[0];

            if let SteelVal::HashSetV(hs) = hashset {
                VectorOperations::vec_construct_iter_normal(hs.iter().cloned())
            } else {
                stop!(TypeMismatch => "hm-keys->vector takes a hashmap")
            }
        })
    }

    pub fn clear() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hs-clear takes 1 argument")
            }

            let hashset = &args[0];

            if let SteelVal::HashSetV(hs) = hashset {
                let mut hs = hs.unwrap();
                hs.clear();
                Ok(SteelVal::HashSetV(Gc::new(hs)))
            } else {
                stop!(TypeMismatch => "hs-clear takes a hashmap")
            }
        })
    }

    pub fn list_to_hashset() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "list->hashset takes one argument")
            }
            if let SteelVal::ListV(l) = &args[0] {
                Ok(SteelVal::HashSetV(Gc::new(l.iter().cloned().collect())))
            } else {
                stop!(TypeMismatch => "list->hashset takes a hashset");
            }
        })
    }
}

#[cfg(test)]
mod hashset_tests {
    use super::*;
    use crate::throw;
    use std::rc::Rc;
    // use im_rc::hashset;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        func.func_or_else(throw!(BadSyntax => "hash tests"))
            .unwrap()(&args)
    }

    #[test]
    fn hs_construct_normal() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
        ];
        let res = apply_function(HashSetOperations::hs_construct(), args);
        let expected = SteelVal::HashSetV(Gc::new(
            vec![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("foo2".into()),
                SteelVal::StringV("bar2".into()),
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_construct_with_duplicates() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
        ];
        let res = apply_function(HashSetOperations::hs_construct(), args);
        let expected = SteelVal::HashSetV(Gc::new(
            vec![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("foo2".into()),
                SteelVal::StringV("bar2".into()),
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_insert_from_empty() {
        let args = vec![
            SteelVal::HashSetV(Gc::new(vec![].into())),
            SteelVal::StringV("foo".into()),
        ];
        let res = apply_function(HashSetOperations::hs_insert(), args);
        let expected = SteelVal::HashSetV(Gc::new(
            vec![SteelVal::StringV("foo".into())]
                .into_iter()
                .map(Gc::new)
                .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_true() {
        let args = vec![
            SteelVal::HashSetV(Gc::new(
                vec![SteelVal::StringV("foo".into())]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            )),
            SteelVal::StringV("foo".into()),
        ];
        let res = apply_function(HashSetOperations::hs_contains(), args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_false() {
        let args = vec![
            SteelVal::HashSetV(Gc::new(
                vec![SteelVal::StringV("foo".into())]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            )),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(HashSetOperations::hs_contains(), args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_keys_to_vector_normal() {
        let args = vec![SteelVal::HashSetV(Gc::new(
            vec![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into()),
            ]
            .into_iter()
            .collect(),
        ))];
        let res = apply_function(HashSetOperations::keys_to_vector(), args);
        let expected = SteelVal::VectorV(Gc::new(
            vec![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into()),
            ]
            .into_iter()
            .collect(),
        ));

        // pull out the vectors and sort them
        // let unwrapped_expected: SteelVal = (*expected).clone();

        let mut res_vec_string: Vec<Rc<str>> = if let SteelVal::VectorV(v) = res.unwrap() {
            v.iter()
                .map(|x| {
                    if let SteelVal::StringV(ref s) = x {
                        std::rc::Rc::clone(s)
                    } else {
                        panic!("test failed")
                    }
                })
                .collect()
        } else {
            panic!("test failed")
        };

        let mut expected_vec_string: Vec<Rc<str>> = if let SteelVal::VectorV(v) = expected {
            v.iter()
                .map(|x| {
                    if let SteelVal::StringV(ref s) = x {
                        std::rc::Rc::clone(s)
                    } else {
                        panic!("test failed")
                    }
                })
                .collect()
        } else {
            panic!("test failed")
        };

        res_vec_string.sort();
        expected_vec_string.sort();

        assert_eq!(res_vec_string, expected_vec_string);
    }
}
