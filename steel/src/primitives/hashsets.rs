use crate::env::{FALSE, TRUE};
use crate::gc::Gc;
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use im_rc::HashSet;

use crate::primitives::ListOperations;
use crate::primitives::VectorOperations;

pub struct HashSetOperations {}

impl HashSetOperations {
    pub fn hs_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut hs = HashSet::new();

            for key in args {
                if key.is_hashable() {
                    hs.insert(Gc::clone(key));
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
            }

            Ok(Gc::new(SteelVal::HashSetV(hs)))
        })
    }

    pub fn hs_insert() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "set insert takes 2 arguments")
            }

            let hashset = Gc::clone(&args[0]);
            let key = Gc::clone(&args[1]);

            if let SteelVal::HashSetV(hs) = hashset.as_ref() {
                let mut hs = hs.clone();
                if key.is_hashable() {
                    hs.insert(key);
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
                Ok(Gc::new(SteelVal::HashSetV(hs)))
            } else {
                stop!(TypeMismatch => "set insert takes a set")
            }
        })
    }

    pub fn hs_contains() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "set-contains? get takes 2 arguments")
            }

            let hashset = Gc::clone(&args[0]);
            let key = &args[1];

            if let SteelVal::HashSetV(hm) = hashset.as_ref() {
                if key.is_hashable() {
                    if hm.contains(key) {
                        Ok(TRUE.with(|x| Gc::clone(x)))
                    } else {
                        Ok(FALSE.with(|x| Gc::clone(x)))
                    }
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
            } else {
                stop!(TypeMismatch => "set-contains? takes a hashmap")
            }
        })
    }

    // keys as list
    pub fn keys_to_list() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-keys->list takes 1 argument")
            }

            let hashset = Gc::clone(&args[0]);

            if let SteelVal::HashSetV(hs) = hashset.as_ref() {
                let keys = hs.iter().map(Gc::clone).collect::<Vec<Gc<SteelVal>>>();
                ListOperations::built_in_list_func_flat(&keys)
            } else {
                stop!(TypeMismatch => "hm-keys->list takes a hashmap")
            }
        })
    }

    // keys as vectors
    pub fn keys_to_vector() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-keys->vector takes 1 argument")
            }

            let hashset = Gc::clone(&args[0]);

            if let SteelVal::HashSetV(hs) = hashset.as_ref() {
                VectorOperations::vec_construct_iter_normal(hs.iter().map(Gc::clone))
            } else {
                stop!(TypeMismatch => "hm-keys->vector takes a hashmap")
            }
        })
    }

    pub fn clear() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hs-clear takes 1 argument")
            }

            let hashset = Gc::clone(&args[0]);

            if let SteelVal::HashSetV(hs) = hashset.as_ref() {
                let mut hs = hs.clone();
                hs.clear();
                Ok(Gc::new(SteelVal::HashSetV(hs)))
            } else {
                stop!(TypeMismatch => "hs-clear takes a hashmap")
            }
        })
    }

    pub fn list_to_hashset() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "list->hashset takes one argument")
            }
            if let SteelVal::Pair(_, _) = &args[0].as_ref() {
                let root = Gc::clone(&args[0]);
                let hashset: HashSet<Gc<SteelVal>> = SteelVal::iter(root).collect();
                Ok(Gc::new(SteelVal::HashSetV(hashset)))
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
    // use im_rc::hashset;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "hash tests"))
            .unwrap()(&args)
    }

    #[test]
    fn hs_construct_normal() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("foo2".to_string()),
            SteelVal::StringV("bar2".to_string()),
        ];
        let res = apply_function(HashSetOperations::hs_construct(), args);
        let expected = Gc::new(SteelVal::HashSetV(
            vec![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("foo2".to_string()),
                SteelVal::StringV("bar2".to_string()),
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
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("foo2".to_string()),
            SteelVal::StringV("bar2".to_string()),
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("foo2".to_string()),
            SteelVal::StringV("bar2".to_string()),
        ];
        let res = apply_function(HashSetOperations::hs_construct(), args);
        let expected = Gc::new(SteelVal::HashSetV(
            vec![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("foo2".to_string()),
                SteelVal::StringV("bar2".to_string()),
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
            SteelVal::HashSetV(vec![].into()),
            SteelVal::StringV("foo".to_string()),
        ];
        let res = apply_function(HashSetOperations::hs_insert(), args);
        let expected = Gc::new(SteelVal::HashSetV(
            vec![SteelVal::StringV("foo".to_string())]
                .into_iter()
                .map(Gc::new)
                .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_true() {
        let args = vec![
            SteelVal::HashSetV(
                vec![SteelVal::StringV("foo".to_string())]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
            SteelVal::StringV("foo".to_string()),
        ];
        let res = apply_function(HashSetOperations::hs_contains(), args);
        let expected = Gc::new(SteelVal::BoolV(true));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_false() {
        let args = vec![
            SteelVal::HashSetV(
                vec![SteelVal::StringV("foo".to_string())]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(HashSetOperations::hs_contains(), args);
        let expected = Gc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_keys_to_vector_normal() {
        let args = vec![SteelVal::HashSetV(
            vec![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string()),
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        )];
        let res = apply_function(HashSetOperations::keys_to_vector(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vec![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string()),
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));

        // pull out the vectors and sort them
        let unwrapped_res: SteelVal = (*res.unwrap()).clone();
        let unwrapped_expected: SteelVal = (*expected).clone();

        let mut res_vec_string: Vec<String> = if let SteelVal::VectorV(v) = &unwrapped_res {
            v.into_iter()
                .map(|x| {
                    if let SteelVal::StringV(s) = (*x).clone().as_ref() {
                        s.clone()
                    } else {
                        panic!("test failed")
                    }
                })
                .collect()
        } else {
            panic!("test failed")
        };

        let mut expected_vec_string: Vec<String> = if let SteelVal::VectorV(v) = &unwrapped_expected
        {
            v.into_iter()
                .map(|x| {
                    if let SteelVal::StringV(s) = (*x).clone().as_ref() {
                        s.clone()
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
