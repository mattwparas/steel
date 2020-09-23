use crate::env::{FALSE, TRUE};
use crate::gc::Gc;
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use im_rc::HashMap;

use crate::primitives::ListOperations;
use crate::primitives::VectorOperations;

pub struct HashMapOperations {}

impl HashMapOperations {
    pub fn hm_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut hm = HashMap::new();

            let mut arg_iter = args.into_iter().map(Gc::clone);

            loop {
                match (arg_iter.next(), arg_iter.next()) {
                    (Some(key), Some(value)) => {
                        if key.is_hashable() {
                            hm.insert(key, value);
                        } else {
                            stop!(TypeMismatch => "hash key not hashable!");
                        }
                    }
                    (None, None) => break,
                    _ => {
                        stop!(ArityMismatch => "hash map must have a value for every key!");
                    }
                }
            }

            Ok(Gc::new(SteelVal::HashMapV(hm)))
        })
    }

    pub fn hm_insert() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 3 {
                stop!(ArityMismatch => "hm insert takes 3 arguments")
            }

            let hashmap = Gc::clone(&args[0]);
            let key = Gc::clone(&args[1]);
            let value = Gc::clone(&args[2]);

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                let mut hm = hm.clone();
                if key.is_hashable() {
                    hm.insert(key, value);
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
                Ok(Gc::new(SteelVal::HashMapV(hm)))
            } else {
                stop!(TypeMismatch => "hm insert takes a hashmap")
            }
        })
    }

    pub fn hm_get() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "hm get takes 2 arguments")
            }

            let hashmap = Gc::clone(&args[0]);
            let key = &args[1];

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                match hm.get(key) {
                    Some(v) => Ok(Gc::clone(v)),
                    None => stop!(Generic => "hash map key not found!"),
                }
            } else {
                stop!(TypeMismatch => "hm-insert takes a hashmap")
            }
        })
    }

    pub fn hm_try_get() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "hm get takes 2 arguments")
            }

            let hashmap = Gc::clone(&args[0]);
            let key = &args[1];

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                match hm.get(key) {
                    Some(v) => Ok(Gc::clone(v)),
                    None => Ok(FALSE.with(|x| Gc::clone(x))),
                }
            } else {
                stop!(TypeMismatch => "hm-insert takes a hashmap")
            }
        })
    }

    pub fn hm_contains() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "hm-contains? get takes 2 arguments")
            }

            let hashmap = Gc::clone(&args[0]);
            let key = &args[1];

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                if key.is_hashable() {
                    if hm.contains_key(key) {
                        Ok(TRUE.with(|x| Gc::clone(x)))
                    } else {
                        Ok(FALSE.with(|x| Gc::clone(x)))
                    }
                } else {
                    stop!(TypeMismatch => "hash key not hashable!");
                }
            } else {
                stop!(TypeMismatch => "hm-contains? takes a hashmap")
            }
        })
    }

    // keys as list
    pub fn keys_to_list() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-keys->list takes 1 argument")
            }

            let hashmap = Gc::clone(&args[0]);

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                let keys = hm.keys().map(Gc::clone).collect::<Vec<Gc<SteelVal>>>();
                ListOperations::built_in_list_func_flat(&keys)
            } else {
                stop!(TypeMismatch => "hm-keys->list takes a hashmap")
            }
        })
    }

    // values as list
    pub fn values_to_list() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-values->list takes 1 argument")
            }

            let hashmap = Gc::clone(&args[0]);

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                let keys = hm.values().map(Gc::clone).collect::<Vec<Gc<SteelVal>>>();
                ListOperations::built_in_list_func_flat(&keys)
            } else {
                stop!(TypeMismatch => "hm-values->list takes a hashmap")
            }
        })
    }

    // keys as vectors
    pub fn keys_to_vector() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-keys->vector takes 1 argument")
            }

            let hashmap = Gc::clone(&args[0]);

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                VectorOperations::vec_construct_iter_normal(hm.keys().map(Gc::clone))
            } else {
                stop!(TypeMismatch => "hm-keys->vector takes a hashmap")
            }
        })
    }

    pub fn values_to_vector() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-values->vector takes 1 argument")
            }

            let hashmap = Gc::clone(&args[0]);

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                VectorOperations::vec_construct_iter_normal(hm.values().map(Gc::clone))
            } else {
                stop!(TypeMismatch => "hm-values->vector takes a hashmap")
            }
        })
    }

    pub fn clear() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "hm-clear takes 1 argument")
            }

            let hashmap = Gc::clone(&args[0]);

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                let mut hm = hm.clone();
                hm.clear();
                Ok(Gc::new(SteelVal::HashMapV(hm)))
            } else {
                stop!(TypeMismatch => "hm-clear takes a hashmap")
            }
        })
    }
}

#[cfg(test)]
mod hashmap_tests {
    use super::*;
    use crate::throw;
    use im_rc::hashmap;

    use crate::rvals::SteelVal::*;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "hash tests"))
            .unwrap()(&args)
    }

    #[test]
    fn hm_construct_normal() {
        let args = vec![
            StringV("foo".to_string()),
            StringV("bar".to_string()),
            StringV("foo2".to_string()),
            StringV("bar2".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_construct(), args);
        let expected = Gc::new(SteelVal::HashMapV(hashmap! {
            Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string())),
            Gc::new(StringV("foo2".to_string())) => Gc::new(StringV("bar2".to_string()))
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_construct_with_duplicates() {
        let args = vec![
            StringV("foo".to_string()),
            StringV("bar".to_string()),
            StringV("foo2".to_string()),
            StringV("bar2".to_string()),
            StringV("foo".to_string()),
            StringV("bar".to_string()),
            StringV("foo2".to_string()),
            StringV("bar2".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_construct(), args);
        let expected = Gc::new(SteelVal::HashMapV(hashmap! {
            Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string())),
            Gc::new(StringV("foo2".to_string())) => Gc::new(StringV("bar2".to_string()))
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_insert_from_empty() {
        let args = vec![
            HashMapV(hashmap![]),
            StringV("foo".to_string()),
            StringV("bar".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_insert(), args);
        let expected = Gc::new(SteelVal::HashMapV(hashmap! {
            Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_get_found() {
        let args = vec![
            HashMapV(hashmap! {
                Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
            }),
            StringV("foo".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_get(), args);
        let expected = Gc::new(StringV("bar".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_get_error() {
        let args = vec![
            HashMapV(hashmap! {
                Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
            }),
            StringV("garbage".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_get(), args);
        assert!(res.is_err());
    }

    #[test]
    fn hm_try_get_found() {
        let args = vec![
            HashMapV(hashmap! {
                Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
            }),
            StringV("foo".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_try_get(), args);
        let expected = Gc::new(StringV("bar".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_try_get_error() {
        let args = vec![
            HashMapV(hashmap! {
                Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
            }),
            StringV("garbage".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_contains(), args);
        let expected = Gc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_contains_true() {
        let args = vec![
            HashMapV(hashmap! {
                Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
            }),
            StringV("foo".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_contains(), args);
        let expected = Gc::new(SteelVal::BoolV(true));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_contains_false() {
        let args = vec![
            HashMapV(hashmap! {
                Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string()))
            }),
            StringV("bar".to_string()),
        ];
        let res = apply_function(HashMapOperations::hm_contains(), args);
        let expected = Gc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_keys_to_vector_normal() {
        let args = vec![HashMapV(hashmap! {
            Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string())),
            Gc::new(StringV("bar".to_string())) => Gc::new(StringV("baz".to_string())),
            Gc::new(StringV("baz".to_string())) => Gc::new(StringV("quux".to_string()))
        })];
        let res = apply_function(HashMapOperations::keys_to_vector(), args);
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

    #[test]
    fn hm_values_to_vector_normal() {
        let args = vec![HashMapV(hashmap! {
            Gc::new(StringV("foo".to_string())) => Gc::new(StringV("bar".to_string())),
            Gc::new(StringV("bar".to_string())) => Gc::new(StringV("baz".to_string())),
            Gc::new(StringV("baz".to_string())) => Gc::new(StringV("quux".to_string()))
        })];
        let res = apply_function(HashMapOperations::values_to_vector(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vec![
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string()),
                SteelVal::StringV("quux".to_string()),
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
