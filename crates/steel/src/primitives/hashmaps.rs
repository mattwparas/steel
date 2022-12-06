use crate::{core::utils::declare_const_ref_functions, gc::Gc};
use crate::{
    rvals::{Result, SteelVal},
    steel_vm::builtin::BuiltInModule,
};
use crate::{steel_vm::builtin::DocTemplate, stop};
use im_rc::HashMap;

use crate::primitives::VectorOperations;

use crate::primitives::utils::SliceExt;

declare_const_ref_functions!(
    HM_CONSTRUCT => hm_construct,
    HM_INSERT => hm_insert,
    HM_GET => hm_get,
    HM_TRY_GET => hm_try_get,
    HM_LENGTH => hm_length,
    HM_CONTAINS => hm_contains,
    HM_KEYS_TO_LIST => keys_to_list,
    HM_VALUES_TO_LIST => values_to_list,
    HM_KEYS_TO_VEC => keys_to_vector,
    HM_VALUES_TO_VEC => values_to_vector,
    HM_CLEAR => clear,
    HM_EMPTY => hm_empty,
    HM_UNION => hm_union,
);

pub(crate) fn hashmap_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/hash".to_string());
    module
        .register_value_with_doc("hash", HM_CONSTRUCT, HASH_DOC)
        .register_value_with_doc("hash-insert", HM_INSERT, HASH_INSERT_DOC)
        .register_value_with_doc("hash-get", HM_GET, HASH_GET_DOC)
        .register_value("hash-try-get", HM_TRY_GET)
        .register_value_with_doc("hash-length", HM_LENGTH, HASH_LENGTH_DOC)
        .register_value_with_doc("hash-contains?", HM_CONTAINS, HASH_CONTAINS_DOC)
        .register_value("hash-keys->list", HM_KEYS_TO_LIST)
        .register_value("hash-keys->vector", HM_KEYS_TO_VEC)
        .register_value("hash-values->list", HM_VALUES_TO_LIST)
        .register_value("hash-values->vector", HM_VALUES_TO_VEC)
        .register_value("hash-clear", HM_CLEAR)
        .register_value("hash-empty?", HM_EMPTY)
        .register_value("hash-union", HM_UNION);
    module
}

pub struct HashMapOperations {}

const HASH_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash key val ...) -> hash?",
    params: &["key : hashable?", "val : any/c"],
    description: r#"Creates an immutable hash table with each given `key` mapped to the following `val; each key must have a val, so the total number of arguments must be even.
    
Note, the key must be hashable."#,
    examples: &[(
        "> (hash 'a 10 'b 20)",
        r#"=> #<hashmap {
        'a: 10,
        'b: 20,
    }>"#,
    )],
};

pub fn hm_construct(args: &[SteelVal]) -> Result<SteelVal> {
    let mut hm = HashMap::new();

    let mut arg_iter = args.iter().cloned();

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

    Ok(SteelVal::HashMapV(Gc::new(hm)))
}

const HASH_INSERT_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash-insert map key val) -> hash?",
    params: &["map : hash?", "key : any/c", "val : any/c"],
    description: r#"Returns a new hashmap with the additional key value pair added. Performs a functional update, so the old hash map is still accessible."#,
    examples: &[(
        "> (hash-insert (hash 'a 10 'b 20) 'c 30)",
        r#"=> #<hashmap {
        'a: 10,
        'b: 20,
        'c: 30
    }>"#,
    )],
};

pub fn hm_insert(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 3 {
        stop!(ArityMismatch => "hm insert takes 3 arguments")
    }

    let hashmap = args.get_clone(0);
    let key = args.get_clone(1);
    let value = args.get_clone(2);

    if let SteelVal::HashMapV(hm) = hashmap {
        let mut hm = hm.unwrap();
        if key.is_hashable() {
            hm.insert(key, value);
        } else {
            stop!(TypeMismatch => "hash key not hashable!");
        }
        Ok(SteelVal::HashMapV(Gc::new(hm)))
    } else {
        stop!(TypeMismatch => "hm insert takes a hashmap")
    }
}

const HASH_GET_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash-get map key) -> any/c?",
    params: &["map : hash?", "key : any/c"],
    description: r#"Gets the `key` from the given `map`. Raises an error if the key does not exist"#,
    examples: &[("> (hash-get (hash 'a 10 'b 20) 'b)", r#"=> 20"#)],
};

pub fn hm_get(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "hm get takes 2 arguments")
    }

    let hashmap = &args[0];
    let key = &args[1];

    if let SteelVal::HashMapV(hm) = hashmap {
        match hm.get(key) {
            Some(v) => Ok(v.clone()),
            None => stop!(Generic => "hash map key not found!"),
        }
    } else {
        stop!(TypeMismatch => "hm-get takes a hashmap, found: {}", hashmap)
    }
}

pub fn hm_try_get(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "hm get takes 2 arguments")
    }

    let hashmap = &args[0];
    let key = &args[1];

    if let SteelVal::HashMapV(hm) = hashmap {
        match hm.get(key) {
            Some(v) => Ok(v.clone()),
            None => Ok(SteelVal::BoolV(false)),
        }
    } else {
        stop!(TypeMismatch => format!("hash-try-get takes a hashmap, found: {}", hashmap))
    }
}

const HASH_LENGTH_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash-length map) -> (and positive? int?)",
    params: &["map : hash?"],
    description: r#"Returns the number of key value pairs in the map."#,
    examples: &[("> (hash-length (hash 'a 10 'b 20))", r#"=> 2"#)],
};

pub fn hm_length(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hm-length takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        Ok(SteelVal::IntV(hm.len() as isize))
    } else {
        stop!(TypeMismatch => "hm-length takes a hashmap")
    }
}

const HASH_CONTAINS_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash-contains? map key) -> bool?",
    params: &["map : hash?", "key : hashable?"],
    description: r#"Checks whether the given map contains the given key. Key must be hashable."#,
    examples: &[
        ("> (hash-contains? (hash 'a 10 'b 20) 'a)", r#"=> #true"#),
        (
            "> (hash-contains? (hash 'a 10 'b 20) 'not-there)",
            r#"=> #false"#,
        ),
    ],
};

pub fn hm_contains(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "hm-contains? get takes 2 arguments")
    }

    let hashmap = &args[0];
    let key = &args[1];

    if let SteelVal::HashMapV(hm) = hashmap {
        if key.is_hashable() {
            if hm.contains_key(key) {
                Ok(SteelVal::BoolV(true))
            } else {
                Ok(SteelVal::BoolV(false))
            }
        } else {
            stop!(TypeMismatch => "hash key not hashable!");
        }
    } else {
        stop!(TypeMismatch => "hm-contains? takes a hashmap")
    }
}

pub fn keys_to_list(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hm-keys->list takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        Ok(SteelVal::ListV(hm.keys().cloned().collect()))
    } else {
        stop!(TypeMismatch => "hm-keys->list takes a hashmap")
    }
}

// values as list
pub fn values_to_list(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hm-values->list takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        // let keys = hm.values().cloned().collect::<Vec<SteelVal>>();
        // ListOperations::built_in_list_func_flat(&keys)
        Ok(SteelVal::ListV(hm.values().cloned().collect()))
    } else {
        stop!(TypeMismatch => "hm-values->list takes a hashmap")
    }
}

pub fn keys_to_vector(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hm-keys->vector takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        VectorOperations::vec_construct_iter_normal(hm.keys().cloned())
    } else {
        stop!(TypeMismatch => "hm-keys->vector takes a hashmap")
    }
}

pub fn values_to_vector(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hm-values->vector takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        VectorOperations::vec_construct_iter_normal(hm.values().cloned())
    } else {
        stop!(TypeMismatch => "hm-values->vector takes a hashmap")
    }
}

pub fn clear(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hm-clear takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        let mut hm = hm.unwrap();
        hm.clear();
        Ok(SteelVal::HashMapV(Gc::new(hm)))
    } else {
        stop!(TypeMismatch => "hm-clear takes a hashmap")
    }
}

pub fn hm_empty(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hash-empty? takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashMapV(hm) = hashmap {
        Ok(SteelVal::BoolV(hm.is_empty()))
    } else {
        stop!(TypeMismatch => "hash-empty? takes a hashmap")
    }
}

pub fn hm_union(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "hash-union takes 2 arguments")
    }

    let left = &args[0];
    let right = &args[1];

    if let SteelVal::HashMapV(hml) = left {
        if let SteelVal::HashMapV(hmr) = right {
            let hml = hml.unwrap();
            let hmr = hmr.unwrap();
            Ok(SteelVal::HashMapV(Gc::new(hml.union(hmr))))
        } else {
            stop!(TypeMismatch => "hash-union takes a hashmap, found {}", right)
        }
    } else {
        stop!(TypeMismatch => "hash-union takes a hashmap, found: {}", left)
    }
}

#[cfg(test)]
mod hashmap_tests {
    use super::*;
    use im_rc::hashmap;

    use crate::rvals::{SteelString, SteelVal::*};

    #[test]
    fn hm_construct_normal() {
        let args = [
            StringV("foo".into()),
            StringV("bar".into()),
            StringV("foo2".into()),
            StringV("bar2".into()),
        ];
        let res = hm_construct(&args);
        let expected = SteelVal::HashMapV(Gc::new(hashmap! {
            StringV("foo".into()) => StringV("bar".into()),
            StringV("foo2".into()) => StringV("bar2".into())
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_construct_with_duplicates() {
        let args = [
            StringV("foo".into()),
            StringV("bar".into()),
            StringV("foo2".into()),
            StringV("bar2".into()),
            StringV("foo".into()),
            StringV("bar".into()),
            StringV("foo2".into()),
            StringV("bar2".into()),
        ];
        let res = hm_construct(&args);
        let expected = SteelVal::HashMapV(Gc::new(hashmap! {
            StringV("foo".into()) => StringV("bar".into()),
            StringV("foo2".into()) => StringV("bar2".into())
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_insert_from_empty() {
        let args = [
            HashMapV(Gc::new(hashmap![])),
            StringV("foo".into()),
            StringV("bar".into()),
        ];
        let res = hm_insert(&args);
        let expected = SteelVal::HashMapV(Gc::new(hashmap! {
            StringV("foo".into()) => StringV("bar".into())
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_get_found() {
        let args = [
            HashMapV(Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })),
            StringV("foo".into()),
        ];
        let res = hm_get(&args);
        let expected = StringV("bar".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_get_error() {
        let args = [
            HashMapV(Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })),
            StringV("garbage".into()),
        ];
        let res = hm_get(&args);
        assert!(res.is_err());
    }

    #[test]
    fn hm_try_get_found() {
        let args = [
            HashMapV(Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })),
            StringV("foo".into()),
        ];
        let res = hm_try_get(&args);
        let expected = StringV("bar".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_try_get_error() {
        let args = [
            HashMapV(Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })),
            StringV("garbage".into()),
        ];
        let res = hm_contains(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_contains_true() {
        let args = [
            HashMapV(Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })),
            StringV("foo".into()),
        ];
        let res = hm_contains(&args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_contains_false() {
        let args = [
            HashMapV(Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })),
            StringV("bar".into()),
        ];
        let res = hm_contains(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_keys_to_vector_normal() {
        let args = vec![HashMapV(Gc::new(hashmap! {
            StringV("foo".into()) => StringV("bar".into()),
            StringV("bar".into()) => StringV("baz".into()),
            StringV("baz".into()) => StringV("quux".into())
        }))];
        let res = keys_to_vector(&args);
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
        // let unwrapped_res: SteelVal = (*res.unwrap()).clone();
        // let unwrapped_expected: SteelVal = (*expected).clone();

        let mut res_vec_string: Vec<SteelString> = if let SteelVal::VectorV(v) = res.unwrap() {
            v.iter()
                .map(|x| {
                    if let SteelVal::StringV(ref s) = x {
                        s.clone()
                    } else {
                        panic!("test failed")
                    }
                })
                .collect()
        } else {
            panic!("test failed")
        };

        let mut expected_vec_string: Vec<SteelString> = if let SteelVal::VectorV(v) = expected {
            v.iter()
                .map(|x| {
                    if let SteelVal::StringV(ref s) = x {
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
        let args = vec![HashMapV(Gc::new(hashmap! {
            StringV("foo".into()) => StringV("bar".into()),
            StringV("bar".into()) => StringV("baz".into()),
            StringV("baz".into()) => StringV("quux".into())
        }))];
        let res = values_to_vector(&args);
        let expected = SteelVal::VectorV(Gc::new(
            vec![
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into()),
                SteelVal::StringV("quux".into()),
            ]
            .into_iter()
            .collect(),
        ));

        // pull out the vectors and sort them

        let mut res_vec_string: Vec<SteelString> = if let SteelVal::VectorV(v) = res.unwrap() {
            v.iter()
                .map(|x| {
                    if let SteelVal::StringV(ref s) = x {
                        s.clone()
                    } else {
                        panic!("test failed")
                    }
                })
                .collect()
        } else {
            panic!("test failed")
        };

        let mut expected_vec_string: Vec<SteelString> = if let SteelVal::VectorV(v) = expected {
            v.iter()
                .map(|x| {
                    if let SteelVal::StringV(ref s) = x {
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
