use crate::{core::utils::declare_const_ref_functions, gc::Gc};
use crate::{
    rvals::{Result, SteelVal},
    steel_vm::builtin::BuiltInModule,
};
use crate::{steel_vm::builtin::DocTemplate, stop};
use im_rc::HashMap;

use crate::primitives::VectorOperations;

use steel_derive::function;

declare_const_ref_functions!(
    HM_CONSTRUCT => hm_construct,
    HM_INSERT => steel_hash_insert,
    HM_GET => steel_hash_ref,
    // HM_TRY_GET => hm_try_get,
    // HM_LENGTH => hm_length,
    // HM_CONTAINS => steel_hash_contains,
    // HM_KEYS_TO_LIST => keys_to_list,
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
        .register_value("%keyword-hash", SteelVal::FuncV(hm_construct_keywords))
        .register_native_fn_definition(HASH_INSERT_DEFINITION)
        .register_native_fn_definition(HASH_REF_DEFINITION)
        .register_value("hash-get", SteelVal::FuncV(steel_hash_ref))
        .register_native_fn_definition(HASH_TRY_GET_DEFINITION)
        .register_native_fn_definition(HASH_LENGTH_DEFINITION)
        .register_native_fn_definition(HASH_CONTAINS_DEFINITION)
        .register_native_fn_definition(KEYS_TO_LIST_DEFINITION)
        .register_value("hash-keys->vector", HM_KEYS_TO_VEC)
        .register_value_with_doc(
            "hash-values->list",
            HM_VALUES_TO_LIST,
            HASH_VALUES_TO_LIST_DOC,
        )
        .register_value("hash-values->vector", HM_VALUES_TO_VEC)
        .register_value("hash-clear", HM_CLEAR)
        .register_value("hash-empty?", HM_EMPTY)
        .register_value("hash-union", HM_UNION);
    module
}

const HASH_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash key val ...) -> hash?",
    params: &["key : hashable?", "val : any/c"],
    description: r#"Creates an immutable hash table with each given `key` mapped to the following `val`; each key must have a val, so the total number of arguments must be even.
    
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

    Ok(SteelVal::HashMapV(Gc::new(hm).into()))
}

pub fn hm_construct_keywords(args: &[SteelVal]) -> Result<SteelVal> {
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
                stop!(ArityMismatch => "Missing keyword argument!");
            }
        }
    }

    Ok(SteelVal::HashMapV(Gc::new(hm).into()))
}

/// Returns a new hashmap with the additional key value pair added. Performs a functional update,
/// so the old hash map is still accessible.
///
/// (hash-insert map key val) -> hash?
///
/// * map : hash?
/// * key : any/c
/// * val : any/c
///
/// # Examples
/// ```scheme
/// > (hash-insert (hash 'a 10 'b 20) 'c 30)
///
/// => #<hashmap {
///         'a: 10,
///         'b: 20,
///         'c: 30
///     }>
/// ```
#[function(name = "hash-insert")]
pub fn hash_insert(
    map: &Gc<HashMap<SteelVal, SteelVal>>,
    key: SteelVal,
    value: SteelVal,
) -> Result<SteelVal> {
    if key.is_hashable() {
        Ok(SteelVal::HashMapV(Gc::new(map.update(key, value)).into()))
    } else {
        stop!(TypeMismatch => "hash key not hashable: {:?}", key)
    }
}

/// Gets the `key` from the given `map`. Raises an error if the key does not exist. `hash-get` is an alias for this.
///
/// (hash-ref map key) -> any/c
///
/// * map : hash?
/// * key : any/c
///
/// # Examples
/// ```scheme
/// > (hash-ref (hash 'a 10 'b 20) 'b) ;; => 20
/// ```
#[function(name = "hash-ref")]
pub fn hash_ref(map: &Gc<HashMap<SteelVal, SteelVal>>, key: &SteelVal) -> Result<SteelVal> {
    if key.is_hashable() {
        match map.get(key) {
            Some(value) => Ok(value.clone()),
            None => stop!(Generic => "key not found in hash map: {}", key),
        }
    } else {
        stop!(TypeMismatch => "key not hashable: {}", key)
    }
}

/// Gets the `key` from the given `map`. Returns #false if the key does not exist.
///
/// (hash-try-get map key) -> (or any/c #false)
///
/// * map : hash?
/// * key : any/c
///
/// # Examples
///
/// ```scheme
/// > (hash-try-get (hash 'a 10 'b 20) 'b) ;; => 20
/// > (hash-try-get (hash 'a 10 'b 20) 'does-not-exist) ;; => #false
/// ```
#[function(name = "hash-try-get")]
pub fn hash_try_get(map: &Gc<HashMap<SteelVal, SteelVal>>, key: &SteelVal) -> SteelVal {
    match map.get(key) {
        Some(v) => v.clone(),
        None => SteelVal::BoolV(false),
    }
}

/// Returns the number of key value pairs in the map
///
/// (hash-length map) -> (and positive? int?)
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash-length (hash 'a 10 'b 20)) ;; => 2
/// ```
#[function(name = "hash-length")]
pub fn hash_length(map: &Gc<HashMap<SteelVal, SteelVal>>) -> usize {
    map.len()
}

/// Checks whether the given map contains the given key. Key must be hashable.
///
/// (hash-contains? map key) -> bool?
///
/// * map : hash?
/// * key : hashable?
///
/// # Example
///
/// ```scheme
/// > (hash-contains? (hash 'a 10 'b 20) 'a) ;; => #true
/// > (hash-contains? (hash 'a 10 'b 20) 'not-there) ;; => #false
/// ```
#[function(name = "hash-contains?")]
pub fn hash_contains(map: &Gc<HashMap<SteelVal, SteelVal>>, key: &SteelVal) -> Result<SteelVal> {
    if key.is_hashable() {
        Ok(SteelVal::BoolV(map.contains_key(key)))
    } else {
        stop!(TypeMismatch => "hash key not hashable!");
    }
}

/// Returns the keys of the given hash map as a list.
///
/// ```scheme
/// (hash-keys->list map) -> (listof hashable?)
/// ```
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash-keys->list? (hash 'a 'b 20)) ;; => '(a b)
/// ```
#[function(name = "hash-keys->list")]
pub fn keys_to_list(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    Ok(SteelVal::ListV(hashmap.keys().cloned().collect()))
}

const HASH_VALUES_TO_LIST_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(hash-values->list? map) -> (listof any/c)?",
    params: &["map : hash?"],
    description: r#"Returns the values of the given hash map as a list"#,
    examples: &[(
        "> (hash-values->list? (hash 'a 10 'b 20) 'a)",
        r#"=> '(10 20)"#,
    )],
};

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
        let mut hm = hm.0.unwrap();
        hm.clear();
        Ok(SteelVal::HashMapV(Gc::new(hm).into()))
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
            let hml = hml.0.unwrap();
            let hmr = hmr.0.unwrap();
            Ok(SteelVal::HashMapV(Gc::new(hml.union(hmr)).into()))
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
        let expected = SteelVal::HashMapV(
            Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into()),
                StringV("foo2".into()) => StringV("bar2".into())
            })
            .into(),
        );
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
        let expected = SteelVal::HashMapV(
            Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into()),
                StringV("foo2".into()) => StringV("bar2".into())
            })
            .into(),
        );
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_insert_from_empty() {
        let args = [
            HashMapV(Gc::new(hashmap![]).into()),
            StringV("foo".into()),
            StringV("bar".into()),
        ];
        let res = steel_hash_insert(&args);
        let expected = SteelVal::HashMapV(
            Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into())
            })
            .into(),
        );
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_get_found() {
        let args = [
            HashMapV(
                Gc::new(hashmap! {
                    StringV("foo".into()) => StringV("bar".into())
                })
                .into(),
            ),
            StringV("foo".into()),
        ];
        let res = steel_hash_ref(&args);
        let expected = StringV("bar".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_get_error() {
        let args = [
            HashMapV(
                Gc::new(hashmap! {
                    StringV("foo".into()) => StringV("bar".into())
                })
                .into(),
            ),
            StringV("garbage".into()),
        ];
        let res = steel_hash_ref(&args);
        assert!(res.is_err());
    }

    #[test]
    fn hm_try_get_found() {
        let args = [
            HashMapV(
                Gc::new(hashmap! {
                    StringV("foo".into()) => StringV("bar".into())
                })
                .into(),
            ),
            StringV("foo".into()),
        ];
        let res = steel_hash_try_get(&args);
        let expected = StringV("bar".into());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_try_get_error() {
        let args = [
            HashMapV(
                Gc::new(hashmap! {
                    StringV("foo".into()) => StringV("bar".into())
                })
                .into(),
            ),
            StringV("garbage".into()),
        ];
        let res = steel_hash_contains(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_contains_true() {
        let args = [
            HashMapV(
                Gc::new(hashmap! {
                    StringV("foo".into()) => StringV("bar".into())
                })
                .into(),
            ),
            StringV("foo".into()),
        ];
        let res = steel_hash_contains(&args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_contains_false() {
        let args = [
            HashMapV(
                Gc::new(hashmap! {
                    StringV("foo".into()) => StringV("bar".into())
                })
                .into(),
            ),
            StringV("bar".into()),
        ];
        let res = steel_hash_contains(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hm_keys_to_vector_normal() {
        let args = vec![HashMapV(
            Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into()),
                StringV("bar".into()) => StringV("baz".into()),
                StringV("baz".into()) => StringV("quux".into())
            })
            .into(),
        )];
        let res = keys_to_vector(&args);
        let expected = im_rc::vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into()),
        ]
        .into();

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
        let args = vec![HashMapV(
            Gc::new(hashmap! {
                StringV("foo".into()) => StringV("bar".into()),
                StringV("bar".into()) => StringV("baz".into()),
                StringV("baz".into()) => StringV("quux".into())
            })
            .into(),
        )];
        let res = values_to_vector(&args);
        let expected = im_rc::vector![
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into()),
            SteelVal::StringV("quux".into()),
        ]
        .into();

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
