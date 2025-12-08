use std::hash::{DefaultHasher, Hash, Hasher};

use crate::rvals::SteelHashMap;
use crate::stop;
use crate::values::lists::Pair;
use crate::values::HashMap;
use crate::{core::utils::declare_const_ref_functions, gc::Gc};
use crate::{
    rvals::{Result, SteelVal},
    steel_vm::builtin::BuiltInModule,
};

use crate::primitives::vectors::vec_construct_iter_normal;

use num_bigint::BigInt;
use steel_derive::function;

declare_const_ref_functions!(
    HM_CONSTRUCT => hm_construct,
    HM_GET => steel_hash_ref,
    // HM_EMPTY => hm_empty,
);

pub const HM_INSERT: SteelVal = SteelVal::MutFunc(steel_hash_insert);

pub(crate) fn hashmap_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/hash".to_string());
    module
        .register_native_fn_definition(HM_CONSTRUCT_DEFINITION)
        .register_value("%keyword-hash", SteelVal::FuncV(hm_construct_keywords))
        .register_native_fn_definition(HASH_INSERT_DEFINITION)
        .register_native_fn_definition(HASH_REF_DEFINITION)
        .register_value("hash-get", SteelVal::FuncV(steel_hash_ref))
        .register_native_fn_definition(HASH_TRY_GET_DEFINITION)
        .register_native_fn_definition(HASH_LENGTH_DEFINITION)
        .register_native_fn_definition(HASH_CONTAINS_DEFINITION)
        .register_native_fn_definition(HASH_TO_LIST_DEFINITION)
        .register_native_fn_definition(HASH_TO_VECTOR_DEFINITION)
        .register_native_fn_definition(KEYS_TO_LIST_DEFINITION)
        .register_native_fn_definition(KEYS_TO_VECTOR_DEFINITION)
        .register_native_fn_definition(VALUES_TO_LIST_DEFINITION)
        .register_native_fn_definition(VALUES_TO_VECTOR_DEFINITION)
        .register_native_fn_definition(CLEAR_DEFINITION)
        .register_native_fn_definition(HM_EMPTY_DEFINITION)
        .register_native_fn_definition(HM_UNION_DEFINITION)
        .register_native_fn_definition(HASH_REMOVE_DEFINITION)
        .register_native_fn_definition(HASH_CODE_DEFINITION);
    module
}

/// Gets the hash code for the given value;
///
/// (hash-code v) -> integer?
///
/// * v : any/c
///
/// # Examples
/// ```scheme
/// (hash-code 10) ;; => 16689870864682149525
/// (hash-code "hello world") ;; => 12361891819228967546
/// ```
#[steel_derive::function(name = "hash-code", constant = false)]
pub fn hash_code(arg: &SteelVal) -> Result<SteelVal> {
    let mut hasher = DefaultHasher::new();

    arg.hash(&mut hasher);
    let value = hasher.finish();

    if let Ok(v) = isize::try_from(value) {
        Ok(SteelVal::IntV(v))
    } else {
        Ok(SteelVal::BigNum(Gc::new(BigInt::from(value))))
    }
}

/// Creates an immutable hash table with each given `key` mapped to the following `val`.
/// Each key must have a val, so the total number of arguments must be even.
///
/// (hash key val ...) -> hash?
///
/// * key : any/c
/// * val : any/c
///
/// Note: the keys must be hashable.
///
/// # Examples
/// ```scheme
/// > (hash 'a 10 'b 20)
/// => '#hash((a . 10) (b . 20))
/// ```
#[steel_derive::native(name = "hash", arity = "AtLeast(0)")]
pub fn hm_construct(args: &[SteelVal]) -> Result<SteelVal> {
    let mut hm = HashMap::new();

    let mut arg_iter = args.iter().cloned();

    loop {
        match (arg_iter.next(), arg_iter.next()) {
            (Some(key), Some(value)) => {
                hm.insert(key, value);
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
                hm.insert(key, value);
            }
            (None, None) => break,
            _ => {
                stop!(ArityMismatch => "Missing keyword argument!");
            }
        }
    }

    Ok(SteelVal::HashMapV(Gc::new(hm).into()))
}

/// Returns a new hashmap with the given key removed. Performs a functional
/// update, so the old hash map is still available with the original key value pair.
///
/// (hash-remove map key) -> hash?
///
/// * map : hash?
/// * key : any/c
///
/// # Examples
///
/// ```scheme
/// > (hash-remove (hash 'a 10 'b 20) 'a)
/// => '#hash(('b . 20))
/// ```
#[function(name = "hash-remove")]
pub fn hash_remove(map: &mut SteelVal, key: SteelVal) -> Result<SteelVal> {
    if let SteelVal::HashMapV(SteelHashMap(ref mut m)) = map {
        match Gc::get_mut(m) {
            Some(m) => {
                m.remove(&key);
                Ok(std::mem::replace(map, SteelVal::Void))
            }
            None => {
                let mut m = m.unwrap();
                m.remove(&key);

                Ok(SteelVal::HashMapV(Gc::new(m).into()))
            }
        }
    } else {
        stop!(TypeMismatch => "hash-remove expects a hash map, found: {:?}", map);
    }
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
///
/// ```scheme
/// > (hash-insert (hash 'a 10 'b 20) 'c 30)
/// => '#hash((a . 10) (b . 20) (c . 30))
/// ```
#[function(name = "hash-insert")]
pub fn hash_insert(
    map: &mut SteelVal,
    key: &mut SteelVal,
    value: &mut SteelVal,
) -> Result<SteelVal> {
    let key = std::mem::take(key);
    let value = std::mem::take(value);
    if let SteelVal::HashMapV(SteelHashMap(ref mut m)) = map {
        match Gc::get_mut(m) {
            Some(m) => {
                m.insert(key, value);
                Ok(std::mem::replace(map, SteelVal::Void))
            }
            None => Ok(SteelVal::HashMapV(Gc::new(m.update(key, value)).into())),
        }
    } else {
        stop!(TypeMismatch => "hash-insert expects a hash map, found: {:?}", map);
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
///
/// ```scheme
/// > (hash-ref (hash 'a 10 'b 20) 'b) ;; => 20
/// ```
#[function(name = "hash-ref")]
pub fn hash_ref(map: &Gc<HashMap<SteelVal, SteelVal>>, key: &SteelVal) -> Result<SteelVal> {
    match map.get(key) {
        Some(value) => Ok(value.clone()),
        None => stop!(Generic => "key not found in hash map: {} - map: {:?}", key, map),
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
/// (hash-length map) -> (and/c positive? int?)
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
/// * key : any/c
///
/// # Example
///
/// ```scheme
/// > (hash-contains? (hash 'a 10 'b 20) 'a) ;; => #true
/// > (hash-contains? (hash 'a 10 'b 20) 'not-there) ;; => #false
/// ```
#[function(name = "hash-contains?")]
pub fn hash_contains(map: &Gc<HashMap<SteelVal, SteelVal>>, key: &SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(map.contains_key(key)))
}

/// Returns a list of the key-value pairs of a given hash map.
///
/// (hash->list map) -> (listof (cons/c any/c any/c))
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash->list (hash 'a 10 'b 20)) ;; => '((a . 10) (b . 20))
/// ```
#[function(name = "hash->list")]
pub fn hash_to_list(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    Ok(SteelVal::ListV(
        hashmap
            .iter()
            .map(|(key, val)| SteelVal::Pair(Gc::new(Pair::cons(key.clone(), val.clone()))))
            .collect(),
    ))
}

/// Returns the keys of the given hash map as a list.
///
/// (hash-keys->list map) -> (listof any/c)
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash-keys->list (hash 'a 10 'b 20))
/// => '(a b)
/// ```
#[function(name = "hash-keys->list")]
pub fn keys_to_list(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    Ok(SteelVal::ListV(hashmap.keys().cloned().collect()))
}

/// Returns the values of the given hash map as a list
///
/// (hash-values->list map) -> (listof any/c)
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash-values->list (hash 'a 10 'b 20))
/// => '(10 20)
/// ```
#[steel_derive::function(name = "hash-values->list")]
pub fn values_to_list(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    Ok(SteelVal::ListV(hashmap.values().cloned().collect()))
}

/// Returns a list of the key-value pairs of a given hash map.
///
/// (hash->vector map) -> (vectorof (cons/c any/c any/c))
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash->vector (hash 'a 10 'b 20)) ;; => '#((a . 10) (b . 20))
/// ```
#[function(name = "hash->vector")]
pub fn hash_to_vector(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    vec_construct_iter_normal(
        hashmap
            .iter()
            .map(|(key, val)| SteelVal::Pair(Gc::new(Pair::cons(key.clone(), val.clone())))),
    )
}
/// Returns the keys of the given hash map as an immutable vector
///
/// (hash-keys->vector map) -> (vectorof any/c)
///
/// * map: hash?
///
/// # Examples
/// ```scheme
/// > (hash-keys->vector (hash 'a 10 'b 20))
/// => '#(a b)
/// ```
#[steel_derive::function(name = "hash-keys->vector")]
pub fn keys_to_vector(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    vec_construct_iter_normal(hashmap.keys().cloned())
}

/// Returns the values of the given hash map as an immutable vector
///
/// (hash-values->vector map) -> (vectorof any/c)?
///
/// * map : hash?
///
/// # Examples
///
/// ```scheme
/// > (hash-values->vector (hash 'a 10 'b 20))
/// => '#(10 20)
/// ```
#[steel_derive::function(name = "hash-values->vector")]
pub fn values_to_vector(hashmap: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    vec_construct_iter_normal(hashmap.values().cloned())
}

/// Clears the entries out of the existing hashmap.
/// Will attempt to reuse the existing memory if there are no other references
/// to the hashmap.
///
/// (hash-clear map) -> hash?
///
/// * map : hash?
///
/// # Examples
/// ```scheme
/// > (hash-clear (hash 'a 10 'b 20))
/// => '#hash()
/// ```
#[steel_derive::function(name = "hash-clear")]
pub fn clear(hashmap: &mut SteelVal) -> Result<SteelVal> {
    if let SteelVal::HashMapV(SteelHashMap(ref mut m)) = hashmap {
        match Gc::get_mut(m) {
            Some(m) => {
                // m.insert(key, value);
                m.clear();
                Ok(std::mem::replace(hashmap, SteelVal::Void))
            }
            None => Ok(SteelVal::HashMapV(Gc::new(HashMap::new()).into())),
        }
    } else {
        stop!(TypeMismatch => "hash-clear expected a hashmap, found: {:?}", hashmap);
    }
}

/// Checks whether the hash map is empty
///
/// (hash-empty? map) -> bool?
///
/// * map : hash?
///
/// # Examples
/// ```scheme
/// > (hash-empty? (hash 'a 10)) ;; => #f
/// > (hash-emptY? (hash)) ;; => #true
/// ```
#[steel_derive::function(name = "hash-empty?")]
pub fn hm_empty(hm: &Gc<HashMap<SteelVal, SteelVal>>) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(hm.is_empty()))
}

/// Constructs the union of two hashmaps, keeping the values
/// in the left map when the keys exist in both maps.
///
/// Will reuse memory where possible.
///
/// (hash-union l r) -> hash?
///
/// # Examples
/// ```scheme
/// > (hash-union (hash 'a 10) (hash 'b 20)) ;; => '#hash((a . 10) (b . 20))
/// ```
#[steel_derive::function(name = "hash-union")]
pub fn hm_union(mut hml: &mut SteelVal, mut hmr: &mut SteelVal) -> Result<SteelVal> {
    match (&mut hml, &mut hmr) {
        (
            SteelVal::HashMapV(SteelHashMap(ref mut l)),
            SteelVal::HashMapV(SteelHashMap(ref mut r)),
        ) => match (Gc::get_mut(l), Gc::get_mut(r)) {
            (None, None) => {
                let hml = l.unwrap();
                let hmr = r.unwrap();
                Ok(SteelVal::HashMapV(Gc::new(hml.union(hmr)).into()))
            }
            (None, Some(r_map)) => {
                let right_side_value = std::mem::take(r_map);

                *r_map = l.unwrap().union(right_side_value);

                Ok(std::mem::replace(hmr, SteelVal::Void))
            }
            (Some(l_map), None) => {
                let left_side_value = std::mem::take(l_map);

                *l_map = left_side_value.union(r.unwrap());

                Ok(std::mem::replace(hml, SteelVal::Void))
            }
            (Some(l_map), Some(r_map)) => {
                let left_side_value = std::mem::take(l_map);
                let right_side_value = std::mem::take(r_map);

                *l_map = left_side_value.union(right_side_value);

                Ok(std::mem::replace(hml, SteelVal::Void))
            }
        },

        _ => {
            stop!(TypeMismatch => "hash-union expects two hash maps, found: {:?} and {:?}", hml, hmr)
        }
    }
}

#[cfg(test)]
mod hashmap_tests {
    use super::*;

    #[cfg(not(feature = "sync"))]
    use im_rc::hashmap;

    #[cfg(not(feature = "sync"))]
    use im_rc::vector;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::hashmap;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::vector;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::hashmap;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::vector;

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
        let mut args = [
            HashMapV(Gc::new(hashmap![]).into()),
            StringV("foo".into()),
            StringV("bar".into()),
        ];
        let res = steel_hash_insert(&mut args);
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
        let res = steel_keys_to_vector(&args);
        let expected = vector![
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
        let res = steel_values_to_vector(&args);
        let expected = vector![
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
