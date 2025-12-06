use crate::rvals::SteelHashSet;
use crate::rvals::{Result, SteelVal};
use crate::steel_vm::vm::VmCore;
use crate::values::lists::List;
use crate::values::HashSet;
use crate::{gc::Gc, steel_vm::builtin::BuiltInModule};
use crate::{stop, Vector};

pub(crate) fn hashset_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/sets");
    module
        .register_native_fn_definition(HS_CONSTRUCT_DEFINITION)
        .register_native_fn_definition(HASHSET_LENGTH_DEFINITION)
        .register_native_fn_definition(HASHSET_CONTAINS_DEFINITION)
        .register_native_fn_definition(HASHSET_UNION_DEFINITION)
        .register_native_fn_definition(HASHSET_INTERSECTION_DEFINITION)
        .register_native_fn_definition(HASHSET_DIFFERENCE_DEFINITION)
        .register_native_fn_definition(HS_INSERT_DEFINITION)
        .register_native_fn_definition(HASHSET_TO_LIST_DEFINITION)
        .register_native_fn_definition(HASHSET_TO_IMMUTABLE_VECTOR_DEFINITION)
        .register_native_fn_definition(HASHSET_TO_MUTABLE_VECTOR_DEFINITION)
        .register_native_fn_definition(HASHSET_CLEAR_DEFINITION)
        .register_native_fn_definition(HASHSET_IS_SUBSET_DEFINITION)
        .register_native_fn_definition(LIST_TO_HASHSET_DEFINITION);
    module
}

/// Constructs a new hash set
///
/// # Examples
/// ```scheme
/// (hashset 10 20 30 40)
/// ```
#[steel_derive::native(name = "hashset", arity = "AtLeast(0)")]
pub fn hs_construct(args: &[SteelVal]) -> Result<SteelVal> {
    let mut hs = HashSet::new();

    for key in args {
        hs.insert(key.clone());
    }

    Ok(SteelVal::HashSetV(Gc::new(hs).into()))
}

/// Get the number of elements in the hashset
///
/// # Examples
/// ```scheme
/// (hashset-length (hashset 10 20 30)) ;; => 3
/// ```
#[steel_derive::function(name = "hashset-length")]
pub fn hashset_length(hashset: &SteelHashSet) -> usize {
    hashset.len()
}

/// Insert a new element into the hashset. Returns a hashset.
///
/// # Examples
/// ```scheme
/// (define hs (hashset 10 20 30))
/// (define updated (hashset-insert hs 40))
/// (equal? hs (hashset 10 20 30)) ;; => #true
/// (equal? updated (hashset 10 20 30 40)) ;; => #true
/// ```
#[steel_derive::function(name = "hashset-insert")]
pub fn hs_insert(hashset: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    if let SteelVal::HashSetV(SteelHashSet(hs)) = hashset {
        match Gc::get_mut(hs) {
            Some(m) => {
                m.insert(value);
                Ok(std::mem::replace(hashset, SteelVal::Void))
            }

            None => Ok(SteelVal::HashSetV(SteelHashSet(Gc::new(hs.update(value))))),
        }
    } else {
        stop!(TypeMismatch => "set insert takes a set")
    }
}

/// Test if the hashset contains a given element.
///
/// # Examples
/// ```scheme
/// (hashset-contains? (hashset 10 20) 10) ;; => #true
/// (hashset-contains? (hashset 10 20) "foo") ;; => #false
/// ```
#[steel_derive::function(name = "hashset-contains?")]
pub fn hashset_contains(hashset: &SteelHashSet, key: &SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(hashset.contains(key)))
}

/// Check if the left set is a subset of the right set
///
/// # Examples
/// ```scheme
/// (hashset-subset? (hash 10) (hashset 10 20)) ;; => #true
/// (hashset-subset? (hash 100) (hashset 30)) ;; => #false
/// ```
#[steel_derive::function(name = "hashset-subset?")]
pub fn hashset_is_subset(left: &SteelHashSet, right: &SteelHashSet) -> bool {
    left.is_subset(right.0.as_ref())
}

/// Creates a list from this hashset. The order of the list is not guaranteed.
///
/// # Examples
/// ```scheme
/// (hashset->list (hashset 10 20 30)) ;; => '(10 20 30)
/// (hashset->list (hashset 10 20 30)) ;; => '(20 10 30)
/// ```
#[steel_derive::function(name = "hashset->list")]
pub fn hashset_to_list(hashset: &SteelHashSet) -> SteelVal {
    SteelVal::ListV(hashset.iter().cloned().collect::<List<SteelVal>>())
}

/// Finds the union between the two hash sets.
///
/// # Examples
/// ```scheme
/// (hashset-union (hashset 10) (hashset 20)) ;; => (hashset 10 20)
/// ```
#[steel_derive::function(name = "hashset-union")]
pub fn hashset_union(l: &SteelHashSet, r: &SteelHashSet) -> SteelVal {
    SteelVal::HashSetV(SteelHashSet(Gc::new(l.0.unwrap().union(r.0.unwrap()))))
}

/// Finds the intersection between the two hash sets.
///
/// # Examples
/// ```scheme
/// (hashset-intersection (hashset 10 20) (hashset 20)) ;; => (hashset 10)
/// ```
#[steel_derive::function(name = "hashset-intersection")]
pub fn hashset_intersection(l: &SteelHashSet, r: &SteelHashSet) -> SteelVal {
    SteelVal::HashSetV(SteelHashSet(Gc::new(
        l.0.unwrap().intersection(r.0.unwrap()),
    )))
}

/// Finds the difference between the two hash sets.
///
/// # Examples
/// ```scheme
/// (hashset-difference (hashset 10 20 30) (hashset 20 30 40)) ;; => (hashset 40 10)
/// ```
#[steel_derive::function(name = "hashset-difference")]
pub fn hashset_difference(l: &SteelHashSet, r: &SteelHashSet) -> SteelVal {
    #[cfg(not(feature = "imbl"))]
    {
        SteelVal::HashSetV(SteelHashSet(Gc::new(l.0.unwrap().difference(r.0.unwrap()))))
    }
    #[cfg(feature = "imbl")]
    SteelVal::HashSetV(SteelHashSet(Gc::new(
        l.0.unwrap().symmetric_difference(r.0.unwrap()),
    )))
}

/// Creates an immutable vector from this hashset. The order of the vector is not guaranteed.
///
/// # Examples
/// ```scheme
/// (hashset->immutable-vector (hashset 10 20 30)) ;; => '#(10 20 30)
/// (hashset->immutable-vector (hashset 10 20 30)) ;; => '#(20 10 30)
/// ```
#[steel_derive::function(name = "hashset->immutable-vector")]
pub fn hashset_to_immutable_vector(hashset: &SteelHashSet) -> SteelVal {
    SteelVal::VectorV(Gc::new(hashset.0.iter().cloned().collect::<Vector<_>>()).into())
}

/// Creates a mutable vector from this hashset. The order of the vector is not guaranteed.
///
/// # Examples
/// ```scheme
/// (hashset->vector (hashset 10 20 30)) ;; => '#(10 20 30)
/// (hashset->vector (hashset 10 20 30)) ;; => '#(20 10 30)
/// ```
#[steel_derive::context(name = "hashset->vector", arity = "Exact(1)")]
pub fn hashset_to_mutable_vector(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    fn hashset_to_mutable_vector_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
        let hashset = &args[0];

        if let SteelVal::HashSetV(hs) = hashset {
            Ok(ctx.make_mutable_vector(hs.iter().cloned().collect()))
        } else {
            stop!(TypeMismatch => "hashset->vector takes a hashset")
        }
    }

    Some(hashset_to_mutable_vector_impl(ctx, args))
}

/// Clears the hashset and returns the passed in hashset.
/// This first checks if there are no other references to this hashset,
/// and if there aren't, clears that allocation. Given that there are
/// functional updates, this is only relevant if there are no more
/// references to a given hashset, and you want to reuse its allocation.
#[steel_derive::function(name = "hashset-clear")]
pub fn hashset_clear(hashset: &mut SteelVal) -> Result<SteelVal> {
    if let SteelVal::HashSetV(SteelHashSet(hs)) = hashset {
        match Gc::get_mut(hs) {
            Some(m) => {
                m.clear();
                Ok(std::mem::replace(hashset, SteelVal::Void))
            }
            None => Ok(SteelVal::HashSetV(Gc::new(HashSet::new()).into())),
        }
    } else {
        stop!(TypeMismatch => format!("hashset-clear takes a hashset, found: {}", hashset))
    }
}

/// Convert the given list into a hashset.
///
/// # Examples
/// ```scheme
/// (list 10 20 30) ;; => (hashset 10 20 30)
/// ```
#[steel_derive::function(name = "list->hashset")]
pub fn list_to_hashset(l: &List<SteelVal>) -> SteelVal {
    SteelVal::HashSetV(Gc::new(l.iter().cloned().collect::<HashSet<_>>()).into())
}

#[cfg(test)]
mod hashset_tests {
    use crate::rvals::SteelString;

    use super::*;

    #[cfg(not(feature = "sync"))]
    use im_rc::vector;

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    use im::vector;

    #[cfg(all(feature = "sync", feature = "imbl"))]
    use imbl::vector;

    #[test]
    fn hs_construct_normal() {
        let args = [
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
        ];
        let res = hs_construct(&args);
        let expected = SteelVal::HashSetV(
            Gc::new(
                vec![
                    SteelVal::StringV("foo".into()),
                    SteelVal::StringV("bar".into()),
                    SteelVal::StringV("foo2".into()),
                    SteelVal::StringV("bar2".into()),
                ]
                .into_iter()
                .map(Gc::new)
                .collect::<HashSet<_>>(),
            )
            .into(),
        );
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_construct_with_duplicates() {
        let args = [
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
        ];
        let res = hs_construct(&args);
        let expected = SteelVal::HashSetV(
            Gc::new(
                vec![
                    SteelVal::StringV("foo".into()),
                    SteelVal::StringV("bar".into()),
                    SteelVal::StringV("foo2".into()),
                    SteelVal::StringV("bar2".into()),
                ]
                .into_iter()
                .map(Gc::new)
                .collect::<HashSet<_>>(),
            )
            .into(),
        );
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_insert_from_empty() {
        let mut args = [
            SteelVal::HashSetV(Gc::new(HashSet::new()).into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = steel_hs_insert(&mut args);
        let expected = SteelVal::HashSetV(
            Gc::new(
                vec![SteelVal::StringV("foo".into())]
                    .into_iter()
                    .map(Gc::new)
                    .collect::<HashSet<_>>(),
            )
            .into(),
        );
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_true() {
        let args = [
            SteelVal::HashSetV(
                Gc::new(
                    vec![SteelVal::StringV("foo".into())]
                        .into_iter()
                        .map(Gc::new)
                        .collect::<HashSet<_>>(),
                )
                .into(),
            ),
            SteelVal::StringV("foo".into()),
        ];
        let res = steel_hashset_contains(&args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_false() {
        let args = [
            SteelVal::HashSetV(
                Gc::new(
                    vec![SteelVal::StringV("foo".into())]
                        .into_iter()
                        .map(Gc::new)
                        .collect::<HashSet<_>>(),
                )
                .into(),
            ),
            SteelVal::StringV("bar".into()),
        ];
        let res = steel_hashset_contains(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_keys_to_vector_normal() {
        let args = [SteelVal::HashSetV(
            Gc::new(
                vec![
                    SteelVal::StringV("foo".into()),
                    SteelVal::StringV("bar".into()),
                    SteelVal::StringV("baz".into()),
                ]
                .into_iter()
                .collect::<HashSet<_>>(),
            )
            .into(),
        )];
        let res = steel_hashset_to_immutable_vector(&args);
        let expected = vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into()),
        ]
        .into();

        // pull out the vectors and sort them
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
}
