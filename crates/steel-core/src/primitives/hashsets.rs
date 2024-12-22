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
        .register_native_fn_definition(HS_INSERT_DEFINITION)
        .register_native_fn_definition(HASHSET_TO_LIST_DEFINITION)
        .register_native_fn_definition(HASHSET_TO_IMMUTABLE_VECTOR_DEFINITION)
        .register_native_fn_definition(HASHSET_TO_MUTABLE_VECTOR_DEFINITION)
        .register_native_fn_definition(HASHSET_CLEAR_DEFINITION)
        .register_native_fn_definition(HASHSET_IS_SUBSET_DEFINITION)
        .register_native_fn_definition(LIST_TO_HASHSET_DEFINITION);
    module
}

#[steel_derive::native(name = "hashset", arity = "AtLeast(0)")]
pub fn hs_construct(args: &[SteelVal]) -> Result<SteelVal> {
    let mut hs = HashSet::new();

    for key in args {
        if key.is_hashable() {
            hs.insert(key.clone());
        } else {
            stop!(TypeMismatch => "hash key not hashable!");
        }
    }

    Ok(SteelVal::HashSetV(Gc::new(hs).into()))
}

#[steel_derive::function(name = "hashset-length")]
pub fn hashset_length(hashset: &SteelHashSet) -> usize {
    hashset.len()
}

#[steel_derive::function(name = "hashset-insert")]
pub fn hs_insert(hashset: &mut SteelVal, value: SteelVal) -> Result<SteelVal> {
    if value.is_hashable() {
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
    } else {
        stop!(TypeMismatch => "hash key not hashable!");
    }
}

#[steel_derive::function(name = "hashset-contains?")]
pub fn hashset_contains(hashset: &SteelHashSet, key: &SteelVal) -> Result<SteelVal> {
    if key.is_hashable() {
        Ok(SteelVal::BoolV(hashset.contains(key)))
    } else {
        stop!(TypeMismatch => "hash key not hashable!: {}", key);
    }
}

#[steel_derive::function(name = "hashset-subset?")]
pub fn hashset_is_subset(left: &SteelHashSet, right: &SteelHashSet) -> bool {
    left.is_subset(right.0.as_ref())
}

#[steel_derive::function(name = "hashset->list")]
pub fn hashset_to_list(hashset: &SteelHashSet) -> SteelVal {
    SteelVal::ListV(hashset.iter().cloned().collect::<List<SteelVal>>())
}

#[steel_derive::function(name = "hashset->immutable-vector")]
pub fn hashset_to_immutable_vector(hashset: &SteelHashSet) -> SteelVal {
    SteelVal::VectorV(Gc::new(hashset.0.iter().cloned().collect::<Vector<_>>()).into())
}

#[steel_derive::context(name = "hashset->vector", arity = "Exact(1)")]
pub fn hashset_to_mutable_vector(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    fn hashset_to_mutable_vector_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
        if args.len() != 1 {
            stop!(ArityMismatch => "hashset->vector takes 1 argument")
        }

        let hashset = &args[0];

        if let SteelVal::HashSetV(hs) = hashset {
            Ok(ctx.make_mutable_vector(hs.iter().cloned().collect()))
        } else {
            stop!(TypeMismatch => "hashset->vector takes a hashset")
        }
    }

    Some(hashset_to_mutable_vector_impl(ctx, args))
}

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

    #[cfg(feature = "sync")]
    use im::vector;

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
