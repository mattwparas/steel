use crate::stop;
use crate::{
    core::utils::declare_const_ref_functions,
    rvals::{Result, SteelVal},
};
use crate::{gc::Gc, steel_vm::builtin::BuiltInModule};
use im_lists::list::List;
use im_rc::HashSet;

use crate::primitives::VectorOperations;

declare_const_ref_functions!(
    HS_CONSTRUCT => hs_construct,
    HS_LENGTH => hs_length,
    HS_CONTAINS => hs_contains,
    HS_INSERT => hs_insert,
    HS_TO_LIST => keys_to_list,
    HS_TO_VEC => keys_to_vector,
    HS_CLEAR => clear,
    HS_SUBSET => is_subset,
    LIST_TO_HS => list_to_hashset,
);

pub(crate) fn hashset_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/sets");
    module
        .register_value("hashset", HS_CONSTRUCT)
        .register_value("hashset-length", HS_LENGTH)
        .register_value("hashset-contains?", HS_CONTAINS)
        .register_value("hashset-insert", HS_INSERT)
        .register_value("hashset->list", HS_TO_LIST)
        .register_value("hashset->vector", HS_TO_VEC)
        .register_value("hashset-clear", HS_CLEAR)
        .register_value("hashset-subset?", HS_SUBSET)
        .register_value("list->hashset", LIST_TO_HS);
    module
}

pub fn hs_construct(args: &[SteelVal]) -> Result<SteelVal> {
    let mut hs = HashSet::new();

    for key in args {
        if key.is_hashable() {
            hs.insert(key.clone());
        } else {
            stop!(TypeMismatch => "hash key not hashable!");
        }
    }

    Ok(SteelVal::HashSetV(Gc::new(hs)))
}

pub fn hs_length(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hs-length takes 1 argument")
    }

    let hashmap = &args[0];

    if let SteelVal::HashSetV(hm) = hashmap {
        Ok(SteelVal::IntV(hm.len() as isize))
    } else {
        stop!(TypeMismatch => "hs-length takes a hashmap")
    }
}

pub fn hs_insert(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn hs_contains(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn is_subset(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "hashset-subset? takes 2 arguments")
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
        stop!(TypeMismatch => "hashset-subset? takes a hashset")
    }
}

// keys as list
pub fn keys_to_list(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hs-keys->list takes 1 argument")
    }

    let hashset = &args[0];

    if let SteelVal::HashSetV(hs) = hashset {
        Ok(SteelVal::ListV(
            hs.iter().cloned().collect::<List<SteelVal>>(),
        ))
    } else {
        stop!(TypeMismatch => "hs-keys->list takes a hashmap")
    }
}

// keys as vectors
pub fn keys_to_vector(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "hs-keys->vector takes 1 argument")
    }

    let hashset = &args[0];

    if let SteelVal::HashSetV(hs) = hashset {
        VectorOperations::vec_construct_iter_normal(hs.iter().cloned())
    } else {
        stop!(TypeMismatch => "hs-keys->vector takes a hashmap")
    }
}

pub fn clear(args: &[SteelVal]) -> Result<SteelVal> {
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
}

pub fn list_to_hashset(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 1 {
        stop!(ArityMismatch => "list->hashset takes one argument")
    }
    if let SteelVal::ListV(l) = &args[0] {
        Ok(SteelVal::HashSetV(Gc::new(l.iter().cloned().collect())))
    } else {
        stop!(TypeMismatch => "list->hashset takes a hashset");
    }
}

#[cfg(test)]
mod hashset_tests {
    use crate::rvals::SteelString;

    use super::*;

    #[test]
    fn hs_construct_normal() {
        let args = [
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("foo2".into()),
            SteelVal::StringV("bar2".into()),
        ];
        let res = hs_construct(&args);
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
        let args = [
            SteelVal::HashSetV(Gc::new(vec![].into())),
            SteelVal::StringV("foo".into()),
        ];
        let res = hs_insert(&args);
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
        let args = [
            SteelVal::HashSetV(Gc::new(
                vec![SteelVal::StringV("foo".into())]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            )),
            SteelVal::StringV("foo".into()),
        ];
        let res = hs_contains(&args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_contains_false() {
        let args = [
            SteelVal::HashSetV(Gc::new(
                vec![SteelVal::StringV("foo".into())]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            )),
            SteelVal::StringV("bar".into()),
        ];
        let res = hs_contains(&args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn hs_keys_to_vector_normal() {
        let args = [SteelVal::HashSetV(Gc::new(
            vec![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into()),
            ]
            .into_iter()
            .collect(),
        ))];
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
