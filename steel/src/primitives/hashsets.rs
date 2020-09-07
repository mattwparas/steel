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

    // pub fn
}
