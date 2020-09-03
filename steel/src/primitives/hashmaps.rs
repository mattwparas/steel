use crate::env::{FALSE, TRUE};
use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal::*;
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
                        hm.insert(key, value);
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
                hm.insert(key, value);
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

    pub fn hm_contains() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "hm-contains? get takes 2 arguments")
            }

            let hashmap = Gc::clone(&args[0]);
            let key = &args[1];

            if let SteelVal::HashMapV(hm) = hashmap.as_ref() {
                if hm.contains_key(key) {
                    Ok(TRUE.with(|x| Gc::clone(x)))
                } else {
                    Ok(FALSE.with(|x| Gc::clone(x)))
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

    // pub fn
}
