use crate::evaluator::Result;

use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;

//use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
// use std::result::Result;

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[&RucketVal]| -> Result<RucketVal> {
            let floats = unwrap_list_of_floats(args)?;
            let first = floats.first().ok_or(RucketErr::ExpectedNumber(
                "expected at least one number".to_string(),
            ))?;
            let rest = &floats[1..];
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            };
            Ok(RucketVal::BoolV(f(first, rest)))
        }
    }};
}

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

pub struct Env {
    bindings: HashMap<String, RucketVal>,
    parent: Weak<RefCell<Env>>,
}
impl Env {
    pub fn new(parent: &Rc<RefCell<Self>>) -> Self {
        Env {
            bindings: HashMap::new(),
            parent: Rc::downgrade(&parent),
        }
    }

    pub fn insert_binding(&mut self, var: String, val: RucketVal) {
        self.bindings.insert(var, val);
    }

    pub fn define(&mut self, key: String, val: RucketVal) {
        self.bindings.insert(key, val);
    }

    pub fn set(&mut self, key: String, val: RucketVal) -> Result<RucketVal> {
        if self.bindings.contains_key(&key) {
            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else {
            let parent = self.parent.upgrade();
            match parent {
                Some(par) => par.borrow_mut().set(key, val),
                None => Err(RucketErr::FreeIdentifier(key)),
            }
        }
    }

    pub fn remove(&mut self, key: &str) -> Result<RucketVal> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else {
            let parent = self.parent.upgrade();
            match parent {
                Some(par) => par.borrow_mut().remove(key),
                None => Err(RucketErr::FreeIdentifier(key.to_string())),
            }
        }
    }

    pub fn lookup(&self, name: &str) -> Result<RucketVal> {
        if self.bindings.contains_key(name) {
            Ok(self.bindings[name].clone())
        } else {
            let parent = self.parent.upgrade();
            match parent {
                Some(par) => par.borrow().lookup(name),
                None => Err(RucketErr::FreeIdentifier(name.to_string())),
            }
        }
    }
}

// #[derive(Clone)]
// pub struct Env {
//     bindings: HashMap<String, RucketVal>,
//     parent: EnvRef,
// }

// TODO
// Think about functional data structures to avoid cloning here
/*
impl Env {
    // TODO
    pub fn new(parent: EnvRef) -> Env {
        Env {
            bindings: HashMap::new(),
            parent,
        }
    }

    pub fn insert_binding(&mut self, var: String, val: RucketVal) {
        self.bindings.insert(var, val);
    }

    pub fn with_values(parent: EnvRef, bindings: HashMap<String, RucketVal>) -> Env {
        Env { parent, bindings }
    }

    /// Converts `Env` into a `EnvRef`.
    /// This function moves `Env` into a `RefCell`.
    /// If you need another pointer to newly created EnvRef,
    /// use `EnvRef::clone_ref()` which only copies the pointer,
    /// not the environment itself.
    pub fn into_ref(self) -> EnvRef {
        EnvRef::new(self)
    }

    // TODO make this not bad
    pub fn lookup(&self, name: &str) -> Result<RucketVal> {
        if self.bindings.contains_key(name) {
            Ok(self.bindings[name].clone())
        } else if self.parent.is_some() {
            self.parent.lookup(name)
        } else {
            Err(RucketErr::FreeIdentifier(name.to_string()))
        }
    }

    // pub fn with_ref<F, T>(&self, name: &str, mut f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     if self.bindings.contains_key(name) {
    //         let sexpr = &self.bindings[name];
    //         f(sexpr)
    //     } else if self.parent.is_some() {
    //         self.parent.with_ref(name, f)
    //     } else {
    //         Err(RucketErr::FreeIdentifier(name.to_string()))
    //         // bail!(UnboundVar => name)
    //     }
    // }

    // pub fn with_mut_ref<F, T>(&mut self, name: &str, mut f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&mut RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     if self.bindings.contains_key(name) {
    //         let sexpr = self.bindings.get_mut(name).unwrap();
    //         f(sexpr)
    //     } else if self.parent.is_some() {
    //         self.parent.with_mut_ref(name, f)
    //     } else {
    //         Err(RucketErr::FreeIdentifier(name.to_string()))
    //         // bail!(UnboundVar => name)
    //     }
    // }

    pub fn define(&mut self, key: String, val: RucketVal) {
        self.bindings.insert(key, val);
    }

    pub fn set(&mut self, key: String, val: RucketVal) -> Result<RucketVal> {
        if self.bindings.contains_key(&key) {
            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else if self.parent.is_some() {
            self.parent.set(key, val)
        } else {
            Err(RucketErr::FreeIdentifier(key))
        }
    }

    pub fn remove(&mut self, key: &str) -> Result<RucketVal> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else if self.parent.is_some() {
            self.parent.remove(key)
        } else {
            Err(RucketErr::FreeIdentifier(key.to_string()))
        }
    }

    pub fn pack(&mut self, keys: &[String], vals: Vec<RucketVal>) {
        for (i, arg) in vals.into_iter().enumerate() {
            self.bindings.insert(keys[i].clone(), arg);
        }
    }
}*/

pub fn default_env() -> Env {
    let mut data: HashMap<String, RucketVal> = HashMap::new();
    data.insert(
        "+".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            let sum = unwrap_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);

            Ok(RucketVal::NumV(sum))
        }),
    );

    data.insert(
        "*".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            let sum = unwrap_list_of_floats(args)?
                .iter()
                .fold(1.0, |sum, a| sum * a);

            Ok(RucketVal::NumV(sum))
        }),
    );

    data.insert(
        "/".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            let floats = unwrap_list_of_floats(args)?;
            let first = *floats.first().ok_or(RucketErr::ArityMismatch(
                "expected at least one number".to_string(),
            ))?;
            let sum_of_rest = floats[1..].iter().fold(first, |sum, a| sum / a);

            Ok(RucketVal::NumV(sum_of_rest))
        }),
    );

    data.insert(
        "-".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            let floats = unwrap_list_of_floats(args)?;
            let first = *floats.first().ok_or(RucketErr::ArityMismatch(
                "expected at least one number".to_string(),
            ))?;
            let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

            Ok(RucketVal::NumV(first - sum_of_rest))
        }),
    );

    data.insert(
        "list".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            let new_lst = args.iter().map(|&x| x.clone()).collect();
            Ok(RucketVal::ListV(new_lst))
        }),
    );

    data.insert(
        "cons".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            if args.len() == 2 {
                let elem = args[0];
                let lst = args[1];

                if let RucketVal::ListV(v) = lst {
                    let mut l = v.clone();
                    l.insert(0, elem.clone());
                    return Ok(RucketVal::ListV(l));
                } else {
                    return Ok(RucketVal::ListV(vec![elem.clone(), lst.clone()]));
                }
            } else {
                return Err(RucketErr::ArityMismatch(
                    "cons takes two arguments".to_string(),
                ));
            }
        }),
    );

    data.insert(
        "append".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            let lsts: Vec<RucketVal> = unwrap_list_of_lists(args)?
                .iter()
                .flat_map(|x| x.clone())
                .collect();
            Ok(RucketVal::ListV(lsts))
        }),
    );

    data.insert(
        "car".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            if args.len() == 1 {
                match &args[0] {
                    RucketVal::ListV(v) => {
                        if v.len() == 0 {
                            return Err(RucketErr::ContractViolation(
                                "car expects a non empty list".to_string(),
                            ));
                        } else {
                            return Ok(v[0].clone());
                        }
                    }
                    e => {
                        return Err(RucketErr::ExpectedList(format!(
                            "car takes a list, given: {}",
                            e
                        )));
                    }
                }
            } else {
                return Err(RucketErr::ArityMismatch(
                    "car takes one argument".to_string(),
                ));
            }
        }),
    );

    data.insert(
        "cdr".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            if args.len() == 1 {
                match &args[0] {
                    RucketVal::ListV(v) => {
                        if v.len() == 0 {
                            return Err(RucketErr::ContractViolation(
                                "car expects a non empty list".to_string(),
                            ));
                        } else {
                            return Ok(RucketVal::ListV(v[1..].to_vec()));
                        }
                    }
                    e => {
                        return Err(RucketErr::ExpectedList(format!(
                            "cdr takes a list, given: {}",
                            e
                        )));
                    }
                }
            } else {
                return Err(RucketErr::ArityMismatch(
                    "cdr takes one argument".to_string(),
                ));
            }
        }),
    );

    data.insert(
        "=".to_string(),
        RucketVal::FuncV(ensure_tonicity!(|a, b| a == b)),
    );
    data.insert(
        ">".to_string(),
        RucketVal::FuncV(ensure_tonicity!(|a, b| a > b)),
    );
    data.insert(
        ">=".to_string(),
        RucketVal::FuncV(ensure_tonicity!(|a, b| a >= b)),
    );
    data.insert(
        "<".to_string(),
        RucketVal::FuncV(ensure_tonicity!(|a, b| a < b)),
    );
    data.insert(
        "<=".to_string(),
        RucketVal::FuncV(ensure_tonicity!(|a, b| a <= b)),
    );

    Env {
        bindings: data,
        parent: Weak::new(),
    }
}

fn unwrap_list_of_floats(args: &[&RucketVal]) -> Result<Vec<f64>> {
    args.iter().map(|x| unwrap_single_float(x)).collect()
}

fn unwrap_single_float(exp: &RucketVal) -> Result<f64> {
    match exp {
        RucketVal::NumV(num) => Ok(*num),
        _ => Err(RucketErr::ExpectedNumber("expected a number".to_string())),
    }
}

fn unwrap_list_of_lists(args: &[&RucketVal]) -> Result<Vec<Vec<RucketVal>>> {
    args.iter().map(|x| unwrap_single_list(x)).collect()
}

fn unwrap_single_list(exp: &RucketVal) -> Result<Vec<RucketVal>> {
    match exp {
        RucketVal::ListV(lst) => Ok(lst.clone()),
        _ => Err(RucketErr::ExpectedNumber("expected a list".to_string())),
    }
}

#[cfg(test)]
mod env_tests {
    use super::*;
    #[test]
    fn env_basic() {
        // default_env <- c1 <- c2
        let default_env = Rc::new(RefCell::new(default_env()));
        assert!(default_env.borrow().lookup("+").is_ok());
        let c1 = Rc::new(RefCell::new(Env::new(&default_env)));
        c1.borrow_mut().define("x".to_owned(), RucketVal::NumV(1.0));
        let c2 = Rc::new(RefCell::new(Env::new(&c1)));
        c2.borrow_mut().define("y".to_owned(), RucketVal::NumV(2.0));
        assert!(default_env.borrow_mut().lookup("+").is_ok());
        assert!(c2.borrow_mut().lookup("+").is_ok());
        assert_eq!(
            unwrap_single_float(&c2.borrow_mut().lookup("y").unwrap()).unwrap(),
            2.0
        );
        assert_eq!(
            unwrap_single_float(&c2.borrow_mut().lookup("x").unwrap()).unwrap(),
            1.0
        );
        assert!(c2.borrow_mut().lookup("z").is_err());
    }
}
