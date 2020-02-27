use crate::evaluator::Result;
// #[macro_use]
use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use crate::stop;

//use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
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
    parent: Option<Rc<RefCell<Env>>>,
}
impl Env {
    /// Make a new `Env` from
    /// another parent `Env`.
    ///
    /// At the top level, the global env has no parent
    pub fn new(parent: &Rc<RefCell<Self>>) -> Self {
        println!(
            "Allocating a new env, parent ref count is: {}",
            Rc::strong_count(parent)
        );
        Env {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(&parent)),
        }
    }

    /// Within the current environment, bind
    /// identifier `key` to `val`
    pub fn define(&mut self, key: String, val: RucketVal) {
        self.bindings.insert(key, val);
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// If found, update binding for
    /// `key` with `val` and return old value.
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn set(&mut self, key: String, val: RucketVal) -> Result<RucketVal> {
        if self.bindings.contains_key(&key) {
            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else {
            match &self.parent {
                Some(par) => par.borrow_mut().set(key, val),
                None => stop!(FreeIdentifier => key), // Err(RucketErr::FreeIdentifier(key)),
            }
        }
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// If found, remove the binding and return the value
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn remove(&mut self, key: &str) -> Result<RucketVal> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else {
            match &self.parent {
                Some(par) => par.borrow_mut().remove(key),
                None => stop!(FreeIdentifier => key), // Err(RucketErr::FreeIdentifier(key.to_string())),
            }
        }
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// if found, return that value
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn lookup(&self, name: &str) -> Result<RucketVal> {
        if self.bindings.contains_key(name) {
            // value needs to be cloned because
            // user needs to be able to own a persistent value
            // from Cell that may be modified later
            Ok(self.bindings[name].clone())
        } else {
            match &self.parent {
                Some(par) => par.borrow().lookup(name),
                None => stop!(FreeIdentifier => name), // Err(RucketErr::FreeIdentifier(name.to_string())),
            }
        }
    }
}

impl Drop for Env {
    fn drop(&mut self) {
        if let Some(p) = &self.parent {
            println!(
                "Dropping an Environment!, parent ref count is {}",
                Rc::strong_count(p)
            );
        } else {
            println!("Dropping the global environment!");
        }
    }
}

/// default environment contains bindings for
/// implementations of constants and things like
/// `car`, `cdr`, `+`
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
                stop!(ArityMismatch => "cons takes two arguments");
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
                        if v.is_empty() {
                            stop!(ContractViolation => "car expects a non empty list");
                        } else {
                            return Ok(v[0].clone());
                        }
                    }
                    e => {
                        stop!(ExpectedList => "car takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "car takes one argument");
            }
        }),
    );

    data.insert(
        "cdr".to_string(),
        RucketVal::FuncV(|args: &[&RucketVal]| -> Result<RucketVal> {
            if args.len() == 1 {
                match &args[0] {
                    RucketVal::ListV(v) => {
                        if v.is_empty() {
                            stop!(ContractViolation => "car expects a non empty list");
                        } else {
                            return Ok(RucketVal::ListV(v[1..].to_vec()));
                        }
                    }
                    e => {
                        stop!(ExpectedList => "cdr takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "cdr takes one argument");
            }
        }),
    );

    data.insert(
        "=".to_string(),
        RucketVal::FuncV(ensure_tonicity!(|a, b| (a - b) < std::f64::EPSILON)),
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
        parent: None,
    }
}

// TODO: make this a trait or something
fn unwrap_list_of_floats(args: &[&RucketVal]) -> Result<Vec<f64>> {
    args.iter().map(|x| unwrap_single_float(x)).collect()
}

fn unwrap_single_float(exp: &RucketVal) -> Result<f64> {
    match exp {
        RucketVal::NumV(num) => Ok(*num),
        _ => stop!(ExpectedNumber => "expected a number"),
    }
}

fn unwrap_list_of_lists(args: &[&RucketVal]) -> Result<Vec<Vec<RucketVal>>> {
    args.iter().map(|x| unwrap_single_list(x)).collect()
}

fn unwrap_single_list(exp: &RucketVal) -> Result<Vec<RucketVal>> {
    match exp {
        RucketVal::ListV(lst) => Ok(lst.clone()),
        _ => stop!(ExpectedList => "expected a list"),
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
