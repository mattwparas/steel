use crate::evaluator::Result;
// #[macro_use]
use crate::primitives::{Adder, Divider, Multiplier, SteelFunctor, Subtractor};
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;
use crate::stop;

use im_rc::Vector;
use std::cell::RefCell;
use std::collections::HashMap;
// use std::collections::Vector;
use std::rc::Rc;

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            // let floats = unwrap_list_of_floats(args)?;
            let args_iter: Vec<SteelVal> = args.into_iter().map(|x| (*x).clone()).collect();
            let mut args_iter = args_iter.iter();
            let first = args_iter.next().ok_or(SteelErr::ArityMismatch(
                "expected at least one argument".to_string(),
            ))?;
            // let rest = &floats[1..];
            fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            };
            Ok(Rc::new(SteelVal::BoolV(f(&first, args_iter))))
        }
    }};
}

#[macro_use]
macro_rules! gen_pred {
    ($variant:ident) => {{
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.first() {
                if let SteelVal::$variant(..) = first.as_ref() {
                    return Ok(Rc::new(BoolV(true)));
                }
            }
            Ok(Rc::new(BoolV(false)))
        })
    }};
}

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

pub struct Env {
    bindings: HashMap<String, Rc<SteelVal>>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    /// Make a new `Env` from
    /// another parent `Env`.
    pub fn new(parent: &Rc<RefCell<Self>>) -> Self {
        Env {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(&parent)),
        }
    }

    /// top level global env has no parent
    pub fn root() -> Self {
        Env {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn clear_bindings(&mut self) {
        self.bindings.clear();
    }

    /// Within the current environment, bind
    /// identifier `key` to `val`
    pub fn define(&mut self, key: String, val: Rc<SteelVal>) {
        self.bindings.insert(key, val);
    }
    /// Within the current environment,
    /// bind identifiers `keys` to `vals`
    /// throws arity mismatch if they don't have the same length
    pub fn define_all(&mut self, keys: &[String], vals: Vec<Rc<SteelVal>>) -> Result<()> {
        let expected_len = keys.len();
        let actual_len = vals.len();
        if expected_len != actual_len {
            let e = format!(
                "function expected {} params, got {}",
                expected_len, actual_len
            );
            stop!(ArityMismatch => e);
        }
        // let iter = keys.iter().map(String::as_ref).zip(vals.into_iter());
        let iter = keys.iter().cloned().zip(vals.into_iter());
        self.define_zipped(iter);
        Ok(())
    }

    pub fn define_zipped(&mut self, zipped: impl Iterator<Item = (String, Rc<SteelVal>)>) {
        zipped.for_each(|(param, arg)| self.define(param, arg))
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// If found, update binding for
    /// `key` with `val` and return old value.
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn set(&mut self, key: String, val: Rc<SteelVal>) -> Result<Rc<SteelVal>> {
        if self.bindings.contains_key(&key) {
            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| SteelErr::FreeIdentifier(key.to_string()))
        } else {
            match &self.parent {
                Some(par) => par.borrow_mut().set(key, val),
                None => stop!(FreeIdentifier => key), // Err(SteelErr::FreeIdentifier(key)),
            }
        }
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// If found, remove the binding and return the value
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn remove(&mut self, key: &str) -> Result<Rc<SteelVal>> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| SteelErr::FreeIdentifier(key.to_string()))
        } else {
            match &self.parent {
                Some(par) => par.borrow_mut().remove(key),
                None => stop!(FreeIdentifier => key), // Err(SteelErr::FreeIdentifier(key.to_string())),
            }
        }
    }

    /// Search starting from the current environment
    /// for `key`, looking through the parent chain in order.
    ///
    /// if found, return that value
    ///
    /// Otherwise, error with `FreeIdentifier`
    pub fn lookup(&self, name: &str) -> Result<Rc<SteelVal>> {
        if self.bindings.contains_key(name) {
            // value needs to be cloned because
            // user needs to be able to own a persistent value
            // from Cell that may be modified later
            Ok(self.bindings[name].clone())
        } else {
            match &self.parent {
                Some(par) => par.borrow().lookup(name),
                None => stop!(FreeIdentifier => name), // Err(SteelErr::FreeIdentifier(name.to_string())),
            }
        }
    }
    /// default environment contains bindings for
    /// implementations of constants and things like
    /// `car`, `cdr`, `+`
    pub fn default_env() -> Env {
        let mut env = Env::root();
        env.define_zipped(
            Env::default_bindings()
                .into_iter()
                .map(|x| (x.0.to_string(), Rc::new(x.1))),
        );
        env
    }
    pub fn default_bindings() -> Vec<(&'static str, SteelVal)> {
        vec![
            ("+", SteelVal::FuncV(Adder::new_func())),
            ("*", SteelVal::FuncV(Multiplier::new_func())),
            ("/", SteelVal::FuncV(Divider::new_func())),
            ("-", SteelVal::FuncV(Subtractor::new_func())),
            ("list", ListOperations::list()),
            ("car", ListOperations::car()),
            ("cdr", ListOperations::cdr()),
            ("cons", ListOperations::cons()),
            ("reverse", ListOperations::reverse()),
            ("range", ListOperations::range()),
            ("vector", VectorOperations::vec_construct()),
            ("push-front", VectorOperations::vec_cons()),
            ("pop-front", VectorOperations::vec_car()),
            ("vec-rest", VectorOperations::vec_cdr()),
            ("null?", VectorOperations::list_vec_null()),
            ("push", VectorOperations::vec_push()),
            ("range-vec", VectorOperations::vec_range()),
            ("number?", gen_pred!(NumV)),
            ("string?", gen_pred!(StringV)),
            ("symbol?", gen_pred!(SymbolV)),
            ("vector?", gen_pred!(VectorV)),
            ("list?", gen_pred!(Pair)),
            ("=", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            ("equal?", SteelVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            (">", SteelVal::FuncV(ensure_tonicity!(|a, b| a > b))),
            (">=", SteelVal::FuncV(ensure_tonicity!(|a, b| a >= b))),
            ("<", SteelVal::FuncV(ensure_tonicity!(|a, b| a < b))),
            ("<=", SteelVal::FuncV(ensure_tonicity!(|a, b| a <= b))),
            ("display", IoFunctions::display()),
            ("newline", IoFunctions::newline()),
        ]
    }
}

pub struct ListOperations {}
impl ListOperations {
    pub fn cons() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => match lst.as_ref() {
                    SteelVal::VectorV(l) => {
                        if l.is_empty() {
                            Ok(Rc::new(SteelVal::Pair(elem, None)))
                        } else {
                            Ok(Rc::new(SteelVal::Pair(elem, Some(lst))))
                        }
                    }
                    _ => Ok(Rc::new(SteelVal::Pair(elem, Some(lst)))),
                },
                _ => stop!(ArityMismatch => "cons-pair takes two arguments"),
            }
        })
    }

    pub fn car() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(car, _) => Ok(Rc::clone(car)),
                    e => {
                        stop!(TypeMismatch => "car takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "car takes one argument");
            }
        })
    }

    pub fn cdr() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(_, cdr) => match cdr {
                        Some(rest) => match rest.as_ref() {
                            Pair(_, _) => Ok(Rc::clone(rest)),
                            _ => Ok(Rc::new(SteelVal::Pair(Rc::clone(rest), None))), // Ok(Rc::clone(rest))
                        },
                        None => Ok(Rc::new(SteelVal::VectorV(Vector::new()))), // TODO
                    },
                    e => {
                        stop!(TypeMismatch => "cdr takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "cdr takes one argument");
            }
        })
    }

    pub fn list() -> SteelVal {
        SteelVal::FuncV(Self::built_in_list_func())
    }

    pub fn range() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (NumV(lower), NumV(upper)) = (elem, lst) {
                        let mut res = Vec::new();
                        for i in lower as usize..upper as usize {
                            res.push(Rc::new(SteelVal::NumV(i as f64)));
                        }
                        Self::built_in_list_func()(res)
                    } else {
                        stop!(TypeMismatch => "range expected number")
                    }
                }
                _ => stop!(ArityMismatch => "range takes two arguments"),
            }
        })
    }

    pub fn reverse() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                match &args[0].as_ref() {
                    SteelVal::Pair(_, _) => {
                        let mut lst = Self::collect_into_vec(&args[0])?;
                        lst.reverse();
                        Self::built_in_list_func()(lst)
                    }
                    SteelVal::VectorV(v) => Ok(Rc::new(SteelVal::BoolV(v.is_empty()))),
                    _ => Ok(Rc::new(SteelVal::BoolV(false))),
                }
            } else {
                stop!(ArityMismatch => "reverse takes one argument");
            }
        })
    }

    pub fn collect_into_vec(mut p: &Rc<SteelVal>) -> Result<Vec<Rc<SteelVal>>> {
        let mut lst = Vec::new();
        // let mut p = &args[0];

        loop {
            match p.as_ref() {
                SteelVal::Pair(cons, cdr) => {
                    lst.push(Rc::clone(cons));
                    match cdr.as_ref() {
                        Some(rest) => match rest.as_ref() {
                            Pair(_, _) => p = rest,
                            _ => {
                                lst.push(Rc::clone(rest));
                                break;
                            }
                        },
                        None => break,
                    }
                }
                _ => stop!(TypeMismatch => "reverse expected a list"),
            }
        }

        Ok(lst)
    }

    pub fn built_in_list_func() -> fn(Vec<Rc<SteelVal>>) -> Result<Rc<SteelVal>> {
        |args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().rev();
            let mut pairs = Vec::new();
            match (args.next(), args.next()) {
                (cdr, Some(car)) => {
                    pairs.push(Rc::new(SteelVal::Pair(car, cdr)));
                }
                (Some(cdr), None) => {
                    pairs.push(Rc::new(SteelVal::Pair(cdr, None)));
                }
                (_, _) => {
                    return Ok(Rc::new(SteelVal::VectorV(Vector::new())));
                }
            }

            for (i, val) in args.enumerate() {
                pairs.push(Rc::new(SteelVal::Pair(val, Some(Rc::clone(&pairs[i])))));
            }
            pairs
                .pop()
                .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string()))
        }
    }
}

pub struct VectorOperations {}
impl VectorOperations {
    pub fn vec_construct() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            Ok(Rc::new(SteelVal::VectorV(
                args.into_iter().map(|x| (*x).clone()).collect(),
            )))
        })
    }

    pub fn vec_append() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let lsts: Vector<SteelVal> =
                unwrap_list_of_lists(args.into_iter().map(|x| (*x).clone()).collect())?
                    .into_iter()
                    .flatten()
                    .collect();
            Ok(Rc::new(SteelVal::VectorV(lsts)))
        })
    }

    pub fn vec_range() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (NumV(lower), NumV(upper)) = (elem, lst) {
                        let mut res = Vector::new();
                        for i in lower as usize..upper as usize {
                            res.push_back(SteelVal::NumV(i as f64));
                        }
                        Ok(Rc::new(SteelVal::VectorV(res)))
                    } else {
                        stop!(TypeMismatch => "range expected number")
                    }
                }
                _ => stop!(ArityMismatch => "range takes two arguments"),
            }
        })
    }

    pub fn vec_push() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(ref l) = lst {
                        let mut l = l.clone();
                        l.push_back(elem);
                        Ok(Rc::new(SteelVal::VectorV(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(elem);
                        new.push_front(lst);
                        Ok(Rc::new(SteelVal::VectorV(new)))
                    }
                }
                _ => stop!(ArityMismatch => "push takes two arguments"),
            }
        })
    }

    pub fn vec_cons() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(ref l) = lst {
                        let mut l = l.clone();
                        l.push_front(elem);
                        Ok(Rc::new(SteelVal::VectorV(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(lst);
                        new.push_front(elem);
                        Ok(Rc::new(SteelVal::VectorV(new)))
                    }
                }
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn vec_car() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.into_iter().map(|x| (*x).clone()).next() {
                match first {
                    SteelVal::VectorV(ref e) => {
                        let mut e = e.clone();
                        match e.pop_front() {
                            Some(e) => Ok(Rc::new(e)),
                            None => stop!(ContractViolation => "car expects a non empty list"),
                        }
                    }
                    e => {
                        stop!(TypeMismatch => "car takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "car takes one argument");
            }
        })
    }

    pub fn vec_cdr() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.into_iter().map(|x| (*x).clone()).next() {
                match first {
                    SteelVal::VectorV(ref e) => {
                        let mut e = e.clone();
                        if !e.is_empty() {
                            e.pop_front();
                            Ok(Rc::new(SteelVal::VectorV(e)))
                        } else {
                            stop!(ContractViolation => "cdr expects a non empty list")
                        }
                    }
                    e => {
                        stop!(TypeMismatch => "cdr takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "cdr takes one argument");
            }
        })
    }

    pub fn list_vec_null() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                match &args[0].as_ref() {
                    SteelVal::VectorV(v) => Ok(Rc::new(SteelVal::BoolV(v.is_empty()))),
                    _ => Ok(Rc::new(SteelVal::BoolV(false))),
                }
            } else {
                stop!(ArityMismatch => "null? takes one argument");
            }
        })
    }
}

pub struct IoFunctions {}
impl IoFunctions {
    pub fn display() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                let print_val = (*args[0]).clone();
                print!("{:?}", print_val);
                Ok(Rc::new(SteelVal::Void))
            } else {
                stop!(ArityMismatch => "display takes one argument");
            }
        })
    }

    pub fn newline() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 0 {
                println!("");
                Ok(Rc::new(SteelVal::Void))
            } else {
                stop!(ArityMismatch => "newline takes no arguments");
            }
        })
    }
}

fn unwrap_list_of_lists(args: Vec<SteelVal>) -> Result<Vec<Vector<SteelVal>>> {
    args.iter().map(unwrap_single_list).collect()
}

fn unwrap_single_list(exp: &SteelVal) -> Result<Vector<SteelVal>> {
    match exp {
        SteelVal::VectorV(lst) => Ok(lst.clone()),
        _ => stop!(TypeMismatch => "expected a list"),
    }
}

#[cfg(test)]
mod env_tests {
    use super::*;
    fn unwrap_single_float(exp: &SteelVal) -> Result<f64> {
        match exp {
            SteelVal::NumV(num) => Ok(*num),
            _ => stop!(TypeMismatch => "expected a number"),
        }
    }
    #[test]
    fn env_basic() {
        // default_env <- c1 <- c2
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(default_env.borrow().lookup("+").is_ok());
        let c1 = Rc::new(RefCell::new(Env::new(&default_env)));
        c1.borrow_mut()
            .define("x".to_owned(), Rc::new(SteelVal::NumV(1.0)));
        let c2 = Rc::new(RefCell::new(Env::new(&c1)));
        c2.borrow_mut()
            .define("y".to_owned(), Rc::new(SteelVal::NumV(2.0)));
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
