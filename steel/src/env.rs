// use crate::rvals::Result;
// #[macro_use]
use crate::primitives::IoFunctions;
use crate::primitives::ListOperations;
use crate::primitives::StringOperations;
use crate::primitives::VectorOperations;
use crate::primitives::{Adder, Divider, Multiplier, SteelFunctor, Subtractor};
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    pub static VOID: Rc<SteelVal> = Rc::new(SteelVal::Void);
    pub static TRUE: Rc<SteelVal> = Rc::new(SteelVal::BoolV(true));
    pub static FALSE: Rc<SteelVal> = Rc::new(SteelVal::BoolV(false));
}

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let args_iter: Vec<SteelVal> = args.into_iter().map(|x| (*x).clone()).collect();
            let mut args_iter = args_iter.iter();
            let first = args_iter.next().ok_or(SteelErr::ArityMismatch(
                "expected at least one argument".to_string(),
            ))?;
            fn f<'a>(prev: &SteelVal, mut xs: impl Iterator<Item = &'a SteelVal>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            };
            let res = f(&first, args_iter);
            if res {
                Ok(TRUE.with(|f| Rc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Rc::clone(f)))
            }
        }
    }};
}

#[macro_use]
macro_rules! gen_pred {
    ($variant:ident) => {{
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.first() {
                if let SteelVal::$variant(..) = first.as_ref() {
                    return Ok(TRUE.with(|f| Rc::clone(f)));
                }
            }
            Ok(FALSE.with(|f| Rc::clone(f)))
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
    fn default_bindings() -> Vec<(&'static str, SteelVal)> {
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
            ("list->vector", ListOperations::list_to_vec()),
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
            ("read-to-string", IoFunctions::read_to_string()),
            ("string-append", StringOperations::string_append()),
        ]
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
