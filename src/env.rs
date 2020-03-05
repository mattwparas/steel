use crate::evaluator::Result;
// #[macro_use]
use crate::converter::RucketFunctor;
use crate::primitives::{Adder, Divider, Multiplier, Subtractor};
use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use crate::stop;
use std::result;

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

#[macro_export]
macro_rules! try_from_impl {
    ($type:ident => $($body:ty),*) => {
        $(
            impl TryFrom<RucketVal> for $body {
                type Error = RucketErr;
                fn try_from(value: RucketVal) -> result::Result<Self, Self::Error> {
                    match value {
                        RucketVal::$type(x) => Ok(x as $body),
                        _ => Err(RucketErr::ConversionError("Expected number".to_string())),
                    }
                }
            }
        )*
    };
}

try_from_impl!(NumV => f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);
try_from_impl!(StringV => String);

#[macro_export]
macro_rules! implement {
    ($type:ty) => {
        as_item! {
            impl crate::rvals::CustomType for $type {
                fn box_clone(&self) -> Box<dyn CustomType> {
                    Box::new((*self).clone())
                }
                fn as_any(&self) -> Box<dyn Any> {
                    Box::new((*self).clone())
                }
                fn new_rucket_val(&self) -> RucketVal {
                    RucketVal::Custom(Box::new(self.clone()))
                }
                // fn generate_bindings() -> Vec<(&'static str, RucketVal)> {
                //     vec![]
                // }
            }
        }

        // as_item! {
        //     impl TryFrom<RucketVal> for $type {
        //         type Error = RucketErr;
        //         fn try_from(value: RucketVal) -> result::Result<Self, Self::Error> {
        //             Ok(value.new_rucket_val())
        //         }
        //     }
        // }

        as_item! {
            impl From<$type> for RucketVal {
                fn from(val: $type) -> RucketVal {
                    val.new_rucket_val()
                }
            }
        }

        // as_item! {
        //     impl From<RucketVal> for $type {
        //         fn from(val: RucketVal) -> $type {
        //             println!("inside from rucketval to {}", stringify!($type));
        //             unwrap!(val, $type).unwrap()
        //             // println!("{}", val);
        //             // match val {
        //                 // RucketVal::Custom(_) => unwrap!(val, $type).unwrap(),
        //                 // _ => <$type>::from(val),
        //             // }
        //         }
        //     }
        // }

    };

    ($type:ident, $($e:ident, $t: ty),*) => {
        as_item! {
            impl crate::rvals::CustomType for $type {
                fn box_clone(&self) -> Box<dyn CustomType> {
                    Box::new((*self).clone())
                }
                fn as_any(&self) -> Box<dyn Any> {
                    Box::new((*self).clone())
                }
                fn new_rucket_val(&self) -> RucketVal {
                    RucketVal::Custom(Box::new(self.clone()))
                }
            }
        }

        as_item! {
            impl From<$type> for RucketVal {
                fn from(val: $type) -> RucketVal {
                    val.new_rucket_val()
                }
            }
        }

        as_item! {
            impl From<RucketVal> for $type {
                fn from(val: RucketVal) -> $type {
                    println!("inside from rucketval to {}", stringify!($type));
                    unwrap!(val, $type).unwrap()
                }
            }
        }

        as_item! {
            impl $type {
                pub fn generate_bindings() -> Vec<(&'static str, RucketVal)> {
                    use std::convert::TryFrom;
                    use crate::rvals::RucketVal;
                    use crate::rerrs::RucketErr;
                    use crate::unwrap;
                    use crate::stop;
                    let mut vec_binding = vec![];

                    // generate predicate
                    let name = concat!(stringify!($type), "?");
                    println!("{}", name);
                    let func =
                         RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                            let mut args_iter = args.into_iter();
                            if let Some(first) = args_iter.next() {
                                return Ok(RucketVal::BoolV(unwrap!(first, $type).is_ok()));
                            }
                            stop!(ArityMismatch => "set! expected 2 arguments");
                        });
                    vec_binding.push((name, func));
                    $(
                        // generate setters
                        let name = concat!("set-", stringify!($type), "-", stringify!($e), "!");
                        let func =
                             RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                                let mut args_iter = args.into_iter();
                                if let Some(first) = args_iter.next() {
                                    if let Some(second) = args_iter.next() {
                                        println!("{}", stringify!($type));
                                        let my_struct = unwrap!(first, $type)?;
                                        println!("We got after the unwrap!");
                                        println!("{:?}", my_struct);
                                        let new_struct = $type {
                                            $e : match second {
                                                RucketVal::Custom(_) => {
                                                    println!("Inside custom: {}", stringify!($t));
                                                    unwrap!(second, $t)?
                                                },
                                                _ => {
                                                    print!("Inside else: {}", second);
                                                    <$t>::try_from(second)?
                                                 }
                                            },
                                            ..my_struct
                                        };
                                        return Ok(new_struct.new_rucket_val());
                                    }
                                    stop!(ArityMismatch => "set! expected 2 arguments");
                                }
                                stop!(ArityMismatch => "set! expected 2 arguments");
                            });
                        vec_binding.push((name, func));

                        // generate getters
                        let name = concat!(stringify!($type), "-", stringify!($e));
                        println!("{}", name);
                        let func =
                             RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
                                let mut args_iter = args.into_iter();
                                if let Some(first) = args_iter.next() {
                                    let my_struct = unwrap!(first, $type)?;
                                    return Ok(my_struct.$e.into());
                                }
                                stop!(ArityMismatch => "set! expected 2 arguments");
                            });
                        vec_binding.push((name, func));
                    ) *
                    vec_binding
                }
            }
        }
    };
}

// #[macro_use]
macro_rules! as_item {
    ($i:item) => {
        $i
    };
}

// macro_rules! as_expr {
//     ($e:expr) => {
//         #e
//     };
// }

#[macro_use]
macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: Vec<RucketVal>| -> Result<RucketVal> {
            // let floats = unwrap_list_of_floats(args)?;
            let mut args_iter = args.iter();
            let first = args_iter.next().ok_or(RucketErr::ContractViolation(
                "expected at least one argument".to_string(),
            ))?;
            // let rest = &floats[1..];
            fn f<'a>(prev: &RucketVal, mut xs: impl Iterator<Item = &'a RucketVal>) -> bool {
                match xs.next() {
                    Some(x) => $check_fn(prev, x) && f(x, xs),
                    None => true,
                }
            };
            Ok(RucketVal::BoolV(f(first, args_iter)))
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
    pub fn define(&mut self, key: String, val: RucketVal) {
        self.bindings.insert(key, val);
    }
    /// Within the current environment,
    /// bind identifiers `keys` to `vals`
    /// throws arity mismatch if they don't have the same length
    pub fn define_all(&mut self, keys: &[String], vals: Vec<RucketVal>) -> Result<()> {
        let expected_len = keys.len();
        let actual_len = vals.len();
        if expected_len != actual_len {
            let e = format!(
                "function expected {} params, got {}",
                expected_len, actual_len
            );
            stop!(ArityMismatch => e);
        }
        let iter = keys.iter().map(String::as_ref).zip(vals.into_iter());
        self.define_zipped(iter);
        Ok(())
    }

    pub fn define_zipped<'a>(&mut self, zipped: impl Iterator<Item = (&'a str, RucketVal)>) {
        zipped.for_each(|(param, arg)| self.define(param.to_string(), arg))
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
    /// default environment contains bindings for
    /// implementations of constants and things like
    /// `car`, `cdr`, `+`
    pub fn default_env() -> Env {
        let mut env = Env::root();
        env.define_zipped(Env::default_bindings().into_iter());
        env
    }
    pub fn default_bindings() -> Vec<(&'static str, RucketVal)> {
        vec![
            ("+", RucketVal::FuncV(Adder::new_func())),
            ("*", RucketVal::FuncV(Multiplier::new_func())),
            ("/", RucketVal::FuncV(Divider::new_func())),
            ("-", RucketVal::FuncV(Subtractor::new_func())),
            (
                "list",
                RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal> {
                    Ok(RucketVal::ListV(args))
                }),
            ),
            (
                "cons",
                RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal> {
                    let mut args = args.into_iter();
                    match (args.next(), args.next()) {
                        (Some(elem), Some(lst)) => {
                            if let RucketVal::ListV(mut l) = lst {
                                l.insert(0, elem);
                                return Ok(RucketVal::ListV(l));
                            } else {
                                return Ok(RucketVal::ListV(vec![elem, lst]));
                            }
                        }
                        _ => stop!(ArityMismatch => "cons takes two arguments"),
                    }
                }),
            ),
            (
                "null?",
                RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal> {
                    if args.len() == 1 {
                        match &args[0] {
                            RucketVal::ListV(v) => {
                                if v.is_empty() {
                                    return Ok(RucketVal::BoolV(true));
                                } else {
                                    return Ok(RucketVal::BoolV(false));
                                }
                            }
                            _ => Ok(RucketVal::BoolV(false)),
                        }
                    } else {
                        stop!(ArityMismatch => "car takes one argument");
                    }
                }),
            ),
            (
                "append",
                RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal> {
                    let lsts: Vec<RucketVal> =
                        unwrap_list_of_lists(args)?.into_iter().flatten().collect();
                    Ok(RucketVal::ListV(lsts))
                }),
            ),
            (
                "car",
                RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal> {
                    if let Some(first) = args.into_iter().next() {
                        match first {
                            RucketVal::ListV(e) => match e.into_iter().next() {
                                Some(e) => Ok(e),
                                None => stop!(ContractViolation => "car expects a non empty list"),
                            },
                            e => {
                                stop!(ExpectedList => "car takes a list, given: {}", e);
                            }
                        }
                    } else {
                        stop!(ArityMismatch => "car takes one argument");
                    }
                }),
            ),
            (
                "cdr",
                RucketVal::FuncV(|args: Vec<RucketVal>| -> Result<RucketVal> {
                    if let Some(first) = args.into_iter().next() {
                        match first {
                            RucketVal::ListV(e) => {
                                if !e.is_empty() {
                                    Ok(RucketVal::ListV(e.into_iter().skip(1).collect()))
                                } else {
                                    stop!(ContractViolation => "cdr expects a non empty list")
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
            ),
            ("=", RucketVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            ("equal?", RucketVal::FuncV(ensure_tonicity!(|a, b| a == b))),
            (">", RucketVal::FuncV(ensure_tonicity!(|a, b| a > b))),
            (">=", RucketVal::FuncV(ensure_tonicity!(|a, b| a >= b))),
            ("<", RucketVal::FuncV(ensure_tonicity!(|a, b| a < b))),
            ("<=", RucketVal::FuncV(ensure_tonicity!(|a, b| a <= b))),
        ]
    }
}

fn unwrap_list_of_lists(args: Vec<RucketVal>) -> Result<Vec<Vec<RucketVal>>> {
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
    fn unwrap_single_float(exp: &RucketVal) -> Result<f64> {
        match exp {
            RucketVal::NumV(num) => Ok(*num),
            _ => stop!(ExpectedNumber => "expected a number"),
        }
    }
    #[test]
    fn env_basic() {
        // default_env <- c1 <- c2
        let default_env = Rc::new(RefCell::new(Env::default_env()));
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
