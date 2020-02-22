#![allow(dead_code)] // TODO
#![allow(unused_imports)] // TODO

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use std::iter::{Iterator, Peekable};
use std::rc::Rc;
use std::result;
use std::str::Chars;
use thiserror::Error;

use crate::lexer::{Token, TokenError, Tokenizer};
use crate::parser::{Expr, ParseError, Parser};

#[derive(Clone)]
pub enum RucketVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<RucketVal>),
    StringV(String),
    FuncV(fn(&[RucketVal]) -> result::Result<RucketVal, RucketErr>),
    LambdaV(RucketLambda),
}

#[derive(Clone)]
pub struct RucketLambda {
    params_exp: Vec<String>,
    body_exp: Expr,
}

impl RucketLambda {
    pub fn new(params_exp: Vec<String>, body_exp: Expr) -> RucketLambda {
        RucketLambda {
            params_exp,
            body_exp,
        }
    }
}

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

impl fmt::Display for RucketVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RucketVal::BoolV(b) => write!(f, "#{}", b),
            RucketVal::NumV(x) => write!(f, "{}", x),
            RucketVal::StringV(s) => write!(f, "{}", s),
            RucketVal::ListV(lst) => {
                let lst = lst
                    .iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>();
                write!(f, "{:?}", lst)
            }
            RucketVal::FuncV(_) => write!(f, "Function"),
            RucketVal::LambdaV(_) => write!(f, "Anonymous Function"),
            _ => write!(f, "display not implemented"), // RucketVal::ListV(x) => write!(f, "()")
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum RucketErr {
    #[error("Arity Mismatch")]
    ArityMismatch(String),
    #[error("Expected Number, got {0}")]
    ExpectedNumber(String),
    #[error("Free Identifier: {0}")]
    FreeIdentifier(String),
    #[error("Expected Function: {0}")]
    ExpectedFunction(String),
    #[error("Expected identifier: {0}")]
    ExpectedIdentifier(String),
    #[error("Expected arguments to lambda: {0}")]
    ExpectedArgumentsToLambda(String),
    #[error("Environment Not Found")]
    EnvironmentNotFound,
}

#[derive(Clone)]
pub struct Env {
    bindings: HashMap<String, RucketVal>,
    parent: EnvRef,
}

// TODO
// Think about functional data structures to avoid cloning here
impl Env {
    // TODO
    pub fn new(parent: EnvRef) -> Env {
        Env {
            bindings: HashMap::new(),
            parent: parent,
        }
    }

    // pub fn add_env(&'a self) -> Env<'a> {
    //     Env::new(Some(self))
    // }

    // TODO
    // pub fn lookup(&self, var: &str) -> result::Result<RucketVal, RucketErr> {
    //     let mut p = Some(self);
    //     while let Some(par) = p {
    //         if let Some(b) = par.bindings.get(var) {
    //             return Ok(b.clone());
    //         } else {
    //             p = par.parent;
    //         }
    //     }
    //     Err(RucketErr::FreeIdentifier(var.to_string()))
    // }

    pub fn insert_binding(&mut self, var: String, val: RucketVal) {
        self.bindings.insert(var, val);
    }

    // pub fn new(parent: EnvRef) -> Env {
    //     Env {
    //         parent,
    //         values: HashMap::new(),
    //     }
    // }

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

    pub fn lookup(&self, name: &str) -> Result<RucketVal, RucketErr> {
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

    pub fn set(&mut self, key: String, val: RucketVal) -> Result<RucketVal, RucketErr> {
        if self.bindings.contains_key(&key) {
            self.bindings
                .insert(key.clone(), val)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else if self.parent.is_some() {
            self.parent.set(key, val)
        } else {
            Err(RucketErr::FreeIdentifier(key))
            // bail!(UnboundVar => key)
        }
    }

    pub fn remove(&mut self, key: &str) -> Result<RucketVal, RucketErr> {
        if self.bindings.contains_key(key) {
            self.bindings
                .remove(key)
                .ok_or_else(|| RucketErr::FreeIdentifier(key.to_string()))
        } else if self.parent.is_some() {
            self.parent.remove(key)
        } else {
            Err(RucketErr::FreeIdentifier(key.to_string()))
            // bail!(UnboundVar => key)
        }
    }

    pub fn pack(&mut self, keys: &[String], vals: Vec<RucketVal>) {
        for (i, arg) in vals.into_iter().enumerate() {
            self.bindings.insert(keys[i].clone(), arg);
        }
    }
}

#[derive(Clone)]
pub struct EnvRef(RcRefCell<Option<Env>>);

impl EnvRef {
    // pub fn new() -> EnvRef {
    //     EnvRef {
    //         env: Env::new(None),
    //     }
    // }

    // pub fn update_env(&mut self) {
    //     self.env = Env::new(Some(&self.env));
    // }

    // pub fn lookup(&self, var: &str) -> result::Result<RucketVal, RucketErr> {
    //     self.env.lookup(var)
    // }

    // pub fn insert_binding(&mut self, var: String, val: RucketVal) {
    //     self.env.insert_binding(var, val);
    // }

    // pub fn default_env_ref() -> EnvRef {
    //     EnvRef { env: default_env() }
    // }

    /// A null environment.
    /// Used as parent environment of global environment.
    pub fn null() -> EnvRef {
        EnvRef(new_rc_ref_cell(None))
    }

    pub fn new(env: Env) -> EnvRef {
        EnvRef(new_rc_ref_cell(Some(env)))
    }

    pub fn is_some(&self) -> bool {
        self.0.borrow().as_ref().is_some()
    }

    pub fn clone_ref(&self) -> EnvRef {
        EnvRef(Rc::clone(&self.0))
    }

    pub fn lookup(&self, name: &str) -> Result<RucketVal, RucketErr> {
        self.0
            .borrow()
            .as_ref()
            .ok_or_else(|| RucketErr::EnvironmentNotFound)?
            .lookup(name)
    }

    /// Use this function to get a real reference to what is inside the Environment,
    /// not a copy of it. Useful for Ports particularly.
    /// It's impossible to return a reference to something inside a RefCell.
    /// (Actually it's quite possible trough std::cell::Ref but not in this
    /// particular case) So we need this extra functions.
    // pub fn with_ref<F, T>(&self, name: &str, f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     self.0
    //         .borrow()
    //         .as_ref()
    //         .ok_or_else(|| RucketErr::EnvironmentNotFound)?
    //         .with_ref(name, f)
    // }

    // pub fn with_mut_ref<F, T>(&self, name: &str, f: F) -> Result<RucketVal, RucketErr>
    // where
    //     F: FnMut(&mut RucketVal) -> Result<RucketVal, RucketErr>,
    // {
    //     self.0
    //         .borrow_mut()
    //         .as_mut()
    //         .ok_or_else(|| RucketErr::EnvironmentNotFound)?
    //         .with_mut_ref(name, f)
    // }

    pub fn define(&self, key: String, val: RucketVal) {
        self.0
            .borrow_mut()
            .as_mut()
            .expect("Can't find environment")
            .define(key, val);
    }

    pub fn set(&self, key: String, val: RucketVal) -> Result<RucketVal, RucketErr> {
        self.0
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| RucketErr::EnvironmentNotFound)?
            .set(key, val)
    }

    pub fn remove(&self, key: &str) -> Result<RucketVal, RucketErr> {
        self.0
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| RucketErr::EnvironmentNotFound)?
            .remove(key)
    }
}

fn default_env() -> Env {
    let mut data: HashMap<String, RucketVal> = HashMap::new();
    data.insert(
        "+".to_string(),
        RucketVal::FuncV(
            |args: &[RucketVal]| -> result::Result<RucketVal, RucketErr> {
                let sum = unwrap_list_of_floats(args)?
                    .iter()
                    .fold(0.0, |sum, a| sum + a);

                Ok(RucketVal::NumV(sum))
            },
        ),
    );

    data.insert(
        "-".to_string(),
        RucketVal::FuncV(
            |args: &[RucketVal]| -> result::Result<RucketVal, RucketErr> {
                let floats = unwrap_list_of_floats(args)?;
                let first = *floats.first().ok_or(RucketErr::ArityMismatch(
                    "expected at least one number".to_string(),
                ))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RucketVal::NumV(first - sum_of_rest))
            },
        ),
    );

    Env {
        bindings: data,
        parent: EnvRef::null(),
    }
}

fn unwrap_list_of_floats(args: &[RucketVal]) -> result::Result<Vec<f64>, RucketErr> {
    args.iter().map(|x| unwrap_single_float(x)).collect()
}

fn unwrap_single_float(exp: &RucketVal) -> result::Result<f64, RucketErr> {
    match exp {
        RucketVal::NumV(num) => Ok(*num),
        _ => Err(RucketErr::ExpectedNumber("expected a number".to_string())),
    }
}

pub fn evaluator(expr: Expr) -> result::Result<RucketVal, RucketErr> {
    // let mut global_env = EnvRef::with_values(default_env());
    let mut global_env = EnvRef::new(default_env());
    evaluate(&expr, &mut global_env)
}

pub fn parse_list_of_identifiers(identifiers: Expr) -> Result<Vec<String>, RucketErr> {
    match identifiers {
        Expr::ListVal(l) => {
            let res: Result<Vec<String>, RucketErr> = l
                .iter()
                .map(|x| match x {
                    Expr::Atom(Token::Identifier(s)) => Ok(s.clone()),
                    _ => Err(RucketErr::ExpectedIdentifier(
                        "Lambda must have symbols as arguments".to_string(),
                    )),
                })
                .collect();
            res
        }
        _ => Err(RucketErr::ExpectedArgumentsToLambda(
            "Malformed lambda arguments".to_string(),
        )),
    }
}

pub fn evaluate(expr: &Expr, env: &EnvRef) -> result::Result<RucketVal, RucketErr> {
    let mut env = env.clone_ref();
    let mut expr = expr.clone();

    loop {
        match expr {
            Expr::Atom(t) => match t {
                Token::BooleanLiteral(b) => {
                    return Ok(RucketVal::BoolV(b));
                }
                Token::Identifier(s) => {
                    // return env.lookup(&s);
                    return env.lookup(&s);
                }
                Token::NumberLiteral(n) => {
                    return Ok(RucketVal::NumV(n));
                }
                Token::StringLiteral(s) => {
                    return Ok(RucketVal::StringV(s));
                }
                _ => {
                    return Err(RucketErr::FreeIdentifier("blah".to_string()));
                }
            },

            Expr::ListVal(list_of_tokens) => {
                let mut eval_iter = list_of_tokens.iter();

                if let Some(f) = eval_iter.next() {
                    // let eval_f = evaluate(f, env);

                    match f {
                        Expr::Atom(Token::If) => {
                            // establish that it needs to be length 3?
                            if list_of_tokens.len() != 4 {
                                return Err(RucketErr::ArityMismatch(
                                    "If takes 3 arguments".to_string(),
                                ));
                            }

                            // TODO don't use unwrap
                            let test_expr = list_of_tokens.get(1).unwrap();
                            let then_expr = list_of_tokens.get(2).unwrap();
                            let else_expr = list_of_tokens.get(3).unwrap();

                            // let test_expr = list_of_tokens.remove(1);
                            // let then_expr = list_of_tokens.remove(1);
                            // let else_expr = list_of_tokens.remove(1);

                            // list_of_tokens[0]

                            match evaluate(&test_expr, &env)? {
                                RucketVal::BoolV(true) => {
                                    expr = then_expr.clone();
                                }
                                _ => {
                                    expr = else_expr.clone();
                                }
                            }
                        }

                        // globally scoped potentially
                        Expr::Atom(Token::Define) => {
                            unimplemented!();
                        }

                        // TODO don't use unwrap
                        Expr::Atom(Token::Lambda) => {
                            let list_of_symbols = list_of_tokens.get(1).unwrap();
                            let body_exp = list_of_tokens.get(2).unwrap();

                            let parsed_list = parse_list_of_identifiers(list_of_symbols.clone())?;
                            let constructed_lambda =
                                RucketLambda::new(parsed_list, body_exp.clone());
                            return Ok(RucketVal::LambdaV(constructed_lambda));
                        }

                        Expr::Atom(Token::Let) => {
                            unimplemented!();
                        }

                        sym => match evaluate(&sym, &env)? {
                            RucketVal::FuncV(func) => {
                                let args_eval: Result<Vec<RucketVal>, RucketErr> =
                                    eval_iter.map(|x| evaluate(&x, &env)).collect();
                                return func(&args_eval?);
                            }

                            RucketVal::LambdaV(lambda) => {
                                let args_eval: Result<Vec<RucketVal>, RucketErr> =
                                    eval_iter.map(|x| evaluate(&x, &env)).collect();

                                let mut inner_env = Env::new(env.clone_ref());

                                let good_args_eval = args_eval?;

                                let param_arg_iter =
                                    lambda.params_exp.iter().zip(good_args_eval.iter());

                                for (param, arg) in param_arg_iter {
                                    inner_env.define(param.to_string(), arg.clone())
                                }

                                env = inner_env.into_ref();
                                expr = lambda.body_exp;
                            }
                            e => {
                                return Err(RucketErr::ExpectedFunction(e.to_string()));
                            }
                        },
                    }
                } else {
                    return Err(RucketErr::ExpectedFunction("TODO".to_string()));
                }
            }
        }
    }
}

// TODO write macro to destructure vector
