#![allow(dead_code)] // TODO
#![allow(unused_imports)] // TODO

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use std::iter::{Iterator, Peekable};
use std::result;
use std::str::Chars;

use crate::lexer::{Token, TokenError, Tokenizer};
use crate::parser::{Expr, ParseError, Parser};

#[derive(Clone)]
pub enum RucketVal {
    BoolV(bool),
    NumV(f64),
    ListV(Vec<RucketVal>),
    StringV(String),
    FuncV(fn(&[RucketVal]) -> result::Result<RucketVal, RucketErr>),
}

impl fmt::Display for RucketVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RucketVal::BoolV(b) => write!(f, "#{}", b),
            RucketVal::NumV(x) => write!(f, "{}", x),
            RucketVal::StringV(s) => write!(f, "{}", s),
            _ => write!(f, "display not implemented"), // RucketVal::ListV(x) => write!(f, "()")
        }
    }
}

#[derive(Clone, Debug)]
pub enum RucketErr {
    ArityMismatch(String),
    ExpectedNumber(String),
    FreeIdentifier(String),
    ApplicationNotAProcedure(String),
}

impl fmt::Display for RucketErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RucketErr::FreeIdentifier(s) => write!(f, "Free Identifier: {}", s),
            RucketErr::ArityMismatch(s) => write!(f, "Arity Mismatch: {}", s),
            RucketErr::ExpectedNumber(s) => write!(f, "Expected Number: {}", s),
            RucketErr::ApplicationNotAProcedure(s) => write!(f, "ApplicationNotAProcedure: {}", s),
        }
    }
}

// #[derive(Clone)]
pub struct Env<'a> {
    bindings: HashMap<String, RucketVal>,
    parent: Option<&'a Env<'a>>,
}

// TODO
// Think about functional data structures to avoid cloning here
impl<'a> Env<'a> {
    // TODO
    pub fn new(parent: Option<&'a Env>) -> Env<'a> {
        Env {
            bindings: HashMap::new(),
            parent,
        }
    }

    // TODO
    pub fn lookup(&self, var: String) -> result::Result<RucketVal, RucketErr> {
        let mut p = Some(self);
        while let Some(par) = p {
            if let Some(b) = par.bindings.get(&var) {
                return Ok(b.clone());
            } else {
                p = par.parent;
            }
        }
        Err(RucketErr::FreeIdentifier(var))
    }

    pub fn insert_binding(&mut self, var: String, val: RucketVal) {
        self.bindings.insert(var, val);
    }
}

fn default_env<'a>() -> Env<'a> {
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
        parent: None,
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
    let mut global_env = default_env();
    evaluate(expr, &mut global_env)
}

pub fn evaluate(mut expr: Expr, env: &mut Env) -> result::Result<RucketVal, RucketErr> {
    loop {
        match expr {
            Expr::Atom(t) => match t {
                Token::BooleanLiteral(b) => {
                    return Ok(RucketVal::BoolV(b));
                }
                Token::Identifier(s) => {
                    return env.lookup(s);
                } // TODO
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
                    match evaluate(f.clone(), env)? {
                        RucketVal::FuncV(func) => {
                            // println!("We have found a function");
                            let args_eval: Result<Vec<RucketVal>, RucketErr> =
                                eval_iter.map(|x| evaluate(x.clone(), env)).collect();

                            // for i in args_eval? {
                            //     println!("{}", i);
                            // }
                            // returnErr(RucketErr::ApplicationNotAProcedure(e.to_string()))
                            let res = func(&args_eval?);
                            // println!("result: {}", res.clone().unwrap());
                            return res;
                            // return Err(RucketErr::ApplicationNotAProcedure(.to_string()));
                        }
                        e => {
                            return Err(RucketErr::ApplicationNotAProcedure(e.to_string()));
                        }
                    }
                } else {
                    return Err(RucketErr::ApplicationNotAProcedure("TODO".to_string()));
                }
            }
        }
    }
}

// TODO write macro to destructure vector
