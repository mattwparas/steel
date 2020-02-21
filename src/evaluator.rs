#![allow(dead_code)] // TODO
#![allow(unused_imports)] // TODO

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;
use std::iter::{Iterator, Peekable};
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
    params_exp: Vec<RucketVal>,
    body_exp: Expr,
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
    pub fn lookup(&self, var: &str) -> result::Result<RucketVal, RucketErr> {
        let mut p = Some(self);
        while let Some(par) = p {
            if let Some(b) = par.bindings.get(var) {
                return Ok(b.clone());
            } else {
                p = par.parent;
            }
        }
        Err(RucketErr::FreeIdentifier(var.to_string()))
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
                    return env.lookup(&s);
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

                    match f {
                        Expr::Atom(Token::If) => {
                            // establish that it needs to be length 3?
                            if list_of_tokens.len() != 4 {
                                return Err(RucketErr::ArityMismatch(
                                    "If takes 3 arguments".to_string(),
                                ));
                            }

                            let test_expr = list_of_tokens.get(1).unwrap();
                            let then_expr = list_of_tokens.get(2).unwrap();
                            let else_expr = list_of_tokens.get(3).unwrap();

                            // let test_expr = list_of_tokens.remove(1);
                            // let then_expr = list_of_tokens.remove(1);
                            // let else_expr = list_of_tokens.remove(1);

                            // list_of_tokens[0]

                            match evaluate(test_expr.clone(), env)? {
                                RucketVal::BoolV(true) => {
                                    expr = then_expr.clone();
                                }
                                _ => {
                                    expr = else_expr.clone();
                                }
                            }
                        }

                        // Expr::Atom(Token::Define) => {
                        //     todo!();
                        // }

                        // Expr::Atom(Token::Lambda) => {
                        //     todo!();
                        // }

                        // Expr::Atom(Token::Let) => {
                        //     todo!();
                        // }
                        sym => match evaluate(sym.clone(), env)? {
                            RucketVal::FuncV(func) => {
                                let args_eval: Result<Vec<RucketVal>, RucketErr> =
                                    eval_iter.map(|x| evaluate(x.clone(), env)).collect();
                                return func(&args_eval?);
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
