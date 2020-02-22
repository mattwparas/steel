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

use crate::env::{default_env, Env, EnvRef};
use crate::lexer::{Token, TokenError, Tokenizer};
use crate::parser::{Expr, ParseError, Parser};
use crate::rerrs::RucketErr;
use crate::rvals::{RucketLambda, RucketVal};

pub fn evaluator(expr: Expr) -> result::Result<RucketVal, RucketErr> {
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

                        // Let is actually just a lambda so update values to be that and loop
                        // Syntax of a let -> (let ((a 10) (b 20) (c 25)) (body ...))
                        // transformed ((lambda (a b c) (body ...)) 10 20 25)
                        Expr::Atom(Token::Let) => {
                            // should have form ((a 10) (b 20) (c 25))
                            let bindings = list_of_tokens.get(1).ok_or_else(|| {
                                return RucketErr::MalformedLet;
                            })?;

                            let body = list_of_tokens.get(2).ok_or_else(|| {
                                return RucketErr::MalformedLet;
                            })?;

                            let mut bindings_to_check: Vec<Expr> = Vec::new();
                            let mut args_to_check: Vec<Expr> = Vec::new();

                            // TODO fix this noise
                            match bindings {
                                Expr::ListVal(list_of_pairs) => {
                                    for pair in list_of_pairs {
                                        match pair {
                                            Expr::ListVal(p) => match p.as_slice() {
                                                [binding, expression] => {
                                                    bindings_to_check.push(binding.clone());
                                                    args_to_check.push(expression.clone());
                                                }
                                                _ => {
                                                    unimplemented!();
                                                }
                                            },
                                            _ => {
                                                unimplemented!();
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    unimplemented!();
                                }
                            }

                            let new_lambda = vec![
                                Expr::Atom(Token::Lambda),
                                Expr::ListVal(bindings_to_check),
                                body.clone(),
                            ];

                            let mut combined = vec![Expr::ListVal(new_lambda)];
                            combined.append(&mut args_to_check);

                            let application = Expr::ListVal(combined);
                            expr = application;
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

                                let params = lambda.params_exp();

                                let param_arg_iter = params.iter().zip(good_args_eval.iter());

                                for (param, arg) in param_arg_iter {
                                    inner_env.define(param.to_string(), arg.clone())
                                }

                                env = inner_env.into_ref();
                                expr = lambda.body_exp();
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
