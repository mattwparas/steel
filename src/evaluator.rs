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

pub struct Evaluator {
    global_env: EnvRef,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            global_env: EnvRef::new(default_env()),
        }
    }
    pub fn eval(&mut self, expr: &Expr) -> Result<RucketVal, RucketErr> {
        evaluate(&expr, &mut self.global_env)
    }
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

/// returns error if tokens.len() != expected
pub fn check_length(what: &str, tokens: &[Expr], expected: usize) -> Result<(), RucketErr> {
    if tokens.len() == expected {
        Ok(())
    } else {
        Err(RucketErr::ArityMismatch(format!(
            "{}: expected {} args got {}",
            what,
            expected,
            tokens.len()
        )))
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
                    return env.lookup(&s);
                }
                Token::NumberLiteral(n) => {
                    return Ok(RucketVal::NumV(n));
                }
                Token::StringLiteral(s) => {
                    return Ok(RucketVal::StringV(s));
                }
                what => {
                    return Err(RucketErr::UnexpectedToken(what.to_string()));
                }
            },

            Expr::ListVal(list_of_tokens) => {
                let mut eval_iter = list_of_tokens.iter();

                if let Some(f) = eval_iter.next() {
                    match f {
                        // (if test then else)
                        Expr::Atom(Token::If) => expr = eval_if(&list_of_tokens, &env)?,
                        // globally scoped potentially
                        Expr::Atom(Token::Define) => {
                            unimplemented!();
                        }
                        Expr::Atom(Token::Quote) => { // TODO 
                            return Ok(RucketVal::SyntaxV(Expr::ListVal(list_of_tokens)))
                        }
                        // (lambda (vars*) (body))
                        Expr::Atom(Token::Lambda) => return eval_make_lambda(&list_of_tokens, env),
                        // (let (var binding)* (body))
                        Expr::Atom(Token::Let) => expr = eval_let(&list_of_tokens, &env)?,
                        // (sym args*), sym must be a procedure
                        sym => match eval_procedure(sym, eval_iter, env) {
                            Ok((RucketVal::SyntaxV(a), b)) => {
                                expr = a;
                                env = b;
                            }
                            Ok((a, _)) => {
                                return Ok(a);
                            }
                            Err(e) => return Err(e),
                        },
                    }
                } else {
                    return Err(RucketErr::ExpectedFunction("TODO".to_string()));
                }
            }
        }
    }
}

/// evaluates `(test then else)` into `then` or `else`
pub fn eval_if(list_of_tokens: &[Expr], env: &EnvRef) -> Result<Expr, RucketErr> {
    check_length("If", list_of_tokens, 4)?;

    // if we check the length beforehand
    // it is guaranteed to exist
    let test_expr = &list_of_tokens[1];
    let then_expr = &list_of_tokens[2];
    let else_expr = &list_of_tokens[3];

    match evaluate(&test_expr, env)? {
        RucketVal::BoolV(true) => Ok(then_expr.clone()),
        _ => Ok(else_expr.clone()),
    }
}

// TODO: actually use the env
pub fn eval_make_lambda(list_of_tokens: &[Expr], env: EnvRef) -> Result<RucketVal, RucketErr> {
    check_length("Lambda", &list_of_tokens, 3)?;
    let list_of_symbols = &list_of_tokens[1];
    let body_exp = &list_of_tokens[2];

    let parsed_list = parse_list_of_identifiers(list_of_symbols.clone())?;
    let constructed_lambda = RucketLambda::new(parsed_list, body_exp.clone(), env);
    Ok(RucketVal::LambdaV(constructed_lambda))
}

// Let is actually just a lambda so update values to be that and loop
// Syntax of a let -> (let ((a 10) (b 20) (c 25)) (body ...))
// transformed ((lambda (a b c) (body ...)) 10 20 25)
// TODO: actually use the env
pub fn eval_let(list_of_tokens: &[Expr], _env: &EnvRef) -> Result<Expr, RucketErr> {
    check_length("let", &list_of_tokens, 2)?;
    // should have form ((a 10) (b 20) (c 25))
    let bindings = &list_of_tokens[1];
    let body = &list_of_tokens[2];

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

    let mut combined = vec![Expr::ListVal(vec![
        Expr::Atom(Token::Lambda),
        Expr::ListVal(bindings_to_check),
        body.clone(),
    ])];
    combined.append(&mut args_to_check);

    let application = Expr::ListVal(combined);
    Ok(application)
}

pub fn eval_procedure<'a>(
    sym: &Expr,
    eval_iter: impl Iterator<Item = &'a Expr>,
    env: EnvRef,
) -> Result<(RucketVal, EnvRef), RucketErr> {
    match evaluate(sym, &env)? {
        RucketVal::FuncV(func) => {
            let args_eval: Result<Vec<RucketVal>, RucketErr> =
                eval_iter.map(|x| evaluate(&x, &env)).collect();
            let expr = func(&args_eval?)?;
            return Ok((expr, env));
        }

        RucketVal::LambdaV(lambda) => {
            let args_eval: Result<Vec<RucketVal>, RucketErr> =
                eval_iter.map(|x| evaluate(&x, &env)).collect();

            let mut inner_env = Env::new(lambda.env().clone_ref());

            let good_args_eval = args_eval?;

            let params = lambda.params_exp().iter();

            params
                .zip(good_args_eval.iter())
                .for_each(|(param, arg)| inner_env.define(param.to_string(), arg.clone()));

            let env = inner_env.into_ref();
            return Ok((RucketVal::SyntaxV(lambda.body_exp()), env));
        }
        e => {
            return Err(RucketErr::ExpectedFunction(e.to_string()));
        }
    }
}

// TODO write macro to destructure vector
