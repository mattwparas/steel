#![allow(dead_code)] // TODO
#![allow(unused_imports)] // TODO

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::fmt;
use std::iter::{Iterator, Peekable};
use std::rc::Rc;
use std::result;
use std::str::Chars;
use thiserror::Error;

use crate::env::{default_env, Env, EnvRef};
use crate::lexer::Tokenizer;
use crate::parser::{Expr, ParseError, Parser};
use crate::rerrs::RucketErr;
use crate::rvals::{RucketLambda, RucketVal};
use crate::tokens::{Token, TokenError};

pub type Result<T> = result::Result<T, RucketErr>;

pub struct Evaluator {
    global_env: EnvRef,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            global_env: EnvRef::new(default_env()),
        }
    }
    pub fn eval(&mut self, expr: &Expr) -> Result<RucketVal> {
        // global environment updates automatically
        let r = evaluate(&expr, &self.global_env)?;
        Ok(r)
    }
}

// impl<'a> Iterator for Evaluator<'a> {
//     type Item = Result<RucketVal, RucketErr>;

//     // fn next(&mut self) -> Option<Self::Item> {
//     //     self.tokenizer.next().map(|res| match res {
//     //         Err(e) => Err(ParseError::TokenError(e)),
//     //         Ok(tok) => match tok {
//     //             Token::OpenParen => self.read_from_tokens(),
//     //             tok if tok.is_reserved_keyword() => Err(ParseError::Unexpected(tok)),
//     //             tok => Ok(Expr::Atom(tok)),
//     //         },
//     //     })
//     // }
// }

pub fn parse_list_of_identifiers(identifiers: Expr) -> Result<Vec<String>> {
    match identifiers {
        Expr::ListVal(l) => {
            let res: Result<Vec<String>> = l
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
pub fn check_length(what: &str, tokens: &[Expr], expected: usize) -> Result<()> {
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

pub fn evaluate(expr: &Expr, env: &EnvRef) -> Result<RucketVal> {
    let mut env = env.clone_ref();
    let mut expr = expr.clone();
    //println!("evaluating expr: {}", expr);

    loop {
        match expr {
            Expr::Atom(t) => match t {
                Token::BooleanLiteral(b) => {
                    return Ok(RucketVal::BoolV(b));
                }
                Token::Identifier(s) => {
                    return Ok(env.lookup(&s)?);
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
                        // TODO make evaluate an iterator over the results to use global environment
                        Expr::Atom(Token::Define) => match eval_define(&list_of_tokens, env) {
                            Ok(_e) => {
                                return Ok(RucketVal::Void);
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        },
                        Expr::Atom(Token::Quote) => {
                            // TODO make this safer?
                            check_length("Quote", &list_of_tokens, 2)?;
                            let converted = RucketVal::try_from(list_of_tokens[1].clone())?;
                            return Ok(converted);
                        }
                        // (lambda (vars*) (body))
                        Expr::Atom(Token::Lambda) => {
                            // create new environment whos parent is the current environment
                            let new_env = Env::new(env.clone_ref());
                            return Ok(eval_make_lambda(&list_of_tokens, new_env)?);
                        }
                        // (let (var binding)* (body))
                        Expr::Atom(Token::Let) => expr = eval_let(&list_of_tokens, &env)?,
                        // (sym args*), sym must be a procedure
                        sym => match evaluate(sym, &env)? {
                            RucketVal::FuncV(func) => {
                                let args_eval: Result<Vec<RucketVal>> =
                                    eval_iter.map(|x| evaluate(&x, &env)).collect();
                                let args_eval = args_eval?;
                                // pure function doesn't need the env
                                let args_without_env: Vec<&RucketVal> = args_eval.iter().collect();
                                let rval = func(&args_without_env)?;
                                return Ok(rval);
                            }
                            RucketVal::LambdaV(lambda) => {
                                let args_eval: Result<Vec<RucketVal>> =
                                    eval_iter.map(|x| evaluate(&x, &env)).collect();
                                let good_args_eval: Vec<RucketVal> =
                                    args_eval?.iter().map(|x| x.clone()).collect();
                                let inner_env = lambda.env();
                                lambda
                                    .params_exp()
                                    .iter()
                                    .zip(good_args_eval.iter())
                                    .for_each(|(param, arg)| {
                                        inner_env.define(param.to_string(), arg.clone())
                                    });
                                // loop back and continue
                                // using the body as continuation
                                // environment also gets updated
                                env = inner_env.clone_ref();
                                expr = lambda.body_exp();
                            }
                            e => {
                                return Err(RucketErr::ExpectedFunction(e.to_string()));
                            }
                        },
                    }
                } else {
                    return Err(RucketErr::ExpectedFunction("Empty List".to_string()));
                }
            }
        }
    }
}

/// evaluates `(test then else)` into `then` or `else`
pub fn eval_if(list_of_tokens: &[Expr], env: &EnvRef) -> Result<Expr> {
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

// TODO write tests for this
pub fn eval_make_lambda(list_of_tokens: &[Expr], new_env: Env) -> Result<RucketVal> {
    check_length("Lambda", &list_of_tokens, 3)?;
    let list_of_symbols = &list_of_tokens[1];
    let body_exp = &list_of_tokens[2];

    let parsed_list = parse_list_of_identifiers(list_of_symbols.clone())?;
    let constructed_lambda = RucketLambda::new(parsed_list, body_exp.clone(), EnvRef::new(new_env));
    Ok(RucketVal::LambdaV(constructed_lambda))
}

// TODO maybe have to evaluate the params but i'm not sure
pub fn eval_define(list_of_tokens: &[Expr], env: EnvRef) -> Result<EnvRef> {
    check_length("Define", &list_of_tokens, 3)?;
    let symbol = &list_of_tokens[1];
    let body = &list_of_tokens[2];

    match symbol {
        Expr::Atom(Token::Identifier(s)) => {
            let eval_body = evaluate(body, &env)?;
            env.define(s.to_string(), eval_body);
            Ok(env)
        }
        // construct lambda to parse
        Expr::ListVal(list_of_identifiers) => {
            // check_length("Define", tokens: &[Expr], expected: usize)

            if list_of_identifiers.len() == 0 {
                return Err(RucketErr::ExpectedIdentifier(
                    "define expected an identifier, got empty list".to_string(),
                ));
            }
            if let Expr::Atom(Token::Identifier(s)) = &list_of_identifiers[0] {
                let mut fake_lambda: Vec<Expr> = vec![Expr::Atom(Token::Lambda)];

                fake_lambda.push(Expr::ListVal(list_of_identifiers[1..].to_vec()));
                fake_lambda.push(body.clone());

                let constructed_lambda = Expr::ListVal(fake_lambda);

                // let constructed_lambda = eval_make_lambda(&fake_lambda, env)?;

                let eval_body = evaluate(&constructed_lambda, &env)?;
                env.define(s.to_string(), eval_body);
                Ok(env)

            // eval_make_lambda(, env: EnvRef)
            } else {
                Err(RucketErr::ExpectedIdentifier(format!(
                    "Define expects identifier, got: {}",
                    symbol
                )))
            }
        }
        _ => Err(RucketErr::ExpectedIdentifier(format!(
            "Define expects identifier, got: {}",
            symbol
        ))),
    }

    // if let Expr::Atom(Token::Identifier(s)) = symbol {
    //     let (eval_body, _) = evaluate(body, &env)?;
    //     env.define(s.to_string(), eval_body);
    //     Ok(env)
    // } else {
    //     Err(RucketErr::ExpectedIdentifier(format!(
    //         "Define expects identifier, got: {}",
    //         symbol
    //     )))
    // }
}

// Let is actually just a lambda so update values to be that and loop
// Syntax of a let -> (let ((a 10) (b 20) (c 25)) (body ...))
// transformed ((lambda (a b c) (body ...)) 10 20 25)
// TODO: actually use the env
pub fn eval_let(list_of_tokens: &[Expr], _env: &EnvRef) -> Result<Expr> {
    check_length("let", &list_of_tokens, 3)?;
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
                            return Err(RucketErr::ContractViolation(
                                "Let requires pairs for binding".to_string(),
                            ));
                        }
                    },
                    _ => {
                        return Err(RucketErr::BadSyntax("Let: Missing body".to_string()));
                    }
                }
            }
        }
        _ => {
            return Err(RucketErr::BadSyntax(
                "Let: Missing name or binding pairs".to_string(),
            ));
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

// TODO write macro to destructure vector
