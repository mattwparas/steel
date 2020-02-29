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

use crate::env::Env;
use crate::lexer::Tokenizer;
use crate::parser::{Expr, ParseError, Parser};
use crate::rerrs::RucketErr;
use crate::rvals::{RucketLambda, RucketVal};
use crate::stop;
use crate::tokens::{Token, TokenError};

pub type Result<T> = result::Result<T, RucketErr>;

pub struct Evaluator {
    global_env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            global_env: Rc::new(RefCell::new(Env::default_env())),
        }
    }
    pub fn eval(&mut self, expr: &Expr) -> Result<RucketVal> {
        // global environment updates automatically
        evaluate(&expr, &self.global_env)
    }

    // TODO check this
    pub fn parse_and_eval(&mut self, expr_str: &str) -> Result<Vec<RucketVal>> {
        let parsed: result::Result<Vec<Expr>, ParseError> = Parser::new(expr_str).collect();
        match parsed {
            Ok(pvec) => pvec.iter().map(|x| self.eval(&x)).collect(),
            Err(e) => Err(RucketErr::BadSyntax(e.to_string())), // I think we should combine this into one error type?
        }
    }

    pub fn clear_bindings(&mut self) {
        self.global_env.borrow_mut().clear_bindings();
    }
}

impl Drop for Evaluator {
    fn drop(&mut self) {
        self.global_env.borrow_mut().clear_bindings();
    }
}

// impl<'a> Iterator for Evaluator<'a> {
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

pub fn evaluate(expr: &Expr, env: &Rc<RefCell<Env>>) -> Result<RucketVal> {
    let mut env = Rc::clone(env);
    let mut expr = expr.clone();

    loop {
        match expr {
            Expr::Atom(t) => match t {
                Token::BooleanLiteral(b) => {
                    return Ok(RucketVal::BoolV(b));
                }
                Token::Identifier(s) => {
                    return Ok(env.borrow().lookup(&s)?);
                }
                Token::NumberLiteral(n) => {
                    return Ok(RucketVal::NumV(n));
                }
                Token::StringLiteral(s) => {
                    return Ok(RucketVal::StringV(s));
                }
                what => stop!(UnexpectedToken => what),
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
                            return Ok(eval_make_lambda(&list_of_tokens, env)?);
                        }
                        // Evaluate a quoted statement
                        Expr::Atom(Token::Eval) => return eval_eval_expr(&list_of_tokens, &env),
                        // set! expression
                        Expr::Atom(Token::Set) => return eval_set(&list_of_tokens, &env),
                        // (let (var binding)* (body))
                        Expr::Atom(Token::Let) => expr = eval_let(&list_of_tokens, &env)?,
                        Expr::Atom(Token::Begin) => expr = eval_begin(&list_of_tokens, &env)?,
                        // (sym args*), sym must be a procedure
                        sym => match evaluate(sym, &env)? {
                            RucketVal::FuncV(func) => {
                                let args_eval: Result<Vec<RucketVal>> =
                                    eval_iter.map(|x| evaluate(&x, &env)).collect();
                                let args_eval = args_eval?;
                                // pure function doesn't need the env
                                let rval = func(args_eval)?;
                                return Ok(rval);
                            }
                            RucketVal::LambdaV(lambda) => {
                                let args_eval: Result<Vec<RucketVal>> =
                                    eval_iter.map(|x| evaluate(&x, &env)).collect();
                                let args_eval: Vec<RucketVal> = args_eval?;
                                // build a new environment using the parent environment
                                let parent_env = lambda.parent_env();
                                let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
                                let params_exp = lambda.params_exp();
                                inner_env.borrow_mut().define_all(params_exp, args_eval)?;
                                // loop back and continue
                                // using the body as continuation
                                // environment also gets updated
                                env = inner_env;
                                expr = lambda.body_exp();
                            }
                            e => stop!(ExpectedFunction => e),
                        },
                    }
                } else {
                    stop!(ExpectedFunction => "Given empty list")
                }
            }
        }
    }
}
/// TODO: refactor this with iterators
/// evaluates `(test then else)` into `then` or `else`
pub fn eval_if(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<Expr> {
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
pub fn eval_make_lambda(
    list_of_tokens: &[Expr],
    parent_env: Rc<RefCell<Env>>,
) -> Result<RucketVal> {
    check_length("Lambda", &list_of_tokens, 3)?;
    let list_of_symbols = &list_of_tokens[1];
    let body_exp = &list_of_tokens[2];

    let parsed_list = parse_list_of_identifiers(list_of_symbols.clone())?;
    let constructed_lambda = RucketLambda::new(parsed_list, body_exp.clone(), parent_env);
    Ok(RucketVal::LambdaV(constructed_lambda))
}

// Evaluate all but the last, pass the last back up to the loop
pub fn eval_begin(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<Expr> {
    let mut i = 1;
    while i < list_of_tokens.len() - 1 {
        evaluate(&list_of_tokens[i], env)?;
        i += 1;
    }
    if let Some(v) = list_of_tokens.last() {
        return Ok(v.clone());
    } else {
        stop!(ArityMismatch => "begin requires one argument");
    }
}

pub fn eval_set(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<RucketVal> {
    check_length("Set", &list_of_tokens, 3)?;
    let symbol = &list_of_tokens[1];
    let rest_expr = &list_of_tokens[2];
    let value = evaluate(rest_expr, env)?;

    if let Expr::Atom(Token::Identifier(s)) = symbol {
        env.borrow_mut().set(s.clone(), value)
    } else {
        stop!(ExpectedIdentifier => symbol)
    }
}

// TODO write tests
// Evaluate the inner expression, check that it is a quoted expression,
// evaluate body of quoted expression
pub fn eval_eval_expr(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<RucketVal> {
    check_length("Eval", list_of_tokens, 2)?;
    let res_expr = evaluate(&list_of_tokens[1], env)?;
    match Expr::try_from(res_expr) {
        Ok(e) => evaluate(&e, env),
        Err(_) => stop!(ContractViolation => "Eval not given an expression"),
    }
}

// TODO maybe have to evaluate the params but i'm not sure
pub fn eval_define(list_of_tokens: &[Expr], env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Env>>> {
    check_length("Define", &list_of_tokens, 3)?;
    let symbol = &list_of_tokens[1];
    let body = &list_of_tokens[2];

    match symbol {
        Expr::Atom(Token::Identifier(s)) => {
            let eval_body = evaluate(body, &env)?;
            env.borrow_mut().define(s.to_string(), eval_body);
            Ok(env)
        }
        // construct lambda to parse
        Expr::ListVal(list_of_identifiers) => {
            if list_of_identifiers.is_empty() {
                stop!(ExpectedIdentifier => "define expected an identifier, got empty list")
            }
            if let Expr::Atom(Token::Identifier(s)) = &list_of_identifiers[0] {
                // TODO: this code needs go through evaluate again
                // we can cut the middle man and go straight to
                // eval_make_lambda
                let mut fake_lambda: Vec<Expr> = vec![Expr::Atom(Token::Lambda)];

                fake_lambda.push(Expr::ListVal(list_of_identifiers[1..].to_vec()));
                fake_lambda.push(body.clone());

                let constructed_lambda = Expr::ListVal(fake_lambda);

                let eval_body = evaluate(&constructed_lambda, &env)?;
                env.borrow_mut().define(s.to_string(), eval_body);
                Ok(env)
            } else {
                stop!(ExpectedIdentifier => "Define expected identifier, got: {}", symbol);
            }
        }
        _ => stop!(ExpectedIdentifier => "Define expects an identifier, got: {}", symbol),
    }
}

// Let is actually just a lambda so update values to be that and loop
// Syntax of a let -> (let ((a 10) (b 20) (c 25)) (body ...))
// transformed ((lambda (a b c) (body ...)) 10 20 25)
pub fn eval_let(list_of_tokens: &[Expr], _env: &Rc<RefCell<Env>>) -> Result<Expr> {
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
                        _ => stop!(ContractViolation => "Let requires pairs for binding"),
                    },
                    _ => stop!(BadSyntax => "Let: Missing body"),
                }
            }
        }
        _ => stop!(BadSyntax => "Let: Missing name or binding pairs"),
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

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

// TODO write macro to destructure vector

#[cfg(test)]
mod length_test {
    use super::*;
    use crate::parser::Expr::Atom;
    use crate::tokens::Token::NumberLiteral;

    #[test]
    fn length_test() {
        let tokens = vec![Atom(NumberLiteral(1.0)), Atom(NumberLiteral(2.0))];
        assert!(check_length("Test", &tokens, 2).is_ok());
    }

    #[test]
    fn mismatch_test() {
        let tokens = vec![Atom(NumberLiteral(1.0)), Atom(NumberLiteral(2.0))];
        assert!(check_length("Test", &tokens, 1).is_err());
    }
}

#[cfg(test)]
mod parse_identifiers_test {
    use super::*;
    use crate::parser::Expr::{Atom, ListVal};
    use crate::tokens::Token::{Identifier, NumberLiteral};

    #[test]
    fn non_symbols_test() {
        let identifier = ListVal(vec![Atom(NumberLiteral(1.0)), Atom(NumberLiteral(2.0))]);

        let res = parse_list_of_identifiers(identifier);

        assert!(res.is_err());
    }

    #[test]
    fn symbols_test() {
        let identifier = ListVal(vec![
            Atom(Identifier("a".to_string())),
            Atom(Identifier("b".to_string())),
        ]);

        let res = parse_list_of_identifiers(identifier);

        assert_eq!(res.unwrap(), vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn malformed_test() {
        let identifier = Atom(Identifier("a".to_string()));

        let res = parse_list_of_identifiers(identifier);

        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_make_lambda_test {
    use super::*;
    use crate::parser::Expr::{Atom, ListVal};
    use crate::tokens::Token::Identifier;

    #[test]
    fn not_enough_args_test() {
        let list = vec![Atom(Identifier("a".to_string()))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list, default_env);
        assert!(res.is_err());
    }

    #[test]
    fn not_list_val_test() {
        let list = vec![
            Atom(Identifier("a".to_string())),
            Atom(Identifier("b".to_string())),
            Atom(Identifier("c".to_string())),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list, default_env);
        assert!(res.is_err());
    }

    #[test]
    fn ok_test() {
        let list = vec![
            Atom(Identifier("a".to_string())),
            ListVal(vec![Atom(Identifier("b".to_string()))]),
            Atom(Identifier("c".to_string())),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list, default_env);
        assert!(res.is_ok());
    }
}

#[cfg(test)]
mod eval_if_test {
    use super::*;
    use crate::parser::Expr::{Atom, ListVal};
    use crate::tokens::Token::{BooleanLiteral, If, StringLiteral};

    #[test]
    fn true_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        //        let list = vec![Atom(If), ListVal(vec![Atom(StringLiteral(">".to_string())), Atom(StringLiteral("5".to_string())), Atom(StringLiteral("4".to_string()))]), Atom(BooleanLiteral(true)), Atom(BooleanLiteral(false))];
        let list = vec![
            Atom(If),
            Atom(BooleanLiteral(true)),
            Atom(BooleanLiteral(true)),
            Atom(BooleanLiteral(false)),
        ];
        let res = eval_if(&list, &default_env);
        assert_eq!(res.unwrap(), Atom(BooleanLiteral(true)));
    }

    #[test]
    fn false_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let list = vec![
            Atom(If),
            Atom(BooleanLiteral(false)),
            Atom(BooleanLiteral(true)),
            Atom(BooleanLiteral(false)),
        ];
        let res = eval_if(&list, &default_env);
        assert_eq!(res.unwrap(), Atom(BooleanLiteral(false)));
    }

    #[test]
    fn wrong_length_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let list = vec![
            Atom(If),
            Atom(BooleanLiteral(true)),
            Atom(BooleanLiteral(false)),
        ];
        let res = eval_if(&list, &default_env);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_define_test {
    use super::*;
    use crate::parser::Expr::{Atom, ListVal};
    use crate::tokens::Token::{BooleanLiteral, Define, Identifier, StringLiteral};

    #[test]
    fn wrong_length_test() {
        let list = vec![Atom(Identifier("a".to_string()))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list, default_env);
        assert!(res.is_err());
    }

    #[test]
    fn no_identifier_test() {
        let list = vec![Atom(StringLiteral("a".to_string()))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list, default_env);
        assert!(res.is_err());
    }

    #[test]
    fn atom_test() {
        let list = vec![
            Atom(Define),
            Atom(Identifier("a".to_string())),
            Atom(BooleanLiteral(true)),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list, default_env);
        assert!(res.is_ok());
    }

    #[test]
    fn list_val_test() {
        let list = vec![
            Atom(Define),
            ListVal(vec![Atom(Identifier("a".to_string()))]),
            Atom(BooleanLiteral(true)),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list, default_env);
        assert!(res.is_ok());
    }

    #[test]
    fn list_val_no_identifier_test() {
        let list = vec![
            Atom(Define),
            ListVal(vec![Atom(StringLiteral("a".to_string()))]),
            Atom(BooleanLiteral(true)),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list, default_env);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_let_test {
    use super::*;
    use crate::parser::Expr::{Atom, ListVal};
    use crate::tokens::Token::{BooleanLiteral, Let, NumberLiteral, StringLiteral};

    #[test]
    fn ok_test() {
        let list = vec![
            Atom(Let),
            ListVal(vec![ListVal(vec![
                Atom(StringLiteral("a".to_string())),
                Atom(NumberLiteral(10.0)),
            ])]),
            Atom(BooleanLiteral(true)),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_let(&list, &default_env);
        assert!(res.is_ok());
    }

    #[test]
    fn missing_body_test() {
        let list = vec![
            Atom(Let),
            ListVal(vec![ListVal(vec![Atom(NumberLiteral(10.0))])]),
            Atom(BooleanLiteral(true)),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_let(&list, &default_env);
        assert!(res.is_err());
    }

    #[test]
    fn missing_pair_binding_test() {
        let list = vec![Atom(Let), Atom(Let), Atom(BooleanLiteral(true))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_let(&list, &default_env);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_test {
    use super::*;
    use crate::parser::Expr::{Atom, ListVal};
    use crate::tokens::Token::{BooleanLiteral, Identifier, If, NumberLiteral, StringLiteral};

    #[test]
    fn boolean_test() {
        let input = Atom(BooleanLiteral(true));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn identifier_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let input = Atom(Identifier("+".to_string()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn number_test() {
        let input = Atom(NumberLiteral(10.0));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn string_test() {
        let input = Atom(StringLiteral("test".to_string()));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn what_test() {
        let input = Atom(If);
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_err());
    }

    #[test]
    fn list_if_test() {
        let list = vec![
            Atom(If),
            Atom(BooleanLiteral(true)),
            Atom(BooleanLiteral(true)),
            Atom(BooleanLiteral(false)),
        ];
        let input = ListVal(list);
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }
}
