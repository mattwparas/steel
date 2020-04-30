use std::cell::RefCell;
use std::convert::TryFrom;
use std::iter::Iterator;
use std::rc::Rc;
use std::result;

use crate::env::{Env, FALSE, TRUE, VOID};
use crate::parser::tokens::Token;
use crate::parser::{Expr, ParseError, Parser};
use crate::primitives::ListOperations;
use crate::rerrs::SteelErr;
use crate::rvals::{FunctionSignature, Result, SteelLambda, SteelVal};
use crate::stop;
use std::collections::HashMap;
use std::ops::Deref;

// use crate::rvals::MacroPattern;
use crate::rvals::SteelMacro;

pub struct Evaluator {
    global_env: Rc<RefCell<Env>>,
    intern_cache: HashMap<String, Rc<Expr>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            intern_cache: HashMap::new(),
        }
    }
    pub fn eval(&mut self, expr: Expr) -> Result<SteelVal> {
        // global environment updates automatically
        let expr = Rc::new(expr);
        evaluate(&expr, &self.global_env).map(|x| (*x).clone())
    }

    pub fn parse_and_eval(&mut self, expr_str: &str) -> Result<Vec<SteelVal>> {
        // println!(
        //     "{:?}",
        //     self.intern_cache
        //         .iter()
        //         .map(|(x, y)| (x, Rc::strong_count(y)))
        //         .collect::<Vec<(&String, usize)>>()
        // );
        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut self.intern_cache).collect();
        let parsed = parsed?;

        // let test = parsed.clone();

        // let arg_vec = vec![
        //     MacroPattern::Single("a".to_string()),
        //     MacroPattern::Single("b".to_string()),
        // ];

        // let res: Vec<Rc<Expr>> = test
        //     .into_iter()
        //     .map(|x| MacroCase::rename_identifiers(Rc::new(x), &self.global_env, &arg_vec))
        //     .collect();
        // println!("{:?}", res);

        parsed.into_iter().map(|x| self.eval(x)).collect()
    }

    pub fn clear_bindings(&mut self) {
        self.global_env.borrow_mut().clear_bindings();
    }

    pub fn insert_binding(&mut self, name: String, value: SteelVal) {
        self.global_env.borrow_mut().define(name, Rc::new(value));
    }

    pub fn insert_bindings(&mut self, vals: Vec<(String, SteelVal)>) {
        self.global_env
            .borrow_mut()
            .define_zipped(vals.into_iter().map(|x| (x.0, Rc::new(x.1))));
    }

    pub fn lookup_binding(&mut self, name: &str) -> Result<SteelVal> {
        self.global_env
            .borrow_mut()
            .lookup(name)
            .map(|x| (*x).clone())
    }
}

impl Drop for Evaluator {
    fn drop(&mut self) {
        self.global_env.borrow_mut().clear_bindings();
        self.intern_cache.clear();
    }
}

fn parse_list_of_identifiers(identifiers: Rc<Expr>) -> Result<Vec<String>> {
    match identifiers.deref() {
        Expr::VectorVal(l) => {
            let res: Result<Vec<String>> = l
                .iter()
                .map(|x| match &**x {
                    Expr::Atom(Token::Identifier(s)) => Ok(s.clone()),
                    _ => Err(SteelErr::TypeMismatch(
                        "Lambda must have symbols as arguments".to_string(),
                    )),
                })
                .collect();
            res
        }
        _ => Err(SteelErr::TypeMismatch("List of Identifiers".to_string())),
    }
}

/// returns error if tokens.len() != expected
fn check_length(what: &str, tokens: &[Rc<Expr>], expected: usize) -> Result<()> {
    if tokens.len() == expected {
        Ok(())
    } else {
        Err(SteelErr::ArityMismatch(format!(
            "{}: expected {} args got {}",
            what,
            expected,
            tokens.len()
        )))
    }
}

fn evaluate(expr: &Rc<Expr>, env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    let mut env = Rc::clone(env);
    let mut expr = Rc::clone(expr);

    loop {
        match expr.deref() {
            Expr::Atom(t) => return eval_atom(t, &env),

            Expr::VectorVal(list_of_tokens) => {
                if let Some(f) = list_of_tokens.first() {
                    match f.deref() {
                        Expr::Atom(Token::Identifier(s)) if s == "quote" => {
                            check_length("Quote", &list_of_tokens, 2)?;
                            let converted = SteelVal::try_from(list_of_tokens[1].clone())?;
                            return Ok(Rc::new(converted));
                        }
                        Expr::Atom(Token::Identifier(s)) if s == "if" => {
                            expr = eval_if(&list_of_tokens[1..], &env)?
                        }
                        Expr::Atom(Token::Identifier(s)) if s == "define" => {
                            return eval_define(&list_of_tokens[1..], env)
                                .map(|_| VOID.with(|f| Rc::clone(f))); // TODO
                        }
                        Expr::Atom(Token::Identifier(s)) if s == "define-syntax" => {
                            return eval_macro_def(&list_of_tokens[1..], env)
                                .map(|_| VOID.with(|f| Rc::clone(f)));
                        }
                        // (lambda (vars*) (body))
                        Expr::Atom(Token::Identifier(s)) if s == "lambda" || s == "λ" => {
                            return eval_make_lambda(&list_of_tokens[1..], env);
                        }
                        Expr::Atom(Token::Identifier(s)) if s == "eval" => {
                            return eval_eval_expr(&list_of_tokens[1..], &env)
                        }
                        // set! expression
                        Expr::Atom(Token::Identifier(s)) if s == "set!" => {
                            return eval_set(&list_of_tokens[1..], &env)
                        }
                        // (let (var binding)* (body))
                        Expr::Atom(Token::Identifier(s)) if s == "let" => {
                            expr = eval_let(&list_of_tokens[1..], &env)?
                        }
                        Expr::Atom(Token::Identifier(s)) if s == "begin" => {
                            expr = eval_begin(&list_of_tokens[1..], &env)?
                        }
                        // Expr::Atom(Token::Identifier(s)) if s == "and" => {
                        //     return eval_and(&list_of_tokens[1..], &env)
                        // }
                        // Expr::Atom(Token::Identifier(s)) if s == "or" => {
                        //     return eval_or(&list_of_tokens[1..], &env)
                        // }
                        Expr::Atom(Token::Identifier(s)) if s == "map'" => {
                            return eval_map(&list_of_tokens[1..], &env)
                        }
                        Expr::Atom(Token::Identifier(s)) if s == "filter'" => {
                            return eval_filter(&list_of_tokens[1..], &env)
                        }
                        // (sym args*), sym must be a procedure
                        _sym => match evaluate(f, &env)?.as_ref() {
                            SteelVal::FuncV(func) => {
                                return eval_func(*func, &list_of_tokens[1..], &env)
                            }
                            SteelVal::LambdaV(lambda) => {
                                let (new_expr, new_env) =
                                    eval_lambda(lambda, &list_of_tokens[1..], &env)?;
                                expr = new_expr;
                                env = new_env;
                            }
                            SteelVal::MacroV(steel_macro) => {
                                // println!("Found macro definition!");
                                expr = steel_macro.expand(&list_of_tokens)?;
                                // println!("{:?}", expr.clone());
                                // println!()
                            }
                            e => {
                                println!("Getting here");
                                stop!(TypeMismatch => e)
                            }
                        },
                    }
                } else {
                    stop!(TypeMismatch => "Given empty list")
                }
            }
        }
    }
}

fn eval_macro_def(list_of_tokens: &[Rc<Expr>], env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Env>>> {
    let parsed_macro = SteelMacro::parse_from_tokens(list_of_tokens, &env)?;

    // println!("{:?}", parsed_macro);
    env.borrow_mut().define(
        parsed_macro.name().to_string(),
        Rc::new(SteelVal::MacroV(parsed_macro)),
    );
    Ok(env)
}

// evaluates a special form 'filter' for speed up
// TODO fix this noise
fn eval_filter(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    if let [func_expr, list_expr] = list_of_tokens {
        let func_res = evaluate(&func_expr, env)?;
        let list_res = evaluate(&list_expr, env)?;

        match list_res.as_ref() {
            SteelVal::Pair(_, _) => {}
            _ => stop!(TypeMismatch => "map expected a list"),
        }

        let vec_of_vals = ListOperations::collect_into_vec(&list_res)?;
        let mut collected_results = Vec::new();

        for val in vec_of_vals {
            match func_res.as_ref() {
                SteelVal::FuncV(func) => {
                    let result = func(vec![val.clone()])?;
                    if let SteelVal::BoolV(true) = result.as_ref() {
                        collected_results.push(val);
                    }
                }
                SteelVal::LambdaV(lambda) => {
                    // build a new environment using the parent environment
                    let parent_env = lambda.parent_env();
                    let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
                    let params_exp = lambda.params_exp();
                    inner_env
                        .borrow_mut()
                        .define_all(params_exp, vec![val.clone()])?;

                    let result = evaluate(&lambda.body_exp(), &inner_env)?;

                    if let SteelVal::BoolV(true) = result.as_ref() {
                        collected_results.push(val);
                    }
                }
                e => stop!(TypeMismatch => e),
            }
        }

        ListOperations::built_in_list_func()(collected_results)

    // unimplemented!();
    } else {
        let e = format!(
            "{}: expected {} args got {}",
            "map",
            2,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e);
    }
}

/// evaluates a special form 'map' for speed up
fn eval_map(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    if let [func_expr, list_expr] = list_of_tokens {
        let func_res = evaluate(&func_expr, env)?;
        let list_res = evaluate(&list_expr, env)?;

        match list_res.as_ref() {
            SteelVal::Pair(_, _) => {}
            _ => stop!(TypeMismatch => "map expected a list"),
        }

        let vec_of_vals = ListOperations::collect_into_vec(&list_res)?;
        let mut collected_results = Vec::new();

        for val in vec_of_vals {
            match func_res.as_ref() {
                SteelVal::FuncV(func) => {
                    collected_results.push(func(vec![val])?);
                }
                SteelVal::LambdaV(lambda) => {
                    // build a new environment using the parent environment
                    let parent_env = lambda.parent_env();
                    let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
                    let params_exp = lambda.params_exp();
                    inner_env.borrow_mut().define_all(params_exp, vec![val])?;

                    let result = evaluate(&lambda.body_exp(), &inner_env)?;
                    collected_results.push(result);
                }
                e => stop!(TypeMismatch => e),
            }
        }

        ListOperations::built_in_list_func()(collected_results)

    // unimplemented!();
    } else {
        let e = format!(
            "{}: expected {} args got {}",
            "map",
            2,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e);
    }
}

/// evaluates an atom expression in given environment
fn eval_atom(t: &Token, env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    match t {
        Token::BooleanLiteral(b) => {
            if *b {
                Ok(TRUE.with(|f| Rc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Rc::clone(f)))
            }
        }
        Token::Identifier(s) => env.borrow().lookup(&s),
        Token::NumberLiteral(n) => Ok(Rc::new(SteelVal::NumV(*n))),
        Token::StringLiteral(s) => Ok(Rc::new(SteelVal::StringV(s.clone()))),
        Token::CharacterLiteral(c) => Ok(Rc::new(SteelVal::CharV(*c))),
        what => stop!(UnexpectedToken => what),
    }
}
/// evaluates a primitive function into single returnable value
fn eval_func(
    func: FunctionSignature,
    list_of_tokens: &[Rc<Expr>],
    env: &Rc<RefCell<Env>>,
) -> Result<Rc<SteelVal>> {
    let args_eval: Result<Vec<Rc<SteelVal>>> =
        list_of_tokens.iter().map(|x| evaluate(&x, &env)).collect();
    let args_eval = args_eval?;
    // pure function doesn't need the env
    func(args_eval)
}

fn eval_and(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    for expr in list_of_tokens {
        match evaluate(expr, env)?.as_ref() {
            SteelVal::BoolV(true) => continue,
            SteelVal::BoolV(false) => return Ok(FALSE.with(|f| Rc::clone(f))),
            _ => continue,
        }
    }
    Ok(TRUE.with(|f| Rc::clone(f)))
}

fn eval_or(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    for expr in list_of_tokens {
        match evaluate(expr, env)?.as_ref() {
            SteelVal::BoolV(true) => return Ok(TRUE.with(|f| Rc::clone(f))), // Rc::new(SteelVal::BoolV(true))),
            _ => continue,
        }
    }
    Ok(FALSE.with(|f| Rc::clone(f)))
}

/// evaluates a lambda into a body expression to execute
/// and an inner environment
/// TODO - come back to eliminate the cloning that occurs in the symbol -> String process
fn eval_lambda(
    lambda: &SteelLambda,
    list_of_tokens: &[Rc<Expr>],
    env: &Rc<RefCell<Env>>,
) -> Result<(Rc<Expr>, Rc<RefCell<Env>>)> {
    let args_eval: Result<Vec<Rc<SteelVal>>> =
        list_of_tokens.iter().map(|x| evaluate(&x, &env)).collect();
    let args_eval: Vec<Rc<SteelVal>> = args_eval?;
    // build a new environment using the parent environment
    let parent_env = lambda.parent_env();
    let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
    let params_exp = lambda.params_exp();
    inner_env.borrow_mut().define_all(params_exp, args_eval)?;
    // loop back and continue
    // using the body as continuation
    // environment also gets updated
    Ok((lambda.body_exp(), inner_env))
}
/// evaluates `(test then else)` into `then` or `else`
fn eval_if(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<Expr>> {
    if let [test_expr, then_expr, else_expr] = list_of_tokens {
        match evaluate(&test_expr, env)?.as_ref() {
            SteelVal::BoolV(true) => Ok(Rc::clone(then_expr)),
            _ => Ok(Rc::clone(else_expr)),
        }
    } else {
        let e = format!("{}: expected {} args got {}", "If", 3, list_of_tokens.len());
        stop!(ArityMismatch => e);
    }
}

fn eval_make_lambda(
    list_of_tokens: &[Rc<Expr>],
    parent_env: Rc<RefCell<Env>>,
) -> Result<Rc<SteelVal>> {
    if list_of_tokens.len() > 1 {
        let list_of_symbols = &list_of_tokens[0];
        let mut body_exps = list_of_tokens[1..].to_vec();
        let mut begin_body: Vec<Rc<Expr>> =
            vec![Rc::new(Expr::Atom(Token::Identifier("begin".to_string())))];
        begin_body.append(&mut body_exps);

        let parsed_list = parse_list_of_identifiers(Rc::clone(list_of_symbols))?;
        let constructed_lambda = SteelLambda::new(
            parsed_list,
            Rc::new(Expr::VectorVal(begin_body)),
            parent_env,
        );
        Ok(Rc::new(SteelVal::LambdaV(constructed_lambda)))
    } else {
        let e = format!(
            "{}: expected at least {} args got {}",
            "Lambda",
            1,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e)
    }
}

// Evaluate all but the last, pass the last back up to the loop
fn eval_begin(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<Expr>> {
    let mut tokens_iter = list_of_tokens.iter();
    let last_token = tokens_iter.next_back();
    // throw away intermediate evaluations
    for token in tokens_iter {
        evaluate(token, env)?;
    }
    if let Some(v) = last_token {
        Ok(Rc::clone(v))
    } else {
        stop!(ArityMismatch => "begin requires at least one argument");
    }
}

fn eval_set(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    if let [symbol, rest_expr] = list_of_tokens {
        let value = evaluate(rest_expr, env)?;

        if let Expr::Atom(Token::Identifier(s)) = &**symbol {
            env.borrow_mut().set(s.clone(), value)
        } else {
            stop!(TypeMismatch => symbol)
        }
    } else {
        let e = format!(
            "{}: expected {} args got {}",
            "Set",
            2,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e)
    }
}

// TODO write tests
// Evaluate the inner expression, check that it is a quoted expression,
// evaluate body of quoted expression
fn eval_eval_expr(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    if let [e] = list_of_tokens {
        let res_expr = evaluate(e, env)?;
        match <Rc<Expr>>::try_from(&(*res_expr).clone()) {
            Ok(e) => evaluate(&e, env),
            Err(_) => stop!(ContractViolation => "Eval not given an expression"),
        }
    } else {
        let e = format!(
            "{}: expected {} args got {}",
            "Eval",
            1,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e)
    }
}

// TODO maybe have to evaluate the params but i'm not sure
fn eval_define(list_of_tokens: &[Rc<Expr>], env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Env>>> {
    if list_of_tokens.len() > 1 {
        match (
            list_of_tokens.get(0).as_ref(),
            list_of_tokens.get(1).as_ref(),
        ) {
            (Some(symbol), Some(body)) => {
                match symbol.deref().deref() {
                    Expr::Atom(Token::Identifier(s)) => {
                        if list_of_tokens.len() != 2 {
                            let e = format!(
                                "{}: multiple expressions after the identifier, expected {} args got {}",
                                "Define",
                                2,
                                list_of_tokens.len()
                            );
                            stop!(ArityMismatch => e)
                        }
                        let eval_body = evaluate(body, &env)?;
                        env.borrow_mut().define(s.to_string(), eval_body);
                        Ok(env)
                    }
                    // construct lambda to parse
                    Expr::VectorVal(list_of_identifiers) => {
                        if list_of_identifiers.is_empty() {
                            stop!(TypeMismatch => "define expected an identifier, got empty list")
                        }
                        if let Expr::Atom(Token::Identifier(s)) = &*list_of_identifiers[0] {
                            let mut lst = list_of_tokens[1..].to_vec();
                            let mut begin_body: Vec<Rc<Expr>> =
                                vec![Rc::new(Expr::Atom(Token::Identifier("begin".to_string())))];
                            begin_body.append(&mut lst);

                            // eval_make_lambda(&list_of_tokens[1..], env);

                            // eval_make_lambda
                            let fake_lambda: Vec<Rc<Expr>> = vec![
                                Rc::new(Expr::Atom(Token::Identifier("lambda".to_string()))),
                                Rc::new(Expr::VectorVal(list_of_identifiers[1..].to_vec())),
                                Rc::new(Expr::VectorVal(begin_body)),
                            ];
                            let constructed_lambda = Rc::new(Expr::VectorVal(fake_lambda));
                            let eval_body = evaluate(&constructed_lambda, &env)?;
                            env.borrow_mut().define(s.to_string(), eval_body);
                            Ok(env)
                        } else {
                            stop!(TypeMismatch => "Define expected identifier, got: {}", symbol);
                        }
                    }
                    _ => stop!(TypeMismatch => "Define expects an identifier, got: {}", symbol),
                }
            }
            _ => {
                let e = format!(
                    "{}: expected at least {} args got {}",
                    "Define",
                    2,
                    list_of_tokens.len()
                );
                stop!(ArityMismatch => e)
            }
        }
    } else {
        let e = format!(
            "{}: expected at least {} args got {}",
            "Define",
            2,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e)
    }
}

// Let is actually just a lambda so update values to be that and loop
// Syntax of a let -> (let ((a 10) (b 20) (c 25)) (body ...))
// transformed ((lambda (a b c) (body ...)) 10 20 25)
fn eval_let(list_of_tokens: &[Rc<Expr>], _env: &Rc<RefCell<Env>>) -> Result<Rc<Expr>> {
    if let [bindings, body] = list_of_tokens {
        let mut bindings_to_check: Vec<Rc<Expr>> = Vec::new();
        let mut args_to_check: Vec<Rc<Expr>> = Vec::new();

        // TODO fix this noise
        match bindings.deref() {
            Expr::VectorVal(list_of_pairs) => {
                for pair in list_of_pairs {
                    match pair.deref() {
                        Expr::VectorVal(p) => match p.as_slice() {
                            [binding, expression] => {
                                bindings_to_check.push(binding.clone());
                                args_to_check.push(expression.clone());
                            }
                            _ => stop!(BadSyntax => "Let requires pairs for binding"),
                        },
                        _ => stop!(BadSyntax => "Let: Missing body"),
                    }
                }
            }
            _ => stop!(BadSyntax => "Let: Missing name or binding pairs"),
        }

        let mut combined = vec![Rc::new(Expr::VectorVal(vec![
            Rc::new(Expr::Atom(Token::Identifier("lambda".to_string()))),
            Rc::new(Expr::VectorVal(bindings_to_check)),
            Rc::clone(body),
        ]))];
        combined.append(&mut args_to_check);

        let application = Expr::VectorVal(combined);
        Ok(Rc::new(application))
    } else {
        let e = format!(
            "{}: expected {} args got {}",
            "Let",
            2,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e)
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod length_test {
    use super::*;
    use crate::parser::tokens::Token::NumberLiteral;
    use crate::parser::Expr::Atom;

    #[test]
    fn length_test() {
        let tokens = vec![
            Rc::new(Atom(NumberLiteral(1.0))),
            Rc::new(Atom(NumberLiteral(2.0))),
        ];
        assert!(check_length("Test", &tokens, 2).is_ok());
    }

    #[test]
    fn mismatch_test() {
        let tokens = vec![
            Rc::new(Atom(NumberLiteral(1.0))),
            Rc::new(Atom(NumberLiteral(2.0))),
        ];
        assert!(check_length("Test", &tokens, 1).is_err());
    }
}

#[cfg(test)]
mod parse_identifiers_test {
    use super::*;
    use crate::parser::tokens::Token::{Identifier, NumberLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn non_symbols_test() {
        let identifier = Rc::new(VectorVal(vec![
            Rc::new(Atom(NumberLiteral(1.0))),
            Rc::new(Atom(NumberLiteral(2.0))),
        ]));

        let res = parse_list_of_identifiers(identifier);

        assert!(res.is_err());
    }

    #[test]
    fn symbols_test() {
        let identifier = Rc::new(VectorVal(vec![
            Rc::new(Atom(Identifier("a".to_string()))),
            Rc::new(Atom(Identifier("b".to_string()))),
        ]));

        let res = parse_list_of_identifiers(identifier);

        assert_eq!(res.unwrap(), vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn malformed_test() {
        let identifier = Rc::new(Atom(Identifier("a".to_string())));

        let res = parse_list_of_identifiers(identifier);

        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_make_lambda_test {
    use super::*;
    use crate::parser::tokens::Token::Identifier;
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn not_enough_args_test() {
        let list = vec![Rc::new(Atom(Identifier("a".to_string())))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list, default_env);
        assert!(res.is_err());
    }

    #[test]
    fn not_list_val_test() {
        let list = vec![
            Rc::new(Atom(Identifier("a".to_string()))),
            Rc::new(Atom(Identifier("b".to_string()))),
            Rc::new(Atom(Identifier("c".to_string()))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list[1..], default_env);
        assert!(res.is_err());
    }

    #[test]
    fn ok_test() {
        let list = vec![
            Rc::new(Atom(Identifier("a".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(Atom(Identifier("b".to_string())))])),
            Rc::new(Atom(Identifier("c".to_string()))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list[1..], default_env);
        assert!(res.is_ok());
    }
}

#[cfg(test)]
mod eval_if_test {
    use super::*;
    use crate::parser::tokens::Token::BooleanLiteral;
    use crate::parser::Expr::Atom;

    #[test]
    fn true_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        //        let list = vec![Atom(If), VectorVal(vec![Atom(StringLiteral(">".to_string())), Atom(StringLiteral("5".to_string())), Atom(StringLiteral("4".to_string()))]), Atom(BooleanLiteral(true)), Atom(BooleanLiteral(false))];
        let list = vec![
            Rc::new(Atom(Token::Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let res = eval_if(&list[1..], &default_env);
        assert_eq!(res.unwrap(), Rc::new(Atom(BooleanLiteral(true))));
    }

    #[test]
    fn false_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let list = vec![
            Rc::new(Atom(Token::Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(false))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let res = eval_if(&list[1..], &default_env);
        assert_eq!(res.unwrap(), Rc::new(Atom(BooleanLiteral(false))));
    }

    #[test]
    fn wrong_length_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let list = vec![
            Rc::new(Atom(Token::Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let res = eval_if(&list[1..], &default_env);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_define_test {
    use super::*;
    use crate::parser::tokens::Token::{BooleanLiteral, Identifier, StringLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn wrong_length_test() {
        let list = vec![Rc::new(Atom(Identifier("a".to_string())))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], default_env);
        assert!(res.is_err());
    }

    #[test]
    fn no_identifier_test() {
        let list = vec![Rc::new(Atom(StringLiteral("a".to_string())))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], default_env);
        assert!(res.is_err());
    }

    #[test]
    fn atom_test() {
        let list = vec![
            Rc::new(Atom(Identifier("define".to_string()))),
            Rc::new(Atom(Identifier("a".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], default_env);
        assert!(res.is_ok());
    }

    #[test]
    fn list_val_test() {
        let list = vec![
            Rc::new(Atom(Identifier("define".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(Atom(Identifier("a".to_string())))])),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], default_env);
        assert!(res.is_ok());
    }

    #[test]
    fn list_val_no_identifier_test() {
        let list = vec![
            Rc::new(Atom(Identifier("define".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(Atom(StringLiteral(
                "a".to_string(),
            )))])),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], default_env);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_let_test {
    use super::*;
    use crate::parser::tokens::Token::{BooleanLiteral, NumberLiteral, StringLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn ok_test() {
        let list = vec![
            Rc::new(Atom(Token::Identifier("let".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(VectorVal(vec![
                Rc::new(Atom(StringLiteral("a".to_string()))),
                Rc::new(Atom(NumberLiteral(10.0))),
            ]))])),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_let(&list[1..], &default_env);
        assert!(res.is_ok());
    }

    #[test]
    fn missing_body_test() {
        let list = vec![
            Rc::new(Atom(Token::Identifier("let".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(VectorVal(vec![Rc::new(Atom(
                NumberLiteral(10.0),
            ))]))])),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_let(&list[1..], &default_env);
        assert!(res.is_err());
    }

    #[test]
    fn missing_pair_binding_test() {
        let list = vec![
            Rc::new(Atom(Token::Identifier("let".to_string()))),
            Rc::new(Atom(Token::Identifier("let".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_let(&list[1..], &default_env);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_test {
    use super::*;
    use crate::parser::tokens::Token::{BooleanLiteral, Identifier, NumberLiteral, StringLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn boolean_test() {
        let input = Rc::new(Atom(BooleanLiteral(true)));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn identifier_test() {
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let input = Rc::new(Atom(Identifier("+".to_string())));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn number_test() {
        let input = Rc::new(Atom(NumberLiteral(10.0)));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn string_test() {
        let input = Rc::new(Atom(StringLiteral("test".to_string())));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }

    #[test]
    fn what_test() {
        let input = Rc::new(Atom(Identifier("if".to_string())));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_err());
    }

    #[test]
    fn list_if_test() {
        let list = vec![
            Rc::new(Atom(Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let input = Rc::new(VectorVal(list));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env).is_ok());
    }
}
