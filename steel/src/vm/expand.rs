use crate::parser::tokens::TokenType;
use crate::parser::Expr;
use crate::parser::SyntaxObject;
// use crate::primitives::ListOperations;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};

use std::ops::Deref;

use crate::env::Env;
use std::cell::RefCell;
use std::rc::Rc;

use crate::expander::SteelMacro;
use crate::structs::SteelStruct;

use crate::env::MacroEnv;
use crate::vm::SymbolMap;

use std::collections::HashSet;

use crate::gc::Gc;

pub struct MacroSet(HashSet<String>);

impl MacroEnv for MacroSet {
    fn validate_identifier(&self, name: &str) -> bool {
        self.0.contains(name)
    }
}

impl MacroSet {
    pub fn new() -> MacroSet {
        MacroSet(HashSet::new())
    }

    pub fn insert(&mut self, name: String) {
        self.0.insert(name);
    }

    pub fn insert_from_iter(&mut self, vals: impl Iterator<Item = String>) {
        for val in vals {
            self.insert(val);
        }
    }
}

pub fn get_definition_names(exprs: &[Expr]) -> Vec<String> {
    exprs.iter().filter_map(get_definition_name).collect()
}

fn get_definition_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Atom(_) => None,
        Expr::VectorVal(list_of_tokens) => match (list_of_tokens.first(), list_of_tokens.get(1)) {
            (
                Some(Expr::Atom(SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                })),
                Some(Expr::Atom(SyntaxObject {
                    ty: TokenType::Identifier(n),
                    ..
                })),
            ) if s == "define-syntax" || s == "define" => Some(n.to_string()),
            (
                Some(Expr::Atom(SyntaxObject {
                    ty: TokenType::Identifier(s),
                    ..
                })),
                Some(Expr::VectorVal(l)),
            ) if s == "define" => {
                if let Some(Expr::Atom(SyntaxObject {
                    ty: TokenType::Identifier(n),
                    ..
                })) = l.get(0)
                {
                    Some(n.to_string())
                } else {
                    None
                }
            }
            _ => None,
        },
    }
}

fn is_macro_definition(expr: &Expr) -> bool {
    // let expr = Rc::clone(expr);
    match expr {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        if s == "define-syntax" {
                            // println!("Found macro definition!");
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

// pub fn is_definition(expr: &Expr) -> bool {
//     match expr {
//         Expr::Atom(_) => return false,
//         Expr::VectorVal(list_of_tokens) => {
//             if let Some(f) = list_of_tokens.first() {
//                 if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
//                     if let TokenType::Identifier(s) = t {
//                         if s == "define" || s == "defn" {
//                             return true;
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     false
// }

fn is_struct_definition(expr: &Expr) -> bool {
    // let expr = Rc::clone(expr);
    match expr {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        if s == "struct" {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

fn construct_macro_def<M: MacroEnv>(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    macro_set: &M,
) -> Result<()> {
    let parsed_macro = SteelMacro::parse_from_tokens(list_of_tokens, macro_set)?;
    // println!("{:?}", parsed_macro);
    env.borrow_mut().define(
        parsed_macro.name().to_string(),
        Gc::new(SteelVal::MacroV(parsed_macro)),
    );
    // env.borrow().print_bindings();
    Ok(())
}

pub fn extract_macro_definitions<M: MacroEnv>(
    exprs: Vec<Expr>,
    macro_env: &Rc<RefCell<Env>>,
    struct_env: &Rc<RefCell<Env>>,
    sm: &mut SymbolMap,
    macro_set: &M,
) -> Result<Vec<Expr>> {
    let mut others: Vec<Expr> = Vec::new();
    for expr in exprs {
        match expr {
            Expr::VectorVal(list_of_tokens) if is_macro_definition(&expr) => {
                // println!("Constructing a macro definition");
                construct_macro_def(&list_of_tokens[1..], macro_env, macro_set)?;
            }
            Expr::VectorVal(list_of_tokens) if is_struct_definition(&expr) => {
                let defs = SteelStruct::generate_from_tokens(&list_of_tokens[1..])?;
                struct_env
                    .borrow_mut()
                    .define_zipped_rooted(sm, defs.into_iter());

                let defs = SteelStruct::generate_from_tokens(&list_of_tokens[1..])?;
                struct_env
                    .borrow_mut()
                    .repl_define_zipped_rooted(sm, defs.into_iter());

                // env.borrow_mut()
                //     .define_zipped(defs.into_iter().map(|x| (x.0, Rc::new(x.1))));
            }
            _ => others.push(expr),
        }
    }
    Ok(others)
}

pub fn expand_statements(
    exprs: Vec<Expr>,
    env: &Rc<RefCell<Env>>,
    macro_env: &Rc<RefCell<Env>>,
) -> Result<Vec<Expr>> {
    exprs
        .into_iter()
        .map(|x| expand(x, env, macro_env))
        .collect()
}

// fn extract_globals_and_macro_names()

// Let is actually just a lambda so update values to be that and loop
// Syntax of a let -> (let ((a 10) (b 20) (c 25)) (body ...))
// transformed ((lambda (a b c) (body ...)) 10 20 25)
// TODO fix this cloning issue
fn expand_let(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    macro_env: &Rc<RefCell<Env>>,
) -> Result<Expr> {
    if let [bindings, body] = list_of_tokens {
        let mut bindings_to_check: Vec<Expr> = Vec::new();
        let mut args_to_check: Vec<Expr> = Vec::new();

        // TODO fix this noise
        match bindings.deref() {
            Expr::VectorVal(list_of_pairs) => {
                for pair in list_of_pairs {
                    match pair {
                        Expr::VectorVal(p) => match p.as_slice() {
                            [binding, expression] => {
                                bindings_to_check.push(binding.clone());
                                args_to_check.push(expand(expression.clone(), env, macro_env)?);
                            }
                            _ => stop!(BadSyntax => "Let requires pairs for binding"),
                        },
                        _ => stop!(BadSyntax => "Let: Missing body"),
                    }
                }
            }
            _ => stop!(BadSyntax => "Let: Missing name or binding pairs"),
        }

        // Change the body to contain more than one expression
        let expanded_body = expand(body.clone(), env, macro_env)?;

        let mut combined = vec![Expr::VectorVal(vec![
            Expr::Atom(SyntaxObject::default(TokenType::Identifier(
                "lambda".to_string(),
            ))),
            Expr::VectorVal(bindings_to_check),
            expanded_body, // body.clone(), // TODO expand body here
        ])];
        combined.append(&mut args_to_check);

        let application = Expr::VectorVal(combined);
        Ok(application)
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

// fn expand_define(list_of_tokens: Vec<Expr>) -> Result<Expr> {
//     unimplemented!();
// }

// TODO maybe have to evaluate the params but i'm not sure
fn expand_define(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    macro_env: &Rc<RefCell<Env>>,
) -> Result<Expr> {
    // env.borrow_mut().set_binding_context(true);

    if list_of_tokens.len() > 2 {
        match (list_of_tokens.get(1), list_of_tokens.get(2)) {
            (Some(symbol), Some(body)) => {
                match symbol.deref() {
                    Expr::Atom(SyntaxObject { ty: t, .. }) => {
                        if list_of_tokens.len() != 3 {
                            stop!(ArityMismatch => "define statement expects an identifier and an expression")
                        }
                        if let TokenType::Identifier(_) = t {
                            let expanded_body = expand(body.clone(), env, macro_env)?;
                            let return_val = Expr::VectorVal(vec![
                                list_of_tokens[0].clone(),
                                symbol.clone(),
                                expanded_body,
                            ]);

                            Ok(return_val)

                        // return Ok(Expr::VectorVal(list_of_tokens.to_vec()));
                        } else {
                            stop!(TypeMismatch => "Define expected identifier, got: {}", symbol);
                        }
                    }
                    // construct lambda to parse
                    Expr::VectorVal(list_of_identifiers) => {
                        if list_of_identifiers.is_empty() {
                            stop!(TypeMismatch => "define expected an identifier, got empty list")
                        }
                        if let Expr::Atom(SyntaxObject { ty: t, .. }) = &list_of_identifiers[0] {
                            if let TokenType::Identifier(_s) = t {
                                let mut begin_body = list_of_tokens[2..].to_vec();
                                let mut fake_lambda: Vec<Expr> = vec![
                                    Expr::Atom(SyntaxObject::default(TokenType::Identifier(
                                        "lambda".to_string(),
                                    ))),
                                    Expr::VectorVal(list_of_identifiers[1..].to_vec()),
                                ];
                                fake_lambda.append(&mut begin_body);
                                let constructed_lambda = Expr::VectorVal(fake_lambda);
                                let expanded_body = expand(constructed_lambda, env, macro_env)?;

                                let return_val = Expr::VectorVal(vec![
                                    list_of_tokens[0].clone(),
                                    list_of_identifiers[0].clone(),
                                    expanded_body,
                                ]);

                                Ok(return_val)
                            } else {
                                stop!(TypeMismatch => "Define expected identifier, got: {}", symbol);
                            }
                        } else {
                            stop!(TypeMismatch => "Define expected identifier, got: {}", symbol);
                        }
                    } // _ => stop!(TypeMismatch => "Define expects an identifier, got: {}", symbol),
                }
            }
            _ => {
                let e = format!(
                    "{}: expected at least {} args got {}",
                    "Define",
                    2,
                    &list_of_tokens[1..].len()
                );
                stop!(ArityMismatch => e)
            }
        }
    } else {
        let e = format!(
            "{}: expected at least {} args got {}",
            "Define",
            2,
            &list_of_tokens[1..].len()
        );
        stop!(ArityMismatch => e)
    }
}

// TODO include the intern cache when possible
pub fn expand(expr: Expr, env: &Rc<RefCell<Env>>, macro_env: &Rc<RefCell<Env>>) -> Result<Expr> {
    // let env = Rc::clone(env);
    // let expr = Rc::clone(expr);

    match &expr {
        Expr::Atom(_) => Ok(expr),
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        match s.as_str() {
                            "define" | "defn" => {
                                return expand_define(list_of_tokens, env, macro_env)
                            }
                            "let" => {
                                // not exactly sure why this is happening but wrapping this in the expand works well
                                // return expand(
                                return expand_let(&list_of_tokens[1..], env, macro_env);
                                // env,
                                // macro_env,
                                // );
                            }
                            _ => {
                                // println!("Looking up {}", s);
                                // macro_env.borrow().print_bindings();
                                let lookup = macro_env.borrow().lookup(&s);

                                if let Ok(v) = lookup {
                                    if let SteelVal::MacroV(steel_macro) = v.as_ref() {
                                        let expanded = steel_macro.expand(&list_of_tokens)?;
                                        return expand(expanded, env, macro_env);
                                        // return steel_macro.expand(&list_of_tokens)?;
                                    }
                                }
                            }
                        }
                    }
                }
                let result: Result<Vec<Expr>> = list_of_tokens
                    .iter()
                    .map(|x| expand(x.clone(), env, macro_env))
                    .collect();
                Ok(Expr::VectorVal(result?))
            } else {
                Ok(expr)
            }
        }
    }
}
