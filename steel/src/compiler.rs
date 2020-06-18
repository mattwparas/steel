/*
First, consume the entire syntax tree and create global definitions
*/

// use std::fs::File;
// use std::io::prelude::*;

use crate::env::Env;
use crate::parser::tokens::Token;
use crate::parser::Expr;
// use crate::rerrs::SteelErr;
use std::cell::RefCell;
// use std::collections::HashMap;
use std::rc::Rc;

use crate::expander::SteelMacro;
use crate::rvals::Result;
use crate::rvals::SteelVal;
use crate::structs::SteelStruct;

use std::ops::Deref;

use crate::interpreter::evaluator::eval_define;

pub struct AST {
    expr: Vec<Rc<Expr>>,
    env: Rc<RefCell<Env>>,
}

impl AST {
    pub fn new(expr: Vec<Rc<Expr>>, env: Rc<RefCell<Env>>) -> Self {
        AST { expr, env }
    }

    pub fn get_expr(&self) -> &[Rc<Expr>] {
        &self.expr
    }

    pub fn get_env(&self) -> &Rc<RefCell<Env>> {
        &self.env
    }

    pub fn compile(exprs: Vec<Expr>, env: Rc<RefCell<Env>>) -> Result<Self> {
        let mut heap = Vec::new();
        let exprs: Vec<Rc<Expr>> = exprs.into_iter().map(Rc::new).collect();
        let macros_extracted = extract_macro_definitions(&exprs, &env)?;
        let functions_extracted =
            extract_and_expand_function_definitions(&macros_extracted, &env, &mut heap)?;

        Ok(AST::new(functions_extracted, env))
    }
}

// fn expression_is_constant(expr: Rc<Expr>) -> bool {
//     match expr.as_ref() {
//         Expr::Atom(t)
//     }
// }

/*
identify flat functions
procedure cloning / versioning

(define test-function (lambda (a) (+ a 5)))

constant folding

how to check if value is a constant:


*/

// fn function_call_is_closure(expr: Rc<Expr>, env: &Rc<RefCell<Env>>) -> bool {
//     match expr.as_ref() {
//         Expr::Atom(_) => false,
//         Expr::VectorVal(list_of_tokens) => {
//             if let Some(function_name) = list_of_tokens.get(0) {

//             } else {
//                 false
//             }
//         }
//     }
// }

fn extract_macro_definitions(exprs: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<Vec<Rc<Expr>>> {
    let mut others: Vec<Rc<Expr>> = Vec::new();
    for expr in exprs {
        match expr.as_ref() {
            Expr::VectorVal(list_of_tokens) if is_macro_definition(expr) => {
                construct_macro_def(&list_of_tokens[1..], env)?;
            }
            _ => others.push(Rc::clone(expr)),
        }
    }
    Ok(others)
}

fn extract_and_expand_function_definitions(
    exprs: &[Rc<Expr>],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
) -> Result<Vec<Rc<Expr>>> {
    let mut others: Vec<Rc<Expr>> = Vec::new();
    for expr in exprs {
        match expr.as_ref() {
            Expr::VectorVal(list_of_tokens) if is_function_definition(expr) => {
                eval_define(&list_of_tokens[1..], env, heap)?;
            }
            Expr::VectorVal(list_of_tokens) if is_struct_definition(expr) => {
                let defs = SteelStruct::generate_from_tokens(&list_of_tokens[1..])?;
                env.borrow_mut()
                    .define_zipped(defs.into_iter().map(|x| (x.0, Rc::new(x.1))));
            }
            _ => others.push(Rc::clone(expr)),
        }
    }
    Ok(others)
}

pub fn construct_macro_def(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<()> {
    let parsed_macro = SteelMacro::parse_from_tokens(list_of_tokens, &env)?;
    // println!("{:?}", parsed_macro);
    env.borrow_mut().define(
        parsed_macro.name().to_string(),
        Rc::new(SteelVal::MacroV(parsed_macro)),
    );
    Ok(())
}

// fn construct_define(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<()> {
//     unimplemented!()
// }

fn is_macro_definition(expr: &Rc<Expr>) -> bool {
    let expr = Rc::clone(expr);
    match expr.deref() {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(Token::Identifier(s)) = f.as_ref() {
                    if s == "define-syntax" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn is_struct_definition(expr: &Rc<Expr>) -> bool {
    let expr = Rc::clone(expr);
    match expr.deref() {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(Token::Identifier(s)) = f.as_ref() {
                    if s == "struct" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

// TODO include the intern cache when possible
fn is_function_definition(expr: &Rc<Expr>) -> bool {
    let expr = Rc::clone(expr);
    match expr.deref() {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(Token::Identifier(s)) = f.as_ref() {
                    if s == "define" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

// fn write_to_file() -> std::io::Result<()> {
//     let mut file = File::create("test.txt")?;
//     file.write_all(b"Hello, world!")?;
//     Ok(())
// }
