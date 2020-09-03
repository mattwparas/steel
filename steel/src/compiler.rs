/*
First, consume the entire syntax tree and create global definitions
*/

// use std::fs::File;
// use std::io::prelude::*;

use crate::env::Env;
// use crate::parser::tokens::Token;
use crate::parser::tokens::TokenType;
use crate::parser::Expr;
use crate::parser::SyntaxObject;
use crate::rerrs::SteelErr;
use std::cell::RefCell;
// use std::collections::HashMap;
use std::rc::Rc;

use crate::expander::SteelMacro;
use crate::rvals::Result;
use crate::rvals::SteelVal;
use crate::structs::SteelStruct;

use std::collections::HashMap;
use std::collections::HashSet;
// use std::ops::Deref;

use crate::stop;

use crate::interpreter::evaluator::eval_require;
use crate::interpreter::evaluator::Evaluator;
use std::io::Read;

use crate::gc::Gc;

#[derive(Debug)]
pub struct AST {
    source: String,
    expr: Vec<Expr>,
    env: Rc<RefCell<Env>>,
    exported: HashSet<String>,
}

impl AST {
    pub fn new(
        source: String,
        expr: Vec<Expr>,
        env: Rc<RefCell<Env>>,
        exported: HashSet<String>,
    ) -> Self {
        AST {
            source,
            expr,
            env,
            exported,
        }
    }

    // pub fn parse_and_compile_with_env(
    //     &mut self,
    //     expr_str: &str,
    //     env: Rc<RefCell<Env>>,
    // ) -> Result<AST> {
    //     let parsed: result::Result<Vec<Expr>, ParseError> =
    //         Parser::new(expr_str, &mut self.intern_cache).collect();
    //     let parsed = parsed?;
    //     AST::compile(parsed, env)
    // }

    pub fn compile_module<'a>(
        path: &str,
        intern: &'a mut HashMap<String, Rc<TokenType>>,
    ) -> Result<Self> {
        let mut file = std::fs::File::open(path)?;
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;
        // Env::default_env();
        // let env = Env::default_env();
        //

        let env = Evaluator::generate_default_env_with_prelude()?;

        // let parsed: result:Result<Vec<Expr>, ParseError> = Parser::new(expr)
        Evaluator::parse_and_compile_with_env_and_intern(&exprs, env, intern)
        // unimplemented!()
    }

    pub fn get_exported(&self) -> &HashSet<String> {
        &self.exported
    }

    pub fn get_expr(&self) -> &[Expr] {
        &self.expr
    }

    pub fn get_env(&self) -> &Rc<RefCell<Env>> {
        &self.env
    }

    pub fn compile<'a>(
        source: String,
        exprs: Vec<Expr>,
        env: Rc<RefCell<Env>>,
        // intern: &'a mut HashMap<String, Rc<Expr>>,
    ) -> Result<Self> {
        let mut heap = Vec::new();
        let mut expr_stack: Vec<Expr> = Vec::new();
        // let mut last_macro: Option<Rc<Expr>> = None;

        // let exprs: Vec<Expr> = exprs.into_iter().map(Rc::new).collect();

        // Do require step here...
        let exprs = extract_and_compile_requires(&exprs, &env)?;

        identify_function_definitions(&exprs, &env, &mut heap, &mut expr_stack)?;

        // TODO extract functions once, extract macros, extract functions, expand functions in env not in place
        let macros_extracted = extract_macro_definitions(&exprs, &env)?;
        let functions_extracted = extract_and_expand_function_definitions(
            &macros_extracted,
            &env,
            &mut heap,
            &mut expr_stack,
            // &mut last_macro,
        )?;

        let exported_functions = extract_exported_identifiers(&functions_extracted)?;

        Ok(AST::new(
            source,
            functions_extracted,
            env,
            exported_functions,
        ))
    }

    // pub fn compile_from_require()

    pub fn lookup(&self, name: &str) -> Result<Gc<SteelVal>> {
        if self.exported.contains(name) {
            self.env.borrow().lookup(name)
        } else {
            stop!(FreeIdentifier => "name not found in module")
        }
    }

    pub fn lookup_idx(&self, _idx: usize) -> Result<Rc<SteelVal>> {
        unimplemented!()
        // if self.exported.contains(name) {
        //     self.env.borrow().lookup_idx(name)
        // } else {
        //     stop!(FreeIdentifier => "name not found in module")
        // }
    }
}

impl Drop for AST {
    fn drop(&mut self) {
        self.env.borrow_mut().clear_bindings();
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

fn extract_exported_identifiers(exprs: &[Expr]) -> Result<HashSet<String>> {
    let mut symbols: HashSet<String> = HashSet::new();
    for expr in exprs {
        match expr {
            Expr::VectorVal(list_of_tokens) if is_export_statement(expr) => {
                for identifier in &list_of_tokens[1..] {
                    if let Expr::Atom(SyntaxObject { ty: tt, .. }) = identifier {
                        if let TokenType::Identifier(t) = tt {
                            symbols.insert(t.clone());
                        } else {
                            stop!(TypeMismatch => "require expects identifiers");
                        }
                    // symbols.insert(t.clone());
                    } else {
                        stop!(TypeMismatch => "require expects identifiers");
                    }
                }
            }
            _ => {}
        }
    }
    Ok(symbols)
}

// if is_require_statement(e

// fn is_require_statement()

fn extract_macro_definitions(exprs: &[Expr], env: &Rc<RefCell<Env>>) -> Result<Vec<Expr>> {
    let mut others: Vec<Expr> = Vec::new();
    for expr in exprs {
        match expr {
            Expr::VectorVal(list_of_tokens) if is_macro_definition(expr) => {
                construct_macro_def(&list_of_tokens[1..], env)?;
            }
            _ => others.push(expr.clone()),
        }
    }
    Ok(others)
}

fn identify_function_definitions(
    exprs: &[Expr],
    env: &Rc<RefCell<Env>>,
    _heap: &mut Vec<Rc<RefCell<Env>>>,
    _expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<Rc<Expr>>,
) -> Result<()> {
    // let mut others: Vec<Rc<Expr>> = Vec::new();
    for expr in exprs {
        match expr {
            Expr::VectorVal(_list_of_tokens) if is_function_definition(expr) => {
                // eval_define(&list_of_tokens[1..], env, heap, expr_stack)?;
                unimplemented!();
            }
            Expr::VectorVal(list_of_tokens) if is_struct_definition(expr) => {
                let defs = SteelStruct::generate_from_tokens(&list_of_tokens[1..])?;
                env.borrow_mut()
                    .define_zipped(defs.into_iter().map(|x| (x.0, Gc::new(x.1))));
            }
            _ => {}
        }
    }
    Ok(())
    // Ok(others)
}

fn extract_and_expand_function_definitions(
    exprs: &[Expr],
    env: &Rc<RefCell<Env>>,
    _heap: &mut Vec<Rc<RefCell<Env>>>,
    _expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<Rc<Expr>>,
) -> Result<Vec<Expr>> {
    let mut others: Vec<Expr> = Vec::new();
    for expr in exprs {
        match expr {
            Expr::VectorVal(_list_of_tokens) if is_function_definition(expr) => {
                // eval_define(&list_of_tokens[1..], env, heap, expr_stack)?;
                unimplemented!();
            }
            Expr::VectorVal(list_of_tokens) if is_struct_definition(expr) => {
                let defs = SteelStruct::generate_from_tokens(&list_of_tokens[1..])?;
                env.borrow_mut()
                    .define_zipped(defs.into_iter().map(|x| (x.0, Gc::new(x.1))));
            }
            _ => others.push(expr.clone()),
        }
    }
    Ok(others)
}

fn extract_and_compile_requires(exprs: &[Expr], env: &Rc<RefCell<Env>>) -> Result<Vec<Expr>> {
    // unimplemented!();
    let mut others: Vec<Expr> = Vec::new();
    for expr in exprs {
        match expr {
            Expr::VectorVal(list_of_tokens) if is_require_statement(expr) => {
                // eval_define(&list_of_tokens[1..], env, heap, last_expr)?;
                eval_require(&list_of_tokens[1..], env)?;
            }
            _ => others.push(expr.clone()),
        }
    }
    Ok(others)
}

pub fn construct_macro_def(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<()> {
    let parsed_macro = SteelMacro::parse_from_tokens(list_of_tokens, env)?;
    // println!("{:?}", parsed_macro);
    env.borrow_mut().define(
        parsed_macro.name().to_string(),
        Gc::new(SteelVal::MacroV(parsed_macro)),
    );
    Ok(())
}

// fn construct_define(list_of_tokens: &[Rc<Expr>], env: &Rc<RefCell<Env>>) -> Result<()> {
//     unimplemented!()
// }

fn is_require_statement(expr: &Expr) -> bool {
    // let expr = Rc::clone(expr);
    match expr {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        if s == "require" {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

fn is_export_statement(expr: &Expr) -> bool {
    // let expr = Rc::clone(expr);
    match expr {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        if s == "export" {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

pub fn is_macro_definition(expr: &Expr) -> bool {
    // let expr = Rc::clone(expr);
    match expr {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        if s == "define-syntax" {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

pub fn is_struct_definition(expr: &Expr) -> bool {
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

// TODO include the intern cache when possible
pub fn is_function_definition(expr: &Expr) -> bool {
    // let expr = Rc::clone(expr);
    match expr {
        Expr::Atom(_) => return false,
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        if s == "define" {
                            return true;
                        }
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
