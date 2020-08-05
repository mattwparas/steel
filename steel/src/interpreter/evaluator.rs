use std::cell::RefCell;
use std::convert::TryFrom;
use std::iter::Iterator;
use std::rc::Rc;
use std::result;

use crate::env::{Env, FALSE, TRUE, VOID};
use crate::parser::span::Span;
use crate::parser::tokens::TokenType;
use crate::parser::{Expr, ParseError, Parser, SyntaxObject};
use crate::primitives::ListOperations;
use crate::rerrs::SteelErr;
use crate::rvals::{FunctionSignature, Result, SteelLambda, SteelVal, StructClosureSignature};
use crate::stop;
use crate::structs::SteelStruct;
use crate::throw;
// extern crate generational_arena;
use generational_arena::Arena;
use std::collections::HashMap;
// use std::collections::HashSet;
use std::ops::Deref;

use crate::compiler::AST;
// use crate::rvals::MacroPattern;
use crate::expander::SteelMacro;

// use codespan_reporting::files::SimpleFile;
use codespan_reporting::files::SimpleFiles;

pub struct Scope {
    global_env: Rc<RefCell<Env>>,
    intern_cache: HashMap<String, Rc<Expr>>,
}

impl Scope {
    pub fn new(global_env: Rc<RefCell<Env>>, intern_cache: HashMap<String, Rc<Expr>>) -> Self {
        Scope {
            global_env,
            intern_cache,
        }
    }

    pub fn raw() -> Self {
        Scope {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            intern_cache: HashMap::new(),
        }
    }
}

pub struct Evaluator {
    global_env: Rc<RefCell<Env>>,
    intern_cache: HashMap<String, Rc<TokenType>>,
    heap: Vec<Rc<RefCell<Env>>>,
    // expr_stack: Option<&Expr>,
    expr_stack: Vec<Expr>,
    files: SimpleFiles<String, String>,
    // last_macro: Option<&Expr>,
}

impl Evaluator {
    pub fn new() -> Self {
        // env.borrow_mut()
        //         .add_module(AST::compile_module(path, &mut intern)?)

        Evaluator {
            global_env: Self::generate_default_env_with_prelude().unwrap(), // if this fails, we have a problem
            // global_env: Rc::new(RefCell::new(Env::d))
            intern_cache: HashMap::new(),
            heap: Vec::new(),
            // expr_stack: None,
            expr_stack: Vec::new(),
            files: SimpleFiles::new(),
            // last_macro: None,
        }
    }

    pub fn new_raw() -> Self {
        Evaluator {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            intern_cache: HashMap::new(),
            heap: Vec::new(),
            // expr_stack: None,
            expr_stack: Vec::new(),
            files: SimpleFiles::new(),
            // last_macro: None,
        }
    }

    pub fn generate_default_env_with_prelude() -> Result<Rc<RefCell<Env>>> {
        let mut intern = HashMap::new();
        let def_env = Rc::new(RefCell::new(Env::default_env()));

        // let load_order = &[
        //     crate::stdlib::PRELUDE,
        //     crate::stdlib::CONTRACTS,
        //     crate::stdlib::TYPES,
        //     crate::stdlib::METHODS,
        //     crate::stdlib::MERGE,
        // ];

        // In order to fix memory leak, every environment has to hold a weak reference
        // to the parent

        // otherwise this environment will not get dropped I believe

        def_env
            .borrow_mut()
            .add_module(Evaluator::parse_and_compile_with_env_and_intern(
                crate::stdlib::PRELUDE,
                Rc::new(RefCell::new(Env::default_env())),
                &mut intern,
            )?);

        // for module in load_order {
        //     def_env
        //         .borrow_mut()
        //         .add_module(Evaluator::parse_and_compile_with_env_and_intern(
        //             module,
        //             Rc::new(RefCell::new(Env::default_env())),
        //             &mut intern,
        //         )?)
        // }

        Ok(def_env)
    }

    pub fn eval(&mut self, expr: Expr) -> Result<SteelVal> {
        // global environment updates automatically
        let expr = Rc::new(expr);
        let mut heap: Vec<Rc<RefCell<Env>>> = Vec::new();
        let res = evaluate(
            &expr,
            &self.global_env,
            &mut heap,
            &mut self.expr_stack,
            // &mut self.last_macro,
        )
        .map(|x| (*x).clone());

        if self.global_env.borrow().is_binding_context() {
            self.heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
        }

        // for module in self.global_env.borrow().get_modules() {
        //     println!("{:?}", module.get_exported());
        // }
        // println!("{:?}", self.mod)

        res
    }

    pub fn print_bindings(&self) {
        self.global_env.borrow_mut().print_bindings();
    }

    pub fn parse_and_eval(&mut self, expr_str: &str) -> Result<Vec<SteelVal>> {
        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut self.intern_cache).collect();
        let parsed = parsed?;

        let res: Result<Vec<SteelVal>> = parsed.into_iter().map(|x| self.eval(x)).collect();

        // perform the necessary error injection here
        if res.is_err() {
            // println!(
            //     "call stack: {:?}",
            //     &self
            //         .expr_stack
            //         .iter()
            //         .map(|x| x.to_string())
            //         .collect::<Vec<String>>()
            // );

            if let Some(inner) = &self.expr_stack.last() {
                let span = inner.to_string();
                // span.truncate(40);
                // println!("Entire program: {}", expr_str);
                println!("Span information: {}", span);

                if let Err(my_error) = res.as_ref() {
                    // println!("{}", self.last_macro);
                    // if let Some(last) = &self.last_macro {
                    //     println!("Last Macro: {}", last.to_string());
                    // }

                    // println!("Call Stack: {:?}", &self.expr_stack);

                    let mut inner_span = inner.span();

                    if inner_span == Span::new(0, 0) {
                        println!("getting inside here!");

                        let default = Span::new(0, 0);
                        while let Some(l) = &self.expr_stack.iter().rev().next() {
                            let new_span = l.span();
                            if new_span != default {
                                inner_span = new_span;
                                break;
                            }
                        }
                    }

                    // Find the highest env where the error occurred and stash the expression evaluated there?
                    // That way I can say the error occured within a certain context?

                    // for expr in &self.expr_stack.iter().rev() {
                    //     if let Some(pos) =
                    // }

                    my_error.emit_result("repl.rkt", expr_str, inner_span);
                }
            }

            // println!("Last expression executed: {}", expr.to_string());
        }

        self.expr_stack.clear();

        res
    }

    pub fn parse_and_compile_with_env(
        &mut self,
        expr_str: &str,
        env: Rc<RefCell<Env>>,
    ) -> Result<AST> {
        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut self.intern_cache).collect();
        let parsed = parsed?;
        // AST::compile(parsed, env, &mut self.intern_cache)
        AST::compile(expr_str.to_string(), parsed, env)
    }

    pub fn parse_and_compile_with_env_and_intern<'a>(
        expr_str: &str,
        env: Rc<RefCell<Env>>,
        intern: &'a mut HashMap<String, Rc<TokenType>>,
    ) -> Result<AST> {
        let parsed: result::Result<Vec<Expr>, ParseError> = Parser::new(expr_str, intern).collect();
        let parsed = parsed?;
        AST::compile(expr_str.to_string(), parsed, env)
    }

    pub fn eval_with_env_from_ast(compiled_ast: &AST) -> Result<Vec<SteelVal>> {
        let mut heap = Vec::new();
        let mut expr_stack: Vec<Expr> = Vec::new();
        // let mut last_macro: Option<&Expr> = None;
        compiled_ast
            .get_expr()
            .iter()
            .map(|x| {
                evaluate(
                    x,
                    compiled_ast.get_env(),
                    &mut heap,
                    &mut expr_stack,
                    // &mut last_macro,
                )
                .map(|x| (*x).clone())
            })
            .collect()
        // evaluate(AST.get_expr(), AST.get_env())
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

    pub fn get_env(&self) -> &Rc<RefCell<Env>> {
        &self.global_env
    }
}

impl Drop for Evaluator {
    fn drop(&mut self) {
        self.heap.clear();
        // println!("Before exiting: {}", Rc::strong_count(&self.global_env));
        self.global_env.borrow_mut().clear_bindings();
        // println!("After clearing: {}", Rc::strong_count(&self.global_env));
        // TODO print out the intern cache strong counts
        self.intern_cache.clear();
    }
}

fn parse_list_of_identifiers(identifiers: &Expr) -> Result<Vec<String>> {
    match identifiers {
        Expr::VectorVal(l) => {
            let res: Result<Vec<String>> = l
                .iter()
                .map(|x| match x {
                    Expr::Atom(SyntaxObject { ty: t, .. }) => match t {
                        TokenType::Identifier(s) => Ok(s.clone()),
                        _ => Err(SteelErr::TypeMismatch(
                            "lambda must have symbols as arguments".to_string(),
                            Some(identifiers.span()),
                        )),
                    },
                    _ => Err(SteelErr::TypeMismatch(
                        "Lambda must have symbols as arguments".to_string(),
                        Some(identifiers.span()),
                    )),
                })
                .collect();
            res
        }
        _ => Err(SteelErr::TypeMismatch(
            "List of Identifiers".to_string(),
            Some(identifiers.span()),
        )),
    }
}

/// returns error if tokens.len() != expected
fn check_length(what: &str, tokens: &[Expr], expected: usize) -> Result<()> {
    if tokens.len() == expected {
        Ok(())
    } else {
        Err(SteelErr::ArityMismatch(
            format!("{}: expected {} args got {}", what, expected, tokens.len()),
            None,
        ))
    }
}

// TODO include the intern cache when possible
fn expand(expr: &Expr, env: &Rc<RefCell<Env>>) -> Result<Expr> {
    let env = Rc::clone(env);
    // let expr = Rc::clone(expr);

    match expr {
        Expr::Atom(_) => Ok(expr.clone()),
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                if let Expr::Atom(SyntaxObject { ty: t, .. }) = f {
                    if let TokenType::Identifier(s) = t {
                        let lookup = env.borrow().lookup(&s);

                        if let Ok(v) = lookup {
                            if let SteelVal::MacroV(steel_macro) = v.as_ref() {
                                let expanded = steel_macro.expand(&list_of_tokens)?;
                                return expand(&expanded, &env);
                                // return steel_macro.expand(&list_of_tokens)?;
                            }
                        }
                    }
                }
                let result: Result<Vec<Expr>> =
                    list_of_tokens.iter().map(|x| expand(x, &env)).collect();
                Ok(Expr::VectorVal(result?))
            } else {
                Ok(expr.clone())
            }
        }
    }
}

fn evaluate<'a, 'global>(
    expr: &'a Expr,
    env: &'global Rc<RefCell<Env>>,
    heap: &'global mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &'global mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    let mut env = Rc::clone(env);
    let mut expr = expr.clone();
    let mut heap2: Vec<Rc<RefCell<Env>>> = Vec::new();
    let _local_expr_stack: Arena<Expr> = Arena::new();
    let _local_val_stack: Vec<Rc<SteelVal>> = Vec::new();

    loop {
        // expr_stack.push(expr.clone());

        if expr_stack.len() > 200 {
            // println!("Draining the call stack");
            let _ = expr_stack.drain(0..100);
        }

        // println!("{}", count);
        match expr {
            Expr::Atom(t) => return eval_atom(&t, &env),

            Expr::VectorVal(list_of_tokens) => {
                // expr_stack.push(Rc::clone(&expr));
                // last_expr.replace(Rc::clone(&expr));

                if let Some(f) = list_of_tokens.first() {
                    if let Expr::Atom(SyntaxObject { ty: t, .. }) = &f {
                        match t {
                            TokenType::Identifier(s) if s == "quote" => {
                                check_length("Quote", &list_of_tokens, 2)?;
                                let converted = SteelVal::try_from(list_of_tokens[1].clone())?;
                                return Ok(Rc::new(converted));
                            }
                            TokenType::Identifier(s) if s == "if" => {
                                expr = eval_if(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                )?
                                .clone();

                                continue;
                            }
                            TokenType::Identifier(s) if s == "define" => {
                                return eval_define(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                )
                                .map(|_| VOID.with(|f| Rc::clone(f))); // TODO
                            }
                            TokenType::Identifier(s) if s == "define-syntax" => {
                                return eval_macro_def(&list_of_tokens[1..], env)
                                    .map(|_| VOID.with(|f| Rc::clone(f)));
                            }
                            // (lambda (vars*) (body))
                            TokenType::Identifier(s) if s == "lambda" || s == "λ" => {
                                // heap.push(Rc::clone(&env));
                                return eval_make_lambda(&list_of_tokens[1..], env, heap);
                            }
                            TokenType::Identifier(s) if s == "eval" => {
                                return eval_eval_expr(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                );
                            }
                            // set! expression
                            TokenType::Identifier(s) if s == "set!" => {
                                return eval_set(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                );
                            }
                            // (let (var binding)* (body))
                            TokenType::Identifier(s) if s == "let" => {
                                let ex = eval_let(&list_of_tokens[1..], &env)?;
                                // let idx = local_expr_stack.insert(ex);
                                // expr = local_expr_stack.get(idx).unwrap();
                                expr = ex;

                                continue;
                            }
                            TokenType::Identifier(s) if s == "begin" => {
                                expr = eval_begin(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                )?
                                .clone();

                                continue;
                            }
                            TokenType::Identifier(s) if s == "apply" => {
                                return eval_apply(
                                    &list_of_tokens[1..],
                                    env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                );
                            }
                            // Catches errors and captures an Error result from the execution
                            // resumes execution at the other branch of the execution
                            // try! should match the following form:

                            /*
                            (try! [expression1] [except expression2])
                            */
                            TokenType::Identifier(s) if s == "try!" => {
                                // unimplemented!();
                                let result = eval_try(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                )?;
                                match result {
                                    (Some(expr_except), _) => expr = expr_except.clone(),
                                    (None, ok) => return ok,
                                }

                                continue;
                            }

                            TokenType::Identifier(s) if s == "export" => {
                                // TODO
                                unimplemented!()
                            }

                            TokenType::Identifier(s) if s == "require" => {
                                // TODO
                                return eval_require(&list_of_tokens[1..], &env)
                                    .map(|_| VOID.with(|f| Rc::clone(f)));
                            }

                            TokenType::Identifier(s) if s == "mapR" => {
                                return eval_map(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                );
                            }
                            TokenType::Identifier(s) if s == "filterR" => {
                                return eval_filter(
                                    &list_of_tokens[1..],
                                    &env,
                                    heap,
                                    expr_stack,
                                    // last_macro,
                                );
                            }
                            TokenType::Identifier(s) if s == "struct" => {
                                let defs = SteelStruct::generate_from_tokens(&list_of_tokens[1..])?;
                                env.borrow_mut()
                                    .define_zipped(defs.into_iter().map(|x| (x.0, Rc::new(x.1))));
                                return Ok(VOID.with(|f| Rc::clone(f)));
                            }

                            _ => {}
                        }
                    }

                    // let f2 = f.clone();
                    // let e2 = evaluate(f, &env, heap, expr_stack)?;

                    // local_val_stack.push();

                    let evaluated = evaluate(f, &env, heap, expr_stack)?;

                    match evaluated.deref() {
                        SteelVal::FuncV(func) => {
                            // println!("GETTING TO PURE FUNCTION WITH ENV: ");
                            // env.borrow().print_bindings();
                            return eval_func(
                                *func,
                                &list_of_tokens[1..],
                                &env,
                                heap,
                                expr_stack,
                                // last_macro,
                            );
                        }
                        SteelVal::LambdaV(lambda) => {
                            heap2.push(Rc::clone(&env));
                            // heap.push(Rc::clone(&env));
                            // println!("Heap size: {}", heap.len());
                            // println!("Adding to the heap:");
                            // env.borrow().print_bindings();
                            let (new_expr, new_env) = eval_lambda(
                                lambda,
                                &list_of_tokens[1..],
                                &env,
                                heap,
                                expr_stack,
                                // last_macro,
                            )?;

                            expr = new_expr.clone();
                            env = new_env;
                            // heap.pop();
                            // heap.push(Rc::clone(&env));
                        }
                        SteelVal::MacroV(steel_macro) => {
                            // last_macro.replace(Rc::clone(&expr));
                            // println!("Found macro definition!");

                            // let idx = local_expr_stack.insert(steel_macro.expand(&list_of_tokens)?);
                            expr = steel_macro.expand(&list_of_tokens)?;

                            // last_macro.replace(Rc::clone(&expr));
                            // println!("{:?}", expr.clone().to_string());
                            // println!()
                        }
                        SteelVal::StructClosureV(factory, function) => {
                            return eval_struct_constructor(
                                &list_of_tokens[1..],
                                &factory,
                                *function,
                                env,
                                heap,
                                expr_stack,
                                // last_macro,
                            );
                        }
                        e => {
                            let error_message = format!(
                                "Application not a procedure - expected a function, found: {}",
                                e
                            );
                            // let mut span = expr.to_string();
                            // span.truncate(20);
                            // println!("Span information: {}", span);
                            stop!(TypeMismatch => error_message);
                        }
                    }

                // match f {
                //     Expr::Atom(SyntaxObject { ty: t, .. }) => match t.as_ref() {
                //         // (sym args*), sym must be a procedure
                //         _sym => {}
                //     },

                //     // (sym args*), sym must be a procedure
                //     _sym => {
                //         match evaluate(f, &env, heap, expr_stack)?.as_ref() {
                //             SteelVal::FuncV(func) => {
                //                 // println!("GETTING TO PURE FUNCTION WITH ENV: ");
                //                 // env.borrow().print_bindings();
                //                 return eval_func(
                //                     *func,
                //                     &list_of_tokens[1..],
                //                     &env,
                //                     heap,
                //                     expr_stack,
                //                     // last_macro,
                //                 );
                //             }
                //             SteelVal::LambdaV(lambda) => {
                //                 heap2.push(Rc::clone(&env));
                //                 // heap.push(Rc::clone(&env));
                //                 // println!("Heap size: {}", heap.len());
                //                 // println!("Adding to the heap:");
                //                 // env.borrow().print_bindings();
                //                 let (new_expr, new_env) = eval_lambda(
                //                     lambda,
                //                     &list_of_tokens[1..],
                //                     &env,
                //                     heap,
                //                     expr_stack,
                //                     // last_macro,
                //                 )?;
                //                 expr = new_expr;
                //                 env = new_env;

                //                 continue;
                //                 // heap.pop();
                //                 // heap.push(Rc::clone(&env));
                //             }
                //             SteelVal::MacroV(steel_macro) => {
                //                 // last_macro.replace(Rc::clone(&expr));
                //                 // println!("Found macro definition!");
                //                 local_expr_stack.push(steel_macro.expand(&list_of_tokens)?);
                //                 expr = local_expr_stack.last().unwrap();

                //                 // last_macro.replace(Rc::clone(&expr));
                //                 // println!("{:?}", expr.clone().to_string());
                //                 // println!()
                //             }
                //             SteelVal::StructClosureV(factory, function) => {
                //                 return eval_struct_constructor(
                //                     &list_of_tokens[1..],
                //                     &factory,
                //                     *function,
                //                     env,
                //                     heap,
                //                     expr_stack,
                //                     // last_macro,
                //                 );
                //             }
                //             e => {
                //                 let error_message = format!("Application not a procedure - expected a function, found: {}", e);
                //                 // let mut span = expr.to_string();
                //                 // span.truncate(20);
                //                 // println!("Span information: {}", span);
                //                 stop!(TypeMismatch => error_message);
                //             }
                //         }
                //     }
                // }
                } else {
                    stop!(TypeMismatch => "Given empty list")
                }
            }
        }
    }
}

pub fn eval_require(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<()> {
    let mut intern: HashMap<String, Rc<TokenType>> = HashMap::new();
    for expr in list_of_tokens {
        if let Expr::Atom(SyntaxObject { ty: t, .. }) = expr {
            if let TokenType::StringLiteral(path) = t {
                env.borrow_mut()
                    .add_module(AST::compile_module(path, &mut intern)?)
            } else {
                stop!(Generic => "require takes a string as an argument");
            }
        }
    }
    Ok(())
}

// TODO come back to this
// (try! (expr) )
fn eval_try<'a>(
    list_of_tokens: &'a [Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<(Option<&'a Expr>, Result<Rc<SteelVal>>)> {
    // unimplemented!();
    if let [test_expr, except_expr] = list_of_tokens {
        let result = evaluate(&test_expr, env, heap, expr_stack);
        match result.as_ref() {
            // Ok(result) => {
            //     return 
            // }
            Err(_) => {
                return Ok((Some(except_expr), result))
            }
            _ => return Ok((None, result))

            // SteelVal::BoolV(true) => Ok(Rc::clone(then_expr)),
            // _ => Ok(Rc::clone(else_expr)),
        }
    } else {
        let e = format!(
            "{}: expected {} args got {}",
            "try!",
            2,
            list_of_tokens.len()
        );
        stop!(ArityMismatch => e);
    }
}

fn eval_struct_constructor(
    list_of_tokens: &[Expr],
    factory: &SteelStruct,
    func: StructClosureSignature,
    env: Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    let args_eval: Result<Vec<Rc<SteelVal>>> = list_of_tokens
        .iter()
        .map(|x| evaluate(&x, &env, heap, expr_stack))
        .collect();
    let args_eval = args_eval?;
    // not a "pure" function per se, takes the factory from the struct
    func(args_eval, factory)
}

// fn concat_idents(list_of_tokens: &[Expr]) -> Rc<Expr> {
//     list_of_tokens.map(|x|)
// }

// TODO
// this is super super super super not good but it is what it is for now
fn eval_apply(
    list_of_tokens: &[Expr],
    env: Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    let (func, rest) = list_of_tokens
        .split_first()
        .ok_or_else(throw!(TypeMismatch => "apply expects at least 2 arguments"))?;

    let (last, optional_args) = rest
        .split_last()
        .ok_or_else(throw!(TypeMismatch => "apply expected at least 2 arguments"))?;

    let list_res = evaluate(last, &env, heap, expr_stack)?;

    match list_res.as_ref() {
        SteelVal::Pair(_, _) => {}
        _ => stop!(TypeMismatch => "apply expected a list in the last position"),
    }

    let vec_of_vals = ListOperations::collect_into_vec(&list_res)?;
    let optional_args: Result<Vec<Rc<SteelVal>>> = optional_args
        .iter()
        .map(|x| evaluate(&x, &env, heap, expr_stack))
        .collect();

    let mut optional_args = optional_args?;

    optional_args.extend(vec_of_vals);

    match evaluate(func, &env, heap, expr_stack)?.as_ref() {
        SteelVal::FuncV(func) => {
            func(optional_args)
            // return eval_func(*func, &list_of_tokens[1..], &env)
        }
        SteelVal::LambdaV(lambda) => {
            // build a new environment using the parent environment

            if let Some(parent_env) = lambda.parent_env() {
                // let parent_env = lambda.parent_env();
                let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, 0)));
                let params_exp = lambda.params_exp();
                inner_env
                    .borrow_mut()
                    .define_all(params_exp, optional_args)?;

                evaluate(&lambda.body_exp(), &inner_env, heap, expr_stack)
            } else if let Some(parent_env) = lambda.sub_expression_env() {
                // unimplemented!()
                let inner_env =
                    Rc::new(RefCell::new(Env::new_subexpression(parent_env.clone(), 0)));
                let params_exp = lambda.params_exp();
                inner_env
                    .borrow_mut()
                    .define_all(params_exp, optional_args)?;

                evaluate(&lambda.body_exp(), &inner_env, heap, expr_stack)
            } else {
                stop!(Generic => "Root env is missing!")
            }

            // let parent_env = lambda.parent_env();
            // let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
            // let params_exp = lambda.params_exp();
            // inner_env
            //     .borrow_mut()
            //     .define_all(params_exp, optional_args)?;

            // evaluate(&lambda.body_exp(), &inner_env)
        }
        e => {
            let error_message = format!(
                "Application not a procedure - expected a function, found: {}",
                e
            );
            stop!(TypeMismatch => error_message);
        }
    }
}

pub fn eval_macro_def(list_of_tokens: &[Expr], env: Rc<RefCell<Env>>) -> Result<Rc<RefCell<Env>>> {
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
fn eval_filter(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    if let [func_expr, list_expr] = list_of_tokens {
        let func_res = evaluate(&func_expr, env, heap, expr_stack)?;
        let list_res = evaluate(&list_expr, env, heap, expr_stack)?;

        match list_res.as_ref() {
            SteelVal::Pair(_, _) => {}
            _ => stop!(TypeMismatch => "filter expected a list"),
        }

        // let vec_of_vals = ListOperations::collect_into_vec(&list_res)?;
        let vec_of_vals = SteelVal::iter(list_res);
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
                    if let Some(parent_env) = lambda.parent_env() {
                        // let parent_env = lambda.parent_env();
                        let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, 0)));
                        let params_exp = lambda.params_exp();
                        inner_env
                            .borrow_mut()
                            .define_all(params_exp, vec![val.clone()])?;
                        let result = evaluate(&lambda.body_exp(), &inner_env, heap, expr_stack)?;

                        if let SteelVal::BoolV(true) = result.as_ref() {
                            collected_results.push(val);
                        }
                    } else if let Some(parent_env) = lambda.sub_expression_env() {
                        // unimplemented!()
                        let inner_env =
                            Rc::new(RefCell::new(Env::new_subexpression(parent_env.clone(), 0)));
                        let params_exp = lambda.params_exp();
                        inner_env
                            .borrow_mut()
                            .define_all(params_exp, vec![val.clone()])?;
                        let result = evaluate(&lambda.body_exp(), &inner_env, heap, expr_stack)?;
                        if let SteelVal::BoolV(true) = result.as_ref() {
                            collected_results.push(val);
                        }
                    } else {
                        stop!(Generic => "Root env is missing!")
                    }
                }
                // TODO
                // SteelVal::StructClosureV(factory, function) => {
                //     let result = eval_struct_constructor(
                //         &list_of_tokens[1..],
                //         &factory,
                //         *function,
                //         env,
                //         heap,
                //     )?;

                //     let result = func(vec![val.clone()])?;
                //     if let SteelVal::BoolV(true) = result.as_ref() {
                //         collected_results.push(val);
                //     }
                // }
                e => {
                    // println!("Getting in here the filter case");
                    stop!(TypeMismatch => e)
                }
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
fn eval_map(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    println!("inside here!");

    if let [func_expr, list_expr] = list_of_tokens {
        let func_res = evaluate(&func_expr, env, heap, expr_stack)?;
        let list_res = evaluate(&list_expr, env, heap, expr_stack)?;

        match list_res.as_ref() {
            SteelVal::Pair(_, _) => {}
            _ => stop!(TypeMismatch => "map expected a list"),
        }

        let vec_of_vals = SteelVal::iter(list_res);

        // let vec_of_vals = ListOperations::collect_into_vec(&list_res)?;
        let mut collected_results = Vec::new();

        for val in vec_of_vals {
            match func_res.as_ref() {
                SteelVal::FuncV(func) => {
                    collected_results.push(func(vec![val])?);
                }
                SteelVal::LambdaV(lambda) => {
                    if let Some(parent_env) = lambda.parent_env() {
                        // let parent_env = lambda.parent_env();
                        let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, 0)));
                        let params_exp = lambda.params_exp();
                        inner_env
                            .borrow_mut()
                            .define_all(params_exp, vec![val.clone()])?;
                        let result = evaluate(&lambda.body_exp(), &inner_env, heap, expr_stack)?;
                        collected_results.push(result);
                    // if let SteelVal::BoolV(true) = result.as_ref() {
                    //     collected_results.push(val);
                    // }
                    } else if let Some(parent_env) = lambda.sub_expression_env() {
                        // unimplemented!()
                        let inner_env =
                            Rc::new(RefCell::new(Env::new_subexpression(parent_env.clone(), 0)));
                        let params_exp = lambda.params_exp();
                        inner_env
                            .borrow_mut()
                            .define_all(params_exp, vec![val.clone()])?;
                        let result = evaluate(&lambda.body_exp(), &inner_env, heap, expr_stack)?;
                        collected_results.push(result);
                    // if let SteelVal::BoolV(true) = result.as_ref() {
                    //     collected_results.push(val);
                    // }
                    } else {
                        stop!(Generic => "Root env is missing!")
                    }

                    // build a new environment using the parent environment
                    // let parent_env = lambda.parent_env();
                    // let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
                    // let params_exp = lambda.params_exp();
                    // inner_env.borrow_mut().define_all(params_exp, vec![val])?;

                    // let result = evaluate(&lambda.body_exp(), &inner_env)?;
                    // collected_results.push(result);
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
fn eval_atom(t: &SyntaxObject, env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => {
            if *b {
                Ok(TRUE.with(|f| Rc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Rc::clone(f)))
            }
        }
        TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(Rc::new(SteelVal::NumV(*n))),
        TokenType::StringLiteral(s) => Ok(Rc::new(SteelVal::StringV(s.clone()))),
        TokenType::CharacterLiteral(c) => Ok(Rc::new(SteelVal::CharV(*c))),
        TokenType::IntegerLiteral(n) => Ok(Rc::new(SteelVal::IntV(*n))),
        what => {
            // println!("getting here");
            stop!(UnexpectedToken => what)
        }
    }
}
/// evaluates a primitive function into single returnable value
fn eval_func(
    func: FunctionSignature,
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    // let mut expr_stack: Vec<&Expr> = ;
    let args_eval: Result<Vec<Rc<SteelVal>>> = list_of_tokens
        .iter()
        .map(|x| evaluate(&x, &env, heap, expr_stack))
        .collect();
    let args_eval = args_eval?;
    // pure function doesn't need the env
    func(args_eval)
}

// fn eval_and(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
//     for expr in list_of_tokens {
//         match evaluate(expr, env)?.as_ref() {
//             SteelVal::BoolV(true) => continue,
//             SteelVal::BoolV(false) => return Ok(FALSE.with(|f| Rc::clone(f))),
//             _ => continue,
//         }
//     }
//     Ok(TRUE.with(|f| Rc::clone(f)))
// }

// fn eval_or(list_of_tokens: &[Expr], env: &Rc<RefCell<Env>>) -> Result<Rc<SteelVal>> {
//     for expr in list_of_tokens {
//         match evaluate(expr, env)?.as_ref() {
//             SteelVal::BoolV(true) => return Ok(TRUE.with(|f| Rc::clone(f))), // Rc::new(SteelVal::BoolV(true))),
//             _ => continue,
//         }
//     }
//     Ok(FALSE.with(|f| Rc::clone(f)))
// }

/// evaluates a lambda into a body expression to execute
/// and an inner environment
/// TODO - come back to eliminate the cloning that occurs in the symbol -> String process
fn eval_lambda<'a, 'global>(
    lambda: &'a SteelLambda,
    list_of_tokens: &'a [Expr],
    env: &Rc<RefCell<Env>>,
    heap: &'global mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &'global mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<(&'a Expr, Rc<RefCell<Env>>)> {
    let args_eval: Result<Vec<Rc<SteelVal>>> = list_of_tokens
        .iter()
        .map(|x| evaluate(&x, &env, heap, expr_stack))
        .collect();
    let args_eval: Vec<Rc<SteelVal>> = args_eval?;

    // heap.pop();

    if let Some(parent_env) = lambda.parent_env() {
        // let parent_env = lambda.parent_env();
        let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, 0)));
        let params_exp = lambda.params_exp();
        inner_env.borrow_mut().define_all(params_exp, args_eval)?;
        Ok((lambda.body_exp(), inner_env))
    // inner_env
    //     .borrow_mut()
    //     .define_all(params_exp, optional_args)?;

    // evaluate(&lambda.body_exp(), &inner_env)
    } else if let Some(parent_env) = lambda.sub_expression_env() {
        // unimplemented!()
        let inner_env = Rc::new(RefCell::new(Env::new_subexpression(parent_env.clone(), 0)));
        let params_exp = lambda.params_exp();
        inner_env.borrow_mut().define_all(params_exp, args_eval)?;
        Ok((lambda.body_exp(), inner_env))
    // inner_env
    //     .borrow_mut()
    //     .define_all(params_exp, optional_args)?;

    // evaluate(&lambda.body_exp(), &inner_env)
    } else {
        stop!(Generic => "Root env is missing!")
    }

    /*
        // build a new environment using the parent environment
        let parent_env = lambda.parent_env();
        let inner_env = Rc::new(RefCell::new(Env::new(&parent_env)));
        let params_exp = lambda.params_exp();
        inner_env.borrow_mut().define_all(params_exp, args_eval)?;
        // loop back and continue
        // using the body as continuation
        // environment also gets updated
        Ok((lambda.body_exp(), inner_env))
    */
}
/// evaluates `(test then else)` into `then` or `else`
fn eval_if<'a>(
    list_of_tokens: &'a [Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<&'a Expr> {
    if let [test_expr, then_expr, else_expr] = list_of_tokens {
        match evaluate(&test_expr, env, heap, expr_stack)?.as_ref() {
            SteelVal::BoolV(true) => Ok(then_expr),
            _ => Ok(else_expr),
        }
    } else {
        let e = format!("{}: expected {} args got {}", "If", 3, list_of_tokens.len());
        stop!(ArityMismatch => e);
    }
}

fn eval_make_lambda(
    list_of_tokens: &[Expr],
    parent_env: Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
) -> Result<Rc<SteelVal>> {
    if list_of_tokens.len() > 1 {
        let list_of_symbols = &list_of_tokens[0];
        let mut body_exps = list_of_tokens[1..].to_vec();
        let mut begin_body: Vec<Expr> = vec![Expr::Atom(SyntaxObject::default(
            TokenType::Identifier("begin".to_string()),
        ))];
        begin_body.append(&mut body_exps);

        let parsed_list = parse_list_of_identifiers(list_of_symbols)?;

        let new_expr = Expr::VectorVal(begin_body);

        let constructed_lambda = if parent_env.borrow().is_root() {
            // heap.push(Rc::clone(&parent_env));
            // println!("Getting inside here!");
            SteelLambda::new(
                parsed_list,
                expand(&new_expr, &parent_env)?,
                Some(parent_env),
                None,
            )
        } else {
            heap.push(Rc::clone(&parent_env));

            SteelLambda::new(
                parsed_list,
                expand(&new_expr, &parent_env)?,
                None,
                Some(Rc::downgrade(&parent_env)),
            )
        };

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
fn eval_begin<'a>(
    list_of_tokens: &'a [Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<&'a Expr> {
    let mut tokens_iter = list_of_tokens.iter();
    let last_token = tokens_iter.next_back();
    // throw away intermediate evaluations
    for token in tokens_iter {
        evaluate(token, env, heap, expr_stack)?;
    }
    if let Some(v) = last_token {
        Ok(v)
    } else {
        stop!(ArityMismatch => "begin requires at least one argument");
    }
}

fn eval_set(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    if let [symbol, rest_expr] = list_of_tokens {
        let value = evaluate(rest_expr, env, heap, expr_stack)?;

        if let Expr::Atom(SyntaxObject { ty: t, .. }) = symbol {
            if let TokenType::Identifier(s) = t {
                env.borrow_mut().set(s.clone(), value)
            } else {
                stop!(TypeMismatch => symbol)
            }
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
fn eval_eval_expr(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<Rc<SteelVal>> {
    if let [e] = list_of_tokens {
        let res_expr = evaluate(e, env, heap, expr_stack)?;
        match <Expr>::try_from(&(*res_expr).clone()) {
            Ok(e) => evaluate(&e, env, heap, expr_stack),
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
pub fn eval_define(
    list_of_tokens: &[Expr],
    env: &Rc<RefCell<Env>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    expr_stack: &mut Vec<Expr>,
    // last_macro: &mut Option<&Expr>,
) -> Result<()> {
    env.borrow_mut().set_binding_context(true);

    if list_of_tokens.len() > 1 {
        match (list_of_tokens.get(0), list_of_tokens.get(1)) {
            (Some(symbol), Some(body)) => {
                match symbol.deref() {
                    Expr::Atom(SyntaxObject { ty: t, .. }) => {
                        if let TokenType::Identifier(s) = t {
                            if list_of_tokens.len() != 2 {
                                let e = format!(
                                    "{}: multiple expressions after the identifier, expected {} args got {}",
                                    "Define",
                                    2,
                                    list_of_tokens.len()
                                );
                                stop!(ArityMismatch => e)
                            }
                            let eval_body = evaluate(body, env, heap, expr_stack)?;
                            env.borrow_mut().define(s.to_string(), eval_body);
                            Ok(())
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
                            if let TokenType::Identifier(s) = t {
                                let mut begin_body = list_of_tokens[1..].to_vec();
                                let mut fake_lambda: Vec<Expr> = vec![
                                    Expr::Atom(SyntaxObject::default(TokenType::Identifier(
                                        "lambda".to_string(),
                                    ))),
                                    Expr::VectorVal(list_of_identifiers[1..].to_vec()),
                                ];
                                fake_lambda.append(&mut begin_body);
                                let constructed_lambda = Expr::VectorVal(fake_lambda);
                                let eval_body =
                                    evaluate(&constructed_lambda, env, heap, expr_stack)?;
                                env.borrow_mut().define(s.to_string(), eval_body);
                                Ok(())
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
// TODO fix this cloning issue
fn eval_let<'a>(list_of_tokens: &'a [Expr], _env: &Rc<RefCell<Env>>) -> Result<Expr> {
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

        let mut combined = vec![Expr::VectorVal(vec![
            Expr::Atom(SyntaxObject::default(TokenType::Identifier(
                "lambda".to_string(),
            ))),
            Expr::VectorVal(bindings_to_check),
            body.clone(),
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

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod length_test {
    use super::*;
    use crate::parser::tokens::TokenType::NumberLiteral;
    use crate::parser::Expr::Atom;

    #[test]
    fn length_test() {
        let tokens = vec![
            Atom(SyntaxObject::default(NumberLiteral(1.0))),
            Atom(SyntaxObject::default(NumberLiteral(2.0))),
        ];
        assert!(check_length("Test", &tokens, 2).is_ok());
    }

    #[test]
    fn mismatch_test() {
        let tokens = vec![
            Atom(SyntaxObject::default(NumberLiteral(1.0))),
            Atom(SyntaxObject::default(NumberLiteral(2.0))),
        ];
        assert!(check_length("Test", &tokens, 1).is_err());
    }
}

#[cfg(test)]
mod parse_identifiers_test {
    use super::*;
    use crate::parser::tokens::TokenType::{Identifier, NumberLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn non_symbols_test() {
        let identifier = VectorVal(vec![
            Atom(SyntaxObject::default(NumberLiteral(1.0))),
            Atom(SyntaxObject::default(NumberLiteral(2.0))),
        ]);

        let res = parse_list_of_identifiers(&identifier);

        assert!(res.is_err());
    }

    #[test]
    fn symbols_test() {
        let identifier = VectorVal(vec![
            Atom(SyntaxObject::default(Identifier("a".to_string()))),
            Atom(SyntaxObject::default(Identifier("b".to_string()))),
        ]);

        let res = parse_list_of_identifiers(&identifier);

        assert_eq!(res.unwrap(), vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn malformed_test() {
        let identifier = Atom(SyntaxObject::default(Identifier("a".to_string())));

        let res = parse_list_of_identifiers(&identifier);

        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_make_lambda_test {
    use super::*;
    use crate::parser::tokens::TokenType::Identifier;
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn not_enough_args_test() {
        let mut heap = Vec::new();
        let list = vec![Atom(SyntaxObject::default(Identifier("a".to_string())))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list, default_env, &mut heap);
        assert!(res.is_err());
    }

    #[test]
    fn not_list_val_test() {
        let mut heap = Vec::new();
        let list = vec![
            Atom(SyntaxObject::default(Identifier("a".to_string()))),
            Atom(SyntaxObject::default(Identifier("b".to_string()))),
            Atom(SyntaxObject::default(Identifier("c".to_string()))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list[1..], default_env, &mut heap);
        assert!(res.is_err());
    }

    #[test]
    fn ok_test() {
        let mut heap = Vec::new();
        let list = vec![
            Atom(SyntaxObject::default(Identifier("a".to_string()))),
            VectorVal(vec![Atom(SyntaxObject::default(Identifier(
                "b".to_string(),
            )))]),
            Atom(SyntaxObject::default(Identifier("c".to_string()))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_make_lambda(&list[1..], default_env, &mut heap);
        assert!(res.is_ok());
    }
}

/*

#[cfg(test)]
mod eval_if_test {
    use super::*;
    use crate::parser::tokens::TokenType::BooleanLiteral;
    use crate::parser::Expr::Atom;

    #[test]
    fn true_test() {
        let mut heap = Vec::new();
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        //        let list = vec![Atom(If), VectorVal(vec![Atom(StringLiteral(">".to_string())), Atom(StringLiteral("5".to_string())), Atom(StringLiteral("4".to_string()))]), Atom(BooleanLiteral(true)), Atom(BooleanLiteral(false))];
        let list = vec![
            Rc::new(Atom(TokenType::Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let res = eval_if(&list[1..], &default_env, &mut heap);
        assert_eq!(res.unwrap(), Rc::new(Atom(BooleanLiteral(true))));
    }

    #[test]
    fn false_test() {
        let mut heap = Vec::new();
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let list = vec![
            Rc::new(Atom(TokenType::Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(false))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let res = eval_if(&list[1..], &default_env, &mut heap);
        assert_eq!(res.unwrap(), Rc::new(Atom(BooleanLiteral(false))));
    }

    #[test]
    fn wrong_length_test() {
        let mut heap = Vec::new();
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let list = vec![
            Rc::new(Atom(TokenType::Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let res = eval_if(&list[1..], &default_env, &mut heap);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_define_test {
    use super::*;
    use crate::parser::tokens::TokenType::{BooleanLiteral, Identifier, StringLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn wrong_length_test() {
        let mut heap = Vec::new();
        let list = vec![Rc::new(Atom(Identifier("a".to_string())))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], &default_env, &mut heap);
        assert!(res.is_err());
    }

    #[test]
    fn no_identifier_test() {
        let mut heap = Vec::new();
        let list = vec![Rc::new(Atom(StringLiteral("a".to_string())))];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], &default_env, &mut heap);
        assert!(res.is_err());
    }

    #[test]
    fn atom_test() {
        let mut heap = Vec::new();
        let list = vec![
            Rc::new(Atom(Identifier("define".to_string()))),
            Rc::new(Atom(Identifier("a".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], &default_env, &mut heap);
        assert!(res.is_ok());
    }

    #[test]
    fn list_val_test() {
        let mut heap = Vec::new();
        let list = vec![
            Rc::new(Atom(Identifier("define".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(Atom(Identifier("a".to_string())))])),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], &default_env, &mut heap);
        assert!(res.is_ok());
    }

    #[test]
    fn list_val_no_identifier_test() {
        let mut heap = Vec::new();
        let list = vec![
            Rc::new(Atom(Identifier("define".to_string()))),
            Rc::new(VectorVal(vec![Rc::new(Atom(StringLiteral(
                "a".to_string(),
            )))])),
            Rc::new(Atom(BooleanLiteral(true))),
        ];
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let res = eval_define(&list[1..], &default_env, &mut heap);
        assert!(res.is_err());
    }
}

#[cfg(test)]
mod eval_let_test {
    use super::*;
    use crate::parser::tokens::TokenType::{BooleanLiteral, NumberLiteral, StringLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn ok_test() {
        let list = vec![
            Rc::new(Atom(TokenType::Identifier("let".to_string()))),
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
            Rc::new(Atom(TokenType::Identifier("let".to_string()))),
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
            Rc::new(Atom(TokenType::Identifier("let".to_string()))),
            Rc::new(Atom(TokenType::Identifier("let".to_string()))),
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
    use crate::parser::tokens::TokenType::{BooleanLiteral, Identifier, NumberLiteral, StringLiteral};
    use crate::parser::Expr::{Atom, VectorVal};

    #[test]
    fn boolean_test() {
        let mut heap = Vec::new();
        let input = Rc::new(Atom(BooleanLiteral(true)));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env, &mut heap).is_ok());
    }

    #[test]
    fn identifier_test() {
        let mut heap = Vec::new();
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        let input = Rc::new(Atom(Identifier("+".to_string())));
        assert!(evaluate(&input, &default_env, &mut heap).is_ok());
    }

    #[test]
    fn number_test() {
        let mut heap = Vec::new();
        let input = Rc::new(Atom(NumberLiteral(10.0)));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env, &mut heap).is_ok());
    }

    #[test]
    fn string_test() {
        let mut heap = Vec::new();
        let input = Rc::new(Atom(StringLiteral("test".to_string())));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env, &mut heap).is_ok());
    }

    #[test]
    fn what_test() {
        let mut heap = Vec::new();
        let input = Rc::new(Atom(Identifier("if".to_string())));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env, &mut heap).is_err());
    }

    #[test]
    fn list_if_test() {
        let mut heap = Vec::new();
        let list = vec![
            Rc::new(Atom(Identifier("if".to_string()))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(true))),
            Rc::new(Atom(BooleanLiteral(false))),
        ];
        let input = Rc::new(VectorVal(list));
        let default_env = Rc::new(RefCell::new(Env::default_env()));
        assert!(evaluate(&input, &default_env, &mut heap).is_ok());
    }
}

*/
