mod arity;
mod expand;
mod map;

pub use arity::Arity;
pub use arity::ArityMap;
pub use expand::expand;
pub use expand::extract_macro_definitions;
pub use map::SymbolMap;

// pub enum ByteCode {}
// use std::cell::RefCell;
// use std::convert::TryFrom;
use std::iter::Iterator;
// use std::rc::Rc;
use std::result;

// use crate::env::{Env, FALSE, TRUE, VOID};
// use crate::parser::lexer::Tokenizer;
// use crate::parser::lexer::TokenStream;
// use crate::parser::tokens::Token;
// use crate::parser::tokens::TokenError;
use crate::parser::tokens::TokenType;
use crate::parser::SyntaxObject;
use crate::parser::{Expr, ParseError, Parser};
// use crate::primitives::ListOperations;
use crate::rerrs::SteelErr;
use crate::rvals::{ByteCodeLambda, Result, SteelVal};
// use crate::stop;
// use crate::structs::SteelStruct;
// use crate::throw;
use std::collections::HashMap;
// use std::ops::Deref;

// use crate::interpreter::evaluator::Evaluator;

use std::ops::Deref;

use crate::env::Env;
use crate::env::FALSE;
use crate::env::TRUE;
use crate::env::VOID;
use std::cell::RefCell;
use std::rc::Rc;

// use std::collections::HashSet;

// use crate::expander::SteelMacro;
// use crate::structs::SteelStruct;

// pub struct Transformer {
//     name: String,
//     exprs: Vec<Expr>,
//     env: Rc<RefCell<Env>>,
// }

// impl Transformer {
//     pub fn new(name: String, exprs: Vec<Expr>, env: Rc<RefCell<Env>>) -> Self {
//         Transformer { name, exprs, env }
//     }

//     pub fn emit_instructions(
//         parsed_exprs: Vec<Expr>,
//         symbol_table: &mut SymbolMap,
//         constants: &mut Vec<Rc<SteelVal>>,
//     ) -> Vec<DenseInstruction> {
//         panic!("Transformer::emit_instructions(...) not implemented");
//     }

//     // pub fn expand_and_store_ast_with_default_env(
//     //     name: &str,
//     //     expr_str: &str,
//     //     symbol_table: &mut SymbolMap,
//     //     constants: &mut Vec<Rc<SteelVal>>,
//     // ) -> Result<Transformer> {
//     //     let mut intern = HashMap::new();
//     //     // let mut results = Vec::new();

//     //     let parsed: result::Result<Vec<Expr>, ParseError> =
//     //         Parser::new(expr_str, &mut intern).collect();
//     //     let parsed = parsed?;

//     //     let macro_env = Rc::new(RefCell::new(Env::root()));
//     //     // let real_env = Rc::new(RefCell::new(Env::default_env()));

//     //     let extracted_statements =
//     //         extract_macro_definitions(&parsed, &macro_env, &real_env, symbol_table)?;

//     //     unimplemented!()
//     // }
// }

// fn recursive_expand(expr: Expr, )

// use crate::interpreter::evaluator::emit_instructions;

// pass define statement
// identify the handle for the function call
// traverse idenitifying function calls
// if the function call is in the tail position of any of the body, then transform that to be an explicit jump -> __JUMP__
// only need to check the last thing in the body
// pub fn identify_tail_call(expr: &Expr) {}

pub fn transform_tail_call(instructions: &mut Vec<Instruction>, defining_context: &str) -> bool {
    println!(
        "Calling transform tail call with function: {}",
        defining_context
    );

    let last_idx = instructions.len() - 1;

    // could panic
    let mut indices = vec![last_idx];

    let mut transformed = false;

    for (idx, instruction) in instructions.iter().enumerate() {
        if instruction.op_code == OpCode::JMP && instruction.payload_size == last_idx {
            indices.push(idx);
        }
    }

    for index in &indices {
        if *index < 2 {
            continue;
        }
        let prev_instruction = instructions.get(index - 1);
        let prev_func_push = instructions.get(index - 2);

        match (prev_instruction, prev_func_push) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                }),
            ) => {
                if s == defining_context {
                    println!("Making optimization!");

                    let new_jmp = Instruction::new_jmp(0);
                    // inject tail call jump
                    instructions[index - 2] = new_jmp;
                    instructions[index - 1] = Instruction::new_pass();
                    transformed = true;
                }
            }
            _ => {}
        }
    }

    // for index in &indices {
    //     instructions[index - 2] = Instruction::new_pass();
    // }

    // let rev_iter = indices.iter().rev();
    // for index in rev_iter {
    //     instructions.remove(index - 2);
    // }

    return transformed;
}

// insert fast path for built in functions
// rather than look up function in env, be able to call it directly?
pub fn collect_defines_from_current_scope(
    instructions: &[Instruction],
    symbol_map: &mut SymbolMap,
) -> usize {
    let mut def_stack: usize = 0;
    let mut count = 0;

    for i in 0..instructions.len() {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::SDEF,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                if def_stack == 0 {
                    let idx = symbol_map.get_or_add(s);
                    count += 1;
                    println!("####### FOUND DEFINE #########");
                    println!("Renaming: {} to index: {}", s, idx);
                    // if let Some(x) = instructions.get_mut(i) {
                    //     x.contents = None;
                    // }
                }

                // def_stack += 1;
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                def_stack += 1;
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => {
                if def_stack > 0 {
                    def_stack -= 1;
                }
            }
            _ => {}
        }
    }

    count
}

pub fn collect_binds_from_current_scope(
    instructions: &mut [Instruction],
    symbol_map: &mut SymbolMap,
    start: usize,
    end: usize,
) {
    let mut def_stack: usize = 0;
    for i in start..end {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::BIND,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                if def_stack == 1 {
                    let idx = symbol_map.add(s);

                    println!("Renaming: {} to index: {}", s, idx);
                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                        x.contents = None;
                    }
                }
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                def_stack += 1;
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => {
                if def_stack > 0 {
                    def_stack -= 1;
                }
            }
            _ => {}
        }
    }
}

pub fn insert_debruijn_indicies(instructions: &mut [Instruction], symbol_map: &mut SymbolMap) {
    let mut stack: Vec<usize> = Vec::new();
    // let mut def_stack: Vec<usize> = Vec::new();

    // Snag the defines that are going to be available from the global scope
    let _ = collect_defines_from_current_scope(instructions, symbol_map);

    // Snag the binds before the defines
    // collect_binds_from_current_scope(instructions, symbol_map);

    // name mangle
    // Replace all identifiers with indices
    for i in 0..instructions.len() {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::PUSH,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                let idx = symbol_map.get(s);
                println!("Renaming: {} to index: {}", s, idx);
                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                    x.contents = None;
                }
            }
            // Is this even necessary?
            Instruction {
                op_code: OpCode::BIND,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                let idx = symbol_map.get_or_add(s);

                println!("Renaming: {} to index: {}", s, idx);
                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                    x.contents = None;
                }
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                stack.push(symbol_map.len());
                // More stuff goes here
                let payload = *(&instructions[i].payload_size);

                // Go through the current scope and collect binds from the lambds
                collect_binds_from_current_scope(instructions, symbol_map, i, i + payload - 1);

                // Go through the current scope and find defines and the count
                let def_count = collect_defines_from_current_scope(
                    &instructions[i + 1..(i + payload - 1)],
                    symbol_map,
                );
                // Set the def count of the NDEFS instruction after the closure
                if let Some(x) = instructions.get_mut(i + 1) {
                    x.payload_size = def_count;
                }
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => symbol_map.roll_back(stack.pop().unwrap()),
            Instruction {
                op_code: OpCode::SDEF,
                // contents:
                //     Some(SyntaxObject {
                //         ty: TokenType::Identifier(s),
                //         ..
                //     }),
                ..
            } => {
                // let idx = symbol_map.add(s);
                // println!("Renaming: {} to index: {}", s, idx);
                if let Some(x) = instructions.get_mut(i) {
                    x.contents = None;
                }
            }
            _ => {}
        }
    }
}

pub fn emit_instructions(
    expr_str: &str,
    symbol_map: &mut SymbolMap,
    constants: &mut Vec<Rc<SteelVal>>,
    global_env: &Rc<RefCell<Env>>,
    arity_map: &mut ArityMap,
) -> Result<Vec<Vec<DenseInstruction>>> {
    let mut intern = HashMap::new();
    let mut results = Vec::new();

    let parsed: result::Result<Vec<Expr>, ParseError> =
        Parser::new(expr_str, &mut intern).collect();
    let parsed = parsed?;

    let macro_env = Rc::new(RefCell::new(Env::root()));
    // let real_env = Rc::new(RefCell::new(Env::default_env()));

    let extracted_statements =
        extract_macro_definitions(&parsed, &macro_env, global_env, symbol_map)?;

    for expr in extracted_statements {
        let mut instructions: Vec<Instruction> = Vec::new();
        emit_loop(&expr, &mut instructions, None, arity_map)?;
        instructions.push(Instruction::new_pop());

        pretty_print_instructions(&instructions);

        insert_debruijn_indicies(&mut instructions, symbol_map);

        println!("Got after the debruijn indices");

        extract_constants(&mut instructions, constants)?;

        let dense_instructions = densify(instructions);

        results.push(dense_instructions);
    }

    Ok(results)
}

pub fn densify(instructions: Vec<Instruction>) -> Vec<DenseInstruction> {
    instructions.into_iter().map(|x| x.into()).collect()
}

pub fn pretty_print_instructions(instrs: &[Instruction]) {
    // for (i, item) in foo.iter().enumerate()
    for (i, instruction) in instrs.iter().enumerate() {
        if instruction.contents.is_some() {
            println!(
                "{}    {:?} : {}     {}",
                i,
                instruction.op_code,
                instruction.payload_size,
                instruction.contents.as_ref().unwrap().ty
            );
        } else {
            println!(
                "{}    {:?} : {}",
                i, instruction.op_code, instruction.payload_size
            );
        }
    }
}

pub fn pretty_print_dense_instructions(instrs: &[DenseInstruction]) {
    // for (i, item) in foo.iter().enumerate()
    for (i, instruction) in instrs.iter().enumerate() {
        println!(
            "{}    {:?} : {}",
            i, instruction.op_code, instruction.payload_size
        );
    }
}

fn emit_loop(
    expr: &Expr,
    instructions: &mut Vec<Instruction>,
    defining_context: Option<&str>,
    arity_map: &mut ArityMap,
) -> Result<()> {
    match expr {
        Expr::Atom(s) => {
            instructions.push(Instruction::new(OpCode::PUSH, 0, s.clone()));
        }
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                match f.deref() {
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "quote" => {
                    //     instructions.push("quote".to_string());
                    //     for expr in &list_of_tokens[1..] {
                    //         emit_loop(Rc::clone(expr), instructions)?;
                    //     }
                    //     return Ok(());
                    // }
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "if" => {
                        // instructions.push("if".to_string());

                        if let [test_expr, then_expr, else_expr] = &list_of_tokens[1..] {
                            // unimplemented!()

                            // load in the test condition
                            emit_loop(test_expr, instructions, None, arity_map)?;
                            // push in If
                            instructions.push(Instruction::new_if(instructions.len() + 2));
                            // save spot of jump instruction, fill in after
                            let idx = instructions.len();
                            instructions.push(Instruction::new_jmp(0)); // dummy

                            // emit instructions for then expression
                            emit_loop(then_expr, instructions, None, arity_map)?;
                            instructions.push(Instruction::new_jmp(0)); // dummy
                            let false_start = instructions.len();

                            // emit instructions for else expression
                            emit_loop(else_expr, instructions, None, arity_map)?;
                            let j3 = instructions.len(); // first instruction after else

                            // set index of jump instruction to be
                            if let Some(elem) = instructions.get_mut(idx) {
                                (*elem).payload_size = false_start;
                            } else {
                                stop!(Generic => "out of bounds jump");
                            }

                            if let Some(elem) = instructions.get_mut(false_start - 1) {
                                (*elem).payload_size = j3
                            } else {
                                stop!(Generic => "out of bounds jump");
                            }

                        // instructions.get_mut(idx)

                        // println!("{}, {}")

                        // instructions.push(Instruction::new_jmp());
                        // instructions.insert(j2 - 2, Instruction::new_jmp(j3));

                        // instructions.insert(idx, Instruction::new_if(j1));
                        // instructions.insert(idx + 1, Instruction::new_jmp(j2));

                        // unimplemented!();
                        } else {
                            stop!(BadSyntax => "malformed if statement");
                        }
                        //     emit_loop(Rc::clone(expr), instructions)?;
                        // }
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "define" || s == "defn" => {
                        let sidx = instructions.len();
                        instructions.push(Instruction::new_sdef());

                        let identifier = &list_of_tokens[1];

                        match identifier {
                            Expr::Atom(syn) => {
                                let defining_context = if let TokenType::Identifier(name) = &syn.ty
                                {
                                    // Get the defining context for the debruijn indices
                                    if let Some(x) = instructions.get_mut(sidx) {
                                        x.contents = Some(syn.clone());
                                    }
                                    Some(name.as_str())
                                } else {
                                    None
                                };

                                if list_of_tokens.len() != 3 {
                                    let e = format!(
                                        "{}: multiple expressions after the identifier, expected {} args got {}",
                                        "Define",
                                        2,
                                        list_of_tokens.len()
                                    );
                                    stop!(ArityMismatch => e)
                                }

                                emit_loop(
                                    &list_of_tokens[2],
                                    instructions,
                                    defining_context,
                                    arity_map,
                                )?;

                                // for expr in &list_of_tokens[2..] {
                                //     emit_loop(expr, instructions, None)?;
                                // }

                                instructions.push(Instruction::new_pop());
                                let defn_body_size = instructions.len() - sidx;
                                instructions.push(Instruction::new_edef());

                                if let Some(elem) = instructions.get_mut(sidx) {
                                    (*elem).payload_size = defn_body_size;
                                } else {
                                    stop!(Generic => "out of bounds closure len");
                                }

                                instructions.push(Instruction::new_bind(syn.clone()));
                                instructions.push(Instruction::new_void());

                                // Roll back scope to default if the depth > 0?
                            }

                            // _ => {}
                            Expr::VectorVal(_l) => {
                                if list_of_tokens.len() < 3 {
                                    let e = format!("define expected a function body");
                                    stop!(ArityMismatch => e)
                                }

                                // let function_information =

                                panic!("Complex defines not yet supported");
                            }
                        }

                        // if let Expr::Atom(syn) = identifier.as_ref() {

                        // // instructions.push(Instruction::new)
                        // } else {
                        //     panic!("Complex defines not yet supported");
                        // }

                        // instructions.push("define".to_string());
                        // return Ok(());
                        // unimplemented!();
                    }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "define-syntax" => {
                    //     instructions.push("define-syntax".to_string());
                    //     return Ok(());
                    // }
                    // (lambda (vars*) (body))
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "lambda" || s == "λ" || s == "fn" => {
                        let idx = instructions.len();
                        instructions.push(Instruction::new_sclosure());

                        instructions.push(Instruction::new_ndef(0)); // Default with 0 for now

                        let list_of_symbols = &list_of_tokens[1];

                        // make recursive call with "fresh" vector so that offsets are correct
                        let mut body_instructions = Vec::new();

                        // let mut arity = 0;
                        let arity;

                        match list_of_symbols {
                            Expr::VectorVal(l) => {
                                arity = l.len();
                                let rev_iter = l.into_iter().rev();
                                for symbol in rev_iter {
                                    if let Expr::Atom(syn) = symbol {
                                        body_instructions.push(Instruction::new_bind(syn.clone()))
                                    } else {
                                        stop!(Generic => "lambda function requires list of identifiers");
                                    }
                                }
                            }
                            _ => stop!(TypeMismatch => "List of Identifiers"),
                        }

                        // let mut body_instructions = Vec::new();

                        for expr in &list_of_tokens[2..] {
                            emit_loop(expr, &mut body_instructions, None, arity_map)?;
                        }

                        // TODO look out here for the
                        body_instructions.push(Instruction::new_pop());

                        if let Some(ctx) = defining_context {
                            transform_tail_call(&mut body_instructions, ctx);
                            arity_map.insert_exact(ctx, arity);
                        }

                        instructions.append(&mut body_instructions);

                        let closure_body_size = instructions.len() - idx;
                        instructions.push(Instruction::new_eclosure(arity));

                        if let Some(elem) = instructions.get_mut(idx) {
                            (*elem).payload_size = closure_body_size;
                        } else {
                            stop!(Generic => "out of bounds closure len");
                        }

                        return Ok(());
                    }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "eval" => {
                    //     instructions.push("eval".to_string());
                    //     for expr in &list_of_tokens[1..] {
                    //         emit_loop(Rc::clone(expr), instructions)?;
                    //     }
                    //     return Ok(());
                    // }
                    // set! expression
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "set!" => {
                    //     instructions.push("set!".to_string());
                    //     return Ok(());
                    // }
                    // (let (var binding)* (body))
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "let" => {
                    //     instructions.push("let".to_string());
                    //     return Ok(());
                    // }
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "begin" => {
                        // instructions.push("begin".to_string());
                        for expr in &list_of_tokens[1..] {
                            emit_loop(expr, instructions, None, arity_map)?;
                        }
                        return Ok(());
                    }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "apply" => {
                    //     instructions.push("apply".to_string());
                    //     for expr in &list_of_tokens[1..] {
                    //         emit_loop(Rc::clone(expr), instructions)?;
                    //     }
                    //     return Ok(());
                    // }
                    // Catches errors and captures an Error result from the execution
                    // resumes execution at the other branch of the execution
                    // try! should match the following form:

                    /*
                    (try! [expression1] [except expression2])
                    */
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "try!" => {
                    //     instructions.push("try!".to_string());
                    //     return Ok(());
                    // }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "export" => {
                    //     instructions.push("export".to_string());
                    //     return Ok(());
                    // }

                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "require" => {
                    //     instructions.push("require".to_string());
                    //     return Ok(());
                    // }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "mapR" => {
                    //     instructions.push("mapR".to_string());
                    //     return Ok(());
                    // }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "filterR" => {
                    //     instructions.push("filterR".to_string());
                    //     return Ok(());
                    // }
                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "struct" => {
                    //     instructions.push("struct".to_string());
                    //     return Ok(());
                    // }
                    // Expr::Atom(s) => {
                    //     let pop_len = &list_of_tokens[1..].len();
                    //     for expr in &list_of_tokens[1..] {
                    //         emit_loop(Rc::clone(expr), instructions)?;
                    //     }
                    //     // instructions.push(format!("PUSH: Function Call: {}, {}", s, pop_len));
                    //     instructions.push(Instruction::new(OpCode::FUNC, *pop_len, s.clone()));
                    //     return Ok(());

                    //     // instructions.push("function call!".to_string());
                    // }
                    // (sym args*), sym must be a procedure
                    _sym => {
                        let pop_len = list_of_tokens[1..].len();

                        // TODO come back to this
                        // Update arity stuff correctly
                        if let Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(_function_name),
                            ..
                        }) = &list_of_tokens[0]
                        {
                            // if !arity_map.check(function_name.as_str(), pop_len) {
                            //     stop!(ArityMismatch => "arity mismatch in function call with function {}", function_name);
                            // }
                        }

                        for expr in &list_of_tokens[1..] {
                            emit_loop(expr, instructions, None, arity_map)?;
                        }

                        emit_loop(f, instructions, None, arity_map)?;

                        instructions.push(Instruction::new_func(pop_len));

                        return Ok(());
                    } // _ => {}
                }
            } else {
                stop!(TypeMismatch => "Given empty list")
            }
        }
    }

    Ok(())
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq)]
pub enum OpCode {
    VOID = 0,
    PUSH = 1,
    LOOKUP = 2,
    IF = 3,
    JMP = 4,
    FUNC = 5,
    SCLOSURE = 6,
    ECLOSURE = 7,
    STRUCT = 8,
    POP = 9,
    BIND = 10,
    SDEF = 11,
    EDEF = 12,
    PASS = 13,
    PUSHCONST = 14,
    NDEFS = 15,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DenseInstruction {
    op_code: OpCode,
    payload_size: usize,
}

impl DenseInstruction {
    pub fn new(op_code: OpCode, payload_size: usize) -> DenseInstruction {
        DenseInstruction {
            op_code,
            payload_size,
        }
    }
}

impl From<Instruction> for DenseInstruction {
    fn from(val: Instruction) -> DenseInstruction {
        DenseInstruction::new(val.op_code, val.payload_size)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    op_code: OpCode,
    payload_size: usize,
    contents: Option<SyntaxObject>,
}

impl Instruction {
    pub fn new(op_code: OpCode, payload_size: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code,
            payload_size,
            contents: Some(contents),
        }
    }

    pub fn new_ndef(payload_size: usize) -> Instruction {
        Instruction {
            op_code: OpCode::NDEFS,
            payload_size,
            contents: None,
        }
    }

    pub fn new_func(arity: usize) -> Instruction {
        Instruction {
            op_code: OpCode::FUNC,
            payload_size: arity,
            contents: None,
        }
    }

    pub fn new_pop() -> Instruction {
        Instruction {
            op_code: OpCode::POP,
            payload_size: 0,
            contents: None,
        }
    }

    pub fn new_if(true_jump: usize) -> Instruction {
        Instruction {
            op_code: OpCode::IF,
            payload_size: true_jump,
            contents: None,
        }
    }

    pub fn new_jmp(jump: usize) -> Instruction {
        Instruction {
            op_code: OpCode::JMP,
            payload_size: jump,
            contents: None,
        }
    }

    pub fn new_sclosure() -> Instruction {
        Instruction {
            op_code: OpCode::SCLOSURE,
            payload_size: 0,
            contents: None,
        }
    }

    pub fn new_eclosure(arity: usize) -> Instruction {
        Instruction {
            op_code: OpCode::ECLOSURE,
            payload_size: arity,
            contents: None,
        }
    }

    pub fn new_bind(contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::BIND,
            payload_size: 0,
            contents: Some(contents),
        }
    }

    pub fn new_sdef() -> Instruction {
        Instruction {
            op_code: OpCode::SDEF,
            payload_size: 0,
            contents: None,
        }
    }

    pub fn new_edef() -> Instruction {
        Instruction {
            op_code: OpCode::EDEF,
            payload_size: 0,
            contents: None,
        }
    }

    pub fn new_void() -> Instruction {
        Instruction {
            op_code: OpCode::VOID,
            payload_size: 0,
            contents: None,
        }
    }

    pub fn new_pass() -> Instruction {
        Instruction {
            op_code: OpCode::PASS,
            payload_size: 0,
            contents: None,
        }
    }
}

pub struct VirtualMachine {
    global_env: Rc<RefCell<Env>>,
    global_heap: Vec<Rc<RefCell<Env>>>,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            global_heap: Vec::new(),
        }
    }

    pub fn emit_instructions(
        &self,
        expr_str: &str,
        symbol_map: &mut SymbolMap,
        constants: &mut Vec<Rc<SteelVal>>,
        arity_map: &mut ArityMap,
        // global_env: &Rc<RefCell<Env>>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut intern = HashMap::new();
        let mut results = Vec::new();

        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut intern).collect();
        let parsed = parsed?;

        let macro_env = Rc::new(RefCell::new(Env::root()));
        // let real_env = Rc::new(RefCell::new(Env::default_env()));

        let extracted_statements =
            extract_macro_definitions(&parsed, &macro_env, &self.global_env, symbol_map)?;

        let expanded_statements: Vec<Expr> = extracted_statements
            .into_iter()
            .map(|x| expand(x, &self.global_env))
            .collect::<Result<Vec<Expr>>>()?;

        // println!("{}", expanded_statements.to_string());

        // for expr

        for expr in expanded_statements {
            println!("{}", expr.to_string());

            let mut instructions: Vec<Instruction> = Vec::new();
            emit_loop(&expr, &mut instructions, None, arity_map)?;
            instructions.push(Instruction::new_pop());

            pretty_print_instructions(&instructions);

            // let mut stack: Vec<usize> = Vec::new();

            insert_debruijn_indicies(&mut instructions, symbol_map);

            println!("Got after the debruijn indices");

            extract_constants(&mut instructions, constants)?;

            let dense_instructions = densify(instructions);

            results.push(dense_instructions);
        }

        Ok(results)
    }

    pub fn execute(
        &mut self,
        instructions: &[DenseInstruction],
        constants: &[Rc<SteelVal>],
    ) -> Result<Rc<SteelVal>> {
        // execute_vm(instructions)
        let mut stack: Vec<Rc<SteelVal>> = Vec::new();
        let mut heap: Vec<Rc<RefCell<Env>>> = Vec::new();
        // let mut constants: Vec<Rc<RefCell<Env>>

        // let global_env = Rc::new(RefCell::new(Env::default_env()));
        let result = vm(
            instructions,
            &mut stack,
            &mut heap,
            Rc::clone(&self.global_env),
            constants,
        );

        if self.global_env.borrow().is_binding_context() {
            self.global_heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
        }

        result
    }
}

pub fn execute_vm(
    instructions: &[DenseInstruction],
    constants: &[Rc<SteelVal>],
) -> Result<Rc<SteelVal>> {
    let mut stack: Vec<Rc<SteelVal>> = Vec::new();
    let mut heap: Vec<Rc<RefCell<Env>>> = Vec::new();
    // let mut constants: Vec<Rc<SteelVal>> = Vec::new();
    let global_env = Rc::new(RefCell::new(Env::default_env()));
    vm(instructions, &mut stack, &mut heap, global_env, constants)
}

// TODO make this not so garbage but its kind of okay
pub fn extract_constants(
    instructions: &mut [Instruction],
    constants: &mut Vec<Rc<SteelVal>>,
) -> Result<()> {
    for i in 0..instructions.len() {
        let inst = &instructions[i];
        if let OpCode::PUSH = inst.op_code {
            let idx = constants.len();
            if inst.contents.is_some() {
                constants.push(eval_atom(&inst.contents.as_ref().unwrap())?);
                if let Some(x) = instructions.get_mut(i) {
                    x.op_code = OpCode::PUSHCONST;
                    x.payload_size = idx;
                    x.contents = None;
                }
            }
        }
    }

    Ok(())
}

pub fn vm(
    instructions: &[DenseInstruction],
    stack: &mut Vec<Rc<SteelVal>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    global_env: Rc<RefCell<Env>>,
    constants: &[Rc<SteelVal>],
) -> Result<Rc<SteelVal>> {
    // let mut stack: Vec<Rc<SteelVal>> = Vec::new();
    let mut ip = 0;

    if instructions.is_empty() {
        stop!(Generic => "empty stack!");
    }

    let mut cur_inst = &instructions[ip];

    while ip < instructions.len() {
        // println!("{:?}", cur_inst);

        cur_inst = &instructions[ip];
        // println!("instruction #: {}", ip);

        // println!("IP: {}", ip);
        match cur_inst.op_code {
            OpCode::PASS => {
                ip += 1;
                // cur_inst = &instructions[ip];
            }
            OpCode::VOID => {
                stack.push(VOID.with(|f| Rc::clone(f)));
                ip += 1;
                // cur_inst = &instructions[ip];
            }
            OpCode::PUSHCONST => {
                let val = Rc::clone(&constants[cur_inst.payload_size]);
                stack.push(val);
                ip += 1;
                // unimplemented!();
            }
            OpCode::PUSH => {
                // if cur_inst.contents.is_some() {
                //     stack.push(eval_atom(&cur_inst.contents.as_ref().unwrap())?);
                // } else {
                let value = global_env.borrow().lookup_idx(cur_inst.payload_size)?;
                stack.push(value);
                // stack.push()
                // Put the lookup of the index here
                // unimplemented!()
                // }

                ip += 1;
                // cur_inst = &instructions[ip];
            }
            OpCode::FUNC => {
                let stack_func = stack.pop().unwrap();

                match stack_func.as_ref() {
                    // SteelVal::SymbolV(s) => {
                    //     let func = global_env.borrow().lookup(s.as_str())?;
                    //     let args = stack.split_off(stack.len() - cur_inst.payload_size);
                    //     if let SteelVal::FuncV(f) = func.as_ref() {
                    //         stack.push(f(args)?);
                    //         ip += 1;
                    //         cur_inst = &instructions[ip];
                    //     } else {
                    //         unimplemented!();
                    //     }
                    // }
                    SteelVal::StructClosureV(factory, func) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        stack.push(func(args, factory)?);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        // println!("Calling function with args: {:?}", args);
                        stack.push(f(args)?);
                        // println!("{:?}", stack);
                        ip += 1;
                        // cur_inst = &instructions[ip];
                    }
                    SteelVal::Closure(closure) => {
                        // println!("Stack inside closure case: {:?}", stack);

                        let mut args = stack.split_off(stack.len() - cur_inst.payload_size);

                        if let Some(parent_env) = closure.parent_env() {
                            // let offset = parent_env.borrow().len()
                            //     + parent_env.borrow().local_offset();

                            // println!("Top Level Closure ndefs: {}", closure.ndef_body());

                            let offset = closure.offset() + parent_env.borrow().local_offset();

                            // let offset = closure.offset() + global_env.borrow().local_offset();

                            // println!("Closure Offset: {}", closure.offset());
                            // println!("Parent offset: {}", parent_env.borrow().local_offset());

                            // println!("############ Offset: {} ###############", offset);
                            // println!("parent_env length: {}", parent_env.borrow().len());
                            // println!("parent_env offset: {}", parent_env.borrow().local_offset());
                            // println!("global_env length: {}", global_env.borrow().len());
                            // println!("Original offset: {}", closure.offset());

                            // println!("############# inside here ############");

                            // if !global_env.borrow().is_root() {
                            //     offset = offset - 1;
                            // }

                            // let parent_env = lambda.parent_env();
                            let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, offset)));

                            inner_env
                                .borrow_mut()
                                .reserve_defs(if closure.ndef_body() > 0 {
                                    closure.ndef_body() - 1
                                } else {
                                    0
                                });

                            // inner_env.borrow_mut().reserve_defs(closure.arity());

                            // inner_env.borrow_mut().reserve_defs(if closure.arity() > 0 {
                            //     closure.arity()
                            // } else {
                            //     0
                            // });

                            // let params_exp = lambda.params_exp();
                            let result =
                                vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                            stack.push(result);

                        // evaluate(&lambda.body_exp(), &inner_env)
                        } else if let Some(parent_env) = closure.sub_expression_env() {
                            // TODO remove this unwrap
                            // let offset = parent_env.upgrade().unwrap().borrow().len()
                            //     + parent_env.upgrade().unwrap().borrow().local_offset(); // TODO
                            // unimplemented!()
                            // offset = offset + 1;

                            // println!("Closure Offset: {}", closure.offset());
                            // println!(
                            //     "Parent offset: {}",
                            //     parent_env.upgrade().unwrap().borrow().local_offset()
                            // );

                            // println!("closure ndef body: {}", closure.ndef_body());

                            // let parent_ndefs =
                            //     parent_env.upgrade().unwrap().borrow().parent_ndefs();

                            // println!("Parent ndefs: {}", parent_ndefs);

                            // // let offset = closure.offset()
                            // //     + parent_env.upgrade().unwrap().borrow().local_offset();

                            // let closure_offset = if closure.offset() > 0 && parent_ndefs > 0 {
                            //     println!("Case 1");
                            //     parent_ndefs - 1 + closure.offset()
                            // } else if closure.offset() == 0 && parent_ndefs > 0 {
                            //     println!("Case 2");
                            //     parent_ndefs
                            // } else {
                            //     println!("Case 3");
                            //     closure.offset()
                            // };

                            let offset = closure.offset()
                                + parent_env.upgrade().unwrap().borrow().local_offset();

                            // if closure.arity() == 0 {
                            //     offset += 1;
                            // }

                            // offset += closure.arity() + 1;

                            // if parent_env.upgrade().unwrap().borrow().is_binding_context()
                            //     && closure.ndef_body() == 1
                            // {
                            //     offset -= 1
                            // };

                            // println!("Earlier offset: {}", offset);
                            // println!("number of defs: {}", closure.ndef_body());

                            // let offset = closure.offset() + global_env.borrow().local_offset();
                            // + closure.arity();

                            // println!("############ Offset: {} ###############", offset);
                            // println!(
                            //     "parent_env length: {}",
                            //     parent_env.upgrade().unwrap().borrow().len()
                            // );
                            // println!(
                            //     "parent_env offset: {}",
                            //     c
                            // );
                            // println!("global_env length: {}", global_env.borrow().len());
                            // println!("Original offset: {}", closure.offset());
                            // if !global_env.borrow().is_root() {
                            //     println!("############################################## here we are #######");
                            //     offset = offset + 1;
                            // }

                            // println!("$$$$$$$$$$$$$$$$$$$ inside second $$$$$$$$$$$$$$$$$");

                            let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                                parent_env.clone(),
                                offset,
                            )));

                            inner_env
                                .borrow_mut()
                                .reserve_defs(if closure.ndef_body() > 0 {
                                    closure.ndef_body() - 1
                                } else {
                                    0
                                });

                            // inner_env.borrow_mut().reserve_defs(if closure.arity() > 0 {
                            //     closure.arity()
                            // } else {
                            //     0
                            // });

                            // if closure.arity() > 0 {
                            //     inner_env.borrow_mut().reserve_defs(closure.arity() - 1);
                            // }

                            let result =
                                vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                            stack.push(result);
                        } else {
                            stop!(Generic => "Root env is missing!")
                        }

                        ip += 1;
                        // cur_inst = &instructions[ip];
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported");
                    }
                }

                // if let TokenType::Identifier(name) = &cur_inst.contents.as_ref().unwrap().ty {
                //     let func = global_env.borrow().lookup(name)?;
                //     let args = stack.split_off(stack.len() - cur_inst.payload_size);
                //     if let SteelVal::FuncV(f) = func.as_ref() {
                //         stack.push(f(args)?);
                //         ip += 1;
                //         cur_inst = &instructions[ip];
                //     } else {
                //         unimplemented!();
                //     }
                // // stack.push(func())
                // } else {
                //     unimplemented!();
                // }
            }
            OpCode::IF => {
                // println!("stack at if: {:?}", stack);
                // stack.pop()
                // if let Some(SteelVal::BoolV(true)) = stack.pop().as_ref()
                if let SteelVal::BoolV(true) = stack.pop().unwrap().as_ref() {
                    ip = cur_inst.payload_size; // Jump to payload
                                                // ip += 2; // Jump to payload
                                                // cur_inst = &instructions[ip];
                } else {
                    ip += 1;
                    // cur_inst = &instructions[ip];
                }
            }
            OpCode::JMP => {
                ip = cur_inst.payload_size;
                // cur_inst = &instructions[ip];
            }
            OpCode::POP => {
                return stack
                    .pop()
                    .ok_or_else(|| SteelErr::Generic("stack empty at pop".to_string()));
            }
            OpCode::BIND => {
                // println!("{}")
                let offset = global_env.borrow().local_offset();

                // if offset != 0 {
                //     offset = offset - 1;
                // }

                // println!(
                //     "Payload size: {}, Offset: {}",
                //     cur_inst.payload_size, offset
                // );

                // Need to reserve the function definition?

                // println!("Stack: {:?}", stack);

                global_env
                    .borrow_mut()
                    .define_idx(cur_inst.payload_size - offset, stack.pop().unwrap());
                ip += 1;
            }
            OpCode::SCLOSURE => {
                ip += 1;

                let forward_jump = cur_inst.payload_size - 1;
                // Snag the number of definitions here
                let ndefs = instructions[ip].payload_size;

                // println!("Found this many function definitions: {}", ndefs);

                ip += 1;

                // Construct the closure body using the offsets from the payload
                // used to be - 1, now - 2
                let closure_body = instructions[ip..(ip + forward_jump - 1)].to_vec();

                // snag the arity from the eclosure instruction
                let arity = instructions[ip + forward_jump - 1].payload_size;

                // println!(
                //     "Arity instruction: {:?}",
                //     instructions[ip + cur_inst.payload_size]
                // );

                // println!("Closure body:");
                // pretty_print_dense_instructions(&closure_body);

                let capture_env = Rc::clone(&global_env);

                // let closure_offset = global_env.borrow().len()
                //     + if global_env.borrow().is_binding_context() {
                //         1
                //     } else {
                //         1
                //     };

                // let closure_offset = global_env.borrow().len()
                //     + if global_env.borrow().is_binding_context() {
                //         1
                //     } else {
                //         0
                //     };

                // if global_env.borrow().is_binding_context() && ndefs > 0 {
                //     // Reserve the spots
                //     capture_env.borrow_mut().reserve_defs(ndefs - 1);
                // } else {
                //     // Reserve the spots
                //     capture_env.borrow_mut().reserve_defs(ndefs);
                // }

                let mut closure_offset = global_env.borrow().len();

                if global_env.borrow().is_binding_context()
                    && !global_env.borrow().is_binding_offset()
                {
                    global_env.borrow_mut().set_binding_offset(true);
                    closure_offset += 1
                };

                // Reserve the spots of the definitions that will be made
                // capture_env.borrow_mut().reserve_defs(ndefs);

                // let closure_offset = global_env.borrow().len();

                // println!("!!!!!!!!!!!! Closure Offset: {}", closure_offset);
                // println!("!!!!!!!!!!!! Closure Arity: {}", arity);
                // println!("!!!!!!!!!!!! Closure ndefs: {}", ndefs);

                // Determine the kind of bytecode lambda to construct
                let constructed_lambda = if capture_env.borrow().is_root() {
                    // println!("Inside this one");

                    // let closure_offset = global_env.borrow().len()
                    //     + if global_env.borrow().is_binding_context() {
                    //         1
                    //     } else {
                    //         0
                    //     };

                    // let closure_offset = global_env.borrow().len();

                    // println!("!!!!!!!!!!!! Closure Offset: {}", closure_offset);
                    // println!("!!!!!!!!!!!! Closure Arity: {}", arity);

                    ByteCodeLambda::new(
                        closure_body,
                        Some(capture_env),
                        None,
                        closure_offset,
                        arity,
                        ndefs,
                    )
                } else {
                    // set the number of definitions for the environment
                    capture_env.borrow_mut().set_ndefs(ndefs);

                    // let closure_offset = global_env.borrow().len();

                    // if arity > 0 {
                    //     closure_offset += arity - 1;
                    // }

                    // let pndefs = capture_env.borrow().parent_ndefs();
                    // println!("Getting the parent n defs of {}", pndefs);

                    // if pndefs > 0 {
                    //     closure_offset += pndefs - 1;
                    // }

                    // println!("Setting the ndefs here!!!!!!!!");
                    // let closure_offset = arity
                    //     + if global_env.borrow().is_binding_context() {
                    //         0
                    //     } else {
                    //         0
                    //     };

                    // println!("!!!!!!!!!!!! Closure Offset: {}", closure_offset);
                    // println!("!!!!!!!!!!!! Closure Arity: {}", arity);

                    // println!("Inside the second one");

                    // TODO look at this heap thing
                    heap.push(Rc::clone(&capture_env));
                    ByteCodeLambda::new(
                        closure_body,
                        None,
                        Some(Rc::downgrade(&capture_env)),
                        closure_offset,
                        arity,
                        ndefs,
                    )
                };

                stack.push(Rc::new(SteelVal::Closure(constructed_lambda)));

                // println!("{:?}", stack);

                ip += forward_jump;
                // cur_inst = &instructions[ip];

                // let c = ByteCodeLambda::new(closure_body, )
            }
            OpCode::SDEF => {
                ip += 1;

                global_env.borrow_mut().set_binding_context(true);
                global_env.borrow_mut().set_binding_offset(false);

                let defn_body = &instructions[ip..(ip + cur_inst.payload_size - 1)];

                // println!("Instructions for def body: {:?}", defn_body);

                let mut temp_stack = Vec::new();
                let result = vm(
                    defn_body,
                    &mut temp_stack,
                    heap,
                    Rc::clone(&global_env),
                    constants,
                )?;

                stack.push(result);
                ip += cur_inst.payload_size;
                // cur_inst = &instructions[ip];
            }
            _ => {
                unimplemented!();
            }
        }
    }

    unimplemented!()
}

/// evaluates an atom expression in given environment
fn eval_atom(t: &SyntaxObject) -> Result<Rc<SteelVal>> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => {
            if *b {
                Ok(TRUE.with(|f| Rc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Rc::clone(f)))
            }
        }
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
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

/*
(+ 1 2 (+ 3 4 (+ 5 6)))

push 1
push 2
push 3
push 4
push 5
push 6
push (BUILT_IN_FUNCTION + (pop 2))
push (BUILT_IN_FUNCTION + (pop 3))
push BUILT_IN_FUNCTION + (pop 3)
END -> pop last result


(if (= 1 2) (+ 1 2 3) (+ 4 5 6))


push 1
push 2
push (BUILT_IN_FUNCTION) (pop 2)
IF - pop last
JMP payload: 1
JMP payload: 2

JMP - 1
push 1
push 2
push 3
push (BUILT_IN_FUNCTION) (pop 3)
END

JMP - 2
push 4
push 5
push 6
push (BUILT_IN_FUNCTION) (pop 3)
END
*/
