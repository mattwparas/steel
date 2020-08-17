mod arity;
mod constants;
mod expand;
mod map;

pub use arity::Arity;
pub use arity::ArityMap;
pub use constants::ConstantMap;
pub use constants::ConstantTable;
pub use expand::expand;
pub use expand::extract_macro_definitions;
use expand::get_definition_names;
pub use map::SymbolMap;

use expand::MacroSet;

// use expand::is_definition;

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

use std::convert::TryFrom;

use crate::parser::span::Span;

use std::time::Instant;

use crate::primitives::ListOperations;
use crate::primitives::VectorOperations;

use std::convert::TryInto;

use std::io::Read;

use std::path::Path;

use std::collections::HashSet;

// use std::collections::HashSet;

// use crate::expander::SteelMacro;
// use crate::structs::SteelStruct;

// fn recursive_expand(expr: Expr, )

// use crate::interpreter::evaluator::emit_instructions;

// pass define statement
// identify the handle for the function call
// traverse idenitifying function calls
// if the function call is in the tail position of any of the body, then transform that to be an explicit jump -> __JUMP__
// only need to check the last thing in the body
// pub fn identify_tail_call(expr: &Expr) {}

const STACK_LIMIT: usize = 1024;

pub fn transform_tail_call(instructions: &mut Vec<Instruction>, defining_context: &str) -> bool {
    // println!(
    //     "Calling transform tail call with function: {}",
    //     defining_context
    // );

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
                    // println!("Making optimization!");

                    let new_jmp = Instruction::new_jmp(0);
                    // inject tail call jump
                    instructions[index - 2] = new_jmp;
                    instructions[index - 1] = Instruction::new_pass();
                    transformed = true;
                }
                // else {
                //     println!("Found function call in tail position")
                // }
            }
            _ => {}
        }
    }

    return transformed;
}

// Note, this should be called AFTER `transform_tail_call`
fn check_and_transform_mutual_recursion(instructions: &mut [Instruction]) -> bool {
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
                            ty: TokenType::Identifier(_s),
                            ..
                        }),
                    ..
                }),
            ) => {
                if let Some(x) = instructions.get_mut(index - 1) {
                    x.op_code = OpCode::TAILCALL;
                    transformed = true;
                }
            }
            _ => {}
        }
    }

    transformed
}

// Hopefully this doesn't break anything...
// Definitions
fn count_and_collect_global_defines(
    exprs: &[Expr],
    symbol_map: &mut SymbolMap,
) -> (usize, usize, usize) {
    let mut new_count = 0;
    let mut old_count = 0;
    let mut non_defines = 0;
    for expr in exprs {
        match expr {
            Expr::Atom(_) => non_defines += 1,
            Expr::VectorVal(list_of_tokens) => {
                match (list_of_tokens.get(0), list_of_tokens.get(1)) {
                    (
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(def),
                            ..
                        })),
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(name),
                            ..
                        })),
                    ) => {
                        if def == "define" || def == "defn" {
                            // println!(
                            //     "Found definition in `count_and_collect_global_defines`: {}",
                            //     name
                            // );
                            let (_, added) = symbol_map.get_or_add(name.as_str());
                            // count += 1;
                            if added {
                                new_count += 1;
                            } else {
                                old_count += 1;
                            }
                        } else {
                            non_defines += 1;
                        }
                    }
                    (
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(def),
                            ..
                        })),
                        Some(Expr::VectorVal(_)),
                    ) => {
                        if def == "begin" {
                            // println!("Making a recursive call here...");
                            let (res_new, res_old, res_non) =
                                count_and_collect_global_defines(&list_of_tokens[1..], symbol_map);

                            // println!(
                            //     "%%%%%%%%%%%%%%%%%%% {}, {}, {} %%%%%%%%%%%%%%%%",
                            //     res_new, res_old, res_non
                            // );

                            new_count += res_new;
                            old_count += res_old;
                            non_defines += res_non;
                        } else {
                            non_defines += 1;
                        }
                    }
                    _ => {
                        non_defines += 1;
                    }
                }
            }
        }
    }

    (new_count, old_count, non_defines)
}

// insert fast path for built in functions
// rather than look up function in env, be able to call it directly?
pub fn collect_defines_from_current_scope(
    instructions: &[Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<usize> {
    let mut def_stack: usize = 0;
    let mut count = 0;
    let mut bindings: HashSet<&str> = HashSet::new();

    for i in 0..instructions.len() {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::SDEF,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span: sp,
                    }),
                ..
            } => {
                if def_stack == 0 {
                    if bindings.insert(s) {
                        let (_idx, _) = symbol_map.get_or_add(s);
                        count += 1;
                    // println!("####### FOUND DEFINE #########");
                    // println!("Renaming: {} to index: {}", s, idx);
                    // if let Some(x) = instructions.get_mut(i) {
                    //     x.contents = None;
                    // }
                    } else {
                        stop!(Generic => "define-values: duplicate binding name"; *sp)
                    }
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

    Ok(count)
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

                    // println!("Symbol Map -- 302: Renaming: {} to index: {}", s, idx);
                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                        x.constant = false;
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

pub fn insert_debruijn_indices(
    instructions: &mut [Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<()> {
    let mut stack: Vec<usize> = Vec::new();
    // let mut def_stack: Vec<usize> = Vec::new();

    // Snag the defines that are going to be available from the global scope
    let _ = collect_defines_from_current_scope(instructions, symbol_map)?;

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
                let idx = symbol_map.get(s).map_err(|x| {
                    let sp = if let Some(syn) = &instructions[i].contents {
                        syn.span
                    } else {
                        Span::new(0, 0)
                    };

                    x.set_span(sp)
                })?;
                // println!("Renaming: {} to index: {}", s, idx);
                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                    x.constant = false;
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
                let (idx, _) = symbol_map.get_or_add(s);

                // println!("Renaming: {} to index: {}", s, idx);
                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                    // x.contents = None;
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
                )?;
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
                    x.constant = false;
                }
            }
            _ => {}
        }
    }

    Ok(())
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

fn coalesce_clears(instructions: &mut Vec<Instruction>) {
    if instructions.len() < 2 {
        return;
    }
    for i in 0..instructions.len() - 2 {
        match (
            instructions.get(i),
            instructions.get(i + 1),
            instructions.get(i + 2),
        ) {
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CLEAR,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
            ) => {
                if let Some(x) = instructions.get_mut(i + 1) {
                    x.op_code = OpCode::PASS;
                }
            }
            (
                Some(Instruction {
                    op_code: OpCode::FUNC,
                    ..
                }),
                Some(Instruction {
                    op_code: OpCode::CLEAR,
                    ..
                }),
                _,
            ) => {}
            _ => {}
        }
    }
}

fn emit_loop<CT: ConstantTable>(
    expr: &Expr,
    instructions: &mut Vec<Instruction>,
    defining_context: Option<&str>,
    arity_map: &mut ArityMap,
    constant_map: &mut CT,
) -> Result<()> {
    match expr {
        Expr::Atom(s) => {
            instructions.push(Instruction::new(OpCode::PUSH, 0, s.clone(), true));
        }
        Expr::VectorVal(list_of_tokens) => {
            if let Some(f) = list_of_tokens.first() {
                match f.deref() {
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "quote" => {
                        check_length("quote", &list_of_tokens, 2)?;
                        let converted = SteelVal::try_from(list_of_tokens[1].clone())?;
                        let idx = constant_map.add_or_get(Rc::new(converted));
                        instructions.push(Instruction::new_push_const(idx));
                        // instructions.push(Instruction::new_quote());
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "eval" => {
                        check_length("eval", &list_of_tokens, 2)?;
                        instructions.push(Instruction::new_eval());
                        return Ok(());
                    }

                    // Expr::Atom(SyntaxObject {
                    //     ty: TokenType::Identifier(s),
                    //     ..
                    // }) if s == "declare" => {
                    //     // check_length("eval", &list_of_tokens, 2)?;
                    //     instructions.push(Instruction::new_eval());
                    //     return Ok(());
                    // }
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span: sp,
                    }) if s == "if" => {
                        if let [test_expr, then_expr, else_expr] = &list_of_tokens[1..] {
                            // load in the test condition
                            emit_loop(test_expr, instructions, None, arity_map, constant_map)?;
                            // push in If
                            instructions.push(Instruction::new_if(instructions.len() + 2));
                            // save spot of jump instruction, fill in after
                            let idx = instructions.len();
                            instructions.push(Instruction::new_jmp(0)); // dummy

                            // emit instructions for then expression
                            emit_loop(then_expr, instructions, None, arity_map, constant_map)?;
                            instructions.push(Instruction::new_jmp(0)); // dummy
                            let false_start = instructions.len();

                            // emit instructions for else expression
                            emit_loop(else_expr, instructions, None, arity_map, constant_map)?;
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
                        } else {
                            stop!(BadSyntax => "malformed if statement"; *sp);
                        }
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
                                    stop!(ArityMismatch => e; syn.span)
                                }

                                emit_loop(
                                    &list_of_tokens[2],
                                    instructions,
                                    defining_context,
                                    arity_map,
                                    constant_map,
                                )?;

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
                            Expr::VectorVal(_) => {
                                panic!("Complex defines not yet supported");
                            }
                        }
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
                                        // println!("{:?}", syn);
                                        match &syn {
                                            SyntaxObject {
                                                ty: TokenType::Identifier(_),
                                                ..
                                            } => body_instructions
                                                .push(Instruction::new_bind(syn.clone())),
                                            SyntaxObject { ty: _, span: sp } => {
                                                stop!(Generic => "lambda function requires list of identifiers"; *sp);
                                            }
                                        }
                                    } else {
                                        stop!(Generic => "lambda function requires list of identifiers"; symbol.span());
                                    }
                                }
                            }
                            _ => {
                                stop!(TypeMismatch => "List of Identifiers"; list_of_symbols.span())
                            }
                        }

                        // let mut body_instructions = Vec::new();

                        for expr in &list_of_tokens[2..] {
                            emit_loop(expr, &mut body_instructions, None, arity_map, constant_map)?;
                        }

                        // TODO look out here for the
                        body_instructions.push(Instruction::new_pop());
                        // body_instructions.push(Instruction::new_clear());

                        if let Some(ctx) = defining_context {
                            transform_tail_call(&mut body_instructions, ctx);
                            let _b = check_and_transform_mutual_recursion(&mut body_instructions);
                            // if b {
                            //     println!("Transformed mutual recursion!");
                            // }
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
                            emit_loop(expr, instructions, None, arity_map, constant_map)?;
                        }
                        return Ok(());
                    }
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "return" => {
                        check_length("return", &list_of_tokens, 2)?;
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        // pop is equivalent to the last instruction in the function
                        instructions.push(Instruction::new_pop());
                        return Ok(());
                    }
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "panic!" => {
                        check_length("panic!", &list_of_tokens, 2)?;
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;

                        // pop is equivalent to the last instruction in the function
                        instructions.push(Instruction::new_panic(
                            if let Expr::Atom(s) = &list_of_tokens[0] {
                                s.clone()
                            } else {
                                SyntaxObject::default(TokenType::Identifier("panic!".to_string()))
                            },
                        ));
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
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "map'" => {
                        // check_length("map'", tokens, expected)
                        check_length("map'", &list_of_tokens, 3)?;
                        // emit_loop(expr, instructions, defining_context, arity_map, constant_map)
                        // Load in the function
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        // Load in the list
                        emit_loop(
                            &list_of_tokens[2],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        // instructions
                        // emit_loop(expr, instructions, defining_context, arity_map, constant_map)
                        instructions.push(Instruction::new_map());
                        return Ok(());
                    }
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "filter'" => {
                        // check_length("map'", tokens, expected)
                        check_length("filter'", &list_of_tokens, 3)?;
                        // emit_loop(expr, instructions, defining_context, arity_map, constant_map)
                        // Load in the function
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        // Load in the list
                        emit_loop(
                            &list_of_tokens[2],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        // instructions
                        // emit_loop(expr, instructions, defining_context, arity_map, constant_map)
                        instructions.push(Instruction::new_filter());
                        return Ok(());
                    }
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
                            emit_loop(expr, instructions, None, arity_map, constant_map)?;
                        }

                        emit_loop(f, instructions, None, arity_map, constant_map)?;

                        if let Expr::Atom(s) = &list_of_tokens[0] {
                            instructions.push(Instruction::new_func(pop_len, s.clone()));
                        } else {
                            instructions.push(Instruction::new_func(
                                pop_len,
                                SyntaxObject::default(TokenType::Identifier("lambda".to_string())),
                            ));
                            instructions.push(Instruction::new_clear());
                        }

                        // TODO fix this noise
                        // instructions.push(Instruction::new_func(
                        //     pop_len,
                        //     if let Expr::Atom(s) = &list_of_tokens[0] {
                        //         s.clone()
                        //     } else {
                        //         SyntaxObject::default(TokenType::Identifier("lambda".to_string()))
                        //     },
                        // ));

                        // instructions.push(Instruction::new_clear());

                        return Ok(());
                    }
                }
            } else {
                stop!(TypeMismatch => "Given empty list"; expr.span())
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
    EVAL = 16,
    PANIC = 17,
    CLEAR = 18,
    TAILCALL = 19,
    MAP = 20,
    FILTER = 21,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DenseInstruction {
    op_code: OpCode,
    payload_size: usize,
    span: Span,
}

impl DenseInstruction {
    pub fn new(op_code: OpCode, payload_size: usize, span: Span) -> DenseInstruction {
        DenseInstruction {
            op_code,
            payload_size,
            span,
        }
    }
}

pub struct ProfilingInformation {
    counts: HashMap<FunctionCallCtx, usize>,
    threshold: usize,
}

/*

Do some magic with the instructions to generate the profile table

Something like a vector -> hashmaps of rooted function contexts

Can run some profile information that way to understand the counts

Once the threshold passes a certain amount, modify the instructions to
go ahead and change the function to call a more specialized one?




*/

impl ProfilingInformation {
    pub fn new() -> Self {
        ProfilingInformation {
            counts: HashMap::new(),
            threshold: 20,
        }
    }

    // Check if this function was considered already for the JIT
    // add to the profiling information
    pub fn add_or_increment(&mut self, ctx: FunctionCallCtx) -> bool {
        // let ctx = FunctionCallCtx::new()
        let mut t = false;
        if let Some(x) = self.counts.get_mut(&ctx) {
            if *x >= self.threshold {
                t = true;
            }
            *x += 1;
        } else {
            self.counts.insert(ctx, 0);
        }

        t
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct FunctionCallCtx {
    // Rooted functions are assigned an index
    // via the symbol map
    pub(crate) function_id: usize,
    pub(crate) instruction_id: usize,
    // pub()
}

impl FunctionCallCtx {
    pub fn new(function_id: usize, instruction_id: usize) -> Self {
        FunctionCallCtx {
            function_id,
            instruction_id,
        }
    }
}

impl From<Instruction> for DenseInstruction {
    fn from(val: Instruction) -> DenseInstruction {
        DenseInstruction::new(
            val.op_code,
            val.payload_size.try_into().unwrap(),
            if let Some(syn) = val.contents {
                syn.span
            } else {
                Span::new(0, 0)
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    op_code: OpCode,
    payload_size: usize,
    contents: Option<SyntaxObject>,
    constant: bool,
}

impl Instruction {
    pub fn new(
        op_code: OpCode,
        payload_size: usize,
        contents: SyntaxObject,
        constant: bool,
    ) -> Instruction {
        Instruction {
            op_code,
            payload_size,
            contents: Some(contents),
            constant,
        }
    }

    pub fn new_map() -> Instruction {
        Instruction {
            op_code: OpCode::MAP,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_filter() -> Instruction {
        Instruction {
            op_code: OpCode::FILTER,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_panic(span: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::PANIC,
            payload_size: 0,
            contents: Some(span),
            constant: false,
        }
    }

    pub fn new_clear() -> Instruction {
        Instruction {
            op_code: OpCode::CLEAR,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_push_const(idx: usize) -> Instruction {
        Instruction {
            op_code: OpCode::PUSHCONST,
            payload_size: idx,
            contents: None,
            constant: true,
        }
    }

    pub fn new_eval() -> Instruction {
        Instruction {
            op_code: OpCode::EVAL,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_ndef(payload_size: usize) -> Instruction {
        Instruction {
            op_code: OpCode::NDEFS,
            payload_size,
            contents: None,
            constant: false,
        }
    }

    pub fn new_func(arity: usize, contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::FUNC,
            payload_size: arity,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_pop() -> Instruction {
        Instruction {
            op_code: OpCode::POP,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_if(true_jump: usize) -> Instruction {
        Instruction {
            op_code: OpCode::IF,
            payload_size: true_jump,
            contents: None,
            constant: false,
        }
    }

    pub fn new_jmp(jump: usize) -> Instruction {
        Instruction {
            op_code: OpCode::JMP,
            payload_size: jump,
            contents: None,
            constant: false,
        }
    }

    pub fn new_sclosure() -> Instruction {
        Instruction {
            op_code: OpCode::SCLOSURE,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_eclosure(arity: usize) -> Instruction {
        Instruction {
            op_code: OpCode::ECLOSURE,
            payload_size: arity,
            contents: None,
            constant: false,
        }
    }

    pub fn new_bind(contents: SyntaxObject) -> Instruction {
        Instruction {
            op_code: OpCode::BIND,
            payload_size: 0,
            contents: Some(contents),
            constant: false,
        }
    }

    pub fn new_sdef() -> Instruction {
        Instruction {
            op_code: OpCode::SDEF,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_edef() -> Instruction {
        Instruction {
            op_code: OpCode::EDEF,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_void() -> Instruction {
        Instruction {
            op_code: OpCode::VOID,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }

    pub fn new_pass() -> Instruction {
        Instruction {
            op_code: OpCode::PASS,
            payload_size: 0,
            contents: None,
            constant: false,
        }
    }
}

pub struct Ctx<CT: ConstantTable> {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: CT,
    pub(crate) arity_map: ArityMap,
    pub(crate) repl: bool,
}

impl<CT: ConstantTable> Ctx<CT> {
    pub fn new(
        symbol_map: SymbolMap,
        constant_map: CT,
        arity_map: ArityMap,
        repl: bool,
    ) -> Ctx<CT> {
        Ctx {
            symbol_map,
            constant_map,
            arity_map,
            repl,
        }
    }

    pub fn default() -> Ctx<ConstantMap> {
        Ctx::new(
            Env::default_symbol_map(),
            ConstantMap::new(),
            ArityMap::new(),
            false,
        )
    }

    pub fn constant_map(&self) -> &CT {
        &self.constant_map
    }

    pub fn roll_back(&mut self, idx: usize) {
        // unimplemented!()
        self.symbol_map.roll_back(idx);
        self.constant_map.roll_back(idx);
        self.arity_map.roll_back(idx);
    }
}

pub struct VirtualMachine {
    global_env: Rc<RefCell<Env>>,
    global_heap: Vec<Rc<RefCell<Env>>>,
    macro_env: Rc<RefCell<Env>>,
    idents: MacroSet,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            global_heap: Vec::new(),
            macro_env: Rc::new(RefCell::new(Env::root())),
            idents: MacroSet::new(),
        }
    }

    pub fn roll_back(&mut self, idx: usize) {
        unimplemented!()
    }

    // Read in the file from the given path and execute accordingly
    // Loads all the functions in from the given env
    pub fn parse_and_execute_from_path<CT: ConstantTable, P: AsRef<Path>>(
        &mut self,
        path: P,
        ctx: &mut Ctx<CT>,
    ) -> Result<Vec<Rc<SteelVal>>> {
        let mut file = std::fs::File::open(path)?;
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;
        self.parse_and_execute(exprs.as_str(), ctx)
    }

    // pub fn new_with_std

    pub fn parse_and_execute<CT: ConstantTable>(
        &mut self,
        expr_str: &str,
        ctx: &mut Ctx<CT>,
    ) -> Result<Vec<Rc<SteelVal>>> {
        // let now = Instant::now();
        let gen_bytecode = self.emit_instructions(expr_str, ctx)?;

        // previous size of the env
        // let length = self.global_env.borrow().len();

        // println!("Bytecode generated in: {:?}", now.elapsed());
        gen_bytecode
            .into_iter()
            .map(|x| {
                let code = Rc::new(x.into_boxed_slice());
                let now = Instant::now();
                let res = self.execute(code, &ctx.constant_map, ctx.repl);
                println!("Time taken: {:?}", now.elapsed());
                res
            })
            .collect::<Result<Vec<Rc<SteelVal>>>>()

        // .and_then(|x| {
        //     // self.global_env.borrow_mut().pop_last();
        //     let new_length = self.global_env.borrow().len();
        //     println!("New length: {}, Old length: {}", new_length, length);
        //     // if new_length - length > 1 {
        //     //     self.global_env.borrow_mut().pop_last();
        //     // }
        //     Ok(x)
        // })
    }

    pub fn emit_instructions<CT: ConstantTable>(
        &mut self,
        expr_str: &str,
        ctx: &mut Ctx<CT>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut intern = HashMap::new();
        let mut results = Vec::new();

        // Parse the input
        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut intern).collect();
        let parsed = parsed?;

        // populate MacroSet
        self.idents.insert_from_iter(
            get_definition_names(&parsed)
                .into_iter()
                .chain(ctx.symbol_map.copy_underlying_vec().into_iter()),
        );

        // Yoink the macro definitions
        // Add them to our macro env
        // TODO change this to be a unique macro env struct
        // Just a thin wrapper around a hashmap
        let extracted_statements = extract_macro_definitions(
            &parsed,
            &self.macro_env,
            &self.global_env,
            &mut ctx.symbol_map,
            &self.idents,
        )?;

        // Walk through and expand all macros, lets, and defines
        let expanded_statements: Vec<Expr> = extracted_statements
            .into_iter()
            .map(|x| expand(x, &self.global_env, &self.macro_env))
            .collect::<Result<Vec<Expr>>>()?;

        // println!()

        // Collect global defines here first
        let (ndefs_new, ndefs_old, _not) =
            count_and_collect_global_defines(&expanded_statements, &mut ctx.symbol_map);

        // At the global level, let the defines shadow the old ones, but call `drop` on all of the old values

        // Reserve the definitions in the global environment
        // println!("########### Found ndefs new: {}", ndefs_new);
        // println!("########### Found ndefs shadowed: {}", ndefs_old);
        // println!("########### not defines: {}", _not);

        // println!(
        //     "^^^^^^^^^^ Global env length at the start: {}",
        //     self.global_env.borrow().len()
        // );
        // println!(
        //     "Global env state binding context: {}",
        //     self.global_env.borrow().is_binding_context()
        // );
        // println!(
        //     "Global env state binding offset: {}",
        //     self.global_env.borrow().is_binding_offset()
        // );

        // Reserve the definitions in the global environment
        // TODO find a better way to make sure that the definitions are reserved
        // This works for the normal bytecode execution without the repl
        self.global_env
            .borrow_mut()
            .reserve_defs(if ndefs_new > 0 { ndefs_new - 1 } else { 0 }); // used to be ndefs - 1

        match (ndefs_old, ndefs_new) {
            (_, _) if ndefs_old > 0 && ndefs_new == 0 => {
                println!("CASE 1: Popping last!!!!!!!!!");
                self.global_env.borrow_mut().pop_last();
            }
            (_, _) if ndefs_new > 0 && ndefs_old == 0 => {
                // println!("Doing nothing");
            }
            (_, _) if ndefs_new > 0 && ndefs_old > 0 => {
                // println!("$$$$$$$$$$ GOT HERE $$$$$$$$");
                self.global_env.borrow_mut().pop_last();
            }
            (_, _) => {}
        }

        // HACK - make the global definitions line up correctly
        // This is basically a repl only feature
        // if ndefs_old > 0 && ndefs_new == 0 {
        //     // Getting here
        //     self.global_env.borrow_mut().pop_last();
        // }

        // if ndefs_old > ndefs_new && ndefs_new == 0 {
        //     self.global_env.borrow_mut().pop_last();
        // }

        // println!(
        //     "^^^^^^^^^^ Global env length after reserving defs: {}",
        //     self.global_env.borrow().len()
        // );

        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();
        for expr in expanded_statements {
            // println!("{:?}", expr.to_string());
            let mut instructions: Vec<Instruction> = Vec::new();
            emit_loop(
                &expr,
                &mut instructions,
                None,
                &mut ctx.arity_map,
                &mut ctx.constant_map,
            )?;
            // if !script {
            // instructions.push(Instruction::new_clear());
            instructions.push(Instruction::new_pop());
            // }
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        // println!("Got here!");

        insert_debruijn_indices(&mut instruction_buffer, &mut ctx.symbol_map)?;
        extract_constants(&mut instruction_buffer, &mut ctx.constant_map)?;
        coalesce_clears(&mut instruction_buffer);

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            // pretty_print_instructions(extracted.as_slice());
            results.push(densify(extracted));
        }

        Ok(results)
    }

    pub fn execute<CT: ConstantTable>(
        &mut self,
        instructions: Rc<Box<[DenseInstruction]>>,
        constants: &CT,
        repl: bool,
    ) -> Result<Rc<SteelVal>> {
        // execute_vm(instructions)
        let stack: Vec<Rc<SteelVal>> = Vec::new();
        let mut heap: Vec<Rc<RefCell<Env>>> = Vec::new();
        // let mut constants: Vec<Rc<RefCell<Env>>

        // let global_env = Rc::new(RefCell::new(Env::default_env()));
        let result = vm(
            instructions,
            stack,
            &mut heap,
            Rc::clone(&self.global_env),
            constants,
            repl,
        );

        if self.global_env.borrow().is_binding_context() {
            self.global_heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
        }

        // Maybe?????
        // self.global_env.borrow_mut().pop_last();

        // self.global_env.borrow_mut().set_binding_offset(false);

        // println!("Global heap length after: {}", self.global_heap.len());

        result
    }
}

pub fn execute_vm(
    instructions: Rc<Box<[DenseInstruction]>>,
    constants: &ConstantMap,
) -> Result<Rc<SteelVal>> {
    let stack: Vec<Rc<SteelVal>> = Vec::new();
    let mut heap: Vec<Rc<RefCell<Env>>> = Vec::new();
    // let mut constants: Vec<Rc<SteelVal>> = Vec::new();
    let global_env = Rc::new(RefCell::new(Env::default_env()));
    vm(instructions, stack, &mut heap, global_env, constants, false)
}

// TODO make this not so garbage but its kind of okay
pub fn extract_constants<CT: ConstantTable>(
    instructions: &mut [Instruction],
    constants: &mut CT,
) -> Result<()> {
    for i in 0..instructions.len() {
        let inst = &instructions[i];
        if let OpCode::PUSH = inst.op_code {
            // let idx = constants.len();
            if inst.constant {
                let value = eval_atom(&inst.contents.as_ref().unwrap())?;
                let idx = constants.add_or_get(value);
                // constants.push(eval_atom(&inst.contents.as_ref().unwrap())?);
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

fn inspect_heap(heap: &Vec<Rc<RefCell<Env>>>) {
    let hp: Vec<String> = heap
        .into_iter()
        .map(|x| x.borrow().string_bindings_vec())
        .collect();
    println!("{:?}", hp);
}

fn switch_filter_map<'global, CT: ConstantTable>(
    stack_func: Rc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
) {
    unimplemented!()
}

fn inline_map_iter<
    'global,
    I: Iterator<Item = Rc<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Rc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> impl Iterator<Item = Result<Rc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg];
            // if let Some()

            if let Some(parent_env) = closure.sub_expression_env() {
                // TODO remove this unwrap
                let offset =
                    closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

                let mut local_heap = Vec::new();

                // TODO make recursive call here with a very small stack
                // probably a bit overkill, but not much else I can do here I think
                vm(
                    closure.body_exp(),
                    args,
                    &mut local_heap,
                    inner_env,
                    constants,
                    repl,
                )
            } else {
                stop!(Generic => "Something went wrong with map");
            }
        }
        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

fn inline_filter_iter<
    'global,
    I: Iterator<Item = Rc<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Rc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> impl Iterator<Item = Result<Rc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![Rc::clone(&arg)];
            let res = func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span));
            match res {
                Ok(k) => match k.as_ref() {
                    SteelVal::BoolV(true) => Some(Ok(arg)),
                    SteelVal::BoolV(false) => None,
                    _ => None,
                },
                Err(e) => Some(Err(e)),
                // _ => None,
            }
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![Rc::clone(&arg)];
            let res = func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span));
            match res {
                Ok(k) => match k.as_ref() {
                    SteelVal::BoolV(true) => Some(Ok(arg)),
                    SteelVal::BoolV(false) => None,
                    _ => None,
                },
                Err(e) => Some(Err(e)),
            }
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![Rc::clone(&arg)];
            // if let Some()

            if let Some(parent_env) = closure.sub_expression_env() {
                // TODO remove this unwrap
                let offset =
                    closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

                let mut local_heap = Vec::new();

                // TODO make recursive call here with a very small stack
                // probably a bit overkill, but not much else I can do here I think
                let res = vm(
                    closure.body_exp(),
                    args,
                    &mut local_heap,
                    inner_env,
                    constants,
                    repl,
                );

                match res {
                    Ok(k) => match k.as_ref() {
                        SteelVal::BoolV(true) => Some(Ok(arg)),
                        SteelVal::BoolV(false) => None,
                        _ => None,
                    },
                    Err(e) => Some(Err(e)),
                }

            // if let SteelVal::BoolV(true) = res {
            //     Some(Ok(true))
            // } else {
            //     None
            // }
            } else {
                Some(Err(SteelErr::Generic(
                    "Something went wrong with map - internal error".to_string(),
                    Some(*cur_inst_span),
                )))
            }
        }
        _ => Some(Err(SteelErr::TypeMismatch(
            "map expected a function".to_string(),
            Some(*cur_inst_span),
        ))),
    };

    iter.filter_map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

fn inline_map_normal<I: Iterator<Item = Rc<SteelVal>>, CT: ConstantTable>(
    iter: I,
    stack_func: Rc<SteelVal>,
    constants: &CT,
    cur_inst: &DenseInstruction,
    repl: bool,
) -> Result<Vec<Rc<SteelVal>>> {
    // unimplemented!();

    let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    let switch_statement = |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg];
            func(&arg_vec).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg];
            func(arg_vec, factory).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg];
            // if let Some()

            if let Some(parent_env) = closure.sub_expression_env() {
                // TODO remove this unwrap
                let offset =
                    closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

                let mut local_heap = Vec::new();

                // TODO make recursive call here with a very small stack
                // probably a bit overkill, but not much else I can do here I think
                vm(
                    closure.body_exp(),
                    args,
                    &mut local_heap,
                    inner_env,
                    constants,
                    repl,
                )
            } else {
                stop!(Generic => "Something went wrong with map");
            }
        }
        _ => stop!(TypeMismatch => "map expected a function"; cur_inst.span),
    };

    for val in iter {
        collected_results.push(switch_statement(val)?);
    }

    Ok(collected_results)
}

fn inline_filter_normal<I: Iterator<Item = Rc<SteelVal>>, CT: ConstantTable>(
    iter: I,
    stack_func: Rc<SteelVal>,
    constants: &CT,
    cur_inst: &DenseInstruction,
    repl: bool,
) -> Result<Vec<Rc<SteelVal>>> {
    // unimplemented!();

    let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    let switch_statement = |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg];
            func(&arg_vec).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg];
            func(arg_vec, factory).map_err(|x| x.set_span(cur_inst.span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg];
            // if let Some()

            if let Some(parent_env) = closure.sub_expression_env() {
                // TODO remove this unwrap
                let offset =
                    closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

                let mut local_heap = Vec::new();

                // TODO make recursive call here with a very small stack
                // probably a bit overkill, but not much else I can do here I think
                vm(
                    closure.body_exp(),
                    args,
                    &mut local_heap,
                    inner_env,
                    constants,
                    repl,
                )
            } else {
                stop!(Generic => "Something went wrong with map");
            }
        }
        _ => stop!(TypeMismatch => "map expected a function"; cur_inst.span),
    };

    for val in iter {
        let res = switch_statement(Rc::clone(&val))?;
        if let SteelVal::BoolV(true) = res.as_ref() {
            collected_results.push(val);
        }
    }

    Ok(collected_results)
}

// pub struct FunctionDiagnostics {

// }

pub fn vm<CT: ConstantTable>(
    instructions: Rc<Box<[DenseInstruction]>>,
    stack: Vec<Rc<SteelVal>>,
    heap: &mut Vec<Rc<RefCell<Env>>>,
    global_env: Rc<RefCell<Env>>,
    constants: &CT,
    repl: bool,
) -> Result<Rc<SteelVal>> {
    let mut ip = 0;
    let mut global_env = global_env;

    if instructions.is_empty() {
        stop!(Generic => "empty stack!");
    }

    // instruction stack for function calls
    let mut instruction_stack: Vec<(usize, Rc<Box<[DenseInstruction]>>)> = Vec::new();
    // stacks on stacks baby
    let mut stacks: Vec<Vec<Rc<SteelVal>>> = Vec::new();
    // initialize the instruction number pointer
    let mut cur_inst;
    // Pointer to array of instructions
    let mut instructions = instructions;
    // Self explanatory
    let mut stack = stack;
    // Manage current env in its own stack
    let mut env_stack: Vec<Rc<RefCell<Env>>> = Vec::new();
    // Manage the depth of instructions to know when to backtrack
    let mut pop_count = 1;

    while ip < instructions.len() {
        cur_inst = &instructions[ip];

        match cur_inst.op_code {
            OpCode::PANIC => {
                let error_message = stack.pop().unwrap();
                stop!(Generic => error_message.to_string(); cur_inst.span);
            }
            OpCode::EVAL => {
                panic!("eval not yet supported");
            }
            OpCode::PASS => {
                ip += 1;
            }
            OpCode::VOID => {
                stack.push(VOID.with(|f| Rc::clone(f)));
                ip += 1;
            }
            OpCode::PUSHCONST => {
                let val = constants.get(cur_inst.payload_size);
                stack.push(val);
                ip += 1;
            }
            OpCode::PUSH => {
                if repl {
                    let value = global_env.borrow().repl_lookup_idx(cur_inst.payload_size)?;
                    stack.push(value);
                } else {
                    let value = global_env.borrow().lookup_idx(cur_inst.payload_size)?;
                    stack.push(value);
                }
                ip += 1;
            }
            OpCode::CLEAR => {
                // println!("%%%%%%%%%%% Hitting clear! %%%%%%%%%%%");
                // println!("length of heap at clear: {}", heap.len());
                // println!("Heap at clear:");
                // inspect_heap(&heap);
                heap.clear();
                ip += 1;
            }
            OpCode::MAP => {
                let list = stack.pop().unwrap();
                let stack_func = stack.pop().unwrap();

                match list.as_ref() {
                    SteelVal::Pair(_, _) => {
                        let collected_results = inline_map_normal(
                            SteelVal::iter(list),
                            stack_func,
                            constants,
                            &cur_inst,
                            repl,
                        )?;

                        // stack.push(ListOperations::built_in_list_normal_iter(inline_map_iter(
                        //     SteelVal::iter(list),
                        //     stack_func,
                        //     constants,
                        //     &cur_inst.span,
                        // ))?);

                        stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                        // stack.push(ListOperation::built_in_list_func()(inline_map
                    }
                    SteelVal::VectorV(v) => {
                        // TODO get rid of the clone here
                        // let collected_results = inline_map(
                        //     v.into_iter().map(Rc::clone),
                        //     stack_func,
                        //     constants,
                        //     cur_inst,
                        // )?;
                        // stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                        // unimplemented!();
                        stack.push(VectorOperations::vec_construct_iter(inline_map_iter(
                            v.into_iter().map(Rc::clone),
                            stack_func,
                            constants,
                            &cur_inst.span,
                            repl,
                        ))?);
                    }
                    _ => stop!(TypeMismatch => "map expected a list"; cur_inst.span),
                }

                ip += 1;
            }
            OpCode::FILTER => {
                let list = stack.pop().unwrap();
                let stack_func = stack.pop().unwrap();

                // Change inline_map and inline_filter to return iterators... now that would be cool
                match list.as_ref() {
                    SteelVal::Pair(_, _) => {
                        let collected_results = inline_filter_normal(
                            SteelVal::iter(list),
                            stack_func,
                            constants,
                            cur_inst,
                            repl,
                        )?;
                        stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                        // stack.push(ListOperation::built_in_list_func()(inline_map

                        // stack.push(ListOperations::built_in_list_normal_iter(
                        //     inline_filter_iter(
                        //         SteelVal::iter(list),
                        //         stack_func,
                        //         constants,
                        //         &cur_inst.span,
                        //     ),
                        // )?);
                    }
                    SteelVal::VectorV(v) => {
                        // TODO get rid of the clone here
                        // let collected_results = inline_filter(
                        //     v.into_iter().map(Rc::clone),
                        //     stack_func,
                        //     constants,
                        //     cur_inst,
                        // )?;
                        // stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                        // unimplemented!();

                        stack.push(VectorOperations::vec_construct_iter(inline_filter_iter(
                            v.into_iter().map(Rc::clone),
                            stack_func,
                            constants,
                            &cur_inst.span,
                            repl,
                        ))?);
                    }
                    _ => stop!(TypeMismatch => "map expected a list"; cur_inst.span),
                }

                ip += 1;
            }
            OpCode::FUNC => {
                let stack_func = stack.pop().unwrap();

                // inspect_heap(&heap);

                match stack_func.as_ref() {
                    SteelVal::StructClosureV(factory, func) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        // let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        // let args = &stack[stack.len()]

                        // let args = &stack[stack.len() - cur_inst.payload_size..];

                        // stack.pop();

                        let result = f(&stack[stack.len() - cur_inst.payload_size..])
                            .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size);

                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::Closure(closure) => {
                        if stacks.len() == STACK_LIMIT {
                            println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        let args = stack.split_off(stack.len() - cur_inst.payload_size);

                        // TODO fix this
                        if let Some(parent_env) = closure.parent_env() {
                            let offset = closure.offset() + parent_env.borrow().local_offset();

                            // let parent_env = lambda.parent_env();
                            let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, offset)));

                            inner_env
                                .borrow_mut()
                                .reserve_defs(if closure.ndef_body() > 0 {
                                    closure.ndef_body() - 1
                                } else {
                                    0
                                });

                            // let params_exp = lambda.params_exp();
                            // let result =
                            // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;

                            // println!("Found a closure");
                            // instead of the recursive call, update the values and go back through the loop...
                            // closure_stack.push(Rc::clone(&stack_func));
                            env_stack.push(global_env);
                            // println!("Env stack size after pushing up top: {}", env_stack.len());
                            // println!("Env stack:");
                            // inspect_heap(&env_stack);
                            global_env = inner_env;
                            instruction_stack.push((ip + 1, instructions));
                            pop_count += 1;
                            stacks.push(stack);
                            instructions = closure.body_exp();
                            stack = args;
                            ip = 0;

                        // stack.push(result);

                        // evaluate(&lambda.body_exp(), &inner_env)
                        } else if let Some(parent_env) = closure.sub_expression_env() {
                            // TODO remove this unwrap
                            let offset = closure.offset()
                                + parent_env.upgrade().unwrap().borrow().local_offset();

                            // println!("Setting closure offset to be: {}", offset);

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

                            // let result =
                            // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                            // closure_stack.push(Rc::clone(&stack_func));
                            // TODO this is where the memory leak is
                            env_stack.push(global_env);
                            // println!("Env stack size after pushing below: {}", env_stack.len());
                            // println!("Env stack:");
                            // inspect_heap(&env_stack);
                            global_env = inner_env;
                            instruction_stack.push((ip + 1, instructions));
                            pop_count += 1;
                            stacks.push(stack);
                            instructions = closure.body_exp();
                            stack = args;
                            ip = 0;
                        } else {
                            stop!(Generic => "Root env is missing!")
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported"; cur_inst.span);
                    }
                }
            }
            // Tail call basically says "hey this function is exiting"
            // In the closure case, transfer ownership of the stack to the called function
            OpCode::TAILCALL => {
                let stack_func = stack.pop().unwrap();

                match stack_func.as_ref() {
                    SteelVal::StructClosureV(factory, func) => {
                        let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        let result = func(args, factory).map_err(|x| x.set_span(cur_inst.span))?;
                        stack.push(result);
                        ip += 1;
                    }
                    SteelVal::FuncV(f) => {
                        // let args = stack.split_off(stack.len() - cur_inst.payload_size);
                        // let args = &stack[stack.len() - cur_inst.payload_size..];

                        let result = f(&stack[stack.len() - cur_inst.payload_size..])
                            .map_err(|x| x.set_span(cur_inst.span))?;

                        stack.truncate(stack.len() - cur_inst.payload_size);

                        stack.push(result);

                        // println!("{:?}")

                        ip += 1;
                    }
                    SteelVal::Closure(closure) => {
                        if stacks.len() == STACK_LIMIT {
                            println!("stacks at exit: {:?}", stacks);
                            println!("stack frame at exit: {:?}", stack);
                            stop!(Generic => "stack overflowed!"; cur_inst.span);
                        }

                        let args = stack.split_off(stack.len() - cur_inst.payload_size);

                        // TODO fix this
                        if let Some(_parent_env) = closure.parent_env() {
                            unreachable!();

                        // let offset = closure.offset() + parent_env.borrow().local_offset();

                        // // let parent_env = lambda.parent_env();
                        // let inner_env = Rc::new(RefCell::new(Env::new(&parent_env, offset)));

                        // inner_env
                        //     .borrow_mut()
                        //     .reserve_defs(if closure.ndef_body() > 0 {
                        //         closure.ndef_body() - 1
                        //     } else {
                        //         0
                        //     });

                        // // let params_exp = lambda.params_exp();
                        // // let result =
                        // // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;

                        // // println!("Found a closure");
                        // // instead of the recursive call, update the values and go back through the loop...
                        // // closure_stack.push(Rc::clone(&stack_func));
                        // env_stack.push(global_env);
                        // // println!("Env stack size after pushing up top: {}", env_stack.len());
                        // // println!("Env stack:");
                        // // inspect_heap(&env_stack);
                        // global_env = inner_env;
                        // instruction_stack.push((ip + 1, instructions));
                        // pop_count += 1;
                        // stacks.push(stack);
                        // instructions = closure.body_exp();
                        // stack = args;
                        // ip = 0;

                        // stack.push(result);

                        // evaluate(&lambda.body_exp(), &inner_env)
                        } else if let Some(parent_env) = closure.sub_expression_env() {
                            // TODO remove this unwrap
                            let offset = closure.offset()
                                + parent_env.upgrade().unwrap().borrow().local_offset();

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

                            // env_stack.push(global_env);
                            // tail_call_env_stack
                            global_env = inner_env;
                            instructions = closure.body_exp();
                            stack = args;
                            ip = 0;
                        } else {
                            stop!(Generic => "Root env is missing!")
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported"; cur_inst.span);
                    }
                }
            }
            OpCode::IF => {
                if let SteelVal::BoolV(true) = stack.pop().unwrap().as_ref() {
                    ip = cur_inst.payload_size; // Jump to payload
                                                // ip += 2; // Jump to payload
                                                // cur_inst = &instructions[ip];
                } else {
                    ip += 1;
                }
            }
            OpCode::JMP => {
                ip = cur_inst.payload_size;
            }
            OpCode::POP => {
                pop_count -= 1;
                if pop_count == 0 {
                    env_stack.clear();
                    heap.clear();
                    // Maybe
                    // global_env.borrow_mut().set_binding_context(false);
                    global_env.borrow_mut().set_binding_offset(false);

                    return stack.pop().ok_or_else(|| {
                        SteelErr::Generic("stack empty at pop".to_string(), Some(cur_inst.span))
                    });
                } else {
                    let ret_val = stack.pop().unwrap();
                    let prev_state = instruction_stack.pop().unwrap();

                    if prev_state.1.len() != 0 {
                        global_env = env_stack.pop().unwrap();
                        ip = prev_state.0;
                        instructions = prev_state.1;
                    } else {
                        ip += 1;
                    }

                    stack = stacks.pop().unwrap();
                    stack.push(ret_val);
                }
            }
            OpCode::BIND => {
                let offset = global_env.borrow().local_offset();

                // println!(
                //     "Binding: payload size: {}, offset: {}",
                //     cur_inst.payload_size, offset
                // );

                if repl {
                    global_env
                        .borrow_mut()
                        .repl_define_idx(cur_inst.payload_size, stack.pop().unwrap());
                } else {
                    global_env
                        .borrow_mut()
                        .define_idx(cur_inst.payload_size - offset, stack.pop().unwrap());
                }

                // println!(
                //     "Global eng length after binding: {}",
                //     global_env.borrow().len()
                // );

                ip += 1;
            }
            OpCode::SCLOSURE => {
                ip += 1;
                let forward_jump = cur_inst.payload_size - 1;
                // Snag the number of definitions here
                let ndefs = instructions[ip].payload_size;
                ip += 1;
                // Construct the closure body using the offsets from the payload
                // used to be - 1, now - 2
                let closure_body = instructions[ip..(ip + forward_jump - 1)].to_vec();

                // snag the arity from the eclosure instruction
                let arity = instructions[ip + forward_jump - 1].payload_size;

                let capture_env = Rc::clone(&global_env);

                let mut closure_offset = global_env.borrow().len();
                // println!("%%%%%%%%%%% Env length: {} %%%%%%%%%%%", closure_offset);

                // println!("{:?}", global_env.borrow().string_bindings_vec());

                if global_env.borrow().is_binding_context()
                    && !global_env.borrow().is_binding_offset()
                {
                    global_env.borrow_mut().set_binding_offset(true);
                    // println!("Setting offset to TRUE");
                    // println!(
                    //     "Incrementing offset by one for some reason, with ndefs: {}",
                    //     ndefs
                    // );
                    closure_offset += 1;
                    // println!("Setting the closure offset here: {}", closure_offset);
                };

                // set the number of definitions for the environment
                capture_env.borrow_mut().set_ndefs(ndefs);

                // TODO look at this heap thing
                // Need to clear it pop when the environment exits
                // GC...
                // println!("Pushing onto the heap!");

                if !global_env.borrow().is_root() {
                    heap.push(Rc::clone(&capture_env));
                }
                // inspect_heap(&heap);
                let constructed_lambda = ByteCodeLambda::new(
                    closure_body,
                    None,
                    Some(Rc::downgrade(&capture_env)),
                    closure_offset,
                    arity,
                    ndefs,
                );

                // Determine the kind of bytecode lambda to construct
                // let constructed_lambda = if capture_env.borrow().is_root() {
                //     ByteCodeLambda::new(
                //         closure_body,
                //         Some(capture_env),
                //         None,
                //         closure_offset,
                //         arity,
                //         ndefs,
                //     )
                // } else {
                //     // set the number of definitions for the environment
                //     capture_env.borrow_mut().set_ndefs(ndefs);

                //     // TODO look at this heap thing
                //     // Need to clear it pop when the environment exits
                //     // GC...
                //     heap.push(Rc::clone(&capture_env));
                //     ByteCodeLambda::new(
                //         closure_body,
                //         None,
                //         Some(Rc::downgrade(&capture_env)),
                //         closure_offset,
                //         arity,
                //         ndefs,
                //     )
                // };

                stack.push(Rc::new(SteelVal::Closure(constructed_lambda)));

                ip += forward_jump;
                // println!("Performed forward jump to instruction: {}", ip);
            }
            // OpCode::ECLOSURE => {
            //     ip += 1;
            // }
            OpCode::SDEF => {
                ip += 1;

                global_env.borrow_mut().set_binding_context(true);
                global_env.borrow_mut().set_binding_offset(false);

                // println!("Setting binding context to TRUE, offset to FALSE");

                stacks.push(stack);
                stack = Vec::new();

                // placeholder on the instruction_stack
                instruction_stack.push((0, Rc::new(Box::new([]))));
                pop_count += 1;
            }
            OpCode::EDEF => {
                // println!("Found end definition");
                global_env.borrow_mut().set_binding_context(false);
                // def_stack -= 1;
                ip += 1;
                // unimplemented!();
            }
            _ => {
                unimplemented!();
            }
        }
    }

    // unimplemented!()
    println!("###### Out of bounds instruction ######");
    println!(
        "Instruction pointer: {}, instructions length: {}",
        ip,
        instructions.len()
    );
    println!("Instructions at time:");
    pretty_print_dense_instructions(&instructions);
    panic!("Out of bounds instruction")
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
            println!("getting here in the eval_atom");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}

/// returns error if tokens.len() != expected
fn check_length(what: &str, tokens: &[Expr], expected: usize) -> Result<()> {
    if tokens.len() == expected {
        Ok(())
    } else {
        if let Some((first, rest)) = tokens.split_first() {
            let span = rest
                .into_iter()
                .map(|x| x.span())
                .fold(first.span(), |x, y| Span::merge(x, y));

            Err(SteelErr::ArityMismatch(
                format!("{}: expected {} args got {}", what, expected, tokens.len()),
                Some(span),
            ))
        } else {
            Err(SteelErr::ArityMismatch(
                format!("{}: expected {} args got {}", what, expected, tokens.len()),
                None,
            ))
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
