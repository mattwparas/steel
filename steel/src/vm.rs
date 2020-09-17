mod arity;
mod constants;
mod expand;
mod heap;
mod instructions;
mod map;
mod opcode;
mod stack;

pub use arity::Arity;
pub use arity::ArityMap;
pub use constants::ConstantMap;
pub use constants::ConstantTable;
pub use expand::expand;
pub use expand::extract_macro_definitions;
use expand::get_definition_names;
pub use heap::Heap;
pub use instructions::Instruction;
pub use map::SymbolMap;
pub use opcode::OpCode;

pub use stack::{CallStack, EnvStack, Stack, StackFrame};

use expand::MacroSet;

// use log::{debug, error, info, trace, warn};

use crate::parser::{tokens::TokenType, Expr, ParseError, Parser, SyntaxObject};
use std::iter::Iterator;
use std::result;
// use crate::primitives::ListOperations;
use crate::env::{Env, FALSE, TRUE, VOID};
use crate::gc::Gc;
use crate::parser::span::Span;
use crate::primitives::{ListOperations, VectorOperations};
use crate::rerrs::SteelErr;
use crate::rvals::{ByteCodeLambda, Result, SteelVal};
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::io::Read;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;
use std::time::Instant;

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
            }
            | Instruction {
                op_code: OpCode::SET,
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

// Adds a flag to the pop value in order to save the heap to the global heap
// I should really come up with a better name but for now we'll leave it
pub fn inject_heap_save_to_pop(instructions: &mut [Instruction]) {
    match instructions {
        [.., Instruction {
            op_code: OpCode::EDEF,
            ..
        }, Instruction {
            op_code: OpCode::BIND,
            ..
        }, Instruction {
            op_code: OpCode::VOID,
            ..
        }, Instruction {
            op_code: OpCode::POP,
            payload_size: x,
            ..
        }] => {
            // println!("Injecting Heap Save to Pop!");
            *x = 1;
        }
        _ => {}
    }
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

fn _coalesce_clears(instructions: &mut Vec<Instruction>) {
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
                        let idx = constant_map.add_or_get(Gc::new(converted));
                        instructions.push(Instruction::new_push_const(idx));
                        // instructions.push(Instruction::new_quote());
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "eval" => {
                        check_length("eval", &list_of_tokens, 2)?;
                        // load in the expression to be evaluated
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        instructions.push(Instruction::new_eval());
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "read" => {
                        check_length("read", &list_of_tokens, 2)?;
                        // load in the string to be read
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;
                        instructions.push(Instruction::new_read());
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "execute" => {
                        check_length("execute", &list_of_tokens, 3)?;

                        // load in the transducer
                        emit_loop(
                            &list_of_tokens[1],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;

                        // load in the collection
                        emit_loop(
                            &list_of_tokens[2],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;

                        instructions.push(Instruction::new_collect());
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "transduce" => {
                        // (transduce transducer func initial_value iterable)
                        check_length("transduce", &list_of_tokens, 5)?;

                        for expr in &list_of_tokens[1..] {
                            emit_loop(expr, instructions, None, arity_map, constant_map)?;
                        }

                        instructions.push(Instruction::new_transduce());
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
                        check_length("define", &list_of_tokens, 3)?;

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
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "set!" => {
                        // instructions.push()
                        // check_length("map'", tokens, expected)
                        check_length("set", &list_of_tokens, 3)?;
                        // Load in the variable
                        // emit_loop(
                        //     &list_of_tokens[1],
                        //     instructions,
                        //     None,
                        //     arity_map,
                        //     constant_map,
                        // )?;
                        // Load in the expression to reassign
                        emit_loop(
                            &list_of_tokens[2],
                            instructions,
                            None,
                            arity_map,
                            constant_map,
                        )?;

                        let identifier = &list_of_tokens[1];

                        if let Expr::Atom(syn) = identifier {
                            instructions.push(Instruction::new(OpCode::SET, 0, syn.clone(), false));
                        } else {
                            stop!(Generic => "set! takes an identifier")
                        }

                        // match i

                        // instructions.push(Instruction::new_set());
                        // instructions.push(Instruction::new_pop());

                        return Ok(());
                    }
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
                    }) if s == "return!" => {
                        check_length("return!", &list_of_tokens, 2)?;
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
                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "apply" => {
                        // instructions.push("apply".to_string());
                        check_length("apply", &list_of_tokens, 3)?;
                        for expr in &list_of_tokens[1..] {
                            emit_loop(expr, instructions, None, arity_map, constant_map)?;
                        }
                        instructions.push(Instruction::new_apply(
                            if let Expr::Atom(s) = &list_of_tokens[0] {
                                s.clone()
                            } else {
                                SyntaxObject::default(TokenType::Identifier("apply".to_string()))
                            },
                        ));

                        return Ok(());
                    }
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
                            // instructions.push(Instruction::new_clear());
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

#[derive(Copy, Clone, Debug, PartialEq, Hash)]
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

pub struct Ctx<CT: ConstantTable> {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: CT,
    pub(crate) arity_map: ArityMap,
    pub(crate) repl: bool,
}

#[derive(Copy, Clone)]
pub struct EvaluationProgress {
    instruction_count: usize,
    callback: Option<Callback>,
}

impl EvaluationProgress {
    pub fn new() -> Self {
        EvaluationProgress {
            instruction_count: 0,
            callback: None,
        }
    }

    pub fn with_callback(mut self, callback: Callback) -> Self {
        self.callback.replace(callback);
        self
    }

    pub fn callback(&self) {
        if let Some(callback) = &self.callback {
            callback(&self.instruction_count);
        }
    }

    pub fn increment(&mut self) {
        self.instruction_count += 1;
    }
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
            true,
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

pub type Callback = fn(&usize) -> bool;

// pub type FunctionSignature = fn(&[Gc<SteelVal>]) -> Result<Gc<SteelVal>>;

#[macro_export]
macro_rules! build_vm {

    ($($type:ty),* $(,)?) => {
        {
            let mut interpreter = VirtualMachine::new();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *
            interpreter
        }
    };

    (Structs => {$($type:ty),* $(,)?} Functions => {$($binding:expr => $func:expr),* $(,)?}) => {
        {
            let mut interpreter = VirtualMachine::new();
            $ (
                interpreter.insert_bindings(<$type>::generate_bindings());
            ) *

            $ (
                interpreter.insert_binding($binding.to_string(), SteelVal::FuncV($func));
            ) *

            interpreter
        }
    };
}

pub struct VirtualMachine {
    global_env: Rc<RefCell<Env>>,
    global_heap: Heap,
    macro_env: Rc<RefCell<Env>>,
    idents: MacroSet,
    callback: Option<Callback>,
    ctx: Ctx<ConstantMap>,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            global_env: Rc::new(RefCell::new(Env::default_env())),
            global_heap: Heap::new(),
            macro_env: Rc::new(RefCell::new(Env::root())),
            idents: MacroSet::new(),
            callback: None,
            ctx: Ctx::<ConstantMap>::default(),
        }
    }

    pub fn insert_binding(&mut self, name: String, value: SteelVal) {
        self.global_env
            .borrow_mut()
            .add_rooted_value(&mut self.ctx.symbol_map, (name.as_str(), value));
    }

    pub fn insert_bindings(&mut self, vals: Vec<(String, SteelVal)>) {
        self.global_env
            .borrow_mut()
            .repl_define_zipped_rooted(&mut self.ctx.symbol_map, vals.into_iter());
    }

    pub fn on_progress(&mut self, callback: Callback) {
        self.callback.replace(callback);
    }

    pub fn print_bindings(&self) {
        println!(
            "Env length: {}",
            self.global_env.borrow().bindings_map().len()
        );
        println!("{:?}", self.global_env.borrow().bindings_map());
    }

    pub fn roll_back(&mut self, _idx: usize) {
        unimplemented!()
    }

    // Read in the file from the given path and execute accordingly
    // Loads all the functions in from the given env
    pub fn parse_and_execute_from_path<P: AsRef<Path>>(
        &mut self,
        path: P,
        // ctx: &mut Ctx<ConstantMap>,
    ) -> Result<Vec<Gc<SteelVal>>> {
        let mut file = std::fs::File::open(path)?;
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;
        self.parse_and_execute(exprs.as_str())
    }

    // pub fn new_with_std

    pub fn parse_and_execute(
        &mut self,
        expr_str: &str,
        // ctx: &mut Ctx<ConstantMap>,
    ) -> Result<Vec<Gc<SteelVal>>> {
        // let now = Instant::now();
        let gen_bytecode = self.emit_instructions(expr_str)?;

        // previous size of the env
        // let length = self.global_env.borrow().len();

        // println!("Bytecode generated in: {:?}", now.elapsed());
        gen_bytecode
            .into_iter()
            .map(|x| {
                let code = Rc::new(x.into_boxed_slice());
                let now = Instant::now();
                // let constant_map = &self.ctx.constant_map;
                // let repl = self.ctx.repl;
                // let mut heap = Vec::new();
                let res = self.execute(code, self.ctx.repl);
                println!("Time taken: {:?}", now.elapsed());
                res
            })
            .collect::<Result<Vec<Gc<SteelVal>>>>()
    }

    fn emit_instructions_from_exprs(
        &mut self,
        exprs: Vec<Expr>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut results = Vec::new();
        // populate MacroSet
        self.idents.insert_from_iter(
            get_definition_names(&exprs)
                .into_iter()
                .chain(self.ctx.symbol_map.copy_underlying_vec().into_iter()),
        );

        // Yoink the macro definitions
        // Add them to our macro env
        // TODO change this to be a unique macro env struct
        // Just a thin wrapper around a hashmap
        let extracted_statements = extract_macro_definitions(
            &exprs,
            &self.macro_env,
            &self.global_env,
            &mut self.ctx.symbol_map,
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
            count_and_collect_global_defines(&expanded_statements, &mut self.ctx.symbol_map);

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
                // println!("CASE 1: Popping last!!!!!!!!!");
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
                &mut self.ctx.arity_map,
                &mut self.ctx.constant_map,
            )?;
            // if !script {
            // instructions.push(Instruction::new_clear());
            instructions.push(Instruction::new_pop());
            // Maybe see if this gets the job done here
            inject_heap_save_to_pop(&mut instructions);
            // }
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        // println!("Got here!");

        insert_debruijn_indices(&mut instruction_buffer, &mut self.ctx.symbol_map)?;
        extract_constants(&mut instruction_buffer, &mut self.ctx.constant_map)?;
        // coalesce_clears(&mut instruction_buffer);

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            // pretty_print_instructions(extracted.as_slice());
            results.push(densify(extracted));
        }

        Ok(results)
    }

    pub fn emit_instructions(
        &mut self,
        expr_str: &str,
        // ctx: &mut Ctx<ConstantMap>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        // the interner needs to be fixed but for now it just is here for legacy reasons
        // it currently does no allocation
        let mut intern = HashMap::new();

        // Parse the input
        let parsed: result::Result<Vec<Expr>, ParseError> =
            Parser::new(expr_str, &mut intern).collect();
        let parsed = parsed?;

        self.emit_instructions_from_exprs(parsed)
    }

    pub fn execute(
        &mut self,
        instructions: Rc<Box<[DenseInstruction]>>,
        // constants: &CT,
        // heap: &mut Vec<Rc<RefCell<Env>>>,
        repl: bool,
    ) -> Result<Gc<SteelVal>> {
        // execute_vm(instructions)

        // println!("Active Object Count: {:?}", OBJECT_COUNT);

        let stack = StackFrame::new();
        let mut heap = Heap::new();

        // give access to the global root via this method
        heap.plant_root(Rc::downgrade(&self.global_env));

        // let mut constants: Vec<Rc<RefCell<Env>>

        // let global_env = Rc::new(RefCell::new(Env::default_env()));
        let result = vm(
            instructions,
            stack,
            &mut heap,
            // heap,
            Rc::clone(&self.global_env),
            &self.ctx.constant_map,
            repl,
        );

        // println!("$$$$$$$$$$ GETTING HERE! $$$$$$$$$$$$");

        // TODO figure this noise out
        // might be easier to just... write a GC
        if self.global_env.borrow().is_binding_context() {
            // println!("Copying over the heap from the run time:");

            self.global_heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
            // self.global_heap.inspect_heap();
            // inspect_heap(&self.global_heap);
        }

        // Maybe?????
        // self.global_env.borrow_mut().pop_last();

        // self.global_env.borrow_mut().set_binding_offset(false);

        // println!("Global heap length after: {}", self.global_heap.len());

        heap.clear();
        heap.reset_limit();

        // println!("Active Object Count: {:?}", OBJECT_COUNT);
        // println!("Heap length: {}", self.global_heap.len());
        // println!("local heap length: {}", heap.len());

        result
    }
}

pub fn execute_vm(
    instructions: Rc<Box<[DenseInstruction]>>,
    constants: &ConstantMap,
) -> Result<Gc<SteelVal>> {
    let stack = StackFrame::new();
    let mut heap = Heap::new();
    // let mut constants: Vec<Rc<SteelVal>> = Vec::new();
    let global_env = Rc::new(RefCell::new(Env::default_env()));
    vm(
        instructions,
        stack,
        &mut heap,
        global_env,
        constants,
        false,
        // None,
    )
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

// fn switch_filter_map<'global, CT: ConstantTable>(
//     stack_func: Rc<SteelVal>,
//     constants: &'global CT,
//     cur_inst_span: &'global Span,
// ) {
//     unimplemented!()
// }

pub(crate) fn inline_reduce_iter<
    'global,
    I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
    CT: ConstantTable,
>(
    iter: I,
    initial_value: Gc<SteelVal>,
    reducer: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> Result<Gc<SteelVal>> {
    // unimplemented!();

    let switch_statement = move |acc, x| match reducer.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![acc?, x?];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![acc?, x?];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![acc?, x?];
            // if let Some()

            let parent_env = closure.sub_expression_env();

            // TODO remove this unwrap
            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

            let mut local_heap = Heap::new();

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            vm(
                closure.body_exp(),
                args.into(),
                &mut local_heap,
                inner_env,
                constants,
                repl,
            )
        }

        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.fold(Ok(initial_value), switch_statement)
}

pub(crate) fn inline_map_result_iter<
    'global,
    I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![arg?];
            func(&arg_vec).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::StructClosureV(factory, func) => {
            let arg_vec = vec![arg?];
            func(arg_vec, factory).map_err(|x| x.set_span(*cur_inst_span))
        }
        SteelVal::Closure(closure) => {
            // ignore the stack limit here
            let args = vec![arg?];
            // if let Some()

            let parent_env = closure.sub_expression_env();

            // TODO remove this unwrap
            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

            let mut local_heap = Heap::new();

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            vm(
                closure.body_exp(),
                args.into(),
                &mut local_heap,
                inner_env,
                constants,
                repl,
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    // Map<

    // Map::new()

    // std::iter::Map

    iter.map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

pub(crate) fn inline_map_iter<
    'global,
    I: Iterator<Item = Gc<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
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

            let parent_env = closure.sub_expression_env();

            // TODO remove this unwrap
            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

            let mut local_heap = Heap::new();

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            vm(
                closure.body_exp(),
                args.into(),
                &mut local_heap,
                inner_env,
                constants,
                repl,
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; *cur_inst_span),
    };

    iter.map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

pub(crate) fn inline_filter_result_iter<
    'global,
    I: Iterator<Item = Result<Gc<SteelVal>>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg: Result<Gc<SteelVal>>| match arg {
        Ok(arg) => {
            match stack_func.as_ref() {
                SteelVal::FuncV(func) => {
                    let arg_vec = vec![Gc::clone(&arg)];
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
                    let arg_vec = vec![Gc::clone(&arg)];
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
                    let args = vec![Gc::clone(&arg)];
                    // if let Some()

                    let parent_env = closure.sub_expression_env();

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

                    let mut local_heap = Heap::new();

                    // TODO make recursive call here with a very small stack
                    // probably a bit overkill, but not much else I can do here I think
                    let res = vm(
                        closure.body_exp(),
                        args.into(),
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
                }
                _ => Some(Err(SteelErr::TypeMismatch(
                    "map expected a function".to_string(),
                    Some(*cur_inst_span),
                ))),
            }
        }

        _ => Some(arg),
    };

    iter.filter_map(switch_statement)

    // for val in iter {
    //     collected_results.push(switch_statement(val)?);
    // }

    // Ok(collected_results)
}

pub(crate) fn inline_filter_iter<
    'global,
    I: Iterator<Item = Gc<SteelVal>> + 'global,
    // R: Iterator<Item = Result<Rc<SteelVal>>>,
    CT: ConstantTable,
>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &'global CT,
    cur_inst_span: &'global Span,
    repl: bool,
) -> impl Iterator<Item = Result<Gc<SteelVal>>> + 'global {
    // unimplemented!();

    // let mut collected_results: Vec<Rc<SteelVal>> = Vec::new();

    // Maybe use dynamic dispatch (i.e. boxed closure or trait object) instead of this
    // TODO don't allocate this vec for just this
    let switch_statement = move |arg| match stack_func.as_ref() {
        SteelVal::FuncV(func) => {
            let arg_vec = vec![Gc::clone(&arg)];
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
            let arg_vec = vec![Gc::clone(&arg)];
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
            let args = vec![Gc::clone(&arg)];
            // if let Some()

            let parent_env = closure.sub_expression_env();

            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

            let mut local_heap = Heap::new();

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            let res = vm(
                closure.body_exp(),
                args.into(),
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

pub fn inline_map_normal<I: Iterator<Item = Gc<SteelVal>>, CT: ConstantTable>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &CT,
    cur_inst: &DenseInstruction,
    repl: bool,
) -> Result<Vec<Gc<SteelVal>>> {
    // unimplemented!();

    let mut collected_results: Vec<Gc<SteelVal>> = Vec::new();

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

            let parent_env = closure.sub_expression_env();

            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

            let mut local_heap = Heap::new();

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            vm(
                closure.body_exp(),
                args.into(),
                &mut local_heap,
                inner_env,
                constants,
                repl,
            )
        }
        _ => stop!(TypeMismatch => "map expected a function"; cur_inst.span),
    };

    for val in iter {
        collected_results.push(switch_statement(val)?);
    }

    Ok(collected_results)
}

fn inline_filter_normal<I: Iterator<Item = Gc<SteelVal>>, CT: ConstantTable>(
    iter: I,
    stack_func: Gc<SteelVal>,
    constants: &CT,
    cur_inst: &DenseInstruction,
    repl: bool,
) -> Result<Vec<Gc<SteelVal>>> {
    // unimplemented!();

    let mut collected_results: Vec<Gc<SteelVal>> = Vec::new();

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

            let parent_env = closure.sub_expression_env();

            // TODO remove this unwrap
            let offset = closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

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

            let mut local_heap = Heap::new();

            // TODO make recursive call here with a very small stack
            // probably a bit overkill, but not much else I can do here I think
            vm(
                closure.body_exp(),
                args.into(),
                &mut local_heap,
                inner_env,
                constants,
                repl,
            )
        }
        _ => stop!(TypeMismatch => "filter expected a function"; cur_inst.span),
    };

    for val in iter {
        let res = switch_statement(Gc::clone(&val))?;
        if let SteelVal::BoolV(true) = res.as_ref() {
            collected_results.push(val);
        }
    }

    Ok(collected_results)
}

#[derive(Debug)]
pub struct InstructionPointer {
    pub(crate) ip: usize,
    instrs: Rc<Box<[DenseInstruction]>>,
}

impl InstructionPointer {
    pub fn new_raw() -> Self {
        InstructionPointer {
            ip: 0,
            instrs: Rc::new(Box::new([])),
        }
    }

    pub fn new(ip: usize, instrs: Rc<Box<[DenseInstruction]>>) -> Self {
        InstructionPointer { ip, instrs }
    }

    pub fn instrs_ref(&self) -> &Rc<Box<[DenseInstruction]>> {
        &self.instrs
    }

    pub fn instrs(self) -> Rc<Box<[DenseInstruction]>> {
        self.instrs
    }
}

// static const HEAP_LIMIT: usize =

static HEAP_LIMIT: usize = 5000;
pub static MAXIMUM_OBJECTS: usize = 50000;

pub fn vm<CT: ConstantTable>(
    instructions: Rc<Box<[DenseInstruction]>>,
    stack: StackFrame,
    heap: &mut Heap,
    global_env: Rc<RefCell<Env>>,
    constants: &CT,
    repl: bool,
    // callback: Option<Callback>,
) -> Result<Gc<SteelVal>> {
    let mut ip = 0;
    let mut global_env = global_env;

    if instructions.is_empty() {
        stop!(Generic => "empty stack!");
    }

    // instruction stack for function calls
    let mut instruction_stack: Stack<InstructionPointer> = Stack::new();
    // stacks on stacks baby
    let mut stacks: CallStack = Stack::new();
    // initialize the instruction number pointer
    let mut cur_inst;
    // Pointer to array of instructions
    let mut instructions = instructions;
    // Self explanatory
    let mut stack = stack;
    // Manage current env in its own stack
    let mut env_stack: EnvStack = Stack::new();
    // Manage the depth of instructions to know when to backtrack
    let mut pop_count = 1;
    // Manage the instruction count
    let mut instruction_count = 0;

    while ip < instructions.len() {
        // let object_count: usize = Gc::<()>::object_count();

        // // this is how you could go ahead and snatch the memory count in between instructions
        // // this still doesn't answer how to stop a rust built in from exploding the memory though
        // if object_count > MAXIMUM_OBJECTS {
        //     stop!(Generic => "out of memory!");
        // }

        cur_inst = &instructions[ip];

        // trace!()

        match cur_inst.op_code {
            OpCode::PANIC => {
                let error_message = stack.pop().unwrap();
                stop!(Generic => error_message.to_string(); cur_inst.span);
            }
            OpCode::EVAL => {
                panic!("eval not yet supported - internal compiler error");
            }
            OpCode::PASS => {
                ip += 1;
            }
            OpCode::VOID => {
                stack.push(VOID.with(|f| Gc::clone(f)));
                ip += 1;
            }
            OpCode::READ => {
                // this needs to be a string
                let expression_to_parse = stack.pop().unwrap();

                if let SteelVal::StringV(expr) = expression_to_parse.as_ref() {
                    // dummy interning hashmap because the parser is bad
                    // please don't judge I'm working on fixing it
                    // TODO
                    let mut intern = HashMap::new();

                    let parsed: result::Result<Vec<Expr>, ParseError> =
                        Parser::new(expr.as_str(), &mut intern).collect();

                    match parsed {
                        Ok(v) => {
                            // for now, only support one expression
                            // otherwise parse into a list of things
                            // if v.len() != 1 {
                            //     stop!(ArityMismatch => "read only supports one expression")
                            // }

                            let converted: Result<Vec<SteelVal>> = v
                                .into_iter()
                                .map(|x| SteelVal::try_from(x.clone()))
                                .collect();

                            // let converted = Gc::new(SteelVal::try_from(v[0].clone())?);
                            stack.push(ListOperations::built_in_list_func_flat_non_gc(converted?)?);
                            ip += 1;
                        }
                        Err(e) => stop!(Generic => format!("{}", e); cur_inst.span),
                    }
                } else {
                    stop!(TypeMismatch => "read expects a string"; cur_inst.span)
                }
            }
            OpCode::COLLECT => {
                let list = stack.pop().unwrap();
                let transducer = stack.pop().unwrap();

                println!("getting here!");

                if let SteelVal::IterV(transducer) = transducer.as_ref() {
                    let ret_val = transducer.run(list, constants, &cur_inst.span, repl);
                    stack.push(ret_val?);
                } else {
                    stop!(Generic => "Transducer execute takes a list"; cur_inst.span);
                }
                ip += 1;
            }
            OpCode::TRANSDUCE => {
                let list = stack.pop().unwrap();
                let initial_value = stack.pop().unwrap();
                let reducer = stack.pop().unwrap();
                let transducer = stack.pop().unwrap();

                if let SteelVal::IterV(transducer) = transducer.as_ref() {
                    let ret_val = transducer.transduce(
                        list,
                        initial_value,
                        reducer,
                        constants,
                        &cur_inst.span,
                        repl,
                    );
                    stack.push(ret_val?);
                } else {
                    stop!(Generic => "Transduce must take an iterable");
                }
                ip += 1;
            }
            OpCode::SET => {
                let value_to_assign = stack.pop().unwrap();
                // let variable = stack.pop().unwrap();

                // println!("index: {}", cur_inst.payload_size);

                if repl {
                    let value = global_env
                        .borrow_mut()
                        .repl_set_idx(cur_inst.payload_size, value_to_assign)?;

                    // println!("Old value: {}", value);
                    stack.push(value);
                } else {
                    unimplemented!();
                    // let value = global_env.borrow().lookup_idx(cur_inst.payload_size)?;
                    // stack.push(value);
                }
                ip += 1;

                // global_env.borrow_mut().defin
            }
            OpCode::PUSHCONST => {
                let val = constants.get(cur_inst.payload_size);
                stack.push(val);
                ip += 1;
            }
            OpCode::PUSH => {
                // awful awful awful hack to fix the repl environment noise
                if repl {
                    let value = global_env.borrow().repl_lookup_idx(cur_inst.payload_size)?;
                    stack.push(value);
                } else {
                    let value = global_env.borrow().lookup_idx(cur_inst.payload_size)?;
                    stack.push(value);
                }
                ip += 1;
            }
            OpCode::APPLY => {
                let _list = stack.pop().unwrap();
                let _func = stack.pop().unwrap();

                panic!("Apply not implemented - internal compiler error");

                // TODO inline the OpCode::FUNC case here for speedup

                // match list.as_ref() {
                //     SteelVal::Pair(_, _) => {
                //         let args = SteelVal::iter(list)
                //     }
                // }
            }
            OpCode::CLEAR => {
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

                        stack.push(ListOperations::built_in_list_func()(&collected_results)?);
                    }
                    SteelVal::VectorV(v) => {
                        // TODO get rid of the clone here
                        stack.push(VectorOperations::vec_construct_iter(inline_map_iter(
                            v.into_iter().map(Gc::clone),
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
                    }
                    SteelVal::VectorV(v) => {
                        // TODO get rid of the clone here

                        stack.push(VectorOperations::vec_construct_iter(inline_filter_iter(
                            v.into_iter().map(Gc::clone),
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
                        let result = f(stack.peek_range(stack.len() - cur_inst.payload_size..))
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

                        let parent_env = closure.sub_expression_env();

                        // TODO remove this unwrap
                        let offset = closure.offset()
                            + parent_env.upgrade().unwrap().borrow().local_offset();

                        let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                            parent_env.clone(),
                            offset,
                        )));

                        // add this closure to the list of children
                        parent_env
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .add_child(Rc::downgrade(&inner_env));

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
                        env_stack.push(Rc::clone(&global_env));

                        global_env = inner_env;
                        instruction_stack.push(InstructionPointer::new(ip + 1, instructions));
                        pop_count += 1;
                        stacks.push(stack);
                        instructions = closure.body_exp();
                        stack = args.into(); // TODO
                        ip = 0;
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

                        let result = f(stack.peek_range(stack.len() - cur_inst.payload_size..))
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

                        // if !global_env.borrow().is_root() {
                        //     heap.gather_from_slice(&args);
                        //     println!("Gathering from the stack!");
                        //     println!("Stack at this point: {:?}", stack);
                        //     heap.gather_from_slice(stack.as_slice());
                        // }

                        // TODO maybe make this happen from inside that check? not sure
                        // set up test suite for garbage collection...

                        // TODO
                        // heap.gather_from_slice(&args);

                        // heap.gather_from_global_root();
                        // heap.mark();
                        // println!("Gathering from the stack!");
                        // println!("Stack at this point: {:?}", stack);
                        // heap.gather_from_slice(stack.as_slice());

                        // evaluate(&lambda.body_exp(), &inner_env)
                        let parent_env = closure.sub_expression_env();
                        // TODO remove this unwrap
                        let offset = closure.offset()
                            + parent_env.upgrade().unwrap().borrow().local_offset();

                        let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                            parent_env.clone(),
                            offset,
                        )));

                        parent_env
                            .upgrade()
                            .unwrap()
                            .borrow_mut()
                            .add_child(Rc::downgrade(&inner_env));

                        inner_env
                            .borrow_mut()
                            .reserve_defs(if closure.ndef_body() > 0 {
                                closure.ndef_body() - 1
                            } else {
                                0
                            });

                        // if heap.len() > HEAP_LIMIT && !args.is_empty() {
                        //     println!("Args at exit: {:?}", args);
                        //     println!("Heap length before mark and sweep: {}", heap.len());
                        //     println!("Active Object Count: {:?}", OBJECT_COUNT);
                        //     // heap.gather_mark_and_sweep(&parent_env.upgrade().unwrap());
                        //     // println!("Heap length before extra step:, {}", heap.len());
                        //     // heap.gather(&parent_env.upgrade().unwrap());
                        //     // println!("Heap length after extra step: {}", heap.len());

                        //     heap.gather_mark_and_sweep_2(&global_env, &inner_env);

                        // heap.gather(&global_env)

                        //     // heap.gather_from_global_root();
                        //     // heap.mark();
                        //     // heap.sweep();

                        //     println!("Heap length after mark and sweep: {}", heap.len());
                        //     println!("Active Object Count: {:?}", OBJECT_COUNT);
                        //     // heap.add(val)
                        // }

                        // println!("Args at exit: {:?}", args);
                        // println!("Heap length before mark and sweep: {}", heap.len());
                        // println!("Active Object Count: {:?}", OBJECT_COUNT);

                        heap.gather_mark_and_sweep_2(&global_env, &inner_env);
                        heap.collect_garbage();

                        // if heap.len() > HEAP_LIMIT {
                        //     println!("Args at exit: {:?}", args);
                        //     println!("Heap length before mark and sweep: {}", heap.len());
                        //     println!("Active Object Count: {:?}", OBJECT_COUNT);
                        //     // heap.inspect_heap();
                        //     // TODO should GC here but for some reason... not working...
                        //     // heap.gather_mark_and_sweep_2(&global_env, &inner_env);
                        //     // heap.drop_large_refs();
                        //     heap.collect_garbage();

                        //     // heap.inspect_heap();
                        //     println!("Heap length after mark and sweep: {}", heap.len());
                        //     println!("Active Object Count: {:?}", OBJECT_COUNT);
                        // }

                        // println!("Heap length after mark and sweep: {}", heap.len());
                        // println!("Active Object Count: {:?}", OBJECT_COUNT);

                        global_env = inner_env;
                        instructions = closure.body_exp();
                        stack = args.into();
                        ip = 0;
                    }
                    _ => {
                        stop!(BadSyntax => "Application not a procedure or function type not supported"; cur_inst.span);
                    }
                }
            }
            OpCode::IF => {
                // change to truthy...
                if stack.pop().unwrap().is_truthy() {
                    ip = cur_inst.payload_size;
                } else {
                    ip += 1;
                }
            }
            OpCode::JMP => {
                ip = cur_inst.payload_size;
                // HACk
                if ip == 0 && heap.len() > HEAP_LIMIT {
                    println!("Jumping back to the start!");
                    println!("Heap length: {}", heap.len());
                    println!("############################");
                    // heap.gather_mark_and_sweep(&global_env);
                    // heap.drop_large_refs();
                    heap.collect_garbage();
                }
            }
            OpCode::POP => {
                pop_count -= 1;
                if pop_count == 0 {
                    env_stack.clear();

                    if cur_inst.payload_size == 1 {
                        global_env.borrow_mut().set_binding_context(true);
                    }

                    let ret_val = stack.try_pop().ok_or_else(|| {
                        SteelErr::Generic("stack empty at pop".to_string(), Some(cur_inst.span))
                    });

                    global_env.borrow_mut().set_binding_offset(false);

                    return ret_val;
                } else {
                    let ret_val = stack.pop().unwrap();
                    let prev_state = instruction_stack.pop().unwrap();

                    // Heap::gather_and_mark(&global_env);

                    // heap.gather_mark_and_sweep(&global_env);

                    // heap.gather(&global_env);

                    if prev_state.instrs_ref().len() != 0 {
                        // println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
                        // println!("Heap length before mark and sweep: {}", heap.len());
                        // println!("Active Object Count: {:?}", OBJECT_COUNT);
                        // heap.gather_mark_and_sweep(&global_env);
                        // println!("Heap length after mark and sweep: {}", heap.len());
                        // println!("Active Object Count: {:?}", OBJECT_COUNT);
                        // println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");

                        global_env = env_stack.pop().unwrap();
                        // TODO
                        // heap.truncate(heap_stack.pop().unwrap().unwrap());

                        ip = prev_state.ip;
                        instructions = prev_state.instrs();
                    } else {
                        ip += 1;
                    }

                    stack = stacks.pop().unwrap();
                    stack.push(ret_val);
                }
            }
            OpCode::BIND => {
                let offset = global_env.borrow().local_offset();

                // if cur_inst.payload_size == 151 {
                //     println!("Env at this binding:");
                //     global_env.borrow().print_bindings();
                //     println!(
                //         "Binding: payload size: {}, offset: {}",
                //         cur_inst.payload_size, offset
                //     );
                // }

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

                // if !global_env.borrow().is_reachable() {
                heap.add(Rc::clone(&capture_env));
                // }

                // if !global_env.borrow().is_root() && !global_env.borrow().is_reachable() {
                //     // println!("Pushing onto the heap!");
                //     heap.add(Rc::clone(&capture_env));
                //     // heap_stack += 1;
                //     // let hs_len = heap_stack.len() - 1;
                //     // heap_stack[hs_len] += 1;
                //     // inspect_heap(&heap);
                // }

                // inspect_heap(&heap);
                let constructed_lambda = ByteCodeLambda::new(
                    closure_body,
                    Rc::downgrade(&capture_env),
                    closure_offset,
                    arity,
                    ndefs,
                );

                stack.push(Gc::new(SteelVal::Closure(constructed_lambda)));

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
                stack = Stack::new();

                // placeholder on the instruction_stack
                instruction_stack.push(InstructionPointer::new_raw());
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

        // Check the evaluation progress in some capacity
        // if let Some(callback) = callback {
        //     callback(&instruction_count);
        // }

        instruction_count += 1;

        // ip += 1;
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
fn eval_atom(t: &SyntaxObject) -> Result<Gc<SteelVal>> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => {
            if *b {
                Ok(TRUE.with(|f| Gc::clone(f)))
            } else {
                Ok(FALSE.with(|f| Gc::clone(f)))
            }
        }
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(Gc::new(SteelVal::NumV(*n))),
        TokenType::StringLiteral(s) => Ok(Gc::new(SteelVal::StringV(s.clone()))),
        TokenType::CharacterLiteral(c) => Ok(Gc::new(SteelVal::CharV(*c))),
        TokenType::IntegerLiteral(n) => Ok(Gc::new(SteelVal::IntV(*n))),
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
