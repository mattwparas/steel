use crate::vm::arity::ArityMap;
use crate::vm::constants::ConstantTable;
use crate::vm::instructions::Instruction;

use crate::rerrs::SteelErr;
use crate::rvals::Result;

use crate::stop;

use crate::parser::{tokens::TokenType, Expr, SyntaxObject};

use std::ops::Deref;

use crate::vm::opcode::OpCode;

use crate::parser::span::Span;

use crate::rvals::SteelVal;

use crate::gc::Gc;
use std::convert::TryFrom;
// use std::convert::TryInto;s

pub fn emit_loop<CT: ConstantTable>(
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
                        if list_of_tokens.len() == 4 {
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

                            // load in the output_type
                            emit_loop(
                                &list_of_tokens[3],
                                instructions,
                                None,
                                arity_map,
                                constant_map,
                            )?;

                            instructions.push(Instruction::new_collect_to());
                            return Ok(());
                        }

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
                            stop!(ArityMismatch => format!("{}: expected {} args got {}", "if", 3, &list_of_tokens[1..].len()); *sp)

                            // stop!(BadSyntax => "malformed if statement"; *sp);
                        }
                        return Ok(());
                    }

                    Expr::Atom(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }) if s == "define" || s == "defn" => {
                        check_length("define", &list_of_tokens[1..], 2)?;

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
                                let rev_iter = l.iter().rev();
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
                        check_length("set", &list_of_tokens[1..], 2)?;
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

                        // emit instructions for the args
                        for expr in &list_of_tokens[1..] {
                            emit_loop(expr, instructions, None, arity_map, constant_map)?;
                        }

                        // emit instructions for the func
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

    transformed
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

/// returns error if tokens.len() != expected
fn check_length(what: &str, tokens: &[Expr], expected: usize) -> Result<()> {
    if tokens.len() == expected {
        Ok(())
    } else {
        if let Some((first, rest)) = tokens.split_first() {
            let span = rest
                .iter()
                .map(|x| x.span())
                .fold(first.span(), Span::merge);

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
