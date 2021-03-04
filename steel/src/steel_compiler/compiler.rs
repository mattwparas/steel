use crate::core::{instructions::Instruction, opcode::OpCode};
use crate::steel_compiler::{
    // codegen::emit_loop,
    code_generator::CodeGenerator,
    constants::{ConstantMap, ConstantTable},
    // expand::MacroSet,
    map::SymbolMap,
    program::Program,
};

use std::iter::Iterator;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};

use crate::parser::span::Span;

use crate::parser::parser::{ParseError, Parser};

use crate::parser::ast::ExprKind;
use crate::parser::expander::SteelMacro;
use crate::parser::parser::SyntaxObject;
use crate::parser::tokens::TokenType;

use crate::structs::SteelStruct;

use crate::core::instructions::{densify, DenseInstruction};

use crate::stop;
use std::time::SystemTime;

// use crate::compiler::modules::ModuleManager;
// use crate::parser::expand_visitor::{expand, extract_macro_defs};

// use itertools::Itertools;
use log::debug;

use super::modules::{CompiledModule, ModuleManager};

// insert fast path for built in functions
// rather than look up function in env, be able to call it directly?
fn collect_defines_from_current_scope(
    instructions: &[Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<usize> {
    let mut def_stack: usize = 0;
    let mut count = 0;
    let mut bindings: HashSet<&str> = HashSet::new();

    for instruction in instructions {
        match instruction {
            Instruction {
                op_code: OpCode::SDEF,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span: _sp,
                        ..
                    }),
                ..
            } => {
                if def_stack == 0 {
                    if bindings.insert(s) {
                        let (_idx, _) = symbol_map.get_or_add(s);
                        count += 1;
                    }
                    // TODO this needs to get fixed
                    // else {
                    //     stop!(Generic => "define-values: duplicate binding name"; *sp)
                    // }
                }
            }
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                // println!("Entering closure scope!");
                def_stack += 1;
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => {
                // println!("Exiting closure scope!");
                if def_stack > 0 {
                    def_stack -= 1;
                }
            }
            _ => {}
        }
    }

    Ok(count)
}

fn collect_binds_from_current_scope(
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

fn insert_debruijn_indices(
    instructions: &mut [Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<()> {
    let mut stack: Vec<usize> = Vec::new();
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
                let payload = instructions[i].payload_size;

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
                ..
            } => {
                if let Some(x) = instructions.get_mut(i) {
                    x.constant = false;
                }
            }
            _ => {}
        }
    }

    Ok(())
}

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

/// evaluates an atom expression in given environment
fn eval_atom(t: &SyntaxObject) -> Result<SteelVal> {
    match &t.ty {
        TokenType::BooleanLiteral(b) => Ok((*b).into()),
        // TokenType::Identifier(s) => env.borrow().lookup(&s),
        TokenType::NumberLiteral(n) => Ok(SteelVal::NumV(*n)),
        TokenType::StringLiteral(s) => Ok(SteelVal::StringV(s.clone().into())),
        TokenType::CharacterLiteral(c) => Ok(SteelVal::CharV(*c)),
        TokenType::IntegerLiteral(n) => Ok(SteelVal::IntV(*n)),
        what => {
            println!("getting here in the eval_atom");
            stop!(UnexpectedToken => what; t.span)
        }
    }
}

// Adds a flag to the pop value in order to save the heap to the global heap
// I should really come up with a better name but for now we'll leave it
fn inject_heap_save_to_pop(instructions: &mut [Instruction]) {
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
            *x = 1;
        }
        _ => {}
    }
}

pub struct Compiler {
    pub(crate) symbol_map: SymbolMap,
    pub constant_map: ConstantMap,
    pub(crate) macro_env: HashMap<String, SteelMacro>,
    module_manager: ModuleManager,
}

impl Compiler {
    pub fn new(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: HashMap<String, SteelMacro>,
        module_manager: ModuleManager,
    ) -> Compiler {
        Compiler {
            symbol_map,
            constant_map,
            macro_env,
            module_manager,
        }
    }

    pub fn default() -> Self {
        Compiler::new(
            SymbolMap::default_from_env(),
            ConstantMap::new(),
            HashMap::new(),
            ModuleManager::default(),
        )
    }

    pub fn register(&mut self, name: &str) -> usize {
        self.symbol_map.add(name)
    }

    pub fn get_idx(&self, name: &str) -> Option<usize> {
        self.symbol_map.get(name).ok()
    }

    pub fn compile_program(&mut self, expr_str: &str, path: Option<PathBuf>) -> Result<Program> {
        let instructions = self.emit_instructions(expr_str, path)?;

        let program = Program::new(instructions, (&self.constant_map).to_bytes()?);
        Ok(program)
    }

    pub fn emit_instructions(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut intern = HashMap::new();

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> = if let Some(p) = &path {
            Parser::new_from_source(expr_str, &mut intern, p.to_str().unwrap()).collect()
        } else {
            Parser::new(expr_str, &mut intern).collect()
        };

        let parsed = parsed?;

        self.emit_instructions_from_exprs(parsed, false, path)
    }

    pub fn expand_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        self.module_manager
            .compile_main(&mut self.macro_env, exprs, path)
        // println!(
        //     "{:?}",
        //     output
        //         .clone()
        //         .unwrap()
        //         .iter()
        //         .map(|x| x.to_string())
        //         .join(" ")
        // );
        // output
    }

    pub fn extract_structs(
        &mut self,
        exprs: Vec<ExprKind>,
        results: &mut Vec<Vec<DenseInstruction>>,
    ) -> Result<Vec<ExprKind>> {
        let mut non_structs = Vec::new();
        let mut struct_instructions = Vec::new();
        for expr in exprs {
            if let ExprKind::Struct(s) = expr {
                let builder = SteelStruct::generate_from_ast(&s)?;

                // Add the eventual function names to the symbol map
                let indices = self.symbol_map.insert_struct_function_names(&builder);

                // Get the value we're going to add to the constant map for eventual use
                // Throw the bindings in as well
                let constant_values = builder.to_constant_val(indices);
                let idx = self.constant_map.add_or_get(constant_values);

                struct_instructions.push(Instruction::new_struct(idx));
            } else {
                non_structs.push(expr);
            }
        }

        for instruction in densify(struct_instructions) {
            results.push(vec![instruction])
        }

        Ok(non_structs)
    }

    // pub fn extract_structs_and_expand_macros(
    //     &mut self,
    //     exprs: Vec<Expr>,
    //     results: &mut Vec<Vec<DenseInstruction>>,
    // ) -> Result<Vec<Expr>> {
    //     self.idents.insert_from_iter(
    //         get_definition_names(&exprs)
    //             .into_iter()
    //             .chain(self.symbol_map.copy_underlying_vec().into_iter()),
    //     );

    //     let mut struct_instructions = Vec::new();

    //     // Yoink the macro definitions
    //     // Add them to our macro env
    //     // TODO change this to be a unique macro env struct
    //     // Just a thin wrapper around a hashmap
    //     let extracted_statements = extract_macro_definitions(
    //         exprs,
    //         &mut self.macro_env,
    //         // &self.global_env,
    //         &mut self.symbol_map,
    //         &self.idents,
    //         &mut struct_instructions,
    //         &mut self.constant_map,
    //     )?;

    //     info!("Found {} struct definitions", struct_instructions.len());

    //     // Zip up the instructions for structs
    //     // TODO come back to this
    //     for instruction in densify(struct_instructions) {
    //         results.push(vec![instruction])
    //     }

    //     // Walk through and expand all macros, lets, and defines
    //     expand_statements(extracted_statements, &mut self.macro_env)
    // }

    pub fn generate_dense_instructions(
        &mut self,
        expanded_statements: Vec<ExprKind>,
        results: Vec<Vec<DenseInstruction>>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut results = results;
        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();

        for expr in expanded_statements {
            // TODO add printing out the expression as its own special function
            // println!("{:?}", expr.to_string());
            // let mut instructions: Vec<Instruction> = Vec::new();

            let mut instructions = CodeGenerator::new(&mut self.constant_map).compile(&expr)?;

            // TODO double check that arity map doesn't exist anymore
            // emit_loop(&expr, &mut instructions, None, &mut self.constant_map)?;

            instructions.push(Instruction::new_pop());
            inject_heap_save_to_pop(&mut instructions);
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        // println!("Got here!");

        insert_debruijn_indices(&mut instruction_buffer, &mut self.symbol_map)?;
        extract_constants(&mut instruction_buffer, &mut self.constant_map)?;
        // coalesce_clears(&mut instruction_buffer);

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            // pretty_print_instructions(extracted.as_slice());
            results.push(densify(extracted));
        }

        Ok(results)
    }

    pub fn emit_instructions_from_exprs(
        &mut self,
        exprs: Vec<ExprKind>,
        _optimizations: bool,
        path: Option<PathBuf>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut results = Vec::new();

        let expanded_statements = self.expand_expressions(exprs, path)?;

        debug!(
            "Generating instructions for the expression: {:?}",
            expanded_statements
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
        );

        let statements_without_structs = self.extract_structs(expanded_statements, &mut results)?;

        self.generate_dense_instructions(statements_without_structs, results)
    }
}
