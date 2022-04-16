use crate::{
    compiler::{
        code_generator::{convert_call_globals, CodeGenerator},
        constants::{ConstantMap, ConstantTable},
        map::SymbolMap,
        passes::{
            begin::flatten_begins_and_expand_defines,
            lambda_lifting::LambdaLifter,
            reader::{ExpandMethodCalls, MultipleArityFunctions},
        },
        program::Program,
    },
    parser::kernel::Kernel,
    steel_vm::contract_checker::{ContractChecker, GlobalContractCollector},
    values::structs::{StructBuilders, StructFuncBuilderConcrete},
};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    values::structs::StructFuncBuilder,
};

use std::iter::Iterator;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use crate::rvals::{Result, SteelVal};

use crate::parser::ast::ExprKind;
use crate::parser::expander::SteelMacro;
use crate::parser::parser::SyntaxObject;
use crate::parser::parser::{ParseError, Parser};
use crate::parser::tokens::TokenType;

use crate::core::instructions::{densify, DenseInstruction};

use crate::stop;

use log::{debug, log_enabled};

use crate::steel_vm::const_evaluation::ConstantEvaluatorManager;

use super::{
    code_generator::loop_condition_local_const_arity_two, modules::ModuleManager,
    program::RawProgramWithSymbols,
};

use im_rc::HashMap as ImmutableHashMap;

use std::time::Instant;

// use itertools::Itertools;

#[derive(Default)]
pub struct DebruijnIndicesInterner {
    flat_defines: HashSet<String>,
    second_pass_defines: HashSet<String>,
}

impl DebruijnIndicesInterner {
    pub fn collect_first_pass_defines(
        &mut self,
        instructions: &mut [Instruction],
        symbol_map: &mut SymbolMap,
    ) -> Result<()> {
        for i in 2..instructions.len() {
            match (&instructions[i], &instructions[i - 1], &instructions[i - 2]) {
                (
                    Instruction {
                        op_code: OpCode::BIND,
                        contents:
                            Some(SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            }),
                        ..
                    },
                    Instruction {
                        op_code: OpCode::EDEF,
                        ..
                    },
                    Instruction {
                        op_code: OpCode::ECLOSURE,
                        ..
                    },
                ) => {
                    let idx = symbol_map.get_or_add(s);
                    self.flat_defines.insert(s.to_owned());

                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                    }
                }
                (
                    Instruction {
                        op_code: OpCode::BIND,
                        contents:
                            Some(SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            }),
                        ..
                    },
                    ..,
                ) => {
                    let idx = symbol_map.get_or_add(s);
                    self.flat_defines.insert(s.to_owned());

                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub fn collect_second_pass_defines(
        &mut self,
        instructions: &mut [Instruction],
        symbol_map: &mut SymbolMap,
    ) -> Result<()> {
        // let mut second_pass_defines: HashSet<String> = HashSet::new();

        let mut depth = 0;

        // name mangle
        // Replace all identifiers with indices
        for i in 0..instructions.len() {
            match &instructions[i] {
                Instruction {
                    op_code: OpCode::SCLOSURE,
                    ..
                } => {
                    depth += 1;
                }
                Instruction {
                    op_code: OpCode::ECLOSURE,
                    ..
                } => {
                    depth -= 1;
                }
                Instruction {
                    op_code: OpCode::BIND,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                } => {
                    // Keep track of where the defines actually are in the process
                    self.second_pass_defines.insert(s.to_owned());
                }
                Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        }),
                    ..
                }
                | Instruction {
                    op_code: OpCode::CALLGLOBAL,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        }),
                    ..
                }
                | Instruction {
                    op_code: OpCode::CALLGLOBALTAIL,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        }),
                    ..
                }
                | Instruction {
                    op_code: OpCode::SET,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        }),
                    ..
                } => {
                    if self.flat_defines.get(s).is_some() {
                        if self.second_pass_defines.get(s).is_none() && depth == 0 {
                            let message = format!(
                                "Cannot reference an identifier before its definition: {}",
                                s
                            );
                            stop!(FreeIdentifier => message; *span);
                        }
                    }

                    let idx = symbol_map.get(s).map_err(|e| e.set_span(*span))?;

                    // TODO commenting this for now
                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                        x.constant = false;
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}

// TODO this needs to take into account if they are functions or not before adding them
// don't just blindly do all global defines first - need to do them in order correctly
pub fn replace_defines_with_debruijn_indices(
    instructions: &mut [Instruction],
    symbol_map: &mut SymbolMap,
) -> Result<()> {
    let mut flat_defines: HashSet<String> = HashSet::new();

    for i in 2..instructions.len() {
        match (&instructions[i], &instructions[i - 1], &instructions[i - 2]) {
            (
                Instruction {
                    op_code: OpCode::BIND,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                },
                Instruction {
                    op_code: OpCode::EDEF,
                    ..
                },
                Instruction {
                    op_code: OpCode::ECLOSURE,
                    ..
                },
            ) => {
                let idx = symbol_map.get_or_add(s);
                flat_defines.insert(s.to_owned());

                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                }
            }
            (
                Instruction {
                    op_code: OpCode::BIND,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        }),
                    ..
                },
                ..,
            ) => {
                let idx = symbol_map.get_or_add(s);
                flat_defines.insert(s.to_owned());

                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
                }
            }
            _ => {}
        }
    }

    let mut second_pass_defines: HashSet<String> = HashSet::new();

    let mut depth = 0;

    // name mangle
    // Replace all identifiers with indices
    for i in 0..instructions.len() {
        match &instructions[i] {
            Instruction {
                op_code: OpCode::SCLOSURE,
                ..
            } => {
                depth += 1;
            }
            Instruction {
                op_code: OpCode::ECLOSURE,
                ..
            } => {
                depth -= 1;
            }
            Instruction {
                op_code: OpCode::BIND,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        ..
                    }),
                ..
            } => {
                // Keep track of where the defines actually are in the process
                second_pass_defines.insert(s.to_owned());
            }
            Instruction {
                op_code: OpCode::PUSH,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span,
                        ..
                    }),
                ..
            }
            | Instruction {
                op_code: OpCode::CALLGLOBAL,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span,
                        ..
                    }),
                ..
            }
            | Instruction {
                op_code: OpCode::CALLGLOBALTAIL,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span,
                        ..
                    }),
                ..
            }
            | Instruction {
                op_code: OpCode::SET,
                contents:
                    Some(SyntaxObject {
                        ty: TokenType::Identifier(s),
                        span,
                        ..
                    }),
                ..
            } => {
                if flat_defines.get(s).is_some() {
                    if second_pass_defines.get(s).is_none() && depth == 0 {
                        let message = format!(
                            "Cannot reference an identifier before its definition: {}",
                            s
                        );
                        stop!(FreeIdentifier => message; *span);
                    }
                }

                let idx = symbol_map.get(s).map_err(|e| e.set_span(*span))?;

                // TODO commenting this for now
                if let Some(x) = instructions.get_mut(i) {
                    x.payload_size = idx;
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

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub enum OptLevel {
    Zero = 0,
    One,
    Two,
    Three,
}

pub struct Compiler {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: ConstantMap,
    pub(crate) macro_env: HashMap<String, SteelMacro>,
    module_manager: ModuleManager,
    opt_level: OptLevel,
    kernel: Option<Kernel>,
}

impl Compiler {
    fn new(
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
            opt_level: OptLevel::Three,
            kernel: None,
        }
    }

    fn new_with_kernel(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: HashMap<String, SteelMacro>,
        module_manager: ModuleManager,
        kernel: Kernel,
    ) -> Compiler {
        Compiler {
            symbol_map,
            constant_map,
            macro_env,
            module_manager,
            opt_level: OptLevel::Three,
            kernel: Some(kernel),
        }
    }

    pub fn default() -> Self {
        Compiler::new(
            SymbolMap::new(),
            ConstantMap::new(),
            HashMap::new(),
            ModuleManager::default(),
        )
    }

    /// Registers a name in the underlying symbol map and returns the idx that it maps to
    pub fn register(&mut self, name: &str) -> usize {
        self.symbol_map.get_or_add(name)
    }

    /// Get the index associated with a name in the underlying symbol map
    /// If the name hasn't been registered, this will return `None`
    pub fn get_idx(&self, name: &str) -> Option<usize> {
        self.symbol_map.get(name).ok()
    }

    /// Given a program and (optionally) a path to that program, compile and emit the program
    pub fn compile_program(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<Program> {
        // let instructions = self.emit_instructions(expr_str, path, constants)?;

        let (ast, instructions) = self.emit_instructions_with_ast(expr_str, path, constants)?;

        let map = self.map_ast_to_defines(ast);

        // TODO Perhaps use a different representation for the constant map
        // TODO find a way to pass through the AST nicely for the runtime profiling
        let program = Program::new(instructions, self.constant_map.clone(), map);
        Ok(program)
    }

    //
    fn map_ast_to_defines(&self, ast: Vec<ExprKind>) -> HashMap<usize, ExprKind> {
        let mut hm = HashMap::new();

        // Include ast for mapped symbol ->
        for expr in ast {
            if let ExprKind::Define(d) = &expr {
                if let Some(name) = d.name.atom_identifier_or_else(|| unreachable!()).ok() {
                    if let Ok(idx) = self.symbol_map.get(name) {
                        hm.insert(idx, expr);
                    }
                }
            }
        }

        hm
    }

    pub fn compile_executable_from_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<RawProgramWithSymbols> {
        self.compile_raw_program(exprs, constants)
    }

    pub fn compile_executable(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<RawProgramWithSymbols> {
        let mut intern = HashMap::new();

        let now = Instant::now();

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> = if let Some(p) = &path {
            Parser::new_from_source(expr_str, &mut intern, p.clone()).collect()
        } else {
            Parser::new(expr_str, &mut intern).collect()
        };

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Parsing Time: {:?}", now.elapsed());
        }

        let parsed = parsed?;

        // TODO fix this hack
        self.compile_raw_program(parsed, constants)
    }

    pub fn emit_instructions_with_ast(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<(Vec<ExprKind>, Vec<Vec<DenseInstruction>>)> {
        let mut intern = HashMap::new();

        let now = Instant::now();

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> = if let Some(p) = &path {
            Parser::new_from_source(expr_str, &mut intern, p.clone()).collect()
        } else {
            Parser::new(expr_str, &mut intern).collect()
        };

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Parsing Time: {:?}", now.elapsed());
        }

        let parsed = parsed?;

        // TODO fix this hack
        self.emit_instructions_from_exprs(parsed, path, constants)
    }

    pub fn emit_instructions(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let mut intern = HashMap::new();

        let now = Instant::now();

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> = if let Some(p) = &path {
            Parser::new_from_source(expr_str, &mut intern, p.clone()).collect()
        } else {
            Parser::new(expr_str, &mut intern).collect()
        };

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Parsing Time: {:?}", now.elapsed());
        }

        let parsed = parsed?;

        // TODO fix this hack
        Ok(self
            .emit_instructions_from_exprs(parsed, path, constants)?
            .1)
    }

    pub fn emit_debug_instructions(
        &mut self,
        expr_str: &str,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut intern = HashMap::new();

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, &mut intern).collect();

        let parsed = parsed?;

        self.emit_debug_instructions_from_exprs(parsed, constants)
    }

    pub fn emit_expanded_ast(
        &mut self,
        expr_str: &str,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<Vec<ExprKind>> {
        let mut intern = HashMap::new();

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, &mut intern).collect();

        let parsed = parsed?;

        let expanded_statements = self.expand_expressions(parsed, None)?;

        let mut expanded_statements = expanded_statements;

        match self.opt_level {
            OptLevel::Three => loop {
                let mut manager = ConstantEvaluatorManager::new(constants.clone(), self.opt_level);
                expanded_statements = manager.run(expanded_statements)?;
                if !manager.changed {
                    break;
                }
            },
            OptLevel::Two => {
                expanded_statements =
                    ConstantEvaluatorManager::new(constants.clone(), self.opt_level)
                        .run(expanded_statements)?;
            }
            _ => {}
        }

        // let expanded_statements =
        //     ConstantEvaluatorManager::new(constants).run(expanded_statements)?;

        let expanded_statements = flatten_begins_and_expand_defines(expanded_statements);

        // TODO -> lambda lifting should be done here
        // Ok(expanded_statements)

        Ok(LambdaLifter::lift(expanded_statements))

        // self.emit_debug_instructions_from_exprs(parsed)
    }

    pub fn expand_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        #[cfg(feature = "modules")]
        return self
            .module_manager
            .compile_main(&mut self.macro_env, exprs, path);

        #[cfg(not(feature = "modules"))]
        self.module_manager
            .expand_expressions(&mut self.macro_env, exprs)
    }

    // TODO - lots of duplicate code here, clean this up as much as possible

    // This only works at the top level
    // structs then cannot work inside nested scoped
    pub fn extract_structs(
        &mut self,
        exprs: Vec<ExprKind>,
        results: &mut Vec<Vec<DenseInstruction>>,
    ) -> Result<Vec<ExprKind>> {
        let mut non_structs = Vec::new();
        let mut struct_instructions = Vec::new();
        for expr in exprs {
            if let ExprKind::Struct(s) = expr {
                let builder = StructFuncBuilder::generate_from_ast(&s)?;

                // Add the eventual function names to the symbol map
                let indices = self.symbol_map.insert_struct_function_names(&builder);

                // Get the value we're going to add to the constant map for eventual use
                // Throw the bindings in as well
                let constant_values = builder.to_constant_val(indices);
                let idx = self.constant_map.add_or_get(constant_values);

                struct_instructions
                    .push(vec![Instruction::new_struct(idx), Instruction::new_pop()]);
            } else {
                non_structs.push(expr);
            }
        }

        // TODO -> don't densify the results, push directly onto instruction set
        for instruction_set in struct_instructions {
            results.push(densify(instruction_set))
        }

        // for instruction in densify(struct_instructions) {
        //     results.push(vec![instruction])
        // }

        Ok(non_structs)
    }

    fn debug_extract_structs(
        &mut self,
        exprs: Vec<ExprKind>,
        results: &mut Vec<Vec<Instruction>>,
    ) -> Result<Vec<ExprKind>> {
        let mut non_structs = Vec::new();
        let mut struct_instructions = Vec::new();
        for expr in exprs {
            if let ExprKind::Struct(s) = expr {
                let builder = StructFuncBuilder::generate_from_ast(&s)?;

                // Add the eventual function names to the symbol map
                let indices = self.symbol_map.insert_struct_function_names(&builder);

                // Get the value we're going to add to the constant map for eventual use
                // Throw the bindings in as well
                let constant_values = builder.to_constant_val(indices);
                let idx = self.constant_map.add_or_get(constant_values);

                struct_instructions
                    .push(vec![Instruction::new_struct(idx), Instruction::new_pop()]);
            } else {
                non_structs.push(expr);
            }
        }

        for instruction_set in struct_instructions {
            results.push(instruction_set)
        }

        Ok(non_structs)
    }

    pub fn generate_dense_instructions(
        &mut self,
        expanded_statements: &[ExprKind],
        results: Vec<Vec<DenseInstruction>>,
    ) -> Result<Vec<Vec<DenseInstruction>>> {
        let now = Instant::now();

        let mut results = results;
        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();

        for expr in expanded_statements {
            // TODO add printing out the expression as its own special function
            // println!("{:?}", expr.to_string());
            // let mut instructions: Vec<Instruction> = Vec::new();

            let mut instructions =
                CodeGenerator::new(&mut self.constant_map).top_level_compile(expr)?;

            // TODO double check that arity map doesn't exist anymore
            // emit_loop(&expr, &mut instructions, None, &mut self.constant_map)?;

            // instructions.push(Instruction::new_pop());
            inject_heap_save_to_pop(&mut instructions);
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        convert_call_globals(&mut instruction_buffer);
        replace_defines_with_debruijn_indices(&mut instruction_buffer, &mut self.symbol_map)?;

        // TODO
        loop_condition_local_const_arity_two(&mut instruction_buffer);

        // convert_last_usages(&mut instruction_buffer);

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            // pretty_print_instructions(extracted.as_slice());

            // println!("{}", crate::core::instructions::disassemble(&extracted));

            results.push(densify(extracted));
        }

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Instruction generation time: {:?}", now.elapsed());
        }

        Ok(results)
    }

    fn generate_instructions_for_executable(
        &mut self,
        expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut results = Vec::new();
        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();

        for expr in expanded_statements {
            // TODO add printing out the expression as its own special function

            let mut instructions =
                CodeGenerator::new(&mut self.constant_map).top_level_compile(&expr)?;

            // instructions.push(Instruction::new_pop());
            inject_heap_save_to_pop(&mut instructions);
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            results.push(extracted);
        }

        Ok(results)
    }

    fn generate_debug_instructions(
        &mut self,
        expanded_statements: Vec<ExprKind>,
        results: Vec<Vec<Instruction>>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut results = results;
        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();

        for expr in expanded_statements {
            // TODO add printing out the expression as its own special function

            let mut instructions =
                CodeGenerator::new(&mut self.constant_map).top_level_compile(&expr)?;

            // instructions.push(Instruction::new_pop());
            inject_heap_save_to_pop(&mut instructions);
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        convert_call_globals(&mut instruction_buffer);
        replace_defines_with_debruijn_indices(&mut instruction_buffer, &mut self.symbol_map)?;

        // TODO
        loop_condition_local_const_arity_two(&mut instruction_buffer);

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
            // pretty_print_instructions(extracted.as_slice());
            results.push(extracted);
        }

        Ok(results)
    }

    // TODO
    // figure out how the symbols will work so that a raw program with symbols
    // can be later pulled in and symbols can be interned correctly
    fn compile_raw_program(
        &mut self,
        exprs: Vec<ExprKind>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<RawProgramWithSymbols> {
        let expanded_statements = self.expand_expressions(exprs, None)?;

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Generating instructions for the expression: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        let expanded_statements = self.apply_const_evaluation(constants, expanded_statements)?;

        debug!("About to expand defines");
        let expanded_statements = flatten_begins_and_expand_defines(expanded_statements);

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Successfully expanded defines: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        // TODO - make sure I want to keep this
        // let expanded_statements = ExpandMethodCalls::expand_methods(expanded_statements);

        // TODO
        let expanded_statements = LambdaLifter::lift(expanded_statements);

        // TODO - make sure I want to keep this
        let expanded_statements =
            MultipleArityFunctions::expand_multiple_arity_functions(expanded_statements);

        let mut struct_builders = StructBuilders::new();

        let expanded_statements =
            struct_builders.extract_structs_for_executable(expanded_statements)?;

        // TODO: Contract/Type Checking goes here
        // println!("---------------------------------------");

        // let collector = GlobalContractCollector::collect_contracts(&expanded_statements);

        // let mut checker = ContractChecker::new(collector);

        // if let Err(e) = checker.check(&expanded_statements) {
        //     log::error!("{:?}", e);
        // }

        // println!("{:?}", collector.names().collect::<Vec<_>>());

        // println!("---------------------------------------");

        let instructions = self.generate_instructions_for_executable(expanded_statements)?;

        let mut raw_program = RawProgramWithSymbols::new(
            struct_builders.builders,
            instructions,
            self.constant_map.clone(),
            "0.1.0".to_string(),
        );

        // Make sure to apply the peephole optimizations
        raw_program.apply_optimizations();

        Ok(raw_program)
    }

    fn emit_debug_instructions_from_exprs(
        &mut self,
        exprs: Vec<ExprKind>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut results = Vec::new();

        let expanded_statements = self.expand_expressions(exprs, None)?;

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Generating instructions for the expression: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        let expanded_statements = self.apply_const_evaluation(constants, expanded_statements)?;

        debug!("About to expand defines");
        let expanded_statements = flatten_begins_and_expand_defines(expanded_statements);

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Successfully expanded defines: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        // TODO - make sure I want to keep this
        // let expanded_statements = ExpandMethodCalls::expand_methods(expanded_statements);

        // TODO
        let expanded_statements = LambdaLifter::lift(expanded_statements);

        // TODO - make sure I want to keep this
        let expanded_statements =
            MultipleArityFunctions::expand_multiple_arity_functions(expanded_statements);

        let statements_without_structs =
            self.debug_extract_structs(expanded_statements, &mut results)?;

        self.generate_debug_instructions(statements_without_structs, results)
    }

    pub fn emit_instructions_from_exprs(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<String, SteelVal>,
    ) -> Result<(Vec<ExprKind>, Vec<Vec<DenseInstruction>>)> {
        let mut results = Vec::new();

        let now = Instant::now();

        let expanded_statements = self.expand_expressions(exprs, path)?;

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Macro Expansion Time: {:?}", now.elapsed());
        }

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Generating instructions for the expression: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        // let mut expanded_statements = expanded_statements;

        let expanded_statements = self.apply_const_evaluation(constants, expanded_statements)?;

        debug!("About to expand defines");
        let expanded_statements = flatten_begins_and_expand_defines(expanded_statements);

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Successfully expanded defines: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        // TODO - make sure I want to keep this
        // let expanded_statements = ExpandMethodCalls::expand_methods(expanded_statements);

        // TODO
        let expanded_statements = LambdaLifter::lift(expanded_statements);

        // TODO - make sure I want to keep this
        let expanded_statements =
            MultipleArityFunctions::expand_multiple_arity_functions(expanded_statements);

        let statements_without_structs = self.extract_structs(expanded_statements, &mut results)?;
        let dense_instructions =
            self.generate_dense_instructions(&statements_without_structs, results)?;

        Ok((statements_without_structs, dense_instructions))
    }

    fn apply_const_evaluation(
        &mut self,
        constants: ImmutableHashMap<String, SteelVal>,
        mut expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<ExprKind>> {
        let opt_time = Instant::now();

        match self.opt_level {
            // TODO
            // Cut this off at 10 iterations no matter what
            OptLevel::Three => {
                for _ in 0..10 {
                    let mut manager =
                        ConstantEvaluatorManager::new(constants.clone(), self.opt_level);
                    expanded_statements = manager.run(expanded_statements)?;

                    if !manager.changed {
                        break;
                    }
                }
            }
            OptLevel::Two => {
                expanded_statements =
                    ConstantEvaluatorManager::new(constants.clone(), self.opt_level)
                        .run(expanded_statements)?;
            }
            _ => {}
        }

        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(
                target: "pipeline_time",
                "Const Evaluation Time: {:?}",
                opt_time.elapsed()
            );
        };

        Ok(expanded_statements)
    }
}
