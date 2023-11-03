#![allow(unused)]

use crate::{
    compiler::{
        // code_generator::{convert_call_globals, CodeGenerator},
        constants::ConstantMap,
        map::SymbolMap,
        passes::{
            analysis::SemanticAnalysis, begin::flatten_begins_and_expand_defines,
            reader::MultipleArityFunctions, shadow::RenameShadowedVariables,
        },
    },
    parser::{
        ast::AstTools, expand_visitor::expand_kernel, interner::InternedString, kernel::Kernel,
    },
    steel_vm::{builtin::BuiltInModule, cache::MemoizationTable, engine::ModuleContainer},
};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    parser::parser::Sources,
};

use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};
use std::{iter::Iterator, rc::Rc};

// TODO: Replace the usages of hashmap with this directly
use fxhash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::rvals::{Result, SteelVal};

use crate::parser::ast::ExprKind;
use crate::parser::expander::SteelMacro;
use crate::parser::parser::SyntaxObject;
use crate::parser::parser::{ParseError, Parser};
use crate::parser::tokens::TokenType;

// use crate::core::instructions::{densify, DenseInstruction};

use crate::stop;

use log::{debug, log_enabled};

use crate::steel_vm::const_evaluation::ConstantEvaluatorManager;

use super::{
    constants::SerializableConstantMap,
    modules::{CompiledModule, ModuleManager},
    passes::analysis::Analysis,
    program::RawProgramWithSymbols,
};

use im_rc::HashMap as ImmutableHashMap;

use std::time::Instant;

// use itertools::Itertools;

#[derive(Default)]
pub struct DebruijnIndicesInterner {
    flat_defines: HashSet<InternedString>,
    second_pass_defines: HashSet<InternedString>,
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
                    let idx = symbol_map.add(s);
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
                    let idx = symbol_map.add(s);
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
                    op_code: OpCode::SCLOSURE | OpCode::NEWSCLOSURE | OpCode::PUREFUNC,
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
                    op_code: OpCode::SET,
                    contents:
                        Some(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        }),
                    ..
                } => {
                    if self.flat_defines.get(s).is_some()
                        && self.second_pass_defines.get(s).is_none()
                        && depth == 0
                    {
                        let message =
                            format!("Cannot reference an identifier before its definition: {s}");
                        stop!(FreeIdentifier => message; *span);
                    }

                    let idx = symbol_map.get(s).map_err(|e| e.set_span(*span))?;

                    // TODO commenting this for now
                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = idx;
                        x.constant = false;
                    }
                }
                Instruction {
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
                } => {
                    if self.flat_defines.get(s).is_some()
                        && self.second_pass_defines.get(s).is_none()
                        && depth == 0
                    {
                        let message =
                            format!("Cannot reference an identifier before its definition: {s}");
                        stop!(FreeIdentifier => message; *span);
                    }

                    let idx = symbol_map.get(s).map_err(|e| e.set_span(*span))?;

                    // TODO commenting this for now
                    if let Some(x) = instructions.get_mut(i + 1) {
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

#[derive(Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum OptLevel {
    Zero = 0,
    One,
    Two,
    Three,
}

#[derive(Clone)]
pub struct Compiler {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: ConstantMap,
    pub(crate) macro_env: HashMap<InternedString, SteelMacro>,
    module_manager: ModuleManager,
    opt_level: OptLevel,
    pub(crate) kernel: Option<Kernel>,
    memoization_table: MemoizationTable,
    mangled_identifiers: HashSet<InternedString>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializableCompiler {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: SerializableConstantMap,
    pub(crate) macro_env: HashMap<InternedString, SteelMacro>,
    pub(crate) opt_level: OptLevel,
    pub(crate) module_manager: ModuleManager,
    // pub(crate) mangled_identifiers:
}

impl SerializableCompiler {
    pub(crate) fn into_compiler(self) -> Compiler {
        let mut compiler = Compiler::default();

        compiler.symbol_map = self.symbol_map;
        compiler.constant_map = ConstantMap::from_serialized(self.constant_map).unwrap();
        compiler.macro_env = self.macro_env;
        compiler.opt_level = self.opt_level;
        compiler.module_manager = self.module_manager;

        compiler
    }
}

impl Compiler {
    pub(crate) fn into_serializable_compiler(self) -> Result<SerializableCompiler> {
        Ok(SerializableCompiler {
            symbol_map: self.symbol_map,
            constant_map: self.constant_map.into_serializable_map(),
            macro_env: self.macro_env,
            opt_level: self.opt_level,
            module_manager: self.module_manager,
        })
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler::new(
            SymbolMap::new(),
            ConstantMap::new(),
            HashMap::new(),
            ModuleManager::default(),
        )
    }
}

impl Compiler {
    fn new(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: HashMap<InternedString, SteelMacro>,
        module_manager: ModuleManager,
    ) -> Compiler {
        Compiler {
            symbol_map,
            constant_map,
            macro_env,
            module_manager,
            opt_level: OptLevel::Three,
            kernel: None,
            memoization_table: MemoizationTable::new(),
            mangled_identifiers: HashSet::new(),
        }
    }

    fn new_with_kernel(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: HashMap<InternedString, SteelMacro>,
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
            memoization_table: MemoizationTable::new(),
            mangled_identifiers: HashSet::new(),
        }
    }

    pub(crate) fn default_from_kernel(kernel: Kernel) -> Compiler {
        Compiler::new_with_kernel(
            SymbolMap::new(),
            ConstantMap::new(),
            HashMap::new(),
            ModuleManager::default(),
            kernel,
        )
    }

    pub fn default_with_kernel() -> Compiler {
        Compiler::new_with_kernel(
            SymbolMap::new(),
            ConstantMap::new(),
            HashMap::new(),
            ModuleManager::default(),
            Kernel::new(),
        )
    }

    /// Registers a name in the underlying symbol map and returns the idx that it maps to
    pub fn register(&mut self, name: &str) -> usize {
        let spur = name.into();
        self.symbol_map.add(&spur)
    }

    /// Get the index associated with a name in the underlying symbol map
    /// If the name hasn't been registered, this will return `None`
    pub fn get_idx(&self, name: &str) -> Option<usize> {
        self.symbol_map.get(&InternedString::try_get(name)?).ok()
    }

    pub fn register_builtin(&mut self, name: String, contents: String) {
        self.module_manager.add_builtin_module(name, contents);
    }

    pub fn compile_executable_from_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        builtin_modules: ModuleContainer,
        constants: ImmutableHashMap<InternedString, SteelVal>,
        sources: &mut Sources,
    ) -> Result<RawProgramWithSymbols> {
        self.compile_raw_program(exprs, constants, builtin_modules, None, sources)
    }

    pub fn compile_executable(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<InternedString, SteelVal>,
        builtin_modules: ModuleContainer,
        sources: &mut Sources,
    ) -> Result<RawProgramWithSymbols> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        let id = sources.add_source(expr_str.to_string(), path.clone());

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> = if let Some(p) = &path {
            Parser::new_from_source(expr_str, p.clone(), Some(id)).collect()
        } else {
            Parser::new(expr_str, Some(id)).collect()
        };

        #[cfg(feature = "profiling")]
        if log_enabled!(target: "pipeline_time", log::Level::Debug) {
            debug!(target: "pipeline_time", "Parsing Time: {:?}", now.elapsed());
        }

        let parsed = parsed?;

        // TODO fix this hack
        self.compile_raw_program(parsed, constants, builtin_modules, path, sources)
    }

    // TODO: Add a flag/function for parsing comments as well
    // Move the body of this function into the other one, so that way we have proper
    pub fn emit_expanded_ast(
        &mut self,
        expr_str: &str,
        constants: ImmutableHashMap<InternedString, SteelVal>,
        path: Option<PathBuf>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<Vec<ExprKind>> {
        let id = sources.add_source(expr_str.to_string(), path.clone());

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, Some(id)).collect();

        let parsed = parsed?;

        let mut expanded_statements =
            self.expand_expressions(parsed, path, sources, builtin_modules.clone())?;

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Generating instructions for the expression: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        expanded_statements = expanded_statements
            .into_iter()
            .map(|x| expand_kernel(x, self.kernel.as_mut(), builtin_modules.clone()))
            .collect::<Result<Vec<_>>>()?;

        let mut expanded_statements =
            self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);

        let mut analysis = Analysis::from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        // let mut table = HashSet::new();

        // This is definitely broken still
        semantic
            .elide_single_argument_lambda_applications()
            // .lift_pure_local_functions()
            // .lift_all_local_functions()
            .replace_non_shadowed_globals_with_builtins(
                &mut self.macro_env,
                &mut self.module_manager,
                &mut self.mangled_identifiers,
            )
            .remove_unused_globals_with_prefix("mangler", &self.macro_env, &self.module_manager)
            .lift_pure_local_functions()
            .lift_all_local_functions();

        // TODO: Just run this... on each module in particular
        // .remove_unused_globals_with_prefix("mangler");

        debug!("About to expand defines");
        let mut expanded_statements = flatten_begins_and_expand_defines(expanded_statements);

        let mut analysis = Analysis::from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);
        semantic.refresh_variables();

        semantic.flatten_anonymous_functions();

        semantic.refresh_variables();

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
        let expanded_statements =
            MultipleArityFunctions::expand_multiple_arity_functions(expanded_statements);

        let mut expanded_statements =
            self.apply_const_evaluation(constants, expanded_statements, true)?;

        Ok(expanded_statements)
    }

    pub fn compile_module(
        &mut self,
        path: PathBuf,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<()> {
        self.module_manager.add_module(
            path,
            &mut self.macro_env,
            &mut self.kernel,
            sources,
            builtin_modules,
        )
    }

    pub fn modules(&self) -> &HashMap<PathBuf, CompiledModule> {
        self.module_manager.modules()
    }

    pub fn expand_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<Vec<ExprKind>> {
        #[cfg(feature = "modules")]
        return self.module_manager.compile_main(
            &mut self.macro_env,
            &mut self.kernel,
            sources,
            exprs,
            path,
            builtin_modules,
        );

        #[cfg(not(feature = "modules"))]
        self.module_manager
            .expand_expressions(&mut self.macro_env, exprs)
    }

    fn generate_instructions_for_executable(
        &mut self,
        expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut results = Vec::new();
        let mut instruction_buffer = Vec::new();
        let mut index_buffer = Vec::new();

        let analysis = {
            let mut analysis = Analysis::from_exprs(&expanded_statements);
            analysis.populate_captures(&expanded_statements);
            analysis.populate_captures(&expanded_statements);
            analysis
        };

        // expanded_statements.pretty_print();

        for expr in expanded_statements {
            let mut instructions =
                super::code_gen::CodeGenerator::new(&mut self.constant_map, &analysis)
                    .top_level_compile(&expr)?;

            // TODO: I don't think this needs to be here anymore
            // inject_heap_save_to_pop(&mut instructions);
            index_buffer.push(instructions.len());
            instruction_buffer.append(&mut instructions);
        }

        for idx in index_buffer {
            let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
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
        constants: ImmutableHashMap<InternedString, SteelVal>,
        builtin_modules: ModuleContainer,
        path: Option<PathBuf>,
        sources: &mut Sources,
    ) -> Result<RawProgramWithSymbols> {
        log::debug!(target: "expansion-phase", "Expanding macros -> phase 0");

        let mut expanded_statements =
            self.expand_expressions(exprs, path, sources, builtin_modules.clone())?;

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Generating instructions for the expression: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        log::debug!(target: "expansion-phase", "Expanding macros -> phase 1");

        expanded_statements = expanded_statements
            .into_iter()
            .map(|x| expand_kernel(x, self.kernel.as_mut(), builtin_modules.clone()))
            .collect::<Result<Vec<_>>>()?;

        log::debug!(target: "expansion-phase", "Beginning constant folding");

        let mut expanded_statements =
            self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);

        let mut analysis = Analysis::from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        // let mut table = HashSet::new();

        // This is definitely broken still
        semantic
            .elide_single_argument_lambda_applications()
            .replace_non_shadowed_globals_with_builtins(
                &mut self.macro_env,
                &mut self.module_manager,
                &mut self.mangled_identifiers,
            )
            // TODO: To get this to work, we have to check the macros to make sure those
            // are safe to eliminate. In interactive mode, we'll
            // be unable to optimize those away
            .remove_unused_globals_with_prefix("mangler", &self.macro_env, &self.module_manager)
            .lift_pure_local_functions()
            .lift_all_local_functions();
        // .remove_unused_globals_with_prefix("manglersteel/");

        // debug!("About to expand defines");

        log::debug!(target: "expansion-phase", "Flattening begins, converting internal defines to let expressions");

        let mut expanded_statements = flatten_begins_and_expand_defines(expanded_statements);

        // let mut expanded_statements =
        //     self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        let mut analysis = Analysis::from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);
        semantic.refresh_variables();

        semantic.flatten_anonymous_functions();

        semantic.refresh_variables();

        if log_enabled!(log::Level::Debug) {
            debug!(
                "Successfully expanded defines: {:?}",
                expanded_statements
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
            );
        }

        log::debug!(target: "expansion-phase", "Expanding multiple arity functions");

        // TODO - make sure I want to keep this
        let expanded_statements =
            MultipleArityFunctions::expand_multiple_arity_functions(expanded_statements);

        log::info!(target: "expansion-phase", "Aggressive constant evaluation with memoization");

        // let expanded_statements = expanded_statements
        //     .into_iter()
        //     .flat_map(|x| {
        //         if let ExprKind::Begin(b) = x {
        //             b.exprs.into_iter()
        //         } else {
        //             vec![x].into_iter()
        //         }
        //     })
        //     .collect();

        let expanded_statements =
            self.apply_const_evaluation(constants, expanded_statements, true)?;

        // let expanded_statements = expanded_statements
        //     .into_iter()
        //     .flat_map(|x| {
        //         if let ExprKind::Begin(b) = x {
        //             b.exprs.into_iter()
        //         } else {
        //             vec![x].into_iter()
        //         }
        //     })
        //     .collect();

        // TODO:
        // Here we're gonna do the constant evaluation pass, using the kernel for execution of the
        // constant functions w/ memoization:

        log::debug!(target: "expansion-phase", "Generating instructions");

        let instructions = self.generate_instructions_for_executable(expanded_statements)?;

        let mut raw_program = RawProgramWithSymbols::new(
            instructions,
            self.constant_map.clone(),
            "0.1.0".to_string(),
        );

        // Make sure to apply the peephole optimizations
        raw_program.apply_optimizations();

        Ok(raw_program)
    }

    fn run_const_evaluation_with_memoization(
        &mut self,
        constants: ImmutableHashMap<InternedString, SteelVal>,
        mut expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<ExprKind>> {
        todo!("Implement kernel level const evaluation here!")
    }

    fn apply_const_evaluation(
        &mut self,
        constants: ImmutableHashMap<InternedString, SteelVal>,
        mut expanded_statements: Vec<ExprKind>,
        use_kernel: bool,
    ) -> Result<Vec<ExprKind>> {
        #[cfg(feature = "profiling")]
        let opt_time = Instant::now();

        let mut maybe_kernel = None;

        if use_kernel {
            if let Some(kernel) = self.kernel.as_mut() {
                kernel.load_program_for_comptime(constants.clone(), &mut expanded_statements);
            }
        }

        match self.opt_level {
            // TODO
            // Cut this off at 10 iterations no matter what
            OptLevel::Three => {
                for _ in 0..10 {
                    let mut manager = ConstantEvaluatorManager::new(
                        &mut self.memoization_table,
                        constants.clone(),
                        self.opt_level,
                        if use_kernel {
                            &mut self.kernel
                        } else {
                            &mut maybe_kernel
                        },
                    );
                    expanded_statements = manager.run(expanded_statements)?;

                    if !manager.changed {
                        break;
                    }
                }
            }
            OptLevel::Two => {
                expanded_statements = ConstantEvaluatorManager::new(
                    &mut self.memoization_table,
                    constants,
                    self.opt_level,
                    if use_kernel {
                        &mut self.kernel
                    } else {
                        &mut maybe_kernel
                    },
                )
                .run(expanded_statements)?;
            }
            _ => {}
        }

        #[cfg(feature = "profiling")]
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
