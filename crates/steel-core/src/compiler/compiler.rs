use crate::{
    compiler::{
        constants::ConstantMap,
        map::SymbolMap,
        passes::{
            analysis::SemanticAnalysis, begin::flatten_begins_and_expand_defines,
            shadow::RenameShadowedVariables, VisitorMutRefUnit,
        },
    },
    core::{instructions::u24, labels::Expr},
    parser::{
        expand_visitor::{expand_kernel_in_env, expand_kernel_in_env_with_change},
        interner::InternedString,
        kernel::Kernel,
        parser::{lower_entire_ast, lower_macro_and_require_definitions},
    },
    steel_vm::{cache::MemoizationTable, engine::ModuleContainer},
};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    parser::parser::Sources,
};

use std::{borrow::Cow, iter::Iterator};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

// TODO: Replace the usages of hashmap with this directly
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};

use crate::rvals::{Result, SteelVal};

use crate::parser::ast::ExprKind;
use crate::parser::expander::SteelMacro;
use crate::parser::parser::SyntaxObject;
use crate::parser::parser::{ParseError, Parser};
use crate::parser::tokens::TokenType;

// use crate::core::instructions::{densify, DenseInstruction};

use crate::stop;

use crate::steel_vm::const_evaluation::ConstantEvaluatorManager;

use super::{
    constants::SerializableConstantMap,
    modules::{CompiledModule, ModuleManager},
    passes::{analysis::Analysis, mangle::NameMangler},
    program::RawProgramWithSymbols,
};

use im_rc::HashMap as ImmutableHashMap;

#[cfg(feature = "profiling")]
use std::time::Instant;

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
                            Some(Expr::Atom(SyntaxObject {
                                ty: TokenType::Identifier(s),
                                // span,
                                ..
                            })),
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
                    // if !self.flat_defines.insert(s.to_owned()) {
                    //     stop!(BadSyntax => format!("Cannot redefine define within the same scope: {}", s); *span);
                    // }

                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = u24::from_usize(idx);
                    }
                }
                (
                    Instruction {
                        op_code: OpCode::BIND,
                        contents:
                            Some(Expr::Atom(SyntaxObject {
                                ty: TokenType::Identifier(s),
                                // span,
                                ..
                            })),
                        ..
                    },
                    ..,
                ) => {
                    let idx = symbol_map.add(s);
                    self.flat_defines.insert(s.to_owned());
                    // if !self.flat_defines.insert(s.to_owned()) {
                    //     stop!(BadSyntax => format!("Cannot redefine define within the same scope: {}", s); *span);
                    // }

                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = u24::from_usize(idx);
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
                    op_code: OpCode::BEGINSCOPE,
                    ..
                } => {
                    depth += 1;
                }
                Instruction {
                    op_code: OpCode::LETENDSCOPE,
                    ..
                } => {
                    depth -= 1;
                }
                Instruction {
                    op_code: OpCode::BIND,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            ..
                        })),
                    ..
                } => {
                    // Keep track of where the defines actually are in the process
                    self.second_pass_defines.insert(s.to_owned());
                }
                Instruction {
                    op_code: OpCode::PUSH,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        })),
                    ..
                }
                | Instruction {
                    op_code: OpCode::SET,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        })),
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
                        x.payload_size = u24::from_usize(idx);
                    }
                }
                Instruction {
                    op_code: OpCode::CALLGLOBAL,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        })),
                    ..
                }
                | Instruction {
                    op_code: OpCode::CALLGLOBALTAIL,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        })),
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
                        x.payload_size = u24::from_usize(idx);
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
pub(crate) struct KernelDefMacroSpec {
    pub(crate) _env: String,
    pub(crate) _exported: Option<HashSet<InternedString>>,
    pub(crate) name_mangler: NameMangler,
}

#[derive(Clone)]
pub struct Compiler {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: ConstantMap,
    pub(crate) macro_env: FxHashMap<InternedString, SteelMacro>,
    module_manager: ModuleManager,
    opt_level: OptLevel,
    pub(crate) kernel: Option<Kernel>,
    memoization_table: MemoizationTable,
    mangled_identifiers: FxHashSet<InternedString>,
    // Try this out?
    lifted_kernel_environments: HashMap<String, KernelDefMacroSpec>,
    // Macros that... we need to compile against directly at the top level
    // This is really just a hack, but it solves cases for interactively
    // running at the top level using private macros.
    lifted_macro_environments: HashSet<PathBuf>,

    analysis: Analysis,
    shadowed_variable_renamer: RenameShadowedVariables,

    search_dirs: Vec<PathBuf>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializableCompiler {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: SerializableConstantMap,
    pub(crate) macro_env: FxHashMap<InternedString, SteelMacro>,
    pub(crate) opt_level: OptLevel,
    pub(crate) module_manager: ModuleManager,
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
            FxHashMap::default(),
            ModuleManager::default(),
        )
    }
}

impl Compiler {
    fn new(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: FxHashMap<InternedString, SteelMacro>,
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
            mangled_identifiers: FxHashSet::default(),
            lifted_kernel_environments: HashMap::new(),
            lifted_macro_environments: HashSet::new(),
            analysis: Analysis::pre_allocated(),
            shadowed_variable_renamer: RenameShadowedVariables::default(),
            search_dirs: Vec::new(),
        }
    }

    fn new_with_kernel(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: FxHashMap<InternedString, SteelMacro>,
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
            mangled_identifiers: FxHashSet::default(),
            lifted_kernel_environments: HashMap::new(),
            lifted_macro_environments: HashSet::new(),
            analysis: Analysis::pre_allocated(),
            shadowed_variable_renamer: RenameShadowedVariables::default(),
            search_dirs: Vec::new(),
        }
    }

    pub(crate) fn _default_from_kernel(kernel: Kernel) -> Compiler {
        Compiler::new_with_kernel(
            SymbolMap::new(),
            ConstantMap::new(),
            FxHashMap::default(),
            ModuleManager::default(),
            kernel,
        )
    }

    pub fn default_with_kernel() -> Compiler {
        Compiler::new_with_kernel(
            SymbolMap::new(),
            ConstantMap::new(),
            FxHashMap::default(),
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

    pub fn add_search_directory(&mut self, dir: PathBuf) {
        self.search_dirs.push(dir);
    }

    pub fn compile_executable_from_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        builtin_modules: ModuleContainer,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        sources: &mut Sources,
    ) -> Result<RawProgramWithSymbols> {
        self.compile_raw_program(exprs, constants, builtin_modules, None, sources)
    }

    pub fn compile_executable<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        expr_str: E,
        path: Option<PathBuf>,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        builtin_modules: ModuleContainer,
        sources: &mut Sources,
    ) -> Result<RawProgramWithSymbols> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        let expr_str = expr_str.into();

        let id = sources.add_source(expr_str.clone(), path.clone());

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> = path
            .as_ref()
            .map(|p| Parser::new_from_source(expr_str.as_ref(), p.clone(), Some(id)))
            .unwrap_or_else(|| Parser::new(expr_str.as_ref(), Some(id)))
            .without_lowering()
            .map(|x| x.and_then(lower_macro_and_require_definitions))
            .collect();

        #[cfg(feature = "profiling")]
        if log::log_enabled!(target: "pipeline_time", log::Level::Debug) {
            log::debug!(target: "pipeline_time", "Parsing Time: {:?}", now.elapsed());
        }

        // TODO fix this hack
        self.compile_raw_program(parsed?, constants, builtin_modules, path, sources)
    }

    // TODO: Add a flag/function for parsing comments as well
    // Move the body of this function into the other one, so that way we have proper
    pub fn emit_expanded_ast(
        &mut self,
        expr_str: &str,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        path: Option<PathBuf>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<Vec<ExprKind>> {
        let id = sources.add_source(expr_str.to_string(), path.clone());

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, Some(id))
                .without_lowering()
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect();

        let parsed = parsed?;

        self.lower_expressions_impl(parsed, constants, builtin_modules, path, sources)
    }

    pub fn emit_expanded_ast_without_optimizations(
        &mut self,
        expr_str: &str,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        path: Option<PathBuf>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<Vec<ExprKind>> {
        let id = sources.add_source(expr_str.to_string(), path.clone());

        // Could fail here
        let parsed: std::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, Some(id))
                .without_lowering()
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect();

        let parsed = parsed?;

        self.expand_ast(parsed, constants, builtin_modules, path, sources)
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

    pub fn modules(&self) -> &FxHashMap<PathBuf, CompiledModule> {
        self.module_manager.modules()
    }

    pub fn expand_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<Vec<ExprKind>> {
        // #[cfg(feature = "modules")]
        return self.module_manager.compile_main(
            &mut self.macro_env,
            &mut self.kernel,
            sources,
            exprs,
            path,
            builtin_modules,
            &mut self.lifted_kernel_environments,
            &mut self.lifted_macro_environments,
            &self.search_dirs,
        );

        // #[cfg(not(feature = "modules"))]
        // self.module_manager
        //     .expand_expressions(&mut self.macro_env, exprs)
    }

    fn generate_instructions_for_executable(
        &mut self,
        expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut results = Vec::with_capacity(expanded_statements.len());
        // let mut instruction_buffer = Vec::new();
        // let mut index_buffer = Vec::new();

        let analysis = {
            let mut analysis = std::mem::take(&mut self.analysis);

            analysis.fresh_from_exprs(&expanded_statements);
            analysis.populate_captures_twice(&expanded_statements);

            // let mut analysis = Analysis::from_exprs(&expanded_statements);
            // analysis.populate_captures(&expanded_statements);
            // analysis.populate_captures(&expanded_statements);
            analysis
        };

        for expr in expanded_statements {
            let instructions =
                super::code_gen::CodeGenerator::new(&mut self.constant_map, &analysis)
                    .top_level_compile(&expr)?;

            results.push(instructions);
        }

        // This... cannot be efficient?
        // for idx in index_buffer {
        //     let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
        //     results.push(extracted);
        // }

        self.analysis = analysis;

        Ok(results)
    }

    fn expand_ast(
        &mut self,
        exprs: Vec<ExprKind>,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        builtin_modules: ModuleContainer,
        path: Option<PathBuf>,
        sources: &mut Sources,
    ) -> Result<Vec<ExprKind>> {
        let mut expanded_statements =
            self.expand_expressions(exprs, path, sources, builtin_modules.clone())?;

        log::debug!(target: "expansion-phase", "Expanding macros -> phase 1");

        if let Some(kernel) = self.kernel.as_mut() {
            // Label anything at the top as well - top level
            kernel.load_syntax_transformers(&mut expanded_statements, "top-level".to_string())?;
        }

        for expr in expanded_statements.iter_mut() {
            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                builtin_modules.clone(),
                "top-level",
            )?;

            crate::parser::expand_visitor::expand(expr, &self.macro_env)?;
        }

        for expr in expanded_statements.iter_mut() {
            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                builtin_modules.clone(),
                "top-level",
            )?;
            crate::parser::expand_visitor::expand(expr, &self.macro_env)?;
            lower_entire_ast(expr)?;

            for module in &self.lifted_macro_environments {
                if let Some(macro_env) = self.modules().get(module).map(|x| &x.macro_map) {
                    let source_id = sources.get_source_id(module).unwrap();

                    // println!("Expanding macros from: {:?}", module);

                    crate::parser::expand_visitor::expand_with_source_id(
                        expr,
                        macro_env,
                        Some(source_id),
                    )?

                    // crate::parser::expand_visitor::expand(expr, macro_env)?
                }
            }

            // Lift all of the kernel macros as well?
            for (module, lifted_env) in &mut self.lifted_kernel_environments {
                let changed = expand_kernel_in_env_with_change(
                    expr,
                    self.kernel.as_mut(),
                    builtin_modules.clone(),
                    &module,
                )?;

                if changed {
                    lifted_env.name_mangler.visit(expr);
                }
            }

            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                builtin_modules.clone(),
                "top-level",
            )?;

            // TODO: If we have this, then we have to lower all of the expressions again
            crate::parser::expand_visitor::expand(expr, &self.macro_env)?;

            // for expr in expanded_statements.iter_mut() {
            lower_entire_ast(expr)?;
        }

        log::debug!(target: "expansion-phase", "Beginning constant folding");

        let mut expanded_statements =
            self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        // expanded_statements.pretty_print();

        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, true);

        let mut analysis = std::mem::take(&mut self.analysis);
        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

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
            .remove_unused_globals_with_prefix("mangler", &self.macro_env, &self.module_manager);
        // Don't do lambda lifting here
        // .lift_pure_local_functions()
        // .lift_all_local_functions();

        // debug!("About to expand defines");

        log::debug!(target: "expansion-phase", "Flattening begins, converting internal defines to let expressions");

        let mut analysis = semantic.into_analysis();

        let mut expanded_statements = flatten_begins_and_expand_defines(expanded_statements)?;

        // After define expansion, we'll want this
        // RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);

        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, false);

        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        semantic.check_if_values_are_redefined()?;

        semantic.refresh_variables();
        semantic.flatten_anonymous_functions();
        semantic.refresh_variables();

        // Replace mutation with boxes

        semantic.populate_captures_twice();

        semantic.replace_mutable_captured_variables_with_boxes();

        log::debug!(target: "expansion-phase", "Expanding multiple arity functions");

        let mut analysis = semantic.into_analysis();

        // Rename them again
        // RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);

        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, false);

        // let mut expanded_statements =
        log::info!(target: "expansion-phase", "Aggressive constant evaluation with memoization");

        // Begin lowering anonymous function calls to lets

        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);
        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        semantic.replace_anonymous_function_calls_with_plain_lets();

        self.analysis = semantic.into_analysis();

        Ok(expanded_statements)
    }

    fn lower_expressions_impl(
        &mut self,
        exprs: Vec<ExprKind>,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        builtin_modules: ModuleContainer,
        path: Option<PathBuf>,
        sources: &mut Sources,
    ) -> Result<Vec<ExprKind>> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        // println!("Before expanding macros");
        // exprs.pretty_print();

        let mut expanded_statements =
            self.expand_expressions(exprs, path, sources, builtin_modules.clone())?;

        // println!("After expanding macros");
        // expanded_statements.pretty_print();

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Phase 1 module expansion time: {:?}", now.elapsed());

        #[cfg(feature = "profiling")]
        let now = Instant::now();

        log::debug!(target: "expansion-phase", "Expanding macros -> phase 1");

        if let Some(kernel) = self.kernel.as_mut() {
            // Label anything at the top as well - top level
            kernel.load_syntax_transformers(&mut expanded_statements, "top-level".to_string())?;
        }

        for expr in expanded_statements.iter_mut() {
            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                builtin_modules.clone(),
                "top-level",
            )?;
            crate::parser::expand_visitor::expand(expr, &self.macro_env)?;
            lower_entire_ast(expr)?;

            for module in &self.lifted_macro_environments {
                if let Some(macro_env) = self.modules().get(module).map(|x| &x.macro_map) {
                    let source_id = sources.get_source_id(module).unwrap();

                    crate::parser::expand_visitor::expand_with_source_id(
                        expr,
                        macro_env,
                        Some(source_id),
                    )?;
                }
            }

            // Lift all of the kernel macros as well?
            for (module, lifted_env) in &mut self.lifted_kernel_environments {
                let changed = expand_kernel_in_env_with_change(
                    expr,
                    self.kernel.as_mut(),
                    builtin_modules.clone(),
                    &module,
                )?;

                if changed {
                    lifted_env.name_mangler.visit(expr);
                }
            }

            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                builtin_modules.clone(),
                "top-level",
            )?;

            // TODO: If we have this, then we have to lower all of the expressions again
            crate::parser::expand_visitor::expand(expr, &self.macro_env)?;

            // for expr in expanded_statements.iter_mut() {
            lower_entire_ast(expr)?;
        }

        // TODO: Check that defines are in legal positions, post expansion.

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Top level macro expansion time: {:?}", now.elapsed());

        log::debug!(target: "expansion-phase", "Beginning constant folding");

        // expanded_statements.pretty_print();

        let expanded_statements =
            self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        let mut expanded_statements = flatten_begins_and_expand_defines(expanded_statements)?;

        #[cfg(feature = "profiling")]
        let now = Instant::now();

        // println!("--- Before renaming: ----");
        // expanded_statements.pretty_print();

        // TODO: Probably lift this above the const evaluation anyway
        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, true);

        // println!("--- After renaming: ----");
        // expanded_statements.pretty_print();

        // let mut expanded_statements =
        //     self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        let mut analysis = std::mem::take(&mut self.analysis);

        // Pre populate the analysis here
        analysis.fresh_from_exprs(&expanded_statements);

        // let mut analysis = Analysis::from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

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

        // debug!("About to expand defines");

        log::debug!(target: "expansion-phase", "Flattening begins, converting internal defines to let expressions");

        let mut analysis = semantic.into_analysis();

        let mut expanded_statements = flatten_begins_and_expand_defines(expanded_statements)?;

        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, false);

        // After define expansion, we'll want this
        // RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);

        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);

        // expanded_statements.pretty_print();

        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        // Check if the values are redefined!
        semantic.check_if_values_are_redefined()?;

        // semantic.refresh_variables();
        semantic.flatten_anonymous_functions();
        semantic.refresh_variables();

        // Replace mutation with boxes
        semantic.populate_captures();
        semantic.populate_captures();

        semantic.replace_mutable_captured_variables_with_boxes();

        log::debug!(target: "expansion-phase", "Expanding multiple arity functions");

        let mut analysis = semantic.into_analysis();

        // Rename them again
        // RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);
        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, false);

        // TODO - make sure I want to keep this
        // let mut expanded_statements =

        log::info!(target: "expansion-phase", "Aggressive constant evaluation with memoization");

        // Begin lowering anonymous function calls to lets

        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);
        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);
        // semantic.populate_captures();

        semantic.replace_anonymous_function_calls_with_plain_lets();

        #[cfg(feature = "profiling")]
        log::info!(target: "pipeline_time", "CAT time: {:?}", now.elapsed());

        self.analysis = semantic.into_analysis();

        // We don't want to leave this allocate memory just hanging around, but leave enough for
        // interactive usages
        self.analysis.shrink_capacity();

        Ok(expanded_statements)

        // Done lowering anonymous function calls to let
        // TODO: Re-enable this, but not in the repl. This repl causes... issues with the implementation
        // self.apply_const_evaluation(constants, expanded_statements, true)
    }

    // TODO
    // figure out how the symbols will work so that a raw program with symbols
    // can be later pulled in and symbols can be interned correctly
    fn compile_raw_program(
        &mut self,
        exprs: Vec<ExprKind>,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        builtin_modules: ModuleContainer,
        path: Option<PathBuf>,
        sources: &mut Sources,
    ) -> Result<RawProgramWithSymbols> {
        log::debug!(target: "expansion-phase", "Expanding macros -> phase 0");

        let expanded_statements =
            self.lower_expressions_impl(exprs, constants, builtin_modules, path, sources)?;

        // println!("--- Final AST ---");
        // println!("");
        // expanded_statements.pretty_print();

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

    fn _run_const_evaluation_with_memoization(
        &mut self,
        _constants: ImmutableHashMap<InternedString, SteelVal>,
        mut _expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<ExprKind>> {
        todo!("Implement kernel level const evaluation here!")
    }

    fn apply_const_evaluation(
        &mut self,
        constants: ImmutableHashMap<InternedString, SteelVal, FxBuildHasher>,
        mut expanded_statements: Vec<ExprKind>,
        use_kernel: bool,
    ) -> Result<Vec<ExprKind>> {
        #[cfg(feature = "profiling")]
        let opt_time = Instant::now();

        let mut maybe_kernel = None;

        if use_kernel {
            if let Some(kernel) = self.kernel.as_mut() {
                kernel.load_program_for_comptime(constants.clone(), &mut expanded_statements)?;
            }
        }

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

        match self.opt_level {
            // TODO
            // Cut this off at 10 iterations no matter what
            OptLevel::Three => {
                // for _ in 0..10 {
                // println!("Running const evaluation");

                expanded_statements = manager.run(expanded_statements)?;

                // if !manager.changed {
                //     break;
                // }

                // manager.changed = false;
                // }
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
        if log::log_enabled!(target: "pipeline_time", log::Level::Debug) {
            log::debug!(
                target: "pipeline_time",
                "Const Evaluation Time: {:?}",
                opt_time.elapsed()
            );
        };

        Ok(expanded_statements)
    }
}
