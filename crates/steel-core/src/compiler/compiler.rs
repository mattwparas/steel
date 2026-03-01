use crate::{
    compiler::{
        constants::ConstantMap,
        map::SymbolMap,
        modules::{MANGLER_PREFIX, MANGLER_SEPARATOR},
        passes::{
            analysis::SemanticAnalysis, begin::flatten_begins_and_expand_defines,
            opt::SingleExprOptimizer, shadow::RenameShadowedVariables, VisitorMutRefUnit,
            VisitorMutUnitRef,
        },
    },
    core::{instructions::u24, labels::Expr},
    gc::Shared,
    parser::{
        expand_visitor::{expand_kernel_in_env, expand_kernel_in_env_with_change, GlobalMap},
        interner::InternedString,
        kernel::Kernel,
        parser::{lower_entire_ast, lower_macro_and_require_definitions, SourcesCollector},
    },
    rvals::{AsRefSteelVal, SteelString},
    steel_vm::{cache::MemoizationTable, engine::ModuleContainer, primitives::constant_primitives},
    LambdaMetadataTable,
};
use crate::{
    core::{instructions::Instruction, opcode::OpCode},
    parser::parser::Sources,
};

use alloc::borrow::Cow;
use core::iter::Iterator;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use steel_parser::{ast::PROVIDE, span::Span};

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
    modules::{steel_search_dirs, CompiledModule, ModuleManager, SourceModuleResolver},
    passes::{analysis::Analysis, mangle::NameMangler},
    program::RawProgramWithSymbols,
};

use crate::values::HashMap as ImmutableHashMap;

#[cfg(feature = "profiling")]
use crate::time::Instant;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum DefineKind {
    Flat,
    Closure,
}

#[derive(Debug)]
struct FlatDefineLocation {
    kind: DefineKind,
    location: (usize, usize),
}

#[derive(Default)]
pub struct DebruijnIndicesInterner {
    flat_defines: HashMap<InternedString, FlatDefineLocation>,
    second_pass_defines: HashSet<InternedString>,
}

impl DebruijnIndicesInterner {
    pub fn collect_first_pass_defines(
        &mut self,
        index: usize,
        instructions: &mut [Instruction],
        symbol_map: &mut SymbolMap,
    ) -> Result<()> {
        let mut flat_defines_non_closure: HashSet<InternedString> = HashSet::default();

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
                    // Exiting a definition, clear it
                    flat_defines_non_closure.clear();

                    let idx = symbol_map.add(s);
                    self.flat_defines.insert(
                        s.to_owned(),
                        FlatDefineLocation {
                            kind: DefineKind::Closure,
                            location: (index, i),
                        },
                    );

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
                                span,
                                ..
                            })),
                        ..
                    },
                    Instruction {
                        op_code: OpCode::EDEF,
                        ..
                    },
                    ..,
                ) => {
                    let idx = symbol_map.add(s);
                    self.flat_defines.insert(
                        s.to_owned(),
                        FlatDefineLocation {
                            kind: DefineKind::Flat,
                            location: (index, i),
                        },
                    );

                    if flat_defines_non_closure.contains(s) {
                        stop!(BadSyntax => format!("Cannot reference identifier before its definition: {}", s.resolve()); *span);
                    }

                    flat_defines_non_closure.clear();

                    // self.flat_defines_idx.insert(idx);
                    // flat_defines_non_closure.insert(s.to_owned());

                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = u24::from_usize(idx);
                    }
                }
                (
                    Instruction {
                        op_code:
                            OpCode::CALLGLOBAL | OpCode::CALLGLOBALNOARITY | OpCode::CALLPRIMITIVE,
                        contents:
                            Some(Expr::Atom(SyntaxObject {
                                ty: TokenType::Identifier(s),
                                ..
                            })),
                        ..
                    },
                    ..,
                ) => {
                    // We're referencing things within the scope
                    flat_defines_non_closure.insert(*s);
                }

                _ => {}
            }
        }

        Ok(())
    }

    pub fn collect_second_pass_defines(
        &mut self,
        index: usize,
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

                    // let idx = symbol_map.get(s).unwrap();

                    // self.second_pass_defines_idx.insert(idx);
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
                    let flat_define = self.flat_defines.get(s);

                    if flat_define.is_some() && self.second_pass_defines.get(s).is_none() {
                        if depth == 0 {
                            let formatted = if s.resolve().starts_with(MANGLER_PREFIX) {
                                s.resolve()
                                    .split_once(MANGLER_SEPARATOR)
                                    .map(|x| x.1)
                                    .unwrap_or(s.resolve())
                            } else {
                                s.resolve()
                            };

                            let message = format!(
                                "Cannot reference an identifier before its definition: {formatted}"
                            );
                            stop!(FreeIdentifier => message; *span);
                        } else {
                            // If the depth is greater than 0, then we're in a weird spot.
                            // Consider the following:
                            //
                            // (define (foo)
                            //   (list bar 10))
                            // (define bar (foo))
                            //
                            // This should be disallowed, so we need to check if this
                            // definition came _before_ what we wanted.

                            let flat_define = flat_define.unwrap();

                            if flat_define.kind == DefineKind::Flat
                                && flat_define.location < (index, i)
                            {
                                let formatted = if s.resolve().starts_with(MANGLER_PREFIX) {
                                    s.resolve()
                                        .split_once(MANGLER_SEPARATOR)
                                        .map(|x| x.1)
                                        .unwrap_or(s.resolve())
                                } else {
                                    s.resolve()
                                };

                                let message = format!(
                                    "Cannot reference an identifier before its definition: {formatted}"
                                );
                                stop!(FreeIdentifier => message; *span);
                            }
                        }
                    }

                    let idx = symbol_map.get(s).map_err(|e| e.set_span(*span))?;

                    // TODO commenting this for now
                    if let Some(x) = instructions.get_mut(i) {
                        x.payload_size = u24::from_usize(idx);
                    }
                }
                Instruction {
                    op_code: OpCode::CALLGLOBAL | OpCode::CALLGLOBALNOARITY | OpCode::CALLPRIMITIVE,
                    contents:
                        Some(Expr::Atom(SyntaxObject {
                            ty: TokenType::Identifier(s),
                            span,
                            ..
                        })),
                    ..
                }
                | Instruction {
                    op_code: OpCode::CALLGLOBALTAIL | OpCode::CALLGLOBALTAILNOARITY,
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
                        let formatted = if s.resolve().starts_with(MANGLER_PREFIX) {
                            s.resolve()
                                .split_once(MANGLER_SEPARATOR)
                                .map(|x| x.1)
                                .unwrap_or(s.resolve())
                        } else {
                            s.resolve()
                        };

                        let message = format!(
                            "Cannot reference an identifier before its definition: {formatted}"
                        );
                        stop!(FreeIdentifier => message; *span);
                    }

                    if self.flat_defines.get(s).is_some()
                        && self.second_pass_defines.get(s).is_some()
                        && depth == 0
                    {
                        let location = self.flat_defines.get(s).unwrap();
                        if let DefineKind::Flat = location.kind {
                            if (index, i) < location.location {
                                let formatted = if s.resolve().starts_with(MANGLER_PREFIX) {
                                    s.resolve()
                                        .split_once(MANGLER_SEPARATOR)
                                        .map(|x| x.1)
                                        .unwrap_or(s.resolve())
                                } else {
                                    s.resolve()
                                };

                                let message = format!("Cannot reference an identifier before its definition: {formatted}");
                                stop!(FreeIdentifier => message; *span);
                            }
                        }
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

#[derive(Clone, Debug)]
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
    pub(crate) module_manager: ModuleManager,
    opt_level: OptLevel,
    pub(crate) kernel: Option<Kernel>,
    memoization_table: MemoizationTable,
    mangled_identifiers: FxHashSet<InternedString>,
    // Try this out?
    lifted_kernel_environments: HashMap<String, KernelDefMacroSpec>,
    // Macros that... we need to compile against directly at the top level
    // This is really just a hack, but it solves cases for interactively
    // running at the top level using private macros.
    lifted_macro_environments: HashMap<PathBuf, HashSet<InternedString>>,

    analysis: Analysis,
    shadowed_variable_renamer: RenameShadowedVariables,

    search_dirs: Vec<PathBuf>,

    // Include all the sources, module container, and constants
    // so that we can reference those at runtime. We probably should
    // just ignore the constants function in general. This unfortunately,
    // is under the hood, shared references to the engine, since we
    // want to have the compiler share everything with the runtime.
    pub(crate) sources: Sources,
    pub(crate) builtin_modules: ModuleContainer,
}

pub struct SerializableCompiler {
    pub(crate) symbol_map: SymbolMap,
    pub(crate) constant_map: SerializableConstantMap,
    pub(crate) macro_env: FxHashMap<InternedString, SteelMacro>,
    pub(crate) opt_level: OptLevel,
    pub(crate) module_manager: ModuleManager,
}

impl SerializableCompiler {
    #[allow(unused)]
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

pub(crate) enum StringOrSteelString {
    String(String),
    SteelString(SteelString),
}

impl Compiler {
    pub(crate) fn get_doc(&self, value: SteelVal) -> Option<StringOrSteelString> {
        #[cfg(not(feature = "sync"))]
        use crate::gc::shared::ShareableMut;

        for module in self.builtin_modules.inner().values() {
            let doc = module.search(value.clone());

            // If we found a doc on the fast path, just return
            if let Some(doc) = doc {
                let string = doc.doc.map(|x| x.0.to_string());
                return string.map(StringOrSteelString::String);
            }

            // If this is specifically the meta doc, we can attempt to
            // grab a quick one here
            if module.name().as_ref() == "steel/meta" {
                let table = module.try_get_ref("#%function-ptr-table").unwrap();
                let doc = LambdaMetadataTable::as_ref(&table).unwrap().get(value);
                return doc.map(StringOrSteelString::SteelString);
            }

            for (key, module_value) in module.module.read().values.iter() {
                if value.ptr_eq(module_value) {
                    let doc = module.get_documentation(key);
                    return doc.map(StringOrSteelString::String);
                }
            }
        }

        None
    }

    // A better solution _in general_ would be to replace the source
    // id with some kind of weak pointer. The problem there, is that
    // we don't have a perfect solution to converting from an expr
    // representation to a steel representation. Thus, we have to
    // do this walk, and occasionally have to do some kind of heuristic
    // for deciding when to clean incremental expressions.
    //
    // Note: We _probably_ also have to check the running bytecode?
    // Note sure how we'd do that besides doing a full walk. We can
    // probably just check the spans on the interner itself, but that would
    // require access to the runtime as well.
    pub(crate) fn gc_sources<'a>(
        &mut self,
        runtime_spans: impl Iterator<Item = &'a Shared<[Span]>>,
    ) {
        if self.sources.should_gc() {
            let mut sources = SourcesCollector::default();

            for spans in runtime_spans {
                for span in spans.iter() {
                    if let Some(source) = span.source_id() {
                        sources.add(source);
                    }
                }
            }

            // Visit all of the expressions, make sure those sources
            // don't get dropped. Figure out a better way to do this, but for
            // now we can just prune the expressions if it comes to that.
            // All the macros that exist in the top level macro environment
            for m in self.macro_env.values() {
                for expr in m.exprs() {
                    sources.visit(expr);
                }
            }

            // All the macros that exist in the modules macro environments
            for module in self.modules().values() {
                for expr in &module.ast {
                    sources.visit(expr);
                }

                for m in module.macro_map.values() {
                    for expr in m.exprs() {
                        sources.visit(expr);
                    }
                }
            }

            for expression in self
                .builtin_modules
                .inner()
                .values()
                .map(|x| x.cached_expression())
            {
                #[cfg(not(feature = "sync"))]
                use crate::gc::shared::ShareableMut;
                let expression = expression.read();
                if let Some(expression) = expression.as_ref() {
                    sources.visit(expression);
                }
            }

            self.sources.gc(sources.into_set());
            // println!("Time taken: {:?}", now.elapsed());
        }
    }

    #[allow(unused)]
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
            Sources::default(),
            ModuleContainer::default(),
        )
    }
}

impl Compiler {
    fn new(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: FxHashMap<InternedString, SteelMacro>,
        module_manager: ModuleManager,
        sources: Sources,
        builtin_modules: ModuleContainer,
    ) -> Compiler {
        // Include additional search directories by default
        let search_dirs = steel_search_dirs();

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
            lifted_macro_environments: HashMap::new(),
            analysis: Analysis::pre_allocated(),
            shadowed_variable_renamer: RenameShadowedVariables::default(),
            search_dirs,
            sources,
            builtin_modules,
        }
    }

    fn new_with_kernel(
        symbol_map: SymbolMap,
        constant_map: ConstantMap,
        macro_env: FxHashMap<InternedString, SteelMacro>,
        module_manager: ModuleManager,
        kernel: Kernel,
        sources: Sources,
        builtin_modules: ModuleContainer,
    ) -> Compiler {
        let search_dirs = steel_search_dirs();

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
            lifted_macro_environments: HashMap::new(),
            analysis: Analysis::pre_allocated(),
            shadowed_variable_renamer: RenameShadowedVariables::default(),
            search_dirs,
            sources,
            builtin_modules,
        }
    }

    pub fn default_with_kernel(sources: Sources, builtin_modules: ModuleContainer) -> Compiler {
        Compiler::new_with_kernel(
            SymbolMap::new(),
            ConstantMap::new(),
            FxHashMap::default(),
            ModuleManager::default(),
            Kernel::new(),
            sources,
            builtin_modules,
        )
    }

    pub(crate) fn default_without_kernel(
        sources: Sources,
        builtin_modules: ModuleContainer,
    ) -> Compiler {
        Compiler::new(
            SymbolMap::new(),
            ConstantMap::new(),
            FxHashMap::default(),
            ModuleManager::default(),
            sources,
            builtin_modules,
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

    pub fn register_source_module_resolver(
        &mut self,
        resolver: impl SourceModuleResolver + 'static,
    ) {
        self.module_manager.register_module_resolver(resolver);
    }

    pub fn add_search_directory(&mut self, dir: PathBuf) {
        self.search_dirs.push(dir);
    }

    pub fn compile_executable_from_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
    ) -> Result<RawProgramWithSymbols> {
        self.compile_raw_program(exprs, None)
    }

    pub fn compile_executable_from_expressions_from_path(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<RawProgramWithSymbols> {
        self.compile_raw_program(exprs, path)
    }

    pub fn compile_executable<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        expr_str: E,
        path: Option<PathBuf>,
    ) -> Result<RawProgramWithSymbols> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        let expr_str = expr_str.into();

        let id = self.sources.add_source(expr_str.clone(), path.clone());

        // Could fail here
        let parsed: core::result::Result<Vec<ExprKind>, ParseError> = path
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
        self.compile_raw_program(parsed?, path)
    }

    pub fn fully_expand_to_file<E: AsRef<str> + Into<Cow<'static, str>>>(
        &mut self,
        expr_str: E,
        path: Option<PathBuf>,
    ) -> Result<()> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        let expr_str = expr_str.into();

        let id = self.sources.add_source(expr_str.clone(), path.clone());

        // Could fail here
        let parsed: core::result::Result<Vec<ExprKind>, ParseError> = path
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

        self.expand_to_file(parsed?, path)
    }

    // TODO: Add a flag/function for parsing comments as well
    // Move the body of this function into the other one, so that way we have proper
    pub fn emit_expanded_ast(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        let id = self.sources.add_source(expr_str.to_string(), path.clone());

        // Could fail here
        let parsed: core::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, Some(id))
                .without_lowering()
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect();

        let parsed = parsed?;

        self.lower_expressions_impl(parsed, path)
    }

    pub fn emit_expanded_ast_without_optimizations(
        &mut self,
        expr_str: &str,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        let id = self.sources.add_source(expr_str.to_string(), path.clone());

        // Could fail here
        let parsed: core::result::Result<Vec<ExprKind>, ParseError> =
            Parser::new(expr_str, Some(id))
                .without_lowering()
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect();

        let parsed = parsed?;

        self.expand_ast(parsed, path)
    }

    pub fn compile_module(
        &mut self,
        path: PathBuf,
        builtin_modules: ModuleContainer,
    ) -> Result<()> {
        self.module_manager.add_module(
            path,
            &mut self.macro_env,
            &mut self.kernel,
            &mut self.sources,
            builtin_modules,
        )
    }

    pub fn modules(&self) -> &crate::HashMap<PathBuf, CompiledModule> {
        self.module_manager.modules()
    }

    pub fn expand_expressions(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        // #[cfg(feature = "modules")]
        self.module_manager.compile_main(
            &mut self.macro_env,
            &mut self.kernel,
            &mut self.sources,
            exprs,
            path,
            self.builtin_modules.clone(),
            &mut self.lifted_kernel_environments,
            &mut self.lifted_macro_environments,
            &self.search_dirs,
            &self.symbol_map.map(),
        )

        // #[cfg(not(feature = "modules"))]
        // self.module_manager
        //     .expand_expressions(&mut self.macro_env, exprs)
    }

    fn generate_instructions_for_executable(
        &mut self,
        mut expanded_statements: Vec<ExprKind>,
    ) -> Result<Vec<Vec<Instruction>>> {
        let mut results = Vec::with_capacity(expanded_statements.len());
        // let mut instruction_buffer = Vec::new();
        // let mut index_buffer = Vec::new();

        let analysis = {
            let mut analysis = core::mem::take(&mut self.analysis);

            analysis.fresh_from_exprs(&expanded_statements);
            analysis.populate_captures_twice(&expanded_statements);

            // let mut analysis = Analysis::from_exprs(&expanded_statements);
            // analysis.populate_captures(&expanded_statements);
            // analysis.populate_captures(&expanded_statements);
            analysis
        };

        let mut analysis = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);
        analysis.analyze_arity_checks()?;
        let analysis = analysis.into_analysis();

        for expr in expanded_statements {
            let instructions =
                super::code_gen::CodeGenerator::new(&mut self.constant_map, &analysis)
                    .top_level_compile(&expr)?;

            results.push(instructions);
        }

        // Push down the readable constant map
        self.constant_map.flush();

        // This... cannot be efficient?
        // for idx in index_buffer {
        //     let extracted: Vec<Instruction> = instruction_buffer.drain(0..idx).collect();
        //     results.push(extracted);
        // }

        self.analysis = analysis;

        Ok(results)
    }

    // TODO: Compare this to the lower_expressions_impl and merge the behavior
    fn expand_ast(&mut self, exprs: Vec<ExprKind>, path: Option<PathBuf>) -> Result<Vec<ExprKind>> {
        let mut expanded_statements = self.expand_expressions(exprs, path)?;

        log::debug!(target: "expansion-phase", "Expanding macros -> phase 1");

        if let Some(kernel) = self.kernel.as_mut() {
            // Label anything at the top as well - top level
            kernel.load_syntax_transformers(&mut expanded_statements, "top-level".to_string())?;
        }

        // for expr in expanded_statements.iter_mut() {
        //     expand_kernel_in_env(
        //         expr,
        //         self.kernel.as_mut(),
        //         self.builtin_modules.clone(),
        //         "top-level",
        //     )?;

        //     crate::parser::expand_visitor::expand(expr, &self.macro_env)?;
        // }

        for expr in expanded_statements.iter_mut() {
            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                self.builtin_modules.clone(),
                "top-level",
                GlobalMap::Map(self.symbol_map.map()),
            )?;
            crate::parser::expand_visitor::expand(
                expr,
                &self.macro_env,
                GlobalMap::Map(self.symbol_map.map()),
            )?;
            lower_entire_ast(expr)?;

            for (module, shadowed_vars) in &self.lifted_macro_environments {
                if let Some(macro_env) = self.modules().get(module).map(|x| &x.macro_map) {
                    let source_id = self.sources.get_source_id(module).unwrap();

                    crate::parser::expand_visitor::expand_with_source_id(
                        expr,
                        macro_env,
                        &shadowed_vars,
                        Some(source_id),
                        GlobalMap::Map(self.symbol_map.map()),
                    )?
                }
            }

            // Lift all of the kernel macros as well?
            for (module, lifted_env) in &mut self.lifted_kernel_environments {
                let changed = expand_kernel_in_env_with_change(
                    expr,
                    self.kernel.as_mut(),
                    self.builtin_modules.clone(),
                    module,
                    GlobalMap::Map(self.symbol_map.map()),
                )?;

                if changed {
                    lifted_env.name_mangler.visit(expr);
                }
            }

            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                self.builtin_modules.clone(),
                "top-level",
                GlobalMap::Map(self.symbol_map.map()),
            )?;

            // TODO: If we have this, then we have to lower all of the expressions again
            crate::parser::expand_visitor::expand(
                expr,
                &self.macro_env,
                GlobalMap::Map(self.symbol_map.map()),
            )?;

            // for expr in expanded_statements.iter_mut() {
            lower_entire_ast(expr)?;
        }

        // @Matt 11/15/2024
        // TODO: At this point, we'll want to remove
        // any remaining provide statements.

        // log::debug!(target: "expansion-phase", "Beginning constant folding");

        // Remove remaining provides from the top level.

        // let mut expanded_statements =
        // self.apply_const_evaluation(constants.clone(), expanded_statements, false)?;

        // expanded_statements.pretty_print();

        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, true);

        let expanded_statements = flatten_begins_and_expand_defines(expanded_statements)?;

        let mut expanded_statements = filter_provides(expanded_statements);

        let mut analysis = core::mem::take(&mut self.analysis);
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
            .remove_unused_globals_with_prefix(
                MANGLER_PREFIX,
                &self.macro_env,
                &self.module_manager,
            );
        // Don't do lambda lifting here
        // .lift_pure_local_functions()
        // .lift_all_local_functions();

        // debug!("About to expand defines");

        log::debug!(target: "expansion-phase", "Flattening begins, converting internal defines to let expressions");

        let mut analysis = semantic.into_analysis();

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
        // log::info!(target: "expansion-phase", "Aggressive constant evaluation with memoization");

        // Begin lowering anonymous function calls to lets

        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);
        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        semantic.replace_anonymous_function_calls_with_plain_lets();

        self.analysis = semantic.into_analysis();

        Ok(expanded_statements)
    }

    pub(crate) fn lower_expressions_impl(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<Vec<ExprKind>> {
        #[cfg(feature = "profiling")]
        let now = Instant::now();

        let mut expanded_statements = self.expand_expressions(exprs, path)?;

        // println!("---- Post expansion ----");
        // expanded_statements.pretty_print();
        // println!("{:#?}", expanded_statements);

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
                self.builtin_modules.clone(),
                "top-level",
                GlobalMap::Map(self.symbol_map.map()),
            )?;
            crate::parser::expand_visitor::expand(
                expr,
                &self.macro_env,
                GlobalMap::Map(self.symbol_map.map()),
            )?;
            lower_entire_ast(expr)?;

            for (module, shadowed_vars) in &self.lifted_macro_environments {
                if let Some(macro_env) = self.modules().get(module).map(|x| &x.macro_map) {
                    // If this was recently shadowed, then we don't want it any more.

                    let source_id = self.sources.get_source_id(module).unwrap();

                    crate::parser::expand_visitor::expand_with_source_id(
                        expr,
                        macro_env,
                        &shadowed_vars,
                        Some(source_id),
                        GlobalMap::Map(self.symbol_map.map()),
                    )?;
                }
            }

            // Lift all of the kernel macros as well?
            for (module, lifted_env) in &mut self.lifted_kernel_environments {
                let changed = expand_kernel_in_env_with_change(
                    expr,
                    self.kernel.as_mut(),
                    self.builtin_modules.clone(),
                    module,
                    GlobalMap::Map(self.symbol_map.map()),
                )?;

                if changed {
                    lifted_env.name_mangler.visit(expr);
                }
            }

            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                self.builtin_modules.clone(),
                "top-level",
                GlobalMap::Map(self.symbol_map.map()),
            )?;

            // TODO: If we have this, then we have to lower all of the expressions again
            crate::parser::expand_visitor::expand(
                expr,
                &self.macro_env,
                GlobalMap::Map(self.symbol_map.map()),
            )?;

            // for expr in expanded_statements.iter_mut() {
            lower_entire_ast(expr)?;
        }

        // TODO: Check that defines are in legal positions, post expansion.

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Top level macro expansion time: {:?}", now.elapsed());

        log::debug!(target: "expansion-phase", "Beginning constant folding");

        let expanded_statements =
            self.apply_const_evaluation(constant_primitives(), expanded_statements, false)?;

        let expanded_statements = flatten_begins_and_expand_defines(expanded_statements)?;

        // TODO: Move this to its own function
        let mut expanded_statements = filter_provides(expanded_statements);

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

        let mut analysis = core::mem::take(&mut self.analysis);

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
            .remove_unused_globals_with_prefix(
                MANGLER_PREFIX,
                &self.macro_env,
                &self.module_manager,
            )
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

        if std::env::var("STEEL_DEBUG_AST").is_ok() {
            steel_parser::ast::AstTools::pretty_print(&semantic.exprs);
        }

        semantic.replace_mutable_captured_variables_with_boxes();

        log::debug!(target: "expansion-phase", "Expanding multiple arity functions");

        let mut analysis = semantic.into_analysis();

        // Rename them again
        // RenameShadowedVariables::rename_shadowed_vars(&mut expanded_statements);
        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, false);

        // TODO - make sure I want to keep this
        // let mut expanded_statements =

        // log::info!(target: "expansion-phase", "Aggressive constant evaluation with memoization");

        // Begin lowering anonymous function calls to lets
        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);
        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        // Do this, and then inline everything. Do it again
        // TODO: Configure the amount that we inline?
        semantic.inline_function_calls(None)?;
        semantic.refresh_variables();

        let mut analysis = semantic.into_analysis();
        self.shadowed_variable_renamer
            .rename_shadowed_variables(&mut expanded_statements, false);

        analysis.fresh_from_exprs(&expanded_statements);
        analysis.populate_captures(&expanded_statements);
        // analysis.populate_captures(&expanded_statements);
        // Do this again
        let mut semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);

        semantic.replace_anonymous_function_calls_with_plain_lets();

        semantic.refresh_variables();

        // Lets see what this does...
        // semantic.analyze_arity_checks();

        // Flatten the empty lets
        // semantic.flatten_empty_lets();

        #[cfg(feature = "profiling")]
        log::info!(target: "pipeline_time", "CAT time: {:?}", now.elapsed());

        if std::env::var("STEEL_CLOSURE_LIFTING").is_ok() {
            semantic.lift_closures();
        }

        // self.shadowed_variable_renamer
        //     .rename_shadowed_variables(&mut semantic.exprs, false);

        // analysis.fresh_from_exprs(&expanded_statements);
        // semantic.analysis.fresh_from_exprs(&semantic.exprs);

        // semantic.fresh

        // TODO: Configure inlining function size

        // Loop unrolling. That is probably what we need?
        // Inlining?
        if std::env::var("STEEL_INLINE").is_ok() {
            semantic.inline_function_calls(Some(75))?;
            semantic.refresh_variables();
            let mut analysis = semantic.into_analysis();
            self.shadowed_variable_renamer
                .rename_shadowed_variables(&mut expanded_statements, false);
            analysis.fresh_from_exprs(&expanded_statements);
            analysis.populate_captures(&expanded_statements);
            // Do this again
            semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);
            semantic.replace_anonymous_function_calls_with_plain_lets();
        }

        if std::env::var("STEEL_INLINE_RECURSIVE").is_ok() {
            semantic.recursively_inline_function_calls(8)?;
            semantic.refresh_variables();
            let mut analysis = semantic.into_analysis();
            self.shadowed_variable_renamer
                .rename_shadowed_variables(&mut expanded_statements, false);
            analysis.fresh_from_exprs(&expanded_statements);
            analysis.populate_captures(&expanded_statements);
            // Do this again
            semantic = SemanticAnalysis::from_analysis(&mut expanded_statements, analysis);
            semantic.replace_anonymous_function_calls_with_plain_lets();
        }

        self.analysis = semantic.into_analysis();

        // We don't want to leave this allocate memory just hanging around, but leave enough for
        // interactive usages
        self.analysis.shrink_capacity();

        // Run const analysis one more time:

        let mut expanded_statements =
            self.apply_const_evaluation(constant_primitives(), expanded_statements, false)?;

        SingleExprOptimizer::run(&mut expanded_statements);

        // if std::env::var("STEEL_DEBUG_AST").is_ok() {
        //     steel_parser::ast::AstTools::pretty_print_log(&expanded_statements);
        // }

        Ok(expanded_statements)

        // Done lowering anonymous function calls to let
        // TODO: Re-enable this, but not in the repl. This repl causes... issues with the implementation
        // self.apply_const_evaluation(constant_primitives(), expanded_statements, true)
    }

    // Just write the fully expanded AST out. Then, read it in ~special~.
    // This will have to do a lot of noise in order to get things to work properly,
    // but its probably still faster than starting from scratch each time?
    //
    // Unknown.
    pub fn expand_to_file(&mut self, exprs: Vec<ExprKind>, path: Option<PathBuf>) -> Result<()> {
        let expanded_statements = self.lower_expressions_impl(exprs, path)?;
        let mut file = std::fs::File::create("fully-expanded.bin").unwrap();
        let buffer = bincode::serialize(&expanded_statements).unwrap();
        std::io::Write::write_all(&mut file, &buffer).unwrap();
        Ok(())
    }

    pub fn load_from_file(&mut self, path: &str) -> Result<RawProgramWithSymbols> {
        let contents = std::fs::read(path).unwrap();
        let expanded_statements = bincode::deserialize(&contents).unwrap();

        let instructions = self.generate_instructions_for_executable(expanded_statements)?;

        let mut raw_program = RawProgramWithSymbols::new(
            instructions,
            self.constant_map.clone(),
            "0.1.0".to_string(),
        );

        // Make sure to apply the peephole optimizations
        raw_program.apply_optimizations();

        // Lets see everything that gets run!
        // raw_program.debug_print_log();

        Ok(raw_program)
    }

    fn compile_raw_program_impl(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<RawProgramWithSymbols> {
        log::debug!(target: "expansion-phase", "Expanding macros -> phase 0");

        let expanded_statements = self.lower_expressions_impl(exprs, path)?;

        log::debug!(target: "expansion-phase", "Generating instructions");

        let instructions = self.generate_instructions_for_executable(expanded_statements)?;

        let mut raw_program = RawProgramWithSymbols::new(
            instructions,
            self.constant_map.clone(),
            "0.1.0".to_string(),
        );

        // Make sure to apply the peephole optimizations
        raw_program.apply_optimizations();

        // Lets see everything that gets run!
        // raw_program.debug_print_log();

        Ok(raw_program)
    }

    // TODO
    // figure out how the symbols will work so that a raw program with symbols
    // can be later pulled in and symbols can be interned correctly
    fn compile_raw_program(
        &mut self,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
    ) -> Result<RawProgramWithSymbols> {
        // Roll back any dependencies that got compiled, assuming they did.
        let snapshot_modules = self.module_manager.compiled_modules.clone();

        let res = self.compile_raw_program_impl(exprs, path);

        if res.is_err() {
            self.module_manager.compiled_modules = snapshot_modules;
            // Also rollback the metadata to match?
        }

        res
    }

    fn _run_const_evaluation_with_memoization(
        &mut self,
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
                for _ in 0..3 {
                    expanded_statements = manager.run(expanded_statements)?;

                    // if !manager.changed {
                    //     break;
                    // }

                    // manager.changed = false;
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

fn filter_provides(expanded_statements: Vec<ExprKind>) -> Vec<ExprKind> {
    expanded_statements
        .into_iter()
        .filter_map(|expr| match expr {
            ExprKind::Begin(mut b) => {
                let exprs = core::mem::take(&mut b.exprs);
                b.exprs = exprs
                    .into_iter()
                    .filter_map(|e| match e {
                        ExprKind::List(l) => {
                            if l.first_ident().copied() == Some(*PROVIDE) {
                                return None;
                            }
                            Some(ExprKind::List(l))
                        }
                        other => Some(other),
                    })
                    .collect();

                Some(ExprKind::Begin(b))
            }
            ExprKind::List(l) => {
                if l.first_ident().copied() == Some(*PROVIDE) {
                    return None;
                }
                Some(ExprKind::List(l))
            }
            other => Some(other),
        })
        .collect()
}
