use crate::{
    compiler::{passes::VisitorMutRefUnit, program::PROVIDE},
    parser::{
        ast::{Atom, Begin, Define, ExprKind, List, Quote},
        expand_visitor::{expand_kernel_in_env, expand_kernel_in_env_with_change},
        interner::InternedString,
        kernel::Kernel,
        parser::{
            lower_entire_ast, lower_macro_and_require_definitions, ParseError, Parser, Sources,
            SyntaxObject,
        },
        tokens::TokenType,
    },
    steel_vm::{
        engine::{default_prelude_macros, ModuleContainer},
        transducers::interleave,
    },
};
use crate::{parser::expand_visitor::Expander, rvals::Result};

use compact_str::CompactString;
use fxhash::{FxHashMap, FxHashSet};
use once_cell::sync::Lazy;
// use smallvec::SmallVec;
use steel_parser::{ast::PROTO_HASH_GET, expr_list, parser::SourceId, span::Span};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    io::Read,
    path::PathBuf,
    sync::Arc,
};

use crate::parser::expander::SteelMacro;
use crate::stop;

use std::time::SystemTime;

use crate::parser::expand_visitor::{expand, extract_macro_defs};

use super::{
    compiler::KernelDefMacroSpec,
    passes::{
        analysis::is_a_builtin_definition,
        begin::FlattenBegin,
        mangle::{collect_globals, NameMangler},
    },
    program::{FOR_SYNTAX, ONLY_IN, PREFIX_IN, REQUIRE_IDENT_SPEC},
};

macro_rules! time {
    ($label:expr, $e:expr) => {{
        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let e = $e;

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "{}: {:?}", $label, now.elapsed());

        e
    }};
}

macro_rules! declare_builtins {
    ( $( $name:expr => $path:expr ), *) => {
        static BUILT_INS: &[(&str, &str)] = &[
            $( ($name, include_str!($path)), )*
        ];

        pub(crate) fn intern_modules() {
            $(
                let _ = InternedString::from($name);
                let _ = InternedString::from($path);
            )*
        }
    };
}

pub(crate) const MANGLER_SEPARATOR: &str = "__%#__";

macro_rules! create_prelude {
    (
        $( $module:literal, )*
        $( for_syntax $module_for_syntax:literal ),*
    ) => {

        pub static PRELUDE_WITHOUT_BASE: &str = concat!(
            $( "(require \"", $module, "\")\n", )*
            $( "(require (for-syntax \"", $module_for_syntax, "\"))\n", )*
        );

        pub static PRELUDE_STRING: &str = concat!(
            "(require-builtin steel/base)\n",
            $( "(require \"", $module, "\")\n", )*
            $( "(require (for-syntax \"", $module_for_syntax, "\"))\n", )*
        );
    }
}

// TODO: These need to be set up and interned in a stable position.
declare_builtins!(
    "steel/option" => "../scheme/modules/option.scm",
    "steel/result" => "../scheme/modules/result.scm",
    "steel/iterators" => "../scheme/modules/iterators.scm",
    "steel/mutable-vectors" => "../scheme/modules/mvector.scm",
    "steel/async" => "../scheme/modules/async.scm",
    "steel/sync" => "../scheme/modules/sync.scm",
    "#%private/steel/contract" => "../scheme/modules/contracts.scm",
    "#%private/steel/print" => "../scheme/print.scm",
    "#%private/steel/ports" => "../scheme/modules/ports.scm",
    "#%private/steel/control" => "../scheme/modules/parameters.scm",
    "#%private/steel/reader" => "../scheme/modules/reader.scm",
    "#%private/steel/stdlib" => "../scheme/stdlib.scm",
    "#%private/steel/match" => "../scheme/modules/match.scm"
);

create_prelude!(
    "#%private/steel/control",
    "#%private/steel/contract",
    "#%private/steel/print",
    "#%private/steel/ports",
    "#%private/steel/reader",
    "#%private/steel/match",
    for_syntax "#%private/steel/control",
    for_syntax "#%private/steel/contract"
);

#[cfg(not(target_arch = "wasm32"))]
pub static STEEL_HOME: Lazy<Option<String>> = Lazy::new(|| {
    std::env::var("STEEL_HOME").ok().or_else(|| {
        let home = env_home::env_home_dir();

        home.map(|mut x: PathBuf| {
            x.push(".steel");

            // Just go ahead and initialize the directory, even though
            // this is probably not the best place to do this. This almost
            // assuredly could be lifted out of this check since failing here
            // could cause some annoyance.
            if !x.exists() {
                if let Err(_) = std::fs::create_dir(&x) {
                    eprintln!("Unable to create steel home directory {:?}", x)
                }
            }

            x.into_os_string().into_string().unwrap()
        })
    })
});

#[cfg(target_arch = "wasm32")]
pub static STEEL_HOME: Lazy<Option<String>> = Lazy::new(|| None);

pub fn steel_home() -> Option<String> {
    STEEL_HOME.clone()
}

/// Manages the modules
/// keeps some visited state on the manager for traversal
/// Also keeps track of the metadata for each file in order to determine
/// if it needs to be recompiled
#[derive(Clone)]
pub(crate) struct ModuleManager {
    compiled_modules: FxHashMap<PathBuf, CompiledModule>,
    file_metadata: crate::HashMap<PathBuf, SystemTime>,
    visited: FxHashSet<PathBuf>,
    custom_builtins: HashMap<String, String>,
    rollback_metadata: crate::HashMap<PathBuf, SystemTime>,
    // #[serde(skip_serializing, skip_deserializing)]
    module_resolvers: Vec<Arc<dyn SourceModuleResolver>>,
}

pub trait SourceModuleResolver: Send + Sync {
    fn resolve(&self, key: &str) -> Option<String>;
    fn exists(&self, key: &str) -> bool;
}

impl ModuleManager {
    pub(crate) fn new(
        compiled_modules: FxHashMap<PathBuf, CompiledModule>,
        file_metadata: crate::HashMap<PathBuf, SystemTime>,
    ) -> Self {
        ModuleManager {
            compiled_modules,
            file_metadata,
            visited: FxHashSet::default(),
            custom_builtins: HashMap::new(),
            rollback_metadata: crate::HashMap::new(),
            module_resolvers: Vec::new(),
        }
    }

    pub fn add_builtin_module(&mut self, module_name: String, mut text: String) {
        // Custom external builtins should be loaded with the prelude first, otherwise
        // they'll need to handle a bunch of imports
        text.insert_str(0, PRELUDE_STRING);

        self.custom_builtins.insert(module_name, text);
    }

    pub fn modules(&self) -> &FxHashMap<PathBuf, CompiledModule> {
        &self.compiled_modules
    }

    pub fn modules_mut(&mut self) -> &mut FxHashMap<PathBuf, CompiledModule> {
        &mut self.compiled_modules
    }

    pub(crate) fn default() -> Self {
        Self::new(FxHashMap::default(), crate::HashMap::default())
    }

    pub(crate) fn register_module_resolver(
        &mut self,
        resolver: impl SourceModuleResolver + 'static,
    ) {
        self.module_resolvers.push(Arc::new(resolver));
    }

    // Add the module directly to the compiled module cache
    pub(crate) fn add_module(
        &mut self,
        path: PathBuf,
        global_macro_map: &mut FxHashMap<InternedString, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<()> {
        self.visited.clear();

        // TODO: Expand macros on the fly when visiting a module. Don't wait till the end
        // Macro expansion should happen as we enter a module.
        let mut module_builder = ModuleBuilder::new_from_path(
            path,
            &mut self.compiled_modules,
            &mut self.visited,
            &mut self.file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            &self.custom_builtins,
            &[],
            &self.module_resolvers,
        )?;

        module_builder.compile()?;

        Ok(())
    }

    pub(crate) fn rollback_metadata(&mut self) {
        self.file_metadata = self.rollback_metadata.clone();
    }

    // #[allow(unused)]
    pub(crate) fn compile_main(
        &mut self,
        global_macro_map: &mut FxHashMap<InternedString, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        mut exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        builtin_modules: ModuleContainer,
        lifted_kernel_environments: &mut HashMap<String, KernelDefMacroSpec>,
        lifted_macro_environments: &mut HashSet<PathBuf>,
        search_dirs: &[PathBuf],
    ) -> Result<Vec<ExprKind>> {
        // Wipe the visited set on entry
        self.visited.clear();

        self.rollback_metadata = self.file_metadata.clone();

        // TODO
        // This is also explicitly wrong -> we should separate the global macro map from the macros found locally in this module
        // For instance, (cond) is global, but (define-syntax blagh) might be local to main
        // if a module then defines a function (blagh) that is used inside its scope, this would expand the macro in that scope
        // which we do not want
        extract_macro_defs(&mut exprs, global_macro_map)?;

        let mut module_builder = ModuleBuilder::main(
            path,
            exprs,
            &mut self.compiled_modules,
            &mut self.visited,
            &mut self.file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            &self.custom_builtins,
            search_dirs,
            &self.module_resolvers,
        )?;

        let mut module_statements = module_builder.compile()?;

        module_builder.collect_provides()?;

        let mut ast = module_builder.source_ast;

        let mut require_defines = Vec::new();

        let mut explicit_requires = HashMap::new();

        for require_object in &module_builder.require_objects {
            let path = require_object.path.get_path();
            explicit_requires.clear();

            // If there _are_ explicit identifiers to import, limit what we import to what
            // is in the set
            for ident in &require_object.idents_to_import {
                match ident {
                    MaybeRenamed::Normal(i) => {
                        explicit_requires.insert(i.atom_identifier().unwrap().clone(), None);
                    }

                    MaybeRenamed::Renamed(from, to) => {
                        explicit_requires.insert(
                            from.atom_identifier().unwrap().clone(),
                            Some(to.atom_identifier().unwrap().clone()),
                        );
                    }
                }
            }

            // println!("{:?}", path);
            let module = if let Some(module) = module_builder.compiled_modules.get(path.as_ref()) {
                module
            } else {
                // log::debug!(target: "modules", "No provides found for module, skipping: {:?}", path);

                continue;
            };

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    let other_module_prefix = module.prefix();

                    // TODO: Expand the contract out into something we expect
                    // Otherwise, this is going to blow up
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                match *qualifier {
                                    x if x == *REQUIRE_IDENT_SPEC => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let name = l.args.get(1).unwrap();

                                        if !explicit_requires.is_empty()
                                            && !name
                                                .atom_identifier()
                                                .map(|x| explicit_requires.contains_key(x))
                                                .unwrap_or_default()
                                        {
                                            continue;
                                        }

                                        if module
                                            .macro_map
                                            .contains_key(name.atom_identifier().unwrap())
                                        {
                                            continue;
                                        }

                                        let hash_get = expr_list![
                                            ExprKind::atom(*PROTO_HASH_GET),
                                            ExprKind::atom(
                                                CompactString::new(MODULE_PREFIX)
                                                    + &other_module_prefix
                                            ),
                                            ExprKind::Quote(Box::new(Quote::new(
                                                name.clone(),
                                                SyntaxObject::default(TokenType::Quote)
                                            ))),
                                        ];

                                        let mut owned_name = name.clone();

                                        // If we have the alias listed, we should use it
                                        if !explicit_requires.is_empty() {
                                            if let Some(alias) = explicit_requires
                                                .get(name.atom_identifier().unwrap())
                                                .copied()
                                                .flatten()
                                            {
                                                *owned_name.atom_identifier_mut().unwrap() =
                                                    alias.clone();
                                            }
                                        }

                                        if let Some(prefix) = &require_object.prefix {
                                            if let Some(existing) = owned_name.atom_identifier_mut()
                                            {
                                                let mut prefixed_identifier = prefix.clone();
                                                prefixed_identifier.push_str(existing.resolve());

                                                // Update the existing identifier to point to a new one with the prefix applied
                                                *existing = prefixed_identifier.into();
                                            }
                                        }

                                        let define = ExprKind::Define(Box::new(Define::new(
                                            owned_name,
                                            hash_get,
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        require_defines.push(define);
                                    }
                                    _ => {
                                        stop!(TypeMismatch => format!("provide expects either an identifier, (for-syntax <ident>), or (contract/out ...), found: {}", provide))
                                    }
                                }
                            } else {
                                stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                            }
                        }
                        ExprKind::Atom(_) => {
                            if !explicit_requires.is_empty()
                                && !provide
                                    .atom_identifier()
                                    .map(|x| explicit_requires.contains_key(x))
                                    .unwrap_or_default()
                            {
                                continue;
                            }

                            if module
                                .macro_map
                                .contains_key(provide.atom_identifier().unwrap())
                            {
                                continue;
                            }

                            let hash_get = expr_list![
                                ExprKind::atom(*PROTO_HASH_GET),
                                ExprKind::atom(
                                    CompactString::new(MODULE_PREFIX) + &other_module_prefix
                                ),
                                ExprKind::Quote(Box::new(Quote::new(
                                    provide.clone(),
                                    SyntaxObject::default(TokenType::Quote)
                                ))),
                            ];

                            let mut owned_provide = provide.clone();

                            // If we have the alias listed, we should use it
                            if !explicit_requires.is_empty() {
                                if let Some(alias) = explicit_requires
                                    .get(provide.atom_identifier().unwrap())
                                    .copied()
                                    .flatten()
                                {
                                    *owned_provide.atom_identifier_mut().unwrap() = alias.clone();
                                }
                            }

                            // TODO: If there is a prefix applied, use it here?
                            if let Some(prefix) = &require_object.prefix {
                                // println!("Found prefix: {}", prefix);

                                if let Some(existing) = owned_provide.atom_identifier_mut() {
                                    let mut prefixed_identifier = prefix.clone();
                                    prefixed_identifier.push_str(existing.resolve());

                                    // Update the existing identifier to point to a new one with the prefix applied
                                    *existing = prefixed_identifier.into();
                                }
                            }

                            let define = ExprKind::Define(Box::new(Define::new(
                                owned_provide,
                                hash_get,
                                SyntaxObject::default(TokenType::Define),
                            )));

                            // println!("{}", define);

                            require_defines.push(define);
                        }
                        _ => {
                            stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier")
                        }
                    }
                }
            }
        }

        let mut mangled_asts = Vec::with_capacity(ast.len());

        // TODO: Move this to the lower level as well
        // It seems we're only doing this expansion at the top level, but we _should_ do this at the lower level as well
        for require_object in module_builder.require_objects.iter()
        // .filter(|x| x.for_syntax)
        // .map(|x| x.path.get_path())
        {
            let require_for_syntax = require_object.path.get_path();

            let (module, in_scope_macros, mut name_mangler) = Self::find_in_scope_macros(
                &mut self.compiled_modules,
                require_for_syntax.as_ref(),
                &require_object,
                &mut mangled_asts,
            );

            // let kernel_macros_in_scope: HashSet<_> =
            //     module.provides_for_syntax.iter().cloned().collect();

            // let defmacros_exported: HashSet<_> = module.

            // dbg!(&kernel_macros_in_scope);

            let module_name = module.name.to_str().unwrap().to_string();

            if let Some(kernel) = kernel.as_mut() {
                if kernel.exported_defmacros(&module_name).is_some() {
                    lifted_kernel_environments.insert(
                        module_name.clone(),
                        KernelDefMacroSpec {
                            _env: module_name.clone(),
                            _exported: None,
                            name_mangler: name_mangler.clone(),
                        },
                    );
                }
            }

            // TODO: This isn't right - only check if there are defmacro things
            // that we need to lift - just check the values that are in the defmacros
            // environment in the kernel
            // if !kernel_macros_in_scope.is_empty() {
            //     lifted_kernel_environments.insert(
            //         module_name.clone(),
            //         KernelDefMacroSpec {
            //             env: module_name,
            //             exported: None,
            //             name_mangler: name_mangler.clone(),
            //         },
            //     );
            // }

            // let module_name = Cow::from(module.name.to_str().unwrap().to_string());

            for expr in ast.iter_mut() {
                // @matt 12/8/2023
                // The easiest thing to do here, is to go to the other module, and find
                // what defmacros have been exposed on the require for syntax. Once those
                // have been found, we run a pass with kernel expansion, limiting the
                // expander to only use the macros that we've exposed. After that,
                // we run the expansion again, using the full suite of defmacro capabilities.
                //
                // The question that remains - how to define the neat phases of what kinds
                // of macros can expand into what? Can defmacro -> syntax-rules -> defmacro?
                // This could eventually prove to be cumbersome, but it is still early
                // for defmacro. Plus, I need to create a syntax-case or syntax-parse
                // frontend before the defmacro style macros become too pervasive.
                //
                // TODO: Replicate this behavior over to builtin modules

                // First expand the in scope macros
                // These are macros
                let mut expander = Expander::new(&in_scope_macros);
                expander.expand(expr)?;
                let changed = false;

                // (first_round_expanded, changed) = expand_kernel_in_env_with_allowed(
                //     first_round_expanded,
                //     kernel.as_mut(),
                //     // We don't need to expand those here
                //     ModuleContainer::default(),
                //     module.name.to_str().unwrap().to_string(),
                //     &kernel_macros_in_scope,
                // )?;

                // If the kernel expander expanded into something - go ahead
                // and expand all of the macros in this
                // if changed || expander.changed {
                // Expand here?
                // first_round_expanded = expand(first_round_expanded, &module.macro_map)?;

                // Probably don't need this
                // (first_round_expanded, changed) = expand_kernel_in_env_with_change(
                //     first_round_expanded,
                //     kernel.as_mut(),
                //     ModuleContainer::default(),
                //     module.name.to_str().unwrap().to_string(),
                // )?;

                // This is pretty suspect, and needs to be revisited - only the output of the
                // macro expansion and not the whole thing needs to be mangled most likely.
                // Otherwise, we'll run into weird stuff?
                // if changed {
                //     name_mangler.visit(&mut first_round_expanded);
                // }
                // }

                if expander.changed || changed {
                    let _source_id = sources.get_source_id(&module.name).unwrap();

                    // let mut fully_expanded = first_round_expanded;

                    expand(
                        expr,
                        &module.macro_map,
                        // source_id,
                    )?;

                    // Expanding the kernel with only these macros...
                    let changed = expand_kernel_in_env_with_change(
                        expr,
                        kernel.as_mut(),
                        // We don't need to expand those here
                        ModuleContainer::default(),
                        &module_name,
                        // &kernel_macros_in_scope,
                    )?;

                    if changed {
                        name_mangler.visit(expr);
                    }

                    // lifted_kernel_environments.insert(
                    //     module_name.clone(),
                    //     KernelDefMacroSpec {
                    //         env: module_name,
                    //         exported: None,
                    //         name_mangler: name_mangler.clone(),
                    //     },
                    // );

                    // Ok(fully_expanded)
                }
                // else {
                //     Ok(first_round_expanded)
                // }
            }

            // ast = ast
            //     .into_iter()
            //     .map(|x| {
            //         // @matt 12/8/2023
            //         // The easiest thing to do here, is to go to the other module, and find
            //         // what defmacros have been exposed on the require for syntax. Once those
            //         // have been found, we run a pass with kernel expansion, limiting the
            //         // expander to only use the macros that we've exposed. After that,
            //         // we run the expansion again, using the full suite of defmacro capabilities.
            //         //
            //         // The question that remains - how to define the neat phases of what kinds
            //         // of macros can expand into what? Can defmacro -> syntax-rules -> defmacro?
            //         // This could eventually prove to be cumbersome, but it is still early
            //         // for defmacro. Plus, I need to create a syntax-case or syntax-parse
            //         // frontend before the defmacro style macros become too pervasive.
            //         //
            //         // TODO: Replicate this behavior over to builtin modules

            //         // First expand the in scope macros
            //         // These are macros
            //         let mut expander = Expander::new(&in_scope_macros);
            //         let mut first_round_expanded = expander.expand(x)?;
            //         let mut changed = false;

            //         // (first_round_expanded, changed) = expand_kernel_in_env_with_allowed(
            //         //     first_round_expanded,
            //         //     kernel.as_mut(),
            //         //     // We don't need to expand those here
            //         //     ModuleContainer::default(),
            //         //     module.name.to_str().unwrap().to_string(),
            //         //     &kernel_macros_in_scope,
            //         // )?;

            //         // If the kernel expander expanded into something - go ahead
            //         // and expand all of the macros in this
            //         // if changed || expander.changed {
            //         // Expand here?
            //         // first_round_expanded = expand(first_round_expanded, &module.macro_map)?;

            //         // Probably don't need this
            //         // (first_round_expanded, changed) = expand_kernel_in_env_with_change(
            //         //     first_round_expanded,
            //         //     kernel.as_mut(),
            //         //     ModuleContainer::default(),
            //         //     module.name.to_str().unwrap().to_string(),
            //         // )?;

            //         // This is pretty suspect, and needs to be revisited - only the output of the
            //         // macro expansion and not the whole thing needs to be mangled most likely.
            //         // Otherwise, we'll run into weird stuff?
            //         // if changed {
            //         //     name_mangler.visit(&mut first_round_expanded);
            //         // }
            //         // }

            //         if expander.changed || changed {
            //             let source_id = sources.get_source_id(&module.name).unwrap();

            //             let mut fully_expanded = first_round_expanded;

            //             expand(
            //                 &mut fully_expanded,
            //                 &module.macro_map,
            //                 // source_id,
            //             )?;

            //             let module_name = module.name.to_str().unwrap().to_string();

            //             // Expanding the kernel with only these macros...
            //             let changed = expand_kernel_in_env_with_change(
            //                 &mut fully_expanded,
            //                 kernel.as_mut(),
            //                 // We don't need to expand those here
            //                 ModuleContainer::default(),
            //                 module_name.clone(),
            //                 // &kernel_macros_in_scope,
            //             )?;

            //             if changed {
            //                 name_mangler.visit(&mut fully_expanded);
            //             }

            //             // lifted_kernel_environments.insert(
            //             //     module_name.clone(),
            //             //     KernelDefMacroSpec {
            //             //         env: module_name,
            //             //         exported: None,
            //             //         name_mangler: name_mangler.clone(),
            //             //     },
            //             // );

            //             Ok(fully_expanded)
            //         } else {
            //             Ok(first_round_expanded)
            //         }
            //     })
            //     .collect::<Result<_>>()?;

            // Global macro map - also need to expand with ALL macros
            // post expansion in the target environment, which means we can't _just_
            // extend the global macro map with the target in scope macros, we need to
            // do something like the two pass expansion
            global_macro_map.extend(in_scope_macros);

            lifted_macro_environments.insert(module.name.clone());
        }

        // Include the defines from the modules now imported
        module_statements.append(&mut require_defines);

        // The next two lines here expand _all_ of the source code with the top level macros
        // This is necessary because of the std library macros, although this should be able to be
        // solved with scoped imports of the standard library explicitly
        module_statements.append(&mut ast);

        time!("Top level macro evaluation time", {
            for expr in module_statements.iter_mut() {
                expand(expr, global_macro_map)?;
            }
        });

        // @Matt 7/4/23
        // TODO: With mangling, this could cause problems. We'll want to un-mangle quotes AFTER the macro has been expanded,
        // in order to preserve the existing behavior.
        // let result = module_statements
        //     .into_iter()
        //     .map(|x| expand(x, global_macro_map))
        //     .collect::<Result<_>>();

        // result

        Ok(module_statements)
    }

    fn find_in_scope_macros<'a>(
        compiled_modules: &'a mut FxHashMap<PathBuf, CompiledModule>,
        require_for_syntax: &'a PathBuf,
        require_object: &'a RequireObject,
        mangled_asts: &'a mut Vec<ExprKind>,
    ) -> (
        &'a CompiledModule,
        FxHashMap<InternedString, SteelMacro>,
        NameMangler,
    ) {
        let module = compiled_modules
            .get_mut(require_for_syntax)
            .expect(&format!("Module missing!: {:?}", require_for_syntax));

        let prefix = module.prefix();

        let globals = collect_globals(&module.ast);

        let mut name_mangler = NameMangler::new(globals, prefix);

        // If the module hasn't been emitted already, then include it here
        if !module.emitted {
            let mut module_ast = module.ast.clone();

            name_mangler.mangle_vars(&mut module_ast);

            mangled_asts.append(&mut module_ast);
        }

        // let provided_macros = module.provides_for
        // let expander = Expander::new(&module.macro_map);
        // TODO
        // expand expressions one by one
        // if expansion with _just_ public macros from the required module doesn't do anything, stop
        // if it _does_ change, do another pass with all of the macros in scope
        // do this for each of the expressions in the file in this loop
        // TODO -> try not cloning this
        // TODO -> do this in the module expansion as well
        let mut in_scope_macros = module
            .provides_for_syntax
            .iter()
            // Chain with just the normal provides!
            .chain(module.provides.iter().flat_map(|x| {
                x.list().unwrap().args[1..]
                    .iter()
                    .filter_map(|x| x.atom_identifier())
            }))
            .filter_map(|x| {
                let smacro = Arc::make_mut(&mut module.macro_map).get_mut(x);

                if let Some(smacro) = smacro {
                    if !smacro.special_mangled {
                        for expr in smacro.exprs_mut() {
                            name_mangler.visit(expr);
                        }
                    }

                    Some((*x, smacro.clone()))
                } else {
                    None
                }

                // if !x.1.special_mangled {
                //     for expr in x.1.exprs_mut() {
                //         name_mangler.visit(expr);
                //     }
                // }

                // (x.0, x.1.clone())
            })
            .collect::<FxHashMap<_, _>>();

        // If the require_object specifically imports things, we should reference it

        if !require_object.idents_to_import.is_empty() {
            for maybe in &require_object.idents_to_import {
                match maybe {
                    MaybeRenamed::Normal(n) => {
                        if let Some(ident) = n.atom_identifier() {
                            if let Some(mut m) = module.macro_map.get(ident).cloned() {
                                for expr in m.exprs_mut() {
                                    name_mangler.visit(expr);
                                }

                                if let Some(prefix) = &require_object.prefix {
                                    in_scope_macros
                                        .insert((prefix.to_string() + ident.resolve()).into(), m);
                                } else {
                                    in_scope_macros.insert(*ident, m);
                                }
                            }
                        }
                    }
                    MaybeRenamed::Renamed(from, to) => {
                        if let Some(ident) = from.atom_identifier() {
                            if let Some(mut m) = module.macro_map.get(ident).cloned() {
                                for expr in m.exprs_mut() {
                                    name_mangler.visit(expr);
                                }
                                // TODO: Remove this unwrap
                                // in_scope_macros.insert(*to.atom_identifier().unwrap(), m);

                                if let Some(prefix) = &require_object.prefix {
                                    in_scope_macros.insert(
                                        (prefix.to_string()
                                            + to.atom_identifier().unwrap().resolve())
                                        .into(),
                                        m,
                                    );
                                } else {
                                    in_scope_macros.insert(*to.atom_identifier().unwrap(), m);
                                }
                            }
                        }
                    }
                }
            }
        } else {
            // Pull in all of the macros that the module exposes

            for provide_expr in &module.provides {
                if let Some(provide_expr) = provide_expr.list() {
                    for ident in provide_expr.args.split_first().unwrap().1 {
                        // println!("Looking for {}", ident);

                        if let Some(ident) = ident.atom_identifier() {
                            if let Some(mut m) = module.macro_map.get(ident).cloned() {
                                // println!("Pulling in macro: {}", ident);

                                for expr in m.exprs_mut() {
                                    name_mangler.visit(expr);
                                }

                                if let Some(prefix) = &require_object.prefix {
                                    in_scope_macros
                                        .insert((prefix.to_string() + ident.resolve()).into(), m);
                                } else {
                                    in_scope_macros.insert(*ident, m);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Check what macros are in scope here
        // println!(
        //     "In scope macros: {:#?}",
        //     in_scope_macros.keys().collect::<Vec<_>>()
        // );
        (module, in_scope_macros, name_mangler)
    }
}

// Pre-compile module to bytecode? Is it even possible?
// Dynamically linking the module would then make it relatively
// easy to just load everything up at the start.
// Compiled module _should_ be possible now. Just create a target
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompiledModule {
    name: PathBuf,
    provides: Vec<ExprKind>,
    require_objects: Vec<RequireObject>,
    provides_for_syntax: Vec<InternedString>,
    pub(crate) macro_map: Arc<FxHashMap<InternedString, SteelMacro>>,
    ast: Vec<ExprKind>,
    emitted: bool,
    cached_prefix: CompactString,
    downstream: Vec<PathBuf>,
}

pub static MANGLER_PREFIX: &'static str = "##mm";
pub static MODULE_PREFIX: &'static str = "__module-";
pub static MANGLED_MODULE_PREFIX: &'static str = "__module-##mm";

pub fn path_to_module_name(name: PathBuf) -> String {
    let mut base = CompactString::new(MANGLED_MODULE_PREFIX);

    if let Some(steel_home) = STEEL_HOME.as_ref() {
        // Intern this?
        let name = name
            .to_str()
            .unwrap()
            .trim_start_matches(steel_home.as_str());

        let interned = InternedString::from_str(&name);
        let id = interned.get().into_inner();

        base.push_str(&id.to_string());
        base.push_str(MANGLER_SEPARATOR);
    } else {
        let interned = InternedString::from_str(name.to_str().unwrap());
        let id = interned.get().into_inner();

        base.push_str(&id.to_string());
        base.push_str(MANGLER_SEPARATOR);
    }

    base.into_string()
}

// TODO: @Matt 6/12/23 - This _should_ be serializable. If possible, we can try to store intermediate objects down to some file.
impl CompiledModule {
    pub fn new(
        name: PathBuf,
        provides: Vec<ExprKind>,
        require_objects: Vec<RequireObject>,
        provides_for_syntax: Vec<InternedString>,
        macro_map: Arc<FxHashMap<InternedString, SteelMacro>>,
        ast: Vec<ExprKind>,
        downstream: Vec<PathBuf>,
    ) -> Self {
        let mut base = CompactString::new(MANGLER_PREFIX);

        if let Some(steel_home) = STEEL_HOME.as_ref() {
            // Intern this?
            let name = name
                .to_str()
                .unwrap()
                .trim_start_matches(steel_home.as_str());

            let interned = InternedString::from_str(&name);
            let id = interned.get().into_inner();

            // base.push_str(name);
            base.push_str(&id.to_string());
            base.push_str(MANGLER_SEPARATOR);

            // println!("{}", base);
            // println!("Byte length: {}", base.len());
        } else {
            let interned = InternedString::from_str(name.to_str().unwrap());
            let id = interned.get().into_inner();

            // base.push_str(self.name.to_str().unwrap());
            base.push_str(&id.to_string());
            base.push_str(MANGLER_SEPARATOR);
        }

        Self {
            name,
            provides,
            require_objects,
            provides_for_syntax,
            macro_map,
            ast,
            emitted: false,
            cached_prefix: base,
            downstream,
        }
    }

    // TODO: Should cache this
    pub fn prefix(&self) -> CompactString {
        self.cached_prefix.clone()
    }

    pub fn get_ast(&self) -> &[ExprKind] {
        &self.ast
    }

    pub fn get_provides(&self) -> &[ExprKind] {
        &self.provides
    }

    // pub fn get_requires(&self) -> &[PathBuf] {
    //     &self.requires
    // }

    pub fn set_emitted(&mut self, emitted: bool) {
        self.emitted = emitted;
    }

    fn to_top_level_module(
        &self,
        modules: &FxHashMap<PathBuf, CompiledModule>,
        global_macro_map: &FxHashMap<InternedString, SteelMacro>,
    ) -> Result<ExprKind> {
        let mut globals = collect_globals(&self.ast);

        let mut exprs = self.ast.clone();

        let mut provide_definitions = Vec::new();

        // TODO: Change this to not use the full path. Unfortunately that isn't portable,
        // so we should use something that is more agnostic of the target location
        // if we were to load.
        //
        // Probably a better idea would be to somehow assign a unique ID to each module;
        // the path from the $STEEL_HOME would be relatively safe. That way we can just strip
        // the $STEEL_HOME root away from the path if it starts with it, and then have
        // that resolve to be the "name" of the module.
        let prefix = self.prefix();

        // Now we should be able to set up a series of requires with the right style
        // ;; Refresh the module definition in this namespace
        // (define a-module.rkt-b (hash-get 'b b-module.rkt-b))

        let mut explicit_requires = HashMap::new();

        // TODO: This is the same as the top level, they should be merged
        for require_object in &self.require_objects {
            let path = require_object.path.get_path();

            explicit_requires.clear();

            for ident in &require_object.idents_to_import {
                match ident {
                    MaybeRenamed::Normal(i) => {
                        explicit_requires.insert(i.atom_identifier().unwrap().clone(), None);
                    }
                    MaybeRenamed::Renamed(from, to) => {
                        explicit_requires.insert(
                            from.atom_identifier().unwrap().clone(),
                            Some(to.atom_identifier().unwrap().clone()),
                        );
                    }
                }
            }

            // println!("{:?}", path);
            // println!("{:?}", modules.keys().collect::<Vec<_>>());
            let module = modules.get(path.as_ref()).unwrap();

            let other_module_prefix = module.prefix();

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                if module.macro_map.contains_key(qualifier) {
                                    continue;
                                }

                                match *qualifier {
                                    x if x == *REQUIRE_IDENT_SPEC => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let name = l.args.get(1).unwrap();

                                        if !explicit_requires.is_empty()
                                            && !name
                                                .atom_identifier()
                                                .map(|x| explicit_requires.contains_key(x))
                                                .unwrap_or_default()
                                        {
                                            continue;
                                        }

                                        if module
                                            .macro_map
                                            .contains_key(name.atom_identifier().unwrap())
                                        {
                                            continue;
                                        }

                                        let hash_get = expr_list![
                                            ExprKind::atom(*PROTO_HASH_GET),
                                            ExprKind::atom(
                                                CompactString::new(MODULE_PREFIX)
                                                    + &other_module_prefix
                                            ),
                                            ExprKind::Quote(Box::new(Quote::new(
                                                name.clone(),
                                                SyntaxObject::default(TokenType::Quote)
                                            ))),
                                        ];

                                        let mut owned_name = name.clone();

                                        // If we have the alias listed, we should use it
                                        if !explicit_requires.is_empty() {
                                            if let Some(alias) = explicit_requires
                                                .get(name.atom_identifier().unwrap())
                                                .copied()
                                                .flatten()
                                            {
                                                *owned_name.atom_identifier_mut().unwrap() =
                                                    alias.clone();
                                            }
                                        }

                                        if let Some(prefix) = &require_object.prefix {
                                            if let Some(existing) = owned_name.atom_identifier_mut()
                                            {
                                                let mut prefixed_identifier = prefix.clone();
                                                prefixed_identifier.push_str(existing.resolve());

                                                // Update the existing identifier to point to a new one with the prefix applied
                                                *existing = prefixed_identifier.into();
                                            }
                                        }

                                        globals.insert(*name.atom_identifier().unwrap());

                                        let define = ExprKind::Define(Box::new(Define::new(
                                            owned_name,
                                            hash_get,
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        provide_definitions.push(define);
                                    }

                                    // x if x == *CONTRACT_OUT => {
                                    //     // Directly expand into define/contract, but with the value just being the hash get below

                                    //     // (bind/c contract name 'name)

                                    //     let mut name = l.args.get(1).unwrap().clone();
                                    //     let _contract = l.args.get(2).unwrap();

                                    //     if !explicit_requires.is_empty()
                                    //         && !name
                                    //             .atom_identifier()
                                    //             .map(|x| explicit_requires.contains_key(x))
                                    //             .unwrap_or_default()
                                    //     {
                                    //         continue;
                                    //     }

                                    //     // If we have the alias listed, we should use it
                                    //     if !explicit_requires.is_empty() {
                                    //         if let Some(alias) = explicit_requires
                                    //             .get(name.atom_identifier().unwrap())
                                    //             .copied()
                                    //             .flatten()
                                    //         {
                                    //             *name.atom_identifier_mut().unwrap() =
                                    //                 alias.clone();
                                    //         }
                                    //     }

                                    //     if let Some(prefix) = &require_object.prefix {
                                    //         if let Some(existing) = name.atom_identifier_mut() {
                                    //             let mut prefixed_identifier = prefix.clone();
                                    //             prefixed_identifier.push_str(existing.resolve());

                                    //             // Update the existing identifier to point to a new one with the prefix applied
                                    //             *existing = prefixed_identifier.into();
                                    //         }
                                    //     }

                                    //     // Since this is now bound to be in the scope of the current working module, we also want
                                    //     // this to be mangled. In the event we do something like, qualify the import, then we might
                                    //     // have to mangle this differently
                                    //     globals.insert(*name.atom_identifier().unwrap());

                                    //     let hash_get = expr_list![
                                    //         ExprKind::atom(*PROTO_HASH_GET),
                                    //         ExprKind::atom(
                                    //             "__module-".to_string() + &other_module_prefix
                                    //         ),
                                    //         ExprKind::Quote(Box::new(Quote::new(
                                    //             name.clone(),
                                    //             SyntaxObject::default(TokenType::Quote)
                                    //         ))),
                                    //     ];

                                    //     let define = ExprKind::Define(Box::new(Define::new(
                                    //         ExprKind::atom(
                                    //             prefix.clone()
                                    //                 + name.atom_identifier().unwrap().resolve(),
                                    //         ),
                                    //         hash_get,
                                    //         SyntaxObject::default(TokenType::Define),
                                    //     )));

                                    //     provide_definitions.push(define);
                                    // }
                                    _ => {
                                        stop!(TypeMismatch => format!("provide expects either an identifier, (for-syntax <ident>), or (contract/out ...) - found: {}", provide))
                                    }
                                }
                            } else {
                                stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                            }
                        }
                        ExprKind::Atom(_) => {
                            if !explicit_requires.is_empty()
                                && !provide
                                    .atom_identifier()
                                    .map(|x| explicit_requires.contains_key(x))
                                    .unwrap_or_default()
                            {
                                continue;
                            }

                            if module
                                .macro_map
                                .contains_key(provide.atom_identifier().unwrap())
                            {
                                continue;
                            }

                            // Mangle with a prefix if necessary
                            let mut provide = provide.clone();
                            let raw_provide = provide.clone();

                            // If we have the alias listed, we should use it
                            if !explicit_requires.is_empty() {
                                if let Some(alias) = explicit_requires
                                    .get(provide.atom_identifier().unwrap())
                                    .copied()
                                    .flatten()
                                {
                                    *provide.atom_identifier_mut().unwrap() = alias.clone();
                                }
                            }

                            if let Some(prefix) = &require_object.prefix {
                                if let Some(existing) = provide.atom_identifier_mut() {
                                    let mut prefixed_identifier = prefix.clone();
                                    prefixed_identifier.push_str(existing.resolve());

                                    // Update the existing identifier to point to a new one with the prefix applied
                                    *existing = prefixed_identifier.into();
                                }
                            }

                            let provide_ident = provide.atom_identifier().unwrap();

                            // Since this is now bound to be in the scope of the current working module, we also want
                            // this to be mangled. In the event we do something like, qualify the import, then we might
                            // have to mangle this differently
                            globals.insert(*provide_ident);

                            let define = ExprKind::Define(Box::new(Define::new(
                                ExprKind::atom(prefix.clone() + provide_ident.resolve()),
                                expr_list![
                                    ExprKind::atom(*PROTO_HASH_GET),
                                    ExprKind::atom(
                                        CompactString::new(MODULE_PREFIX) + &other_module_prefix
                                    ),
                                    ExprKind::Quote(Box::new(Quote::new(
                                        raw_provide.clone(),
                                        SyntaxObject::default(TokenType::Quote)
                                    )))
                                ],
                                SyntaxObject::default(TokenType::Define),
                            )));

                            // if require_object.prefix.is_some() {
                            //     println!("{}", define);
                            // }

                            provide_definitions.push(define);
                        }
                        _ => {
                            stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier")
                        }
                    }
                }
            }
        }

        // Mangle all of the variables that are either:
        // 1. Defined locally in this file
        // 2. Required by another file
        let mut name_mangler = NameMangler::new(globals, prefix.clone());

        // Afterwards, walk through and unmangle any quoted values, since these
        // were intended to be used with non mangled values.
        // let name_unmangler = NameUnMangler::new(&prefix);

        name_mangler.mangle_vars(&mut exprs);

        // The provide definitions should also be mangled
        name_mangler.mangle_vars(&mut provide_definitions);

        // let mut hash_builder = Vec::new();

        // These are gonna be the pairs
        // hash_builder.push(());

        // Construct the series of provides as well, we'll want these to refer to the correct values
        //
        let mut provides: smallvec::SmallVec<[(ExprKind, ExprKind); 24]> = self
            .provides
            .iter()
            .flat_map(|x| &x.list().unwrap().args[1..])
            .cloned()
            .map(|x| (x.clone(), x))
            .collect();

        for provide in &mut provides {
            match &provide.1 {
                ExprKind::List(l) => {
                    if let Some(qualifier) = l.first_ident() {
                        match qualifier {
                            x if *x == *REQUIRE_IDENT_SPEC => {
                                // *provide = expand(l.get(2).unwrap().clone(), global_macro_map)?;

                                // *provide = expand(l.)

                                provide.0 = l.get(1).unwrap().clone();

                                let mut provide_expr = l.get(2).unwrap().clone();
                                expand(&mut provide_expr, global_macro_map)?;

                                provide.1 = provide_expr;

                                continue;

                                // name_unmangler.unmangle_expr(provide);
                            }
                            // x if *x == *CONTRACT_OUT => {
                            //     // Update the item to point to just the name
                            //     //
                            //     // *provide = l.get(1).unwrap().clone();
                            //     // {
                            //     //     println!("---------");
                            //     //     println!("Provide expr: {}", l.to_string());
                            //     // }

                            //     provide.0 = l.get(1).unwrap().clone();

                            //     let mut provide_expr = expr_list![
                            //         ExprKind::ident("bind/c"),
                            //         l.get(2).unwrap().clone(),
                            //         l.get(1).unwrap().clone(),
                            //         ExprKind::Quote(Box::new(Quote::new(
                            //             l.get(1).unwrap().clone(),
                            //             SyntaxObject::default(TokenType::Quote)
                            //         ))),
                            //     ];

                            //     expand(&mut provide_expr, global_macro_map)?;

                            //     provide.1 = provide_expr;

                            //     name_unmangler.unmangle_expr(&mut provide.1);
                            //     // continue;
                            // }
                            unknown => {
                                stop!(TypeMismatch => "bar provide expects either an identifier, (for-syntax <ident>), or (contract/out ...), found: {}", unknown)
                            }
                        }
                    } else {
                        stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                    }
                }
                ExprKind::Atom(_) => {
                    continue;
                }
                _ => {
                    stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier")
                }
            }
        }

        // Drop all of the macro references here
        provides.retain(|x| !self.macro_map.contains_key(x.0.atom_identifier().unwrap()));

        // We want one without the mangled version, for the actual provides
        let un_mangled = provides.clone();

        let left_unmangled: Vec<_> = un_mangled.into_iter().map(|x| x.0).collect();

        let mut right: Vec<_> = provides.into_iter().map(|x| x.1).collect();

        name_mangler.mangle_vars(&mut right);
        // name_unmangler.unmangle_vars(&mut provides);

        let mut hash_body = vec![ExprKind::ident("hash")];

        // We can put the module name in there, but that doesn't help us get the docs out...
        // Probably need to just store the docs directly in the module itself as well?
        // hash_body.push(ExprKind::atom("#:module-name"));
        // hash_body.push(ExprKind::atom(prefix.clone()));

        // left_unmangled.pretty_print();

        hash_body.extend(interleave(
            left_unmangled.into_iter().map(|x| {
                if let ExprKind::Atom(_) = x {
                    ExprKind::Quote(Box::new(Quote::new(
                        x,
                        SyntaxObject::default(TokenType::Quote),
                    )))
                } else if let ExprKind::List(l) = x {
                    if let Some(qualifier) = l.first_ident() {
                        match qualifier {
                            x if *x == *REQUIRE_IDENT_SPEC => {
                                todo!()
                            }
                            _ => {
                                return ExprKind::Quote(Box::new(Quote::new(
                                    l.get(2).unwrap().clone(),
                                    SyntaxObject::default(TokenType::Quote),
                                )))
                            }
                        }
                    }

                    // Then this is a contract out, and we should handle it here

                    ExprKind::Quote(Box::new(Quote::new(
                        l.get(2).unwrap().clone(),
                        SyntaxObject::default(TokenType::Quote),
                    )))
                    // ExprKind::Quote(Box::new(Quote::new(
                    //     x,
                    //     SyntaxObject::default(TokenType::Quote),
                    // )))
                } else {
                    panic!("TODO this shouldn't be possible")
                }
            }),
            right,
        ));

        let module_define = ExprKind::Define(Box::new(Define::new(
            ExprKind::atom(CompactString::new(MODULE_PREFIX) + &prefix),
            ExprKind::List(List::new(hash_body)),
            SyntaxObject::default(TokenType::Quote),
        )));

        // let mut offset = None;

        // Find offset of first non builtin require definition:
        // for (idx, expr) in exprs.iter().enumerate() {
        //     if let ExprKind::Define(d) = expr {
        //         // if !is_a_builtin_definition(d) || !is_a_require_definition(d) {
        //         if !is_a_builtin_definition(d) {
        //             // println!("Found offset at: {:?}", offset);

        //             offset = Some(idx);
        //             println!("Found offset at: {:?}", offset);
        //             break;
        //         }
        //     }
        // }

        exprs.push(module_define);

        // exprs.append(&mut provide_definitions);

        let mut builtin_definitions = Vec::new();

        exprs.retain_mut(|expr| {
            if let ExprKind::Define(d) = expr {
                if is_a_builtin_definition(d) {
                    builtin_definitions.push(std::mem::take(expr));
                    false
                } else {
                    true
                }
            } else {
                true
            }
        });

        builtin_definitions.append(&mut provide_definitions);
        builtin_definitions.append(&mut exprs);

        // provide_definitions.append(&mut builtin_definitions);
        // provide_definitions.append(&mut exprs);

        exprs = builtin_definitions;

        // if let Some(offset) = offset {
        // for (idx, expr) in provide_definitions.into_iter().enumerate() {
        //     exprs.insert(offset + idx, expr);
        // }
        // } else {
        // provide_definitions.append(&mut exprs);
        // }

        // println!("MODULE DEFINITIONS----");

        // exprs.pretty_print();

        // println!("END MODULE DEFINITIONS");

        // exprs.pretty_print();

        // exprs.push(module_define);

        // Construct the overall definition
        // TODO: Perhaps mangle these as well, especially if they have contracts associated with them

        // if offset.is_none() {
        //     provide_definitions.append(&mut exprs);
        // }

        // Try this out?
        // let mut analysis = Analysis::from_exprs(&provide_definitions);
        // let mut semantic = SemanticAnalysis::from_analysis(&mut provide_definitions, analysis);

        // // This is definitely broken still
        // semantic.remove_unused_globals_with_prefix("mangler");
        // .replace_non_shadowed_globals_with_builtins()
        // .remove_unused_globals_with_prefix("mangler");

        Ok(ExprKind::Begin(Box::new(Begin::new(
            exprs,
            SyntaxObject::default(TokenType::Begin),
        ))))
    }

    // Turn the module into the AST node that represents the macro module in the stdlib
    fn _to_module_ast_node(&self) -> ExprKind {
        let mut body = vec![
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                "module".into(),
            )))),
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                ("___".to_string() + self.name.to_str().unwrap()).into(),
            )))),
        ];

        // Put any provides at the top
        body.append(&mut self.provides.clone());

        // Include any dependencies here
        // body.append(&mut self.requires.clone());

        // TODO: @Matt 10/8/22
        // Reconsider how to address this expansion.
        // We really don't want to pollute the module space - perhaps disallow shadowed built-ins so we don't need this?
        // That would probably be annoying
        // let steel_base = ExprKind::List(List::new(vec![ExprKind::atom("steel/base".to_string())]));

        // self.ast.pretty_print();

        // body.push(steel_base);

        // Put the ast nodes inside the macro
        body.append(&mut self.ast.clone());

        // TODO clean this up
        let res = ExprKind::List(List::new(body));

        // if log_enabled!(target: "requires", log::Level::Debug) {
        //     debug!(target: "requires", "Module ast node: {}", res.to_string());
        // }

        res
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
enum MaybeRenamed {
    Normal(ExprKind),
    Renamed(ExprKind, ExprKind),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct RequireObject {
    path: PathOrBuiltIn,
    for_syntax: bool,
    idents_to_import: Vec<MaybeRenamed>,
    prefix: Option<String>,
    span: Span,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
enum PathOrBuiltIn {
    BuiltIn(Cow<'static, str>),
    Path(PathBuf),
}

impl PathOrBuiltIn {
    pub fn get_path(&self) -> Cow<'_, PathBuf> {
        match self {
            Self::Path(p) => Cow::Borrowed(p),
            Self::BuiltIn(p) => Cow::Owned(PathBuf::from(p.to_string())),
        }
    }
}

#[derive(Default, Debug, Clone)]
struct RequireObjectBuilder {
    path: Option<PathOrBuiltIn>,
    for_syntax: bool,
    idents_to_import: Vec<MaybeRenamed>,
    // Built up prefix
    prefix: Option<String>,
    span: Span,
}

impl RequireObjectBuilder {
    fn build(self) -> Result<RequireObject> {
        let path = self
            .path
            .ok_or_else(crate::throw!(Generic => "require must have a path!"))?;

        Ok(RequireObject {
            path,
            for_syntax: self.for_syntax,
            idents_to_import: self.idents_to_import,
            prefix: self.prefix,
            span: self.span,
        })
    }
}

fn try_canonicalize(path: PathBuf) -> PathBuf {
    std::fs::canonicalize(&path).unwrap_or_else(|_| path)
}

/*
#[derive(Default, Clone)]
struct DependencyGraph {
    downstream: HashMap<PathBuf, Vec<PathBuf>>,
}

impl DependencyGraph {
    // Adding edges downward.
    pub fn add_edges(&mut self, parent: PathBuf, children: Vec<PathBuf>) {
        self.downstream.insert(parent, children);
    }

    pub fn remove(&mut self, parent: &PathBuf) {
        self.downstream.remove(parent);
    }

    pub fn add_edge(&mut self, parent: &PathBuf, child: PathBuf) {
        if let Some(children) = self.downstream.get_mut(parent) {
            children.push(child);
        } else {
            self.downstream.insert(parent.clone(), vec![child]);
        }
    }

    // Check everything downstream of this, to see if anything needs to be invalidated
    pub fn check_downstream_changes(
        &self,
        root: &PathBuf,
        updated_at: &crate::HashMap<PathBuf, SystemTime>,
    ) -> std::io::Result<bool> {
        let mut stack = vec![root];

        while let Some(next) = stack.pop() {
            let meta = std::fs::metadata(next)?;

            if let Some(prev) = updated_at.get(next) {
                if *prev != meta.modified()? {
                    return Ok(true);
                }
            }

            if let Some(children) = self.downstream.get(next) {
                for child in children {
                    stack.push(child);
                }
            }
        }

        Ok(false)
    }
}
*/

struct ModuleBuilder<'a> {
    name: PathBuf,
    main: bool,
    source_ast: Vec<ExprKind>,
    macro_map: Arc<FxHashMap<InternedString, SteelMacro>>,
    // TODO: Change the requires / requires_for_syntax to just be a require enum?
    require_objects: Vec<RequireObject>,

    provides: Vec<ExprKind>,
    provides_for_syntax: Vec<ExprKind>,
    compiled_modules: &'a mut FxHashMap<PathBuf, CompiledModule>,
    visited: &'a mut FxHashSet<PathBuf>,
    file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
    sources: &'a mut Sources,
    kernel: &'a mut Option<Kernel>,
    builtin_modules: ModuleContainer,
    global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
    custom_builtins: &'a HashMap<String, String>,
    search_dirs: &'a [PathBuf],
    module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
}

impl<'a> ModuleBuilder<'a> {
    #[allow(clippy::too_many_arguments)]
    #[allow(unused)]
    fn main(
        name: Option<PathBuf>,
        source_ast: Vec<ExprKind>,
        compiled_modules: &'a mut FxHashMap<PathBuf, CompiledModule>,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        search_dirs: &'a [PathBuf],
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
    ) -> Result<Self> {
        // TODO don't immediately canonicalize the path unless we _know_ its coming from a path
        // change the path to not always be required
        // if its not required we know its not coming in

        #[cfg(not(target_arch = "wasm32"))]
        let name = if let Some(p) = name {
            std::fs::canonicalize(p)?
        } else {
            std::env::current_dir()?
        };

        #[cfg(target_arch = "wasm32")]
        let name = PathBuf::new();

        Ok(ModuleBuilder {
            name,
            main: true,
            source_ast,
            macro_map: default_prelude_macros(),
            require_objects: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            search_dirs,
            module_resolvers,
        })
    }

    fn compile(&mut self) -> Result<Vec<ExprKind>> {
        // debug!(target: "requires", "Visiting: {:?}", self.name);

        // @Matt - 10/3/23
        // This has a relatively fatal flaw at the moment:
        /*

            (define-syntax register-plugin
              (syntax-rules ()
                [(register-plugin plugin-path identifiers ...)
                 (begin
                   (require plugin-path
                            (only-in identifiers ...))
                   (provide identifiers ...))]))
        */
        // This will fail to compile - anything that expands into a require will fail since
        // require is more or less a top level primitive. What we need to do is figure
        // out a way to have this be recursive - the whole compilation step should
        // go again once we discover that there is another require. It shouldn't be too bad,
        // but the base case has to be figured out such that we don't get infinite recursion.
        // I think the condition is probably something along the following:
        //
        // - Did we expand anything
        // - Are there require statements
        //
        // If we expanded anything, we then should check for require statements... maybe

        let mut new_exprs = Vec::new();

        // self.source_ast.pretty_print();

        self.collect_requires()?;
        self.collect_provides()?;

        // if log_enabled!(log::Level::Info) {
        // debug!(target: "requires", "Requires: {:#?}", self.require_objects);
        // debug!(target: "requires", "Provides: {:#?}", self.provides);
        // debug!(target: "requires", "Provides for-syntax: {:?}", self.provides_for_syntax);
        // }

        if self.visited.contains(&self.name) {
            stop!(Generic => format!("circular dependency found during module resolution with: {:?}", self.name))
        }

        self.visited.insert(self.name.clone());

        if self.main {
            let exprs = std::mem::take(&mut self.source_ast);
            self.source_ast = exprs
                .into_iter()
                .filter(|x| {
                    if let ExprKind::List(l) = x {
                        if let Some(provide) = l.first_ident() {
                            return *provide != *PROVIDE;
                        }
                    }
                    true
                })
                .collect();
        }

        self.extract_macro_defs()?;

        // TODO include built ins here
        if self.require_objects.is_empty() && !self.main {
            // We're at a leaf, put into the cache
            new_exprs.push(self.compile_module()?);
        } else {
            // TODO come back for parsing built ins
            for module in self
                .require_objects
                .iter()
                .filter(|x| matches!(x.path, PathOrBuiltIn::BuiltIn(_)))
                .map(|x| x.path.get_path())
            {
                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                // If we already have compiled this module, get it from the cache
                if let Some(_m) = self.compiled_modules.get(module.as_ref()) {
                    // debug!("Getting {:?} from the module cache", module);
                    // println!("Already found in the cache: {:?}", module);
                    // new_exprs.push(m.to_module_ast_node());
                    // No need to do anything
                    continue;
                }

                // TODO this is some bad crap here don't do this
                let input = BUILT_INS
                    .iter()
                    .find(|x| x.0 == module.to_str().unwrap())
                    .map(|x| Cow::Borrowed(x.1))
                    .or_else(|| {
                        self.custom_builtins
                            .get(module.to_str().unwrap())
                            .map(|x| Cow::Owned(x.to_string()))
                    })
                    .or_else(|| {
                        self.module_resolvers
                            .iter()
                            .find_map(|x| x.resolve(module.to_str().unwrap()))
                            // Insert the prelude
                            .map(|mut x| {
                                x.insert_str(0, PRELUDE_STRING);
                                x
                            })
                            .map(Cow::Owned)
                    })
                    .ok_or_else(
                        crate::throw!(Generic => "Unable to find builtin module: {:?}", module),
                    )?;

                let mut new_module = ModuleBuilder::new_built_in(
                    module.into_owned(),
                    input,
                    self.compiled_modules,
                    self.visited,
                    self.file_metadata,
                    self.sources,
                    self.kernel,
                    self.builtin_modules.clone(),
                    self.global_macro_map,
                    self.custom_builtins,
                    self.module_resolvers,
                )?;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                new_exprs.append(&mut module_exprs);

                // Probably want to evaluate a module even if it has no provides?
                if !new_module.provides.is_empty() {
                    new_exprs.push(new_module.compile_module()?);
                } else {
                    // log::debug!(target: "requires", "Found no provides, skipping compilation of module: {:?}", new_module.name);
                }
            }

            // At this point, requires should be fully qualified (absolute) paths

            for (module, require_statement_span) in self
                .require_objects
                .iter()
                .filter(|x| matches!(x.path, PathOrBuiltIn::Path(_)))
                .map(|x| (x.path.get_path(), x.span))
            {
                if cfg!(target_arch = "wasm32") {
                    stop!(Generic => "requiring modules is not supported for wasm");
                }

                let last_modified = std::fs::metadata(module.as_ref())
                    .map_err(|err| {
                        let mut err = crate::SteelErr::from(err);
                        err.prepend_message(&format!(
                            "Attempting to load module from: {:?} ",
                            module
                        ));
                        err.set_span(require_statement_span)
                    })?
                    .modified()?;

                // Check if we should compile based on the last time modified
                // If we're unable to get information, we want to compile
                let should_recompile =
                    if let Some(cached_modified) = self.file_metadata.get(module.as_ref()) {
                        last_modified != *cached_modified
                    } else {
                        true
                    };

                let mut downstream_validated = true;

                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                if !should_recompile {
                    // If we already have compiled this module, get it from the cache
                    if let Some(m) = self.compiled_modules.get(module.as_ref()) {
                        // debug!("Getting {:?} from the module cache", module);
                        // println!("Already found in the cache: {:?}", module);
                        // new_exprs.push(m.to_module_ast_node());
                        // No need to do anything

                        // Check the dependencies all the way down.
                        // if any are invalidated, re-compile the whole thing.
                        let mut stack = m.downstream.clone();

                        while let Some(next) = stack.pop() {
                            let meta = std::fs::metadata(&next)?;

                            if let Some(prev) = self.file_metadata.get(&next) {
                                if *prev != meta.modified()? {
                                    // println!(
                                    //     "Detected change in {:?}, recompiling root starting from {:?}",
                                    //     next,
                                    //     module.as_ref()
                                    // );
                                    downstream_validated = false;
                                    break;
                                }
                            }

                            if let Some(module) = self.compiled_modules.get(&next) {
                                for child in &module.downstream {
                                    stack.push(child.to_owned());
                                }
                            }
                        }

                        if downstream_validated {
                            continue;
                        }
                    }
                }

                let mut new_module = ModuleBuilder::new_from_path(
                    module.into_owned(),
                    self.compiled_modules,
                    self.visited,
                    self.file_metadata,
                    self.sources,
                    self.kernel,
                    self.builtin_modules.clone(),
                    self.global_macro_map,
                    self.custom_builtins,
                    self.search_dirs,
                    self.module_resolvers,
                )?;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                // debug!("Inside {:?} - append {:?}", self.name, module);
                // if log_enabled!(log::Level::Debug) {
                //     debug!(
                //         target: "modules",
                //         "appending with {:?}",
                //         module_exprs.iter().map(|x| x.to_string()).join(" SEP ")
                //     );
                // }

                new_exprs.append(&mut module_exprs);

                // TODO evaluate this

                // let mut ast = std::mem::replace(&mut new_module.source_ast, Vec::new());
                // ast.append(&mut module_exprs);
                // new_module.source_ast = ast;

                // dbg!(&new_module.name);
                // dbg!(&new_module.compiled_modules.contains_key(&new_module.name));

                // If we need to, revisit because there are new provides
                if !new_module.provides.is_empty() {
                    new_exprs.push(new_module.compile_module()?);
                // If the module hasn't yet been compiled, compile it anyway
                } else if !new_module.compiled_modules.contains_key(&new_module.name) {
                    // else if !new_module.compiled_modules.contains_key(&new_module.name) {
                    new_exprs.push(new_module.compile_module()?);
                } else if !downstream_validated {
                    new_exprs.push(new_module.compile_module()?);

                    // log::debug!(target: "requires", "Found no provides, skipping compilation of module: {:?}", new_module.name);
                    // log::debug!(target: "requires", "Module already in the cache: {}", new_module.compiled_modules.contains_key(&new_module.name));
                    // log::debug!(target: "requires", "Compiled modules: {:?}", new_module.compiled_modules.keys().collect::<Vec<_>>());
                }

                // else {
                //     log::debug!(target: "requires", "Found no provides, skipping compilation of module: {:?}", new_module.name);
                // }
            }
        }

        // new_exprs.pretty_print();

        Ok(new_exprs)
    }

    // TODO: This should run again on itself, probably
    fn compile_module(&mut self) -> Result<ExprKind> {
        let mut ast = std::mem::take(&mut self.source_ast);
        let mut provides = std::mem::take(&mut self.provides);
        // Clone the requires... I suppose
        let requires = self.require_objects.clone();

        // info!(
        //     target: "requires",
        //     "Into compiled module: provides for syntax: {:?}",
        //     self.provides_for_syntax
        // );

        // Attempt extracting the syntax transformers from this module
        if let Some(kernel) = self.kernel.as_mut() {
            kernel.load_syntax_transformers(&mut ast, self.name.to_str().unwrap().to_string())?
        };

        for expr in ast.iter_mut() {
            expand(expr, &self.macro_map)?;

            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                self.builtin_modules.clone(),
                // Expanding macros in the environment?
                self.name.to_str().unwrap(),
            )?;

            expand(expr, &self.macro_map)?;
        }

        // Expand first with the macros from *this* module
        // ast = ast
        //     .into_iter()
        //     .map(|x| {
        //         expand(x, &self.macro_map)
        //             .and_then(|x| {
        //                 expand_kernel_in_env(
        //                     x,
        //                     self.kernel.as_mut(),
        //                     self.builtin_modules.clone(),
        //                     // Expanding macros in the environment?
        //                     self.name.to_str().unwrap().to_string(),
        //                 )
        //             })
        //             // Check here - I think it makes sense to expand the
        //             // internal macros again, given now we might have defmacro
        //             // style macros that get expanded into syntax-rules ones?
        //             .and_then(|x| expand(x, &self.macro_map))
        //     })
        //     .collect::<Result<Vec<_>>>()?;

        for expr in provides.iter_mut() {
            expand(expr, &self.macro_map)?;
            // .and_then(|x| {
            // expand_kernel(x, self.kernel.as_mut(), self.builtin_modules.clone())
            expand_kernel_in_env(
                expr,
                self.kernel.as_mut(),
                self.builtin_modules.clone(),
                // Expanding macros in the environment?
                &self.name.to_str().unwrap(),
            )?;
            // })
        }

        // Expand provides for any macros that exist within there
        // provides = provides
        //     .into_iter()
        //     .map(|x| {
        //         expand(x, &self.macro_map).and_then(|x| {
        //             // expand_kernel(x, self.kernel.as_mut(), self.builtin_modules.clone())
        //             expand_kernel_in_env(
        //                 x,
        //                 self.kernel.as_mut(),
        //                 self.builtin_modules.clone(),
        //                 // Expanding macros in the environment?
        //                 self.name.to_str().unwrap().to_string(),
        //             )
        //         })
        //     })
        //     .collect::<Result<Vec<_>>>()?;

        let mut mangled_asts = Vec::with_capacity(ast.len() + 16);
        let mut downstream = Vec::new();

        // Look for the modules in the requires for syntax
        for require_object in self.require_objects.iter()
        // .filter(|x| x.for_syntax)
        {
            let require_for_syntax = require_object.path.get_path();

            if let PathOrBuiltIn::Path(_) = &require_object.path {
                downstream.push(require_for_syntax.clone().into_owned());
            }

            let (module, in_scope_macros, mut name_mangler) = ModuleManager::find_in_scope_macros(
                self.compiled_modules,
                require_for_syntax.as_ref(),
                &require_object,
                &mut mangled_asts,
            );

            // let kernel_macros_in_scope: HashSet<_> =
            //     module.provides_for_syntax.iter().cloned().collect();

            // let module_name = Cow::from(module.name.to_str().unwrap().to_string());

            for expr in ast.iter_mut() {
                // First expand the in scope macros
                // These are macros
                let mut expander = Expander::new(&in_scope_macros);
                expander.expand(expr)?;
                let changed = false;

                // dbg!(expander.changed);

                // (first_round_expanded, changed) = expand_kernel_in_env_with_allowed(
                //     first_round_expanded,
                //     self.kernel.as_mut(),
                //     // We don't need to expand those here
                //     ModuleContainer::default(),
                //     module.name.to_str().unwrap().to_string(),
                //     &kernel_macros_in_scope,
                // )?;

                // If the kernel expander expanded into something - go ahead
                // and expand all of the macros in this
                // if changed || expander.changed {
                // Expand here?
                // first_round_expanded = expand(first_round_expanded, &module.macro_map)?;

                // Probably don't need this
                // (first_round_expanded, changed) = expand_kernel_in_env_with_change(
                //     first_round_expanded,
                //     self.kernel.as_mut(),
                //     ModuleContainer::default(),
                //     module.name.to_str().unwrap().to_string(),
                // )?;

                // name_mangler.visit(&mut first_round_expanded);
                // }

                if expander.changed || changed {
                    // let source_id = self.sources.get_source_id(&module.name).unwrap();

                    let mut fully_expanded = expr;
                    expand(&mut fully_expanded, &module.macro_map)?;

                    // Expanding the kernel with only these macros...
                    let changed = expand_kernel_in_env_with_change(
                        &mut fully_expanded,
                        self.kernel.as_mut(),
                        // We don't need to expand those here
                        ModuleContainer::default(),
                        &module.name.to_str().unwrap(),
                        // &kernel_macros_in_scope,
                    )?;

                    if changed {
                        name_mangler.visit(&mut fully_expanded);
                    }

                    // lifted_kernel_environments.insert(
                    //     module_name.clone(),
                    //     KernelDefMacroSpec {
                    //         env: module_name,
                    //         exported: None,
                    //         name_mangler: name_mangler.clone(),
                    //     },
                    // );

                    // Ok(fully_expanded)
                }
                // else {
                //     Ok(first_round_expanded)
                // }
            }

            // ast = ast
            //     .into_iter()
            //     .map(|x| {
            //         // First expand the in scope macros
            //         // These are macros
            //         let mut expander = Expander::new(&in_scope_macros);
            //         let mut first_round_expanded = expander.expand(x)?;
            //         let mut changed = false;

            //         // dbg!(expander.changed);

            //         // (first_round_expanded, changed) = expand_kernel_in_env_with_allowed(
            //         //     first_round_expanded,
            //         //     self.kernel.as_mut(),
            //         //     // We don't need to expand those here
            //         //     ModuleContainer::default(),
            //         //     module.name.to_str().unwrap().to_string(),
            //         //     &kernel_macros_in_scope,
            //         // )?;

            //         // If the kernel expander expanded into something - go ahead
            //         // and expand all of the macros in this
            //         // if changed || expander.changed {
            //         // Expand here?
            //         // first_round_expanded = expand(first_round_expanded, &module.macro_map)?;

            //         // Probably don't need this
            //         // (first_round_expanded, changed) = expand_kernel_in_env_with_change(
            //         //     first_round_expanded,
            //         //     self.kernel.as_mut(),
            //         //     ModuleContainer::default(),
            //         //     module.name.to_str().unwrap().to_string(),
            //         // )?;

            //         // name_mangler.visit(&mut first_round_expanded);
            //         // }

            //         if expander.changed || changed {
            //             // let source_id = self.sources.get_source_id(&module.name).unwrap();

            //             let mut fully_expanded = first_round_expanded;
            //             expand(&mut fully_expanded, &module.macro_map)?;

            //             let module_name = module.name.to_str().unwrap().to_string();

            //             // Expanding the kernel with only these macros...
            //             let changed = expand_kernel_in_env_with_change(
            //                 &mut fully_expanded,
            //                 self.kernel.as_mut(),
            //                 // We don't need to expand those here
            //                 ModuleContainer::default(),
            //                 module_name.clone(),
            //                 // &kernel_macros_in_scope,
            //             )?;

            //             if changed {
            //                 name_mangler.visit(&mut fully_expanded);
            //             }

            //             // lifted_kernel_environments.insert(
            //             //     module_name.clone(),
            //             //     KernelDefMacroSpec {
            //             //         env: module_name,
            //             //         exported: None,
            //             //         name_mangler: name_mangler.clone(),
            //             //     },
            //             // );

            //             Ok(fully_expanded)
            //         } else {
            //             Ok(first_round_expanded)
            //         }
            //     })
            //     .collect::<Result<_>>()?;

            for expr in provides.iter_mut() {
                // First expand the in scope macros
                // These are macros
                let mut expander = Expander::new(&in_scope_macros);
                expander.expand(expr)?;

                if expander.changed {
                    expand(expr, &module.macro_map)?
                }
                // else {
                //     Ok(first_round_expanded)
                // }
            }

            // provides = provides
            //     .into_iter()
            //     .map(|x| {
            //         // First expand the in scope macros
            //         // These are macros
            //         let mut expander = Expander::new(&in_scope_macros);
            //         let first_round_expanded = expander.expand(x)?;

            //         expander.expand(&mut expr);

            //         if expander.changed {
            //             expand(first_round_expanded, &module.macro_map)
            //         } else {
            //             Ok(first_round_expanded)
            //         }
            //     })
            //     .collect::<Result<_>>()?;
        }

        // let requires_before = self.require_objects.len();

        // self.collect_requires()?;

        // if self.require_objects.len() > requires_before {
        //     println!("EXPANDED INTO A REQUIRE");
        // }

        // TODO: Check HERE for whether there are more requires than were previously found.
        // If so, we should go back and compile the module again

        for expr in &mut ast {
            lower_entire_ast(expr)?;
            FlattenBegin::flatten(expr);
        }

        // TODO: @Matt - fix this hack
        {
            self.source_ast = ast;
            self.provides = provides;

            // println!("Collecting provides again:");
            // println!("{}", self.source_ast);
            // self.source_ast.pretty_print();
            self.collect_provides()?;

            provides = std::mem::take(&mut self.provides);
            ast = std::mem::take(&mut self.source_ast);
        }

        // Put the mangled asts at the top
        // then include the ast there
        // mangled_asts.append(&mut ast);

        for expr in mangled_asts.iter_mut() {
            lower_entire_ast(expr)?;

            FlattenBegin::flatten(expr);
        }

        mangled_asts.append(&mut ast);

        // mangled_asts = mangled_asts
        //     .into_iter()
        //     .map(lower_entire_ast)
        //     // We want this to at least be flattened for querying later
        //     .map(|x| {
        //         x.map(|mut o| {
        //             FlattenBegin::flatten(&mut o);
        //             o
        //         })
        //     })
        //     .collect::<std::result::Result<_, ParseError>>()?;

        // Take ast, expand with self modules, then expand with each of the require for-syntaxes
        // Then mangle the require-for-syntax, include the mangled directly in the ast

        // @Matt: 11/15/2024
        // Try collecting the provides again?

        // TODO: Come back here - we're going to need to figure out the require objects
        let mut module = CompiledModule::new(
            self.name.clone(),
            provides,
            requires,
            self.provides_for_syntax
                .iter()
                .map(|x| *x.atom_identifier().unwrap())
                .collect(),
            // std::mem::take(&mut self.macro_map),
            self.macro_map.clone(),
            mangled_asts,
            downstream,
        );

        module.set_emitted(true);

        // println!(
        //     "-------------- Emitting module: {:?} ----------------------",
        //     self.name
        // );

        let result = module.to_top_level_module(self.compiled_modules, self.global_macro_map)?;

        // println!("{}", result.to_pretty(60));

        // println!("------------------ Finish ----------------------------------");

        // let mut analysis = Analysis::from_exprs(&[result]);

        // let mut semantic = SemanticAnalysis::from_analysis(&mut result, analysis);

        // // This is definitely broken still
        // semantic
        //     .remove_unused_globals_with_prefix("mangler");

        // log::debug!(target: "requires", "Adding compiled module: {:?}", self.name);
        // println!("Adding compiled module: {:?}", self.name);
        // for (key, smacro) in module.macro_map.iter() {
        //     println!("{}", key.resolve());
        //     for expr in smacro.exprs() {
        //         println!("{}", expr);
        //     }
        // }

        self.compiled_modules.insert(self.name.clone(), module);

        Ok(result)
    }

    fn extract_macro_defs(&mut self) -> Result<()> {
        // Probably don't have more than 128 macros in a module, but who knows?
        // let mut macro_indices = SmallVec::<[usize; 128]>::new();

        // let exprs = std::mem::take(&mut self.source_ast);

        let mut error = None;

        self.source_ast.retain_mut(|expr| {
            if let ExprKind::Macro(_) = expr {
                // Replace with dummy begin value so we don't have to copy
                // everything other for every macro definition
                let mut taken_expr = ExprKind::Begin(Box::new(Begin::new(
                    Vec::new(),
                    SyntaxObject::default(TokenType::Begin),
                )));

                std::mem::swap(expr, &mut taken_expr);

                if let ExprKind::Macro(m) = taken_expr {
                    match SteelMacro::parse_from_ast_macro(m) {
                        Ok(generated_macro) => {
                            let name = generated_macro.name();

                            Arc::make_mut(&mut self.macro_map).insert(*name, generated_macro);
                        }
                        Err(e) => {
                            if error.is_none() {
                                error = Some(e);
                            }
                            // error = Some(e);
                            return false;
                        }
                    }
                } else {
                    unreachable!();
                }

                return false;
            }

            true
        });

        if let Some(e) = error {
            return Err(e);
        }

        Ok(())
    }

    // Takes out the (for-syntax) forms from the provides
    fn filter_out_for_syntax_provides(&mut self, exprs: Vec<ExprKind>) -> Result<Vec<ExprKind>> {
        let mut normal_provides = Vec::new();

        for expr in exprs {
            match &expr {
                ExprKind::Atom(_) => {
                    normal_provides.push(expr);
                }
                ExprKind::List(l) => {
                    if let Some(for_syntax) = l.first_ident() {
                        match *for_syntax {
                            x if x == *FOR_SYNTAX => {
                                if l.args.len() != 2 {
                                    stop!(ArityMismatch => "provide expects a single identifier in the (for-syntax <ident>)"; l.location)
                                }

                                // Collect the for syntax expressions
                                // TODO -> remove this clone
                                self.provides_for_syntax.push(l.args[1].clone());
                            }
                            x if x == *REQUIRE_IDENT_SPEC => {
                                normal_provides.push(expr);
                            }
                            // x if x == *CONTRACT_OUT || x == *REQUIRE_IDENT_SPEC => {
                            //     normal_provides.push(expr);
                            // }
                            _ => {
                                normal_provides.push(expr);
                                // stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
                            }
                        }
                    } else {
                        stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)"; l.location)
                    }
                }
                _ => {
                    stop!(TypeMismatch => "provide expects either a (for-syntax <ident>) or an ident"; opt expr.span())
                }
            }
        }

        Ok(normal_provides)
    }

    // TODO -> collect (provide (for-syntax ...))
    // I think these will already be collected for the macro, however I think for syntax should be found earlier
    // Otherwise the macro expansion will not be able to understand it
    fn collect_provides(&mut self) -> Result<()> {
        // let now = std::time::Instant::now();

        let mut non_provides = Vec::with_capacity(self.source_ast.len());
        let exprs = std::mem::take(&mut self.source_ast);

        fn walk(
            module_builder: &mut ModuleBuilder,
            exprs: Vec<ExprKind>,
            non_provides: &mut Vec<ExprKind>,
        ) -> Result<()> {
            for mut expr in exprs {
                match &mut expr {
                    ExprKind::List(l) => {
                        if let Some(provide) = l.first_ident() {
                            if *provide == *PROVIDE {
                                if l.len() == 1 {
                                    stop!(Generic => "provide expects at least one identifier to provide"; l.location);
                                }

                                // Swap out the value inside the list
                                let args = std::mem::take(&mut l.args);

                                let filtered =
                                    module_builder.filter_out_for_syntax_provides(args)?;

                                l.args = filtered;

                                module_builder.provides.push(expr);

                                continue;
                            }
                        }
                    }
                    ExprKind::Begin(b) => {
                        let exprs = std::mem::take(&mut b.exprs);

                        // Reserve capacity for these to be moved to the top level
                        non_provides.reserve(exprs.len());

                        walk(module_builder, exprs, non_provides)?;
                    }
                    _ => {}
                }

                non_provides.push(expr);
            }

            Ok(())
        }

        walk(self, exprs, &mut non_provides)?;

        self.source_ast = non_provides;

        // log::debug!(target: "pipeline_time", "Collecting provides time: {:?}", now.elapsed());

        Ok(())
    }

    fn parse_require_object(
        &mut self,
        home: &Option<PathBuf>,
        r: &crate::parser::ast::Require,
        atom: &ExprKind,
    ) -> Result<RequireObject> {
        let mut object = RequireObjectBuilder::default();

        // Set the span so we can pass through an error if the
        // module is not found
        object.span = r.location.span;

        self.parse_require_object_inner(home, r, atom, &mut object)
            .and_then(|_| object.build())
    }

    // TODO: Recursively crunch the requires to gather up the necessary information
    fn parse_require_object_inner(
        &mut self,
        home: &Option<PathBuf>,
        r: &crate::parser::ast::Require,
        atom: &ExprKind,
        require_object: &mut RequireObjectBuilder,
    ) -> Result<()> {
        match atom {
            ExprKind::Atom(Atom {
                syn:
                    SyntaxObject {
                        ty: TokenType::StringLiteral(s),
                        span,
                        ..
                    },
            }) => {
                if require_object.path.is_some() {
                    stop!(Generic => "require object only expects one path!")
                }

                // Try this?
                if let Some(lib) = BUILT_INS.into_iter().cloned().find(|x| x.0 == s.as_str()) {
                    // self.built_ins.push(PathBuf::from(lib.0));

                    require_object.path = Some(PathOrBuiltIn::BuiltIn(lib.0.into()));

                    return Ok(());
                    // continue;
                }

                if self.custom_builtins.contains_key(s.as_str()) {
                    require_object.path =
                        Some(PathOrBuiltIn::BuiltIn(s.clone().to_string().into()));

                    return Ok(());
                }

                if self
                    .module_resolvers
                    .iter()
                    .find(|x| x.exists(s.as_str()))
                    .is_some()
                {
                    require_object.path =
                        Some(PathOrBuiltIn::BuiltIn(s.clone().to_string().into()));

                    return Ok(());
                }

                if cfg!(target_arch = "wasm32") {
                    stop!(Generic => "requiring modules is not supported for wasm");
                }

                let mut current = self.name.clone();
                if current.is_file() {
                    current.pop();
                }
                current.push(PathBuf::from(s.as_str()));

                // // If the path exists on its own, we can continue
                // // But theres the case where we're searching for a module on the STEEL_HOME
                if !current.exists() {
                    if let Some(mut home) = home.clone() {
                        home.push(PathBuf::from(s.as_str()));
                        current = home;

                        log::info!("Searching STEEL_HOME for {:?}", current);

                        if !current.exists() {
                            for dir in self.search_dirs {
                                let mut dir = dir.clone();
                                dir.push(s.as_str());

                                if dir.exists() {
                                    current = dir;
                                    break;
                                }
                            }
                        }
                    } else {
                        // TODO: Check if this module exists in STEEL_HOME first. If it does, we'll take that as our candidate
                        // and then continue on to the final module resolution part.
                        //
                        // If it doesn't exist, we should iterate through the search directories and attempt to find
                        // a matching path there.

                        for dir in self.search_dirs {
                            let mut dir = dir.clone();
                            dir.push(s.as_str());

                            if dir.exists() {
                                current = dir;
                                break;
                            }
                        }

                        stop!(Generic => format!("Module not found: {:?} with STEEL_HOME: {:?}", current, home); *span)
                    }
                }

                // Get the absolute path and store that
                // self.requires.push(current)

                let current = try_canonicalize(current);

                require_object.path = Some(PathOrBuiltIn::Path(current));
            }

            // TODO: Requires with qualifiers, that aren't just for-syntax
            // Perhaps like:
            // (with-prefix <xyz>)
            ExprKind::List(l) => {
                match l.first_ident() {
                    Some(x) if *x == *ONLY_IN => {
                        if l.args.len() < 2 {
                            stop!(BadSyntax => "only-in expects a require-spec and optionally a list of ids to bind (maybe renamed)"; l.location);
                        }

                        self.parse_require_object_inner(home, r, &l.args[1], require_object)?;

                        for remaining in &l.args[2..] {
                            match remaining {
                                ExprKind::Atom(_) => {
                                    require_object
                                        .idents_to_import
                                        .push(MaybeRenamed::Normal(remaining.clone()));
                                }
                                ExprKind::List(l) => {
                                    if l.len() != 2 {
                                        stop!(BadSyntax => "Expected a pair when renaming required identifiers");
                                    }

                                    let from = &l.args[0];
                                    let to = &l.args[1];

                                    if from.atom_identifier().is_none()
                                        || to.atom_identifier().is_none()
                                    {
                                        stop!(BadSyntax => "only-in expected identifiers to rename");
                                    }

                                    // (<from> <to>)
                                    require_object
                                        .idents_to_import
                                        .push(MaybeRenamed::Renamed(from.clone(), to.clone()));
                                }
                                _ => {
                                    stop!(BadSyntax => "unexpected syntax in only-in form during module requires")
                                }
                            }
                        }
                    }

                    Some(x) if *x == *PREFIX_IN => {
                        if l.args.len() != 3 {
                            stop!(BadSyntax => "prefix-in expects a prefix to prefix a given file or module"; l.location);
                        }

                        let prefix = &l.args[1];

                        if let Some(prefix) = prefix.atom_identifier() {
                            match &mut require_object.prefix {
                                Some(existing_prefix) => {
                                    // Append the new symbol to the existing prefix
                                    existing_prefix.push_str(prefix.resolve());
                                }
                                None => {
                                    require_object.prefix = Some(prefix.resolve().to_string());
                                }
                            }

                            self.parse_require_object_inner(home, r, &l.args[2], require_object)?;
                        } else {
                            stop!(TypeMismatch => "prefix-in expects an identifier to use for the prefix"; opt prefix.span());
                        }
                    }

                    Some(x) if *x == *FOR_SYNTAX => {
                        // We're expecting something like (for-syntax "foo")
                        if l.args.len() != 2 {
                            stop!(BadSyntax => "for-syntax expects one string literal referring to a file or module"; l.location);
                        }

                        let mod_name = &l.args[1];
                        if let Some(path) = mod_name.string_literal() {
                            if let Some(lib) = BUILT_INS.iter().find(|x| x.0 == path) {
                                // self.built_ins.push(PathBuf::from(lib.0));

                                require_object.path = Some(PathOrBuiltIn::BuiltIn(lib.0.into()));
                                require_object.for_syntax = true;

                                return Ok(());
                                // continue;
                            } else if self.custom_builtins.contains_key(path) {
                                require_object.path =
                                    Some(PathOrBuiltIn::BuiltIn(Cow::Owned(path.to_string())));
                                require_object.for_syntax = true;

                                return Ok(());
                            } else if self
                                .module_resolvers
                                .iter()
                                .find(|x| x.exists(path))
                                .is_some()
                            {
                                require_object.path =
                                    Some(PathOrBuiltIn::BuiltIn(Cow::Owned(path.to_string())));
                                require_object.for_syntax = true;
                            } else {
                                let mut current = self.name.clone();
                                if current.is_file() {
                                    current.pop();
                                }
                                current.push(PathBuf::from(path));

                                if !current.exists() {
                                    if let Some(mut home) = home.clone() {
                                        home.push(PathBuf::from(path));
                                        current = home;

                                        if !current.exists() {
                                            for dir in self.search_dirs {
                                                let mut dir = dir.clone();
                                                dir.push(path);

                                                if dir.exists() {
                                                    current = dir;
                                                    break;
                                                }
                                            }
                                        }

                                        log::info!("Searching STEEL_HOME for {:?}", current);
                                    } else {
                                        for dir in self.search_dirs {
                                            let mut dir = dir.clone();
                                            dir.push(path);

                                            if dir.exists() {
                                                current = dir;
                                                break;
                                            }
                                        }

                                        stop!(Generic => format!("Module not found: {:?}", current); mod_name.span().unwrap())
                                    }
                                }

                                require_object.for_syntax = true;
                                let current = try_canonicalize(current);
                                require_object.path = Some(PathOrBuiltIn::Path(current));
                            }
                        } else {
                            stop!(BadSyntax => "for-syntax expects a string literal referring to a file or module"; opt mod_name.span());
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "require accepts either a string literal, a for-syntax expression or an only-in expression"; l.location)
                    }
                }
            }

            unknown => {
                stop!(Generic => format!("require object expected a string literal referring to a file/module, found: {}", unknown); opt atom.span())
            }
        }

        Ok(())
    }

    fn collect_requires(&mut self) -> Result<()> {
        // unimplemented!()

        let mut exprs_without_requires = Vec::new();
        let exprs = std::mem::take(&mut self.source_ast);

        let home = STEEL_HOME
            .clone()
            .map(|x| {
                // TODO: Fix this - try to hack in a root drive
                // for windows if a unix path is provided
                if cfg!(target_os = "windows") {
                    let mut result = x.trim_start_matches("/").to_string();

                    let mut iter = result.chars();
                    iter.next();
                    if matches!(iter.next(), Some(':')) {
                        return PathBuf::from(result);
                    }

                    result.insert(1, ':');
                    return PathBuf::from(result);
                }

                PathBuf::from(x)
            })
            .map(|mut x| {
                x.push("cogs");

                x
            });

        fn walk(
            module_builder: &mut ModuleBuilder,
            home: &Option<PathBuf>,
            exprs_without_requires: &mut Vec<ExprKind>,
            exprs: Vec<ExprKind>,
        ) -> Result<()> {
            for expr in exprs {
                match expr {
                    // Include require/for-syntax here
                    // This way we have some understanding of what dependencies a file has
                    ExprKind::Require(r) => {
                        for atom in &r.modules {
                            // TODO: Consider making this not a reference for r
                            let require_object =
                                module_builder.parse_require_object(&home, &r, atom)?;

                            module_builder.require_objects.push(require_object);
                        }
                    }
                    ExprKind::Begin(b) => {
                        walk(module_builder, home, exprs_without_requires, b.exprs)?
                    }
                    _ => exprs_without_requires.push(expr),
                }
            }

            Ok(())
        }

        walk(self, &home, &mut exprs_without_requires, exprs)?;

        self.source_ast = exprs_without_requires;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn new_built_in(
        name: PathBuf,
        input: Cow<'static, str>,
        compiled_modules: &'a mut FxHashMap<PathBuf, CompiledModule>,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
    ) -> Result<Self> {
        ModuleBuilder::raw(
            name,
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            &[],
            module_resolvers,
            false,
        )
        .parse_builtin(input)
    }

    fn new_from_path(
        name: PathBuf,
        compiled_modules: &'a mut FxHashMap<PathBuf, CompiledModule>,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        search_dirs: &'a [PathBuf],
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
    ) -> Result<Self> {
        ModuleBuilder::raw(
            name,
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            search_dirs,
            module_resolvers,
            true,
        )
        .parse_from_path()
    }

    fn raw(
        name: PathBuf,
        compiled_modules: &'a mut FxHashMap<PathBuf, CompiledModule>,
        visited: &'a mut FxHashSet<PathBuf>,
        file_metadata: &'a mut crate::HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a FxHashMap<InternedString, SteelMacro>,
        custom_builtins: &'a HashMap<String, String>,
        search_dirs: &'a [PathBuf],
        module_resolvers: &'a [Arc<dyn SourceModuleResolver>],
        canonicalize: bool,
    ) -> Self {
        // println!("New module found: {:?}", name);

        let name = if canonicalize {
            try_canonicalize(name)
        } else {
            name
        };

        ModuleBuilder {
            name,
            main: false,
            source_ast: Vec::new(),
            // TODO: This used to be empty
            macro_map: default_prelude_macros(),
            require_objects: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
            custom_builtins,
            search_dirs,
            module_resolvers,
        }
    }

    fn parse_builtin(mut self, input: Cow<'static, str>) -> Result<Self> {
        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let id = self
            .sources
            .add_source(input.clone(), Some(self.name.clone()));

        let parsed = Parser::new_from_source(&input, self.name.clone(), Some(id))
            .without_lowering()
            .map(|x| x.and_then(lower_macro_and_require_definitions))
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        #[cfg(feature = "profiling")]
        log::debug!(target: "pipeline_time", "Parsing: {:?} - {:?}", self.name, now.elapsed());

        // self.source_ast.pretty_print();

        Ok(self)
    }

    fn parse_from_path(mut self) -> Result<Self> {
        log::info!("Opening: {:?}", self.name);
        let mut exprs = String::new();

        // If we were unable to resolve it via any of the built in module resolvers,
        // then we check the file system.
        let mut file = std::fs::File::open(&self.name).map_err(|err| {
            let mut err = crate::SteelErr::from(err);
            err.prepend_message(&format!("Attempting to load module from: {:?}", self.name));
            err
        })?;
        self.file_metadata
            .insert(self.name.clone(), file.metadata()?.modified()?);

        file.read_to_string(&mut exprs)?;

        let mut expressions = Parser::new(&PRELUDE_STRING, SourceId::none())
            .without_lowering()
            .map(|x| x.and_then(lower_macro_and_require_definitions))
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        let id = self.sources.add_source(exprs, Some(self.name.clone()));

        {
            // Fetch the exprs after adding them to the sources
            // We did _just_ add it, so its fine to unwrap
            let guard = self.sources.sources.lock().unwrap();

            let exprs = guard.get(id).unwrap();

            let mut parsed = Parser::new_from_source(&exprs, self.name.clone(), Some(id))
                .without_lowering()
                .map(|x| x.and_then(lower_macro_and_require_definitions))
                .collect::<std::result::Result<Vec<_>, ParseError>>()?;

            expressions.append(&mut parsed);

            self.source_ast = expressions;
        }

        Ok(self)
    }
}
