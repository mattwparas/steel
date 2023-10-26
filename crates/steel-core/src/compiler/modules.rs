#![allow(unused)]
use crate::{
    compiler::{
        passes::{
            analysis::{Analysis, SemanticAnalysis},
            VisitorMutRefUnit,
        },
        program::PROVIDE,
    },
    expr_list,
    parser::{
        ast::{AstTools, Atom, Begin, Define, ExprKind, List, Quote},
        expand_visitor::expand_kernel,
        interner::InternedString,
        kernel::Kernel,
        parser::{ParseError, Parser, Sources, SyntaxObject},
        tokens::TokenType,
    },
    steel_vm::{engine::ModuleContainer, transducers::interleave},
};
use crate::{parser::expand_visitor::Expander, rvals::Result};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    io::Read,
    path::PathBuf,
};

use crate::parser::expander::SteelMacro;
use crate::stop;

use std::time::SystemTime;

use crate::parser::expand_visitor::{expand, extract_macro_defs};

// use itertools::Itertools;
use log::{debug, info, log_enabled};

use crate::parser::ast::IteratorExtensions;

use super::{
    passes::mangle::{collect_globals, NameMangler, NameUnMangler},
    program::{CONTRACT_OUT, FOR_SYNTAX, ONLY_IN, PREFIX_IN, REQUIRE_IDENT_SPEC},
};

static OPTION: &str = include_str!("../scheme/modules/option.scm");
static OPTION_NAME: &str = "steel/option";

static RESULT: &str = include_str!("../scheme/modules/result.scm");
static RESULT_NAME: &str = "steel/result";

static CONTRACT: &str = include_str!("../scheme/modules/contracts.scm");
static CONTRACT_NAME: &str = "#%private/steel/contract";

static ITERATORS: &str = include_str!("../scheme/modules/iterators.scm");
static ITERATORS_NAME: &str = "steel/iterators";

static MUTABLE_VECTORS: &str = include_str!("../scheme/modules/mvector.scm");
static MUTABLE_VECTORS_NAME: &str = "steel/mutable-vectors";

static BUILT_INS: &[(&str, &str)] = &[
    (OPTION_NAME, OPTION),
    (RESULT_NAME, RESULT),
    (CONTRACT_NAME, CONTRACT),
    (ITERATORS_NAME, ITERATORS),
    (MUTABLE_VECTORS_NAME, MUTABLE_VECTORS),
];

pub(crate) const MANGLER_SEPARATOR: &str = "__%#__";

/// Manages the modules
/// keeps some visited state on the manager for traversal
/// Also keeps track of the metadata for each file in order to determine
/// if it needs to be recompiled
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct ModuleManager {
    compiled_modules: HashMap<PathBuf, CompiledModule>,
    file_metadata: HashMap<PathBuf, SystemTime>,
    visited: HashSet<PathBuf>,
    custom_builtins: HashMap<String, String>,
}

impl ModuleManager {
    pub(crate) fn new(
        compiled_modules: HashMap<PathBuf, CompiledModule>,
        file_metadata: HashMap<PathBuf, SystemTime>,
    ) -> Self {
        ModuleManager {
            compiled_modules,
            file_metadata,
            visited: HashSet::new(),
            custom_builtins: HashMap::new(),
        }
    }

    pub fn add_builtin_module(&mut self, module_name: String, text: String) {
        self.custom_builtins.insert(module_name, text);
    }

    pub fn modules(&self) -> &HashMap<PathBuf, CompiledModule> {
        &self.compiled_modules
    }

    pub fn modules_mut(&mut self) -> &mut HashMap<PathBuf, CompiledModule> {
        &mut self.compiled_modules
    }

    pub(crate) fn default() -> Self {
        Self::new(HashMap::new(), HashMap::new())
    }

    // Add the module directly to the compiled module cache
    pub(crate) fn add_module(
        &mut self,
        path: PathBuf,
        global_macro_map: &mut HashMap<InternedString, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        builtin_modules: ModuleContainer,
    ) -> Result<()> {
        // todo!()

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
        )?;

        module_builder.compile()?;

        // println!("{:#?}", self.compiled_modules);

        Ok(())
    }

    #[allow(unused)]
    pub(crate) fn compile_main(
        &mut self,
        global_macro_map: &mut HashMap<InternedString, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        builtin_modules: ModuleContainer,
    ) -> Result<Vec<ExprKind>> {
        // Wipe the visited set on entry
        self.visited.clear();

        // TODO
        // This is also explicitly wrong -> we should separate the global macro map from the macros found locally in this module
        // For instance, (cond) is global, but (define-syntax blagh) might be local to main
        // if a module then defines a function (blagh) that is used inside its scope, this would expand the macro in that scope
        // which we do not want
        let non_macro_expressions = extract_macro_defs(exprs, global_macro_map)?;

        let mut module_builder = ModuleBuilder::main(
            path,
            non_macro_expressions,
            &mut self.compiled_modules,
            &mut self.visited,
            &mut self.file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
        )?;

        let mut module_statements = module_builder.compile()?;

        // println!("Compiled modules: {:?}", module_builder.compiled_modules);

        // Expand the ast first with the macros from global/source file
        let mut ast = module_builder
            .source_ast
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<Vec<_>>>()?;

        {
            module_builder.source_ast = ast;
            module_builder.collect_provides();

            ast = std::mem::take(&mut module_builder.source_ast);
        }

        let mut require_defines = Vec::new();

        let mut mangled_prefixes = module_builder
            .require_objects
            .iter()
            .filter(|x| !x.for_syntax)
            .map(|x| {
                "mangler".to_string() + x.path.get_path().to_str().unwrap() + MANGLER_SEPARATOR
            })
            .collect::<Vec<_>>();

        let mut explicit_requires = HashMap::new();

        for require_object in &module_builder.require_objects
        // .chain(module_builder.built_ins.iter())
        {
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
                log::info!(target: "modules", "No provides found for module, skipping: {:?}", path);

                continue;
            };

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    // println!("{}", provide);

                    // println!("Top level provide handler");

                    // Would be nice if this could be handled by some macro expansion...
                    // See if contract/out

                    let other_module_prefix =
                        "mangler".to_string() + module.name.to_str().unwrap() + MANGLER_SEPARATOR;

                    // TODO: Expand the contract out into something we expect
                    // Otherwise, this is going to blow up
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                match *qualifier {
                                    x if x == *CONTRACT_OUT => {
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

                                        // TODO: THe contract has to get mangled with the prefix as well?
                                        let contract = l.args.get(2).unwrap();

                                        let hash_get = expr_list![
                                            ExprKind::ident("%proto-hash-get%"),
                                            ExprKind::atom(
                                                "__module-".to_string() + &other_module_prefix
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

                                        let hash_get = expr_list![
                                            ExprKind::ident("%proto-hash-get%"),
                                            ExprKind::atom(
                                                "__module-".to_string() + &other_module_prefix
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
                                        stop!(TypeMismatch => "foo provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
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

                            let hash_get = expr_list![
                                ExprKind::ident("%proto-hash-get%"),
                                ExprKind::atom("__module-".to_string() + &other_module_prefix),
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

                            require_defines.push(define);
                        }
                        _ => {
                            stop!(TypeMismatch => "provide expression needs to either be a `contract/out` form or an identifier")
                        }
                    }
                }
            }
        }

        let mut mangled_asts = Vec::new();

        // TODO: Move this to the lower level as well
        // It seems we're only doing this expansion at the top level, but we _should_ do this at the lower level as well
        for require_for_syntax in module_builder
            .require_objects
            .iter()
            .filter(|x| x.for_syntax)
            .map(|x| x.path.get_path())
        {
            let (module, mut in_scope_macros) = Self::find_in_scope_macros(
                &self.compiled_modules,
                require_for_syntax.as_ref(),
                &mut mangled_asts,
            );

            // dbg!(&in_scope_macros);

            // for (key, value) in &mut in_scope_macros {
            //     for line in value.exprs_mut() {
            //         println!("{}", line);
            //     }
            // }

            // ast = ast.into_iter().map(|x| )

            // ast.pretty_print();

            ast = ast
                .into_iter()
                .map(|x| {
                    // First expand the in scope macros
                    // These are macros
                    let mut expander = Expander::new(&in_scope_macros);
                    let first_round_expanded = expander.expand(x)?;

                    if expander.changed {
                        expand(first_round_expanded, &module.macro_map)
                    } else {
                        Ok(first_round_expanded)
                    }

                    // expand(x, &module.macro_map)
                })
                .collect::<Result<_>>()?;

            // TODO: @Matt 10/16/12
            // This won't work if the macros expand to other private macros.
            // Tracking issue here: <TODO>
            global_macro_map.extend(in_scope_macros);
        }

        // Include the defines from the modules now imported
        module_statements.append(&mut require_defines);

        // The next two lines here expand _all_ of the source code with the top level macros
        // This is necessary because of the std library macros, although this should be able to be
        // solved with scoped imports of the standard library explicitly
        module_statements.append(&mut ast);

        // @Matt 7/4/23
        // TODO: With mangling, this could cause problems. We'll want to un-mangle quotes AFTER the macro has been expanded,
        // in order to preserve the existing behavior.
        module_statements
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<_>>()
    }

    fn find_in_scope_macros<'a>(
        compiled_modules: &'a HashMap<PathBuf, CompiledModule>,
        require_for_syntax: &'a PathBuf,
        mangled_asts: &'a mut Vec<ExprKind>,
    ) -> (&'a CompiledModule, HashMap<InternedString, SteelMacro>) {
        let module = compiled_modules
            .get(require_for_syntax)
            .expect(&format!("Module missing!: {:?}", require_for_syntax));

        let prefix = "mangler".to_string() + module.name.to_str().unwrap() + MANGLER_SEPARATOR;

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
        let in_scope_macros = module
            .provides_for_syntax
            .iter()
            .filter_map(|x| module.macro_map.get(x).map(|m| (*x, m.clone()))) // TODO -> fix this unwrap
            .map(|mut x| {
                for expr in x.1.exprs_mut() {
                    name_mangler.visit(expr);
                }

                x
            })
            .collect::<HashMap<_, _>>();
        // Check what macros are in scope here
        debug!(
            "In scope macros: {:#?}",
            in_scope_macros.keys().collect::<Vec<_>>()
        );
        (module, in_scope_macros)
    }

    #[cfg(not(feature = "modules"))]
    pub(crate) fn expand_expressions(
        &mut self,
        global_macro_map: &mut HashMap<InternedString, SteelMacro>,
        exprs: Vec<ExprKind>,
    ) -> Result<Vec<ExprKind>> {
        let non_macro_expressions = extract_macro_defs(exprs, global_macro_map)?;
        non_macro_expressions
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompiledModule {
    name: PathBuf,
    provides: Vec<ExprKind>,
    require_objects: Vec<RequireObject>,
    provides_for_syntax: Vec<InternedString>,
    pub(crate) macro_map: HashMap<InternedString, SteelMacro>,
    ast: Vec<ExprKind>,
    emitted: bool,
}

// TODO: @Matt 6/12/23 - This _should_ be serializable. If possible, we can try to store intermediate objects down to some file.
impl CompiledModule {
    pub fn new(
        name: PathBuf,
        provides: Vec<ExprKind>,
        require_objects: Vec<RequireObject>,
        provides_for_syntax: Vec<InternedString>,
        macro_map: HashMap<InternedString, SteelMacro>,
        ast: Vec<ExprKind>,
    ) -> Self {
        Self {
            name,
            provides,
            require_objects,
            provides_for_syntax,
            macro_map,
            ast,
            emitted: false,
        }
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
        modules: &HashMap<PathBuf, CompiledModule>,
        global_macro_map: &HashMap<InternedString, SteelMacro>,
    ) -> Result<ExprKind> {
        let mut globals = collect_globals(&self.ast);

        let mut exprs = self.ast.clone();
        let mut provide_definitions = Vec::new();

        let prefix = "mangler".to_string() + self.name.to_str().unwrap() + MANGLER_SEPARATOR;

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

            let other_module_prefix =
                "mangler".to_string() + module.name.to_str().unwrap() + MANGLER_SEPARATOR;

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                match *qualifier {
                                    x if x == *CONTRACT_OUT => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let mut name = l.args.get(1).unwrap().clone();
                                        let contract = l.args.get(2).unwrap();

                                        if !explicit_requires.is_empty()
                                            && !name
                                                .atom_identifier()
                                                .map(|x| explicit_requires.contains_key(x))
                                                .unwrap_or_default()
                                        {
                                            continue;
                                        }

                                        // If we have the alias listed, we should use it
                                        if !explicit_requires.is_empty() {
                                            if let Some(alias) = explicit_requires
                                                .get(name.atom_identifier().unwrap())
                                                .copied()
                                                .flatten()
                                            {
                                                *name.atom_identifier_mut().unwrap() =
                                                    alias.clone();
                                            }
                                        }

                                        if let Some(prefix) = &require_object.prefix {
                                            if let Some(existing) = name.atom_identifier_mut() {
                                                let mut prefixed_identifier = prefix.clone();
                                                prefixed_identifier.push_str(existing.resolve());

                                                // Update the existing identifier to point to a new one with the prefix applied
                                                *existing = prefixed_identifier.into();
                                            }
                                        }

                                        // Since this is now bound to be in the scope of the current working module, we also want
                                        // this to be mangled. In the event we do something like, qualify the import, then we might
                                        // have to mangle this differently
                                        globals.insert(*name.atom_identifier().unwrap());

                                        let hash_get = expr_list![
                                            ExprKind::ident("%proto-hash-get%"),
                                            ExprKind::atom(
                                                "__module-".to_string() + &other_module_prefix
                                            ),
                                            ExprKind::Quote(Box::new(Quote::new(
                                                name.clone(),
                                                SyntaxObject::default(TokenType::Quote)
                                            ))),
                                        ];

                                        let define = ExprKind::Define(Box::new(Define::new(
                                            ExprKind::atom(
                                                prefix.clone()
                                                    + name.atom_identifier().unwrap().resolve(),
                                            ),
                                            hash_get,
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        provide_definitions.push(define);
                                    }
                                    _ => {
                                        stop!(TypeMismatch => "foo provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
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

                            // Mangle with a prefix if necessary
                            let mut provide = provide.clone();

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
                                    ExprKind::ident("%proto-hash-get%"),
                                    ExprKind::atom("__module-".to_string() + &other_module_prefix),
                                    ExprKind::Quote(Box::new(Quote::new(
                                        provide.clone(),
                                        SyntaxObject::default(TokenType::Quote)
                                    )))
                                ],
                                SyntaxObject::default(TokenType::Define),
                            )));

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
        let mut name_unmangler = NameUnMangler::new(&prefix);

        name_mangler.mangle_vars(&mut exprs);

        // The provide definitions should also be mangled
        name_mangler.mangle_vars(&mut provide_definitions);

        // let mut hash_builder = Vec::new();

        // These are gonna be the pairs
        // hash_builder.push(());

        // Construct the series of provides as well, we'll want these to refer to the correct values
        //
        let mut provides: Vec<_> = self
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
                                provide.1 = expand(l.get(2).unwrap().clone(), global_macro_map)?;

                                continue;

                                // name_unmangler.unmangle_expr(provide);
                            }
                            x if *x == *CONTRACT_OUT => {
                                // Update the item to point to just the name
                                //
                                // *provide = l.get(1).unwrap().clone();
                                // {
                                //     println!("---------");
                                //     println!("Provide expr: {}", l.to_string());
                                // }

                                provide.0 = l.get(1).unwrap().clone();
                                provide.1 = expand(
                                    expr_list![
                                        ExprKind::ident("bind/c"),
                                        l.get(2).unwrap().clone(),
                                        l.get(1).unwrap().clone(),
                                        ExprKind::Quote(Box::new(Quote::new(
                                            l.get(1).unwrap().clone(),
                                            SyntaxObject::default(TokenType::Quote)
                                        ))),
                                    ],
                                    global_macro_map,
                                )?;

                                // println!("-------- {}", provide.to_pretty(60));

                                name_unmangler.unmangle_expr(&mut provide.1);
                                // continue;
                            }
                            _ => {
                                stop!(TypeMismatch => "bar provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
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
            ExprKind::atom("__module-".to_string() + &prefix),
            ExprKind::List(List::new(hash_body)),
            SyntaxObject::default(TokenType::Quote),
        )));

        // println!("------ {}", module_define.to_pretty(60));

        exprs.push(module_define);

        // Construct the overall definition
        // TODO: Perhaps mangle these as well, especially if they have contracts associated with them
        provide_definitions.append(&mut exprs);

        // Try this out?
        // let mut analysis = Analysis::from_exprs(&provide_definitions);
        // let mut semantic = SemanticAnalysis::from_analysis(&mut provide_definitions, analysis);

        // // This is definitely broken still
        // semantic.remove_unused_globals_with_prefix("mangler");
        // .replace_non_shadowed_globals_with_builtins()
        // .remove_unused_globals_with_prefix("mangler");

        // println!("------ {}", provide_definitions.to_pretty(60));

        Ok(ExprKind::Begin(Begin::new(
            provide_definitions,
            SyntaxObject::default(TokenType::Begin),
        )))
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

        if log_enabled!(target: "requires", log::Level::Info) {
            info!(target: "requires", "Module ast node: {}", res.to_string());
        }

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
        })
    }
}

struct ModuleBuilder<'a> {
    name: PathBuf,
    main: bool,
    source_ast: Vec<ExprKind>,
    macro_map: HashMap<InternedString, SteelMacro>,
    // TODO: Change the requires / requires_for_syntax to just be a require enum?

    // requires: Vec<PathBuf>,
    // requires_for_syntax: Vec<PathBuf>,
    require_objects: Vec<RequireObject>,

    // built_ins: Vec<PathBuf>,
    provides: Vec<ExprKind>,
    provides_for_syntax: Vec<ExprKind>,
    compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
    visited: &'a mut HashSet<PathBuf>,
    file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
    sources: &'a mut Sources,
    kernel: &'a mut Option<Kernel>,
    builtin_modules: ModuleContainer,
    global_macro_map: &'a HashMap<InternedString, SteelMacro>,
}

impl<'a> ModuleBuilder<'a> {
    #[allow(clippy::too_many_arguments)]
    #[allow(unused)]
    fn main(
        name: Option<PathBuf>,
        source_ast: Vec<ExprKind>,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a HashMap<InternedString, SteelMacro>,
    ) -> Result<Self> {
        // TODO don't immediately canonicalize the path unless we _know_ its coming from a path
        // change the path to not always be required
        // if its not required we know its not coming in

        let name = if let Some(p) = name {
            std::fs::canonicalize(p)?
        } else {
            std::env::current_dir()?
        };

        Ok(ModuleBuilder {
            name,
            main: true,
            source_ast,
            macro_map: HashMap::new(),
            // requires: Vec::new(),
            require_objects: Vec::new(),
            // requires_for_syntax: Vec::new(),
            // built_ins: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
            global_macro_map,
        })
    }

    fn compile(&mut self) -> Result<Vec<ExprKind>> {
        debug!(target: "requires", "Visiting: {:?}", self.name);

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
        info!(target: "requires", "Requires: {:#?}", self.require_objects);

        info!(target: "requires", "Provides: {:#?}", self.provides);
        info!(target: "requires", "Provides for-syntax: {:?}", self.provides_for_syntax);
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
                    debug!("Getting {:?} from the module cache", module);
                    // println!("Already found in the cache: {:?}", module);
                    // new_exprs.push(m.to_module_ast_node());
                    // No need to do anything
                    continue;
                }

                // TODO this is some bad crap here don't do this
                let input = BUILT_INS
                    .iter()
                    .find(|x| x.0 == module.to_str().unwrap())
                    .unwrap()
                    .1;

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
                )?;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                // debug!("Inside {:?} - append {:?}", self.name, module);
                if log_enabled!(log::Level::Debug) {
                    debug!(
                        "appending with {:?}",
                        module_exprs.iter().map(|x| x.to_string()).join(" SEP ")
                    );
                }

                new_exprs.append(&mut module_exprs);

                // TODO evaluate this

                // let mut ast = std::mem::replace(&mut new_module.source_ast, Vec::new());
                // ast.append(&mut module_exprs);
                // new_module.source_ast = ast;

                // Probably want to evaluate a module even if it has no provides?
                if !new_module.provides.is_empty() {
                    new_exprs.push(new_module.compile_module()?);
                } else {
                    log::debug!(target: "requires", "Found no provides, skipping compilation of module: {:?}", new_module.name);
                }
            }

            // At this point, requires should be fully qualified (absolute) paths

            for module in self
                .require_objects
                .iter()
                .filter(|x| matches!(x.path, PathOrBuiltIn::Path(_)))
                .map(|x| x.path.get_path())
            {
                let last_modified = std::fs::metadata(module.as_ref())?.modified()?;

                // Check if we should compile based on the last time modified
                // If we're unable to get information, we want to compile
                let should_recompile =
                    if let Some(cached_modified) = self.file_metadata.get(module.as_ref()) {
                        &last_modified != cached_modified
                    } else {
                        true
                    };

                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                if !should_recompile {
                    // If we already have compiled this module, get it from the cache
                    if let Some(_m) = self.compiled_modules.get(module.as_ref()) {
                        debug!("Getting {:?} from the module cache", module);
                        // println!("Already found in the cache: {:?}", module);
                        // new_exprs.push(m.to_module_ast_node());
                        // No need to do anything
                        continue;
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
                )?;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                // debug!("Inside {:?} - append {:?}", self.name, module);
                if log_enabled!(log::Level::Debug) {
                    debug!(
                        target: "modules",
                        "appending with {:?}",
                        module_exprs.iter().map(|x| x.to_string()).join(" SEP ")
                    );
                }

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
                } else {
                    log::debug!(target: "requires", "Found no provides, skipping compilation of module: {:?}", new_module.name);
                    log::debug!(target: "requires", "Module already in the cache: {}", new_module.compiled_modules.contains_key(&new_module.name));
                    log::debug!(target: "requires", "Compiled modules: {:?}", new_module.compiled_modules.keys().collect::<Vec<_>>());
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

        info!(
            target: "requires",
            "Into compiled module: provides for syntax: {:?}",
            self.provides_for_syntax
        );

        // Expand first with the macros from *this* module
        ast = ast
            .into_iter()
            .map(|x| {
                expand(x, &self.macro_map).and_then(|x| {
                    expand_kernel(x, self.kernel.as_mut(), self.builtin_modules.clone())
                })
            })
            .collect::<Result<Vec<_>>>()?;

        // Expand provides for any macros that exist within there
        provides = provides
            .into_iter()
            .map(|x| {
                expand(x, &self.macro_map).and_then(|x| {
                    expand_kernel(x, self.kernel.as_mut(), self.builtin_modules.clone())
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let mut mangled_asts = Vec::new();

        // Look for the modules in the requires for syntax
        for require_for_syntax in self
            .require_objects
            .iter()
            .filter(|x| x.for_syntax)
            .map(|x| x.path.get_path())
        {
            let (module, in_scope_macros) = ModuleManager::find_in_scope_macros(
                self.compiled_modules,
                require_for_syntax.as_ref(),
                &mut mangled_asts,
            );

            ast = ast
                .into_iter()
                .map(|x| {
                    // First expand the in scope macros
                    // These are macros
                    let mut expander = Expander::new(&in_scope_macros);
                    let first_round_expanded = expander.expand(x)?;

                    if expander.changed {
                        expand(first_round_expanded, &module.macro_map)
                    } else {
                        Ok(first_round_expanded)
                    }
                })
                .collect::<Result<_>>()?;

            provides = provides
                .into_iter()
                .map(|x| {
                    // First expand the in scope macros
                    // These are macros
                    let mut expander = Expander::new(&in_scope_macros);
                    let first_round_expanded = expander.expand(x)?;

                    if expander.changed {
                        expand(first_round_expanded, &module.macro_map)
                    } else {
                        Ok(first_round_expanded)
                    }
                })
                .collect::<Result<_>>()?;
        }

        // let requires_before = self.require_objects.len();

        // self.collect_requires()?;

        // if self.require_objects.len() > requires_before {
        //     println!("EXPANDED INTO A REQUIRE");
        // }

        // TODO: Check HERE for whether there are more requires than were previously found.
        // If so, we should go back and compile the module again

        // TODO: @Matt - fix this hack
        {
            self.source_ast = ast;
            self.provides = provides;

            self.collect_provides();

            // let requires_before = self.require_objects.len();

            // self.collect_requires()?;

            // if self.require_objects.len() > requires_before {
            //     println!("EXPANDED INTO A REQUIRE");
            // }

            provides = std::mem::take(&mut self.provides);
            ast = std::mem::take(&mut self.source_ast);
        }

        // Put the mangled asts at the top
        // then include the ast there
        mangled_asts.append(&mut ast);

        // Take ast, expand with self modules, then expand with each of the require for-syntaxes
        // Then mangle the require-for-syntax, include the mangled directly in the ast

        // TODO: Come back here - we're going to need to figure out the require objects
        let mut module = CompiledModule::new(
            self.name.clone(),
            provides,
            requires,
            self.provides_for_syntax
                .iter()
                .map(|x| *x.atom_identifier().unwrap())
                .collect(),
            std::mem::take(&mut self.macro_map),
            mangled_asts,
        );

        module.set_emitted(true);

        let mut result =
            module.to_top_level_module(self.compiled_modules, self.global_macro_map)?;

        // let mut analysis = Analysis::from_exprs(&[result]);

        // let mut semantic = SemanticAnalysis::from_analysis(&mut result, analysis);

        // // This is definitely broken still
        // semantic
        //     .remove_unused_globals_with_prefix("mangler");

        log::debug!(target: "requires", "Adding compiled module: {:?}", self.name);

        self.compiled_modules.insert(self.name.clone(), module);

        Ok(result)
    }

    fn extract_macro_defs(&mut self) -> Result<()> {
        let mut non_macros = Vec::new();
        let exprs = std::mem::take(&mut self.source_ast);

        for expr in exprs {
            if let ExprKind::Macro(m) = expr {
                let generated_macro = SteelMacro::parse_from_ast_macro(m)?;
                let name = generated_macro.name();
                self.macro_map.insert(*name, generated_macro);
            } else {
                non_macros.push(expr)
            }
        }
        self.source_ast = non_macros;
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
                                    stop!(ArityMismatch => "provide expects a single identifier in the (for-syntax <ident>)")
                                }

                                // Collect the for syntax expressions
                                // TODO -> remove this clone
                                self.provides_for_syntax.push(l.args[1].clone());
                            }
                            x if x == *CONTRACT_OUT || x == *REQUIRE_IDENT_SPEC => {
                                normal_provides.push(expr);
                            }
                            _ => {
                                normal_provides.push(expr);
                                // stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
                            }
                        }
                    } else {
                        stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                    }
                }
                _ => {
                    stop!(TypeMismatch => "provide expects either a (for-syntax <ident>) or an ident")
                }
            }
        }

        Ok(normal_provides)
    }

    // TODO -> collect (provide (for-syntax ...))
    // I think these will already be collected for the macro, however I think for syntax should be found earlier
    // Otherwise the macro expansion will not be able to understand it
    fn collect_provides(&mut self) -> Result<()> {
        let mut non_provides = Vec::new();
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
                                    stop!(Generic => "provide expects at least one identifier to provide");
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
        Ok(())
    }

    fn parse_require_object(
        &mut self,
        home: &Option<PathBuf>,
        r: &crate::parser::ast::Require,
        atom: &ExprKind,
    ) -> Result<RequireObject> {
        let mut object = RequireObjectBuilder::default();

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
                        ..
                    },
            }) => {
                if require_object.path.is_some() {
                    stop!(Generic => "require object only expects one path!")
                }

                // Try this?
                if let Some(lib) = BUILT_INS.iter().find(|x| x.0 == s.as_str()) {
                    // self.built_ins.push(PathBuf::from(lib.0));

                    require_object.path = Some(PathOrBuiltIn::BuiltIn(lib.0.into()));

                    return Ok(());
                    // continue;
                }

                let mut current = self.name.clone();
                if current.is_file() {
                    current.pop();
                }
                current.push(s);

                // // If the path exists on its own, we can continue
                // // But theres the case where we're searching for a module on the STEEL_HOME
                if !current.exists() {
                    if let Some(mut home) = home.clone() {
                        home.push(s);
                        current = home;

                        log::info!("Searching STEEL_HOME for {:?}", current);
                    } else {
                        stop!(Generic => format!("Module not found: {:?}", self.name))
                    }
                }

                // Get the absolute path and store that
                // self.requires.push(current)

                require_object.path = Some(PathOrBuiltIn::Path(current));
            }

            // TODO: Requires with qualifiers, that aren't just for-syntax
            // Perhaps like:
            // (with-prefix <xyz>)
            ExprKind::List(l) => {
                match l.first_ident() {
                    Some(x) if *x == *ONLY_IN => {
                        if l.args.len() < 2 {
                            stop!(BadSyntax => "only-in expects a require-spec and optionally a list of ids to bind (maybe renamed)");
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
                            stop!(BadSyntax => "prefix-in expects a prefix to prefix a given file or module"; r.location.span; r.location.source.clone());
                        }

                        if let Some(prefix) = l.args[1].atom_identifier() {
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
                            stop!(TypeMismatch => "prefix-in expects an identifier to use for the prefix");
                        }
                    }

                    Some(x) if *x == *FOR_SYNTAX => {
                        // We're expecting something like (for-syntax "foo")
                        if l.args.len() != 2 {
                            stop!(BadSyntax => "for-syntax expects one string literal referring to a file or module"; r.location.span; r.location.source.clone());
                        }

                        if let Some(path) = l.args[1].string_literal() {
                            if let Some(lib) = BUILT_INS.iter().find(|x| x.0 == path) {
                                // self.built_ins.push(PathBuf::from(lib.0));

                                require_object.path = Some(PathOrBuiltIn::BuiltIn(lib.0.into()));
                                require_object.for_syntax = true;

                                return Ok(());
                                // continue;
                            } else {
                                let mut current = self.name.clone();
                                if current.is_file() {
                                    current.pop();
                                }
                                current.push(path);

                                if !current.exists() {
                                    if let Some(mut home) = home.clone() {
                                        home.push(path);
                                        current = home;

                                        log::info!("Searching STEEL_HOME for {:?}", current);
                                    } else {
                                        stop!(Generic => format!("Module not found: {:?}", self.name))
                                    }
                                }

                                require_object.for_syntax = true;
                                require_object.path = Some(PathOrBuiltIn::Path(current));
                            }
                        } else {
                            stop!(BadSyntax => "for-syntax expects a string literal referring to a file or module"; r.location.span; r.location.source.clone());
                        }
                    }
                    _ => {
                        stop!(BadSyntax => "require accepts either a string literal or a for-syntax expression"; r.location.span; r.location.source.clone())
                    }
                }
            }

            _ => {
                stop!(Generic => "require expected a string literal referring to a file/module"; r.location.span; r.location.source.clone())
            }
        }

        Ok(())
    }

    fn collect_requires(&mut self) -> Result<()> {
        // unimplemented!()

        let mut exprs_without_requires = Vec::new();
        let exprs = std::mem::take(&mut self.source_ast);

        let home = std::env::var("STEEL_HOME")
            .map(PathBuf::from)
            .map(|mut x| {
                x.push("cogs");
                x
            })
            .ok();

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
        input: &str,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,
        global_macro_map: &'a HashMap<InternedString, SteelMacro>,
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
        )
        .parse_builtin(input)
    }

    fn new_from_path(
        name: PathBuf,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,

        global_macro_map: &'a HashMap<InternedString, SteelMacro>,
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
        )
        .parse_from_path()
    }

    fn raw(
        name: PathBuf,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
        sources: &'a mut Sources,
        kernel: &'a mut Option<Kernel>,
        builtin_modules: ModuleContainer,

        global_macro_map: &'a HashMap<InternedString, SteelMacro>,
    ) -> Self {
        ModuleBuilder {
            name,
            main: false,
            source_ast: Vec::new(),
            macro_map: HashMap::new(),
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
        }
    }

    fn parse_builtin(mut self, input: &str) -> Result<Self> {
        let parsed = Parser::new_from_source(input, self.name.clone(), None)
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        Ok(self)
    }

    fn parse_from_path(mut self) -> Result<Self> {
        log::info!("Opening: {:?}", self.name);

        let mut file = std::fs::File::open(&self.name).map_err(|err| {
            let mut err = crate::SteelErr::from(err);
            err.prepend_message(&format!("Attempting to load module from: {:?}", self.name));
            err
        })?;
        self.file_metadata
            .insert(self.name.clone(), file.metadata()?.modified()?);

        // TODO: DEFAULT MODULE LOADER PREFIX
        // let mut exprs = String::new();

        let mut exprs = PRELUDE_STRING.to_string();

        // Add the modules here:

        // exprs.push_str(ALL_MODULES);

        file.read_to_string(&mut exprs)?;

        let id = self.sources.add_source(exprs, Some(self.name.clone()));

        {
            // Fetch the exprs after adding them to the sources
            // We did _just_ add it, so its fine to unwrap
            let guard = self.sources.sources.lock().unwrap();

            let exprs = guard.get(id).unwrap();

            let parsed = Parser::new_from_source(&exprs, self.name.clone(), Some(id))
                .collect::<std::result::Result<Vec<_>, ParseError>>()?;

            self.source_ast = parsed;
        }

        Ok(self)
    }
}

// pub static PRELUDE_STRING: &str = "";

pub static PRELUDE_STRING: &str = "(require-builtin steel/base) (require \"#%private/steel/contract\" (for-syntax \"#%private/steel/contract\")) ";
