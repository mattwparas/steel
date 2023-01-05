use crate::{
    compiler::passes::mangle::mangle_vars_with_prefix,
    expr_list,
    parser::{
        ast::{Atom, Begin, Define, ExprKind, List, Quote},
        expand_visitor::expand_kernel,
        kernel::Kernel,
        parser::{ParseError, Parser, Sources, SyntaxObject},
        tokens::TokenType,
    },
    steel_vm::builtin::BuiltInModule,
};
use crate::{parser::expand_visitor::Expander, rvals::Result};

use std::{
    collections::{HashMap, HashSet},
    io::Read,
    path::PathBuf,
    rc::Rc,
};

use crate::parser::expander::SteelMacro;
use crate::stop;

use std::time::SystemTime;

use crate::parser::expand_visitor::{expand, extract_macro_defs};

use itertools::Itertools;
use log::{debug, info, log_enabled};

use super::passes::mangle::{collect_globals, NameMangler};

use im_rc::HashMap as ImmutableHashMap;

const OPTION: &str = include_str!("../scheme/modules/option.scm");
const OPTION_NAME: &str = "steel/option";

const RESULT: &str = include_str!("../scheme/modules/result.scm");
const RESULT_NAME: &str = "steel/result";

// const DICT: &str = include_str!("../scheme/modules/test.rkt");
// const TEST_NAME: &str = "std::test";

static BUILT_INS: &[(&str, &str)] = &[(OPTION_NAME, OPTION), (RESULT_NAME, RESULT)];

/// Manages the modules
/// keeps some visited state on the manager for traversal
/// Also keeps track of the metadata for each file in order to determine
/// if it needs to be recompiled
pub(crate) struct ModuleManager {
    compiled_modules: HashMap<PathBuf, CompiledModule>,
    file_metadata: HashMap<PathBuf, SystemTime>,
    visited: HashSet<PathBuf>,
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
        }
    }

    pub fn modules(&self) -> &HashMap<PathBuf, CompiledModule> {
        &self.compiled_modules
    }

    pub(crate) fn default() -> Self {
        Self::new(HashMap::new(), HashMap::new())
    }

    // Add the module directly to the compiled module cache
    pub(crate) fn add_module(
        &mut self,
        path: PathBuf,
        _global_macro_map: &mut HashMap<String, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
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
        )?;

        module_builder.compile()?;

        println!("{:#?}", self.compiled_modules);

        Ok(())
    }

    #[allow(unused)]
    pub(crate) fn compile_main(
        &mut self,
        global_macro_map: &mut HashMap<String, SteelMacro>,
        kernel: &mut Option<Kernel>,
        sources: &mut Sources,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
        builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
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
        )?;

        let mut module_statements = module_builder.compile()?;

        // println!("Compiled modules: {:?}", module_builder.compiled_modules);

        // Expand the ast first with the macros from global/source file
        let mut ast = module_builder
            .source_ast
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<Vec<_>>>()?;

        let mut require_defines = Vec::new();

        for path in module_builder
            .requires
            .iter()
            .chain(module_builder.built_ins.iter())
        {
            // println!("{:?}", path);
            let module = module_builder.compiled_modules.get(path).unwrap();

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    // println!("{}", provide);

                    // println!("Top level provide handler");

                    // Would be nice if this could be handled by some macro expansion...
                    // See if contract/out

                    let other_module_prefix = "mangler".to_string() + module.name.to_str().unwrap();

                    // TODO: Expand the contract out into something we expect
                    // Otherwise, this is going to blow up
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                match qualifier {
                                    "contract/out" => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let name = l.args.get(1).unwrap();
                                        let contract = l.args.get(2).unwrap();

                                        let hash_get = expr_list![
                                            ExprKind::ident("hash-get"),
                                            ExprKind::atom(
                                                "__module-".to_string() + &other_module_prefix
                                            ),
                                            ExprKind::Quote(Box::new(Quote::new(
                                                name.clone(),
                                                SyntaxObject::default(TokenType::Quote)
                                            ))),
                                        ];

                                        let define = ExprKind::Define(Box::new(Define::new(
                                            name.clone(),
                                            expr_list![
                                                ExprKind::ident("bind/c"),
                                                contract.clone(),
                                                hash_get,
                                                ExprKind::Quote(Box::new(Quote::new(
                                                    name.clone(),
                                                    SyntaxObject::default(TokenType::Quote)
                                                ))),
                                            ],
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        require_defines.push(define);
                                    }
                                    _ => {
                                        stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
                                    }
                                }
                            } else {
                                stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                            }
                        }
                        ExprKind::Atom(_) => {
                            let hash_get = expr_list![
                                ExprKind::ident("hash-get"),
                                ExprKind::atom("__module-".to_string() + &other_module_prefix),
                                ExprKind::Quote(Box::new(Quote::new(
                                    provide.clone(),
                                    SyntaxObject::default(TokenType::Quote)
                                ))),
                            ];

                            let define = ExprKind::Define(Box::new(Define::new(
                                provide.clone(),
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
        for require_for_syntax in &module_builder.requires_for_syntax {
            let (module, in_scope_macros) = Self::find_in_scope_macros(
                &self.compiled_modules,
                require_for_syntax,
                &mut mangled_asts,
            );

            // ast = ast.into_iter().map(|x| )

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
        }

        // Include the mangled asts in the resulting asts returned
        // module_statements.append(&mut mangled_asts);

        // Include the defines from the modules now imported
        module_statements.append(&mut require_defines);

        // The next two lines here expand _all_ of the source code with the top level macros
        // This is necessary because of the std library macros, although this should be able to be
        // solved with scoped imports of the standard library explicitly
        module_statements.append(&mut ast);

        module_statements
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<_>>()
    }

    fn find_in_scope_macros<'a>(
        compiled_modules: &'a HashMap<PathBuf, CompiledModule>,
        require_for_syntax: &'a PathBuf,
        mangled_asts: &'a mut Vec<ExprKind>,
    ) -> (&'a CompiledModule, HashMap<String, SteelMacro>) {
        let module = compiled_modules
            .get(require_for_syntax)
            .expect("Module missing!");
        let mut module_ast = module.ast.clone();
        mangle_vars_with_prefix(
            "##mangler##".to_string() + module.name.to_str().unwrap(),
            &mut module_ast,
        );
        mangled_asts.append(&mut module_ast);
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
            .filter_map(|x| module.macro_map.get(x).map(|m| (x.to_string(), m.clone()))) // TODO -> fix this unwrap
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
        global_macro_map: &mut HashMap<String, SteelMacro>,
        exprs: Vec<ExprKind>,
    ) -> Result<Vec<ExprKind>> {
        let non_macro_expressions = extract_macro_defs(exprs, global_macro_map)?;
        non_macro_expressions
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect()
    }
}

#[derive(Debug)]
pub struct CompiledModule {
    name: PathBuf,
    provides: Vec<ExprKind>,
    // TODO: Change this to be an ID instead of a string directly
    requires: Vec<PathBuf>,
    provides_for_syntax: Vec<String>,
    macro_map: HashMap<String, SteelMacro>,
    ast: Vec<ExprKind>,
}

// TODO -> cache the construction of the module ast node, then we don't need to reconstruct it every time
// also, just push this down to bytecode immediately -> including a module now is a simple as loading the bytecode for
// the module first, then compiling the instructions for the
impl CompiledModule {
    pub fn new(
        name: PathBuf,
        provides: Vec<ExprKind>,
        requires: Vec<PathBuf>,
        provides_for_syntax: Vec<String>,
        macro_map: HashMap<String, SteelMacro>,
        ast: Vec<ExprKind>,
    ) -> Self {
        Self {
            name,
            provides,
            requires,
            provides_for_syntax,
            macro_map,
            ast,
        }
    }

    fn to_top_level_module(&self, modules: &HashMap<PathBuf, CompiledModule>) -> Result<ExprKind> {
        let mut globals = collect_globals(&self.ast);

        let mut exprs = self.ast.clone();
        let mut provide_definitions = Vec::new();

        let prefix = "mangler".to_string() + self.name.to_str().unwrap();

        // Now we should be able to set up a series of requires with the right style
        // ;; Refresh the module definition in this namespace
        // (define a-module.rkt-b (hash-get 'b b-module.rkt-b))

        // TODO: This is the same as the top level, they should be merged
        for path in &self.requires {
            // println!("{:?}", path);
            // println!("{:?}", modules.keys().collect::<Vec<_>>());
            let module = modules.get(path).unwrap();

            let other_module_prefix = "mangler".to_string() + module.name.to_str().unwrap();

            for provide_expr in &module.provides {
                // For whatever reason, the value coming into module.provides is an expression like: (provide expr...)
                for provide in &provide_expr.list().unwrap().args[1..] {
                    match provide {
                        ExprKind::List(l) => {
                            if let Some(qualifier) = l.first_ident() {
                                match qualifier {
                                    "contract/out" => {
                                        // Directly expand into define/contract, but with the value just being the hash get below

                                        // (bind/c contract name 'name)

                                        let name = l.args.get(1).unwrap();
                                        let contract = l.args.get(2).unwrap();

                                        // Since this is now bound to be in the scope of the current working module, we also want
                                        // this to be mangled. In the event we do something like, qualify the import, then we might
                                        // have to mangle this differently
                                        globals.insert(name.atom_identifier().unwrap().to_string());

                                        let hash_get = expr_list![
                                            ExprKind::ident("hash-get"),
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
                                                prefix.clone() + name.atom_identifier().unwrap(),
                                            ),
                                            expr_list![
                                                ExprKind::ident("bind/c"),
                                                contract.clone(),
                                                hash_get,
                                                ExprKind::Quote(Box::new(Quote::new(
                                                    name.clone(),
                                                    SyntaxObject::default(TokenType::Quote)
                                                ))),
                                            ],
                                            SyntaxObject::default(TokenType::Define),
                                        )));

                                        provide_definitions.push(define);
                                    }
                                    _ => {
                                        stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
                                    }
                                }
                            } else {
                                stop!(TypeMismatch => "provide expects either an identifier or a (for-syntax <ident>)")
                            }
                        }
                        ExprKind::Atom(_) => {
                            let provide_ident = provide.atom_identifier().unwrap();

                            // Since this is now bound to be in the scope of the current working module, we also want
                            // this to be mangled. In the event we do something like, qualify the import, then we might
                            // have to mangle this differently
                            globals.insert(provide_ident.to_string());

                            let define = ExprKind::Define(Box::new(Define::new(
                                ExprKind::atom(prefix.clone() + provide_ident),
                                expr_list![
                                    ExprKind::ident("hash-get"),
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

                // exprs.push(define);

                // todo!()
            }
        }

        // Mangle all of the variables that are either:
        // 1. Defined locally in this file
        // 2. Required by another file
        let mut name_mangler = NameMangler::new(globals, prefix.clone());
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
            .collect();

        for provide in &mut provides {
            match provide {
                ExprKind::List(l) => {
                    if let Some(qualifier) = l.first_ident() {
                        match qualifier {
                            "contract/out" => {
                                // Update the item to point to just the name
                                *provide = l.get(1).unwrap().clone();
                            }
                            _ => {
                                stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
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

        name_mangler.mangle_vars(&mut provides);

        let mut hash_body = vec![ExprKind::ident("hash")];

        hash_body.extend(
            un_mangled
                .into_iter()
                .map(|x| {
                    ExprKind::Quote(Box::new(Quote::new(
                        x,
                        SyntaxObject::default(TokenType::Quote),
                    )))
                })
                .interleave(provides),
        );

        let module_define = ExprKind::Define(Box::new(Define::new(
            ExprKind::atom("__module-".to_string() + &prefix),
            ExprKind::List(List::new(hash_body)),
            SyntaxObject::default(TokenType::Quote),
        )));

        exprs.push(module_define);

        // Construct the overall definition
        // TODO: Perhaps mangle these as well, especially if they have contracts associated with them
        provide_definitions.append(&mut exprs);

        Ok(ExprKind::Begin(Begin::new(
            provide_definitions,
            SyntaxObject::default(TokenType::Begin),
        )))
    }

    // Turn the module into the AST node that represents the macro module in the stdlib
    fn _to_module_ast_node(&self) -> ExprKind {
        let mut body = vec![
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                "module".to_string(),
            )))),
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                "___".to_string() + self.name.to_str().unwrap(),
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

struct ModuleBuilder<'a> {
    name: PathBuf,
    main: bool,
    source_ast: Vec<ExprKind>,
    macro_map: HashMap<String, SteelMacro>,
    requires: Vec<PathBuf>,
    requires_for_syntax: Vec<PathBuf>,
    built_ins: Vec<PathBuf>,
    provides: Vec<ExprKind>,
    provides_for_syntax: Vec<ExprKind>,
    compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
    visited: &'a mut HashSet<PathBuf>,
    file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
    sources: &'a mut Sources,
    kernel: &'a mut Option<Kernel>,
    builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
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
        builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
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
            requires: Vec::new(),
            requires_for_syntax: Vec::new(),
            built_ins: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
        })
    }

    fn compile(&mut self) -> Result<Vec<ExprKind>> {
        debug!(target: "requires", "Visiting: {:?}", self.name);

        self.collect_requires()?;
        // let contains_provides = self.contains_provides();
        self.collect_provides()?;

        if log_enabled!(log::Level::Info) {
            info!("Requires: {:#?}", self.requires);
            info!("Requires for-syntax: {:?}", self.requires_for_syntax);

            info!("Provides: {:#?}", self.provides);
            info!("Provides for-syntax: {:?}", self.provides_for_syntax);
        }

        if self.provides.is_empty() && !self.main {
            self.visited.insert(self.name.clone());
            // println!("getting inside here!");
            return Ok(Vec::new());
        }

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
                            return provide != "provide";
                        }
                    }
                    true
                })
                .collect();
        }

        self.extract_macro_defs()?;
        let mut new_exprs = Vec::new();

        // TODO include built ins here
        if self.requires.is_empty() && self.built_ins.is_empty() && !self.main {
            // We're at a leaf, put into the cache
            // println!("putting {:?} in the cache", self.name);
            if !self.provides.is_empty() {
                new_exprs.push(self.compile_module()?);
            }
        } else {
            // TODO come back for parsing built ins
            for module in &self.built_ins {
                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                // If we already have compiled this module, get it from the cache
                if let Some(_m) = self.compiled_modules.get(module) {
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
                    module.clone(),
                    input,
                    self.compiled_modules,
                    self.visited,
                    self.file_metadata,
                    self.sources,
                    self.kernel,
                    self.builtin_modules.clone(),
                )?;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                debug!("Inside {:?} - append {:?}", self.name, module);
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

                if !new_module.provides.is_empty() {
                    new_exprs.push(new_module.compile_module()?);
                }
            }

            // At this point, requires should be fully qualified (absolute) paths
            for module in &self.requires {
                let last_modified = std::fs::metadata(module)?.modified()?;

                // Check if we should compile based on the last time modified
                // If we're unable to get information, we want to compile
                let should_recompile = if let Some(cached_modified) = self.file_metadata.get(module)
                {
                    &last_modified != cached_modified
                } else {
                    true
                };

                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                if !should_recompile {
                    // If we already have compiled this module, get it from the cache
                    if let Some(_m) = self.compiled_modules.get(module) {
                        debug!("Getting {:?} from the module cache", module);
                        // println!("Already found in the cache: {:?}", module);
                        // new_exprs.push(m.to_module_ast_node());
                        // No need to do anything
                        continue;
                    }
                }

                let mut new_module = ModuleBuilder::new_from_path(
                    module.clone(),
                    self.compiled_modules,
                    self.visited,
                    self.file_metadata,
                    self.sources,
                    self.kernel,
                    self.builtin_modules.clone(),
                )?;

                // Walk the tree and compile any dependencies
                // This will eventually put the module in the cache
                let mut module_exprs = new_module.compile()?;

                debug!("Inside {:?} - append {:?}", self.name, module);
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

                if !new_module.provides.is_empty() {
                    new_exprs.push(new_module.compile_module()?);
                }
            }
        }

        // Define the actual

        // println!("compiling: {}", self.name);

        // println!(
        //     "Exiting with {:?}",
        //     new_exprs.iter().map(|x| x.to_string()).collect::<Vec<_>>()
        // );

        Ok(new_exprs)
    }

    fn compile_module(&mut self) -> Result<ExprKind> {
        let mut ast = std::mem::take(&mut self.source_ast);
        let provides = std::mem::take(&mut self.provides);

        // let mut ast = self.source_ast.clone();
        // let provides = self.provides.clone();

        // Clone the requires... I suppose
        let mut requires = self.requires.clone();

        // println!("built ins: {:?}", self.built_ins);

        requires.append(&mut self.built_ins.clone());

        // TODO -> qualified requires as well
        // qualified requires should be able to adjust the names of the exported functions

        // for require in &self.requires {
        // @Matt 10/8/22
        // Here, instead of building out the entire AST node, just insert a reference to the module at the top level
        // Something like: (require <module_name>)

        // let m = self
        //     .compiled_modules
        //     .get(require)
        //     .unwrap()
        //     .to_module_ast_node();
        // requires.push(m);
        // }

        info!(
            "Into compiled module: provides for syntax: {:?}",
            self.provides_for_syntax
        );
        info!(
            "Into compiled module: requires for syntax: {:?}",
            self.requires_for_syntax
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

        let mut mangled_asts = Vec::new();

        // Look for the modules in the requires for syntax
        for require_for_syntax in &self.requires_for_syntax {
            let (module, in_scope_macros) = ModuleManager::find_in_scope_macros(
                self.compiled_modules,
                require_for_syntax,
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

                    // expand(x, &module.macro_map)
                })
                .collect::<Result<_>>()?;
        }

        // Put the mangled asts at the top
        // then include the ast there
        mangled_asts.append(&mut ast);

        // Take ast, expand with self modules, then expand with each of the require for-syntaxes
        // Then mangle the require-for-syntax, include the mangled directly in the ast

        let module = CompiledModule::new(
            self.name.clone(),
            provides,
            requires,
            self.provides_for_syntax
                .iter()
                .map(|x| x.atom_identifier().unwrap().to_string())
                .collect(),
            std::mem::take(&mut self.macro_map),
            mangled_asts,
        );

        // let result = module.to_module_ast_node();

        let result = module.to_top_level_module(self.compiled_modules)?;

        // println!(
        //     "Into compiled module inserted into the cache: {:?}",
        //     self.name
        // );

        // debug!("Adding {:?} to the module cache", self.name);

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
                self.macro_map.insert(name.to_string(), generated_macro);
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
                        match for_syntax {
                            "for-syntax" => {
                                if l.args.len() != 2 {
                                    stop!(ArityMismatch => "provide expects a single identifier in the (for-syntax <ident>)")
                                }

                                // Collect the for syntax expressions
                                // TODO -> remove this clone
                                self.provides_for_syntax.push(l.args[1].clone());
                            }
                            "contract/out" => {
                                normal_provides.push(expr);
                            }
                            _ => {
                                stop!(TypeMismatch => "provide expects either an identifier, (for-syntax <ident>), or (contract/out ...)")
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

        for mut expr in exprs {
            if let ExprKind::List(l) = &mut expr {
                if let Some(provide) = l.first_ident() {
                    if provide == "provide" {
                        if l.len() == 1 {
                            stop!(Generic => "provide expects at least one identifier to provide");
                        }

                        // Swap out the value inside the list
                        let args = std::mem::take(&mut l.args);

                        let filtered = self.filter_out_for_syntax_provides(args)?;

                        l.args = filtered;

                        self.provides.push(expr);

                        continue;
                    }
                }
            }

            non_provides.push(expr)
        }

        self.source_ast = non_provides;
        Ok(())
    }

    fn collect_requires(&mut self) -> Result<()> {
        // unimplemented!()

        let mut exprs_without_requires = Vec::new();
        let exprs = std::mem::take(&mut self.source_ast);

        for expr in exprs {
            match &expr {
                // Include require/for-syntax here
                // This way we have some understanding of what dependencies a file has
                ExprKind::Require(r) => {
                    for atom in &r.modules {
                        match atom {
                            ExprKind::Atom(Atom {
                                syn:
                                    SyntaxObject {
                                        ty: TokenType::StringLiteral(s),
                                        ..
                                    },
                            }) => {
                                // Try this?
                                if let Some(lib) = BUILT_INS.iter().find(|x| x.0 == s.as_str()) {
                                    self.built_ins.push(PathBuf::from(lib.0));
                                    continue;
                                }

                                let mut current = self.name.clone();
                                if current.is_file() {
                                    current.pop();
                                }
                                current.push(s);

                                // Get the absolute path and store that
                                // current = std::fs::canonicalize(&current)?;
                                // let new_path = PathBuf::new()
                                self.requires.push(current)
                            }

                            ExprKind::List(l) => {
                                match l.first_ident() {
                                    Some("for-syntax") => {
                                        // We're expecting something like (for-syntax "foo")
                                        if l.args.len() != 2 {
                                            stop!(BadSyntax => "for-syntax expects one string literal referring to a file or module"; r.location.span; r.location.source.clone());
                                        }

                                        if let Some(path) = l.args[1].string_literal() {
                                            let mut current = self.name.clone();
                                            if current.is_file() {
                                                current.pop();
                                            }
                                            current.push(path);

                                            self.requires_for_syntax.push(current);
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
                    }
                }
                _ => exprs_without_requires.push(expr),
            }
        }

        // println!("Exprs without requires: {:?}", exprs_without_requires);

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
        builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
    ) -> Result<Self> {
        ModuleBuilder::raw(
            name,
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
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
        builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
    ) -> Result<Self> {
        ModuleBuilder::raw(
            name,
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
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
        builtin_modules: ImmutableHashMap<Rc<str>, BuiltInModule>,
    ) -> Self {
        ModuleBuilder {
            name,
            main: false,
            source_ast: Vec::new(),
            macro_map: HashMap::new(),
            requires: Vec::new(),
            requires_for_syntax: Vec::new(),
            built_ins: Vec::new(),
            provides: Vec::new(),
            provides_for_syntax: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
            sources,
            kernel,
            builtin_modules,
        }
    }

    fn parse_builtin(mut self, input: &str) -> Result<Self> {
        let mut intern = HashMap::new();

        let parsed = Parser::new_from_source(input, &mut intern, self.name.clone(), None)
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        Ok(self)
    }

    fn parse_from_path(mut self) -> Result<Self> {
        println!("Opening: {:?}", self.name);
        let mut file = std::fs::File::open(&self.name)?;
        self.file_metadata
            .insert(self.name.clone(), file.metadata()?.modified()?);
        let mut exprs = String::new();

        // Add the modules here:

        // exprs.push_str(ALL_MODULES);

        file.read_to_string(&mut exprs)?;

        let id = self.sources.add_source(exprs, Some(self.name.clone()));

        // Fetch the exprs after adding them to the sources
        // We did _just_ add it, so its fine to unwrap
        let exprs = self.sources.get(id).unwrap();

        let mut intern = HashMap::new();

        let parsed = Parser::new_from_source(exprs, &mut intern, self.name.clone(), Some(id))
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        Ok(self)
    }
}
