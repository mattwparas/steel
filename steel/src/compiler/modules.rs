use crate::{
    compiler::passes::mangle::mangle_vars_with_prefix,
    parser::{
        ast::{Atom, ExprKind, List},
        kernel::Kernel,
        parser::{ParseError, Parser, SyntaxObject},
        tokens::TokenType,
    },
};
use crate::{parser::expand_visitor::Expander, rvals::Result};

use std::{
    collections::{HashMap, HashSet},
    io::Read,
    path::PathBuf,
};

use crate::parser::expander::SteelMacro;
use crate::stop;

use std::time::SystemTime;

use crate::parser::expand_visitor::{expand, extract_macro_defs};

use itertools::Itertools;
use log::{debug, info, log_enabled};

const OPTION: &str = include_str!("../scheme/modules/option.rkt");
const OPTION_NAME: &str = "std::option";

const RESULT: &str = include_str!("../scheme/modules/result.rkt");
const RESULT_NAME: &str = "std::result";

// const DICT: &str = include_str!("../scheme/modules/test.rkt");
// const TEST_NAME: &str = "std::test";

static BUILT_INS: &[(&'static str, &'static str)] = &[(OPTION_NAME, OPTION), (RESULT_NAME, RESULT)];

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

    pub(crate) fn default() -> Self {
        Self::new(HashMap::new(), HashMap::new())
    }

    pub(crate) fn compile_main(
        &mut self,
        global_macro_map: &mut HashMap<String, SteelMacro>,
        _kernel: &mut Option<Kernel>,
        exprs: Vec<ExprKind>,
        path: Option<PathBuf>,
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
        )?;

        let mut module_statements = module_builder.compile()?;

        // println!("Compiled modules: {:?}", module_builder.compiled_modules);

        // Expand the ast first with the macros from global/source file
        let mut ast = module_builder
            .source_ast
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<Vec<_>>>()?;

        let mut mangled_asts = Vec::new();

        for require_for_syntax in &module_builder.requires_for_syntax {
            let module = self
                .compiled_modules
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
        module_statements.append(&mut mangled_asts);

        // The next two lines here expand _all_ of the source code with the top level macros
        // This is necessary because of the std library macros, although this should be able to be
        // solved with scoped imports of the standard library explicitly
        module_statements.append(&mut ast);

        module_statements
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<_>>()
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
    requires: Vec<ExprKind>,
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
        requires: Vec<ExprKind>,
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

    // Turn the module into the AST node that represents the macro module in the stdlib
    fn to_module_ast_node(&self) -> ExprKind {
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
        body.append(&mut self.requires.clone());

        // TODO: @Matt 10/8/22
        // Reconsider how to address this expansion.
        // We really don't want to pollute the module space - perhaps disallow shadowed built-ins so we don't need this?
        // That would probably be annoying
        let steel_base = ExprKind::List(List::new(vec![ExprKind::atom("steel/base".to_string())]));

        body.push(steel_base);

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
}

impl<'a> ModuleBuilder<'a> {
    fn main(
        name: Option<PathBuf>,
        source_ast: Vec<ExprKind>,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
    ) -> Result<Self> {
        // TODO don't immediately canonicalize the path unless we _know_ its coming from a path
        // change the path to not always be required
        // if its not required we know its not coming in

        let name = if let Some(p) = name {
            std::fs::canonicalize(&p)?
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
            let exprs = std::mem::replace(&mut self.source_ast, Vec::new());

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
        if self.requires.is_empty() && !self.main {
            // We're at a leaf, put into the cache
            // println!("putting {:?} in the cache", self.name);
            if !self.provides.is_empty() {
                new_exprs.push(self.into_compiled_module()?);
            }
        } else {
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
                    if let Some(m) = self.compiled_modules.get(module) {
                        debug!("Getting {:?} from the module cache", module);
                        // println!("Already found in the cache: {:?}", module);
                        // new_exprs.push(m.to_module_ast_node());
                        // No need to do anything
                        continue;
                    }
                }

                let mut new_module = ModuleBuilder::new_from_path(
                    module.clone(),
                    &mut self.compiled_modules,
                    &mut self.visited,
                    &mut self.file_metadata,
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
                    new_exprs.push(new_module.into_compiled_module()?);
                }
            }

            // TODO come back for parsing built ins
            for module in &self.built_ins {
                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                // If we already have compiled this module, get it from the cache
                if let Some(m) = self.compiled_modules.get(module) {
                    debug!("Getting {:?} from the module cache", module);
                    // println!("Already found in the cache: {:?}", module);
                    new_exprs.push(m.to_module_ast_node());
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
                    &mut self.compiled_modules,
                    &mut self.visited,
                    &mut self.file_metadata,
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
                    new_exprs.push(new_module.into_compiled_module()?);
                }
            }
        }

        // println!("compiling: {}", self.name);

        // println!(
        //     "Exiting with {:?}",
        //     new_exprs.iter().map(|x| x.to_string()).collect::<Vec<_>>()
        // );

        return Ok(new_exprs);
    }

    fn into_compiled_module(&mut self) -> Result<ExprKind> {
        // let expanded = ;

        // TODO use std::mem::swap or something here
        let mut ast = std::mem::replace(&mut self.source_ast, Vec::new());
        let provides = std::mem::replace(&mut self.provides, Vec::new());

        // let mut ast = self.source_ast.clone();
        // let provides = self.provides.clone();

        let mut requires = Vec::new();

        // TODO -> qualified requires as well
        // qualified requires should be able to adjust the names of the exported functions

        for require in &self.requires {
            // @Matt 10/8/22
            // Here, instead of building out the entire AST node, just insert a reference to the module at the top level
            // Something like: (require <module_name>)

            // let m = self
            //     .compiled_modules
            //     .get(require)
            //     .unwrap()
            //     .to_module_ast_node();
            // requires.push(m);
        }

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
            .map(|x| expand(x, &self.macro_map))
            .collect::<Result<Vec<_>>>()?;

        let mut mangled_asts = Vec::new();

        // Look for the modules in the requires for syntax
        for require_for_syntax in &self.requires_for_syntax {
            // TODO -> better error handling here
            let module = self
                .compiled_modules
                .get(require_for_syntax)
                .expect("Module missing!");

            let mut module_ast = module.ast.clone();

            // TODO
            mangle_vars_with_prefix(
                "##mangler##".to_string() + module.name.to_str().unwrap(),
                &mut module_ast,
            );

            mangled_asts.append(&mut module_ast);

            // TODO -> actually expand the ast with macros from the other while respecting visibility rules
            // Do a first pass expansion with the public macros, then expand witih the mangled macros
            ast = ast
                .into_iter()
                .map(|x| expand(x, &module.macro_map))
                .collect::<Result<_>>()?;
        }

        // Put the mangled asts at the top
        // then include the ast there
        mangled_asts.append(&mut ast);

        // Take ast, expand with self modules, then expand with each of the require for-syntaxes
        // Then mangle the require-for-syntax, include the mangled directly in the ast

        // let module = CompiledModule {
        //     name: self.name.clone(),
        //     provides,
        //     requires,
        //     ast: mangled_asts,
        //     provides_for_syntax: self
        //         .provides_for_syntax
        //         .iter()
        //         .map(|x| x.atom_identifier().unwrap().to_string())
        //         .collect(),
        //     // ast
        //     //     .into_iter()
        //     //     .map(|x| expand(x, &self.macro_map))
        //     //     .collect::<Result<Vec<_>>>()?,
        //     macro_map: std::mem::take(&mut self.macro_map),
        // };

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

        let result = module.to_module_ast_node();
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
        let exprs = std::mem::replace(&mut self.source_ast, Vec::new());
        // self.source_ast = Vec::new();
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
        let exprs = std::mem::replace(&mut self.source_ast, Vec::new());

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
        let exprs = std::mem::replace(&mut self.source_ast, Vec::new());

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

    fn new_built_in(
        name: PathBuf,
        input: &str,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
    ) -> Result<Self> {
        ModuleBuilder::raw(name, compiled_modules, visited, file_metadata).parse_builtin(input)
    }

    fn new_from_path(
        name: PathBuf,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
    ) -> Result<Self> {
        ModuleBuilder::raw(name, compiled_modules, visited, file_metadata).parse_from_path()
    }

    fn raw(
        name: PathBuf,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
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
        }
    }

    fn parse_builtin(mut self, input: &str) -> Result<Self> {
        let mut intern = HashMap::new();

        let parsed = Parser::new_from_source(input, &mut intern, self.name.clone())
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

        let mut intern = HashMap::new();

        let parsed = Parser::new_from_source(&exprs, &mut intern, self.name.clone())
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        Ok(self)
    }
}
