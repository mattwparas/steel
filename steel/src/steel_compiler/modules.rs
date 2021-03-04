use itertools::Itertools;

use crate::parser::{
    ast::{Atom, ExprKind, List},
    parser::{ParseError, Parser, SyntaxObject},
    tokens::TokenType,
};
use crate::rvals::Result;

use crate::rerrs::{ErrorKind, SteelErr};

use std::{
    collections::{HashMap, HashSet},
    io::Read,
    path::PathBuf,
};

use crate::parser::expander::SteelMacro;
use crate::stop;

use std::time::SystemTime;

// use std::fs::Metadata;

use crate::parser::expand_visitor::{expand, extract_macro_defs};

// pub fn extract_macro_defs(
//     exprs: Vec<ExprKind>,
//     macro_map: &mut HashMap<String, SteelMacro>,
// ) -> Result<Vec<ExprKind>> {

// }

// pub fn expand(expr: ExprKind, map: &HashMap<String, SteelMacro>) -> Result<ExprKind> {
//     Expander { map }.visit(expr)
// }

// #[derive(Clone, PartialEq, Hash)]
// pub struct FilePath {
//     absolute_path: PathBuf,
//     last_modified: SystemTime,
// }

pub struct ModuleManager {
    compiled_modules: HashMap<PathBuf, CompiledModule>,
    file_metadata: HashMap<PathBuf, SystemTime>,
    visited: HashSet<PathBuf>,
}

impl ModuleManager {
    pub fn new(
        compiled_modules: HashMap<PathBuf, CompiledModule>,
        file_metadata: HashMap<PathBuf, SystemTime>,
    ) -> Self {
        ModuleManager {
            compiled_modules,
            file_metadata,
            visited: HashSet::new(),
        }
    }

    pub fn default() -> Self {
        Self::new(HashMap::new(), HashMap::new())
    }

    pub fn compile_main(
        &mut self,
        global_macro_map: &mut HashMap<String, SteelMacro>,
        exprs: Vec<ExprKind>,
        path: PathBuf,
    ) -> Result<Vec<ExprKind>> {
        // Wipe the visited set on entry
        self.visited.clear();

        let non_macro_expressions = extract_macro_defs(exprs, global_macro_map)?;

        let mut module_builder = ModuleBuilder::main(
            path,
            non_macro_expressions,
            &mut self.compiled_modules,
            &mut self.visited,
            &mut self.file_metadata,
        )?;

        let mut module_statements = module_builder.compile()?;

        module_statements.append(&mut module_builder.source_ast);

        module_statements
            .into_iter()
            .map(|x| expand(x, global_macro_map))
            .collect::<Result<_>>()
    }
}

pub struct CompiledModule {
    name: PathBuf,
    provides: Vec<ExprKind>,
    ast: Vec<ExprKind>,
}

impl CompiledModule {
    fn to_module_ast_node(&self) -> ExprKind {
        let mut body = vec![
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                "module".to_string(),
            )))),
            ExprKind::Atom(Atom::new(SyntaxObject::default(TokenType::Identifier(
                "###".to_string() + self.name.to_str().unwrap(),
            )))),
        ];

        // Put any provides at the top
        body.append(&mut self.provides.clone());

        // Put the ast nodes inside the macro
        body.append(&mut self.ast.clone());

        ExprKind::List(List::new(body))
    }
}

pub struct ModuleBuilder<'a> {
    name: PathBuf,
    main: bool,
    source_ast: Vec<ExprKind>,
    macro_map: HashMap<String, SteelMacro>,
    requires: Vec<PathBuf>,
    provides: Vec<ExprKind>,
    compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
    visited: &'a mut HashSet<PathBuf>,
    file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
}

impl<'a> ModuleBuilder<'a> {
    pub fn main(
        name: PathBuf,
        source_ast: Vec<ExprKind>,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
        visited: &'a mut HashSet<PathBuf>,
        file_metadata: &'a mut HashMap<PathBuf, SystemTime>,
    ) -> Result<Self> {
        Ok(ModuleBuilder {
            name: std::fs::canonicalize(&name)?,
            main: true,
            source_ast,
            macro_map: HashMap::new(),
            requires: Vec::new(),
            provides: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
        })
    }

    fn compile(&mut self) -> Result<Vec<ExprKind>> {
        self.collect_requires()?;
        // let contains_provides = self.contains_provides();
        self.collect_provides()?;

        if !self.provides.is_empty() && !self.main {
            return Ok(Vec::new());
        }

        if self.visited.contains(&self.name) {
            stop!(Generic => format!("circular dependency found during module resolution with: {:?}", self.name))
        }

        self.visited.insert(self.name.clone());

        if self.main {
            let exprs = std::mem::replace(&mut self.source_ast, Vec::new());

            // println!("source ast before filtering: {:?}", exprs);
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

            // println!("source_ast: {:?}", self.source_ast);
        }

        self.extract_macro_defs()?;
        let mut new_exprs = Vec::new();

        // println!("Requires in {:?}, {:?}", self.name, self.requires);

        if self.requires.is_empty() && !self.main {
            // We're at a leaf, put into the cache
            // println!("putting {:?} in the cache", self.name);
            new_exprs.push(self.into_compiled_module()?);
        } else {
            // At this point, requires should be fully qualified (absolute) paths
            for module in &self.requires {
                // println!("Inside: {:?}, visiting {:?}", self.name, module);

                let last_modified = std::fs::metadata(module)?.modified()?;

                // Check if we should compile based on the last time modified
                // If we're unable to get information, we want to compile
                let should_recompile = if let Some(cached_modified) = self.file_metadata.get(module)
                {
                    let result = &last_modified == cached_modified;

                    if result {
                        println!("already compiled this file: {:?}, continuing", module);
                    } else {
                        println!("don't need to recompile: {:?}", module);
                    }
                    !result
                } else {
                    true
                };

                // We've established nothing has changed with this file
                // Check to see if its in the cache first
                // Otherwise go ahead and compile
                if !should_recompile {
                    // If we already have compiled this module, get it from the cache
                    if let Some(m) = self.compiled_modules.get(module) {
                        // println!("Already found in the cache: {:?}", module);
                        new_exprs.push(m.to_module_ast_node());
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

                // println!("Inside {:?} - append {:?}", self.name, module);

                // println!(
                //     "appending with {:?}",
                //     module_exprs.iter().map(|x| x.to_string()).join(" ")
                // );

                // TODO evaluate this

                let mut ast = std::mem::replace(&mut new_module.source_ast, Vec::new());
                ast.append(&mut module_exprs);
                new_module.source_ast = ast;

                new_exprs.push(new_module.into_compiled_module()?);
            }
        }

        // println!("compiling: {}", self.name);

        return Ok(new_exprs);
    }

    fn into_compiled_module(&mut self) -> Result<ExprKind> {
        // let expanded = ;

        // TODO use std::mem::swap or something here
        let ast = std::mem::replace(&mut self.source_ast, Vec::new());
        let provides = std::mem::replace(&mut self.provides, Vec::new());
        let module = CompiledModule {
            name: self.name.clone(),
            provides,
            ast: ast
                .into_iter()
                .map(|x| expand(x, &self.macro_map))
                .collect::<Result<Vec<_>>>()?,
        };
        let result = module.to_module_ast_node();
        // println!(
        //     "Into compiled module inserted into the cache: {:?}",
        //     self.name
        // );
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

    fn collect_provides(&mut self) -> Result<()> {
        let mut non_provides = Vec::new();
        let exprs = std::mem::replace(&mut self.source_ast, Vec::new());

        for expr in exprs {
            if let ExprKind::List(l) = &expr {
                if let Some(provide) = l.first_ident() {
                    if provide == "provide" {
                        if l.len() == 1 {
                            stop!(Generic => "provide expects at least one identifier to provide");
                        }

                        self.provides.push(expr)
                    } else {
                        non_provides.push(expr)
                    }
                } else {
                    non_provides.push(expr)
                }
            } else {
                non_provides.push(expr)
            }
        }

        self.source_ast = non_provides;
        Ok(())

        // println!(
        //     "source ast after collecting provides: {:?}",
        //     self.source_ast
        // )
    }

    fn collect_requires(&mut self) -> Result<()> {
        // unimplemented!()

        let mut exprs_without_requires = Vec::new();
        let exprs = std::mem::replace(&mut self.source_ast, Vec::new());

        for expr in exprs {
            if let ExprKind::Require(r) = expr {
                for atom in &r.modules {
                    if let Atom {
                        syn:
                            SyntaxObject {
                                ty: TokenType::StringLiteral(s),
                                ..
                            },
                    } = atom
                    {
                        let mut current = self.name.clone();
                        if current.is_file() {
                            current.pop();
                        }
                        current.push(s);

                        // Get the absolute path and store that
                        // current = std::fs::canonicalize(&current)?;
                        // let new_path = PathBuf::new()
                        self.requires.push(current)
                    } else {
                        stop!(Generic => "require expected a string literal referring to a file/module"; atom.syn.span; atom.syn.source.clone())
                    }
                }
            } else {
                exprs_without_requires.push(expr)
            }
        }

        // println!("Exprs without requires: {:?}", exprs_without_requires);

        self.source_ast = exprs_without_requires;
        Ok(())
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
            provides: Vec::new(),
            compiled_modules,
            visited,
            file_metadata,
        }
    }

    fn parse_from_path(mut self) -> Result<Self> {
        let mut file = std::fs::File::open(&self.name)?;
        self.file_metadata
            .insert(self.name.clone(), file.metadata()?.modified()?);
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;

        let mut intern = HashMap::new();

        let parsed = Parser::new_from_source(&exprs, &mut intern, self.name.to_str().unwrap())
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        Ok(self)
    }
}
