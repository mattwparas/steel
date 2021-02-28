use crate::parser::{
    ast::{Atom, ExprKind, List},
    parser::{ParseError, Parser, SyntaxObject},
    tokens::TokenType,
};
use crate::rvals::Result;

use crate::rerrs::{ErrorKind, SteelErr};

use std::{collections::HashMap, io::Read, path::PathBuf};

use crate::parser::expander::SteelMacro;
use crate::stop;

use crate::parser::expand_visitor::{expand, extract_macro_defs};

// pub fn extract_macro_defs(
//     exprs: Vec<ExprKind>,
//     macro_map: &mut HashMap<String, SteelMacro>,
// ) -> Result<Vec<ExprKind>> {

// }

// pub fn expand(expr: ExprKind, map: &HashMap<String, SteelMacro>) -> Result<ExprKind> {
//     Expander { map }.visit(expr)
// }

pub struct ModuleManager<'a> {
    global_macro_map: &'a mut HashMap<String, SteelMacro>,
    compiled_modules: HashMap<PathBuf, CompiledModule>,
}

impl<'a> ModuleManager<'a> {
    pub fn new(global_macro_map: &'a mut HashMap<String, SteelMacro>) -> Self {
        ModuleManager {
            global_macro_map,
            compiled_modules: HashMap::new(),
        }
    }

    pub fn compile_main(&mut self, exprs: Vec<ExprKind>, path: PathBuf) -> Result<Vec<ExprKind>> {
        let non_macro_expressions = extract_macro_defs(exprs, &mut self.global_macro_map)?;

        // let expanded: Vec<_> = non_macro_expressions
        //     .into_iter()
        //     .map(|x| expand(x, &self.global_macro_map))
        //     .collect::<Result<_>>()?;

        let mut module_builder =
            ModuleBuilder::main(path, non_macro_expressions, &mut self.compiled_modules);

        let mut module_statements = module_builder.compile()?;

        module_statements.append(&mut module_builder.source_ast);

        module_statements
            .into_iter()
            .map(|x| expand(x, &self.global_macro_map))
            .collect::<Result<_>>()
    }
}

pub struct CompiledModule {
    name: PathBuf,
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
    compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
}

impl<'a> ModuleBuilder<'a> {
    pub fn main(
        name: PathBuf,
        source_ast: Vec<ExprKind>,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
    ) -> Self {
        ModuleBuilder {
            name,
            main: true,
            source_ast,
            macro_map: HashMap::new(),
            requires: Vec::new(),
            compiled_modules,
        }
    }

    fn compile(&mut self) -> Result<Vec<ExprKind>> {
        self.collect_requires()?;

        if !self.contains_provides() && !self.main {
            return Ok(Vec::new());
        }

        self.extract_macro_defs()?;
        let mut new_exprs = Vec::new();

        if self.requires.is_empty() && !self.main {
            // We're at a leaf, put into the cache
            new_exprs.push(self.into_compiled_module()?);
        } else {
            for module in &self.requires {
                // If we already have compiled this module, get it from the cache
                if let Some(m) = self.compiled_modules.get(module) {
                    new_exprs.push(m.to_module_ast_node());
                } else {
                    let mut new_module =
                        ModuleBuilder::new_from_path(module.clone(), &mut self.compiled_modules)?;

                    // Walk the tree and compile any dependencies
                    // This will eventually put the module in the cache
                    let mut module_exprs = new_module.compile()?;
                    new_exprs.append(&mut module_exprs);
                }
            }
        }

        // println!("compiling: {}", self.name);

        return Ok(new_exprs);
    }

    fn contains_provides(&self) -> bool {
        for expr in &self.source_ast {
            if let ExprKind::List(l) = expr {
                if let Some(provide) = l.first_ident() {
                    return provide == "provide" && l.len() > 1;
                }
            }
        }

        false
    }

    fn into_compiled_module(&mut self) -> Result<ExprKind> {
        // let expanded = ;

        // TODO use std::mem::swap or something here
        let ast = std::mem::replace(&mut self.source_ast, Vec::new());
        let module = CompiledModule {
            name: self.name.clone(),
            ast: ast
                .into_iter()
                .map(|x| expand(x, &self.macro_map))
                .collect::<Result<Vec<_>>>()?,
        };
        let result = module.to_module_ast_node();
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
                        current.pop();
                        current.push(s);

                        println!("{:?}", current);

                        // let new_path = PathBuf::new()
                        self.requires.push(current)
                    } else {
                        stop!(Generic => "require expected a string literally referring to a file/module"; atom.syn.span; atom.syn.source.clone())
                    }
                }
            } else {
                exprs_without_requires.push(expr)
            }
        }

        self.source_ast = exprs_without_requires;
        Ok(())
    }

    fn new_from_path(
        name: PathBuf,
        compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>,
    ) -> Result<Self> {
        ModuleBuilder::raw(name, compiled_modules).parse_from_path()
    }

    fn raw(name: PathBuf, compiled_modules: &'a mut HashMap<PathBuf, CompiledModule>) -> Self {
        ModuleBuilder {
            name,
            main: false,
            source_ast: Vec::new(),
            macro_map: HashMap::new(),
            requires: Vec::new(),
            compiled_modules,
        }
    }

    fn parse_from_path(mut self) -> Result<Self> {
        let mut file = std::fs::File::open(&self.name)?;
        let mut exprs = String::new();
        file.read_to_string(&mut exprs)?;

        let mut intern = HashMap::new();

        let parsed = Parser::new_from_source(&exprs, &mut intern, self.name.to_str().unwrap())
            .collect::<std::result::Result<Vec<_>, ParseError>>()?;

        self.source_ast = parsed;

        Ok(self)
    }
}
