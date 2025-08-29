use std::{
    collections::{HashMap, HashSet},
    io::Write,
    sync::Arc,
};
use steel::{
    compiler::modules::BUILT_INS,
    parser::{ast::ExprKind, parser::Parser, tokens::TokenType},
    steel_vm::{builtin::BuiltInModule, engine::Engine},
};

fn format_markdown_doc<W: Write>(writer: &mut W, doc: &str) {
    for line in doc.lines() {
        if line.starts_with("# ") {
            write!(writer, "###").unwrap();
        }
        writeln!(writer, "{}", line).unwrap();
    }
}

enum Exported<'a> {
    Builtin(String, &'a BuiltInModule),
    Scheme(String, Option<Arc<String>>),
}

impl PartialEq for Exported<'_> {
    fn eq(&self, other: &Self) -> bool {
        let (
            Exported::Builtin(name1, _) | Exported::Scheme(name1, _),
            Exported::Builtin(name2, _) | Exported::Scheme(name2, _),
        ) = (self, other);

        name1 == name2
    }
}

impl Eq for Exported<'_> {}

impl PartialOrd for Exported<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Exported<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let (
            Exported::Builtin(name1, _) | Exported::Scheme(name1, _),
            Exported::Builtin(name2, _) | Exported::Scheme(name2, _),
        ) = (self, other);

        name1.cmp(name2)
    }
}

fn main() {
    let engine = Engine::new();

    let mut scheme = HashMap::new();
    for (name, module) in BUILT_INS {
        let name = name.trim_start_matches("#%private/");

        let docs = docs_for_scheme_module(module);
        scheme.insert(name.to_owned(), docs);
    }

    // Create the generated directory
    std::fs::DirBuilder::new()
        .recursive(true)
        .create("generated")
        .unwrap();

    let builtin = engine.builtin_modules().inner();

    let modules = builtin
        .keys()
        .map(|name| (**name).to_owned())
        .chain(scheme.keys().map(|name| name.clone()))
        .collect::<HashSet<_>>();

    let mut extra_definitions: Vec<String> = Vec::new();
    for module_name in modules {
        if module_name.starts_with("#%") {
            continue;
        }

        let module_name_without_slashes = module_name.replace("/", "_");

        let mut module_file =
            std::fs::File::create(format!("generated/{}.md", module_name_without_slashes)).unwrap();

        writeln!(&mut module_file, "# {}", module_name).unwrap();

        extra_definitions.clear();
        let mut exported_functions = Vec::new();

        if let Some(module) = builtin.get(&*module_name) {
            if let Some(module_doc) = module.documentation().get(&module_name) {
                if let steel::steel_vm::builtin::Documentation::Markdown(m) = module_doc {
                    format_markdown_doc(&mut module_file, &m.0);
                }
            }

            exported_functions.extend(
                module
                    .names()
                    .into_iter()
                    .filter(|name| !name.starts_with("#%"))
                    .map(|name| Exported::Builtin(name, module)),
            );
        }

        if let Some(module) = scheme.remove(&module_name) {
            exported_functions.extend(
                module
                    .into_iter()
                    .filter(|(name, _)| !name.starts_with("#%"))
                    .map(|(name, docs)| Exported::Scheme(name, docs)),
            );
        }

        exported_functions.sort();

        for exported in exported_functions {
            match exported {
                Exported::Builtin(name, module) => {
                    if let Some(value) = module.documentation().get(&name) {
                        // Don't generate the doc for the module, leave that at the top
                        if name == module_name {
                            continue;
                        }

                        match value {
                            steel::steel_vm::builtin::Documentation::Markdown(m) => {
                                let escaped = name.replace("*", "\\*");
                                writeln!(&mut module_file, "### **{}**", escaped).unwrap();

                                format_markdown_doc(&mut module_file, &m.0);
                            }
                            _ => {}
                        }
                    } else {
                        extra_definitions.push(name);
                    }
                }
                Exported::Scheme(name, docs) => {
                    if let Some(docs) = docs {
                        let escaped = name.replace("*", "\\*");
                        writeln!(&mut module_file, "### **{}**", escaped).unwrap();

                        format_markdown_doc(&mut module_file, &docs);
                    } else {
                        extra_definitions.push(name);
                    }
                }
            }
        }

        for name in &extra_definitions {
            let escaped = name.replace("*", "\\*");
            writeln!(&mut module_file, "### **{}**", escaped).unwrap();
        }
    }
}

fn docs_for_scheme_module(module: &str) -> HashMap<String, Option<Arc<String>>> {
    let parser = Parser::doc_comment_parser(module, None);
    let exprs = parser.filter_map(|x| x.ok()).collect::<Vec<_>>();

    let mut provides = Vec::new();
    let mut docs = HashMap::new();

    for expr in exprs {
        let ExprKind::List(expr) = expr else { continue };
        let mut args = expr.args.iter();

        let Some(ExprKind::Atom(atom)) = args.next() else {
            continue;
        };

        let TokenType::Identifier(ident) = atom.syn.ty else {
            continue;
        };

        if ident.resolve() == "@doc" {
            let Some(ExprKind::Atom(doc)) = args.next() else {
                continue;
            };

            let TokenType::StringLiteral(doc) = &doc.syn.ty else {
                continue;
            };

            let Some(ExprKind::Define(define)) = args.next() else {
                continue;
            };

            let ExprKind::Atom(define) = &define.name else {
                continue;
            };

            let TokenType::Identifier(define) = define.syn.ty else {
                continue;
            };

            let define = define.resolve();

            if define.starts_with("#%") {
                continue;
            }

            docs.insert(define.to_owned(), doc.clone());
        } else if ident.resolve() == "provide" {
            for ident in args {
                let ExprKind::Atom(arg) = ident else {
                    continue;
                };

                let TokenType::Identifier(ident) = arg.syn.ty else {
                    continue;
                };

                let ident = ident.resolve();

                if ident.starts_with("#%") {
                    continue;
                }

                provides.push(ident.to_owned());
            }
        }
    }

    provides
        .into_iter()
        .map(|ident| {
            let doc = docs.remove(&ident);
            (ident, doc)
        })
        .collect()
}
