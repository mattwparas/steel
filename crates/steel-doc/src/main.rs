use std::{
    collections::{HashMap, HashSet},
    io::Write,
    sync::Arc,
};
use steel::{
    compiler::modules::{BUILT_INS, PRELUDE_MODULES},
    parser::{ast::ExprKind, parser::Parser, tokens::TokenType},
    steel_vm::engine::Engine,
};

fn format_markdown_doc<W: Write>(writer: &mut W, doc: &str) {
    for line in doc.lines() {
        if line.starts_with("# ") {
            write!(writer, "###").unwrap();
        }
        writeln!(writer, "{}", line).unwrap();
    }
}

fn main() {
    let engine = Engine::new();

    // Create the generated directory
    std::fs::create_dir_all("generated").unwrap();

    std::fs::create_dir_all("generated/builtins").unwrap();

    let mut found_definitions: HashSet<String> = HashSet::new();
    for (module_name, module) in engine.builtin_modules().inner().iter() {
        if module_name.starts_with("#%") {
            continue;
        }

        let module_name_without_slashes = module_name.replace("/", "_");

        let mut module_file = std::fs::File::create(format!(
            "generated/builtins/{}.md",
            module_name_without_slashes
        ))
        .unwrap();

        writeln!(&mut module_file, "# {}", module_name).unwrap();

        // module.documentation().definitions().get()

        if let Some(module_doc) = module.documentation().get(&module_name) {
            if let steel::steel_vm::builtin::Documentation::Markdown(m) = module_doc {
                format_markdown_doc(&mut module_file, &m.0);
            }
        }

        found_definitions.clear();

        let mut exported_functions: Vec<_> = module
            .names()
            .into_iter()
            .filter(|name| !name.starts_with("#%"))
            .collect();

        exported_functions.sort();

        for name in &exported_functions {
            if let Some(value) = module.documentation().get(name) {
                // Don't generate the doc for the module, leave that at the top
                if name == module_name.as_ref() {
                    continue;
                }

                found_definitions.insert(name.to_string());

                match value {
                    steel::steel_vm::builtin::Documentation::Markdown(m) => {
                        let escaped = name.replace("*", "\\*");
                        writeln!(&mut module_file, "### **{}**", escaped).unwrap();

                        format_markdown_doc(&mut module_file, &m.0);
                    }
                    _ => {}
                }
            }
        }

        for name in exported_functions {
            if !found_definitions.contains(&name) {
                writeln!(&mut module_file, "### **{}**", name).unwrap();
            }
        }
    }

    std::fs::create_dir_all("generated/stdlib").unwrap();
    for (name, module) in BUILT_INS {
        let file_name = name.replace("/", "_").replace("#%", "");
        let mut file = std::fs::File::create(format!("generated/stdlib/{}.md", file_name)).unwrap();

        writeln!(file, "# {name}").unwrap();

        if PRELUDE_MODULES.contains(name) {
            writeln!(file, "**this module is in the prelude and therefore automatically available when running steel.**").unwrap();
            writeln!(file).unwrap();
        }

        let docs = docs_for_scheme_module(module);
        for (item, doc) in docs {
            let escaped = item.replace("*", "\\*");
            if let Some(doc) = doc {
                writeln!(file, "### **{}**", escaped).unwrap();
                format_markdown_doc(&mut file, &doc);
            } else {
                writeln!(file, "### **{}**", escaped).unwrap();
            }
        }
    }
}

fn docs_for_scheme_module(module: &str) -> Vec<(String, Option<Arc<String>>)> {
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

    let mut definitions = provides
        .into_iter()
        .map(|ident| {
            let doc = docs.remove(&ident);
            (ident, doc)
        })
        .collect::<Vec<_>>();
    definitions
        .sort_by(|(k1, d1), (k2, d2)| d2.is_some().cmp(&d1.is_some()).then_with(|| k1.cmp(&k2)));
    definitions
}
