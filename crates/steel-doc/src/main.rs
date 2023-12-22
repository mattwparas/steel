use std::{collections::HashSet, io::Write};
use steel::steel_vm::engine::Engine;

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
    std::fs::DirBuilder::new()
        .recursive(true)
        .create("generated")
        .unwrap();

    let mut found_definitions: HashSet<String> = HashSet::new();

    for (module_name, module) in engine.builtin_modules().inner().iter() {
        // Should still dump what functions are available, even without
        // the definitions...
        if module.documentation().definitions().is_empty() {
            continue;
        }

        let module_name_without_slashes = module_name.replace("/", "_");

        let mut module_file =
            std::fs::File::create(format!("generated/{}.md", module_name_without_slashes)).unwrap();

        writeln!(&mut module_file, "# {}", module_name).unwrap();

        // module.documentation().definitions().get()

        if let Some(module_doc) = module.documentation().get(&module_name) {
            if let steel::steel_vm::builtin::Documentation::Markdown(m) = module_doc {
                format_markdown_doc(&mut module_file, m.0);
            }
        }

        found_definitions.clear();

        for (name, value) in module.documentation().definitions() {
            // Don't generate the doc for the module, leave that at the top
            if name.as_ref() == module_name.as_ref() {
                continue;
            }

            found_definitions.insert(name.to_string());

            match value {
                steel::steel_vm::builtin::Documentation::Markdown(m) => {
                    writeln!(&mut module_file, "### **{}**", name).unwrap();

                    format_markdown_doc(&mut module_file, m.0);
                }
                _ => {}
            }
        }

        for name in module.names() {
            if !found_definitions.contains(&name) {
                writeln!(&mut module_file, "### **{}**", name).unwrap();
            }
        }
    }
}
