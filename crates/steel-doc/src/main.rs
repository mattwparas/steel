use std::io::Write;
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

    for (module_name, module) in engine.builtin_modules().inner().iter() {
        if module.documentation().definitions().is_empty() {
            continue;
        }

        let module_name_without_slashes = module_name.replace("/", "_");

        let mut module_file =
            std::fs::File::create(format!("generated/{}.md", module_name_without_slashes)).unwrap();

        writeln!(&mut module_file, "# {}", module_name).unwrap();

        for (name, value) in module.documentation().definitions() {
            match value {
                steel::steel_vm::builtin::Documentation::Markdown(m) => {
                    writeln!(&mut module_file, "### **{}**", name).unwrap();

                    format_markdown_doc(&mut module_file, m.0);
                }
                _ => {}
            }
        }
    }
}
