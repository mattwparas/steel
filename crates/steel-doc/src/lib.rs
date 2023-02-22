use std::{error::Error, io::BufWriter, path::PathBuf};

use steel::steel_vm::engine::Engine;

use std::collections::HashSet;
use std::io::Write;

struct DocumentGenerator {
    // Root directory for generated documentation to go
    output_dir: PathBuf,
    // Final output locations
    writers: HashSet<PathBuf>,
}

impl DocumentGenerator {
    pub fn new(output_dir: PathBuf) -> Self {
        Self {
            output_dir,
            writers: HashSet::new(),
        }
    }

    pub fn record(&mut self, path: PathBuf) {
        self.writers.insert(path);
    }
}

fn walk_for_defines<W: Write>(
    writer: &mut W,
    ast: &[steel::parser::ast::ExprKind],
) -> Result<(), Box<dyn Error>> {
    let mut nodes = ast.iter();

    while let Some(node) = nodes.next() {
        match &node {
            steel::parser::ast::ExprKind::Define(d) => {
                let name = d.name.atom_identifier().unwrap();

                // We'll only check things that are values
                if !name.starts_with("mangler") && name.ends_with("__doc__") {
                    writeln!(writer, "### **{}**", name.trim_end_matches("__doc__"))?;

                    // println!("{}")

                    let ast_node = nodes.next().unwrap();

                    // println!("{:?}", ast_node);

                    if let steel::parser::ast::ExprKind::Define(def) = &ast_node {
                        if let steel::parser::ast::ExprKind::Quote(q) = &def.body {
                            if let steel::parser::ast::ExprKind::Define(d) = &q.expr {
                                if let steel::parser::ast::ExprKind::LambdaFunction(l) = &d.body {
                                    writeln!(writer, "```scheme")?;
                                    write!(writer, "({}", name.trim_end_matches("__doc__"))?;

                                    if l.rest && l.args.len() == 1 {
                                        write!(writer, " .")?;
                                    }

                                    for arg in &l.args {
                                        if let Some(ident) = arg.atom_identifier() {
                                            // Macros will generate unreadable symbols - so for the sake
                                            // of the documentation generator, we probably want to make this
                                            // more human readable
                                            write!(writer, " {}", ident.trim_start_matches("#"))?;
                                        } else {
                                            write!(writer, " {arg}")?;
                                        }
                                    }

                                    writeln!(writer, ")")?;
                                    writeln!(writer, "```")?;
                                }
                            }
                        }
                    }

                    writeln!(writer, "{}", d.body.string_literal().unwrap())?;
                }
            }

            steel::parser::ast::ExprKind::Begin(b) => {
                walk_for_defines(writer, &b.exprs)?;
            }

            _ => {}
        }
    }

    Ok(())
}

fn top_level_walk_directory(path: PathBuf, vm: &mut Engine) {
    todo!()
}

pub fn walk_dir<W: Write>(
    writer: &mut W,
    path: PathBuf,
    vm: &mut Engine,
) -> Result<(), Box<dyn Error>> {
    if path.is_dir() {
        // If contents are a cog module, then we should grab it
        // OR, if at any point in the recursion we've found a cog module in the directory, we should
        // be good to check it out
        let directory_contents = path.read_dir()?.collect::<Result<Vec<_>, _>>()?;

        for file in directory_contents {
            let path = file.path();
            walk_dir(writer, path, vm)?;
        }
    } else if path.extension().and_then(|x| x.to_str()) == Some("scm")
        && path.file_name().and_then(|x| x.to_str()) != Some("cog.scm")
    {
        writeln!(writer, "# {:?}", path)?;

        let contents = std::fs::read_to_string(&path)?;
        let ast = vm.emit_fully_expanded_ast(&contents, Some(path))?;
        walk_for_defines(writer, &ast)?;
    }

    writer.flush()?;

    Ok(())
}

// Parse the cog file located at the path, and return the package name
// Other things are probably important, but for now we'll just deal with that
pub fn parse_cog_file(path: PathBuf) -> steel::rvals::Result<String> {
    let contents = std::fs::read_to_string(&path)?;
    let exprs = steel::parser::parser::Parser::parse(&contents)?;
    todo!()
}
