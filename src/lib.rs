extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel::steel_vm::engine::Engine;
use steel_doc::walk_dir;
use steel_repl::{register_readline_module, run_repl};

use std::path::PathBuf;
use std::process;
use std::{error::Error, fs};

use clap::{CommandFactory, Parser};

/// Steel Interpreter
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None, trailing_var_arg = true, allow_hyphen_values = true, disable_help_flag = true, disable_help_subcommand = true)]
pub struct Args {
    /// What action to perform on this file, the absence of a subcommand indicates that the given file (if any)
    /// will be run as the entrypoint
    #[clap(subcommand)]
    action: Option<EmitAction>,

    /// The existence of this argument indicates whether we want to run the repl, or interpret this file
    default_file: Option<PathBuf>,

    /// Arguments to the input file
    arguments: Vec<String>,
}

#[derive(clap::Subcommand, Debug)]
enum EmitAction {
    /// Output a debug display of the fully transformed bytecode
    Bytecode { default_file: Option<PathBuf> },
    /// Print a debug display of the fully expanded AST
    Ast {
        default_file: Option<PathBuf>,
        #[arg(long)]
        expanded: Option<bool>,
        #[arg(long)]
        pretty: Option<bool>,
    },
    /// Enter the repl with the given file loaded
    Interactive {
        default_file: Option<PathBuf>,
        arguments: Vec<String>,
    },
    /// Create basic project folder in current dir
    New { folder_name: String },
    /// Tests the module - only tests modules which provide values
    Test { default_file: Option<String> },
    /// Generate the documentation for a file
    Doc { default_file: Option<PathBuf> },
    /// Experimental
    Compile { file: PathBuf },

    /// Build a dylib from the root of this directory
    Dylib,
}

#[cfg(feature = "build-info")]
const VERSION_MESSAGE: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    "-",
    env!("VERGEN_RUSTC_SEMVER"),
    " (",
    env!("VERGEN_BUILD_DATE"),
    ")"
);

pub fn run(clap_args: Args) -> Result<(), Box<dyn Error>> {
    let mut vm = Engine::new();
    vm.register_value("std::env::args", steel::SteelVal::ListV(vec![].into()));

    register_readline_module(&mut vm);

    match clap_args {
        Args {
            default_file: None,
            action: None,
            ..
        } => {
            // if arguments.iter().find(|x| x.as_str() == "--help").is_some() {
            //     println!("{}", Args::command().render_long_help());
            // }

            #[cfg(feature = "build-info")]
            {
                println!("{}", VERSION_MESSAGE);
            }
            run_repl(vm)?;
            Ok(())
        }

        Args {
            default_file: Some(path),
            action: None,
            arguments,
        } => {
            if path
                .as_os_str()
                .to_str()
                .map(|x| x == "--help")
                .unwrap_or_default()
            {
                println!("{}", Args::command().render_long_help());
            }

            vm.register_value(
                "std::env::args",
                steel::SteelVal::ListV(
                    arguments
                        .into_iter()
                        .map(|x| steel::SteelVal::StringV(x.into()))
                        .collect(),
                ),
            );

            let contents = fs::read_to_string(&path)?;
            let res = vm.compile_and_run_raw_program_with_path(contents.clone(), path.clone());

            if let Err(e) = res {
                vm.raise_error(e.clone());
                process::exit(1);
            }

            Ok(())
        }

        Args {
            default_file: None,
            action: Some(EmitAction::Test { default_file }),
            ..
        } => {
            let file_or_current_dir: String = default_file.unwrap_or(".".to_string());
            if let Some(path) = PathBuf::from(file_or_current_dir).to_str() {
                let mut vm = Engine::new();
                vm.register_value(
                    "std::env::args",
                    steel::SteelVal::ListV(vec![path.to_string().into()].into()),
                );
                let test_script = include_str!("../cogs/test-runner.scm");
                if let Err(e) = vm.run(test_script) {
                    vm.raise_error(e.clone());
                    return Err(Box::new(e));
                }
            }
            Ok(())
        }
        Args {
            action: Some(EmitAction::New { folder_name }),
            ..
        } => {
            let folder_creation_result = create_project_folder(&folder_name);
            if let Err(e) = folder_creation_result {
                eprintln!("{e}");
                return Ok(());
            }
            println!("project folder `{}` created", &folder_name);
            Ok(())
        }
        Args {
            default_file: None,
            action: Some(EmitAction::Doc {
                default_file: Some(path),
            }),
            ..
        } => {
            let mut writer = std::io::BufWriter::new(std::io::stdout());
            walk_dir(&mut writer, path, &mut vm)?;
            Ok(())
        }

        Args {
            default_file: None,
            action:
                Some(EmitAction::Bytecode {
                    default_file: Some(path),
                }),
            ..
        } => {
            let contents = fs::read_to_string(&path)?;
            let program = vm.emit_raw_program(contents.clone(), path.clone());

            match program {
                Ok(program) => {
                    vm.debug_print_build(path.to_str().unwrap().to_string(), program)
                        .unwrap();
                }
                Err(e) => vm.raise_error(e),
            }

            Ok(())
        }

        Args {
            default_file: None,
            action:
                Some(EmitAction::Ast {
                    default_file: Some(path),
                    expanded,
                    pretty,
                }),
            ..
        } => {
            let contents = fs::read_to_string(path.clone())?;

            let expanded = expanded.unwrap_or(true);
            let pretty = pretty.unwrap_or(true);

            let res = match (expanded, pretty) {
                (true, true) => vm.emit_fully_expanded_ast_to_string(&contents, Some(path.clone())),
                (true, false) => vm
                    .emit_fully_expanded_ast(&contents, Some(path.clone()))
                    .map(|ast| format!("{:#?}", ast)),
                (false, true) => Engine::emit_ast_to_string(&contents),
                (false, false) => Engine::emit_ast(&contents).map(|ast| format!("{:#?}", ast)),
            };

            match res {
                Ok(ast) => println!("{ast}"),
                Err(e) => vm.raise_error(e),
            }

            Ok(())
        }

        Args {
            default_file: None,
            action:
                Some(EmitAction::Interactive {
                    default_file: Some(path),
                    arguments: _,
                }),
            ..
        } => {
            let core_libraries = &[steel::stdlib::PRELUDE];

            for core in core_libraries {
                let res = vm.compile_and_run_raw_program(*core);
                if let Err(e) = res {
                    eprintln!("{e}");
                    return Ok(());
                }
            }

            let contents =
                fs::read_to_string(&path).expect("Something went wrong reading the file");
            let res = vm.compile_and_run_raw_program_with_path(contents.clone(), path.clone());

            if let Err(e) = res {
                vm.raise_error(e);
            }

            run_repl(vm)?;
            Ok(())
        }

        Args {
            default_file: None,
            action: Some(EmitAction::Compile { file }),
            ..
        } => {
            println!("---- Warning: This is an experimental feature ----");

            let entrypoint =
                fs::read_to_string(&file).expect("Something went wrong reading the file");

            // Something went wrong - TODO: Raise the error correctly
            let non_interactive_program =
                Engine::create_non_interactive_program_image(entrypoint, file).unwrap();

            let mut temporary_output = PathBuf::from("steel_target/src");

            if !temporary_output.exists() {
                std::fs::create_dir_all(&temporary_output).unwrap();
            } else {
                // Clean up I guess?
                std::fs::remove_dir_all(&temporary_output).unwrap();
                std::fs::create_dir_all(&temporary_output).unwrap();
            }

            temporary_output.push("program.bin");

            // This probably needs to get stashed in some temporary target directory?
            non_interactive_program.write_bytes_to_file(&temporary_output);

            temporary_output.pop();

            let rust_entrypoint = r#"
fn main() {
    steel::steel_vm::engine::Engine::execute_non_interactive_program_image(include_bytes!("program.bin"));
}
            "#;

            temporary_output.push("main.rs");

            std::fs::write(&temporary_output, rust_entrypoint).unwrap();

            temporary_output.pop();
            temporary_output.pop();

            temporary_output.push("Cargo.toml");

            let toml_file = r#"
[package]
name = "steel-executable"
authors = [""]
edition = "2021"
license = "MIT OR Apache-2.0"
version = "0.1.0"

[workspace]


[dependencies]
# steel-core = { git = "https://github.com/mattwparas/steel.git", features = ["dylibs", "stacker", "sync"] }
steel-core = { path = "../crates/steel-core", features = ["dylibs", "stacker", "sync"] }

[profile.release]
debug = false
lto = true
            "#;
            std::fs::write(&temporary_output, toml_file).unwrap();
            std::process::Command::new("cargo")
                .current_dir("steel_target")
                .arg("build")
                .arg("--release")
                .spawn()
                .unwrap()
                .wait()
                .unwrap();

            Ok(())
        }

        Args {
            default_file: None,
            action: Some(EmitAction::Dylib),
            ..
        } => {
            #[cfg(not(target_os = "redox"))]
            cargo_steel_lib::run(Vec::new(), Vec::new())?;

            #[cfg(target_os = "redox")]
            println!("Creating dylibs is not yet supported on Redox");

            Ok(())
        }

        _ => {
            run_repl(vm)?;
            Ok(())
        }
    }
}

fn create_project_folder(folder_name: &String) -> Result<(), std::io::Error> {
    fs::create_dir(folder_name)?;
    let manifest_content = format!(
        r#"(define package-name "{folder_name}")
(define version "0.1.0")

(define dependencies '())
"#
    );
    fs::write(format!("{folder_name}/cog.scm"), manifest_content)?;
    let main_content = r#"(require "mysum.scm")

(define (main a b)
    (begin
        ( displayln "hello world!" )
        ( display a )
        ( display " + " )
        ( display b )
        ( display " = " )
        ( displayln (mysum a b) )
    ))

(main 1 1)
"#;
    fs::write(format!("{folder_name}/main.scm"), main_content)?;
    let mysum_content = r#"(provide mysum)

(define (mysum a b) (+ a b) )
"#;
    fs::write(format!("{folder_name}/mysum.scm"), mysum_content)?;
    let mysum_test_content = r#"(require "steel/tests/unit-test.scm" (for-syntax "steel/tests/unit-test.scm"))
(require "mysum.scm")

(define dummy-provide 1)
(provide dummy-provide)

(test-module
  "mysum"
  (check-equal? "my sum works!" 3 (mysum 1 2))
)
"#;
    fs::write(format!("{folder_name}/mysum-test.scm"), mysum_test_content)?;
    Ok(())
}

pub fn finish(result: Result<(), std::io::Error>) -> ! {
    let code = match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!(
                "{}: {}",
                std::env::args().next().unwrap_or_else(|| "steel".into()),
                e
            );
            1
        }
    };

    process::exit(code);
}

#[test]
fn test_runner() {
    let args = Args {
        action: None,
        default_file: Some(PathBuf::from("cogs/test-runner.scm")),
        arguments: vec!["cogs/".to_string()],
    };

    run(args).unwrap()
}

#[test]
fn r5rs_test_suite() {
    let args = Args {
        action: None,
        default_file: Some(PathBuf::from("cogs/r5rs.scm")),
        arguments: vec![],
    };

    run(args).unwrap()
}

#[test]
fn r7rs_test_suite() {
    let args = Args {
        action: None,
        default_file: Some(PathBuf::from("cogs/r7rs.scm")),
        arguments: vec![],
    };

    run(args).unwrap()
}

#[test]
fn r7rs_benchmark_test_suite() {
    let benches = &[
        "r7rs-benchmarks/scheme.scm",
        "r7rs-benchmarks/simplex.scm",
        "r7rs-benchmarks/array1.scm",
        "r7rs-benchmarks/triangl.scm",
    ];

    for bench in benches {
        let args = Args {
            action: None,
            default_file: Some(PathBuf::from(bench)),
            arguments: vec![],
        };

        run(args).unwrap();
    }
}

#[test]
fn syntax_test_suite() {
    let args = Args {
        action: None,
        default_file: Some(PathBuf::from("cogs/syntax-tests.scm")),
        arguments: vec![],
    };

    run(args).unwrap()
}
