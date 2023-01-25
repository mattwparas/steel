extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel::steel_vm::engine::Engine;
use steel_repl::repl::repl_base;

use std::fs;
use std::path::PathBuf;
use std::process;

use clap::Parser;

// use env_logger::Builder;
// use log::LevelFilter;

/// Steel Interpreter
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
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
    Ast { default_file: Option<PathBuf> },
    /// Enter the repl with the given file loaded
    Interactive {
        default_file: Option<PathBuf>,
        arguments: Vec<String>,
    },
    /// Test the module
    Test { default_file: Option<String> },
    /// Generate the documentation for a file
    Doc { default_file: Option<PathBuf> },
}

fn main() {
    // env_logger::init();

    // let mut builder = Builder::new();

    // let log_targets = [
    //     "requires",
    //     "steel::compiler::modules",
    //     "steel::parser::expander",
    // ];

    // for target in log_targets {
    //     builder.filter(Some(target), LevelFilter::Trace);
    // }

    // builder.init();

    let clap_args = Args::parse();

    let mut vm = configure_engine();

    // Register an ephemeral env args to satisfy the semantic analysis
    // vm.register_value("std::env::args", steel::SteelVal::Void);

    match clap_args {
        Args {
            default_file: None,
            action: None,
            ..
        } => finish(repl_base(vm)),

        Args {
            default_file: Some(path),
            action: None,
            arguments,
        } => {
            // let core_libraries = &[
            //     steel::stdlib::PRELUDE,
            //     steel::stdlib::DISPLAY,
            //     steel::stdlib::CONTRACTS,
            // ];

            // for core in core_libraries {
            //     let res = vm.compile_and_run_raw_program(core);
            //     if let Err(e) = res {
            //         eprintln!("{}", e);
            //         return;
            //     }
            // }

            vm.register_value(
                "std::env::args",
                steel::SteelVal::ListV(
                    arguments
                        .into_iter()
                        .map(|x| steel::SteelVal::StringV(x.into()))
                        .collect(),
                ),
            );

            let contents =
                fs::read_to_string(&path).expect("Something went wrong reading the file");
            let res =
                vm.compile_and_run_raw_program_with_path(&contents, PathBuf::from(path.clone()));

            if let Err(e) = res {
                e.emit_result(path.to_str().unwrap(), &contents);
                process::exit(1);
            }

            process::exit(0);
        }

        Args {
            default_file: None,
            action: Some(EmitAction::Test { default_file }),
            ..
        } => {
            todo!()
        }

        Args {
            default_file: None,
            action: Some(EmitAction::Doc { default_file }),
            ..
        } => {
            todo!()
        }

        Args {
            default_file: None,
            action:
                Some(EmitAction::Bytecode {
                    default_file: Some(path),
                }),
            ..
        } => {
            let core_libraries = &[
                steel::stdlib::PRELUDE,
                steel::stdlib::DISPLAY,
                steel::stdlib::CONTRACTS,
            ];

            for core in core_libraries {
                let res = vm.compile_and_run_raw_program(core);
                if let Err(e) = res {
                    eprintln!("{}", e);
                    return;
                }
            }

            let contents =
                fs::read_to_string(&path).expect("Something went wrong reading the file");

            let program = vm.emit_raw_program(&contents, path.clone().into());

            match program {
                Ok(program) => {
                    vm.debug_print_build(path.to_str().unwrap().to_string(), program)
                        .unwrap();
                }
                Err(e) => e.emit_result(path.to_str().unwrap(), &contents),
            }
        }

        Args {
            default_file: None,
            action: Some(EmitAction::Ast {
                default_file: Some(path),
            }),
            ..
        } => {
            let core_libraries = &[
                steel::stdlib::PRELUDE,
                steel::stdlib::DISPLAY,
                steel::stdlib::CONTRACTS,
            ];

            for core in core_libraries {
                let res = vm.compile_and_run_raw_program(core);
                if let Err(e) = res {
                    eprintln!("{}", e);
                    return;
                }
            }

            let contents =
                fs::read_to_string(path.clone()).expect("Something went wrong reading the file");

            let res = vm.emit_fully_expanded_ast_to_string(&contents, Some(path.clone().into()));

            match res {
                Ok(ast) => println!("{ast}"),
                Err(e) => e.emit_result(path.to_str().unwrap(), &contents),
            }
        }

        Args {
            default_file: None,
            action:
                Some(EmitAction::Interactive {
                    default_file: Some(path),
                    arguments,
                }),
            ..
        } => {
            let core_libraries = &[
                steel::stdlib::PRELUDE,
                steel::stdlib::DISPLAY,
                steel::stdlib::CONTRACTS,
            ];

            for core in core_libraries {
                let res = vm.compile_and_run_raw_program(core);
                if let Err(e) = res {
                    eprintln!("{}", e);
                    return;
                }
            }

            let contents =
                fs::read_to_string(&path).expect("Something went wrong reading the file");
            let res =
                vm.compile_and_run_raw_program_with_path(&contents, PathBuf::from(path.clone()));

            if let Err(e) = res {
                e.emit_result(path.to_str().unwrap(), &contents);
            }

            finish(repl_base(vm))
        }

        _ => finish(repl_base(vm)),
    }
}

fn finish(result: Result<(), std::io::Error>) -> ! {
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

pub fn configure_engine() -> Engine {
    // let mut vm = Engine::new_base();
    let vm = Engine::new();

    // register_builtin_modules(&mut vm);

    // vm.compile_and_run_raw_program(crate::steel::steel_vm::primitives::ALL_MODULES)
    //     .unwrap();

    // let mut module = BuiltInModule::new("applesauce".to_string());

    // module.register_value("bananas", SteelVal::IntV(100));
    // module.register_value(
    //     "foobar",
    //     SteelVal::StringV(std::rc::Rc::from("hello world!")),
    // );

    // vm.register_module(module);

    vm

    // let mut vm = Engine::new_base();
    // vm.register_async_fn("test", test_async_function);
    // vm
}
