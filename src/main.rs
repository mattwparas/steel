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

/// Steel Interpreter Client
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The existence of this argument indicates whether we want to run the repl, or interpret this file
    default_file: Option<String>,

    /// Arguments to the input file
    arguments: Vec<String>,

    /// What action to perform on this file, the absence of a subcommand indicates that the given file (if any)
    /// will be run as the entrypoint
    #[clap(subcommand)]
    action: Option<EmitAction>,
}

#[derive(clap::Subcommand, Debug)]
enum EmitAction {
    /// Output a debug display of the fully transformed bytecode
    Bytecode,
    /// Print a debug display of the fully expanded AST
    Ast,
    /// Enter the repl with the given file loaded
    Interactive,
}

fn main() {
    // env_logger::init();

    // let mut builder = Builder::new();

    // let log_targets = ["requires", "steel::compiler::modules"];

    // for target in log_targets {
    //     builder.filter(Some(target), LevelFilter::Trace);
    // }

    // builder.init();

    let clap_args = Args::parse();

    let mut vm = configure_engine();

    match clap_args {
        Args {
            default_file: None,
            action: None,
            ..
        } => finish(repl_base(vm)),

        Args {
            default_file: Some(path),
            action,
            arguments,
        } => match action {
            None => {
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
                let res = vm
                    .compile_and_run_raw_program_with_path(&contents, PathBuf::from(path.clone()));

                if let Err(e) = res {
                    e.emit_result(&path, &contents);
                }
            }
            Some(EmitAction::Bytecode) => {
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
                        vm.debug_print_build(path.clone(), program).unwrap();
                    }
                    Err(e) => e.emit_result(&path, &contents),
                }
            }
            Some(EmitAction::Ast) => {
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

                let contents = fs::read_to_string(path.clone())
                    .expect("Something went wrong reading the file");

                let res =
                    vm.emit_fully_expanded_ast_to_string(&contents, Some(path.clone().into()));

                match res {
                    Ok(ast) => println!("{ast}"),
                    Err(e) => e.emit_result(&path, &contents),
                }
            }
            Some(EmitAction::Interactive) => {
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
                let res = vm
                    .compile_and_run_raw_program_with_path(&contents, PathBuf::from(path.clone()));

                if let Err(e) = res {
                    e.emit_result(&path, &contents);
                }

                finish(repl_base(vm))
            }
        },
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
