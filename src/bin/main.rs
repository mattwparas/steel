extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel::{
    steel_vm::{
        builtin::BuiltInModule, engine::Engine, primitives::register_builtin_modules,
        register_fn::RegisterAsyncFn,
    },
    SteelVal,
};
use steel_repl::repl::repl_base;

use std::env::args;
use std::fs;
use std::process;

use env_logger::Builder;
use log::LevelFilter;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The existence of this argument indicates whether we want to run the repl, or interpret this file
    default_file: Option<String>,
    /// File to run
    #[clap(short, long)]
    file: Option<String>,
    /// Emit bytecode
    #[clap(short, long)]
    bytecode: bool,
    /// Emit AST
    #[clap(short, long)]
    ast: bool,
    /// Enter the repl
    #[clap(short, long)]
    it: bool,
}

fn main() {
    // env_logger::init();

    let clap_args = Args::parse();

    println!("{:?}", clap_args);

    let mut builder = Builder::new();

    builder
        // .filter(Some("pipeline_time"), LevelFilter::Trace)
        .filter(Some("steel::compiler::compiler"), LevelFilter::Error)
        .filter(
            Some("steel::steel_vm::contract_checker"),
            LevelFilter::Trace,
        )
        // .filter(Some("reader-macros"), LevelFilter::Trace)
        // .filter(Some("steel::compiler::modules"), LevelFilter::Trace)
        // .filter(Some("steel::parser::replace_idents"), LevelFilter::Trace)
        .init();

    // builder
    //     .filter(Some("steel::compiler::code_generator"), LevelFilter::Trace)
    //     .init();

    let mut vm = configure_engine();

    if let Some(path) = &clap_args.default_file {
        let core_libraries = &[
            steel::stdlib::PRELUDE,
            steel::stdlib::DISPLAY,
            steel::stdlib::CONTRACTS,
        ];

        for core in core_libraries {
            let res = vm.parse_and_execute_without_optimizations(core);
            if let Err(e) = res {
                eprintln!("{}", e);
                return;
            }
        }

        let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
        let res = vm.parse_and_execute_without_optimizations(&contents);

        if let Err(e) = res {
            e.emit_result(path, &contents);
        }
    } else {
        finish(repl_base(vm));
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

async fn test_async_function() -> usize {
    10
}

pub fn configure_engine() -> Engine {
    // let mut vm = Engine::new_base();
    let mut vm = Engine::new_raw();

    register_builtin_modules(&mut vm);

    vm.compile_and_run_raw_program(crate::steel::steel_vm::primitives::ALL_MODULES)
        .unwrap();

    let mut module = BuiltInModule::new("applesauce".to_string());

    module.register_value("bananas", SteelVal::IntV(100));
    module.register_value(
        "foobar",
        SteelVal::StringV(std::rc::Rc::from("hello world!")),
    );

    vm.register_module(module);

    vm

    // let mut vm = Engine::new_base();
    // vm.register_async_fn("test", test_async_function);
    // vm
}
