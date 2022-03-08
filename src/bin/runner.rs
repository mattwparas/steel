extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel::{
    compiler::program::{
        RawProgramWithSymbols, SerializableProgram, SerializableRawProgramWithSymbols,
    },
    steel_vm::{engine::Engine, register_fn::RegisterAsyncFn},
};
use steel_repl::repl::repl_base;

use std::fs;
use std::process;
use std::{env::args, path::PathBuf};

use env_logger::Builder;
use log::LevelFilter;

fn main() {
    // env_logger::init();

    let mut builder = Builder::new();

    // builder
    //     .filter(Some("pipeline_time"), LevelFilter::Trace)
    //     .init();

    builder
        .filter(Some("pipeline_time"), LevelFilter::Trace)
        .filter(Some("steel::steel_vm::vm"), LevelFilter::Trace)
        .init();

    let args = args().collect::<Vec<_>>();

    let mut vm = Engine::new();

    if args.len() == 1 {
        finish(repl_base(vm));
    } else if args.len() == 2 {
        let path = &args[1];

        // let core_libraries = &[
        //     steel::stdlib::PRELUDE,
        //     steel::stdlib::DISPLAY,
        //     steel::stdlib::CONTRACTS,
        // ];

        // for core in core_libraries {
        //     let res = vm.parse_and_execute_without_optimizations(core);
        //     if let Err(e) = res {
        //         eprintln!("{}", e);
        //         return;
        //     }
        // }

        let program = RawProgramWithSymbols::parse_from_self_hosted_file(path).unwrap();

        println!("Successfully parsed program from file");

        // let program = SerializableRawProgramWithSymbols::read_from_file(path)
        //     .unwrap()
        //     .into_raw_program();

        // let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
        let res = vm.run_raw_program(program);

        match res {
            Ok(s) => {
                println!("{:?}", s);
            }
            Err(e) => {
                eprintln!("{:?}", e);
            }
        }
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

// pub fn configure_engine() -> Engine {
//     let mut vm = Engine::new_base();
//     vm.register_async_fn("test", test_async_function);
//     vm
// }
