extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel::steel_vm::engine::Engine;
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
        .filter(Some("steel::compiler::code_generator"), LevelFilter::Trace)
        .init();

    let args = args().collect::<Vec<_>>();

    let mut vm = Engine::new();

    if args.len() == 1 {
        finish(repl_base(vm));
    } else if args.len() == 2 {
        let path = &args[1];

        let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
        let res = vm.emit_raw_program(&contents, PathBuf::from(path));

        match res {
            Ok(s) => {
                println!("Successfully compiled program");

                // Debug print out the instructions to see what we're dealing with
                s.debug_print();

                s.into_serializable_program()
                    .unwrap()
                    .write_to_file("steel-output")
                    .unwrap();
                println!("Written to file steel-output.txt");
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
