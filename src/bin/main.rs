extern crate steel;
extern crate steel_derive;
extern crate steel_repl;

use steel::steel_vm::{engine::Engine, register_fn::RegisterAsyncFn};
use steel_repl::repl::repl_base;

use std::env::args;
use std::fs;
use std::process;

use env_logger::Builder;
use log::LevelFilter;

fn main() {
    // env_logger::init();

    let mut builder = Builder::new();

    builder
        .filter(Some("pipeline_time"), LevelFilter::Trace)
        .init();

    let args = args().collect::<Vec<_>>();

    let mut vm = configure_engine();

    if args.len() == 1 {
        finish(repl_base(vm));
    } else if args.len() == 2 {
        let path = &args[1];

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
    let mut vm = Engine::new_base();
    vm.register_async_fn("test", test_async_function);
    vm
}
