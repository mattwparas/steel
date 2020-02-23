use std::{io, process};
pub mod env;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod rerrs;
pub mod rvals;

fn main() {
    finish(repl::repl(io::stdin().lock(), io::stdout()));
}

fn finish(result: Result<(), std::io::Error>) -> ! {
    let code = match result {
        Ok(()) => 0,
        Err(e) => {
            eprintln!(
                "{}: {}",
                std::env::args().next().unwrap_or_else(|| "rucket".into()),
                e
            );
            1
        }
    };

    process::exit(code);
}
