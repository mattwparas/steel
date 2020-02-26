use std::process;
pub mod env;
pub mod evaluator;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod rerrs;
pub mod rvals;
pub mod tokens;

fn main() {
    // finish(repl::repl(io::stdin().lock(), io::stdout()));
    finish(repl::repl());
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
