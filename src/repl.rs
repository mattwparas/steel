// use std::vec::Vec;
// use this::lexer;

use std::io;
use std::io::Write;

use crate::evaluator;
use crate::parser;

pub fn repl() -> std::io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    writeln!(stdout, "Welcome to Rucket 1.0")?;

    loop {
        write!(stdout, "λ > ")?;
        stdout.flush()?;
        let mut input = String::new();
        let raw_input = stdin.read_line(&mut input)?;

        if raw_input == 0 {
            println!("EOF reached");
            return Ok(());
        }

        if &input == ":quit\n" {
            return Ok(());
        } else {
            let parsed = parser::Parser::new(&input);
            for expr in parsed {
                match expr {
                    Ok(e) => {
                        let res = evaluator::evaluator(e);
                        match res {
                            Ok(v) => writeln!(stdout, "{}", v),
                            Err(e) => writeln!(stdout, "{}", e),
                        }
                    }
                    Err(e) => writeln!(stdout, "{:?}", e),
                }?
            }
        }
    }
}
