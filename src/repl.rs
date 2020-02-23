// use std::vec::Vec;
// use this::lexer;
use std::io::BufRead;
use std::io::Write;

use crate::evaluator;
use crate::parser;

pub fn repl(mut user_input: impl BufRead, mut output: impl Write) -> std::io::Result<()> {
    let mut evaluator = evaluator::Evaluator::new();

    writeln!(output, "Welcome to Rucket 1.0")?;

    loop {
        write!(output, "λ > ")?;
        output.flush()?;
        let mut input = String::new();
        let raw_input = user_input.read_line(&mut input)?;

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
                        let res = evaluator.eval(&e);
                        match res {
                            Ok(v) => writeln!(output, "{}", v),
                            Err(e) => {
                                writeln!(output, "{}", e)?;
                                break;
                            }
                        }
                    }
                    Err(e) => writeln!(output, "{:?}", e),
                }?
            }
        }
    }
}
