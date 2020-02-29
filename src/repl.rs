// use std::vec::Vec;
// use this::lexer;
// use std::io::BufRead;
// use std::io::Write;
use crate::interpreter;

// pub fn repl(mut user_input: impl BufRead, mut output: impl Write) -> std::io::Result<()> {
//     let mut evaluator = evaluator::Evaluator::new();

//     writeln!(output, "Welcome to Rucket 1.0")?;

//     loop {
//         write!(output, "λ > ")?;
//         output.flush()?;
//         let mut input = String::new();
//         let raw_input = user_input.read_line(&mut input)?;

//         if raw_input == 0 {
//             println!("EOF reached");
//             return Ok(());
//         }

//         if &input == ":quit\n" {
//             return Ok(());
//         } else {
//             let parsed = parser::Parser::new(&input);
//             for expr in parsed {
//                 match expr {
//                     Ok(e) => {
//                         let res = evaluator.eval(&e);
//                         match res {
//                             Ok(v) => writeln!(output, "{}", v),
//                             Err(e) => {
//                                 writeln!(output, "{}", e)?;
//                                 break;
//                             }
//                         }
//                     }
//                     Err(e) => writeln!(output, "{:?}", e),
//                 }?
//             }
//         }
//     }
// }

extern crate rustyline;

use crate::stdlib::PRELUDE;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn repl() -> std::io::Result<()> {
    let mut interpreter = interpreter::RucketInterpreter::new();

    if let Err(e) = interpreter.require(PRELUDE) {
        eprintln!("Error loading prelude: {}", e)
    }
    println!("Welcome to Rucket 1.0");

    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("λ > ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match line.as_str() {
                    ":quit" => return Ok(()),
                    ":reset" => interpreter.reset(),
                    _ => match interpreter.evaluate(&line) {
                        Ok(r) => r.iter().for_each(|x| println!("{}", x)),
                        Err(e) => eprintln!("{}", e),
                    },
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}
