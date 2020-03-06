use crate::interpreter;
// use std::any::Any;

extern crate rustyline;

// use crate::implement;
// use crate::rerrs::SteelErr;
// use crate::rvals::{CustomType, SteelVal};
use crate::stdlib::PRELUDE;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn repl() -> std::io::Result<()> {
    let mut interpreter = interpreter::SteelInterpreter::new();

    if let Err(e) = interpreter.require(PRELUDE) {
        eprintln!("Error loading prelude: {}", e)
    }
    println!("Welcome to Steel 1.0");

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
