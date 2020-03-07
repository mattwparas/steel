use crate::interpreter;
extern crate rustyline;
use crate::stdlib::PRELUDE;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::time::{Duration, Instant};

#[macro_export]
macro_rules! build_repl {
    ($($type:ty),*) => {
        {
            use crate::build_interpreter;
            let mut interpreter = build_interpreter!{
                $(
                    $type
                ),*
            };
            repl_base(interpreter)
        }
    };
}

pub fn repl_base(mut interpreter: interpreter::SteelInterpreter) -> std::io::Result<()> {
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
                    _ => {
                        let now = Instant::now();
                        let res = interpreter.evaluate(&line);
                        // it prints '2'
                        println!("{:?}", now.elapsed());
                        match res {
                            Ok(r) => r.iter().for_each(|x| println!("{}", x)),
                            Err(e) => eprintln!("{}", e),
                        }
                    }
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

pub fn repl() -> std::io::Result<()> {
    repl_base(interpreter::SteelInterpreter::new())
}
