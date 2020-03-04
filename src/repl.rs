// use std::vec::Vec;
// use this::lexer;
// use std::io::BufRead;
// use std::io::Write;
use crate::interpreter;
use std::any::Any;
// use std::any::type_name;

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

use crate::rvals::{CustomType, RucketVal};
use crate::stdlib::PRELUDE;
use crate::unwrap;
use rustyline::error::ReadlineError;
use rustyline::Editor;

// pub trait CustomType {
//     fn box_clone(&self) -> Box<dyn CustomType>;
//     fn name(&self) -> String;
// }

#[derive(Clone, Debug)]
struct MyStruct {}

impl CustomType for MyStruct {
    fn box_clone(&self) -> Box<dyn CustomType> {
        Box::new((*self).clone())
    }

    fn as_any(&self) -> Box<dyn Any> {
        Box::new((*self).clone())
    }

    fn new_rucket_val(&self) -> RucketVal {
        RucketVal::Custom(Box::new(self.clone()))
    }

    // fn unwrap_type(&self) ->
}

// impl usize

pub fn repl() -> std::io::Result<()> {
    let mut interpreter = interpreter::RucketInterpreter::new();

    if let Err(e) = interpreter.require(PRELUDE) {
        eprintln!("Error loading prelude: {}", e)
    }
    println!("Welcome to Rucket 1.0");

    println!("Attempting to insert my own type");

    let testytest = MyStruct {};
    // let testytest: usize = 420;
    let my_val = testytest.new_rucket_val();

    interpreter.insert_binding("test".to_string(), my_val.clone());

    println!("{:?}", unwrap!(my_val, MyStruct).unwrap());

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
