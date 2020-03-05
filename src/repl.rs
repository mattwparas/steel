// use std::vec::Vec;
// use this::lexer;
// use std::io::BufRead;
// use std::io::Write;
// use crate::env::MyStruct;
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

use crate::implement;
// use crate::rerrs::RucketErr;
use crate::rvals::{CustomType, RucketVal};
use crate::stdlib::PRELUDE;
use crate::unwrap;
use rustyline::error::ReadlineError;
use rustyline::Editor;

// pub trait CustomType {
//     fn box_clone(&self) -> Box<dyn CustomType>;
//     fn name(&self) -> String;
// }

/*

There are two things we could do here:

Something like:

impl!($type), for all things that you just want to pass around and use

or

derive(Scheme)

for struct types you want to embed inside the language, generates all the necessary functions
and produces an environment or something that we can generate using a macro or something

*/

// impl usize

#[derive(Clone, Debug)]
pub struct MyStruct {
    pub field: usize,
    pub stays_the_same: usize,
    pub name: String,
}

implement!(usize);
implement!(MyStruct, field, usize, name, String);

pub fn repl() -> std::io::Result<()> {
    let mut interpreter = interpreter::RucketInterpreter::new();

    if let Err(e) = interpreter.require(PRELUDE) {
        eprintln!("Error loading prelude: {}", e)
    }
    println!("Welcome to Rucket 1.0");

    println!("Attempting to insert my own type");

    let testytest = MyStruct {
        field: 69,
        stays_the_same: 0,
        name: "matthew paras".to_string(),
    };
    // let testytest: usize = 420;
    let my_val = testytest.new_rucket_val();

    interpreter.insert_binding("test", my_val.clone());

    interpreter.insert_bindings(MyStruct::generate_bindings());

    // interpreter

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
                println!(
                    "Looking up value and printing: {:?}",
                    unwrap!(interpreter.extract_value("new-test").unwrap(), MyStruct).unwrap(),
                );
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
