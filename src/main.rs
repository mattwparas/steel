extern crate steel;
#[macro_use]
extern crate steel_derive;

use steel::SteelInterpreter;

use steel::unwrap;

use std::any::Any;
use steel::rerrs;
use steel::rvals::{self, CustomType, SteelVal, StructFunctions};

use steel::build_interpreter;
use steel::build_repl;
use steel::repl::repl_base;
// use steel::PRELUDE;
use steel_derive::steel;

use std::process;

fn main() {
    // build_interpreter_and_modify();
    finish(my_repl());
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

#[steel]
pub struct MyStruct {
    pub field: usize,
    pub stays_the_same: usize,
    pub name: String,
}

#[steel]
pub struct CoolTest {
    pub val: f64,
}

pub fn my_repl() -> std::io::Result<()> {
    build_repl! {
        MyStruct,
        CoolTest
    }
}

pub fn build_interpreter_and_modify() {
    let mut interpreter = build_interpreter! {
        MyStruct,
        CoolTest
    };

    let script = "
    (define cool-test (CoolTest 100))
    (define return-val (set-CoolTest-val! cool-test 200))
    ";

    if let Ok(_) = interpreter.evaluate(script) {
        let ret_val = unwrap!(interpreter.extract_value("return-val").unwrap(), CoolTest).unwrap();
        println!("{:?}", ret_val);
    };
}
