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
    build_interpreter_and_modify();
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

#[steel]
pub struct UnnamedFields(pub usize);

#[steel]
pub struct Foo {
    pub f: UnnamedFields,
}

pub fn my_repl() -> std::io::Result<()> {
    build_repl! {
        MyStruct,
        CoolTest,
        Foo
    }
}

pub fn build_interpreter_and_modify() {
    // Construct interpreter with 3 custom structs
    // each has now getters, setters, a predicate and constructor
    let mut interpreter = build_interpreter! {
        MyStruct,
        CoolTest,
        Foo
    };

    // define value outside of interpreter to embed
    let test = UnnamedFields(100);
    // embed the value
    interpreter.insert_binding("unnamed", test.new_steel_val());

    // write a quick script
    let script = "
    (define cool-test (CoolTest 100))
    (define return-val (set-CoolTest-val! cool-test 200))
    (define foo-test (Foo unnamed))
    ";

    // get the values back out
    if let Ok(_) = interpreter.evaluate(script) {
        let ret_val = unwrap!(interpreter.extract_value("return-val").unwrap(), CoolTest).unwrap();
        println!("{:?}", ret_val); // Should be "CoolTest { val: 200.0 }"
        let ret_val2 =
            unwrap!(interpreter.extract_value("unnamed").unwrap(), UnnamedFields).unwrap();
        println!("{:?}", ret_val2); // Should be "UnnamedFields(100)"
        let ret_val3 = unwrap!(interpreter.extract_value("foo-test").unwrap(), Foo).unwrap();
        println!("{:?}", ret_val3); // Should be Foo { f: UnnamedFields(100) }
    };
}
