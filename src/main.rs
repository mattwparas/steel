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

use std::rc::Rc;

use steel::SteelErr;

use std::env::args;
use std::fs;
use steel::PRELUDE;

#[macro_export]
macro_rules! and {
    () => {};
    ($name:tt) => {
            format!("(if {} #t #f)", stringify!($name))
    };
    ($name:tt $($tail:tt)*) => {
            format!("(if {} {} #f)", stringify!($name), and!($($tail)*))
    };
}

/*
(and a b) => (if a (if b #t #f) #f)
*/

fn main() {
    // let expansion = and!("#t" "#t");
    // println!("{}", expansion);

    let args = args().collect::<Vec<_>>();

    if args.len() == 1 {
        // build_interpreter_and_modify();
        finish(my_repl());
    } else if args.len() == 2 {
        let path = &args[1];
        let mut interpreter = build_interpreter! {};
        if let Err(e) = interpreter.require(PRELUDE) {
            eprintln!("Error loading prelude: {}", e)
        }

        let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
        let res = interpreter.evaluate(&contents);

        if let Err(e) = res {
            eprintln!("{}", e);
        }
    }
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

impl CoolTest {
    pub fn thing(&self) {
        println!("Inside a method of CoolTest!");
    }
}

#[steel]
pub struct UnnamedFields(pub usize);

#[steel]
pub struct Foo {
    pub f: UnnamedFields,
}

pub fn add_cool_tests(args: Vec<Rc<SteelVal>>) -> Result<Rc<SteelVal>, SteelErr> {
    // println!("inside add_cool_tests");
    // println!("{:?}", (*(args[0])).clone());
    // println!("{:?}", (*(args[1])).clone());
    let arg1 = unwrap!((*(args[0])).clone(), CoolTest)?;
    let arg2 = unwrap!((*(args[1])).clone(), CoolTest)?;

    let res = CoolTest {
        val: arg1.val + arg2.val,
    };

    res.thing();

    Ok(Rc::new(res.new_steel_val()))

    // unimplemented!();
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

    interpreter.insert_binding("add_cool_tests", SteelVal::FuncV(add_cool_tests));

    // write a quick script
    let script = "
    (define cool-test (CoolTest 100))
    (define cool-test2 (CoolTest 200))
    (define return-val (set-CoolTest-val! cool-test 200))
    (define foo-test (Foo unnamed))
    (define sum-test (add_cool_tests cool-test cool-test2))
    ";

    // get the values back out
    if interpreter.evaluate(script).is_ok() {
        let ret_val = unwrap!(interpreter.extract_value("return-val").unwrap(), CoolTest).unwrap();
        println!("{:?}", ret_val); // Should be "CoolTest { val: 200.0 }"
        let ret_val2 =
            unwrap!(interpreter.extract_value("unnamed").unwrap(), UnnamedFields).unwrap();
        println!("{:?}", ret_val2); // Should be "UnnamedFields(100)"
        let ret_val3 = unwrap!(interpreter.extract_value("foo-test").unwrap(), Foo).unwrap();
        println!("{:?}", ret_val3); // Should be Foo { f: UnnamedFields(100) }
        let ret_val4 = unwrap!(interpreter.extract_value("sum-test").unwrap(), CoolTest).unwrap();
        println!("{:?}", ret_val4);
    };
}
