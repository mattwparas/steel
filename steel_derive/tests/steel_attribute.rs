extern crate steel;
#[macro_use]
extern crate steel_derive;

use crate::steel::*;
use crate::steel_derive::steel;

// use steel::SteelInterpreter;

use steel::unwrap;

use std::any::Any;
use steel::rerrs;
use steel::rvals::{self, CustomType, SteelVal};

// use steel::build_interpreter;
use steel_derive::function;

// use std::process;
// use std::rc::Rc;

// use steel::gc::Gc;
use steel::Gc;

use steel::SteelErr;

// use steel::PRELUDE;

use std::convert::TryFrom;

use std::fmt::Write;
use std::sync::{Arc, Mutex};

#[steel]
pub struct MyStruct {
    pub field: usize,
    pub stays_the_same: usize,
    pub name: String,
}

#[steel]
pub struct VecStruct {
    pub field: Vec<CoolTest>,
}

#[steel]
#[derive(PartialEq)]
pub struct CoolTest {
    pub val: f64,
}

impl CoolTest {
    pub fn thing(&self) {
        println!("Inside a method of CoolTest!");
    }
}

#[steel]
#[derive(PartialEq)]
pub struct UnnamedFields(pub usize);

#[steel]
#[derive(PartialEq)]
pub struct Foo {
    pub f: UnnamedFields,
}

// By design, tuple structs with unnamed fields are not given constructors or accessors
// constructors or accessors must be defined outside the macro
#[steel]
pub struct MutexWrapper(pub Arc<Mutex<usize>>);

#[function]
pub fn new_mutex_wrapper(val: usize) -> MutexWrapper {
    MutexWrapper(Arc::new(Mutex::new(val)))
}

#[function]
pub fn add_cool_tests(arg1: CoolTest, arg2: CoolTest) -> CoolTest {
    let res = CoolTest {
        val: arg1.val + arg2.val,
    };

    res
}

#[function]
pub fn multiple_types(val: u64) -> u64 {
    val + 25
}

#[function]
pub fn test_option(input: usize) -> Option<usize> {
    if input == 1 {
        Some(1)
    } else {
        None
    }
}

#[function]
pub fn test_result(input: usize) -> std::result::Result<usize, String> {
    if input == 1 {
        Ok(1)
    } else {
        Err("We got an error".to_string())
    }
}

/*

// TODO come back and flesh this out
#[test]
fn embed_functions_and_verify_results() {
    let mut interp = build_interpreter! {
        Structs => {
            MyStruct,
            CoolTest,
            Foo,
            MutexWrapper,
            VecStruct
        }
        Functions => {
            "add-cool-tests" => add_cool_tests,
            "multiple-types" => multiple_types,
            "new-mutex-wrapper" => new_mutex_wrapper,
            "test-result" => test_result,
            "test-option" => test_option,
        }
    };

    let script = "
    (define result-res-good (test-result 1))
    (define result-res-false (test-result 2))
    ";

    assert!(interp.evaluate(&script).is_err());

    let script = "
    (define option-res-good (test-option 1))
    (define option-res-bad (test-option 2))
    ";
    assert!(interp.evaluate(&script).is_ok());

    // let bad_val: bool = bool::try_from(interp.extract_value("option-res-bad").unwrap()).unwrap();
}

#[test]
pub fn build_interpreter_and_modify() {
    // Construct interpreter with 3 custom structs
    // each has now getters, setters, a predicate and constructor
    let mut interpreter = build_interpreter! {
        MyStruct,
        CoolTest,
        Foo
    };

    // interpreter.require(PRELUDE).unwrap();

    // define value outside of interpreter to embed
    let test = UnnamedFields(100);
    // embed the value
    interpreter.insert_binding("unnamed", test.new_steel_val());
    interpreter.insert_binding("add_cool_tests", SteelVal::FuncV(add_cool_tests));
    interpreter.insert_binding("multiple_types", SteelVal::FuncV(multiple_types));

    // write a quick script
    let script = "
        (define cool-test (CoolTest 100.0))
        (define cool-test2 (CoolTest 200.0))
        (define return-val (set-CoolTest-val! cool-test 200.0))
        (define foo-test (Foo unnamed))
        (define sum-test (add_cool_tests cool-test cool-test2))
        (displayln (multiple_types 25))
    ";

    // get the values back out
    match interpreter.evaluate(&script) {
        Ok(_) => {
            let ret_val: CoolTest =
                CoolTest::try_from(interpreter.extract_value("return-val").unwrap()).unwrap();
            assert_eq!(ret_val, CoolTest { val: 200.0 });
            let ret_val2 =
                UnnamedFields::try_from(interpreter.extract_value("unnamed").unwrap()).unwrap();
            assert_eq!(ret_val2, UnnamedFields(100));
            let ret_val3 = Foo::try_from(interpreter.extract_value("foo-test").unwrap()).unwrap();
            assert_eq!(
                ret_val3,
                Foo {
                    f: UnnamedFields(100)
                }
            );
            let ret_val4 =
                CoolTest::try_from(interpreter.extract_value("sum-test").unwrap()).unwrap();
            assert_eq!(ret_val4, CoolTest { val: 300.0 })
        }
        Err(_e) => {
            panic!("steel macro test failed");
            // eprintln!("{}", e);
        }
    }
}
*/
