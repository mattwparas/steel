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
use steel::build_vm;
use steel::repl::repl_base;
use steel::vm::VirtualMachine;
use steel_derive::function;
use steel_derive::steel;

use steel::Gc;

use std::process;

// use std::rc::Rc;

use steel::SteelErr;

use std::env::args;
use std::fs;
use steel::PRELUDE;

use std::convert::TryFrom;

use std::fmt::Write;
use std::sync::{Arc, Mutex};
// extern crate reqwest;

// use std::io::Read;

// use std::time::Instant;
// use std::thread::sleep;

// use steel::build_interpreter_2;

//"http://httpbin.org/get"
// #[function]
// fn get_request(url: String) -> reqwest::Result<String> {
//     let mut res = reqwest::blocking::get(&url)?;
//     let mut body = String::new();
//     if let Err(_) = res.read_to_string(&mut body) {};

//     println!("Status: {}", res.status());
//     println!("Headers:\n{:#?}", res.headers());
//     println!("Body:\n{}", body);

//     Ok("Success!".to_string())
// }

// const fn foo() -> &'static str {
//     "hello world"
// }

// use once_cell::sync::Lazy; // 1.3.1

// static ARRAY: Lazy<Mutex<Vec<u8>>> = Lazy::new(|| Mutex::new(vec![]));

// fn do_a_call() {
//     ARRAY.lock().unwrap().push(1);
// }

use once_cell::sync::Lazy; // 1.3.1

static ARRAY: Lazy<Mutex<Vec<u8>>> = Lazy::new(|| Mutex::new(vec![]));

#[function]
fn do_a_call() {
    ARRAY.lock().unwrap().push(1);
    println!("{:?}", ARRAY.lock().unwrap());
}

fn main() {
    let args = args().collect::<Vec<_>>();

    if args.len() == 1 {
        // let mut interpreter = build_interpreter! {};
        // if let Err(e) = interpreter.require(PRELUDE) {
        //     eprintln!("Error loading prelude: {}", e)
        // }
        // let contents =
        //     fs::read_to_string("struct.rkt").expect("Something weont wrong reading the file");

        // let ast = interpreter.compile(&contents).unwrap();
        // // if let Err(e) = ast {
        // //     eprintln!("Error compiling the file: {}", e);
        // // }

        // let res = SteelInterpreter::evaluate_from_ast(&ast);

        // match res {
        //     Ok(r) => r.iter().for_each(|x| match x {
        //         SteelVal::Void => {}
        //         _ => println!("{}", x),
        //     }),
        //     Err(e) => eprintln!("{}", e.to_string()),
        // }

        // interpreter.parse_and

        finish(test_repl());
    } else if args.len() == 2 {
        let path = &args[1];
        let mut interpreter = build_interpreter! {};
        if let Err(e) = interpreter.require(PRELUDE) {
            eprintln!("Error loading prelude: {}", e)
        }

        let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
        // let now = Instant::now();
        let res = interpreter.evaluate(&contents);

        // println!("{:?}", now.elapsed());

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
pub struct VecStruct {
    pub field: Vec<CoolTest>,
}

#[steel]
#[derive(PartialEq)]
pub struct CoolTest {
    pub val: f64,
}

impl CoolTest {
    // #[method]
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
pub fn pretty_print_cool_test(arg: CoolTest) {
    println!("{:?}", arg);
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
pub fn panic_time() {
    panic!("What do I do?")
}

#[function]
pub fn test_result(input: usize) -> std::result::Result<usize, String> {
    if input == 1 {
        Ok(1)
    } else {
        Err("We got an error".to_string())
    }
}

#[function]
pub fn mutation_test(arg: CoolTest) -> CoolTest {
    let mut arg = arg;
    arg.val = 10000.0;
    arg
}

pub fn test_repl() -> std::io::Result<()> {
    repl_base(build_vm! {
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
            "display-cool-test" => pretty_print_cool_test,
            "test-result" => test_result,
            "test-option" => test_option,
            "panic-time" => panic_time,
            "do-a-call" => do_a_call,
        }
    })
}

pub fn my_repl() -> std::io::Result<()> {
    build_repl! {
        MyStruct,
        CoolTest,
        Foo
    }
}

pub fn test_result2(input: usize) -> std::result::Result<usize, String> {
    if input == 1 {
        Ok(1)
    } else {
        Err("We got an error".to_string())
    }
}

// pub fn test_test() {
//     build_interpreter_2! {
//         Structs => {}
//         Functions =>  {
//             "test-result" => test_result2
//         }
//     };
// }

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
            "do-a-call" => do_a_call,
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
fn build_interpreter_and_modify() {
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
        Err(e) => {
            panic!(e.to_string());
        }
    }
}
