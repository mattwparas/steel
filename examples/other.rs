extern crate steel;
#[macro_use]
extern crate steel_derive;
extern crate steel_repl;

// use steel::SteelInterpreter;

use steel::unwrap;

use std::any::Any;
use steel::rerrs::{ErrorKind, SteelErr};
use steel::rvals::{self, CustomType, SteelVal, StructFunctions};

// use steel::build_interpreter;
// use steel_derive::function;
// use steel_derive::steel;
use steel_repl::build_repl;
use steel_repl::repl::repl_base;
use steel_vm::build_engine;
use steel_vm::engine::Engine;

use steel::Gc;

use std::process;

// use std::rc::Rc;

// use steel::SteelErr;

use std::env::args;
use std::fs;
// use steel::PRELUDE;

use std::convert::TryFrom;

use std::fmt::Write;
use std::sync::{Arc, Mutex};

use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

use std::cmp::{max, min};

use steel_vm::register_fn::RegisterFn;
// use steel_vm::engine::RegisterNoArgFn;

use env_logger::Builder;
use log::LevelFilter;

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

// #[function]
fn do_a_call() {
    ARRAY.lock().unwrap().push(1);
    println!("{:?}", ARRAY.lock().unwrap());
}

fn do_a_call_two() {
    ARRAY.lock().unwrap().push(1);
    println!("{:?}", ARRAY.lock().unwrap());
}

fn test_test(input: usize) -> Option<usize> {
    if input == 69 {
        None
    } else {
        Some(10)
    }
}

fn test_result_error(input: usize) -> std::result::Result<usize, String> {
    if input == 10 {
        Ok(10)
    } else {
        Err("can't be doin that now".to_string())
    }
}

fn test_two_args(arg1: usize, arg2: usize) -> usize {
    arg1 + arg2
}

fn no_args_return_empty() {
    println!("hello world!");
}

fn accept_vector(vector: Vec<usize>) -> Option<usize> {
    if vector.len() == 2 {
        None
    } else {
        Some(vector.len())
    }
}

#[derive(Clone, Debug, Steel)]
struct WrapperStruct(usize);

impl WrapperStruct {
    fn method_by_value(self) -> usize {
        self.0
    }

    fn set_num(mut self, number: usize) -> Self {
        self.0 = number;
        self
    }
}

fn takes_wrapper(arg1: WrapperStruct) -> usize {
    arg1.0
}

#[derive(Clone, Debug, Steel)]
struct NormalStruct {
    val: usize,
    test: f64,
    thing: bool,
}

fn main() {
    // env_logger::init();

    let mut builder = Builder::new();

    // builder.filter(Some("steel_vm"), LevelFilter::Trace).init();

    let args = args().collect::<Vec<_>>();

    if args.len() == 1 {
        finish(test_repl());
    } else if args.len() == 2 {
        let path = &args[1];

        let mut vm = build_engine! {};

        vm.register_fn("test-test", test_test)
            .register_fn("blagh", test_two_args)
            .register_fn("no-args", no_args_return_empty)
            .register_fn("do-a-call-2", do_a_call_two)
            .register_fn("takes-wrapper", takes_wrapper)
            .register_type::<WrapperStruct>("WrapperStruct?")
            .register_fn("Wrapper", WrapperStruct)
            .register_fn("self-by-ref", WrapperStruct::method_by_value)
            .register_fn("set-Wrapper-num!", WrapperStruct::set_num)
            .register_fn("accept-vector", accept_vector)
            .register_type::<NormalStruct>("normal-struct?");

        let core_libraries = &[steel::stdlib::PRELUDE, steel::stdlib::CONTRACTS];

        for core in core_libraries {
            let res = vm.parse_and_execute_without_optimizations(core);
            if let Err(e) = res {
                eprintln!("{}", e);
                return;
            }
        }

        let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
        // let now = Instant::now();
        let res = vm.parse_and_execute_without_optimizations(&contents);

        // println!("{:?}", now.elapsed());

        if let Err(e) = res {
            e.emit_result(path, &contents);
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

// #[steel]
pub struct MyStruct {
    pub field: usize,
    pub stays_the_same: usize,
    pub name: String,
}

// #[steel]
pub struct VecStruct {
    pub field: Vec<CoolTest>,
}

// #[steel]
#[derive(PartialEq)]
pub struct CoolTest {
    pub val: f64,
}

impl CoolTest {
    // #[method]
    pub fn thing(&self) {
        println!("Inside a method of CoolTest!");
    }

    // #[function]
    pub fn blargh() {
        println!("do some stuff");
    }
}

// #[steel]
#[derive(PartialEq)]
pub struct UnnamedFields(pub usize);

// #[steel]
#[derive(PartialEq)]
pub struct Foo {
    pub f: UnnamedFields,
}

// #[steel]
pub struct Mutation(pub Rc<RefCell<usize>>);

// #[function]
pub fn new_mutation() -> Mutation {
    Mutation(Rc::new(RefCell::new(0)))
}

// #[function]
pub fn mutation_inner(value: Mutation) {
    *value.0.borrow_mut() += 1;
}

// By design, tuple structs with unnamed fields are not given constructors or accessors
// constructors or accessors must be defined outside the macro
// #[steel]
pub struct MutexWrapper(pub Arc<Mutex<usize>>);

// #[function]
pub fn new_mutex_wrapper(val: usize) -> MutexWrapper {
    MutexWrapper(Arc::new(Mutex::new(val)))
}

// #[function]
pub fn add_cool_tests(arg1: CoolTest, arg2: CoolTest) -> CoolTest {
    let res = CoolTest {
        val: arg1.val + arg2.val,
    };

    res
}

// #[function]
// pub fn pretty_print_cool_test(arg: CoolTest) {
//     println!("{:?}", arg);
// }

// #[function]
pub fn multiple_types(val: u64) -> u64 {
    val + 25
}

// #[function]
pub fn test_option(input: usize) -> Option<usize> {
    if input == 1 {
        Some(1)
    } else {
        None
    }
}

// #[function]
pub fn panic_time() {
    panic!("What do I do?")
}

// #[function]
pub fn test_result(input: usize) -> std::result::Result<usize, String> {
    if input == 1 {
        Ok(1)
    } else {
        Err("We got an error".to_string())
    }
}

// #[function]
pub fn mutation_test(arg: CoolTest) -> CoolTest {
    let mut arg = arg;
    arg.val = 10000.0;
    arg
}

// #[steel]
// pub struct Cache(Rc<RefCell<HashMap<Gc<SteelVal>, Gc<SteelVal>>>>);

// #[function]
// pub fn new_cache() -> Cache {
//     Cache(Rc::new(RefCell::new(HashMap::new())))
// }

// #[function]
// pub fn cache_insert(cache: Cache, key: Gc<SteelVal>, value: Gc<SteelVal>) {
//     cache.0.borrow_mut().insert(key, value);
// }

// #[function]
// pub fn cache_lookup(cache: Cache, key: Gc<SteelVal>) -> Option<Gc<SteelVal>> {
//     cache.0.borrow().get(&key).map(|x| Gc::clone(x))
// }

pub fn test_repl() -> std::io::Result<()> {
    let mut vm = build_engine! {};

    vm.register_fn("test-test", test_test)
        .register_fn("blagh", test_two_args)
        .register_fn("no-args", no_args_return_empty)
        .register_fn("do-a-call-2", do_a_call_two)
        .register_fn("takes-wrapper", takes_wrapper)
        .register_type::<WrapperStruct>("WrapperStruct?")
        .register_fn("Wrapper", WrapperStruct)
        .register_fn("self-by-ref", WrapperStruct::method_by_value)
        .register_fn("set-Wrapper-num!", WrapperStruct::set_num)
        .register_fn("accept-vector", accept_vector);

    repl_base(vm)
}

pub fn test_repl_with_progress() -> std::io::Result<()> {
    let mut vm = build_engine! {};

    vm.on_progress(|count| {
        // parameter is 'u64' - number of operations already performed
        if count % 1000 == 0 {
            println!("Number of instructions up to this point: {}", count); // print out a progress log every 1,000 operations
            return false;
        }
        true
    });

    repl_base(vm)
}

pub fn my_repl() -> std::io::Result<()> {
    build_repl! {}
}

pub fn test_result2(input: usize) -> std::result::Result<usize, String> {
    if input == 1 {
        Ok(1)
    } else {
        Err("We got an error".to_string())
    }
}

// TODO come back and flesh this out
#[test]
fn embed_functions_and_verify_results() {
    // let mut interp = build_engine! {
    //     Structs => {
    //         MyStruct,
    //         CoolTest,
    //         Foo,
    //         MutexWrapper,
    //         VecStruct
    //     }
    //     Functions => {
    //         "add-cool-tests" => add_cool_tests,
    //         "multiple-types" => multiple_types,
    //         "new-mutex-wrapper" => new_mutex_wrapper,
    //         "test-result" => test_result,
    //         "test-option" => test_option,
    //         "do-a-call" => do_a_call,
    //     }
    // };

    // let script = "
    // (define result-res-good (test-result 1))
    // (define result-res-false (test-result 2))
    // ";

    // assert!(interp.parse_and_execute(&script).is_err());

    // let script = "
    // (define option-res-good (test-option 1))
    // (define option-res-bad (test-option 2))
    // ";
    // assert!(interp.parse_and_execute(&script).is_ok());
}

#[test]
fn build_interpreter_and_modify() {
    // // Construct interpreter with 3 custom structs
    // // each has now getters, setters, a predicate and constructor
    // let mut interpreter = build_engine! {
    //     MyStruct,
    //     CoolTest,
    //     Foo
    // };

    // // define value outside of interpreter to embed
    // let test = UnnamedFields(100);
    // // embed the value
    // interpreter.register_value("unnamed", test.new_steel_val());
    // interpreter.register_value("add_cool_tests", SteelVal::FuncV(add_cool_tests));
    // interpreter.register_value("multiple_types", SteelVal::FuncV(multiple_types));

    // // write a quick script
    // let script = "
    //     (define cool-test (CoolTest 100.0))
    //     (define cool-test2 (CoolTest 200.0))
    //     (define return-val (set-CoolTest-val! cool-test 200.0))
    //     (define foo-test (Foo unnamed))
    //     (define sum-test (add_cool_tests cool-test cool-test2))
    //     (define mt (multiple_types 25))
    // ";

    // // get the values back out
    // match interpreter.parse_and_execute_without_optimizations(&script) {
    //     Ok(_) => {
    //         let ret_val: CoolTest =
    //             CoolTest::try_from(interpreter.extract_value("return-val").unwrap()).unwrap();
    //         assert_eq!(ret_val, CoolTest { val: 200.0 });
    //         let ret_val2 =
    //             UnnamedFields::try_from(interpreter.extract_value("unnamed").unwrap()).unwrap();
    //         assert_eq!(ret_val2, UnnamedFields(100));
    //         let ret_val3 = Foo::try_from(interpreter.extract_value("foo-test").unwrap()).unwrap();
    //         assert_eq!(
    //             ret_val3,
    //             Foo {
    //                 f: UnnamedFields(100)
    //             }
    //         );
    //         let ret_val4 =
    //             CoolTest::try_from(interpreter.extract_value("sum-test").unwrap()).unwrap();
    //         assert_eq!(ret_val4, CoolTest { val: 300.0 })
    //     }
    //     Err(e) => {
    //         panic!(e.to_string());
    //     }
    // }
}
