extern crate steel;
extern crate steel_derive;
// extern crate steel_repl;

use steel::steel_vm::{engine::Engine, register_fn::RegisterAsyncFn};
// use steel_repl::repl::repl_base;

use std::env::args;
use std::fs;
use std::process;

use env_logger::Builder;
use log::LevelFilter;

use std::time::Instant;

// use steel::jit::

use core::mem;
// use std::rc::Rc;
use steel::jit::code_gen::JIT;

use steel::parser::ast::ExprKind;

const RECURSIVE_FIB_CODE: &str = r#"
    (define (fib n) 
        (if (<= n 2) 
            1
            (+ (fib (- n 1)) (fib (- n 2)))))
"#;

const LET_CODE: &str = r#"
    (define (test x y z)
        ;; (let ((x x) (y y) (z z))
            (+ x y z))
"#;

fn run_fib<I>(jit: &mut JIT, code: &ExprKind, input: I) -> Result<isize, String> {
    unsafe { run_code(jit, code, input) }
}

/// Executes the given code using the cranelift JIT compiler.
///
/// Feeds the given input into the JIT compiled function and returns the resulting output.
///
/// # Safety
///
/// This function is unsafe since it relies on the caller to provide it with the correct
/// input and output types. Using incorrect types at this point may corrupt the program's state.
unsafe fn run_code<I, O>(jit: &mut JIT, code: &ExprKind, input: I) -> Result<O, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile(code)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = mem::transmute::<_, fn(I) -> O>(code_ptr);
    // And now we can call it!
    Ok(code_fn(input))
}

fn main() -> Result<(), String> {
    // env_logger::init();

    let mut builder = Builder::new();

    builder
        .filter(Some("pipeline_time"), LevelFilter::Trace)
        .init();

    // let args = args().collect::<Vec<_>>();

    let mut vm = configure_engine();
    let mut jit = JIT::default();
    let res = vm.emit_expanded_ast(LET_CODE);

    match res {
        Ok(func) => {
            let ast = &func[0];
            println!("{}", ast.to_pretty(60));
            let now = Instant::now();
            println!(
                "(test 10 20 30) = {}",
                run_fib(&mut jit, ast, (10, 20, 30))?
            );
            println!("Time taken: {:?}", now.elapsed());
        }
        Err(e) => {
            e.emit_result("repl.rkt", LET_CODE);
        }
    }

    Ok(())
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

async fn test_async_function() -> usize {
    10
}

pub fn configure_engine() -> Engine {
    let mut vm = Engine::new_base();
    vm.register_async_fn("test", test_async_function);
    vm
}
