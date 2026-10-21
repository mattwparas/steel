use alloc::sync::Arc;
use core::marker::PhantomData;
use steel_gen::OpCode;

use crate::compiler::code_gen::fresh_function_id;
use crate::containers::RegisterValue;
use crate::core::instructions::{u24, DenseInstruction};
use crate::gc::Gc;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{as_underlying_type, Custom, FromSteelVal, IntoSteelVal, Result, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::engine::Engine;
use crate::steel_vm::register_fn::host_signature;
use crate::steel_vm::vm::{InstructionPointer, VmCore};
use crate::stop;
use crate::values::functions::{ByteCodeLambda, CaptureVec, HostSignature};

/// What a function registered with
/// [`RegisterRuntimeFn::register_fn_with_runtime`] gets first, for calling
/// back into Steel while it runs.
///
/// It only lives for the call. Don't hold a lock or a borrow of a Steel
/// value, such as [`crate::rvals::SteelByteVector::bytes`], across a call
/// back into Steel, since the code it runs can reach the same value.
pub struct Runtime<'a, 'b> {
    vm: &'a mut VmCore<'b>,
}

impl Runtime<'_, '_> {
    /// Calls `function` with `args` and returns what it returns.
    ///
    /// An error it raises comes back as `Err`, and so does a continuation it
    /// calls to leave through this function. Return that error rather than
    /// carrying on, so the continuation gets where it is going.
    ///
    /// Anything Steel code can call works: closures, native and host
    /// functions, builtins such as `apply`, continuations, and structs that
    /// can be called.
    pub fn call_function(
        &mut self,
        function: &SteelVal,
        args: impl IntoIterator<Item = SteelVal>,
    ) -> Result<SteelVal> {
        let result = call(self.vm, function, args.into_iter().collect())?;
        // Kept where a collection sees it until the host function returns,
        // since the host may hold it across the next call
        self.vm.thread.stack.push(result.clone());
        Ok(result)
    }
}

fn call(vm: &mut VmCore, function: &SteelVal, mut args: Vec<SteelVal>) -> Result<SteelVal> {
    match function {
        SteelVal::Closure(closure) => vm.call_with_args(closure, args),
        SteelVal::FuncV(function) => function(&args),
        SteelVal::MutFunc(function) => function(&mut args),
        SteelVal::BoxedFunction(function) => function.func()(&args),
        SteelVal::CustomStruct(s) => match s.maybe_proc() {
            Some(SteelVal::HeapAllocated(procedure)) => call(vm, &procedure.get(), args),
            Some(procedure) => {
                let procedure = procedure.clone();
                call(vm, &procedure, args)
            }
            None => stop!(TypeMismatch => "call-function: not a procedure: {}", function),
        },
        SteelVal::MutableStruct(s) => match s.maybe_proc() {
            Some(procedure) => call(vm, &procedure, args),
            None => stop!(TypeMismatch => "call-function: not a procedure: {}", function),
        },
        // These can leave a frame for the vm to run instead of returning, or
        // leave the run altogether, so they go through `apply` in a closure,
        // which the vm runs like any other.
        SteelVal::BuiltIn(_) | SteelVal::ContinuationFunction(_) => {
            let applier = APPLIER.with(Gc::clone);
            vm.call_with_args(&applier, [function.clone(), SteelVal::ListV(args.into())])
        }
        _ => stop!(TypeMismatch => "call-function: not a procedure: {}", function),
    }
}

thread_local! {
    // `(lambda (f args) (apply f args))`, with `apply` captured
    static APPLIER: Gc<ByteCodeLambda> = {
        let instruction = |op_code, payload: usize| DenseInstruction {
            op_code,
            payload_size: u24::from_usize(payload),
        };
        let mut captures = CaptureVec::new();
        captures.push(SteelVal::BuiltIn(crate::steel_vm::vm::apply));
        Gc::new(ByteCodeLambda::new(
            fresh_function_id() as _,
            InstructionPointer::from(vec![
                instruction(OpCode::MOVEREADLOCAL0, 0),
                instruction(OpCode::MOVEREADLOCAL1, 1),
                instruction(OpCode::READCAPTURED, 0),
                instruction(OpCode::TAILCALL, 2),
                instruction(OpCode::POPPURE, 0),
            ]),
            2,
            false,
            captures,
        ))
    };
}

type RuntimeFn = dyn Fn(&mut Runtime, &[SteelVal]) -> Result<SteelVal> + Send + Sync + 'static;

// The host function behind a wrapper made by `wrap`
struct RuntimeFunction {
    function: Arc<RuntimeFn>,
}

impl Custom for RuntimeFunction {}

// Calls the host function in the first argument with the rest. A builtin, so
// it gets its own copy of the arguments, and can run Steel code.
fn call_with_runtime(vm: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let function = match args.first() {
        Some(SteelVal::Custom(value)) => {
            as_underlying_type::<RuntimeFunction>(value.read().as_ref())
                .map(|f| Arc::clone(&f.function))
        }
        _ => None,
    };
    let Some(function) = function else {
        return Some(Err(SteelErr::new(
            ErrorKind::Generic,
            "expected a host function that takes the runtime".to_string(),
        )));
    };
    // What `call_function` returns sits on the stack until the function
    // returns, where a collection sees it. The arguments are a builtin's,
    // which collections see already.
    let held = vm.thread.stack.len();
    let result = function(&mut Runtime { vm }, &args[1..]);
    vm.thread.stack.truncate(held);
    Some(result)
}

/// A Steel function of `arity` arguments that calls `function` with them,
/// declaring `signature`.
///
/// `function` needs the vm, which a boxed host function can't be given, and a
/// builtin that can be given it is a plain function pointer that holds nothing.
/// So this is a small closure that pushes `function`, then its arguments, and
/// tail calls a builtin that unpacks it. Both are captures rather than
/// globals, so the closure doesn't belong to any one engine, and a module that
/// holds it can be loaded into several.
fn wrap(arity: usize, function: Arc<RuntimeFn>, signature: HostSignature) -> SteelVal {
    let instruction = |op_code, payload: usize| DenseInstruction {
        op_code,
        payload_size: u24::from_usize(payload),
    };
    let mut instrs = Vec::with_capacity(arity + 4);
    instrs.push(instruction(OpCode::READCAPTURED, 0));
    for i in 0..arity {
        let op = match i {
            0 => OpCode::MOVEREADLOCAL0,
            1 => OpCode::MOVEREADLOCAL1,
            2 => OpCode::MOVEREADLOCAL2,
            3 => OpCode::MOVEREADLOCAL3,
            _ => OpCode::MOVEREADLOCAL,
        };
        instrs.push(instruction(op, i));
    }
    instrs.push(instruction(OpCode::READCAPTURED, 1));
    instrs.push(instruction(OpCode::TAILCALL, arity + 1));
    instrs.push(instruction(OpCode::POPPURE, 0));

    let mut captures = CaptureVec::new();
    captures.push(RuntimeFunction { function }.into_steelval().unwrap());
    captures.push(SteelVal::BuiltIn(call_with_runtime));

    let lambda = ByteCodeLambda::new(
        fresh_function_id() as _,
        InstructionPointer::from(instrs),
        arity,
        false,
        captures,
    );
    super::host_signature::note_wrapper(lambda.id, signature);

    SteelVal::Closure(Gc::new(lambda))
}

/// Registers host functions that can call back into Steel. Each takes a
/// [`Runtime`] before its arguments.
///
/// ```
/// # use steel::steel_vm::engine::Engine;
/// # use steel::steel_vm::runtime_fn::{RegisterRuntimeFn, Runtime};
/// # use steel::rvals::{Result, SteelVal};
/// let mut engine = Engine::new();
/// engine.register_fn_with_runtime(
///     "twice",
///     |runtime: &mut Runtime, f: SteelVal, x: SteelVal| -> Result<SteelVal> {
///         let once = runtime.call_function(&f, [x])?;
///         runtime.call_function(&f, [once])
///     },
/// );
/// let result = engine.run("(twice (lambda (x) (* x 3)) 2)").unwrap();
/// assert_eq!(result.last(), Some(&SteelVal::IntV(18)));
/// ```
pub trait RegisterRuntimeFn<FN, ARGS, RET> {
    fn register_fn_with_runtime(&mut self, name: &'static str, func: FN) -> &mut Self;
}

pub struct RuntimeArgs<ARGS>(PhantomData<ARGS>);

// The host function behind a wrapper for `func`, converting its arguments
macro_rules! runtime_function {
    ($name:expr, $func:expr, $($param:ident: $idx:expr),*) => {{
        let name = $name;
        let func = $func;
        move |_runtime: &mut Runtime, _args: &[SteelVal]| -> Result<SteelVal> {
            func(
                _runtime,
                $(<$param>::from_steelval(&_args[$idx]).map_err(|mut err| {
                    err.prepend_message(":");
                    err.prepend_message(name);
                    err
                })?,)*
            )
            .into_steelval()
        }
    }};
}

macro_rules! impl_register_runtime_fn {
    ($arg_count:expr => $($param:ident: $idx:expr),*) => {
        impl<
            $($param: FromSteelVal,)*
            FN: Fn(&mut Runtime, $($param),*) -> RET + Send + Sync + 'static,
            RET: IntoSteelVal
        > RegisterRuntimeFn<FN, RuntimeArgs<($($param,)*)>, RET> for Engine {
            fn register_fn_with_runtime(&mut self, name: &'static str, func: FN) -> &mut Self {
                let function = runtime_function!(name, func, $($param: $idx),*);
                let signature = host_signature(
                    const { &[$(<$param as FromSteelVal>::ARG_TYPE),*] },
                    <RET as IntoSteelVal>::RETURN_TYPE,
                );
                self.register_value(name, wrap($arg_count, Arc::new(function), signature))
            }
        }

        impl<
            $($param: FromSteelVal,)*
            FN: Fn(&mut Runtime, $($param),*) -> RET + Send + Sync + 'static,
            RET: IntoSteelVal
        > RegisterRuntimeFn<FN, RuntimeArgs<($($param,)*)>, RET> for BuiltInModule {
            fn register_fn_with_runtime(&mut self, name: &'static str, func: FN) -> &mut Self {
                let function = runtime_function!(name, func, $($param: $idx),*);
                let signature = host_signature(
                    const { &[$(<$param as FromSteelVal>::ARG_TYPE),*] },
                    <RET as IntoSteelVal>::RETURN_TYPE,
                );
                self.register_value(name, wrap($arg_count, Arc::new(function), signature))
            }
        }
    };
}

impl_register_runtime_fn!(0 =>);
impl_register_runtime_fn!(1 => A: 0);
impl_register_runtime_fn!(2 => A: 0, B: 1);
impl_register_runtime_fn!(3 => A: 0, B: 1, C: 2);
impl_register_runtime_fn!(4 => A: 0, B: 1, C: 2, D: 3);
impl_register_runtime_fn!(5 => A: 0, B: 1, C: 2, D: 3, E: 4);
impl_register_runtime_fn!(6 => A: 0, B: 1, C: 2, D: 3, E: 4, F: 5);
impl_register_runtime_fn!(7 => A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6);
impl_register_runtime_fn!(8 => A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7);
