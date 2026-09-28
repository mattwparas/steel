#![allow(improper_ctypes_definitions)]

use crate::values::functions::BoxedDynFunction;

use crate::values::structs::StructRef;

use core::{hint::unreachable_unchecked, mem::ManuallyDrop, sync::atomic::AtomicUsize};

use steel_derive::cross_platform_fn;

use super::VmCore;

use crate::{
    gc::Gc,
    primitives::{
        lists::{cdr_no_check_two, cons, list_ref, steel_list_contains, steel_memq},
        numbers::add_two,
        vectors::{mut_vec_push, mut_vec_set, steel_mut_vec_set},
    },
    rvals::Result,
    steel_vm::primitives::{gt_primitive, gte_primitive, listp, lt_primitive},
    values::structs::StructTypeDescriptor,
    SteelVal,
};

use super::*;

#[inline(always)]
fn should_trampoline(ctx: &mut VmCore) -> bool {
    ctx.thread.stack_frames.len() < 100
}

// Debug knobs for narrowing a jit miscompilation down to a single function.
// STEEL_JIT_LOG logs every candidate, STEEL_JIT_MAX only compiles the first n of
// them, and STEEL_JIT_ONLY / STEEL_JIT_SKIP take a comma separated list of ids.
//
// Binary search on STEEL_JIT_MAX until it breaks, pull the id at that index out of
// the log, then confirm with STEEL_JIT_SKIP.
fn jit_should_compile(id: u32) -> bool {
    let config = crate::config::config();

    if let Some(only) = &config.jit_only {
        if !only.contains(&id) {
            return false;
        }
    }

    if config
        .jit_skip
        .as_ref()
        .is_some_and(|skip| skip.contains(&id))
    {
        return false;
    }

    static COUNT: AtomicUsize = AtomicUsize::new(0);
    let n = COUNT.fetch_add(1, core::sync::atomic::Ordering::Relaxed);

    if config.jit_log {
        eprintln!("jit: candidate #{n} -> id {id}");
    }

    match config.jit_max {
        Some(max) if n >= max => false,
        _ => true,
    }
}

/// The set of global indices the compiler proved are never `set!`, now or by
/// a later compile (`is_closed_global`).
///
/// Generated code can bake these globals' values in. Any other global can
/// later hold a different function, or a closure instance with different
/// captures, so code that bakes one checks it first.
fn closed_globals(ctx: &VmCore) -> std::collections::HashSet<usize> {
    ctx.thread.compiler.read().symbol_map.closed_globals.clone()
}

/// `jit_compile_lambda`, except that with background compilation on it only
/// prepares the lambda and returns the compile as a job. Submit the job with
/// `submit_background_job` once the lambda is in its final `Gc`.
pub(crate) fn jit_compile_lambda_deferred(
    ctx: &mut VmCore,
    func: ByteCodeLambda,
    self_slot: Option<&mut Gc<ByteCodeLambda>>,
    maybe_index: Option<usize>,
) -> (ByteCodeLambda, Option<BackgroundJob>) {
    let defer = jit_background_enabled();
    jit_compile_lambda_impl(ctx, func, self_slot, maybe_index, defer)
}

fn jit_compile_lambda_impl(
    ctx: &mut VmCore,
    mut func: ByteCodeLambda,
    mut self_slot: Option<&mut Gc<ByteCodeLambda>>,
    maybe_index: Option<usize>,
    defer: bool,
) -> (ByteCodeLambda, Option<BackgroundJob>) {
    if func
        .body_exp
        .iter()
        .any(|x| matches!(x.op_code, OpCode::PUREFUNC))
    {
        return (func, None);
    }

    if !jit_should_compile(func.id) {
        return (func, None);
    }

    if crate::config::config().jit_log_names {
        let name = maybe_index.and_then(|x| {
            ctx.thread
                .compiler
                .read()
                .symbol_map
                .values()
                .get(x)
                .copied()
        });
        eprintln!(
            "jit-name: id {} arity {} -> {:?}",
            func.id,
            func.arity,
            name.map(|x| x.resolve().to_string())
        );
    }

    if ctx.thread.compiler.read().kernel.is_none() {
        return (func, None);
    }

    if crate::config::config()
        .jit_dump_bc
        .as_ref()
        .is_some_and(|ids| ids.contains(&func.id))
    {
        eprintln!("=== bytecode for id {} (arity {}) ===", func.id, func.arity);
        crate::core::instructions::pretty_print_dense_instructions(&func.body_exp);
        let guard = ctx.thread.compiler.read();
        let values = guard.symbol_map.values();
        for (i, inst) in func.body_exp.iter().enumerate() {
            if matches!(
                inst.op_code,
                OpCode::CALLGLOBAL
                    | OpCode::CALLGLOBALTAIL
                    | OpCode::CALLGLOBALNOARITY
                    | OpCode::CALLGLOBALTAILNOARITY
                    | OpCode::PUSH
            ) {
                eprintln!(
                    "  {i}: {:?} -> {:?}",
                    inst.op_code,
                    values
                        .get(inst.payload_size.to_usize())
                        .map(|x| x.resolve().to_string())
                );
            }
        }
    }

    let name = func.id.to_string();

    let instructions = func.body_exp.iter().copied().collect::<Vec<_>>();

    // Save the entry instruction in the event we end up serializing this
    // later.
    func.header = Some(instructions[0].op_code);
    func.body_exp = InstructionPointer::from(instructions);

    if let Some(self_slot) = self_slot.as_mut() {
        unsafe {
            steel_rc::BiasedRc::get_mut_unchecked(&mut self_slot.0).body_exp =
                func.body_exp.clone();
        }
    }

    let fn_ptr_name = maybe_index.and_then(|x| {
        ctx.thread
            .compiler
            .read()
            .symbol_map
            .values()
            .get(x)
            .copied()
    });

    let closed_globals = closed_globals(ctx);

    // For background compilation, capture everything the compile reads from the
    // VM now, when a synchronous compile would read it, so both make the same
    // decisions. The function's own global is still unbound here, which the self
    // call fast path relies on. Only the install happens later.
    if defer {
        let job = BackgroundJob {
            name,
            arity: func.arity,
            maybe_index,
            fn_ptr_name,
            globals: sparse_globals_snapshot(ctx, &func.body_exp, maybe_index),
            constants: ctx.thread.constant_map.clone(),
            closed_globals,
            jit: Arc::clone(&ctx.thread.jit),
            self_slot: self_slot.is_some(),
        };
        return (func, Some(job));
    }

    // The jit is shared by every engine in the process, so recover the lock if a
    // panic in cranelift poisoned it, otherwise one failed compile breaks every
    // engine.
    let fn_pointer = lock_jit_for_interpreter(&ctx.thread.jit).compile_bytecode(
        name,
        func.arity,
        &func.body_exp,
        &ctx.thread.global_env.roots(),
        &ctx.thread.constant_map,
        maybe_index,
        self_slot.map(|x| &*x),
        fn_ptr_name,
        &closed_globals,
    );

    let fn_pointer = if let Ok(fn_pointer) = fn_pointer {
        fn_pointer
    } else {
        return (func, None);
    };

    let super_instructions = Some(fn_pointer);
    func.set_super_instructions(super_instructions);

    unsafe {
        steel_rc::BiasedRc::get_mut_unchecked(&mut func.body_exp)[0].op_code =
            OpCode::DynSuperInstruction
    };

    (func, None)
}

// ---------------------------------------------------------------------------
// Background compilation (on by default; `STEEL_JIT_BACKGROUND=0` turns it off).
//
// A definition queues its compile on a single process wide worker thread (the
// jit is already process wide), and the interpreter keeps running the function
// until the compiled code is installed. This keeps compilation of the prelude
// off the main thread at startup. Turn it off when bisecting a miscompile
// (STEEL_JIT_MAX/ONLY/SKIP) or profiling compiled code, so which code runs
// native doesn't depend on how fast the worker is.

pub(crate) fn jit_background_enabled() -> bool {
    crate::config::config().jit_background
}

/// Everything a compile reads from the running VM, captured on the thread that
/// defined the function.
pub(crate) struct BackgroundJob {
    name: String,
    arity: u16,
    maybe_index: Option<usize>,
    fn_ptr_name: Option<steel_parser::interner::InternedString>,
    globals: Vec<SteelVal>,
    constants: crate::compiler::constants::ConstantMap,
    closed_globals: std::collections::HashSet<usize>,
    jit: Arc<std::sync::Mutex<crate::jit2::cgen::JIT>>,
    // Whether the lambda this compiles into is also the target of its self
    // calls (a pure function's slot). False for a closure prototype.
    self_slot: bool,
}

struct QueuedJob {
    job: BackgroundJob,
    lambda: Gc<ByteCodeLambda>,
    // Where the job goes once compiled, so the thread that queued it drops it.
    // Its `Gc`s (the lambda, the closures in `globals`, the constant map) were
    // cloned on that thread. Under biased reference counting, a decrement from
    // another thread queues the object for a merge, after which every clone and
    // drop of it is atomic. That would slow down calls to the lambda itself.
    returns: crossbeam_channel::Sender<QueuedJob>,
    // Reference counts the compiled code owes values it baked in (`jit_leak`).
    // Paid by the queueing thread for the same reason, before `job` is dropped,
    // since `job` keeps those values alive until then.
    leaks: Vec<crate::jit2::cgen::DeferredLeak>,
}

impl QueuedJob {
    fn pay_leaks(&mut self) {
        for leak in self.leaks.drain(..) {
            leak.pay();
        }
    }
}

std::thread_local! {
    static BACKGROUND_RETURNS: (
        crossbeam_channel::Sender<QueuedJob>,
        crossbeam_channel::Receiver<QueuedJob>,
    ) = crossbeam_channel::unbounded();

    // Jobs this thread queued that have not come back yet. Every job comes
    // back, compiled or not, so this reaching zero means all of this thread's
    // compiles are installed (or failed).
    static BACKGROUND_OUTSTANDING: core::cell::Cell<usize> = const { core::cell::Cell::new(0) };

    // See `settle_background_compiles`.
    static SETTLE_AT_TOP_LEVEL: core::cell::Cell<bool> = const { core::cell::Cell::new(false) };
}

fn settle_returned_job(mut queued: QueuedJob) {
    queued.pay_leaks();
    let _ = BACKGROUND_OUTSTANDING.try_with(|n| n.set(n.get().saturating_sub(1)));
}

/// Drop the finished jobs this thread queued, on this thread.
fn drop_returned_background_jobs() {
    let _ = BACKGROUND_RETURNS.try_with(|(_, receiver)| {
        while let Ok(queued) = receiver.try_recv() {
            settle_returned_job(queued);
        }
    });
}

/// Block until every compile this thread queued has been installed. Only
/// this thread's: tests run side by side in one process and share the worker.
pub(crate) fn wait_for_background_compiles() {
    let _ = BACKGROUND_RETURNS.try_with(|(_, receiver)| {
        while BACKGROUND_OUTSTANDING.with(|n| n.get()) != 0 {
            match receiver.recv() {
                Ok(queued) => settle_returned_job(queued),
                Err(_) => break,
            }
        }
    });
}

/// With background compilation on, make this thread wait for its queued
/// compiles after every top level form (`SteelThread::run_executable`).
///
/// For tests. A test program normally finishes before the worker gets to it, so
/// it would only run interpreted. When settled, each form sees the functions
/// defined before it already compiled, like synchronous compilation, while every
/// compile still goes through the worker, the snapshot and the install. The
/// unsettled stress tests cover installs that land mid-run.
#[cfg(test)]
pub fn settle_background_compiles(settle: bool) {
    SETTLE_AT_TOP_LEVEL.with(|flag| flag.set(settle));
}

#[inline]
pub(crate) fn settle_after_top_level_form() {
    if SETTLE_AT_TOP_LEVEL.with(|flag| flag.get()) {
        wait_for_background_compiles();
    }
}

/// The globals as the compiler sees them, without copying the environment.
///
/// The compiler only looks up a global by an index from this function's bytecode
/// (a call or push operand, or the function's own slot), and only cares whether
/// it is a closure, a primitive, a builtin or a struct function. So the snapshot
/// has the same length (`get` past the end still means unbound), those kinds are
/// copied at the indices the bytecode mentions, and everything else is `Void`,
/// which only sends the compiler down a generic path. Cloning the shared
/// environment is O(1), but it is copy on write, so the next `define` would copy
/// the whole thing once per queued function.
fn sparse_globals_snapshot(
    ctx: &VmCore,
    body: &[crate::core::instructions::DenseInstruction],
    own_index: Option<usize>,
) -> Vec<SteelVal> {
    let roots = ctx.thread.global_env.roots();
    let mut globals = vec![SteelVal::Void; roots.len()];
    let mut copy = |index: usize| {
        if let Some(value) = roots.get(index) {
            if matches!(
                value,
                SteelVal::Closure(_)
                    | SteelVal::FuncV(_)
                    | SteelVal::MutFunc(_)
                    | SteelVal::BoxedFunction(_)
                    | SteelVal::BuiltIn(_)
                    | SteelVal::CustomStruct(_)
            ) {
                globals[index] = value.clone();
            }
        }
    };
    for instruction in body {
        copy(instruction.payload_size.to_usize());
    }
    if let Some(index) = own_index {
        copy(index);
    }
    globals
}

// Entry points installed in the background, keyed by body address, which a
// closure prototype and all of its instances share. Never pruned, since the code
// is never freed either. A later body at a reused address is only looked up once
// its own compile has overwritten the slot (a synchronously compiled lambda
// always carries its entry and never looks it up).
static BACKGROUND_ENTRIES: std::sync::LazyLock<
    std::sync::RwLock<rustc_hash::FxHashMap<usize, usize>>,
> = std::sync::LazyLock::new(Default::default);

/// The entry point for a lambda whose body was patched to enter native code
/// after the lambda itself was made (so it copied an empty entry); cached on
/// the lambda so this only happens once per lambda.
pub(crate) fn resolve_background_entry(
    lambda: &ByteCodeLambda,
) -> Option<crate::jit2::cgen::JitFnPointer> {
    if !jit_background_enabled() {
        return None;
    }
    let address = *BACKGROUND_ENTRIES
        .read()
        .unwrap_or_else(|e| e.into_inner())
        .get(&(lambda.body_exp.as_ptr() as usize))?;
    let pointer = crate::jit2::cgen::JitFnPointer(core::ptr::NonNull::new(address as *mut u8)?);
    lambda.install_super_instructions(pointer);
    Some(pointer)
}

/// Whether a background compile has switched this body over to native code.
/// The worker writes that opcode while interpreters run, hence the atomic read.
pub(crate) fn body_entered_natively(body: &[crate::core::instructions::DenseInstruction]) -> bool {
    let Some(first) = body.first() else {
        return false;
    };
    let op = core::ptr::addr_of!(first.op_code) as *const core::sync::atomic::AtomicU8;
    let byte = unsafe { (*op).load(core::sync::atomic::Ordering::Acquire) };
    byte == unsafe { core::mem::transmute::<OpCode, u8>(OpCode::DynSuperInstruction) }
}

// Interpreter threads blocked on the jit lock. The mutex is not fair, and the
// worker drops and retakes it between jobs, so a synchronous compile (capturing
// closures still compile in line) could wait out the whole queue. The worker
// steps aside while this is non zero, so the wait is at most one compile.
static INTERPRETER_WAITING_ON_JIT: AtomicUsize = AtomicUsize::new(0);

fn lock_jit_for_interpreter(
    jit: &std::sync::Mutex<crate::jit2::cgen::JIT>,
) -> std::sync::MutexGuard<'_, crate::jit2::cgen::JIT> {
    INTERPRETER_WAITING_ON_JIT.fetch_add(1, core::sync::atomic::Ordering::AcqRel);
    let guard = jit.lock().unwrap_or_else(|e| e.into_inner());
    INTERPRETER_WAITING_ON_JIT.fetch_sub(1, core::sync::atomic::Ordering::AcqRel);
    guard
}

static BACKGROUND_QUEUE: std::sync::OnceLock<crossbeam_channel::Sender<QueuedJob>> =
    std::sync::OnceLock::new();

fn background_queue() -> &'static crossbeam_channel::Sender<QueuedJob> {
    BACKGROUND_QUEUE.get_or_init(|| {
        let (sender, receiver) = crossbeam_channel::unbounded::<QueuedJob>();
        std::thread::Builder::new()
            .name("steel-jit".to_string())
            // Translation recurses over the bytecode; the main thread gets a
            // large stack for the same reason.
            .stack_size(64 * 1024 * 1024)
            .spawn(move || {
                for mut queued in receiver {
                    // A panic in one compile must not take the worker (and every
                    // compile after it) down; the lambda stays interpreted.
                    crate::jit2::cgen::begin_deferred_leaks();
                    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        run_background_job(&mut queued)
                    }));
                    queued.leaks = crate::jit2::cgen::take_deferred_leaks();
                    // If the queueing thread has gone there is no owner left to
                    // protect, and the job is settled here.
                    let returns = queued.returns.clone();
                    if let Err(crossbeam_channel::SendError(mut queued)) = returns.send(queued) {
                        queued.pay_leaks();
                    }
                }
            })
            .expect("spawning the background jit thread");
        sender
    })
}

/// Queue `lambda`'s compile. `lambda` must be the final, shared `Gc` the
/// program will call, since the compile bakes self calls against it and
/// installs into it.
pub(crate) fn submit_background_job(job: BackgroundJob, lambda: &Gc<ByteCodeLambda>) {
    drop_returned_background_jobs();
    let Ok(returns) = BACKGROUND_RETURNS.try_with(|(sender, _)| sender.clone()) else {
        return;
    };
    BACKGROUND_OUTSTANDING.with(|n| n.set(n.get() + 1));
    let queued = QueuedJob {
        job,
        lambda: Gc::clone(lambda),
        returns,
        leaks: Vec::new(),
    };
    if background_queue().send(queued).is_err() {
        BACKGROUND_OUTSTANDING.with(|n| n.set(n.get() - 1));
    }
}

fn run_background_job(queued: &mut QueuedJob) {
    let QueuedJob { job, lambda, .. } = queued;

    while INTERPRETER_WAITING_ON_JIT.load(core::sync::atomic::Ordering::Acquire) != 0 {
        std::thread::yield_now();
    }

    let compiled = job
        .jit
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .compile_bytecode(
            core::mem::take(&mut job.name),
            job.arity,
            &lambda.body_exp,
            &job.globals,
            &job.constants,
            job.maybe_index,
            job.self_slot.then_some(&*lambda),
            job.fn_ptr_name,
            &job.closed_globals,
        );

    let Ok(fn_pointer) = compiled else {
        return;
    };

    // Install the entry point before the opcode that sends the interpreter to
    // it, so whoever sees `DynSuperInstruction` finds the pointer.
    lambda.install_super_instructions(fn_pointer);
    BACKGROUND_ENTRIES
        .write()
        .unwrap_or_else(|e| e.into_inner())
        .insert(
            lambda.body_exp.as_ptr() as usize,
            fn_pointer.0.as_ptr() as usize,
        );

    // The interpreter enters native code by executing this lambda's first
    // instruction, so patching it switches the lambda over, including a loop
    // already running interpreted, whose next self tail call jumps back to it.
    // The body is this lambda's private copy (`jit_compile_lambda_impl` made it)
    // and the store is a single byte, so dispatch loops see either the old
    // opcode or the new one.
    //
    // Other lambdas can share this body with an empty entry, since closure
    // instances copy their prototype's entry when created, and ones made before
    // this have none. They find it in `BACKGROUND_ENTRIES`
    // (`resolve_background_entry`) the first time they need it.
    const _: () = assert!(core::mem::size_of::<OpCode>() == 1);
    let byte = unsafe { core::mem::transmute::<OpCode, u8>(OpCode::DynSuperInstruction) };
    unsafe {
        let first = lambda.body_exp.as_ptr();
        let op = core::ptr::addr_of!((*first).op_code) as *const core::sync::atomic::AtomicU8;
        (*op).store(byte, core::sync::atomic::Ordering::Release);
    }
}

// Block until every background compile this thread queued has been
// installed. For tests and measurements that want compiled code; returns at
// once when nothing is queued.
#[steel_derive::context(
    name = "#%jit-drain",
    arity = Exact(0),
    returns = Void,
    args(),
    raises = Never
)]
pub(crate) fn jit_drain(_ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    wait_for_background_compiles();
    Some(Ok(SteelVal::Void))
}

#[steel_derive::context(
    name = "#%jit-compile-2",
    arity = AtLeast(1),
    args(Any, rest = Any),
    raises = Never
)]
pub(crate) fn jit_compile_two(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let function = &args[0];

    let mut value_to_assign = None;

    if let SteelVal::Closure(func) = function {
        for (index, v) in ctx.thread.global_env.roots().iter().enumerate() {
            if let SteelVal::Closure(f) = v {
                if f.super_instructions().is_some() {
                    continue;
                }

                if Gc::ptr_eq(func, f) {
                    let mut func = func.unwrap();

                    let name = func.id.to_string();

                    let closed_globals = closed_globals(ctx);
                    let compiled = ctx
                        .thread
                        .jit
                        .lock()
                        .unwrap_or_else(|e| e.into_inner())
                        .compile_bytecode(
                            name,
                            func.arity,
                            &func.body_exp,
                            &ctx.thread.global_env.roots(),
                            &ctx.thread.constant_map,
                            None,
                            None,
                            None,
                            &closed_globals,
                        );
                    let fn_pointer = match compiled {
                        Ok(p) => p,
                        Err(_) => continue,
                    };

                    let super_instructions = Some(fn_pointer);
                    func.set_super_instructions(super_instructions);

                    let mut instructions = func.body_exp.iter().copied().collect::<Vec<_>>();
                    instructions[0].op_code = OpCode::DynSuperInstruction;

                    func.body_exp = InstructionPointer::from(instructions);

                    let return_func = Gc::new(func);
                    ctx.thread
                        .function_interner
                        .jit_funcs
                        .insert(return_func.id, return_func.clone());

                    // Whatever, we've rooted it somehow?

                    value_to_assign = Some((index, SteelVal::Closure(return_func)));
                }
            }
        }
    }

    if let Some((index, value_to_assign)) = value_to_assign {
        ctx.thread
            .with_locked_env(|_, env| env.set_idx(index, value_to_assign));
    }

    Some(Ok(SteelVal::Void))
}

#[steel_derive::context(
    name = "#%jit-compile",
    arity = AtLeast(1),
    args(Any, rest = Any),
    raises = Never
)]
pub(crate) fn jit_compile(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let function = &args[0];
    // Provide the name for the function for recursive calls
    // so we can jump straight into it without bailing back
    // into the function.
    let function_name = args.get(1).and_then(|x| x.as_string()).and_then(|ident| {
        ctx.thread
            .compiler
            .read()
            .symbol_map
            .get(&ident.as_str().into())
            .ok()
    });

    if let SteelVal::Closure(func) = function {
        let mut func = func.unwrap();

        let name = func.id.to_string();

        let closed_globals = closed_globals(ctx);
        let compiled = ctx
            .thread
            .jit
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .compile_bytecode(
                name,
                func.arity,
                &func.body_exp,
                &ctx.thread.global_env.roots(),
                &ctx.thread.constant_map,
                function_name,
                None,
                None,
                &closed_globals,
            );
        let fn_pointer = match compiled {
            Ok(p) => p,
            Err(_) => return Some(Ok(function.clone())),
        };

        let super_instructions = Some(fn_pointer);
        func.set_super_instructions(super_instructions);

        let mut instructions = func.body_exp.iter().copied().collect::<Vec<_>>();
        instructions[0].op_code = OpCode::DynSuperInstruction;

        func.body_exp = InstructionPointer::from(instructions);

        let return_func = Gc::new(func);
        ctx.thread
            .function_interner
            .jit_funcs
            .insert(return_func.id, return_func.clone());

        // Whatever, we've rooted it somehow?

        Some(Ok(SteelVal::Closure(return_func)))
    } else {
        Some(Ok(SteelVal::Void))
    }
}

/// A family of jit helpers that differ only in how many trailing `SteelVal`
/// arguments they take. Generates `$defs`, whose `register` adds one function
/// per arity to the jit's symbol map and whose `arity_to_name` finds it, and in
/// module `$module` those functions, `f0` up to `f16`. Each is registered as
/// `$prefix` + arity + `$suffix`. `$add` is `add_func` for functions taking the
/// vm context first, `add_func2` otherwise. The body sees the trailing arguments
/// as an array bound to `$args`.
macro_rules! helper_family {
    (
        $defs:ident, mod $module:ident,
        names $prefix:literal $suffix:literal,
        $add:ident fn $params:tt -> $ret:ty,
        arities [$($n:tt)*],
        |$args:ident| $body:expr $(,)?
    ) => {
        pub struct $defs;

        impl $defs {
            const NAMES: &'static [(usize, &'static str)] =
                &[$(($n, concat!($prefix, $n, $suffix))),*];

            pub fn register(map: &mut crate::jit2::cgen::FunctionMap) {
                $(
                    helper_family!(@arity register $n {
                        map, $module, $add, concat!($prefix, $n, $suffix), $params, $ret
                    });
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                Self::NAMES
                    .iter()
                    .find(|(arity, _)| *arity == count)
                    .map(|(_, name)| *name)
            }
        }

        mod $module {
            use super::*;
            $( helper_family!(@arity emit $n { $params, $ret, $args, $body }); )*
        }
    };

    // The argument names for each arity.
    (@arity $mode:ident 0 $rest:tt) => { helper_family!(@ $mode f0 0 [] $rest); };
    (@arity $mode:ident 1 $rest:tt) => { helper_family!(@ $mode f1 1 [a] $rest); };
    (@arity $mode:ident 2 $rest:tt) => { helper_family!(@ $mode f2 2 [a b] $rest); };
    (@arity $mode:ident 3 $rest:tt) => { helper_family!(@ $mode f3 3 [a b c] $rest); };
    (@arity $mode:ident 4 $rest:tt) => { helper_family!(@ $mode f4 4 [a b c d] $rest); };
    (@arity $mode:ident 5 $rest:tt) => { helper_family!(@ $mode f5 5 [a b c d e] $rest); };
    (@arity $mode:ident 6 $rest:tt) => { helper_family!(@ $mode f6 6 [a b c d e f] $rest); };
    (@arity $mode:ident 7 $rest:tt) => { helper_family!(@ $mode f7 7 [a b c d e f g] $rest); };
    (@arity $mode:ident 8 $rest:tt) => { helper_family!(@ $mode f8 8 [a b c d e f g h] $rest); };
    (@arity $mode:ident 9 $rest:tt) => { helper_family!(@ $mode f9 9 [a b c d e f g h i] $rest); };
    (@arity $mode:ident 10 $rest:tt) => { helper_family!(@ $mode f10 10 [a b c d e f g h i j] $rest); };
    (@arity $mode:ident 11 $rest:tt) => { helper_family!(@ $mode f11 11 [a b c d e f g h i j k] $rest); };
    (@arity $mode:ident 12 $rest:tt) => { helper_family!(@ $mode f12 12 [a b c d e f g h i j k l] $rest); };
    (@arity $mode:ident 13 $rest:tt) => { helper_family!(@ $mode f13 13 [a b c d e f g h i j k l m] $rest); };
    (@arity $mode:ident 14 $rest:tt) => { helper_family!(@ $mode f14 14 [a b c d e f g h i j k l m n] $rest); };
    (@arity $mode:ident 15 $rest:tt) => { helper_family!(@ $mode f15 15 [a b c d e f g h i j k l m n o] $rest); };
    (@arity $mode:ident 16 $rest:tt) => { helper_family!(@ $mode f16 16 [a b c d e f g h i j k l m n o p] $rest); };

    (@register $f:ident $n:tt [$($arg:ident)*] {
        $map:ident, $module:ident, $add:ident, $name:expr,
        ($($pre:ident: $pre_ty:ty),*), $ret:ty
    }) => {
        #[cfg(target_os = "windows")]
        $map.$add(
            $name,
            $module::$f as extern "sysv64-unwind" fn($($pre: $pre_ty,)* $($arg: SteelVal),*) -> $ret,
        );
        #[cfg(not(target_os = "windows"))]
        $map.$add(
            $name,
            $module::$f as extern "C-unwind" fn($($pre: $pre_ty,)* $($arg: SteelVal),*) -> $ret,
        );
    };

    (@emit $f:ident $n:tt [$($arg:ident)*] {
        ($($pre:ident: $pre_ty:ty),*), $ret:ty, $args:ident, $body:expr
    }) => {
        #[steel_derive::cross_platform_fn]
        fn $f($($pre: $pre_ty,)* $($arg: SteelVal),*) -> $ret {
            #[allow(unused_mut)]
            let mut $args: [SteelVal; $n] = [$($arg),*];
            $body
        }
    };
}

mod calls;
mod census;
mod closures;
mod data;
mod frames;
mod numeric;
mod primitive_calls;
mod refcount;
mod tail_calls;

pub use calls::*;
pub use census::*;
pub use closures::*;
pub use data::*;
pub use frames::*;
pub use numeric::*;
pub use primitive_calls::*;
pub use refcount::*;
pub use tail_calls::*;
