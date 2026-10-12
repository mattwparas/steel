use super::*;

/// Whether `func` takes a frame when called, instead of returning a value.
#[inline(always)]
pub(super) fn takes_frame(func: &SteelVal) -> bool {
    matches!(
        func,
        SteelVal::Closure(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::BuiltIn(_)
            | SteelVal::CustomStruct(_)
            | SteelVal::MutableStruct(_)
    )
}

/// Whether calling `closure` from here runs it in the interpreter.
#[inline(always)]
fn closure_leaves_native(ctx: &mut VmCore, closure: &Gc<ByteCodeLambda>) -> bool {
    !(closure.0.super_instructions().is_some() && should_trampoline(ctx))
}

/// Whether calling `func` leaves compiled code. Everything that takes a frame
/// does, except a jitted closure the trampoline can run from here.
#[inline(always)]
fn leaves_native(ctx: &mut VmCore, func: &SteelVal) -> bool {
    match func {
        SteelVal::Closure(c) => closure_leaves_native(ctx, c),
        _ => takes_frame(func),
    }
}

/// The value a call hands back to compiled code. An error is stashed for the
/// interpreter, which takes over when the caller checks for a deopt.
#[inline(always)]
pub(super) fn stash_error(ctx: &mut VmCore, result: Result<SteelVal>) -> SteelVal {
    match result {
        Ok(v) => v,
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));
            SteelVal::Void
        }
    }
}

#[cold]
fn not_a_procedure(ctx: &mut VmCore, func: &SteelVal) -> Result<SteelVal> {
    stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", func); ctx.current_span());
}

/// Calls `func` on `args` if it is a native function, which returns its value.
#[inline(always)]
pub(super) fn call_native(
    ctx: &mut VmCore,
    func: &SteelVal,
    args: &mut [SteelVal],
) -> Option<Result<SteelVal>> {
    let result = match func {
        SteelVal::FuncV(f) => ctx.thread.enter_safepoint_holding(args, |_| f(args)),
        SteelVal::BoxedFunction(f) => ctx.thread.enter_safepoint_holding(args, |_| f.func()(args)),
        SteelVal::MutFunc(f) => f(args),
        _ => return None,
    };
    Some(result.map_err(|e| e.set_span_if_none(ctx.current_span())))
}

/// Calls `closure` on the `arity` values on top of the stack. With native stack
/// to spare, a jitted closure runs on the trampoline and its value comes back
/// here. Otherwise it only gets its frame, for the interpreter to run.
#[inline(always)]
fn call_closure<const CHECK_ARITY: bool>(
    ctx: &mut VmCore,
    closure: Gc<ByteCodeLambda>,
    arity: usize,
) -> Result<SteelVal> {
    let jitted = if should_trampoline(ctx) {
        closure.0.super_instructions()
    } else {
        None
    };

    let pop_count = ctx.pop_count;
    let depth = ctx.thread.stack_frames.len();

    // An arity mismatch is an ordinary error here.
    if CHECK_ARITY {
        ctx.handle_function_call_closure_jit(closure, arity)?;
    } else {
        ctx.handle_function_call_closure_jit_no_arity(closure)?;
    }

    let Some(func) = jitted else {
        return Ok(SteelVal::Void);
    };

    let tramp = ctx.thread.trampoline;
    let res = (tramp)(ctx, func);

    if ctx.is_native {
        debug_assert_eq!(ctx.pop_count, pop_count);
        debug_assert_eq!(ctx.thread.stack_frames.len(), depth);
        Ok(res)
    } else {
        Ok(SteelVal::Void)
    }
}

/// Calls a callee that takes a frame on the `arity` values on top of the stack.
/// In tail position a closure replaces the current frame.
#[inline(always)]
pub(super) fn call_frame<const TAIL: bool, const CHECK_ARITY: bool>(
    ctx: &mut VmCore,
    func: SteelVal,
    arity: usize,
) -> Result<SteelVal> {
    match func {
        SteelVal::Closure(closure) if TAIL => {
            ctx.new_handle_tail_call_closure(closure, arity)?;
            Ok(SteelVal::Void)
        }
        SteelVal::Closure(closure) => call_closure::<CHECK_ARITY>(ctx, closure, arity),
        SteelVal::ContinuationFunction(cc) => {
            ctx.call_continuation(cc)?;
            Ok(SteelVal::Void)
        }
        SteelVal::BuiltIn(f) => {
            ctx.call_builtin_func(f, arity)?;
            Ok(SteelVal::Void)
        }
        SteelVal::CustomStruct(s) => {
            ctx.call_custom_struct(&s, arity)?;
            Ok(SteelVal::Void)
        }
        SteelVal::MutableStruct(s) => {
            ctx.call_mutable_struct(&s, arity)?;
            Ok(SteelVal::Void)
        }
        _ => not_a_procedure(ctx, &func),
    }
}

/// Calls `func` on `args`. Anything but a native function gets the args pushed
/// and takes a frame.
#[inline(always)]
pub(super) fn call_with_args<const TAIL: bool, const CHECK_ARITY: bool>(
    ctx: &mut VmCore,
    func: SteelVal,
    args: &mut [SteelVal],
) -> Result<SteelVal> {
    if let Some(result) = call_native(ctx, &func, args) {
        return result;
    }

    if !takes_frame(&func) {
        return not_a_procedure(ctx, &func);
    }

    let arity = args.len();
    for val in args {
        ctx.thread
            .stack
            .push(core::mem::replace(val, SteelVal::Void));
    }

    call_frame::<TAIL, CHECK_ARITY>(ctx, func, arity)
}

fn drop_spilled_args(ctx: &mut VmCore, arity: usize) {
    let len = ctx.thread.stack.len();
    let _ = ctx.thread.stack.truncate(len - arity);
}

/// Calls `func` on the `arity` values already on top of the stack. A native
/// function's args are dropped after the call, except in tail position, where
/// returning from the frame releases them.
#[inline(always)]
pub(super) fn call_spilled<const TAIL: bool>(
    ctx: &mut VmCore,
    func: SteelVal,
    arity: usize,
) -> Result<SteelVal> {
    let result = match func {
        SteelVal::FuncV(func) => ctx
            .thread
            .enter_safepoint(move |t| func(&t.stack[t.stack.len() - arity..])),
        SteelVal::BoxedFunction(func) => ctx
            .thread
            .enter_safepoint(move |t| func.func()(&t.stack[t.stack.len() - arity..])),
        SteelVal::MutFunc(func) => {
            let len = ctx.thread.stack.len();
            func(&mut ctx.thread.stack[len - arity..])
        }
        _ => return call_frame::<TAIL, true>(ctx, func, arity),
    };

    let result = result.map_err(|e| e.set_span_if_none(ctx.current_span()));
    if !TAIL {
        drop_spilled_args(ctx, arity);
    }
    result
}

fn call_global_function_deopt(
    ctx: &mut VmCore,
    lookup_index: usize,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    debug_assert!(ctx.is_native);
    let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);

    ctx.ip = fallback_ip;
    if leaves_native(ctx, &func) {
        ctx.is_native = false;
    }

    // Only the calls that leave compiled code. Native functions return here.
    if deopt_census_enabled() && !ctx.is_native {
        record_deopt(&func, lookup_index, ctx);
    }

    let result = call_with_args::<false, true>(ctx, func, args);
    stash_error(ctx, result)
}

helper_family! {
    CallGlobalFunctionDefinitions, mod call_global_function_deopt,
    names "call_global_function_deopt_" "",
    add_func fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| unsafe { call_global_function_deopt(&mut *ctx, lookup_index, fallback_ip, &mut args) }
}

/// `call_global_function_deopt_N`, for a call the compiler proved has the
/// right number of arguments.
fn call_global_function_deopt_no_arity<const N: usize>(
    ctx: &mut VmCore,
    lookup_index: usize,
    fallback_ip: usize,
    mut args: [SteelVal; N],
) -> SteelVal {
    debug_assert!(ctx.is_native);
    let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);

    ctx.ip = fallback_ip;
    if leaves_native(ctx, &func) {
        ctx.is_native = false;
    }

    let result = call_with_args::<false, false>(ctx, func, &mut args);
    stash_error(ctx, result)
}

helper_family! {
    CallGlobalNoArityFunctionDefinitions, mod call_global_function_deopt_no_arity,
    names "call_global_function_deopt_" "_no_arity",
    add_func fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| {
        call_global_function_deopt_no_arity(unsafe { &mut *ctx }, lookup_index, fallback_ip, args)
    }
}

// A call to a global with its `arity` args already spilled to the stack.
#[cross_platform_fn]
fn call_global_function_deopt_spilled(
    ctx: *mut VmCore,
    lookup_index: usize,
    fallback_ip: usize,
    arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    debug_assert!(ctx.is_native);
    let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);

    ctx.ip = fallback_ip;
    if leaves_native(ctx, &func) {
        ctx.is_native = false;
    }

    let result = call_spilled::<false>(ctx, func, arity);
    stash_error(ctx, result)
}

/// A call to a value off the stack. `fallback_ip` is one past the call.
fn call_function_deopt(
    ctx: &mut VmCore,
    func: SteelVal,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    debug_assert!(ctx.is_native);

    ctx.ip = fallback_ip - 1;
    if leaves_native(ctx, &func) {
        ctx.is_native = false;
    }

    let result = call_with_args::<false, true>(ctx, func, args);
    stash_error(ctx, result)
}

// `call_function_deopt_N` for more arguments than those take, which are
// already spilled to the stack.
#[cross_platform_fn]
fn call_function_deopt_spilled(
    ctx: *mut VmCore,
    func: SteelVal,
    fallback_ip: usize,
    arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    debug_assert!(ctx.is_native);

    ctx.ip = fallback_ip - 1;
    if leaves_native(ctx, &func) {
        ctx.is_native = false;
    }

    let result = call_spilled::<false>(ctx, func, arity);
    stash_error(ctx, result)
}

helper_family! {
    CallFunctionDefinitions, mod call_function_deopt,
    names "call_function_deopt_" "",
    add_func fn(ctx: *mut VmCore, func: SteelVal, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15],
    |args| unsafe { call_function_deopt(&mut *ctx, func, fallback_ip, &mut args) }
}

/// A call to `func`, the function being run, with `args`. The caller keeps
/// `func` alive.
fn call_self_function_deopt_no_arity<const N: usize>(
    ctx: &mut VmCore,
    func: Gc<ByteCodeLambda>,
    fallback_ip: usize,
    args: [SteelVal; N],
) -> SteelVal {
    debug_assert!(ctx.is_native);
    let closure = (*ManuallyDrop::new(func)).clone();

    ctx.ip = fallback_ip;
    if closure_leaves_native(ctx, &closure) {
        ctx.is_native = false;
    }

    for arg in args {
        ctx.thread.stack.push(arg);
    }

    let result = call_closure::<false>(ctx, closure, N);
    stash_error(ctx, result)
}

helper_family! {
    CallSelfNoArityFunctionDefinitions, mod call_self_function_deopt_no_arity,
    names "call_self_function_deopt_" "_no_arity",
    add_func fn(ctx: *mut VmCore, func: Gc<ByteCodeLambda>, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| call_self_function_deopt_no_arity(unsafe { &mut *ctx }, func, fallback_ip, args)
}

// Call of a global that held `func` when the caller was compiled. The caller
// keeps `func` alive. If the global has been changed since, this is the
// generic call, or the generic tail call when `TAIL` is set.
fn call_boxed_function_handler<const TAIL: bool>(
    ctx: &mut VmCore,
    func: *const BoxedDynFunction,
    index: usize,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    let unchanged = matches!(
        ctx.thread.global_env.repl_lookup_idx_ref(index),
        SteelVal::BoxedFunction(f) if core::ptr::eq(f.as_ptr(), func)
    );

    if !unchanged {
        return if TAIL {
            callglobal_tail_handler_deopt(ctx, index, fallback_ip, args)
        } else {
            call_global_function_deopt(ctx, index, fallback_ip, args)
        };
    }

    let func = unsafe { &*func };
    ctx.ip = fallback_ip;

    let result = ctx
        .thread
        .enter_safepoint_holding(args, |_| func.func()(args))
        .map_err(|e| e.set_span_if_none(ctx.current_span()));
    stash_error(ctx, result)
}

helper_family! {
    CallBoxedFunctionDefinitions, mod call_boxed_function,
    names "call_boxed_function_" "",
    add_func fn(
        ctx: *mut VmCore,
        func: *const BoxedDynFunction,
        lookup_index: usize,
        fallback_ip: usize
    ) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| unsafe {
        call_boxed_function_handler::<false>(&mut *ctx, func, lookup_index, fallback_ip, &mut args)
    }
}

helper_family! {
    CallBoxedFunctionTailDefinitions, mod call_boxed_function_tail,
    names "call_boxed_function_tail_" "",
    add_func fn(
        ctx: *mut VmCore,
        func: *const BoxedDynFunction,
        lookup_index: usize,
        fallback_ip: usize
    ) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| unsafe {
        call_boxed_function_handler::<true>(&mut *ctx, func, lookup_index, fallback_ip, &mut args)
    }
}
