//! Tail calls from jitted code. A callee that takes a frame replaces the
//! current one and the interpreter runs it. A native function's value is the
//! current frame's return value.

use super::*;

/// Hands a tail call's result to the interpreter. A value from a native
/// callee is returned from the current frame.
#[inline(always)]
fn finish_tail_call(ctx: &mut VmCore, took_frame: bool, result: Result<SteelVal>) -> SteelVal {
    match result {
        Ok(v) if !took_frame => {
            extern_handle_pop(ctx, v);
            ctx.is_native = false;
            SteelVal::Void
        }
        result => stash_error(ctx, result),
    }
}

pub(super) fn callglobal_tail_handler_deopt(
    ctx: &mut VmCore,
    index: usize,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    let func = ctx.thread.global_env.repl_lookup_idx(index);
    debug_assert!(ctx.is_native);

    if deopt_census_enabled() {
        record_deopt(&func, index, ctx);
    }

    ctx.ip = fallback_ip;
    ctx.is_native = false;

    if let SteelVal::BuiltIn(f) = &func {
        let f = *f;
        let len = args.len();
        for val in args.iter_mut() {
            ctx.thread
                .stack
                .push(core::mem::replace(val, SteelVal::Void));
        }
        let base = ctx.thread.stack.len() - len;
        let frames_before = ctx.thread.stack_frames.len();
        let top_before = ctx.thread.stack_frames.last().map(|x| x.function.id);
        match ctx.call_builtin_func_reporting(f, len) {
            Ok(returned_value) => {
                // Both checks are needed. The frame count alone misses a
                // tail-position `apply`, which replaces the current frame and
                // leaves the count unchanged. The builtin's own answer alone
                // misses `call/cc`, which returns a value and sets up a frame.
                let produced_value = returned_value
                    && ctx.thread.stack_frames.len() == frames_before
                    && ctx.thread.stack_frames.last().map(|x| x.function.id) == top_before;

                if produced_value && ctx.thread.stack.len() > base {
                    let v = ctx.thread.stack.pop().unwrap();
                    extern_handle_pop(ctx, v);
                }
            }
            Err(e) => {
                ctx.result = Some(Err(e));
            }
        }
        return SteelVal::Void;
    }

    let took_frame = takes_frame(&func);
    let result = call_with_args::<true, true>(ctx, func, args);
    finish_tail_call(ctx, took_frame, result)
}

helper_family! {
    CallGlobalTailFunctionDefinitions, mod call_global_function_tail_deopt,
    names "call_global_function_tail_deopt_" "",
    add_func fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14],
    |args| unsafe { callglobal_tail_handler_deopt(&mut *ctx, lookup_index, fallback_ip, &mut args) }
}

// Tail call of a global that held a native function when the caller was
// compiled. A native function's value is returned to the caller, which returns
// it without leaving compiled code. Anything else goes through the deopt handler.
fn callglobal_native_tail_handler(
    ctx: &mut VmCore,
    index: usize,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    let func = ctx.thread.global_env.repl_lookup_idx(index);
    ctx.ip = fallback_ip;

    match call_native(ctx, &func, args) {
        Some(result) => stash_error(ctx, result),
        None => callglobal_tail_handler_deopt(ctx, index, fallback_ip, args),
    }
}

helper_family! {
    CallGlobalNativeTailDefinitions, mod call_global_native_tail,
    names "call_global_native_tail_" "",
    add_func fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8],
    |args| unsafe { callglobal_native_tail_handler(&mut *ctx, lookup_index, fallback_ip, &mut args) }
}

// A tail call to a global with its `arity` args already spilled to the stack.
#[cross_platform_fn]
fn callglobal_tail_handler_deopt_spilled(
    ctx: *mut VmCore,
    index: usize,
    fallback_ip: usize,
    arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let func = ctx.thread.global_env.repl_lookup_idx(index);

    ctx.ip = fallback_ip;
    ctx.is_native = false;

    let took_frame = takes_frame(&func);
    let result = call_spilled::<true>(ctx, func, arity);
    finish_tail_call(ctx, took_frame, result)
}

/// A tail call to a value off the stack. `fallback_ip` is one past the call.
fn call_function_tail_deopt(
    ctx: &mut VmCore,
    func: SteelVal,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    debug_assert!(ctx.is_native);

    ctx.ip = fallback_ip - 1;
    ctx.is_native = false;

    let took_frame = takes_frame(&func);
    let result = call_with_args::<true, true>(ctx, func, args);
    finish_tail_call(ctx, took_frame, result)
}

// `call_function_tail_deopt_N` for more arguments than those take, which are
// already spilled to the stack.
#[cross_platform_fn]
fn call_function_tail_deopt_spilled(
    ctx: *mut VmCore,
    func: SteelVal,
    fallback_ip: usize,
    arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    debug_assert!(ctx.is_native);

    ctx.ip = fallback_ip - 1;
    ctx.is_native = false;

    let took_frame = takes_frame(&func);
    let result = call_spilled::<true>(ctx, func, arity);
    finish_tail_call(ctx, took_frame, result)
}

helper_family! {
    CallFunctionTailDefinitions, mod call_function_tail_deopt,
    names "call_function_tail_deopt_" "",
    add_func fn(ctx: *mut VmCore, func: SteelVal, fallback_ip: usize) -> SteelVal,
    arities [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14],
    |args| unsafe { call_function_tail_deopt(&mut *ctx, func, fallback_ip, &mut args) }
}

// A self tail call the interpreter finishes. Moves the `current_arity` args
// down over the frame's locals, collecting a rest list first.
#[cross_platform_fn]
fn tcojmp_handler(ctx: *mut VmCore, current_arity: usize) {
    let this = unsafe { &mut *ctx };
    debug_assert!(this.is_native);

    if let Err(e) = tco_jmp_handler_multi_arity(current_arity, this) {
        this.result = Some(Err(e));
    }

    this.is_native = false;
}

fn tco_jmp_handler_multi_arity(mut current_arity: usize, this: &mut VmCore<'_>) -> Result<()> {
    let function = &this.thread.stack_frames.last().unwrap().function;
    let is_multi_arity = function.is_multi_arity;
    let original_arity = function.arity();

    this.ip = 0;

    handle_multi_arity(
        is_multi_arity,
        original_arity,
        current_arity,
        this,
        &mut current_arity,
    )?;

    let back = this.thread.stack.len() - current_arity;
    let _ = this.thread.stack.drain(this.sp..back);

    Ok(())
}
