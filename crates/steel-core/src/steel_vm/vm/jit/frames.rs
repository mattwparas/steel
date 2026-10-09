//! The value stack, call frames and locals, for jitted code.

use super::*;

#[cross_platform_fn]
fn set_handler_c(ctx: *mut VmCore, index: usize, value_to_assign: SteelVal) -> SteelVal {
    let this = unsafe { &mut *ctx };
    let value = if this.thread.safepoints_enabled {
        #[cfg(feature = "sync")]
        {
            this.thread
                .with_locked_env(|_, env| env.set_idx(index, value_to_assign))
        }

        #[cfg(not(feature = "sync"))]
        this.thread
            .with_locked_env(|this| this.global_env.repl_set_idx(index, value_to_assign))?
    } else {
        this.thread.local_set(index, value_to_assign).unwrap()
    };

    this.ip += 1;

    value
}

#[cross_platform_fn]
fn handle_attachments_pop(
    ctx: *mut VmCore,
    attachments: Option<Box<StackFrameAttachments>>,
    sp: usize,
) {
    let ctx = unsafe { &mut *ctx };

    if let Some(cont_mark) = attachments.as_ref().and_then(|x| {
        x.weak_continuation_mark
            .as_ref()
            .and_then(|x| WeakShared::upgrade(&x.inner))
    }) {
        // See `Continuation::outlives`
        let dying = ctx.thread.stack.get(sp..).unwrap_or(&[]);
        if Continuation::outlives(&cont_mark, 1, dying) {
            cont_mark.write().close(ctx);
        }
    }
}

#[cross_platform_fn]
fn pop_slow_path_finish(ctx: *mut VmCore, value: SteelVal) {
    let ctx = unsafe { &mut *ctx };
    let last = ctx.thread.stack_frames.pop();

    let rollback_index = last
        .map(|x| {
            ctx.close_continuation_marks(&x);
            x.sp
        })
        .unwrap_or(0);

    // Move forward past the pop
    ctx.ip += 1;
    ctx.thread.stack.truncate(rollback_index as _);
    ctx.sp = 0;
    ctx.result = Some(Ok(value));
}

// Collect the trailing `count` arguments into the list a variadic callee
// expects in its last slot. This is what `adjust_stack_for_multi_arity` does
// for the interpreter. A jitted call site that sets up the frame itself calls
// this instead, once it knows at compile time how many arguments are surplus.
#[cross_platform_fn]
fn collect_rest_args(ctx: *mut VmCore, count: usize) {
    let ctx = unsafe { &mut *ctx };
    let start = ctx.thread.stack.len() - count;
    let values = ctx.thread.stack.drain(start..).collect();
    ctx.thread.stack.push(SteelVal::ListV(values));
}

// Frameless self calls.
//
// A jitted self call to a function's `_frameless` copy pushes no `StackFrame`.
// The arguments are on the value stack where a framed call would put them, and
// the frame record is only built if something has to leave native code. Then
// each caller whose frameless callee comes back not native records the frame
// that call would have pushed. This happens innermost first as the native stack
// unwinds, but the frames have to go on outermost first. So the records collect
// here, and the framed copy that made the first frameless call (the boundary)
// splices them onto `stack_frames` in reverse.
thread_local! {
    static FRAMELESS_PENDING: core::cell::RefCell<Vec<StackFrame>> =
        const { core::cell::RefCell::new(Vec::new()) };
}

// Record the frame a frameless call from a jitted caller would have pushed,
// with the callee's `sp`, the caller's return `ip` and instructions, and the
// closure. This takes its own reference to the closure, since the frameless
// call never did.
#[cfg(feature = "rooted-instructions")]
#[cross_platform_fn]
fn frameless_record_frame(
    _ctx: *mut VmCore,
    sp: usize,
    ip: usize,
    function: Gc<ByteCodeLambda>,
    instructions_ptr: usize,
    instructions_len: usize,
) {
    let function = ManuallyDrop::new(function);
    let frame = StackFrame {
        sp: sp as _,
        ip: ip as _,
        instructions: RootedInstructions {
            ptr: instructions_ptr as *const _,
            len: instructions_len,
        },
        function: (*function).clone(),
        attachments: None,
    };
    FRAMELESS_PENDING.with(|pending| pending.borrow_mut().push(frame));
}

// Put the recorded frames on the frame stack, outermost first. Each is one
// more frame for the interpreter to pop before it hands back to native code.
#[cross_platform_fn]
fn frameless_splice_frames(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    FRAMELESS_PENDING.with(|pending| {
        let mut pending = pending.borrow_mut();
        ctx.pop_count += pending.len();
        while let Some(frame) = pending.pop() {
            ctx.thread.stack_frames.push(frame);
        }
    });
}

#[cross_platform_fn]
fn grow_stack_slow(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    ctx.thread.stack.grow_capacity();
}

#[cross_platform_fn]
fn stack_ensure_capacity(ctx: *mut VmCore, capacity: usize) {
    let ctx = unsafe { &mut *ctx };
    ctx.thread.stack.reserve_exact(capacity)
}

#[cross_platform_fn]
fn grow_frame_stack_slow(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    ctx.thread.stack_frames.grow_capacity();
}

// TODO: Do this for all the other handlers,
// and figure out the return values!

// TODO:

#[cross_platform_fn]
fn extern_handle_pop(ctx: *mut VmCore, value: SteelVal) {
    unsafe {
        let this = &mut *ctx;
        let res = this.handle_pop_pure_value(value);
        this.result = res;
    }
}

impl<'a> VmCore<'a> {
    // Get the local value directly, without having to
    // push it to the VM stack. This is fine UNTIL we hit a begin scope
    #[inline(always)]
    fn get_local_value(&mut self, index: usize) -> SteelVal {
        let offset = self.get_offset();

        let value = self.thread.stack[index + offset].clone();

        self.ip += 1;
        return value;
    }

    #[inline(always)]
    fn move_local_value(&mut self, index: usize) -> SteelVal {
        let offset = self.get_offset();

        let value = core::mem::replace(&mut self.thread.stack[index + offset], SteelVal::Void);
        self.ip += 1;
        return value;
    }

    #[inline(always)]
    fn get_const_index(&mut self, idx: usize) -> SteelVal {
        self.constants.get_value(idx)
    }
}

#[cross_platform_fn]
fn push_const_value_index_c(ctx: *mut VmCore, index: usize) -> SteelVal {
    unsafe { (&mut *ctx).get_const_index(index) }
}

#[cross_platform_fn]
fn move_read_local_0_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(0) }
}

#[cross_platform_fn]
fn move_read_local_1_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(1) }
}

#[cross_platform_fn]
fn move_read_local_2_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(2) }
}

#[cross_platform_fn]
fn move_read_local_3_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(3) }
}

#[cross_platform_fn]
fn read_local_0_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(0) }
}

#[cross_platform_fn]
fn read_local_1_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(1) }
}

#[cross_platform_fn]
fn read_local_2_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(2) }
}

#[cross_platform_fn]
fn read_local_3_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(3) }
}

#[cross_platform_fn]
fn set_local_any_c(ctx: *mut VmCore, offset: usize, value: SteelVal) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    guard.handle_set_local_value(offset, value)
}
