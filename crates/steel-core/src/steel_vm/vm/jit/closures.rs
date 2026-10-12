use super::*;

#[cross_platform_fn]
fn setup_closure_call_arity(
    ctx: *mut VmCore,
    closure: Gc<ByteCodeLambda>,
    arity: usize,
    fallback_arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };

    // With `STEEL_STACK_DEBUG=1`, report how much native stack the jitted frames
    // are using when they hand off here.
    if crate::config::config().stack_debug {
        static N: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        if N.fetch_add(1, std::sync::atomic::Ordering::Relaxed) % 200000 == 0 {
            let here = 0u8;
            let sp = &here as *const u8 as usize;
            let anchor = ctx.thread.native_stack_anchor;
            eprintln!(
                "[stack] anchor={anchor:#x} sp={sp:#x} used={} frames={}",
                anchor.wrapping_sub(sp) as i64,
                ctx.thread.stack_frames.len()
            );
        }
    }
    let closure = ManuallyDrop::new(closure);

    ctx.ip = fallback_arity;
    // Arity mismatches end up here, since the generated fast path only takes a
    // closure whose arity matches. The error has to be reported, otherwise no
    // frame is pushed and the stack is misread.
    if let Err(e) = ctx.handle_function_call_closure_jit((&*closure).clone(), arity) {
        ctx.result = Some(Err(e));
        ctx.is_native = false;
    }
    SteelVal::Void
}

#[inline(never)]
pub(super) fn handle_multi_arity(
    is_multi_arity: bool,
    original_arity: usize,
    payload_size: usize,
    ctx: &mut VmCore<'_>,
    current_arity: &mut usize,
) -> Result<()> {
    if likely(!is_multi_arity) {
        if unlikely(original_arity != payload_size) {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", original_arity, payload_size); ctx.current_span());
        }
    } else {
        if payload_size < original_arity - 1 {
            stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", original_arity - 1, payload_size); ctx.current_span());
        }

        // (define (test x . y))
        // (test 1 2 3 4 5)
        // in this case, arity = 2 and payload size = 5
        // pop off the last 4, collect into a list
        let amount_to_remove = 1 + payload_size - original_arity;

        let values = ctx
            .thread
            .stack
            .drain(ctx.thread.stack.len() - amount_to_remove..)
            .collect();

        let list = SteelVal::ListV(values);

        ctx.thread.stack.push(list);

        *current_arity = original_arity;
    }

    Ok(())
}

#[cross_platform_fn]
fn handle_pure_function(ctx: *mut VmCore, ip: usize, offset: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    ctx.ip = ip;

    assert!(ctx.ip < ctx.instructions.len());

    ctx.ip += 1;

    let is_multi_arity = ctx.instructions[ctx.ip].payload_size.to_u32() == 1;

    ctx.ip += 1;

    // Check whether this is a let or a rooted function
    let closure_id = ctx.instructions[ctx.ip].payload_size.to_u32();

    ctx.ip += 1;

    // TODO - used to be offset - 2, now 3 with the multi arity
    let forward_jump = offset - 2;

    // TODO clean this up a bit
    // hold the spot for where we need to jump aftwards
    let forward_index = ctx.ip + forward_jump;

    let constructed_lambda = if let Some(prototype) = ctx
        .thread
        .function_interner
        .pure_function_interner
        .get(&closure_id)
    {
        prototype.clone()
    } else {
        // TODO -> this is probably quite slow
        // If extraneous lets are lifted, we probably don't need this
        // or if instructions get stored in some sort of shared memory so I'm not deep cloning the window

        let forward_jump_index = ctx.ip + forward_jump - 1;

        // Construct the closure body using the offsets from the payload
        // used to be - 1, now - 2
        let closure_body = ctx.instructions[ctx.ip..forward_jump_index].into();

        let spans = if let Some(spans) = ctx
            .thread
            .stack_frames
            .last()
            .and_then(|x| ctx.thread.function_interner.spans.get(&x.function.id))
        {
            if forward_jump_index >= spans.len() {
                if let Some(span_range) = ctx.root_spans.get(ctx.ip..forward_jump_index) {
                    span_range.to_vec().into()
                } else {
                    Shared::from(Vec::new())
                }
            } else {
                spans[ctx.ip..forward_jump_index].into()
            }
        } else {
            // TODO: Under certain circumstances, it is possible we're doing some funky
            // call/cc business, and the rooted spans don't make it back on during the call/cc
            // section of the evaluation. In this case, we should try to optimistically store
            // all of the spans that we have, rather than doing it lazily like we're doing now.
            // See this body of code for an offending edge case:
            /*

            λ > (define the-empty-cont #f)
            λ > (call/cc (lambda (k) (set! the-empty-cont k)))
            => #false
            λ > the-empty-cont
            => #<procedure>
            λ > (+ 10 20 30 40 (call/cc (k) (the-empty-cont k) k))
            error[E02]: FreeIdentifier
              ┌─ :1:26
              │
            1 │ (+ 10 20 30 40 (call/cc (k) (the-empty-cont k) k))
              │                          ^ k

            λ > (+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k)))
            => #<procedure>
            λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))) 100)
            => #<procedure>
            λ > (+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k)))
            => #<procedure>
            λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))))
            => #<procedure>
            λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))) 10)
            => #<procedure>
            λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))) 100)
            => #<procedure>
            λ > (define the-next-cont #f)
            λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) k)))
            error[E03]: TypeMismatch
              ┌─ :1:2
              │
            1 │ (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) k)))
              │  ^ + expects a number, found: (Continuation)

            λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))))

            => #false
            λ > the-next-cont
            => #<procedure>
            λ > (the-next-cont 10)
            => 110
            λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))) (begin (displayln "hello world") 100))
            => #false
            λ > (the-next-cont 10)
            hello world
            => 210
            // λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))) (begin (displayln "hello world") (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))) 100))
            => #false
            λ > (the-next-cont 10)
            hello world
            thread 'main' panicked at crates/steel-core/src/steel_vm/vm.rs:2699:32:
            range end index 31 out of range for slice of length 4
            note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

                            */
            if let Some(span_range) = ctx.root_spans.get(ctx.ip..forward_jump_index) {
                span_range.to_vec().into()
            } else {
                Shared::from(Vec::new())
            }
        };

        // snag the arity from the eclosure instruction
        let arity = ctx.instructions[forward_index - 1].payload_size;

        let constructed_lambda = ByteCodeLambda::new(
            closure_id,
            closure_body,
            arity.to_usize(),
            is_multi_arity,
            CaptureVec::new(),
        );

        // Put the spans into the interner as well
        ctx.thread.function_interner.spans.insert(closure_id, spans);

        #[cfg(feature = "jit2")]
        let (constructed_lambda, background_job) =
            jit::jit_compile_lambda_deferred(ctx, constructed_lambda, None, None);

        let constructed_lambda = Gc::new(constructed_lambda);

        if let Some(job) = background_job {
            jit::submit_background_job(job, &constructed_lambda);
        }

        ctx.thread
            .function_interner
            .pure_function_interner
            .insert(closure_id, Gc::clone(&constructed_lambda));

        constructed_lambda
    };

    let res = SteelVal::Closure(constructed_lambda);

    ctx.ip = forward_index;

    res
}

#[cross_platform_fn]
fn handle_new_start_closure(ctx: *mut VmCore, ip: usize, offset: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    ctx.ip = ip;

    ctx.ip += 1;

    let is_multi_arity = ctx.instructions[ctx.ip].payload_size.to_u32() == 1;

    ctx.ip += 1;

    // // Get the ID of the function
    let closure_id = ctx.instructions[ctx.ip].payload_size.to_u32();

    ctx.ip += 1;

    let forward_jump = offset - 3;

    // Snag the number of upvalues here
    let ndefs = ctx.instructions[ctx.ip].payload_size.to_usize();
    ctx.ip += 1;

    // TODO preallocate size
    let mut captures = CaptureVec::with_capacity(ndefs);

    // TODO clean this up a bit
    // hold the spot for where we need to jump aftwards
    let forward_index = ctx.ip + forward_jump;

    // Insert metadata
    // TODO: We probably can get a bit better than this.
    // A lot of the captures are static, I'm not quite sure we necessarily need to patch down _every_ single one
    // each time, especially since each lambda is a standalone instance of this.

    // Top level %plain-let with closures will panic here:
    // (%plain-let ((foo 10) (bar 20)) (lambda (+ foo bar)))
    // So, this should probably do something like this:

    if let Some(guard) = ctx.thread.stack_frames.last() {
        let stack_index = ctx.get_offset();

        for _ in 0..ndefs {
            let instr = ctx.instructions[ctx.ip];
            match (instr.op_code, instr.payload_size) {
                (OpCode::COPYCAPTURESTACK, n) => {
                    let offset = stack_index;
                    let value = ctx.thread.stack[n.to_usize() + offset].clone();
                    captures.push(value);
                }
                (OpCode::COPYCAPTURECLOSURE, n) => {
                    debug_assert!(
                        !ctx.thread.stack_frames.is_empty(),
                        "Trying to capture from closure that doesn't exist",
                    );

                    debug_assert!((n.to_usize()) < guard.function.captures().len());

                    let value = guard.function.captures()[n.to_usize()].clone();

                    captures.push(value);
                }
                (l, _) => {
                    crate::core::instructions::pretty_print_dense_instructions(&ctx.instructions);
                    panic!(
                        "Something went wrong in closure construction!, found: {:?} @ {}",
                        l, ctx.ip,
                    );
                }
            }
            ctx.ip += 1;
        }
    } else {
        let stack_index = ctx.get_offset();

        for _ in 0..ndefs {
            let instr = ctx.instructions[ctx.ip];
            match (instr.op_code, instr.payload_size) {
                (OpCode::COPYCAPTURESTACK, n) => {
                    let offset = stack_index;
                    let value = ctx.thread.stack[n.to_usize() + offset].clone();
                    captures.push(value);
                }
                (l, _) => {
                    panic!(
                        "Something went wrong in closure construction!, found: {:?} @ {}",
                        l, ctx.ip,
                    );
                }
            }
            ctx.ip += 1;
        }
    }

    // TODO: Consider moving these captures into the interned closure directly
    // Its possible we're just doing a lot of extra capturing that way if we repeatedly copy things
    let constructed_lambda = if let Some(prototype) = ctx
        .thread
        .function_interner
        .closure_interner
        .get(&closure_id)
    {
        ByteCodeLambda::from_prototype(prototype, captures)
    } else {
        debug_assert!(ctx.instructions[forward_index - 1].op_code == OpCode::ECLOSURE);

        if forward_index - 1 > ctx.instructions.len() {
            crate::core::instructions::pretty_print_dense_instructions(ctx.instructions.as_ref());
            println!("Forward index: {}", ctx.ip + forward_jump - 1);
            println!("Length: {}", ctx.instructions.len());
            panic!("Out of bounds forward jump");
        }

        let forward_jump_index = forward_index - 1;

        // Construct the closure body using the offsets from the payload
        // used to be - 1, now - 2
        let closure_body = ctx.instructions[ctx.ip..forward_jump_index].into();

        let spans = if let Some(spans) = ctx
            .thread
            .stack_frames
            .last()
            .and_then(|x| ctx.thread.function_interner.spans.get(&x.function.id))
        {
            if forward_jump_index >= spans.len() {
                if let Some(span_range) = ctx.root_spans.get(ctx.ip..forward_jump_index) {
                    span_range.to_vec().into()
                } else {
                    Shared::from(Vec::new())
                }
            } else {
                spans[ctx.ip..forward_jump_index].into()
            }
        } else {
            // For now, lets go ahead and use this... hack to get us going
            if let Some(span_range) = ctx.root_spans.get(ctx.ip..forward_jump_index) {
                span_range.to_vec().into()
            } else {
                Shared::from(Vec::new())
            }
        };

        // snag the arity from the eclosure instruction
        let arity = ctx.instructions[forward_jump_index].payload_size;

        let constructed_lambda = ByteCodeLambda::new(
            closure_id,
            closure_body,
            arity.to_usize(),
            is_multi_arity,
            CaptureVec::new(),
        );

        // Put the spans into the interner
        ctx.thread.function_interner.spans.insert(closure_id, spans);

        #[cfg(feature = "jit2")]
        let mut constructed_lambda = if crate::config::config().jit
            && cfg!(not(all(target_os = "windows", target_arch = "aarch64")))
        {
            let (lambda, background_job) =
                jit::jit_compile_lambda_deferred(ctx, constructed_lambda, None, None);
            // Interned by value, as in `VmCore::handle_new_start_closure`.
            if let Some(job) = background_job {
                jit::submit_background_job(job, &Gc::new(lambda.clone()));
            }
            lambda
        } else {
            constructed_lambda
        };

        ctx.thread
            .function_interner
            .closure_interner
            .insert(closure_id, constructed_lambda.clone());

        constructed_lambda.set_captures(captures);

        constructed_lambda
    };

    ctx.ip = forward_index;

    let closure = SteelVal::Closure(Gc::new(constructed_lambda));

    closure
}
