use std::mem::ManuallyDrop;

use steel_gen::opcode::{MAX_OPCODE_SIZE, OPCODES_ARRAY};

use super::VmCore;
use crate::{
    gc::Gc,
    primitives::lists::{cons, list_ref},
    rvals::Result,
    steel_vm::primitives::{gt_primitive, gte_primitive, lt_primitive},
    SteelVal,
};

use super::*;

pub const TRAMPOLINE: bool = true;

pub(crate) fn jit_compile_lambda(ctx: &mut VmCore, mut func: ByteCodeLambda) -> ByteCodeLambda {
    if func
        .body_exp
        .iter()
        .any(|x| matches!(x.op_code, OpCode::PUREFUNC))
    {
        return func;
    }

    if ctx.thread.compiler.read().kernel.is_none() {
        return func;
    }

    if func.is_multi_arity {
        return func;
    }

    let name = func.id.to_string();

    // let mut inner = func.unwrap();
    let fn_pointer = ctx.thread.jit.lock().unwrap().compile_bytecode(
        name,
        func.arity,
        &func.body_exp,
        &ctx.thread.global_env.roots(),
        &ctx.thread.constant_map,
        None,
    );

    let fn_pointer = if let Ok(fn_pointer) = fn_pointer {
        fn_pointer
    } else {
        return func;
    };

    let super_instructions = Some(fn_pointer);
    func.super_instructions = super_instructions;

    let mut instructions = func.body_exp.iter().copied().collect::<Vec<_>>();
    instructions[0].op_code = OpCode::DynSuperInstruction;

    func.body_exp = Arc::from(instructions.into_boxed_slice());

    println!("Compiled: {}", func.id);

    func
}

#[steel_derive::context(name = "#%jit-compile-2", arity = "AtLeast(1)")]
pub(crate) fn jit_compile_two(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let function = &args[0];

    let mut value_to_assign = None;

    if let SteelVal::Closure(func) = function {
        for (index, v) in ctx.thread.global_env.roots().iter().enumerate() {
            if let SteelVal::Closure(f) = v {
                if f.is_multi_arity {
                    continue;
                }

                if f.super_instructions.is_some() {
                    continue;
                }

                if Gc::ptr_eq(func, f) {
                    let mut func = func.unwrap();

                    let name = func.id.to_string();

                    // let mut inner = func.unwrap();
                    let fn_pointer = ctx
                        .thread
                        .jit
                        .lock()
                        .unwrap()
                        .compile_bytecode(
                            name,
                            func.arity,
                            &func.body_exp,
                            &ctx.thread.global_env.roots(),
                            &ctx.thread.constant_map,
                            None,
                        )
                        .unwrap();

                    let super_instructions = Some(fn_pointer);
                    func.super_instructions = super_instructions;

                    let mut instructions = func.body_exp.iter().copied().collect::<Vec<_>>();
                    instructions[0].op_code = OpCode::DynSuperInstruction;

                    func.body_exp = Arc::from(instructions.into_boxed_slice());

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

#[steel_derive::context(name = "#%jit-compile", arity = "AtLeast(1)")]
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

        // let mut inner = func.unwrap();
        let fn_pointer = ctx
            .thread
            .jit
            .lock()
            .unwrap()
            .compile_bytecode(
                name,
                func.arity,
                &func.body_exp,
                &ctx.thread.global_env.roots(),
                &ctx.thread.constant_map,
                function_name,
            )
            .unwrap();

        let super_instructions = Some(fn_pointer);
        func.super_instructions = super_instructions;

        let mut instructions = func.body_exp.iter().copied().collect::<Vec<_>>();
        instructions[0].op_code = OpCode::DynSuperInstruction;

        func.body_exp = Arc::from(instructions.into_boxed_slice());

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

pub static C_HANDLERS: [OpHandlerC; MAX_OPCODE_SIZE] = initialize_handlers();

pub type OpHandlerC = extern "C-unwind" fn(*mut VmCore) -> bool;

const fn initialize_handlers() -> [OpHandlerC; MAX_OPCODE_SIZE] {
    let mut ops = [unhandled_handler_impl_c as _; MAX_OPCODE_SIZE];

    let mut i = 0;
    while i < MAX_OPCODE_SIZE {
        let op = OPCODES_ARRAY[i];
        ops[i] = match op {
            OpCode::VOID => void_handler_impl_c as _,
            OpCode::PUSH => push_handler_impl_c as _,
            OpCode::IF => if_handler_impl_c as _,
            OpCode::JMP => jmp_handler_impl_c as _,
            OpCode::FUNC => func_handler_impl_c as _,
            OpCode::BIND => bind_handler_impl_c as _,
            OpCode::SDEF => sdef_handler_impl_c as _,
            OpCode::EDEF => edef_handler_impl_c as _,
            OpCode::POPPURE => poppure_handler_impl_c as _,
            OpCode::POPN => popn_handler_impl_c as _,
            OpCode::POPSINGLE => popsingle_handler_impl_c as _,
            OpCode::PASS => pass_handler_impl_c as _,
            OpCode::PUSHCONST => pushconst_handler_impl_c as _,
            OpCode::PANIC => panic_handler_impl_c as _,
            OpCode::TAILCALL => tailcall_handler_impl_c as _,
            OpCode::SET => set_handler_impl_c as _,
            OpCode::READLOCAL => readlocal_handler_impl_c as _,
            OpCode::READLOCAL0 => local_handler0_impl_c as _,
            OpCode::READLOCAL1 => local_handler1_impl_c as _,
            OpCode::READLOCAL2 => local_handler2_impl_c as _,
            OpCode::READLOCAL3 => local_handler3_impl_c as _,
            OpCode::SETLOCAL => setlocal_handler_impl_c as _,
            OpCode::TCOJMP => tcojmp_handler_impl_c as _,
            OpCode::CALLGLOBAL => callglobal_handler_impl_c as _,
            // TODO:
            OpCode::CALLGLOBALTAIL => callglobaltail_handler_impl_c as _,
            OpCode::LOADINT0 => loadint0_handler_impl_c as _,
            OpCode::LOADINT1 => loadint1_handler_impl_c as _,
            OpCode::LOADINT2 => loadint2_handler_impl_c as _,
            OpCode::MOVEREADLOCAL => movereadlocal_handler_impl_c as _,
            OpCode::MOVEREADLOCAL0 => movereadlocal0_handler_impl_c as _,
            OpCode::MOVEREADLOCAL1 => movereadlocal1_handler_impl_c as _,
            OpCode::MOVEREADLOCAL2 => movereadlocal2_handler_impl_c as _,
            OpCode::MOVEREADLOCAL3 => movereadlocal3_handler_impl_c as _,
            OpCode::READCAPTURED => readcaptured_handler_impl_c as _,
            OpCode::BEGINSCOPE => beginscope_handler_impl_c as _,
            OpCode::LETENDSCOPE => let_end_scope_handler_impl_c as _,
            OpCode::PUREFUNC => purefunc_handler_impl_c as _,
            OpCode::ADD => add_handler_impl_c as _,
            OpCode::SUB => sub_handler_impl_c as _,
            OpCode::MUL => mul_handler_impl_c as _,
            OpCode::DIV => div_handler_impl_c as _,
            OpCode::EQUAL => equal_handler_impl_c as _,
            OpCode::NUMEQUAL => numequal_handler_impl_c as _,
            OpCode::NULL => null_handler_impl_c as _,
            OpCode::LTE => lte_handler_impl_c as _,
            OpCode::LT => lt_handler_impl_c as _,
            OpCode::GTE => gte_handler_impl_c as _,
            OpCode::GT => gt_handler_impl_c as _,
            OpCode::CONS => cons_handler_impl_c as _,
            OpCode::LIST => list_handler_impl_c as _,
            OpCode::CAR => car_handler_impl_c as _,
            OpCode::CDR => cdr_handler_impl_c as _,
            OpCode::NEWBOX => newbox_handler_impl_c as _,
            OpCode::SETBOX => setbox_handler_impl_c as _,
            OpCode::UNBOX => unbox_handler_impl_c as _,
            OpCode::NEWSCLOSURE => newsclosure_handler_impl_c as _,
            OpCode::ADDREGISTER => addregister_handler_impl_c as _,
            OpCode::SUBREGISTER => subregister_handler_impl_c as _,
            OpCode::LTEREGISTER => lteregister_handler_impl_c as _,
            OpCode::SUBREGISTER1 => subregister1_handler_impl_c as _,
            OpCode::ALLOC => alloc_handler_impl_c as _,
            OpCode::READALLOC => read_alloc_handler_impl_c as _,
            OpCode::SETALLOC => set_alloc_handler_impl_c as _,
            OpCode::Arity => arity_handler_impl_c as _,
            OpCode::ADDIMMEDIATE => addimmediate_handler_impl_c as _,
            OpCode::SUBIMMEDIATE => subimmediate_handler_impl_c as _,
            OpCode::LTEIMMEDIATE => lteimmediate_handler_impl_c as _,
            OpCode::BINOPADD => binopadd_handler_impl_c as _,
            OpCode::LTEIMMEDIATEIF => lteimmediateif_handler_impl_c as _,
            OpCode::NOT => not_handler_impl_c as _,
            OpCode::VEC => vec_handler_impl_c as _,

            // TODO:
            // OpCode::POPJMP => popjmp_handler_impl_c as _,
            // // TODO:
            // OpCode::BINOPADDTAIL => binopaddtail_handler_impl_c as _,
            // // TODO:
            // OpCode::LOADINT1POP => loadint1pop_handler_impl_c as _,
            _ => unhandled_handler_impl_c as _,
        };

        i += 1;
    }

    ops
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn set_handler_c(
    ctx: *mut VmCore,
    index: usize,
    value_to_assign: SteelVal,
) -> SteelVal {
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

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn drop_value(ctx: *mut VmCore, arg: SteelVal) {
    unsafe { &mut *ctx }.ip += 1;
    drop(arg);
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn pop_value(ctx: *mut VmCore) -> SteelVal {
    unsafe { &mut *ctx }.thread.stack.pop().unwrap()
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn car_handler_value(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    unsafe { &mut *ctx }.ip += 2;
    match car(&arg) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn list_ref_handler_c(
    ctx: *mut VmCore,
    list: SteelVal,
    index: SteelVal,
) -> SteelVal {
    fn inner_list_ref(list: SteelVal, index: SteelVal) -> Result<SteelVal> {
        if let SteelVal::ListV(l) = list {
            if let SteelVal::IntV(i) = index {
                list_ref(&l, i)
            } else {
                stop!(TypeMismatch => "list-ref expected an integer, found: {}", index);
            }
        } else {
            stop!(TypeMismatch => "list-ref expected a list, found: {}", list);
        }
    }

    match inner_list_ref(list, index) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn vector_ref_handler_c(
    ctx: *mut VmCore,
    vec: SteelVal,
    index: SteelVal,
) -> SteelVal {
    match vec_ref(&vec, &index) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn box_handler_c(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    let this = unsafe { &mut *ctx };

    let allocated_var = this.thread.heap.lock().unwrap().allocate(
        arg,
        &this.thread.stack,
        this.thread.stack_frames.iter().map(|x| x.function.as_ref()),
        this.thread.global_env.roots(),
        &this.thread.thread_local_storage,
        &mut this.thread.synchronizer,
    );

    SteelVal::HeapAllocated(allocated_var)
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn unbox_handler_c(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    let this = unsafe { &mut *ctx };

    if let SteelVal::HeapAllocated(h) = arg {
        h.get()
    } else {
        let err = || stop!(TypeMismatch => "unbox expected a boxed value, found: {}", arg);

        this.is_native = false;
        this.result = Some(err());
        SteelVal::Void
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn setbox_handler_c(
    ctx: *mut VmCore,
    arg: SteelVal,
    value: SteelVal,
) -> SteelVal {
    let this = unsafe { &mut *ctx };

    if let SteelVal::HeapAllocated(h) = arg {
        h.set_and_return(value)
    } else {
        let err = || stop!(TypeMismatch => "unbox expected a boxed value, found: {}", arg);

        this.is_native = false;
        this.result = Some(err());
        SteelVal::Void
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn cons_handler_value(
    ctx: *mut VmCore,
    mut arg: SteelVal,
    mut arg2: SteelVal,
) -> SteelVal {
    // let mut arg = ManuallyDrop::new(arg);
    // let mut arg2 = ManuallyDrop::new(arg2);

    unsafe { &mut *ctx }.ip += 2;

    // The problem here, is that arg2 is expecting
    // a reference to the stack slot. If its _actually_
    // the last value, then we'll have a clone of it.
    //
    // Otherwise, we might have a problem. We also need
    // to be sure that we're not _reusing_ variables off
    // of the stack here, and that their destructors are
    // getting called after, if they exist.
    match cons(&mut arg, &mut arg2) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn cdr_handler_value(ctx: *mut VmCore, arg: SteelVal) -> SteelVal {
    unsafe { &mut *ctx }.ip += 2;
    let mut arg = arg;

    match cdr(&mut arg) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

macro_rules! extern_c {
    ($func:expr, $name:tt) => {
        extern "C-unwind" fn $name(ctx: *mut VmCore) -> bool {
            unsafe { $func(&mut *ctx).is_ok() }
        }
    };
}

extern_c!(unhandled_handler_tco, unhandled_handler_impl_c);

type Dispatch = ();

// TODO: Do this for all the other handlers,
// and figure out the return values!
extern_c!(void_handler_impl, void_handler_impl_c);
extern_c!(push_handler_impl, push_handler_impl_c);
extern_c!(if_handler_impl, if_handler_impl_c);
extern_c!(jmp_handler_impl, jmp_handler_impl_c);
extern_c!(func_handler_impl, func_handler_impl_c);
extern_c!(bind_handler_impl, bind_handler_impl_c);
extern_c!(sdef_handler_impl, sdef_handler_impl_c);
extern_c!(edef_handler_impl, edef_handler_impl_c);
extern_c!(poppure_handler_impl, poppure_handler_impl_c);
extern_c!(popn_handler_impl, popn_handler_impl_c);
extern_c!(popsingle_handler_impl, popsingle_handler_impl_c);
extern_c!(pass_handler_impl, pass_handler_impl_c);
extern_c!(pushconst_handler_impl, pushconst_handler_impl_c);
extern_c!(panic_handler_impl, panic_handler_impl_c);
extern_c!(tailcall_handler_impl, tailcall_handler_impl_c);
extern_c!(set_handler_impl, set_handler_impl_c);
extern_c!(readlocal_handler_impl, readlocal_handler_impl_c);
extern_c!(local_handler0, local_handler0_impl_c);
extern_c!(local_handler1, local_handler1_impl_c);
extern_c!(local_handler2, local_handler2_impl_c);
extern_c!(local_handler3, local_handler3_impl_c);
extern_c!(setlocal_handler_impl, setlocal_handler_impl_c);

extern_c!(tcojmp_handler_impl, tcojmp_handler_impl_c);
extern_c!(callglobal_handler_impl, callglobal_handler_impl_c);
// TODO:
extern_c!(callglobaltail_handler_impl, callglobaltail_handler_impl_c);
extern_c!(loadint0_handler_impl, loadint0_handler_impl_c);
extern_c!(loadint1_handler_impl, loadint1_handler_impl_c);
extern_c!(loadint2_handler_impl, loadint2_handler_impl_c);
extern_c!(movereadlocal_handler_impl, movereadlocal_handler_impl_c);
extern_c!(movereadlocal0_handler_impl, movereadlocal0_handler_impl_c);
extern_c!(movereadlocal1_handler_impl, movereadlocal1_handler_impl_c);
extern_c!(movereadlocal2_handler_impl, movereadlocal2_handler_impl_c);
extern_c!(movereadlocal3_handler_impl, movereadlocal3_handler_impl_c);
extern_c!(readcaptured_handler_impl, readcaptured_handler_impl_c);
extern_c!(beginscope_handler_impl, beginscope_handler_impl_c);
extern_c!(let_end_scope_handler, let_end_scope_handler_impl_c);
extern_c!(purefunc_handler_impl, purefunc_handler_impl_c);
extern_c!(add_handler_impl, add_handler_impl_c);
extern_c!(sub_handler_impl, sub_handler_impl_c);
extern_c!(mul_handler_impl, mul_handler_impl_c);
extern_c!(div_handler_impl, div_handler_impl_c);
extern_c!(equal_handler_impl, equal_handler_impl_c);
extern_c!(numequal_handler_impl, numequal_handler_impl_c);
extern_c!(null_handler_impl, null_handler_impl_c);
extern_c!(lte_handler_impl, lte_handler_impl_c);
extern_c!(lt_handler_impl, lt_handler_impl_c);
extern_c!(gte_handler_impl, gte_handler_impl_c);
extern_c!(gt_handler_impl, gt_handler_impl_c);
extern_c!(cons_handler, cons_handler_impl_c);
extern_c!(list_handler_impl, list_handler_impl_c);
extern_c!(car_handler, car_handler_impl_c);
extern_c!(cdr_handler, cdr_handler_impl_c);
extern_c!(new_box_handler, newbox_handler_impl_c);
extern_c!(setbox_handler, setbox_handler_impl_c);
extern_c!(unbox_handler, unbox_handler_impl_c);
extern_c!(newsclosure_handler_impl, newsclosure_handler_impl_c);
extern_c!(addregister_handler_impl, addregister_handler_impl_c);
extern_c!(subregister_handler_impl, subregister_handler_impl_c);
extern_c!(lteregister_handler_impl, lteregister_handler_impl_c);
extern_c!(subregister1_handler_impl, subregister1_handler_impl_c);
extern_c!(alloc_handler, alloc_handler_impl_c);
extern_c!(read_alloc_handler, read_alloc_handler_impl_c);
extern_c!(set_alloc_handler, set_alloc_handler_impl_c);
extern_c!(arity_handler_impl, arity_handler_impl_c);
extern_c!(addimmediate_handler_impl, addimmediate_handler_impl_c);
extern_c!(subimmediate_handler_impl, subimmediate_handler_impl_c);
extern_c!(lteimmediate_handler_impl, lteimmediate_handler_impl_c);
extern_c!(binopadd_handler_impl, binopadd_handler_impl_c);
extern_c!(lteimmediateif_handler_impl, lteimmediateif_handler_impl_c);
extern_c!(not_handler, not_handler_impl_c);
extern_c!(vec_handler_impl, vec_handler_impl_c);

// TODO:
// extern_c!(popjmp_handler_tco, popjmp_handler_impl_c);
// // TODO:
// extern_c!(binopaddtail_handler_tco, binopaddtail_handler_impl_c);
// // TODO:
// extern_c!(loadint1pop_handler_tco, loadint1pop_handler_impl_c);
// extern_c!(unhandled_handler_tco, unhandled_handler_impl_c);

#[inline(always)]
fn subregister1_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let read_local = &ctx.instructions[ctx.ip + 1];
    // get the local
    // let offset = frame.index;
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let local_value = ctx.thread.stack[read_local.payload_size.to_usize() + offset].clone();

    let result = match subtract_primitive(&[local_value, SteelVal::IntV(1)]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.push(result);

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn list_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    list_handler(ctx, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn addregister_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];
    let offset = ctx.get_offset();
    let local_value = ctx.thread.stack[read_local.payload_size.to_usize() + offset].clone();

    // get the const
    let const_val = ctx.constants.get_value(push_const.payload_size.to_usize());

    let result = match add_primitive(&[local_value, const_val]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.push(result);

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn subregister_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];

    // get the local
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let local_value = ctx.thread.stack[read_local.payload_size.to_usize() + offset].clone();

    // get the const
    let const_val = ctx.constants.get_value(push_const.payload_size.to_usize());

    let result = match subtract_primitive(&[local_value, const_val]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.push(result);

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn lteregister_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];

    // get the local
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let local_value = ctx.thread.stack[read_local.payload_size.to_usize() + offset].clone();

    // get the const
    let const_val = ctx.constants.get_value(push_const.payload_size.to_usize());

    let result = match lte_primitive(&[local_value, const_val]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.push(result);

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn addimmediate_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];

    // get the local
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let local_value = &ctx.thread.stack[read_local.payload_size.to_usize() + offset];

    // get the const value, if it can fit into the value...
    // let const_val = SteelVal::IntV(push_const.payload_size.to_usize() as isize);
    let const_val = push_const.payload_size.to_usize() as isize;

    let result = match local_value {
        SteelVal::IntV(l) => {
            match l.checked_add(const_val) {
                Some(r) => SteelVal::IntV(r),
                // Slow path
                None => SteelVal::BigNum(Gc::new(BigInt::from(*l) + const_val)),
            }
        }
        _ => {
            cold();
            add_slow(ctx, local_value.clone(), SteelVal::IntV(const_val))?
        }
    };

    ctx.thread.stack.push(result);
    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn subimmediate_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let result = subimmediate_impl(ctx)?;
    ctx.thread.stack.push(result);
    ctx.ip += 2;
    Ok(())
}

fn subimmediate_impl(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];
    let offset = ctx.get_offset();
    let l = &ctx.thread.stack[read_local.payload_size.to_usize() + offset];
    let r = push_const.payload_size.to_usize() as isize;
    let result = match l {
        // Fast path with an integer, otherwise slow path
        SteelVal::IntV(l) => {
            match l.checked_sub(&r) {
                Some(r) => SteelVal::IntV(r),
                // Slow path
                None => SteelVal::BigNum(Gc::new(BigInt::from(*l) - r)),
            }
        }

        SteelVal::NumV(_)
        | SteelVal::Rational(_)
        | SteelVal::BigNum(_)
        | SteelVal::BigRational(_) => {
            cold();
            subtract_slow(l, r).map_err(|x| x.set_span_if_none(ctx.current_span()))?
        }
        _ => {
            cold();
            stop!(TypeMismatch => "sub expected a number, found: {}", l)
        }
    };
    Ok(result)
}

#[inline(never)]
fn subtract_slow(l: &SteelVal, r: isize) -> Result<SteelVal> {
    subtract_primitive(&[l.clone(), SteelVal::IntV(r)])
}

#[inline(always)]
fn lteimmediate_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    // inline_register_primitive_immediate!(subtract_primitive)
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];

    // get the local
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let l = &ctx.thread.stack[read_local.payload_size.to_usize() + offset];

    // get the const value, if it can fit into the value...
    let r = push_const.payload_size.to_usize() as isize;

    let result = match l {
        SteelVal::IntV(_)
        | SteelVal::NumV(_)
        | SteelVal::Rational(_)
        | SteelVal::BigNum(_)
        | SteelVal::BigRational(_) => l.clone() <= SteelVal::IntV(r),
        _ => {
            stop!(TypeMismatch => format!("lte expected an number, found: {}", l); ctx.current_span())
        }
    };

    ctx.thread.stack.push(SteelVal::BoolV(result));

    ctx.ip += 2;
    Ok(())
}

fn lteimmediateif_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let result = lteimmediateif_function(ctx)?;

    ctx.ip += 2;

    // change to truthy...
    if result {
        ctx.ip += 1;
    } else {
        ctx.ip = ctx.instructions[ctx.ip].payload_size.to_usize();
    }

    Ok(())
}

// #[inline(never)]
fn lteimmediateif_function(ctx: &mut VmCore<'_>) -> Result<bool> {
    let read_local = &ctx.instructions[ctx.ip];
    let push_const = &ctx.instructions[ctx.ip + 1];

    // get the local
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let l = &ctx.thread.stack[read_local.payload_size.to_usize() + ctx.sp];

    // get the const value, if it can fit into the value...
    let r = push_const.payload_size.to_usize() as isize;

    if let SteelVal::IntV(l) = l {
        Ok(*l <= r)
    } else {
        lte_slow(l.clone(), r, ctx)
    }
}

#[inline(never)]
fn lte_slow(l: SteelVal, r: isize, ctx: &mut VmCore<'_>) -> Result<bool> {
    match l {
        SteelVal::IntV(_)
        | SteelVal::NumV(_)
        | SteelVal::Rational(_)
        | SteelVal::BigNum(_)
        | SteelVal::BigRational(_) => Ok(l <= SteelVal::IntV(r)),
        _ => {
            cold();
            stop!(TypeMismatch => format!("lte expected an number, found: {}", l); ctx.current_span())
        }
    }
}

#[inline(always)]
fn add_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    add_handler_payload(ctx, payload_size.to_usize())
}

#[inline(always)]
fn binopadd_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let right = ctx.thread.stack.pop().unwrap();
    let left = ctx.thread.stack.last_mut().unwrap();

    let result = match add_two(left, &right) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    *left = result;

    ctx.ip += 2;
    Ok(())
}

#[inline(never)]
fn add_slow(ctx: &mut VmCore, left: SteelVal, right: SteelVal) -> Result<SteelVal> {
    match add_two(&left, &right) {
        Ok(value) => Ok(value),
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    }
}

#[inline(always)]
fn sub_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    sub_handler_payload(ctx, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn mul_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let last_index = ctx.thread.stack.len() - payload_size.to_usize();

    let result = match multiply_primitive(&mut ctx.thread.stack[last_index..]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.truncate(last_index + 1);
    *ctx.thread.stack.last_mut().unwrap() = result;

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn div_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let last_index = ctx.thread.stack.len() - payload_size.to_usize();

    let result = match divide_primitive(&mut ctx.thread.stack[last_index..]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.truncate(last_index + 1);
    *ctx.thread.stack.last_mut().unwrap() = result;

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn equal_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let last_index = ctx.thread.stack.len() - payload_size.to_usize();

    let result = match equality_primitive(&mut ctx.thread.stack[last_index..]) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    ctx.thread.stack.truncate(last_index + 1);
    *ctx.thread.stack.last_mut().unwrap() = result;

    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn numequal_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    numequal_slow(ctx)?;
    ctx.ip += 2;
    Ok(())
}

#[inline(never)]
fn numequal_slow(ctx: &mut VmCore<'_>) -> Result<()> {
    let mut last = ctx.thread.stack.pop().unwrap();
    let second_last = ctx.thread.stack.last_mut().unwrap();
    let result = match number_equality(second_last, &mut last) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };
    *second_last = result;
    Ok(())
}

#[inline(always)]
fn null_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    // Simply fast path case for checking null or empty
    let last = ctx.thread.stack.last_mut().unwrap();
    let result = is_empty(last);
    *last = SteelVal::BoolV(result);
    ctx.ip += 2;
    Ok(())
}

#[inline(always)]
fn pop(ctx: &mut VmCore) {
    drop(ctx.thread.stack.pop());
}

#[inline(always)]
fn popsingle_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    pop(ctx);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn popn_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let last = ctx.thread.stack.pop().unwrap();
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.thread
        .stack
        .truncate(ctx.thread.stack.len() - payload_size.to_usize());
    ctx.thread.stack.push(last);
    ctx.ip += 1;
    Ok(())
}

fn poppure_handler_impl(ctx: &mut VmCore<'_>) -> Result<Dispatch> {
    if let Some(r) = ctx.handle_pop_pure() {
        // return r;
        ctx.return_value = Some(r?);
        return Ok(());
    }

    Ok(())
}

// Duped from vm.rs
macro_rules! handler_inline_primitive_payload {
    ($ctx:expr, $name:tt, $payload_size: expr) => {{
        let last_index = $ctx.thread.stack.len() - $payload_size;

        let result = match $name(&mut $ctx.thread.stack[last_index..]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
        };

        // This is the old way... lets see if the below way improves the speed
        // $ctx.thread.stack.truncate(last_index);
        // $ctx.thread.stack.push(result);

        $ctx.thread.stack.truncate(last_index + 1);
        *$ctx.thread.stack.last_mut().unwrap() = result;

        $ctx.ip += 2;
    }};
}

pub(super) fn lt_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, lt_primitive, payload);
    Ok(())
}

pub(super) fn gt_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, gt_primitive, payload);
    Ok(())
}

pub(super) fn gte_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, gte_primitive, payload);
    Ok(())
}

// OpCode::ALLOC
fn alloc_handler(_ctx: &mut VmCore<'_>) -> Result<()> {
    panic!("Deprecated now - this shouldn't be hit");
}

#[inline(always)]
fn gte_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    gte_handler_payload(ctx, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn gt_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    gt_handler_payload(ctx, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn lt_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    lt_handler_payload(ctx, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn lte_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    lte_handler_payload(ctx, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn void_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.thread.stack.push(SteelVal::Void);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn set_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_set(payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn pushconst_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let val = ctx.constants.get_value(payload_size.to_usize());
    ctx.thread.stack.push(val);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn push_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_push(payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn readlocal_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_local(payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn readcaptured_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_read_captures(payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn movereadlocal_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_move_local(payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn movereadlocal0_handler_impl(ctx: &mut VmCore<'_>) -> Result<()> {
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(offset);

    ctx.thread.stack.push(value);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn movereadlocal1_handler_impl(ctx: &mut VmCore) -> Result<()> {
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(offset + 1);
    ctx.thread.stack.push(value);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn movereadlocal2_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(offset + 2);

    ctx.thread.stack.push(value);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn movereadlocal3_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(offset + 3);

    ctx.thread.stack.push(value);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn setlocal_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_set_local(payload_size.to_usize());
    Ok(())
}

#[inline(always)]
fn loadint0_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.thread.stack.push(SteelVal::INT_ZERO);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn loadint1_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.thread.stack.push(SteelVal::INT_ONE);
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn loadint2_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.thread.stack.push(SteelVal::INT_TWO);
    ctx.ip += 1;
    Ok(())
}

pub(crate) extern "C-unwind" fn callglobal_handler_deopt_c(ctx: *mut VmCore) -> u8 {
    unsafe { callglobal_handler_deopt(&mut *ctx) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_handle_pop(ctx: *mut VmCore, value: SteelVal) {
    unsafe {
        let this = &mut *ctx;
        // println!("Native pop: {}", value);
        // println!("stack: {:?}", this.thread.stack);
        let res = this.handle_pop_pure_value(value);
        // println!("stack after pop: {:?}", this.thread.stack);
        this.is_native = false;
        this.result = res;
    }
}

// If its 1 -> all good
// if its 0 -> error
// if its 2 -> return
#[inline(always)]
fn callglobal_handler_deopt(ctx: &mut VmCore) -> u8 {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.ip += 1;
    let payload_size = ctx.instructions[ctx.ip].payload_size.to_usize();
    let func = ctx.thread.global_env.repl_lookup_idx(index.to_usize());

    // Deopt -> Meaning, check the return value if we're done - so we just
    // will eventually check the stashed error.
    let ret_value = match &func {
        SteelVal::Closure(_) | SteelVal::ContinuationFunction(_) => 0,
        _ => 1,
    };
    match ctx.handle_global_function_call(func, payload_size) {
        Ok(_) => return ret_value,
        Err(_) => return 0,
    }
}

// Equality... via the usual scheme? Otherwise this is gonna be an issue?
#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn num_equal_value(
    _: *mut VmCore,
    left: SteelVal,
    right: SteelVal,
) -> SteelVal {
    // println!("GETTING TO NUM EQUAL VALUE: {} - {}", left, right,);
    // unsafe { &mut *ctx }.ip += 2;

    if let Ok(b) = number_equality(&left, &right) {
        b
    } else {
        unreachable!()
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn num_equal_int(
    // _: *mut VmCore,
    left: SteelVal,
    right: SteelVal,
) -> SteelVal {
    // println!("GETTING TO NUM EQUAL VALUE: {} - {}", left, right,);
    // unsafe { &mut *ctx }.ip += 2;

    assert!(matches!(right, SteelVal::IntV(_) | SteelVal::BigNum(_)));

    if let Ok(b) = number_equality(&left, &right) {
        b
    } else {
        unreachable!()
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn equal_binop(
    _ctx: *mut VmCore,
    left: SteelVal,
    right: SteelVal,
) -> SteelVal {
    // println!("Calling equal binop: {} - {}", left, right);

    SteelVal::BoolV(left == right)
}

pub(crate) extern "C-unwind" fn num_equal_value_unboxed(
    _ctx: *mut VmCore,
    left: i128,
    right: i128,
) -> bool {
    // println!("Calling num equal value");

    unsafe {
        if let Ok(SteelVal::BoolV(b)) =
            number_equality(&std::mem::transmute(left), &std::mem::transmute(right))
        {
            b
        } else {
            unreachable!()
        }
    }
}

macro_rules! extern_binop {
    ($name:tt, $func:tt) => {
        #[allow(improper_ctypes_definitions)]
        pub(crate) extern "C-unwind" fn $name(
            _ctx: *mut VmCore,
            a: SteelVal,
            b: SteelVal,
        ) -> SteelVal {
            // println!("Calling with args: {} - {}", a, b);
            // unsafe { println!("Current stack: {:#?}", (&mut *ctx).thread.stack) };

            // unsafe { (&mut *ctx).ip += 2 };
            $func(&[a, b]).unwrap()
        }
    };
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
        let value = std::mem::replace(&mut self.thread.stack[index + offset], SteelVal::Void);
        self.ip += 1;
        return value;
    }

    // Check what the constant is. If it is a non heap allocated constant,
    // we can inline it as an immediate - and ignore the fetching from the constant
    // map.
    #[inline(always)]
    fn get_const(&mut self) -> SteelVal {
        let payload_size = self.instructions[self.ip].payload_size;
        let val = self.constants.get_value(payload_size.to_usize());
        self.ip += 1;
        return val;
    }

    #[inline(always)]
    fn get_const_index(&mut self, idx: usize) -> SteelVal {
        self.constants.get_value(idx)
    }
}

// Set up these for doing each of the handlers
// extern_binop!(extern_c_add_two, add_primitive);
#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_c_add_two(
    ctx: *mut VmCore,
    a: SteelVal,
    b: SteelVal,
) -> SteelVal {
    // println!("Calling add with: {} - {}", a, b);

    // let a = ManuallyDrop::new(a);
    // let b = ManuallyDrop::new(b);
    match add_two(&a, &b) {
        Ok(v) => v,
        Err(e) => {
            unsafe {
                let guard = &mut *ctx;
                guard.result = Some(Err(e));
                guard.is_native = false;
            }

            SteelVal::Void
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_c_sub_two_int(a: SteelVal, b: SteelVal) -> SteelVal {
    // let a = ManuallyDrop::new(a);
    let rhs = if let SteelVal::IntV(i) = b {
        i
    } else {
        panic!()
    };

    match a {
        SteelVal::IntV(l) => match l.checked_sub(rhs) {
            Some(x) => SteelVal::IntV(x),
            None => {
                let res = BigInt::from(l) - rhs;
                res.into_steelval().unwrap()
            }
        },
        SteelVal::BigNum(n) => {
            todo!("found big: {:?}", n);
        }
        _ => {
            todo!("{}", a)
        }
    }
}

extern_binop!(extern_c_sub_two, subtract_primitive);
// extern_binop!(extern_c_lt_two, lt_primitive);
// extern_binop!(extern_c_lte_two, lte_primitive);

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_c_lte_two(
    _ctx: *mut VmCore,
    a: SteelVal,
    b: SteelVal,
) -> SteelVal {
    // println!("lte two - {} <= {}", a, b);

    // let a = ManuallyDrop::new(a);
    // let b = ManuallyDrop::new(b);
    SteelVal::BoolV(a <= b)
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_c_lt_two(
    _ctx: *mut VmCore,
    a: SteelVal,
    b: SteelVal,
) -> SteelVal {
    // println!("lte two - {} <= {}", a, b);

    // let a = ManuallyDrop::new(a);
    // let b = ManuallyDrop::new(b);
    SteelVal::BoolV(a < b)
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_c_lte_two_int(a: SteelVal, b: SteelVal) -> SteelVal {
    assert!(matches!(b, SteelVal::IntV(_)));
    SteelVal::BoolV(a <= b)
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn extern_c_null_handler(a: SteelVal) -> SteelVal {
    let result = match a {
        SteelVal::ListV(l) => l.is_empty(),
        SteelVal::VectorV(v) => v.is_empty(),
        SteelVal::MutableVector(v) => {
            let ptr = v.strong_ptr();
            let guard = &ptr.read().value;
            guard.is_empty()
        }
        _ => false,
    };

    SteelVal::BoolV(result)
}

extern_binop!(extern_c_gt_two, gt_primitive);
extern_binop!(extern_c_gte_two, gte_primitive);
extern_binop!(extern_c_mult_two, multiply_primitive);
extern_binop!(extern_c_div_two, divide_primitive);

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn push_const_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_const() }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn push_const_value_index_c(
    ctx: *mut VmCore,
    index: usize,
) -> SteelVal {
    unsafe { (&mut *ctx).get_const_index(index) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn move_read_local_0_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(0) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn move_read_local_1_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(1) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn move_read_local_2_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(2) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn move_read_local_3_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).move_local_value(3) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn move_read_local_any_value_c(
    ctx: *mut VmCore,
    offset: usize,
) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    guard.move_local_value(offset)
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn read_local_0_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(0) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn read_local_1_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(1) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn read_local_2_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(2) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn read_local_3_value_c(ctx: *mut VmCore) -> SteelVal {
    unsafe { (&mut *ctx).get_local_value(3) }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn read_local_any_value_c(
    ctx: *mut VmCore,
    offset: usize,
) -> SteelVal {
    let guard = unsafe { &mut *ctx };
    guard.get_local_value(offset)
}

// Read the global value at the registered index
#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn push_global(ctx: *mut VmCore, index: usize) -> SteelVal {
    unsafe {
        let this = &mut *ctx;
        this.thread.global_env.repl_lookup_idx(index)
    }
}

#[inline(always)]
fn new_callglobal_tail_handler_deopt_test(
    ctx: &mut VmCore,
    index: usize,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    let func = ctx.thread.global_env.repl_lookup_idx(index);
    // println!("Calling global tail: {}", func);
    debug_assert!(ctx.is_native);
    // inspect(ctx, &[func.clone()]);
    // println!("What is left on the stack: {:#?}", ctx.thread.stack);

    // Deopt -> Meaning, check the return value if we're done - so we just
    // will eventually check the stashed error.
    let should_yield = match &func {
        // SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
        SteelVal::Closure(_)
        | SteelVal::ContinuationFunction(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::CustomStruct(_) => true,
        _ => false,
    };

    // println!("Should yield: {}", should_yield);

    if should_yield {
        // println!("Yielding: {:?}", args);
        ctx.ip = fallback_ip;
        ctx.is_native = false;
    } else {
        // println!("Not yielding");
        // ctx.ip += 1;
        // ctx.ip = fallback_ip + 1;

        ctx.ip = fallback_ip;
    }

    // dbg!(ctx.instructions[ctx.ip]);
    // println!("Calling global tail - should yield: {}", should_yield);

    match handle_global_tail_call_deopt_with_args(ctx, func, args) {
        Ok(v) => {
            // ctx.thread.stack.push(v);
            // return SteelVal::Void;
            return v;
        }
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));

            return SteelVal::Void;
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn callglobal_tail_handler_deopt_spilled(
    ctx: *mut VmCore,
    index: usize,
    fallback_ip: usize,
    arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let func = ctx.thread.global_env.repl_lookup_idx(index);
    let should_yield = match &func {
        SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
        SteelVal::Closure(_)
        | SteelVal::ContinuationFunction(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::CustomStruct(_) => true,
        _ => false,
    };

    if should_yield {
        ctx.ip = fallback_ip;
        ctx.is_native = false;
    } else {
        ctx.ip = fallback_ip;
        // ctx.ip += 1;
    }

    match handle_global_tail_call_deopt_spilled(ctx, func, arity) {
        Ok(v) => {
            return v;
        }
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));

            return SteelVal::Void;
        }
    }
}

#[inline(always)]
fn handle_global_tail_call_deopt_with_args(
    ctx: &mut VmCore,
    stack_func: SteelVal,
    args: &mut [SteelVal],
) -> Result<SteelVal> {
    match stack_func {
        SteelVal::FuncV(func) => func(args).map_err(|x| x.set_span_if_none(ctx.current_span())),
        SteelVal::BoxedFunction(func) => {
            func.func()(args).map_err(|x| x.set_span_if_none(ctx.current_span()))
        }
        SteelVal::MutFunc(func) => func(args).map_err(|x| x.set_span_if_none(ctx.current_span())),

        SteelVal::Closure(closure) => {
            let arity = args.len();
            // Just put them all on the stack
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }

            // TODO:
            // Just write directly to the earlier spot.
            // println!("stack before calling closure: {:#?}", ctx.thread.stack);

            // We're going to de-opt in this case - unless we intend to do some fun inlining business
            ctx.new_handle_tail_call_closure(closure, arity)?;
            Ok(SteelVal::Void)
        }

        // This is probably no good here anyway
        SteelVal::ContinuationFunction(cc) => {
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }
            ctx.call_continuation(cc)?;
            Ok(SteelVal::Void)
        }
        SteelVal::BuiltIn(f) => {
            let len = args.len();
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }
            ctx.call_builtin_func(f, len)?;
            Ok(SteelVal::Void)
        }
        SteelVal::CustomStruct(s) => {
            let len = args.len();
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }

            ctx.call_custom_struct(&s, len)?;

            Ok(SteelVal::Void)
        }

        // Literaly anything else, just push on to the stack
        // and fall back to the main loop?
        _ => {
            cold();
            stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); ctx.current_span());
        }
    }
}

#[inline(always)]
fn handle_global_tail_call_deopt_spilled(
    ctx: &mut VmCore,
    stack_func: SteelVal,
    arity: usize,
) -> Result<SteelVal> {
    match stack_func {
        SteelVal::FuncV(func) => func(&ctx.thread.stack[ctx.thread.stack.len() - arity..])
            .map_err(|x| x.set_span_if_none(ctx.current_span())),
        SteelVal::BoxedFunction(func) => {
            func.func()(&ctx.thread.stack[ctx.thread.stack.len() - arity..])
                .map_err(|x| x.set_span_if_none(ctx.current_span()))
        }
        SteelVal::MutFunc(func) => {
            let len = ctx.thread.stack.len();
            func(&mut ctx.thread.stack[len - arity..])
                .map_err(|x| x.set_span_if_none(ctx.current_span()))
        }

        SteelVal::Closure(closure) => {
            // TODO:
            // Just write directly to the earlier spot.
            // println!("stack before calling closure: {:#?}", ctx.thread.stack);

            // We're going to de-opt in this case - unless we intend to do some fun inlining business
            ctx.new_handle_tail_call_closure(closure, arity)?;
            Ok(SteelVal::Void)
        }

        // This is probably no good here anyway
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

        // Literaly anything else, just push on to the stack
        // and fall back to the main loop?
        _ => {
            cold();
            stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); ctx.current_span());
        }
    }
}

#[inline(always)]
fn handle_global_function_call_with_args(
    ctx: &mut VmCore,
    stack_func: SteelVal,
    args: &mut [SteelVal],
) -> Result<SteelVal> {
    match stack_func {
        SteelVal::FuncV(func) => func(args).map_err(|x| x.set_span_if_none(ctx.current_span())),
        SteelVal::BoxedFunction(func) => {
            func.func()(args).map_err(|x| x.set_span_if_none(ctx.current_span()))
        }
        SteelVal::MutFunc(func) => func(args).map_err(|x| x.set_span_if_none(ctx.current_span())),
        SteelVal::Closure(closure) => {
            let arity = args.len();
            // Just put them all on the stack
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }

            if TRAMPOLINE {
                // We're going to de-opt in this case - unless we intend to do some fun inlining business
                if let Some(func) = closure.0.super_instructions.as_ref().copied() {
                    let pop_count = ctx.pop_count;
                    let depth = ctx.thread.stack_frames.len();

                    // inspect_impl(ctx, &[SteelVal::Closure(closure.clone())]);

                    ctx.handle_function_call_closure_jit(closure, arity)
                        .unwrap();

                    (func)(ctx);

                    if ctx.is_native {
                        // if ctx.pop_count != pop_count {
                        // println!("---------------------------------------------");

                        // let last_func =
                        //     ctx.thread.stack_frames.last().unwrap().function.clone();

                        // inspect_impl(ctx, &[SteelVal::Closure(last_func)]);
                        // }

                        debug_assert_eq!(ctx.pop_count, pop_count);
                        debug_assert_eq!(ctx.thread.stack_frames.len(), depth);
                        // Don't deopt?
                        Ok(ctx.thread.stack.pop().unwrap())
                    } else {
                        // println!("Deopting, pushing void to stack");

                        Ok(SteelVal::Void)
                    }
                } else {
                    // We're going to de-opt in this case - unless we intend to do some fun inlining business
                    ctx.handle_function_call_closure_jit(closure, arity)?;
                    Ok(SteelVal::Void)
                }
            } else {
                // We're going to de-opt in this case - unless we intend to do some fun inlining business
                ctx.handle_function_call_closure_jit(closure, arity)?;
                Ok(SteelVal::Void)
            }
        }

        // This is probably no good here anyway
        SteelVal::ContinuationFunction(cc) => {
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }
            ctx.call_continuation(cc)?;
            Ok(SteelVal::Void)
        }
        SteelVal::BuiltIn(f) => {
            let len = args.len();
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }
            ctx.call_builtin_func(f, len)?;
            Ok(SteelVal::Void)
        }
        SteelVal::CustomStruct(s) => {
            let len = args.len();
            for val in args {
                ctx.thread
                    .stack
                    .push(std::mem::replace(val, SteelVal::Void));
            }

            ctx.call_custom_struct(&s, len)?;

            Ok(SteelVal::Void)
        }
        _ => {
            cold();
            stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); ctx.current_span());
        }
    }
}

// #[inline(always)]
// fn handle_global_function_call_with_args_no_arity(
//     ctx: &mut VmCore,
//     stack_func: SteelVal,
//     mut args: SmallVec<[SteelVal; 5]>,
// ) -> Result<SteelVal> {
//     match stack_func {
//         SteelVal::FuncV(func) => func(&args).map_err(|x| x.set_span_if_none(ctx.current_span())),
//         SteelVal::BoxedFunction(func) => {
//             func.func()(&args).map_err(|x| x.set_span_if_none(ctx.current_span()))
//         }
//         SteelVal::MutFunc(func) => {
//             func(&mut args).map_err(|x| x.set_span_if_none(ctx.current_span()))
//         }
//         SteelVal::Closure(closure) => {
//             // Just put them all on the stack
//             ctx.thread.stack.extend(args.into_iter());

//             if TRAMPOLINE {
//                 if let Some(func) = closure.0.super_instructions.as_ref().copied() {
//                     let pop_count = ctx.pop_count;
//                     let depth = ctx.thread.stack_frames.len();

//                     // Just call handle_function_call_closure_jit
//                     // But then just directly invoke the super instruction
//                     // and snag the return type. That way we don't have
//                     // to yield right away, but we can instead
//                     // just jump in to calling the function.

//                     // dbg!(&ctx.thread.stack);

//                     // println!("Calling trampoline: {}", depth);

//                     // Install the function, so that way we can just trampoline
//                     // without needing to spill the stack
//                     ctx.handle_function_call_closure_jit_no_arity(closure)
//                         .unwrap();

//                     (func)(ctx);

//                     if ctx.is_native {
//                         debug_assert_eq!(ctx.pop_count, pop_count);
//                         debug_assert_eq!(ctx.thread.stack_frames.len(), depth);

//                         // Don't deopt?
//                         Ok(ctx.thread.stack.pop().unwrap())
//                     } else {
//                         Ok(SteelVal::Void)
//                     }
//                 } else {
//                     // We're going to de-opt in this case - unless we intend to do some fun inlining business
//                     ctx.handle_function_call_closure_jit_no_arity(closure)?;
//                     Ok(SteelVal::Void)
//                 }
//             } else {
//                 ctx.handle_function_call_closure_jit_no_arity(closure)?;
//                 Ok(SteelVal::Void)
//             }
//         }

//         // This is probably no good here anyway
//         SteelVal::ContinuationFunction(cc) => {
//             ctx.call_continuation(cc)?;
//             Ok(SteelVal::Void)
//         }
//         SteelVal::BuiltIn(f) => {
//             ctx.call_builtin_func(f, args.len())?;
//             Ok(SteelVal::Void)
//         }
//         _ => {
//             cold();
//             stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); ctx.current_span());
//         }
//     }
// }

#[inline(always)]
pub(crate) extern "C-unwind" fn should_continue(ctx: *mut VmCore) -> bool {
    dbg!(unsafe { &mut *ctx }.is_native)
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn push_to_vm_stack(ctx: *mut VmCore, value: SteelVal) {
    // println!("Pushing to vm stack: {}", value);
    unsafe {
        (&mut *ctx).thread.stack.push(value);
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn _push_to_vm_stack_function_spill(
    ctx: *mut VmCore,
    value: SteelVal,
) {
    // println!("Pushing to vm stack in function call spill: {}", value);
    unsafe {
        (&mut *ctx).thread.stack.push(value);
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn push_to_vm_stack_let_var(ctx: *mut VmCore, value: SteelVal) {
    // println!("Pushing to vm stack in spill: {}", value);
    // println!(
    //     "stack before pushing: {:#?}",
    //     unsafe { &mut *ctx }.thread.stack
    // );
    unsafe {
        (&mut *ctx).thread.stack.push(value);
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn push_to_vm_stack_two(
    ctx: *mut VmCore,
    value: SteelVal,
    value2: SteelVal,
) {
    unsafe {
        let guard = &mut *ctx;
        guard.thread.stack.reserve_exact(2);
        guard.thread.stack.push(value);
        guard.thread.stack.push(value2);
    }
}

// TODO:
// This is tricky, since we need to remove a range from the stack, where the value on the stack
// could be genuinely there, versus on the native stack.
//
// We could have it so that we don't remove the last value?
// Or pass in the
pub(crate) extern "C-unwind" fn let_end_scope_c(ctx: *mut VmCore, beginning_scope: usize) {
    let ctx = unsafe { &mut *ctx };
    let offset = ctx.get_offset();

    // Move to the pop
    // ctx.ip += 1;

    let rollback_index = beginning_scope + offset;

    // println!("Calling let end scope c");
    // println!("Stack: {:?}", ctx.thread.stack);
    // println!("Dropping: {:?}", ctx.thread.stack.get(rollback_index..));

    let _ = ctx.thread.stack.truncate(rollback_index);
}

macro_rules! make_call_global_function_tail_deopt {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct CallGlobalTailFunctionDefinitions;

        impl CallGlobalTailFunctionDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(

            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                lookup_index: usize,
                fallback_ip: usize,
                $($typ: SteelVal),*
            ) -> SteelVal {
                unsafe { new_callglobal_tail_handler_deopt_test(&mut *ctx, lookup_index, fallback_ip, &mut [$($typ), *]) }
            }

        )*
    };
}

make_call_global_function_tail_deopt!(
    (call_global_function_tail_deopt_0,),
    (call_global_function_tail_deopt_1, a),
    (call_global_function_tail_deopt_2, a, b),
    (call_global_function_tail_deopt_3, a, b, c),
    (call_global_function_tail_deopt_4, a, b, c, d),
    (call_global_function_tail_deopt_5, a, b, c, d, e),
    (call_global_function_tail_deopt_6, a, b, c, d, e, f),
    (call_global_function_tail_deopt_7, a, b, c, d, e, f, g),
    (call_global_function_tail_deopt_8, a, b, c, d, e, f, g, h)
);

macro_rules! make_list_handlers {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct ListHandlerDefinitions;

        impl ListHandlerDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(*mut VmCore, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(

            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                _: *mut VmCore,
                $($typ: SteelVal),*
            ) -> SteelVal {
                // unsafe { new_callglobal_tail_handler_deopt_test(&mut *ctx, lookup_index, fallback_ip, &mut [$($typ), *]) }

                SteelVal::ListV(vec![$($typ),*].into())
            }

        )*
    };
}

make_list_handlers!(
    (make_list_0,),
    (make_list_1, a),
    (make_list_2, a, b),
    (make_list_3, a, b, c),
    (make_list_4, a, b, c, d),
    (make_list_5, a, b, c, d, e),
    (make_list_6, a, b, c, d, e, f),
    (make_list_7, a, b, c, d, e, f, g),
    (make_list_8, a, b, c, d, e, f, g, h),
    (make_list_9, a, b, c, d, e, f, g, h, i),
    (make_list_10, a, b, c, d, e, f, g, h, i, j),
    (make_list_11, a, b, c, d, e, f, g, h, i, j, k),
    (make_list_12, a, b, c, d, e, f, g, h, i, j, k, l),
    (make_list_13, a, b, c, d, e, f, g, h, i, j, k, l, m),
    (make_list_14, a, b, c, d, e, f, g, h, i, j, k, l, m, n),
    (make_list_15, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o),
    (make_list_16, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
);

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn list_handler_c(ctx: *mut VmCore<'_>, payload: usize) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    let last_index = ctx.thread.stack.len() - payload;
    let remaining = ctx.thread.stack.split_off(last_index);
    SteelVal::ListV(remaining.into())
}

// Note: This should only get called with a closure, so we are safe to only
// check against those.
pub(crate) extern "C-unwind" fn should_spill(ctx: *mut VmCore, lookup_index: usize) -> bool {
    unsafe {
        let this = &mut *ctx;
        let func = &this.thread.global_env.roots()[lookup_index];

        matches!(func, SteelVal::Closure(_))
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn should_spill_value(_: *mut VmCore, func: SteelVal) -> bool {
    // println!("Checking if this value should be spilled!: {}", func);
    let func = ManuallyDrop::new(func);
    matches!(&*func, SteelVal::Closure(_))
}

// 0 -> No good
// 1 -> Spill
// 2 -> Call and Spill
pub(crate) extern "C-unwind" fn check_callable_spill(ctx: *mut VmCore, lookup_index: usize) -> u8 {
    unsafe {
        let this = &mut *ctx;
        let func = &this.thread.global_env.roots()[lookup_index];

        if TRAMPOLINE {
            if let SteelVal::Closure(c) = func {
                if c.0.super_instructions.as_ref().is_some() {
                    return 2;
                }
            }
        }

        // Builtins can yield control in a funky way.
        if matches!(
            func,
            SteelVal::Closure(_)
                | SteelVal::ContinuationFunction(_)
                | SteelVal::BuiltIn(_)
                | SteelVal::CustomStruct(_)
        ) {
            1
        } else {
            0
        }
    }
}

// Just check if this thing is callable. If its not, just spill everything up to the args
// to the stack, and otherwise don't continue?
pub(crate) extern "C-unwind" fn check_callable(ctx: *mut VmCore, lookup_index: usize) -> bool {
    // Check that the function we're calling is in fact something callable via native code.
    // We'll want to spill the stack otherwise.
    unsafe {
        let this = &mut *ctx;
        let func = &this.thread.global_env.roots()[lookup_index];

        if TRAMPOLINE {
            if let SteelVal::Closure(c) = func {
                if c.0.super_instructions.as_ref().is_some() {
                    return true;
                }
            }
        }

        // Builtins can yield control in a funky way.
        !matches!(
            func,
            SteelVal::Closure(_)
                | SteelVal::ContinuationFunction(_)
                | SteelVal::BuiltIn(_)
                | SteelVal::CustomStruct(_)
        )
    }
}

pub(crate) extern "C-unwind" fn check_callable_tail(ctx: *mut VmCore, lookup_index: usize) -> bool {
    // Check that the function we're calling is in fact something callable via native code.
    // We'll want to spill the stack otherwise.
    unsafe {
        let this = &mut *ctx;
        let func = &this.thread.global_env.roots()[lookup_index];

        // println!("Checking if callable in tail: {}", func);

        // Builtins can yield control in a funky way.
        !matches!(
            func,
            SteelVal::Closure(_)
                | SteelVal::ContinuationFunction(_)
                | SteelVal::BuiltIn(_)
                | SteelVal::CustomStruct(_)
        )
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn check_callable_value(_: *mut VmCore, func: SteelVal) -> bool {
    // Check that the function we're calling is in fact something callable via native code.
    // We'll want to spill the stack otherwise.
    let func = ManuallyDrop::new(func);

    if TRAMPOLINE {
        if let SteelVal::Closure(c) = &*func {
            if c.0.super_instructions.as_ref().is_some() {
                return true;
            }
        }
    }

    // Builtins can yield control in a funky way.
    !matches!(
        &*func,
        SteelVal::Closure(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::BuiltIn(_)
            | SteelVal::CustomStruct(_)
    )
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn check_callable_value_tail(_: *mut VmCore, func: SteelVal) -> bool {
    // Check that the function we're calling is in fact something callable via native code.
    // We'll want to spill the stack otherwise.
    let func = ManuallyDrop::new(func);

    // Builtins can yield control in a funky way.
    !matches!(
        &*func,
        SteelVal::Closure(_)
            | SteelVal::ContinuationFunction(_)
            | SteelVal::BuiltIn(_)
            | SteelVal::CustomStruct(_)
    )
}

// Directly call and get the result of the next function
// TODO: Directly call the function since we know that its going to be hanging around.
// Which means we can generate two different forms of the function, and we don't need
// to emit a pop on it since its going to be within itself?
#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn trampoline(
    ctx: *mut VmCore,
    arity: usize,
    lookup_index: usize,
) -> SteelVal {
    unsafe {
        let this = &mut *ctx;
        let func = this.thread.global_env.repl_lookup_idx(lookup_index);
        // println!("Calling trampoline");
        // Builtins can yield control in a funky way.
        if let SteelVal::Closure(c) = func {
            if let Some(func) = c.0.super_instructions.as_ref().copied() {
                // Just call handle_function_call_closure_jit
                // But then just directly invoke the super instruction
                // and snag the return type. That way we don't have
                // to yield right away, but we can instead
                // just jump in to calling the function.

                // Install the function, so that way we can just trampoline
                // without needing to spill the stack
                this.handle_function_call_closure_jit(c, arity).unwrap();

                (func)(this);

                dbg!(this.is_native);

                // Don't deopt?
                this.thread.stack.pop().unwrap()
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }
}

#[allow(improper_ctypes_definitions)]
pub(crate) extern "C-unwind" fn trampoline_no_arity(
    ctx: *mut VmCore,
    lookup_index: usize,
) -> SteelVal {
    unsafe {
        let this = &mut *ctx;
        let func = this.thread.global_env.repl_lookup_idx(lookup_index);
        // println!("Calling trampoline");
        // Builtins can yield control in a funky way.
        if let SteelVal::Closure(c) = func {
            if let Some(func) = c.0.super_instructions.as_ref().copied() {
                // Just call handle_function_call_closure_jit
                // But then just directly invoke the super instruction
                // and snag the return type. That way we don't have
                // to yield right away, but we can instead
                // just jump in to calling the function.

                // Install the function, so that way we can just trampoline
                // without needing to spill the stack
                this.handle_function_call_closure_jit_no_arity(c).unwrap();

                (func)(this);

                dbg!(this.is_native);

                // Don't deopt?
                this.thread.stack.pop().unwrap()
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    }
}

macro_rules! make_call_global_function_deopt {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct CallGlobalFunctionDefinitions;

        impl CallGlobalFunctionDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(
            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                lookup_index: usize,
                fallback_ip: usize,
                $($typ: SteelVal),*
            ) -> SteelVal {
                unsafe { call_global_function_deopt(&mut *ctx, lookup_index, fallback_ip, &mut [$($typ), *]) }
            }
        )*
    };
}

make_call_global_function_deopt!(
    (call_global_function_deopt_0,),
    (call_global_function_deopt_1, a),
    (call_global_function_deopt_2, a, b),
    (call_global_function_deopt_3, a, b, c),
    (call_global_function_deopt_4, a, b, c, d),
    (call_global_function_deopt_5, a, b, c, d, e),
    (call_global_function_deopt_6, a, b, c, d, e, f),
    (call_global_function_deopt_7, a, b, c, d, e, f, g),
    (call_global_function_deopt_8, a, b, c, d, e, f, g, h)
);

macro_rules! make_call_function_deopt {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct CallFunctionDefinitions;

        impl CallFunctionDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, func: SteelVal, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(
            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                func: SteelVal,
                fallback_ip: usize,
                $($typ: SteelVal),*
            ) -> SteelVal {
                unsafe { call_function_deopt(&mut *ctx, func, fallback_ip, &mut [$($typ), *]) }
            }
        )*
    };
}

make_call_function_deopt!(
    (call_function_deopt_0,),
    (call_function_deopt_1, a),
    (call_function_deopt_2, a, b),
    (call_function_deopt_3, a, b, c),
    (call_function_deopt_4, a, b, c, d),
    (call_function_deopt_5, a, b, c, d, e),
    (call_function_deopt_6, a, b, c, d, e, f),
    (call_function_deopt_7, a, b, c, d, e, f, g),
    (call_function_deopt_8, a, b, c, d, e, f, g, h)
);

macro_rules! make_call_function_tail_deopt {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct CallFunctionTailDefinitions;

        impl CallFunctionTailDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, func: SteelVal, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(
            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                func: SteelVal,
                fallback_ip: usize,
                $($typ: SteelVal),*
            ) -> SteelVal {
                unsafe { call_function_tail_deopt(&mut *ctx, func, fallback_ip, &mut [$($typ), *]) }
            }
        )*
    };
}

make_call_function_tail_deopt!(
    (call_function_tail_deopt_0,),
    (call_function_tail_deopt_1, a),
    (call_function_tail_deopt_2, a, b),
    (call_function_tail_deopt_3, a, b, c),
    (call_function_tail_deopt_4, a, b, c, d),
    (call_function_tail_deopt_5, a, b, c, d, e),
    (call_function_tail_deopt_6, a, b, c, d, e, f),
    (call_function_tail_deopt_7, a, b, c, d, e, f, g),
    (call_function_tail_deopt_8, a, b, c, d, e, f, g, h)
);

macro_rules! make_call_global_function_deopt_no_arity {
    ($(($name:tt, $($typ:ident),*)),*) => {

        pub struct CallGlobalNoArityFunctionDefinitions;

        impl CallGlobalNoArityFunctionDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, lookup_index: usize, fallback_ip: usize, $($typ: SteelVal),*) -> SteelVal
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(

            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                lookup_index: usize,
                fallback_ip: usize,
                $($typ: SteelVal),*
            ) -> SteelVal {
                let ctx = unsafe { &mut *ctx };
                debug_assert!(ctx.is_native);

                let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);

                // Deopt -> Meaning, check the return value if we're done - so we just
                // will eventually check the stashed error.
                let should_yield = match &func {
                    SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
                    SteelVal::Closure(_) | SteelVal::ContinuationFunction(_) | SteelVal::BuiltIn(_) | SteelVal::CustomStruct(_) => true,
                    _ => false,
                };

                debug_assert!(ctx.is_native);

                if should_yield {
                    ctx.ip = fallback_ip;
                    ctx.is_native = false;
                } else {
                    ctx.ip = fallback_ip;
                }


                fn inner_handle_global_function_call_with_args_no_arity(
                    ctx: &mut VmCore,
                    stack_func: SteelVal,
                    $($typ: SteelVal),*
                ) -> Result<SteelVal> {
                    match stack_func {
                        SteelVal::FuncV(func) => {
                            let args = [$($typ),*];
                            func(&args).map_err(|x| x.set_span_if_none(ctx.current_span()))
                        },
                        SteelVal::BoxedFunction(func) => {
                            let args = [$($typ),*];
                            func.func()(&args).map_err(|x| x.set_span_if_none(ctx.current_span()))
                        }
                        SteelVal::MutFunc(func) => {
                            let mut args = [$($typ),*];
                            func(&mut args).map_err(|x| x.set_span_if_none(ctx.current_span()))
                        }
                        SteelVal::Closure(closure) => {
                            // TODO: Consider reserving the amount?
                            $(
                                ctx.thread.stack.push($typ);
                            )*

                            if TRAMPOLINE {
                                if let Some(func) = closure.0.super_instructions.as_ref().copied() {
                                    let pop_count = ctx.pop_count;
                                    let depth = ctx.thread.stack_frames.len();

                                    // println!("Calling function from context @ ip: {}", ctx.ip);

                                    // if let Some(last) = ctx.thread.stack_frames.last().map(|x| x.function.clone()) {
                                        // inspect(ctx, &[SteelVal::Closure(last)]);
                                    // }


                                    // Install the function, so that way we can just trampoline
                                    // without needing to spill the stack
                                    ctx.handle_function_call_closure_jit_no_arity(closure.clone())
                                        .unwrap();

                                    (func)(ctx);

                                    if ctx.is_native {
                                        // if ctx.pop_count != pop_count {

                                            // println!("Just called");
                                            // inspect(ctx, &[SteelVal::Closure(closure)]);


                                            // println!("{}", ctx.ip);
                                            // println!("{:?}", ctx.instructions[ctx.ip]);
                                        // }
                                        debug_assert_eq!(ctx.pop_count, pop_count);
                                        debug_assert_eq!(ctx.thread.stack_frames.len(), depth);

                                        // Don't deopt?
                                        Ok(ctx.thread.stack.pop().unwrap())
                                    } else {
                                        Ok(SteelVal::Void)
                                    }
                                } else {
                                    // We're going to de-opt in this case - unless we intend to do some fun inlining business
                                    ctx.handle_function_call_closure_jit_no_arity(closure)?;
                                    Ok(SteelVal::Void)
                                }
                            } else {
                                ctx.handle_function_call_closure_jit_no_arity(closure)?;
                                Ok(SteelVal::Void)
                            }
                        }

                        // This is probably no good here anyway
                        SteelVal::ContinuationFunction(cc) => {
                            $(
                                ctx.thread.stack.push($typ);
                            )*
                            ctx.call_continuation(cc)?;
                            Ok(SteelVal::Void)
                        }
                        SteelVal::BuiltIn(f) => {
                            let args: [SteelVal; _] = [$($typ),*];
                            let len = args.len();

                            for arg in args {
                                ctx.thread.stack.push(arg);
                            }

                            ctx.call_builtin_func(f, len)?;
                            Ok(SteelVal::Void)
                        }
                        SteelVal::CustomStruct(s) => {
                            let args: [SteelVal; _] = [$($typ),*];
                            let len = args.len();
                            for val in args {
                                ctx.thread
                                    .stack
                                    .push(val);
                            }

                            ctx.call_custom_struct(&s, len)?;

                            Ok(SteelVal::Void)
                        }
                        _ => {
                            cold();
                            stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); ctx.current_span());
                        }
                    }
                }


                match inner_handle_global_function_call_with_args_no_arity(ctx, func, $($typ),*) {
                    Ok(v) => v,
                    Err(e) => {
                        ctx.is_native = false;
                        ctx.result = Some(Err(e));
                        return SteelVal::Void;
                    }
                }
            }

        )*
    };
}

make_call_global_function_deopt_no_arity!(
    (call_global_function_deopt_0_no_arity,),
    (call_global_function_deopt_1_no_arity, a),
    (call_global_function_deopt_2_no_arity, a, b),
    (call_global_function_deopt_3_no_arity, a, b, c),
    (call_global_function_deopt_4_no_arity, a, b, c, d),
    (call_global_function_deopt_5_no_arity, a, b, c, d, e),
    (call_global_function_deopt_6_no_arity, a, b, c, d, e, f),
    (call_global_function_deopt_7_no_arity, a, b, c, d, e, f, g),
    (
        call_global_function_deopt_8_no_arity,
        a,
        b,
        c,
        d,
        e,
        f,
        g,
        h
    )
);

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn call_global_function_deopt_no_arity_spilled(
    ctx: *mut VmCore,
    lookup_index: usize,
    fallback_ip: usize,
    arity: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    debug_assert!(ctx.is_native);

    let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);

    // Deopt -> Meaning, check the return value if we're done - so we just
    // will eventually check the stashed error.
    let should_yield = match &func {
        SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
        SteelVal::Closure(_)
        | SteelVal::ContinuationFunction(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::CustomStruct(_) => true,
        _ => false,
    };

    if should_yield {
        ctx.ip = fallback_ip;
        ctx.is_native = false;
    } else {
        ctx.ip = fallback_ip;
    }

    fn inner_handle_global_function_call_with_args_no_arity(
        ctx: &mut VmCore,
        stack_func: SteelVal,
        arity: usize,
    ) -> Result<SteelVal> {
        match stack_func {
            // TODO: Enter safepoint?
            SteelVal::FuncV(func) => func(&ctx.thread.stack[ctx.thread.stack.len() - arity..])
                .map_err(|x| x.set_span_if_none(ctx.current_span())),
            SteelVal::BoxedFunction(func) => {
                func.func()(&ctx.thread.stack[ctx.thread.stack.len() - arity..])
                    .map_err(|x| x.set_span_if_none(ctx.current_span()))
            }
            SteelVal::MutFunc(func) => {
                let len = ctx.thread.stack.len();
                func(&mut ctx.thread.stack[len - arity..])
                    .map_err(|x| x.set_span_if_none(ctx.current_span()))
            }
            SteelVal::Closure(closure) => {
                // TODO: Consider reserving the amount?

                if TRAMPOLINE {
                    if let Some(func) = closure.0.super_instructions.as_ref().copied() {
                        let pop_count = ctx.pop_count;
                        let depth = ctx.thread.stack_frames.len();

                        // Install the function, so that way we can just trampoline
                        // without needing to spill the stack
                        ctx.handle_function_call_closure_jit_no_arity(closure)
                            .unwrap();

                        (func)(ctx);

                        if ctx.is_native {
                            debug_assert_eq!(ctx.pop_count, pop_count);
                            debug_assert_eq!(ctx.thread.stack_frames.len(), depth);

                            // Don't deopt?
                            Ok(ctx.thread.stack.pop().unwrap())
                        } else {
                            Ok(SteelVal::Void)
                        }
                    } else {
                        // We're going to de-opt in this case - unless we intend to do some fun inlining business
                        ctx.handle_function_call_closure_jit_no_arity(closure)?;
                        Ok(SteelVal::Void)
                    }
                } else {
                    ctx.handle_function_call_closure_jit_no_arity(closure)?;
                    Ok(SteelVal::Void)
                }
            }

            // This is probably no good here anyway
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

            _ => {
                cold();
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); ctx.current_span());
            }
        }
    }

    match inner_handle_global_function_call_with_args_no_arity(ctx, func, arity) {
        Ok(v) => v,
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));
            return SteelVal::Void;
        }
    }
}

#[inline(always)]
fn call_global_function_deopt(
    ctx: &mut VmCore,
    lookup_index: usize,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);
    debug_assert!(ctx.is_native);

    // println!("Calling function: {}", func);

    // Deopt -> Meaning, check the return value if we're done - so we just
    // will eventually check the stashed error.
    match &func {
        SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => {
            ctx.ip = fallback_ip;
        }
        SteelVal::Closure(_)
        | SteelVal::ContinuationFunction(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::CustomStruct(_) => {
            ctx.ip = fallback_ip;
            ctx.is_native = false;
        }
        _ => {
            ctx.ip = fallback_ip;
        }
    };

    match handle_global_function_call_with_args(ctx, func, args) {
        Ok(v) => v,
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));
            return SteelVal::Void;
        }
    }
}

// Either... return a value, or deopt and yield control back to the runtime.
// How do we signal to yield back to the runtime?
#[inline(always)]
fn call_function_deopt(
    ctx: &mut VmCore,
    func: SteelVal,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    debug_assert!(ctx.is_native);
    // println!("Calling function off of the stack: {}", func);

    // Deopt -> Meaning, check the return value if we're done - so we just
    // will eventually check the stashed error.
    let should_yield = match &func {
        SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
        SteelVal::Closure(_)
        | SteelVal::ContinuationFunction(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::CustomStruct(_) => true,
        _ => false,
    };

    // println!("Deopting...: {} - {:?}", func, args);
    // println!("Stack: {:?}", ctx.thread.stack);

    if should_yield {
        ctx.ip = fallback_ip - 1;
        // println!("------------------------");

        // println!("Prev instruction: {:?}", ctx.instructions[ctx.ip - 1]);
        // println!("Fallback instruction: {:?}", ctx.instructions[ctx.ip]);
        // println!("After instruction: {:?}", ctx.instructions[ctx.ip + 1]);

        // pretty_print_dense_instructions(&ctx.instructions);

        // println!("------------------------");

        ctx.is_native = false;
    } else {
        ctx.ip = fallback_ip - 1;
        // println!(
        //     "not yield: Fallback instruction {}: {:?}",
        //     ctx.ip, ctx.instructions[ctx.ip]
        // );
        // pretty_print_dense_instructions(&ctx.instructions);
        // ctx.ip += 1;
    }

    match handle_global_function_call_with_args(ctx, func, args) {
        Ok(v) => v,
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));
            return SteelVal::Void;
        }
    }
}

#[inline(always)]
fn call_function_tail_deopt(
    ctx: &mut VmCore,
    func: SteelVal,
    fallback_ip: usize,
    args: &mut [SteelVal],
) -> SteelVal {
    debug_assert!(ctx.is_native);
    // Deopt -> Meaning, check the return value if we're done - so we just
    // will eventually check the stashed error.
    let should_yield = match &func {
        // SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
        SteelVal::Closure(_)
        | SteelVal::ContinuationFunction(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::CustomStruct(_) => true,
        _ => false,
    };

    if should_yield {
        // ctx.ip = fallback_ip - 1;
        // println!("Tail call: {:?}", ctx.instructions[ctx.ip]);
        ctx.ip = fallback_ip - 1;
        // println!("Tail call: {:?}", ctx.instructions[ctx.ip]);

        ctx.is_native = false;
    } else {
        // ctx.ip += 2;
        ctx.ip = fallback_ip - 1;
    }

    match handle_global_tail_call_deopt_with_args(ctx, func, args) {
        Ok(v) => v,
        Err(e) => {
            ctx.is_native = false;
            ctx.result = Some(Err(e));
            return SteelVal::Void;
        }
    }
}

// #[inline(always)]
// fn call_global_function_deopt_no_arity(
//     ctx: &mut VmCore,
//     lookup_index: usize,
//     fallback_ip: usize,
//     args: SmallVec<[SteelVal; 5]>,
// ) -> SteelVal {
//     let func = ctx.thread.global_env.repl_lookup_idx(lookup_index);
//     // Deopt -> Meaning, check the return value if we're done - so we just
//     // will eventually check the stashed error.
//     let should_yield = match &func {
//         SteelVal::Closure(c) if c.0.super_instructions.is_some() && TRAMPOLINE => false,
//         SteelVal::Closure(_) | SteelVal::ContinuationFunction(_) | SteelVal::BuiltIn(_) => true,
//         _ => false,
//     };
//     if should_yield {
//         ctx.ip = fallback_ip;
//         ctx.is_native = false;
//     } else {
//         ctx.ip = fallback_ip;
//     }
//     match handle_global_function_call_with_args_no_arity(ctx, func, args) {
//         Ok(v) => v,
//         Err(e) => {
//             ctx.is_native = false;
//             ctx.result = Some(Err(e));
//             return SteelVal::Void;
//         }
//     }
// }

#[inline(always)]
fn callglobal_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.ip += 1;
    let next_inst = ctx.instructions[ctx.ip];
    ctx.handle_call_global(payload_size.to_usize(), next_inst.payload_size.to_usize())?;
    Ok(())
}

#[inline(never)]
fn call_function_tco(
    stack_func: SteelVal,
    ctx: &mut VmCore<'_>,
    payload_size: usize,
) -> Result<bool> {
    use SteelVal::*;
    match stack_func {
        FuncV(f) => {
            let last_index = ctx.thread.stack.len() - payload_size;
            match ctx
                .thread
                .enter_safepoint(move |ctx: &SteelThread| f(&ctx.stack[last_index..]))
            {
                Ok(result) => {
                    // This is the old way... lets see if the below way improves the speed
                    ctx.thread.stack.truncate(last_index);
                    if let Some(r) = ctx.handle_pop_pure_value(result) {
                        // return r;
                        ctx.return_value = Some(r?);
                        return Ok(true);
                    }
                }
                Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
            };

            Ok(())
        }
        MutFunc(f) => ctx.call_primitive_mut_func(f, payload_size),
        BoxedFunction(f) => ctx.call_boxed_func(f.func(), payload_size),
        Closure(closure) => ctx.new_handle_tail_call_closure(closure, payload_size),
        BuiltIn(f) => ctx.call_builtin_func(f, payload_size),
        CustomStruct(s) => ctx.call_custom_struct(&s, payload_size),
        ContinuationFunction(cc) => ctx.call_continuation(cc),
        _ => {
            cold();
            stop!(BadSyntax => format!("TailCall - Application not a procedure or function type 
                    not supported: {stack_func}"); ctx.current_span());
        }
    }?;
    Ok(false)
}

// TODO: This needs to have a way to signal that we're not continuing.
// The API that we return probably needs to change;
#[inline(always)]
fn callglobaltail_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let next_inst = ctx.instructions[ctx.ip + 1];
    let stack_func = ctx
        .thread
        .global_env
        .repl_lookup_idx(payload_size.to_usize());
    ctx.ip += 1;
    let payload_size = next_inst.payload_size.to_usize();

    if call_function_tco(stack_func, ctx, payload_size)? {
        return Ok(());
    }

    Ok(())
}

#[inline(always)]
fn func_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let func = ctx.thread.stack.pop().unwrap();
    ctx.handle_function_call(func, payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn tailcall_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let func = ctx.thread.stack.pop().unwrap();
    ctx.handle_tail_call(func, payload_size.to_usize())?;
    Ok(())
}

#[inline(never)]
fn pop_test(ctx: &mut VmCore) -> bool {
    let test = ctx.thread.stack.pop().unwrap();
    test.is_truthy()
}

pub(crate) extern "C-unwind" fn if_handler_raw_value(_: *mut VmCore, value: i128) -> bool {
    // let ctx = unsafe { &mut *raw_ctx };
    let test: SteelVal = unsafe { std::mem::transmute(value) };
    test.is_truthy()

    // if result {
    //     ctx.ip += 1;
    // } else {
    //     ctx.ip = ctx.instructions[ctx.ip].payload_size.to_usize();
    // }

    // result
}

pub(crate) extern "C-unwind" fn not_handler_raw_value(_: *mut VmCore, value: SteelVal) -> SteelVal {
    SteelVal::BoolV(!value.is_truthy())
}

// Pop the value off?
pub(crate) extern "C-unwind" fn if_handler_value(raw_ctx: *mut VmCore) -> bool {
    let ctx = unsafe { &mut *raw_ctx };

    let test = ctx.thread.stack.pop().unwrap();
    let result = test.is_truthy();

    if result {
        ctx.ip += 1;
    } else {
        ctx.ip = ctx.instructions[ctx.ip].payload_size.to_usize();
    }

    result
}

#[inline(always)]
fn if_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    // let payload_size = ctx.instructions[ctx.ip].payload_size;
    let result = pop_test(ctx);
    // change to truthy...
    if result {
        ctx.ip += 1;
    } else {
        ctx.ip = ctx.instructions[ctx.ip].payload_size.to_usize();
    }

    Ok(())
}

pub(crate) extern "C-unwind" fn tcojmp_handler(ctx: *mut VmCore, current_arity: usize) {
    // println!("Calling self tail call");
    let this = unsafe { &mut *ctx };
    debug_assert!(this.is_native);

    // TODO: When this is done with the trampoline, we can do tail
    // call directly into it

    if let Err(e) = tco_jmp_handler_multi_arity(current_arity, this) {
        this.is_native = false;
        this.result = Some(Err(e));
    }

    this.is_native = false;
}

fn tco_jmp_handler_multi_arity(mut current_arity: usize, this: &mut VmCore<'_>) -> Result<()> {
    let last_stack_frame = this.thread.stack_frames.last().unwrap();

    this.ip = 0;

    // TODO: Adjust the stack for multiple arity functions
    let is_multi_arity = last_stack_frame.function.is_multi_arity;
    let original_arity = last_stack_frame.function.arity();
    let payload_size = current_arity;
    // let new_arity = &mut closure_arity;

    // TODO: Reuse the original list allocation, if it exists.
    if likely(!is_multi_arity) {
        if unlikely(original_arity != payload_size) {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", original_arity, payload_size); this.current_span());
        }
    } else {
        // println!(
        //     "multi closure function, multi arity, arity: {:?} - called with: {:?}",
        //     original_arity, payload_size
        // );

        if payload_size < original_arity - 1 {
            stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", original_arity - 1, payload_size); this.current_span());
        }

        // (define (test x . y))
        // (test 1 2 3 4 5)
        // in this case, arity = 2 and payload size = 5
        // pop off the last 4, collect into a list
        let amount_to_remove = 1 + payload_size - original_arity;

        let values = this
            .thread
            .stack
            .drain(this.thread.stack.len() - amount_to_remove..)
            .collect();

        let list = SteelVal::ListV(values);

        this.thread.stack.push(list);

        current_arity = original_arity;
    }

    // println!("Stack before: {:#?}", this.thread.stack);

    let back = this.thread.stack.len() - current_arity;
    let _ = this.thread.stack.drain(this.sp..back);

    // println!("Stack after: {:#?}", this.thread.stack);

    // println!("---- done with tail call handler ----");
    Ok(())
}

// Tail call back into the native code on the thing?
pub(crate) extern "C-unwind" fn self_tail_call_handler(ctx: *mut VmCore, arity: usize) {
    println!("Calling self tail call");
    let this = unsafe { &mut *ctx };
    this.ip = 0;
    let back = this.thread.stack.len() - arity;
    this.is_native = false;

    debug_assert_eq!(this.sp as u32, this.thread.stack_frames.last().unwrap().sp);

    let _ = this.thread.stack.drain(this.sp..back);
}

pub(crate) extern "C-unwind" fn self_tail_call_handler_loop(ctx: *mut VmCore, arity: usize) {
    // println!("Calling self tail call loop");
    let this = unsafe { &mut *ctx };
    // this.ip = 0;
    let back = this.thread.stack.len() - arity;
    let _ = this.thread.stack.drain(this.sp..back);
}

// macro_rules! count {
//     () => (0usize);
//     ( $x:tt $($xs:tt)* ) => (1usize + count!($($xs)*));
// }

macro_rules! make_self_tail_call_no_arity {
    ($(($name:tt, $(($typ:ident = $i:expr)),*)),*) => {

        pub struct CallSelfTailCallNoArityDefinitions;

        impl CallSelfTailCallNoArityDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, arity: usize, $($typ: SteelVal),*)
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(

            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                arity: usize,
                $($typ: SteelVal),*
            ) {
                let this = unsafe { &mut *ctx };
                this.ip = 0;
                this.is_native = false;

                $(
                    this.thread.stack[this.sp + $i] = $typ;
                )*

                this.thread.stack.truncate(this.sp + arity);
            }

        )*
    };
}

make_self_tail_call_no_arity!(
    (self_tail_call_no_arity_0,),
    (self_tail_call_no_arity_1, (a = 0)),
    (self_tail_call_no_arity_2, (a = 0), (b = 1)),
    (self_tail_call_no_arity_3, (a = 0), (b = 1), (c = 2)),
    (
        self_tail_call_no_arity_4,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3)
    ),
    (
        self_tail_call_no_arity_5,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4)
    ),
    (
        self_tail_call_no_arity_6,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4),
        (f = 5)
    ),
    (
        self_tail_call_no_arity_7,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4),
        (f = 5),
        (g = 6)
    ),
    (
        self_tail_call_no_arity_8,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4),
        (f = 5),
        (g = 6),
        (h = 7)
    )
);

macro_rules! make_self_tail_call_no_arity_loop {
    ($(($name:tt, $(($typ:ident = $i:expr)),*)),*) => {

        pub struct CallSelfTailCallNoArityLoopDefinitions;

        impl CallSelfTailCallNoArityLoopDefinitions {
            pub fn register(map: &mut crate::jit2::gen::FunctionMap) {
                $(
                    map.add_func(
                        stringify!($name),
                        $name as extern "C-unwind" fn(ctx: *mut VmCore, arity: usize, $($typ: SteelVal),*)
                    );
                )*
            }

            pub fn arity_to_name(count: usize) -> Option<&'static str> {
                $(
                    {
                        $(
                            let $typ = 0usize;
                        )*

                        let arr: &[usize] = &[$($typ),*];

                        if count == arr.len() {
                            return Some(stringify!($name));
                        }
                    }
                )*

                None
            }
        }

        $(

            #[allow(improper_ctypes_definitions)]
            pub(crate) extern "C-unwind" fn $name(
                ctx: *mut VmCore,
                arity: usize,
                $($typ: SteelVal),*
            ) {
                let this = unsafe { &mut *ctx };
                // this.ip = 0;
                // this.is_native = false;

                $(
                    this.thread.stack[this.sp + $i] = $typ;
                )*

                this.thread.stack.truncate(this.sp + arity);
            }

        )*
    };
}

make_self_tail_call_no_arity_loop!(
    (self_tail_call_no_arity_loop_0,),
    (self_tail_call_no_arity_loop_1, (a = 0)),
    (self_tail_call_no_arity_loop_2, (a = 0), (b = 1)),
    (self_tail_call_no_arity_loop_3, (a = 0), (b = 1), (c = 2)),
    (
        self_tail_call_no_arity_loop_4,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3)
    ),
    (
        self_tail_call_no_arity_loop_5,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4)
    ),
    (
        self_tail_call_no_arity_loop_6,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4),
        (f = 5)
    ),
    (
        self_tail_call_no_arity_loop_7,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4),
        (f = 5),
        (g = 6)
    ),
    (
        self_tail_call_no_arity_loop_8,
        (a = 0),
        (b = 1),
        (c = 2),
        (d = 3),
        (e = 4),
        (f = 5),
        (g = 6),
        (h = 7)
    )
);

#[inline(always)]
fn tcojmp_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let mut current_arity = payload_size.to_usize();
    let last_stack_frame = ctx.thread.stack_frames.last().unwrap();

    #[cfg(feature = "dynamic")]
    {
        last_stack_frame.function.increment_call_count();
    }

    ctx.instructions = last_stack_frame.function.body_exp();
    ctx.sp = last_stack_frame.sp as _;
    ctx.ip = 0;

    // TODO: Reuse the original list allocation, if it exists.
    handle_multi_arity(
        last_stack_frame.function.is_multi_arity,
        last_stack_frame.function.arity(),
        current_arity,
        ctx,
        &mut current_arity,
    )?;

    // We should have arity at this point, drop the stack up to this point
    // take the last arity off the stack, go back and replace those in order
    // [... arg1 arg2 arg3]
    //      ^^^ <- back = this index
    // offset = the start of the stack frame
    // Copy the arg1 arg2 arg3 values to
    // [... frame-start ... arg1 arg2 arg3]
    //      ^^^^^^~~~~~~~~
    let back = ctx.thread.stack.len() - current_arity;
    drain_back(ctx, ctx.sp, back);
    Ok(())
}

#[inline(never)]
fn handle_multi_arity(
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
            stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", original_arity, payload_size); ctx.current_span());
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

pub extern "C-unwind" fn handle_pure_function(
    ctx: *mut VmCore,
    ip: usize,
    offset: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    ctx.ip = ip;

    // println!("Instruction: {:?}", ctx.instructions[ctx.ip]);

    // if ctx.instructions[ctx.ip].payload_size == 1 {
    //     println!("Found multi arity function");
    // }

    assert!(ctx.ip < ctx.instructions.len());

    ctx.ip += 1;

    let is_multi_arity = ctx.instructions[ctx.ip].payload_size.to_u32() == 1;

    ctx.ip += 1;

    // Check whether this is a let or a rooted function
    let closure_id = ctx.instructions[ctx.ip].payload_size.to_u32();

    // if is_multi_arity {
    //     println!("Found multi arity function");
    // }

    ctx.ip += 1;

    // TODO - used to be offset - 2, now 3 with the multi arity
    let forward_jump = offset - 2;
    // let forward_jump = offset;

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
        // debug_assert!(ctx.instructions[forward_index - 1].op_code == OpCode::ECLOSURE);

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
                // TODO: This is a bug! We can panic here if we are
                // not running on the root?
                // ctx.root_spans[ctx.ip..forward_jump_index].into()
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
            // ctx.root_spans[ctx.ip..forward_jump_index].into()
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

        #[cfg(feature = "jit2")]
        let constructed_lambda = jit::jit_compile_lambda(ctx, constructed_lambda);

        let constructed_lambda = Gc::new(constructed_lambda);

        ctx.thread
            .function_interner
            .pure_function_interner
            .insert(closure_id, Gc::clone(&constructed_lambda));

        // Put the spans into the interner as well
        ctx.thread.function_interner.spans.insert(closure_id, spans);

        constructed_lambda
    };

    let res = SteelVal::Closure(constructed_lambda);

    ctx.ip = forward_index;

    res
}

#[allow(improper_ctypes_definitions)]
pub extern "C-unwind" fn handle_new_start_closure(
    ctx: *mut VmCore,
    ip: usize,
    offset: usize,
) -> SteelVal {
    let ctx = unsafe { &mut *ctx };
    ctx.ip = ip;

    // assert!(ctx.ip < ctx.instructions.len());
    ctx.ip += 1;

    let is_multi_arity = ctx.instructions[ctx.ip].payload_size.to_u32() == 1;

    ctx.ip += 1;

    // // Get the ID of the function
    let closure_id = ctx.instructions[ctx.ip].payload_size.to_u32();

    ctx.ip += 1;

    // TODO - used to be offset - 2, now 3 with the multi arity
    // let forward_jump = offset;
    let forward_jump = offset - 3;
    // println!("Forward jump: {}", forward_jump);

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
                    panic!(
                        "Something went wrong in closure construction!, found: {:?} @ {}",
                        l, ctx.ip,
                    );
                }
            }
            ctx.ip += 1;
        }
    } else {
        // let stack_index = ctx.stack_index.last().copied().unwrap_or(0);
        // let stack_index = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
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
        // log::trace!("Fetching closure from cache");

        let mut prototype = prototype.clone();
        prototype.set_captures(captures);
        // prototype.set_heap_allocated(heap_vars);
        prototype
    } else {
        // log::trace!("Constructing closure for the first time");

        debug_assert!(ctx.instructions[forward_index - 1].op_code == OpCode::ECLOSURE);

        // debug_assert!(ctx.ip + forward_jump - 1 <= ctx.instructions.len())

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
        // let spans = ctx.spans[ctx.ip..forward_jump_index].into();

        let spans = if let Some(spans) = ctx
            .thread
            .stack_frames
            .last()
            .and_then(|x| ctx.thread.function_interner.spans.get(&x.function.id))
        {
            if forward_jump_index >= spans.len() {
                // ctx.root_spans[ctx.ip..forward_jump_index].into()

                if let Some(span_range) = ctx.root_spans.get(ctx.ip..forward_jump_index) {
                    span_range.to_vec().into()
                } else {
                    Shared::from(Vec::new())
                }
            } else {
                spans[ctx.ip..forward_jump_index].into()
            }
        } else {
            // TODO: This seems to be causing an error
            // ctx.root_spans[ctx.ip..forward_jump_index].into()

            // For now, lets go ahead and use this... hack to get us going
            if let Some(span_range) = ctx.root_spans.get(ctx.ip..forward_jump_index) {
                span_range.to_vec().into()
            } else {
                Shared::from(Vec::new())
            }
        };

        // snag the arity from the eclosure instruction
        let arity = ctx.instructions[forward_jump_index].payload_size;

        let mut constructed_lambda = ByteCodeLambda::new(
            closure_id,
            closure_body,
            arity.to_usize(),
            is_multi_arity,
            CaptureVec::new(),
        );

        ctx.thread
            .function_interner
            .closure_interner
            .insert(closure_id, constructed_lambda.clone());

        // Put the spans into the interner
        ctx.thread.function_interner.spans.insert(closure_id, spans);

        constructed_lambda.set_captures(captures);

        constructed_lambda
    };

    ctx.ip = forward_index;

    // dbg!(ctx.instructions[ctx.ip - 1]);

    SteelVal::Closure(Gc::new(constructed_lambda))
}

#[inline(never)]
fn drain_back(ctx: &mut VmCore, offset: usize, back: usize) {
    drop(ctx.thread.stack.drain(offset..back));
}

#[inline(always)]
fn jmp_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.ip = payload_size.to_usize();
    Ok(())
}

#[inline(always)]
fn beginscope_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn bind_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_bind(payload_size.to_usize());
    Ok(())
}

#[inline(always)]
fn newsclosure_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_new_start_closure(payload_size.to_usize())?;
    Ok(())
}

#[inline(always)]
fn purefunc_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_pure_function(payload_size.to_usize());
    Ok(())
}

#[inline(always)]
fn sdef_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.handle_start_def();
    Ok(())
}

#[inline(always)]
fn edef_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn arity_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.ip += 1;
    Ok(())
}

#[inline(always)]
fn panic_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.handle_panic(ctx.current_span())?;
    Ok(())
}

#[inline(always)]
fn pass_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    ctx.ip += 1;
    Ok(())
}

#[inline(never)]
fn vec_handler_impl(ctx: &mut VmCore) -> Result<Dispatch> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let payload = payload_size.to_usize();
    let len = payload / 2;
    let bytes = payload % 2 != 0;

    let args = ctx.thread.stack.split_off(ctx.thread.stack.len() - len);

    let val = if bytes {
        let buffer: Vec<_> = args
            .into_iter()
            .flat_map(|val| {
                let int = val.int_or_else(|| "unexpected non integer");

                debug_assert!(int.is_ok());

                int.ok()
            })
            .flat_map(|int| {
                let byte = u8::try_from(int);

                debug_assert!(byte.is_ok());
                byte.ok()
            })
            .collect();

        SteelVal::ByteVector(crate::rvals::SteelByteVector::new(buffer))
    } else {
        SteelVal::VectorV(crate::rvals::SteelVector(Gc::new(args.into())))
    };

    ctx.thread.stack.push(val);
    ctx.ip += 1;
    Ok(())
}

fn unhandled_handler_tco<'a, 'b>(ctx: &'a mut VmCore<'b>) -> Result<Dispatch> {
    panic!(
        "Unhandled opcode: {:?} @ {}",
        ctx.instructions[ctx.ip], ctx.ip
    );
    // Ok(SteelVal::Void)
}
