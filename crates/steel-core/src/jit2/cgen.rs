#![allow(improper_ctypes_definitions)]
#![allow(unpredictable_function_pointer_comparisons)]

mod native;

use core::{mem::offset_of, ptr::NonNull};
use cranelift::{
    codegen::ir::{ArgumentPurpose, AtomicRmwOp, BlockArg, FuncRef, Type},
    frontend::Switch,
    prelude::{isa::CallConv, *},
};
use cranelift_jit::{ArenaMemoryProvider, JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::HashSet;
use std::{collections::HashMap, mem::ManuallyDrop};
use steel_derive::cross_platform_fn;
use steel_gen::OpCode;
use steel_parser::interner::InternedString;

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::{pretty_print_dense_instructions, DenseInstruction},
    gc::Gc,
    primitives::{
        bytevectors::{steel_bytes_ref, steel_bytes_set},
        lists::{steel_is_empty, steel_list_contains, steel_memq, steel_pair, steel_reverse},
        numbers::{
            bitwise_and, bitwise_ior, bitwise_xor, floor_quotient, floor_remainder, modulo,
            quotient, remainder, steel_ceiling, steel_exact, steel_floor, steel_round,
            steel_truncate, truncate_quotient, truncate_remainder,
        },
        ports::{eof_objectp_jit, steel_eof_objectp},
        strings::steel_char_equals,
        vectors::{flat_vector_construct, mut_vec_push, steel_mut_vec_set},
    },
    rvals::{FunctionSignature, SteelString},
    steel_vm::{
        primitives::{steel_eq, steel_listp, steel_stringp, steel_symbolp, steel_voidp},
        vm::{jit::*, StackFrame, StackFrameAttachments, SteelThread, VmCore},
    },
    values::{
        functions::{ByteCodeLambda, RootedInstructions},
        lists::{List, SteelList},
        structs::{create_struct_spec, StructFunctionType, StructTypeDescriptor},
    },
    SteelVal,
};

// Switches for individual optimizations, read from `config.rs`.

/// Inline tail calls to primitives the JIT already knows how to emit, instead
/// of routing them through `call_global_function_tail_deopt_*`.
/// `STEEL_JIT_PRIM_TAIL=0` disables it.
fn inline_primitive_tail_calls_enabled() -> bool {
    crate::config::config().jit_prim_tail
}

/// `STEEL_JIT_FLOAT_INLINE=0` sends float arithmetic back out to the helper.
fn float_inline_enabled() -> bool {
    crate::config::config().jit_float_inline
}

/// Inline `=` / `<` / `<=` / `>` / `>=` on two arbitrary values when both turn
/// out to be integers, and `vector-ref` when the vector is not in a register.
/// `STEEL_JIT_GENERIC_INLINE=0` disables it.
fn generic_inline_enabled() -> bool {
    crate::config::config().jit_generic_inline
}

/// `STEEL_JIT_CALLGLOBAL_DIRECT=0` disables direct calls to closed global
/// closures (`global_is_closed`).
fn callglobal_direct_enabled() -> bool {
    crate::config::config().jit_callglobal_direct
}

/// `STEEL_JIT_LATE_TAIL=0` disables the run-time compiled check on a direct
/// global tail call whose callee was not compiled yet.
fn late_tail_enabled() -> bool {
    crate::config::config().jit_late_tail
}

fn rest_args_enabled() -> bool {
    crate::config::config().jit_rest_args
}

/// Whether calls check the remaining native stack instead of counting vm
/// frames. `STEEL_JIT_STACK_CHECK=0` uses a 100-frame cap instead;
/// `STEEL_JIT_STACK_BUDGET=<mb>` overrides how much stack the check allows.
fn native_stack_check_enabled() -> bool {
    crate::config::config().jit_stack_check
}

/// `STEEL_JIT_INLINE_TAIL_CALL=0` disables the direct (address-baked) global
/// tail call, for isolating bugs in that path.
fn use_inline_global_tail_call() -> bool {
    crate::config::config().jit_inline_tail_call
}

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    // data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    function_map: OwnedFunctionMap,

    names: HashMap<u32, String>,

    // Names that made it all the way through define_function. `compile` declares a
    // name before it translates, so a failed compile leaves a declaration with no
    // body, and passing that to get_finalized_function panics.
    defined: HashSet<String>,

    function_return_types: HashMap<u32, HashSet<InferredType>>,

    /// Whether the next `translate` may walk lists as cursors. Set around the
    /// specialized-copy compile only, and cleared again when that translation
    /// bailed. See `FunctionTranslator::cursor_bail`.
    allow_cursors: bool,

    // perf inject --jit support. None unless STEEL_JIT_DUMP is set, since opening
    // it writes a jit-<pid>.dump into the cwd of every process that builds a JIT.
    #[cfg(target_os = "linux")]
    jitdump: Option<wasmtime_jit_debug::perf_jitdump::JitDumpFile>,
}

pub struct FunctionMap<'a> {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    map2: HashMap<&'static str, Box<dyn FunctionToCranelift2 + Send + Sync + 'static>>,
    builder: &'a mut JITBuilder,
}

#[allow(unused)]
struct OwnedFunctionMap {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    map2: HashMap<&'static str, Box<dyn FunctionToCranelift2 + Send + Sync + 'static>>,
}

impl OwnedFunctionMap {
    pub fn get_signature(&self, name: &str, module: &JITModule) -> Signature {
        self.map
            .get(name)
            .map(|x| x.to_cranelift(module))
            .unwrap_or_else(|| self.map2.get(name).map(|x| x.to_cranelift(module)).unwrap())
    }
}

impl<'a> FunctionMap<'a> {
    // Do the thing?
    pub fn add_func(
        &mut self,
        name: &'static str,
        func: impl FunctionToCranelift + Send + Sync + 'static,
    ) {
        self.builder.symbol(name, func.as_pointer());
        self.map.insert(name, Box::new(func));
    }

    pub fn add_func2(
        &mut self,
        name: &'static str,
        func: impl FunctionToCranelift2 + Send + Sync + 'static,
    ) {
        self.builder.symbol(name, func.as_pointer());
        self.map2.insert(name, Box::new(func));
    }
}

pub trait FunctionToCranelift {
    fn to_cranelift(&self, module: &JITModule) -> Signature;
    fn as_pointer(&self) -> *const u8;
}

pub trait FunctionToCranelift2 {
    fn to_cranelift(&self, module: &JITModule) -> Signature;
    fn as_pointer(&self) -> *const u8;
}

macro_rules! register_function_pointers_return {
    ($($typ:ident),*) => {
        #[cfg(target_os = "windows")]
        impl<RET, $($typ),*> FunctionToCranelift for extern "sysv64-unwind" fn(*mut VmCore, $($typ),*) -> RET {
            fn to_cranelift(&self, module: &JITModule) -> Signature {
                let mut sig = module.make_signature();

                // VmCore pointer
                sig.params
                    .push(AbiParam::new(module.target_config().pointer_type()));

                $(
                    sig.params.push(AbiParam::new(type_to_ir_type::<$typ>()));
                )*

                let return_size = core::mem::size_of::<RET>();

                if return_size != 0 {
                    sig.returns.push(AbiParam::new(type_to_ir_type::<RET>()));
                }

                sig
            }

            fn as_pointer(&self) -> *const u8 {
                *self as _
            }
        }

        #[cfg(not(target_os = "windows"))]
        impl<RET, $($typ),*> FunctionToCranelift for extern "C-unwind" fn(*mut VmCore, $($typ),*) -> RET {
            fn to_cranelift(&self, module: &JITModule) -> Signature {
                let mut sig = module.make_signature();

                // VmCore pointer
                sig.params
                    .push(AbiParam::new(module.target_config().pointer_type()));

                $(
                    sig.params.push(AbiParam::new(type_to_ir_type::<$typ>()));
                )*

                let return_size = core::mem::size_of::<RET>();

                if return_size != 0 {
                    sig.returns.push(AbiParam::new(type_to_ir_type::<RET>()));
                }

                sig
            }

            fn as_pointer(&self) -> *const u8 {
                *self as _
            }
        }

        #[cfg(target_os = "windows")]
        impl<RET, $($typ),*> FunctionToCranelift2 for extern "sysv64-unwind" fn($($typ),*) -> RET {
            fn to_cranelift(&self, module: &JITModule) -> Signature {
                let mut sig = module.make_signature();

                $(
                    sig.params.push(AbiParam::new(type_to_ir_type::<$typ>()));
                )*

                let return_size = core::mem::size_of::<RET>();

                if return_size != 0 {
                    sig.returns.push(AbiParam::new(type_to_ir_type::<RET>()));
                }

                sig
            }

            fn as_pointer(&self) -> *const u8 {
                *self as _
            }
        }

        #[cfg(not(target_os = "windows"))]
        impl<RET, $($typ),*> FunctionToCranelift2 for extern "C-unwind" fn($($typ),*) -> RET {
            fn to_cranelift(&self, module: &JITModule) -> Signature {
                let mut sig = module.make_signature();

                $(
                    sig.params.push(AbiParam::new(type_to_ir_type::<$typ>()));
                )*

                let return_size = core::mem::size_of::<RET>();

                if return_size != 0 {
                    sig.returns.push(AbiParam::new(type_to_ir_type::<RET>()));
                }

                sig
            }

            fn as_pointer(&self) -> *const u8 {
                *self as _
            }
        }
    };
}

register_function_pointers_return!();
register_function_pointers_return!(A);
register_function_pointers_return!(A, B);
register_function_pointers_return!(A, B, C);
register_function_pointers_return!(A, B, C, D);
register_function_pointers_return!(A, B, C, D, E);
register_function_pointers_return!(A, B, C, D, E, F);
register_function_pointers_return!(A, B, C, D, E, F, G);
register_function_pointers_return!(A, B, C, D, E, F, G, H);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K, L);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K, L, M);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
register_function_pointers_return!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);

fn type_to_ir_type<T>() -> Type {
    Type::int(core::mem::size_of::<T>() as u16 * 8).unwrap()
}

macro_rules! abi {
    ($func:ident as $($tokens:tt)*) => {
        {
            #[cfg(target_os = "windows")]
            {
                $func as extern "sysv64-unwind" $($tokens)*
            }

            #[cfg(not(target_os = "windows"))]
            {
                $func as extern "C-unwind" $($tokens)*
            }
        }
    };
}

mod helpers;
use helpers::*;
mod guarded;
mod inline_primitives;
mod lower_calls;
mod lower_control;
mod lower_data;
mod lower_locals;
mod lower_numeric;

/// How many speculative exits a function may take before speculation stops
/// being worth it for that function.
///
/// A polymorphic site exits on every call, which is worse than the generic
/// fallback since each exit is a full return to the interpreter. Past this many
/// exits the function is recorded and compiled without speculation from then on.
fn speculation_deopt_limit() -> u64 {
    crate::config::config().jit_deopt_limit
}

/// Exit counts per jitted function, and the set that no longer speculates.
/// Shared across threads because the jit and its code cache are.
static SPECULATION_DEOPTS: std::sync::Mutex<Option<std::collections::HashMap<usize, u64>>> =
    std::sync::Mutex::new(None);

static SPECULATION_DISABLED: std::sync::Mutex<Option<std::collections::HashSet<usize>>> =
    std::sync::Mutex::new(None);

/// Whether this function should still be compiled with speculation.
fn should_speculate_for(function_index: Option<usize>) -> bool {
    if !speculate_int_tag_enabled() {
        return false;
    }

    let Some(index) = function_index else {
        return true;
    };

    SPECULATION_DISABLED
        .lock()
        .map(|guard| guard.as_ref().map_or(true, |set| !set.contains(&index)))
        .unwrap_or(true)
}

#[cross_platform_fn]
// Called from a speculative exit. Counts it, and turns off speculation for this
// function once it passes the limit.
fn record_speculation_deopt(function_index: i64) {
    let index = function_index as usize;
    let limit = speculation_deopt_limit();

    let Ok(mut guard) = SPECULATION_DEOPTS.lock() else {
        return;
    };

    let counts = guard.get_or_insert_with(Default::default);
    let count = counts.entry(index).or_insert(0);
    *count += 1;

    if *count == limit {
        log::debug!(
            target: "jit-deopt",
            "function {index} exited speculatively {limit} times; not speculating it again"
        );

        if let Ok(mut disabled) = SPECULATION_DISABLED.lock() {
            disabled.get_or_insert_with(Default::default).insert(index);
        }
    }
}

#[cross_platform_fn]
fn debug_count(value: i32) {
    println!("Count: {}", value);
}

#[cross_platform_fn]
fn debug_int(value: i64) {
    println!("Value: {}", value);
}

#[cross_platform_fn]
fn debug_value(value: SteelVal) {
    let value = ManuallyDrop::new(value);
    println!("Value: {}", &*value);
}

#[cross_platform_fn]
fn debug_stack_frames(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    println!("--- after push stack frames ---");
    println!("Stack frame length: {}", ctx.thread.stack_frames.len());

    let last_frame = ctx.thread.stack_frames.last();

    if let Some(last) = last_frame {
        println!("sp: {}", last.sp);
        println!("ip: {}", last.ip);
        println!("instruction addr: {:p}", last.instructions.ptr);
    } else {
        println!("No frame on stack")
    }
}

#[cross_platform_fn]
fn debug_instructions_before(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    println!("instructions before call: {:?}", ctx.instructions);
}

#[cross_platform_fn]
fn debug_instructions_after(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    println!("instructions after call: {:?}", ctx.instructions);
}

#[cross_platform_fn]
fn debug_stack_before(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    println!("Stack length before: {}", ctx.thread.stack.len());
}

#[cross_platform_fn]
fn debug_stack_after(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    println!("Stack length after: {}", ctx.thread.stack.len());
}

#[cross_platform_fn]
fn debug_is_native(ctx: *mut VmCore) {
    let ctx = unsafe { &mut *ctx };
    println!("vm is native: {}", ctx.is_native);
}

#[cross_platform_fn]
fn debug_tag(value: i8) {
    println!("Tag: {}", value);
}

#[cross_platform_fn]
fn debug_instructions(value: RootedInstructions) {
    println!("Current instructions: {:?}", value);
}

#[cross_platform_fn]
fn debug_instructions2(value: RootedInstructions) {
    println!("fat pointer instructions: {:?}", value);
}

/// An opcode with no lowering. Translation panics are caught and handled like
/// any other failure to compile, so the function stays interpreted.
fn unsupported_opcode(op: OpCode) -> ! {
    panic!("jit: no lowering for {op:?}; the function stays interpreted")
}

/// Whether a call to `name` can reallocate `SteelThread.stack`. Any callee not in
/// `KEEPS_STACK` is assumed to, since a cached buffer pointer would dangle
/// otherwise.
fn callee_can_move_value_stack(name: &str) -> bool {
    !KEEPS_STACK.contains(&name)
}

/// Speculate on the int tag and bail to the interpreter when it misses, instead
/// of merging with a generic slow path.
///
/// A `converging_if` joining an int arm with an arm of unknown type has to agree
/// on the boxed `i128` representation. Branching to an exit instead means the
/// fast arm never joins, so it can carry a raw i64. This is how
/// `InferredType::Int64` values get produced.
fn speculate_int_tag_enabled() -> bool {
    crate::config::config().jit_speculate_int
}

/// Caching `SteelThread.stack.buf` across a block. It only moves when the stack
/// grows, and the jit already invalidates at both of its own growth points; this
/// adds invalidation around calls, which can grow it from the rust side.
fn stack_buf_cache_enabled() -> bool {
    crate::config::config().jit_buf_ptr_cache
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        flag_builder
            .set("enable_llvm_abi_extensions", "true")
            .unwrap();

        flag_builder.set("opt_level", "speed").unwrap();

        // On in release too, so a miscompile gets caught before it is emitted.
        // It is slow on compile heavy programs, so it can be turned off.
        if !crate::config::config().jit_verifier {
            flag_builder.set("enable_verifier", "false").unwrap();
        }

        // Required because the jit emits tail calls, and cranelift's x64 backend
        // asserts on tail calls without frame pointers. They cost some prologue
        // work, so revisit if that restriction is lifted upstream.
        flag_builder.set("preserve_frame_pointers", "true").unwrap();

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let arena_mib = crate::config::config()
            .jit_memory_size
            .filter(|mib| *mib > 0)
            // Specialized copies roughly double the code a process generates, and
            // every test in `cargo test` shares one jit. On unix this only reserves
            // address space. `region` commits it all on windows, so stay small there.
            .unwrap_or(if cfg!(windows) { 256 } else { 1024 })
            .min(2047);
        builder.memory_provider(Box::new(
            ArenaMemoryProvider::new_with_size(arena_mib << 20)
                .expect("failed to reserve JIT code arena"),
        ));

        // How to take the if branch - this will return a boolean. 1 = true, 0 = false

        let mut map = FunctionMap {
            map: HashMap::new(),
            map2: HashMap::new(),
            builder: &mut builder,
        };

        register_helpers(&mut map);

        // Value functions:

        // TODO: Inline this!

        CallStructConstructorsDefinitions::register(&mut map);
        CallMutableStructConstructorsDefinitions::register(&mut map);
        CallMutableVectorConstructorsDefinitions::register(&mut map);

        CallFlatVectorConstructorsDefinitions::register(&mut map);

        CallSelfNoArityFunctionDefinitions::register(&mut map);

        CallGlobalFunctionDefinitions::register(&mut map);
        CallGlobalNoArityFunctionDefinitions::register(&mut map);
        CallFunctionDefinitions::register(&mut map);
        CallFunctionTailDefinitions::register(&mut map);
        CallGlobalTailFunctionDefinitions::register(&mut map);
        CallGlobalNativeTailDefinitions::register(&mut map);
        CallBoxedFunctionDefinitions::register(&mut map);
        CallBoxedFunctionTailDefinitions::register(&mut map);
        ListHandlerDefinitions::register(&mut map);

        // Primitive calls:
        CallPrimitiveDefinitions::register(&mut map);
        CallPrimitiveMutDefinitions::register(&mut map);

        CallPrimitiveFixedDefinitions::register(&mut map);

        // Check if the function at the global location is in fact the right one.

        // TODO: Add type checked variants as well which can allow
        // passing through unboxed values on the stack

        // map.add_func(
        //     "cdr-mut-reg",
        //     abi! { cdr_handler_mut_reg as fn(*mut VmCore, usize) },

        // TODO: Add type checked variants as well which can allow
        // passing through unboxed values on the stack

        // TODO: Pick up from here!

        let function_map = OwnedFunctionMap {
            map: map.map,
            map2: map.map2,
        };

        let module = JITModule::new(builder);

        #[cfg(target_os = "linux")]
        let jitdump = open_jitdump();

        Self {
            builder_context: FunctionBuilderContext::new(),
            allow_cursors: false,
            ctx: module.make_context(),
            module,
            function_map,
            names: Default::default(),
            defined: Default::default(),
            function_return_types: Default::default(),
            #[cfg(target_os = "linux")]
            jitdump,
        }
    }
}

// The weak counter's offset and width in the box header. `inline_weak_decrement`
// writes it directly, so both have to track the type. At the wrong offset this
// decrements the spinlock instead and the next acquire never completes.
fn weak_counter_offset() -> i64 {
    steel_rc::weak::weak_offset::<
        crate::values::lock::SpinLock<crate::values::closed::HeapAllocated<SteelVal>>,
    >() as i64
}

/// `STEEL_JIT_INLINE_WEAK_CLONE=0` sends heap reference clones back through the
/// out-of-line `clone_one` call. On by default.
fn inline_weak_clone_enabled() -> bool {
    crate::config::config().jit_inline_weak_clone
}

/// `STEEL_JIT_BORROW=0` clones every value read out of a container as it is
/// read, even when the consumer only needs it borrowed.
pub(super) fn borrow_enabled() -> bool {
    crate::config::config().jit_borrow
}

/// `STEEL_JIT_CLOSED_GLOBAL_OPERANDS=0` clones a closed global onto the operand
/// stack when it is pushed, like any other global.
pub(super) fn closed_global_operands_enabled() -> bool {
    crate::config::config().jit_closed_global_operands
}

/// Which of the four integer division operators `inline_int_divmod` is
/// lowering. The truncating pair round toward zero and take their sign from the
/// dividend; the flooring pair round down and take it from the divisor.
#[derive(Clone, Copy)]
enum DivMode {
    TruncQuotient,
    TruncRemainder,
    FloorQuotient,
    FloorRemainder,
}

#[derive(Clone, Copy)]
enum RoundOp {
    Floor,
    Ceiling,
    Truncate,
    Round,
    Exact,
}

#[derive(Clone, Copy)]
enum BitwiseOp {
    And,
    Ior,
    Xor,
}

/// `STEEL_JIT_INLINE_SETBOX=0` sends the `SETBOX` opcode back out to
/// `set-box-handler`, which pays a `Weak::upgrade` CAS and the spin lock on
/// every write even when the box is unshared.
pub(super) fn inline_setbox_enabled() -> bool {
    crate::config::config().jit_inline_setbox
}

/// `STEEL_JIT_BYTEVECTOR_STRICT_UNSHARED=1` makes the bytevector lock elision
/// additionally require the owner's reference count to be exactly one.
///
/// Off by default. A zero shared half-word already proves every reference
/// belongs to the owner thread, and references held by one thread cannot race
/// with each other.
pub(super) fn byte_vec_strict_unshared() -> bool {
    crate::config::config().jit_bytevector_strict_unshared
}

/// `STEEL_JIT_LIST_CURSOR=1` walks lists as an unboxed `(cell, index)` pair in
/// registers instead of materialising a list per `cdr`.
///
/// Off by default, this is a prototype. It only helps once a cursor survives a
/// loop unmaterialised. Every deopt reachable while a cursor is live has to
/// materialise it, or the interpreter resumes from the head of the list.
pub(super) fn list_cursor_enabled() -> bool {
    crate::config::config().jit_list_cursor
}

/// `STEEL_JIT_INLINE_BYTEVECTOR=0` sends `bytes-ref` / `bytes-set!` back out to
/// the generic primitive call.
pub(super) fn inline_bytevector_enabled() -> bool {
    crate::config::config().jit_inline_bytevector
}

/// `STEEL_JIT_INLINE_DIVMOD=0` sends `quotient` / `remainder` / `modulo` back
/// out through the generic primitive call. Otherwise the two-fixnum case is
/// inlined.
pub(super) fn inline_divmod_enabled() -> bool {
    crate::config::config().jit_inline_divmod
}

/// `STEEL_JIT_INLINE_BITWISE=0` sends `bitwise-and` / `bitwise-ior` /
/// `bitwise-xor` back out through the generic primitive call.
pub(super) fn inline_bitwise_enabled() -> bool {
    crate::config::config().jit_inline_bitwise
}

/// `STEEL_JIT_INLINE_ROUNDING=0` sends `floor` / `ceiling` / `truncate` /
/// `round` / `exact` back out through the generic primitive call.
pub(super) fn inline_rounding_enabled() -> bool {
    crate::config::config().jit_inline_rounding
}

/// `STEEL_JIT_INLINE_EQ=0` sends `eq?` on two non-constant operands back
/// through the out-of-line helpers.
pub(super) fn inline_eq_enabled() -> bool {
    crate::config::config().jit_inline_eq
}

/// Tail-position inlining of the list predicates, `cdr` and `mutable-vector`.
/// `STEEL_JIT_PRIM_TAIL2=0` disables these, leaving `#%unbox` / `#%set-box!`.
fn prim_tail2_enabled() -> bool {
    crate::config::config().jit_prim_tail2
}

/// Each part can be turned off on its own with `STEEL_JIT_TAIL_PRED` / `_CDR` /
/// `_VEC` set to 0.
fn tail_pred_enabled() -> bool {
    prim_tail2_enabled() && crate::config::config().jit_tail_pred
}

fn tail_cdr_enabled() -> bool {
    prim_tail2_enabled() && crate::config::config().jit_tail_cdr
}

/// `reverse` in tail position. The function returns from jitted code here
/// instead of deopting, which depends on the tail call handler treating a
/// tail-position `apply` (which replaces the current frame) correctly. See
/// `new_callglobal_tail_handler_deopt_test`.
fn tail_reverse_enabled() -> bool {
    prim_tail2_enabled() && crate::config::config().jit_tail_rev
}

fn tail_vec_enabled() -> bool {
    prim_tail2_enabled() && crate::config::config().jit_tail_vec
}

/// `STEEL_JIT_PRIM_TAIL_EXTRA=0` turns off tail-position inlining of `eq?`,
/// `vector-set!` and `#%make-mutable-struct`, leaving `#%unbox` / `#%set-box!`.
fn extra_primitive_tail_calls_enabled() -> bool {
    crate::config::config().jit_prim_tail_extra
}

fn weak_counter_type() -> Type {
    match steel_rc::weak::ref_count_width() {
        8 => types::I64,
        4 => types::I32,
        other => unreachable!("unsupported refcount width: {other}"),
    }
}

/// `SteelVal::Pair` carries the allocation pointer, so `car` and `cdr` sit past
/// the reference counting header. Derived, since the header size is not fixed.
fn pair_field_offset(field: usize) -> i32 {
    (steel_rc::BiasedRc::<crate::values::lists::Pair>::data_offset() + field) as i32
}

/// `SteelVal::Closure` carries the allocation pointer, so a field of the lambda
/// sits past the reference counting header. Derived, since the header size is
/// not fixed.
fn closure_field_offset(field: usize) -> i32 {
    (steel_rc::BiasedRc::<ByteCodeLambda>::data_offset() + field) as i32
}

/// The biased half-word, which generated code adjusts directly. The owner id is
/// in the high bits and its count in the low bits, so `word +/- 1` only moves
/// the count.
fn biased_word_offset() -> i32 {
    steel_rc::biased_offset() as i32
}

// Byte offsets from the pointer a `SteelVal::ListV` carries to the cell's
// fields. That pointer is the `RcBox`, so the cell data starts after the header.
// Both are derived from the types, the way the heap-box offsets are.
fn list_cell_base() -> i64 {
    steel_rc::BiasedRc::<SteelVal>::data_offset() as i64
}

fn list_index_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_index_offset() as i64) as i32
}

fn list_elements_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_elements_offset() as i64) as i32
}

/// Where a chunked cell keeps its buffer pointer. Shares bytes with the inline
/// element, so only read it once the discriminant says the cell is chunked.
fn list_buffer_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_buffer_offset() as i64) as i32
}

/// The link to the next cell. Its low bit says whether this cell holds its
/// single element inline, so that is where the discriminant is read from.
fn list_next_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_next_offset() as i64) as i32
}

/// The bits of `next` that are tag rather than pointer.
fn list_next_tag_mask() -> i64 {
    SteelList::<SteelVal>::cell_next_tag_mask() as i64
}

// Where the slice data starts in an `RcBox<[DenseInstruction]>`. Same header as
// any other `RcBox`, so take it from the type instead of hardcoding it.
const fn rcbox_slice_data_offset() -> i64 {
    steel_rc::BiasedRc::<DenseInstruction>::data_offset() as i64
}

/// Loop specialization. A loop or recursive function whose argument slots have
/// a guessed type (see `SpecType`) gets a second compiled copy with those slots
/// typed. The generic copy checks the slots on entry and tail calls into the
/// specialized one; the specialized copy loops on itself for as long as the
/// slots keep their types, and tail calls back otherwise.
#[derive(Clone, Debug)]
enum SpecMode {
    None,
    Generic {
        spec_id: FuncId,
        seed: Vec<(usize, SpecType)>,
    },
    Specialized {
        generic_id: FuncId,
        // This copy, for self calls whose arguments are proven to fit the seed.
        self_id: FuncId,
        seed: Vec<(usize, SpecType)>,
        // The type assumed for the results of those direct self calls; only
        // kept if every return of the function turns out to have it.
        assume_result: Option<InferredType>,
    },
}

/// Frameless self calls. A self recursive function's `_frameless` copy is
/// called with its arguments already on the value stack at `sp`, and pushes no
/// `StackFrame`. There is no frame record, `pop_count`, closure refcount, or
/// `VmCore` ip/instructions restore on return. Frames are rebuilt only when
/// something leaves native code, see `frameless_record_frame` in vm/jit.rs.
///
/// `STEEL_JIT_FRAMELESS=0` disables.
fn frameless_enabled() -> bool {
    crate::config::config().jit_frameless
}

/// Whether a function's body only does what a frameless copy can: local reads
/// and writes, fixnum constants, the inlined arithmetic and comparisons,
/// branches, let scopes, returns, and calls to itself. Everything else either
/// reads the frame stack, can hand control to the interpreter mid call, or
/// calls something that could push a frame of its own. A real frame pushed
/// underneath pending frameless ones would come out in the wrong order.
fn frameless_eligible(stmts: &[DenseInstruction], function_index: Option<usize>) -> bool {
    let Some(me) = function_index else {
        return false;
    };
    let mut calls_self = false;
    let ok = stmts.iter().all(|ins| match ins.op_code {
        OpCode::READLOCAL
        | OpCode::READLOCAL0
        | OpCode::READLOCAL1
        | OpCode::READLOCAL2
        | OpCode::READLOCAL3
        | OpCode::MOVEREADLOCAL
        | OpCode::MOVEREADLOCAL0
        | OpCode::MOVEREADLOCAL1
        | OpCode::MOVEREADLOCAL2
        | OpCode::MOVEREADLOCAL3
        | OpCode::LOADINT0
        | OpCode::LOADINT1
        | OpCode::LOADINT2
        // Constant pushes only touch the translator's shadow stack; a spill
        // writes them out like any other value.
        | OpCode::PUSHCONST
        | OpCode::TRUE
        | OpCode::FALSE
        | OpCode::VOID
        | OpCode::ADD
        | OpCode::SUB
        | OpCode::LT
        | OpCode::LTE
        | OpCode::GT
        | OpCode::GTE
        | OpCode::NUMEQUAL
        | OpCode::PASS
        | OpCode::IF
        | OpCode::JMP
        | OpCode::POPJMP
        | OpCode::BEGINSCOPE
        | OpCode::LetVar
        | OpCode::LETENDSCOPE
        | OpCode::POPPURE
        | OpCode::FUNCNOARITY
        | OpCode::SELFTAILCALLNOARITY => true,
        OpCode::CALLGLOBALNOARITY if ins.payload_size.to_usize() == me => {
            calls_self = true;
            true
        }
        _ => false,
    });
    ok && calls_self
}

/// Which side of a frameless call a translation is on.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FramelessRole {
    /// The specialized copy. It is entered with a real frame and makes the first
    /// frameless call, so it is the one that splices recorded frames back.
    Boundary,
    /// The `_frameless` copy itself. Takes `(vm_ctx, sp)` and has no frame.
    Callee,
}

#[derive(Clone, Copy, Debug)]
struct FramelessInfo {
    /// The `_frameless` copy that self calls go to.
    id: FuncId,
    role: FramelessRole,
    /// What a frameless self call returns, as validated when the frameless copy
    /// was compiled.
    result: Option<InferredType>,
}

/// What a translation reports back to the compile driver.
struct TranslateInfo {
    /// Types of the values returned through ordinary returns.
    returned: HashSet<InferredType>,
    /// A reachable exit other than an ordinary return or a deopt exit.
    untyped_non_deopt_exit: bool,
    /// Self calls compiled as direct calls to this same specialized copy.
    direct_self_calls: usize,
    /// The translation met something it could not keep a list cursor sound
    /// across, so it has to be redone without cursors.
    cursor_bailed: bool,
}

/// Body of a placeholder function; see `define_stub`.
#[derive(Clone, Copy)]
enum StubBody {
    TailCall(FuncId),
    InterpretFromStart,
}

/// What a seeded argument slot is specialized to hold.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SpecType {
    Fixnum,
    Float,
    List,
    MutableVector,
    Struct(StructTypeDescriptor),
}

impl SpecType {
    fn tag(self) -> u8 {
        match self {
            Self::Fixnum => SteelVal::INT_TAG,
            Self::Float => SteelVal::FLOAT_TAG,
            Self::List => SteelVal::LIST_TAG,
            Self::MutableVector => SteelVal::HEAP_REF_VECTOR_TAG,
            Self::Struct(_) => SteelVal::STRUCT_TAG,
        }
    }

    fn inferred(self) -> InferredType {
        match self {
            Self::Fixnum => InferredType::Int,
            Self::Float => InferredType::Float,
            Self::List => InferredType::List,
            Self::MutableVector => InferredType::MutableVector,
            Self::Struct(d) => InferredType::Struct(d),
        }
    }

    /// The property that records this type on a register. Lists use
    /// `ProperList`, which is what the list fast paths look for and what
    /// `register_type` reads back as `List`.
    fn property(self) -> Properties {
        match self {
            Self::List => Properties::ProperList,
            other => Properties::InferredType(other.inferred()),
        }
    }
}

/// Emit a check that every `(slot, type)` holds, reading the slots at
/// `frame_base`. Falls to `fail` at the first mismatch and leaves the builder
/// in the block where everything passed. A struct's descriptor is only loaded
/// once its tag has matched, since the payload is not a pointer otherwise.
fn emit_slot_type_checks(
    builder: &mut FunctionBuilder,
    frame_base: Value,
    checks: &[(usize, SpecType)],
    fail: Block,
) {
    for &(k, ty) in checks {
        let base = (k * std::mem::size_of::<SteelVal>()) as i32;
        let tag = builder
            .ins()
            .load(types::I8, MemFlagsData::trusted(), frame_base, base);
        let tag_ok = builder.ins().icmp_imm_s(IntCC::Equal, tag, ty.tag() as i64);
        let next = builder.create_block();
        builder.ins().brif(tag_ok, next, &[], fail, &[]);
        builder.seal_block(next);
        builder.switch_to_block(next);

        if let SpecType::Struct(descriptor) = ty {
            let payload =
                builder
                    .ins()
                    .load(types::I64, MemFlagsData::trusted(), frame_base, base + 8);
            let on_heap = builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                payload,
                crate::values::structs::StructStorage::header_offset() as i32,
            );
            let descriptor_ok =
                builder
                    .ins()
                    .icmp_imm_s(IntCC::Equal, on_heap, descriptor.key() as i64);
            let next = builder.create_block();
            builder.ins().brif(descriptor_ok, next, &[], fail, &[]);
            builder.seal_block(next);
            builder.switch_to_block(next);
        }
    }
}

/// Type-specialized copies of loops and self-recursive functions. On by default;
/// `STEEL_JIT_SPECIALIZE_LOOPS=0` turns it off.
fn loop_specialization_enabled() -> bool {
    crate::config::config().jit_specialize_loops
}

fn spec_debug_enabled() -> bool {
    crate::config::config().jit_spec_debug
}

/// Guess a type for each argument slot of a loop or recursive function from how
/// the body consumes it: arithmetic and comparisons (fixnum, or float when a
/// float literal is involved), `car`/`cdr`/`null?` (list), `vector-ref`
/// (mutable vector), a struct getter (that struct). A slot with conflicting
/// evidence, or that is `set!`, gets no guess. The guard at run time decides,
/// so this only has to be cheap and usually right.
///
/// It walks the bytecode with an abstract stack of where each value came from,
/// forgetting everything at a jump target or at anything it does not model.
fn loop_type_seed(
    code: &[DenseInstruction],
    arity: u16,
    constants: &ConstantMap,
    globals: &[SteelVal],
    function_index: Option<usize>,
) -> Vec<(usize, SpecType)> {
    use std::collections::{BTreeMap, BTreeSet};

    // Loops (a self tail call) and recursive functions (a self call that is not
    // in tail position) both run the same body again with the next arguments.
    let is_self_call = |i: &DenseInstruction| {
        matches!(i.op_code, OpCode::CALLGLOBAL | OpCode::CALLGLOBALNOARITY)
            && function_index == Some(i.payload_size.to_usize())
    };
    if !code
        .iter()
        .any(|i| i.op_code == OpCode::SELFTAILCALLNOARITY || is_self_call(i))
    {
        return Vec::new();
    }
    let arity = arity as usize;

    #[derive(Clone, Copy, PartialEq)]
    enum Entry {
        Slot(usize),
        IntLit,
        FloatLit,
        Other,
    }
    #[derive(Default)]
    struct Uses {
        numeric: bool,
        list: bool,
        vector: bool,
        structs: BTreeSet<StructTypeDescriptor>,
        // The body asks what type the slot holds, so it expects more than one.
        type_tested: bool,
    }

    let targets: BTreeSet<usize> = code
        .iter()
        .filter(|i| matches!(i.op_code, OpCode::IF | OpCode::JMP | OpCode::POPJMP))
        .map(|i| i.payload_size.to_usize())
        .collect();

    let mut uses: BTreeMap<usize, Uses> = BTreeMap::new();
    let mut mutated = BTreeSet::new();
    let mut floats = BTreeSet::new();
    let mut numeric_groups: Vec<Vec<usize>> = Vec::new();
    let mut stack: Vec<Entry> = Vec::new();
    // Set by an op whose trailing FUNC/FUNCNOARITY only carries metadata.
    let mut skip_meta = false;

    let slot_of = |ins: &DenseInstruction| match ins.op_code {
        OpCode::READLOCAL0 | OpCode::MOVEREADLOCAL0 => Some(0),
        OpCode::READLOCAL1 | OpCode::MOVEREADLOCAL1 => Some(1),
        OpCode::READLOCAL2 | OpCode::MOVEREADLOCAL2 => Some(2),
        OpCode::READLOCAL3 | OpCode::MOVEREADLOCAL3 => Some(3),
        OpCode::READLOCAL | OpCode::MOVEREADLOCAL => Some(ins.payload_size.to_usize()),
        _ => None,
    };

    let mut idx = 0;
    while idx < code.len() {
        let ins = &code[idx];
        let payload = ins.payload_size.to_usize();
        if targets.contains(&idx) {
            stack.clear();
        }
        let meta = std::mem::take(&mut skip_meta);

        if let Some(slot) = slot_of(ins) {
            stack.push(if slot < arity {
                Entry::Slot(slot)
            } else {
                Entry::Other
            });
            idx += 1;
            continue;
        }

        match ins.op_code {
            OpCode::LOADINT0 | OpCode::LOADINT1 | OpCode::LOADINT2 => stack.push(Entry::IntLit),
            OpCode::PUSHCONST => stack.push(match constants.get(payload) {
                SteelVal::IntV(_) => Entry::IntLit,
                SteelVal::NumV(_) => Entry::FloatLit,
                _ => Entry::Other,
            }),
            OpCode::PUSH | OpCode::READCAPTURED | OpCode::TRUE | OpCode::FALSE | OpCode::VOID => {
                stack.push(Entry::Other)
            }
            OpCode::PASS | OpCode::BEGINSCOPE => {}
            OpCode::FUNC | OpCode::FUNCNOARITY if meta => {}

            OpCode::ADD
            | OpCode::SUB
            | OpCode::MUL
            | OpCode::LT
            | OpCode::LTE
            | OpCode::GT
            | OpCode::GTE
            | OpCode::NUMEQUAL
                if payload <= stack.len() =>
            {
                let operands = stack.split_off(stack.len() - payload);
                let slots: Vec<usize> = operands
                    .iter()
                    .filter_map(|e| {
                        if let Entry::Slot(k) = e {
                            Some(*k)
                        } else {
                            None
                        }
                    })
                    .collect();
                let has_float = operands.contains(&Entry::FloatLit);
                for &k in &slots {
                    uses.entry(k).or_default().numeric = true;
                    if has_float {
                        floats.insert(k);
                    }
                }
                numeric_groups.push(slots);
                stack.push(Entry::Other);
            }

            OpCode::CAR | OpCode::CDR | OpCode::NULL if !stack.is_empty() => {
                if let Some(Entry::Slot(k)) = stack.pop() {
                    uses.entry(k).or_default().list = true;
                }
                stack.push(Entry::Other);
                skip_meta = true;
            }

            OpCode::NOT if !stack.is_empty() => {
                stack.pop();
                stack.push(Entry::Other);
                skip_meta = true;
            }

            OpCode::VECTORREF if stack.len() >= 2 => {
                let args = stack.split_off(stack.len() - 2);
                if let Entry::Slot(k) = args[0] {
                    uses.entry(k).or_default().vector = true;
                }
                stack.push(Entry::Other);
                skip_meta = true;
            }

            OpCode::CALLGLOBAL | OpCode::CALLGLOBALNOARITY | OpCode::CALLPRIMITIVE => {
                let call_arity = code
                    .get(idx + 1)
                    .filter(|n| matches!(n.op_code, OpCode::FUNC | OpCode::FUNCNOARITY))
                    .map(|n| n.payload_size.to_usize());
                match call_arity {
                    Some(n) if n <= stack.len() => {
                        let args = stack.split_off(stack.len() - n);
                        let getter = globals
                            .get(payload)
                            .cloned()
                            .and_then(create_struct_spec)
                            .filter(|spec| {
                                matches!(
                                    spec.typ,
                                    StructFunctionType::GetterProto
                                        | StructFunctionType::GetterProtoVec(_)
                                )
                            });
                        if let (Some(spec), Some(Entry::Slot(k))) = (getter, args.first()) {
                            uses.entry(*k).or_default().structs.insert(spec.descriptor);
                        }
                        // `null?` is a separate opcode and stays compatible, since
                        // the empty list is a list.
                        let is_type_test = n == 1
                            && ins.op_code == OpCode::CALLPRIMITIVE
                            && matches!(globals.get(payload), Some(SteelVal::FuncV(f))
                                if [
                                    steel_pair as FunctionSignature,
                                    steel_listp as FunctionSignature,
                                    steel_symbolp as FunctionSignature,
                                    steel_stringp as FunctionSignature,
                                    steel_voidp as FunctionSignature,
                                    steel_eof_objectp as FunctionSignature,
                                ]
                                .contains(f));
                        if let (true, Some(Entry::Slot(k))) = (is_type_test, args.first()) {
                            uses.entry(*k).or_default().type_tested = true;
                        }
                        stack.push(Entry::Other);
                        skip_meta = true;
                    }
                    _ => stack.clear(),
                }
            }

            OpCode::IF | OpCode::LetVar | OpCode::POPSINGLE => {
                stack.pop();
            }

            OpCode::SETLOCAL => {
                mutated.insert(payload);
                stack.clear();
            }

            _ => stack.clear(),
        }
        idx += 1;
    }

    // `(+ i sum)` with `i` carrying floats makes `sum` a float too.
    loop {
        let before = floats.len();
        for group in &numeric_groups {
            if group.iter().any(|k| floats.contains(k)) {
                floats.extend(group.iter().copied());
            }
        }
        if floats.len() == before {
            break;
        }
    }

    let mut seed = Vec::new();
    for (slot, u) in uses {
        // A guess from usage is only worth a guard when the slot has one type.
        // A slot that is type tested (e.g. `(pair? a)` before `(car a)`) is
        // expected to see other types, so the guard would often fail.
        if mutated.contains(&slot) || u.type_tested {
            continue;
        }
        let kinds = u.numeric as usize
            + u.list as usize
            + u.vector as usize
            + (!u.structs.is_empty()) as usize;
        if kinds != 1 || u.structs.len() > 1 {
            continue;
        }
        let ty = if u.numeric {
            if floats.contains(&slot) {
                SpecType::Float
            } else {
                SpecType::Fixnum
            }
        } else if u.list {
            SpecType::List
        } else if u.vector {
            SpecType::MutableVector
        } else {
            SpecType::Struct(*u.structs.iter().next().unwrap())
        };
        seed.push((slot, ty));
    }
    seed
}

/// At the top of a generic copy, tail call into the specialized copy when every
/// seeded argument slot holds its seeded type. These are raw loads emitted before the
/// translator exists, so none of its caches see them.
fn emit_spec_entry_guard(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    vm_ctx: Value,
    spec_id: FuncId,
    seed: &[(usize, SpecType)],
    arity: u16,
) {
    let thread = builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
        vm_ctx,
        offset_of!(VmCore, thread) as i32,
    );
    let buf_ptr = builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
        thread,
        (offset_of!(SteelThread, stack) + steel_vec::Vec::<SteelVal>::buf_offset()) as i32,
    );
    let sp = builder.ins().load(
        types::I64,
        MemFlagsData::trusted(),
        vm_ctx,
        offset_of!(VmCore, sp) as i32,
    );
    let sp_bytes = builder.ins().ishl_imm_u(sp, 4);
    let frame_base = builder.ins().iadd(buf_ptr, sp_bytes);

    let body_block = builder.create_block();
    let checks: Vec<(usize, SpecType)> = seed
        .iter()
        .copied()
        .filter(|(k, _)| *k < arity as usize)
        .collect();
    emit_slot_type_checks(builder, frame_base, &checks, body_block);

    let callee = module.declare_func_in_func(spec_id, builder.func);
    builder.ins().return_call(callee, &[vm_ctx]);

    builder.seal_block(body_block);
    builder.switch_to_block(body_block);
}

/// Whether a reachable exit of `func` returns a value whose type was not
/// recorded. Unreachable blocks are skipped. The translator parks code after a
/// return in a fresh block with no predecessors, and ends every function with
/// a `return` in one, so counting those would erase every return type.
fn has_untyped_exit(
    func: &cranelift::codegen::ir::Function,
    typed: &HashSet<cranelift::codegen::ir::Inst>,
) -> bool {
    use cranelift::codegen::flowgraph::ControlFlowGraph;
    use cranelift::codegen::ir::Opcode;

    let Some(entry) = func.layout.entry_block() else {
        return false;
    };
    let cfg = ControlFlowGraph::with_function(func);
    let mut seen = HashSet::new();
    let mut work = vec![entry];
    while let Some(block) = work.pop() {
        if !seen.insert(block) {
            continue;
        }
        for inst in func.layout.block_insts(block) {
            let is_exit = matches!(
                func.dfg.insts[inst].opcode(),
                Opcode::Return | Opcode::ReturnCall | Opcode::ReturnCallIndirect
            );
            if is_exit && !typed.contains(&inst) {
                return true;
            }
        }
        work.extend(cfg.succ_iter(block));
    }
    false
}

fn discriminant(value: &SteelVal) -> u8 {
    // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
    // between `repr(C)` structs, each of which has the `u8` discriminant as its first
    // field, so we can read the discriminant without offsetting the pointer.
    unsafe { *(value as *const SteelVal).cast::<u8>() }
}

/// A reference count increment owed to a value baked into generated code,
/// to be paid by the thread that owns the value (see [`jit_leak`]).
pub(crate) struct DeferredLeak(Box<dyn FnOnce()>);

// The closure only ever runs on the thread that queued the compile.
unsafe impl Send for DeferredLeak {}

impl DeferredLeak {
    pub(crate) fn pay(self) {
        (self.0)()
    }
}

std::thread_local! {
    static DEFERRED_LEAKS: core::cell::RefCell<Option<Vec<DeferredLeak>>> =
        const { core::cell::RefCell::new(None) };
}

/// Compiles on this thread from now on owe their leaks to another thread.
pub(crate) fn begin_deferred_leaks() {
    DEFERRED_LEAKS.with(|leaks| *leaks.borrow_mut() = Some(Vec::new()));
}

pub(crate) fn take_deferred_leaks() -> Vec<DeferredLeak> {
    DEFERRED_LEAKS.with(|leaks| leaks.borrow_mut().take().unwrap_or_default())
}

/// A reference to `value` for generated code to hold forever. The caller
/// consumes the result without dropping it (`into_raw`, a transmute).
///
/// Normally that is a clone. On the background compile thread it is a bitwise
/// copy, with the increment queued for the thread that owns `value`. Under
/// biased reference counting an increment from another thread lands in the
/// shared count, and once the owner's count drains the object merges and every
/// later clone and drop of it is atomic. The compile job keeps `value` alive
/// until the owner has paid.
pub(crate) fn jit_leak<T: Clone + 'static>(value: &T) -> T {
    DEFERRED_LEAKS.with(|leaks| match leaks.borrow_mut().as_mut() {
        Some(leaks) => {
            let copy = core::mem::ManuallyDrop::new(unsafe { core::ptr::read(value) });
            leaks.push(DeferredLeak(Box::new(move || {
                core::mem::forget(T::clone(&copy));
            })));
            unsafe { core::ptr::read(value) }
        }
        None => value.clone(),
    })
}

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct JitFnPointer(pub(crate) NonNull<u8>);

unsafe impl Send for JitFnPointer {}
unsafe impl Sync for JitFnPointer {}

// Compile the bytecode assuming that things... work okay?
unsafe fn compile_bytecode(
    jit: &mut JIT,
    name: String,
    arity: u16,
    code: &[DenseInstruction],
    globals: &[SteelVal],
    constants: &ConstantMap,
    function_index: Option<usize>,
    slot: Option<&Gc<ByteCodeLambda>>,
    top_level_name: Option<InternedString>,
    closed_globals: &HashSet<usize>,
) -> Result<JitFnPointer, String> {
    let code_ptr = jit.compile(
        name,
        arity,
        code,
        globals,
        constants,
        function_index,
        slot,
        top_level_name,
        closed_globals,
    )?;
    let code_fn = JitFnPointer(NonNull::new_unchecked(code_ptr.cast_mut()));

    Ok(code_fn)
}

// perf metadata for jitted code, off by default since both write a file named
// after the pid and cost a syscall per compiled function. STEEL_JIT_PERF_MAP
// appends to /tmp/perf-<pid>.map for perf report, STEEL_JIT_DUMP writes
// jit-<pid>.dump in the cwd for perf inject --jit. Read once, since these are
// on the compile path.
fn perf_map_enabled() -> bool {
    crate::config::config().jit_perf_map
}

#[cfg(target_os = "linux")]
fn jitdump_enabled() -> bool {
    crate::config::config().jit_dump
}

// Returns None if we can't make the file, since a read only working directory
// is a normal way to run.
#[cfg(target_os = "linux")]
fn open_jitdump() -> Option<wasmtime_jit_debug::perf_jitdump::JitDumpFile> {
    use object::elf;
    use target_lexicon::Architecture;

    if !jitdump_enabled() {
        return None;
    }

    let e_machine = match target_lexicon::HOST.architecture {
        Architecture::X86_64 => elf::EM_X86_64 as u32,
        Architecture::Aarch64(_) => elf::EM_AARCH64 as u32,
        Architecture::Arm(_) => elf::EM_ARM as u32,
        other => {
            log::warn!(target: "jit", "STEEL_JIT_DUMP is not supported on {other:?}");
            return None;
        }
    };

    let path = format!("./jit-{}.dump", std::process::id());

    match wasmtime_jit_debug::perf_jitdump::JitDumpFile::new(&path, e_machine) {
        Ok(file) => Some(file),
        Err(e) => {
            log::warn!(target: "jit", "unable to open {path} for STEEL_JIT_DUMP: {e}");
            None
        }
    }
}

// Write an entry to `/tmp/perf-<pid>.map` so that perf can resolve function addresses
// to names in flamegraphs with the format: `<start_hex> <size_hex> <name>`
fn write_perf_map_entry(addr: *const u8, size: usize, name: &str) {
    use std::io::Write;

    if !perf_map_enabled() {
        return;
    }

    let pid = std::process::id();
    let path = format!("/tmp/perf-{}.map", pid);
    if let Ok(mut file) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
    {
        let _ = writeln!(file, "{:x} {:x} {}", addr as usize, size, name);
        let _ = file.flush();
    }
}

impl JIT {
    pub fn compile_bytecode(
        &mut self,
        name: String,
        arity: u16,
        code: &[DenseInstruction],
        globals: &[SteelVal],
        constants: &ConstantMap,
        function_index: Option<usize>,
        slot: Option<&Gc<ByteCodeLambda>>,
        top_level_name: Option<InternedString>,
        closed_globals: &HashSet<usize>,
    ) -> Result<JitFnPointer, String> {
        unsafe {
            compile_bytecode(
                self,
                name,
                arity,
                code,
                globals,
                constants,
                function_index,
                slot,
                top_level_name,
                closed_globals,
            )
        }
    }
}

impl JIT {
    // Use this to get the trampoline, and then our new entrypoint from
    // the rust VM is a trampoline into the tail call world.
    pub fn compile_trampoline(&mut self) -> *const u8 {
        let frontend_config = self.module.target_config();
        let ptr_ty = frontend_config.pointer_type();

        // Outer signature: C ABI, takes (vm_ctx, target)
        let mut outer_sig = self.module.make_signature();
        let mut vm_param = AbiParam::new(ptr_ty);
        vm_param.purpose = ArgumentPurpose::VMContext;
        outer_sig.params.push(vm_param);
        outer_sig.params.push(AbiParam::new(ptr_ty));
        outer_sig.returns.push(AbiParam::new(types::I128));

        let mut inner_sig = self.module.make_signature();
        let mut vm_param = AbiParam::new(ptr_ty);
        vm_param.purpose = ArgumentPurpose::VMContext;
        inner_sig.params.push(vm_param);
        inner_sig.call_conv = CallConv::Tail;

        inner_sig.returns.push(AbiParam::new(types::I128));

        let func_id = self
            .module
            // .declare_function("jit_trampoline", Linkage::Local, &outer_sig)
            .declare_function("jit_trampoline", Linkage::Export, &outer_sig)
            .unwrap();

        self.ctx.func.signature = outer_sig;
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let vm_ctx = builder.block_params(block)[0];
        let target = builder.block_params(block)[1];

        let sig_ref = builder.import_signature(inner_sig);

        let call = builder.ins().call_indirect(sig_ref, target, &[vm_ctx]);

        let result = builder.inst_results(call)[0];

        builder.ins().return_(&[result]);

        builder.seal_all_blocks();
        builder.finalize(frontend_config);

        self.module.define_function(func_id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();
        self.module.get_finalized_function(func_id)
    }

    // Tier 1 JIT.
    // Tier 2 jit should be a little bit more advanced:
    // We should be able to compile straight into a function pointer
    // that accepts the context.
    //
    // The calling convention of the function should be a bit more
    // concrete if we know that this thing is closed - i.e. it doesn't
    // call any other functions except native functions or itself.
    //
    // If that is the case, we can directly `call` it (or `tailcall`)
    // rather than doing the weird calling convention that we're doing now.
    //
    // On top of that, we also can convert TCOs into loops more effectively
    // than we're doing now, since the TCOs just yield back to the runtime.
    //
    // We should just convert the TCOs back into the loop, and set up blocks
    // for the args, so something like:
    //
    // Entry block <block args>
    // Pass those args in
    //
    // jump back to the top
    // etc.
    pub fn compile(
        &mut self,
        name: String,
        arity: u16,
        instructions: &[DenseInstruction],
        globals: &[SteelVal],
        constants: &ConstantMap,
        function_index: Option<usize>,
        slot: Option<&Gc<ByteCodeLambda>>,
        top_level_name: Option<InternedString>,
        closed_globals: &HashSet<usize>,
    ) -> Result<*const u8, String> {
        let id = str::parse::<u32>(&name).unwrap();

        // `STEEL_JIT_LOG` prints candidates by id; this prints their names, for
        // use with `STEEL_JIT_SKIP` when bisecting.
        if crate::config::config().jit_log {
            eprintln!("jit: compiling id {id} -> {top_level_name:?}");
        }

        // `STEEL_JIT_DUMP_ID=<id>,...` prints the bytecode of those functions.
        if crate::config::config()
            .jit_dump_id
            .as_ref()
            .is_some_and(|ids| ids.contains(&id))
        {
            eprintln!("jit: bytecode for id {id} ({top_level_name:?}):");
            for (i, ins) in instructions.iter().enumerate() {
                eprintln!(
                    "    {i:3}  {:?} {}",
                    ins.op_code,
                    ins.payload_size.to_usize()
                );
            }
        }

        let inner_name = if let Some(top_level_name) = top_level_name {
            format!("{}_{}_inner", top_level_name, name)
        } else {
            // Store the name
            format!("{}_inner", name)
        };

        self.names.insert(id, inner_name.clone());

        if self.defined.contains(&inner_name) {
            if let Some(data) = self.module.get_name(&inner_name) {
                match data {
                    cranelift_module::FuncOrDataId::Func(func_id) => {
                        return Ok(self.module.get_finalized_function(func_id));
                    }
                    cranelift_module::FuncOrDataId::Data(_) => panic!(),
                }
            }
        }

        let stmts = instructions;

        self.init_jit_signature();

        let inner_id = self
            .module
            .declare_function(&inner_name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // Loop specialization. Declared up front because the generic copy tail
        // calls it. It is always defined once the generic copy is (see
        // `compile_specialized`), so the reference is never left dangling.
        let seed = if loop_specialization_enabled()
            // For bisecting a specialization problem to one function. Comma
            // separated name substrings to allow (`STEEL_JIT_SPEC_ONLY`) or
            // exclude (`STEEL_JIT_SPEC_SKIP`).
            && crate::config::config().jit_spec_only.as_ref().map_or(true, |parts| {
                parts.iter().any(|part| inner_name.contains(part.as_str()))
            })
            && crate::config::config().jit_spec_skip.as_ref().map_or(true, |parts| {
                !parts.iter().any(|part| inner_name.contains(part.as_str()))
            }) {
            loop_type_seed(stmts, arity, constants, globals, function_index)
        } else {
            Vec::new()
        };
        if spec_debug_enabled() && !seed.is_empty() {
            eprintln!("[spec] seed for {inner_name}: {seed:?}");
        }
        let spec = if seed.is_empty() {
            None
        } else {
            let spec_name = format!("{}_spec", inner_name);
            let spec_id = self
                .module
                .declare_function(&spec_name, Linkage::Local, &self.ctx.func.signature)
                .map_err(|e| e.to_string())?;
            Some((spec_id, spec_name))
        };
        // The specialized copy is compiled first, because the generic copy only
        // hands off to it if it compiled. If it did not, it is still defined as a
        // stub, but the generic copy gets no guard so nothing calls it. A guard in
        // front of a stub that calls back into the generic copy loops forever.
        let mut spec_code_size = None;

        // Frameless self calls, for a specialized copy whose body is simple
        // enough (see `frameless_eligible`). The frameless copy comes first since
        // the specialized copy only calls it if it compiled.
        let frameless = match &spec {
            Some(_) if frameless_enabled() && frameless_eligible(stmts, function_index) => {
                let frameless_name = format!("{}_frameless", inner_name);
                let mut sig = self.module.make_signature();
                let mut vm_param = AbiParam::new(self.module.target_config().pointer_type());
                vm_param.purpose = ArgumentPurpose::VMContext;
                sig.params.push(vm_param);
                sig.params.push(AbiParam::new(types::I64));
                sig.call_conv = CallConv::Tail;
                sig.returns.push(AbiParam::new(types::I128));
                let frameless_id = self
                    .module
                    .declare_function(&frameless_name, Linkage::Local, &sig)
                    .map_err(|e| e.to_string())?;
                let compiled = self.compile_frameless(
                    id,
                    frameless_id,
                    &frameless_name,
                    inner_id,
                    arity,
                    stmts,
                    globals,
                    constants,
                    function_index,
                    slot,
                    closed_globals,
                    seed.clone(),
                );
                self.module.clear_context(&mut self.ctx);
                self.init_jit_signature();
                compiled.map(|result| FramelessInfo {
                    id: frameless_id,
                    role: FramelessRole::Boundary,
                    result,
                })
            }
            _ => None,
        };

        let mode = match &spec {
            Some((spec_id, spec_name)) => {
                self.module.clear_context(&mut self.ctx);
                let (size, compiled) = self.compile_specialized(
                    id,
                    *spec_id,
                    spec_name,
                    inner_id,
                    arity,
                    stmts,
                    globals,
                    constants,
                    function_index,
                    slot,
                    closed_globals,
                    seed.clone(),
                    frameless,
                );
                if compiled {
                    spec_code_size = Some(size);
                }
                self.init_jit_signature();
                if compiled {
                    SpecMode::Generic {
                        spec_id: *spec_id,
                        seed: seed.clone(),
                    }
                } else {
                    SpecMode::None
                }
            }
            None => SpecMode::None,
        };

        // Then, translate the AST nodes into Cranelift IR. A panic in
        // translation (an internal check, like `check_collection_roots`) is
        // treated as a failure to compile. The error path below still defines the
        // stub a specialized copy needs, so no declaration is left undefined.
        let translated = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.translate(
                id,
                inner_name.clone(),
                inner_id,
                arity,
                stmts,
                globals,
                constants,
                function_index,
                slot,
                closed_globals,
                mode,
                None,
            )
        })) {
            Ok(translated) => translated,
            Err(_) => {
                self.builder_context = FunctionBuilderContext::new();
                Err("translate panicked".to_string())
            }
        };

        // `STEEL_JIT_DUMP_CLIF=<substring>` prints the ir for every function whose
        // name contains it. `=1` prints only a function the verifier rejected.
        if let Some(filter) = &crate::config::config().jit_dump_clif {
            if filter != "1" && inner_name.contains(filter.as_str()) {
                eprintln!("--- clif for {} ---\n{}", inner_name, self.ctx.func);
                // The ir prints callees as `fnN = u0:M`; M is the module's FuncId,
                // so this legend turns them back into the helper names.
                let mut legend: Vec<String> = Vec::new();
                for (fref, data) in self.ctx.func.dfg.ext_funcs.iter() {
                    if let cranelift::codegen::ir::ExternalName::User(nameref) = data.name {
                        let user = &self.ctx.func.params.user_named_funcs()[nameref];
                        let decl = self
                            .module
                            .declarations()
                            .get_function_decl(cranelift_module::FuncId::from_u32(user.index));
                        legend.push(format!(
                            "{} = {}",
                            fref,
                            decl.name.as_deref().unwrap_or("<anon>")
                        ));
                    }
                }
                eprintln!("--- callees ---\n{}", legend.join("\n"));
            }
        }

        let generic_failure = match translated {
            Err(e) => Some(e),
            Ok(_) => match cranelift::codegen::verify_function(&self.ctx.func, self.module.isa()) {
                Err(e) => {
                    // STEEL_JIT_DUMP_CLIF=1 prints the function that failed, for
                    // tracing a dominance error back to the block that defines the value.
                    if crate::config::config().jit_dump_clif.as_deref() == Some("1") {
                        eprintln!("--- clif for failed function ---\n{}", self.ctx.func);
                    }
                    eprintln!("{:#?}", e);
                    Some(format!("errors: {:#?}", e))
                }
                Ok(()) => self
                    .module
                    .define_function(inner_id, &mut self.ctx)
                    .err()
                    .map(|e| {
                        eprintln!("error in defining function: {}", e);
                        e.to_string()
                    }),
            },
        };

        if let Some(e) = generic_failure {
            self.module.clear_context(&mut self.ctx);
            // The specialized and frameless copies (and their stubs) call back
            // into this one, so this name has to be defined before anything is
            // finalized, or the next finalize can't resolve it. The error below
            // keeps the function interpreted, so the stub hands the call to the
            // interpreter from the first instruction.
            {
                if let Err(stub) = self.define_stub(inner_id, StubBody::InterpretFromStart) {
                    log::debug!(target: "jit", "{inner_name}: stub not defined either: {stub}");
                }
            }
            return Err(e);
        }

        self.defined.insert(inner_name.clone());

        // This is for perf
        let code_size = self
            .ctx
            .compiled_code()
            .map(|cc| cc.code_buffer().len())
            .unwrap_or(0);

        self.module.clear_context(&mut self.ctx);

        self.module
            .finalize_definitions()
            .map_err(|e| e.to_string())?;

        let code = self.module.get_finalized_function(inner_id);

        // Lets figure out... what we need here
        write_perf_map_entry(code, code_size, &inner_name);
        if let (Some((spec_id, spec_name)), Some(size)) = (&spec, spec_code_size) {
            let spec_code = self.module.get_finalized_function(*spec_id);
            write_perf_map_entry(spec_code, size, spec_name);

            #[cfg(target_os = "linux")]
            if let Some(jitdump) = self.jitdump.as_mut() {
                let code_bytes = unsafe { std::slice::from_raw_parts(spec_code, size) };
                let timestamp = jitdump.get_time_stamp();
                let pid = std::process::id();
                let tid = rustix::thread::gettid().as_raw_nonzero().get() as u32;
                if let Err(e) =
                    jitdump.dump_code_load_record(spec_name, code_bytes, timestamp, pid, tid)
                {
                    log::warn!(target: "jit", "failed to write a jitdump record: {e}; disabling");
                    self.jitdump = None;
                }
            }
        }

        #[cfg(target_os = "linux")]
        if let Some(jitdump) = self.jitdump.as_mut() {
            // after finalize_definitions(), for each function:
            let code_bytes = unsafe { std::slice::from_raw_parts(code, code_size) };
            let timestamp = jitdump.get_time_stamp();
            let pid = std::process::id();
            let tid = rustix::thread::gettid().as_raw_nonzero().get() as u32;

            if let Err(e) =
                jitdump.dump_code_load_record(&inner_name, code_bytes, timestamp, pid, tid)
            {
                // Skip the profiling output instead of panicking mid compile
                log::warn!(target: "jit", "failed to write a jitdump record: {e}; disabling");
                self.jitdump = None;
            }
        }

        Ok(code)
    }

    /// The vmctx-in, value-out tail signature every jitted function shares.
    fn init_jit_signature(&mut self) {
        let pointer = self.module.target_config().pointer_type();

        let mut param = AbiParam::new(pointer);
        param.purpose = ArgumentPurpose::VMContext;

        self.ctx.func.signature.params.push(param);
        self.ctx.func.signature.call_conv = CallConv::Tail;

        // Return a value. If we concretely return a value,
        // we're going to avoid writing it to the stack
        // to save some time.
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I128));
    }

    /// `init_jit_signature` plus the frame base a frameless copy takes as its
    /// second parameter.
    fn init_frameless_signature(&mut self) {
        self.init_jit_signature();
        self.ctx
            .func
            .signature
            .params
            .push(AbiParam::new(types::I64));
    }

    /// `STEEL_JIT_DUMP_CLIF=<substring>`, applied to the specialized and
    /// frameless copies too.
    fn dump_clif_if_asked(&mut self, name: &str) {
        if let Some(filter) = &crate::config::config().jit_dump_clif {
            if filter != "1" && name.contains(filter.as_str()) {
                eprintln!("--- clif for {} ---\n{}", name, self.ctx.func);
            }
        }
        // `STEEL_JIT_DUMP_ASM` has cranelift keep the disassembly of what it is
        // about to emit; `dump_asm_if_asked` prints it once defined.
        if crate::config::config()
            .jit_dump_asm
            .as_ref()
            .is_some_and(|filter| name.contains(filter.as_str()))
        {
            self.ctx.set_disasm(true);
        }
    }

    /// The machine code `dump_clif_if_asked` asked cranelift to keep.
    fn dump_asm_if_asked(&mut self, name: &str) {
        if let Some(code) = self.ctx.compiled_code().and_then(|c| c.vcode.clone()) {
            eprintln!("--- asm for {name} ---\n{code}");
        }
        self.ctx.set_disasm(false);
    }

    /// Compile the `_frameless` copy of a self recursive function; see
    /// `frameless_enabled`. Returns the validated return type when it compiled,
    /// and `None` when it did not. In that case it is defined as a stub that
    /// nothing calls, because a declared function has to be defined.
    fn compile_frameless(
        &mut self,
        id: u32,
        frameless_id: FuncId,
        name: &str,
        generic_id: FuncId,
        arity: u16,
        stmts: &[DenseInstruction],
        globals: &[SteelVal],
        constants: &ConstantMap,
        function_index: Option<usize>,
        slot: Option<&Gc<ByteCodeLambda>>,
        closed_globals: &HashSet<usize>,
        seed: Vec<(usize, SpecType)>,
    ) -> Option<Option<InferredType>> {
        // Fixnum (float) seeded functions most likely return fixnums (floats),
        // and a frameless copy's self calls return what it returns, so assume it
        // and check, as `compile_specialized` does.
        let guess = if seed.iter().any(|(_, t)| *t == SpecType::Fixnum) {
            Some(InferredType::Int)
        } else if seed.iter().any(|(_, t)| *t == SpecType::Float) {
            Some(InferredType::Float)
        } else {
            None
        };

        let attempt = |this: &mut Self, assume: Option<InferredType>| {
            this.module.clear_context(&mut this.ctx);
            this.builder_context = FunctionBuilderContext::new();
            this.init_frameless_signature();
            let mode = SpecMode::Specialized {
                generic_id,
                self_id: frameless_id,
                seed: seed.clone(),
                assume_result: assume,
            };
            let info = FramelessInfo {
                id: frameless_id,
                role: FramelessRole::Callee,
                result: assume,
            };
            let translated = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                this.translate(
                    id,
                    name.to_string(),
                    frameless_id,
                    arity,
                    stmts,
                    globals,
                    constants,
                    function_index,
                    slot,
                    closed_globals,
                    mode,
                    Some(info),
                )
            }));
            match translated {
                Ok(Ok(info))
                    if !info.untyped_non_deopt_exit
                        && assume
                            .map_or(true, |t| info.returned.iter().all(|r| r.boxed() == t)) =>
                {
                    match cranelift::codegen::verify_function(&this.ctx.func, this.module.isa()) {
                        Ok(()) => true,
                        Err(e) => {
                            if spec_debug_enabled() {
                                eprintln!("[frameless] {name}: verify failed: {e:#?}");
                            }
                            false
                        }
                    }
                }
                Ok(Ok(_)) => false,
                Ok(Err(e)) => {
                    if spec_debug_enabled() {
                        eprintln!("[frameless] {name}: {e}");
                    }
                    false
                }
                Err(_) => false,
            }
        };

        let mut result = None;
        if attempt(self, guess) {
            result = Some(guess);
        } else if guess.is_some() && attempt(self, None) {
            result = Some(None);
        }

        if result.is_some() {
            self.dump_clif_if_asked(&name);
        }
        if result.is_some() && {
            let defined = self.module.define_function(frameless_id, &mut self.ctx);
            self.dump_asm_if_asked(&name);
            defined.is_err()
        } {
            result = None;
        }
        if result.is_none() {
            if let Err(e) =
                self.define_stub_with_signature(frameless_id, StubBody::InterpretFromStart, true)
            {
                log::debug!(target: "jit", "{name}: frameless stub not defined either: {e}");
            }
        } else if spec_debug_enabled() {
            eprintln!("[frameless] compiled {name}, returns {:?}", result.unwrap());
        }
        self.module.clear_context(&mut self.ctx);
        result
    }

    /// Compile and define the specialized copy, returning its code size. It
    /// must always end up defined since the generic copy already tail calls it.
    /// On a failure, including a panic in the translator, it is defined as a stub
    /// that tail calls back into the generic copy. A panic that escaped would
    /// also poison the jit's shared mutex.
    #[allow(clippy::too_many_arguments)]
    fn compile_specialized(
        &mut self,
        id: u32,
        spec_id: FuncId,
        spec_name: &str,
        generic_id: FuncId,
        arity: u16,
        stmts: &[DenseInstruction],
        globals: &[SteelVal],
        constants: &ConstantMap,
        function_index: Option<usize>,
        slot: Option<&Gc<ByteCodeLambda>>,
        closed_globals: &HashSet<usize>,
        seed: Vec<(usize, SpecType)>,
        frameless: Option<FramelessInfo>,
    ) -> (usize, bool) {
        self.init_jit_signature();

        let translate_with = |this: &mut Self, assume_result: Option<InferredType>| {
            let mode = SpecMode::Specialized {
                generic_id,
                self_id: spec_id,
                seed: seed.clone(),
                assume_result,
            };
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                this.translate(
                    id,
                    spec_name.to_string(),
                    spec_id,
                    arity,
                    stmts,
                    globals,
                    constants,
                    function_index,
                    slot,
                    closed_globals,
                    mode,
                    frameless,
                )
            }))
        };

        self.allow_cursors = list_cursor_enabled();
        let mut translated = translate_with(self, None);

        // A cursor the translator could not keep sound spoils the whole
        // translation, so redo it without cursors.
        if matches!(&translated, Ok(Ok(info)) if info.cursor_bailed) {
            if spec_debug_enabled() {
                eprintln!("[spec] {spec_name}: list cursors bailed, retranslating without");
            }
            self.module.clear_context(&mut self.ctx);
            self.builder_context = FunctionBuilderContext::new();
            self.init_jit_signature();
            self.allow_cursors = false;
            translated = translate_with(self, None);
        }

        // A recursive function's direct self calls return whatever this copy
        // returns, which is circular. In a first translation those results are
        // untyped, so its returns cannot suggest the type. Guess from the seed
        // instead (a function specialized on fixnums most likely returns one) and
        // translate again with direct self call results assumed to have that type.
        // The assumption is sound when every return of that translation has the
        // type and nothing else leaves the function except deopt exits (whose
        // callers check before using the value), so it is kept only then.
        let candidate = match &translated {
            Ok(Ok(info)) if info.direct_self_calls > 0 => {
                if seed.iter().any(|(_, t)| *t == SpecType::Fixnum) {
                    Some(InferredType::Int)
                } else if seed.iter().any(|(_, t)| *t == SpecType::Float) {
                    Some(InferredType::Float)
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(assumed) = candidate {
            self.module.clear_context(&mut self.ctx);
            self.init_jit_signature();
            let second = translate_with(self, Some(assumed));
            if spec_debug_enabled() {
                if let Ok(Ok(info)) = &second {
                    eprintln!(
                        "[spec] {spec_name}: with the assumption: returned {:?}, direct self calls {}, untyped exit {}",
                        info.returned, info.direct_self_calls, info.untyped_non_deopt_exit
                    );
                }
            }
            let holds = matches!(&second, Ok(Ok(info))
                if !info.untyped_non_deopt_exit
                    && !info.cursor_bailed
                    && info.returned.iter().all(|t| t.boxed() == assumed));
            if spec_debug_enabled() {
                eprintln!(
                    "[spec] {spec_name}: assumed self calls return {assumed:?}: {}",
                    if holds { "kept" } else { "rejected" }
                );
            }
            if holds {
                translated = second;
            } else {
                self.module.clear_context(&mut self.ctx);
                self.builder_context = FunctionBuilderContext::new();
                self.init_jit_signature();
                translated = translate_with(self, None);
            }
        }

        self.allow_cursors = false;

        let failure = match translated {
            Ok(Ok(_)) => {
                match cranelift::codegen::verify_function(&self.ctx.func, self.module.isa()) {
                    Ok(()) => match {
                        self.dump_clif_if_asked(&spec_name);
                        let defined = self.module.define_function(spec_id, &mut self.ctx);
                        self.dump_asm_if_asked(&spec_name);
                        defined
                    } {
                        Ok(()) => None,
                        Err(e) => Some(format!("define: {e}")),
                    },
                    Err(e) => {
                        if crate::config::config().jit_dump_clif.as_deref() == Some("1") {
                            eprintln!(
                                "--- clif for failed specialized copy ---\n{}",
                                self.ctx.func
                            );
                        }
                        Some(format!("verify: {e:#?}"))
                    }
                }
            }
            Ok(Err(e)) => Some(format!("translate: {e}")),
            Err(_) => Some("translate panicked".to_string()),
        };

        let compiled = failure.is_none();
        if let Some(reason) = failure {
            if spec_debug_enabled() {
                eprintln!("[spec] {spec_name} fell back to a stub: {reason}");
            }
            log::debug!(target: "jit", "{spec_name} fell back to a stub: {reason}");
            self.module.clear_context(&mut self.ctx);
            // Never called since the generic copy gets no guard, but it is
            // declared, so it has to be defined.
            if let Err(e) = self.define_stub(spec_id, StubBody::TailCall(generic_id)) {
                log::debug!(target: "jit", "{spec_name}: stub not defined either: {e}");
            }
            return (0, false);
        } else if spec_debug_enabled() {
            eprintln!("[spec] compiled {spec_name}");
        }

        let size = self
            .ctx
            .compiled_code()
            .map(|cc| cc.code_buffer().len())
            .unwrap_or(0);
        self.module.clear_context(&mut self.ctx);
        (size, compiled)
    }

    /// Define `func_id` as a tiny function, for a name that must exist but whose
    /// real translation failed. Returns its size. It can still fail to define
    /// when the code arena is full, and that must not panic since the shared jit
    /// is locked.
    fn define_stub(&mut self, func_id: FuncId, body: StubBody) -> Result<usize, String> {
        self.define_stub_with_signature(func_id, body, false)
    }

    /// `define_stub`, for a frameless copy's `(vm_ctx, sp)` signature when
    /// `frameless` is set.
    fn define_stub_with_signature(
        &mut self,
        func_id: FuncId,
        body: StubBody,
        frameless: bool,
    ) -> Result<usize, String> {
        // The translator may have been torn down mid block.
        self.module.clear_context(&mut self.ctx);
        self.builder_context = FunctionBuilderContext::new();
        if frameless {
            self.init_frameless_signature();
        } else {
            self.init_jit_signature();
        }
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry = builder.create_block();
            builder.append_block_params_for_function_params(entry);
            builder.switch_to_block(entry);
            builder.seal_block(entry);
            let vm_ctx = builder.block_params(entry)[0];
            match body {
                StubBody::TailCall(target) => {
                    let target = self.module.declare_func_in_func(target, builder.func);
                    builder.ins().return_call(target, &[vm_ctx]);
                }
                StubBody::InterpretFromStart => {
                    let zero = builder.ins().iconst(types::I64, 0);
                    builder.ins().store(
                        MemFlagsData::trusted(),
                        zero,
                        vm_ctx,
                        offset_of!(VmCore, ip) as i32,
                    );
                    let not_native = builder.ins().iconst(types::I8, 0);
                    builder.ins().store(
                        MemFlagsData::trusted(),
                        not_native,
                        vm_ctx,
                        offset_of!(VmCore, is_native) as i32,
                    );
                    let tag = builder.ins().iconst(types::I64, SteelVal::VOID_TAG as i64);
                    let payload = builder.ins().iconst(types::I64, 0);
                    let void = builder.ins().iconcat(tag, payload);
                    builder.ins().return_(&[void]);
                }
            }
            builder.finalize(self.module.target_config());
        }
        let defined = self.module.define_function(func_id, &mut self.ctx);
        let size = self
            .ctx
            .compiled_code()
            .map(|cc| cc.code_buffer().len())
            .unwrap_or(0);
        self.module.clear_context(&mut self.ctx);
        defined.map(|()| size).map_err(|e| e.to_string())
    }

    fn translate(
        &mut self,
        id: u32,
        name: String,
        _func_id: FuncId,
        arity: u16,
        bytecode: &[DenseInstruction],
        globals: &[SteelVal], // stmts: Vec<Expr>,
        constants: &ConstantMap,
        function_context: Option<usize>,
        slot: Option<&Gc<ByteCodeLambda>>,
        closed_globals: &HashSet<usize>,
        spec_mode: SpecMode,
        frameless: Option<FramelessInfo>,
    ) -> Result<TranslateInfo, String> {
        let int = Type::int(128).unwrap();

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let contains_tail_call = bytecode
            .iter()
            .any(|x| matches!(x.op_code, OpCode::TCOJMP | OpCode::SELFTAILCALLNOARITY));

        // vmctx is the first signature param, so it arrives as the entry block's
        // first block param.
        let vm_context = builder.block_params(entry_block)[0];

        // The generic copy of a specialized loop checks its seeded slots on the
        // way in and hands the whole call to the specialized copy, first
        // iteration included. Checking at the tail call would run the first
        // iteration generically, and repeat a failing check on every iteration of
        // a loop whose slots are never fixnums.
        if let SpecMode::Generic { spec_id, seed } = &spec_mode {
            emit_spec_entry_guard(
                &mut builder,
                &mut self.module,
                vm_context,
                *spec_id,
                seed,
                arity,
            );
        }

        // Created now, jumped to once the translator exists. A loop that walks
        // lists as cursors seeds them in the entry block, ahead of the header,
        // so every back edge feeds the header's phis instead of each iteration
        // re-seeding from a slot that no longer holds the current position.
        let fake_entry_block = if contains_tail_call {
            Some(builder.create_block())
        } else {
            None
        };
        let allow_cursors = self.allow_cursors;

        let exit_block = builder.create_block();
        let mut exit_types = HashSet::new();

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            name,
            int,
            builder,
            module: &mut self.module,
            instructions: bytecode,
            ip: 0,
            _globals: globals,
            shadow_stack: Vec::new(),
            arity,
            constants,
            value_to_local_map: HashMap::new(),
            local_to_value_map: HashMap::new(),
            let_var_stack: Vec::new(),
            tco: false,
            intrinsics: &self.function_map,
            fake_entry_block,
            exit_block,
            deopt_return_block: None,
            frameless,
            frameless_bail: false,
            frameless_calls: 0,
            checked_native_block: None,
            pending_deopt_exits: Vec::new(),
            cursors_on: false,
            cursor_bail: false,
            cursor_advances: 0,
            cursor_carries: 0,
            cursor_copy_reads: 0,
            header_cursors: HashMap::new(),
            carry_skip: HashSet::new(),
            properties: Default::default(),
            visited: HashSet::default(),
            join_targets: bytecode
                .iter()
                .filter(|x| matches!(x.op_code, OpCode::JMP | OpCode::POPJMP | OpCode::IF))
                .map(|x| x.payload_size.to_usize())
                .collect(),
            depth: 0,
            if_stack: Vec::new(),
            if_bound: None,
            if_merge_blocks: Vec::new(),
            if_merge_flags: Vec::new(),
            pending_borrow: None,
            vm_context,
            slot,
            function_context,
            closed_globals,
            names: &self.names,
            function_return_types: &self.function_return_types,
            exit_types: &mut exit_types,
            typed_returns: HashSet::new(),
            thread_pointer: None,
            should_trampoline: None,
            sp: None,
            pop_count: None,
            pop_count_plus_one: None,
            pop_count_minus_one: None,
            thread_id: None,
            use_lbbv: crate::config::config().lbbv,
            known_tags: HashMap::new(),
            spec_mode: spec_mode.clone(),
            deopt_returns: HashSet::new(),
            direct_self_calls: 0,
        };

        match &spec_mode {
            // The generic copy only tail calls in with these slots holding these
            // types, and this copy only loops on itself while they still do.
            SpecMode::Specialized { seed, .. } => {
                for &(i, ty) in seed.iter().filter(|(i, _)| *i < arity as usize) {
                    trans
                        .properties
                        .set_property(ValueOrRegister::Register(i), ty.property());
                    trans.local_to_value_map.insert(i, ty.inferred());
                }
            }
            SpecMode::Generic { .. } | SpecMode::None => {}
        }

        // A frameless copy's frame base arrives as its second parameter, and it
        // is published to `VmCore.sp` for the length of the activation, so every
        // exit, helper and deopt sees this activation's slots as a framed one
        // would. The caller puts its own back when this returns.
        if matches!(
            frameless,
            Some(FramelessInfo {
                role: FramelessRole::Callee,
                ..
            })
        ) {
            let sp = trans.builder.block_params(entry_block)[1];
            trans.builder.ins().store(
                MemFlagsData::trusted(),
                sp,
                vm_context,
                offset_of!(VmCore, sp) as i32,
            );
            trans.sp = Some(sp);
        }

        trans.cursors_on = allow_cursors
            && frameless.is_none()
            && matches!(spec_mode, SpecMode::Specialized { .. })
            && fake_entry_block.is_some();

        if let Some(fake_entry) = fake_entry_block {
            if trans.cursors_on {
                trans.seed_header_cursors();
            }
            trans.builder.ins().jump(fake_entry, &[]);
            trans.builder.switch_to_block(fake_entry);
            trans.forget_entry_block_caches();
        }

        {
            let vm_ctx = trans.get_ctx();
            trans.get_thread_pointer(vm_ctx);
            trans.get_thread_id();
        }

        trans.stack_to_ssa();

        if let Some(fake_entry_block) = fake_entry_block {
            trans.builder.seal_block(fake_entry_block);
        }

        let void = trans.encode_void();

        trans.builder.ins().return_(&[void]);

        trans.builder.seal_block(exit_block);

        // Cold exits last (see `defer_deopt_exit`). After the body's own return,
        // so the builder is not left positioned inside one of them.
        trans.flush_deopt_exits();

        // Just seal all the blocks?
        trans.builder.seal_all_blocks();

        // Tell the builder we're done with this function.
        let frontend_config = trans.module.target_config();
        let deopts = trans.deopt_return_block.is_some();
        let untyped_exit = has_untyped_exit(trans.builder.func, &trans.typed_returns);
        let untyped_non_deopt_exit = {
            let mut known = trans.typed_returns.clone();
            known.extend(trans.deopt_returns.iter().copied());
            has_untyped_exit(trans.builder.func, &known)
        };
        let direct_self_calls = trans.direct_self_calls;
        let frameless_bail = trans.frameless_bail;
        let cursor_bailed = trans.cursor_bail
            || (trans.cursors_on && (trans.cursor_advances == 0 || trans.cursor_carries == 0));

        // `STEEL_CURSOR_STATS=1` reports why a function kept or lost its cursors.
        if trans.cursors_on && crate::config::config().cursor_stats {
            eprintln!(
                "[cursor-stats] {} advances={} carries={} copy_reads={} bail={}",
                trans.name,
                trans.cursor_advances,
                trans.cursor_carries,
                trans.cursor_copy_reads,
                cursor_bailed
            );
        }
        trans.builder.finalize(frontend_config);

        let returned = exit_types.clone();

        // exit_types only sees the POPPURE returns; a deopt returns void.
        if deopts {
            exit_types.insert(InferredType::Void);
        }

        // Callers trust a single recorded return type (a call whose callee only
        // ever returned `Bool` is branched on without a tag check), so every
        // other way out has to count. A tail call, the memq shortcut or a
        // speculative exit returns an untyped value, e.g. the `(g x)` in
        // `(if (p x) #t (g x))`.
        if untyped_exit {
            exit_types.insert(InferredType::Any);
        }

        // The specialized copy shares the generic copy's id; callers only ever
        // reach the generic copy, so its exits are the ones that describe a call.
        if !matches!(spec_mode, SpecMode::Specialized { .. }) {
            self.function_return_types.insert(id, exit_types);
        }

        if frameless_bail {
            return Err("frameless: body needs a frame".to_string());
        }

        Ok(TranslateInfo {
            returned,
            untyped_non_deopt_exit,
            direct_self_calls,
            cursor_bailed,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum InferredType {
    // A bare i64 payload, untagged. `as_steelval` rebuilds the fixnum tag at
    // whatever boundary the value escapes through.
    Int64,

    // A boxed value carrying the fixnum tag (`SteelVal::IntV`), never a bignum.
    // The arithmetic and comparison fast arms unbox an `Int` operand without
    // checking its tag, and `known_tags` folds its tag checks away. So only
    // produce it for values that are fixnums by construction (currently fixnum
    // constants). An operation that can overflow into a bignum may only produce
    // `Int` if its overflow path deopts.
    Int,
    // Is just straight up, unboxed, meaning
    // its represented by a u8 on the stack on not
    // a 128.
    UnboxedBool,

    // when we know its a floating point
    Float,

    // Generic number, could be anything
    Number,

    // Boxed boolean
    Bool,

    // TODO: We'll want unboxed variants of all of these as well.
    // That way, we can keep the value around as untagged for use
    // within the function itself.
    List,
    Any,

    // Boxed value. Eventually we can introduce Box<T> types,
    // but those will need to get stored within the type checker
    // context
    Box,
    Void,
    Pair,

    // Narrowed version, list or pair, for operations that can
    // work on both
    ListOrPair,

    Function,

    BytecodeFunction,

    Char,

    String,

    Symbol,

    // TODO: See if we can lift this out, as it inflates the size
    Struct(StructTypeDescriptor),

    MutableVector,
}

/// The type of what a call to the primitive `f` returns, from the return type
/// its definition declares (see `crate::value_type`).
fn primitive_result_type(f: FunctionSignature) -> InferredType {
    let config = crate::config::config();
    if !config.jit_primitive_types {
        return InferredType::Any;
    }
    let metadata = crate::steel_vm::builtin::get_function_name(f);
    let ty = metadata.as_ref().map_or(InferredType::Any, |m| {
        InferredType::from_value_type(m.return_type)
    });
    if config.jit_primitive_types_log {
        let name = metadata
            .as_ref()
            .map_or("<unregistered>", |m| m.name.as_ref());
        eprintln!("[prim-type] {name} -> {ty:?}");
    }
    ty
}

impl InferredType {
    /// The tag every boxed value of this type carries, for types that pin it
    /// down to one. Tag checks on a value with one of these fold away, so the
    /// type has to be exact wherever it is produced. `Int` is only a fixnum, and
    /// `List` only a proper list.
    pub(super) fn proven_tag(&self) -> Option<u8> {
        Some(match self {
            InferredType::Int | InferredType::Int64 => SteelVal::INT_TAG,
            InferredType::Float => SteelVal::FLOAT_TAG,
            InferredType::Bool | InferredType::UnboxedBool => SteelVal::BOOL_TAG,
            InferredType::Char => SteelVal::CHAR_TAG,
            InferredType::String => SteelVal::STRING_TAG,
            InferredType::Symbol => SteelVal::SYMBOL_TAG,
            InferredType::List => SteelVal::LIST_TAG,
            InferredType::Pair => SteelVal::PAIR_TAG,
            InferredType::Void => SteelVal::VOID_TAG,
            InferredType::MutableVector => SteelVal::HEAP_REF_VECTOR_TAG,
            _ => return None,
        })
    }

    /// A value of `ty`, held the ordinary way (boxed, tagged).
    pub(crate) fn from_value_type(ty: crate::value_type::ValueType) -> Self {
        use crate::value_type::ValueType;
        match ty {
            ValueType::Any => InferredType::Any,
            ValueType::Bool => InferredType::Bool,
            ValueType::Int => InferredType::Int,
            ValueType::Float => InferredType::Float,
            ValueType::Number => InferredType::Number,
            ValueType::Char => InferredType::Char,
            ValueType::String => InferredType::String,
            ValueType::Symbol => InferredType::Symbol,
            ValueType::List => InferredType::List,
            ValueType::Pair => InferredType::Pair,
            ValueType::MutableVector => InferredType::MutableVector,
            ValueType::Void => InferredType::Void,
            // No jit type for these yet.
            ValueType::Vector
            | ValueType::HashMap
            | ValueType::HashSet
            | ValueType::ByteVector
            | ValueType::FlatVector
            | ValueType::Procedure
            | ValueType::Custom(_) => InferredType::Any,
        }
    }

    /// The `SteelVal` tag a value of this type is guaranteed to carry, for the
    /// types whose producers only ever make that variant. `Number`, `Any` and
    /// the list-or-pair family say nothing about the tag.
    ///
    /// `Bool`, `Char` and `Void` are left out. Helpers are typed `Bool` on paths
    /// that can still return an error marker, and `Void` doubles as "moved out".
    fn exact_tag(self) -> Option<u8> {
        match self {
            Self::Int => Some(SteelVal::INT_TAG),
            Self::Float => Some(SteelVal::FLOAT_TAG),
            Self::Symbol => Some(SteelVal::SYMBOL_TAG),
            Self::List => Some(SteelVal::LIST_TAG),
            Self::MutableVector => Some(SteelVal::HEAP_REF_VECTOR_TAG),
            Self::Struct(_) => Some(SteelVal::STRUCT_TAG),
            _ => None,
        }
    }

    /// Carries no reference, so overwriting or popping it needs no drop.
    fn is_immediate(self) -> bool {
        matches!(
            self,
            Self::Int | Self::Float | Self::Bool | Self::Char | Self::Void
        )
    }

    /// The least type both describe, or `Any`. Numeric kinds meet at `Number`;
    /// everything else only survives when it is the same on both sides.
    fn join(self, other: Self) -> Self {
        let (a, b) = (self.boxed(), other.boxed());
        if a == b {
            return a;
        }
        let numeric = |t: Self| matches!(t, Self::Int | Self::Float | Self::Number);
        if numeric(a) && numeric(b) {
            return Self::Number;
        }
        Self::Any
    }

    /// What this value's type becomes once it has been materialized as a
    /// `SteelVal`.
    ///
    /// The untagged types describe how a value is being carried, so they must
    /// not survive materialization. A slot recorded as `Int64` after the tagged
    /// value was written to it would be tagged a second time on the next read.
    fn boxed(self) -> Self {
        match self {
            // Use `Number` here. The dispatch guards match on `Int` and then
            // assume the operand is a value or a constant, not a register.
            InferredType::Int64 => InferredType::Number,
            InferredType::UnboxedBool => InferredType::Bool,
            other => other,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct StackValue {
    // Unfortunately this could be both a i128 or (i8, i64)
    value: Value,
    inferred_type: InferredType,
    // Whether or not the value is spilled
    // to the stack
    spilled: bool,
}

impl StackValue {
    /// The value as a full `SteelVal`, tagging it if it is being carried
    /// untagged.
    ///
    /// This is the only place an untagged payload becomes a `SteelVal`, so
    /// anything that reads `self.value` directly has to be sure the value is not
    /// one of the untagged types below.
    pub fn as_steelval(&self, ctx: &mut FunctionTranslator) -> Value {
        match self.inferred_type {
            InferredType::UnboxedBool => {
                let value = ctx.builder.ins().uextend(types::I64, self.value);
                ctx.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, value)
            }
            // A bare i64 payload known to fit. The tag is implied by the type, so
            // it is rebuilt here instead of occupying a register.
            InferredType::Int64 => {
                let boxed = ctx.encode_value(SteelVal::INT_TAG as i64, self.value);
                ctx.known_tags.insert(boxed, SteelVal::INT_TAG);
                boxed
            }
            _ => {
                // The type proves the tag, so every tag check on this value from
                // here on folds to a constant (`get_tag`), and a guard built
                // from them to one arm (`converging_if`).
                if let Some(tag) = self
                    .inferred_type
                    .proven_tag()
                    .filter(|_| crate::config::config().jit_proven_tags)
                {
                    ctx.known_tags.entry(self.value).or_insert(tag);
                }
                self.value
            }
        }

        // Encode it using the discriminant associated with the inferred
        // type, if its unboxed.
        //
        // Otherwise, its just the value itself.
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ConstantValue {
    Int(isize),
    Bool(bool),
    Char(char),

    Float(f64),

    List(usize),

    Symbol(usize),

    // HeapConstant
    Index(usize),

    // A closed global, read from its slot when it is used. It is never
    // reassigned, so reading it late gives the same value as reading it when
    // pushed, and a consumer that only looks at it can skip the clone.
    Global(usize),
}

struct StackFrameRepr {
    sp: Value,
    ip: Value,
    instructions: Value,
    function: Value,
    attachments: Value,
}

impl ConstantValue {
    fn as_typ(self) -> InferredType {
        match self {
            ConstantValue::Int(_) => InferredType::Int,
            ConstantValue::Bool(_) => InferredType::Bool,
            ConstantValue::Char(_) => InferredType::Char,
            ConstantValue::Float(_) => InferredType::Number,
            ConstantValue::Index(_) | ConstantValue::Global(_) => InferredType::Any,
            ConstantValue::Symbol(_) => InferredType::Symbol,
            ConstantValue::List(_) => InferredType::List,
        }
    }

    fn to_value(self, ctx: &mut FunctionTranslator) -> (Value, InferredType) {
        match self {
            // TODO: We can probably infer the type here though, since we know
            // what the type is based on the values coming in
            ConstantValue::Index(p) => (ctx.push_const_index(p), InferredType::Any),

            ConstantValue::Global(index) => {
                let value = ctx.inline_lookup_global(index);
                ctx.clone_value(value);
                (value, InferredType::Any)
            }

            ConstantValue::Int(i) => (ctx.encode_integer(i as _), InferredType::Int),

            ConstantValue::Float(n) => (ctx.encode_float(n), InferredType::Float),

            ConstantValue::Char(c) => (ctx.encode_char(c), InferredType::Char),

            ConstantValue::Symbol(i) => {
                let constant = ctx.constants.get_value(i);
                ctx.constant_to_value(i, constant)
            }

            ConstantValue::List(i) => {
                let constant = ctx.constants.get_value(i);
                ctx.constant_to_value(i, constant)
            }

            ConstantValue::Bool(b) if b => (ctx.encode_true(), InferredType::Bool),

            ConstantValue::Bool(b) if !b => (ctx.encode_false(), InferredType::Bool),

            _ => {
                // (value, self.as_typ())

                dbg!(self);
                panic!()
            }
        }
    }
}

// TODO: Include another variant for this, which can decide whether the `Value`
// itself is actually a value represented by a SteelVal, or if its something
// that is represented by an unboxed version of that value.
//
// For example, if we return a `bool` from a function, we should be able to leave
// it as a u8, unless we pass it directly to a function. At that point, we can
// then convert it to a proper steel value. However, at this point, we have an
// encoding of values that more or less assumes that this is a `SteelVal` when
// its in the `StackValue` state.
//
// We'll also want constants to be encoded in the value as well, like we have
// below.
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
enum MaybeStackValue {
    // If we're on the stack, this could be any size.
    Value(StackValue),

    // These should already be spilled by default.
    //
    // Mutable registers will get &mut SteelVal via the
    // index
    MutRegister(usize),

    // Registers will get &SteelVal via the index
    Register(usize),

    Constant(ConstantValue),

    // A value read without cloning out of something that is still alive, like a
    // list in a local or a box in a struct in a local. Only operations that
    // neither keep nor free their operand, like `eq?`, can use it as is; before
    // any other operation it is materialized into a `Value`. See
    // `materialize_borrowed`.
    Borrowed(BorrowedValue),
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct BorrowedValue {
    value: Value,
    // An i8 variable, 1 when a slow path cloned the value so it is owned, and 0
    // when it is only borrowed.
    owned: Variable,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum ValueOrRegister {
    Value(Value),
    Register(usize),
}

/// A list being walked as a `(cell, index)` pair in registers instead of as a
/// `SteelVal` in its stack slot.
///
/// `cdr` is then arithmetic: decrement the index, or follow the link when it
/// runs out. Materialising a real list would copy the cell, because `im-lists`
/// keeps the cursor inside the cell, so two cursors need two cells.
///
/// The slot keeps holding the head of the list the whole time, which keeps
/// every cell the cursor reaches alive. Nothing here owns a reference.
/// `cell` is the `RcBox` pointer, the same as a `ListV` payload, so the
/// `list_*_offset()` helpers apply to it directly. The links between cells and
/// the `im-lists` raw-pointer API both use the data pointer instead
/// (`BiasedRc::into_raw` / `from_raw`), `list_cell_base()` bytes further on.
/// `cursor_advance` and `emit_cursor_write_back` convert between the two.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct ListCursor {
    cell: Variable,
    index: Variable,
}

#[derive(Default, Clone)]
struct CachedLookupMap {
    registers: HashMap<usize, Value>,

    /// Registers currently represented by a cursor instead of by their slot.
    /// Merged the same way as `registers`, so a cursor survives a branch only if
    /// both arms agree on it.
    cursors: HashMap<usize, ListCursor>,

    // Every time we pop off the stack, we'll increase this.
    stack_length_capacity: usize,

    // This is a lifted reference to the actual stack pointer.
    // In the event there is a reallocation, we'll have to
    // invalidate this. So we can hold this in place until
    // a call to a slow grow stack.
    stack_buf_pointer: Option<Value>,
}

#[derive(Default, Clone)]
struct PropertyMap {
    // So we're going to do something like this.
    props: HashMap<ValueOrRegister, Vec<Properties>>,

    cached_lookups: CachedLookupMap,

    // The builder is past a return or a loop jump. Nothing emitted from here
    // until the next join runs, so this arm contributes nothing to it.
    dead: bool,
}

impl PropertyMap {
    /// Keep only what both branches agree on; anything else is unknown here.
    ///
    /// Returns whether the two arms disagreed about a list cursor. A cursor is
    /// the authoritative copy of its value and the slot behind it is stale, so
    /// one that survives in only one arm can't be dropped here like a cached
    /// read can.
    pub fn meet(&mut self, other: &PropertyMap) -> bool {
        // An arm that ended in a return or a loop jump never reaches the join.
        if other.dead {
            return false;
        }
        if self.dead {
            *self = other.clone();
            return false;
        }

        let cursor_conflict = self.cached_lookups.cursors != other.cached_lookups.cursors;
        // `STEEL_CURSOR_TRACE=1` reports which join lost a cursor, and from where.
        if cursor_conflict && crate::config::config().cursor_trace {
            let mut a: Vec<_> = self
                .cached_lookups
                .cursors
                .iter()
                .map(|(k, v)| (*k, v.cell.as_u32(), v.index.as_u32()))
                .collect();
            let mut b: Vec<_> = other
                .cached_lookups
                .cursors
                .iter()
                .map(|(k, v)| (*k, v.cell.as_u32(), v.index.as_u32()))
                .collect();
            a.sort();
            b.sort();
            let bt = std::backtrace::Backtrace::force_capture().to_string();
            let frames: Vec<&str> = bt.lines().filter(|l| l.contains("cgen")).take(6).collect();
            eprintln!(
                "[cursor] meet conflict self={a:?} other={b:?}\n{}",
                frames.join("\n")
            );
        }

        self.props.retain(|key, props| match other.props.get(key) {
            Some(other_props) => {
                props.retain(|p| other_props.contains(p));
                !props.is_empty()
            }
            None => false,
        });

        self.cached_lookups
            .registers
            .retain(|key, value| other.cached_lookups.registers.get(key) == Some(value));

        self.cached_lookups
            .cursors
            .retain(|key, value| other.cached_lookups.cursors.get(key) == Some(value));

        self.cached_lookups.stack_length_capacity = self
            .cached_lookups
            .stack_length_capacity
            .min(other.cached_lookups.stack_length_capacity);

        if self.cached_lookups.stack_buf_pointer != other.cached_lookups.stack_buf_pointer {
            self.cached_lookups.stack_buf_pointer = None;
        }

        cursor_conflict
    }

    pub fn remove(&mut self, value: &ValueOrRegister) {
        self.props.remove(value);
    }

    pub fn get(&self, value: &ValueOrRegister) -> Option<Properties> {
        self.props.get(value).and_then(|x| {
            if x.len() == 1 {
                x.first().copied()
            } else {
                None
            }
        })
    }

    pub fn set_property(&mut self, value: ValueOrRegister, prop: Properties) {
        if let Some(exists) = self.props.get_mut(&value) {
            exists.clear();
            exists.push(prop);
        } else {
            self.props.insert(value, vec![prop]);
        }
    }

    pub fn add_property(&mut self, value: ValueOrRegister, prop: Properties) {
        if let Some(exists) = self.props.get_mut(&value) {
            match prop {
                Properties::NonEmptyListOrPair => {
                    for p in exists.iter() {
                        if let Properties::NonEmptyListOrPair = p {
                            break;
                        }
                    }

                    for p in exists.iter() {
                        if let Properties::ProperNonEmptyList = p {
                            break;
                        }
                    }

                    for p in exists.iter_mut() {
                        if let Properties::ProperList = p {
                            *p = Properties::ProperNonEmptyList;
                            break;
                        }
                    }

                    for p in exists.iter_mut() {
                        if let Properties::NonNull = p {
                            *p = Properties::NonEmptyListOrPair;
                            break;
                        }
                    }
                }
                Properties::ProperList => {
                    for p in exists.iter() {
                        if let Properties::ProperList = p {
                            break;
                        }
                    }

                    for p in exists.iter() {
                        if let Properties::ProperNonEmptyList = p {
                            break;
                        }
                    }

                    for p in exists.iter_mut() {
                        if let Properties::NonEmptyListOrPair = p {
                            *p = Properties::ProperNonEmptyList;
                        }
                    }
                }
                // The false arm of a `null?` test. Pushed as a second entry it
                // would leave two properties, and `get` returns nothing for more
                // than one, so a non-null proper list would never become
                // `ProperNonEmptyList`.
                Properties::NonNull => {
                    let already = exists.iter().any(|p| {
                        matches!(
                            p,
                            Properties::NonNull
                                | Properties::NonEmptyListOrPair
                                | Properties::ProperNonEmptyList
                        )
                    });
                    if !already {
                        match exists
                            .iter_mut()
                            .find(|p| matches!(p, Properties::ProperList))
                        {
                            Some(p) => *p = Properties::ProperNonEmptyList,
                            None => exists.push(prop),
                        }
                    }
                }
                _ => {
                    // Coalesce properties on push. In the event there are properties
                    // that are related, we should infer things about them here.
                    exists.push(prop);
                }
            }
        } else {
            self.props.insert(value, vec![prop]);
        }
    }

    // This is how we can infer a property?
    pub fn infer_property_bool(&mut self, condition_value: Value, branch: bool) {
        if let Some(props) = self.props.get(&ValueOrRegister::Value(condition_value)) {
            for prop in props {
                match prop {
                    Properties::CheckedList(value_or_register) => {
                        if branch {
                            self.add_property(*value_or_register, Properties::ProperList)
                        } else {
                            // Mark as not a list? Is that even worth checking?
                            self.add_property(*value_or_register, Properties::NotAList)
                        }
                        break;
                    }
                    Properties::CheckedPair(value_or_register) => {
                        if branch {
                            self.add_property(*value_or_register, Properties::NonEmptyListOrPair);
                        }
                        break;
                    }
                    Properties::CheckedNull(value_or_register) => {
                        if branch {
                            self.add_property(*value_or_register, Properties::Null);
                        } else {
                            // TODO: This isn't quite right. Just because null returned
                            // #f does not mean its a non empty list. If its _anything_
                            // but null, then it won't return true. So what we probably do is
                            // just assert the exact opposite, which is that its _definitely_ not
                            // a null list.
                            self.add_property(*value_or_register, Properties::NonNull);
                        }
                        break;
                    }
                    Properties::CheckedNotNull(value_or_register) => {
                        if branch {
                            self.add_property(*value_or_register, Properties::NonNull);
                        } else {
                            self.add_property(*value_or_register, Properties::Null);
                        }
                        break;
                    }
                    Properties::ConditionLessThan(value_or_register, integer) => {
                        if branch {
                            self.add_property(*value_or_register, Properties::LessThan(*integer));
                        } else {
                            self.add_property(
                                *value_or_register,
                                Properties::GreaterThan(*integer),
                            );
                        }
                        break;
                    }
                    _ => {}
                }
            }
        }
    }
}

// TODO: Figure out a good way to align inferred type but also
// additional properties.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Properties {
    // If we can call car on this list successfully, downstream of this
    // then we're both going to be listed as a proper list type,
    // and also this will successfully return without error.
    NonEmptyListOrPair,

    NonNull,

    NotAList,

    Null,

    // Assuming coming in to this we didn't know the type,
    // after running an identity via something like `list?`,
    // then we can tag the value for use later on. This is helpful
    // when reaching a phi node where the type might not be known
    // across branches. If the type is found to be true, then during
    // the true branch, we can assert that this is the type,
    // and skip a bunch of extraneous type checks. So in this case,
    // this is a conditional check, attached to a boolean or unboxed
    // boolean, stating that its possible that this value may be
    // true or false.
    CheckedList(ValueOrRegister),
    // `pair?`. Where it holds, `car` / `cdr` of the register succeed.
    CheckedPair(ValueOrRegister),
    CheckedNull(ValueOrRegister),
    // `(not (null? x))`, the same test with its arms swapped.
    CheckedNotNull(ValueOrRegister),

    ProperList,

    ProperNonEmptyList,

    InferredType(InferredType),

    // Encode the property that this thing
    // is a certain range.
    ConditionLessThan(ValueOrRegister, i64),

    PositiveInteger,

    // Realized encoding of the above
    GreaterThan(i64),
    LessThan(i64),
}

impl MaybeStackValue {
    fn into_index(self) -> usize {
        match self {
            MaybeStackValue::MutRegister(p) => p,
            MaybeStackValue::Register(p) => p,
            _ => panic!(),
        }
    }

    fn into_value(self, ctx: &mut FunctionTranslator) -> StackValue {
        match self {
            Self::Borrowed(b) => StackValue {
                value: ctx.materialize(b),
                inferred_type: InferredType::Any,
                spilled: false,
            },
            Self::Value(_) | Self::Constant(_) => self.as_value(ctx).unwrap(),
            Self::MutRegister(p) => {
                let (value, inferred_type) = ctx.mut_register_to_value(p);
                StackValue {
                    value,
                    inferred_type,
                    spilled: false,
                }
            }
            Self::Register(p) => {
                let (value, inferred_type) = ctx.immutable_register_to_value(p);
                StackValue {
                    value,
                    inferred_type,
                    spilled: false,
                }
            }
        }
    }

    fn into_constant_int(self, _ctx: &mut FunctionTranslator) -> Option<isize> {
        match self {
            Self::Constant(ConstantValue::Int(i)) => Some(i),
            _ => None,
        }
    }

    fn as_value(self, ctx: &mut FunctionTranslator) -> Option<StackValue> {
        match self {
            Self::Value(v) => Some(v),
            Self::Constant(c) => {
                let (v, ty) = c.to_value(ctx);
                Some(StackValue {
                    value: v,
                    inferred_type: ty,
                    spilled: false,
                })
            }
            _ => None,
        }
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    name: String,
    int: types::Type,
    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,

    // We're gonna use a cursor to understand the before and after implications
    // of the instructions. For now, we'll compile sequences of hot instructions
    // together into a dynamic sequence.
    instructions: &'a [DenseInstruction],
    ip: usize,
    // Instructions some branch jumps to.
    join_targets: HashSet<usize>,
    _globals: &'a [SteelVal],

    // Local values - whenever things are locally read, we can start using them
    // here. We should also keep track of which values are actually just registers,
    // and when calling we can lazily pull them in if it doesn't
    // fit the calling convention of the function. But, in the event we're calling a
    // function with one or two args, this seems like a good tradeoff to make.
    // stack: Vec<StackValue>,
    shadow_stack: Vec<MaybeStackValue>,
    // cloned_stack: bool,

    // Local value mapping, can allow
    // us to elide type checks if we have them
    value_to_local_map: HashMap<Value, usize>,

    local_to_value_map: HashMap<usize, InferredType>,

    // This should probably be something more sophisticated, but for now it'll work.
    properties: PropertyMap,

    arity: u16,
    constants: &'a ConstantMap,

    tco: bool,

    let_var_stack: Vec<usize>,

    intrinsics: &'a OwnedFunctionMap,

    fake_entry_block: Option<Block>,
    exit_block: Block,
    deopt_return_block: Option<Block>,
    /// Set when this translation is one side of a frameless call.
    frameless: Option<FramelessInfo>,
    /// A frameless copy met something it can't compile without a frame, so the
    /// whole translation is abandoned.
    frameless_bail: bool,
    /// Self calls emitted as frameless calls, so a call arm can tell whether
    /// it took the frameless path.
    frameless_calls: usize,
    /// The block a `check_deopt` last continued into. Another check in that
    /// block, with no call or store emitted since, would load the same
    /// `is_native` and can be skipped.
    checked_native_block: Option<Block>,

    /// Queued cold exits, filled once the body is done. Emitting them inline
    /// puts their loads ahead of the fast path they branch away from, and those
    /// values then do not dominate it, which cranelift rejects.
    ///
    /// Each carries the list cursors that were live where it was queued. Those
    /// have to be written back to their slots on the way out.
    pending_deopt_exits: Vec<(
        Block,
        Vec<MaybeStackValue>,
        usize,
        HashMap<usize, ListCursor>,
    )>,

    /// List cursors are allowed in this translation. Only ever true in a
    /// specialized copy with a loop, where `List`-seeded parameters are known to
    /// be lists on entry.
    cursors_on: bool,

    /// Something happened while a cursor was live that the translator cannot
    /// keep sound, usually branch arms disagreeing about cursors. The caller
    /// throws this translation away and redoes it without cursors.
    cursor_bail: bool,
    // How many `cdr`s walked a cursor. None means the cursors did not help and
    // only cost a write-back at each escape and a re-seed per iteration.
    cursor_advances: usize,
    // How many cursors crossed a back edge still unmaterialised. A walk that is
    // materialised before every call or loop jump anyway costs a write-back for
    // each in-place `cdr` it replaced.
    cursor_carries: usize,
    // `(cdr l)` where `l` is read again, which a derived cursor (a second
    // cursor for the result) would cover. Reported by `STEEL_CURSOR_STATS`.
    cursor_copy_reads: usize,

    /// The cursor for each `List`-seeded parameter, seeded before the loop
    /// header. Every back edge has to arrive with these variables current.
    header_cursors: HashMap<usize, ListCursor>,

    /// Parameter slots a self tail call is carrying as a cursor. The argument
    /// is the slot's own value, so the write is skipped and the slot keeps the
    /// head that keeps the cursor's cells alive.
    carry_skip: HashSet<usize>,
    visited: HashSet<usize>,

    depth: usize,

    if_bound: Option<usize>,

    if_stack: Vec<usize>,

    // Stack of merge blocks for enclosing if/else constructs. The innermost
    // IF's merge block is on top. A fork that exits at `if_bound` uses this
    // to register its tail as an extra predecessor of the right merge block.
    if_merge_blocks: Vec<Block>,
    // Parallel to `if_merge_blocks`: the owned flag for the value each merge
    // receives. Every jump to a merge block defines it.
    if_merge_flags: Vec<Variable>,
    // Set by a struct getter that left its result borrowed, for the caller that
    // pushes the result.
    pending_borrow: Option<Variable>,

    vm_context: Value,
    // vm_context: StackSlot,
    // generators: LazyInstructionGenerators,
    slot: Option<&'a Gc<ByteCodeLambda>>,
    function_context: Option<usize>,

    /// Global indices that can never hold another value: never `set!` in their
    /// own compile, and not reachable from a later one (`is_closed_global`).
    /// Only such a global may have its value baked into generated code.
    closed_globals: &'a HashSet<usize>,

    names: &'a HashMap<u32, String>,

    function_return_types: &'a HashMap<u32, HashSet<InferredType>>,
    exit_types: &'a mut HashSet<InferredType>,
    // The `return` instructions whose value's type went into `exit_types`.
    // Any other reachable exit makes the function's result type unknown.
    typed_returns: HashSet<cranelift::codegen::ir::Inst>,

    thread_pointer: Option<Value>,

    should_trampoline: Option<Value>,

    sp: Option<Value>,

    pop_count: Option<Value>,

    pop_count_plus_one: Option<Value>,
    pop_count_minus_one: Option<Value>,

    thread_id: Option<Value>,

    use_lbbv: bool,
    // The tag an SSA value is known to carry. An SSA value never changes, so the
    // fact holds at every use. Tag checks on these fold to a constant and
    // `converging_if` only emits the arm the constant selects.
    known_tags: HashMap<Value, u8>,
    // Loop specialization role of this translation; see `SpecMode`.
    spec_mode: SpecMode,
    // Returns emitted by deopt exits. They hand the call to the interpreter, so
    // no caller uses their value without checking first.
    deopt_returns: HashSet<cranelift::codegen::ir::Inst>,
    direct_self_calls: usize,
}

pub fn split_big(a: i128) -> [i64; 2] {
    [(a >> 64) as i64, a as i64]
}

pub fn encode_big(tag: u8, payload: i64) -> i128 {
    let tag = tag as i128;
    ((payload as i128) << 64) | tag as i128
}

// We should be able to read local values off the stack.
// Functions _will_ be called via reading from the stack, so the locals will be there,
// but any other values pushed to the stack do not necessarily have to get pushed on
// to the stack.

/// The index `variadic-numeric-spilled` knows each arithmetic and comparison
/// opcode by. The compiler emits these opcodes for any positive arity, but
/// there are fixed-arity helpers for only a few.
fn variadic_numeric_code(op: OpCode) -> Option<usize> {
    Some(match op {
        OpCode::ADD => 0,
        OpCode::SUB => 1,
        OpCode::MUL => 2,
        OpCode::DIV => 3,
        OpCode::LT => 4,
        OpCode::LTE => 5,
        OpCode::GT => 6,
        OpCode::GTE => 7,
        _ => return None,
    })
}

/// The helper the generic lowering of an opcode calls, by how many operands it
/// takes and whether it can raise.
#[derive(Clone, Copy)]
enum OperandHelper {
    One(Helper<1, true, true, true>),
    Two(Helper<2, true, true, true>),
    Three(Helper<3, true, true, true>),
    Four(Helper<4, true, true, true>),
    OneSafe(Helper<1, true, true, false>),
    TwoSafe(Helper<2, true, true, false>),
}

impl OperandHelper {
    fn name(&self) -> &'static str {
        match self {
            OperandHelper::One(h) => h.name,
            OperandHelper::Two(h) => h.name,
            OperandHelper::Three(h) => h.name,
            OperandHelper::Four(h) => h.name,
            OperandHelper::OneSafe(h) => h.name,
            OperandHelper::TwoSafe(h) => h.name,
        }
    }
}

/// The helper for `op` applied to `payload` operands, if there is one at that
/// arity.
fn operand_helper(op: OpCode, payload: usize) -> Option<OperandHelper> {
    use OperandHelper::*;
    Some(match (op, payload) {
        (OpCode::ADD, 2) => Two(ADD_BINOP),
        (OpCode::ADD, 3) => Three(ADD_THREE),
        (OpCode::ADD, 4) => Four(ADD_FOUR),
        (OpCode::SUB, 1) => One(SUB_NEGATE),
        (OpCode::SUB, 2) => Two(SUB_BINOP),
        (OpCode::SUB, 3) => Three(SUB_THREE),
        (OpCode::MUL, 2) => Two(MULT_TWO),
        (OpCode::MUL, 3) => Three(MULT_THREE),
        (OpCode::DIV, 1) => One(DIV_ONE),
        (OpCode::DIV, 2) => Two(DIV_TWO),
        (OpCode::LT, 2) => Two(LT_BINOP),
        (OpCode::LT, 3) => Three(LT_THREE),
        (OpCode::LTE, 2) => Two(LTE_BINOP),
        (OpCode::LTE, 3) => Three(LTE_THREE),
        (OpCode::GT, 2) => Two(GT_BINOP),
        (OpCode::GT, 3) => Three(GT_THREE),
        (OpCode::GTE, 2) => Two(GTE_BINOP),
        (OpCode::GTE, 3) => Three(GTE_THREE),
        (OpCode::NUMEQUAL, 2) => Two(NUM_EQUAL_VALUE_BOOL),
        (OpCode::EQUAL | OpCode::EQUAL2, 2) => TwoSafe(EQUAL_BINOP_BOOL),
        (OpCode::NOT, 1) => OneSafe(NOT_VALUE),
        (OpCode::CAR, 1) => One(CAR_HANDLER_VALUE),
        (OpCode::CDR, 1) => One(CDR_HANDLER_VALUE),
        (OpCode::CONS, 2) => Two(CONS_HANDLER_VALUE),
        (OpCode::NEWBOX, 1) => OneSafe(BOX_HANDLER),
        (OpCode::UNBOX, 1) => One(UNBOX_HANDLER),
        (OpCode::SETBOX, 2) => Two(SET_BOX_HANDLER),
        (OpCode::LISTREF, 2) => Two(LIST_REF_VALUE),
        (OpCode::VECTORREF, 2) => Two(VECTOR_REF_VALUE),
        _ => return None,
    })
}

/// The helper that reads a local for `op`.
fn read_helper(op: OpCode) -> Option<Helper<0, true, true, false>> {
    Some(match op {
        OpCode::READLOCAL0 => READ_LOCAL_0,
        OpCode::READLOCAL1 => READ_LOCAL_1,
        OpCode::READLOCAL2 => READ_LOCAL_2,
        OpCode::READLOCAL3 => READ_LOCAL_3,
        OpCode::MOVEREADLOCAL0 => MOVE_READ_LOCAL_0,
        OpCode::MOVEREADLOCAL1 => MOVE_READ_LOCAL_1,
        OpCode::MOVEREADLOCAL2 => MOVE_READ_LOCAL_2,
        OpCode::MOVEREADLOCAL3 => MOVE_READ_LOCAL_3,
        _ => return None,
    })
}

impl FunctionTranslator<'_> {
    /// For a call about to take the top `arity` values, the local each one was
    /// read from, if it is a plain read of one. Taken before `split_off`, which
    /// loses this. A `MutRegister` is excluded, since it hands the callee the
    /// local's value to move out of, so afterwards the slot may not hold it.
    fn argument_origins(&self, arity: usize) -> Vec<Option<usize>> {
        let len = self.shadow_stack.len();
        self.shadow_stack[len.saturating_sub(arity)..]
            .iter()
            .map(|value| match value {
                MaybeStackValue::Register(slot) => Some(*slot),
                MaybeStackValue::Value(v) => self.value_to_local_map.get(&v.value).copied(),
                _ => None,
            })
            .collect()
    }

    /// After a call to a native function has returned normally (past its
    /// `check_deopt`), its `args` declaration says each argument had its
    /// declared type (see `crate::value_type::ArgTypes`), so an untyped local
    /// passed to it gets that type from here on. Only fills in a local typed
    /// `Any`, and leaves a more precise known type alone.
    fn narrow_locals_after_call(
        &mut self,
        metadata: Option<crate::steel_vm::builtin::FunctionSignatureMetadata>,
        origins: &[Option<usize>],
    ) {
        let Some(metadata) = metadata else { return };
        self.narrow_locals_after_call_to(&metadata.name, metadata.arg_types, origins);
    }

    /// [`Self::narrow_locals_after_call`], for a function named `name` whose
    /// arguments are declared `arg_types`.
    fn narrow_locals_after_call_to(
        &mut self,
        name: &str,
        arg_types: Option<crate::value_type::ArgTypes>,
        origins: &[Option<usize>],
    ) {
        let config = crate::config::config();
        if !config.jit_primitive_arg_types {
            return;
        }
        let Some(arg_types) = arg_types else {
            return;
        };
        let n = origins.len();
        if n < arg_types.fixed.len() || (arg_types.rest.is_none() && n > arg_types.fixed.len()) {
            return;
        }
        for (i, origin) in origins.iter().enumerate() {
            // A union has no single inferred type to narrow to.
            let (Some(slot), Some(declared)) =
                (origin, arg_types.get(i).and_then(|ty| ty.single()))
            else {
                continue;
            };
            let ty = InferredType::from_value_type(declared);
            if matches!(ty, InferredType::Any) {
                continue;
            }
            if matches!(
                self.local_to_value_map.get(slot),
                None | Some(InferredType::Any)
            ) {
                if config.jit_primitive_types_log {
                    eprintln!("[prim-arg] {name} arg {i}: local {slot} -> {ty:?}");
                }
                self.local_to_value_map.insert(*slot, ty);
            }
        }
    }

    fn mark_local_type_from_var(&mut self, last: StackValue, typ: InferredType) {
        if let Some(from_local) = self.value_to_local_map.get(&last.value) {
            self.local_to_value_map.insert(*from_local, typ);
        }
    }

    fn shadow_mark_local_type_from_var(&mut self, last: MaybeStackValue, typ: InferredType) {
        if let MaybeStackValue::Value(value) = last {
            self.mark_local_type_from_var(value, typ);
        }

        if let MaybeStackValue::Register(p) = last {
            self.local_to_value_map.insert(p, typ);
        }

        if let MaybeStackValue::MutRegister(p) = last {
            self.local_to_value_map.insert(p, typ);
        }
    }

    fn maybe_check_last(&self) {
        let last = self.shadow_stack.last().unwrap();

        if let MaybeStackValue::Value(s) = last {
            assert!(!s.spilled);
        }
    }

    fn inferred_type(&self, value: &MaybeStackValue) -> Option<InferredType> {
        match value {
            MaybeStackValue::Borrowed(_) => None,
            MaybeStackValue::Value(stack_value) => Some(stack_value.inferred_type.clone()),
            MaybeStackValue::MutRegister(i) => self.local_to_value_map.get(i).cloned(),
            MaybeStackValue::Register(i) => self.local_to_value_map.get(i).cloned(),
            MaybeStackValue::Constant(constant_value) => Some(constant_value.as_typ()),
        }
    }

    // Read values off of the stack - push them on to
    // wherever they need to go.
    // Assuming the whole instruction set is translated and we also confirm
    // that _only_
    // native functions get used, we can
    // probably just rewrite the function
    // into a function pointer, and we don't need to thread the
    // context through at all.
    fn stack_to_ssa(&mut self) -> bool {
        self.depth += 1;
        while self.ip < self.instructions.len() {
            if let Some(last) = self.if_bound {
                if self.ip == last {
                    self.depth -= 1;
                    return false;
                }
            }

            if let Some(last) = self.if_stack.last().copied() {
                if self.ip <= last {
                    dbg!(self.ip);
                    dbg!(last);
                    pretty_print_dense_instructions(&self.instructions);
                }

                assert!(self.ip > last);
            }

            let instr = self.instructions[self.ip];
            let op = instr.op_code;
            let payload = instr.payload_size.to_usize();

            if !self.visited.insert(self.ip) {
                panic!("Already visited this instruction",);
            }

            if !self.op_keeps_borrows(op, payload) {
                self.materialize_borrowed();
            }

            // Only ever set by the self tail call that is carrying cursors, and
            // it must not reach any other op's writes.
            self.carry_skip.clear();

            // A call can re-enter the vm, and a call that deopts resumes this
            // frame in the interpreter, which reads the slots. Put every cursor
            // back first. Plain `FuncV` primitives are Rust functions that cannot
            // do either. If they raise, that goes through `check_deopt`, which
            // writes the cursors back itself.
            if self.cursors_on
                && !self.properties.cached_lookups.cursors.is_empty()
                && self.op_may_resume_interpreted(op, payload)
            {
                self.materialize_all_cursors();
            }

            match op {
                OpCode::LOADINT1POP | OpCode::BINOPADDTAIL => {
                    todo!("{:?}", op);
                }

                OpCode::SCLOSURE => {
                    panic!("Deprecated opcode");
                }
                OpCode::POPPURE => {
                    self.lower_pop_pure();
                    return false;
                }
                OpCode::VOID => {
                    let value = self.encode_void();
                    self.push(value, InferredType::Void);
                    self.ip += 1;
                }
                OpCode::PUSH
                    if self.instructions.get(self.ip + 1).map(|x| x.op_code)
                        == Some(OpCode::UNBOX)
                        && !self.func_is_join_target(self.ip + 1)
                        && self.global_is_closed(payload) =>
                {
                    self.lower_closed_global_unbox(payload)
                }

                OpCode::PUSH
                    if closed_global_operands_enabled() && self.global_is_closed(payload) =>
                {
                    self.shadow_push(MaybeStackValue::Constant(ConstantValue::Global(payload)));
                    self.ip += 1;
                }

                OpCode::PUSH => {
                    let result = self.inline_lookup_global(payload);

                    // Clone the resulting value, then move on
                    self.clone_value(result);

                    // TODO: If we know the type of a global
                    // and we also know that the value is immutable, then
                    // we should be able to fuss with it
                    self.push(result, InferredType::Any);

                    self.ip += 1;
                }

                // TODO: Still adjust the ip as needed
                OpCode::IF => self.lower_if(),
                OpCode::JMP | OpCode::POPJMP => {
                    assert!(payload > self.ip);
                    self.ip = payload;
                }
                // Call func... lets see how this goes...
                OpCode::FUNC | OpCode::FUNCNOARITY => self.lower_func(payload),

                // TODO: Revisit this!
                OpCode::TAILCALL | OpCode::TAILCALLNOARITY => {
                    self.lower_tail_call(payload);
                    return false;
                }
                OpCode::NEWSCLOSURE => self.lower_new_closure(payload),

                // Something is up here - we don't want to do this!
                OpCode::PUREFUNC => self.lower_pure_func(payload),

                OpCode::ECLOSURE => panic!("Should not hit a ECLOSURE during jit pass"),
                OpCode::BIND => panic!("Should not hit a BIND during jit pass"),
                OpCode::SDEF => panic!("Should not hit a SDEF during jit pass"),
                OpCode::EDEF => panic!("Should not hit a EDEF during jit pass"),
                OpCode::POPN => {
                    for _ in 0..payload {
                        self.pop_single();
                    }
                    self.ip += 1;
                }
                OpCode::POPSINGLE => {
                    self.pop_single();
                    self.ip += 1;
                }
                OpCode::PASS => panic!("Should not hit a pass during jit pass"),
                OpCode::NDEFS => panic!("Should not get hit during jit pass"),
                OpCode::PANIC => unsupported_opcode(op),
                OpCode::SET => {
                    let value = self.shadow_pop();
                    self.value_to_local_map.remove(&value.0);
                    let result = self.call_set(payload, value.0);
                    self.push(result, InferredType::Any);
                    self.ip += 1;
                }
                // TODO:
                // Move read local does not require spilling since
                // it will only be used one. Read local does since we want
                // to clone it
                OpCode::READLOCAL | OpCode::MOVEREADLOCAL => {
                    let value = self.spilled_read_local_value(op, payload);
                    self.ip += 1;
                    self.shadow_push(value);
                }
                OpCode::PUSHCONST => self.lower_push_const(),
                OpCode::TRUE => {
                    let value = self.encode_true();
                    self.ip += 1;
                    self.push(value, InferredType::Bool);
                }
                OpCode::FALSE => {
                    let value = self.encode_false();
                    self.ip += 1;
                    self.push(value, InferredType::Any);
                }

                // Handle inferred type with constants as well?
                OpCode::LOADINT0 => {
                    self.ip += 1;
                    self.shadow_push(MaybeStackValue::Constant(ConstantValue::Int(0)));
                }
                OpCode::LOADINT1 => {
                    self.ip += 1;
                    self.shadow_push(MaybeStackValue::Constant(ConstantValue::Int(1)));
                }
                OpCode::LOADINT2 => {
                    self.ip += 1;
                    self.shadow_push(MaybeStackValue::Constant(ConstantValue::Int(2)));
                }

                OpCode::LetVar => self.lower_let_var(),
                OpCode::READLOCAL0
                | OpCode::READLOCAL1
                | OpCode::READLOCAL2
                | OpCode::READLOCAL3
                | OpCode::MOVEREADLOCAL0
                | OpCode::MOVEREADLOCAL1
                | OpCode::MOVEREADLOCAL2
                | OpCode::MOVEREADLOCAL3 => {
                    let value = self.spilled_read_local_fixed(op, payload);
                    self.ip += 1;
                    self.shadow_push(value);
                }
                // Set local is totally fair game and should be adjusted here:
                OpCode::SETLOCAL => self.lower_set_local(payload),

                OpCode::COPYCAPTURESTACK => panic!("Should be unreachable - copycapturestack"),
                OpCode::COPYCAPTURECLOSURE => panic!("Should be unreachable - copycaptureclosure"),
                OpCode::COPYHEAPCAPTURECLOSURE => {
                    panic!("Should be unreachable - copyheapcaptureclosure")
                }
                OpCode::FIRSTCOPYHEAPCAPTURECLOSURE => {
                    panic!("Should be unreachable - firstcopyheapcaptureclosure")
                }

                // TODO: If the function contains a tco jump
                // or a self tail call no arity, store a reference
                // to the entry block. We're going to loop there
                // instead.
                OpCode::TCOJMP => {
                    // TODO: Make this act like the self tail call no arity!
                    // Figure out why this isn't working quite right!
                    self.translate_tco_jmp(payload);

                    self.finish_tail_position();

                    self.check_deopt();

                    return false;
                }
                // TODO:
                //
                // Once we hit the self tail call no arity, we can unroll the call one time (basically)
                // by then calling into a specialized version of the function, where the types
                // are known. So, we fork from here; re compile the whole input function, and then
                // hit a direct call into that. We mark that this thing is specialized, so it won't
                // continuously do that; next iterations will do that. We'll also need to mark which
                // jump back points are known so that we can keep those across boundaries.
                OpCode::SELFTAILCALLNOARITY => {
                    let _ = self.translate_tco_jmp_no_arity_loop_no_spill(payload);
                    //

                    // Jump to out of bounds so signal we're done
                    self.finish_tail_position();

                    return false;
                }
                // TODO: This is ripe for inlining, assuming the value
                // is 1. immutable and 2. A primitive.
                //
                // Okay so what we need to do here, is actually inline a return_call_indirect
                // in order to actually make the proper tail call, in the case that this is
                // a closure. So, the easiest way to do that, would be to:
                //
                // 1. First check that this is a closure,
                // 2. Check that it has a super instruction
                // 3. If it is, then we do the tail call business where we spill
                //    everything to the VM stack, and then reuse the stack frame.
                // 4. Make the indirect tail call, and then we're done with the
                //    control flow.
                OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALTAILNOARITY
                | OpCode::CALLPRIMITIVETAIL => {
                    self.lower_global_tail_call(payload);
                    return false;
                }
                OpCode::CALLGLOBALNOARITY if self.func_is_join_target(self.ip + 1) => {
                    self.push_global_callee(payload);
                }

                OpCode::CALLGLOBALNOARITY => self.lower_call_global_no_arity(payload),

                OpCode::CALLPRIMITIVE if self.func_is_join_target(self.ip + 1) => {
                    self.push_global_callee(payload);
                }

                OpCode::CALLPRIMITIVE => self.lower_call_primitive(payload),

                OpCode::CALLGLOBAL => {
                    if self.func_is_join_target(self.ip + 1) {
                        self.push_global_callee(payload);
                    } else {
                        self.call_global_impl(payload);
                    }
                }

                // Pattern is
                // READCAPTURED
                // UNBOX
                //
                // we can
                OpCode::READCAPTURED
                    if self.instructions.get(self.ip + 1).map(|x| x.op_code)
                        == Some(OpCode::UNBOX) =>
                {
                    self.lower_read_captured_unbox(payload)
                }

                OpCode::READCAPTURED => {
                    let res = self.inline_read_captured(payload, true);

                    // TODO: Keep track of the inferred type on the capture as well
                    self.push(res, InferredType::Any);

                    self.ip += 1;
                }

                // Begin scope means we're starting
                // a let scope, which means we'll have some amount
                // of values that are retained on the stack for
                // further usage.
                OpCode::BEGINSCOPE => {
                    self.let_var_stack.push(0);

                    // TODO: Same idea here; lets figure out a way
                    // to coalesce any reads / writes that are happening here on the
                    // stack into one. Same for pushing multiple values to the stack.

                    self.spill_stack();

                    self.ip += 1;
                }
                OpCode::LETENDSCOPE => self.lower_let_end_scope(payload),

                // Both operands are fixnums by construction, so there is no tag to
                // check. On overflow it exits to the interpreter, which produces the
                // bignum, so the result can stay `Int`.
                OpCode::ADD
                | OpCode::SUB
                | OpCode::MUL
                | OpCode::LT
                | OpCode::LTE
                | OpCode::GT
                | OpCode::GTE
                | OpCode::NUMEQUAL
                    if payload == 2 && self.top_two_are_fixnums() =>
                {
                    self.fixnum_binop(op);
                }

                // Both operands are statically floats (a Float seeded register, a
                // float constant or a float typed result), so the result is a float
                // too. The arms below would type it `Number`, which doesn't match a
                // Float seed. A known float on one side only isn't enough, since a
                // complex number minus a float is complex.
                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV
                    if payload == 2 && float_inline_enabled() && self.top_two_are_floats() =>
                {
                    let res = self.inline_float_binop_two(op);
                    self.push(res, InferredType::Float);
                    self.ip += 2;
                }

                // When we have two registers, we can add them in place.
                // we should use type inference if we have it
                OpCode::SUB
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                            ])
                        ) =>
                {
                    self.sub_register_two();
                }

                // TODO: Generalize this to any immediate!
                OpCode::SUB
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Constant(ConstantValue::Int(_))
                            ])
                        ) =>
                {
                    self.sub_register_constant();
                }

                // TODO: Depending on the inferred type, we can save a lot of
                // operations here.
                //
                // TODO: Add floats
                OpCode::SUB
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Value(StackValue {
                                    inferred_type: InferredType::Int,
                                    ..
                                }) | MaybeStackValue::Constant(ConstantValue::Int(_))
                            ])
                        ) =>
                {
                    self.sub_register_int_constant();
                }

                OpCode::SUB
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Value(StackValue {
                                    inferred_type: InferredType::Float,
                                    ..
                                })
                            ])
                        ) =>
                {
                    self.sub_register_float();
                }

                // TODO: Handle floats as well, just like the above.
                OpCode::SUB
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Value(StackValue { .. })
                            ])
                        ) =>
                {
                    self.sub_register_value()
                }

                OpCode::SUB if payload == 2 && self.top_operand_is_int() => {
                    self.sub_int_operand(payload)
                }

                OpCode::ADD
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Constant(ConstantValue::Int(_))
                            ])
                        ) =>
                {
                    self.add_register_int()
                }

                // Specializing addition such that we'll handle when the first argument
                // is a register.
                //
                // We should probably also handle if the value is an immediate; Can it be
                // encoded unboxed?
                OpCode::ADD
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Value(_)
                            ])
                        ) =>
                {
                    self.add_register_value()
                }

                OpCode::ADD
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                            ])
                        ) =>
                {
                    let register_r = self.shadow_stack_pop().unwrap().into_index();
                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    let (res, t) = self.binop_add_value_register(register_l, register_r);

                    self.push(res, t);

                    self.ip += 2;
                }

                OpCode::ADD
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[MaybeStackValue::Value(_), MaybeStackValue::Value(_),])
                        ) =>
                {
                    let args = self.split_off(2);
                    let (res, t) = self.binop_add_value_both(args[0].0, args[1].0);

                    self.push(res, t);

                    self.ip += 2;
                }

                OpCode::ADD
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[MaybeStackValue::Value(_), MaybeStackValue::Value(_)])
                        ) =>
                {
                    self.add_two_values()
                }

                OpCode::ADD | OpCode::SUB | OpCode::MUL
                    if matches!(payload, 2 | 3)
                        && generic_inline_enabled()
                        && float_inline_enabled() =>
                {
                    let res = match operand_helper(op, payload) {
                        Some(OperandHelper::Two(fallback)) => {
                            self.inline_number_op::<2>(op, fallback)
                        }
                        Some(OperandHelper::Three(fallback)) => {
                            self.inline_number_op::<3>(op, fallback)
                        }
                        _ => unreachable!("{op:?} has a helper for {payload} operands"),
                    };
                    self.push(res, InferredType::Number);
                    self.ip += 2;
                }

                // TODO: Specialize this a bit more. If we know that the RHS is some kind
                // of constant, we can probably encode that a little bit more effectively
                // in the generated code.
                // Integer arithmetic is inlined above. For floats, check both tags
                // and emit the op directly instead of calling a helper that builds
                // a slice and a `Result` for one machine instruction.
                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV
                    if payload == 2 && generic_inline_enabled() && float_inline_enabled() =>
                {
                    let res = self.inline_float_binop_two(op);
                    self.push(res, InferredType::Number);
                    self.ip += 2;
                }

                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    // Call the func
                    self.func_ret_val(op, payload, 2, InferredType::Number);
                    self.check_deopt();
                }

                OpCode::LT | OpCode::LTE | OpCode::GT | OpCode::GTE
                    if self.int_against_register(payload) =>
                {
                    let (cc, fallback) = match op {
                        OpCode::LT => (IntCC::SignedLessThan, LT_REGISTER_INT),
                        OpCode::LTE => (IntCC::SignedLessThanOrEqual, LTE_REGISTER_INT),
                        OpCode::GT => (IntCC::SignedGreaterThan, GT_REGISTER_INT),
                        _ => (IntCC::SignedGreaterThanOrEqual, GTE_REGISTER_INT),
                    };
                    self.compare_register_int(cc, fallback);
                }

                OpCode::LT | OpCode::LTE if payload == 2 && self.top_operand_is_int() => {
                    self.mark_operands(payload, InferredType::Number);
                    // TODO: This isn't quite right. This doesn not check the input
                    // type properly
                    let helper = if op == OpCode::LT {
                        LT_BINOP_INT
                    } else {
                        LTE_BINOP_INT
                    };
                    self.call_helper_on_operands(helper, InferredType::Bool);
                }

                // When the value is a mutable register, or register, and we're comparing
                // it to a known constant / etc

                // Inferred type is not int

                // TODO: Introduce an abstraction to insert blocks when _both_ types are the same.
                // Reduce number of type checks, inline comparisons.
                OpCode::GTE if payload == 2 && self.second_operand_is_register() => {
                    self.gte_register_any(payload)
                }

                OpCode::LTE
                    if payload == 2
                        && matches!(self.shadow_stack.last(), Some(MaybeStackValue::Value(_)))
                        && self.second_operand_is_register() =>
                {
                    self.lte_register_value(payload)
                }

                OpCode::NUMEQUAL
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[MaybeStackValue::Value(_), MaybeStackValue::Value(_),])
                        ) =>
                {
                    self.num_equal_two_values()
                }

                // In the event they're register @ value, we can probably just flip it
                OpCode::NUMEQUAL
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Value(_),
                            ])
                        )
                        && self.top_operand_is_int() =>
                {
                    self.num_equal_register_int()
                }

                // In the event they're register @ value, we can probably just flip it
                OpCode::NUMEQUAL
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::Value(_),
                                MaybeStackValue::Constant(ConstantValue::Int(_)),
                            ])
                        )
                        && self.top_operand_is_int() =>
                {
                    self.num_equal_value_int_constant()
                }

                // TODO: @Matt
                //
                // This is where we have to be better; use the registers, use the constants,
                // and inline the numeric ops since these are likely to be extremely common.
                // The arms above need one side to be a compile-time constant or a
                // register, which generic numeric code rarely has. Check both tags
                // instead. When they're both ints the comparison is a single `icmp`,
                // and everything else still reaches the same helper.
                OpCode::NUMEQUAL if payload == 2 && generic_inline_enabled() => {
                    let res = self.inline_int_compare_two(IntCC::Equal, NUM_EQUAL_VALUE_BOOL);
                    self.push(res, InferredType::UnboxedBool);
                    self.ip += 2;
                }

                OpCode::NUMEQUAL if payload == 2 && self.top_operand_is_int() => {
                    self.call_helper_on_operands(NUM_EQUAL_INT, InferredType::Bool);
                }

                // Same as NUMEQUAL above. The specialised arms all want a register
                // or a constant, so this handles two ordinary values inline.
                OpCode::LTE | OpCode::GTE | OpCode::LT | OpCode::GT
                    if payload == 2 && generic_inline_enabled() =>
                {
                    self.compare_two_values(op, payload)
                }

                OpCode::LTE | OpCode::GTE | OpCode::LT | OpCode::GT => {
                    if payload == 2 {
                        self.mark_operands(payload, InferredType::Number);
                    }

                    self.func_ret_val(op, payload, 2, InferredType::Bool);
                }

                // Inlining equality... figure out a way to make this faster
                // without needing to make a call. So for this, we'll just do
                // pointer equality fast paths?
                OpCode::EQUAL2 => self.lower_equal2(op, payload),

                // Lets inline equal2
                OpCode::EQUAL | OpCode::NUMEQUAL => {
                    self.func_ret_val(op, payload, 2, InferredType::UnboxedBool);
                }

                // TODO: Use the register here. Checking is null might be slightly more involved?
                // Or rather, lets just use is-empty?
                OpCode::NULL
                    if matches!(
                        self.shadow_stack.last(),
                        Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                    ) =>
                {
                    self.null_register()
                }

                OpCode::NULL => {
                    let [value] = self.pop_operands::<1>();
                    let result = self.call_helper(NULL_HANDLER, [value]);
                    self.check_deopt();
                    self.push(result, InferredType::Bool);
                    self.ip += 2;
                }

                // Cons handler value - lets optimize this
                OpCode::CONS => self.lower_cons(op),

                // Cdr reg no type check, should be faster
                OpCode::CDR => self.lower_cdr(op),
                OpCode::LIST => self.lower_list(payload),

                OpCode::VEC => self.lower_vec(payload),

                // Specialize car for when its on a register, to avoid doing
                // the read local operations.
                OpCode::CAR => self.lower_car(op),

                OpCode::NEWBOX => {
                    self.func_ret_val(op, 1, 2, InferredType::Box);
                }
                OpCode::SETBOX => self.lower_set_box(op),
                OpCode::UNBOX => self.lower_unbox(op),
                OpCode::ADDREGISTER => unsupported_opcode(op),
                OpCode::SUBREGISTER => unsupported_opcode(op),
                OpCode::LTEREGISTER => unsupported_opcode(op),
                OpCode::SUBREGISTER1 => unsupported_opcode(op),
                OpCode::ALLOC => unsupported_opcode(op),
                OpCode::READALLOC => unsupported_opcode(op),
                OpCode::SETALLOC => unsupported_opcode(op),
                OpCode::DynSuperInstruction => unsupported_opcode(op),
                OpCode::Arity => unsupported_opcode(op),
                OpCode::ADDIMMEDIATE => unsupported_opcode(op),
                OpCode::SUBIMMEDIATE => unsupported_opcode(op),
                OpCode::LTEIMMEDIATE => unsupported_opcode(op),
                OpCode::BINOPADD => unsupported_opcode(op),
                OpCode::BINOPSUB => unsupported_opcode(op),
                OpCode::LTEIMMEDIATEIF => unsupported_opcode(op),

                // TODO: This should pretty much be able to be inlined entirely?
                OpCode::NOT => self.lower_not(),
                OpCode::Apply => unsupported_opcode(op),
                OpCode::LOADINT0POP => unsupported_opcode(op),
                OpCode::LOADINT2POP => unsupported_opcode(op),
                OpCode::CGLOCALCONST => unsupported_opcode(op),
                OpCode::READLOCAL0CALLGLOBAL => unsupported_opcode(op),
                OpCode::READLOCAL1CALLGLOBAL => unsupported_opcode(op),
                OpCode::LISTREF => {
                    self.func_ret_val(op, 2, 2, InferredType::Any);
                }

                // Add an op code for vector ref as well.
                //
                // Probably can do a check based on the previous calls
                // to skip checks against the vector since
                // the type is inferred.
                OpCode::VECTORREF => self.lower_vector_ref(op),
                OpCode::NULLIF => unsupported_opcode(op),
                OpCode::UNBOXCALL => unsupported_opcode(op),
                OpCode::UNBOXTAIL => unsupported_opcode(op),
                OpCode::EQUALCONST => unsupported_opcode(op),
            }
        }

        self.depth -= 1;

        return true;
    }

    /// Compare a local's register (under) with an int operand (on top), as
    /// guarded by `int_against_register`. When the local holds an int too, the
    /// two compare inline with `cc`, against an immediate when the operand is a
    /// constant. Otherwise `fallback` compares them. The result is an unboxed
    /// bool. A `<` against a constant is recorded for later narrowing.
    fn compare_register_int(&mut self, cc: IntCC, fallback: Helper<2, true, true, true>) {
        #[derive(Clone, Copy)]
        enum Operand {
            Value(Value),
            Int(i64),
        }

        self.mark_operands(2, InferredType::Number);

        // Materialized, since an untagged payload read raw would be used as
        // though it were a SteelVal. `shadow_stack_pop` so a spilled operand is
        // taken off the vm stack as well, instead of left there to shift
        // everything above it (see the `cons` arm).
        let rhs = match self.shadow_stack_pop().unwrap() {
            MaybeStackValue::Value(v) => Operand::Value(v.as_steelval(self)),
            MaybeStackValue::Constant(ConstantValue::Int(i)) => Operand::Int(i as _),
            // The guard admits any stack entry whose inferred type is `Int`,
            // and a let slot bound to an int is a register with that type, so
            // registers have to be handled here.
            MaybeStackValue::Register(r) => Operand::Value(self.immutable_register_to_value(r).0),
            MaybeStackValue::MutRegister(r) => Operand::Value(self.mut_register_to_value(r).0),
            _ => panic!(),
        };

        let register_l = self.shadow_stack_pop().unwrap().into_index();

        let local_value = self.read_from_vm_stack(register_l);
        let is_int = self.is_type(local_value, SteelVal::INT_TAG);

        let result = self.converging_if(
            is_int,
            |ctx| {
                let lhs = ctx.unbox_value_to_pointer(local_value);
                match rhs {
                    Operand::Value(value) => {
                        let rhs = ctx.unbox_value_to_pointer(value);
                        ctx.builder.ins().icmp(cc, lhs, rhs)
                    }
                    Operand::Int(i) => ctx.builder.ins().icmp_imm_s(cc, lhs, i),
                }
            },
            |ctx| {
                let register_l = ctx.i64_const(register_l as i64);
                let rhs_value = match rhs {
                    Operand::Value(value) => value,
                    Operand::Int(i) => ctx.encode_integer(i),
                };
                ctx.call_helper_checked(fallback, [register_l, rhs_value])
            },
            types::I8,
        );

        if let (IntCC::SignedLessThan, Operand::Int(i)) = (cc, rhs) {
            self.properties.add_property(
                ValueOrRegister::Value(result),
                Properties::ConditionLessThan(ValueOrRegister::Register(register_l), i),
            );
        }

        self.push(result, InferredType::UnboxedBool);
        self.ip += 2;
    }

    fn sub_register_float(&mut self) {
        let value = self.shadow_stack_pop().unwrap().into_value(self);
        let register_index = self.shadow_stack_pop().unwrap().into_index();

        // Do the shift here, in an effort to avoid passing more stuff?
        let value = value.as_steelval(self);

        let local_value = self.read_from_vm_stack(register_index);
        let is_float = self.is_type(local_value, SteelVal::FLOAT_TAG);

        let sp = |ctx: &mut Self| {
            let register = ctx.i64_const(register_index as i64);
            let args = [register, value];
            let result = ctx.call_helper_checked(SUB_BINOP_FLOAT_REG, args);

            // The helper reports a type error by flagging the vm. Without this
            // check the jitted code would keep going and the error would surface
            // after `with-handler` had been unwound.

            result
        };

        let result = self.converging_if(
            is_float,
            |ctx| {
                // If its an int, then we'll do checked subtraction:
                let lhs = ctx.unbox_value_to_float(local_value);
                let rhs = ctx.unbox_value_to_float(value);
                let subbed = ctx.builder.ins().fsub(lhs, rhs);
                ctx.encode_float_value(subbed)
            },
            sp,
            types::I128,
        );

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Float);

        self.ip += 2;
    }

    fn sub_register_int_constant(&mut self) {
        let value = self.shadow_stack_pop().unwrap().into_value(self);
        let register_index = self.shadow_stack_pop().unwrap().into_index();

        // Do the shift here, in an effort to avoid passing more stuff?
        let value = value.as_steelval(self);

        let local_value = self.read_from_vm_stack(register_index);
        let is_int = self.is_type(local_value, SteelVal::INT_TAG);

        let sp = |ctx: &mut Self| {
            let register = ctx.i64_const(register_index as i64);
            let args = [register, value];
            let result = ctx.call_helper_checked(SUB_BINOP_INT_REG, args);

            // The helper reports a type error by flagging the vm. Without this
            // check the jitted code would keep going and the error would surface
            // after `with-handler` had been unwound.

            result
        };

        let result = self.converging_if(
            is_int,
            |ctx| {
                // If its an int, then we'll do checked subtraction:
                let lhs = ctx.unbox_value_to_pointer(local_value);
                let rhs = ctx.unbox_value_to_pointer(value);

                let (subbed, overflow_flag) = ctx.builder.ins().ssub_overflow(lhs, rhs);

                ctx.converging_if(
                    overflow_flag,
                    sp,
                    |ctx| ctx.encode_value(SteelVal::INT_TAG as _, subbed),
                    types::I128,
                )
            },
            sp,
            types::I128,
        );

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.ip += 2;
    }

    fn sub_register_constant(&mut self) {
        // Snapshot before the operands come off. On a deopt the interpreter
        // re-runs this instruction, so it needs them back on its operand stack.
        // The shadow stack is translator bookkeeping, so restoring the vector is
        // enough to make `spill_stack_for_branch` write the right things.
        let deopt_ip = self.ip;
        let pre_pop_stack = self.shadow_stack.clone();

        let constant_value = self
            .shadow_stack_pop()
            .unwrap()
            .into_constant_int(self)
            .unwrap();
        let register_index = self.shadow_stack_pop().unwrap().into_index();

        let (tag, local_value) = self.read_from_vm_stack_split(register_index);

        let is_int = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, tag, SteelVal::INT_TAG as i64);

        let sp = |ctx: &mut Self| {
            let register = ctx.i64_const(register_index as i64);
            // The constant this call subtracts, which may not be 1. Reached
            // whenever the register is not an int or the fast path overflows,
            // e.g. `(- 100.0 5)`.
            let value = ctx.encode_integer(constant_value as _);
            let args = [register, value];
            let result = ctx.call_helper_checked(SUB_BINOP_INT_REG, args);

            // The helper reports a type error by flagging the vm. Without this
            // check the jitted code would keep going and the error would surface
            // after `with-handler` had been unwound.

            result
        };

        // Speculative form. There is no merge, so the fast arm can stay untagged.
        if should_speculate_for(self.function_context) {
            // Uniform shadow stack before a two way branch, like every other
            // branch here. Without it a value defined on one side survives in
            // translator state past a later join and is used there without having
            // been passed through it.
            //
            // Everything is already written back, so the exit below only has to
            // set `ip` and return.
            self.spill_stack_for_branch();

            let fast_block = self.builder.create_block();
            let deopt_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(is_int, fast_block, &[], deopt_block, &[]);

            self.builder.seal_block(deopt_block);
            self.defer_deopt_exit(deopt_block, deopt_ip, pre_pop_stack.clone());

            self.builder.switch_to_block(fast_block);
            self.builder.seal_block(fast_block);

            // The tag is established, so this is a plain checked subtraction on
            // the payload. Overflow still has to go somewhere the interpreter can
            // finish, so it takes the same exit.
            let rhs = self.i64_const(constant_value as i64);
            let (subbed, overflow_flag) = self.builder.ins().ssub_overflow(local_value, rhs);

            let ok_block = self.builder.create_block();
            let overflow_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(overflow_flag, overflow_block, &[], ok_block, &[]);

            self.builder.seal_block(overflow_block);
            self.defer_deopt_exit(overflow_block, deopt_ip, pre_pop_stack);

            self.builder.switch_to_block(ok_block);
            self.builder.seal_block(ok_block);

            // Untagged. The tag is implied by `Int64`, and `as_steelval` puts it
            // back at whatever boundary this value escapes through (a spill, a
            // call, a return, or the deopt exit above). Until then it uses one
            // register instead of a pair.
            self.push(subbed, InferredType::Int64);
            self.ip += 2;

            return;
        }

        let result = self.converging_if(
            is_int,
            |ctx| {
                if let Some(Properties::GreaterThan(v)) = ctx
                    .properties
                    .get(&ValueOrRegister::Register(register_index))
                {
                    if v >= 0 {
                        let lhs = local_value;

                        // Negate it and do an immediate add
                        let subbed = ctx.builder.ins().iadd_imm_s(lhs, -(constant_value as i64));
                        ctx.encode_value(SteelVal::INT_TAG as _, subbed)
                    } else {
                        let lhs = local_value;

                        let rhs = ctx.i64_const(constant_value as i64);

                        let (subbed, overflow_flag) = ctx.builder.ins().ssub_overflow(lhs, rhs);

                        ctx.converging_if(
                            overflow_flag,
                            sp,
                            |ctx| ctx.encode_value(SteelVal::INT_TAG as _, subbed),
                            types::I128,
                        )
                    }
                } else {
                    let lhs = local_value;
                    let rhs = ctx.i64_const(constant_value as i64);

                    let (subbed, overflow_flag) = ctx.builder.ins().ssub_overflow(lhs, rhs);

                    ctx.converging_if(
                        overflow_flag,
                        sp,
                        |ctx| ctx.encode_value(SteelVal::INT_TAG as _, subbed),
                        types::I128,
                    )
                }

                // // If its an int, then we'll do checked subtraction:
            },
            sp,
            types::I128,
        );

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.ip += 2;
    }

    fn sub_register_two(&mut self) {
        let register_r = self.shadow_stack_pop().unwrap().into_index();
        let register_l = self.shadow_stack_pop().unwrap().into_index();

        // Read both from the vm stack - we could read them together,
        // but for now this will do
        let local_value_r = self.read_from_vm_stack(register_r);
        let local_value_l = self.read_from_vm_stack(register_l);

        let both_int = self.tags_are(&[
            (local_value_r, SteelVal::INT_TAG),
            (local_value_l, SteelVal::INT_TAG),
        ]);

        // TODO: Adjust all of these things - in the event we're subtracting two
        // integer values, then we need to do
        let sp = |ctx: &mut Self| {
            let register_l = ctx.i64_const(register_l as i64);
            let register_r = ctx.i64_const(register_r as i64);
            let args = [register_l, register_r];
            let result = ctx.call_helper_checked(SUB_BINOP_BOTH_REG, args);

            result
        };

        let result = self.converging_if(
            both_int,
            |ctx| {
                let lhs = ctx.unbox_value_to_pointer(local_value_l);
                let rhs = ctx.unbox_value_to_pointer(local_value_r);

                let (subbed, overflow_flag) = ctx.builder.ins().ssub_overflow(lhs, rhs);

                ctx.converging_if(
                    overflow_flag,
                    sp,
                    |ctx| ctx.encode_value(SteelVal::INT_TAG as _, subbed),
                    types::I128,
                )
            },
            sp,
            types::I128,
        );

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.ip += 2;
    }

    // TODO: We have to include the arity check as well!
    // Otherwise, we're going to have a problem.
    fn inline_call_func(
        &mut self,
        arity: usize,
        name: &str,
        func: Value,
        should_clone: bool,
        should_pop_func: bool,
    ) {
        let is_closure = self.is_type(func, SteelVal::CLOSURE_TAG);
        // If it is a closure, we need to clone the value:

        // Okay, now that we've gotten that out of the way, we can
        // continue doing our thing:
        let typ = self.int;

        let old_stack = self.shadow_stack.clone();
        let old_map = self.value_to_local_map.clone();

        let res = self.converging_if(
            is_closure,
            |ctx| {
                let vm_ctx = ctx.get_ctx();
                let closure = ctx.unbox_value_to_pointer(func);

                if should_pop_func {
                    // Missing shadow stack pop!
                    ctx.shadow_stack_pop().unwrap();
                }

                // TODO: Clone / restore this for this branch,
                // so the other branch can inherit the changes
                let args_off_the_stack = ctx
                    .split_off(arity)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                // Track the inferred type of register arguments,
                // to the point that we can elide all sorts of drops
                ctx.spill_stack();

                let arity = args_off_the_stack.len();
                // Always take the check here too. See the note in the non-tail
                // call path above.
                ctx.push_to_many_vm_stack_let_var_new(&args_off_the_stack);

                // The checked push reserves only what it needs, so there
                // is no headroom left over.
                ctx.properties.cached_lookups.stack_length_capacity = 0;

                let should_trampoline = ctx.check_should_trampoline(vm_ctx);

                // TODO: This is not going to work here. Instead, we need to load
                // the id of the instruction.
                let super_instruction = ctx.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted().with_readonly(),
                    closure,
                    // Offset for the RC payload
                    closure_field_offset(offset_of!(ByteCodeLambda, super_instructions)),
                );

                let super_instruction_exists =
                    ctx.builder
                        .ins()
                        .icmp_imm_s(IntCC::NotEqual, super_instruction, 0);

                let fixed_arity = ctx.closure_accepts_arity(closure, arity);
                let compiled_and_fixed = ctx
                    .builder
                    .ins()
                    .band(super_instruction_exists, fixed_arity);

                let should_continue = ctx
                    .builder
                    .ins()
                    .band(should_trampoline, compiled_and_fixed);

                let should_yield = ctx.builder.ins().bxor_imm_u(should_continue, 1);
                let fallback_ip = ctx.ip - 1;
                ctx.update_ip_native_if_yield(vm_ctx, should_yield, fallback_ip + 1);

                let res = ctx.converging_if(
                    should_continue,
                    |ctx| {
                        if should_clone {
                            // Increment the ref count of the closure
                            ctx.increment_ref_count_closure(closure);
                        }

                        let body_exp_offset =
                            closure_field_offset(offset_of!(ByteCodeLambda, body_exp));

                        let rcbox_ptr = ctx.builder.ins().load(
                            types::I64,
                            MemFlagsData::trusted().with_readonly(),
                            closure,
                            body_exp_offset,
                        );

                        let len = ctx.builder.ins().load(
                            types::I64,
                            MemFlagsData::trusted().with_readonly(),
                            closure,
                            body_exp_offset + 8,
                        );

                        let data_ptr = ctx
                            .builder
                            .ins()
                            .iadd_imm_s(rcbox_ptr, rcbox_slice_data_offset());

                        let instr_fat_ptr = ctx.builder.ins().iconcat(data_ptr, len);

                        ctx.push_stack_frame(
                            arity as _,
                            closure,
                            instr_fat_ptr,
                            fallback_ip,
                            false,
                        );

                        // TODO: Abstract this to a function:
                        // Attempt to look up a value indirectly:
                        let sig_ref = ctx.create_jit_sig_ref();

                        // TODO: This is gonna be a problem now.
                        // Assuming we're not storing both on there.
                        //
                        // TODO: Add another field on functions for
                        // the non trampoline function, or generically
                        // call the trampoline function (i.e. have one
                        // trampoline that we can pass by value to the
                        // tail calling convention code.)
                        // Same reasoning as `get_local_callee`: the callee can
                        // grow the value stack.
                        ctx.invalidate_buf_ptr();

                        let call =
                            ctx.builder
                                .ins()
                                .call_indirect(sig_ref, super_instruction, &[vm_ctx]);

                        let res = ctx.builder.inst_results(call)[0];

                        let is_still_native =
                            ctx.load_field(types::I8, vm_ctx, offset_of!(VmCore, is_native));

                        ctx.converging_if(is_still_native, |_| res, |ctx| ctx.encode_void(), typ)
                    },
                    |ctx| {
                        let arity = ctx.i64_const(arity as i64);
                        let fallback_ip = ctx.i64_const(fallback_ip as i64);
                        // TODO: Set up the closure for multi arity?
                        // Part of the call protocol. An arity error is picked up after the merge.
                        let res = ctx.call_helper_unchecked(
                            SETUP_CLOSURE_ARITY,
                            [closure, arity, fallback_ip],
                        );
                        // The helper clones the closure into the frame it pushes,
                        // so an owned callee is still ours to release.
                        if !should_clone {
                            ctx.drop_tagged_value(func);
                        }
                        res
                    },
                    typ,
                );

                res
            },
            |ctx| {
                ctx.shadow_stack = old_stack.clone();
                ctx.value_to_local_map = old_map.clone();

                if should_pop_func {
                    ctx.call_function(arity, name, false)
                } else {
                    ctx.call_function_with_func(arity, name, false, func)
                }
            },
            typ,
        );

        self.push(res, InferredType::Any)
    }

    /// Emit a tail call to a primitive inline, when we recognise it.
    ///
    /// The callee is compared by function pointer, since the global's value is
    /// already known at the call site and comparing the `FuncV` is cheaper than
    /// resolving a symbol.
    ///
    /// Only primitives with an existing inline lowering are handled; anything
    /// else returns None and takes the generic path. `#%unbox` becomes an
    /// inline sequence. `#%set-box!` still calls a native helper, but directly
    /// instead of through the deopt trampoline, which skips the global lookup,
    /// the `SteelVal` kind match and the argument marshalling.
    fn inline_primitive_tail_call(
        &mut self,
        func: Option<&SteelVal>,
        arity: usize,
    ) -> Option<Value> {
        if !inline_primitive_tail_calls_enabled() {
            return None;
        }

        let target = match func? {
            SteelVal::FuncV(f) => *f as usize,
            SteelVal::MutFunc(f) => *f as usize,
            SteelVal::BuiltIn(b) => *b as usize,
            _ => return None,
        };

        // Primitives with a lowering of their own; see `inline_primitives.rs`.
        self.try_inline_primitive_tail(target, arity)
    }

    /// `#%unbox` / `#%set-box!` inline, in any position. Returns the result
    /// without pushing it, or `None` (having emitted nothing) when the generic
    /// call has to handle it.
    fn inline_box_primitive(&mut self, target: usize, arity: usize) -> Option<Value> {
        if target == crate::steel_vm::primitives::steel_unbox_mutable as *const () as usize
            && arity == 1
        {
            let last = self.shadow_stack.last().copied()?;
            self.shadow_mark_local_type_from_var(last, InferredType::Box);

            let (value, owned) = match last {
                MaybeStackValue::Borrowed(_) => return None,
                MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i) => {
                    self.shadow_stack_pop();
                    (self.read_from_vm_stack(i), false)
                }
                MaybeStackValue::Value(StackValue { value, .. }) => {
                    self.shadow_stack_pop();
                    (value, true)
                }
                // A constant is never a box; let the generic path raise.
                MaybeStackValue::Constant(_) => return None,
            };

            return Some(self.unbox_value_checked_register(value, owned));
        }

        if target == crate::steel_vm::primitives::steel_set_box_mutable as *const () as usize
            && arity == 2
        {
            // `split_off` hands back owned operands, cloning a register, so the
            // box is released after the write.
            let args = self
                .split_off(arity)
                .into_iter()
                .map(|x| x.0)
                .collect::<Vec<_>>();

            return Some(self.set_box_value_checked_register(args[0], args[1], true));
        }

        None
    }

    /// Whether `inline_box_primitive` will take this call, without emitting.
    fn box_primitive_inlinable(&self, target: usize, arity: usize) -> bool {
        if target == crate::steel_vm::primitives::steel_unbox_mutable as *const () as usize
            && arity == 1
        {
            return matches!(
                self.shadow_stack.last(),
                Some(
                    MaybeStackValue::MutRegister(_)
                        | MaybeStackValue::Register(_)
                        | MaybeStackValue::Value(_)
                )
            );
        }
        target == crate::steel_vm::primitives::steel_set_box_mutable as *const () as usize
            && arity == 2
            && self.shadow_stack.len() >= 2
    }

    /// `null?` in tail position. The empty list is inlined; the primitive also
    /// answers for empty vectors, which the helper handles.
    fn inline_null_predicate(&mut self) -> Value {
        let value = self.pop_boxed();
        let is_list = self.is_type(value, SteelVal::LIST_TAG);
        let typ = self.int;

        self.converging_if(
            is_list,
            |ctx| {
                let cell = ctx.unbox_value_to_pointer(value);
                let index = ctx.list_cell_index(cell);
                let empty = ctx.builder.ins().icmp_imm_s(IntCC::Equal, index, 0);
                ctx.drop_tagged_value(value);
                let empty = ctx.builder.ins().uextend(types::I64, empty);
                ctx.encode_value(SteelVal::BOOL_TAG as i64, empty)
            },
            // Consumes the value, as the primitive does.
            |ctx| ctx.call_helper(NULL_HANDLER, [value]),
            typ,
        )
    }

    /// `cdr` in tail position, as a direct call to the primitive instead of the
    /// generic tail call handler, which looks up the global again, matches on
    /// its kind and hands the frame to the interpreter.
    fn inline_mut_func_tail_call(&mut self, target: usize, arity: usize) -> Option<Value> {
        let helper = if target == crate::primitives::lists::steel_cdr as *const () as usize
            && tail_cdr_enabled()
        {
            CDR_TAIL_VALUE
        } else if target == crate::primitives::lists::steel_reverse as *const () as usize
            && tail_reverse_enabled()
        {
            REVERSE_TAIL_VALUE
        } else {
            return None;
        };

        if arity != 1 || self.shadow_stack.is_empty() {
            return None;
        }

        let value = self.pop_boxed();
        let fallback_ip = self.i64_const(self.ip as i64);

        Some(self.call_helper_checked(helper, [fallback_ip, value]))
    }

    /// `mutable-vector` called in any other position. The same direct call, but
    /// anything else the jitted frame is still holding has to reach the vm
    /// stack first, because the allocation can collect and only the vm stack
    /// and the arguments themselves are roots.
    fn inline_builtin_global_call(&mut self, index: usize, arity: usize) -> Option<Value> {
        // This bakes the builtin in (`global_is_closed`).
        if !extra_primitive_tail_calls_enabled()
            || !tail_vec_enabled()
            || !self.global_is_closed(index)
        {
            return None;
        }

        let SteelVal::BuiltIn(b) = self._globals.get(index)? else {
            return None;
        };

        let builtin = *b as usize;
        let make_vector = builtin == crate::primitives::vectors::make_vector as *const () as usize;

        // The constructor for this many elements, or `None` for `make-vector`.
        let constructor =
            if builtin == crate::primitives::vectors::mut_vec_construct as *const () as usize {
                Some(CallMutableVectorConstructorsDefinitions::arity_to_name(
                    arity,
                )?)
            } else if make_vector && (arity == 1 || arity == 2) {
                None
            } else {
                return None;
            };

        let below = self.shadow_stack.len().checked_sub(arity)?;
        for i in 0..below {
            self.shadow_spill(i);
        }

        let result = match constructor {
            Some(name) => {
                let args: Vec<Value> = self.split_off(arity).into_iter().map(|x| x.0).collect();
                self.call_function_returns_value_args(name, &args)
            }
            None => {
                // It can raise on a negative length or a non-integer, so it needs
                // somewhere to hand the frame back to.
                let fallback_ip = self.i64_const(self.ip as i64);
                if arity == 1 {
                    let [length] = self.pop_operands();
                    self.call_helper_checked(CALL_MAKE_VECTOR_1, [fallback_ip, length])
                } else {
                    let [length, fill] = self.pop_operands();
                    self.call_helper_checked(CALL_MAKE_VECTOR_2, [fallback_ip, length, fill])
                }
            }
        };

        Some(result)
    }

    /// `mutable-vector` in tail position, as a direct call to the allocation.
    /// Same shape and condition as the mutable struct constructor above. The
    /// arguments have to be the whole operand stack, because the allocation can
    /// collect and only the vm stack and these are roots.
    fn inline_mutable_vector_tail_call(&mut self, builtin: usize, arity: usize) -> Option<Value> {
        if builtin != crate::primitives::vectors::mut_vec_construct as *const () as usize {
            return None;
        }

        let name = CallMutableVectorConstructorsDefinitions::arity_to_name(arity)?;

        if self.shadow_stack.len() != arity {
            return None;
        }

        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        Some(self.call_function_returns_value_args(name, &args))
    }

    /// `#%make-mutable-struct` in tail position, as a direct call to the
    /// allocation instead of the generic tail call handler.
    ///
    /// Only when the constructor's arguments are the whole operand stack. The
    /// allocation can run a collection, and the collector's roots are the vm
    /// stack plus these arguments, so an operand held only in the jitted frame
    /// would not be a root. In tail position nothing else is usually live, as
    /// in `(define (cons a b) (mpair a b))`.
    fn inline_mutable_struct_tail_call(&mut self, builtin: usize, arity: usize) -> Option<Value> {
        if builtin != crate::steel_vm::primitives::make_mutable_struct as *const () as usize {
            return None;
        }

        let name = CallMutableStructConstructorsDefinitions::arity_to_name(arity)?;

        if self.shadow_stack.len() != arity {
            return None;
        }

        let fallback_ip = self.i64_const(self.ip as i64);

        let mut args = vec![fallback_ip];
        args.extend(self.split_off(arity).into_iter().map(|x| x.0));

        let result = self.call_function_returns_value_args(name, &args);
        self.check_deopt();

        Some(result)
    }

    /// Whether `inline_byte_vector_op` can take this call. Immutable, because a
    /// match guard cannot borrow `self` mutably.
    fn byte_vector_shape_ok(&self, arity: usize) -> bool {
        let Some(base) = self.shadow_stack.len().checked_sub(arity) else {
            return false;
        };

        // A borrowed operand would have to be materialised before the helper
        // could consume it; leave those to the generic call.
        (0..arity).all(|k| {
            !matches!(
                self.shadow_stack.get(base + k),
                None | Some(MaybeStackValue::Borrowed(_))
            )
        })
    }

    /// `bytes-ref` / `bytes-set!` with no call.
    ///
    /// Operands come off with `split_off`, the same way `inline_box_primitive`
    /// takes `#%set-box!`'s. Reading a spilled operand's value out of the shadow
    /// stack by hand and materialising the others afterwards puts the
    /// definition in a block that no longer dominates the use, which cranelift
    /// rejects.
    fn inline_byte_vector_op(&mut self, function: FunctionSignature, arity: usize) -> Value {
        // `split_off` hands back owned operands, cloning a register, so the
        // bytevector is released after the operation.
        let owned = true;

        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let bytevector_value = args[0];
        let ip = self.ip;

        let fallback = {
            let args = args.clone();
            move |ctx: &mut Self| {
                let func = ctx
                    .builder
                    .ins()
                    .iconst(ctx.module.target_config().pointer_type(), function as i64);
                let fallback_ip = ctx.i64_const(ip as i64);

                let name = CallPrimitiveDefinitions::arity_to_name(arity).unwrap();
                let mut call_args = vec![func, fallback_ip];
                call_args.extend(args.iter().copied());

                let res = ctx.call_function_returns_value_args(name, &call_args);
                ctx.check_deopt();
                res
            }
        };

        if arity == 2 {
            self.inline_bytes_ref(bytevector_value, args[1], owned, fallback)
        } else {
            self.inline_bytes_set(bytevector_value, args[1], args[2], owned, fallback)
        }
    }

    /// The four integer division operators, on two fixnums.
    ///
    /// None of these have an opcode, so otherwise every call goes out through
    /// `call_primitive_function_deopt_2`, which publishes the safepoint and
    /// hands the arguments to the generic primitive.
    ///
    /// Cranelift's `sdiv`/`srem` trap on a zero divisor and on
    /// `isize::MIN / -1`, and the primitive answers those two with an error and
    /// a `BigNum`, so `b == -1` also goes out of line. Within that guard neither
    /// the truncating nor the flooring form can overflow, so there is no exit on
    /// the fast path. `q - 1` is bounded because `|q| <= |a| / 2`, and `r + b` is
    /// bounded because `r` is smaller than `b` and has the opposite sign.
    ///
    /// The result is pushed as `Any`, since the fallback can return a float or
    /// a `BigNum`, and both arms have to describe the merge.
    fn inline_int_divmod(&mut self, mode: DivMode, function: FunctionSignature) -> Value {
        self.guarded_op::<2>()
            .require([SteelVal::INT_TAG, SteelVal::INT_TAG])
            // Reading the payload is pure bit extraction, so it is fine to do
            // before the tag check has decided anything.
            .and_also(|ctx, [_, rhs]| {
                let divisor = ctx.unbox_value_to_pointer(rhs);
                let not_zero = ctx.builder.ins().icmp_imm_s(IntCC::NotEqual, divisor, 0);
                let not_neg_one = ctx.builder.ins().icmp_imm_s(IntCC::NotEqual, divisor, -1);
                ctx.builder.ins().band(not_zero, not_neg_one)
            })
            .emit(
                |ctx, [lhs, rhs]| {
                    let l = ctx.unbox_value_to_pointer(lhs);
                    let r = ctx.unbox_value_to_pointer(rhs);

                    let res = match mode {
                        DivMode::TruncQuotient => ctx.builder.ins().sdiv(l, r),
                        DivMode::TruncRemainder => ctx.builder.ins().srem(l, r),

                        // The floored forms differ from the truncated ones only
                        // when the remainder is non zero and has the opposite sign
                        // to the divisor. `(r ^ b) < 0` is that sign test without
                        // materialising either comparison.
                        DivMode::FloorQuotient | DivMode::FloorRemainder => {
                            let rem = ctx.builder.ins().srem(l, r);
                            let rem_nonzero = ctx.builder.ins().icmp_imm_s(IntCC::NotEqual, rem, 0);
                            let signs = ctx.builder.ins().bxor(rem, r);
                            let opposite =
                                ctx.builder
                                    .ins()
                                    .icmp_imm_s(IntCC::SignedLessThan, signs, 0);
                            let correct = ctx.builder.ins().band(rem_nonzero, opposite);

                            if let DivMode::FloorQuotient = mode {
                                let quo = ctx.builder.ins().sdiv(l, r);
                                let lowered = ctx.builder.ins().iadd_imm_s(quo, -1);
                                ctx.builder.ins().select(correct, lowered, quo)
                            } else {
                                let shifted = ctx.builder.ins().iadd(rem, r);
                                ctx.builder.ins().select(correct, shifted, rem)
                            }
                        }
                    };

                    ctx.encode_value(discriminant(&SteelVal::IntV(0)) as i64, res)
                },
                |ctx, [lhs, rhs]| {
                    let func = ctx
                        .builder
                        .ins()
                        .iconst(ctx.module.target_config().pointer_type(), function as i64);
                    let fallback_ip = ctx.i64_const(ctx.ip as i64);

                    let name = CallPrimitiveDefinitions::arity_to_name(2).unwrap();
                    let res =
                        ctx.call_function_returns_value_args(name, &[func, fallback_ip, lhs, rhs]);
                    ctx.check_deopt();
                    res
                },
                types::I128,
            )
    }

    /// Inline a two-operand integer comparison, checking both tags at runtime.
    ///
    /// The other fast paths for `=`, `<` and friends require one side to be a
    /// compile-time constant or a known register. Here both operands are
    /// materialised as values, both tags are tested, and the fast path is a
    /// single `icmp` on the payloads.
    ///
    /// `fallback` must be a helper returning an unboxed bool, so both arms of
    /// the branch agree on type and the result can be consumed directly by a
    /// following `if` without boxing.
    fn inline_int_compare_two(
        &mut self,
        cc: IntCC,
        fallback: Helper<2, true, true, true>,
    ) -> Value {
        self.guarded_op::<2>()
            .require([SteelVal::INT_TAG, SteelVal::INT_TAG])
            .emit(
                |ctx, [lhs, rhs]| {
                    // Tags already checked, so only the payloads need comparing.
                    let l = ctx.unbox_value_to_pointer(lhs);
                    let r = ctx.unbox_value_to_pointer(rhs);
                    ctx.builder.ins().icmp(cc, l, r)
                },
                |ctx, [lhs, rhs]| ctx.call_helper_checked(fallback, [lhs, rhs]),
                types::I8,
            )
    }

    /// `+`, `-`, `*` or `/` (`op`) of two floats inline, and the two argument
    /// helper for anything else.
    fn inline_float_binop_two(&mut self, op: OpCode) -> Value {
        let fallback = match op {
            OpCode::ADD => ADD_BINOP,
            OpCode::SUB => SUB_BINOP,
            OpCode::MUL => MULT_TWO,
            _ => DIV_TWO,
        };
        self.guarded_op::<2>()
            .require([SteelVal::FLOAT_TAG, SteelVal::FLOAT_TAG])
            .emit(
                |ctx, [lhs, rhs]| {
                    let l = ctx.unbox_value_to_float(lhs);
                    let r = ctx.unbox_value_to_float(rhs);
                    let res = match op {
                        OpCode::ADD => ctx.builder.ins().fadd(l, r),
                        OpCode::SUB => ctx.builder.ins().fsub(l, r),
                        OpCode::MUL => ctx.builder.ins().fmul(l, r),
                        _ => ctx.builder.ins().fdiv(l, r),
                    };
                    ctx.encode_float_value(res)
                },
                |ctx, [lhs, rhs]| ctx.call_helper_checked(fallback, [lhs, rhs]),
                types::I128,
            )
    }

    /// `+`, `-` or `*` of `N` operands of unknown type. All fixnums are
    /// combined inline unless a step overflows, all floats likewise, and
    /// anything else goes to `fallback`, which folds left to right the same way.
    fn inline_number_op<const N: usize>(
        &mut self,
        op: OpCode,
        fallback: Helper<N, true, true, true>,
    ) -> Value {
        let args = self.pop_boxed_args::<N>();
        let call = move |ctx: &mut Self| ctx.call_helper_checked(fallback, args);

        let ints = self.tags_are(&args.map(|arg| (arg, SteelVal::INT_TAG)));
        self.converging_if(
            ints,
            |ctx| {
                let payloads = args.map(|arg| ctx.unbox_value_to_pointer(arg));
                let mut acc = payloads[0];
                let mut overflowed = None;
                for &next in &payloads[1..] {
                    let (res, flag) = match op {
                        OpCode::ADD => ctx.builder.ins().sadd_overflow(acc, next),
                        OpCode::SUB => ctx.builder.ins().ssub_overflow(acc, next),
                        _ => ctx.builder.ins().smul_overflow(acc, next),
                    };
                    acc = res;
                    overflowed = Some(match overflowed {
                        Some(any) => ctx.builder.ins().bor(any, flag),
                        None => flag,
                    });
                }
                ctx.converging_if(
                    overflowed.unwrap(),
                    call,
                    |ctx| ctx.encode_value(SteelVal::INT_TAG as _, acc),
                    types::I128,
                )
            },
            |ctx| {
                let floats = ctx.tags_are(&args.map(|arg| (arg, SteelVal::FLOAT_TAG)));
                ctx.converging_if(
                    floats,
                    |ctx| {
                        let values = args.map(|arg| ctx.unbox_value_to_float(arg));
                        let res = values[1..].iter().fold(values[0], |acc, &next| match op {
                            OpCode::ADD => ctx.builder.ins().fadd(acc, next),
                            OpCode::SUB => ctx.builder.ins().fsub(acc, next),
                            _ => ctx.builder.ins().fmul(acc, next),
                        });
                        ctx.encode_float_value(res)
                    },
                    call,
                    types::I128,
                )
            },
            types::I128,
        )
    }

    /// `floor`, `ceiling`, `truncate`, `round` or `exact` inline. A fixnum is
    /// its own result, a float is rounded in place (for `exact`, one with no
    /// fractional part is converted the way the primitive's `as isize` does),
    /// and anything else calls the primitive.
    fn inline_rounding(&mut self, op: RoundOp, function: FunctionSignature) -> Value {
        let arg = self.pop_boxed();
        let call = move |ctx: &mut Self| {
            let func = ctx
                .builder
                .ins()
                .iconst(ctx.module.target_config().pointer_type(), function as i64);
            let fallback_ip = ctx.i64_const(ctx.ip as i64);

            let name = CallPrimitiveDefinitions::arity_to_name(1).unwrap();
            let res = ctx.call_function_returns_value_args(name, &[func, fallback_ip, arg]);
            ctx.check_deopt();
            res
        };

        let int = self.tags_are(&[(arg, SteelVal::INT_TAG)]);
        self.converging_if(
            int,
            move |_| arg,
            |ctx| {
                let float = ctx.tags_are(&[(arg, SteelVal::FLOAT_TAG)]);
                match op {
                    RoundOp::Exact => {
                        let x = ctx.unbox_value_to_float(arg);
                        let whole = ctx.builder.ins().trunc(x);
                        // Zero only when `x` is finite with no fractional part.
                        let fraction = ctx.builder.ins().fsub(x, whole);
                        let zero = ctx.builder.ins().f64const(0.0);
                        let integral = ctx.builder.ins().fcmp(FloatCC::Equal, fraction, zero);
                        let fast = ctx.builder.ins().band(float, integral);
                        ctx.converging_if(
                            fast,
                            |ctx| {
                                let n = ctx.builder.ins().fcvt_to_sint_sat(types::I64, x);
                                ctx.encode_value(SteelVal::INT_TAG as _, n)
                            },
                            call,
                            types::I128,
                        )
                    }
                    _ => ctx.converging_if(
                        float,
                        |ctx| {
                            let x = ctx.unbox_value_to_float(arg);
                            let res = match op {
                                RoundOp::Floor => ctx.builder.ins().floor(x),
                                RoundOp::Ceiling => ctx.builder.ins().ceil(x),
                                RoundOp::Truncate => ctx.builder.ins().trunc(x),
                                _ => ctx.builder.ins().nearest(x),
                            };
                            ctx.encode_float_value(res)
                        },
                        call,
                        types::I128,
                    ),
                }
            },
            types::I128,
        )
    }

    /// `bitwise-and`, `bitwise-ior` or `bitwise-xor` of two fixnums inline.
    /// The primitive raises for anything else, so the result is always a fixnum.
    fn inline_int_bitwise(&mut self, op: BitwiseOp, function: FunctionSignature) -> Value {
        self.guarded_op::<2>()
            .require([SteelVal::INT_TAG, SteelVal::INT_TAG])
            .emit(
                |ctx, [lhs, rhs]| {
                    let l = ctx.unbox_value_to_pointer(lhs);
                    let r = ctx.unbox_value_to_pointer(rhs);
                    let res = match op {
                        BitwiseOp::And => ctx.builder.ins().band(l, r),
                        BitwiseOp::Ior => ctx.builder.ins().bor(l, r),
                        BitwiseOp::Xor => ctx.builder.ins().bxor(l, r),
                    };
                    ctx.encode_value(SteelVal::INT_TAG as _, res)
                },
                |ctx, [lhs, rhs]| {
                    let func = ctx
                        .builder
                        .ins()
                        .iconst(ctx.module.target_config().pointer_type(), function as i64);
                    let fallback_ip = ctx.i64_const(ctx.ip as i64);

                    let name = CallPrimitiveDefinitions::arity_to_name(2).unwrap();
                    let res =
                        ctx.call_function_returns_value_args(name, &[func, fallback_ip, lhs, rhs]);
                    ctx.check_deopt();
                    res
                },
                types::I128,
            )
    }

    fn slow_path_deopt_tail_call(&mut self, function_index: usize, arity: usize) {
        let name = CallGlobalTailFunctionDefinitions::arity_to_name(arity);

        if let Some(name) = name {
            // TODO: We need to spill the local variables here!
            // This function pushes back on to the stack, and then we should just
            // return since we're done now.
            let v = self.call_global_function(arity, name, function_index, true);

            self.push(v, InferredType::Any);
        } else {
            // TODO: We need to spill the local variables here!
            let v = self.call_global_function_spilled(
                arity,
                CALL_GLOBAL_TAIL_SPILLED,
                function_index,
                true,
            );

            self.push(v, InferredType::Any)
        }

        self.check_deopt();

        self.finish_tail_position();
    }

    /// A tail call to global `function_index` that uses whatever it holds when
    /// the call runs.
    fn generic_global_tail_call(&mut self, function_index: usize, arity: usize) {
        match CallFunctionTailDefinitions::arity_to_name(arity) {
            Some(name) if late_tail_enabled() => {
                self.tail_call_global_at_run_time(function_index, arity, name);
                self.depth -= 1;
            }
            _ => self.slow_path_deopt_tail_call(function_index, arity),
        }
    }

    /// Read the global when the call runs and tail call it the way a local
    /// closure is. That path checks for a compiled, fixed arity closure, and
    /// falls back to the helper otherwise.
    fn tail_call_global_at_run_time(&mut self, function_index: usize, arity: usize, name: &str) {
        // The local tail call's ip is one past the `TAILCALL` that carries the
        // arity; ours is still on it.
        self.ip += 1;

        // Borrowed, like a register read (see that arm).
        let value = self.inline_lookup_global(function_index);
        let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);
        let old_stack = self.shadow_stack.clone();

        self.converging_if_no_value(
            is_closure,
            |ctx| {
                let closure = ctx.unbox_value_to_pointer(value);
                ctx.increment_ref_count_closure(closure);
                ctx.inline_local_tail_call(arity, name, closure, value);
            },
            |ctx| {
                ctx.shadow_stack = old_stack.clone();
                ctx.clone_value(value);
                let v = ctx.call_function_with_func(arity, name, true, value);
                ctx.push(v, InferredType::Any);
            },
        );

        self.ip = self.instructions.len() + 1;
        self.check_deopt();
    }

    fn check_null_no_drop(&mut self, value: Value) -> Value {
        // Encode this manually:
        let tag = self.get_tag(value);

        let list_tag = self.tag(23);

        // Compare these tags:
        // TODO: Also - we'll need to check the length of the list!
        // this should be able to be done inline as well, we just have to load
        // the index of the list.
        let is_list = self.builder.ins().icmp(IntCC::Equal, tag, list_tag);

        let pair_block = self.builder.create_block();
        let not_pair_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I8);

        self.builder
            .ins()
            .brif(is_list, pair_block, &[], not_pair_block, &[]);

        self.builder.switch_to_block(pair_block);
        self.builder.seal_block(pair_block);

        let pointer_value = self.unbox_value_to_pointer(value);
        let length = self.list_cell_index(pointer_value);

        let is_empty = self.builder.ins().icmp_imm_s(IntCC::Equal, length, 0);

        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(is_empty)]);

        self.builder.switch_to_block(not_pair_block);
        self.builder.seal_block(not_pair_block);

        let false_value = self.builder.ins().iconst(types::I8, 0);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(false_value)]);

        self.builder.switch_to_block(merge_block);
        let result = self.builder.block_params(merge_block)[0];
        result
    }

    fn register_index(&mut self, index: usize) -> Value {
        // Helpers that take a register number read the slot themselves.
        self.materialize_cursor(index);
        self.i64_const(index as i64)
    }

    fn register_index_small(&mut self, index: usize) -> Value {
        // Helpers that take a register number read the slot themselves.
        self.materialize_cursor(index);
        self.builder.ins().iconst(types::I16, index as i64)
    }

    fn tag(&mut self, tag: u8) -> Value {
        self.builder.ins().iconst(types::I8, tag as i64)
    }

    // TODO: Generalize this to by value functions
    // to work with anything where every argument is just by value.
    //
    // Then, we can specialize as needed.
    fn eof_object(&mut self) {
        let name = CallPrimitiveFixedDefinitions::arity_to_name(1).unwrap();
        let additional_args = self.split_off(1);

        let fallback_ip = self.i64_const(self.ip as i64);

        let function = self.builder.ins().iconst(
            self.module.target_config().pointer_type(),
            eof_objectp_jit as *const () as i64,
        );

        let mut args = vec![function, fallback_ip];
        args.extend(additional_args.into_iter().map(|x| x.0));

        let result = self.call_function_returns_value_args(name, &args);
        // The eof object, which is not a char.
        self.push(result, InferredType::Any);
        self.ip += 1;

        // Don't need to check deopt on predicates
    }

    // Inline decrement of a steel_rc::weak::Weak. ptr is the ArcInner, the weak
    // count sits at offset 8.
    //
    // Weak::drop also frees the allocation at zero, which we can't do from here, so
    // if we're the one taking it to zero we put it back and let the real destructor
    // run. The free list holds a strong ref while the slot is live, so that's cold.
    fn inline_weak_decrement(
        &mut self,
        ptr: Value,
        drop_fn: Helper<1, false, false, false>,
        drop_arg: Value,
    ) {
        let one = self.builder.ins().iconst(weak_counter_type(), 1);
        let offset = self.builder.ins().iadd_imm_s(ptr, weak_counter_offset());

        // atomic_rmw hands back what was in memory before the operation
        let previous = self.builder.ins().atomic_rmw(
            weak_counter_type(),
            MemFlagsData::trusted(),
            AtomicRmwOp::Sub,
            offset,
            one,
        );

        // The raw counter carries an implicit +1 while the heap still owns the
        // box. A previous value of 2 is the last handle to a live box, whose
        // value has to be released now (`Drop for HeapRef`), and 1 is the last
        // handle to one the heap already let go of, whose memory is freed here.
        // Both cases go to the out-of-line drop.
        let hit_zero = self
            .builder
            .ins()
            .icmp_imm_u(IntCC::UnsignedLessThanOrEqual, previous, 2);

        self.converging_if_no_value_else_cold(
            hit_zero,
            |ctx| {
                let one = ctx.builder.ins().iconst(weak_counter_type(), 1);
                let offset = ctx.builder.ins().iadd_imm_s(ptr, weak_counter_offset());
                ctx.builder.ins().atomic_rmw(
                    weak_counter_type(),
                    MemFlagsData::trusted(),
                    AtomicRmwOp::Add,
                    offset,
                    one,
                );
                ctx.call_helper_void(drop_fn, [drop_arg]);
            },
            |_| {},
        );
    }

    fn drop_weak_rc(&mut self, value: Value) {
        // drop-one takes a whole SteelVal, so it gets the tagged value instead of
        // the unboxed pointer we do the arithmetic on.
        let ptr = self.unbox_value_to_pointer(value);

        self.inline_weak_decrement(ptr, DROP_ONE, value);
    }

    fn drop_tagged_value(&mut self, value: Value) {
        let tag = self.get_tag(value);

        let mask = self.i64_const(SteelVal::UNBOXED_MASK as i64);
        let shifted = self.builder.ins().ushr(mask, tag);
        let is_unboxed = self.builder.ins().band_imm_u(shifted, 1);

        self.converging_if_no_value(
            is_unboxed,
            //
            |_| {
                // Do nothing, we don't want to invoke any drop glue
                // since this type is unboxed anyway
            },
            |ctx| {
                let special_rc_mask = ctx.i64_const(SteelVal::SPECIAL_RC_MASK as i64);
                let special_rc_shifted = ctx.builder.ins().ushr(special_rc_mask, tag);
                let is_special_rc = ctx.builder.ins().band_imm_u(special_rc_shifted, 1);

                ctx.converging_if_no_value(
                    is_special_rc,
                    |ctx| {
                        ctx.drop_biased_rc(value);
                    },
                    |ctx| {
                        let weak_rc_mask = ctx.i64_const(SteelVal::WEAK_RC_MASK as i64);
                        let weak_rc_shifted = ctx.builder.ins().ushr(weak_rc_mask, tag);
                        let is_weak_rc = ctx.builder.ins().band_imm_u(weak_rc_shifted, 1);

                        ctx.converging_if_no_value(
                            is_weak_rc,
                            |ctx| {
                                ctx.drop_weak_rc(value);
                            },
                            |ctx| {
                                ctx.drop_value(value);
                            },
                        );
                    },
                );
            },
        );
    }

    /// Is this object biased to the running thread? BRC packs the owner id into
    /// the high bits of the biased half-word, so read it out and compare.
    ///
    /// An unowned object has a zero id, and `VmCore::thread_id` is never zero
    /// in jitted code, so the unowned case fails to match.
    fn check_value_tl(&mut self, value: Value) -> Value {
        let thread_id = self.get_thread_id();
        // Not readonly, since the owner id shares this word with the count,
        // which the increment and decrement paths write.
        let biased = self.builder.ins().load(
            types::I32,
            MemFlagsData::trusted(),
            value,
            biased_word_offset(),
        );

        let obj_thread_id = self
            .builder
            .ins()
            .band_imm_u(biased, steel_rc::biased_tid_mask() as i64);

        self.builder
            .ins()
            .icmp(IntCC::Equal, thread_id, obj_thread_id)
    }

    fn drop_biased_rc(&mut self, tagged_value: Value) {
        let value = self.unbox_value_to_pointer(tagged_value);

        let is_thread_local = self.check_value_tl(value);

        let yes_tl = self.builder.create_block();
        let no_tl = self.builder.create_block();
        let total_merge = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_thread_local, yes_tl, &[], no_tl, &[]);

        self.builder.switch_to_block(yes_tl);
        self.builder.seal_block(yes_tl);

        {
            let biased = self.builder.ins().load(
                types::I32,
                MemFlagsData::trusted(),
                value,
                biased_word_offset(),
            );

            let sub_one = self
                .builder
                .ins()
                .iadd_imm_s(biased, -(steel_rc::biased_counter_one() as i64));

            self.builder.ins().store(
                MemFlagsData::trusted(),
                sub_one,
                value,
                biased_word_offset(),
            );

            let sub_one = self
                .builder
                .ins()
                .ushr_imm_u(sub_one, steel_rc::biased_counter_shift() as i64);

            let yes_drop = self.builder.create_block();
            let merge_block = self.builder.create_block();

            let should_continue =
                self.builder
                    .ins()
                    .icmp_imm_s(IntCC::SignedGreaterThan, sub_one, 0);

            self.builder
                .ins()
                .brif(should_continue, merge_block, &[], yes_drop, &[]);

            self.builder.switch_to_block(yes_drop);
            self.builder.seal_block(yes_drop);

            self.call_helper_void(DROP_VALUE_POST_FAST_DEC, [tagged_value]);

            self.builder.ins().jump(merge_block, &[]);

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);

            self.builder.ins().jump(total_merge, &[]);
        }

        {
            self.builder.switch_to_block(no_tl);
            self.builder.seal_block(no_tl);

            self.call_helper_void(DROP_VALUE_SLOW_DEC, [tagged_value]);

            self.builder.ins().jump(total_merge, &[]);
        }

        self.builder.switch_to_block(total_merge);
        self.builder.seal_block(total_merge);
    }

    fn drop_biased_rc_unboxed_closure(&mut self, value: Value) {
        // First, we need to get the pointer to the box,
        // load it, and then we'll inline the calls for decrement.
        //
        // That will also mean we'll need to add the thread id
        // as an argument to the JIT. For now we're not going to do that
        // while I figure out if we can even do this thing properly.

        let is_thread_local = self.check_value_tl(value);

        // Make two kinds of blocks:
        let yes_tl = self.builder.create_block();
        let no_tl = self.builder.create_block();

        let total_merge = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_thread_local, yes_tl, &[], no_tl, &[]);

        self.builder.switch_to_block(yes_tl);
        self.builder.seal_block(yes_tl);

        // Yes block
        {
            let biased = self.builder.ins().load(
                types::I32,
                MemFlagsData::trusted(),
                value,
                biased_word_offset(),
            );

            let sub_one = self
                .builder
                .ins()
                .iadd_imm_s(biased, -(steel_rc::biased_counter_one() as i64));

            self.builder.ins().store(
                MemFlagsData::trusted(),
                sub_one,
                value,
                biased_word_offset(),
            );

            let sub_one = self
                .builder
                .ins()
                .ushr_imm_u(sub_one, steel_rc::biased_counter_shift() as i64);

            let yes_drop = self.builder.create_block();
            let merge_block = self.builder.create_block();

            // Then we need to check if its greater than 0:

            let should_continue =
                self.builder
                    .ins()
                    .icmp_imm_s(IntCC::SignedGreaterThan, sub_one, 0);

            // Merge block because we need to jump back and continue
            self.builder
                .ins()
                .brif(should_continue, merge_block, &[], yes_drop, &[]);

            self.builder.switch_to_block(yes_drop);
            self.builder.seal_block(yes_drop);

            // Drop the value after we've determined that we can inline the drop function.
            self.call_helper_void(DROP_VALUE_POST_FAST_DEC_CLOSURE, [value]);

            self.builder.ins().jump(merge_block, &[]);

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);

            self.builder.ins().jump(total_merge, &[]);
        }

        // Slow drop with decrement included
        {
            self.builder.switch_to_block(no_tl);
            self.builder.seal_block(no_tl);

            self.call_helper_void(DROP_VALUE_SLOW_DEC_CLOSURE, [value]);

            self.builder.ins().jump(total_merge, &[]);
        }

        self.builder.switch_to_block(total_merge);
        self.builder.seal_block(total_merge);
    }

    // TODO: Replace this with a more sophisticated implementation that doesn't necessarily
    // need the call if we have something like that
    fn drop_value(&mut self, value: Value) {
        self.call_helper_void(DROP_ONE, [value]);
    }

    // value is a HeapRef<_>, i.e. the bare Weak pointer, which is what
    // drop-box / drop-boxed-vec take by value.
    fn drop_heap_box(&mut self, value: Value) {
        self.inline_weak_decrement(value, DROP_BOX, value);
    }

    fn inline_pair_car_unboxed(&mut self, value: Value) -> Value {
        self.builder.ins().load(
            types::I128,
            MemFlagsData::trusted().with_readonly(),
            value,
            pair_field_offset(offset_of!(crate::values::lists::Pair, car)),
        )
    }

    fn inline_pair_car(&mut self, value: Value) -> Value {
        let value = self.unbox_value_to_pointer(value);
        self.builder.ins().load(
            types::I128,
            MemFlagsData::trusted().with_readonly(),
            value,
            pair_field_offset(offset_of!(crate::values::lists::Pair, car)),
        )
    }

    /// This cell's cursor. `index` lives in the chunk arm, sharing bytes with an
    /// inline element, so it can't be read from an inline cell. An inline cell
    /// holds one element and its cursor is always 1.
    fn list_cell_index(&mut self, value: Value) -> Value {
        let is_inline = self.list_is_inline(value);
        self.converging_if(
            is_inline,
            |ctx| ctx.builder.ins().iconst(types::I32, 1),
            |ctx| {
                ctx.builder.ins().load(
                    types::I32,
                    MemFlagsData::trusted(),
                    value,
                    list_index_offset(),
                )
            },
            types::I32,
        )
    }

    /// Does this cell keep its single element inline? The flag rides in the low
    /// bit of the link to the next cell.
    fn list_is_inline(&mut self, value: Value) -> Value {
        let link = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            value,
            list_next_offset(),
        );
        let flag = self
            .builder
            .ins()
            .band_imm_u(link, SteelList::<SteelVal>::cell_inline_flag() as i64);
        self.builder.ins().icmp_imm_s(IntCC::NotEqual, flag, 0)
    }

    /// Address of the cell's `index`-th slot, counting from 1. A cell holding a
    /// single element stores it in the storage field itself; otherwise storage
    /// points at the chunk and the slot sits past its header.
    fn list_slot_ptr(&mut self, value: Value, index: Value) -> Value {
        let is_inline = self.list_is_inline(value);

        // An inline element sits in the cell itself; a chunk sits past its
        // header. Either way the slot is the same stride in from that base.
        let base = self.converging_if(
            is_inline,
            |ctx| {
                ctx.builder
                    .ins()
                    .iadd_imm_s(value, list_elements_offset() as i64)
            },
            |ctx| {
                let chunk = ctx.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    value,
                    list_buffer_offset(),
                );
                ctx.builder
                    .ins()
                    .iadd_imm_s(chunk, SteelList::<SteelVal>::vector_header_size() as i64)
            },
            types::I64,
        );

        let slot = self.builder.ins().iadd_imm_s(index, -1);
        let offset = self
            .builder
            .ins()
            .imul_imm_s(slot, std::mem::size_of::<SteelVal>() as i64);
        self.builder.ins().iadd(base, offset)
    }

    fn unchecked_car_unboxed(&mut self, value: Value) -> Value {
        let index = self.list_cell_index(value);
        let index = self.builder.ins().uextend(types::I64, index);

        let slot_ptr = self.list_slot_ptr(value, index);

        self.builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0)
    }

    fn checked_car(&mut self, original_value: Value, reg: usize) -> Value {
        let value = self.unbox_value_to_pointer(original_value);

        let index = self.list_cell_index(value);
        let index = self.builder.ins().uextend(types::I64, index);

        let is_valid = self.builder.ins().icmp_imm_s(IntCC::NotEqual, index, 0);

        let typ = self.int;

        self.converging_if(
            is_valid,
            |ctx| {
                let slot_ptr = ctx.list_slot_ptr(value, index);
                ctx.builder
                    .ins()
                    .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0)
            },
            |ctx| {
                // Slow path!
                let reg = ctx.register_index(reg);
                let res = ctx.call_helper_checked(CAR_REG, [reg]);
                res
            },
            typ,
        )
    }

    fn is_type(&mut self, value: Value, check_tag: u8) -> Value {
        let tag = self.get_tag(value);
        self.builder
            .ins()
            .icmp_imm_s(IntCC::Equal, tag, check_tag as i64)
    }

    // TODO: Should this advance the ip?
    fn spilled_read_local_fixed(&mut self, op: OpCode, payload: usize) -> MaybeStackValue {
        let let_var_offset: usize = self.let_var_stack.iter().sum();

        if payload > self.arity as usize + let_var_offset {
            let upper_bound = payload - self.arity as usize - let_var_offset;

            for i in 0..upper_bound {
                self.shadow_spill(i);
            }
        }

        // TODO: @Matt -> This is a big deal!
        if payload < self.arity as _ || true {
            match op {
                OpCode::READLOCAL0
                | OpCode::READLOCAL1
                | OpCode::READLOCAL2
                | OpCode::READLOCAL3 => MaybeStackValue::Register(payload),
                OpCode::MOVEREADLOCAL0
                | OpCode::MOVEREADLOCAL1
                | OpCode::MOVEREADLOCAL2
                | OpCode::MOVEREADLOCAL3 => {
                    for index in 0..self.shadow_stack.len() {
                        let item = self.shadow_stack.get_mut(index).unwrap();

                        match item {
                            MaybeStackValue::Register(i) if *i == payload => {
                                let (value, typ) = self.immutable_register_to_value(payload);
                                self.properties.cached_lookups.registers.remove(&payload);
                                self.properties.cached_lookups.cursors.remove(&payload);

                                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                                    value,
                                    inferred_type: typ,
                                    spilled: false,
                                });
                            }
                            _ => {}
                        }
                    }

                    MaybeStackValue::MutRegister(payload)
                }
                _ => panic!(),
            }
        } else {
            // TODO: Have this just read the value itself
            //
            // TODO: On let end scope, if a value is left as a register reference, then
            // what we need to do is replace the values on the stack directly if any of those values
            // are remaining. So in the let var end scope, check if any of the remaining things on the
            // stack references those things. If they do, spill them then dynamically? Wouldn't mutable
            // references to that already? Maybe not?

            // Replace this... with just reading from the vector?
            let value = self.call_func_or_immediate(op, payload);

            self.value_to_local_map.insert(value, payload);

            let inferred_type = if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
                *inferred_type
            } else {
                InferredType::Any
            };
            MaybeStackValue::Value(StackValue {
                value,
                inferred_type,
                spilled: false,
            })
        }
    }

    fn spilled_read_local_value(&mut self, op: OpCode, payload: usize) -> MaybeStackValue {
        let let_var_offset: usize = self.let_var_stack.iter().sum();

        if payload > self.arity as usize + let_var_offset {
            let upper_bound = payload - self.arity as usize - let_var_offset;

            for i in 0..upper_bound {
                self.shadow_spill(i);
            }
        }

        //
        match op {
            OpCode::READLOCAL => MaybeStackValue::Register(payload),
            OpCode::MOVEREADLOCAL => {
                // Check existing stack, and see if we need to spill any existing ones:
                for index in 0..self.shadow_stack.len() {
                    let item = self.shadow_stack.get_mut(index).unwrap();

                    match item {
                        MaybeStackValue::Register(i) if *i == payload => {
                            let (value, typ) = self.immutable_register_to_value(payload);
                            self.properties.cached_lookups.registers.remove(&payload);
                            self.properties.cached_lookups.cursors.remove(&payload);

                            self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                                value,
                                inferred_type: typ,
                                spilled: false,
                            });
                        }
                        _ => {}
                    }
                }

                MaybeStackValue::MutRegister(payload)
            }
            _ => panic!(),
        }
    }

    // Both arms share a FUNC that is a branch target, so it can't be fused
    // into the CALLGLOBAL. Push the callee and let the FUNC apply it.
    fn func_is_join_target(&self, index: usize) -> bool {
        self.join_targets.contains(&index)
            && matches!(
                self.instructions.get(index).map(|x| x.op_code),
                Some(OpCode::FUNC | OpCode::FUNCNOARITY)
            )
    }

    fn push_global_callee(&mut self, payload: usize) {
        let result = self.inline_lookup_global(payload);
        self.clone_value(result);
        self.push(result, InferredType::Any);
        self.ip += 1;
    }

    fn call_global_impl(&mut self, payload: usize) {
        // First - find the index that we have to lookup.
        let function_index = payload;
        self.ip += 1;
        let arity = self.instructions[self.ip].payload_size.to_usize();

        let name = CallGlobalFunctionDefinitions::arity_to_name(arity);

        // Inline builtins in non-tail position too. A call like `(vector rte a)`
        // used as an argument never reaches the tail path above.
        if let Some(value) = self.inline_builtin_global_call(function_index, arity) {
            self.push(value, InferredType::Any);
            // Past the `FUNC` that carried the arity; the generic paths below
            // do this inside `call_global_function`.
            self.ip += 1;
            return;
        }

        // A direct call to a compiled closure. The `CALLGLOBALNOARITY` arm
        // does this too, but that opcode is only assigned when the analysis
        // proves a fixed-arity callee, so a variadic one like `map` arrives here
        // as plain `CALLGLOBAL`. The call site builds the rest list; see
        // `rest_arg_plan`.
        //
        // This bakes the closure in, so it is only done for a global that can
        // never change (`global_is_closed`).
        if callglobal_direct_enabled() && self.global_is_closed(function_index) {
            if let Some(SteelVal::Closure(func)) = self._globals.get(function_index).cloned() {
                // Fixed-arity callees land here too when the analysis could not
                // assign `CALLGLOBALNOARITY` (a name without the `##` prefix, or a
                // define that is not a bare lambda). `rest_arg_plan` rejects an
                // arity mismatch for those.
                if func.super_instructions().is_some()
                    && Self::rest_arg_plan(&func, arity).is_some()
                {
                    let inferred_type = self.return_type_of(func.id);
                    let result = self.call_self_function_experimental(arity, func, None);
                    self.push(result, inferred_type);

                    // The callee can deopt partway through (e.g. a builtin tail
                    // call), leaving the interpreter to finish its frame, so this
                    // needs the same exit check as every other call below.
                    self.check_deopt();
                    return;
                }
            }
        }

        // Only for a closed global, since this bakes the struct function in.
        if self.global_is_closed(function_index) {
            let maybe_global = self._globals.get(function_index).cloned();
            if let Some(maybe_global) = maybe_global {
                if let Some(spec) = create_struct_spec(maybe_global) {
                    // TODO: This is where we inline the calls for struct
                    // functions
                    if let Some((value, typ)) =
                        self.inline_struct_call_no_drop(spec, arity, function_index)
                    {
                        self.push_struct_result(value, typ);
                        return;
                    }
                }
            }
        }

        // A host function, e.g. from `register_fn`. The helper is given the
        // function, and only checks that the global still holds it.
        if let Some(SteelVal::BoxedFunction(func)) = self._globals.get(function_index).cloned() {
            if let Some(helper) = CallBoxedFunctionDefinitions::arity_to_name(arity) {
                // The signature describes this function. Otherwise the helper
                // calls whatever the global holds now, unless it is closed.
                let signature = func
                    .signature
                    .filter(|_| self.global_is_closed(function_index));
                let origins = self.argument_origins(arity);
                let leaked = self.leak_boxed_function(&func);
                let result = self.call_global_function_with(
                    arity,
                    helper,
                    function_index,
                    false,
                    Some(leaked),
                );
                let config = crate::config::config();
                let ty = match signature {
                    Some(signature) if config.jit_primitive_types => {
                        InferredType::from_value_type(signature.return_type)
                    }
                    _ => InferredType::Any,
                };
                if config.jit_primitive_types_log {
                    eprintln!(
                        "[host-type] {} -> {ty:?}",
                        func.name().unwrap_or("<unnamed>")
                    );
                }
                self.push(result, ty);
                self.check_deopt();
                if let Some(signature) = signature {
                    self.narrow_locals_after_call_to(
                        func.name().unwrap_or("<unnamed>"),
                        Some(signature.arg_types),
                        &origins,
                    );
                }
                return;
            }
        }

        if let Some(name) = name {
            let result = self.call_global_function(arity, name, function_index, false);

            // Assuming this worked, we'll want to push this result on to the stack.
            self.push(result, InferredType::Any);
        } else {
            let v = self.call_global_function_spilled(
                arity,
                CALL_GLOBAL_SPILLED,
                function_index,
                false,
            );

            // Only reached when the callee returned a value instead of setting
            // up a frame.
            self.check_deopt();
            self.forget_spilled_args(arity);

            self.push(v, InferredType::Any);
            return;
        }

        // Then, we're gonna check the result and see if we should deopt
        self.check_deopt();
    }

    fn call_set(&mut self, index: usize, value: Value) -> Value {
        self.properties.cached_lookups.cursors.remove(&index);
        let local_callee = self.helper_callee(SET_HANDLER);

        let ctx = self.get_ctx();

        let index = self.i64_const(index as i64);

        let arg_values = vec![ctx, index, value];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];

        result
    }

    fn pop_single(&mut self) {
        let last = self.shadow_pop();

        let local_callee = self.helper_callee(DROP_VALUE);
        let ctx = self.get_ctx();

        let arg_values = vec![ctx, last.0];

        let _ = self.builder.ins().call(local_callee, &arg_values);
    }

    fn pop_value_from_vm_stack(&mut self) -> Value {
        let vm_ctx = self.get_ctx();
        self.inline_pop_from_stack(vm_ctx)
    }

    fn call_test_handler_register(&mut self, register: usize) -> Value {
        let local_callee = self.helper_callee(IF_BRANCH_REGISTER);

        let ctx = self.get_ctx();

        let register = self.register_index(register);

        let arg_values = [ctx, register];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn translate_tco_jmp(&mut self, payload: usize) {
        self.spill_stack();

        let local_callee = self.helper_callee(TCO_JUMP);

        let ctx = self.get_ctx();
        let arity = self.i64_const(payload as i64);

        let arg_values = [ctx, arity];
        let _call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn translate_tco_jmp_no_arity_loop_no_spill(&mut self, payload: usize) {
        self.materialize_borrowed();
        // Which seeded slots are passed a value of their seeded type by
        // construction, read before any of them is popped. An unchanged slot
        // shows up as its own register.
        let proven: Vec<bool> = {
            let n = self.shadow_stack.len();
            let seed = match &self.spec_mode {
                SpecMode::Specialized { seed, .. } => seed.clone(),
                _ => Vec::new(),
            };
            (0..payload)
                .map(|pos| {
                    seed.iter().find(|(k, _)| *k == pos).is_some_and(|(_, ty)| {
                        self.shadow_stack
                            .get(n + pos - payload)
                            .is_some_and(|e| self.entry_has_type(e, *ty))
                    })
                })
                .collect()
        };

        // List cursors across the back edge. A parameter passed its own value
        // (`MutRegister(j)` / `Register(j)` at position `j`, which is what an
        // in-place `(cdr l)` leaves) is carried. Its cursor variables are
        // already the next iteration's, so the argument is replaced by a
        // placeholder nobody reads and its write is skipped. Any other live
        // cursor is materialised first, because the arguments may read it.
        let mut handed_over: Vec<(usize, ListCursor)> = Vec::new();
        if self.cursors_on && payload > 0 && !self.properties.cached_lookups.cursors.is_empty() {
            let n = self.shadow_stack.len();

            // The trailing run of self-moves that `amount_dropped` below pops
            // without reading or writing. A carried slot inside it must be left
            // as it is. A placeholder would end the run early, and every
            // self-move below it would then be written back over its own slot,
            // dropping the value it is storing.
            let mut trailing = 0;
            while trailing < payload {
                let pos = payload - trailing - 1;
                match self.shadow_stack.get(n - payload + pos) {
                    Some(MaybeStackValue::MutRegister(r) | MaybeStackValue::Register(r))
                        if *r == pos =>
                    {
                        trailing += 1
                    }
                    _ => break,
                }
            }
            let first_trailing = payload - trailing;

            // A let slot's cursor handed to parameter `p`, e.g. `(let ((x2 (cdr
            // x))) ... (loop (cdr x2)))` after self-inlining. The head moves from
            // the let slot into `p`'s slot and the header cursor takes over the
            // position, so the walk crosses the back edge unmaterialised.
            // Only when no other argument reads either slot.
            let arity = self.arity as usize;
            for p in 0..first_trailing {
                let Some(MaybeStackValue::MutRegister(r)) =
                    self.shadow_stack.get(n - payload + p).copied()
                else {
                    continue;
                };
                if r == p || r < arity {
                    continue;
                }
                let (Some(from), Some(to)) = (
                    self.properties.cached_lookups.cursors.get(&r).copied(),
                    self.header_cursors.get(&p).copied(),
                ) else {
                    continue;
                };
                let shared = (0..payload).filter(|&q| q != p).any(|q| {
                    matches!(
                        self.shadow_stack.get(n - payload + q),
                        Some(MaybeStackValue::MutRegister(x) | MaybeStackValue::Register(x))
                            if *x == r || *x == p
                    )
                });
                if shared {
                    continue;
                }

                let head = self.remove_from_vm_stack_raw(r);
                self.properties.cached_lookups.cursors.remove(&r);
                self.properties.cached_lookups.registers.remove(&r);
                self.properties.props.insert(
                    ValueOrRegister::Register(r),
                    vec![Properties::InferredType(InferredType::Void)],
                );
                // `p`'s own cursor dies with the head the write below drops.
                self.properties.cached_lookups.cursors.remove(&p);

                let cell = self.builder.use_var(from.cell);
                let index = self.builder.use_var(from.index);
                self.builder.def_var(to.cell, cell);
                self.builder.def_var(to.index, index);

                self.shadow_stack[n - payload + p] = MaybeStackValue::Value(StackValue {
                    value: head,
                    inferred_type: InferredType::List,
                    spilled: false,
                });
                handed_over.push((p, to));
                self.cursor_carries += 1;
            }

            let mut live: Vec<usize> = self
                .properties
                .cached_lookups
                .cursors
                .keys()
                .copied()
                .collect();
            live.sort_unstable();
            for slot in live {
                if slot < payload && slot >= first_trailing {
                    // Popped untouched by `amount_dropped`, so the slot keeps its
                    // head and the cursor its position.
                    self.cursor_carries += 1;
                    continue;
                }
                let carried = slot < payload
                    && matches!(
                        self.shadow_stack.get(n - payload + slot),
                        Some(MaybeStackValue::MutRegister(r) | MaybeStackValue::Register(r))
                            if *r == slot
                    );
                // Nothing among the arguments reads it, and the slot is about
                // to be overwritten or truncated. Either way the head it walked
                // from is dropped, so there is nothing to write back.
                let referenced = (0..payload).any(|q| {
                    matches!(
                        self.shadow_stack.get(n - payload + q),
                        Some(MaybeStackValue::MutRegister(x) | MaybeStackValue::Register(x))
                            if *x == slot
                    )
                });
                if !referenced {
                    self.properties.cached_lookups.cursors.remove(&slot);
                    continue;
                }
                if carried {
                    self.cursor_carries += 1;
                    let void = self.encode_void();
                    self.shadow_stack[n - payload + slot] = MaybeStackValue::Value(StackValue {
                        value: void,
                        inferred_type: InferredType::Void,
                        spilled: false,
                    });
                    self.carry_skip.insert(slot);
                } else {
                    self.materialize_cursor(slot);
                }
            }
        }

        if payload > 0 {
            let mut amount_dropped = 0;

            while let Some(last) = self.shadow_stack.last().copied() {
                match last {
                    MaybeStackValue::Borrowed(_) => break,
                    MaybeStackValue::Value(_) => break,
                    MaybeStackValue::Constant(_) => break,
                    MaybeStackValue::MutRegister(r) => {
                        if r == (payload - amount_dropped - 1) {
                            self.shadow_stack_pop();
                            amount_dropped += 1;
                        } else {
                            break;
                        }
                    }
                    MaybeStackValue::Register(r) => {
                        if r == (payload - amount_dropped - 1) {
                            self.shadow_stack_pop();
                            amount_dropped += 1;
                        } else {
                            break;
                        }
                    }
                }
            }

            // TODO: Translate this back to being inlined!
            if amount_dropped != 0 {
                // Original payload, is the original amount to call
                let original_payload = payload;

                // How many elements did we drop off
                let payload = payload - amount_dropped;

                // Check every remaining argument. In `(loop (cdr x) (- n 1) y)`
                // the in-place `cdr` is left as `MutRegister(0)` but the next one
                // is a computed value, and taking the no-drop write-back there
                // would leak the list on every iteration.
                let args_before_drop = &self.shadow_stack[self.shadow_stack.len() - payload..];

                // Aggressive optimization: if the value is moved, we don't need to put a drop
                // call inlined here. That holds only when every slot written over
                // already gave up its value, as one of the moved arguments or by
                // an earlier move. In `(lp next s)`, with `s` passing through, the
                // slot `next` lands in still holds the old `r`.
                let moved_out = |slot: usize| {
                    args_before_drop
                        .iter()
                        .any(|x| matches!(x, MaybeStackValue::MutRegister(r) if *r == slot))
                        || matches!(
                            self.properties.get(&ValueOrRegister::Register(slot)),
                            Some(Properties::InferredType(InferredType::Void))
                        )
                };
                if args_before_drop
                    .iter()
                    .all(|x| matches!(x, MaybeStackValue::MutRegister(_)))
                    && (0..payload).all(moved_out)
                {
                    let args_off_the_stack = self.split_off_all_mut_register(payload);

                    let args = args_off_the_stack
                        .into_iter()
                        .map(|x| x.0)
                        .collect::<Vec<_>>();

                    self.inline_call_self_tail_call_no_arity_loop_all_mut_register(
                        original_payload as _,
                        &args,
                    );
                } else {
                    // This is what we have left: so this will be the first n elements left
                    let args_off_the_stack = self.split_off(payload);

                    let args = args_off_the_stack
                        .into_iter()
                        .map(|x| x.0)
                        .collect::<Vec<_>>();

                    self.inline_call_self_tail_call_no_arity_loop(original_payload as _, &args);
                }

                // Only after the writes, since before them a live `p` would be
                // materialised over the head just handed to it.
                for (p, to) in &handed_over {
                    self.properties.cached_lookups.cursors.insert(*p, *to);
                }
                self.emit_self_tail_jump(&proven);

                return;
            }
        }

        let args_off_the_stack = self.split_off(payload);

        let args = args_off_the_stack
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        self.inline_call_self_tail_call_no_arity_loop(payload as _, &args);

        // Only after the writes, since before them a live `p` would be
        // materialised over the head just handed to it.
        for (p, to) in &handed_over {
            self.properties.cached_lookups.cursors.insert(*p, *to);
        }
        self.emit_self_tail_jump(&proven);
    }

    /// Back to the top of the loop. The arguments are already written back.
    fn emit_loop_jump(&mut self) {
        // The header's cursor variables have to be current on every edge in.
        // A cursor still live here already is; one that was materialised or
        // overwritten on this path is not, and the slot holds its value now.
        if self.cursors_on {
            let mut header: Vec<(usize, ListCursor)> =
                self.header_cursors.iter().map(|(k, v)| (*k, *v)).collect();
            header.sort_unstable_by_key(|(k, _)| *k);
            for (slot, cursor) in header {
                if self.properties.cached_lookups.cursors.get(&slot) != Some(&cursor) {
                    self.define_cursor_from_slot(cursor, slot);
                }
            }
        }

        let test = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

        let else_block = self.builder.create_block();

        let fake_entry_block = self.fake_entry_block.unwrap();

        // Jump to the fake entry block.
        //
        // Construct a fake loop to otherwise jump back to the normal control
        // flow?
        self.builder
            .ins()
            .brif(test, fake_entry_block, &[], else_block, &[]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        self.properties.dead = true;
    }

    /// The end of a self tail call, once the arguments are written back. Where
    /// it goes depends on which copy of the function this is. The generic copy
    /// loops on itself, since its entry guard already decided, and the
    /// specialized copy moves back out when a seeded slot might not hold its type.
    fn emit_self_tail_jump(&mut self, proven: &[bool]) {
        match self.spec_mode.clone() {
            SpecMode::None => self.emit_loop_jump(),

            // The entry guard already decided; a generic copy stays generic.
            SpecMode::Generic { .. } => {
                self.emit_loop_jump();
            }

            SpecMode::Specialized {
                generic_id, seed, ..
            } => {
                // Slots the translator can prove still hold their type need no
                // check; the rest are checked on the values just written.
                let unproven: Vec<(usize, SpecType)> = seed
                    .iter()
                    .copied()
                    .filter(|(k, _)| !proven.get(*k).copied().unwrap_or(false))
                    .collect();

                if unproven.is_empty() {
                    self.emit_loop_jump();
                    return;
                }

                // Read the tags from memory, since the tail call just wrote new
                // values into these slots and the translator's facts about them
                // are stale.
                let ctx = self.get_ctx();
                let sp = self.get_sp(ctx);
                let buf_ptr = self.stack_buf_ptr(ctx);
                let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
                let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

                let generic_block = self.builder.create_block();
                emit_slot_type_checks(&mut self.builder, frame_base, &unproven, generic_block);
                let pass_block = self.builder.current_block().unwrap();

                // Fill the fallback first and jump back to the top last. The loop
                // jump leaves the builder in a fresh block that the rest of the
                // translation continues into, and a block the builder switches
                // away from while still empty never makes it into the function.
                self.builder.seal_block(generic_block);
                self.builder.switch_to_block(generic_block);
                // The generic copy reads the parameters out of their slots.
                if self.cursors_on && !self.properties.cached_lookups.cursors.is_empty() {
                    let live = self.properties.cached_lookups.cursors.clone();
                    self.write_back_cursors(&live);
                }
                if self.is_frameless_callee() {
                    // The generic copy expects a frame. The arguments are in
                    // their slots, so start the body over in the interpreter
                    // instead, which is what a tail call to it would do.
                    self.frameless_exit_to_interpreter(0);
                } else {
                    self.emit_tail_call_into(generic_id);
                }

                self.builder.switch_to_block(pass_block);
                self.emit_loop_jump();
            }
        }
    }

    fn emit_tail_call_into(&mut self, callee: FuncId) {
        let vm_ctx = self.get_ctx();
        let callee = self.module.declare_func_in_func(callee, self.builder.func);
        self.builder.ins().return_call(callee, &[vm_ctx]);
    }

    // Make the call:
    fn inline_call_self_tail_call_no_arity_loop(&mut self, arity: i64, args: &[Value]) {
        let vm_ctx = self.get_ctx();

        self.write_to_vm_stack_starting_at(0, args, true);

        let index = self.get_sp(vm_ctx);

        let index = self.builder.ins().iadd_imm_s(index, arity);

        if self.let_slots_all_fixnum() {
            self.truncate_stack_no_drop(vm_ctx, index);
        } else {
            self.truncate_stack(vm_ctx, index, None);
        }
    }

    /// Everything above the arguments is a let slot holding an immediate, so
    /// truncating is only a length store, with no runtime loop over the slots'
    /// tags to find values to drop.
    fn let_slots_all_fixnum(&self) -> bool {
        if !self.shadow_stack.is_empty() {
            return false;
        }
        let lets: usize = self.let_var_stack.iter().sum();
        let base = self.arity as usize;
        (base..base + lets).all(|r| self.register_is_immediate(r))
    }

    fn truncate_stack_no_drop(&mut self, vm_ctx: Value, index: Value) {
        let thread_pointer = self.get_thread_pointer(vm_ctx);
        let stack_offset = offset_of!(SteelThread, stack);
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();
        self.builder.ins().store(
            MemFlagsData::trusted(),
            index,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );
    }

    fn inline_call_self_tail_call_no_arity_loop_all_mut_register(
        &mut self,
        arity: i64,
        args: &[Value],
    ) {
        let vm_ctx = self.get_ctx();

        self.write_to_vm_stack_starting_at(0, args, false);

        let index = self.get_sp(vm_ctx);

        let index = self.builder.ins().iadd_imm_s(index, arity);

        self.truncate_stack(vm_ctx, index, None);
    }

    fn call_function(&mut self, arity: usize, name: &str, tail: bool) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let fallback_ip = self.i64_const(self.ip as i64);

        let func = self.shadow_pop().0;

        let mut arg_values = vec![ctx, func, fallback_ip];

        // Use split off instead?
        // arg_values.extend(
        //     self.stack
        //         .drain(self.stack.len() - arity..)

        arg_values.extend(self.split_off(arity).into_iter().map(|x| x.0));

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_function_with_func(
        &mut self,
        arity: usize,
        name: &str,
        tail: bool,
        func: Value,
    ) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let fallback_ip = self.i64_const(self.ip as i64);

        let mut arg_values = vec![ctx, func, fallback_ip];

        arg_values.extend(self.split_off(arity).into_iter().map(|x| x.0));

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_function_with_func_with_args(
        &mut self,
        _arity: usize,
        name: &str,
        tail: bool,
        func: Value,
        args: Vec<Value>,
    ) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let fallback_ip = self.i64_const(self.ip as i64);

        let mut arg_values = vec![ctx, func, fallback_ip];

        arg_values.extend(args);

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// In a specialized copy, a self call whose arguments are proven to have
    /// the seeded types can go straight to this copy. Returns it, with the type
    /// its result is assumed to have.
    fn direct_self_call_target(&self, arity: usize) -> Option<(FuncId, Option<InferredType>)> {
        let SpecMode::Specialized {
            self_id,
            seed,
            assume_result,
            ..
        } = &self.spec_mode
        else {
            return None;
        };
        if arity != self.arity as usize || self.shadow_stack.len() < arity {
            return None;
        }
        let base = self.shadow_stack.len() - arity;
        let fits = seed
            .iter()
            .all(|(k, ty)| self.entry_has_type(&self.shadow_stack[base + k], *ty));
        fits.then_some((*self_id, *assume_result))
    }

    /// How a call of `arity` arguments reaches `func`'s frame. `None` when the
    /// arguments already line up, `Some((surplus, declared))` when the callee is
    /// variadic and the trailing `surplus` of them become its rest list.
    ///
    /// `None` for a variadic callee given too few arguments, since that has to
    /// raise, and the helper path carries the message and span.
    fn rest_arg_plan(func: &Gc<ByteCodeLambda>, arity: usize) -> Option<Option<(usize, usize)>> {
        if !func.is_multi_arity {
            // Exactly its own arity, or the call has to raise. A `NOARITY` call
            // site is proven to match, but a plain `CALLGLOBAL` is not, and a
            // frame built from the wrong argument count reads past its own slots.
            return (func.arity() == arity).then_some(None);
        }

        // `STEEL_JIT_REST_ARGS=0` leaves variadic callees to the helper path.
        if !rest_args_enabled() {
            return None;
        }

        let declared = func.arity();
        if arity + 1 < declared {
            return None;
        }

        Some(Some((arity + 1 - declared, declared)))
    }

    /// The current length of the value stack.
    fn vm_stack_len(&mut self, vm_ctx: Value) -> Value {
        let thread_pointer = self.get_thread_pointer(vm_ctx);
        self.load_field(
            types::I64,
            thread_pointer,
            (offset_of!(SteelThread, stack) + steel_vec::Vec::<SteelVal>::len_offset()),
        )
    }

    /// Leave native code for the interpreter to carry on at `resume_ip` in this
    /// activation. `VmCore.sp` already describes it (a frameless copy keeps it
    /// current), and whoever called us records our frame.
    fn frameless_exit_to_interpreter(&mut self, resume_ip: usize) {
        let vm_ctx = self.get_ctx();
        let ip = self.i64_const(resume_ip as i64);
        self.store_field(ip, vm_ctx, offset_of!(VmCore, ip));
        let not_native = self.builder.ins().iconst(types::I8, 0);
        self.store_field(not_native, vm_ctx, offset_of!(VmCore, is_native));
        let void = self.encode_void();
        let ret = self.builder.ins().return_(&[void]);
        self.deopt_returns.insert(ret);
    }

    /// A self call that pushes no frame; see `frameless_enabled`.
    ///
    /// The value stack ends up as a framed call leaves it (pending operands
    /// spilled, then the arguments), so the callee's slots start at the same
    /// `sp`. Only the frame record and the work around it are skipped. If the
    /// callee comes back not native, the frame it would have had is recorded
    /// then (and, at the boundary, spliced onto the frame stack), so the
    /// interpreter finds what a framed call would have left.
    fn frameless_self_call(
        &mut self,
        arity: usize,
        func: Gc<ByteCodeLambda>,
        info: FramelessInfo,
    ) -> Value {
        self.frameless_calls += 1;

        let instructions = func.body_exp();
        // Leaked, like every lambda baked into code; the frame record takes a
        // reference of its own if one is ever made.
        jit_leak(&func).into_raw();
        let baked = jit_leak(&func);
        drop(func);
        let closure =
            self.i64_const(unsafe { std::mem::transmute::<Gc<ByteCodeLambda>, i64>(baked) });

        // `self.ip` is on the FUNCNOARITY that carries the arity; the call
        // instruction is the one before it.
        let fallback_ip = self.ip;
        self.ip += 1;

        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();
        self.spill_stack();

        let vm_ctx = self.get_ctx();
        // Our own frame base, to put back once the callee returns.
        let own_sp = self.get_sp(vm_ctx);

        self.push_to_many_vm_stack_let_var_new(&args);
        self.properties.cached_lookups.stack_length_capacity = 0;

        let headroom = self.check_should_trampoline(vm_ctx);

        match info.role {
            FramelessRole::Boundary => {
                // The framed fallback below yields to the interpreter.
                let should_yield = self.builder.ins().bxor_imm_u(headroom, 1);
                self.update_ip_native_if_yield(vm_ctx, should_yield, fallback_ip + 1);
            }
            FramelessRole::Callee => {}
        }

        let typ = self.int;
        let res = self.converging_if(
            headroom,
            |ctx| {
                let len = ctx.vm_stack_len(vm_ctx);
                let callee_sp = ctx.builder.ins().iadd_imm_s(len, -(arity as i64));

                let callee = ctx.module.declare_func_in_func(info.id, ctx.builder.func);
                // The callee can grow the value stack.
                ctx.invalidate_buf_ptr();
                let call = ctx.builder.ins().call(callee, &[vm_ctx, callee_sp]);
                let res = ctx.builder.inst_results(call)[0];

                let native = ctx.load_field(types::I8, vm_ctx, offset_of!(VmCore, is_native));
                let back_block = ctx.builder.create_block();
                let exit_block = ctx.builder.create_block();
                ctx.builder
                    .ins()
                    .brif(native, back_block, &[], exit_block, &[]);

                // Not native, so record the frame this call would have pushed.
                ctx.builder.switch_to_block(exit_block);
                ctx.builder.seal_block(exit_block);
                ctx.builder.set_cold_block(exit_block);
                let (instructions_ptr, instructions_len) = {
                    let fat = ctx.rooted_instructions_const(instructions);
                    let halves = ctx.builder.ins().isplit(fat);
                    (halves.0, halves.1)
                };
                let return_ip = ctx.i64_const((fallback_ip + 1) as i64);
                ctx.call_helper_void(
                    FRAMELESS_RECORD_FRAME,
                    [
                        callee_sp,
                        return_ip,
                        closure,
                        instructions_ptr,
                        instructions_len,
                    ],
                );
                if info.role == FramelessRole::Boundary {
                    ctx.call_helper_void(FRAMELESS_SPLICE_FRAMES, []);
                }
                let void = ctx.encode_void();
                let ret = ctx.builder.ins().return_(&[void]);
                ctx.deopt_returns.insert(ret);

                ctx.builder.switch_to_block(back_block);
                ctx.builder.seal_block(back_block);
                // The callee published its own frame base; put ours back.
                ctx.store_field(own_sp, vm_ctx, offset_of!(VmCore, sp));
                ctx.checked_native_block = Some(back_block);
                res
            },
            |ctx| match info.role {
                // Out of native stack at the boundary, so do the ordinary framed
                // hand off.
                FramelessRole::Boundary => {
                    let arity_val = ctx.i64_const(arity as i64);
                    let fallback = ctx.i64_const(fallback_ip as i64);
                    // Part of the call protocol. An arity error is picked up after the merge.
                    ctx.call_helper_unchecked(SETUP_CLOSURE_ARITY, [closure, arity_val, fallback])
                }
                // Inside a frameless copy there is no frame to hand off from, so
                // let the interpreter redo the call instruction instead. The
                // arguments are already where it expects them.
                FramelessRole::Callee => {
                    ctx.frameless_exit_to_interpreter(fallback_ip - 1);
                    let dead = ctx.builder.create_block();
                    ctx.builder.switch_to_block(dead);
                    ctx.builder.seal_block(dead);
                    ctx.encode_void()
                }
            },
            typ,
        );
        res
    }

    fn call_self_function_experimental(
        &mut self,
        arity: usize,
        func: Gc<ByteCodeLambda>,
        target: Option<FuncId>,
    ) -> Value {
        let id = func.id;

        let rest = Self::rest_arg_plan(&func, arity)
            .expect("the caller checks the arity before taking this path");

        let callee_is_self = self.callee_shares_our_instructions(func.body_exp());
        let instr_fat_ptr = self.rooted_instructions_const(func.body_exp());

        jit_leak(&func).into_raw();
        let baked = jit_leak(&func);
        drop(func);

        // Horrendous crimes, but we'll allow it. We'll also leak the instructions...
        let lookup_index =
            self.i64_const(unsafe { std::mem::transmute::<Gc<ByteCodeLambda>, i64>(baked) });

        let fallback_ip = self.ip;

        // Advance to the next thing
        self.ip += 1;

        let args_off_the_stack = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        self.spill_stack();

        self.inline_call_global_function_to(
            id,
            lookup_index,
            fallback_ip,
            &args_off_the_stack,
            instr_fat_ptr,
            callee_is_self,
            target,
            rest,
        )
    }

    fn call_self_function(
        &mut self,
        arity: usize,
        name: &str,
        func: Gc<ByteCodeLambda>,
        tail: bool,
    ) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let baked = jit_leak(&func);
        drop(func);
        // Horrendous crimes, but we'll allow it
        let lookup_index =
            self.i64_const(unsafe { std::mem::transmute::<Gc<ByteCodeLambda>, i64>(baked) });

        let fallback_ip = self.i64_const(self.ip as i64);

        // Advance to the next thing
        self.ip += 1;

        let mut arg_values = vec![ctx, lookup_index, fallback_ip];

        let args_off_the_stack = self.split_off(arity);

        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    // TODO: Assert that this is immutable. We can avoid looking it up.
    // First things first - worst case always is to just bail out and fall back
    // to the existing behavior.
    fn inline_global_tail_call(&mut self, arity: usize, func: Gc<ByteCodeLambda>) {
        let rest = Self::rest_arg_plan(&func, arity)
            .expect("the caller checks the arity before taking this path");

        // First things first, we first get the args off. Worst case, we'll spill
        // these back on to the args
        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let vm_ctx = self.get_ctx();
        let _id = func.id;

        // TODO: Consider if we need to spill the whole stack here. We could also
        // just call drop, but writing the values to the stack will help us drop
        // them.
        self.spill_cloned_stack();

        let instr_fat_ptr = self.rooted_instructions_const(func.body_exp());

        // `func.super_instructions()` is not baked in as a constant. The
        // callee's compiled body can be replaced later (e.g. by a tier upgrade),
        // and a baked address would keep calling whatever was current when this
        // caller was compiled. The lambda pointer below is stable (its refcount
        // is leaked), so load the entry point through it at run time instead.
        jit_leak(&func).into_raw();
        let baked = jit_leak(&func);
        drop(func);

        // Horrendous crimes, but we'll allow it. We'll also leak the instructions...
        let lookup_index =
            self.i64_const(unsafe { std::mem::transmute::<Gc<ByteCodeLambda>, i64>(baked) });

        self.ip += 1;

        self.increment_ref_count_closure(lookup_index);

        // Pass this through
        let offset = self.update_last_stackframe(vm_ctx, lookup_index);

        self.store_field(instr_fat_ptr, vm_ctx, offset_of!(VmCore, instructions));

        self.truncate_stack_with_args(vm_ctx, offset, &args);

        // The callee's last slot is its rest list, built from whatever the call
        // site passed beyond its named arguments.
        if let Some((surplus, _)) = rest {
            let count = self.i64_const(surplus as i64);
            self.call_helper_void(COLLECT_REST_ARGS, [count]);
        }

        // Implement the body of `new_handle_tail_call_closure` here

        // New args now that we've spilled everything
        let args = [vm_ctx];

        let zero = self.i64_const(0);

        self.store_field(zero, vm_ctx, offset_of!(VmCore, ip));

        let sig_ref = self.create_jit_sig_ref();
        let func_ptr = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            lookup_index,
            // Skip the refcount header to reach the lambda itself.
            closure_field_offset(offset_of!(ByteCodeLambda, super_instructions)),
        );

        self.builder
            .ins()
            .return_call_indirect(sig_ref, func_ptr, &args);

        let cold_block = self.builder.create_block();
        self.builder.switch_to_block(cold_block);
    }

    fn inline_local_tail_call(
        &mut self,
        arity: usize,
        name: &str,
        closure: Value,
        original: Value,
    ) {
        // First things first, we first get the args off. Worst case, we'll spill
        // these back on to the args
        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        // The vm context is not pinned to r15 with get_pinned_reg/set_pinned_reg;
        // see `get_ctx`.

        let vm_ctx = self.get_ctx();

        // TODO: This is not going to work here. Instead, we need to load
        // the id of the instruction.
        // See note in inline_call_func. super_instructions is effectively
        // readonly for execution purposes (tier upgrades install a still-valid
        // pointer; a stale None only misses the JIT fast path).
        let super_instruction = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted().with_readonly(),
            closure,
            // Offset for the RC payload
            closure_field_offset(offset_of!(ByteCodeLambda, super_instructions)),
        );

        let super_instruction_exists =
            self.builder
                .ins()
                .icmp_imm_s(IntCC::NotEqual, super_instruction, 0);

        let fixed_arity = self.closure_accepts_arity(closure, arity);
        let super_instruction_exists = self
            .builder
            .ins()
            .band(super_instruction_exists, fixed_arity);

        // I think I just need to drop the values, not spill them, since they're
        // going to get truncated anyway
        self.spill_cloned_stack();

        // TODO: Check if super instruction exists here:

        self.converging_if_no_value(
            super_instruction_exists,
            |ctx| {
                let body_exp_offset = closure_field_offset(offset_of!(ByteCodeLambda, body_exp));

                // See note in inline_call_func. body_exp's buffer pointer and
                // length are immutable after the lambda is constructed.
                let rcbox_ptr = ctx.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted().with_readonly(),
                    closure,
                    body_exp_offset,
                );

                let len = ctx.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted().with_readonly(),
                    closure,
                    body_exp_offset + 8,
                );

                let data_ptr = ctx
                    .builder
                    .ins()
                    .iadd_imm_s(rcbox_ptr, rcbox_slice_data_offset());

                let instr_fat_ptr = ctx.builder.ins().iconcat(data_ptr, len);

                // Not sure if we're gonna need this?
                let _fallback_ip = ctx.ip;

                ctx.ip += 1;

                // Pass this through
                let offset = ctx.update_last_stackframe(vm_ctx, closure);

                ctx.store_field(instr_fat_ptr, vm_ctx, offset_of!(VmCore, instructions));

                ctx.truncate_stack_with_args(vm_ctx, offset, &args);

                // Implement the body of `new_handle_tail_call_closure` here

                // New args now that we've spilled everything
                let args = [vm_ctx];

                let zero = ctx.i64_const(0);

                ctx.store_field(zero, vm_ctx, offset_of!(VmCore, ip));

                let sig_ref = ctx.create_jit_sig_ref();
                ctx.builder
                    .ins()
                    .return_call_indirect(sig_ref, super_instruction, &args);

                let cold_block = ctx.builder.create_block();
                ctx.builder.switch_to_block(cold_block);
            },
            |ctx| {
                let v = ctx.call_function_with_func_with_args(
                    arity,
                    name,
                    true,
                    original,
                    args.clone(),
                );
                ctx.push(v, InferredType::Any);
            },
        );
    }

    fn call_global_function(
        &mut self,
        arity: usize,
        name: &str,
        function_index: usize,
        tail: bool,
    ) -> Value {
        self.call_global_function_with(arity, name, function_index, tail, None)
    }

    /// `call_global_function`, passing the helper the address of the function
    /// the global held at compile time, when there is one (see
    /// `call_boxed_function_handler`).
    fn call_global_function_with(
        &mut self,
        arity: usize,
        name: &str,
        function_index: usize,
        tail: bool,
        func: Option<i64>,
    ) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let func = func.map(|func| self.i64_const(func));

        let lookup_index = self.i64_const(function_index as i64);

        let fallback_ip = self.i64_const(self.ip as i64);

        // Advance to the next thing
        self.ip += 1;

        let mut arg_values = vec![ctx];
        arg_values.extend(func);
        arg_values.extend([lookup_index, fallback_ip]);

        let args_off_the_stack = self.split_off(arity);

        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// Return `value` from the call in tail position that ends this function.
    /// What is emitted after the return goes into an unreachable block.
    fn return_from_tail_call(&mut self, value: Value) {
        let real_res = self.inline_handle_pop(value);
        self.builder.ins().return_(&[real_res]);

        let cold_block = self.builder.create_block();
        self.builder.switch_to_block(cold_block);

        self.finish_tail_position();
    }

    /// A call in tail position ends this function. Stop translating it, and
    /// leave the depth it was entered at.
    fn finish_tail_position(&mut self) {
        self.depth -= 1;
        self.ip = self.instructions.len() + 1;
    }

    fn is_frameless_callee(&self) -> bool {
        matches!(
            self.frameless,
            Some(FramelessInfo {
                role: FramelessRole::Callee,
                ..
            })
        )
    }

    /// The top `N` operands, in call order, taken off the operand stack.
    fn pop_operands<const N: usize>(&mut self) -> [Value; N] {
        let args: Vec<Value> = self.split_off(N).into_iter().map(|x| x.0).collect();
        args.try_into().unwrap()
    }

    /// Call `helper` on the top `N` operands, and push its result as `typ`,
    /// past the call.
    fn call_helper_on_operands<const N: usize>(
        &mut self,
        helper: Helper<N, true, true, true>,
        typ: InferredType,
    ) {
        let args = self.pop_operands::<N>();
        let result = self.call_helper_checked(helper, args);
        self.push(result, typ);
        self.ip += 2;
    }

    /// Record that the top `count` operands are numbers, for the locals they
    /// were read from.
    fn mark_operands(&mut self, count: usize, typ: InferredType) {
        for arg in self
            .shadow_stack
            .get(self.shadow_stack.len() - count..)
            .unwrap()
            .to_vec()
        {
            self.shadow_mark_local_type_from_var(arg, typ);
        }
    }

    fn top_operand_is_int(&self) -> bool {
        self.shadow_stack.last().and_then(|x| self.inferred_type(x)) == Some(InferredType::Int)
    }

    fn second_operand_is_register(&self) -> bool {
        matches!(
            self.shadow_stack.get(self.shadow_stack.len() - 2),
            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
        )
    }

    /// Two operands: an int on top, and a local's register under it.
    fn int_against_register(&self, payload: usize) -> bool {
        payload == 2 && self.top_operand_is_int() && self.second_operand_is_register()
    }

    /// `SteelThread.stack_frames.len()`.
    fn stack_frames_len(&mut self, thread_pointer: Value) -> Value {
        self.load_field(
            types::I64,
            thread_pointer,
            offset_of!(SteelThread, stack_frames) + steel_vec::Vec::<StackFrame>::len_offset(),
        )
    }

    fn set_stack_frames_len(&mut self, thread_pointer: Value, length: Value) {
        self.store_field(
            length,
            thread_pointer,
            offset_of!(SteelThread, stack_frames) + steel_vec::Vec::<StackFrame>::len_offset(),
        );
    }

    /// The buffer behind `SteelThread.stack_frames`.
    fn stack_frames_buf(&mut self, thread_pointer: Value) -> Value {
        self.load_field(
            types::I64,
            thread_pointer,
            offset_of!(SteelThread, stack_frames) + steel_vec::Vec::<StackFrame>::buf_offset(),
        )
    }

    /// The frame at `index` in the stack frame buffer `buf`.
    fn stack_frame_at(&mut self, buf: Value, index: Value) -> Value {
        let size = std::mem::size_of::<StackFrame>() as i64;
        let offset = self.builder.ins().imul_imm_s(index, size);
        self.builder.ins().iadd(buf, offset)
    }

    fn i64_const(&mut self, value: i64) -> Value {
        self.builder.ins().iconst(types::I64, value)
    }

    /// A trusted load of a `ty` at `offset` into `base`, e.g. a field found
    /// with `offset_of!`.
    fn load_field(&mut self, ty: Type, base: Value, offset: usize) -> Value {
        self.builder
            .ins()
            .load(ty, MemFlagsData::trusted(), base, offset as i32)
    }

    /// A trusted store of `value` at `offset` into `base`.
    fn store_field(&mut self, value: Value, base: Value, offset: usize) {
        self.builder
            .ins()
            .store(MemFlagsData::trusted(), value, base, offset as i32);
    }

    /// Whether global `index` can never hold anything but its current value, so
    /// a call can bake that value in (`closed_globals`).
    fn global_is_closed(&self, index: usize) -> bool {
        self.closed_globals.contains(&index)
    }

    /// `PUSH` of a closed global, then `UNBOX`. The global is never reassigned,
    /// so the box it holds stays alive without the clone a `PUSH` takes, and is
    /// read in place.
    fn lower_closed_global_unbox(&mut self, index: usize) {
        let value = self.inline_lookup_global(index);
        let res = self.unbox_value_checked_register(value, false);

        // Past the `PUSH`, the `UNBOX` and the `FUNC` after it.
        self.ip += 3;
        self.push(res, InferredType::Any);
    }

    /// The return type recorded for function `id`, when it has only one.
    fn return_type_of(&self, id: u32) -> InferredType {
        match self.function_return_types.get(&id) {
            Some(ret_types) if ret_types.len() == 1 => ret_types.iter().next().copied().unwrap(),
            _ => InferredType::Any,
        }
    }

    // The address of `func` for generated code to hold, which keeps a
    // reference to it (see `jit_leak`).
    fn leak_boxed_function(&self, func: &Gc<crate::values::functions::BoxedDynFunction>) -> i64 {
        let func = core::mem::ManuallyDrop::new(jit_leak(func));
        Gc::as_ptr(&func) as i64
    }

    // call_global_function without materialising the operand stack.
    fn call_global_function_no_spill(
        &mut self,
        arity: usize,
        name: &str,
        function_index: usize,
    ) -> Value {
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let lookup_index = self.i64_const(function_index as i64);
        let fallback_ip = self.i64_const(self.ip as i64);
        self.ip += 1;
        let mut arg_values = vec![ctx, lookup_index, fallback_ip];
        let args_off_the_stack = self.split_off(arity);
        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn call_global_function_spilled(
        &mut self,
        arity: usize,
        helper: Helper<3, true, true, true>,
        function_index: usize,
        tail: bool,
    ) -> Value {
        let local_callee = self.helper_callee(helper);

        let ctx = self.get_ctx();

        let lookup_index = self.i64_const(function_index as i64);

        let fallback_ip = self.i64_const(self.ip as i64);

        let arity_value = self.i64_const(arity as i64);

        // Advance to the next thing
        self.ip += 1;

        let arg_values = vec![ctx, lookup_index, fallback_ip, arity_value];

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// After a spilled call returns a value, its arguments are gone from the vm
    /// stack: the handler dropped them, or the callee's frame took them.
    fn forget_spilled_args(&mut self, arity: usize) {
        self.shadow_stack.truncate(self.shadow_stack.len() - arity);
        self.properties.cached_lookups.stack_length_capacity = self
            .properties
            .cached_lookups
            .stack_length_capacity
            .saturating_add(arity);
    }

    /// Call the function under `arity` arguments, for more arguments than the
    /// per arity helpers take. They are spilled to the vm stack, where the
    /// helper finds them.
    fn call_function_spilled(
        &mut self,
        arity: usize,
        helper: Helper<3, true, true, true>,
        tail: bool,
    ) -> Value {
        let local_callee = self.helper_callee(helper);
        let ctx = self.get_ctx();
        let fallback_ip = self.i64_const(self.ip as i64);
        let func = self.shadow_pop().0;
        let arity_value = self.i64_const(arity as i64);

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self
            .builder
            .ins()
            .call(local_callee, &[ctx, func, fallback_ip, arity_value]);
        self.builder.inst_results(call)[0]
    }

    fn get_thread_id(&mut self) -> Value {
        if let Some(thread_id) = self.thread_id {
            thread_id
        } else {
            let ctx = self.get_ctx();
            // `Option<ThreadId>` niches into a single u32.
            let thread_id = self.load_field(types::I32, ctx, offset_of!(VmCore, thread_id));

            self.thread_id = Some(thread_id);

            thread_id
        }
    }

    fn check_deopt_ptr_load(&mut self) -> Value {
        let ctx = self.get_ctx();
        let is_native = self.load_field(Type::int(8).unwrap(), ctx, offset_of!(VmCore, is_native));

        is_native
    }

    /// Queue an exit to be emitted after the body, so its loads land after every
    /// block that branches to it.
    fn defer_deopt_exit(&mut self, block: Block, bytecode_ip: usize, stack: Vec<MaybeStackValue>) {
        let cursors = self.properties.cached_lookups.cursors.clone();
        self.pending_deopt_exits
            .push((block, stack, bytecode_ip, cursors));
    }

    /// Fill every queued exit. Each restores the translator state it was queued
    /// with, so the exits do not see each other either.
    fn flush_deopt_exits(&mut self) {
        while let Some((block, stack, bytecode_ip, cursors)) = self.pending_deopt_exits.pop() {
            self.builder.switch_to_block(block);
            // Use the cursors live where this exit was queued, instead of the
            // ones live at the end of the function.
            if !cursors.is_empty() {
                self.write_back_cursors(&cursors);
            }
            self.emit_deopt_exit_scoped(bytecode_ip, stack);
        }
    }

    /// `emit_deopt_exit` in its own scope. The exit block emits loads and
    /// rewrites the caches as it materializes, and none of that may be visible
    /// to the block we return to, since values defined in the exit do not
    /// dominate it.
    fn emit_deopt_exit_scoped(&mut self, bytecode_ip: usize, stack: Vec<MaybeStackValue>) {
        let saved_stack = core::mem::replace(&mut self.shadow_stack, stack);
        let saved_properties = self.properties.clone();
        // Already written back by the caller; the end-of-function cursor table
        // says nothing about this exit.
        self.properties.cached_lookups.cursors.clear();
        let saved_value_to_local = self.value_to_local_map.clone();
        let saved_local_to_value = self.local_to_value_map.clone();
        let saved_let_var_stack = self.let_var_stack.clone();

        self.emit_deopt_exit(bytecode_ip);

        self.shadow_stack = saved_stack;
        self.properties = saved_properties;
        self.value_to_local_map = saved_value_to_local;
        self.local_to_value_map = saved_local_to_value;
        self.let_var_stack = saved_let_var_stack;
    }

    /// Leave jitted code and let the interpreter re-execute `bytecode_ip`.
    ///
    /// The shadow stack knows every live value and the vm stack slot it belongs
    /// to, and spilling it writes them back as `SteelVal`s (`as_steelval` tags
    /// anything carried untagged). The exit materializes, points `ip` at the
    /// instruction to redo, clears `is_native` so the caller resumes the loop
    /// instead of taking our return value, and returns.
    ///
    /// The caller must not have emitted any effect for that instruction yet,
    /// since it is about to happen again.
    fn emit_deopt_exit(&mut self, bytecode_ip: usize) {
        // Write back everything the interpreter will expect on its operand
        // stack. This is the same materialization a two way branch does.
        self.spill_stack_for_branch();

        let ctx = self.get_ctx();

        let ip = self.i64_const(bytecode_ip as i64);
        self.store_field(ip, ctx, offset_of!(VmCore, ip));

        let not_native = self.builder.ins().iconst(types::I8, 0);
        self.store_field(not_native, ctx, offset_of!(VmCore, is_native));

        // Tell the policy this exit happened. A function that keeps arriving
        // here is compiled without speculation from then on.
        if let Some(index) = self.function_context {
            let index = self.i64_const(index as i64);
            self.call_helper_void(RECORD_SPECULATION_DEOPT, [index]);
        }

        let void = self.encode_void();
        let ret = self.builder.ins().return_(&[void]);
        self.deopt_returns.insert(ret);
    }

    /// Whether `is_native` is already known to be set here, because the previous
    /// check continued into this block and nothing since could have cleared it. Only
    /// a call (a helper, or a jitted callee deopting) or a store to the context
    /// changes it.
    fn native_already_checked(&self) -> bool {
        let Some(block) = self.builder.current_block() else {
            return false;
        };
        if self.checked_native_block != Some(block) {
            return false;
        }
        let func = &self.builder.func;
        !func.layout.block_insts(block).any(|inst| {
            let op = func.dfg.insts[inst].opcode();
            op.is_call() || op.can_store()
        })
    }

    fn check_deopt(&mut self) {
        if self.native_already_checked() {
            return;
        }

        let result = self.check_deopt_ptr_load();

        // The shared deopt block only returns. With a cursor live the
        // interpreter would resume from a slot still holding the head of the
        // list, so this exit writes the cursors back first. It needs its own
        // block for that, since different sites have different cursors.
        if self.cursors_on && !self.properties.cached_lookups.cursors.is_empty() {
            let live = self.properties.cached_lookups.cursors.clone();
            let then_block = self.builder.create_block();
            let exit_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(result, then_block, &[], exit_block, &[]);

            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
            self.builder.set_cold_block(exit_block);
            self.write_back_cursors(&live);
            let void = self.encode_void();
            let ret = self.builder.ins().return_(&[void]);
            self.deopt_returns.insert(ret);

            self.builder.switch_to_block(then_block);
            self.builder.seal_block(then_block);
            self.checked_native_block = Some(then_block);
            return;
        }

        let then_block = self.builder.create_block();
        let (deopt_block, needs_fill) = match self.deopt_return_block {
            Some(b) => (b, false),
            None => {
                let b = self.builder.create_block();
                self.deopt_return_block = Some(b);
                (b, true)
            }
        };

        self.builder
            .ins()
            .brif(result, then_block, &[], deopt_block, &[]);

        if needs_fill {
            self.builder.switch_to_block(deopt_block);
            let void = self.encode_void();
            let ret = self.builder.ins().return_(&[void]);
            self.deopt_returns.insert(ret);
        }

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        self.checked_native_block = Some(then_block);
    }

    // Make the shadow stack uniform before a two way branch.
    //
    // Entries can be lazy references to a vm stack slot, and can be spilled or
    // not. The two arms don't have to agree about either, since a call or a scope
    // end in one arm moves a slot out and leaves void behind, or spills the
    // pending entries. The merge only inherits one arm's bookkeeping, so the
    // other path would read emptied slots or a stack of the wrong depth. Spilling
    // here runs in the block that dominates both arms, so they can't disagree.
    fn spill_stack_for_branch(&mut self) {
        for index in 0..self.shadow_stack.len() {
            self.shadow_spill(index);
        }
    }

    fn shadow_spill(&mut self, index: usize) -> Option<()> {
        if let Some(MaybeStackValue::Borrowed(b)) = self.shadow_stack.get(index).copied() {
            let value = self.materialize(b);
            self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                value,
                inferred_type: InferredType::Any,
                spilled: false,
            });
        }
        let guard = self.shadow_stack.get_mut(index)?;
        let mut spilled = false;
        match guard {
            MaybeStackValue::Borrowed(_) => {
                unreachable!("borrowed values are materialized before spilling")
            }
            MaybeStackValue::Value(stack_value) => {
                if !stack_value.spilled {
                    stack_value.spilled = true;
                    spilled = true;
                }
            }
            // The register's type is the value's type, and spilling does not
            // change it.
            MaybeStackValue::MutRegister(p) => {
                let p = *p;
                let (value, inferred_type) = self.mut_register_to_value(p);
                spilled = true;

                self.properties.cached_lookups.registers.remove(&p);

                self.properties.cached_lookups.cursors.remove(&p);

                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type,
                    spilled: true,
                });
            }
            MaybeStackValue::Register(p) => {
                let p = *p;
                let (value, inferred_type) = self.immutable_register_to_value(p);
                spilled = true;

                self.properties.cached_lookups.registers.remove(&p);

                self.properties.cached_lookups.cursors.remove(&p);

                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type,
                    spilled: true,
                });
            }
            MaybeStackValue::Constant(c) => {
                let c = *c;
                let (value, typ) = c.to_value(self);
                spilled = true;

                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type: typ,
                    spilled: true,
                });
            }
        }

        if spilled {
            let value = self.shadow_stack[index].into_value(self);
            let steelval = value.as_steelval(self);

            self.push_to_vm_stack(steelval);
        }

        Some(())
    }

    // TODO: For spilling to the stack, we just _have_ to spill in order.
    // We have a cursor which will go through and mark if the value has already been pushed to the stack.
    // As long as we push the values in the right order, we're good.
    fn push(&mut self, value: Value, typ: InferredType) {
        if let Some(tag) = typ.exact_tag() {
            self.known_tags.insert(value, tag);
        }
        self.shadow_stack.push(MaybeStackValue::Value(StackValue {
            value,
            inferred_type: typ,
            spilled: false,
        }))
    }

    /// Whether a borrowed value can stay borrowed across this instruction. It
    /// neither runs code nor writes anything that could free what the value was
    /// read from, or it is a consumer that takes borrowed operands itself.
    fn op_keeps_borrows(&self, op: OpCode, payload: usize) -> bool {
        match op {
            OpCode::READLOCAL0
            | OpCode::READLOCAL1
            | OpCode::READLOCAL2
            | OpCode::READLOCAL3
            | OpCode::LOADINT0
            | OpCode::LOADINT1
            | OpCode::LOADINT2 => true,
            OpCode::CALLPRIMITIVE => {
                inline_eq_enabled()
                    && !self.func_is_join_target(self.ip + 1)
                    && self
                        .instructions
                        .get(self.ip + 1)
                        .is_some_and(|ins| ins.payload_size.to_usize() == 2)
                    && matches!(
                        self._globals.get(payload),
                        Some(SteelVal::FuncV(f)) if *f as usize == steel_eq as *const () as usize
                    )
            }
            _ => false,
        }
    }

    fn has_borrowed(&self) -> bool {
        self.shadow_stack
            .iter()
            .any(|v| matches!(v, MaybeStackValue::Borrowed(_)))
    }

    /// A fresh owned flag, defined in the current block.
    fn owned_flag(&mut self, owned: bool) -> Variable {
        let var = self.builder.declare_var(types::I8);
        let value = self.builder.ins().iconst(types::I8, owned as i64);
        self.builder.def_var(var, value);
        var
    }

    /// Clone a borrowed value unless a slow path already did. The flag is left
    /// set, so materializing the same value again on the same path does not
    /// clone it twice.
    fn materialize(&mut self, borrowed: BorrowedValue) -> Value {
        let value = borrowed.value;
        let owned = self.builder.use_var(borrowed.owned);
        self.converging_if_no_value(owned, |_| {}, |ctx| ctx.clone_value(value));
        let one = self.builder.ins().iconst(types::I8, 1);
        self.builder.def_var(borrowed.owned, one);
        value
    }

    /// Turn every borrowed value on the operand stack into an owned one. Run
    /// before any instruction that could run code or write memory, which keeps
    /// borrowing sound. At that point whatever the value was read from is still
    /// alive and unchanged, so cloning now is the same as having cloned when it
    /// was read.
    fn materialize_borrowed(&mut self) {
        if !self.has_borrowed() {
            return;
        }
        for i in 0..self.shadow_stack.len() {
            if let MaybeStackValue::Borrowed(b) = self.shadow_stack[i] {
                let value = self.materialize(b);
                self.shadow_stack[i] = MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type: InferredType::Any,
                    spilled: false,
                });
            }
        }
    }

    /// Relabel a borrowed top of stack as a plain value without cloning it, for
    /// a merge that carries its owned flag across instead.
    fn unborrow_top(&mut self) {
        if let Some(MaybeStackValue::Borrowed(b)) = self.shadow_stack.last().copied() {
            *self.shadow_stack.last_mut().unwrap() = MaybeStackValue::Value(StackValue {
                value: b.value,
                inferred_type: InferredType::Any,
                spilled: false,
            });
        }
    }

    /// Push an inlined struct call's result, borrowed if the getter left it so.
    fn push_struct_result(&mut self, value: Value, typ: InferredType) {
        match self.pending_borrow.take() {
            Some(owned) => self.push_borrowed(value, owned),
            None => self.push(value, typ),
        }
    }

    /// An inlined struct call's result, owned.
    fn take_struct_result(&mut self, value: Value) -> Value {
        match self.pending_borrow.take() {
            Some(owned) => self.materialize(BorrowedValue { value, owned }),
            None => value,
        }
    }

    fn push_borrowed(&mut self, value: Value, owned: Variable) {
        self.shadow_stack
            .push(MaybeStackValue::Borrowed(BorrowedValue { value, owned }));
    }

    fn shadow_push(&mut self, last: MaybeStackValue) {
        self.shadow_stack.push(last);
    }

    // TODO: Coalesce the reads from the stack the same
    // way we're coalescing the reads from the stack on the
    // spill_cloned_stack side.
    fn spill_stack(&mut self) {
        self.spill_stack_coalesced();
    }

    fn spill_stack_coalesced(&mut self) {
        self.materialize_borrowed();
        let mut buffered_reads = Vec::new();

        // Types are read before the reads, since moving a mutable register out
        // marks its slot `Void`. Spilling does not change what the value is, so
        // operands pending under a call (`n` in `(* n (fact (- n 1)))`) keep
        // their type.
        let mut register_types = HashMap::new();
        for value in &self.shadow_stack {
            match value {
                MaybeStackValue::MutRegister(p) => {
                    register_types.insert(*p, self.register_type(*p));
                    buffered_reads.push((*p, true));
                }
                MaybeStackValue::Register(p) => {
                    register_types.insert(*p, self.register_type(*p));
                    buffered_reads.push((*p, false));
                }
                _ => {}
            }
        }

        let coalesced_reads = self.read_multiple_from_stack(buffered_reads);

        let mut values_to_spill = Vec::new();

        for index in 0..self.shadow_stack.len() {
            let guard = self.shadow_stack.get_mut(index).unwrap();
            let mut spilled = false;
            match guard {
                MaybeStackValue::Borrowed(_) => {
                    unreachable!("borrowed values are materialized before spilling")
                }
                MaybeStackValue::Value(stack_value) => {
                    if !stack_value.spilled {
                        stack_value.spilled = true;
                        spilled = true;
                    }
                }
                MaybeStackValue::MutRegister(p) => {
                    let p = *p;

                    let value = coalesced_reads.get(&p).copied().unwrap();

                    spilled = true;

                    self.properties.cached_lookups.registers.remove(&p);

                    self.properties.cached_lookups.cursors.remove(&p);

                    self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: register_types[&p],
                        spilled: true,
                    });
                }
                MaybeStackValue::Register(p) => {
                    let p = *p;
                    let value = coalesced_reads.get(&p).copied().unwrap();

                    spilled = true;

                    self.properties.cached_lookups.registers.remove(&p);

                    self.properties.cached_lookups.cursors.remove(&p);

                    self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: register_types[&p],
                        spilled: true,
                    });
                }
                MaybeStackValue::Constant(c) => {
                    let c = *c;
                    let (value, typ) = c.to_value(self);
                    spilled = true;

                    self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: typ,
                        spilled: true,
                    });
                }
            }

            if spilled {
                let value = self.shadow_stack[index].into_value(self);
                let steelval = value.as_steelval(self);

                values_to_spill.push(steelval);
            }
        }

        self.push_to_many_vm_stack_let_var_new(&values_to_spill)
    }

    // Spill all of these at one time!
    fn spill_cloned_stack(&mut self) {
        self.materialize_borrowed();

        let mut values_to_spill = Vec::new();

        let mut buffered_reads = Vec::new();

        for value in &self.shadow_stack {
            match value {
                MaybeStackValue::MutRegister(p) => {
                    buffered_reads.push((*p, true));
                }
                MaybeStackValue::Register(p) => {
                    buffered_reads.push((*p, false));
                }
                _ => {}
            }
        }

        let coalesced_reads = self.read_multiple_from_stack(buffered_reads);

        for value in self.shadow_stack.clone() {
            match value {
                MaybeStackValue::Borrowed(_) => {
                    unreachable!("borrowed values are materialized before spilling")
                }
                MaybeStackValue::Value(stack_value) => {
                    if !stack_value.spilled {
                        let steelval = stack_value.as_steelval(self);
                        values_to_spill.push(steelval);
                    }
                }

                // TODO: Buffer these reads!
                MaybeStackValue::MutRegister(p) => {
                    let value = coalesced_reads.get(&p).unwrap();

                    values_to_spill.push(*value);
                }
                MaybeStackValue::Register(p) => {
                    let value = coalesced_reads.get(&p).unwrap();
                    values_to_spill.push(*value);
                }
                MaybeStackValue::Constant(c) => {
                    let (value, _) = c.to_value(self);
                    values_to_spill.push(value);
                }
            }
        }

        self.push_to_many_vm_stack_let_var_new(&values_to_spill);
    }

    fn shadow_stack_pop(&mut self) -> Option<MaybeStackValue> {
        let popped = self.shadow_stack.pop();

        if let Some(MaybeStackValue::Borrowed(b)) = popped {
            let value = self.materialize(b);
            return Some(MaybeStackValue::Value(StackValue {
                value,
                inferred_type: InferredType::Any,
                spilled: false,
            }));
        }

        // A spilled entry lives on the vm stack, and the ssa value it was
        // spilled from is only good while it still dominates. Once a branch has
        // merged it does not, so read the value back instead of reusing it.
        // `pop_value_from_vm_stack` both loads it and shortens the stack.
        if let Some(MaybeStackValue::Value(stack_value)) = &popped {
            if stack_value.spilled {
                let inferred_type = stack_value.inferred_type.boxed();
                let value = self.pop_value_from_vm_stack();
                if let Some(tag) = inferred_type.exact_tag() {
                    self.known_tags.insert(value, tag);
                }

                return Some(MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type,
                    spilled: false,
                }));
            }
        }

        popped
    }

    fn shadow_pop(&mut self) -> (Value, InferredType) {
        let last = self.shadow_stack_pop().unwrap();

        match last {
            MaybeStackValue::Borrowed(_) => {
                unreachable!("shadow_stack_pop materializes borrowed values")
            }
            MaybeStackValue::Value(last) => {
                assert!(!last.spilled);

                self.value_to_local_map.remove(&last.value);
                (last.as_steelval(self), last.inferred_type.boxed())
            }

            // TODO: @matt specialize these for readlocal 0, 1, 2, etc.
            MaybeStackValue::MutRegister(p) => self.mut_register_to_value(p),
            MaybeStackValue::Register(p) => self.immutable_register_to_value(p),
            MaybeStackValue::Constant(c) => c.to_value(self),
        }
    }

    // TODO: The issue seems to be that values are coming in out of order?
    // We should split off and generate the values in reverse order I believe.
    fn mut_register_to_value(&mut self, p: usize) -> (Value, InferredType) {
        let value = self.remove_from_vm_stack(p);

        let inferred_type = self.register_type(p);

        // We've removed from the stack, meaning we don't need to emit drop glue for this.
        self.properties.props.insert(
            ValueOrRegister::Register(p),
            vec![Properties::InferredType(InferredType::Void)],
        );

        (value, inferred_type)
    }

    // This is a little nicer, although it isn't amazing. Extra clones for no reason.
    //
    // For mutable registers, we can probably brand the values based on what operations
    // are performed on them.
    /// Type of an if-merge's value, the join of every arm reaching it.
    fn merge_type(&mut self, phi: Value, arms: &[InferredType]) -> InferredType {
        let Some((first, rest)) = arms.split_first() else {
            return InferredType::Any;
        };
        // A merge block parameter is always a boxed value, and `join` boxes.
        let t = rest.iter().fold(first.boxed(), |acc, t| acc.join(*t));
        if let Some(tag) = t.exact_tag() {
            self.known_tags.insert(phi, tag);
        }
        t
    }

    /// A shadow stack entry known to hold a fixnum.
    fn entry_is_fixnum(&self, entry: &MaybeStackValue) -> bool {
        match entry {
            MaybeStackValue::Constant(ConstantValue::Int(_)) => true,
            MaybeStackValue::Value(v) => {
                matches!(v.inferred_type, InferredType::Int | InferredType::Int64)
            }
            MaybeStackValue::Register(r) | MaybeStackValue::MutRegister(r) => {
                self.register_is_fixnum(*r)
            }
            _ => false,
        }
    }

    /// What `cdr` of this register gives. A proper list when the register is
    /// one (`ListV` is always proper), nothing known otherwise.
    fn cdr_result_type(&self, r: usize) -> InferredType {
        match self.properties.get(&ValueOrRegister::Register(r)) {
            Some(Properties::ProperList | Properties::ProperNonEmptyList) => InferredType::List,
            _ => InferredType::Any,
        }
    }

    /// A shadow stack entry known to hold a value of `ty`.
    fn entry_has_type(&self, entry: &MaybeStackValue, ty: SpecType) -> bool {
        match (entry, ty) {
            (MaybeStackValue::Borrowed(_), _) => false,
            (MaybeStackValue::Constant(ConstantValue::Int(_)), SpecType::Fixnum) => true,
            (MaybeStackValue::Constant(ConstantValue::Float(_)), SpecType::Float) => true,
            (MaybeStackValue::Constant(ConstantValue::List(_)), SpecType::List) => true,
            (MaybeStackValue::Constant(_), _) => false,
            (MaybeStackValue::Value(v), SpecType::Fixnum) => {
                matches!(v.inferred_type, InferredType::Int | InferredType::Int64)
            }
            (MaybeStackValue::Value(v), ty) => v.inferred_type.boxed() == ty.inferred(),
            (MaybeStackValue::Register(r) | MaybeStackValue::MutRegister(r), ty) => {
                self.register_type(*r) == ty.inferred()
            }
        }
    }

    fn top_two_are_floats(&self) -> bool {
        let n = self.shadow_stack.len();
        n >= 2
            && self.entry_has_type(&self.shadow_stack[n - 1], SpecType::Float)
            && self.entry_has_type(&self.shadow_stack[n - 2], SpecType::Float)
    }

    fn top_two_are_fixnums(&self) -> bool {
        let n = self.shadow_stack.len();
        n >= 2
            && self.entry_is_fixnum(&self.shadow_stack[n - 1])
            && self.entry_is_fixnum(&self.shadow_stack[n - 2])
    }

    /// The untagged payload of an entry `entry_is_fixnum` accepted, already
    /// popped (so a spilled one has been reloaded).
    fn fixnum_payload(&mut self, entry: MaybeStackValue) -> Value {
        match entry {
            MaybeStackValue::Constant(ConstantValue::Int(i)) => self.i64_const(i as i64),
            MaybeStackValue::Value(v) => {
                self.value_to_local_map.remove(&v.value);
                match v.inferred_type {
                    InferredType::Int64 => v.value,
                    // A reload of a spilled `Int64` comes back boxed.
                    _ => self.unbox_value_to_pointer(v.value),
                }
            }
            // A fixnum owns nothing, so reading the payload is enough. There is
            // no reference to take and no drop the slot still owes.
            MaybeStackValue::Register(r) | MaybeStackValue::MutRegister(r) => {
                self.read_from_vm_stack_split(r).1
            }
            other => unreachable!("not a fixnum entry: {:?}", other),
        }
    }

    /// `op` on two fixnum operands. Comparisons need nothing but the compare.
    /// Arithmetic checks for overflow and, on overflow, exits to the interpreter
    /// at this instruction with the operands restored, so the interpreter
    /// produces the bignum and the jitted result is always a fixnum.
    fn fixnum_binop(&mut self, op: OpCode) {
        let deopt_ip = self.ip;

        // Facts about the operands that make overflow impossible, read before
        // the pops. A register already known to be >= some bound, minus a
        // non-negative constant; or known < some bound, plus a constant that
        // keeps the bound representable.
        let n = self.shadow_stack.len();
        let lhs_register = match self.shadow_stack[n - 2] {
            MaybeStackValue::Register(r) | MaybeStackValue::MutRegister(r) => Some(r),
            _ => None,
        };
        let rhs_constant = match self.shadow_stack[n - 1] {
            MaybeStackValue::Constant(ConstantValue::Int(i)) => Some(i as i64),
            _ => None,
        };

        // The snapshot for the overflow exit is taken after the pops. Popping a
        // spilled operand reloads it and shortens the vm stack on the fast path
        // before the overflow branch. A snapshot from before the pops would still
        // call the operand spilled, so the exit would not write it back and the
        // interpreter would re-run the instruction one operand short.
        let rhs_entry = self.shadow_stack_pop().unwrap();
        let lhs_entry = self.shadow_stack_pop().unwrap();
        let mut pre_pop_stack = self.shadow_stack.clone();
        pre_pop_stack.push(lhs_entry);
        pre_pop_stack.push(rhs_entry);

        let rhs = self.fixnum_payload(rhs_entry);
        let lhs = self.fixnum_payload(lhs_entry);

        let compare = match op {
            OpCode::LT => Some(IntCC::SignedLessThan),
            OpCode::LTE => Some(IntCC::SignedLessThanOrEqual),
            OpCode::GT => Some(IntCC::SignedGreaterThan),
            OpCode::GTE => Some(IntCC::SignedGreaterThanOrEqual),
            OpCode::NUMEQUAL => Some(IntCC::Equal),
            _ => None,
        };

        if let Some(cc) = compare {
            let result = self.builder.ins().icmp(cc, lhs, rhs);
            // Same range fact the register-vs-constant `<` arm records, so a
            // later subtraction under this test can skip its overflow check.
            if let (OpCode::LT, Some(r), Some(i)) = (op, lhs_register, rhs_constant) {
                self.properties.add_property(
                    ValueOrRegister::Value(result),
                    Properties::ConditionLessThan(ValueOrRegister::Register(r), i),
                );
            }
            self.push(result, InferredType::UnboxedBool);
            self.ip += 2;
            return;
        }

        // `properties.get` only answers when a slot has exactly one fact, and a
        // fixnum register with a range fact has two, so look through the list.
        let has_fact = |props: &PropertyMap, r: usize, pred: &dyn Fn(&Properties) -> bool| {
            props
                .props
                .get(&ValueOrRegister::Register(r))
                .is_some_and(|facts| facts.iter().any(|f| pred(f)))
        };
        let cannot_overflow = match (op, lhs_register, rhs_constant) {
            (OpCode::SUB, Some(r), Some(c)) if c >= 0 => has_fact(
                &self.properties,
                r,
                &|f| matches!(f, Properties::GreaterThan(bound) if *bound >= 0),
            ),
            (OpCode::ADD, Some(r), Some(c)) if c >= 0 => has_fact(
                &self.properties,
                r,
                &|f| matches!(f, Properties::LessThan(bound) if bound.checked_add(c).is_some()),
            ),
            _ => false,
        };

        let raw = if cannot_overflow {
            match op {
                OpCode::ADD => self.builder.ins().iadd(lhs, rhs),
                _ => self.builder.ins().isub(lhs, rhs),
            }
        } else {
            let (raw, overflow) = match op {
                OpCode::ADD => self.builder.ins().sadd_overflow(lhs, rhs),
                OpCode::SUB => self.builder.ins().ssub_overflow(lhs, rhs),
                _ => self.builder.ins().smul_overflow(lhs, rhs),
            };

            // No spill before the branch. The overflow side is an exit that
            // never rejoins, and `emit_deopt_exit_scoped` materializes the
            // pre-pop snapshot inside the exit block itself. Spilling here would
            // put every pending value and register through a store and a reload
            // on the fast path, and turn unchanged arguments back into writes at
            // the tail call.
            let ok_block = self.builder.create_block();
            let overflow_block = self.builder.create_block();
            self.builder
                .ins()
                .brif(overflow, overflow_block, &[], ok_block, &[]);
            self.builder.seal_block(overflow_block);
            self.defer_deopt_exit(overflow_block, deopt_ip, pre_pop_stack);
            self.builder.switch_to_block(ok_block);
            self.builder.seal_block(ok_block);
            raw
        };

        // Boxed instead of `Int64`, since many arms trust `Int` and treat the
        // value as a whole `SteelVal`, and none of them expect a bare i64.
        let boxed = self.encode_value(SteelVal::INT_TAG as i64, raw);
        self.push(boxed, InferredType::Int);
        self.ip += 2;
    }

    /// At a merge, keep only the register types every incoming path agrees on
    /// (joined), and only the value-to-register links both paths share. The
    /// current state is one path; `locals`/`values` are the other.
    pub(super) fn meet_register_maps(
        &mut self,
        locals: &HashMap<usize, InferredType>,
        values: &HashMap<Value, usize>,
    ) {
        self.local_to_value_map
            .retain(|slot, t| match locals.get(slot) {
                Some(other) => {
                    *t = t.join(*other);
                    *t != InferredType::Any
                }
                None => false,
            });
        self.value_to_local_map
            .retain(|value, slot| values.get(value) == Some(slot));
    }

    /// The slot holds a fixnum. Its recorded type is `Int`, which `LetVar`
    /// only records for fixnum bindings and `SETLOCAL`/`LETENDSCOPE` clear.
    fn register_is_fixnum(&self, r: usize) -> bool {
        self.register_type(r) == InferredType::Int
    }

    fn register_tag(&self, r: usize) -> Option<u8> {
        self.register_type(r).exact_tag()
    }

    /// The slot holds a value with no reference, so nothing needs dropping.
    fn register_is_immediate(&self, r: usize) -> bool {
        self.register_type(r).is_immediate()
    }

    fn mark_register_read(&mut self, r: usize, value: Value) {
        if let Some(tag) = self.register_tag(r) {
            self.known_tags.insert(value, tag);
        }
    }

    fn const_int(&self, v: Value) -> Option<i64> {
        use cranelift::codegen::ir::{InstructionData, Opcode, ValueDef};
        let dfg = &self.builder.func.dfg;
        let ValueDef::Result(inst, _) = dfg.value_def(v) else {
            return None;
        };
        match dfg.insts[inst] {
            InstructionData::UnaryImm {
                opcode: Opcode::Iconst,
                imm,
            } => Some(imm.bits()),
            _ => None,
        }
    }

    /// Statically evaluate a branch condition built from constants, which is
    /// what a tag check on a `known_tags` value becomes via `get_tag`.
    pub(super) fn const_bool(&self, v: Value) -> Option<bool> {
        use cranelift::codegen::ir::{InstructionData, Opcode, ValueDef};
        let dfg = &self.builder.func.dfg;
        let ValueDef::Result(inst, _) = dfg.value_def(v) else {
            return None;
        };
        match dfg.insts[inst] {
            InstructionData::UnaryImm {
                opcode: Opcode::Iconst,
                imm,
            } => Some(imm.bits() != 0),
            // `icmp_imm_s` is sugar for an `iconst` plus an `icmp` in 0.135.
            InstructionData::IntCompare { cond, args, .. } => {
                let a = self.const_int(args[0])?;
                let b = self.const_int(args[1])?;
                match cond {
                    IntCC::Equal => Some(a == b),
                    IntCC::NotEqual => Some(a != b),
                    _ => None,
                }
            }
            InstructionData::Binary {
                opcode: Opcode::Band,
                args,
            } => Some(self.const_bool(args[0])? && self.const_bool(args[1])?),
            _ => None,
        }
    }

    /// The type recorded for a value-stack slot. `properties` is what
    /// `immutable_register_to_value` reads; `local_to_value_map` is what the
    /// dispatch guards read. `LetVar` writes both, so they agree, but a slot can
    /// be present in one and not the other.
    pub(super) fn register_type(&self, r: usize) -> InferredType {
        // Search the facts instead of using `properties.get`, which only answers
        // when a slot has one fact. A typed register that a comparison also gave
        // a range fact would read back as untyped.
        if let Some(facts) = self.properties.props.get(&ValueOrRegister::Register(r)) {
            for fact in facts {
                match fact {
                    Properties::InferredType(t) => return *t,
                    Properties::ProperList | Properties::ProperNonEmptyList => {
                        return InferredType::List
                    }
                    _ => {}
                }
            }
        }
        self.local_to_value_map
            .get(&r)
            .copied()
            .unwrap_or(InferredType::Any)
    }

    fn immutable_register_to_value(&mut self, p: usize) -> (Value, InferredType) {
        let value = self.read_from_vm_stack(p);

        // Increment the ref count for the value:
        self.clone_value(value);

        let inferred_type = self.register_type(p);

        (value, inferred_type)
    }

    fn maybe_patch_from_stack(&mut self, args_off_the_stack: &mut Vec<StackValue>) {
        let mut indices_to_get_from_shadow_stack = Vec::new();

        for (idx, arg) in args_off_the_stack.iter().enumerate() {
            if arg.spilled {
                indices_to_get_from_shadow_stack.push(idx);
            }
        }

        for idx in indices_to_get_from_shadow_stack.iter().rev() {
            let value = self.pop_value_from_vm_stack();

            // A spill and reload does not change the value, so its type survives,
            // as in `shadow_stack_pop`. Spilled values are boxed, hence `boxed`.
            let inferred_type = args_off_the_stack[*idx].inferred_type.boxed();
            if let Some(tag) = inferred_type.exact_tag() {
                self.known_tags.insert(value, tag);
            }

            args_off_the_stack[*idx] = StackValue {
                value,
                inferred_type,
                spilled: false,
            };
        }
    }

    // TODO: This can be done to spill to the stack for argument calling
    //
    // In the event of single arg, double arg, etc, we can keep the values on the stack.
    //

    // TODO: We should coalesce register reads in order to avoid subsequent buf pointer
    // loads?
    fn split_off(&mut self, payload: usize) -> Vec<(Value, InferredType)> {
        let mut args = self
            .shadow_stack
            .split_off(self.shadow_stack.len() - payload);

        let mut buffered_reads = Vec::new();

        for value in &args {
            match value {
                MaybeStackValue::MutRegister(p) => {
                    buffered_reads.push((*p, true));
                }
                MaybeStackValue::Register(p) => {
                    buffered_reads.push((*p, false));
                }
                _ => {}
            }
        }

        let coalesced_reads = self.read_multiple_from_stack(buffered_reads);

        args = args
            .into_iter()
            .map(|x| match x {
                MaybeStackValue::Borrowed(b) => MaybeStackValue::Value(StackValue {
                    value: self.materialize(b),
                    inferred_type: InferredType::Any,
                    spilled: false,
                }),
                MaybeStackValue::Value(stack_value) => MaybeStackValue::Value(stack_value),
                MaybeStackValue::MutRegister(p) => {
                    let value = coalesced_reads.get(&p).copied().unwrap();

                    self.properties.cached_lookups.registers.remove(&p);

                    self.properties.cached_lookups.cursors.remove(&p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Register(p) => {
                    let value = coalesced_reads.get(&p).copied().unwrap();

                    self.properties.cached_lookups.registers.remove(&p);

                    self.properties.cached_lookups.cursors.remove(&p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Constant(c) => {
                    let (value, typ) = c.to_value(self);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: typ,
                        spilled: false,
                    })
                }
            })
            .collect();

        // Patch the args if needed
        for arg in &args {
            if let MaybeStackValue::Value(v) = arg {
                self.value_to_local_map.remove(&v.value);
            }
        }

        let mut args = args
            .into_iter()
            .map(|x| {
                if let MaybeStackValue::Value(value) = x {
                    value
                } else {
                    unreachable!()
                }
            })
            .collect();

        // TODO:
        // Check if this is necessary:
        self.maybe_patch_from_stack(&mut args);

        args.into_iter()
            // Materialized above, so the type has to follow. See `boxed`.
            .map(|x| {
                let v = x.as_steelval(self);
                let t = x.inferred_type.boxed();
                if let Some(tag) = t.exact_tag() {
                    self.known_tags.insert(v, tag);
                }
                (v, t)
            })
            .collect()
    }

    fn split_off_all_mut_register(&mut self, payload: usize) -> Vec<(Value, InferredType)> {
        let mut args = self
            .shadow_stack
            .split_off(self.shadow_stack.len() - payload);

        let mut buffered_reads = Vec::new();

        for value in &args {
            match value {
                MaybeStackValue::MutRegister(p) => {
                    buffered_reads.push((*p, false));
                }
                MaybeStackValue::Register(p) => {
                    buffered_reads.push((*p, false));
                }
                _ => {}
            }
        }

        let coalesced_reads = self.read_multiple_from_stack_no_write_back(buffered_reads);

        args = args
            .into_iter()
            .map(|x| match x {
                MaybeStackValue::Borrowed(b) => MaybeStackValue::Value(StackValue {
                    value: self.materialize(b),
                    inferred_type: InferredType::Any,
                    spilled: false,
                }),
                MaybeStackValue::Value(stack_value) => MaybeStackValue::Value(stack_value),
                MaybeStackValue::MutRegister(p) => {
                    let value = coalesced_reads.get(&p).copied().unwrap();
                    self.properties.cached_lookups.registers.remove(&p);
                    self.properties.cached_lookups.cursors.remove(&p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Register(p) => {
                    let value = coalesced_reads.get(&p).copied().unwrap();
                    self.properties.cached_lookups.registers.remove(&p);
                    self.properties.cached_lookups.cursors.remove(&p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Constant(c) => {
                    let (value, typ) = c.to_value(self);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: typ,
                        spilled: false,
                    })
                }
            })
            .collect();

        // Patch the args if needed
        for arg in &args {
            if let MaybeStackValue::Value(v) = arg {
                self.value_to_local_map.remove(&v.value);
            }
        }

        let mut args = args
            .into_iter()
            .map(|x| {
                if let MaybeStackValue::Value(value) = x {
                    value
                } else {
                    unreachable!()
                }
            })
            .collect();

        // TODO:
        // Check if this is necessary:
        self.maybe_patch_from_stack(&mut args);

        args.into_iter()
            // Materialized above, so the type has to follow. See `boxed`.
            .map(|x| (x.as_steelval(self), x.inferred_type.boxed()))
            .collect()
    }

    fn func_ret_val(
        &mut self,
        op: OpCode,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
    ) {
        let Some(helper) = operand_helper(op, payload) else {
            if let Some(code) = variadic_numeric_code(op) {
                // No helper at this arity, so hand the operands over on the vm
                // stack to one that applies the primitive to a slice.
                let args = self.split_off(payload);
                for arg in args {
                    self.push_to_vm_stack(arg.0);
                }
                let code = self.i64_const(code as i64);
                let arity = self.i64_const(payload as i64);
                let result = self.call_helper_checked(VARIADIC_NUMERIC_SPILLED, [code, arity]);
                self.push(result, inferred_type.boxed());
                self.ip += ip_inc;
                return;
            }
            panic!("no helper for {op:?} with {payload} operands");
        };

        // The collector only sees the vm stack, so a helper that can collect
        // needs every value pending below its operands spilled first, as the
        // vector constructors do.
        if helper_collects(helper.name()) {
            let below = self.shadow_stack.len().saturating_sub(payload);
            for i in 0..below {
                self.shadow_spill(i);
            }
        }

        let result = match helper {
            OperandHelper::One(h) => {
                let args = self.pop_operands();
                self.call_helper_unchecked(h, args)
            }
            OperandHelper::Two(h) => {
                let args = self.pop_operands();
                self.call_helper_unchecked(h, args)
            }
            OperandHelper::Three(h) => {
                let args = self.pop_operands();
                self.call_helper_unchecked(h, args)
            }
            OperandHelper::Four(h) => {
                let args = self.pop_operands();
                self.call_helper_unchecked(h, args)
            }
            OperandHelper::OneSafe(h) => {
                let args = self.pop_operands();
                self.call_helper(h, args)
            }
            OperandHelper::TwoSafe(h) => {
                let args = self.pop_operands();
                self.call_helper(h, args)
            }
        };

        // Any of these can raise. If one did, bail out here instead of carrying
        // on and popping the frame that holds the handler out from under it.
        self.check_deopt();

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);

        self.ip += ip_inc;
    }

    fn constant_to_value(&mut self, payload: usize, constant: SteelVal) -> (Value, InferredType) {
        match &constant {
            SteelVal::NumV(n) => (self.encode_float(*n), InferredType::Float),
            SteelVal::IntV(i) => (self.encode_integer(*i as _), InferredType::Int),

            // Leak the constant, and then just push it up instead
            SteelVal::SymbolV(c) => {
                let value = jit_leak(c);

                // Leak the value?
                value.0.into_raw();

                let as_ptr: i64 = unsafe { std::mem::transmute::<SteelString, _>(jit_leak(c)) };

                let value = self.i64_const(as_ptr);

                // Clone it
                self.increment_ref_count_closure(value);

                (
                    self.encode_value(SteelVal::SYMBOL_TAG as _, value),
                    InferredType::Symbol,
                )
            }

            SteelVal::ListV(c) => {
                let as_ptr: usize =
                    unsafe { std::mem::transmute::<crate::values::lists::List<_>, _>(jit_leak(c)) };

                let value = self.i64_const(as_ptr as i64);

                self.increment_ref_count_closure(value);

                (
                    self.encode_value(SteelVal::LIST_TAG as _, value),
                    InferredType::List,
                )
            }

            _ => (self.push_const_index(payload), InferredType::Any),
        }
    }

    fn call_func_or_immediate(&mut self, op1: OpCode, payload: usize) -> Value {
        match op1 {
            OpCode::LOADINT0 => self.encode_integer(0),
            OpCode::LOADINT1 => self.encode_integer(1),
            OpCode::LOADINT2 => self.encode_integer(2),

            OpCode::PUSHCONST => {
                // Attempt to inline the constant, if it is something that can be inlined.
                // Assuming we know the type of it, we can get really fancy here since
                // we _should_ be able to do something with it if there are other types
                // in play - we can avoid the unboxing / boxing of the type if we know
                // what we're dealing with.

                let constant = self.constants.get_value(payload);

                match &constant {
                    SteelVal::CharV(c) => {
                        let res = self.i64_const(*c as i64);
                        self.encode_value(SteelVal::CHAR_TAG as _, res)
                    }

                    SteelVal::BoolV(b) => {
                        if *b {
                            self.encode_true()
                        } else {
                            self.encode_false()
                        }
                    }

                    SteelVal::IntV(i) => self.encode_integer(*i as _),

                    _ => self.push_const_index(payload),
                }
            }
            _ => {
                let helper = read_helper(op1)
                    .unwrap_or_else(|| panic!("no helper to read a value for {op1:?}"));
                self.call_helper(helper, [])
            }
        }
    }

    // Just... store as a i128, and hope for the best. Don't need to encode
    // it directly as anything really?
    /// Whether a callee runs the same instruction stream we are compiling.
    ///
    /// For a self call the instructions saved into the new frame and the ones
    /// installed into `VmCore` are the same value, so both the load of the
    /// current stream and the store of the new one can be skipped.
    fn callee_shares_our_instructions(&self, callee: RootedInstructions) -> bool {
        core::ptr::eq(callee.ptr, self.instructions.as_ptr())
            && callee.len as usize == self.instructions.len()
    }

    // Build the i128 the jit passes a RootedInstructions around as. Low half is
    // the pointer, high half the length, the same order iconcat(data_ptr, len)
    // produces elsewhere and the fields are declared in.
    fn rooted_instructions_const(&mut self, instructions: RootedInstructions) -> Value {
        let int = Type::int(64).unwrap();
        let ptr = self.builder.ins().iconst(int, instructions.ptr as i64);
        let len = self.builder.ins().iconst(int, instructions.len as i64);

        self.builder.ins().iconcat(ptr, len)
    }

    // If this is an unboxed value, then we need to cast the value
    // to the requisite type if its a boolean.
    #[allow(unused)]
    fn encode_value(&mut self, tag: i64, value: Value) -> Value {
        let tag = self.i64_const(tag);
        self.builder.ins().iconcat(tag, value)
    }

    fn encode_true(&mut self) -> Value {
        let res = self.i64_const(1);
        let boolean = self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
        boolean
    }

    fn encode_float_value(&mut self, value: Value) -> Value {
        let as_int = self
            .builder
            .ins()
            .bitcast(types::I64, MemFlagsData::new(), value);
        self.encode_value(SteelVal::FLOAT_TAG as _, as_int)
    }

    fn encode_float(&mut self, float: f64) -> Value {
        let res = self.builder.ins().f64const(float);
        let as_int = self
            .builder
            .ins()
            .bitcast(types::I64, MemFlagsData::new(), res);
        self.encode_value(discriminant(&SteelVal::NumV(float)) as i64, as_int)
    }

    fn encode_false(&mut self) -> Value {
        let res = self.i64_const(0);
        let boolean = self.encode_value(discriminant(&SteelVal::BoolV(false)) as i64, res);
        boolean
    }

    fn encode_void(&mut self) -> Value {
        let res = self.i64_const(0);
        self.encode_value(discriminant(&SteelVal::Void) as i64, res)
    }

    fn encode_integer(&mut self, integer: i64) -> Value {
        let res = self.i64_const(integer as i64);
        let integer = self.encode_value(discriminant(&SteelVal::IntV(0)) as i64, res);
        self.known_tags.insert(integer, SteelVal::INT_TAG);
        integer
    }

    fn encode_char(&mut self, c: char) -> Value {
        let res = self.i64_const(c as i64);
        let integer = self.encode_value(SteelVal::CHAR_TAG as _, res);
        integer
    }

    // Remove the tag from a value to get to the underlying type
    fn unbox_value(&mut self, value: Value) -> Value {
        let amount_to_shift = self.i64_const(64);
        let encoded_rhs = self.builder.ins().sshr(value, amount_to_shift);
        encoded_rhs
    }

    fn unbox_value_to_pointer(&mut self, value: Value) -> Value {
        let (_, high) = self.builder.ins().isplit(value);
        high
    }

    fn unbox_value_to_float(&mut self, value: Value) -> Value {
        let (_, high) = self.builder.ins().isplit(value);
        self.builder
            .ins()
            .bitcast(types::F64, MemFlagsData::new(), high)
    }

    fn get_tag(&mut self, value: Value) -> Value {
        if let Some(tag) = self.known_tags.get(&value).copied() {
            return self.builder.ins().iconst(types::I8, tag as i64);
        }
        self.builder.ins().ireduce(types::I8, value)
    }

    fn translate_if_else_value(
        &mut self,
        condition_value: Value,
        then_start: usize,
        else_start: usize,
    ) -> Value {
        let last_bound = self.if_bound;
        let mut saved_then_bound = None;

        if matches!(
            self.instructions[else_start - 1].op_code,
            OpCode::JMP | OpCode::POPJMP // | OpCode::POPPURE
                                         // | OpCode::TCOJMP
                                         // | OpCode::SELFTAILCALLNOARITY
                                         // | OpCode::TAILCALLNOARITY
                                         // | OpCode::CALLGLOBALTAIL
                                         // | OpCode::CALLPRIMITIVETAIL
                                         // | OpCode::CALLGLOBALTAILNOARITY
                                         // | OpCode::TAILCALL
        ) {
            self.if_bound = Some(else_start - 1);
            saved_then_bound = self.if_bound;
        }

        let else_offset = Some(self.instructions[else_start - 1].payload_size.to_usize());

        let start = self.ip;
        let depth = self.depth;

        self.spill_stack_for_branch();

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the toy language have a return value.
        // In traditional SSA form, this would produce a PHI between
        // the then and else bodies. Cranelift uses block parameters,
        // so set up a parameter in the merge block, and we'll pass
        // the return values to it from the branches.
        self.builder.append_block_param(merge_block, self.int);

        // Make this merge block visible to any LBBV fork that runs inside
        // the then/else branches so it can register its tail as an extra
        // predecessor.
        self.if_merge_blocks.push(merge_block);
        let merge_flag = self.builder.declare_var(types::I8);
        self.if_merge_flags.push(merge_flag);
        let mut merge_borrowed = false;

        // Test the if condition and conditionally branch.
        self.builder
            .ins()
            .brif(condition_value, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);

        // Set the ip to the right spot:
        self.ip = then_start;

        let let_stack = self.let_var_stack.clone();
        let frozen_stack = self.shadow_stack.clone();

        let original_val_to_local = self.value_to_local_map.clone();
        let original_local_to_val = self.local_to_value_map.clone();
        let original_properties = self.properties.clone();

        self.if_stack.push(start);

        self.check_then_properties(condition_value);

        self.stack_to_ssa();

        self.if_stack.pop();

        self.if_bound = last_bound;

        assert_eq!(self.depth, depth);

        let then_out_of_bounds = self.ip > self.instructions.len();

        // Unwrap or... must have been a tail call?

        let mut then_type = InferredType::Any;
        // A borrowed then value crosses the merge borrowed, with its owned flag;
        // anything else reaches the merge owned.
        let then_owned = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Borrowed(b)) if !then_out_of_bounds => {
                merge_borrowed = true;
                self.unborrow_top();
                self.builder.use_var(b.owned)
            }
            _ => self.builder.ins().iconst(types::I8, 1),
        };
        self.builder.def_var(merge_flag, then_owned);
        let then_return = if then_out_of_bounds {
            BlockArg::Value(self.encode_integer(12345))
        } else {
            let (v, t) = self.shadow_pop();
            then_type = t;
            BlockArg::Value(v)
        };

        let then_stack = self.shadow_stack.clone();
        let then_let_stack = self.let_var_stack.clone();
        let local_map = self.local_to_value_map.clone();
        let value_to_local = self.value_to_local_map.clone();
        let properties = self.properties.clone();

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);

        self.if_stack.push(else_start - 1);

        self.ip = else_start;

        self.if_stack.pop();

        self.tco = false;

        // TODO: Also encode the local var map
        self.let_var_stack = let_stack;
        self.shadow_stack = frozen_stack;
        self.local_to_value_map = original_local_to_val;
        self.value_to_local_map = original_val_to_local;
        self.properties = original_properties;

        // Set the if bound for the else case as well
        self.if_bound = else_offset;

        self.check_else_properties(condition_value);

        self.stack_to_ssa();

        assert_eq!(self.depth, depth);

        let else_out_of_bounds = self.ip > self.instructions.len();

        // Returned, therefore we don't need to do anything.
        let mut else_type = InferredType::Any;
        // A borrowed else value stays borrowed only if the then arm's did too,
        // or the then arm never reaches the merge; `shadow_pop` materializes it
        // otherwise.
        let else_owned = match self.shadow_stack.last().copied() {
            Some(MaybeStackValue::Borrowed(b))
                if !else_out_of_bounds && (merge_borrowed || then_out_of_bounds) =>
            {
                merge_borrowed = true;
                self.unborrow_top();
                self.builder.use_var(b.owned)
            }
            _ => self.builder.ins().iconst(types::I8, 1),
        };
        self.builder.def_var(merge_flag, else_owned);
        let else_return = if else_out_of_bounds {
            BlockArg::Value(self.encode_integer(12345))
        } else {
            let (v, t) = self.shadow_pop();
            else_type = t;
            BlockArg::Value(v)
        };

        let phi = match (then_out_of_bounds, else_out_of_bounds) {
            (true, true) => {
                // No merge block necessary.

                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);

                // Switch to the merge block for subsequent statements.
                self.builder.switch_to_block(merge_block);

                // We've now seen all the predecessors of the merge block.
                self.builder.seal_block(merge_block);

                // Read the value of the if-else by reading the merge block
                // parameter.
                let phi = self.builder.block_params(merge_block)[0];

                phi
            }
            (true, false) => {
                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);

                // Switch to merge block, continue on.
                self.builder.switch_to_block(merge_block);
                self.if_bound = last_bound;

                self.ip = else_offset.unwrap();

                let phi = self.builder.block_params(merge_block)[0];

                let phi_type = self.merge_type(phi, &[else_type]);
                if merge_borrowed {
                    self.push_borrowed(phi, merge_flag);
                } else {
                    self.push(phi, phi_type);
                }

                self.stack_to_ssa();

                self.if_bound = last_bound;

                self.encode_void()
            }
            (false, true) => {
                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);
                // Switch to merge block, continue on.
                self.builder.switch_to_block(merge_block);
                self.if_bound = last_bound;

                self.ip = saved_then_bound.unwrap();
                self.shadow_stack = then_stack;
                self.let_var_stack = then_let_stack;
                self.local_to_value_map = local_map;
                self.value_to_local_map = value_to_local;
                self.properties = properties;

                let phi = self.builder.block_params(merge_block)[0];

                let phi_type = self.merge_type(phi, &[then_type]);
                if merge_borrowed {
                    self.push_borrowed(phi, merge_flag);
                } else {
                    self.push(phi, phi_type);
                }

                self.stack_to_ssa();

                self.if_bound = last_bound;

                self.encode_void()
            }
            (false, false) => {
                // Switch to merge block, pop off value, return
                // the value.

                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);

                // Switch to the merge block for subsequent statements.
                self.builder.switch_to_block(merge_block);

                // We've now seen all the predecessors of the merge block.
                self.builder.seal_block(merge_block);

                // Cursors the arms disagree on only matter if a slot is read
                // after the join. When all that is left is the return, no slot
                // is, and the frame drop releases the heads either way.
                let mut properties = properties;
                if self.cursors_on
                    && self.properties.cached_lookups.cursors != properties.cached_lookups.cursors
                    && self.only_returns_from(else_offset.unwrap())
                {
                    self.properties.cached_lookups.cursors.clear();
                    properties.cached_lookups.cursors.clear();
                }

                // Only what both arms agree on survives the merge
                self.cursor_bail |= self.properties.meet(&properties);
                self.meet_register_maps(&local_map, &value_to_local);

                self.if_bound = last_bound;

                assert_eq!(self.ip, else_offset.unwrap());

                self.ip = else_offset.unwrap();

                let phi = self.builder.block_params(merge_block)[0];

                let phi_type = self.merge_type(phi, &[then_type, else_type]);
                if merge_borrowed {
                    self.push_borrowed(phi, merge_flag);
                } else {
                    self.push(phi, phi_type);
                }

                self.stack_to_ssa();

                self.if_bound = last_bound;

                // Read the value of the if-else by reading the merge block
                // parameter.

                phi
            }
        };

        self.builder.seal_block(then_block);
        self.builder.seal_block(else_block);

        let popped = self.if_merge_blocks.pop();
        debug_assert_eq!(popped, Some(merge_block));
        self.if_merge_flags.pop();

        phi
    }

    fn check_else_properties(&mut self, condition_value: Value) {
        self.properties.infer_property_bool(condition_value, false);
    }

    fn check_then_properties(&mut self, condition_value: Value) {
        self.properties.infer_property_bool(condition_value, true);
    }

    fn push_to_vm_stack(&mut self, value: Value) {
        self.push_to_vm_stack_let_var_new(value);
    }

    fn read_multiple_from_stack(&mut self, values: Vec<(usize, bool)>) -> HashMap<usize, Value> {
        if values.is_empty() {
            return Default::default();
        }

        // The loads below are raw, and a cursor-backed slot holds only its
        // head, so materialize the cursors first.
        for (slot, _) in &values {
            self.materialize_cursor(*slot);
        }

        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let buf_ptr = self.stack_buf_ptr(ctx);

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        let mut results = HashMap::new();

        for (index, kind) in values {
            let res = self.builder.ins().load(
                types::I128,
                MemFlagsData::trusted(),
                frame_base,
                (index * std::mem::size_of::<SteelVal>()) as i32,
            );

            if kind {
                let value = self.encode_void();

                self.builder.ins().store(
                    MemFlagsData::trusted(),
                    value,
                    frame_base,
                    (index * std::mem::size_of::<SteelVal>()) as i32,
                );

                // We've removed from the stack, meaning we don't need to emit drop glue for this.
                self.properties.props.insert(
                    ValueOrRegister::Register(index),
                    vec![Properties::InferredType(InferredType::Void)],
                );
            } else {
                self.clone_value(res);
            }

            results.insert(index, res);
        }

        results
    }

    fn read_multiple_from_stack_no_write_back(
        &mut self,
        values: Vec<(usize, bool)>,
    ) -> HashMap<usize, Value> {
        if values.is_empty() {
            return Default::default();
        }

        // The loads below are raw, and a cursor-backed slot holds only its
        // head, so materialize the cursors first.
        for (slot, _) in &values {
            self.materialize_cursor(*slot);
        }

        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let buf_ptr = self.stack_buf_ptr(ctx);

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        let mut results = HashMap::new();

        for (index, kind) in values {
            let res = self.builder.ins().load(
                types::I128,
                MemFlagsData::trusted(),
                frame_base,
                (index * std::mem::size_of::<SteelVal>()) as i32,
            );

            if !kind {
                self.clone_value(res);
            } else {
                self.properties.props.insert(
                    ValueOrRegister::Register(index),
                    vec![Properties::InferredType(InferredType::Void)],
                );
            }

            results.insert(index, res);
        }

        results
    }

    // Cache this. In the event we read something twice, we're going
    // to save the lookup, but we can save it per branch so that we
    // don't mess things up.
    /// Seed a cursor for every `List`-seeded parameter. Runs in the entry
    /// block, ahead of the loop header, so the header's phis see the entry
    /// values and each back edge's values. Re-seeding from the slot would only
    /// see the head of a list the loop has walked past.
    fn seed_header_cursors(&mut self) {
        let SpecMode::Specialized { seed, .. } = self.spec_mode.clone() else {
            return;
        };

        // `STEEL_CURSOR_ONLY=name,...` seeds cursors only in these functions,
        // to bisect which one breaks a program or costs time.
        if let Some(only) = &crate::config::config().cursor_only {
            // The closure-lifting gensym number differs run to run, so it is
            // normalised away before matching.
            fn stable(name: &str) -> String {
                match name.find("closure-lifting-") {
                    Some(i) => {
                        let rest = &name[i + "closure-lifting-".len()..];
                        let digits = rest.chars().take_while(|c| c.is_ascii_digit()).count();
                        if digits == 0 {
                            return name.to_string();
                        }
                        format!("{}closure-lifting-N{}", &name[..i], &rest[digits..])
                    }
                    None => name.to_string(),
                }
            }
            let me = stable(&self.name);
            if !only
                .split(',')
                .any(|o| !o.is_empty() && me.contains(stable(o).as_str()))
            {
                return;
            }
        }

        let mut slots: Vec<usize> = seed
            .iter()
            .filter(|(slot, ty)| *ty == SpecType::List && *slot < self.arity as usize)
            .map(|(slot, _)| *slot)
            .collect();
        slots.sort_unstable();

        for slot in slots {
            let cursor = ListCursor {
                cell: self.builder.declare_var(types::I64),
                index: self.builder.declare_var(types::I64),
            };
            self.define_cursor_from_slot(cursor, slot);
            self.header_cursors.insert(slot, cursor);
        }
        if crate::config::config().cursor_dump && !self.header_cursors.is_empty() {
            eprintln!("[cursor] bytecode for {}:", self.name);
            for (i, ins) in self.instructions.iter().enumerate() {
                eprintln!(
                    "    {i:3}  {:?} {}",
                    ins.op_code,
                    ins.payload_size.to_usize()
                );
            }
        }
        if crate::config::config().cursor_trace && !self.header_cursors.is_empty() {
            let mut slots: Vec<_> = self.header_cursors.keys().copied().collect();
            slots.sort_unstable();
            eprintln!("[cursor] seed fn={} slots={:?}", self.name, slots);
        }
    }

    /// Point `cursor` at whatever list is in `slot` right now.
    ///
    /// A raw load, since here the slot is the source of truth and going through
    /// `read_from_vm_stack` would materialise the cursor being defined.
    fn define_cursor_from_slot(&mut self, cursor: ListCursor, slot: usize) {
        let saved = self.properties.cached_lookups.clone();
        let ptr = self.vm_stack_slot_ptr(slot);
        self.properties.cached_lookups = saved;

        let value = self
            .builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), ptr, 0);
        let cell = self.unbox_value_to_pointer(value);
        let index = self.list_cell_index(cell);
        let index = self.builder.ins().uextend(types::I64, index);

        self.builder.def_var(cursor.cell, cell);
        self.builder.def_var(cursor.index, index);
    }

    /// The entry block's cached loads do not dominate a back edge, and the
    /// value stack can have been reallocated by the time one arrives. Start
    /// the header with nothing cached but the cursors themselves.
    fn forget_entry_block_caches(&mut self) {
        self.thread_pointer = None;
        self.thread_id = None;
        // A frameless copy's sp is a parameter, which dominates everything.
        if !self.is_frameless_callee() {
            self.sp = None;
        }
        self.should_trampoline = None;
        self.pop_count = None;
        self.pop_count_plus_one = None;
        self.pop_count_minus_one = None;

        let cursors = self.header_cursors.clone();
        self.properties.cached_lookups = CachedLookupMap::default();
        self.properties.cached_lookups.cursors = cursors;
    }

    /// Store the list `cursor` stands for into `slot`, releasing the head the
    /// slot was holding. The head is released after the store, since the
    /// cursor's cell is reachable from the value just built and dropping the
    /// head first could free it.
    fn emit_cursor_write_back(&mut self, slot: usize, cursor: ListCursor) {
        let cell = self.builder.use_var(cursor.cell);
        let idx = self.builder.use_var(cursor.index);
        // `from_raw_parts` wants the data pointer instead of the box pointer.
        let data = self.builder.ins().iadd_imm_s(cell, list_cell_base());

        // The slot's head is handed over instead of dropped afterwards, so
        // cells only it owns can be reused in place.
        let ptr = self.vm_stack_slot_ptr(slot);
        let old = self
            .builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), ptr, 0);
        let list = self.call_helper(LIST_CURSOR_MATERIALIZE_OWNED, [old, data, idx]);
        // It drops what it does not reuse, and a drop is not on the list of
        // callees known to leave the value stack where it was.
        let ptr = self.vm_stack_slot_ptr(slot);
        self.builder
            .ins()
            .store(MemFlagsData::trusted(), list, ptr, 0);
    }

    /// Write every given cursor back to its slot without touching the
    /// translator's state, for an exit, which the hot path does not continue
    /// from. Caches are restored so nothing loaded here leaks into code the
    /// exit block does not dominate.
    fn write_back_cursors(&mut self, cursors: &HashMap<usize, ListCursor>) {
        let saved = self.properties.cached_lookups.clone();
        let mut live: Vec<(usize, ListCursor)> = cursors.iter().map(|(k, v)| (*k, *v)).collect();
        live.sort_unstable_by_key(|(k, _)| *k);
        for (slot, cursor) in live {
            self.emit_cursor_write_back(slot, cursor);
        }
        self.properties.cached_lookups = saved;
    }

    /// Put a register that a cursor has been walking back into its slot, and
    /// stop tracking the cursor. From here on the slot is the source of truth.
    ///
    /// Everything that reads a slot goes through here first, so the rest of
    /// the translator only ever sees an ordinary list.
    fn materialize_cursor(&mut self, index: usize) {
        let Some(cursor) = self.properties.cached_lookups.cursors.get(&index).copied() else {
            return;
        };

        self.emit_cursor_write_back(index, cursor);
        self.properties.cached_lookups.cursors.remove(&index);
        self.properties.cached_lookups.registers.remove(&index);
    }

    /// Whether straight-line code from `ip` does nothing but return. No slot
    /// is read, and nothing can deopt and resume reading one.
    fn only_returns_from(&self, mut ip: usize) -> bool {
        for _ in 0..16 {
            let Some(ins) = self.instructions.get(ip) else {
                return false;
            };
            match ins.op_code {
                OpCode::POPPURE => return true,
                OpCode::LETENDSCOPE => ip += 1,
                OpCode::JMP | OpCode::POPJMP if ins.payload_size.to_usize() > ip => {
                    ip = ins.payload_size.to_usize()
                }
                _ => return false,
            }
        }
        false
    }

    fn op_may_resume_interpreted(&self, op: OpCode, payload: usize) -> bool {
        match op {
            OpCode::CALLPRIMITIVE | OpCode::CALLPRIMITIVETAIL => {
                !matches!(self._globals.get(payload), Some(SteelVal::FuncV(_)))
            }
            OpCode::FUNC
            | OpCode::FUNCNOARITY
            | OpCode::TAILCALL
            | OpCode::TAILCALLNOARITY
            | OpCode::TCOJMP
            | OpCode::CALLGLOBAL
            | OpCode::CALLGLOBALTAIL
            | OpCode::CALLGLOBALNOARITY
            | OpCode::CALLGLOBALTAILNOARITY
            | OpCode::READLOCAL0CALLGLOBAL
            | OpCode::READLOCAL1CALLGLOBAL
            | OpCode::UNBOXCALL
            | OpCode::UNBOXTAIL => true,
            _ => false,
        }
    }

    fn materialize_all_cursors(&mut self) {
        let mut slots: Vec<usize> = self
            .properties
            .cached_lookups
            .cursors
            .keys()
            .copied()
            .collect();
        slots.sort_unstable();
        for slot in slots {
            self.materialize_cursor(slot);
        }
    }

    /// One `cdr`, as arithmetic.
    ///
    /// If there is still room in this cell, step the cursor down. Otherwise
    /// follow the link to the next cell and start at its own position. Off the
    /// end, the index goes to 0, which means the empty list here, matching
    /// `list_cell_index`, where 0 is what `null?` tests for.
    ///
    /// No cell is allocated on any of these paths. `im-lists` keeps the cursor
    /// inside the cell, so a materialised `cdr` has to copy the cell to give
    /// the copy its own cursor.
    fn cursor_advance(&mut self, cursor: ListCursor) {
        self.cursor_advances += 1;
        let index = self.builder.use_var(cursor.index);
        let more_here = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::SignedGreaterThan, index, 1);

        let dec_block = self.builder.create_block();
        let step_block = self.builder.create_block();
        let take_block = self.builder.create_block();
        let end_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(more_here, dec_block, &[], step_block, &[]);
        self.builder.seal_block(dec_block);
        self.builder.seal_block(step_block);

        // Room left in this cell.
        self.builder.switch_to_block(dec_block);
        let decremented = self.builder.ins().iadd_imm_s(index, -1);
        self.builder.def_var(cursor.index, decremented);
        self.builder.ins().jump(merge_block, &[]);

        // Cell exhausted. Is there another?
        self.builder.switch_to_block(step_block);
        let cell = self.builder.use_var(cursor.cell);
        let link = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            cell,
            list_next_offset(),
        );
        let next = self.builder.ins().band_imm_u(link, !list_next_tag_mask());
        let has_next = self.builder.ins().icmp_imm_s(IntCC::NotEqual, next, 0);
        self.builder
            .ins()
            .brif(has_next, take_block, &[], end_block, &[]);
        self.builder.seal_block(take_block);
        self.builder.seal_block(end_block);

        self.builder.switch_to_block(take_block);
        // The link holds a data pointer; the cursor keeps the box pointer.
        let next = self.builder.ins().iadd_imm_s(next, -list_cell_base());
        let next_index = self.list_cell_index(next);
        let next_index = self.builder.ins().uextend(types::I64, next_index);
        self.builder.def_var(cursor.cell, next);
        self.builder.def_var(cursor.index, next_index);
        self.builder.ins().jump(merge_block, &[]);

        // Walked off the end, so this is the empty list.
        self.builder.switch_to_block(end_block);
        let zero = self.i64_const(0);
        self.builder.def_var(cursor.index, zero);
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }

    /// Address of a frame slot, for a read or a write.
    fn vm_stack_slot_ptr(&mut self, index: usize) -> Value {
        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let buf_ptr = self.stack_buf_ptr(ctx);

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);
        self.builder
            .ins()
            .iadd_imm_s(frame_base, (index * std::mem::size_of::<SteelVal>()) as i64)
    }

    fn read_from_vm_stack(&mut self, index: usize) -> Value {
        // A cursor-backed register has to become a real list before anyone
        // reads its slot.
        self.materialize_cursor(index);

        // Cache let vars since they're going to be on the stack,
        // but we already had it available.
        if let Some(local) = self.properties.cached_lookups.registers.get(&index) {
            log::debug!(target: "letvar", "HIT  slot {index}");
            let local = *local;
            self.mark_register_read(index, local);
            return local;
        }
        log::debug!(target: "letvar", "MISS slot {index}");
        let cache_this_read = crate::config::config().jit_letvar_cache;

        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let buf_ptr = self.stack_buf_ptr(ctx);

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        let res = self.builder.ins().load(
            types::I128,
            MemFlagsData::trusted(),
            frame_base,
            (index * std::mem::size_of::<SteelVal>()) as i32,
        );

        // Read-through. The index came from the bytecode, so unlike the one
        // `LetVar` computes it is always the real stack slot. Later reads of the
        // same slot reuse this load until something invalidates it.
        if cache_this_read {
            self.properties.cached_lookups.registers.insert(index, res);
        }

        self.mark_register_read(index, res);

        res
    }

    // Read the tag and payload from the vm stack separately; this is helpful
    // in the event that we are then splitting on the value from the stack itself.
    fn read_from_vm_stack_split(&mut self, index: usize) -> (Value, Value) {
        self.materialize_cursor(index);
        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let buf_ptr = self.stack_buf_ptr(ctx);

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        let tag = self.builder.ins().load(
            types::I8,
            MemFlagsData::trusted(),
            frame_base,
            (index * std::mem::size_of::<SteelVal>()) as i32,
        );

        let value = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            frame_base,
            (index * std::mem::size_of::<SteelVal>() + 8) as i32,
        );

        let tag = match self.register_tag(index) {
            Some(known) => self.builder.ins().iconst(types::I8, known as i64),
            None => tag,
        };

        (tag, value)
    }

    // Read with an additional 8 offset
    fn read_from_vm_stack_unboxed(&mut self, index: usize) -> Value {
        self.materialize_cursor(index);
        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let buf_ptr = self.stack_buf_ptr(ctx);

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            frame_base,
            (index * std::mem::size_of::<SteelVal>()) as i32 + 8,
        )
    }

    fn remove_from_vm_stack(&mut self, index: usize) -> Value {
        // A move out of the slot has to move the current value.
        self.materialize_cursor(index);
        self.remove_from_vm_stack_raw(index)
    }

    /// Move the slot's value out, leaving void, without looking at a cursor.
    /// For a cursor that moves with the value, since the head owns the cells it
    /// walks.
    fn remove_from_vm_stack_raw(&mut self, index: usize) -> Value {
        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);

        let thread_pointer = self.get_thread_pointer(ctx);

        let buf_ptr = self.load_field(
            Type::int(64).unwrap(),
            thread_pointer,
            (offset_of!(SteelThread, stack) + steel_vec::Vec::<SteelVal>::buf_offset()),
        );

        debug_assert_eq!(std::mem::size_of::<SteelVal>(), 16);

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        let local_value = self.builder.ins().load(
            types::I128,
            MemFlagsData::trusted(),
            frame_base,
            (index * std::mem::size_of::<SteelVal>()) as i32,
        );

        let value = self.encode_void();

        self.builder.ins().store(
            MemFlagsData::trusted(),
            value,
            frame_base,
            (index * std::mem::size_of::<SteelVal>()) as i32,
        );

        self.mark_register_read(index, local_value);

        local_value
    }

    // TODO: Should flatten these ops when calling this multiple times!
    fn write_to_vm_stack(&mut self, index: usize, value: Value) {
        // The slot still owns the head the cursor walked from, which the drop
        // below releases; the new value replaces the cursor outright.
        self.properties.cached_lookups.cursors.remove(&index);
        self.properties
            .cached_lookups
            .registers
            .insert(index, value);

        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        let local_offset = self.builder.ins().iadd_imm_s(sp, index as i64);
        let offset = self.builder.ins().imul_imm_s(local_offset, size);

        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let local_value =
            self.builder
                .ins()
                .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

        self.drop_tagged_value(local_value);

        self.builder
            .ins()
            .store(MemFlagsData::trusted(), value, slot_ptr, 0);
    }

    fn write_to_vm_stack_starting_at_lifted(
        &mut self,
        index: usize,
        values: &[Value],
        should_drop: bool,
        sp: Value,
        buf_ptr: Value,
    ) {
        if values.is_empty() {
            return;
        }

        let mut index = index;

        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        for value in values {
            // Carried as a cursor, so the slot already holds the value's head.
            if self.carry_skip.contains(&index) {
                index += 1;
                continue;
            }

            // A fixnum has nothing to drop.
            if should_drop && !self.register_is_immediate(index) {
                match self.properties.get(&ValueOrRegister::Register(index)) {
                    // TODO: Can do even better, read less things
                    Some(
                        Properties::NonEmptyListOrPair
                        | Properties::ProperNonEmptyList
                        | Properties::ProperList,
                    ) => {
                        let local_value = self.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            frame_base,
                            (index * std::mem::size_of::<SteelVal>()) as i32,
                        );

                        self.drop_biased_rc(local_value);
                    }

                    Some(Properties::InferredType(InferredType::Void)) => {}

                    _ => {
                        let local_value = self.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            frame_base,
                            (index * std::mem::size_of::<SteelVal>()) as i32,
                        );

                        self.drop_tagged_value(local_value);
                    }
                }
            }

            self.builder.ins().store(
                MemFlagsData::trusted(),
                *value,
                frame_base,
                (index * std::mem::size_of::<SteelVal>()) as i32,
            );

            index += 1;
        }
    }

    fn drop_from_vm_stack_starting_at(
        &mut self,
        index: usize,
        amt: usize,
        sp: Value,
        buf_ptr: Value,
    ) {
        if amt == 0 {
            return;
        }

        let mut index = index;
        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        for _ in 0..amt {
            if !self.register_is_immediate(index) {
                match self.properties.get(&ValueOrRegister::Register(index)) {
                    // TODO: Can do even better, read less thing
                    Some(
                        Properties::NonEmptyListOrPair
                        | Properties::ProperNonEmptyList
                        | Properties::ProperList,
                    ) => {
                        let local_value = self.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            frame_base,
                            (index * std::mem::size_of::<SteelVal>()) as i32,
                        );

                        self.drop_biased_rc(local_value);
                    }

                    Some(Properties::InferredType(InferredType::Void)) => {}

                    _ => {
                        let local_value = self.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            frame_base,
                            (index * std::mem::size_of::<SteelVal>()) as i32,
                        );

                        self.drop_tagged_value(local_value);
                    }
                }
            }

            index += 1;
        }
    }

    fn write_to_vm_stack_starting_at(&mut self, index: usize, values: &[Value], should_drop: bool) {
        if values.is_empty() {
            return;
        }

        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);
        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let mut index = index;
        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        for value in values {
            // Carried as a cursor, so the slot already holds the value's head.
            if self.carry_skip.contains(&index) {
                index += 1;
                continue;
            }

            // A fixnum has nothing to drop.
            if should_drop && !self.register_is_immediate(index) {
                match self.properties.get(&ValueOrRegister::Register(index)) {
                    // TODO: Can do even better, read less things
                    Some(
                        Properties::NonEmptyListOrPair
                        | Properties::ProperNonEmptyList
                        | Properties::ProperList,
                    ) => {
                        let local_value = self.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            frame_base,
                            (index * std::mem::size_of::<SteelVal>()) as i32,
                        );

                        self.drop_biased_rc(local_value);
                    }

                    Some(Properties::InferredType(InferredType::Void)) => {}

                    _ => {
                        let local_value = self.builder.ins().load(
                            types::I128,
                            MemFlagsData::trusted(),
                            frame_base,
                            (index * std::mem::size_of::<SteelVal>()) as i32,
                        );

                        self.drop_tagged_value(local_value);
                    }
                }
            }

            self.builder.ins().store(
                MemFlagsData::trusted(),
                *value,
                frame_base,
                (index * std::mem::size_of::<SteelVal>()) as i32,
            );

            index += 1;
        }
    }

    /// Whether a closure only known at run time can take `arity` arguments
    /// through a frame this code sets up itself. It needs fixed arity and that
    /// many parameters. A variadic callee needs its rest list built by
    /// `adjust_stack_for_multi_arity`, which only the interpreter-side paths
    /// call. Anything else goes to the helper, which adjusts or raises
    /// the arity error. The generated frame setup checks neither, so a
    /// mismatch would read garbage slots.
    fn closure_accepts_arity(&mut self, closure: Value, arity: usize) -> Value {
        let multi = self.builder.ins().load(
            types::I8,
            MemFlagsData::trusted().with_readonly(),
            closure,
            closure_field_offset(offset_of!(ByteCodeLambda, is_multi_arity)),
        );
        let fixed = self.builder.ins().icmp_imm_s(IntCC::Equal, multi, 0);

        let declared = self.builder.ins().load(
            types::I16,
            MemFlagsData::trusted().with_readonly(),
            closure,
            closure_field_offset(offset_of!(ByteCodeLambda, arity)),
        );
        let matches = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::Equal, declared, arity as i64);

        self.builder.ins().band(fixed, matches)
    }

    fn check_should_trampoline(&mut self, ctx: Value) -> Value {
        if let Some(should_trampoline) = self.should_trampoline {
            return should_trampoline;
        }

        let thread_pointer = self.get_thread_pointer(ctx);

        // Is there room on the native stack for another jitted frame? The limit
        // comes from the running thread's real stack bounds, so a 2MB spawned
        // thread and an 8MB main thread each get their own answer.
        if native_stack_check_enabled() {
            let limit = self.load_field(
                Type::int(64).unwrap(),
                thread_pointer,
                offset_of!(SteelThread, native_stack_limit),
            );

            let sp = self.builder.ins().get_stack_pointer(Type::int(64).unwrap());

            // Stack grows down, so staying native means staying above it.
            return self
                .builder
                .ins()
                .icmp(IntCC::UnsignedGreaterThan, sp, limit);
        }

        let stack_frame_length = self.stack_frames_len(thread_pointer);

        let res = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::UnsignedLessThan, stack_frame_length, 100);

        res
    }

    /// `rest` is `(surplus, declared arity)` when the callee is variadic. The
    /// fast path collects that many trailing arguments into the rest list and
    /// then has `declared arity` slots, which is the frame it builds. The
    /// fallback hands the arguments over untouched, since
    /// `#%setup-closure-arity` does its own adjusting.
    fn inline_call_global_function_to(
        &mut self,
        id: u32,
        func: Value,
        fallback_ip: usize,
        args: &[Value],
        instr_fat_ptr: Value,
        callee_is_self: bool,
        target: Option<FuncId>,
        rest: Option<(usize, usize)>,
    ) -> Value {
        let vm_ctx = self.get_ctx();
        let should_trampoline = self.check_should_trampoline(vm_ctx);

        let should_yield = self.builder.ins().bxor_imm_u(should_trampoline, 1);

        self.update_ip_native_if_yield(vm_ctx, should_yield, fallback_ip + 1);

        let typ = self.int;
        let arity = args.len();

        // stack_length_capacity is a compile time model of a runtime quantity and it
        // can be wrong. An unchecked push would then write past the end of the
        // stack buffer and corrupt the allocator, so always take the check.
        self.push_to_many_vm_stack_let_var_new(&args);

        // The checked push reserves only what it needs, so there is no
        // headroom left over.
        self.properties.cached_lookups.stack_length_capacity = 0;

        self.converging_if(
            should_trampoline,
            |ctx| {
                // Increment the ref count before we go in
                ctx.increment_ref_count_closure(func);

                let frame_arity = match rest {
                    Some((surplus, declared)) => {
                        let count = ctx.i64_const(surplus as i64);
                        ctx.call_helper_void(COLLECT_REST_ARGS, [count]);
                        declared
                    }
                    None => arity,
                };

                // TODO: Consider moving this up before things are pushed on in order
                // to capture the stack length without needing to compute the value
                ctx.push_stack_frame(
                    frame_arity as _,
                    func,
                    instr_fat_ptr,
                    fallback_ip,
                    callee_is_self,
                );

                // TODO: Change the calling convention of the function to return a
                // (tag, Value) rather than an i128 value directly.
                let jit_func = match target {
                    Some(target) => ctx.module.declare_func_in_func(target, ctx.builder.func),
                    None => ctx.get_jit_func(id),
                };

                // Check the result here
                let call = ctx.builder.ins().call(jit_func, &[vm_ctx]);

                // No `is_native` test here. A callee that deopts returns void
                // from every exit, so the result is already void in that case,
                // and every caller runs `check_deopt` next.
                ctx.builder.inst_results(call)[0]
            },
            |ctx| {
                let arity_val = ctx.i64_const(arity as i64);
                let fallback_ip_val = ctx.i64_const(fallback_ip as i64);
                // Part of the call protocol. An arity error is picked up after the merge.
                ctx.call_helper_unchecked(SETUP_CLOSURE_ARITY, [func, arity_val, fallback_ip_val])
            },
            typ,
        )
    }

    fn inline_let_end_scope(&mut self, amt: usize, count: usize) {
        let vm_ctx = self.get_ctx();
        let offset = self.get_sp(vm_ctx);

        let rollback_index = self.builder.ins().iadd_imm_s(offset, amt as i64);

        // We can guarantee that there is an additional amt capacity
        // since we've truncated the stack back by that much.
        self.properties.cached_lookups.stack_length_capacity += amt;

        self.truncate_stack(vm_ctx, rollback_index, Some(count as _));
    }

    fn truncate_stack_with_args(&mut self, vm_ctx: Value, index: Value, args: &[Value]) {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let capacity_offset = steel_vec::Vec::<SteelVal>::capacity_offset();
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // We're going to check the capacity before we do anything
        let stack_capacity = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + capacity_offset) as i32,
        );

        // Read before the store below overwrites it. Everything past the
        // arguments is the frame's own let bound locals and any operands spilled
        // under them, which the tail call discards and so has to release.
        let old_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // This is the spot:
        // The new length will be this, so we just have to check the capacity of this
        // if we're going to re alloc
        let new_length = self.builder.ins().iadd_imm_s(index, args.len() as i64);

        // Passing more arguments than this function takes writes the extras
        // over slots past its own arguments, which can still hold its let bound
        // locals and spilled operands. The writes below don't release those, so
        // release them first. Whatever lies past the new arguments is released
        // at the end.
        let live_above = self.let_var_stack.iter().sum::<usize>() + self.shadow_stack.len();
        if args.len() > self.arity as usize
            && live_above > 0
            && !(self.arity as usize..args.len()).all(|slot| self.register_is_immediate(slot))
        {
            let from = self.builder.ins().iadd_imm_s(index, self.arity as i64);
            let to = self.builder.ins().smin(old_length, new_length);
            self.drop_vm_stack_range(vm_ctx, from, to);
        }

        // Store the _new_ arity
        self.builder.ins().store(
            MemFlagsData::trusted(),
            new_length,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // We're calling a function which has _more_ args
        // than we currently have. Then we need to check the capacity.

        if args.len() == self.arity as usize {
            // Write to the vm stack
            self.write_to_vm_stack_starting_at(0, &args, true);
        } else if args.len() > self.arity as usize {
            // Grows by more than one slot, so we have to check that the new length
            // still fits, instead of checking that it lands on the capacity. Same
            // comparison push_to_many_vm_stack_let_var_new uses.
            let needs_more_capacity =
                self.builder
                    .ins()
                    .icmp(IntCC::UnsignedGreaterThan, new_length, stack_capacity);

            self.converging_if_no_else_no_value_cold(
                needs_more_capacity,
                |ctx| {
                    let amt = args.len() - ctx.arity as usize;
                    let amt = ctx.i64_const(amt as i64);
                    ctx.call_helper_void(SLOW_STACK_RESERVE_EXACT, [amt]);
                },
                |ctx| {
                    // Drop this many:
                    let amount_to_drop = ctx.arity as usize;
                    let sp = ctx.get_sp(vm_ctx);
                    let thread_pointer = ctx.get_thread_pointer(vm_ctx);

                    let stack_offset = offset_of!(SteelThread, stack);

                    let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

                    let buf_ptr = ctx.builder.ins().load(
                        Type::int(64).unwrap(),
                        MemFlagsData::trusted(),
                        thread_pointer,
                        (stack_offset + ptr_offset) as i32,
                    );

                    // Write the first set:
                    ctx.write_to_vm_stack_starting_at_lifted(
                        0,
                        &args[..amount_to_drop],
                        true,
                        sp,
                        buf_ptr,
                    );
                    ctx.write_to_vm_stack_starting_at_lifted(
                        amount_to_drop,
                        &args[amount_to_drop..],
                        false,
                        sp,
                        buf_ptr,
                    );
                },
            );
        }
        // Then we're at args.len() < self.arity, then we don't need to check the
        // capacity. We're always going to have a smaller final length than that,
        // so we can just write the args in place. We can first truncate, and then
        // start writing.
        else {
            let amount_to_drop = self.arity as usize - args.len();

            let sp = self.get_sp(vm_ctx);
            let thread_pointer = self.get_thread_pointer(vm_ctx);

            let stack_offset = offset_of!(SteelThread, stack);

            let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

            let buf_ptr = self.builder.ins().load(
                Type::int(64).unwrap(),
                MemFlagsData::trusted(),
                thread_pointer,
                (stack_offset + ptr_offset) as i32,
            );

            // Write the first set:
            self.write_to_vm_stack_starting_at_lifted(0, &args, true, sp, buf_ptr);

            // Just call drop on the remaining values, don't write.
            self.drop_from_vm_stack_starting_at(args.len(), amount_to_drop, sp, buf_ptr);
        }

        // Each branch above releases only the slots it writes over, up to the
        // larger of the old and new argument counts. Past that sit the frame's let
        // bound locals and spilled operands, and shortening the length alone
        // would discard them while they still hold their references, leaking
        // them. Starting above what the branches already dropped keeps this from
        // releasing anything twice.
        //
        // The loop runs on every jitted tail call, so skip it when this frame has
        // nothing above the arguments (no let bindings open and no operands
        // spilled under them), which is the common case.

        // Nor is there anything to release when every slot above the arguments
        // holds an immediate, like a fixnum or the void a move read left behind.
        let base_slot = args.len().max(self.arity as usize);
        let all_immediate = (0..live_above).all(|k| self.register_is_immediate(base_slot + k));

        // Unrolling this into `arity + live_above - base_slot` drops at fixed
        // offsets doesn't help, since the cost is the refcount and free work
        // itself.
        if live_above > 0 && !all_immediate {
            let drop_from = self.builder.ins().iadd_imm_s(index, base_slot as i64);
            self.drop_vm_stack_range(vm_ctx, drop_from, old_length);
        }
    }

    /// Release every stack slot in `[from, to)`. Both are runtime values.
    fn drop_vm_stack_range(&mut self, vm_ctx: Value, from: Value, to: Value) {
        let thread_pointer = self.get_thread_pointer(vm_ctx);
        let stack_offset = offset_of!(SteelThread, stack);
        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let loop_header = self.builder.create_block();
        let loop_body = self.builder.create_block();
        let loop_exit = self.builder.create_block();

        self.builder.append_block_param(loop_header, types::I64);
        let start = BlockArg::Value(from);
        self.builder.ins().jump(loop_header, &[start]);

        self.builder.switch_to_block(loop_header);
        let i = self.builder.block_params(loop_header)[0];
        let done = self
            .builder
            .ins()
            .icmp(IntCC::SignedGreaterThanOrEqual, i, to);
        self.builder
            .ins()
            .brif(done, loop_exit, &[], loop_body, &[]);

        self.builder.switch_to_block(loop_body);
        self.builder.seal_block(loop_body);

        let byte_offset = self
            .builder
            .ins()
            .imul_imm_s(i, size_of::<SteelVal>() as i64);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, byte_offset);
        let val = self
            .builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);
        self.drop_tagged_value(val);

        let i_next = BlockArg::Value(self.builder.ins().iadd_imm_s(i, 1));
        self.builder.ins().jump(loop_header, &[i_next]);
        self.builder.seal_block(loop_header);
        self.builder.switch_to_block(loop_exit);
        self.builder.seal_block(loop_exit);
    }

    fn truncate_stack(&mut self, vm_ctx: Value, index: Value, count: Option<i32>) {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // Current stack length:
        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        self.builder.ins().store(
            MemFlagsData::trusted(),
            index,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let count = count
            .map(|count| self.i64_const(count as i64))
            .unwrap_or_else(|| self.builder.ins().isub(stack_length, index));
        {
            let loop_header = self.builder.create_block();
            let loop_body = self.builder.create_block();
            let loop_exit = self.builder.create_block();

            self.builder.append_block_param(loop_header, types::I64);

            let zero = BlockArg::Value(self.i64_const(0));
            self.builder.ins().jump(loop_header, &[zero]);

            self.builder.switch_to_block(loop_header);
            let i = self.builder.block_params(loop_header)[0];

            let done = self
                .builder
                .ins()
                .icmp(IntCC::SignedGreaterThanOrEqual, i, count);
            self.builder
                .ins()
                .brif(done, loop_exit, &[], loop_body, &[]);

            self.builder.switch_to_block(loop_body);
            self.builder.seal_block(loop_body);

            let slot_index = self.builder.ins().iadd(index, i);
            let byte_offset = self
                .builder
                .ins()
                .imul_imm_s(slot_index, size_of::<SteelVal>() as i64);
            let slot_ptr = self.builder.ins().iadd(buf_ptr, byte_offset);
            let val = self
                .builder
                .ins()
                .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

            self.drop_tagged_value(val);

            let i_next = BlockArg::Value(self.builder.ins().iadd_imm_s(i, 1));
            self.builder.ins().jump(loop_header, &[i_next]);

            self.builder.seal_block(loop_header);

            self.builder.switch_to_block(loop_exit);
            self.builder.seal_block(loop_exit);
        }
    }

    fn invalidate_buf_ptr(&mut self) {
        if self.properties.cached_lookups.stack_buf_pointer.is_some() {
            log::debug!(target: "bufptr", "INVALIDATE");
        }
        self.properties.cached_lookups.stack_buf_pointer.take();
    }

    /// The value stack's buffer pointer, reused within a block.
    ///
    /// Invalidated by `invalidate_buf_ptr` at the jit's own growth points, by
    /// every call (rust can grow the stack out from under us), and on a control
    /// flow join where the two sides disagree (see `PropertyMap::meet`).
    fn stack_buf_ptr(&mut self, vm_ctx: Value) -> Value {
        if let Some(buf_pointer) = self.properties.cached_lookups.stack_buf_pointer {
            log::debug!(target: "bufptr", "HIT");
            return buf_pointer;
        }
        log::debug!(target: "bufptr", "MISS");

        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let buf_ptr = self.load_field(
            Type::int(64).unwrap(),
            thread_pointer,
            (offset_of!(SteelThread, stack) + steel_vec::Vec::<SteelVal>::buf_offset()),
        );

        if stack_buf_cache_enabled() {
            self.properties.cached_lookups.stack_buf_pointer = Some(buf_ptr);
        }

        buf_ptr
    }

    // 1. Probably need to check the length, slow path otherwise
    // 2. Should figure out a better way of doing things.
    fn inline_lookup_global(&mut self, payload: usize) -> Value {
        let vm_ctx = self.get_ctx();
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Env pointer
        let env_pointer = self.load_field(
            types::I64,
            thread_pointer,
            offset_of!(SteelThread, global_env),
        );

        let smaller: u32 = payload.try_into().unwrap();

        let index = self.builder.ins().iconst(types::I32, smaller as i64);

        // Amount we're going to shift by
        let header_size = SteelList::<SteelVal>::vector_header_size();

        // Length, stored at the start, after the capacity
        let vector_length =
            self.builder
                .ins()
                .load(types::I32, MemFlagsData::trusted(), env_pointer, 4);

        // if the index > the length, return void
        let test = self
            .builder
            .ins()
            .icmp(IntCC::SignedGreaterThan, index, vector_length);

        self.converging_if(
            test,
            |ctx| ctx.encode_void(),
            |ctx| {
                let index = ctx.builder.ins().uextend(types::I64, index);
                let shift_left = ctx.builder.ins().ishl_imm_u(index, 4);
                let index = ctx.builder.ins().iadd_imm_s(shift_left, header_size as i64);
                let value = ctx.builder.ins().iadd(env_pointer, index);
                ctx.builder
                    .ins()
                    .load(types::I128, MemFlagsData::trusted(), value, 0)
            },
            types::I128,
        )
    }

    fn inline_pop_from_stack(&mut self, vm_ctx: Value) -> Value {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // Stack length can also be cached per branch.
        //
        // Any push will increase the length, and pop
        // will decrease the length.
        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let new_length = self.builder.ins().iadd_imm_s(stack_length, -1);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            new_length,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        let offset = self.builder.ins().imul_imm_s(new_length, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let res = self
            .builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

        res
    }

    // Subtract one from the pop count, returns
    // the new pop count
    fn sub_one_pop_count(&mut self, vm_ctx: Value) -> Value {
        let sub_one = self.pop_count_sub_one(vm_ctx);

        self.store_field(sub_one, vm_ctx, offset_of!(VmCore, pop_count));
        sub_one
    }

    fn inline_handle_pop(&mut self, value: Value) -> Value {
        self.inline_handle_pop_with(value, false)
    }

    /// Every argument and let slot of this frame is known to hold an immediate,
    /// and nothing else is on the value stack above them, so the return can
    /// drop the frame's slots with a length store. The generic truncate loads
    /// and tests each slot's tag first.
    fn frame_slots_all_immediate(&self) -> bool {
        if !self.shadow_stack.is_empty() {
            return false;
        }
        let lets: usize = self.let_var_stack.iter().sum();
        (0..self.arity as usize + lets).all(|r| self.register_is_immediate(r))
    }

    fn inline_handle_pop_with(&mut self, value: Value, slots_immediate: bool) -> Value {
        // A frameless copy has no frame to pop and nothing in the VM to
        // restore, so drop its own slots and hand the value back. `pop_count`
        // never counted it.
        if self.is_frameless_callee() {
            let vm_ctx = self.get_ctx();
            let sp = self.get_sp(vm_ctx);
            if slots_immediate {
                self.truncate_stack_no_drop(vm_ctx, sp);
            } else {
                self.truncate_stack(vm_ctx, sp, None);
            }
            return value;
        }

        let vm_ctx = self.get_ctx();
        let new_pop_count = self.sub_one_pop_count(vm_ctx);
        let should_continue = self.pop_count_should_continue(new_pop_count);

        // TODO: Figure out what the return type is! We don't really need
        // to return anything. The result should just be assigned to the result
        // on the VM context; In the else case, we assign the result
        // on to the VM value. In the former case, we don't need to do anything.
        self.converging_if(
            should_continue,
            |ctx| {
                // This is the frame itself. We'll need to call drop
                // on it properly -> specifically on the function
                // and possibly the attachments, depending on whether
                // its null or not. If it is, we'll want to pass the frame
                // by value in, call drop on it, and close the things as needed
                // in order for drop to get called.
                let (popped_frame, _) = ctx.inline_pop_from_stack_frames(vm_ctx);

                // first, we'll check if the attachments isn't null.
                // And then, we'll invoke drop on the frame for the attachments.

                let attachment_exists =
                    ctx.builder
                        .ins()
                        .icmp_imm_s(IntCC::NotEqual, popped_frame.attachments, 0);

                ctx.converging_if_no_else_no_value_cold(
                    attachment_exists,
                    |ctx| {
                        // If the attachments exist, then we need to:
                        // 1. Call the rust function to close the frame
                        // 2. Close the destructor.
                        ctx.call_helper_void(HANDLE_ATTACHMENTS, [popped_frame.attachments]);
                    },
                    |ctx| {
                        // The rest:
                        // Truncate the stack, and then push the new value one.
                        //
                        // Then, we restore the ip, instructions, and sp
                        // on to the requisite spots on the VM context

                        let sp = ctx.builder.ins().uextend(types::I64, popped_frame.sp);

                        // TODO: Keep track of the new length, pass that in
                        // to push to vm stack in order to simply just write to
                        // the new location; or alternatively, fuse these operations
                        // together, where I can just truncate and then immediately
                        // write this value to the end, eliminate a store of the length
                        // changing twice.
                        if slots_immediate {
                            ctx.truncate_stack_no_drop(vm_ctx, sp);
                        } else {
                            ctx.truncate_stack(vm_ctx, sp, None);
                        }

                        let ip = ctx.builder.ins().uextend(types::I64, popped_frame.ip);
                        ctx.store_field(ip, vm_ctx, offset_of!(VmCore, ip));

                        ctx.store_field(
                            popped_frame.instructions,
                            vm_ctx,
                            offset_of!(VmCore, instructions),
                        );

                        let sp = ctx.read_last_sp(vm_ctx);

                        // TODO: @Matt
                        // Call drop on the function here!
                        // We don't need to totally encode the function here like this
                        ctx.drop_biased_rc_unboxed_closure(popped_frame.function);

                        ctx.store_field(sp, vm_ctx, offset_of!(VmCore, sp));
                    },
                );

                value
            },
            |ctx| {
                ctx.call_helper_void(POP_SLOW_PATH_FINISH, [value]);

                // Return void in this case
                ctx.encode_void()
            },
            types::I128,
        )
    }

    // Whether we should continue running:
    fn pop_count_should_continue(&mut self, pop_count: Value) -> Value {
        self.builder.ins().icmp_imm_s(IntCC::NotEqual, pop_count, 0)
    }

    fn update_last_stackframe(&mut self, vm_ctx: Value, function: Value) -> Value {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let stack_length = self.stack_frames_len(thread_pointer);
        let last = self.builder.ins().iadd_imm_s(stack_length, -1);
        let buf_ptr = self.stack_frames_buf(thread_pointer);
        let slot_ptr = self.stack_frame_at(buf_ptr, last);

        let value = self.load_field(types::I32, slot_ptr, offset_of!(StackFrame, sp));

        let old_function = self.load_field(types::I64, slot_ptr, offset_of!(StackFrame, function));

        self.drop_biased_rc_unboxed_closure(old_function);

        self.store_field(function, slot_ptr, offset_of!(StackFrame, function));

        self.builder.ins().uextend(types::I64, value)
    }

    // TODO:
    // Extend by a certain amount - we probably can
    // make this better by passing in the length from
    // the previous read so we don't have to read all
    // this stuff. Its already read from before.
    fn read_last_sp(&mut self, vm_ctx: Value) -> Value {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let stack_length = self.stack_frames_len(thread_pointer);

        let condition = self.builder.ins().icmp_imm_s(IntCC::Equal, stack_length, 0);

        self.converging_if(
            condition,
            |ctx| ctx.i64_const(0),
            |ctx| {
                let last = ctx.builder.ins().iadd_imm_s(stack_length, -1);
                let buf_ptr = ctx.stack_frames_buf(thread_pointer);
                let slot_ptr = ctx.stack_frame_at(buf_ptr, last);

                let value = ctx.load_field(types::I32, slot_ptr, offset_of!(StackFrame, sp));

                ctx.builder.ins().uextend(types::I64, value)
            },
            types::I64,
        )
    }

    fn pop_count_sub_one(&mut self, vm_ctx: Value) -> Value {
        let current_pop_count = self.get_pop_count(vm_ctx);
        let sub_one = self.builder.ins().iadd_imm_s(current_pop_count, -1);
        self.pop_count_minus_one = Some(sub_one);
        sub_one
    }

    fn pop_count_add_one(&mut self, vm_ctx: Value) -> Value {
        let current_pop_count = self.get_pop_count(vm_ctx);
        let plus_one = self.builder.ins().iadd_imm_s(current_pop_count, 1);
        self.pop_count_plus_one = Some(plus_one);
        plus_one
    }

    fn get_pop_count(&mut self, vm_ctx: Value) -> Value {
        if let Some(pop) = self.pop_count {
            pop
        } else {
            let old_pop_count = self.load_field(types::I64, vm_ctx, offset_of!(VmCore, pop_count));

            old_pop_count
        }
    }

    // TODO: Cache these at the start? In the function prelude?
    fn get_thread_pointer(&mut self, vm_ctx: Value) -> Value {
        if let Some(thread_pointer) = self.thread_pointer {
            thread_pointer
        } else {
            let tp = self.load_field(Type::int(64).unwrap(), vm_ctx, offset_of!(VmCore, thread));

            self.thread_pointer = Some(tp);

            tp
        }
    }

    // TODO: Cache this at the beginning, since it won't change
    // during the duration of a function call?
    fn get_sp(&mut self, vm_ctx: Value) -> Value {
        if let Some(sp) = self.sp {
            sp
        } else {
            let sp = self.load_field(Type::int(64).unwrap(), vm_ctx, offset_of!(VmCore, sp));

            sp
        }
    }

    /// Pop the last stack frame off
    fn inline_pop_from_stack_frames(&mut self, vm_ctx: Value) -> (StackFrameRepr, (Value, Value)) {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let stack_length = self.stack_frames_len(thread_pointer);
        let new_length = self.builder.ins().iadd_imm_s(stack_length, -1);
        self.set_stack_frames_len(thread_pointer, new_length);

        let buf_ptr = self.stack_frames_buf(thread_pointer);
        let slot_ptr = self.stack_frame_at(buf_ptr, new_length);

        let sp_offset = (buf_ptr, new_length);

        // Load the stack frame. We're going to use this later.
        (
            StackFrameRepr {
                sp: self.load_field(types::I32, slot_ptr, offset_of!(StackFrame, sp)),
                ip: self.load_field(types::I32, slot_ptr, offset_of!(StackFrame, ip)),
                instructions: self.load_field(
                    types::I128,
                    slot_ptr,
                    offset_of!(StackFrame, instructions),
                ),
                function: self.load_field(types::I64, slot_ptr, offset_of!(StackFrame, function)),
                attachments: self.load_field(
                    types::I64,
                    slot_ptr,
                    offset_of!(StackFrame, attachments),
                ),
            },
            sp_offset,
        )
    }

    // TODO: Might want to merge this with some of the
    // other instructions as to avoid generating extra instructions
    fn update_ip_native_if_yield(
        &mut self,
        vm_ctx: Value,
        should_yield: Value,
        fallback_ip: usize,
    ) {
        self.converging_if_no_else_no_value(
            should_yield,
            |ctx| {
                let zero = ctx.builder.ins().iconst(types::I8, 0);
                ctx.store_field(zero, vm_ctx, offset_of!(VmCore, is_native));
            },
            |ctx| {
                let ip = ctx.i64_const(fallback_ip as i64);
                ctx.store_field(ip, vm_ctx, offset_of!(VmCore, ip));
            },
        );
    }

    /// Clones a value. If the value is not heap allocated,
    /// this does nothing after checking the tag.
    fn clone_value(&mut self, value: Value) {
        let tag = self.get_tag(value);

        let mask = self.i64_const(SteelVal::UNBOXED_MASK as i64);
        let shifted = self.builder.ins().ushr(mask, tag);
        let is_unboxed = self.builder.ins().band_imm_u(shifted, 1);

        let unboxed_block = self.builder.create_block();
        let needs_drop = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_unboxed, unboxed_block, &[], needs_drop, &[]);

        // Unboxed, meaning there is nothing to do here
        self.builder.switch_to_block(unboxed_block);
        self.builder.seal_block(unboxed_block);
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(needs_drop);
        self.builder.seal_block(needs_drop);

        let std_mask = self.i64_const(SteelVal::STANDARD_RC_MASK as i64);
        let std_shifted = self.builder.ins().ushr(std_mask, tag);
        let is_standard_rc = self.builder.ins().band_imm_u(std_shifted, 1);

        let standard_rc_block = self.builder.create_block();
        let special_rc_block = self.builder.create_block();
        let drop_merge = self.builder.create_block();

        self.builder.ins().brif(
            is_standard_rc,
            standard_rc_block,
            &[],
            special_rc_block,
            &[],
        );

        self.builder.switch_to_block(standard_rc_block);
        self.builder.seal_block(standard_rc_block);
        self.clone_rc_value(value); // straight RC decrement
        self.builder.ins().jump(drop_merge, &[]);

        self.builder.switch_to_block(special_rc_block);
        self.builder.seal_block(special_rc_block);
        self.clone_biased_rc(value);
        self.builder.ins().jump(drop_merge, &[]);

        self.builder.switch_to_block(drop_merge);
        self.builder.seal_block(drop_merge);
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }

    /// Clone a standard RC value.
    ///
    /// Boxes and mutable vectors are `HeapRef`s, whose clone is a single atomic
    /// increment of a `Weak` counter at a fixed offset, the mirror of the
    /// decrement `inline_weak_decrement` emits. This avoids a `SteelVal` round
    /// trip through `clone_one`. Continuations still take the call.
    ///
    /// `Weak::clone` panics if the count passes `i32::MAX`; this does not
    /// check. Reaching that needs two billion live references to one heap
    /// object, the same assumption the inline decrement makes.
    fn clone_rc_value(&mut self, value: Value) {
        if !inline_weak_clone_enabled() {
            self.call_helper_void(CLONE_STD_RC, [value]);
            return;
        }

        let tag = self.get_tag(value);
        let weak_mask = self.i64_const(SteelVal::WEAK_RC_MASK as i64);
        let shifted = self.builder.ins().ushr(weak_mask, tag);
        let is_weak = self.builder.ins().band_imm_u(shifted, 1);

        self.converging_if_no_value(
            is_weak,
            |ctx| {
                let ptr = ctx.unbox_value_to_pointer(value);
                let one = ctx.builder.ins().iconst(weak_counter_type(), 1);
                let counter = ctx.builder.ins().iadd_imm_s(ptr, weak_counter_offset());
                ctx.builder.ins().atomic_rmw(
                    weak_counter_type(),
                    MemFlagsData::trusted(),
                    AtomicRmwOp::Add,
                    counter,
                    one,
                );
            },
            |ctx| {
                ctx.call_helper_void(CLONE_STD_RC, [value]);
            },
        );
    }

    /// Clone a biased rc value
    fn clone_biased_rc(&mut self, value: Value) {
        let ptr = self.unbox_value_to_pointer(value);

        // TODO: @Matt
        // DO NOT MERGE THIS THIS WAY, ITS BROKEN
        // THIS NEEDS TO USE THE TAG OR ELSE IT WILL DIE
        // WHEN GOING ACROSS THE BOUNDARY. WE HAVE TO REPLACE
        // raw-slow-increment-closure with a proper inlined
        // call so that we can operate agnostically
        self.increment_ref_count_closure(ptr);
    }

    /// Increments a ref count directly on a biased rc value.
    fn increment_ref_count_closure(&mut self, value: Value) {
        let is_thread_local = self.check_value_tl(value);

        self.converging_if_no_value(
            is_thread_local,
            |ctx| {
                // Fast path increment the counter
                // TODO: Panic here if u32 == max?
                let biased = ctx.builder.ins().load(
                    types::I32,
                    MemFlagsData::trusted(),
                    value,
                    biased_word_offset(),
                );

                // The count is 18 bits, so it can saturate. Spill to the shared
                // counter instead of letting it wrap, since the two sum to the
                // true count either way.
                let count = ctx
                    .builder
                    .ins()
                    .ushr_imm_u(biased, steel_rc::biased_counter_shift() as i64);
                let saturated = ctx.builder.ins().icmp_imm_s(
                    IntCC::Equal,
                    count,
                    steel_rc::biased_counter_max() as i64,
                );

                ctx.converging_if_no_value(
                    saturated,
                    |c| {
                        c.call_helper_void(RAW_SLOW_INCREMENT_CLOSURE, [value]);
                    },
                    |c| {
                        let add_one = c
                            .builder
                            .ins()
                            .iadd_imm_s(biased, steel_rc::biased_counter_one() as i64);

                        c.builder.ins().store(
                            MemFlagsData::trusted(),
                            add_one,
                            value,
                            biased_word_offset(),
                        );
                    },
                );
            },
            |ctx| {
                // TODO: @Matt - we can inline this as well!
                // Slow path increment the counter - this needs to be more generic
                ctx.call_helper_void(RAW_SLOW_INCREMENT_CLOSURE, [value]);
            },
        );
    }

    fn inline_read_captured(&mut self, index: usize, clone: bool) -> Value {
        let vm_ctx = self.get_ctx();
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let stack_length = self.stack_frames_len(thread_pointer);
        let buf_ptr = self.stack_frames_buf(thread_pointer);
        let last = self.builder.ins().iadd_imm_s(stack_length, -1);
        let slot_ptr = self.stack_frame_at(buf_ptr, last);

        let function = self.load_field(types::I64, slot_ptr, offset_of!(StackFrame, function));

        let capture_pointer = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted().with_readonly(),
            function,
            closure_field_offset(offset_of!(ByteCodeLambda, captures)),
        );

        // Just load an offset from there:
        let value = self.builder.ins().load(
            types::I128,
            MemFlagsData::trusted().with_readonly(),
            capture_pointer,
            index as i32 * std::mem::size_of::<SteelVal>() as i32,
        );

        if clone {
            self.clone_value(value);
        }

        value
    }

    // Note: The function _must_ have been cloned already
    // TODO: @Matt: we need a better representation for fat pointers
    // that is custom and has a stable abi in order to properly do this.
    //
    // Also: We can probably check the capacity at the start of the function
    // so that we don't need to do those checks each time we call a function.
    //
    // For example, if there are more functions call that one, we should just
    // grow the stack eagerly, to make sure that we're going to be at capacity,
    // and we can avoid doing the check repeatedly on each function call
    fn push_stack_frame(
        &mut self,
        arity: i64,
        function: Value,
        instr_fat_ptr: Value,
        fallback_ip: usize,
        callee_is_self: bool,
    ) {
        // Lets just see if this is even worth it?
        // We could insert a block before hand and link it in
        // to avoid things?

        let vm_ctx = self.get_ctx();

        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let stack_capacity = self.load_field(
            types::I64,
            thread_pointer,
            offset_of!(SteelThread, stack_frames) + steel_vec::Vec::<StackFrame>::capacity_offset(),
        );
        let stack_frame_length = self.stack_frames_len(thread_pointer);

        // Stack values, for figuring out where we have to put the sp
        let stack_offset = offset_of!(SteelThread, stack);
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();
        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let at_capacity = self
            .builder
            .ins()
            .icmp(IntCC::Equal, stack_capacity, stack_frame_length);

        // At capacity could be lifted to basic block branching, to avoid the check
        // entirely.
        self.converging_if_no_else_no_value_cold(
            at_capacity,
            // If at capacity, call grow
            |ctx| ctx.call_helper_void(SLOW_GROW_FRAME_STACK, []),
            |ctx| {
                // Write the new frame after the last one.
                let buf_ptr = ctx.stack_frames_buf(thread_pointer);
                let slot_ptr = ctx.stack_frame_at(buf_ptr, stack_frame_length);

                let sp_offset = offset_of!(VmCore, sp);

                let arity = -arity;

                let new_sp = ctx.builder.ins().iadd_imm_s(stack_length, arity as i64);

                ctx.builder
                    .ins()
                    .store(MemFlagsData::trusted(), new_sp, vm_ctx, sp_offset as i32);

                // Reduce it before going in the stack frame
                let new_sp = ctx.builder.ins().ireduce(types::I32, new_sp);

                // Stack pointer:
                ctx.store_field(new_sp, slot_ptr, offset_of!(StackFrame, sp));

                // Instruction pointer:

                let ip_plus_one = ctx
                    .builder
                    .ins()
                    .iconst(types::I32, (fallback_ip + 1) as i64);

                ctx.store_field(ip_plus_one, slot_ptr, offset_of!(StackFrame, ip));

                // Instructions:

                // On a self call the stream we would save is the stream we are
                // about to install, and it is already a constant, so read it
                // from `instr_fat_ptr` instead of loading it back out of the vm.
                let current_instructions = if callee_is_self {
                    instr_fat_ptr
                } else {
                    ctx.load_field(types::I128, vm_ctx, offset_of!(VmCore, instructions))
                };

                // TODO: This doesn't work correctly; we need to
                // construct a fat pointer here. So I need something
                // that is FFI safe in order to construct a fat
                // pointer to the *const [DenseInstruction]

                let instructions = instr_fat_ptr;

                ctx.store_field(
                    current_instructions,
                    slot_ptr,
                    offset_of!(StackFrame, instructions),
                );

                // Store the instructions back to the VM pointer. A self call is
                // already running this stream, so the store would write the value
                // that is there.
                if !callee_is_self {
                    ctx.store_field(instructions, vm_ctx, offset_of!(VmCore, instructions));
                }

                // Store the function itself
                ctx.store_field(function, slot_ptr, offset_of!(StackFrame, function));

                let null_pointer = ctx.i64_const(0);

                // Null for the attachments
                ctx.store_field(null_pointer, slot_ptr, offset_of!(StackFrame, attachments));

                let new_pop_count = ctx.pop_count_add_one(vm_ctx);

                ctx.store_field(new_pop_count, vm_ctx, offset_of!(VmCore, pop_count));

                // Set ip to 0
                let zero = ctx.i64_const(0);
                ctx.store_field(zero, vm_ctx, offset_of!(VmCore, ip));

                // Add one to the length:
                let new_length = ctx.builder.ins().iadd_imm_s(stack_frame_length, 1);
                ctx.set_stack_frames_len(thread_pointer, new_length);
            },
        );
    }

    // Attempt to push this to the VM stack inline:
    fn push_to_vm_stack_let_var_new(&mut self, value: Value) {
        self.properties.cached_lookups.stack_length_capacity = self
            .properties
            .cached_lookups
            .stack_length_capacity
            .saturating_sub(1);

        let ctx = self.get_ctx();

        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let capacity_offset = steel_vec::Vec::<SteelVal>::capacity_offset();
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // This is the actual stack, steel_vec::Vec<SteelVal>
        let stack_capacity = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + capacity_offset) as i32,
        );

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // Then, we'll say _if_ the values are equal, we can do something,
        // otherwise we really just have to write the value in and call
        // it a day. Lets see if this is any faster... odds are that its not,
        // but then we can start eliding all sorts of good things because
        // we have direct access to the stack.
        let at_capacity = self
            .builder
            .ins()
            .icmp(IntCC::Equal, stack_capacity, stack_length);

        self.invalidate_buf_ptr();

        self.converging_if_no_else_no_value_cold(
            at_capacity,
            // If at capacity, call grow
            |ctx| ctx.call_helper_void(SLOW_GROW_STACK, []),
            |ctx| {
                // Write the value:
                let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlagsData::trusted(),
                    thread_pointer,
                    (stack_offset + ptr_offset) as i32,
                );

                let size: i64 = std::mem::size_of::<SteelVal>() as _;
                let offset = ctx.builder.ins().imul_imm_s(stack_length, size);
                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                ctx.builder
                    .ins()
                    .store(MemFlagsData::trusted(), value, slot_ptr, 0);

                // Add one to the length:
                let new_length = ctx.builder.ins().iadd_imm_s(stack_length, 1);

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    new_length,
                    thread_pointer,
                    (stack_offset + len_offset) as i32,
                );
            },
        );
    }

    fn push_to_many_vm_stack_let_var_new(&mut self, values: &[Value]) {
        if values.is_empty() {
            return;
        }

        let ctx = self.get_ctx();
        let arity = values.len();

        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let capacity_offset = steel_vec::Vec::<SteelVal>::capacity_offset();
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // This is the actual stack, steel_vec::Vec<SteelVal>
        let stack_capacity = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + capacity_offset) as i32,
        );

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // Update the new length:
        let new_length = self.builder.ins().iadd_imm_s(stack_length, arity as i64);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            new_length,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let at_capacity =
            self.builder
                .ins()
                .icmp(IntCC::UnsignedGreaterThan, new_length, stack_capacity);

        self.invalidate_buf_ptr();

        self.converging_if_no_else_no_value_cold(
            at_capacity,
            // If at capacity, call grow
            |ctx| {
                let amt = ctx.i64_const(arity as i64);
                ctx.call_helper_void(SLOW_STACK_RESERVE_EXACT, [amt])
            },
            |ctx| {
                // Write the value(s):
                let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlagsData::trusted(),
                    thread_pointer,
                    (stack_offset + ptr_offset) as i32,
                );

                let offset = ctx.builder.ins().ishl_imm_u(stack_length, 4);
                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                let size: i32 = std::mem::size_of::<SteelVal>() as i32;

                for (index, value) in values.iter().enumerate() {
                    ctx.builder.ins().store(
                        MemFlagsData::trusted(),
                        *value,
                        slot_ptr,
                        index as i32 * size,
                    );
                }
            },
        );
    }

    fn push_const_index(&mut self, index: usize) -> Value {
        let local_callee = self.helper_callee(PUSH_CONST_INDEX);

        let ctx = self.get_ctx();

        let value = self.i64_const(index as i64);

        let arg_values = [ctx, value];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn get_signature(&self, name: &str) -> Signature {
        let mut sig = self.intrinsics.get_signature(name, &self.module);
        if cfg!(target_os = "windows") {
            sig.call_conv = CallConv::SystemV;
        }
        sig
    }

    // Fetch a function byname
    fn get_local_callee(&mut self, name: &str) -> FuncRef {
        // Most callees can push onto the value stack and reallocate it.
        if callee_can_move_value_stack(name) {
            self.invalidate_buf_ptr();
        }

        let sig = self.get_signature(name);

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        self.module.declare_func_in_func(callee, self.builder.func)
    }

    /// Sets up a FuncRef so that we can call another JIT function directly,
    /// assuming it has been compiled already.
    ///
    /// In this case, we've encountered a bytecode function id, which we then
    /// register in the jit context. From there, we can fetch the value from it,
    /// and subsequently make a direct call to the function.
    fn get_jit_func(&mut self, id: u32) -> FuncRef {
        let mut sig = self.module.make_signature();

        let mut param = AbiParam::new(self.module.target_config().pointer_type());

        param.purpose = ArgumentPurpose::VMContext;

        // VmCore pointer
        sig.params.push(param);
        sig.returns.push(AbiParam::new(types::I128));

        if cfg!(target_os = "windows") {
            sig.call_conv = CallConv::SystemV;
        }

        sig.call_conv = CallConv::Tail;

        let name = self
            .names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("{}_inner", id));

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");

        self.module.declare_func_in_func(callee, self.builder.func)
    }

    /// Fetches a pointer to the VM context
    ///
    /// The vm context stays an ordinary ssa value, which cranelift spills to a
    /// stack slot and reloads after calls. It is not pinned to r15, since the
    /// reloads are cheap L1 hits and reserving r15 costs the allocator a
    /// register in functions that already use all five callee-saved ones.
    fn get_ctx(&mut self) -> Value {
        self.vm_context
    }

    /// Call a helper declared in `helpers.rs` with its arguments. The vm
    /// context goes first implicitly if it takes one. Only for helpers that
    /// can't raise; see `call_helper_checked`.
    fn call_helper<const N: usize, const CTX: bool>(
        &mut self,
        helper: Helper<N, CTX, true, false>,
        args: [Value; N],
    ) -> Value {
        let call = self.emit_helper_call(helper.name, CTX, &args);
        self.builder.inst_results(call)[0]
    }

    /// `call_helper` for a helper that returns nothing.
    fn call_helper_void<const N: usize, const CTX: bool>(
        &mut self,
        helper: Helper<N, CTX, false, false>,
        args: [Value; N],
    ) {
        self.emit_helper_call(helper.name, CTX, &args);
    }

    /// Call a helper that can raise, and leave native code straight away if it
    /// did.
    fn call_helper_checked<const N: usize, const CTX: bool>(
        &mut self,
        helper: Helper<N, CTX, true, true>,
        args: [Value; N],
    ) -> Value {
        let call = self.emit_helper_call(helper.name, CTX, &args);
        let result = self.builder.inst_results(call)[0];
        self.check_deopt();
        result
    }

    /// `call_helper_checked` for a helper that returns nothing.
    #[allow(dead_code)] // the rest of the typed call api; no void helper raises yet
    fn call_helper_void_checked<const N: usize, const CTX: bool>(
        &mut self,
        helper: Helper<N, CTX, false, true>,
        args: [Value; N],
    ) {
        self.emit_helper_call(helper.name, CTX, &args);
        self.check_deopt();
    }

    /// Call a helper that can raise without checking whether it did. The caller
    /// has to reach a `check_deopt` (or hand control back to the interpreter
    /// some other way) before anything uses the result.
    fn call_helper_unchecked<const N: usize, const CTX: bool>(
        &mut self,
        helper: Helper<N, CTX, true, true>,
        args: [Value; N],
    ) -> Value {
        let call = self.emit_helper_call(helper.name, CTX, &args);
        self.builder.inst_results(call)[0]
    }

    /// `call_helper_unchecked` for a helper that returns nothing.
    #[allow(dead_code)] // the rest of the typed call api; no void helper raises yet
    fn call_helper_void_unchecked<const N: usize, const CTX: bool>(
        &mut self,
        helper: Helper<N, CTX, false, true>,
        args: [Value; N],
    ) {
        self.emit_helper_call(helper.name, CTX, &args);
    }

    /// The callee for a declared helper, for building a call by hand.
    fn helper_callee<const N: usize, const CTX: bool, const RET: bool, const RAISES: bool>(
        &mut self,
        helper: Helper<N, CTX, RET, RAISES>,
    ) -> FuncRef {
        self.get_local_callee(helper.name)
    }

    fn emit_helper_call(
        &mut self,
        name: &'static str,
        ctx: bool,
        args: &[Value],
    ) -> cranelift::codegen::ir::Inst {
        self.check_collection_roots(name);
        let callee = self.get_local_callee(name);
        if ctx {
            let mut all = Vec::with_capacity(args.len() + 1);
            all.push(self.get_ctx());
            all.extend_from_slice(args);
            self.builder.ins().call(callee, &all)
        } else {
            self.builder.ins().call(callee, args)
        }
    }

    /// Before a call to a helper that can collect (`helpers::COLLECTS`), every
    /// operand still pending has to be on the vm stack where the collector can
    /// see it, or be a value it doesn't manage. Spilling here would be wrong
    /// inside one arm of a branch (the arms would disagree about the stack), so
    /// the lowering has to have spilled already; this checks it.
    fn check_collection_roots(&self, name: &str) {
        if !helper_collects(name) {
            return;
        }
        for (depth, entry) in self.shadow_stack.iter().enumerate() {
            let exposed = match entry {
                MaybeStackValue::Value(value) => {
                    !value.spilled
                        && !value
                            .inferred_type
                            .proven_tag()
                            .is_some_and(|tag| tag != SteelVal::HEAP_REF_VECTOR_TAG)
                }
                MaybeStackValue::Borrowed(_) => true,
                MaybeStackValue::Register(_)
                | MaybeStackValue::MutRegister(_)
                | MaybeStackValue::Constant(_) => false,
            };
            // A lowering bug. Refuse, and the function stays interpreted
            // (translation panics are caught).
            assert!(
                !exposed,
                "jit: {} calls `{name}`, which can collect, with operand {depth} of {} \
                 held only in a register: {:?}",
                self.name,
                self.shadow_stack.len(),
                entry
            );
        }
    }

    /// The callee for a helper called by a name chosen at run time, checked
    /// against its registered signature. A wrong argument count would otherwise
    /// only surface as a verifier failure, or as garbage in a register.
    fn checked_callee(&mut self, name: &str, argument_count: usize) -> FuncRef {
        self.check_collection_roots(name);
        let expected = self.get_signature(name).params.len();
        assert_eq!(
            expected, argument_count,
            "jit helper `{name}` takes {expected} arguments (vm context included), called with {argument_count}"
        );
        self.get_local_callee(name)
    }

    /// Call a function by name, with the first argument implicitly as the VM context.
    /// Returns a value.
    fn call_function_returns_value_args(&mut self, name: &str, args: &[Value]) -> Value {
        let local_callee = self.checked_callee(name, args.len() + 1);
        let ctx = self.get_ctx();

        let mut arg_values = vec![ctx];
        arg_values.extend(args.iter());
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// Calls a function by name, without implicitly passing a context.
    /// Returns a value.
    fn call_function_returns_value_args_no_context(&mut self, name: &str, args: &[Value]) -> Value {
        let local_callee = self.checked_callee(name, args.len());
        let call = self.builder.ins().call(local_callee, &args);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// Creates a signature ref for use in calling a function indirectly.
    fn create_jit_sig_ref(&mut self) -> codegen::ir::SigRef {
        let mut sig = self.module.make_signature();
        let mut param = AbiParam::new(self.module.target_config().pointer_type());
        param.purpose = ArgumentPurpose::VMContext;

        // VmCore pointer
        sig.params.push(param);

        sig.call_conv = CallConv::Tail;

        sig.returns.push(AbiParam::new(types::I128));

        let sig_ref = self.builder.import_signature(sig);
        sig_ref
    }
}
