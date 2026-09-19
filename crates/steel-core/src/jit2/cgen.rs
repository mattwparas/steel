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
use steel_gen::{opcode::OPCODES_ARRAY, OpCode};
use steel_parser::interner::InternedString;

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::{pretty_print_dense_instructions, DenseInstruction},
    gc::Gc,
    primitives::{
        bytevectors::{steel_bytes_ref, steel_bytes_set},
        lists::{steel_is_empty, steel_list_contains, steel_memq, steel_pair, steel_reverse},
        numbers::{
            floor_quotient, floor_remainder, modulo, quotient, remainder, truncate_quotient,
            truncate_remainder,
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

// Various optimizations that we've added one by one.
// Its important that we get this right
const INLINE_STRUCT_FUNCTION_CALLS: bool = true;
const INLINE_STRUCT_FUNCTION_TAIL_CALLS: bool = true;

const USE_INLINE_CALL_FUNC: bool = true;
/// Inline tail calls to primitives the JIT already knows how to emit, instead
/// of routing them through `call_global_function_tail_deopt_*`.
/// `STEEL_JIT_PRIM_TAIL=0` disables, for A/B measurement.
fn inline_primitive_tail_calls_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_PRIM_TAIL").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `STEEL_JIT_FLOAT_INLINE=0` sends float arithmetic back out to the helper.
fn float_inline_enabled() -> bool {
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_FLOAT_INLINE").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// Inline `=` / `<` / `<=` / `>` / `>=` on two arbitrary values when both turn
/// out to be integers, and `vector-ref` when the vector is not in a register -
/// cases that previously always went out to a helper.
/// `STEEL_JIT_GENERIC_INLINE=0` disables, for A/B measurement.
fn generic_inline_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_GENERIC_INLINE").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `STEEL_JIT_INLINE_TAIL_CALL=0` disables the direct (address-baked) global
/// tail call, for isolating bugs in that path.
fn use_inline_global_tail_call() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        true && !matches!(
            std::env::var("STEEL_JIT_INLINE_TAIL_CALL").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

const USE_INLINE_LOCAL_TAIL_CALL: bool = true;

const USE_EXPERIMENTAL_CALL: bool = true;
const USE_INLINE_TAIL_CALL: bool = true;

const USE_INLINE_CALL_GLOBAL: bool = true;

const INLINE_READ_CAPTURED: bool = true;

const USE_INLINE_DROP_HEAP_BOX: bool = true;

const INLINE_MUTABLE_VECTOR_OPS: bool = true;
const INLINE_FLAT_VECTOR_REF: bool = true;

// const USE_INPLACE_WRITES: bool = true;

// Use inline push global; the shared vector implementation
// is Repr C, so we can look up values directly in this.
const USE_INLINE_PUSH_GLOBAL: bool = true;

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
    // name before it translates, so a failed compile leaves the declaration behind
    // with no body - handing that back to get_finalized_function panics.
    defined: HashSet<String>,

    function_return_types: HashMap<u32, HashSet<InferredType>>,

    // perf inject --jit support. None unless STEEL_JIT_DUMP asked for it -
    // opening it eagerly drops a jit-<pid>.dump into the cwd of every process
    // that builds a JIT
    #[cfg(target_os = "linux")]
    jitdump: Option<wasmtime_jit_debug::perf_jitdump::JitDumpFile>,
}

pub struct FunctionMap<'a> {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    map2: HashMap<&'static str, Box<dyn FunctionToCranelift2 + Send + Sync + 'static>>,
    return_type_hints: HashMap<&'static str, InferredType>,
    builder: &'a mut JITBuilder,
}

#[allow(unused)]
struct OwnedFunctionMap {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    map2: HashMap<&'static str, Box<dyn FunctionToCranelift2 + Send + Sync + 'static>>,
    return_type_hints: HashMap<&'static str, InferredType>,
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

    pub fn add_func_hint(
        &mut self,
        name: &'static str,
        func: impl FunctionToCranelift + Send + Sync + 'static,
        return_type: InferredType,
    ) {
        self.add_func(name, func);
        self.return_type_hints.insert(name, return_type);
    }

    pub fn add_func_hint2(
        &mut self,
        name: &'static str,
        func: impl FunctionToCranelift2 + Send + Sync + 'static,
        return_type: InferredType,
    ) {
        self.add_func2(name, func);
        self.return_type_hints.insert(name, return_type);
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

/// How many speculative exits a function may take before speculation stops
/// being worth it for that function.
///
/// A site that is genuinely polymorphic exits on every call, which is strictly
/// worse than the generic fallback speculation replaced: a full return to the
/// interpreter instead of a predictable branch. Past this many exits the
/// function is recorded and compiled without speculation from then on.
fn speculation_deopt_limit() -> u64 {
    static V: std::sync::OnceLock<u64> = std::sync::OnceLock::new();
    *V.get_or_init(|| {
        std::env::var("STEEL_JIT_DEOPT_LIMIT")
            .ok()
            .and_then(|x| x.parse().ok())
            .unwrap_or(64)
    })
}

/// Exit counts per jitted function, and the set that has given up on
/// speculating. Shared across threads because the jit and its code cache are.
static SPECULATION_DEOPTS: std::sync::Mutex<
    Option<std::collections::HashMap<usize, u64>>,
> = std::sync::Mutex::new(None);

static SPECULATION_DISABLED: std::sync::Mutex<
    Option<std::collections::HashSet<usize>>,
> = std::sync::Mutex::new(None);

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
// Called from a speculative exit. Counts it, and retires speculation for this
// function once the exits stop looking like a cold path.
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

/// Caching `SteelThread.stack.buf` across a block. It only moves when the stack
/// grows, and the jit already invalidates at both of its own growth points; this
/// adds invalidation around calls, which can grow it from the rust side.
///
/// fib reloaded it 27 times for 2.1% of its runtime - the cache field and the
/// invalidator were both written, the reader was left commented out and never
/// populated it.
/// Helpers that provably cannot reallocate `SteelThread.stack`, so a cached
/// buffer pointer survives a call to them.
///
/// Conservative by construction: the answer is "it can move" unless the callee
/// is on this list. Everything here either does arithmetic on `SteelVal`s it was
/// handed, or bumps a reference count - none of them push onto the value stack
/// or re-enter the vm. Getting an entry wrong here leaves generated code using a
/// dangling buffer pointer, so add to it only after checking the callee.
fn callee_can_move_value_stack(name: &str) -> bool {
    const CANNOT: &[&str] = &[
        // arithmetic and comparison slow paths - operate on values, not the stack
        "add-binop",
        "add-binop-int-reg",
        "add-three",
        "sub-binop",
        "sub-binop-int",
        "sub-binop-int-reg",
        "sub-binop-both-reg",
        "sub-negate",
        "sub-three",
        "lt-binop",
        "lt-binop-int",
        "lt-register-int",
        "lt-two-value-bool",
        "lte-two-value-bool",
        "gt-two-value-bool",
        "gte-two-value-bool",
        // refcount traffic only
        "#%clone-std-rc",
        "raw-slow-increment-closure",
    ];

    !CANNOT.contains(&name)
}

/// Speculate on the int tag and bail to the interpreter when it misses, instead
/// of merging with a generic slow path.
///
/// The merge is what forces the boxed representation: a `converging_if` joining
/// an int arm with a "could be anything" arm has to agree on `i128`. Branching to
/// an exit instead means the fast arm never joins, so it can carry a raw i64 -
/// which is what makes `InferredType::Int64` producible at all.
///
/// Milestone scope: one operation, to answer whether jitted code can hand the
/// interpreter a state it can resume from.
fn speculate_int_tag_enabled() -> bool {
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| std::env::var("STEEL_JIT_SPECULATE_INT").as_deref() == Ok("1"))
}

fn stack_buf_cache_enabled() -> bool {
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| std::env::var("STEEL_JIT_BUF_PTR_CACHE").as_deref() == Ok("1"))
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

        // On in release too, so a miscompile gets caught rather than emitted.
        // Costs real time on compile heavy programs, hence the opt out
        if std::env::var("STEEL_JIT_VERIFIER").as_deref() == Ok("false") {
            flag_builder.set("enable_verifier", "false").unwrap();
        }

        // Required, not a choice: cranelift's x64 backend asserts on tail calls
        // without them ("the current implementation relies on them being
        // present"), and the jit emits tail calls. Worth ~3.4% of fib in prologue
        // cost plus whatever %rbp would buy the allocator, so revisit if that
        // restriction is ever lifted upstream.
        flag_builder.set("preserve_frame_pointers", "true").unwrap();

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let arena_mib = std::env::var("STEEL_JIT_MEMORY_SIZE")
            .ok()
            .and_then(|s| s.trim().parse::<usize>().ok())
            .filter(|mib| *mib > 0)
            // Specialized copies roughly double the code a process generates, and
            // every test in `cargo test` shares one jit. On unix this is reserved
            // address space, not committed memory; `region` commits it all on
            // windows, so stay small there.
            .unwrap_or(if cfg!(windows) { 256 } else { 1024 })
            .min(2047);
        builder.memory_provider(Box::new(
            ArenaMemoryProvider::new_with_size(arena_mib << 20)
                .expect("failed to reserve JIT code arena"),
        ));

        for op_code in OPCODES_ARRAY {
            builder.symbol(
                format!("{:?}", op_code),
                crate::steel_vm::vm::jit::C_HANDLERS[op_code as usize] as *const u8,
            );
        }

        // How to take the if branch - this will return a boolean. 1 = true, 0 = false

        let mut map = FunctionMap {
            map: HashMap::new(),
            map2: HashMap::new(),
            builder: &mut builder,
            return_type_hints: HashMap::new(),
        };

        map.add_func2("#%debug-steel-value", abi! { debug_value as fn(SteelVal) });
        map.add_func2("#%debug-value", abi! { debug_int as fn(i64) });
        map.add_func2("#%debug-count", abi! { debug_count as fn(i32) });
        map.add_func2(
            "#%record-speculation-deopt",
            abi! { record_speculation_deopt as fn(i64) },
        );
        map.add_func2("#%debug-tag", abi! { debug_tag as fn(i8) });

        map.add_func2(
            "#%debug-instructions",
            abi! { debug_instructions as fn(RootedInstructions)},
        );

        map.add_func2(
            "#%debug-instructions2",
            abi! { debug_instructions2 as fn(RootedInstructions)},
        );

        map.add_func(
            "#%debug-stack-frames",
            abi! { debug_stack_frames as fn(*mut VmCore) },
        );

        map.add_func(
            "#%debug-instructions-before",
            abi! { debug_instructions_before as fn(*mut VmCore) },
        );
        map.add_func(
            "#%debug-instructions-after",
            abi! { debug_instructions_after as fn(*mut VmCore) },
        );

        map.add_func(
            "#%debug-stack-before",
            abi! { debug_stack_before as fn(*mut VmCore) },
        );
        map.add_func(
            "#%debug-stack-after",
            abi! { debug_stack_after as fn(*mut VmCore) },
        );
        map.add_func(
            "#%debug-is-native",
            abi! { debug_is_native as fn(*mut VmCore) },
        );

        map.add_func(
            "pair?",
            abi! { is_pair_c_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func2(
            "pair?-value",
            abi! { is_pair_value as fn(SteelVal) -> SteelVal },
        );

        map.add_func(
            "list?",
            abi! { is_list_c_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func2(
            "list?-value",
            abi! { is_list_value as fn(SteelVal) -> SteelVal },
        );

        map.add_func(
            "void?",
            abi! { is_void_c_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func2(
            "void?-value",
            abi! { is_void_value as fn(SteelVal) -> SteelVal },
        );

        map.add_func(
            "string?",
            abi! { is_string_c_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func2(
            "string?-value",
            abi! { is_string_value as fn(SteelVal) -> SteelVal },
        );

        map.add_func(
            "symbol?",
            abi! { is_symbol_c_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func2(
            "symbol?-value",
            abi! { is_symbol_value as fn(SteelVal) -> SteelVal },
        );

        map.add_func(
            "empty?",
            abi! { is_empty_c_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func2(
            "empty?-value",
            abi! { is_empty_value as fn(SteelVal) -> SteelVal },
        );

        map.add_func(
            "if-branch",
            abi! { if_handler_value as fn(*mut VmCore) -> bool },
        );

        map.add_func(
            "if-branch-value",
            abi! { if_handler_raw_value as fn(*mut VmCore, i128) -> bool },
        );

        map.add_func(
            "if-branch-register",
            abi! { if_handler_register as fn(*mut VmCore, u64) -> bool },
        );

        map.add_func(
            "not-value",
            abi! { not_handler_raw_value as fn(*mut VmCore, SteelVal) -> SteelVal },
        );

        map.add_func(
            "call-global",
            abi! { callglobal_handler_deopt_c as fn(*mut VmCore) -> u8 },
        );

        map.add_func(
            "call-global-tail-spilled",
            abi! { callglobal_tail_handler_deopt_spilled as fn(*mut VmCore, usize, usize, usize) -> SteelVal },
        );

        map.add_func(
            "call-global-no-arity-spilled",
            abi! { call_global_function_deopt_no_arity_spilled as fn(*mut VmCore, usize, usize, usize) -> SteelVal },
        );

        map.add_func(
            "call-global-spilled",
            abi! { call_global_function_deopt_spilled as fn(*mut VmCore, usize, usize, usize) -> SteelVal },
        );

        map.add_func(
            "list-handler-spilled",
            abi! { list_handler_c as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func(
            "variadic-numeric-spilled",
            abi! { variadic_numeric_spilled as fn(*mut VmCore, usize, usize) -> SteelVal },
        );

        map.add_func(
            "vec-handler-spilled",
            abi! { vec_handler_c as fn(*mut VmCore, usize) -> SteelVal },
        );

        // Value functions:
        map.add_func(
            "num-equal-value",
            abi! { num_equal_value as fn(*mut VmCore, SteelVal, SteelVal) -> SteelVal },
        );

        map.add_func(
            "num-equal-value-bool",
            abi! { num_equal_value_bool as fn(*mut VmCore, SteelVal, SteelVal) -> bool },
        );

        map.add_func(
            "num-equal-int",
            abi! { num_equal_int as fn(*mut VmCore, SteelVal, SteelVal) -> SteelVal },
        );

        map.add_func(
            "num-equal-int-register",
            abi! { num_equal_int_register as fn(*mut VmCore, usize, SteelVal) -> bool },
        );

        map.add_func(
            "equal-binop-bool",
            abi! { equal_binop_bool as fn(*mut VmCore, SteelVal, SteelVal) -> bool },
        );

        map.add_func(
            "equal-binop-register-bool",
            abi! { equal_binop_register_bool as fn(*mut VmCore, usize, SteelVal) -> bool },
        );

        map.add_func(
            "num-equal-value-unboxed",
            abi! { num_equal_value_unboxed as fn(*mut VmCore, i128, i128) -> bool },
        );

        map.add_func(
            "let-end-scope-c",
            abi! { let_end_scope_c as fn(*mut VmCore, usize) },
        );

        map.add_func(
            "drop-value",
            abi! { drop_value as fn(*mut VmCore, SteelVal) },
        );

        map.add_func2("drop-one", abi! { drop_one as fn(SteelVal) });

        // TODO: Inline this!
        map.add_func2(
            "drop-box",
            abi! { drop_box as fn(crate::values::closed::HeapRef<SteelVal>) },
        );

        map.add_func2(
            "drop-boxed-vec",
            abi! { drop_boxed_vec as fn(crate::values::closed::HeapRef<crate::values::closed::HeapVec>) },
        );

        map.add_func2("log-let-var", abi! { log_counter as fn() });

        map.add_func2("#%clone-std-rc", abi! { clone_one as fn(SteelVal) });

        map.add_func2(
            "drop-value-post-fast-dec",
            abi! { drop_value_post_fast_decrement as fn(SteelVal) },
        );

        map.add_func2(
            "drop-value-post-fast-dec-closure",
            abi! { drop_value_post_fast_decrement_closure as fn(Gc<ByteCodeLambda>) },
        );

        map.add_func2(
            "drop-value-slow-dec",
            abi! { drop_value_slow_decrement as fn(SteelVal) },
        );

        map.add_func2(
            "drop-value-slow-dec-closure",
            abi! { drop_value_slow_decrement_closure as fn(Gc<ByteCodeLambda>) },
        );

        map.add_func2(
            "raw-slow-increment",
            abi! { increment_ref_count_slow as fn(SteelVal) },
        );

        map.add_func2(
            "raw-slow-increment-closure",
            abi! { increment_ref_count_slow_closure as fn(Gc<ByteCodeLambda>) },
        );

        map.add_func(
            "#%handle-attachments",
            abi! { handle_attachments_pop as fn(*mut VmCore, Option<Box<StackFrameAttachments>>) },
        );

        map.add_func(
            "#%pop-slow-path-finish",
            abi! { pop_slow_path_finish as fn(*mut VmCore, value: SteelVal) },
        );

        map.add_func(
            "slow-grow-stack",
            abi! { grow_stack_slow as fn(*mut VmCore) },
        );

        map.add_func(
            "slow-stack-reserve-exact",
            abi! { stack_ensure_capacity as fn(*mut VmCore, usize) },
        );

        map.add_func(
            "slow-grow-frame-stack",
            abi! { grow_frame_stack_slow as fn(*mut VmCore) },
        );

        map.add_func(
            "pop-from-stack",
            abi! { pop_value as fn(*mut VmCore) -> SteelVal },
        );

        map.add_func(
            "handle-pop!",
            abi! { extern_handle_pop as fn(*mut VmCore, SteelVal) },
        );

        map.add_func(
            "new-closure",
            abi! {handle_new_start_closure as fn(*mut VmCore, usize, usize) -> SteelVal },
        );

        map.add_func(
            "pure-func",
            abi! { handle_pure_function as fn(*mut VmCore, usize, usize) -> SteelVal },
        );

        map.add_func(
            "#%setup-closure",
            abi! { setup_closure_call as fn(*mut VmCore, Gc<ByteCodeLambda>) -> SteelVal },
        );

        map.add_func(
            "#%setup-closure-arity",
            abi! { setup_closure_call_arity as fn(*mut VmCore, Gc<ByteCodeLambda>, usize, usize) -> SteelVal },
        );

        CallStructConstructorsDefinitions::register(&mut map);
        CallMutableStructConstructorsDefinitions::register(&mut map);
        CallFlatVectorConstructorsDefinitions::register(&mut map);

        CallSelfNoArityFunctionDefinitions::register(&mut map);

        CallGlobalFunctionDefinitions::register(&mut map);
        CallGlobalNoArityFunctionDefinitions::register(&mut map);
        CallFunctionDefinitions::register(&mut map);
        CallFunctionTailDefinitions::register(&mut map);
        CallGlobalTailFunctionDefinitions::register(&mut map);
        CallSelfTailCallNoArityDefinitions::register(&mut map);
        CallSelfTailCallNoArityLoopDefinitions::register(&mut map);
        ListHandlerDefinitions::register(&mut map);

        // Primitive calls:
        CallPrimitiveDefinitions::register(&mut map);
        CallPrimitiveMutDefinitions::register(&mut map);

        CallPrimitiveFixedDefinitions::register(&mut map);

        // CallRegisterPrimitiveFixedDefinitions::register(&mut map);

        // DebugStackDefinitions::register(&mut map);

        map.add_func(
            "push-global-value",
            abi! { push_global as fn(ctx: *mut VmCore, index: usize) -> SteelVal },
        );

        // Check if the function at the global location is in fact the right one.
        map.add_func(
            "check-callable",
            abi! { check_callable as fn(ctx: *mut VmCore, index: usize) -> bool },
        );

        map.add_func(
            "should-spill",
            abi! { should_spill as fn(ctx: *mut VmCore, index: usize) -> bool },
        );

        map.add_func(
            "should-spill-value",
            abi! { should_spill_value as fn(ctx: *mut VmCore, value: SteelVal) -> bool },
        );

        map.add_func(
            "check-callable-tail",
            abi! { check_callable_tail as fn(ctx: *mut VmCore, index: usize) -> bool },
        );

        map.add_func(
            "check-callable-value",
            abi! { check_callable_value as fn(ctx: *mut VmCore, func: SteelVal) -> bool },
        );

        map.add_func(
            "check-callable-tail-value",
            abi! { check_callable_value_tail as fn(ctx: *mut VmCore, func: SteelVal) -> bool },
        );

        map.add_func(
            "push-to-vm-stack",
            abi! { push_to_vm_stack as fn(ctx: *mut VmCore, value: SteelVal) },
        );

        map.add_func(
            "push-to-vm-stack-let-var",
            abi! { push_to_vm_stack_let_var as fn(ctx: *mut VmCore, value: SteelVal) },
        );

        map.add_func(
            "push-to-vm-stack-function-spill",
            abi! { _push_to_vm_stack_function_spill as fn(ctx: *mut VmCore, value: SteelVal) },
        );

        map.add_func(
            "push-to-vm-stack-2",
            abi! { push_to_vm_stack_two as fn(ctx: *mut VmCore, value: SteelVal, value2: SteelVal) },
        );

        #[cfg(target_os = "windows")]
        type Vm01 = extern "sysv64-unwind" fn(*mut VmCore) -> SteelVal;

        #[cfg(not(target_os = "windows"))]
        type Vm01 = extern "C-unwind" fn(*mut VmCore) -> SteelVal;

        #[cfg(target_os = "windows")]
        type Vm02 = extern "sysv64-unwind" fn(*mut VmCore, SteelVal) -> SteelVal;
        #[cfg(not(target_os = "windows"))]
        type Vm02 = extern "C-unwind" fn(*mut VmCore, SteelVal) -> SteelVal;

        #[allow(improper_ctypes_definitions)]
        #[cfg(target_os = "windows")]
        type VmBinOp =
            extern "sysv64-unwind" fn(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal;

        #[cfg(not(target_os = "windows"))]
        #[allow(improper_ctypes_definitions)]
        type VmBinOp = extern "C-unwind" fn(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal;

        #[allow(improper_ctypes_definitions)]
        #[cfg(target_os = "windows")]
        type BinOp = extern "sysv64-unwind" fn(a: SteelVal, b: SteelVal) -> SteelVal;
        #[allow(improper_ctypes_definitions)]
        #[cfg(not(target_os = "windows"))]
        type BinOp = extern "C-unwind" fn(a: SteelVal, b: SteelVal) -> SteelVal;

        // TODO: Add type checked variants as well which can allow
        // passing through unboxed values on the stack
        map.add_func("car-handler-value", car_handler_value as Vm02);
        map.add_func("cdr-handler-value", cdr_handler_value as Vm02);

        map.add_func(
            "cdr-reg",
            abi! { cdr_handler_reg as fn(*mut VmCore, usize) -> SteelVal },
        );
        map.add_func(
            "cdr-mut-reg",
            abi! { cdr_handler_mut_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        // map.add_func(
        //     "cdr-mut-reg",
        //     abi! { cdr_handler_mut_reg as fn(*mut VmCore, usize) },
        // );

        map.add_func(
            "cdr-reg-no-check",
            abi! { cdr_handler_reg_no_check as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func(
            "cdr-mut-reg-no-check",
            abi! { cdr_handler_mut_reg_no_check as fn(*mut VmCore, usize) },
        );

        map.add_func("cons-handler-value", cons_handler_value as VmBinOp);

        map.add_func(
            "cons-handler-value-register",
            abi! { cons_handler_value_register as fn (*mut VmCore, SteelVal, usize) },
        );

        map.add_func(
            "cons-handler-register-register",
            abi! { cons_handler_register_register as fn (*mut VmCore, usize, usize) },
        );

        map.add_func(
            "car-reg",
            abi! { car_handler_reg as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func(
            "car-reg-unchecked",
            abi! { car_handler_reg_no_check as fn(*mut VmCore, usize) -> SteelVal },
        );

        // TODO: Add type checked variants as well which can allow
        // passing through unboxed values on the stack
        map.add_func("box-handler", box_handler_c as Vm02);
        map.add_func("unbox-handler", unbox_handler_c as Vm02);
        map.add_func("set-box-handler", setbox_handler_c as VmBinOp);
        map.add_func("list-ref-value", list_ref_handler_c as VmBinOp);
        map.add_func("vector-ref-value", vector_ref_handler_c as VmBinOp);

        map.add_func(
            "vector-ref-reg-1",
            abi! { vector_ref_handler_register as fn(*mut VmCore, u16, SteelVal) -> SteelVal },
        );

        map.add_func(
            "vector-ref-reg-2",
            abi! { vector_ref_handler_register_two as fn(*mut VmCore, usize, usize) -> SteelVal },
        );

        map.add_func(
            "vector-ref-reg-2-unboxed-index",
            abi! { vector_ref_handler_register_two_unboxed as fn(*mut VmCore, usize, usize) -> SteelVal },
        );

        map.add_func(
            "vector-set-args",
            abi! { vector_set_handler_stack
                as fn(
                    ctx: *mut VmCore,
                    SteelVal,
                    SteelVal,
                    SteelVal,
                ) -> SteelVal
            },
        );

        map.add_func(
            "vector-push-args",
            abi! { vector_push_handler_stack
                as fn(
                    ctx: *mut VmCore,
                    SteelVal,
                    SteelVal,
                ) -> SteelVal
            },
        );

        map.add_func(
            "vector-set-reg-1",
            abi! { vector_set_handler_register_one
            as fn(
                ctx: *mut VmCore,
                usize,
                SteelVal,
                SteelVal,
            ) -> SteelVal },
        );

        map.add_func(
            "vector-set-reg-2",
            abi! { vector_set_handler_register_two
            as fn(ctx: *mut VmCore, usize, usize, SteelVal) -> SteelVal },
        );

        map.add_func(
            "vector-set-reg-3",
            abi! { vector_set_handler_register_three
            as fn(ctx: *mut VmCore, usize, usize, usize) -> SteelVal },
        );

        map.add_func(
            "list-contains-reg",
            abi! { list_contains_reg as fn(ctx: *mut VmCore, usize, List<SteelVal>) -> bool },
        );

        map.add_func(
            "list-contains-reg-constant",
            abi! { list_contains_reg_constant as fn(ctx: *mut VmCore, usize, List<SteelVal>) -> bool },
        );

        map.add_func(
            "list-contains-value",
            abi! { list_contains_value as fn(ctx: *mut VmCore, SteelVal, SteelVal) -> bool },
        );

        map.add_func2(
            "memq-unchecked-list",
            abi! { memq_unchecked_list as fn(SteelVal, List<SteelVal>) -> SteelVal },
        );

        map.add_func(
            "memq-value",
            abi! { memq_value as fn(ctx: *mut VmCore, SteelVal, SteelVal) -> SteelVal },
        );

        map.add_func(
            "eq?-reg-2",
            abi! { eq_reg_2 as fn(ctx: *mut VmCore, usize, usize) -> bool },
        );

        map.add_func(
            "eq?-reg-1",
            abi! { eq_reg_1 as fn(ctx: *mut VmCore, usize, SteelVal) -> bool },
        );

        map.add_func2(
            "symbol-equal?-no-drop",
            abi! { symbol_equal_no_drop as fn(SteelString, SteelString) -> bool },
        );

        map.add_func2(
            "eq?-args",
            abi! { eq_value as fn(SteelVal, SteelVal) -> bool },
        );

        map.add_func2(
            "eq?-no-drop",
            abi! { eq_value_no_drop as fn(SteelVal, SteelVal) -> bool },
        );

        map.add_func("push-const", push_const_value_c as Vm01);
        map.add_func(
            "push-const-index",
            abi! { push_const_value_index_c as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func_hint(
            "add-binop",
            extern_c_add_two as VmBinOp,
            InferredType::Number,
        );

        map.add_func_hint(
            "add-three",
            abi! { extern_c_add_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "add-four",
            abi! { extern_c_add_four
            as fn(
                *mut VmCore,
                SteelVal,
                SteelVal,
                SteelVal,
                SteelVal,
            ) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "add-binop-reg",
            abi! { extern_c_add_two_binop_register
            as  fn(*mut VmCore, usize, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "add-binop-reg-2",
            abi! { extern_c_add_two_binop_register_both
            as fn(*mut VmCore, usize, usize) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop",
            extern_c_sub_two as VmBinOp,
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-negate",
            abi! { extern_c_negate as fn(ctx: *mut VmCore, a: SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop-int",
            extern_c_sub_two_int as VmBinOp,
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop-both-reg",
            abi! { extern_c_sub_two_both_reg as fn(ctx: *mut VmCore, a: usize, b: usize) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop-int-reg",
            abi! { extern_c_sub_two_int_reg
            as fn(*mut VmCore, usize, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "add-binop-int-reg",
            abi! { extern_c_add_two_int_reg
            as fn(*mut VmCore, usize, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop-float-reg",
            abi! { extern_c_sub_two_float_reg
            as fn(*mut VmCore, usize, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop-reg",
            abi! { extern_c_sub_two_reg
            as fn(*mut VmCore, usize, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint("lt-binop", extern_c_lt_two as VmBinOp, InferredType::Bool);

        map.add_func_hint(
            "lt-two-value-bool",
            abi! { extern_c_lt_two_value_bool as fn(*mut VmCore, SteelVal, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );
        map.add_func_hint(
            "lte-two-value-bool",
            abi! { extern_c_lte_two_value_bool as fn(*mut VmCore, SteelVal, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );
        map.add_func_hint(
            "gt-two-value-bool",
            abi! { extern_c_gt_two_value_bool as fn(*mut VmCore, SteelVal, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );
        map.add_func_hint(
            "gte-two-value-bool",
            abi! { extern_c_gte_two_value_bool as fn(*mut VmCore, SteelVal, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );

        map.add_func_hint("lte-binop", extern_c_lte_two as VmBinOp, InferredType::Bool);

        map.add_func_hint(
            "lte-binop-int",
            extern_c_lte_two_int as VmBinOp,
            InferredType::Bool,
        );

        map.add_func_hint(
            "lte-register",
            abi! { extern_c_lte_register as fn(*mut VmCore, usize, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );

        map.add_func_hint(
            "lte-register-int",
            abi! { extern_c_lte_register_int as fn(*mut VmCore, usize, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );

        map.add_func_hint(
            "gte-register-int",
            abi! { extern_c_gte_register_int as fn(*mut VmCore, usize, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );

        map.add_func_hint(
            "gte-register-unknown",
            abi! { extern_c_gte_register_unknown as fn(*mut VmCore, usize, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );

        map.add_func_hint(
            "lt-register-int",
            abi! { extern_c_lt_register_int as fn(*mut VmCore, usize, SteelVal) -> bool },
            InferredType::UnboxedBool,
        );

        map.add_func_hint(
            "lt-binop-int",
            extern_c_lt_two_int as VmBinOp,
            InferredType::Bool,
        );

        map.add_func_hint2(
            "null-handler",
            abi! { extern_c_null_handler as fn(a: SteelVal) -> SteelVal },
            InferredType::Bool,
        );

        map.add_func_hint(
            "lt-three",
            abi! { extern_c_lt_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Bool,
        );

        map.add_func_hint(
            "lte-three",
            abi! { extern_c_lte_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Bool,
        );

        map.add_func_hint(
            "gt-three",
            abi! { extern_c_gt_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Bool,
        );

        map.add_func_hint(
            "gte-three",
            abi! { extern_c_gte_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Bool,
        );

        map.add_func_hint("gt-binop", extern_c_gt_two as VmBinOp, InferredType::Bool);
        map.add_func_hint("gte-binop", extern_c_gte_two as VmBinOp, InferredType::Bool);
        map.add_func_hint(
            "mult-two",
            extern_c_mult_two as VmBinOp,
            InferredType::Number,
        );

        map.add_func_hint(
            "mult-three",
            abi! { extern_c_mult_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-three",
            abi! { extern_c_sub_three
            as fn(*mut VmCore, SteelVal, SteelVal, SteelVal) -> SteelVal },
            InferredType::Number,
        );

        map.add_func_hint("div-two", extern_c_div_two as VmBinOp, InferredType::Number);
        map.add_func_hint("div-one", extern_c_div_one as Vm02, InferredType::Number);

        // TODO: Pick up from here!
        map.add_func("read-local-0", read_local_0_value_c as Vm01);
        map.add_func("read-local-1", read_local_1_value_c as Vm01);
        map.add_func("read-local-2", read_local_2_value_c as Vm01);
        map.add_func("read-local-3", read_local_3_value_c as Vm01);
        map.add_func(
            "set-local-any",
            abi! { set_local_any_c as fn(*mut VmCore, usize, SteelVal) -> SteelVal },
        );

        map.add_func(
            "read-local-any",
            abi! { read_local_any_value_c
            as fn(ctx: *mut VmCore, lookup_index: usize) -> SteelVal },
        );

        map.add_func(
            "read-captured",
            abi! { read_captured_c
            as fn(ctx: *mut VmCore, index: usize) -> SteelVal },
        );

        map.add_func("move-read-local-0", move_read_local_0_value_c as Vm01);
        map.add_func("move-read-local-1", move_read_local_1_value_c as Vm01);
        map.add_func("move-read-local-2", move_read_local_2_value_c as Vm01);
        map.add_func("move-read-local-3", move_read_local_3_value_c as Vm01);

        map.add_func(
            "move-read-local-any",
            abi! { move_read_local_any_value_c
            as fn(ctx: *mut VmCore, lookup_index: usize) -> SteelVal },
        );

        map.add_func(
            "self-tail-call",
            abi! { self_tail_call_handler as fn(*mut VmCore, usize) },
        );

        map.add_func(
            "self-tail-call-loop",
            abi! { self_tail_call_handler_loop as fn(*mut VmCore, usize) },
        );

        map.add_func(
            "tco-jump",
            abi! { tcojmp_handler as fn(*mut VmCore, usize) },
        );

        map.add_func(
            "set-handler",
            abi! { set_handler_c as fn(*mut VmCore, usize, SteelVal) -> SteelVal },
        );

        let function_map = OwnedFunctionMap {
            map: map.map,
            map2: map.map2,
            return_type_hints: map.return_type_hints,
        };

        let module = JITModule::new(builder);

        #[cfg(target_os = "linux")]
        let jitdump = open_jitdump();

        Self {
            builder_context: FunctionBuilderContext::new(),
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

// Where the slice data starts in an `RcBox<[DenseInstruction]>`. Same header as
// any other `RcBox`, so take it from the type rather than a literal.
// The weak counter's offset and width in the box header. `inline_weak_decrement`
// writes it directly, so both have to track the type - at the wrong offset this
// decrements the spinlock instead and the next acquire never completes.
fn weak_counter_offset() -> i64 {
    steel_rc::weak::weak_offset::<
        crate::values::lock::SpinLock<crate::values::closed::HeapAllocated<SteelVal>>,
    >() as i64
}

/// `STEEL_JIT_INLINE_WEAK_CLONE=0` sends heap reference clones back through the
/// out-of-line `clone_one` call. On by default.
fn inline_weak_clone_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_INLINE_WEAK_CLONE").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `STEEL_JIT_PRIM_TAIL_EXTRA=0` turns off tail-position inlining of `eq?`,
/// `vector-set!` and `#%make-mutable-struct`, leaving `#%unbox` / `#%set-box!`.
/// `STEEL_JIT_BORROW=0` clones every value read out of a container as it is
/// read, instead of leaving it borrowed for a consumer that doesn't need it
/// owned.
pub(super) fn borrow_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_BORROW").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
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

/// `STEEL_JIT_INLINE_SETBOX=0` sends the `SETBOX` opcode back out to
/// `set-box-handler`, which pays a `Weak::upgrade` CAS and the spin lock on
/// every write even when the box is unshared.
pub(super) fn inline_setbox_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_INLINE_SETBOX").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `STEEL_JIT_BYTEVECTOR_STRICT_UNSHARED=1` makes the bytevector lock elision
/// additionally require the owner's reference count to be exactly one.
///
/// Off by default: a zero shared half-word already proves every reference
/// belongs to the owner thread, and several references held by one thread
/// cannot race with each other. The strict form exists to A/B that reasoning.
pub(super) fn byte_vec_strict_unshared() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        matches!(
            std::env::var("STEEL_JIT_BYTEVECTOR_STRICT_UNSHARED")
                .ok()
                .as_deref(),
            Some("1") | Some("true")
        )
    })
}

/// `STEEL_JIT_INLINE_BYTEVECTOR=0` sends `bytes-ref` / `bytes-set!` back out to
/// the generic primitive call.
pub(super) fn inline_bytevector_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_INLINE_BYTEVECTOR").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// Maps a primitive to the division it performs, or `None` if it is not one.
///
/// `quotient` / `remainder` / `modulo` are thin wrappers over the
/// `truncate-*` and `floor-*` procedures, but they are distinct fn items with
/// distinct addresses, so each one has to be named here.
fn divmod_mode(f: FunctionSignature) -> Option<DivMode> {
    if f == quotient as FunctionSignature || f == truncate_quotient as FunctionSignature {
        Some(DivMode::TruncQuotient)
    } else if f == remainder as FunctionSignature
        || f == truncate_remainder as FunctionSignature
    {
        Some(DivMode::TruncRemainder)
    } else if f == floor_quotient as FunctionSignature {
        Some(DivMode::FloorQuotient)
    } else if f == modulo as FunctionSignature || f == floor_remainder as FunctionSignature {
        Some(DivMode::FloorRemainder)
    } else {
        None
    }
}

/// `STEEL_JIT_INLINE_DIVMOD=0` sends `quotient` / `remainder` / `modulo` back
/// out through the generic primitive call instead of inlining the two-fixnum
/// case.
pub(super) fn inline_divmod_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_INLINE_DIVMOD").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

/// `STEEL_JIT_INLINE_EQ=0` sends `eq?` on two non-constant operands back
/// through the out-of-line helpers.
pub(super) fn inline_eq_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_INLINE_EQ").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

fn extra_primitive_tail_calls_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        !matches!(
            std::env::var("STEEL_JIT_PRIM_TAIL_EXTRA").ok().as_deref(),
            Some("0") | Some("false")
        )
    })
}

fn weak_counter_type() -> Type {
    match steel_rc::weak::ref_count_width() {
        8 => types::I64,
        4 => types::I32,
        other => unreachable!("unsupported refcount width: {other}"),
    }
}

// Byte offsets from the pointer a `SteelVal::ListV` carries to the cell's
// fields. That pointer is the `RcBox`, so the cell data starts after the header
// - derive both rather than hardcoding, the way the heap-box offsets do.
/// The biased half-word, which generated code adjusts directly. The owner id is
/// in the high bits and its count in the low bits, so `word +/- 1` moves the
/// count alone and never disturbs the id.
/// `SteelVal::Closure` carries the allocation pointer, so a field of the lambda
/// sits past the reference counting header. Derived, because that header's size
/// is not a constant of the language.
/// `SteelVal::Pair` carries the allocation pointer, so `car` and `cdr` sit past
/// the reference counting header. Derived - that header's size is not a
/// constant of the language.
fn pair_field_offset(field: usize) -> i32 {
    (steel_rc::BiasedRc::<crate::values::lists::Pair>::data_offset() + field) as i32
}

fn closure_field_offset(field: usize) -> i32 {
    (steel_rc::BiasedRc::<ByteCodeLambda>::data_offset() + field) as i32
}

fn biased_word_offset() -> i32 {
    steel_rc::biased_offset() as i32
}

fn list_cell_base() -> i64 {
    steel_rc::BiasedRc::<SteelVal>::data_offset() as i64
}

fn list_index_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_index_offset() as i64) as i32
}

fn list_elements_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_elements_offset() as i64) as i32
}

fn list_size_offset() -> i32 {
    (list_cell_base() + SteelList::<SteelVal>::cell_size_offset() as i64) as i32
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

const fn rcbox_slice_data_offset() -> i64 {
    steel_rc::BiasedRc::<DenseInstruction>::data_offset() as i64
}

/// Loop specialization (M3). A function whose self tail call carries fixnums
/// in some argument slots gets a second compiled copy with those slots typed
/// `Int`. The generic copy checks the slots once, at its tail call, and tail
/// calls into the specialized one; the specialized copy loops on itself for as
/// long as the slots stay fixnums, and tail calls back otherwise.
#[derive(Clone, Debug)]
enum SpecMode {
    None,
    Generic { spec_id: FuncId, seed: Vec<(usize, SpecType)> },
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

/// What a translation reports back to the compile driver.
struct TranslateInfo {
    /// Types of the values returned through ordinary returns.
    returned: HashSet<InferredType>,
    /// A reachable exit other than an ordinary return or a deopt exit.
    untyped_non_deopt_exit: bool,
    /// Self calls compiled as direct calls to this same specialized copy.
    direct_self_calls: usize,
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
            let payload = builder
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
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| std::env::var("STEEL_JIT_SPECIALIZE_LOOPS").as_deref() != Ok("0"))
}

fn spec_debug_enabled() -> bool {
    static V: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *V.get_or_init(|| std::env::var_os("STEEL_JIT_SPEC_DEBUG").is_some())
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
            stack.push(if slot < arity { Entry::Slot(slot) } else { Entry::Other });
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
                    .filter_map(|e| if let Entry::Slot(k) = e { Some(*k) } else { None })
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
                        // `null?` is a separate opcode and stays compatible: the
                        // empty list is a list.
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
        // `deriv` does `(car a)` after `(pair? a)`, and half its calls pass a
        // symbol: the list seed cost a failing guard on those and bought nothing.
        if mutated.contains(&slot) || u.type_tested {
            continue;
        }
        let kinds = u.numeric as usize + u.list as usize + u.vector as usize + (!u.structs.is_empty()) as usize;
        if kinds != 1 || u.structs.len() > 1 {
            continue;
        }
        let ty = if u.numeric {
            if floats.contains(&slot) { SpecType::Float } else { SpecType::Fixnum }
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

/// At the top of a generic copy: tail call into the specialized copy when every
/// seeded argument slot holds a fixnum. Raw loads, emitted before the
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
/// recorded. Unreachable blocks are skipped: the translator parks code after a
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
    unsafe { *<*const _>::from(value).cast::<u8>() }
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
    non_mutable_globals: &HashSet<usize>,
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
        non_mutable_globals,
    )?;
    let code_fn = JitFnPointer(NonNull::new_unchecked(code_ptr.cast_mut()));

    Ok(code_fn)
}

// perf metadata for jitted code, off unless asked for - both write a file named
// after the pid and cost a syscall per compiled function. STEEL_JIT_PERF_MAP
// appends to /tmp/perf-<pid>.map for perf report, STEEL_JIT_DUMP writes
// jit-<pid>.dump in the cwd for perf inject --jit. Read once, these are on the
// compile path.
fn perf_map_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("STEEL_JIT_PERF_MAP").is_some())
}

#[cfg(target_os = "linux")]
fn jitdump_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("STEEL_JIT_DUMP").is_some())
}

// None rather than a panic if we can't make the file - a read only working
// directory is a normal way to run.
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
        non_mutable_globals: &HashSet<usize>,
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
                non_mutable_globals,
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
        non_mutable_globals: &HashSet<usize>,
    ) -> Result<*const u8, String> {
        let id = str::parse::<u32>(&name).unwrap();

        let inner_name = if let Some(top_level_name) = top_level_name {
            format!("{}_{}_inner", top_level_name, name)
        } else {
            // Store the name
            format!("{}_inner", name)
        };

        self.names.insert(id, inner_name.clone());

        // self.ctx.set_disasm(true);

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

        // Loop specialization: declared up front because the generic copy tail
        // calls it. It is always defined once the generic copy is (see
        // `compile_specialized`), so the reference is never left dangling.
        let seed = if loop_specialization_enabled()
            // For bisecting a specialization problem to one function: comma
            // separated name substrings to allow (`STEEL_JIT_SPEC_ONLY`) or
            // exclude (`STEEL_JIT_SPEC_SKIP`).
            && std::env::var("STEEL_JIT_SPEC_ONLY").map_or(true, |v| {
                v.split(',').any(|part| inner_name.contains(part))
            })
            && std::env::var("STEEL_JIT_SPEC_SKIP").map_or(true, |v| {
                !v.split(',').any(|part| inner_name.contains(part))
            })
        {
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
        // The specialized copy is compiled first, because whether the generic
        // copy may hand off to it depends on whether it compiled. If it did not,
        // it is still defined - as a stub - but the generic copy gets no guard,
        // so nothing ever calls it. (A guard in front of a stub that calls back
        // into the generic copy never runs an iteration: it loops forever.)
        let mut spec_code_size = None;
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
                    non_mutable_globals,
                    seed.clone(),
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
        let spec_compiled = matches!(mode, SpecMode::Generic { .. });

        // Then, translate the AST nodes into Cranelift IR.
        let translated = self.translate(
            id,
            inner_name.clone(),
            inner_id,
            arity,
            stmts,
            globals,
            constants,
            function_index,
            slot,
            non_mutable_globals,
            mode,
        );

        // `STEEL_JIT_DUMP_CLIF=<substring>` prints the ir for every function whose
        // name contains it - the only way to read what a loop's back edge really
        // emits. `=1` keeps the original behaviour of printing only a function the
        // verifier rejected.
        if let Ok(filter) = std::env::var("STEEL_JIT_DUMP_CLIF") {
            if filter != "1" && inner_name.contains(&filter) {
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
                    // STEEL_JIT_DUMP_CLIF=1 prints the function that failed, which is the
                    // only practical way to chase a dominance error back to the block
                    // that defines the offending value.
                    if std::env::var("STEEL_JIT_DUMP_CLIF").as_deref() == Ok("1") {
                        eprintln!("--- clif for failed function ---\n{}", self.ctx.func);
                    }
                    eprintln!("{:#?}", e);
                    Some(format!("errors: {:#?}", e))
                }
                Ok(()) => self.module.define_function(inner_id, &mut self.ctx).err().map(|e| {
                    eprintln!("error in defining function: {}", e);
                    e.to_string()
                }),
            },
        };

        if let Some(e) = generic_failure {
            self.module.clear_context(&mut self.ctx);
            // A compiled specialized copy tail calls back into this one, so this
            // name has to be defined before anything is finalized. The function
            // itself is not installed - the error below keeps it interpreted - so
            // the stub only has to be harmless: it hands the call to the
            // interpreter from the first instruction.
            if spec_compiled {
                if let Err(stub) = self.define_stub(inner_id, StubBody::InterpretFromStart) {
                    log::debug!(target: "jit", "{inner_name}: stub not defined either: {stub}");
                }
            }
            return Err(e);
        }

        self.defined.insert(inner_name.clone());

        // let asm = self.ctx.compiled_code().map(|x| x.vcode.as_ref()).flatten();
        // if let Some(asm) = asm {
        //     println!("{}", asm);
        // }

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
                // Give up on the profiling output rather than going down mid compile
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

    /// Compile and define the specialized copy, returning its code size. It
    /// must end up defined no matter what - the generic copy already tail calls
    /// it - so a failure, including a panic in the translator, defines a stub
    /// that tail calls straight back into the generic copy instead. A panic that
    /// escaped would also poison the jit's shared mutex.
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
        non_mutable_globals: &HashSet<usize>,
        seed: Vec<(usize, SpecType)>,
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
                    non_mutable_globals,
                    mode,
                )
            }))
        };

        let mut translated = translate_with(self, None);

        // A recursive function's direct self calls return whatever this copy
        // returns, which is circular: in a first translation those results are
        // untyped, so everything built from them is too, and its returns cannot
        // suggest the type. Guess from the seed instead - a function specialized
        // on fixnums most likely returns one - and translate again with direct
        // self call results assumed to have that type. The assumption is sound
        // exactly when every return of that translation has the type and nothing
        // else leaves the function except deopt exits (whose callers check before
        // using the value), so it is kept only then.
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
                    eprintln!("[spec] {spec_name}: with the assumption: returned {:?}, direct self calls {}, untyped exit {}", info.returned, info.direct_self_calls, info.untyped_non_deopt_exit);
                }
            }
            let holds = matches!(&second, Ok(Ok(info))
                if !info.untyped_non_deopt_exit
                    && info.returned.iter().all(|t| t.boxed() == assumed));
            if spec_debug_enabled() {
                eprintln!("[spec] {spec_name}: assumed self calls return {assumed:?}: {}", if holds { "kept" } else { "rejected" });
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

        let failure = match translated {
            Ok(Ok(_)) => {
                match cranelift::codegen::verify_function(&self.ctx.func, self.module.isa()) {
                    Ok(()) => match self.module.define_function(spec_id, &mut self.ctx) {
                        Ok(()) => None,
                        Err(e) => Some(format!("define: {e}")),
                    },
                    Err(e) => {
                        if std::env::var("STEEL_JIT_DUMP_CLIF").as_deref() == Ok("1") {
                            eprintln!("--- clif for failed specialized copy ---\n{}", self.ctx.func);
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
            // Never called - the generic copy gets no guard - but it is declared,
            // so it has to be defined.
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
    /// when the code arena is full, and that must not panic: the panic would
    /// happen with the shared jit locked.
    fn define_stub(&mut self, func_id: FuncId, body: StubBody) -> Result<usize, String> {
        // The translator may have been torn down mid block.
        self.module.clear_context(&mut self.ctx);
        self.builder_context = FunctionBuilderContext::new();
        self.init_jit_signature();
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
        non_mutable_globals: &HashSet<usize>,
        spec_mode: SpecMode,
    ) -> Result<TranslateInfo, String> {
        // println!("----- Compiling function ----");

        // pretty_print_dense_instructions(bytecode);

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
        // first block param. cranelift 0.135 dropped the global_value instruction
        // that used to materialize it.
        let vm_context = builder.block_params(entry_block)[0];

        // The generic copy of a specialized loop checks its seeded slots on the
        // way in and hands the whole call to the specialized copy, first
        // iteration included. Checking at the tail call instead ran the first
        // iteration generically every call - most of the work in a short loop -
        // and repeated a failing check on every iteration of a loop whose slots
        // were never fixnums.
        if let SpecMode::Generic { spec_id, seed } = &spec_mode {
            emit_spec_entry_guard(&mut builder, &mut self.module, vm_context, *spec_id, seed, arity);
        }

        let fake_entry_block = if contains_tail_call {
            let fake_entry = builder.create_block();
            builder.ins().jump(fake_entry, &[]);
            builder.switch_to_block(fake_entry);

            Some(fake_entry)
        } else {
            None
        };

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
            pending_deopt_exits: Vec::new(),
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
            non_mutable_globals,
            names: &self.names,
            function_return_types: &self.function_return_types,
            exit_types: &mut exit_types,
            typed_returns: HashSet::new(),
            potentially_could_deopt: false,
            tier: JitTier::Baseline,
            thread_pointer: None,
            should_trampoline: None,
            sp: None,
            pop_count: None,
            pop_count_plus_one: None,
            pop_count_minus_one: None,
            compilation_stats: CompilationStats::default(),
            thread_id: None,
            use_lbbv: std::env::var("STEEL_LBBV").is_ok(),
            known_tags: HashMap::new(),
            spec_mode: spec_mode.clone(),
            deopt_returns: HashSet::new(),
            direct_self_calls: 0,
        };

        {
            let vm_ctx = trans.get_ctx();
            trans.get_thread_pointer(vm_ctx);
            trans.get_thread_id();
        }

        match &spec_mode {
            // Guarded: the generic copy only tail calls in with these slots
            // holding these types, and this copy only loops on itself while
            // they still do.
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

        trans.stack_to_ssa();

        if let Some(fake_entry_block) = fake_entry_block {
            trans.builder.seal_block(fake_entry_block);
        }

        // trans.builder.switch_to_block(exit_block);

        let void = trans.encode_void();

        trans.builder.ins().return_(&[void]);

        trans.builder.seal_block(exit_block);

        // Cold exits last - see `defer_deopt_exit`. After the body's own return,
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
        trans.builder.finalize(frontend_config);

        /*
        if !trans.potentially_could_deopt {
            println!("Found a candidate for tier 2 compilation: {}", trans.name);
        }

        println!("Stats: {:#?}", trans.compilation_stats);
        */

        let returned = exit_types.clone();

        // exit_types only sees the POPPURE returns; a deopt returns void.
        if deopts {
            exit_types.insert(InferredType::Void);
        }

        // Callers trust a single recorded return type (a call whose callee only
        // ever returned `Bool` is branched on without a tag check), so every
        // other way out has to count. A tail call, the memq shortcut or a
        // speculative exit returns a value nothing typed; before this, a
        // function like `(if (p x) #t (g x))` was recorded as returning `Bool`.
        if untyped_exit {
            exit_types.insert(InferredType::Any);
        }

        // The specialized copy shares the generic copy's id; callers only ever
        // reach the generic copy, so its exits are the ones that describe a call.
        if !matches!(spec_mode, SpecMode::Specialized { .. }) {
            self.function_return_types.insert(id, exit_types);
        }

        Ok(TranslateInfo {
            returned,
            untyped_non_deopt_exit,
            direct_self_calls,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum InferredType {
    // A bare i64 payload, untagged. `as_steelval` rebuilds the fixnum tag at
    // whatever boundary the value escapes through.
    Int64,

    // A boxed value carrying the fixnum tag (`SteelVal::IntV`) - never a
    // bignum. The arithmetic and comparison fast arms rely on this: they unbox
    // an `Int` operand without checking its tag, and `known_tags` folds its
    // tag checks away. So only produce it for values that are fixnums by
    // construction (today: fixnum constants). An operation that can overflow
    // into a bignum may only produce `Int` if its overflow path deopts.
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

impl InferredType {
    /// What this value's type becomes once it has been materialized as a
    /// `SteelVal`.
    ///
    /// The untagged types describe how a value is being *carried*, not what it
    /// is, so they must not survive materialization - a slot recorded as
    /// `Int64` after the tagged value was written to it would be tagged a second
    /// time on the next read.
    /// The `SteelVal` tag a value of this type is guaranteed to carry, for the
    /// types whose producers only ever make that variant. `Number`, `Any` and
    /// the list-or-pair family say nothing about the tag.
    ///
    /// Not `Bool`, `Char` or `Void`: helpers are typed `Bool` on paths that can
    /// still return an error marker, `Void` doubles as "moved out", and nothing
    /// gains enough from folding those checks to be worth auditing every one.
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

    /// Carries no reference: overwriting or popping it needs no drop.
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

    fn boxed(self) -> Self {
        match self {
            // Number, not Int: `Int` is load bearing in the dispatch guards,
            // which match on it and then assume the operand is a value or a
            // constant rather than a register. `Number` is what this operation
            // reported before it carried the result untagged.
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
            // A bare i64 payload known to fit - the tag is implied by the type,
            // so it is rebuilt here rather than occupying a register.
            InferredType::Int64 => {
                let boxed = ctx.encode_value(SteelVal::INT_TAG as i64, self.value);
                ctx.known_tags.insert(boxed, SteelVal::INT_TAG);
                boxed
            }
            _ => self.value,
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
            ConstantValue::Index(_) => InferredType::Any,
            ConstantValue::Symbol(_) => InferredType::Symbol,
            ConstantValue::List(_) => InferredType::List,
        }
    }

    fn to_value(self, ctx: &mut FunctionTranslator) -> (Value, InferredType) {
        match self {
            // TODO: We can probably infer the type here though, since we know
            // what the type is based on the values coming in
            ConstantValue::Index(p) => (ctx.push_const_index(p), InferredType::Any),

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
                // let value = ctx.create_i128(encode(self.as_steelval()));
                // let value = ctx.encode_void();
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

    // A value read out of something that is still alive - a list in a local, a
    // box in a struct in a local - without being cloned. Only a few operations
    // that neither keep nor free their operand, like `eq?`, can use it as is;
    // before any other operation it is materialized into a `Value`. See
    // `materialize_borrowed`.
    Borrowed(BorrowedValue),
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct BorrowedValue {
    value: Value,
    // An i8 variable: 1 when the value was cloned after all (a slow path took
    // it) and so is owned, 0 when it is only borrowed.
    owned: Variable,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum ValueOrRegister {
    Value(Value),
    Register(usize),
}

#[derive(Default, Clone)]
struct CachedLookupMap {
    registers: HashMap<usize, Value>,

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
}



impl PropertyMap {
    // Keep only what both branches agree on; anything else is unknown here.
    pub fn meet(&mut self, other: &PropertyMap) {
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

        self.cached_lookups.stack_length_capacity = self
            .cached_lookups
            .stack_length_capacity
            .min(other.cached_lookups.stack_length_capacity);

        if self.cached_lookups.stack_buf_pointer != other.cached_lookups.stack_buf_pointer {
            self.cached_lookups.stack_buf_pointer = None;
        }
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
            // println!("Adding property: {:?} to exists: {:?}", prop, exists);
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
                _ => {
                    // Coalesce properties on push. In the event there are properties
                    // that are related, we should infer things about them here.
                    exists.push(prop);
                }
            }

            // println!("Results: {:?}", exists);
        } else {
            self.props.insert(value, vec![prop]);
        }
    }

    // This is how we can infer a property?
    pub fn infer_property_bool(&mut self, condition_value: Value, branch: bool) {
        if let Some(props) = self.props.get(&ValueOrRegister::Value(condition_value)) {
            for prop in props {
                match prop {
                    Properties::CheckedString(value_or_register) => {
                        // todo
                    }
                    Properties::CheckedList(value_or_register) => {
                        if branch {
                            // println!("Adding proper list at branch");
                            self.add_property(*value_or_register, Properties::ProperList)
                        } else {
                            // Mark as not a list? Is that even worth checking?
                            self.add_property(*value_or_register, Properties::NotAList)
                        }
                        break;
                    }
                    Properties::CheckedPair(value_or_register) => {
                        // todo
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
    CheckedString(ValueOrRegister),
    CheckedList(ValueOrRegister),
    CheckedPair(ValueOrRegister),
    CheckedNull(ValueOrRegister),

    ProperList,

    ProperNonEmptyList,

    InferredType(InferredType),

    // Encode the property that this thing
    // is a certain range.
    ConditionGreaterThan(ValueOrRegister, i64),
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
            Self::Borrowed(b) => StackValue { value: ctx.materialize(b), inferred_type: InferredType::Any, spilled: false },
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

    fn into_constant_int(self, ctx: &mut FunctionTranslator) -> Option<isize> {
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

#[derive(Copy, Clone, PartialEq, Eq, Default)]
enum JitTier {
    #[default]
    Baseline,
    Tier2,
}

#[derive(Default, Clone, Debug)]
struct CompilationStats {
    stack_frame_pushes: usize,
    max_stack_size: usize,
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

    // Speculative exits, filled once the body is done. Emitting them inline puts
    // their loads ahead of the fast path they branch away from, and those values
    // then do not dominate it - cranelift reports that as a non-dominating use.
    pending_deopt_exits: Vec<(Block, Vec<MaybeStackValue>, usize)>,
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

    /// Global indices the compiler proved are never `set!`. Only such a global
    /// may have its *value* baked into generated code.
    non_mutable_globals: &'a HashSet<usize>,

    names: &'a HashMap<u32, String>,

    function_return_types: &'a HashMap<u32, HashSet<InferredType>>,
    exit_types: &'a mut HashSet<InferredType>,
    // The `return` instructions whose value's type went into `exit_types`.
    // Any other reachable exit makes the function's result type unknown.
    typed_returns: HashSet<cranelift::codegen::ir::Inst>,

    potentially_could_deopt: bool,

    tier: JitTier,

    thread_pointer: Option<Value>,

    should_trampoline: Option<Value>,

    sp: Option<Value>,

    pop_count: Option<Value>,

    pop_count_plus_one: Option<Value>,
    pop_count_minus_one: Option<Value>,

    thread_id: Option<Value>,

    compilation_stats: CompilationStats,

    use_lbbv: bool,
    // The tag an SSA value is known to carry. An SSA value never changes, so the
    // fact holds at every use. Tag checks on these fold to a constant and
    // `converging_if` only emits the arm the constant selects.
    known_tags: HashMap<Value, u8>,
    // Loop specialization role of this translation; see `SpecMode`.
    spec_mode: SpecMode,
    // Returns emitted by deopt exits: they hand the call to the interpreter, so
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

fn op_to_name_payload(op: OpCode, payload: usize) -> &'static str {
    try_op_to_name_payload(op, payload).unwrap_or_else(|| {
        panic!(
            "couldn't match the name for the op code + payload: {:?}",
            (op, payload)
        )
    })
}

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

fn try_op_to_name_payload(op: OpCode, payload: usize) -> Option<&'static str> {
    Some(match (op, payload) {
        (OpCode::IF, _) => "if-branch-value",
        (OpCode::CALLGLOBAL, _) => "call-global",
        (OpCode::PUSHCONST, _) => "push-const",
        (OpCode::READLOCAL0, _) => "read-local-0",
        (OpCode::READLOCAL1, _) => "read-local-1",
        (OpCode::READLOCAL2, _) => "read-local-2",
        (OpCode::READLOCAL3, _) => "read-local-3",
        (OpCode::READLOCAL, _) => "read-local-any",
        (OpCode::READCAPTURED, _) => "read-captured",
        (OpCode::MOVEREADLOCAL0, _) => "move-read-local-0",
        (OpCode::MOVEREADLOCAL1, _) => "move-read-local-1",
        (OpCode::MOVEREADLOCAL2, _) => "move-read-local-2",
        (OpCode::MOVEREADLOCAL3, _) => "move-read-local-3",
        (OpCode::MOVEREADLOCAL, _) => "move-read-local-any",
        (OpCode::ADD, 2) => "add-binop",
        (OpCode::ADD, 3) => "add-three",
        (OpCode::ADD, 4) => "add-four",
        (OpCode::SUB, 2) => "sub-binop",
        (OpCode::SUB, 3) => "sub-three",
        (OpCode::SUB, 1) => "sub-negate",
        (OpCode::LT, 2) => "lt-binop",
        (OpCode::LT, 3) => "lt-three",
        (OpCode::LTE, 2) => "lte-binop",
        (OpCode::LTE, 3) => "lte-three",
        (OpCode::GT, 2) => "gt-binop",
        (OpCode::GT, 3) => "gt-three",
        (OpCode::GTE, 2) => "gte-binop",
        (OpCode::GTE, 3) => "gte-three",
        (OpCode::MUL, 2) => "mult-two",
        (OpCode::MUL, 3) => "mult-three",
        (OpCode::DIV, 2) => "div-two",
        (OpCode::DIV, 1) => "div-one",
        (OpCode::PUSH, _) => "push-global-value",
        (OpCode::NOT, _) => "not-value",
        (OpCode::NUMEQUAL, 2) => "num-equal-value-bool",
        (OpCode::EQUAL2, _) => "equal-binop-bool",
        (OpCode::EQUAL, _) => "equal-binop-bool",
        (OpCode::CAR, _) => "car-handler-value",
        (OpCode::CDR, _) => "cdr-handler-value",
        (OpCode::CONS, _) => "cons-handler-value",
        (OpCode::NEWBOX, _) => "box-handler",
        (OpCode::UNBOX, _) => "unbox-handler",
        (OpCode::SETBOX, _) => "set-box-handler",
        (OpCode::LISTREF, _) => "list-ref-value",
        (OpCode::VECTORREF, _) => "vector-ref-value",

        _ => return None,
    })
}

impl FunctionTranslator<'_> {
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
            // At every point, we're
            self.record_stack_size();

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

            match op {
                OpCode::LOADINT1POP | OpCode::BINOPADDTAIL => {
                    todo!("{:?}", op);
                }

                OpCode::SCLOSURE => {
                    panic!("Deprecated opcode");
                }
                OpCode::POPPURE => {
                    self.maybe_check_last();
                    let (value, ty) = self.shadow_pop();

                    // Whatever the exit type is, record it, if
                    // we have it.
                    self.exit_types.insert(ty);

                    self.spill_cloned_stack();
                    let real_res = self.vm_pop(value);

                    // if we hit this, the value should not end up getting read?
                    let ret = self.builder.ins().return_(&[real_res]);
                    self.typed_returns.insert(ret);

                    let cold_block = self.builder.create_block();
                    self.builder.switch_to_block(cold_block);

                    self.ip = self.instructions.len() + 1;
                    self.depth -= 1;

                    return false;
                }
                OpCode::VOID => {
                    let value = self.encode_void();
                    self.push(value, InferredType::Void);
                    self.ip += 1;
                }
                OpCode::PUSH => {
                    if USE_INLINE_PUSH_GLOBAL {
                        let result = self.inline_lookup_global(payload);

                        // Clone the resulting value, then move on
                        self.clone_value(result);

                        // TODO: If we know the type of a global
                        // and we also know that the value is immutable, then
                        // we should be able to fuss with it
                        self.push(result, InferredType::Any);

                        self.ip += 1;
                    } else {
                        // Let value to push:
                        let index = self
                            .builder
                            .ins()
                            .iconst(Type::int(64).unwrap(), payload as i64);

                        let function_name = op_to_name_payload(op, payload);

                        let result = self.call_function_returns_value_args(function_name, &[index]);

                        // Check the inferred type, if we know of it
                        self.push(result, InferredType::Any);

                        self.ip += 1;
                    }
                }

                // TODO: Still adjust the ip as needed
                OpCode::IF => {
                    // If we can check the local variable on the stack, we should do that
                    if matches!(
                        self.shadow_stack.last(),
                        Some(MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_))
                    ) {
                        let test = self.shadow_stack_pop().unwrap();

                        let false_instr = self.instructions[self.ip].payload_size;
                        let true_instr = self.ip + 1;

                        let test_bool = self.call_test_handler_register(test.into_index());

                        self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());

                        // self.push(res, InferredType::Any);
                    } else {
                        let last_ref = self
                            .shadow_stack
                            .last()
                            .copied()
                            .and_then(|x| x.as_value(self));

                        if last_ref.map(|x| x.inferred_type) == Some(InferredType::UnboxedBool) {
                            let false_instr = self.instructions[self.ip].payload_size;
                            let true_instr = self.ip + 1;

                            // Explicitly want the unboxed value here
                            let test_bool = last_ref.unwrap().value;

                            // dbg!(self.builder.func.dfg.value_type(test_bool));

                            self.shadow_stack_pop();

                            self.translate_if_else_value(
                                test_bool,
                                true_instr,
                                false_instr.to_usize(),
                            );

                            // self.push(res, InferredType::Any);
                        } else {
                            // TODO: Type inference here! Change which function is called!
                            let (test, typ) = self.shadow_pop();

                            let false_instr = self.instructions[self.ip].payload_size;
                            let true_instr = self.ip + 1;

                            let test_bool = match typ {
                                InferredType::Bool => {
                                    let amount_to_shift =
                                        self.builder.ins().iconst(Type::int(64).unwrap(), 64);
                                    let shift_right =
                                        self.builder.ins().sshr(test, amount_to_shift);

                                    // Do we need to do this at all?
                                    self.builder
                                        .ins()
                                        .ireduce(Type::int(8).unwrap(), shift_right)
                                }
                                InferredType::List => {
                                    // self.drop_tagged_value(test);

                                    self.drop_biased_rc(test);

                                    self.builder.ins().iconst(types::I8, 1)
                                }
                                _ => {
                                    let is_bool = self.is_type(test, SteelVal::BOOL_TAG);
                                    let amount_to_shift =
                                        self.builder.ins().iconst(Type::int(64).unwrap(), 64);

                                    let shift_right =
                                        self.builder.ins().sshr(test, amount_to_shift);

                                    let small = self
                                        .builder
                                        .ins()
                                        .ireduce(Type::int(8).unwrap(), shift_right);

                                    let is_false =
                                        self.builder.ins().icmp_imm_s(IntCC::Equal, small, 0);

                                    // Is bool and is false:
                                    let overall = self.builder.ins().band(is_bool, is_false);
                                    let is_truthy = self.builder.ins().bxor_imm_u(overall, 1);

                                    self.drop_tagged_value(test);

                                    is_truthy
                                }
                            };

                            self.translate_if_else_value(
                                test_bool,
                                true_instr,
                                false_instr.to_usize(),
                            );
                        }
                    }
                }
                OpCode::JMP | OpCode::POPJMP => {
                    // println!("Jumping from {} -> {}", self.ip, payload);
                    assert!(payload > self.ip);
                    self.ip = payload;
                }
                // Call func... lets see how this goes...
                OpCode::FUNC | OpCode::FUNCNOARITY => {
                    let arity = payload;
                    let name = CallFunctionDefinitions::arity_to_name(arity);
                    self.ip += 1;

                    self.potentially_could_deopt = true;

                    // TODO: @Matt
                    // Lets do a hierarchical check. In the event this is a
                    // closure, we can fast path calling the function ourselves
                    // with the function calling prelude like what we use
                    // otherwise.
                    //
                    // The only thing we're missing here is how to construct
                    // a fat pointer from the closure itself. In this case,
                    // I think we just have to pair the pointer to the data
                    // + the length, and we'll be good to go.

                    match self.shadow_stack.last() {
                        // We can clone the value off the register in order
                        // to push this value on to the stack. Once we pull
                        // it off the register, we then run _clone_ on this,
                        // to make sure that its accessible from the child.
                        //
                        // Then, we'll check that it has a super instruction
                        // and that our depth isn't too large.
                        Some(MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i))
                            if USE_INLINE_CALL_FUNC =>
                        {
                            // Check the type:
                            let func = self.read_from_vm_stack(*i);
                            self.inline_call_func(arity, name, func, true, true);
                        }

                        Some(MaybeStackValue::Value(StackValue { value, .. }))
                            if USE_INLINE_CALL_FUNC =>
                        {
                            self.inline_call_func(arity, name, *value, false, true);
                        }

                        _ => {
                            if let Some(name) = name {
                                let v = self.call_function(arity, name, false);
                                self.push(v, InferredType::Any);
                            } else {
                                todo!("Implement spilled function call (arity {})", arity);
                            }
                        }
                    }

                    self.check_deopt();
                }

                // TODO: Revisit this!
                OpCode::TAILCALL | OpCode::TAILCALLNOARITY => {
                    let arity = payload;
                    let name = CallFunctionTailDefinitions::arity_to_name(arity);
                    self.ip += 1;

                    self.potentially_could_deopt = true;

                    match self.shadow_stack.last().copied() {
                        Some(MaybeStackValue::MutRegister(i)) if USE_INLINE_LOCAL_TAIL_CALL => {
                            // Remove the register argument
                            self.shadow_stack_pop();

                            // We don't need to clone, because we've just read it from the stack.
                            let value = self.remove_from_vm_stack(i);

                            let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);

                            // Capture what the stack is before hand, we'll need this
                            let old_stack = self.shadow_stack.clone();

                            self.converging_if_no_value(
                                is_closure,
                                |ctx| {
                                    let closure = ctx.unbox_value_to_pointer(value);
                                    ctx.inline_local_tail_call(arity, closure, value);
                                },
                                |ctx| {
                                    ctx.shadow_stack = old_stack.clone();

                                    // Lets call the function:
                                    if let Some(name) = name {
                                        let v =
                                            ctx.call_function_with_func(arity, name, true, value);
                                        ctx.push(v, InferredType::Any);
                                    } else {
                                        todo!("Implement spilled function call bail out case (arity {})", arity);
                                    }
                                },
                            );

                            self.ip = self.instructions.len() + 1;
                            self.check_deopt();
                            self.depth -= 1;
                        }

                        Some(MaybeStackValue::Value(StackValue { value, .. }))
                            if USE_INLINE_LOCAL_TAIL_CALL =>
                        {
                            // Remove the register argument
                            self.shadow_stack_pop();

                            let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);

                            // Capture what the stack is before hand, we'll need this
                            let old_stack = self.shadow_stack.clone();

                            self.converging_if_no_value(
                                is_closure,
                                |ctx| {
                                    let closure = ctx.unbox_value_to_pointer(value);
                                    ctx.inline_local_tail_call(arity, closure, value);
                                },
                                |ctx| {
                                    ctx.shadow_stack = old_stack.clone();

                                    // Lets call the function:
                                    if let Some(name) = name {
                                        let v =
                                            ctx.call_function_with_func(arity, name, true, value);
                                        ctx.push(v, InferredType::Any);
                                    } else {
                                        todo!("Implement spilled function call bail out case (arity {})", arity);
                                    }
                                },
                            );

                            self.ip = self.instructions.len() + 1;
                            self.check_deopt();
                            self.depth -= 1;
                        }

                        Some(MaybeStackValue::Register(i)) if USE_INLINE_LOCAL_TAIL_CALL => {
                            // Remove the register argument
                            self.shadow_stack_pop();

                            // We don't need to clone, because we've just read it from the stack.
                            let value = self.read_from_vm_stack(i);

                            let is_closure = self.is_type(value, SteelVal::CLOSURE_TAG);

                            // Capture what the stack is before hand, we'll need this
                            let old_stack = self.shadow_stack.clone();

                            self.converging_if_no_value(
                                is_closure,
                                |ctx| {
                                    let closure = ctx.unbox_value_to_pointer(value);
                                    ctx.increment_ref_count_closure(closure);
                                    ctx.inline_local_tail_call(arity, closure, value);
                                },
                                |ctx| {
                                    ctx.shadow_stack = old_stack.clone();
                                    ctx.clone_value(value);
                                    // Lets call the function:
                                    if let Some(name) = name {
                                        let v =
                                            ctx.call_function_with_func(arity, name, true, value);
                                        ctx.push(v, InferredType::Any);
                                    } else {
                                        todo!("Implement spilled function call bail out case (arity {})", arity);
                                    }
                                },
                            );

                            self.ip = self.instructions.len() + 1;
                            self.check_deopt();
                            self.depth -= 1;
                        }

                        _ => {
                            if let Some(name) = name {
                                let v = self.call_function(arity, name, true);
                                self.push(v, InferredType::Any);
                            } else {
                                todo!("Implement spilled function call (arity {})", arity);
                            }

                            self.ip = self.instructions.len() + 1;

                            self.check_deopt();

                            self.depth -= 1;
                        }
                    }

                    return false;
                }
                OpCode::NEWSCLOSURE => {
                    let ip = self.ip;
                    let offset = payload;
                    self.ip += payload + 1;

                    assert_eq!(self.instructions[self.ip - 1].op_code, OpCode::ECLOSURE);
                    assert!(self.ip < self.instructions.len());

                    // println!("Instruction after newsclosure: {}", self.ip);

                    let ip_value = self.builder.ins().iconst(Type::int(64).unwrap(), ip as i64);
                    let offset_value = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), offset as i64);

                    // code gen the sclosure creation:
                    let v = self
                        .call_function_returns_value_args("new-closure", &[ip_value, offset_value]);

                    self.push(v, InferredType::BytecodeFunction);
                }

                // Something is up here - we don't want to do this!
                OpCode::PUREFUNC => {
                    let ip = self.ip;
                    let offset = payload;

                    self.ip += 1;
                    self.ip += 1;
                    self.ip += 1;

                    let forward_jump = offset - 2;
                    let forward_index = self.ip + forward_jump;

                    self.ip = forward_index;

                    // println!("instruction after {} -> {}", ip, forward_index);

                    let ip_value = self.builder.ins().iconst(Type::int(64).unwrap(), ip as i64);
                    let offset_value = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), offset as i64);

                    // code gen the sclosure creation:
                    let v = self
                        .call_function_returns_value_args("pure-func", &[ip_value, offset_value]);

                    self.push(v, InferredType::BytecodeFunction);
                }

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
                OpCode::PANIC => todo!(),
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
                OpCode::PUSHCONST => {
                    let payload = self.instructions[self.ip].payload_size.to_usize();
                    // let (value, typ) = self.get_const(op, payload);

                    let constant = self.constants.get(payload);

                    let encoded = match constant {
                        SteelVal::NumV(n) => ConstantValue::Float(n),
                        SteelVal::IntV(i) => ConstantValue::Int(i),
                        SteelVal::BoolV(b) => ConstantValue::Bool(b),
                        SteelVal::CharV(c) => ConstantValue::Char(c),
                        SteelVal::ListV(_) => ConstantValue::List(payload),
                        SteelVal::SymbolV(_) => ConstantValue::Symbol(payload),
                        _ => ConstantValue::Index(payload),
                    };

                    self.shadow_push(MaybeStackValue::Constant(encoded));
                    self.ip += 1;
                }
                OpCode::TRUE => {
                    let value = self.encode_true();
                    self.ip += 1;
                    // self.advance_ip();
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

                OpCode::LetVar => {
                    self.ip += 1;

                    // Instrument with calls for profiling in general
                    // self.call_function_args_no_context("log-let-var", &[]);

                    self.maybe_check_last();

                    let (last, typ) = self.shadow_pop();

                    // All enclosing scopes, not just this one: `let*` nests, and
                    // both read paths index with `let_var_stack.iter().sum()`. Using
                    // `last()` here agrees only while the stack is one deep, so the
                    // inner binding of a `let*` collided with the outer one.
                    // The slot `push_to_vm_stack_let_var_new` puts the value in:
                    // after the arguments, the enclosing let bindings, and any
                    // pending operands already spilled onto the vm stack below
                    // this one. Leaving out the spilled operands recorded the
                    // binding's type against the slot of the operand beneath it -
                    // e.g. the first half of `(+ (let ...) (let ...))` - so the
                    // binding lost its type and that operand gained a wrong one.
                    let spilled_below = self
                        .shadow_stack
                        .iter()
                        .filter(|e| matches!(e, MaybeStackValue::Value(v) if v.spilled))
                        .count();
                    let local_index = self.let_var_stack.iter().sum::<usize>()
                        + self.arity as usize
                        + spilled_below;
                    self.value_to_local_map.insert(last, local_index);
                    self.local_to_value_map.insert(local_index, typ);

                    *self.let_var_stack.last_mut().unwrap() += 1;

                    match typ {
                        InferredType::List => {
                            // println!("Adding proper list at let var: {}", local_index);
                            self.properties.set_property(
                                ValueOrRegister::Register(local_index),
                                Properties::ProperList,
                            );
                        }
                        // Nothing to record, and writing it would only mask a
                        // better fact established later in the scope.
                        InferredType::Any => {
                            self.properties
                                .remove(&ValueOrRegister::Register(local_index));
                        }
                        // Carry the binding's type onto the slot. Without this
                        // only `List` survived, so `immutable_register_to_value`
                        // reported `Any` for every other let-bound register and
                        // the arithmetic fast arms re-checked a tag we already
                        // knew. `LETENDSCOPE` clears these when the scope ends
                        // and `SETLOCAL` clears them on mutation.
                        typ => {
                            self.properties.set_property(
                                ValueOrRegister::Register(local_index),
                                Properties::InferredType(typ),
                            );
                        }
                    }

                    // Caching the let var here would let `read_from_vm_stack`
                    // skip the reload, which is where a lot of the spill/reload
                    // cost lives. It still MISCOMPILES, so this is opt-in.
                    //
                    // What is already ruled out: it is not the loop back-edge, and
                    // not branch merging - the smallest failing case has no `if` at
                    // all. `PropertyMap::meet` is the only join and LETENDSCOPE now
                    // purges the scope's slots.
                    //
                    // Smallest reproducer:
                    //   (let ((q (quotient x y)))
                    //     (let ((r (- x (* q y))))
                    //       (+ q r)))
                    // The trigger is an inner binding whose RHS reads the OUTER let
                    // var while the body reads it too; make `r` independent of `q`
                    // and it passes. Note r5rs/r7rs/syntax all pass with this on, so
                    // the suites do not cover it - use the reproducer.
                    // Deliberately not cached here. `local_index` counts let vars
                    // only, so it misses operands spilled to the value stack across
                    // a scope boundary - an inlined call under a pending `+` lands
                    // its bindings one slot higher than this computes. The cache is
                    // populated on read instead, where the index comes from the
                    // bytecode and is always right.

                    // TODO: @mparas - in the event we're using a local value,
                    // we need to check if this is actually spilled or not.
                    //
                    // It shouldn't be though
                    self.push_to_vm_stack_let_var_new(last);
                }
                OpCode::READLOCAL0
                | OpCode::READLOCAL1
                | OpCode::READLOCAL2
                | OpCode::READLOCAL3
                | OpCode::MOVEREADLOCAL0
                | OpCode::MOVEREADLOCAL1
                | OpCode::MOVEREADLOCAL2
                | OpCode::MOVEREADLOCAL3 => {
                    // let (value, inferred_type) = self.read_local_fixed(op, payload);
                    let value = self.spilled_read_local_fixed(op, payload);
                    self.ip += 1;
                    // self.push(value, inferred_type);
                    self.shadow_push(value);
                }
                // Set local is totally fair game and should be adjusted here:
                OpCode::SETLOCAL => {
                    let index = self.register_index(payload);
                    let (value, _) = self.shadow_pop();

                    // The slot's type is whatever `LetVar` recorded when it was
                    // bound, and this overwrites the slot. Both maps have to go:
                    // `local_to_value_map` feeds the dispatch guards and
                    // `properties` feeds `immutable_register_to_value`. Benign
                    // while every fast arm still tag-checks, but not once a known
                    // type is allowed to skip one.
                    let slot = payload as usize;
                    self.local_to_value_map.remove(&slot);
                    self.properties.remove(&ValueOrRegister::Register(slot));
                    self.properties.cached_lookups.registers.remove(&slot);

                    let value =
                        self.call_function_returns_value_args("set-local-any", &[index, value]);
                    self.push(value, InferredType::Any);
                    self.ip += 1;
                }

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
                    // let _ = self.translate_tco_jmp_no_arity_loop_no_spill(payload);

                    self.ip = self.instructions.len() + 1;

                    self.depth -= 1;

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
                    // let _ = self.translate_tco_jmp_no_arity_loop_no_spill(payload);
                    let _ = self.translate_tco_jmp_no_arity_loop_no_spill(payload);
                    //
                    // TODO: Move back to using loop?
                    // let _ = self.translate_tco_jmp_no_arity_without_spill(payload);

                    // let _ = self._translate_tco_jmp_no_arity(payload);

                    // self.translate_tco_jmp(payload);
                    // Jump to out of bounds so signal we're done
                    self.ip = self.instructions.len() + 1;

                    self.depth -= 1;

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
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();

                    let func = self._globals.get(function_index).cloned();

                    // TODO: For calling struct operations, then we'll
                    // just call the thing, and we can move on to the pop
                    // I think.
                    let maybe_global = self._globals.get(function_index).cloned();
                    if let Some(maybe_global) = maybe_global {
                        match maybe_global {
                            SteelVal::FuncV(f) if f == steel_memq && arity == 2 => {
                                let res = self.memq();

                                // Spilling the cloned stack: Coalesce the reads to avoid
                                // repeated lookups on the buf pointer. At this point
                                // the buf pointer is the same, so we can lift it up.
                                self.spill_cloned_stack();

                                // Try this out?
                                let real_res = self.inline_handle_pop(res);

                                // TODO: Deal with this new return value
                                self.builder.ins().return_(&[real_res]);

                                let cold_block = self.builder.create_block();
                                self.builder.switch_to_block(cold_block);

                                self.exit_types.insert(InferredType::List);
                                self.exit_types.insert(InferredType::Bool);

                                self.depth -= 1;
                                self.ip = self.instructions.len() + 1;
                                return false;
                            }

                            SteelVal::MutFunc(f) if f == steel_reverse && arity == 1 => {
                                // let value = self.shadow_stack.pop().unwrap();

                                // match value {
                                //     MaybeStackValue::Value(stack_value) => {
                                //         todo!()
                                //     },

                                //     // Just leave it in place, spill the cloned stack though
                                //     MaybeStackValue::MutRegister(_) => {
                                //     },
                                //     MaybeStackValue::Register(_) => {

                                //         todo!()

                                //     },
                                //     MaybeStackValue::Constant(constant_value) => {},
                                // }
                            }

                            _ => {}
                        }

                        if INLINE_STRUCT_FUNCTION_TAIL_CALLS {
                            // TODO: @Matt -> This is the issue, something is
                            // wrong with the way I'm reading this, or doing something.
                            if let Some(spec) = create_struct_spec(maybe_global) {
                                // TODO: This is where we inline the calls for struct
                                // functions
                                if let Some((value, typ)) =
                                    self.inline_struct_call_no_drop(spec, arity, function_index)
                                {
                                    let value = self.take_struct_result(value);
                                    self.spill_cloned_stack();
                                    let real_res = self.inline_handle_pop(value);

                                    // TODO: Deal with this new return value!
                                    self.builder.ins().return_(&[real_res]);

                                    let cold_block = self.builder.create_block();
                                    self.builder.switch_to_block(cold_block);

                                    self.depth -= 1;
                                    self.ip = self.instructions.len() + 1;
                                    return false;
                                }
                            }
                        }
                    }

                    // Call direct, by hard coding this, and we're gonna check that the
                    // instructions exist already...
                    if use_inline_global_tail_call() && matches!(func, Some(SteelVal::Closure(_))) {
                        let function = if let Some(SteelVal::Closure(v)) = func {
                            v
                        } else {
                            unreachable!();
                        };

                        // Take this fast path if the super instructions already exists.
                        // Then we can do a direct call.
                        //
                        // We bake *this particular* `Gc<ByteCodeLambda>` into the generated
                        // code, so this is only sound when the global can never come to hold
                        // anything else - which is what the analysis records in
                        // `reified_non_mutable`. Captures are how this surfaced, but they're
                        // the wrong test: a `set!` to a different capture-free function breaks
                        // it just as badly, and a capturing closure in a never-mutated global
                        // is perfectly fine to bake.
                        //
                        // Normally unreachable, since callee globals are usually still unbound
                        // when a caller is compiled at construction time; tier-up is the first
                        // thing to resolve them.
                        if function.super_instructions().is_some()
                            && self.non_mutable_globals.contains(&function_index)
                        {
                            if function.tier2.is_none() {
                                self.potentially_could_deopt = true;
                            }

                            self.inline_global_tail_call(arity, function);
                            self.ip = self.instructions.len() + 1;
                            self.depth -= 1;
                        } else {
                            self.potentially_could_deopt = true;
                            self.slow_path_deopt_tail_call(function_index, arity);
                        }

                        return false;
                    } else {
                        // A tail call to a primitive we can emit directly doesn't
                        // need the generic deopt helper at all. Mutable struct
                        // fields are boxes, so `(#%unbox (getter ...))` and
                        // `#%set-box!` land here constantly, and the helper just
                        // re-looks-up the global and re-matches on its kind -
                        // both of which we already know right here.
                        if let Some(value) = self.inline_primitive_tail_call(func.as_ref(), arity) {
                            self.spill_cloned_stack();
                            let real_res = self.inline_handle_pop(value);
                            self.builder.ins().return_(&[real_res]);

                            let cold_block = self.builder.create_block();
                            self.builder.switch_to_block(cold_block);

                            self.depth -= 1;
                            self.ip = self.instructions.len() + 1;
                            return false;
                        }

                        if !matches!(func, Some(SteelVal::FuncV(_))) {
                            self.potentially_could_deopt = true;
                        }

                        self.slow_path_deopt_tail_call(function_index, arity);
                        return false;
                    }
                }
                OpCode::CALLGLOBALNOARITY if self.func_is_join_target(self.ip + 1) => {
                    self.push_global_callee(payload);
                }

                OpCode::CALLGLOBALNOARITY => {
                    // First - find the index that we have to lookup.
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();

                    // Okay, lets do a few things:
                    //
                    // We should attach some context for whether or not this is a closure.
                    //
                    // If its not a closure (i.e. has no captured values) then we can actually
                    // embed the function directly into the call site. This should in theory,
                    // make things a lot faster since now we'll be able to avoid dispatches
                    // on the global environment.
                    //
                    // The biggest issue now, is that the function pointer that we pass to _this_
                    // is not necessarily bound yet at the VM level. What we can do though is probably
                    // eagerly determine for pure functions what index we're going to bind to.
                    //
                    // If its bound to a pure function, then we can embed the pointer directly
                    // into the value, and then also leak the ref count here and embed it
                    // directly into the generated code, so that we can call the function
                    // without needing to look it up?
                    // Recognise struct constructors, predicates and getters here
                    // for the same reason `call_global_impl` does - this opcode
                    // had no such check, so all of them went out through the
                    // generic deopt helper even though we can emit them directly.
                    if INLINE_STRUCT_FUNCTION_CALLS {
                        if let Some(spec) = self
                            ._globals
                            .get(function_index)
                            .cloned()
                            .and_then(create_struct_spec)
                        {
                            if let Some((value, typ)) =
                                self.inline_struct_call_no_drop(spec, arity, function_index)
                            {
                                // `stack_to_ssa` *is* the translation loop, so a
                                // `return` here would abandon the rest of the
                                // function body and emit truncated code.
                                // `inline_struct_call_no_drop` has already
                                // advanced past the FUNC instruction, so just go
                                // round again.
                                self.push_struct_result(value, typ);
                                continue;
                            }
                        }
                    }

                    let name = CallGlobalNoArityFunctionDefinitions::arity_to_name(arity);

                    let self_name = CallSelfNoArityFunctionDefinitions::arity_to_name(arity);

                    // Pessimise the calls here, since in general we're going
                    // to be calling functions that aren't primitives here.
                    self.potentially_could_deopt = true;

                    // Okay, lets try to install the self call if we have the ability to.
                    //
                    // We're also going to commit some very nasty crimes by just arbitrarily passing
                    // the value to the function as is, and hope for the best :)
                    if self.slot.is_some()
                        && self_name.is_some()
                        && self._globals.get(payload).is_none()
                        && self.function_context == Some(function_index)
                    {
                        if USE_EXPERIMENTAL_CALL {
                            let slot = self.slot.unwrap().clone();

                            // TODO: Somehow... we'll have to change the calling convention
                            // to always be (tag, Value), and handle that accordingly
                            // in the function signature wherever possible
                            match self.direct_self_call_target(arity) {
                                // Arguments proven to fit this specialized copy:
                                // call it directly, skipping the generic copy's
                                // guard. Check for a deopt before the result is
                                // used, since it may carry an assumed type.
                                Some((target, assumed)) => {
                                    let result = self.call_self_function_experimental(
                                        arity,
                                        slot,
                                        Some(target),
                                    );
                                    self.check_deopt();
                                    self.direct_self_calls += 1;
                                    self.push(result, assumed.unwrap_or(InferredType::Any));
                                }
                                None => {
                                    let result =
                                        self.call_self_function_experimental(arity, slot, None);
                                    self.push(result, InferredType::Any);
                                }
                            }
                        } else {
                            let slot = self.slot.unwrap().clone();
                            let result =
                                self.call_self_function(arity, self_name.unwrap(), slot, false);

                            // Assuming this worked, we'll want to push this result on to the stack.
                            self.push(result, InferredType::Any);
                        }
                    }
                    // TODO: inline the call global here!
                    else if USE_INLINE_CALL_GLOBAL
                        && matches!(self._globals.get(payload), Some(SteelVal::Closure(_)))
                        // And, we actually have a super instruction
                        && self._globals.get(payload).map(|x| {
                            if let SteelVal::Closure(c) = x {
                                c.super_instructions().is_some()
                            } else {
                                false
                            }
                        }).unwrap_or_default()
                    {
                        // TODO: Insert guard for whether this is mutable or not!
                        // Inline the call to this global, assuming its not mutable

                        let func = if let SteelVal::Closure(c) = self._globals.get(payload).unwrap()
                        {
                            c.clone()
                        } else {
                            panic!();
                        };

                        let id = func.id;

                        let result = self.call_self_function_experimental(arity, func, None);


                        let inferred_type =
                            if let Some(ret_types) = self.function_return_types.get(&id) {
                                if ret_types.len() == 1 {
                                    ret_types.iter().next().copied().unwrap()
                                } else {
                                    InferredType::Any
                                }
                            } else {
                                InferredType::Any
                            };

                        self.push(result, inferred_type);
                    } else if USE_INLINE_CALL_GLOBAL
                        && CallFunctionDefinitions::arity_to_name(arity).is_some()
                    {
                        // Local name here is important
                        let name = CallFunctionDefinitions::arity_to_name(arity);

                        let value = self.inline_lookup_global(payload);

                        self.clone_value(value);

                        self.ip += 1;

                        // This pushes the value on to the shadow stack for us
                        self.inline_call_func(arity, name, value, false, false);
                    } else if let Some(name) = name {
                        // There is also the case that the function is not implemented yet.
                        // For this, unfortunately I think we're going to have to look up the function,
                        // and then call it, since it could be dynamically changing.
                        //
                        // We can use the same functions for calling functions, and just inline
                        // the global lookup here as well; we can get rid of that call entirely
                        // once global lookups are added to be inlined.

                        let result = self.call_global_function(arity, name, function_index, false);

                        // Assuming this worked, we'll want to push this result on to the stack.
                        self.push(result, InferredType::Any);
                    } else {
                        let name = "call-global-no-arity-spilled";

                        let v =
                            self.call_global_function_spilled(arity, name, function_index, false);

                        self.push(v, InferredType::Any)
                    }

                    // Then, we're gonna check the result and see if we should deopt
                    self.check_deopt();
                }

                OpCode::CALLPRIMITIVE if self.func_is_join_target(self.ip + 1) => {
                    self.push_global_callee(payload);
                }

                OpCode::CALLPRIMITIVE => {
                    // Check the actual value that we're looking up, see if its there
                    let function_index = payload;

                    let global = self._globals.get(function_index);

                    match global.cloned() {
                        Some(SteelVal::FuncV(f)) => {
                            // Attempt the other call
                            self.ip += 1;
                            let arity = self.instructions[self.ip].payload_size.to_usize();

                            // TODO: Can we abstract this into its own thing?
                            match f {
                                f if f == steel_stringp as FunctionSignature && arity == 1 => {
                                    self.is_string()
                                }

                                f if f == steel_char_equals as FunctionSignature && arity == 2 => {
                                    self.char_equals(arity)
                                }

                                f if f == steel_listp as FunctionSignature && arity == 1 => {
                                    self.is_list()
                                }

                                f if f == steel_voidp as FunctionSignature && arity == 1 => {
                                    self.is_void()
                                }

                                f if f == steel_eof_objectp as FunctionSignature && arity == 1 => {
                                    self.eof_object()
                                }

                                f if f == steel_symbolp as FunctionSignature && arity == 1 => {
                                    self.is_symbol()
                                }

                                f if f == steel_mut_vec_set as FunctionSignature && arity == 3 => {
                                    self.vector_set()
                                }

                                f if f == mut_vec_push as FunctionSignature && arity == 2 => {
                                    self.vector_push()
                                }

                                f if f == flat_vector_construct as FunctionSignature
                                    && CallFlatVectorConstructorsDefinitions::arity_to_name(
                                        arity,
                                    )
                                    .is_some() =>
                                {
                                    self.flat_vector_construct(arity)
                                }

                                f if inline_bytevector_enabled()
                                    && ((f == steel_bytes_ref as FunctionSignature && arity == 2)
                                        || (f == steel_bytes_set as FunctionSignature
                                            && arity == 3))
                                    && self.byte_vector_shape_ok(arity) =>
                                {
                                    let v = self.inline_byte_vector_op(f, arity);
                                    self.push(v, InferredType::Any);
                                    self.ip += 1;
                                }

                                f if f == steel_eq as FunctionSignature && arity == 2 => self.eq(),

                                f if inline_divmod_enabled()
                                    && arity == 2
                                    && divmod_mode(f).is_some() =>
                                {
                                    let mode = divmod_mode(f).unwrap();
                                    let v = self.inline_int_divmod(mode, f);
                                    self.push(v, InferredType::Any);
                                    self.ip += 1;
                                }

                                f if f == steel_pair as FunctionSignature && arity == 1 => {
                                    self.is_pair()
                                }

                                f if f == steel_list_contains as FunctionSignature
                                    && arity == 2 =>
                                {
                                    self.list_contains()
                                }

                                f if f == steel_is_empty as FunctionSignature && arity == 1 => {
                                    self.is_empty()
                                }
                                // An inlined accessor's `(#%unbox (getter x))` is a
                                // non-tail call, so it lands here rather than in the
                                // tail call arm.
                                f if inline_primitive_tail_calls_enabled()
                                    && (f as usize
                                        == crate::steel_vm::primitives::steel_unbox_mutable
                                            as usize
                                        || f as usize
                                            == crate::steel_vm::primitives::steel_set_box_mutable
                                                as usize)
                                    && self.box_primitive_inlinable(f as usize, arity) =>
                                {
                                    let value = self.inline_box_primitive(f as usize, arity).unwrap();
                                    self.push(value, InferredType::Any);
                                    self.ip += 1;
                                }
                                _ => {
                                    let name = CallPrimitiveDefinitions::arity_to_name(arity);

                                    if let Some(name) = name {
                                        // attempt to move forward with it
                                        let additional_args = self.split_off(arity);

                                        let function = self.builder.ins().iconst(
                                            self.module.target_config().pointer_type(),
                                            // f as *const fn(&[SteelVal]) -> Result<SteelVal, crate::SteelErr>
                                            //     as i64,
                                            f as i64,
                                        );

                                        let fallback_ip = self
                                            .builder
                                            .ins()
                                            .iconst(Type::int(64).unwrap(), self.ip as i64);

                                        let mut args = vec![function, fallback_ip];

                                        args.extend(additional_args.into_iter().map(|x| x.0));

                                        let result =
                                            self.call_function_returns_value_args(name, &args);
                                        self.push(result, InferredType::Any);
                                        self.ip += 1;
                                        self.check_deopt();
                                    } else {
                                        self.ip -= 1;
                                        self.call_global_impl(payload);
                                    }
                                }
                            };
                        }

                        Some(SteelVal::MutFunc(f)) => {
                            // Attempt the other call
                            self.ip += 1;
                            let arity = self.instructions[self.ip].payload_size.to_usize();

                            let name = CallPrimitiveMutDefinitions::arity_to_name(arity);

                            if let Some(name) = name {
                                // attempt to move forward with it
                                let additional_args = self.split_off(arity);

                                let function = self.builder.ins().iconst(
                                    self.module.target_config().pointer_type(),
                                    // f as *const fn(
                                    //     &mut [SteelVal],
                                    // )
                                    //     -> Result<SteelVal, crate::SteelErr>
                                    //     as i64,
                                    f as i64,
                                );

                                let fallback_ip = self
                                    .builder
                                    .ins()
                                    .iconst(Type::int(64).unwrap(), self.ip as i64);

                                let mut args = vec![function, fallback_ip];

                                args.extend(additional_args.into_iter().map(|x| x.0));

                                let result = self.call_function_returns_value_args(name, &args);
                                self.push(result, InferredType::Any);
                                self.ip += 1;
                                self.check_deopt();
                            } else {
                                self.ip -= 1;
                                self.potentially_could_deopt = true;
                                self.call_global_impl(payload);
                            }
                        }

                        _ => {
                            self.potentially_could_deopt = true;

                            // println!("code-gen: {:?}", global);
                            self.call_global_impl(payload);
                        }
                    }
                }

                OpCode::CALLGLOBAL => {
                    if self.func_is_join_target(self.ip + 1) {
                        self.push_global_callee(payload);
                    } else {
                        self.potentially_could_deopt = true;
                        self.call_global_impl(payload);
                    }
                }

                // Pattern is
                // READCAPTURED
                // UNBOX
                //
                // we can
                OpCode::READCAPTURED
                    if INLINE_READ_CAPTURED
                        && self.instructions.get(self.ip + 1).map(|x| x.op_code)
                            == Some(OpCode::UNBOX) =>
                {
                    // Borrowed, not cloned: the capture array belongs to the
                    // closure being run, which outlives this read, and the unbox
                    // below is told not to release it. Cloning here and not
                    // dropping leaked a reference to the box on every execution -
                    // `graphs` grew to 23GB once inlining made this pattern hot.
                    let value = self.inline_read_captured(payload, false);
                    // Advance for the read captured, but we can elide the unbox since its not
                    // going to escape - we're just reading the box value. We can probably inline this
                    // even more but it'll be fine for now.
                    self.ip += 1;

                    let res = self.unbox_value_checked_register(value, false);
                    self.ip += 2;

                    self.push(res, InferredType::Any);
                }

                OpCode::READCAPTURED if INLINE_READ_CAPTURED => {
                    let res = self.inline_read_captured(payload, true);

                    // TODO: Keep track of the inferred type on the capture as well
                    self.push(res, InferredType::Any);

                    self.ip += 1;
                }

                // TODO: Inline this, and cache the resulting captures values
                // buffer, since this should be immutable for the duration
                // of the function, and we can skip the bounds checks, etc.
                OpCode::READCAPTURED => {
                    let index = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), payload as i64);

                    let value = self.call_function_returns_value_args(
                        op_to_name_payload(op, payload),
                        &[index],
                    );

                    self.push(value, InferredType::Any);

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
                OpCode::LETENDSCOPE => {
                    // self.local_count = payload;
                    self.ip += 1;

                    // let last = self.let_var_stack.iter().sum::<usize>() + self.arity as usize;

                    let amt = self.let_var_stack.pop().unwrap();

                    let mut properties_to_remove = Vec::new();

                    // println!("let end scope amount: {}", amt);

                    for index in 0..self.shadow_stack.len() {
                        let v = self.shadow_stack.get_mut(index).unwrap();
                        match v {
                            MaybeStackValue::MutRegister(r) if *r >= payload as usize => {
                                let r = *r;
                                let (value, _) = self.mut_register_to_value(r);

                                self.properties.remove(&ValueOrRegister::Register(r));

                                properties_to_remove.push(r);

                                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                                    value,
                                    inferred_type: InferredType::Any,
                                    spilled: false,
                                });
                            }
                            MaybeStackValue::Register(r) if *r >= payload as usize => {
                                let r = *r;
                                let (value, _) = self.immutable_register_to_value(r);

                                self.properties.remove(&ValueOrRegister::Register(r));
                                self.properties.cached_lookups.registers.remove(&r);

                                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                                    value,
                                    inferred_type: InferredType::Any,
                                    spilled: false,
                                });
                            }
                            _ => {}
                        }
                    }

                    // If we know exactly how many elements we're dropping:

                    // self.call_end_scope_handler(payload);

                    // for i in payload..payload + amt {
                    //     let i = i - self.arity as usize;
                    //     println!("Removing: {}", i);
                    //     self.properties.cached_lookups.registers.remove(&i);
                    // }

                    self.inline_let_end_scope(payload, amt);

                    for i in payload..payload + amt {
                        self.properties.remove(&ValueOrRegister::Register(i));
                        self.local_to_value_map.remove(&i);
                        // The loop above only reaches slots still referenced from
                        // the shadow stack. A cached slot that nothing currently
                        // points at outlives its scope otherwise, and the next let
                        // to reuse the index reads the previous scope's value.
                        // Keyed the same way `LetVar` writes it - let_var_stack +
                        // arity - which is what `payload` already counts in.
                        self.properties.cached_lookups.registers.remove(&i);
                    }

                    // for p in properties_to_remove {
                    //     self.properties.remove(&ValueOrRegister::Register(p));
                    // }

                    // self.call_end_scope_handler_new(payload, amt);
                }

                // Both operands are fixnums by construction (M1's facts), so there is
                // no tag to check. Arithmetic still has to leave the fixnum range
                // somewhere: it exits to the interpreter, which produces the bignum.
                // That is what lets the result stay `Int`.
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
                    let value = self.shadow_stack_pop().unwrap().into_value(self);
                    let register = self.shadow_stack_pop().unwrap().into_index();

                    let register = self.builder.ins().iconst(types::I64, register as i64);

                    let args = [register, value.as_steelval(self)];
                    let result = self.call_function_returns_value_args("sub-binop-reg", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Number);

                    self.check_deopt();

                    self.ip += 2;
                }

                OpCode::SUB
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    // Call the func

                    let args = self.split_off(payload);

                    // TODO: Use the type hints! For now we're not going to for the sake
                    // of getting something running
                    let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

                    let result = self.call_function_returns_value_args("sub-binop-int", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Number);

                    self.check_deopt();

                    self.ip += 2;
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
                    let rhs_int = self
                        .shadow_stack
                        .pop()
                        .unwrap()
                        .into_constant_int(self)
                        .unwrap();
                    let register_index = self.shadow_stack_pop().unwrap().into_index();

                    let local_value = self.read_from_vm_stack(register_index);
                    let is_int = self.is_type(local_value, SteelVal::INT_TAG);

                    let sp = |ctx: &mut Self| {
                        let register = ctx.builder.ins().iconst(types::I64, register_index as i64);

                        let value = ctx.encode_integer(rhs_int as i64);

                        let args = [register, value];
                        let result =
                            ctx.call_function_returns_value_args("add-binop-int-reg", &args);

                        // The helper reports a type error by flagging the vm rather than
                        // returning one; without this the jitted code kept going and the
                        // error surfaced after `with-handler` had already been unwound.
                        ctx.check_deopt();

                        result
                    };

                    let result = self.converging_if(
                        is_int,
                        |ctx| {
                            // If its an int, then we'll do checked subtraction:
                            let lhs = ctx.unbox_value_to_pointer(local_value);
                            let rhs = ctx.builder.ins().iconst(types::I64, rhs_int as i64);

                            let (subbed, overflow_flag) = ctx.builder.ins().sadd_overflow(lhs, rhs);

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

                    // let args = [register, value];
                    // let result = self.call_function_returns_value_args("sub-binop-int-reg", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Number);

                    self.ip += 2;
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
                    let value = self.shadow_stack_pop().unwrap().into_value(self);

                    let value_as_steelval = value.as_steelval(self);

                    let register = self.shadow_stack_pop().unwrap().into_index();

                    // Lets check the left hand size, handle overflow as necessary:

                    let register_value = self.read_from_vm_stack(register);
                    let left_is_int = self.is_type(register_value, SteelVal::INT_TAG);

                    // If the right hand side is already classified as an int, then
                    // we can skip checking if its an integer.
                    let both_int = if value.inferred_type == InferredType::Int {
                        left_is_int
                    } else {
                        let right_is_int = self.is_type(value_as_steelval, SteelVal::INT_TAG);
                        let both_int = self.builder.ins().band(left_is_int, right_is_int);
                        both_int
                    };

                    let typ = self.int;

                    let mut sp = |ctx: &mut Self| {
                        let register = ctx.builder.ins().iconst(types::I64, register as i64);
                        let args = [register, value_as_steelval];
                        let result = ctx.call_function_returns_value_args("add-binop-reg", &args);

                        // The helper reports a type error by flagging the vm rather than
                        // returning one; without this the jitted code kept going and the
                        // error surfaced after `with-handler` had already been unwound.
                        ctx.check_deopt();

                        result
                    };

                    let res = self.converging_if(
                        both_int,
                        |ctx| {
                            // This is pointer sized, we're good to shrink it down
                            // to a pointer. both have to be int tag, otherwise we fall back to
                            // a function, and we'll return the usual
                            let left_payload = ctx.unbox_value_to_pointer(register_value);
                            let right_payload = ctx.unbox_value_to_pointer(value_as_steelval);

                            // Add the values, did they overflow?
                            let (added, overflow_flag) =
                                ctx.builder.ins().sadd_overflow(left_payload, right_payload);

                            ctx.converging_if(
                                overflow_flag,
                                sp,
                                |ctx| {
                                    // Happy path, just return the boxed integer value.
                                    ctx.encode_value(SteelVal::INT_TAG as _, added)
                                },
                                typ,
                            )
                        },
                        |ctx| {
                            let res = sp(ctx);
                            ctx.check_deopt();
                            res
                        },
                        typ,
                    );

                    self.push(res, InferredType::Number);

                    self.ip += 2;
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
                    let MaybeStackValue::Value(r) = self.shadow_stack_pop().unwrap() else {
                        panic!()
                    };
                    let MaybeStackValue::Value(l) = self.shadow_stack_pop().unwrap() else {
                        panic!()
                    };

                    // TODO: Might be worth attempting to figure out what the inferred type
                    // for function calls are, to propagate downward in the calls
                    let (res, t) = self.binop_add_value(l, r);

                    self.push(res, t);

                    self.ip += 2;
                }

                // TODO: Specialize this a bit more. If we know that the RHS is some kind
                // of constant, we can probably encode that a little bit more effectively
                // in the generated code.
                // Integer arithmetic is inlined above; floats fell all the way
                // through to a helper that builds a slice and a `Result` for one
                // machine instruction. Check both tags and emit the op directly.
                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV
                    if payload == 2 && generic_inline_enabled() && float_inline_enabled() =>
                {
                    let fallback = op_to_name_payload(op, payload);
                    let res = self.inline_float_binop_two(op, fallback);
                    self.push(res, InferredType::Number);
                    self.ip += 2;
                }

                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    // Call the func
                    self.func_ret_val(op, payload, 2, InferredType::Number);
                    self.check_deopt();
                }

                OpCode::LT
                    if payload == 2
                        // Okay this could be an int, or not an int
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int)
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2),
                            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                        ) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    enum Either {
                        Value(Value),
                        Int(i64),
                    }

                    // Materialized rather than read raw: an untagged payload
                    // would otherwise be used as though it were a SteelVal.
                    // `shadow_stack_pop` so a spilled operand is taken off the vm
                    // stack as well, rather than left there to shift everything
                    // above it - see the `cons` arm above.
                    let rhs_int = match self.shadow_stack_pop().unwrap() {
                        MaybeStackValue::Value(v) => Either::Value(v.as_steelval(self)),
                        MaybeStackValue::Constant(ConstantValue::Int(i)) => Either::Int(i as _),
                        // The guard admits any stack entry whose inferred type is
                        // `Int`, and a let slot bound to an int is a register with
                        // that type. This used to fall to `panic!()`; it is
                        // unreachable only while arguments are never typed.
                        MaybeStackValue::Register(r) => {
                            Either::Value(self.immutable_register_to_value(r).0)
                        }
                        MaybeStackValue::MutRegister(r) => {
                            Either::Value(self.mut_register_to_value(r).0)
                        }
                        _ => panic!(),
                    };

                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    // Happy path, lets check the type of the lhs. If its an integer tag, then we can go ahead and do the thing.
                    // Otherwise, we'll bail and fall through:

                    let local_value = self.read_from_vm_stack(register_l);
                    let is_int = self.is_type(local_value, SteelVal::INT_TAG);

                    let result = self.converging_if(
                        is_int,
                        |ctx| {
                            // Just do less than or equal on the value:
                            let lhs = ctx.unbox_value_to_pointer(local_value);

                            let res = match rhs_int {
                                Either::Value(value) => {
                                    let rhs = ctx.unbox_value_to_pointer(value);
                                    let res =
                                        ctx.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);

                                    res
                                }
                                Either::Int(i) => {
                                    // Lets encode the property here then?
                                    ctx.builder.ins().icmp_imm_s(IntCC::SignedLessThan, lhs, i)
                                }
                            };

                            res
                        },
                        |ctx| {
                            let register_l =
                                ctx.builder.ins().iconst(types::I64, register_l as i64);

                            let rhs_value = match rhs_int {
                                Either::Value(value) => value,
                                Either::Int(i) => ctx.encode_integer(i),
                            };

                            let args = [register_l, rhs_value];
                            let result =
                                ctx.call_function_returns_value_args("lt-register-int", &args);
                            ctx.check_deopt();

                            result
                        },
                        types::I8,
                    );

                    // Encoding the property here
                    if let Either::Int(i) = rhs_int {
                        self.properties.add_property(
                            ValueOrRegister::Value(result),
                            Properties::ConditionLessThan(ValueOrRegister::Register(register_l), i),
                        );
                    }

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::UnboxedBool);

                    self.ip += 2;
                }

                OpCode::LT
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }
                    // TODO: This isn't quite right. This doesn not check the input
                    // type properly
                    self.func_ret_val_named_with_context("lt-binop-int", payload, 2, InferredType::Bool);
                }

                // When the value is a mutable register, or register, and we're comparing
                // it to a known constant / etc
                OpCode::LTE
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int)
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2),
                            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                        ) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    let rhs_int = self.shadow_stack_pop().unwrap().into_value(self);

                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    // Happy path, lets check the type of the lhs. If its an integer tag,
                    // then we can go ahead and do the thing.
                    // Otherwise, we'll bail and fall through:

                    let (tag, local_value) = self.read_from_vm_stack_split(register_l);

                    let is_int =
                        self.builder
                            .ins()
                            .icmp_imm_s(IntCC::Equal, tag, SteelVal::INT_TAG as i64);

                    let result = self.converging_if(
                        is_int,
                        |ctx| {
                            // Just do less than or equal on the value:
                            // let lhs = ctx.unbox_value_to_pointer(local_value);
                            let lhs = local_value;
                            let rhs = ctx.unbox_value_to_pointer(rhs_int.value);

                            let res =
                                ctx.builder
                                    .ins()
                                    .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs);

                            res
                        },
                        |ctx| {
                            let register_l =
                                ctx.builder.ins().iconst(types::I64, register_l as i64);
                            let args = [register_l, rhs_int.value];
                            let result =
                                ctx.call_function_returns_value_args("lte-register-int", &args);
                            ctx.check_deopt();

                            result
                        },
                        types::I8,
                    );

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::UnboxedBool);

                    self.ip += 2;
                }

                OpCode::LTE
                    if payload == 2
                        && matches!(
                            self.shadow_stack.last(),
                            Some(MaybeStackValue::Constant(ConstantValue::Int(_)))
                        )
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2),
                            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                        ) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    let rhs_int = self
                        .shadow_stack_pop()
                        .unwrap()
                        .into_constant_int(self)
                        .unwrap();

                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    // Happy path, lets check the type of the lhs. If its an integer tag,
                    // then we can go ahead and do the thing.
                    // Otherwise, we'll bail and fall through:

                    let (tag, local_value) = self.read_from_vm_stack_split(register_l);

                    let is_int =
                        self.builder
                            .ins()
                            .icmp_imm_s(IntCC::Equal, tag, SteelVal::INT_TAG as i64);

                    let result = self.converging_if(
                        is_int,
                        |ctx| {
                            // Just do less than or equal on the value:
                            // let lhs = ctx.unbox_value_to_pointer(local_value);
                            let lhs = local_value;
                            // let rhs = ctx.unbox_value_to_pointer(rhs_int.value);

                            let res = ctx.builder.ins().icmp_imm_s(
                                IntCC::SignedLessThanOrEqual,
                                lhs,
                                rhs_int as i64,
                            );

                            res
                        },
                        |ctx| {
                            let register_l =
                                ctx.builder.ins().iconst(types::I64, register_l as i64);

                            let value = ctx.encode_integer(rhs_int as _);

                            let args = [register_l, value];
                            let result =
                                ctx.call_function_returns_value_args("lte-register-int", &args);
                            ctx.check_deopt();

                            result
                        },
                        types::I8,
                    );

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::UnboxedBool);

                    self.ip += 2;
                }

                // Inferred type is not int
                OpCode::GTE
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int)
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2),
                            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                        ) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    let rhs_int = self.shadow_stack_pop().unwrap().into_value(self);
                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    // Happy path, lets check the type of the lhs. If its an integer tag,
                    // then we can go ahead and do the thing.
                    // Otherwise, we'll bail and fall through:

                    let local_value = self.read_from_vm_stack(register_l);
                    let is_int = self.is_type(local_value, SteelVal::INT_TAG);

                    let result = self.converging_if(
                        is_int,
                        |ctx| {
                            // Just do less than or equal on the value:
                            let lhs = ctx.unbox_value_to_pointer(local_value);
                            let rhs = ctx.unbox_value_to_pointer(rhs_int.value);

                            let res =
                                ctx.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs);

                            res
                        },
                        |ctx| {
                            let register_l =
                                ctx.builder.ins().iconst(types::I64, register_l as i64);
                            let args = [register_l, rhs_int.value];
                            let result =
                                ctx.call_function_returns_value_args("gte-register-int", &args);
                            ctx.check_deopt();

                            result
                        },
                        types::I8,
                    );

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::UnboxedBool);

                    self.ip += 2;
                }

                // TODO: Introduce an abstraction to insert blocks when _both_ types are the same.
                // Reduce number of type checks, inline comparisons.
                OpCode::GTE
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2),
                            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                        ) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    let rhs_int = self.shadow_stack_pop().unwrap().into_value(self);
                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    // Happy path, lets check the type of the lhs. If its an integer tag,
                    // then we can go ahead and do the thing.
                    // Otherwise, we'll bail and fall through:

                    let local_value = self.read_from_vm_stack(register_l);

                    // Both operands: checking the register twice compared an int
                    // or float register against the raw payload of whatever the
                    // other operand was - `(>= 1.5 2)` read 2 as a denormal float.
                    let register_is_int = self.is_type(local_value, SteelVal::INT_TAG);
                    let is_int = self.is_type(rhs_int.value, SteelVal::INT_TAG);

                    let both_int = self.builder.ins().band(register_is_int, is_int);

                    let result = self.converging_if(
                        both_int,
                        |ctx| {
                            // Just do less than or equal on the value:
                            let lhs = ctx.unbox_value_to_pointer(local_value);
                            let rhs = ctx.unbox_value_to_pointer(rhs_int.value);

                            let res =
                                ctx.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs);

                            res
                        },
                        |ctx| {
                            let register_is_float = ctx.is_type(local_value, SteelVal::FLOAT_TAG);
                            let is_float = ctx.is_type(rhs_int.value, SteelVal::FLOAT_TAG);

                            let both_float = ctx.builder.ins().band(register_is_float, is_float);

                            ctx.converging_if(
                                both_float,
                                |ctx| {
                                    // Just do less than or equal on the value:
                                    let lhs = ctx.unbox_value_to_float(local_value);
                                    let rhs = ctx.unbox_value_to_float(rhs_int.value);

                                    let res = ctx.builder.ins().fcmp(
                                        FloatCC::GreaterThanOrEqual,
                                        lhs,
                                        rhs,
                                    );

                                    res
                                },
                                |ctx| {
                                    let register_l =
                                        ctx.builder.ins().iconst(types::I64, register_l as i64);
                                    let args = [register_l, rhs_int.value];
                                    let result = ctx.call_function_returns_value_args(
                                        "gte-register-unknown",
                                        &args,
                                    );
                                    ctx.check_deopt();

                                    result
                                },
                                types::I8,
                            )
                        },
                        types::I8,
                    );

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::UnboxedBool);

                    self.ip += 2;
                }

                OpCode::LTE
                    if payload == 2
                        && matches!(self.shadow_stack.last(), Some(MaybeStackValue::Value(_)))
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2),
                            Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                        ) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    let rhs_int = self.shadow_stack_pop().unwrap().into_value(self);
                    let register_l = self.shadow_stack_pop().unwrap().into_index();

                    let register_l = self.builder.ins().iconst(types::I64, register_l as i64);

                    let args = [register_l, rhs_int.value];
                    let result = self.call_function_returns_value_args("lte-register", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::UnboxedBool);

                    self.check_deopt();

                    self.ip += 2;
                }

                OpCode::LTE
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }
                    self.func_ret_val_named_with_context("lte-binop-int", payload, 2, InferredType::Bool);
                }

                OpCode::NUMEQUAL
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[MaybeStackValue::Value(_), MaybeStackValue::Value(_),])
                        ) =>
                {
                    // Lets assume, for now, that we'll happy path the case where they're both
                    // ints. We would need to implement something more sophisticated for checking
                    // otherwise.

                    // This will be our constant value:
                    let right = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };
                    let left = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };

                    // Check if the tags are the same, and they're numeric:
                    let left_int = self.is_type(left, SteelVal::INT_TAG);
                    let right_int = self.is_type(right, SteelVal::INT_TAG);

                    let both_int = self.builder.ins().band(left_int, right_int);

                    // Just check equality of the unboxed values

                    let res = self.converging_if(
                        both_int,
                        |ctx| {
                            // We can't rely on the tag matching in this world unfortunately.
                            // Tags coming from rust land in debug mode could have garbage in the
                            // padding that otherwise isn't there in the release build.
                            let left_payload = ctx.unbox_value_to_pointer(left);
                            let right_payload = ctx.unbox_value_to_pointer(right);
                            ctx.builder
                                .ins()
                                .icmp(IntCC::Equal, left_payload, right_payload)
                        },
                        |ctx| {
                            // Handle the else case here as well
                            let vm_ctx = ctx.get_ctx();

                            let res = ctx.call_function_returns_value_args_no_context(
                                "num-equal-value-bool",
                                &[vm_ctx, left, right],
                            );

                            // Make sure to check the deopt case here
                            ctx.check_deopt();

                            res
                        },
                        types::I8,
                    );

                    self.push(res, InferredType::UnboxedBool);
                    self.ip += 2;
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
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    // Lets assume, for now, that we'll happy path the case where they're both
                    // ints. We would need to implement something more sophisticated for checking
                    // otherwise.

                    // This will be our constant value:
                    let last = self.shadow_stack_pop().unwrap().into_value(self);

                    // This is now our register; this is where the values will live.
                    let register = self.shadow_stack_pop().unwrap().into_index();
                    let register_value = self.read_from_vm_stack(register);

                    // Check if the tags are the same, and they're numeric:
                    let is_register_int = self.is_type(register_value, SteelVal::INT_TAG);

                    // Just check equality of the unboxed values

                    let res = self.converging_if(
                        is_register_int,
                        |ctx| {
                            // We can't rely on the tag matching in this world unfortunately.
                            // Tags coming from rust land in debug mode could have garbage in the
                            // padding that otherwise isn't there in the release build.
                            let register_payload = ctx.unbox_value_to_pointer(register_value);
                            let last_steelval = last.as_steelval(ctx);
                            let rhs = ctx.unbox_value_to_pointer(last_steelval);
                            ctx.builder.ins().icmp(IntCC::Equal, register_payload, rhs)
                        },
                        |ctx| {
                            // Handle the else case here as well
                            // todo!();

                            let register_int =
                                ctx.builder.ins().iconst(types::I64, register as i64);

                            let vm_ctx = ctx.get_ctx();

                            let last_steelval = last.as_steelval(ctx);
                            let res = ctx.call_function_returns_value_args_no_context(
                                "num-equal-int-register",
                                &[vm_ctx, register_int, last_steelval],
                            );

                            // Make sure to check the deopt case here
                            ctx.check_deopt();

                            res
                        },
                        types::I8,
                    );

                    self.push(res, InferredType::UnboxedBool);
                    self.ip += 2;
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
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    // Lets assume, for now, that we'll happy path the case where they're both
                    // ints. We would need to implement something more sophisticated for checking
                    // otherwise.

                    let int = self
                        .shadow_stack
                        .pop()
                        .unwrap()
                        .into_constant_int(self)
                        .unwrap();

                    // This is now our register; this is where the values will live.
                    let value = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };

                    // Check if the tags are the same, and they're numeric:
                    let is_value_int = self.is_type(value, SteelVal::INT_TAG);

                    // Just check equality of the unboxed values

                    let res = self.converging_if(
                        is_value_int,
                        |ctx| {
                            // We can't rely on the tag matching in this world unfortunately.
                            // Tags coming from rust land in debug mode could have garbage in the
                            // padding that otherwise isn't there in the release build.
                            let register_payload = ctx.unbox_value_to_pointer(value);
                            ctx.builder
                                .ins()
                                .icmp_imm_s(IntCC::Equal, register_payload, int as i64)
                        },
                        |ctx| {
                            // Handle the else case here as well
                            // todo!();

                            let vm_ctx = ctx.get_ctx();

                            let last = ctx.encode_integer(int as _);

                            let res = ctx.call_function_returns_value_args_no_context(
                                "num-equal-value-bool",
                                &[vm_ctx, value, last],
                            );

                            // Make sure to check the deopt case here
                            ctx.check_deopt();

                            res
                        },
                        types::I8,
                    );

                    self.push(res, InferredType::UnboxedBool);
                    self.ip += 2;
                }

                // TODO: @Matt
                //
                // This is where we have to be better; use the registers, use the constants,
                // and inline the numeric ops since these are likely to be extremely common.
                // The arms above need one side to be a compile-time constant or a
                // register, which generic numeric code rarely obliges. Check both
                // tags instead: when they're both ints the comparison is a single
                // `icmp`, and everything else still reaches the same helper.
                OpCode::NUMEQUAL if payload == 2 && generic_inline_enabled() => {
                    let res = self.inline_int_compare_two(IntCC::Equal, "num-equal-value-bool");
                    self.push(res, InferredType::UnboxedBool);
                    self.ip += 2;
                }

                OpCode::NUMEQUAL
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    self.func_ret_val_named_with_context(
                        "num-equal-int",
                        payload,
                        2,
                        InferredType::Bool,
                    );
                }

                // OpCode::GTE if payload == 2 => {
                //     // If we know the concrete type, we might be able to
                //     // do something inline?
                //     self.gte()
                // }
                // Same story as NUMEQUAL above - the specialised arms all want a
                // register or a constant, so two ordinary values went out to a
                // helper every time.
                OpCode::LTE | OpCode::GTE | OpCode::LT | OpCode::GT
                    if payload == 2 && generic_inline_enabled() =>
                {
                    for arg in self
                        .shadow_stack
                        .get(self.shadow_stack.len() - payload..)
                        .unwrap()
                        .to_vec()
                    {
                        self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                    }

                    let (cc, fallback) = match op {
                        OpCode::LT => (IntCC::SignedLessThan, "lt-two-value-bool"),
                        OpCode::LTE => (IntCC::SignedLessThanOrEqual, "lte-two-value-bool"),
                        OpCode::GT => (IntCC::SignedGreaterThan, "gt-two-value-bool"),
                        OpCode::GTE => (IntCC::SignedGreaterThanOrEqual, "gte-two-value-bool"),
                        _ => unreachable!(),
                    };

                    let res = self.inline_int_compare_two(cc, fallback);
                    self.push(res, InferredType::UnboxedBool);
                    self.ip += 2;
                }

                OpCode::LTE | OpCode::GTE | OpCode::LT | OpCode::GT => {
                    if payload == 2 {
                        for arg in self
                            .shadow_stack
                            .get(self.shadow_stack.len() - payload..)
                            .unwrap()
                            .to_vec()
                        {
                            self.shadow_mark_local_type_from_var(arg, InferredType::Number);
                        }
                    }

                    self.func_ret_val(op, payload, 2, InferredType::Bool);
                }

                // Inlining equality... figure out a way to make this faster
                // without needing to make a call. So for this, we'll just do
                // pointer equality fast paths?
                OpCode::EQUAL2 => {
                    let args = self
                        .shadow_stack
                        .get(self.shadow_stack.len() - 2..)
                        .unwrap();

                    // self.func_ret_val(op, payload, 2, InferredType::Bool);

                    match args {
                        &[MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i), MaybeStackValue::Value(v)]
                        | &[MaybeStackValue::Value(v), MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)] =>
                        {
                            self.shadow_stack_pop();
                            self.shadow_stack_pop();

                            let register_index = self.builder.ins().iconst(types::I64, i as i64);

                            let v_steelval = v.as_steelval(self);
                            let res = self.call_function_returns_value_args(
                                "equal-binop-register-bool",
                                &[register_index, v_steelval],
                            );

                            self.push(res, InferredType::UnboxedBool);

                            self.ip += 2;
                        }

                        &[MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i), MaybeStackValue::Constant(ConstantValue::Symbol(v))]
                        | &[MaybeStackValue::Constant(ConstantValue::Symbol(v)), MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)] =>
                        {
                            self.shadow_stack_pop();
                            self.shadow_stack_pop();

                            let constant = self.constants.get(v);
                            let SteelVal::SymbolV(s) = constant else {
                                panic!()
                            };

                            let as_ptr: i64 =
                                unsafe { std::mem::transmute::<SteelString, _>(s.clone()) };
                            let value = self.builder.ins().iconst(types::I64, as_ptr);

                            // If they're the same type, just compare the bytes. Don't do a lookup.
                            let left_value = self.read_from_vm_stack(i);
                            let is_symbol = self.is_type(left_value, SteelVal::SYMBOL_TAG);
                            // Get the left value
                            let lvalue = self.unbox_value_to_pointer(left_value);

                            let test =
                                self.builder
                                    .ins()
                                    .icmp_imm_s(IntCC::Equal, lvalue, as_ptr as i64);

                            let fast_path = self.builder.ins().band(is_symbol, test);

                            let res = self.converging_if(
                                fast_path,
                                |ctx| ctx.builder.ins().iconst(types::I8, 1),
                                |ctx| {
                                    ctx.converging_if(
                                        is_symbol,
                                        |ctx| {
                                            ctx.call_function_returns_value_args_no_context(
                                                "symbol-equal?-no-drop",
                                                &[lvalue, value],
                                            )
                                        },
                                        |ctx| ctx.builder.ins().iconst(types::I8, 0),
                                        types::I8,
                                    )
                                },
                                types::I8,
                            );

                            self.push(res, InferredType::UnboxedBool);

                            self.ip += 2;
                        }

                        // Register + Constant,
                        // we might be able to say they're not equal
                        // right off the bat based on inferred types, but
                        // for now we're going to continue moving on. If the constant
                        // is a symbol, we can inline an eq call before falling back to
                        // equality with symbols.
                        &[MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i), MaybeStackValue::Constant(v)]
                        | &[MaybeStackValue::Constant(v), MaybeStackValue::Register(i) | MaybeStackValue::MutRegister(i)] =>
                        {
                            self.shadow_stack_pop();
                            self.shadow_stack_pop();

                            let register_index = self.builder.ins().iconst(types::I64, i as i64);

                            let value = v.to_value(self);

                            let res = self.call_function_returns_value_args(
                                "equal-binop-register-bool",
                                &[register_index, value.0],
                            );

                            // TODO: Somewhere, I'm not calling unboxed bool to_value properly!
                            self.push(res, InferredType::UnboxedBool);

                            self.ip += 2;
                        }

                        _ => {
                            self.func_ret_val(op, payload, 2, InferredType::UnboxedBool);
                        }
                    };
                }

                // Lets inline equal2
                OpCode::EQUAL | OpCode::NUMEQUAL => {
                    // println!("Generating code for equal");
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
                    let last = self.shadow_stack_pop().unwrap().into_index();
                    let value = self.read_from_vm_stack(last);
                    let result = self.check_null_no_drop(value);

                    // Okay, we're going to try branch on properties, and assert
                    // type checks depending on which branch we take. If the test
                    // condition has a property associated with it, then we go ahead
                    // and take that on the then branch. Luckily, `and` conditions
                    // lower to if statements, so we should really only have an
                    // individual check on the condition.
                    self.properties.add_property(
                        ValueOrRegister::Value(result),
                        Properties::CheckedNull(ValueOrRegister::Register(last)),
                    );

                    self.push(result, InferredType::UnboxedBool);

                    self.ip += 2;
                }

                OpCode::NULL => {
                    self.func_ret_val_named("null-handler", 1, 2, InferredType::Bool);
                }

                // Cons handler value - lets optimize this
                OpCode::CONS => {
                    let args = self
                        .shadow_stack
                        .get(self.shadow_stack.len() - 2..)
                        .unwrap();

                    match args {
                        // TODO: Finish this
                        // &[MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i), MaybeStackValue::MutRegister(l)] =>
                        // {
                        //     todo!()
                        // }

                        // Probably could instead just write the value back to the register
                        &[MaybeStackValue::Value(_), MaybeStackValue::MutRegister(l)] => {
                            let register = self.shadow_stack_pop().unwrap().into_index();
                            // `shadow_stack_pop`, not a bare pop: a spilled operand
                            // lives on the vm stack, and taking only its ssa value
                            // leaves that copy behind. Everything above it then
                            // shifts by a slot - the self tail call below reads its
                            // arguments off the top and got this one instead.
                            let value = self
                                .shadow_stack_pop()
                                .unwrap()
                                .into_value(self)
                                .as_steelval(self);

                            let register = self.builder.ins().iconst(types::I64, register as i64);

                            // Just... leave it in place if it mutates the register.
                            // We can lazily have the register move around.
                            self.call_function_args_no_return(
                                "cons-handler-value-register",
                                &[value, register],
                            );

                            let result = MaybeStackValue::MutRegister(l);

                            // TODO: Make the type as inferred?

                            // Check the inferred type, if we know of it
                            self.shadow_push(result);

                            self.ip += 2;
                        }

                        &[MaybeStackValue::MutRegister(v), MaybeStackValue::MutRegister(l)] => {
                            let register = self.shadow_stack_pop().unwrap().into_index();
                            let value = self.shadow_stack_pop().unwrap().into_index();

                            let register = self.builder.ins().iconst(types::I64, register as i64);
                            let value = self.register_index(value);

                            // Just... leave it in place if it mutates the register.
                            // We can lazily have the register move around.
                            self.call_function_args_no_return(
                                "cons-handler-register-register",
                                &[value, register],
                            );

                            let result = MaybeStackValue::MutRegister(l);

                            self.properties.set_property(
                                ValueOrRegister::Register(v),
                                Properties::InferredType(InferredType::Void),
                            );

                            // TODO: Make the type as inferred?

                            // Check the inferred type, if we know of it
                            self.shadow_push(result);

                            self.ip += 2;
                        }

                        // Anything else, we'll move on. Not `List`: consing onto
                        // anything but a list makes a pair.
                        _ => {
                            self.func_ret_val(op, 2, 2, InferredType::ListOrPair);
                        }
                    }
                }

                // Cdr reg no type check, should be faster
                OpCode::CDR => {
                    if let Some(last) = self.shadow_stack.last().copied() {
                        self.shadow_mark_local_type_from_var(last, InferredType::ListOrPair);
                    }

                    match self.shadow_stack.last().unwrap().clone() {
                        MaybeStackValue::Register(reg) => {
                            let can_skip_bounds_check = matches!(
                                self.properties.get(&ValueOrRegister::Register(reg)),
                                Some(Properties::ProperNonEmptyList)
                            );
                            // The rest of a proper list is a proper list. On an
                            // empty list the helper raises, and the deopt check
                            // below leaves before the result is used.
                            let rest_type = self.cdr_result_type(reg);

                            self.properties.add_property(
                                ValueOrRegister::Register(reg),
                                Properties::NonEmptyListOrPair,
                            );

                            self.shadow_stack_pop();
                            let reg = self.register_index(reg);

                            if can_skip_bounds_check {
                                let func = "cdr-reg-no-check";
                                let res = self.call_function_returns_value_args(func, &[reg]);
                                self.push(res, rest_type);
                            } else {
                                let func = "cdr-reg";
                                let res = self.call_function_returns_value_args(func, &[reg]);
                                self.check_deopt();
                                self.push(res, rest_type);
                            };

                            self.ip += 2;
                        }

                        // Can we... avoid checking this fact?
                        MaybeStackValue::MutRegister(reg) => {
                            let can_skip_bounds_check = matches!(
                                self.properties.get(&ValueOrRegister::Register(reg)),
                                Some(Properties::ProperNonEmptyList)
                            );

                            // let can_skip_bounds_check = false;

                            self.shadow_stack_pop();
                            let ir_reg = self.register_index(reg);

                            if can_skip_bounds_check {
                                let func = "cdr-mut-reg-no-check";
                                // println!("Adding inferred type void for register: {}", reg);
                                // self.properties.props.insert(
                                //     ValueOrRegister::Register(reg),
                                //     vec![Properties::InferredType(InferredType::Void)],
                                // );

                                self.properties.set_property(
                                    ValueOrRegister::Register(reg),
                                    Properties::ProperList,
                                );

                                self.call_function_args_no_return(func, &[ir_reg]);
                                self.shadow_push(MaybeStackValue::MutRegister(reg));
                            } else {
                                let func = "cdr-mut-reg";
                                let rest_type = self.cdr_result_type(reg);
                                // println!("Adding inferred type void for register: {}", reg);
                                self.properties.props.insert(
                                    ValueOrRegister::Register(reg),
                                    vec![Properties::InferredType(InferredType::Void)],
                                );

                                let res = self.call_function_returns_value_args(func, &[ir_reg]);
                                self.check_deopt();
                                self.push(res, rest_type);
                            };

                            self.ip += 2;
                        }

                        _ => {
                            self.func_ret_val(op, 1, 2, InferredType::Any);
                            self.check_deopt();
                        }
                    }
                }
                OpCode::LIST => {
                    // Return a list:
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();
                    self.ip += 1;

                    if let Some(function_name) = ListHandlerDefinitions::arity_to_name(payload) {
                        let args = self.split_off(payload);

                        // println!("Calling list: {:?}", args);

                        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

                        let result = self.call_function_returns_value_args(function_name, &args);

                        // Check the inferred type, if we know of it
                        self.push(result, InferredType::List);
                    } else {
                        let args = self.split_off(payload);

                        for arg in args {
                            self.push_to_vm_stack(arg.0);
                        }

                        let arity_value = self
                            .builder
                            .ins()
                            .iconst(Type::int(64).unwrap(), arity as i64);

                        let result = self.call_function_returns_value_args(
                            "list-handler-spilled",
                            &[arity_value],
                        );

                        // Check the inferred type, if we know of it
                        self.push(result, InferredType::List);
                    }
                }

                OpCode::VEC => {
                    let arity = payload / 2;

                    self.ip += 1;

                    let args = self.split_off(arity);

                    for arg in args {
                        self.push_to_vm_stack(arg.0);
                    }

                    let arity_value = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), payload as i64);

                    let result = self
                        .call_function_returns_value_args("vec-handler-spilled", &[arity_value]);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Any);
                }

                // Specialize car for when its on a register, to avoid doing
                // the read local operations.
                OpCode::CAR => {
                    if let Some(last) = self.shadow_stack.last().copied() {
                        self.shadow_mark_local_type_from_var(last, InferredType::ListOrPair);
                    }

                    match self.shadow_stack.last().unwrap().clone() {
                        MaybeStackValue::MutRegister(reg) | MaybeStackValue::Register(reg) => {
                            // Don't think we can do this. When checking null?, we also want to check
                            // that the value is a list - if we assert its a list earlier, we can avoid
                            // a lot of checks.
                            // let can_skip_bounds_check = matches!(
                            //     self.properties.get(&ValueOrRegister::Register(reg)),
                            //     Some(Properties::NonEmptyList)
                            // );

                            match self.properties.get(&ValueOrRegister::Register(reg)) {
                                Some(Properties::NonNull) if self.use_lbbv => {
                                    let car_moves = matches!(
                                        self.shadow_stack.last(),
                                        Some(MaybeStackValue::MutRegister(_))
                                    );
                                    self.shadow_stack_pop();
                                    // let value = self.read_from_vm_stack(reg);

                                    let (tag, value) = self.read_from_vm_stack_split(reg);

                                    let typ = self.int;

                                    // let is_list = self.is_type(value, SteelVal::LIST_TAG);

                                    let is_list = self.builder.ins().icmp_imm_s(
                                        IntCC::Equal,
                                        tag,
                                        SteelVal::LIST_TAG as i64,
                                    );

                                    self.branch_on_condition_and_property(
                                        is_list,
                                        reg,
                                        Properties::ProperNonEmptyList,
                                        Properties::NonEmptyListOrPair,
                                        |ctx| (ctx.unchecked_car_unboxed(value), InferredType::Any),
                                        |ctx| {
                                            let is_pair = ctx.builder.ins().icmp_imm_s(
                                                IntCC::Equal,
                                                tag,
                                                SteelVal::PAIR_TAG as i64,
                                            );

                                            let value = ctx.converging_if_else_cold(
                                                is_pair,
                                                // Inline car for a pair:
                                                // Unboxed variant: this site reads with
                                                // `read_from_vm_stack_split`, so `value` is
                                                // already the payload. Unboxing again isplits
                                                // an i64 into i32 halves and uses one as a
                                                // pointer, which the verifier rejects.
                                                |ctx| ctx.inline_pair_car_unboxed(value),
                                                |ctx| {
                                                    let reg = ctx.register_index(reg);
                                                    let res = ctx.call_function_returns_value_args(
                                                        "car-reg",
                                                        &[reg],
                                                    );

                                                    ctx.check_deopt();
                                                    res
                                                },
                                                typ,
                                            );

                                            (value, InferredType::Any)
                                        },
                                        |ctx, res, typ| {
                                            // The list stays in its register, so
                                            // the element can be left borrowed.
                                            if borrow_enabled() && !car_moves {
                                                let owned = ctx.owned_flag(false);
                                                ctx.push_borrowed(res, owned);
                                            } else {
                                                ctx.clone_value(res);
                                                ctx.push(res, typ);
                                            }
                                            ctx.ip += 2;
                                        },
                                    );
                                }

                                Some(Properties::NonNull) => {
                                    self.shadow_stack_pop();
                                    let (tag, value) = self.read_from_vm_stack_split(reg);

                                    let typ = self.int;

                                    let is_list = self.builder.ins().icmp_imm_s(
                                        IntCC::Equal,
                                        tag,
                                        SteelVal::LIST_TAG as i64,
                                    );

                                    let res = self.converging_if(
                                        is_list,
                                        |ctx| ctx.unchecked_car_unboxed(value),
                                        |ctx| {
                                            let is_pair = ctx.is_type(value, SteelVal::PAIR_TAG);

                                            ctx.converging_if(
                                                is_pair,
                                                // Inline car for a pair:
                                                |ctx| ctx.inline_pair_car_unboxed(value),
                                                |ctx| {
                                                    let reg = ctx.register_index(reg);
                                                    let res = ctx.call_function_returns_value_args(
                                                        "car-reg",
                                                        &[reg],
                                                    );

                                                    res
                                                },
                                                typ,
                                            )
                                        },
                                        typ,
                                    );

                                    self.properties.add_property(
                                        ValueOrRegister::Register(reg),
                                        Properties::NonEmptyListOrPair,
                                    );

                                    // If this is a
                                    self.clone_value(res);

                                    self.push(res, InferredType::Any);
                                    self.ip += 2;
                                }

                                Some(Properties::ProperNonEmptyList) => {
                                    self.shadow_stack_pop();

                                    let value = self.read_from_vm_stack_unboxed(reg);
                                    let res = self.unchecked_car_unboxed(value);

                                    self.clone_value(res);

                                    self.push(res, InferredType::Any);
                                    self.ip += 2;
                                }

                                // _ if false => {
                                //     self.shadow_stack.pop();
                                //     let reg = self.register_index(reg);
                                //     let res =
                                //         self.call_function_returns_value_args("car-reg", &[reg]);
                                //     self.push(res, InferredType::Any);
                                //     self.ip += 2;
                                //     self.check_deopt();
                                // }
                                _ => {
                                    self.shadow_stack_pop();
                                    let value = self.read_from_vm_stack(reg);

                                    let typ = self.int;

                                    let is_list = self.is_type(value, SteelVal::LIST_TAG);

                                    let res = self.converging_if(
                                        is_list,
                                        |ctx| ctx.checked_car(value, reg),
                                        |ctx| {
                                            let is_pair = ctx.is_type(value, SteelVal::PAIR_TAG);

                                            ctx.converging_if(
                                                is_pair,
                                                // Inline car for a pair:
                                                |ctx| ctx.inline_pair_car(value),
                                                |ctx| {
                                                    let reg = ctx.register_index(reg);
                                                    let res = ctx.call_function_returns_value_args(
                                                        "car-reg",
                                                        &[reg],
                                                    );
                                                    ctx.check_deopt();

                                                    res
                                                },
                                                typ,
                                            )
                                        },
                                        typ,
                                    );

                                    self.properties.add_property(
                                        ValueOrRegister::Register(reg),
                                        Properties::NonEmptyListOrPair,
                                    );

                                    // If this is a
                                    self.clone_value(res);

                                    self.push(res, InferredType::Any);
                                    self.ip += 2;
                                }
                            }
                        }

                        _ => {
                            self.func_ret_val(op, 1, 2, InferredType::Any);
                            self.check_deopt();
                        }
                    }
                }

                OpCode::NEWBOX => {
                    self.func_ret_val(op, 1, 2, InferredType::Box);
                }
                OpCode::SETBOX => {
                    if let Some(last) = self.shadow_stack.get(self.shadow_stack.len() - 2).copied()
                    {
                        self.shadow_mark_local_type_from_var(last, InferredType::Box);
                    }

                    // `inline_box_primitive` already lowers `#%set-box!` with
                    // the unshared fast path - strong count of 1 stores without
                    // the spin lock and without `Weak::upgrade`. The opcode used
                    // to go out of line to `set-box-handler` -> `set_and_return`
                    // and pay both on every write, which is what `(set! x ...)`
                    // on a captured variable compiles to.
                    //
                    // Reuse that lowering rather than repeating the operand
                    // handling: the new value is *stored into the box*, so it
                    // has to be materialised as owned. A hand written version
                    // that let a borrowed register through gave the box a second
                    // owner and made `puzzle` nondeterministic.
                    let inlined = if inline_setbox_enabled() {
                        self.inline_box_primitive(
                            crate::steel_vm::primitives::steel_set_box_mutable as usize,
                            2,
                        )
                    } else {
                        None
                    };

                    match inlined {
                        Some(res) => {
                            self.ip += 2;
                            self.push(res, InferredType::Any);
                        }
                        None => {
                            self.func_ret_val(op, 2, 2, InferredType::Any);
                        }
                    }
                }
                OpCode::UNBOX => {
                    let last = self.shadow_stack.last().copied().unwrap();

                    self.shadow_mark_local_type_from_var(last, InferredType::Box);

                    match last {
                        MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i) => {
                            self.shadow_stack_pop();
                            let value = self.read_from_vm_stack(i);
                            let res = self.unbox_value_checked_register(value, false);

                            self.ip += 2;
                            self.push(res, InferredType::Any);
                        }

                        MaybeStackValue::Value(StackValue { value, .. }) => {
                            self.shadow_stack_pop();
                            let res = self.unbox_value_checked_register(value, true);
                            self.ip += 2;
                            self.push(res, InferredType::Any);
                        }

                        _ => {
                            self.func_ret_val(op, 1, 2, InferredType::Any);
                        }
                    }
                }
                OpCode::ADDREGISTER => todo!(),
                OpCode::SUBREGISTER => todo!(),
                OpCode::LTEREGISTER => todo!(),
                OpCode::SUBREGISTER1 => todo!(),
                OpCode::ALLOC => todo!(),
                OpCode::READALLOC => todo!(),
                OpCode::SETALLOC => todo!(),
                OpCode::DynSuperInstruction => todo!(),
                OpCode::Arity => todo!(),
                OpCode::ADDIMMEDIATE => todo!(),
                OpCode::SUBIMMEDIATE => todo!(),
                OpCode::LTEIMMEDIATE => todo!(),
                OpCode::BINOPADD => todo!(),
                OpCode::BINOPSUB => todo!(),
                OpCode::LTEIMMEDIATEIF => todo!(),

                // TODO: This should pretty much be able to be inlined entirely?
                OpCode::NOT => {
                    let last_ref = self
                        .shadow_stack
                        .last()
                        .copied()
                        .and_then(|x| x.as_value(self));

                    if last_ref.map(|x| x.inferred_type) == Some(InferredType::UnboxedBool) {
                        let test = last_ref.unwrap().value;
                        // let test = self.builder.ins().uextend(types::I64, test);
                        self.shadow_stack_pop();
                        // let value = self.builder.ins().icmp_imm_s(IntCC::Equal, test, 0);

                        let value = self.builder.ins().bxor_imm_u(test, 1);

                        self.push(value, InferredType::UnboxedBool);
                        self.ip += 2;
                    } else if last_ref.map(|x| x.inferred_type) == Some(InferredType::Bool) {
                        let (test, _) = self.shadow_pop();
                        // If this matches SteelVal::BoolV(false)
                        // exactly, then we're done.
                        let payload = self.unbox_value(test);
                        let test_condition = self.builder.ins().ireduce(types::I8, payload);
                        let comparison =
                            self.builder
                                .ins()
                                .icmp_imm_s(IntCC::Equal, test_condition, 0);

                        // let  = self.builder.ins().uextend(types::I64, comparison);
                        // let boolean =
                        //     self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
                        self.push(comparison, InferredType::UnboxedBool);
                        self.ip += 2;
                    } else {
                        let (test, _) = self.shadow_pop();
                        // If this matches SteelVal::BoolV(false)
                        // exactly, then we're done.

                        let is_bool = self.is_type(test, SteelVal::BOOL_TAG);
                        let payload = self.unbox_value(test);

                        let test_condition = self.builder.ins().ireduce(types::I8, payload);
                        let comparison =
                            self.builder
                                .ins()
                                .icmp_imm_s(IntCC::Equal, test_condition, 0);

                        let comparison = self.builder.ins().band(comparison, is_bool);
                        // let res = self.builder.ins().uextend(types::I64, comparison);

                        // Make sure to drop this before we're done in the event its not
                        // a boolean.
                        self.drop_tagged_value(test);

                        self.push(comparison, InferredType::UnboxedBool);
                        self.ip += 2;
                    }
                }
                OpCode::Apply => todo!(),
                OpCode::LOADINT0POP => todo!(),
                OpCode::LOADINT2POP => todo!(),
                OpCode::CGLOCALCONST => todo!(),
                OpCode::READLOCAL0CALLGLOBAL => todo!(),
                OpCode::READLOCAL1CALLGLOBAL => todo!(),
                OpCode::LISTREF => {
                    self.func_ret_val(op, 2, 2, InferredType::Any);
                }

                // Add an op code for vector ref as well.
                //
                // Probably can do a check based on the previous calls
                // to skip checks against the vector since
                // the type is inferred.
                OpCode::VECTORREF => {
                    let args = self
                        .shadow_stack
                        .get(self.shadow_stack.len() - 2..)
                        .unwrap();

                    // Generate code to handle registers for both reference and setting
                    match args {
                        &[MaybeStackValue::MutRegister(v) | MaybeStackValue::Register(v), MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i)] =>
                        {
                            let vector = self.register_index(v);
                            let vector_value = self.read_from_vm_stack(v);
                            let index_value = self.read_from_vm_stack(i);

                            let res = match self.properties.get(&ValueOrRegister::Register(i)) {
                                Some(Properties::PositiveInteger) => {
                                    self.shadow_stack_pop();
                                    self.shadow_stack_pop();

                                    let index = self.read_from_vm_stack_unboxed(i);

                                    let fallback = move |ctx: &mut Self| {
                                        ctx.call_function_returns_value_args(
                                            "vector-ref-reg-2-unboxed-index",
                                            &[vector, index],
                                        )
                                    };

                                    if INLINE_FLAT_VECTOR_REF {
                                        self.inline_vector_ref(vector_value, index_value, fallback)
                                    } else {
                                        fallback(self)
                                    }
                                }

                                _ => {
                                    let index = self.register_index(i);

                                    // If we're indexing with a value
                                    self.properties.add_property(
                                        ValueOrRegister::Register(i),
                                        Properties::PositiveInteger,
                                    );

                                    // Pop them off
                                    self.shadow_stack_pop();
                                    self.shadow_stack_pop();

                                    let fallback = move |ctx: &mut Self| {
                                        ctx.call_function_returns_value_args(
                                            "vector-ref-reg-2",
                                            &[vector, index],
                                        )
                                    };

                                    if INLINE_FLAT_VECTOR_REF {
                                        self.inline_vector_ref(vector_value, index_value, fallback)
                                    } else {
                                        fallback(self)
                                    }
                                }
                            };

                            self.push(res, InferredType::Any);
                            self.ip += 2;
                        }
                        &[MaybeStackValue::MutRegister(v) | MaybeStackValue::Register(v), MaybeStackValue::Value(_) | MaybeStackValue::Constant(_)] =>
                        {
                            let index = self.shadow_pop();
                            let vector_value = self.read_from_vm_stack(v);
                            let vector = self.register_index_small(v);
                            self.shadow_stack_pop();

                            let index_value = index.0;
                            let fallback = move |ctx: &mut Self| {
                                ctx.call_function_returns_value_args(
                                    "vector-ref-reg-1",
                                    &[vector, index_value],
                                )
                            };

                            let res = if INLINE_FLAT_VECTOR_REF {
                                self.inline_vector_ref(vector_value, index_value, fallback)
                            } else {
                                fallback(self)
                            };

                            self.push(res, InferredType::Any);
                            self.ip += 2;
                        }

                        // Neither operand is a register - both are values or
                        // constants. The inline sequence does not actually need
                        // a register, only the two values, so the old fallback
                        // here was leaving the bounds-checked fast path unused.
                        _ if generic_inline_enabled() => {
                            // `shadow_pop` asserts the operand is not spilled;
                            // in this general arm either operand may well be,
                            // and that is fine - `shadow_stack_pop` already
                            // accounts for the stack slot, and the SSA value
                            // stays valid to read. Go through `into_value` the
                            // way the other inline arms do.
                            let index = self.shadow_stack_pop().unwrap().into_value(self);
                            let index = index.as_steelval(self);
                            let vector_value = self.shadow_stack_pop().unwrap().into_value(self);
                            let vector_value = vector_value.as_steelval(self);

                            let fallback = move |ctx: &mut Self| {
                                let res = ctx.call_function_returns_value_args(
                                    "vector-ref-value",
                                    &[vector_value, index],
                                );
                                ctx.check_deopt();
                                res
                            };

                            let res = if INLINE_FLAT_VECTOR_REF {
                                self.inline_vector_ref(vector_value, index, fallback)
                            } else {
                                fallback(self)
                            };

                            self.push(res, InferredType::Any);
                            self.ip += 2;
                        }

                        _ => {
                            self.func_ret_val(op, 2, 2, InferredType::Any);
                        }
                    }
                }
                OpCode::NULLIF => todo!(),
                OpCode::UNBOXCALL => todo!(),
                OpCode::UNBOXTAIL => todo!(),
                OpCode::EQUALCONST => todo!(),

                _ => {
                    todo!()
                }
            }
        }

        self.record_stack_size();
        self.depth -= 1;

        return true;
    }

    fn sub_register_float(&mut self) {
        let value = self.shadow_stack_pop().unwrap().into_value(self);
        let register_index = self.shadow_stack_pop().unwrap().into_index();

        // Do the shift here, in an effort to avoid passing more stuff?
        let value = value.as_steelval(self);

        let local_value = self.read_from_vm_stack(register_index);
        let is_float = self.is_type(local_value, SteelVal::FLOAT_TAG);

        let sp = |ctx: &mut Self| {
            let register = ctx.builder.ins().iconst(types::I64, register_index as i64);
            let args = [register, value];
            let result = ctx.call_function_returns_value_args("sub-binop-float-reg", &args);

            // The helper reports a type error by flagging the vm rather than
            // returning one; without this the jitted code kept going and the
            // error surfaced after `with-handler` had already been unwound.
            ctx.check_deopt();

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

        // let args = [register, value];
        // let result = self.call_function_returns_value_args("sub-binop-int-reg", &args);

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
            let register = ctx.builder.ins().iconst(types::I64, register_index as i64);
            let args = [register, value];
            let result = ctx.call_function_returns_value_args("sub-binop-int-reg", &args);

            // The helper reports a type error by flagging the vm rather than
            // returning one; without this the jitted code kept going and the
            // error surfaced after `with-handler` had already been unwound.
            ctx.check_deopt();

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

        // let args = [register, value];
        // let result = self.call_function_returns_value_args("sub-binop-int-reg", &args);

        // Check the inferred type, if we know of it
        self.push(result, InferredType::Number);

        self.ip += 2;
    }

    fn sub_register_constant(&mut self) {
        // Snapshot before the operands come off: on a deopt the interpreter
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
            let register = ctx.builder.ins().iconst(types::I64, register_index as i64);
            // The constant this call subtracts, not a literal 1. Reached whenever
            // the register is not an int, or the fast path overflows - so before
            // this, `(- 100.0 5)` came back as 99.0 under the JIT.
            let value = ctx.encode_integer(constant_value as _);
            let args = [register, value];
            let result = ctx.call_function_returns_value_args("sub-binop-int-reg", &args);

            // The helper reports a type error by flagging the vm rather than
            // returning one; without this the jitted code kept going and the
            // error surfaced after `with-handler` had already been unwound.
            ctx.check_deopt();

            result
        };

        // Speculative form: no merge, so the fast arm is free to stay untagged.
        if should_speculate_for(self.function_context) {
            // Uniform shadow stack before a two way branch, the same discipline
            // every other branch here follows. Without it a value defined on one
            // side survives in translator state past a later join and is used
            // there without having been passed through it.
            //
            // It also means the exit below has nothing left to load: everything
            // is already written back, so the exit only has to set `ip` and
            // return.
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
            let rhs = self.builder.ins().iconst(types::I64, constant_value as i64);
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
            // back at whatever boundary this value escapes through - a spill, a
            // call, a return, or the deopt exit above. Until then it costs one
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
                        // If its an int, then we'll do checked subtraction:
                        // let lhs = ctx.unbox_value_to_pointer(local_value);
                        let lhs = local_value;

                        // Negate it and do an immediate add
                        let subbed = ctx.builder.ins().iadd_imm_s(lhs, -(constant_value as i64));
                        ctx.encode_value(SteelVal::INT_TAG as _, subbed)
                    } else {
                        // let lhs = ctx.unbox_value_to_pointer(local_value);
                        let lhs = local_value;
                        // let value = ctx.encode_integer(constant_value as _);
                        // let rhs = ctx.unbox_value_to_pointer(value);

                        let rhs = ctx.builder.ins().iconst(types::I64, constant_value as i64);

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
                    let rhs = ctx.builder.ins().iconst(types::I64, constant_value as i64);

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

        let is_right_int = self.is_type(local_value_r, SteelVal::INT_TAG);
        let is_left_int = self.is_type(local_value_l, SteelVal::INT_TAG);

        let both_int = self.builder.ins().band(is_right_int, is_left_int);

        // TODO: Adjust all of these things - in the event we're subtracting two
        // integer values, then we need to do
        let sp = |ctx: &mut Self| {
            let register_l = ctx.builder.ins().iconst(types::I64, register_l as i64);
            let register_r = ctx.builder.ins().iconst(types::I64, register_r as i64);
            let args = [register_l, register_r];
            let result = ctx.call_function_returns_value_args("sub-binop-both-reg", &args);

            ctx.check_deopt();

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

    fn record_stack_size(&mut self) {
        self.compilation_stats.max_stack_size = self
            .compilation_stats
            .max_stack_size
            .max(self.shadow_stack.len());
    }

    // TODO: We have to include the arity check as well!
    // Otherwise, we're going to have a problem.
    fn inline_call_func(
        &mut self,
        arity: usize,
        name: Option<&str>,
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
                // Always take the check here too - see the note in the non-tail
                // call path above.
                ctx.push_to_many_vm_stack_let_var_new(&args_off_the_stack);

                // The checked push reserves exactly what it needs, so there
                // is no headroom left over:
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

                let should_continue = ctx
                    .builder
                    .ins()
                    .band(should_trampoline, super_instruction_exists);

                let should_yield = ctx.builder.ins().bxor_imm_u(should_continue, 1);
                let fallback_ip = ctx.ip - 1;
                // let fallback_ip = ctx.ip;
                ctx.update_ip_native_if_yield(vm_ctx, should_yield, fallback_ip + 1);

                let res = ctx.converging_if(
                    should_continue,
                    |ctx| {
                        if should_clone {
                            // Increment the ref count of the closure
                            ctx.increment_ref_count_closure(closure);
                        }

                        let body_exp_offset = closure_field_offset(offset_of!(ByteCodeLambda, body_exp));

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

                        ctx.push_stack_frame(arity as _, closure, instr_fat_ptr, fallback_ip, false);

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

                        let is_still_native = ctx.builder.ins().load(
                            types::I8,
                            MemFlagsData::trusted(),
                            vm_ctx,
                            offset_of!(VmCore, is_native) as i32,
                        );

                        ctx.converging_if(is_still_native, |_| res, |ctx| ctx.encode_void(), typ)
                    },
                    |ctx| {
                        let arity = ctx.builder.ins().iconst(types::I64, arity as i64);
                        let fallback_ip = ctx.builder.ins().iconst(types::I64, fallback_ip as i64);
                        // TODO: Set up the closure for multi arity?
                        ctx.call_function_returns_value_args(
                            "#%setup-closure-arity",
                            &[closure, arity, fallback_ip],
                        )
                    },
                    typ,
                );

                res
            },
            |ctx| {
                ctx.shadow_stack = old_stack.clone();
                ctx.value_to_local_map = old_map.clone();

                if let Some(name) = name {
                    let v = if should_pop_func {
                        ctx.call_function(arity, name, false)
                    } else {
                        ctx.call_function_with_func(arity, name, false, func)
                    };

                    v
                } else {
                    todo!("Implement spilled function call (arity {})", arity);
                }
            },
            typ,
        );

        self.push(res, InferredType::Any)
    }

    /// Emit a tail call to a primitive inline, when we recognise it.
    ///
    /// The callee is compared by function pointer rather than by name: the
    /// global's value is already in hand at the call site, and an identity
    /// check on the `FuncV` is both cheaper and harder to get wrong than
    /// resolving a symbol.
    ///
    /// Only primitives with an existing inline lowering are handled; anything
    /// else returns None and takes the generic path. `#%unbox` becomes a real
    /// inline sequence; `#%set-box!` still calls a native helper, but a direct
    /// one rather than the deopt trampoline, which skips the global lookup,
    /// the `SteelVal` kind match and the argument marshalling.
    fn inline_primitive_tail_call(
        &mut self,
        func: Option<&SteelVal>,
        arity: usize,
    ) -> Option<Value> {
        if !inline_primitive_tail_calls_enabled() {
            return None;
        }

        let func = func?;
        let extra = extra_primitive_tail_calls_enabled();

        if let SteelVal::BuiltIn(b) = func {
            if !extra {
                return None;
            }
            return self.inline_mutable_struct_tail_call(*b as usize, arity);
        }

        let SteelVal::FuncV(f) = func else {
            return None;
        };
        let target = *f as usize;

        // `eq?` and `vector-set!` already have inline lowerings for a call in
        // any other position: reuse them, then take the result back off the
        // operand stack to return it. `eq?`'s result is an unboxed bool, so
        // `as_steelval` boxes it. Their `ip` advance is irrelevant here - the
        // tail call arm moves past the end of the function once this returns.
        if extra && target == steel_eq as usize && arity == 2 {
            self.eq();
            return Some(self.pop_as_steelval());
        }

        if extra && target == steel_mut_vec_set as usize && arity == 3 {
            self.vector_set();
            return Some(self.pop_as_steelval());
        }

        // Same story for the integer divisions. Inlining them only in operand
        // position left the tail call deopting once per iteration: in
        // `bv2string` the prng ends in `(remainder (quotient x 8) n)`, and that
        // single site was **99.79% of every tail call deopt in the program** -
        // 2.48M of 2.49M - dropping into the interpreter each time.
        if inline_divmod_enabled() && arity == 2 {
            if let Some(mode) = divmod_mode(*f) {
                return Some(self.inline_int_divmod(mode, *f));
            }
        }

        self.inline_box_primitive(target, arity)
    }

    /// `#%unbox` / `#%set-box!` inline, in any position. Returns the result
    /// without pushing it, or `None` (having emitted nothing) when the generic
    /// call has to handle it.
    fn inline_box_primitive(&mut self, target: usize, arity: usize) -> Option<Value> {
        if target == crate::steel_vm::primitives::steel_unbox_mutable as usize && arity == 1 {
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

        if target == crate::steel_vm::primitives::steel_set_box_mutable as usize && arity == 2 {
            // Operand order is (box, new-value); the box is the one whose
            // ownership we may have to release, matching the read path.
            let owned = matches!(
                self.shadow_stack
                    .get(self.shadow_stack.len().checked_sub(2)?),
                Some(MaybeStackValue::Value(_))
            );

            let args = self
                .split_off(arity)
                .into_iter()
                .map(|x| x.0)
                .collect::<Vec<_>>();

            return Some(self.set_box_value_checked_register(args[0], args[1], owned));
        }

        None
    }

    /// Whether `inline_box_primitive` will take this call, without emitting.
    fn box_primitive_inlinable(&self, target: usize, arity: usize) -> bool {
        if target == crate::steel_vm::primitives::steel_unbox_mutable as usize && arity == 1 {
            return matches!(
                self.shadow_stack.last(),
                Some(
                    MaybeStackValue::MutRegister(_)
                        | MaybeStackValue::Register(_)
                        | MaybeStackValue::Value(_)
                )
            );
        }
        target == crate::steel_vm::primitives::steel_set_box_mutable as usize
            && arity == 2
            && self.shadow_stack.len() >= 2
    }

    fn pop_as_steelval(&mut self) -> Value {
        let top = self.shadow_stack_pop().unwrap().into_value(self);
        top.as_steelval(self)
    }

    /// `#%make-mutable-struct` in tail position, as a direct call to the
    /// allocation instead of the generic tail call handler.
    ///
    /// Only when the constructor's arguments are the whole operand stack. The
    /// allocation can run a collection, and the collector's roots are the vm
    /// stack plus these arguments: an operand still held only in the jitted
    /// frame would not be one. In tail position nothing else is live anyway, so
    /// this costs nothing in practice - it is `(define (cons a b) (mpair a b))`.
    fn inline_mutable_struct_tail_call(&mut self, builtin: usize, arity: usize) -> Option<Value> {
        if builtin != crate::steel_vm::primitives::make_mutable_struct as usize {
            return None;
        }

        let name = CallMutableStructConstructorsDefinitions::arity_to_name(arity)?;

        if self.shadow_stack.len() != arity {
            return None;
        }

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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
    /// definition in a block that no longer dominates the use - cranelift
    /// rejects it with "uses value from non-dominating inst".
    fn inline_byte_vector_op(&mut self, function: FunctionSignature, arity: usize) -> Value {
        let base = self.shadow_stack.len() - arity;

        // A register is a borrow that stays live in its slot; a spilled value is
        // ours to release.
        let owned = matches!(
            self.shadow_stack.get(base),
            Some(MaybeStackValue::Value(_))
        );

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
                // The helper takes ownership, so a borrow has to be cloned
                // first; a spilled value we already own.
                if !owned {
                    ctx.clone_value(bytevector_value);
                }

                let func = ctx
                    .builder
                    .ins()
                    .iconst(ctx.module.target_config().pointer_type(), function as i64);
                let fallback_ip = ctx.builder.ins().iconst(types::I64, ip as i64);

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
    /// None of these have an opcode, so every call went out through
    /// `call_primitive_function_deopt_2`, which publishes the safepoint and
    /// hands the arguments to the generic primitive. In `bv2string`'s random
    /// number generator that path was 26% of the run.
    ///
    /// The guard is wider than the arithmetic strictly needs. Cranelift's
    /// `sdiv`/`srem` trap on a zero divisor *and* on `isize::MIN / -1`, and the
    /// primitive answers those two with something that is not a fixnum anyway -
    /// an error, and a `BigNum` - so `b == -1` goes out of line rather than
    /// being special cased here. Within that guard neither the truncating nor
    /// the flooring form can overflow, so there is no exit on the fast path:
    /// `q - 1` is bounded because `|q| <= |a| / 2`, and `r + b` is bounded
    /// because `r` is smaller than `b` and points the other way.
    ///
    /// The result is pushed as `Any`, not `Int`: the fallback can return a
    /// float or a `BigNum`, and both arms have to describe the merge.
    fn inline_int_divmod(&mut self, mode: DivMode, function: FunctionSignature) -> Value {
        let rhs = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };
        let lhs = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };

        let lhs_is_int = self.is_type(lhs, SteelVal::INT_TAG);
        let rhs_is_int = self.is_type(rhs, SteelVal::INT_TAG);
        let both_int = self.builder.ins().band(lhs_is_int, rhs_is_int);

        // Reading the payload is pure bit extraction, so it is fine to do
        // before the tag check has decided anything.
        let divisor = self.unbox_value_to_pointer(rhs);
        let not_zero = self.builder.ins().icmp_imm_s(IntCC::NotEqual, divisor, 0);
        let not_neg_one = self.builder.ins().icmp_imm_s(IntCC::NotEqual, divisor, -1);
        let safe_divisor = self.builder.ins().band(not_zero, not_neg_one);
        let inlineable = self.builder.ins().band(both_int, safe_divisor);

        self.converging_if(
            inlineable,
            |ctx| {
                let l = ctx.unbox_value_to_pointer(lhs);
                let r = ctx.unbox_value_to_pointer(rhs);

                let res = match mode {
                    DivMode::TruncQuotient => ctx.builder.ins().sdiv(l, r),
                    DivMode::TruncRemainder => ctx.builder.ins().srem(l, r),

                    // The floored forms differ from the truncated ones only
                    // when the remainder is non zero and points the opposite
                    // way to the divisor - `(r ^ b) < 0` is that sign test
                    // without materialising either comparison.
                    DivMode::FloorQuotient | DivMode::FloorRemainder => {
                        let rem = ctx.builder.ins().srem(l, r);
                        let rem_nonzero = ctx.builder.ins().icmp_imm_s(IntCC::NotEqual, rem, 0);
                        let signs = ctx.builder.ins().bxor(rem, r);
                        let opposite =
                            ctx.builder.ins().icmp_imm_s(IntCC::SignedLessThan, signs, 0);
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
            |ctx| {
                let func = ctx.builder.ins().iconst(
                    ctx.module.target_config().pointer_type(),
                    function as i64,
                );
                let fallback_ip = ctx.builder.ins().iconst(types::I64, ctx.ip as i64);

                let res = ctx.call_function_returns_value_args(
                    "call_primitive_function_deopt_2",
                    &[func, fallback_ip, lhs, rhs],
                );
                ctx.check_deopt();
                res
            },
            types::I128,
        )
    }

    /// Inline a two-operand integer comparison, checking both tags at runtime.
    ///
    /// The existing fast paths for `=`, `<` and friends all require one side to
    /// be a compile-time constant or a known register. That covers loop
    /// counters against literals but not generic arithmetic, which then paid a
    /// full native call per comparison. Here both operands are materialised as
    /// values, both tags are tested, and the happy path is a single `icmp` on
    /// the payloads.
    ///
    /// `fallback` must be a helper returning an *unboxed* bool, so both arms of
    /// the branch agree on type and the result can be consumed directly by a
    /// following `if` without boxing.
    fn inline_int_compare_two(&mut self, cc: IntCC, fallback: &str) -> Value {
        let rhs = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };
        let lhs = {
            let sv = self.shadow_stack_pop().unwrap().into_value(self);
            sv.as_steelval(self)
        };

        let lhs_is_int = self.is_type(lhs, SteelVal::INT_TAG);
        let rhs_is_int = self.is_type(rhs, SteelVal::INT_TAG);
        let both_int = self.builder.ins().band(lhs_is_int, rhs_is_int);

        self.converging_if(
            both_int,
            |ctx| {
                // Tags already checked, so the payloads are the whole story.
                let l = ctx.unbox_value_to_pointer(lhs);
                let r = ctx.unbox_value_to_pointer(rhs);
                ctx.builder.ins().icmp(cc, l, r)
            },
            |ctx| {
                let vm_ctx = ctx.get_ctx();
                let res =
                    ctx.call_function_returns_value_args_no_context(fallback, &[vm_ctx, lhs, rhs]);
                ctx.check_deopt();
                res
            },
            types::I8,
        )
    }

    fn inline_float_binop_two(&mut self, op: OpCode, fallback: &str) -> Value {
        let rhs = self.shadow_stack_pop().unwrap().into_value(self);
        let rhs = rhs.as_steelval(self);
        let lhs = self.shadow_stack_pop().unwrap().into_value(self);
        let lhs = lhs.as_steelval(self);

        let lhs_is_float = self.is_type(lhs, SteelVal::FLOAT_TAG);
        let rhs_is_float = self.is_type(rhs, SteelVal::FLOAT_TAG);
        let both_float = self.builder.ins().band(lhs_is_float, rhs_is_float);

        self.converging_if(
            both_float,
            |ctx| {
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
            |ctx| {
                let vm_ctx = ctx.get_ctx();
                let res =
                    ctx.call_function_returns_value_args_no_context(fallback, &[vm_ctx, lhs, rhs]);
                ctx.check_deopt();
                res
            },
            types::I128,
        )
    }

    fn slow_path_deopt_tail_call(&mut self, function_index: usize, arity: usize) {
        let name = CallGlobalTailFunctionDefinitions::arity_to_name(arity);
        // let name = None;

        if let Some(name) = name {
            // TODO: We need to spill the local variables here!
            // This function pushes back on to the stack, and then we should just
            // return since we're done now.
            let v = self.call_global_function(arity, name, function_index, true);

            self.push(v, InferredType::Any);
        } else {
            let name = "call-global-tail-spilled";

            // TODO: We need to spill the local variables here!
            let v = self.call_global_function_spilled(arity, name, function_index, true);

            self.push(v, InferredType::Any)
        }

        self.check_deopt();

        self.ip = self.instructions.len() + 1;

        self.depth -= 1;
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
        self.builder.ins().iconst(types::I64, index as i64)
    }

    fn register_index_small(&mut self, index: usize) -> Value {
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

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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
    // run. The free list holds a strong ref while the slot is live, so thats cold.
    fn inline_weak_decrement(&mut self, ptr: Value, drop_fn: &'static str, drop_arg: Value) {
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

        let hit_zero = self.builder.ins().icmp_imm_s(IntCC::Equal, previous, 1);

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
                ctx.call_function_args_no_context(drop_fn, &[drop_arg]);
            },
            |_| {},
        );
    }

    fn drop_weak_rc(&mut self, value: Value) {
        // drop-one takes a whole SteelVal, so it gets the tagged value - not the
        // unboxed pointer we do the arithmetic on.
        let ptr = self.unbox_value_to_pointer(value);

        if USE_INLINE_DROP_HEAP_BOX {
            self.inline_weak_decrement(ptr, "drop-one", value);
        } else {
            self.call_function_args_no_context("drop-one", &[value]);
        }
    }

    fn drop_tagged_value(&mut self, value: Value) {
        let tag = self.get_tag(value);

        let mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::UNBOXED_MASK as i64);
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
                let special_rc_mask = ctx
                    .builder
                    .ins()
                    .iconst(types::I64, SteelVal::SPECIAL_RC_MASK as i64);
                let special_rc_shifted = ctx.builder.ins().ushr(special_rc_mask, tag);
                let is_special_rc = ctx.builder.ins().band_imm_u(special_rc_shifted, 1);

                ctx.converging_if_no_value(
                    is_special_rc,
                    |ctx| {
                        ctx.drop_biased_rc(value);
                    },
                    |ctx| {
                        let weak_rc_mask = ctx
                            .builder
                            .ins()
                            .iconst(types::I64, SteelVal::WEAK_RC_MASK as i64);
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

    fn drop_tagged_value_old(&mut self, value: Value) {
        let tag = self.get_tag(value);

        let mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::UNBOXED_MASK as i64);
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

        let std_mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::STANDARD_RC_MASK as i64);
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
        self.drop_value(value); // straight RC decrement
        self.builder.ins().jump(drop_merge, &[]);

        self.builder.switch_to_block(special_rc_block);
        self.builder.seal_block(special_rc_block);
        self.drop_biased_rc(value);
        self.builder.ins().jump(drop_merge, &[]);

        self.builder.switch_to_block(drop_merge);
        self.builder.seal_block(drop_merge);
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
    }

    /// Is this object biased to the running thread? BRC packs the owner id into
    /// the high bits of the biased half-word, so read it out and compare.
    ///
    /// An unowned object has a zero id, and `VmCore::thread_id` is never zero
    /// in jitted code, so the unowned case simply fails to match.
    fn check_value_tl(&mut self, value: Value) -> Value {
        let thread_id = self.get_thread_id();
        // Not readonly: the owner id now shares this word with the count, which
        // the increment and decrement paths write.
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

    // We can decide on whether to actually drop the value,
    // based on whether or not the type needs drop glue at all.
    //
    // In the event we've asserted that this type does _not_
    // need drop glue, then we're good to continue without
    // emitting drop glue. What that means in this case is that
    // something like unboxed types don't need drop glue.
    fn drop_tagged_value_inferred_type(
        &mut self,
        tagged_value: Value,
        inferred_type: InferredType,
    ) {
        todo!()
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

            self.builder
                .ins()
                .store(MemFlagsData::trusted(), sub_one, value, biased_word_offset());

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

            self.call_function_args_no_context("drop-value-post-fast-dec", &[tagged_value]);

            self.builder.ins().jump(merge_block, &[]);

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);

            self.builder.ins().jump(total_merge, &[]);
        }

        {
            self.builder.switch_to_block(no_tl);
            self.builder.seal_block(no_tl);

            self.call_function_args_no_context("drop-value-slow-dec", &[tagged_value]);

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

            self.builder
                .ins()
                .store(MemFlagsData::trusted(), sub_one, value, biased_word_offset());

            let sub_one = self
                .builder
                .ins()
                .ushr_imm_u(sub_one, steel_rc::biased_counter_shift() as i64);

            let yes_drop = self.builder.create_block();
            let merge_block = self.builder.create_block();

            // let updated_count =
            //     self.builder
            //         .ins()
            //         .load(Type::int(32).unwrap(), MemFlagsData::new(), value, 8);

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
            self.call_function_args_no_context("drop-value-post-fast-dec-closure", &[value]);

            self.builder.ins().jump(merge_block, &[]);

            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);

            self.builder.ins().jump(total_merge, &[]);
        }

        // Slow drop with decrement included
        {
            self.builder.switch_to_block(no_tl);
            self.builder.seal_block(no_tl);

            self.call_function_args_no_context("drop-value-slow-dec-closure", &[value]);

            self.builder.ins().jump(total_merge, &[]);
        }

        self.builder.switch_to_block(total_merge);
        self.builder.seal_block(total_merge);
    }

    // TODO: Replace this with a more sophisticated implementation that doesn't necessarily
    // need the call if we have something like that
    fn drop_value(&mut self, value: Value) {
        self.call_function_args_no_context("drop-one", &[value]);
    }

    // value is a HeapRef<_>, i.e. the bare Weak pointer, which is what
    // drop-box / drop-boxed-vec take by value.
    fn drop_heap_box(&mut self, value: Value) {
        if USE_INLINE_DROP_HEAP_BOX {
            self.inline_weak_decrement(value, "drop-box", value);
        } else {
            self.call_function_args_no_context("drop-box", &[value]);
        }
    }

    fn drop_heap_box_vec(&mut self, value: Value) {
        if USE_INLINE_DROP_HEAP_BOX {
            self.inline_weak_decrement(value, "drop-boxed-vec", value);
        } else {
            self.call_function_args_no_context("drop-boxed-vec", &[value]);
        }
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
    /// inline element, so an inline cell cannot be read for it - it holds
    /// exactly one element and its cursor is always 1.
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
        let link =
            self.builder
                .ins()
                .load(types::I64, MemFlagsData::trusted(), value, list_next_offset());
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
                ctx.builder.ins().iadd_imm_s(
                    chunk,
                    SteelList::<SteelVal>::vector_header_size() as i64,
                )
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

    // First, check the tag:
    fn unchecked_car(&mut self, value: Value) -> Value {
        let value = self.unbox_value_to_pointer(value);
        self.unchecked_car_unboxed(value)
    }

    fn checked_car(&mut self, original_value: Value, reg: usize) -> Value {
        // let is_list = self.is_type(value, SteelVal::LIST_TAG);
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
                let res = ctx.call_function_returns_value_args("car-reg", &[reg]);
                ctx.check_deopt();
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

    /*
    fn read_char(&mut self, arity: usize) {
        let test_stack = self
            .shadow_stack
            .get(self.shadow_stack.len() - arity..)
            .unwrap()
            .to_vec();

        let shape = test_stack
            .iter()
            .map(|x| match x {
                MaybeStackValue::Value(_) => 0,
                MaybeStackValue::MutRegister(_) => 2,
                MaybeStackValue::Register(_) => 1,
                MaybeStackValue::Constant(_) => 0,
            })
            .collect::<Vec<_>>();

        let func = CallRegisterPrimitiveFixedDefinitions::shape_to_name(&shape).unwrap();

        let function = self.builder.ins().iconst(
            self.module.target_config().pointer_type(),
            crate::primitives::ports::read_char_single_ref as i64,
        );

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

        let mut args = vec![function, fallback_ip];

        // dbg!(test_stack);
        let additional_args = self.split_off_reg(arity);
        // dbg!(&additional_args);
        args.extend(additional_args);

        let result = self.call_function_returns_value_args(func, &args);
        // A char, or the eof object at the end of input.
        self.push(result, InferredType::Any);
        self.ip += 1;
        self.check_deopt();
    }
    */

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
            // println!(
            //     "Getting here -> Reading a local value, instead of generating a register instruction: {:?}", op
            // );

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

            // MaybeStackValue::Register(())
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

        // if payload < self.arity as _ {
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
    // into the CALLGLOBAL - push the callee and let the FUNC apply it.
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

        if INLINE_STRUCT_FUNCTION_CALLS {
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

        if let Some(name) = name {
            let result = self.call_global_function(arity, name, function_index, false);

            // Assuming this worked, we'll want to push this result on to the stack.
            self.push(result, InferredType::Any);
        } else {
            let name = "call-global-spilled";

            let v = self.call_global_function_spilled(arity, name, function_index, false);

            // Only reached when the callee returned a value rather than setting
            // up a frame - the handler dropped the spilled arguments, so drop
            // them from the model too.
            self.check_deopt();
            self.shadow_stack.truncate(self.shadow_stack.len() - arity);
            self.properties.cached_lookups.stack_length_capacity = self
                .properties
                .cached_lookups
                .stack_length_capacity
                .saturating_add(arity);

            self.push(v, InferredType::Any);
            return;
        }

        // Then, we're gonna check the result and see if we should deopt
        self.check_deopt();
    }

    fn call_set(&mut self, index: usize, value: Value) -> Value {
        let local_callee = self.get_local_callee("set-handler");

        let ctx = self.get_ctx();

        let index = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), index as i64);

        let arg_values = vec![ctx, index, value];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];

        result
    }

    fn pop_single(&mut self) {
        let last = self.shadow_pop();

        let local_callee = self.get_local_callee("drop-value");
        let ctx = self.get_ctx();

        let arg_values = vec![ctx, last.0];

        let _ = self.builder.ins().call(local_callee, &arg_values);
    }

    fn pop_value_from_vm_stack(&mut self) -> Value {
        let vm_ctx = self.get_ctx();
        self.inline_pop_from_stack(vm_ctx)
    }

    fn call_test_handler_register(&mut self, register: usize) -> Value {
        let local_callee = self.get_local_callee("if-branch-register");

        let ctx = self.get_ctx();

        // Advance to the next thing
        // self.ip += 1;

        let register = self.builder.ins().iconst(types::I64, register as i64);

        let arg_values = [ctx, register];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_test_handler(&mut self, test_value: Value) -> Value {
        let local_callee = self.get_local_callee("if-branch-value");

        let ctx = self.get_ctx();

        // Advance to the next thing
        // self.ip += 1;

        let arg_values = [ctx, test_value];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn translate_tco_jmp(&mut self, payload: usize) {
        self.spill_stack();

        let local_callee = self.get_local_callee("tco-jump");

        let ctx = self.get_ctx();
        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let arg_values = [ctx, arity];
        let _call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn translate_tco_jmp_no_arity_loop_no_spill(&mut self, payload: usize) {
        if std::env::var_os("STEEL_DEBUG_TCO").is_some() { // TEMP-DEBUG
            eprintln!("[selftail fn={} ip={} payload={} shadow={:?} lets={:?}]",
                self.name, self.ip, payload, self.shadow_stack, self.let_var_stack);
        }
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
                // println!(
                //     "{} - tail call amount dropped: {}",
                //     self.name, amount_dropped
                // );

                // Original payload, is the original amount to call
                let original_payload = payload;

                // How many elements did we drop off
                let payload = payload - amount_dropped;

                let args_before_drop = self.shadow_stack.get(self.shadow_stack.len() - payload);

                // Aggressive optimization: if the value is moved, we don't need to put a drop
                // call inlined here
                if args_before_drop
                    .iter()
                    .all(|x| matches!(x, MaybeStackValue::MutRegister(_)))
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

                self.emit_self_tail_jump(&proven);

                return;
            }
        }

        // println!("---- Getting to this split off step -----");
        // println!(
        //     "{:?}",
        //     self.shadow_stack.get(self.shadow_stack.len() - payload..)
        // );

        let args_off_the_stack = self.split_off(payload);

        // println!("-----------------------------------------");

        if USE_INLINE_TAIL_CALL {
            let args = args_off_the_stack
                .into_iter()
                .map(|x| x.0)
                .collect::<Vec<_>>();

            self.inline_call_self_tail_call_no_arity_loop(payload as _, &args);
        } else {
            let name = CallSelfTailCallNoArityLoopDefinitions::arity_to_name(payload).unwrap();
            let local_callee = self.get_local_callee(name);
            let ctx = self.get_ctx();

            let arity = self
                .builder
                .ins()
                .iconst(Type::int(16).unwrap(), payload as i64);

            let mut arg_values = vec![ctx, arity];
            arg_values.extend(args_off_the_stack.iter().map(|x| x.0));
            let _call = self.builder.ins().call(local_callee, &arg_values);
        }

        self.emit_self_tail_jump(&proven);
    }

    /// Back to the top of the loop. The arguments are already written back.
    fn emit_loop_jump(&mut self) {
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
    }

    /// The end of a self tail call, once the arguments are written back. Where
    /// it goes depends on which copy of the function this is: the generic copy
    /// moves into the specialized one when the seeded slots hold fixnums, and
    /// the specialized copy moves back out when one might not.
    fn emit_self_tail_jump(&mut self, proven: &[bool]) {
        match self.spec_mode.clone() {
            SpecMode::None => self.emit_loop_jump(),

            // The entry guard already decided; a generic copy stays generic.
            SpecMode::Generic { .. } => {
                self.emit_loop_jump();
            }

            SpecMode::Specialized { generic_id, seed, .. } => {
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

                // Read the tags from memory on purpose: the tail call just wrote
                // new values into these slots, so the translator's facts about
                // them are stale.
                let ctx = self.get_ctx();
                let sp = self.get_sp(ctx);
                let buf_ptr = self.stack_buf_ptr(ctx);
                let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
                let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

                let generic_block = self.builder.create_block();
                emit_slot_type_checks(&mut self.builder, frame_base, &unproven, generic_block);
                let pass_block = self.builder.current_block().unwrap();

                // Fill the fallback first and jump back to the top last: the loop
                // jump leaves the builder in a fresh block that the rest of the
                // translation continues into, and a block the builder switches
                // away from while still empty never makes it into the function.
                self.builder.seal_block(generic_block);
                self.builder.switch_to_block(generic_block);
                self.emit_tail_call_into(generic_id);

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

        // Read from VM stack, write back, drop value.
        // then, truncate
        // for (i, arg) in args.iter().enumerate() {
        //     self.write_to_vm_stack(i as _, *arg);
        // }

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
    /// truncating is only a length store - the runtime loop that loads each
    /// slot's tag to discover there is nothing to drop can go.
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

        // Read from VM stack, write back, drop value.
        // then, truncate
        // for (i, arg) in args.iter().enumerate() {
        //     self.write_to_vm_stack(i as _, *arg);
        // }

        self.write_to_vm_stack_starting_at(0, args, false);

        let index = self.get_sp(vm_ctx);

        let index = self.builder.ins().iadd_imm_s(index, arity);

        self.truncate_stack(vm_ctx, index, None);
    }

    fn call_function_directly(
        &mut self,
        func: Value,
        arity: usize,
        name: &str,
        tail: bool,
    ) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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

    fn call_function(&mut self, arity: usize, name: &str, tail: bool) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

        let func = self.shadow_pop().0;

        let mut arg_values = vec![ctx, func, fallback_ip];

        // Use split off instead?
        // arg_values.extend(
        //     self.stack
        //         .drain(self.stack.len() - arity..)
        //         .map(|x| x.value),
        // );

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

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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
        arity: usize,
        name: &str,
        tail: bool,
        func: Value,
        args: Vec<Value>,
    ) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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

    fn test(&mut self) {
        // self.builder.ins().iconcat(lo, hi)
        // self.builder.ins()
    }

    /// In a specialized copy, a self call whose arguments are proven to have
    /// the seeded types can go straight to this copy. Returns it, with the type
    /// its result is assumed to have.
    fn direct_self_call_target(&self, arity: usize) -> Option<(FuncId, Option<InferredType>)> {
        let SpecMode::Specialized { self_id, seed, assume_result, .. } = &self.spec_mode else {
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

    fn call_self_function_experimental(
        &mut self,
        arity: usize,
        func: Gc<ByteCodeLambda>,
        target: Option<FuncId>,
    ) -> Value {
        // let local_callee = self.get_local_callee(name);

        let id = func.id;

        /* TODO: Add this back!!
        let _ = steel_rc::BiasedRc::into_raw(func.body_exp.clone());
        */

        let callee_is_self = self.callee_shares_our_instructions(func.body_exp());
        let instr_fat_ptr = self.rooted_instructions_const(func.body_exp());

        func.clone().into_raw();

        // Horrendous crimes, but we'll allow it. We'll also leak the instructions...
        let lookup_index = self.builder.ins().iconst(Type::int(64).unwrap(), unsafe {
            std::mem::transmute::<Gc<ByteCodeLambda>, i64>(func)
        });

        // let fallback_ip = self
        //     .builder
        //     .ins()
        //     .iconst(Type::int(64).unwrap(), self.ip as i64);

        let fallback_ip = self.ip;

        // Advance to the next thing
        self.ip += 1;

        // let mut arg_values = vec![ctx, lookup_index, fallback_ip];

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

        // Horrendous crimes, but we'll allow it
        let lookup_index = self.builder.ins().iconst(Type::int(64).unwrap(), unsafe {
            std::mem::transmute::<Gc<ByteCodeLambda>, i64>(func)
        });

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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
        // First things first, we first get the args off. Worst case, we'll spill
        // these back on to the args
        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        let vm_ctx = self.get_ctx();
        let id = func.id;

        // TODO: Consider if we need to spill the whole stack here. We could also
        // just call drop, but writing the values to the stack will help us drop
        // them.
        self.spill_cloned_stack();

        let instr_fat_ptr = self.rooted_instructions_const(func.body_exp());

        // Deliberately *not* baking `func.super_instructions()` in as a
        // constant. The callee's compiled body can be replaced later - a tier
        // upgrade does exactly that - and a baked address would keep calling
        // whatever was current when this caller happened to be compiled. The
        // lambda pointer below is stable (its refcount is leaked), so load the
        // entry point through it at run time instead; it is one dependent load
        // on a line that is warm anyway, and it always reaches the newest body.
        debug_assert!(func.super_instructions().is_some());

        func.clone().into_raw();

        // Horrendous crimes, but we'll allow it. We'll also leak the instructions...
        let lookup_index = self.builder.ins().iconst(Type::int(64).unwrap(), unsafe {
            std::mem::transmute::<Gc<ByteCodeLambda>, i64>(func)
        });

        self.ip += 1;

        self.increment_ref_count_closure(lookup_index);

        // Pass this through
        let offset = self.update_last_stackframe(vm_ctx, lookup_index);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            instr_fat_ptr,
            vm_ctx,
            offset_of!(VmCore, instructions) as i32,
        );

        // let offset = self.read_last_sp(vm_ctx, None);

        // Then, truncate the stack back to where we were before:
        // self.truncate_stack(vm_ctx, offset, None);
        // self.push_to_many_vm_stack_let_var_new(&args);

        self.truncate_stack_with_args(vm_ctx, offset, &args);

        // Implement the body of `new_handle_tail_call_closure` here

        // New args now that we've spilled everything
        let args = [vm_ctx];

        let zero = self.builder.ins().iconst(types::I64, 0);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            zero,
            vm_ctx,
            offset_of!(VmCore, ip) as i32,
        );

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

        /*

        let sig_ref = self.get_jit_func(id);
        self.builder.ins().return_call(sig_ref, &args);

        */

        // // Look up the return type of this one:
        // if let Some(ret_types) = self.function_return_types.get(&id) {
        //     println!("inline global tail call return types: {:#?}", ret_types);
        // }

        let cold_block = self.builder.create_block();
        self.builder.switch_to_block(cold_block);
    }

    fn inline_local_tail_call(&mut self, arity: usize, closure: Value, original: Value) {
        // First things first, we first get the args off. Worst case, we'll spill
        // these back on to the args
        let args = self
            .split_off(arity)
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();

        // Pinning the vm context to r15 with get_pinned_reg/set_pinned_reg was
        // implemented and measured: ctx reloads in fib went 99 -> 2 and spill
        // traffic 18.4% -> 14.9%, but instructions went 1287 -> 1372 and fib was
        // 17% slower, reproducibly. A reload that hits L1 is already free at this
        // IPC, while reserving r15 costs the allocator a register in a function
        // that already uses all five callee-saved ones. See `get_ctx`.

        let vm_ctx = self.get_ctx();

        // TODO: This is not going to work here. Instead, we need to load
        // the id of the instruction.
        // See note in inline_call_func: super_instructions is effectively
        // readonly for execution purposes (tier upgrades install a still-valid
        // pointer; a stale None just misses the JIT optimization opportunity).
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

        // I think I just need to drop the values, not spill them, since they're
        // going to get truncated anyway
        self.spill_cloned_stack();

        // TODO: Check if super instruction exists here:

        self.converging_if_no_value(
            super_instruction_exists,
            |ctx| {
                let body_exp_offset = closure_field_offset(offset_of!(ByteCodeLambda, body_exp));

                // See note in inline_call_func: body_exp's buffer pointer and
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
                let fallback_ip = ctx.ip;

                ctx.ip += 1;

                // Pass this through
                let offset = ctx.update_last_stackframe(vm_ctx, closure);

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    instr_fat_ptr,
                    vm_ctx,
                    offset_of!(VmCore, instructions) as i32,
                );

                // let offset = ctx.read_last_sp(vm_ctx, None);

                // Then, truncate the stack back to where we were before:
                // ctx.truncate_stack(vm_ctx, offset, None);
                // ctx.push_to_many_vm_stack_let_var_new(&args);

                ctx.truncate_stack_with_args(vm_ctx, offset, &args);

                // Implement the body of `new_handle_tail_call_closure` here

                // New args now that we've spilled everything
                let args = [vm_ctx];

                let zero = ctx.builder.ins().iconst(types::I64, 0);

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    zero,
                    vm_ctx,
                    offset_of!(VmCore, ip) as i32,
                );

                let sig_ref = ctx.create_jit_sig_ref();
                ctx.builder
                    .ins()
                    .return_call_indirect(sig_ref, super_instruction, &args);

                let cold_block = ctx.builder.create_block();
                ctx.builder.switch_to_block(cold_block);
            },
            |ctx| {
                let name = CallFunctionTailDefinitions::arity_to_name(arity);

                if let Some(name) = name {
                    let v = ctx.call_function_with_func_with_args(
                        arity,
                        name,
                        true,
                        original,
                        args.clone(),
                    );
                    ctx.push(v, InferredType::Any);
                } else {
                    todo!(
                        "Implement spilled function call bail out case (arity {})",
                        arity
                    );
                }
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
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let lookup_index = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), function_index as i64);

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

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

    // call_global_function without materialising the operand stack
    fn call_global_function_no_spill(
        &mut self,
        arity: usize,
        name: &str,
        function_index: usize,
    ) -> Value {
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let lookup_index = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), function_index as i64);
        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);
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
        name: &str,
        function_index: usize,
        tail: bool,
    ) -> Value {
        // println!("--------------------Call global tail spilled---------------------");

        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let lookup_index = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), function_index as i64);

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

        let arity_value = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), arity as i64);

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

    fn get_thread_id(&mut self) -> Value {
        if let Some(thread_id) = self.thread_id {
            thread_id
        } else {
            let ctx = self.get_ctx();
            // `Option<ThreadId>` niches into a single u32.
            let thread_id = self.builder.ins().load(
                types::I32,
                MemFlagsData::trusted(),
                ctx,
                offset_of!(VmCore, thread_id) as i32,
            );

            self.thread_id = Some(thread_id);

            thread_id
        }
    }

    fn check_deopt_ptr_load(&mut self) -> Value {
        let ctx = self.get_ctx();
        let is_native = self.builder.ins().load(
            Type::int(8).unwrap(),
            MemFlagsData::trusted(),
            ctx,
            offset_of!(VmCore, is_native) as i32,
        );

        is_native
    }

    /// Leave jitted code and let the interpreter re-execute `bytecode_ip`.
    ///
    /// Everything needed to resume is already tracked: the shadow stack knows
    /// every live value and the vm stack slot it belongs to, and spilling it
    /// writes them back as `SteelVal`s - `as_steelval` tags anything being
    /// carried untagged. So the exit is: materialize, point `ip` at the
    /// instruction to redo, clear `is_native` so the caller resumes the loop
    /// rather than taking our return value, and return.
    ///
    /// The caller must not have emitted any effect for that instruction yet,
    /// since it is about to happen again.
    /// `emit_deopt_exit` in its own scope: the exit block emits loads and
    /// rewrites the caches as it materializes, and none of that may be visible
    /// to the block we return to - values defined in the exit do not dominate
    /// it, which cranelift's verifier reports as a non-dominating use.
    /// Queue an exit to be emitted after the body, so its loads land after every
    /// block that branches to it.
    fn defer_deopt_exit(&mut self, block: Block, bytecode_ip: usize, stack: Vec<MaybeStackValue>) {
        self.pending_deopt_exits.push((block, stack, bytecode_ip));
    }

    /// Fill every queued exit. Each restores the translator state it was queued
    /// with, so the exits do not see each other either.
    fn flush_deopt_exits(&mut self) {
        while let Some((block, stack, bytecode_ip)) = self.pending_deopt_exits.pop() {
            self.builder.switch_to_block(block);
            self.emit_deopt_exit_scoped(bytecode_ip, stack);
        }
    }

    fn emit_deopt_exit_scoped(&mut self, bytecode_ip: usize, stack: Vec<MaybeStackValue>) {
        let saved_stack = core::mem::replace(&mut self.shadow_stack, stack);
        let saved_properties = self.properties.clone();
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

    fn emit_deopt_exit(&mut self, bytecode_ip: usize) {
        // Write back everything the interpreter will expect on its operand
        // stack. This is the same materialization a two way branch does.
        self.spill_stack_for_branch();

        let ctx = self.get_ctx();

        let ip = self.builder.ins().iconst(types::I64, bytecode_ip as i64);
        self.builder
            .ins()
            .store(MemFlagsData::trusted(), ip, ctx, offset_of!(VmCore, ip) as i32);

        let not_native = self.builder.ins().iconst(types::I8, 0);
        self.builder.ins().store(
            MemFlagsData::trusted(),
            not_native,
            ctx,
            offset_of!(VmCore, is_native) as i32,
        );

        // Tell the policy this exit happened. A function that keeps arriving
        // here is one speculation is not paying for.
        if let Some(index) = self.function_context {
            let index = self.builder.ins().iconst(types::I64, index as i64);
            self.call_function_args_no_context("#%record-speculation-deopt", &[index]);
        }

        let void = self.encode_void();
        let ret = self.builder.ins().return_(&[void]);
        self.deopt_returns.insert(ret);
    }

    fn check_deopt(&mut self) {
        let result = self.check_deopt_ptr_load();

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
    }

    // Same as func_ret_val_named, but the helper takes the vm context so it can
    // report an error rather than panicking
    fn func_ret_val_named_with_context(
        &mut self,
        function_name: &str,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
    ) {
        let args = self.split_off(payload);
        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

        let result = self.call_function_returns_value_args(function_name, &args);

        self.check_deopt();

        self.push(result, inferred_type);
        self.ip += ip_inc;
    }

    fn func_ret_val_named(
        &mut self,
        function_name: &str,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
    ) {
        let args = self.split_off(payload);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

        let result = self.call_function_returns_value_args_no_context(function_name, &args);

        self.check_deopt();

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);
        self.ip += ip_inc;
    }

    // Make the shadow stack uniform before a two way branch.
    //
    // Entries can be lazy references to a vm stack slot, and can be spilled or
    // not. The two arms don't have to agree about either - a call or a scope end
    // in one arm moves a slot out and leaves void behind, or spills the pending
    // entries - and the merge only inherits one arm's bookkeeping, so the other
    // path reads emptied slots or a stack of the wrong depth. Spilling here runs
    // in the block that dominates both arms, so there is nothing left to disagree
    // about.
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
        // assert!(!self.cloned_stack);
        let guard = self.shadow_stack.get_mut(index)?;
        let mut spilled = false;
        match guard {
            MaybeStackValue::Borrowed(_) => unreachable!("borrowed values are materialized before spilling"),
            MaybeStackValue::Value(stack_value) => {
                if !stack_value.spilled {
                    stack_value.spilled = true;
                    spilled = true;
                }
            }
            // The register's type is the value's type; spilling does not change
            // it. It used to reset to `Any`, which is how an unchanged fixnum
            // argument lost its proof whenever a later argument's arithmetic
            // spilled the stack.
            MaybeStackValue::MutRegister(p) => {
                let p = *p;
                let (value, inferred_type) = self.mut_register_to_value(p);
                spilled = true;

                self.properties.cached_lookups.registers.remove(&p);

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
            // self.push_to_vm_stack_spill(steelval);

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

    /// Whether a borrowed value can stay borrowed across this instruction: it
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
                        Some(SteelVal::FuncV(f)) if *f as usize == steel_eq as usize
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
    /// set, so materializing the same value again on the same path - which the
    /// operand stack's shape should never ask for, but which costs one store to
    /// make harmless - does not clone it twice.
    fn materialize(&mut self, borrowed: BorrowedValue) -> Value {
        let value = borrowed.value;
        let owned = self.builder.use_var(borrowed.owned);
        self.converging_if_no_value(owned, |_| {}, |ctx| ctx.clone_value(value));
        let one = self.builder.ins().iconst(types::I8, 1);
        self.builder.def_var(borrowed.owned, one);
        value
    }

    /// Turn every borrowed value on the operand stack into an owned one. Run
    /// before any instruction that could run code or write memory, which is
    /// what keeps borrowing sound: at that point whatever the value was read
    /// from is still alive and unchanged, so cloning now is the same as having
    /// cloned when it was read.
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
        /*
        for arg in 0..self.shadow_stack.len() {
            self.shadow_spill(arg);
        }
        */

        self.spill_stack_coalesced();
    }

    fn spill_stack_coalesced(&mut self) {
        self.materialize_borrowed();
        let mut buffered_reads = Vec::new();

        // Types are read before the reads: moving a mutable register out marks
        // its slot `Void`. Spilling does not change what the value is, and
        // resetting these to `Any` (as this did) dropped the type of every
        // operand pending under a call - `n` in `(* n (fact (- n 1)))`.
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
                MaybeStackValue::Borrowed(_) => unreachable!("borrowed values are materialized before spilling"),
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

                    self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: register_types[&p],
                        spilled: true,
                    });
                }
                MaybeStackValue::Register(p) => {
                    let p = *p;
                    // let (value, _) = self.immutable_register_to_value(p);
                    let value = coalesced_reads.get(&p).copied().unwrap();

                    spilled = true;

                    self.properties.cached_lookups.registers.remove(&p);

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
        // println!("Spilling cloned stack: {:?}", self.shadow_stack);
        // assert!(!self.cloned_stack);
        // self.cloned_stack = true;

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
                MaybeStackValue::Borrowed(_) => unreachable!("borrowed values are materialized before spilling"),
                MaybeStackValue::Value(stack_value) => {
                    if !stack_value.spilled {
                        let steelval = stack_value.as_steelval(self);
                        values_to_spill.push(steelval);
                    }
                }

                // TODO: Buffer these reads!
                MaybeStackValue::MutRegister(p) => {
                    // Read the value:
                    // let (value, _) = self.mut_register_to_value(p);

                    let value = coalesced_reads.get(&p).unwrap();

                    values_to_spill.push(*value);
                }
                MaybeStackValue::Register(p) => {
                    let value = coalesced_reads.get(&p).unwrap();
                    // let (value, _) = self.immutable_register_to_value(p);
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
        // merged it does not, so read the value back rather than reusing it -
        // `pop_value_from_vm_stack` both loads it and shortens the stack, which
        // is what the bare decrement here used to do without the load.
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
            MaybeStackValue::Borrowed(_) => unreachable!("shadow_stack_pop materializes borrowed values"),
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
        // println!("Adding inferred type void for register: {}", p);

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
    /// The type recorded for a value-stack slot. `properties` is what
    /// `immutable_register_to_value` reads; `local_to_value_map` is what the
    /// dispatch guards read. `LetVar` writes both, so they agree, but a slot can
    /// be present in one and not the other.
    /// Type of an if-merge's value: the join of every arm reaching it.
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

    /// What `cdr` of this register gives: a proper list when the register is
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
            MaybeStackValue::Constant(ConstantValue::Int(i)) => {
                self.builder.ins().iconst(types::I64, i as i64)
            }
            MaybeStackValue::Value(v) => {
                self.value_to_local_map.remove(&v.value);
                match v.inferred_type {
                    InferredType::Int64 => v.value,
                    // A reload of a spilled `Int64` comes back boxed.
                    _ => self.unbox_value_to_pointer(v.value),
                }
            }
            // A fixnum owns nothing, so reading the payload is the whole move:
            // there is no reference to take and no drop the slot still owes.
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
        // the pops: a register already known to be >= some bound, minus a
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
        // spilled operand reloads it and shortens the vm stack, and that happens
        // on the fast path before the overflow branch - so a snapshot from before
        // the pops still called the operand spilled, the exit did not write it
        // back, and the interpreter re-ran the instruction one operand short.
        // `(+ 4611686018427387904 (f (- n 1)))` added `n` instead of the constant.
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
            (OpCode::SUB, Some(r), Some(c)) if c >= 0 => has_fact(&self.properties, r, &|f| {
                matches!(f, Properties::GreaterThan(bound) if *bound >= 0)
            }),
            (OpCode::ADD, Some(r), Some(c)) if c >= 0 => has_fact(&self.properties, r, &|f| {
                matches!(f, Properties::LessThan(bound) if bound.checked_add(c).is_some())
            }),
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
            // pre-pop snapshot inside the exit block itself. Spilling here put
            // every pending value and register through a store and a reload on
            // the fast path - three times an iteration in nqueens' `ok?` - and
            // turned unchanged arguments back into writes at the tail call.
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

        // Boxed rather than `Int64`: plenty of arms trust `Int` and treat the
        // value as a whole `SteelVal`, and none of them expect a bare i64.
        let boxed = self.encode_value(SteelVal::INT_TAG as i64, raw);
        self.push(boxed, InferredType::Int);
        self.ip += 2;
    }

    /// At a merge, keep only the register types every incoming path agrees on
    /// (joined), and only the value-to-register links both paths share. The
    /// current state is one path; `locals`/`values` are the other. Before this,
    /// merges kept whichever path was translated last, so a type established on
    /// just that path outlived the merge.
    pub(super) fn meet_register_maps(
        &mut self,
        locals: &HashMap<usize, InferredType>,
        values: &HashMap<Value, usize>,
    ) {
        self.local_to_value_map.retain(|slot, t| match locals.get(slot) {
            Some(other) => {
                *t = t.join(*other);
                *t != InferredType::Any
            }
            None => false,
        });
        self.value_to_local_map
            .retain(|value, slot| values.get(value) == Some(slot));
    }

    /// The slot holds a fixnum: its recorded type is `Int`, which `LetVar`
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
            InstructionData::UnaryImm { opcode: Opcode::Iconst, imm } => Some(imm.bits()),
            _ => None,
        }
    }

    /// Statically evaluate a branch condition built from constants - which is
    /// what a tag check on a `known_tags` value becomes, via `get_tag`.
    pub(super) fn const_bool(&self, v: Value) -> Option<bool> {
        use cranelift::codegen::ir::{InstructionData, Opcode, ValueDef};
        let dfg = &self.builder.func.dfg;
        let ValueDef::Result(inst, _) = dfg.value_def(v) else {
            return None;
        };
        match dfg.insts[inst] {
            InstructionData::UnaryImm { opcode: Opcode::Iconst, imm } => Some(imm.bits() != 0),
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
            InstructionData::Binary { opcode: Opcode::Band, args } => {
                Some(self.const_bool(args[0])? && self.const_bool(args[1])?)
            }
            _ => None,
        }
    }

    pub(super) fn register_type(&self, r: usize) -> InferredType {
        // Search the facts rather than `properties.get`, which only answers when
        // a slot has exactly one: a typed register that a comparison has also
        // given a range fact used to read back as untyped.
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

    // fn maybe_shadow_pop(&mut self) -> Option<(Value, InferredType)> {
    //     let last = self.shadow_stack.pop()?;

    //     Some(match last {
    //         MaybeStackValue::Value(last) => {
    //             // What is going on here?
    //             if last.spilled && self.ip > self.instructions.len() {
    //                 // dbg!(&self.shadow_stack);
    //                 // dbg!(self.ip);
    //                 // dbg!(self.instructions.len());
    //                 // pretty_print_dense_instructions(&self.instructions);
    //                 return None;
    //             }

    //             assert!(!last.spilled);

    //             // Pop it off the stack?
    //             // self.pop_value_from_vm_stack();

    //             self.value_to_local_map.remove(&last.value);
    //             (last.as_steelval(self), last.inferred_type)
    //         }

    //         // TODO: @matt specialize these for readlocal 0, 1, 2, etc.
    //         MaybeStackValue::MutRegister(p) => self.mut_register_to_value(p),
    //         MaybeStackValue::Register(p) => self.immutable_register_to_value(p),
    //         MaybeStackValue::Constant(c) => c.to_value(self),
    //     })
    // }

    fn maybe_patch_from_stack(&mut self, args_off_the_stack: &mut Vec<StackValue>) {
        let mut indices_to_get_from_shadow_stack = Vec::new();

        // dbg!(&args_off_the_stack);

        for (idx, arg) in args_off_the_stack.iter().enumerate() {
            if arg.spilled {
                indices_to_get_from_shadow_stack.push(idx);
            }
        }

        // dbg!(&indices_to_get_from_shadow_stack);

        for idx in indices_to_get_from_shadow_stack.iter().rev() {
            let value = self.pop_value_from_vm_stack();

            // A spill and reload does not change the value, so its type survives.
            // `shadow_stack_pop` already kept it on the same reload; this path
            // used to reset it to `Any`. Spilled values are boxed, hence `boxed`.
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
    // Dispatch via the usual mechanism to see if this gets the job done
    // fn shadow_maybe_patch_from_stack(&mut self, args_off_the_stack: &mut Vec<MaybeStackValue>) {
    //     let mut indices_to_get_from_shadow_stack = Vec::new();

    //     for (idx, arg) in args_off_the_stack.iter().enumerate() {
    //         if let MaybeStackValue::Value(arg) = arg {
    //             if arg.spilled {
    //                 indices_to_get_from_shadow_stack.push(idx);
    //             }
    //         }
    //     }

    //     for idx in indices_to_get_from_shadow_stack.iter().rev() {
    //         let value = self.pop_value_from_vm_stack();

    //         args_off_the_stack[*idx] = MaybeStackValue::Value(StackValue {
    //             value,
    //             inferred_type: InferredType::Any,
    //             spilled: false,
    //         });
    //     }
    // }

    // fn split_off_reg(&mut self, payload: usize) -> Vec<Value> {
    //     let mut args = self
    //         .shadow_stack
    //         .split_off(self.shadow_stack.len() - payload);

    //     // Patch the args if needed
    //     for arg in &args {
    //         if let MaybeStackValue::Value(v) = arg {
    //             self.value_to_local_map.remove(&v.value);
    //         }
    //     }

    //     self.shadow_maybe_patch_from_stack(&mut args);

    //     // dbg!(&args);

    //     args.into_iter()
    //         .map(|x| match x {
    //             MaybeStackValue::Value(stack_value) => stack_value.as_steelval(self),
    //             MaybeStackValue::MutRegister(p) => {
    //                 self.builder.ins().iconst(Type::int(64).unwrap(), p as i64)
    //             }
    //             MaybeStackValue::Register(p) => {
    //                 self.builder.ins().iconst(Type::int(64).unwrap(), p as i64)
    //             }
    //             MaybeStackValue::Constant(constant_value) => constant_value.to_value(self).0,
    //         })
    //         .collect()
    // }

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

        // dbg!(&args);

        args = args
            .into_iter()
            .map(|x| match x {
                MaybeStackValue::Borrowed(b) => MaybeStackValue::Value(StackValue { value: self.materialize(b), inferred_type: InferredType::Any, spilled: false }),
                MaybeStackValue::Value(stack_value) => MaybeStackValue::Value(stack_value),
                MaybeStackValue::MutRegister(p) => {
                    // let (value, _) = self.mut_register_to_value(p);

                    let value = coalesced_reads.get(&p).copied().unwrap();

                    self.properties.cached_lookups.registers.remove(&p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Register(p) => {
                    // let (value, _) = self.immutable_register_to_value(p);

                    let value = coalesced_reads.get(&p).copied().unwrap();

                    self.properties.cached_lookups.registers.remove(&p);
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
            // self.value_to_local_map.remove(&arg.value);

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
            // Materialized above, so the type has to follow - see `boxed`.
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
                MaybeStackValue::Borrowed(b) => MaybeStackValue::Value(StackValue { value: self.materialize(b), inferred_type: InferredType::Any, spilled: false }),
                MaybeStackValue::Value(stack_value) => MaybeStackValue::Value(stack_value),
                MaybeStackValue::MutRegister(p) => {
                    let value = coalesced_reads.get(&p).copied().unwrap();
                    self.properties.cached_lookups.registers.remove(&p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Register(p) => {
                    let value = coalesced_reads.get(&p).copied().unwrap();
                    self.properties.cached_lookups.registers.remove(&p);
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
            // Materialized above, so the type has to follow - see `boxed`.
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
        let Some(function_name) = try_op_to_name_payload(op, payload) else {
            if let Some(code) = variadic_numeric_code(op) {
                // No helper at this arity: hand the operands over on the vm
                // stack to one that applies the primitive to a slice.
                let args = self.split_off(payload);
                for arg in args {
                    self.push_to_vm_stack(arg.0);
                }
                let code = self.builder.ins().iconst(types::I64, code as i64);
                let arity = self.builder.ins().iconst(types::I64, payload as i64);
                let result =
                    self.call_function_returns_value_args("variadic-numeric-spilled", &[code, arity]);
                self.check_deopt();
                self.push(result, inferred_type.boxed());
                self.ip += ip_inc;
                return;
            }
            op_to_name_payload(op, payload);
            unreachable!()
        };

        // dbg!(function_name);
        // dbg!(&self.shadow_stack);

        let args = self.split_off(payload);

        // dbg!(&args);
        // dbg!(&self.shadow_stack);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

        let result = self.call_function_returns_value_args(function_name, &args);

        // Any of these can raise - if one did, bail out here rather than carrying
        // on and popping the frame that holds the handler out from under it
        self.check_deopt();

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);

        self.ip += ip_inc;
    }

    // fn get_const(&mut self, op1: OpCode, payload: usize) -> (Value, InferredType) {
    //     match op1 {
    //         OpCode::LOADINT0 => (
    //             // self.create_i128(encode(SteelVal::INT_ZERO)),
    //             self.encode_integer(0),
    //             InferredType::Int,
    //         ),
    //         OpCode::LOADINT1 => (
    //             // self.create_i128(encode(SteelVal::INT_ONE)),
    //             self.encode_integer(1),
    //             InferredType::Int,
    //         ),
    //         OpCode::LOADINT2 => (
    //             // self.create_i128(encode(SteelVal::INT_TWO)),
    //             self.encode_integer(2),
    //             InferredType::Int,
    //         ),
    //         OpCode::PUSHCONST => {
    //             // Attempt to inline the constant, if it is something that can be inlined.
    //             // Assuming we know the type of it, we can get really fancy here since
    //             // we _should_ be able to do something with it if there are other types
    //             // in play - we can avoid the unboxing / boxing of the type if we know
    //             // what we're dealing with.

    //             let constant = self.constants.get_value(payload);
    //             self.constant_to_value(payload, constant)
    //         }
    //         _ => (
    //             self.call_function_returns_value(op_to_name_payload(op1, payload)),
    //             InferredType::Any,
    //         ),
    //     }
    // }

    fn constant_to_value(&mut self, payload: usize, constant: SteelVal) -> (Value, InferredType) {
        match &constant {
            SteelVal::NumV(n) => (self.encode_float(*n), InferredType::Float),
            SteelVal::IntV(i) => (self.encode_integer(*i as _), InferredType::Int),

            // Leak the constant, and then just push it up instead
            SteelVal::SymbolV(c) => {
                let value = c.clone();

                // Leak the value?
                value.0.into_raw();

                let as_ptr: i64 = unsafe { std::mem::transmute::<SteelString, _>(c.clone()) };

                let value = self.builder.ins().iconst(types::I64, as_ptr);

                // Clone it
                self.increment_ref_count_closure(value);

                (
                    self.encode_value(SteelVal::SYMBOL_TAG as _, value),
                    InferredType::Symbol,
                )
            }

            SteelVal::ListV(c) => {
                let value = c.clone();

                let as_ptr: usize =
                    unsafe { std::mem::transmute::<crate::values::lists::List<_>, _>(c.clone()) };

                let value = self.builder.ins().iconst(types::I64, as_ptr as i64);

                self.increment_ref_count_closure(value);

                (
                    self.encode_value(SteelVal::LIST_TAG as _, value),
                    InferredType::List,
                )
            }

            // SteelVal::BoolV(_) => (self.create_i128(encode(constant)), InferredType::Bool),
            // SteelVal::IntV(_) => (self.create_i128(encode(constant)), InferredType::Int),
            // SteelVal::CharV(_) => (self.create_i128(encode(constant)), InferredType::Char),
            _ => (self.push_const_index(payload), InferredType::Any),
        }
    }

    fn call_func_or_immediate(&mut self, op1: OpCode, payload: usize) -> Value {
        match op1 {
            // OpCode::LOADINT0 => self.create_i128(encode(SteelVal::INT_ZERO)),
            // OpCode::LOADINT1 => self.create_i128(encode(SteelVal::INT_ONE)),
            // OpCode::LOADINT2 => self.create_i128(encode(SteelVal::INT_TWO)),
            OpCode::LOADINT0 =>
            // self.create_i128(encode(SteelVal::INT_ZERO)),
            {
                self.encode_integer(0)
            }
            OpCode::LOADINT1 =>
            // self.create_i128(encode(SteelVal::INT_ONE)),
            {
                self.encode_integer(1)
            }
            OpCode::LOADINT2 =>
            // self.create_i128(encode(SteelVal::INT_TWO)),
            {
                self.encode_integer(2)
            }

            OpCode::PUSHCONST => {
                // Attempt to inline the constant, if it is something that can be inlined.
                // Assuming we know the type of it, we can get really fancy here since
                // we _should_ be able to do something with it if there are other types
                // in play - we can avoid the unboxing / boxing of the type if we know
                // what we're dealing with.

                let constant = self.constants.get_value(payload);

                match &constant {
                    // SteelVal::CharV(_) => self.create_i128(encode(constant)),
                    SteelVal::CharV(c) => {
                        let res = self.builder.ins().iconst(Type::int(64).unwrap(), *c as i64);
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
                    // _ => self.call_function_returns_value(op_to_name_payload(op1, payload)),
                }
            }
            _ => self.call_function_returns_value(op_to_name_payload(op1, payload)),
        }
    }

    // Just... store as a i128, and hope for the best. Don't need to encode
    // it directly as anything really?
    // Build the i128 the jit passes a RootedInstructions around as. Low half is
    // the pointer, high half the length - same order iconcat(data_ptr, len)
    // produces elsewhere, and the same order the fields are declared in.
    /// Whether a callee runs the same instruction stream we are compiling.
    ///
    /// For a self call the instructions saved into the new frame and the ones
    /// installed into `VmCore` are the same value, so both the load of the
    /// current stream and the store of the new one are ceremony - fib spent
    /// ~2.8% of a call window on exactly those four instructions.
    fn callee_shares_our_instructions(&self, callee: RootedInstructions) -> bool {
        core::ptr::eq(callee.ptr, self.instructions.as_ptr())
            && callee.len as usize == self.instructions.len()
    }

    fn rooted_instructions_const(&mut self, instructions: RootedInstructions) -> Value {
        let int = Type::int(64).unwrap();
        let ptr = self.builder.ins().iconst(int, instructions.ptr as i64);
        let len = self.builder.ins().iconst(int, instructions.len as i64);

        self.builder.ins().iconcat(ptr, len)
    }

    fn create_i128(&mut self, value: i128) -> Value {
        let [left, right] = split_big(value);
        let int = Type::int(64).unwrap();

        // Payload
        let lhs = self.builder.ins().iconst(int, left);

        // Tag
        let rhs = self.builder.ins().iconst(int, right);

        // TODO:
        self.builder.ins().iconcat(rhs, lhs)
    }

    // If this is an unboxed value, then we need to cast the value
    // to the requisite type if its a boolean.
    #[allow(unused)]
    fn encode_value(&mut self, tag: i64, value: Value) -> Value {
        let tag = self.builder.ins().iconst(Type::int(64).unwrap(), tag);
        self.builder.ins().iconcat(tag, value)
    }

    fn encode_true(&mut self) -> Value {
        let res = self.builder.ins().iconst(Type::int(64).unwrap(), 1);
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
        let res = self.builder.ins().iconst(Type::int(64).unwrap(), 0);
        let boolean = self.encode_value(discriminant(&SteelVal::BoolV(false)) as i64, res);
        boolean
    }

    fn encode_void(&mut self) -> Value {
        let res = self.builder.ins().iconst(Type::int(64).unwrap(), 0);
        self.encode_value(discriminant(&SteelVal::Void) as i64, res)
    }

    fn encode_integer(&mut self, integer: i64) -> Value {
        let res = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), integer as i64);
        let integer = self.encode_value(discriminant(&SteelVal::IntV(0)) as i64, res);
        self.known_tags.insert(integer, SteelVal::INT_TAG);
        integer
    }

    fn encode_char(&mut self, c: char) -> Value {
        let res = self.builder.ins().iconst(Type::int(64).unwrap(), c as i64);
        let integer = self.encode_value(SteelVal::CHAR_TAG as _, res);
        integer
    }

    // Remove the tag from a value to get to the underlying type
    fn unbox_value(&mut self, value: Value) -> Value {
        let amount_to_shift = self.builder.ins().iconst(types::I64, 64);
        let encoded_rhs = self.builder.ins().sshr(value, amount_to_shift);
        encoded_rhs
    }

    // fn unbox_value_to_pointer(&mut self, value: Value) -> Value {
    //     let amount_to_shift = self.builder.ins().iconst(types::I64, 64);
    //     let encoded_rhs = self.builder.ins().sshr(value, amount_to_shift);
    //     self.builder.ins().ireduce(types::I64, encoded_rhs)
    // }

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
        // println!("Visiting if @ {} - depth: {}", self.ip, self.depth);
        // println!("Existing if bound: {:?}", self.if_bound);

        // let mut else_offset;

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

        // println!("Then bound: {:?}", self.if_bound);
        // println!("Else bound: {:?}", else_offset);

        // Have to stop before we get here
        // self.if_stack.push(else_start);

        // println!("If stack: {:?}", self.if_stack);

        let start = self.ip;
        let depth = self.depth;

        self.spill_stack_for_branch();

        // if self.visited.insert(self.ip) {
        // }

        // if !self.visited.insert(self.ip) {
        //     pretty_print_dense_instructions(&self.instructions);
        //     panic!("Already visited this if instruction");
        // }

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

        // Update with the proper return value
        // let mut then_return = self
        //     .builder
        //     .ins()
        //     .iconst(codegen::ir::Type::int(128).unwrap(), 1);

        // println!("if: {} - setting ip to then: {}", start, then_start);

        // Set the ip to the right spot:
        self.ip = then_start;

        let let_stack = self.let_var_stack.clone();
        // let local_count = self.local_count;
        let frozen_stack = self.shadow_stack.clone();

        let original_val_to_local = self.value_to_local_map.clone();
        let original_local_to_val = self.local_to_value_map.clone();
        let original_properties = self.properties.clone();

        // let cloned_stack = self.cloned_stack;
        // let tco = self.tco;

        self.if_stack.push(start);

        self.check_then_properties(condition_value);

        self.stack_to_ssa();

        self.if_stack.pop();

        self.if_bound = last_bound;

        assert_eq!(self.depth, depth);

        // println!("---------- then done ----------");

        // let then_ip = self.ip;

        let then_out_of_bounds = self.ip > self.instructions.len();

        // println!(
        //     "ip, instructions len: {} - {}",
        //     self.ip,
        //     self.instructions.len()
        // );

        /*

        TODO: Insert a guard to see where the then expression
        finishes. If the then expression finishes in bounds,
        i.e. hits a JMP where the JMP isn't quite the end,
        then we also want to check the else branch, and see where
        that finishes.

        We have two cases:

        Then and Else both stop at a JMP. In theory they'll converge
        onto the same spot. We create a merge block, and then continue
        generating code from there.

        Then returns, but else does not. Swap back to else, finish
        generating instructions.

        Same but the opposite.

        Both converge, no need for merge block.

        */

        // println!("Stack after then branch: {:?}", self.stack);

        // Unwrap or... must have been a tail call?

        let mut then_type = InferredType::Any;
        // The merge keeps a borrowed then value borrowed, passing its flag on;
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
            // BlockArg::Value(self.create_i128(encode(SteelVal::IntV(12345))))
            // self.create_i128(encode(SteelVal::IntV(12345)))

            BlockArg::Value(self.encode_integer(12345))
            // BlockArg::Value(
            //     self.maybe_shadow_pop()
            //         .map(|x| {
            //             // assert!(!x.spilled);
            //             let value = x.0;
            //             self.value_to_local_map.remove(&value);
            //             value
            //         })
            //         // .unwrap(),
            //         .unwrap_or_else(|| self.create_i128(encode(SteelVal::Void))),
            // )
        } else {
            // BlockArg::Value(self.create_i128(encode(SteelVal::Void)))
            // BlockArg::Value(self.shadow_pop().0)
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

        // TODO: Update with the proper return value
        // let mut else_return; = self
        //     .builder
        //     .ins()
        //     .iconst(codegen::ir::Type::int(8).unwrap(), 128);

        // println!("if: {} - Setting ip to else: {}", start, else_start);

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

                // dbg!(self.if_bound);

                // dbg!(&self.shadow_stack);
                // dbg!(&then_stack);

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

                // self.create_i128(encode(SteelVal::Void))

                self.encode_void()
            }
            (false, true) => {
                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);
                // Switch to merge block, continue on.
                self.builder.switch_to_block(merge_block);
                self.if_bound = last_bound;

                // dbg!(else_start);
                // dbg!(saved_then_bound);
                // dbg!(last_bound);

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

                // self.create_i128(encode(SteelVal::Void))

                self.encode_void()
            }
            (false, false) => {
                // TODO:
                // if then_stack.len() != self.shadow_stack.len() {
                //     dbg!(&self.ip);
                //     pretty_print_dense_instructions(&self.instructions);
                // }
                // TODO: Check this out? Why is this the way it is?
                // assert_eq!(then_stack.len(), self.shadow_stack.len());

                // Pop the values - merge the result of the calls?
                // println!("Getting here");

                // Switch to merge block, pop off value, return
                // the value.

                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);

                // Switch to the merge block for subsequent statements.
                self.builder.switch_to_block(merge_block);

                // We've now seen all the predecessors of the merge block.
                self.builder.seal_block(merge_block);

                // Only what both arms agree on survives the merge
                self.properties.meet(&properties);
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
        // self.builder.seal_block(merge_block);

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

    fn vm_pop(&mut self, value: Value) -> Value {
        self.inline_handle_pop(value)
    }

    fn push_to_vm_stack(&mut self, value: Value) {
        self.push_to_vm_stack_let_var_new(value);
    }

    fn read_multiple_from_stack(&mut self, values: Vec<(usize, bool)>) -> HashMap<usize, Value> {
        if values.is_empty() {
            return Default::default();
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
                // println!("Adding inferred type void for register: {}", index);

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
                // println!("Adding inferred type void for register: {}", index);
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
    fn read_from_vm_stack(&mut self, index: usize) -> Value {
        // Cache let vars since they're going to be on the stack,
        // but we already had it available.
        if let Some(local) = self.properties.cached_lookups.registers.get(&index) {
            log::debug!(target: "letvar", "HIT  slot {index}");
            let local = *local;
            self.mark_register_read(index, local);
            return local;
        }
        log::debug!(target: "letvar", "MISS slot {index}");
        let cache_this_read =
            std::env::var("STEEL_JIT_LETVAR_CACHE").as_deref() == Ok("1");

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

        // Read-through: the index came from the bytecode, so unlike the one
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

    /*
    // Note: This value should _not_ be dropped!
    fn read_from_vm_stack_old(&mut self, index: usize) -> Value {
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

        let local_value = self
            .builder
            .ins()
            .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

        local_value
    }
    */

    fn remove_from_vm_stack(&mut self, index: usize) -> Value {
        let ctx = self.get_ctx();
        let sp = self.get_sp(ctx);

        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack offset:
        // let stack_offset = offset_of!(SteelThread, stack);

        // let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        // let buf_ptr = self.builder.ins().load(
        //     Type::int(64).unwrap(),
        //     MemFlagsData::trusted(),
        //     thread_pointer,
        //     (stack_offset + ptr_offset) as i32,
        // );

        // let size: i64 = std::mem::size_of::<SteelVal>() as _;
        // let local_offset = self.builder.ins().iadd_imm_s(sp, index as i64);
        // let offset = self.builder.ins().imul_imm_s(local_offset, size);

        // let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        // let local_value = self
        //     .builder
        //     .ins()
        //     .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (offset_of!(SteelThread, stack) + steel_vec::Vec::<SteelVal>::buf_offset()) as i32,
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
            // let local_offset = self.builder.ins().iadd_imm_s(sp, index as i64);
            // let offset = self.builder.ins().imul_imm_s(local_offset, size);
            // let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

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
        // let size: i64 = std::mem::size_of::<SteelVal>() as _;

        if amt == 0 {
            return;
        }

        let mut index = index;
        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        for _ in 0..amt {
            // let local_offset = self.builder.ins().iadd_imm_s(sp, index as i64);
            // let offset = self.builder.ins().imul_imm_s(local_offset, size);

            // let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

            if !self.register_is_immediate(index)
            {
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

        // let size: i64 = std::mem::size_of::<SteelVal>() as _;

        let mut index = index;
        let sp_bytes = self.builder.ins().ishl_imm_u(sp, 4);
        let frame_base = self.builder.ins().iadd(buf_ptr, sp_bytes);

        for value in values {
            // let local_offset = self.builder.ins().iadd_imm_s(sp, index as i64);
            // let offset = self.builder.ins().imul_imm_s(local_offset, size);
            // let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

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

    // We can probably just cache this per branch, so that subsequent calls
    // can avoid this load.
    // ctx.thread.stack_frames.len() < 100
    fn check_should_trampoline(&mut self, ctx: Value) -> Value {
        if let Some(should_trampoline) = self.should_trampoline {
            return should_trampoline;
        }

        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack frame offset:
        let stack_frame_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_frame_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_frame_offset + len_offset) as i32,
        );

        let res = self
            .builder
            .ins()
            .icmp_imm_s(IntCC::UnsignedLessThan, stack_frame_length, 100);

        // self.should_trampoline = Some(res);

        res
    }

    fn inline_call_global_function_to(
        &mut self,
        id: u32,
        func: Value,
        fallback_ip: usize,
        args: &[Value],
        instr_fat_ptr: Value,
        callee_is_self: bool,
        target: Option<FuncId>,
    ) -> Value {
        let vm_ctx = self.get_ctx();
        let should_trampoline = self.check_should_trampoline(vm_ctx);

        let should_yield = self.builder.ins().bxor_imm_u(should_trampoline, 1);

        self.update_ip_native_if_yield(vm_ctx, should_yield, fallback_ip + 1);

        let typ = self.int;
        let arity = args.len();

        // Push each value on to the stack:
        // for arg in args {
        //     self.push_to_vm_stack_let_var_new(*arg);
        // }

        // stack_length_capacity is a compile time model of a runtime quantity and it
        // can run ahead of the truth. The unchecked push then writes past the end of
        // the stack buffer and corrupts the allocator, so always take the check - it
        // costs nothing measurable.
        self.push_to_many_vm_stack_let_var_new(&args);

        // The checked push reserves exactly what it needs, so there is no
        // headroom left over:
        self.properties.cached_lookups.stack_length_capacity = 0;

        self.converging_if(
            should_trampoline,
            |ctx| {
                // Increment the ref count before we go in
                ctx.increment_ref_count_closure(func);

                // TODO: Consider moving this up before things are pushed on in order
                // to capture the stack length without needing to compute the value
                ctx.push_stack_frame(arity as _, func, instr_fat_ptr, fallback_ip, callee_is_self);

                // TODO: Change the calling convention of the function to return a
                // (tag, Value) rather than an i128 value directly.
                let jit_func = match target {
                    Some(target) => ctx.module.declare_func_in_func(target, ctx.builder.func),
                    None => ctx.get_jit_func(id),
                };

                // Check the result here
                let call = ctx.builder.ins().call(jit_func, &[vm_ctx]);

                let res = ctx.builder.inst_results(call)[0];

                // Check if native still:
                let is_still_native = ctx.builder.ins().load(
                    types::I8,
                    MemFlagsData::trusted(),
                    vm_ctx,
                    offset_of!(VmCore, is_native) as i32,
                );

                ctx.converging_if(
                    is_still_native,
                    // |ctx| ctx.inline_pop_from_stack(vm_ctx),
                    |_| res,
                    |ctx| ctx.encode_void(),
                    typ,
                )
            },
            |ctx| {
                let arity_val = ctx.builder.ins().iconst(types::I64, arity as i64);
                let fallback_ip_val = ctx.builder.ins().iconst(types::I64, fallback_ip as i64);
                ctx.call_function_returns_value_args(
                    "#%setup-closure-arity",
                    &[func, arity_val, fallback_ip_val],
                )
            },
            typ,
        )
    }

    fn inline_let_end_scope(&mut self, amt: usize, count: usize) {
        let vm_ctx = self.get_ctx();
        let offset = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            vm_ctx,
            offset_of!(VmCore, sp) as i32,
        );

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
            // still fits - not that it lands exactly on the capacity. Same comparison
            // push_to_many_vm_stack_let_var_new uses.
            let needs_more_capacity =
                self.builder
                    .ins()
                    .icmp(IntCC::UnsignedGreaterThan, new_length, stack_capacity);

            self.converging_if_no_else_no_value_then_cold(
                needs_more_capacity,
                |ctx| {
                    let amt = args.len() - ctx.arity as usize;
                    let amt = ctx.builder.ins().iconst(types::I64, amt as i64);
                    ctx.call_function_args_no_return("slow-stack-reserve-exact", &[amt]);
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
        // discards them while they still hold their references: a continuation
        // closure built inside a `let` and tail called leaked one closure per
        // iteration, 23GB of them in `graphs`. Starting above what the branches
        // already dropped keeps this from releasing anything twice.
        //
        // The loop itself is not free - it runs on every jitted tail call, and
        // emitting it unconditionally cost earley 10% of its instructions - so
        // skip it when this frame has nothing above the arguments: no let
        // bindings open and no operands spilled under them, which is the common
        // shape.
        let live_above =
            self.let_var_stack.iter().sum::<usize>() + self.shadow_stack.len();

        // Nor is there anything to release when every slot above the arguments
        // holds an immediate - a fixnum, or the void a move read left behind.
        // `conform`, `browse` and `destruc` free no memory from this loop at all,
        // so the work there is all tag checks on slots that own nothing.
        let base_slot = args.len().max(self.arity as usize);
        let all_immediate =
            (0..live_above).all(|k| self.register_is_immediate(base_slot + k));

        // Unrolling this - emitting `arity + live_above - base_slot` drops at
        // fixed offsets instead of the loop - was measured and is a wash:
        // earley -0.45% instructions, conform -0.09%, browse and destruc 0.00%,
        // with cycles inside the noise floor in both directions. The cost here
        // is the refcount and free work for values that used to be leaked, not
        // the loop driving it, so there is nothing to win by driving it better.
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

        // let difference = self.builder.ins().isub(stack_length, index);
        // let new_length = self.builder.ins().iconst(types::, N)

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

        // let size: i64 = std::mem::size_of::<SteelVal>() as _;

        // TODO: Don't use a loop here?
        // Use the calculated difference if thats what we have to do
        // let count = count
        //     .map(|count| self.builder.ins().iconst(types::I64, count as i64))
        //     .unwrap_or_else(|| self.builder.ins().isub(stack_length, index));
        // {
        //     let loop_header = self.builder.create_block();
        //     let loop_body = self.builder.create_block();
        //     let loop_exit = self.builder.create_block();

        //     self.builder.append_block_param(loop_header, types::I64);

        //     let zero = BlockArg::Value(self.builder.ins().iconst(types::I64, 0));
        //     self.builder.ins().jump(loop_header, &[zero]);

        //     self.builder.switch_to_block(loop_header);
        //     let i = self.builder.block_params(loop_header)[0];

        //     let done = self
        //         .builder
        //         .ins()
        //         .icmp(IntCC::SignedGreaterThanOrEqual, i, count);
        //     self.builder
        //         .ins()
        //         .brif(done, loop_exit, &[], loop_body, &[]);

        //     self.builder.switch_to_block(loop_body);
        //     self.builder.seal_block(loop_body);

        //     let slot_index = self.builder.ins().iadd(index, i);
        //     let byte_offset = self
        //         .builder
        //         .ins()
        //         .imul_imm_s(slot_index, size_of::<SteelVal>() as i64);
        //     let slot_ptr = self.builder.ins().iadd(buf_ptr, byte_offset);
        //     let val = self
        //         .builder
        //         .ins()
        //         .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);
        //     self.drop_tagged_value(val);

        //     let i_next = BlockArg::Value(self.builder.ins().iadd_imm_s(i, 1));
        //     self.builder.ins().jump(loop_header, &[i_next]);

        //     self.builder.seal_block(loop_header);

        //     self.builder.switch_to_block(loop_exit);
        //     self.builder.seal_block(loop_exit);
        // }

        let count = count
            .map(|count| self.builder.ins().iconst(types::I64, count as i64))
            .unwrap_or_else(|| self.builder.ins().isub(stack_length, index));
        {
            let loop_header = self.builder.create_block();
            let loop_body = self.builder.create_block();
            let loop_exit = self.builder.create_block();

            self.builder.append_block_param(loop_header, types::I64);

            let zero = BlockArg::Value(self.builder.ins().iconst(types::I64, 0));
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

    // TODO: Pick up here! Anywhere where we're truncating, we should also keep
    // track of what has already been moved, and then make sure that those are
    // not included in the drop calls since we don't need to drop them anymore.
    fn truncate_stack_write_last(&mut self, vm_ctx: Value, index: Value, new_last: Value) {
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

        // let difference = self.builder.ins().isub(stack_length, index);
        // let new_length = self.builder.ins().iconst(types::, N)

        let new_length = self.builder.ins().iadd_imm_s(index, 1);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            // index,
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

        // let size: i64 = std::mem::size_of::<SteelVal>() as _;

        // TODO: Don't use a loop here?
        // Use the calculated difference if thats what we have to do
        // let count = self.builder.ins().isub(stack_length, index);

        // {
        //     let loop_header = self.builder.create_block();
        //     let loop_body = self.builder.create_block();
        //     let loop_exit = self.builder.create_block();

        //     self.builder.append_block_param(loop_header, types::I64);

        //     let zero = BlockArg::Value(self.builder.ins().iconst(types::I64, 0));
        //     self.builder.ins().jump(loop_header, &[zero]);

        //     self.builder.switch_to_block(loop_header);
        //     let i = self.builder.block_params(loop_header)[0];

        //     let done = self
        //         .builder
        //         .ins()
        //         .icmp(IntCC::SignedGreaterThanOrEqual, i, count);
        //     self.builder
        //         .ins()
        //         .brif(done, loop_exit, &[], loop_body, &[]);

        //     self.builder.switch_to_block(loop_body);
        //     self.builder.seal_block(loop_body);

        //     let slot_index = self.builder.ins().iadd(index, i);
        //     let byte_offset = self
        //         .builder
        //         .ins()
        //         .imul_imm_s(slot_index, size_of::<SteelVal>() as i64);
        //     let slot_ptr = self.builder.ins().iadd(buf_ptr, byte_offset);
        //     let val = self
        //         .builder
        //         .ins()
        //         .load(types::I128, MemFlagsData::trusted(), slot_ptr, 0);
        //     self.drop_tagged_value(val);

        //     let i_next = BlockArg::Value(self.builder.ins().iadd_imm_s(i, 1));
        //     self.builder.ins().jump(loop_header, &[i_next]);

        //     self.builder.seal_block(loop_header);

        //     self.builder.switch_to_block(loop_exit);
        //     self.builder.seal_block(loop_exit);
        // }

        // let last_byte_offset = self
        //     .builder
        //     .ins()
        //     .imul_imm_s(index, size_of::<SteelVal>() as i64);
        // let last_slot_ptr = self.builder.ins().iadd(buf_ptr, last_byte_offset);
        // self.builder
        //     .ins()
        //     .store(MemFlagsData::trusted(), new_last, last_slot_ptr, 0);

        let count = self.builder.ins().isub(stack_length, index);

        {
            let loop_header = self.builder.create_block();
            let loop_body = self.builder.create_block();
            let loop_exit = self.builder.create_block();

            self.builder.append_block_param(loop_header, types::I64);

            let zero = BlockArg::Value(self.builder.ins().iconst(types::I64, 0));
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

        let last_byte_offset = self
            .builder
            .ins()
            .imul_imm_s(index, size_of::<SteelVal>() as i64);
        let last_slot_ptr = self.builder.ins().iadd(buf_ptr, last_byte_offset);
        self.builder
            .ins()
            .store(MemFlagsData::trusted(), new_last, last_slot_ptr, 0);
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
    /// flow join where the two sides disagree - see `PropertyMap::meet`.
    fn stack_buf_ptr(&mut self, vm_ctx: Value) -> Value {
        if let Some(buf_pointer) = self.properties.cached_lookups.stack_buf_pointer {
            log::debug!(target: "bufptr", "HIT");
            return buf_pointer;
        }
        log::debug!(target: "bufptr", "MISS");

        let thread_pointer = self.get_thread_pointer(vm_ctx);

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (offset_of!(SteelThread, stack) + steel_vec::Vec::<SteelVal>::buf_offset()) as i32,
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
        let env_pointer = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            thread_pointer,
            offset_of!(SteelThread, global_env) as i32,
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
                // Make sure this is 16
                // let size = std::mem::size_of::<SteelVal>();
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
        // self.call_function_no_return("#%debug-stack-before");

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

        // self.call_function_no_return("#%debug-stack-after");

        res
    }

    // Subtract one from the pop count, returns
    // the new pop count
    fn sub_one_pop_count(&mut self, vm_ctx: Value) -> Value {
        let sub_one = self.pop_count_sub_one(vm_ctx);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            sub_one,
            vm_ctx,
            offset_of!(VmCore, pop_count) as i32,
        );
        sub_one
    }

    fn inline_handle_pop(&mut self, value: Value) -> Value {
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

                ctx.converging_if_no_else_no_value_else_cold(
                    attachment_exists,
                    |ctx| {
                        // If the attachments exist, then we need to:
                        // 1. Call the rust function to close the frame
                        // 2. Close the destructor.
                        ctx.call_function_args_no_return(
                            "#%handle-attachments",
                            &[popped_frame.attachments],
                        );
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
                        ctx.truncate_stack(vm_ctx, sp, None);

                        let ip = ctx.builder.ins().uextend(types::I64, popped_frame.ip);
                        ctx.builder.ins().store(
                            MemFlagsData::trusted(),
                            ip,
                            vm_ctx,
                            offset_of!(VmCore, ip) as i32,
                        );

                        ctx.builder.ins().store(
                            MemFlagsData::trusted(),
                            popped_frame.instructions,
                            vm_ctx,
                            offset_of!(VmCore, instructions) as i32,
                        );

                        // let sp = ctx.read_last_sp(vm_ctx, Some(fat_ptr));
                        let sp = ctx.read_last_sp(vm_ctx);

                        // TODO: @Matt
                        // Call drop on the function here!
                        // We don't need to totally encode the function here like this
                        // let func =
                        //     ctx.encode_value(SteelVal::CLOSURE_TAG as _, popped_frame.function);
                        ctx.drop_biased_rc_unboxed_closure(popped_frame.function);

                        ctx.builder.ins().store(
                            MemFlagsData::trusted(),
                            sp,
                            vm_ctx,
                            offset_of!(VmCore, sp) as i32,
                        );
                    },
                );

                value
            },
            |ctx| {
                ctx.call_function_args_no_return("#%pop-slow-path-finish", &[value]);

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

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // Last thing
        let new_length = self.builder.ins().iadd_imm_s(stack_length, -1);

        let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<StackFrame>() as _;
        let offset = self.builder.ins().imul_imm_s(new_length, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        // Load the stack frame. We're going to use this later.
        let value = self.builder.ins().load(
            types::I32,
            MemFlagsData::trusted(),
            slot_ptr,
            offset_of!(StackFrame, sp) as i32,
        );

        //
        let old_function = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            slot_ptr,
            offset_of!(StackFrame, function) as i32,
        );

        self.drop_biased_rc_unboxed_closure(old_function);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            function,
            slot_ptr,
            offset_of!(StackFrame, function) as i32,
        );

        self.builder.ins().uextend(types::I64, value)
    }

    // TODO:
    // Extend by a certain amount - we probably can
    // make this better by passing in the length from
    // the previous read so we don't have to read all
    // this stuff. Its already read from before.
    fn read_last_sp(&mut self, vm_ctx: Value) -> Value {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let condition = self.builder.ins().icmp_imm_s(IntCC::Equal, stack_length, 0);

        self.converging_if(
            condition,
            |ctx| ctx.builder.ins().iconst(types::I64, 0),
            |ctx| {
                // Last thing
                let new_length = ctx.builder.ins().iadd_imm_s(stack_length, -1);

                let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlagsData::trusted(),
                    thread_pointer,
                    (stack_offset + ptr_offset) as i32,
                );

                let size: i64 = std::mem::size_of::<StackFrame>() as _;
                let offset = ctx.builder.ins().imul_imm_s(new_length, size);
                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                // Load the stack frame. We're going to use this later.
                let value = ctx.builder.ins().load(
                    types::I32,
                    MemFlagsData::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, sp) as i32,
                );

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
            let old_pop_count = self.builder.ins().load(
                types::I64,
                MemFlagsData::trusted(),
                vm_ctx,
                offset_of!(VmCore, pop_count) as i32,
            );

            old_pop_count
        }
    }

    // TODO: Cache these at the start? In the function prelude?
    fn get_thread_pointer(&mut self, vm_ctx: Value) -> Value {
        if let Some(thread_pointer) = self.thread_pointer {
            thread_pointer
        } else {
            let tp = self.builder.ins().load(
                Type::int(64).unwrap(),
                MemFlagsData::trusted(),
                vm_ctx,
                offset_of!(VmCore, thread) as i32,
            );

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
            let sp = self.builder.ins().load(
                Type::int(64).unwrap(),
                MemFlagsData::trusted(),
                vm_ctx,
                offset_of!(VmCore, sp) as i32,
            );

            sp
        }
    }

    /// Pop the last stack frame off
    fn inline_pop_from_stack_frames(&mut self, vm_ctx: Value) -> (StackFrameRepr, (Value, Value)) {
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);

        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

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

        let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<StackFrame>() as _;
        let offset = self.builder.ins().imul_imm_s(new_length, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let sp_offset = (buf_ptr, new_length);

        // Load the stack frame. We're going to use this later.
        (
            StackFrameRepr {
                sp: self.builder.ins().load(
                    types::I32,
                    MemFlagsData::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, sp) as i32,
                ),
                ip: self.builder.ins().load(
                    types::I32,
                    MemFlagsData::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, ip) as i32,
                ),
                instructions: self.builder.ins().load(
                    types::I128,
                    MemFlagsData::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, instructions) as i32,
                ),
                function: self.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, function) as i32,
                ),
                attachments: self.builder.ins().load(
                    types::I64,
                    MemFlagsData::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, attachments) as i32,
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
                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    zero,
                    vm_ctx,
                    offset_of!(VmCore, is_native) as i32,
                );
            },
            |ctx| {
                let ip = ctx.builder.ins().iconst(types::I64, fallback_ip as i64);
                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    ip,
                    vm_ctx,
                    offset_of!(VmCore, ip) as i32,
                );
            },
        );
    }

    /// Clones a value. If the value is not heap allocated,
    /// this does nothing after checking the tag.
    fn clone_value(&mut self, value: Value) {
        let tag = self.get_tag(value);

        let mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::UNBOXED_MASK as i64);
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

        let std_mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::STANDARD_RC_MASK as i64);
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
    /// increment of a `Weak` counter at a fixed offset - the mirror of the
    /// decrement `inline_weak_decrement` already emits. Calling out to
    /// `clone_one` for it cost a whole `SteelVal` round trip per clone: 17% of
    /// conform, which reads mutable vector records and shim pairs constantly.
    /// Continuations still take the call.
    ///
    /// `Weak::clone` panics if the count passes `i32::MAX`; this does not
    /// check. Reaching that needs two billion live references to one heap
    /// object, the same assumption the inline decrement makes.
    fn clone_rc_value(&mut self, value: Value) {
        if !inline_weak_clone_enabled() {
            self.call_function_args_no_context("#%clone-std-rc", &[value]);
            return;
        }

        let tag = self.get_tag(value);
        let weak_mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::WEAK_RC_MASK as i64);
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
                ctx.call_function_args_no_context("#%clone-std-rc", &[value]);
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

                // The count is 14 bits now, so saturation is reachable where a
                // full word never was. Spill to the shared counter rather than
                // letting it wrap - the two sum to the true count either way.
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
                        c.call_function_args_no_context("raw-slow-increment-closure", &[value]);
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
                ctx.call_function_args_no_context("raw-slow-increment-closure", &[value]);
            },
        );
    }

    fn inline_read_captured(&mut self, index: usize, clone: bool) -> Value {
        let vm_ctx = self.get_ctx();
        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<StackFrame>() as _;

        let last = self.builder.ins().iadd_imm_s(stack_length, -1);

        let offset = self.builder.ins().imul_imm_s(last, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let function = self.builder.ins().load(
            types::I64,
            MemFlagsData::trusted(),
            slot_ptr,
            offset_of!(StackFrame, function) as i32,
        );

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
        self.compilation_stats.stack_frame_pushes += 1;

        let vm_ctx = self.get_ctx();

        let thread_pointer = self.get_thread_pointer(vm_ctx);

        // Stack frame offset:
        let stack_frame_offset = offset_of!(SteelThread, stack_frames);

        let capacity_offset = steel_vec::Vec::<StackFrame>::capacity_offset();
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_capacity = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_frame_offset + capacity_offset) as i32,
        );

        // self.call_function_args_no_context("#%debug-value", &[stack_capacity]);

        let stack_frame_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_frame_offset + len_offset) as i32,
        );

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
        self.converging_if_no_else_no_value_then_cold(
            at_capacity,
            // If at capacity, call grow
            |ctx| ctx.call_function_no_return("slow-grow-frame-stack"),
            |ctx| {
                // Write the value:
                let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlagsData::trusted(),
                    thread_pointer,
                    (stack_frame_offset + ptr_offset) as i32,
                );

                // Okay so here, now we're going to move things around
                // such that we can snag values from things
                let size: i64 = std::mem::size_of::<StackFrame>() as _;
                let offset = ctx.builder.ins().imul_imm_s(stack_frame_length, size);

                // let offset = ctx.builder.ins().ishl_imm_u(stack_frame_length, 6);

                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                let sp_offset = offset_of!(VmCore, sp);

                let arity = -arity;

                let new_sp = ctx.builder.ins().iadd_imm_s(stack_length, arity as i64);

                ctx.builder
                    .ins()
                    .store(MemFlagsData::trusted(), new_sp, vm_ctx, sp_offset as i32);

                // Reduce it before going in the stack frame
                let new_sp = ctx.builder.ins().ireduce(types::I32, new_sp);

                // Stack pointer:
                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    new_sp,
                    slot_ptr,
                    offset_of!(StackFrame, sp) as i32,
                );

                // Instruction pointer:

                let ip_plus_one = ctx
                    .builder
                    .ins()
                    .iconst(types::I32, (fallback_ip + 1) as i64);

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    ip_plus_one,
                    slot_ptr,
                    offset_of!(StackFrame, ip) as i32,
                );

                // Instructions:

                // On a self call the stream we would save is the stream we are
                // about to install, and it is already a constant - so read it
                // from `instr_fat_ptr` rather than loading it back out of the vm.
                let current_instructions = if callee_is_self {
                    instr_fat_ptr
                } else {
                    ctx.builder.ins().load(
                        types::I128,
                        MemFlagsData::trusted(),
                        vm_ctx,
                        offset_of!(VmCore, instructions) as i32,
                    )
                };

                // TODO: This doesn't work correctly; we need to
                // construct a fat pointer here. So I need something
                // that is FFI safe in order to construct a fat
                // pointer to the *const [DenseInstruction]

                // Okay, now load the instructions from the function:
                // let instructions = ctx.builder.ins().load(
                //     types::I128,
                //     MemFlagsData::trusted(),
                //     function,
                //     offset_of!(ByteCodeLambda, body_exp) as i32,
                // );

                let instructions = instr_fat_ptr;

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    current_instructions,
                    slot_ptr,
                    offset_of!(StackFrame, instructions) as i32,
                );

                // Store the instructions back to the VM pointer. A self call is
                // already running this stream, so the store would write the value
                // that is there.
                if !callee_is_self {
                    ctx.builder.ins().store(
                        MemFlagsData::trusted(),
                        instructions,
                        vm_ctx,
                        offset_of!(VmCore, instructions) as i32,
                    );
                }

                // Store the function itself
                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    function,
                    slot_ptr,
                    offset_of!(StackFrame, function) as i32,
                );

                let null_pointer = ctx.builder.ins().iconst(types::I64, 0);

                // Null for the attachments
                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    null_pointer,
                    slot_ptr,
                    offset_of!(StackFrame, attachments) as i32,
                );

                let new_pop_count = ctx.pop_count_add_one(vm_ctx);

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    new_pop_count,
                    vm_ctx,
                    offset_of!(VmCore, pop_count) as i32,
                );

                // Set ip to 0
                let zero = ctx.builder.ins().iconst(types::I64, 0);
                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    zero,
                    vm_ctx,
                    offset_of!(VmCore, ip) as i32,
                );

                // Add one to the length:
                let new_length = ctx.builder.ins().iadd_imm_s(stack_frame_length, 1);

                ctx.builder.ins().store(
                    MemFlagsData::trusted(),
                    new_length,
                    thread_pointer,
                    (stack_frame_offset + len_offset) as i32,
                );
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

        self.converging_if_no_else_no_value_then_cold(
            at_capacity,
            // If at capacity, call grow
            |ctx| ctx.call_function_no_return("slow-grow-stack"),
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

    fn decrement_vm_stack_len(&mut self, n: i64) {
        if n == 0 {
            return;
        }

        self.properties.cached_lookups.stack_length_capacity = self
            .properties
            .cached_lookups
            .stack_length_capacity
            .saturating_add(n as usize);

        let ctx = self.get_ctx();
        let thread_pointer = self.get_thread_pointer(ctx);

        let stack_offset = offset_of!(SteelThread, stack);
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let new_length = self.builder.ins().iadd_imm_s(stack_length, -n);

        self.builder.ins().store(
            MemFlagsData::trusted(),
            new_length,
            thread_pointer,
            (stack_offset + len_offset) as i32,
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

        self.converging_if_no_else_no_value_then_cold(
            at_capacity,
            // If at capacity, call grow
            |ctx| {
                let amt = ctx.builder.ins().iconst(types::I64, arity as i64);
                ctx.call_function_args_no_return("slow-stack-reserve-exact", &[amt])
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

    fn push_to_many_vm_stack_let_var_new_unchecked(&mut self, values: &[Value]) {
        let ctx = self.get_ctx();
        let arity = values.len();

        let thread_pointer = self.get_thread_pointer(ctx);

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

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

        // Write the value(s):
        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlagsData::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let offset = self.builder.ins().ishl_imm_u(stack_length, 4);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let size: i32 = std::mem::size_of::<SteelVal>() as i32;

        for (index, value) in values.iter().enumerate() {
            self.builder.ins().store(
                MemFlagsData::trusted(),
                *value,
                slot_ptr,
                index as i32 * size,
            );
        }
    }

    fn push_const_index(&mut self, index: usize) -> Value {
        let local_callee = self.get_local_callee("push-const-index");

        let ctx = self.get_ctx();

        let value = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), index as i64);

        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_function_returns_value(&mut self, name: &str) -> Value {
        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let arg_values = [ctx];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    // Call a function by name with no return value, implicitly
    // passes the context
    fn call_function_no_return(&mut self, name: &str) {
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let arg_values = [ctx];
        let _ = self.builder.ins().call(local_callee, &arg_values);
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
    // The vm context stays an ordinary ssa value, which cranelift spills to a
    // stack slot and reloads after calls (99 times in fib). Pinning it to r15
    // instead was tried and measured: the reloads dropped to 2, but each read
    // became a register move rather than a free L1 hit, and reserving r15 cost
    // the allocator a register in a function that already used all five
    // callee-saved ones. fib came out 17% slower, reproducibly.
    fn get_ctx(&mut self) -> Value {
        self.vm_context
    }

    /// Call a function by name, with the first argument implicitly as the VM context.
    /// Returns a value.
    fn call_function_returns_value_args(&mut self, name: &str, args: &[Value]) -> Value {
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();

        let mut arg_values = vec![ctx];
        arg_values.extend(args.iter());
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// Calls a function by name, with the first argument implicitly as the VM context.
    /// Does not return a value.
    fn call_function_args_no_return(&mut self, name: &str, args: &[Value]) {
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();

        let mut arg_values = vec![ctx];
        arg_values.extend(args.iter());
        self.builder.ins().call(local_callee, &arg_values);
    }

    /// Calls a function by name, without implicitly passing a context.
    /// Returns a value.
    fn call_function_returns_value_args_no_context(&mut self, name: &str, args: &[Value]) -> Value {
        let local_callee = self.get_local_callee(name);
        let call = self.builder.ins().call(local_callee, &args);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// Calls a function by name, without implicitly passing a context.
    ///
    /// Does not return a value.
    fn call_function_args_no_context(&mut self, name: &str, args: &[Value]) {
        let local_callee = self.get_local_callee(name);
        let _ = self.builder.ins().call(local_callee, &args);
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
