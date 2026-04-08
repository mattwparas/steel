#![allow(unused)]
#![allow(improper_ctypes_definitions)]
#![allow(unpredictable_function_pointer_comparisons)]

mod native;

use core::{mem::offset_of, ptr::NonNull};
use cranelift::{
    codegen::ir::{ArgumentPurpose, FuncRef, GlobalValue, StackSlot, Type},
    frontend::Switch,
    prelude::{isa::CallConv, *},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use std::{collections::HashSet, slice};
use std::{
    collections::{HashMap, VecDeque},
    mem::ManuallyDrop,
};
use steel_derive::cross_platform_fn;
use steel_gen::{opcode::OPCODES_ARRAY, OpCode};

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::{pretty_print_dense_instructions, DenseInstruction},
    gc::Gc,
    primitives::{
        lists::{steel_is_empty, steel_pair},
        ports::{eof_objectp_jit, read_char_single_ref, steel_eof_objectp, steel_read_char},
        strings::{char_equals_binop, steel_char_equals},
        vectors::steel_mut_vec_set,
    },
    rvals::FunctionSignature,
    steel_vm::{
        primitives::{steel_eq, steel_listp, steel_stringp, steel_symbolp, steel_voidp},
        vm::{jit::*, StackFrame, StackFrameAttachments, SteelThread, VmCore},
    },
    values::{
        functions::{ByteCodeLambda, RootedInstructions},
        lists::SteelList,
        structs::create_struct_spec,
    },
    SteelVal,
};

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
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    function_map: OwnedFunctionMap,
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

// Build table mapping the function signatures
// from the runtime representation to their specialized one via op codes.

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
struct PrimitiveSignature {
    func: i64,
    arity: usize,
    shape: &'static [CallKind],
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
enum CallKind {
    Value,
    Ref,
    RefMut,
}

enum FunctionOrInstructionSet {
    Pointer(i64),
    InstructionSet(fn(&mut FunctionTranslator, arity: usize)),
    InstructionSetWithFallback(fn(&mut FunctionTranslator, arity: usize) -> bool, i64),
}

struct PrimitiveTable {
    map: HashMap<PrimitiveSignature, i64>,
}

impl PrimitiveTable {
    pub fn new() -> Self {
        let mut map = HashMap::new();

        map.insert(
            PrimitiveSignature {
                func: steel_read_char as *const () as i64,
                arity: 1,
                shape: &[CallKind::Ref],
            },
            read_char_single_ref as *const () as i64,
        );

        map.insert(
            PrimitiveSignature {
                func: steel_char_equals as *const () as i64,
                arity: 2,
                shape: &[CallKind::Value, CallKind::Value],
            },
            char_equals_binop as *const () as i64,
        );

        Self { map }
    }
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
    let mut value = ManuallyDrop::new(value);
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
        println!("instruction addr: {:p}", last.instructions.inner);
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

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();

        flag_builder
            .set("enable_llvm_abi_extensions", "true")
            .unwrap();

        flag_builder.set("opt_level", "speed").unwrap();

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

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
            "vec-handler-spilled",
            abi! { vec_handler_c as fn(*mut VmCore, usize) -> SteelVal },
        );

        // Value functions:
        map.add_func(
            "num-equal-value",
            abi! { num_equal_value as fn(*mut VmCore, SteelVal, SteelVal) -> SteelVal },
        );

        map.add_func2(
            "num-equal-int",
            abi! { num_equal_int as fn(SteelVal, SteelVal) -> SteelVal },
        );

        map.add_func(
            "num-equal-int-register",
            abi! { num_equal_int_register as fn(*mut VmCore, usize, SteelVal) -> bool },
        );

        map.add_func(
            "equal-binop",
            abi! { equal_binop as fn(*mut VmCore, SteelVal, SteelVal) -> SteelVal },
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
            "eq?-reg-2",
            abi! { eq_reg_2 as fn(ctx: *mut VmCore, usize, usize) -> bool },
        );

        map.add_func(
            "eq?-reg-1",
            abi! { eq_reg_1 as fn(ctx: *mut VmCore, usize, SteelVal) -> bool },
        );

        map.add_func2(
            "eq?-args",
            abi! { eq_value as fn(SteelVal, SteelVal) -> bool },
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
            "sub-binop-int-reg",
            abi! { extern_c_sub_two_int_reg
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

        map.add_func_hint("lte-binop", extern_c_lte_two as VmBinOp, InferredType::Bool);

        map.add_func_hint2(
            "lte-binop-int",
            extern_c_lte_two_int as BinOp,
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

        map.add_func_hint2(
            "lt-binop-int",
            extern_c_lt_two_int as BinOp,
            InferredType::Bool,
        );

        map.add_func_hint2(
            "null-handler",
            abi! { extern_c_null_handler as fn(a: SteelVal) -> SteelVal },
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
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
            function_map,
        }
    }
}

fn discriminant(value: &SteelVal) -> u8 {
    // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
    // between `repr(C)` structs, each of which has the `u8` discriminant as its first
    // field, so we can read the discriminant without offsetting the pointer.
    unsafe { *<*const _>::from(value).cast::<u8>() }
}

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct JitFnPointer(NonNull<u8>);

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
) -> Result<JitFnPointer, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    // TODO: We'll want to do two different functions.
    // One which interfaces with the VM, and another which can be called directly
    // without going through the VM.
    //
    // That way we can have direct calls for tail calls and for recursive calls which
    // aren't tail calls.
    //
    // The only difference I believe will be the signature, and we'll have to
    // take the parameters into account in this case - they won't come from the VM
    // stack but will instead come from the native stack.
    let code_ptr = jit.compile(name, arity, code, globals, constants, function_index, slot)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    // let code_fn = core::mem::transmute::<*const u8, fn(&mut VmCore)>(code_ptr);

    let code_fn = JitFnPointer(NonNull::new_unchecked(code_ptr.cast_mut()));

    Ok(code_fn)
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
            )
        }
    }
}

impl JIT {
    // Use this to get the trampoline, and then our new entrypoint from
    // the rust VM is a trampoline into the tail call world.
    pub fn compile_trampoline(&mut self) -> *const u8 {
        let ptr_ty = self.module.target_config().pointer_type();

        // Outer signature: C ABI, takes (vm_ctx, target)
        let mut outer_sig = self.module.make_signature();
        let mut vm_param = AbiParam::new(ptr_ty);
        vm_param.purpose = ArgumentPurpose::VMContext;
        outer_sig.params.push(vm_param);
        outer_sig.params.push(AbiParam::new(ptr_ty));

        let mut inner_sig = self.module.make_signature();
        let mut vm_param = AbiParam::new(ptr_ty);
        vm_param.purpose = ArgumentPurpose::VMContext;
        inner_sig.params.push(vm_param);
        inner_sig.call_conv = CallConv::Tail;

        let func_id = self
            .module
            .declare_function("jit_trampoline", Linkage::Local, &outer_sig)
            .unwrap();

        self.ctx.func.signature = outer_sig;
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);

        let vm_ctx = builder.block_params(block)[0];
        let target = builder.block_params(block)[1];

        let sig_ref = builder.import_signature(inner_sig);
        builder.ins().call_indirect(sig_ref, target, &[vm_ctx]);
        builder.ins().return_(&[]);

        builder.seal_all_blocks();
        builder.finalize();

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
    ) -> Result<*const u8, String> {
        // self.ctx.set_disasm(true);

        if let Some(data) = self.module.get_name(&name) {
            match data {
                cranelift_module::FuncOrDataId::Func(func_id) => {
                    return Ok(self.module.get_finalized_function(func_id));
                }
                cranelift_module::FuncOrDataId::Data(data_id) => panic!(),
            }
        }

        let (params, stmts) = (Default::default(), instructions);

        let pointer = self.module.target_config().pointer_type();

        let mut param = AbiParam::new(pointer);
        param.purpose = ArgumentPurpose::VMContext;

        self.ctx.func.signature.params.push(param);
        self.ctx.func.signature.call_conv = CallConv::Tail;

        let inner_name = format!("{}_inner", name);
        let inner_id = self
            .module
            .declare_function(&inner_name, Linkage::Local, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // let id = self
        //     .module
        //     .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
        //     .map_err(|e| e.to_string())?;

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(
            inner_name.clone(),
            inner_id,
            arity,
            params,
            stmts,
            globals,
            constants,
            function_index,
            slot,
        )?;

        if let Err(e) = cranelift::codegen::verify_function(&self.ctx.func, self.module.isa()) {
            // println!("{:#?}", self.ctx.func);
            eprintln!("{:#?}", e);
            self.module.clear_context(&mut self.ctx);
            return Err(format!("errors: {:#?}", e));
        }

        self.module
            .define_function(inner_id, &mut self.ctx)
            .map_err(|e| {
                eprintln!("error in defining function: {}", e);
                self.module.clear_context(&mut self.ctx);
                e.to_string()
            })?;

        // let asm = self.ctx.compiled_code().map(|x| x.vcode.as_ref()).flatten();
        // if let Some(asm) = asm {
        //     println!("{}", asm);
        // }

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code = self.module.get_finalized_function(inner_id);

        // Set up the trampoline

        Ok(code)
    }

    fn translate(
        &mut self,
        _name: String,
        _func_id: FuncId,
        arity: u16,
        params: Vec<String>,
        bytecode: &[DenseInstruction],
        globals: &[SteelVal], // stmts: Vec<Expr>,
        constants: &ConstantMap,
        function_context: Option<usize>,
        slot: Option<&Gc<ByteCodeLambda>>,
    ) -> Result<(), String> {
        // println!("----- Compiling function ----");

        // pretty_print_dense_instructions(bytecode);

        let int = Type::int(128).unwrap();

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let vm_context = builder.create_global_value(GlobalValueData::VMContext);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let contains_tail_call = bytecode
            .iter()
            .any(|x| matches!(x.op_code, OpCode::TCOJMP | OpCode::SELFTAILCALLNOARITY));

        let vm_context = builder.create_global_value(GlobalValueData::VMContext);

        let variables = Default::default();

        let fake_entry_block = if contains_tail_call {
            let fake_entry = builder.create_block();
            builder.ins().jump(fake_entry, &[]);
            builder.switch_to_block(fake_entry);

            Some(fake_entry)
        } else {
            None
        };

        let exit_block = builder.create_block();

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
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
            properties: Default::default(),
            visited: HashSet::default(),
            depth: 0,
            if_stack: Vec::new(),
            if_bound: None,
            vm_context,
            slot,
            function_context,
        };

        trans.stack_to_ssa();

        if let Some(fake_entry_block) = fake_entry_block {
            trans.builder.seal_block(fake_entry_block);
        }

        // trans.builder.switch_to_block(exit_block);

        trans.builder.ins().return_(&[]);

        trans.builder.seal_block(exit_block);

        // Just seal all the blocks?
        trans.builder.seal_all_blocks();

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum InferredType {
    // If we know this is an i64 concretely
    Int64,

    // Could be either an i64 or a big int
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

    Char,

    String,

    Symbol,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct StackValue {
    value: Value,
    inferred_type: InferredType,
    // Whether or not the value is spilled
    // to the stack
    spilled: bool,
}

impl StackValue {
    pub fn as_steelval(&self, ctx: &mut FunctionTranslator) -> Value {
        match self.inferred_type {
            InferredType::UnboxedBool => {
                let value = ctx.builder.ins().uextend(types::I64, self.value);
                ctx.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, value)
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
    fn as_steelval(self) -> SteelVal {
        match self {
            ConstantValue::Int(i) => SteelVal::IntV(i),
            ConstantValue::Bool(b) => SteelVal::BoolV(b),
            ConstantValue::Char(c) => SteelVal::CharV(c),
            ConstantValue::Float(f) => SteelVal::NumV(f),
            ConstantValue::Index(_) => panic!(),
        }
    }

    fn into_index(self) -> usize {
        if let Self::Index(i) = self {
            i
        } else {
            panic!()
        }
    }

    fn as_typ(self) -> InferredType {
        match self {
            ConstantValue::Int(_) => InferredType::Int,
            ConstantValue::Bool(_) => InferredType::Bool,
            ConstantValue::Char(_) => InferredType::Char,
            ConstantValue::Float(_) => InferredType::Number,
            ConstantValue::Index(_) => InferredType::Any,
        }
    }

    fn to_value(self, ctx: &mut FunctionTranslator) -> (Value, InferredType) {
        match self {
            // TODO: We can probably infer the type here though, since we know
            // what the type is based on the values coming in
            ConstantValue::Index(p) => (ctx.push_const_index(p), InferredType::Any),
            _ => {
                // let value = ctx.create_i128(encode(self.as_steelval()));
                let value = ctx.encode_void();
                (value, self.as_typ())
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
    Value(StackValue),

    // These should already be spilled by default.
    //
    // Mutable registers will get &mut SteelVal via the
    // index
    MutRegister(usize),

    // Registers will get &SteelVal via the index
    Register(usize),

    Constant(ConstantValue),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum ValueOrRegister {
    Value(Value),
    Register(usize),
}

#[derive(Default, Clone)]
struct PropertyMap {
    // So we're going to do something like this.
    props: HashMap<ValueOrRegister, Vec<Properties>>,
}

impl PropertyMap {
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

    pub fn add_property(&mut self, value: ValueOrRegister, prop: Properties) {
        if let Some(exists) = self.props.get_mut(&value) {
            match prop {
                Properties::NonEmptyListOrPair => {
                    for p in exists {
                        if let Properties::ProperList = p {
                            *p = Properties::ProperNonEmptyList;
                        }
                    }
                }
                Properties::ProperList => {
                    for p in exists {
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
                    _ => {}
                }
            }
        }
    }
}

// TODO: Figure out a good way to align inferred type but also
// additional properties.
#[derive(Debug, Clone, Copy)]
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
}

impl MaybeStackValue {
    fn into_index(self) -> usize {
        match self {
            MaybeStackValue::MutRegister(p) => p,
            MaybeStackValue::Register(p) => p,
            _ => panic!(),
        }
    }

    fn into_value(self) -> StackValue {
        if let Self::Value(v) = self {
            v
        } else {
            panic!()
        }
    }

    fn as_value(self) -> Option<StackValue> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,

    // We're gonna use a cursor to understand the before and after implications
    // of the instructions. For now, we'll compile sequences of hot instructions
    // together into a dynamic sequence.
    instructions: &'a [DenseInstruction],
    ip: usize,
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

    // function_context: Option<usize>,
    // id: FuncId,
    // call_global_all_native: bool,
    // name: String,
    intrinsics: &'a OwnedFunctionMap,

    fake_entry_block: Option<Block>,
    exit_block: Block,
    visited: HashSet<usize>,

    depth: usize,

    if_bound: Option<usize>,

    if_stack: Vec<usize>,

    vm_context: GlobalValue,
    // vm_context: StackSlot,
    // generators: LazyInstructionGenerators,
    slot: Option<&'a Gc<ByteCodeLambda>>,
    function_context: Option<usize>,
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
    match (op, payload) {
        (OpCode::IF, _) => "if-branch-value",
        (OpCode::CALLGLOBAL, _) => "call-global",
        (OpCode::PUSHCONST, _) => "push-const",
        (OpCode::READLOCAL0, _) => "read-local-0",
        (OpCode::READLOCAL1, _) => "read-local-1",
        (OpCode::READLOCAL2, _) => "read-local-2",
        (OpCode::READLOCAL3, _) => "read-local-3",

        // (OpCode::READLOCAL, 4) => "read-local-4",
        // (OpCode::READLOCAL, 5) => "read-local-5",
        // (OpCode::READLOCAL, 6) => "read-local-6",
        // (OpCode::READLOCAL, 7) => "read-local-7",
        // (OpCode::READLOCAL, 8) => "read-local-8",
        (OpCode::READLOCAL, _) => "read-local-any",

        (OpCode::READCAPTURED, _) => "read-captured",

        (OpCode::MOVEREADLOCAL0, _) => "move-read-local-0",
        (OpCode::MOVEREADLOCAL1, _) => "move-read-local-1",
        (OpCode::MOVEREADLOCAL2, _) => "move-read-local-2",
        (OpCode::MOVEREADLOCAL3, _) => "move-read-local-3",
        // (OpCode::MOVEREADLOCAL, 4) => "move-read-local-4",
        // (OpCode::MOVEREADLOCAL, 5) => "move-read-local-5",
        // (OpCode::MOVEREADLOCAL, 6) => "move-read-local-6",
        // (OpCode::MOVEREADLOCAL, 7) => "move-read-local-7",
        // (OpCode::MOVEREADLOCAL, 8) => "move-read-local-8",
        (OpCode::MOVEREADLOCAL, _) => "move-read-local-any",

        (OpCode::ADD, 2) => "add-binop",
        (OpCode::ADD, 3) => "add-three",
        (OpCode::ADD, 4) => "add-four",
        (OpCode::SUB, 2) => "sub-binop",
        (OpCode::SUB, 3) => "sub-three",

        (OpCode::SUB, 1) => "sub-negate",

        (OpCode::LT, 2) => "lt-binop",
        (OpCode::LTE, 2) => "lte-binop",
        (OpCode::GT, 2) => "gt-binop",
        (OpCode::GTE, 2) => "gte-binop",
        (OpCode::MUL, 2) => "mult-two",
        (OpCode::MUL, 3) => "mult-three",
        (OpCode::DIV, 2) => "div-two",
        (OpCode::PUSH, _) => "push-global-value",

        (OpCode::NOT, _) => "not-value",

        // TODO!()
        (OpCode::NUMEQUAL, 2) => "num-equal-value",
        (OpCode::EQUAL2, _) => "equal-binop",

        (OpCode::CAR, _) => "car-handler-value",
        (OpCode::CDR, _) => "cdr-handler-value",
        (OpCode::CONS, _) => "cons-handler-value",

        (OpCode::NEWBOX, _) => "box-handler",
        (OpCode::UNBOX, _) => "unbox-handler",
        (OpCode::SETBOX, _) => "set-box-handler",

        (OpCode::LISTREF, _) => "list-ref-value",
        (OpCode::VECTORREF, _) => "vector-ref-value",

        other => panic!(
            "couldn't match the name for the op code + payload: {:?}",
            other
        ),
    }
}

// TODO: This is certainly no good. We should instead not use transmute,
// but rather encode the enum variant manually as to not create any
// nasal demons
fn encode(value: SteelVal) -> i128 {
    unsafe { core::mem::transmute(value) }
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
                // Have to tell if the if statement converged, and to continue
                // going from there
                if self.ip == last {
                    // println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% breaking");
                    // let instr = self.instructions[self.ip];
                    // let op = instr.op_code;
                    // let payload = instr.payload_size.to_usize();
                    // println!("{:?}:{} @ {}", op, payload, self.ip);
                    // println!("length: {}", self.instructions.len());

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

            // println!("{:?}:{} @ {}", op, payload, self.ip);

            // if self.queue.len() == 10000 {
            //     self.queue.pop_front();
            // }

            // self.queue.push_back((op, self.ip, payload));

            if !self.visited.insert(self.ip) {
                panic!("Already visited this instruction",);
            }

            match op {
                OpCode::LOADINT1POP | OpCode::BINOPADDTAIL => {
                    todo!("{:?}", op);
                }

                OpCode::SCLOSURE => {
                    panic!("Deprecated opcode");
                }
                // Primitives can be checked to see if we have
                // better information, and then can be inlined
                // accordingly with a trampoline without needing
                // a special opcode.
                // OpCode::CALLPRIMITIVE => {
                //     todo!("{:?}", op);
                // }
                // OpCode::POPJMP | OpCode::POPPURE => {
                OpCode::POPPURE => {
                    self.maybe_check_last();
                    let (value, _) = self.shadow_pop();
                    self.spill_cloned_stack();
                    self.vm_pop(value);
                    self.ip = self.instructions.len() + 1;

                    /*

                    let last = self.stack.last().unwrap();

                    assert!(!last.spilled);

                    // Push the remaining value back on to the stack.
                    let value = self.pop();

                    for value in self.stack.clone() {
                        if !value.spilled {
                            self.push_to_vm_stack(value.value);
                        }
                    }

                    self.vm_pop(value.0);
                    self.ip = self.instructions.len() + 1;

                    */

                    self.depth -= 1;

                    return false;
                }
                OpCode::VOID => {
                    // Push void onto stack?
                    // let void = SteelVal::Void;
                    // let value = self.create_i128(encode(void));

                    let value = self.encode_void();

                    self.push(value, InferredType::Void);

                    self.ip += 1;
                }
                OpCode::PUSH => {
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

                // TODO: Still adjust the ip as needed
                OpCode::IF => {
                    // If we can check the local variable on the stack, we should do that
                    if matches!(
                        self.shadow_stack.last(),
                        Some(MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_))
                    ) {
                        let test = self.shadow_stack.pop().unwrap();

                        let false_instr = self.instructions[self.ip].payload_size;
                        let true_instr = self.ip + 1;

                        let test_bool = self.call_test_handler_register(test.into_index());

                        self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());

                        // self.push(res, InferredType::Any);
                    } else {
                        let last_ref = self.shadow_stack.last().and_then(|x| x.as_value());

                        if last_ref.map(|x| x.inferred_type) == Some(InferredType::UnboxedBool) {
                            let false_instr = self.instructions[self.ip].payload_size;
                            let true_instr = self.ip + 1;

                            // Explicitly want the unboxed value here
                            let test_bool = last_ref.unwrap().value;

                            // dbg!(self.builder.func.dfg.value_type(test_bool));

                            self.shadow_stack.pop();

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
                                _ => self.call_test_handler(test),
                            };

                            self.translate_if_else_value(
                                test_bool,
                                true_instr,
                                false_instr.to_usize(),
                            );

                            // self.push(res, InferredType::Any);
                        }
                    }

                    // self.depth -= 1;
                    // return false;
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

                    const USE_INLINE_CALL_FUNC: bool = true;

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

                                    // Missing shadow stack pop!
                                    ctx.shadow_stack.pop().unwrap();

                                    // TODO: Clone / restore this for this branch,
                                    // so the other branch can inherit the changes
                                    let args_off_the_stack = ctx
                                        .split_off(arity)
                                        .into_iter()
                                        .map(|x| x.0)
                                        .collect::<Vec<_>>();

                                    ctx.spill_stack();

                                    let arity = args_off_the_stack.len();

                                    // TODO: Figure out a way to swap these inline?
                                    // Directly push these on to the stack.
                                    for arg in &args_off_the_stack {
                                        ctx.push_to_vm_stack_let_var_new(*arg);
                                    }

                                    let should_trampoline = ctx.check_should_trampoline(vm_ctx);

                                    // TODO: This is not going to work here. Instead, we need to load
                                    // the id of the instruction.
                                    let super_instruction = ctx.builder.ins().load(
                                        types::I64,
                                        MemFlags::trusted(),
                                        closure,
                                        // Offset for the RC payload
                                        16 + offset_of!(ByteCodeLambda, super_instructions) as i32,
                                    );

                                    let super_instruction_exists = ctx.builder.ins().icmp_imm(
                                        IntCC::NotEqual,
                                        super_instruction,
                                        0,
                                    );

                                    let should_continue = ctx
                                        .builder
                                        .ins()
                                        .band(should_trampoline, super_instruction_exists);

                                    let should_yield =
                                        ctx.builder.ins().bxor_imm(should_continue, 1);
                                    let fallback_ip = ctx.ip - 1;
                                    // let fallback_ip = ctx.ip;
                                    ctx.update_ip_native_if_yield(
                                        vm_ctx,
                                        should_yield,
                                        fallback_ip + 1,
                                    );

                                    let res = ctx.converging_if(
                                        should_continue,
                                        |ctx| {
                                            // Increment the ref count of the closure
                                            ctx.increment_ref_count_closure(closure);

                                            let body_exp_offset =
                                                16 + offset_of!(ByteCodeLambda, body_exp) as i32;

                                            let rcbox_ptr = ctx.builder.ins().load(
                                                types::I64,
                                                MemFlags::trusted(),
                                                closure,
                                                body_exp_offset,
                                            );

                                            let len = ctx.builder.ins().load(
                                                types::I64,
                                                MemFlags::trusted(),
                                                closure,
                                                body_exp_offset + 8,
                                            );

                                            let data_ptr =
                                                ctx.builder.ins().iadd_imm(rcbox_ptr, 16);

                                            let instr_fat_ptr =
                                                ctx.builder.ins().iconcat(data_ptr, len);

                                            ctx.push_stack_frame(
                                                arity as _,
                                                closure,
                                                instr_fat_ptr,
                                                fallback_ip,
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
                                            ctx.builder.ins().call_indirect(
                                                sig_ref,
                                                super_instruction,
                                                &[vm_ctx],
                                            );

                                            let is_still_native = ctx.builder.ins().load(
                                                types::I8,
                                                MemFlags::trusted(),
                                                vm_ctx,
                                                offset_of!(VmCore, is_native) as i32,
                                            );

                                            ctx.converging_if(
                                                is_still_native,
                                                |ctx| ctx.inline_pop_from_stack(vm_ctx),
                                                |ctx| ctx.encode_void(),
                                                typ,
                                            )
                                        },
                                        |ctx| {
                                            let arity =
                                                ctx.builder.ins().iconst(types::I64, arity as i64);
                                            let fallback_ip = ctx
                                                .builder
                                                .ins()
                                                .iconst(types::I64, fallback_ip as i64);
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
                                        let v = ctx.call_function(arity, name, false);

                                        v
                                    } else {
                                        todo!("Implement spilled function call");
                                    }
                                },
                                typ,
                            );

                            self.push(res, InferredType::Any)
                        }

                        _ => {
                            if let Some(name) = name {
                                let v = self.call_function(arity, name, false);
                                self.push(v, InferredType::Any);
                            } else {
                                todo!("Implement spilled function call");
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

                    if let Some(name) = name {
                        let v = self.call_function(arity, name, true);
                        self.push(v, InferredType::Any);
                    } else {
                        todo!("Implement spilled function call");
                    }

                    self.ip = self.instructions.len() + 1;

                    self.check_deopt();

                    self.depth -= 1;

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

                    self.push(v, InferredType::Function);
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

                    self.push(v, InferredType::Function);
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
                    // TODO: @matt remove this and make it more general
                    // TODO: This optimization should be more general
                    // if payload < self.arity as usize
                    //     && payload + 1 == self.arity as usize
                    //     && self
                    //         .instructions
                    //         .get(self.ip + 1)
                    //         .map(|x| x.op_code == OpCode::SELFTAILCALLNOARITY)
                    //         .unwrap_or_default()
                    // {
                    //     // Leave the last in place
                    //     self.ip += 1;
                    //     let payload = self.instructions[self.ip].payload_size.to_usize();
                    //     let _ = self
                    //         .test_translate_tco_jmp_no_arity_loop_no_spill(payload - 1, payload);
                    //     self.ip = self.instructions.len() + 1;
                    //     return false;
                    // } else {

                    // let (value, inferred_type) = self.read_local_value(op, payload);
                    let value = self.spilled_read_local_value(op, payload);
                    self.ip += 1;
                    // self.push(value, inferred_type);
                    self.shadow_push(value);
                    // }
                }
                OpCode::PUSHCONST => {
                    let payload = self.instructions[self.ip].payload_size.to_usize();
                    let (value, typ) = self.get_const(op, payload);
                    self.ip += 1;
                    self.push(value, typ);
                }
                OpCode::TRUE => {
                    let constant = SteelVal::BoolV(true);
                    // let value = self.create_i128(encode(constant));
                    let value = self.encode_true();
                    self.ip += 1;
                    // self.advance_ip();
                    self.push(value, InferredType::Bool);
                }
                OpCode::FALSE => {
                    let constant = SteelVal::BoolV(false);
                    // let value = self.create_i128(encode(constant));
                    let value = self.encode_false();
                    self.ip += 1;
                    // self.advance_ip();
                    self.push(value, InferredType::Any);
                }
                OpCode::LOADINT0 | OpCode::LOADINT1 | OpCode::LOADINT2 => {
                    let payload = self.instructions[self.ip].payload_size.to_usize();
                    // self.advance_ip();
                    let value = self.call_func_or_immediate(op, payload);
                    self.ip += 1;
                    self.push(value, InferredType::Int);
                }

                OpCode::LetVar => {
                    self.ip += 1;

                    // let spilled = self.stack.last().unwrap().spilled;
                    // assert!(!spilled);

                    self.maybe_check_last();

                    // self.spill(self.stack.len() - 1);

                    let (last, typ) = self.shadow_pop();

                    let local_index = *self.let_var_stack.last().unwrap() + self.arity as usize;
                    self.value_to_local_map.insert(last, local_index);
                    self.local_to_value_map.insert(local_index, typ);

                    *self.let_var_stack.last_mut().unwrap() += 1;

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
                OpCode::SELFTAILCALLNOARITY => {
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

                    const USE_INLINE_GLOBAL_TAIL_CALL: bool = true;

                    let func = self._globals.get(function_index).cloned();

                    // TODO: For calling struct operations, then we'll
                    // just call the thing, and we can move on to the pop
                    // I think.
                    let maybe_global = self._globals.get(function_index).cloned();
                    if let Some(maybe_global) = maybe_global {
                        if let Some(spec) = create_struct_spec(maybe_global) {
                            // TODO: This is where we inline the calls for struct
                            // functions
                            if let Some((value, typ)) =
                                self.inline_struct_call_no_drop(spec, arity, function_index)
                            {
                                println!("Compiling the tail call for structs");
                                self.inline_handle_pop(value);
                                self.depth -= 1;
                                self.ip = self.instructions.len() + 1;
                                return false;
                            }
                        }
                    }

                    // Call direct, by hard coding this, and we're gonna check that the
                    // instructions exist already...
                    if USE_INLINE_GLOBAL_TAIL_CALL && matches!(func, Some(SteelVal::Closure(_))) {
                        let function = if let Some(SteelVal::Closure(v)) = func {
                            v
                        } else {
                            unreachable!();
                        };

                        // Take this fast path if the super instructions already exists.
                        // Then we can do a direct call.
                        if function.super_instructions.is_some() {
                            self.inline_global_tail_call(function_index, arity, function);
                            self.ip = self.instructions.len() + 1;
                            self.depth -= 1;
                        } else {
                            self.slow_path_deopt_tail_call(function_index, arity);
                        }

                        return false;
                    } else {
                        self.slow_path_deopt_tail_call(function_index, arity);
                        return false;
                    }
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
                    let name = CallGlobalNoArityFunctionDefinitions::arity_to_name(arity);

                    let self_name = CallSelfNoArityFunctionDefinitions::arity_to_name(arity);

                    // Okay, lets try to install the self call if we have the ability to.
                    //
                    // We're also going to commit some very nasty crimes by just arbitrarily passing
                    // the value to the function as is, and hope for the best :)
                    if self.slot.is_some()
                        && self_name.is_some()
                        && self._globals.get(payload).is_none()
                        && self.function_context == Some(function_index)
                    {
                        const USE_EXPERIMENTAL_CALL: bool = true;

                        if USE_EXPERIMENTAL_CALL {
                            let slot = self.slot.unwrap().clone();

                            let result = self.call_self_function_experimental(
                                arity,
                                self_name.unwrap(),
                                slot,
                            );

                            self.push(result, InferredType::Any);
                        } else {
                            let slot = self.slot.unwrap().clone();
                            let result =
                                self.call_self_function(arity, self_name.unwrap(), slot, false);

                            // Assuming this worked, we'll want to push this result on to the stack.
                            self.push(result, InferredType::Any);
                        }
                    } else if let Some(name) = name {
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

                OpCode::CALLPRIMITIVE => {
                    // Check the actual value that we're looking up, see if its there
                    let function_index = payload;

                    // dbg!(function_index);

                    let global = self._globals.get(function_index);

                    match global.cloned() {
                        Some(SteelVal::FuncV(f)) => {
                            // Attempt the other call
                            self.ip += 1;
                            let arity = self.instructions[self.ip].payload_size.to_usize();

                            // Install lots of lookups for this stuff
                            // if f == crate::primitives::strings::steel_char_equals
                            //     as FunctionSignature
                            //     && arity == 2
                            // {
                            //     self.char_equals(arity);
                            // }
                            /*
                            else if f == steel_read_char as FunctionSignature
                                && arity == 1
                                && false
                            {
                                self.read_char(arity);
                            }
                            */

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

                                f if f == steel_eq as FunctionSignature && arity == 2 => self.eq(),

                                f if f == steel_pair as FunctionSignature && arity == 1 => {
                                    self.is_pair()
                                }

                                f if f == steel_is_empty as FunctionSignature && arity == 1 => {
                                    self.is_empty()
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
                                self.call_global_impl(payload);
                            }
                        }

                        _ => {
                            // println!("code-gen: {:?}", global);
                            self.call_global_impl(payload);
                        }
                    }
                }

                OpCode::CALLGLOBAL => {
                    self.call_global_impl(payload);
                }
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

                    // for arg in 0..self.stack.len() {
                    //     self.spill(arg);
                    // }

                    for arg in 0..self.shadow_stack.len() {
                        self.shadow_spill(arg);
                    }

                    self.ip += 1;
                }
                OpCode::LETENDSCOPE => {
                    // self.local_count = payload;
                    self.ip += 1;

                    // let last = self.let_var_stack.iter().sum::<usize>() + self.arity as usize;

                    let amt = self.let_var_stack.pop().unwrap();

                    // println!("let end scope amount: {}", amt);

                    for index in 0..self.shadow_stack.len() {
                        let v = self.shadow_stack.get_mut(index).unwrap();
                        match v {
                            MaybeStackValue::MutRegister(r) if *r >= payload as usize => {
                                let r = *r;
                                let (value, _) = self.mut_register_to_value(r);

                                self.properties.remove(&ValueOrRegister::Register(r));

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

                    self.inline_let_end_scope(payload, amt);

                    // self.call_end_scope_handler_new(payload, amt);
                }

                // TODO: Depending on the inferred type, we can save a lot of
                // operations here.
                OpCode::SUB
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[
                                MaybeStackValue::MutRegister(_) | MaybeStackValue::Register(_),
                                MaybeStackValue::Value(StackValue {
                                    inferred_type: InferredType::Int,
                                    ..
                                })
                            ])
                        ) =>
                {
                    let value = self.shadow_stack.pop().unwrap().into_value();
                    let register_index = self.shadow_stack.pop().unwrap().into_index();

                    // Do the shift here, in an effort to avoid passing more stuff?
                    let value = value.as_steelval(self);

                    let local_value = self.read_from_vm_stack(register_index);
                    let is_int = self.is_type(local_value, SteelVal::INT_TAG);

                    let sp = |ctx: &mut Self| {
                        let register = ctx.builder.ins().iconst(types::I64, register_index as i64);
                        let args = [register, value];
                        let result =
                            ctx.call_function_returns_value_args("sub-binop-int-reg", &args);

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
                    let value = self.shadow_stack.pop().unwrap().into_value();
                    let register = self.shadow_stack.pop().unwrap().into_index();

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
                    let value = self.shadow_stack.pop().unwrap().into_value();

                    let value_as_steelval = value.as_steelval(self);

                    let register = self.shadow_stack.pop().unwrap().into_index();

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
                    let register_r = self.shadow_stack.pop().unwrap().into_index();
                    let register_l = self.shadow_stack.pop().unwrap().into_index();

                    // dbg!(self.local_to_value_map.get(&register_r));
                    // dbg!(self.local_to_value_map.get(&register_l));

                    let register_r = self.builder.ins().iconst(types::I64, register_r as i64);
                    let register_l = self.builder.ins().iconst(types::I64, register_l as i64);

                    let args = [register_l, register_r];
                    let result = self.call_function_returns_value_args("add-binop-reg-2", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Number);

                    self.ip += 2;
                }

                OpCode::ADD
                    if payload == 2
                        && matches!(
                            self.shadow_stack.get(self.shadow_stack.len() - 2..),
                            Some(&[MaybeStackValue::Value(_), MaybeStackValue::Value(_)])
                        ) =>
                {
                    let MaybeStackValue::Value(r) = self.shadow_stack.pop().unwrap() else {
                        panic!()
                    };
                    let MaybeStackValue::Value(l) = self.shadow_stack.pop().unwrap() else {
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
                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    // if op == OpCode::ADD {
                    //     println!(
                    //         "Getting here: {:?}",
                    //         self.shadow_stack.get(self.shadow_stack.len() - 2)
                    //     );
                    // }
                    // Call the func
                    self.func_ret_val(op, payload, 2, InferredType::Number);
                    self.check_deopt();
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
                    self.func_ret_val_named("lt-binop-int", payload, 2, InferredType::Bool);
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

                    let rhs_int = self.shadow_stack.pop().unwrap().into_value();
                    let register_l = self.shadow_stack.pop().unwrap().into_index();

                    // Happy path, lets check the type of the lhs. If its an integer tag, then we can go ahead and do the thing.
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

                    // dbg!(self.local_to_value_map.get(&register_r));
                    // dbg!(self.local_to_value_map.get(&register_l));

                    // let local_value = self.read_from_vm_stack(register_l);
                    // self.call_function_args_no_context("#%debug-steel-value", &[local_value]);

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

                    let rhs_int = self.shadow_stack.pop().unwrap().into_value();
                    let register_l = self.shadow_stack.pop().unwrap().into_index();

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
                    self.func_ret_val_named("lte-binop-int", payload, 2, InferredType::Bool);
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
                    let last = self.shadow_stack.pop().unwrap().into_value();

                    // This is now our register; this is where the values will live.
                    let register = self.shadow_stack.pop().unwrap().into_index();
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
                            let rhs = ctx.unbox_value_to_pointer(last.value);
                            ctx.builder.ins().icmp(IntCC::Equal, register_payload, rhs)
                        },
                        |ctx| {
                            // Handle the else case here as well
                            // todo!();

                            let register_int =
                                ctx.builder.ins().iconst(types::I64, register as i64);

                            let vm_ctx = ctx.get_ctx();

                            let res = ctx.call_function_returns_value_args_no_context(
                                "num-equal-int-register",
                                &[vm_ctx, register_int, last.value],
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
                OpCode::NUMEQUAL
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    self.func_ret_val_named("num-equal-int", payload, 2, InferredType::Bool);
                }

                // OpCode::GTE if payload == 2 => {
                //     // If we know the concrete type, we might be able to
                //     // do something inline?
                //     self.gte()
                // }
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

                OpCode::EQUAL | OpCode::NUMEQUAL | OpCode::EQUAL2 => {
                    // println!("Generating code for equal");
                    self.func_ret_val(op, payload, 2, InferredType::Bool);
                }

                // TODO: Use the register here. Checking is null might be slightly more involved?
                // Or rather, lets just use is-empty?
                OpCode::NULL
                    if matches!(
                        self.shadow_stack.last(),
                        Some(MaybeStackValue::Register(_) | MaybeStackValue::MutRegister(_))
                    ) =>
                {
                    let last = self.shadow_stack.pop().unwrap().into_index();
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

                        // Probably
                        &[MaybeStackValue::Value(_), MaybeStackValue::MutRegister(l)] => {
                            let register = self.shadow_stack.pop().unwrap().into_index();
                            let value = self
                                .shadow_stack
                                .pop()
                                .unwrap()
                                .into_value()
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

                        // Anything else, we'll move on
                        _ => {
                            self.func_ret_val(op, 2, 2, InferredType::List);
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
                            // let can_skip_bounds_check = matches!(
                            //     self.properties.get(&ValueOrRegister::Register(reg)),
                            //     Some(Properties::NonEmptyList)
                            // );

                            let can_skip_bounds_check = false;

                            self.shadow_stack.pop();
                            let reg = self.register_index(reg);

                            let func = if can_skip_bounds_check {
                                "cdr-reg-no-check"
                            } else {
                                "cdr-reg"
                            };

                            let res = self.call_function_returns_value_args(func, &[reg]);
                            self.push(res, InferredType::Any);
                            self.check_deopt();
                            self.ip += 2;
                        }

                        MaybeStackValue::MutRegister(reg) => {
                            // let can_skip_bounds_check = matches!(
                            //     self.properties.get(&ValueOrRegister::Register(reg)),
                            //     Some(Properties::NonEmptyList)
                            // );

                            let can_skip_bounds_check = false;

                            self.shadow_stack.pop();
                            let ir_reg = self.register_index(reg);

                            if can_skip_bounds_check {
                                let func = "cdr-mut-reg-no-check";
                                self.call_function_args_no_return(func, &[ir_reg]);
                                self.shadow_push(MaybeStackValue::MutRegister(reg));
                            } else {
                                let func = "cdr-mut-reg";

                                let res = self.call_function_returns_value_args(func, &[ir_reg]);
                                self.push(res, InferredType::Any);
                            };

                            self.check_deopt();
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

                            let can_skip_bounds_check = false;

                            if can_skip_bounds_check {
                                self.shadow_stack.pop();

                                let value = self.read_from_vm_stack(reg);
                                let res = self.unchecked_car(value);

                                self.clone_value(res);

                                /*
                                let reg = self.register_index(reg);
                                let res = self
                                    .call_function_returns_value_args("car-reg-unchecked", &[reg]);
                                */

                                self.push(res, InferredType::Any);
                                self.ip += 2;
                            } else {
                                // If its a non empty list, the next time we use it, we can skip bounds
                                // checks since we know that it has a cdr.
                                // self.properties.insert(
                                //     ValueOrRegister::Register(reg),
                                //     Properties::NonEmptyList,
                                // );

                                self.shadow_stack.pop();
                                let reg = self.register_index(reg);
                                let res = self.call_function_returns_value_args("car-reg", &[reg]);
                                self.push(res, InferredType::Any);
                                self.ip += 2;
                            }

                            self.check_deopt();
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

                    self.func_ret_val(op, 2, 2, InferredType::Any);
                }
                OpCode::UNBOX => {
                    let last = self.shadow_stack.last().copied().unwrap();

                    self.shadow_mark_local_type_from_var(last, InferredType::Box);

                    match last {
                        MaybeStackValue::MutRegister(i) | MaybeStackValue::Register(i) => {
                            self.shadow_stack.pop();
                            let value = self.read_from_vm_stack(i);
                            let res = self.unbox_value_checked_register(value);

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
                    let last_ref = self.shadow_stack.last().and_then(|x| x.as_value());

                    if last_ref.map(|x| x.inferred_type) == Some(InferredType::UnboxedBool) {
                        let test = last_ref.unwrap().value;
                        // let test = self.builder.ins().uextend(types::I64, test);
                        self.shadow_stack.pop();
                        // let value = self.builder.ins().icmp_imm(IntCC::Equal, test, 0);

                        let value = self.builder.ins().bxor_imm(test, 1);

                        self.push(value, InferredType::UnboxedBool);
                        self.ip += 2;
                    } else if last_ref.map(|x| x.inferred_type) == Some(InferredType::Bool) {
                        let (test, _) = self.shadow_pop();
                        // If this matches SteelVal::BoolV(false)
                        // exactly, then we're done.
                        let payload = self.unbox_value(test);
                        let test_condition = self.builder.ins().ireduce(types::I8, payload);
                        let comparison =
                            self.builder.ins().icmp_imm(IntCC::Equal, test_condition, 0);

                        let res = self.builder.ins().uextend(types::I64, comparison);
                        // let boolean =
                        //     self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
                        self.push(res, InferredType::UnboxedBool);
                        self.ip += 2;
                    } else {
                        let (test, _) = self.shadow_pop();
                        // If this matches SteelVal::BoolV(false)
                        // exactly, then we're done.

                        let is_bool = self.is_type(test, SteelVal::BOOL_TAG);
                        let payload = self.unbox_value(test);
                        let test_condition = self.builder.ins().ireduce(types::I8, payload);
                        let comparison =
                            self.builder.ins().icmp_imm(IntCC::Equal, test_condition, 0);

                        let comparison = self.builder.ins().band(comparison, is_bool);
                        let res = self.builder.ins().uextend(types::I64, comparison);

                        // Make sure to drop this before we're done in the event its not
                        // a boolean.
                        self.drop_tagged_value(test);

                        self.push(res, InferredType::UnboxedBool);
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
                            let index = self.register_index(i);

                            // Pop them off
                            self.shadow_stack.pop();
                            self.shadow_stack.pop();

                            let res = self.call_function_returns_value_args(
                                "vector-ref-reg-2",
                                &[vector, index],
                            );

                            self.push(res, InferredType::Any);
                            self.ip += 2;
                        }
                        &[MaybeStackValue::MutRegister(v) | MaybeStackValue::Register(v), MaybeStackValue::Value(_)] =>
                        {
                            let index = self.shadow_pop();
                            let vector = self.register_index_small(v);
                            self.shadow_stack.pop();

                            let res = self.call_function_returns_value_args(
                                "vector-ref-reg-1",
                                &[vector, index.0],
                            );

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

        self.depth -= 1;

        return true;
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
            // @matt: 1/3/26
            // TODO: There is a bug with this function!
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
        let length = self
            .builder
            .ins()
            .load(types::I32, MemFlags::new(), pointer_value, 16);

        let is_empty = self.builder.ins().icmp_imm(IntCC::Equal, length, 0);

        self.builder.ins().jump(merge_block, &[is_empty]);

        self.builder.switch_to_block(not_pair_block);
        self.builder.seal_block(not_pair_block);

        let false_value = self.builder.ins().iconst(types::I8, 0);
        self.builder.ins().jump(merge_block, &[false_value]);

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
        self.push(result, InferredType::Char);
        self.ip += 1;

        // Don't need to check deopt on predicates
    }

    fn drop_tagged_value(&mut self, value: Value) {
        let tag = self.get_tag(value);

        let mask = self
            .builder
            .ins()
            .iconst(types::I64, SteelVal::UNBOXED_MASK as i64);
        let shifted = self.builder.ins().ushr(mask, tag);
        let is_unboxed = self.builder.ins().band_imm(shifted, 1);

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
        let is_standard_rc = self.builder.ins().band_imm(std_shifted, 1);

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

    fn check_value_tl(&mut self, value: Value) -> Value {
        let thread_id = self.get_thread_id();
        let obj_thread_id =
            self.builder
                .ins()
                .load(Type::int(64).unwrap(), MemFlags::new(), value, 0);

        // Now, we're going to check if the value is local to this thread:
        let is_thread_local = self
            .builder
            .ins()
            .icmp(IntCC::Equal, thread_id, obj_thread_id);

        is_thread_local
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
            let local_count =
                self.builder
                    .ins()
                    .load(Type::int(32).unwrap(), MemFlags::new(), value, 8);

            let sub_one = self.builder.ins().iadd_imm(local_count, -1);

            self.builder.ins().store(MemFlags::new(), sub_one, value, 8);

            let yes_drop = self.builder.create_block();
            let merge_block = self.builder.create_block();

            let should_continue = self
                .builder
                .ins()
                .icmp_imm(IntCC::SignedGreaterThan, sub_one, 0);

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
            let local_count =
                self.builder
                    .ins()
                    .load(Type::int(32).unwrap(), MemFlags::new(), value, 8);

            // let one = self.builder.ins().iconst(Type::int(32).unwrap(), 1);

            let sub_one = self.builder.ins().iadd_imm(local_count, -1);

            self.builder.ins().store(MemFlags::new(), sub_one, value, 8);

            let yes_drop = self.builder.create_block();
            let merge_block = self.builder.create_block();

            // let updated_count =
            //     self.builder
            //         .ins()
            //         .load(Type::int(32).unwrap(), MemFlags::new(), value, 8);

            // Then we need to check if its greater than 0:

            let should_continue = self
                .builder
                .ins()
                .icmp_imm(IntCC::SignedGreaterThan, sub_one, 0);

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

    // First, check the tag:
    fn unchecked_car(&mut self, value: Value) -> Value {
        // let is_list = self.is_type(value, SteelVal::LIST_TAG);
        let value = self.unbox_value_to_pointer(value);

        // Lets figure out where car is:
        let index = self
            .builder
            .ins()
            .load(types::I32, MemFlags::trusted(), value, 16);

        // Thats the index:
        let shared_vector_ptr =
            self.builder
                .ins()
                .load(types::I64, MemFlags::trusted(), value, 16 + 8 as i32);

        let index = self.builder.ins().uextend(types::I64, index);

        // Now get car:
        // TODO:
        // Header size is just gonna be 16
        let header_size = SteelList::<SteelVal>::vector_header_size();

        // self.elements.get(self.index as usize - 1)

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        // Okay so this might be wrong: But if we have the value at a certain
        // location, then we'll need to find the offset of everything directly.
        //
        // The header size is
        let real_slot = self.builder.ins().iadd_imm(index, -1);

        let offset = self.builder.ins().imul_imm(real_slot, size);
        let slot_ptr = self.builder.ins().iadd(shared_vector_ptr, offset);
        let slot_ptr = self.builder.ins().iadd_imm(slot_ptr, header_size as i64);

        let local_value = self
            .builder
            .ins()
            .load(types::I128, MemFlags::trusted(), slot_ptr, 0);

        local_value
    }

    fn is_type(&mut self, value: Value, check_tag: u8) -> Value {
        let tag = self.get_tag(value);
        self.builder
            .ins()
            .icmp_imm(IntCC::Equal, tag, check_tag as i64)
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
        self.push(result, InferredType::Char);
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

        if payload < self.arity as _ {
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
            let value = self.read_from_vm_stack(payload);
            self.clone_value(value);

            MaybeStackValue::Value(StackValue {
                value,
                inferred_type: InferredType::Any,
                spilled: false,
            })
        }
    }

    fn call_global_impl(&mut self, payload: usize) {
        // First - find the index that we have to lookup.
        let function_index = payload;
        self.ip += 1;
        let arity = self.instructions[self.ip].payload_size.to_usize();

        let name = CallGlobalFunctionDefinitions::arity_to_name(arity);

        let maybe_global = self._globals.get(function_index).cloned();
        if let Some(maybe_global) = maybe_global {
            if let Some(spec) = create_struct_spec(maybe_global) {
                // TODO: This is where we inline the calls for struct
                // functions
                if let Some((value, typ)) =
                    self.inline_struct_call_no_drop(spec, arity, function_index)
                {
                    self.push(value, typ);
                    return;
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

            self.push(v, InferredType::Any)
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
        // for i in 0..self.stack.len() {
        //     self.spill(i);
        // }
        for i in 0..self.shadow_stack.len() {
            self.shadow_spill(i);
        }

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
        let name = CallSelfTailCallNoArityLoopDefinitions::arity_to_name(payload).unwrap();

        // let mut args_off_the_stack = self
        //     .stack
        //     .drain(self.stack.len() - payload..)
        //     .collect::<Vec<_>>();

        // self.maybe_patch_from_stack(&mut args_off_the_stack);
        // self.shadow_stack.get(self.shadow_stack.len() - payload..);

        if payload > 0 {
            let mut amount_dropped = 0;

            while let Some(last) = self.shadow_stack.last().copied() {
                match last {
                    MaybeStackValue::Value(_) => break,
                    MaybeStackValue::Constant(_) => break,
                    MaybeStackValue::MutRegister(r) => {
                        if r == (payload - amount_dropped - 1) {
                            self.shadow_stack.pop();
                            amount_dropped += 1;
                        } else {
                            break;
                        }
                    }
                    MaybeStackValue::Register(r) => {
                        if r == (payload - amount_dropped - 1) {
                            self.shadow_stack.pop();
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

                // This is what we have left: so this will be the first n elements left
                let args_off_the_stack = self.split_off(payload);

                let args = args_off_the_stack
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                self.inline_call_self_tail_call_no_arity_loop(original_payload as _, &args);

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

        const USE_INLINE_TAIL_CALL: bool = true;

        if USE_INLINE_TAIL_CALL {
            let args = args_off_the_stack
                .into_iter()
                .map(|x| x.0)
                .collect::<Vec<_>>();

            self.inline_call_self_tail_call_no_arity_loop(payload as _, &args);
        } else {
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

        let test = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

        let else_block = self.builder.create_block();
        // let merge_block = self.builder.create_block();

        // Set up a while loop based on the result of the call.
        // It is always going to succeed, so we'll just have an if else
        // and see if that helps us figure out the proper SSA construction.

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

    // Make the call:
    fn inline_call_self_tail_call_no_arity_loop(&mut self, arity: i64, args: &[Value]) {
        let vm_ctx = self.get_ctx();
        // Read from VM stack, write back, drop value.
        // then, truncate
        for (i, arg) in args.iter().enumerate() {
            self.write_to_vm_stack(i as _, *arg);
        }

        let index = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            vm_ctx,
            offset_of!(VmCore, sp) as i32,
        );

        let index = self.builder.ins().iadd_imm(index, arity);

        self.truncate_stack(vm_ctx, index, None);
    }

    fn call_function(&mut self, arity: usize, name: &str, tail: bool) -> Value {
        // let sig = self.get_signature(name);

        // let callee = self
        //     .module
        //     .declare_function(&name, Linkage::Import, &sig)
        //     .expect("problem declaring function");
        // let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

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

    fn call_self_function_experimental(
        &mut self,
        arity: usize,
        name: &str,
        func: Gc<ByteCodeLambda>,
    ) -> Value {
        // let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();
        let id = func.id;

        /* TODO: Add this back!!
        let _ = steel_rc::BiasedRc::into_raw(func.body_exp.clone());
        */

        let instr_fat_ptr = func.body_exp();
        let instr_fat_ptr = self.create_i128(unsafe { std::mem::transmute(instr_fat_ptr) });

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

        self.inline_call_global_function(
            id,
            lookup_index,
            fallback_ip,
            &args_off_the_stack,
            instr_fat_ptr,
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
    fn inline_global_tail_call(
        &mut self,
        function_index: usize,
        arity: usize,
        func: Gc<ByteCodeLambda>,
    ) {
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

        let instr_fat_ptr = func.body_exp();
        let instr_fat_ptr = self.create_i128(unsafe { std::mem::transmute(instr_fat_ptr) });

        func.clone().into_raw();

        // Horrendous crimes, but we'll allow it. We'll also leak the instructions...
        let lookup_index = self.builder.ins().iconst(Type::int(64).unwrap(), unsafe {
            std::mem::transmute::<Gc<ByteCodeLambda>, i64>(func)
        });

        // Not sure if we're gonna need this?
        let fallback_ip = self.ip;

        self.ip += 1;

        self.increment_ref_count_closure(lookup_index);

        // Pass this through
        self.update_last_stackframe(vm_ctx, instr_fat_ptr, lookup_index);

        let offset = self.read_last_sp(vm_ctx, None);

        // Then, truncate the stack back to where we were before:
        self.truncate_stack(vm_ctx, offset, None);

        for arg in args {
            self.push_to_vm_stack(arg);
        }

        // Implement the body of `new_handle_tail_call_closure` here

        // TODO: Adjust the stack frame!
        let jit_func = self.get_jit_func(id);

        // New args now that we've spilled everything
        let args = [vm_ctx];

        let zero = self.builder.ins().iconst(types::I64, 0);

        self.builder.ins().store(
            MemFlags::trusted(),
            zero,
            vm_ctx,
            offset_of!(VmCore, ip) as i32,
        );

        self.builder.ins().return_call(jit_func, &args);
        let cold_block = self.builder.create_block();
        self.builder.switch_to_block(cold_block);
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
        let ctx = self.get_ctx();
        let thread_id =
            self.builder
                .ins()
                .load(Type::int(64).unwrap(), MemFlags::trusted(), ctx, 8);

        thread_id
    }

    fn check_deopt_ptr_load(&mut self) -> Value {
        let ctx = self.get_ctx();
        let is_native = self
            .builder
            .ins()
            .load(Type::int(8).unwrap(), MemFlags::trusted(), ctx, 0);

        is_native
    }

    fn check_deopt(&mut self) {
        let result = self.check_deopt_ptr_load();

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(result, then_block, &[], else_block, &[]);

        // Emit the return for the else block
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        self.builder.ins().return_(&[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        // todo!()
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

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);
        self.ip += ip_inc;
    }

    fn shadow_spill(&mut self, index: usize) -> Option<()> {
        // assert!(!self.cloned_stack);
        let guard = self.shadow_stack.get_mut(index)?;
        let mut spilled = false;
        match guard {
            MaybeStackValue::Value(stack_value) => {
                if !stack_value.spilled {
                    stack_value.spilled = true;
                    spilled = true;
                }
            }
            MaybeStackValue::MutRegister(p) => {
                let p = *p;
                let (value, _) = self.mut_register_to_value(p);
                spilled = true;

                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type: InferredType::Any,
                    spilled: true,
                });
            }
            MaybeStackValue::Register(p) => {
                let p = *p;
                let (value, _) = self.immutable_register_to_value(p);
                spilled = true;

                self.shadow_stack[index] = MaybeStackValue::Value(StackValue {
                    value,
                    inferred_type: InferredType::Any,
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
            let value = self.shadow_stack[index].into_value();
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
        self.shadow_stack.push(MaybeStackValue::Value(StackValue {
            value,
            inferred_type: typ,
            spilled: false,
        }))
    }

    fn shadow_push(&mut self, last: MaybeStackValue) {
        self.shadow_stack.push(last);
    }

    fn spill_stack(&mut self) {
        for arg in 0..self.shadow_stack.len() {
            self.shadow_spill(arg);
        }
    }

    fn spill_cloned_stack(&mut self) {
        // println!("Spilling cloned stack: {:?}", self.shadow_stack);

        // assert!(!self.cloned_stack);
        // self.cloned_stack = true;
        for value in self.shadow_stack.clone() {
            match value {
                MaybeStackValue::Value(stack_value) => {
                    if !stack_value.spilled {
                        let steelval = stack_value.as_steelval(self);
                        self.push_to_vm_stack(steelval);
                    }
                }
                MaybeStackValue::MutRegister(p) => {
                    // Read the value:
                    let (value, _) = self.mut_register_to_value(p);
                    self.push_to_vm_stack(value);
                }
                MaybeStackValue::Register(p) => {
                    let (value, _) = self.immutable_register_to_value(p);
                    self.push_to_vm_stack(value);
                }
                MaybeStackValue::Constant(c) => {
                    let (value, _) = c.to_value(self);
                    self.push_to_vm_stack(value);
                }
            }
        }
    }

    fn shadow_pop(&mut self) -> (Value, InferredType) {
        let last = self.shadow_stack.pop().unwrap();

        match last {
            MaybeStackValue::Value(last) => {
                assert!(!last.spilled);

                self.value_to_local_map.remove(&last.value);
                (last.as_steelval(self), last.inferred_type)
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
        (value, InferredType::Any)
    }

    // This is a little nicer, although it isn't amazing. Extra clones for no reason.
    //
    // For mutable registers, we can probably brand the values based on what operations
    // are performed on them.
    fn immutable_register_to_value(&mut self, p: usize) -> (Value, InferredType) {
        let value = self.read_from_vm_stack(p);

        // Increment the ref count for the value:
        self.clone_value(value);

        (value, InferredType::Any)
    }

    fn maybe_shadow_pop(&mut self) -> Option<(Value, InferredType)> {
        let last = self.shadow_stack.pop()?;

        Some(match last {
            MaybeStackValue::Value(last) => {
                // What is going on here?
                if last.spilled && self.ip > self.instructions.len() {
                    // dbg!(&self.shadow_stack);
                    // dbg!(self.ip);
                    // dbg!(self.instructions.len());
                    // pretty_print_dense_instructions(&self.instructions);
                    return None;
                }

                assert!(!last.spilled);

                // Pop it off the stack?
                // self.pop_value_from_vm_stack();

                self.value_to_local_map.remove(&last.value);
                (last.as_steelval(self), last.inferred_type)
            }

            // TODO: @matt specialize these for readlocal 0, 1, 2, etc.
            MaybeStackValue::MutRegister(p) => self.mut_register_to_value(p),
            MaybeStackValue::Register(p) => self.immutable_register_to_value(p),
            MaybeStackValue::Constant(c) => c.to_value(self),
        })
    }

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

            args_off_the_stack[*idx] = StackValue {
                value,
                inferred_type: InferredType::Any,
                spilled: false,
            };
        }
    }

    // TODO: This can be done to spill to the stack for argument calling
    //
    // In the event of single arg, double arg, etc, we can keep the values on the stack.
    //
    // Dispatch via the usual mechanism to see if this gets the job done
    fn shadow_maybe_patch_from_stack(&mut self, args_off_the_stack: &mut Vec<MaybeStackValue>) {
        let mut indices_to_get_from_shadow_stack = Vec::new();

        for (idx, arg) in args_off_the_stack.iter().enumerate() {
            if let MaybeStackValue::Value(arg) = arg {
                if arg.spilled {
                    indices_to_get_from_shadow_stack.push(idx);
                }
            }
        }

        for idx in indices_to_get_from_shadow_stack.iter().rev() {
            let value = self.pop_value_from_vm_stack();

            args_off_the_stack[*idx] = MaybeStackValue::Value(StackValue {
                value,
                inferred_type: InferredType::Any,
                spilled: false,
            });
        }
    }

    fn split_off_reg(&mut self, payload: usize) -> Vec<Value> {
        let mut args = self
            .shadow_stack
            .split_off(self.shadow_stack.len() - payload);

        // Patch the args if needed
        for arg in &args {
            if let MaybeStackValue::Value(v) = arg {
                self.value_to_local_map.remove(&v.value);
            }
        }

        self.shadow_maybe_patch_from_stack(&mut args);

        // dbg!(&args);

        args.into_iter()
            .map(|x| match x {
                MaybeStackValue::Value(stack_value) => stack_value.as_steelval(self),
                MaybeStackValue::MutRegister(p) => {
                    self.builder.ins().iconst(Type::int(64).unwrap(), p as i64)
                }
                MaybeStackValue::Register(p) => {
                    self.builder.ins().iconst(Type::int(64).unwrap(), p as i64)
                }
                MaybeStackValue::Constant(constant_value) => constant_value.to_value(self).0,
            })
            .collect()
    }

    fn split_off(&mut self, payload: usize) -> Vec<(Value, InferredType)> {
        let mut args = self
            .shadow_stack
            .split_off(self.shadow_stack.len() - payload);

        args = args
            .into_iter()
            .map(|x| match x {
                MaybeStackValue::Value(stack_value) => MaybeStackValue::Value(stack_value),
                MaybeStackValue::MutRegister(p) => {
                    let (value, _) = self.mut_register_to_value(p);
                    MaybeStackValue::Value(StackValue {
                        value,
                        inferred_type: InferredType::Any,
                        spilled: false,
                    })
                }
                MaybeStackValue::Register(p) => {
                    let (value, _) = self.immutable_register_to_value(p);
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

        self.maybe_patch_from_stack(&mut args);

        args.into_iter()
            .map(|x| (x.as_steelval(self), x.inferred_type))
            .collect()
    }

    fn func_ret_val(
        &mut self,
        op: OpCode,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
    ) {
        let function_name = op_to_name_payload(op, payload);

        // dbg!(function_name);
        // dbg!(&self.shadow_stack);

        let args = self.split_off(payload);

        // dbg!(&args);
        // dbg!(&self.shadow_stack);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

        let result = self.call_function_returns_value_args(function_name, &args);

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);

        self.ip += ip_inc;
    }

    fn get_const(&mut self, op1: OpCode, payload: usize) -> (Value, InferredType) {
        match op1 {
            OpCode::LOADINT0 => (
                // self.create_i128(encode(SteelVal::INT_ZERO)),
                self.encode_integer(0),
                InferredType::Int,
            ),
            OpCode::LOADINT1 => (
                // self.create_i128(encode(SteelVal::INT_ONE)),
                self.encode_integer(1),
                InferredType::Int,
            ),
            OpCode::LOADINT2 => (
                // self.create_i128(encode(SteelVal::INT_TWO)),
                self.encode_integer(2),
                InferredType::Int,
            ),
            OpCode::PUSHCONST => {
                // Attempt to inline the constant, if it is something that can be inlined.
                // Assuming we know the type of it, we can get really fancy here since
                // we _should_ be able to do something with it if there are other types
                // in play - we can avoid the unboxing / boxing of the type if we know
                // what we're dealing with.

                let constant = self.constants.get_value(payload);

                match &constant {
                    // SteelVal::BoolV(_) => (self.create_i128(encode(constant)), InferredType::Bool),
                    // SteelVal::IntV(_) => (self.create_i128(encode(constant)), InferredType::Int),
                    // SteelVal::CharV(_) => (self.create_i128(encode(constant)), InferredType::Char),
                    _ => (self.push_const_index(payload), InferredType::Any),
                }
            }
            _ => (
                self.call_function_returns_value(op_to_name_payload(op1, payload)),
                InferredType::Any,
            ),
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
        integer
    }

    // Remove the tag from a value to get to the underlying type
    fn unbox_value(&mut self, value: Value) -> Value {
        let amount_to_shift = self.builder.ins().iconst(types::I64, 64);
        let encoded_rhs = self.builder.ins().sshr(value, amount_to_shift);
        encoded_rhs
    }

    fn unbox_value_to_pointer(&mut self, value: Value) -> Value {
        let amount_to_shift = self.builder.ins().iconst(types::I64, 64);
        let encoded_rhs = self.builder.ins().sshr(value, amount_to_shift);
        self.builder.ins().ireduce(types::I64, encoded_rhs)
    }

    fn get_tag(&mut self, value: Value) -> Value {
        // Split the value into two:
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

        let then_return = if then_out_of_bounds {
            // BlockArg::Value(self.create_i128(encode(SteelVal::IntV(12345))))
            // self.create_i128(encode(SteelVal::IntV(12345)))

            self.encode_integer(12345)
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
            self.shadow_pop().0
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

        // println!("---------- else done ----------");

        // if let Some(else_offset) = else_offset {
        //     assert_eq!(self.ip, else_offset)
        // }

        // println!(
        //     "ip after else: {} - instructions length: {}",
        //     self.ip,
        //     self.instructions.len()
        // );

        assert_eq!(self.depth, depth);

        let else_out_of_bounds = self.ip > self.instructions.len();

        // dbg!(then_out_of_bounds);
        // dbg!(else_out_of_bounds);
        // dbg!(self.ip);
        // dbg!(self.instructions.len());

        // Returned, therefore we don't need to do anything.
        let else_return = if else_out_of_bounds {
            // BlockArg::Value(self.create_i128(encode(SteelVal::IntV(12345))))

            self.encode_integer(12345)

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
            self.shadow_pop().0
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

                self.push(phi, InferredType::Any);

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

                self.push(phi, InferredType::Any);

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

                self.if_bound = last_bound;

                assert_eq!(self.ip, else_offset.unwrap());

                self.ip = else_offset.unwrap();

                let phi = self.builder.block_params(merge_block)[0];

                self.push(phi, InferredType::Any);

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

        phi
    }

    fn check_else_properties(&mut self, condition_value: Value) {
        self.properties.infer_property_bool(condition_value, false);
    }

    fn check_then_properties(&mut self, condition_value: Value) {
        self.properties.infer_property_bool(condition_value, false);
    }

    fn vm_pop(&mut self, value: Value) {
        self.inline_handle_pop(value);
    }

    fn push_to_vm_stack(&mut self, value: Value) {
        self.push_to_vm_stack_let_var_new(value);
    }

    // Note: This value should _not_ be dropped!
    fn read_from_vm_stack(&mut self, index: usize) -> Value {
        let stack_pointer_offset = offset_of!(VmCore, sp);
        // Lets do the thing?

        let ctx = self.get_ctx();
        let sp = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            stack_pointer_offset as i32,
        );

        let thread_offset = offset_of!(VmCore, thread);
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        let local_offset = self.builder.ins().iadd_imm(sp, index as i64);
        let offset = self.builder.ins().imul_imm(local_offset, size);

        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let local_value = self
            .builder
            .ins()
            .load(types::I128, MemFlags::trusted(), slot_ptr, 0);

        local_value
    }

    fn remove_from_vm_stack(&mut self, index: usize) -> Value {
        let stack_pointer_offset = offset_of!(VmCore, sp);
        // Lets do the thing?

        let ctx = self.get_ctx();
        let sp = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            stack_pointer_offset as i32,
        );

        let thread_offset = offset_of!(VmCore, thread);
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        let local_offset = self.builder.ins().iadd_imm(sp, index as i64);
        let offset = self.builder.ins().imul_imm(local_offset, size);

        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let local_value = self
            .builder
            .ins()
            .load(types::I128, MemFlags::trusted(), slot_ptr, 0);

        let value = self.encode_void();

        self.builder
            .ins()
            .store(MemFlags::trusted(), value, slot_ptr, 0);

        local_value
    }

    // TODO: Should flatten these ops when calling this multiple times!
    fn write_to_vm_stack(&mut self, index: usize, value: Value) {
        let stack_pointer_offset = offset_of!(VmCore, sp);
        // Lets do the thing?

        let ctx = self.get_ctx();
        let sp = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            stack_pointer_offset as i32,
        );

        let thread_offset = offset_of!(VmCore, thread);
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        let local_offset = self.builder.ins().iadd_imm(sp, index as i64);
        let offset = self.builder.ins().imul_imm(local_offset, size);

        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let local_value = self
            .builder
            .ins()
            .load(types::I128, MemFlags::trusted(), slot_ptr, 0);

        self.drop_tagged_value(local_value);

        self.builder
            .ins()
            .store(MemFlags::trusted(), value, slot_ptr, 0);
    }

    // ctx.thread.stack_frames.len() < 100
    fn check_should_trampoline(&mut self, ctx: Value) -> Value {
        let thread_offset = offset_of!(VmCore, thread);

        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            thread_offset as i32,
        );

        // Stack frame offset:
        let stack_frame_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_frame_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_frame_offset + len_offset) as i32,
        );

        self.builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, stack_frame_length, 100)
    }

    fn inline_call_global_function(
        &mut self,
        id: u32,
        func: Value,
        fallback_ip: usize,
        args: &[Value],
        instr_fat_ptr: Value,
    ) -> Value {
        let vm_ctx = self.get_ctx();
        let should_trampoline = self.check_should_trampoline(vm_ctx);

        let should_yield = self.builder.ins().bxor_imm(should_trampoline, 1);

        self.update_ip_native_if_yield(vm_ctx, should_yield, fallback_ip + 1);

        let typ = self.int;
        let arity = args.len();

        // Push each value on to the stack:
        for arg in args {
            self.push_to_vm_stack_let_var_new(*arg);
        }

        self.converging_if(
            should_trampoline,
            |ctx| {
                // Increment the ref count before we go in
                ctx.increment_ref_count_closure(func);

                // TODO: Consider moving this up before things are pushed on in order
                // to capture the stack length without needing to compute the value
                ctx.push_stack_frame(arity as _, func, instr_fat_ptr, fallback_ip);
                let jit_func = ctx.get_jit_func(id);
                ctx.builder.ins().call(jit_func, &[vm_ctx]);

                // Check if native still:
                let is_still_native = ctx.builder.ins().load(
                    types::I8,
                    MemFlags::trusted(),
                    vm_ctx,
                    offset_of!(VmCore, is_native) as i32,
                );

                ctx.converging_if(
                    is_still_native,
                    |ctx| ctx.inline_pop_from_stack(vm_ctx),
                    |ctx| ctx.encode_void(),
                    typ,
                )
            },
            |ctx| ctx.call_function_returns_value_args("#%setup-closure", &[func]),
            typ,
        )
    }

    fn inline_let_end_scope(&mut self, amt: usize, count: usize) {
        let vm_ctx = self.get_ctx();
        let offset = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            vm_ctx,
            offset_of!(VmCore, sp) as i32,
        );

        let beginning_scope = self.builder.ins().iconst(types::I64, amt as i64);

        let rollback_index = self.builder.ins().iadd(beginning_scope, offset);

        self.truncate_stack(vm_ctx, rollback_index, Some(count as _));
    }

    fn truncate_stack(&mut self, vm_ctx: Value, index: Value, count: Option<i32>) {
        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            vm_ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // Current stack length:
        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // let difference = self.builder.ins().isub(stack_length, index);
        // let new_length = self.builder.ins().iconst(types::, N)

        self.builder.ins().store(
            MemFlags::trusted(),
            index,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;

        // Use the calculated difference if thats what we have to do
        let count = count
            .map(|count| self.builder.ins().iconst(types::I64, count as i64))
            .unwrap_or_else(|| self.builder.ins().isub(stack_length, index));
        {
            let loop_header = self.builder.create_block();
            let loop_body = self.builder.create_block();
            let loop_exit = self.builder.create_block();

            self.builder.append_block_param(loop_header, types::I64); // i

            let zero = self.builder.ins().iconst(types::I64, 0);
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
                .imul_imm(slot_index, size_of::<SteelVal>() as i64);
            let slot_ptr = self.builder.ins().iadd(buf_ptr, byte_offset);
            let val = self
                .builder
                .ins()
                .load(types::I128, MemFlags::trusted(), slot_ptr, 0);
            self.drop_tagged_value(val);

            let i_next = self.builder.ins().iadd_imm(i, 1);
            self.builder.ins().jump(loop_header, &[i_next]);

            self.builder.seal_block(loop_header);

            self.builder.switch_to_block(loop_exit);
            self.builder.seal_block(loop_exit);
        }
    }

    fn inline_pop_from_stack(&mut self, vm_ctx: Value) -> Value {
        // self.call_function_no_return("#%debug-stack-before");

        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            vm_ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let new_length = self.builder.ins().iadd_imm(stack_length, -1);

        self.builder.ins().store(
            MemFlags::trusted(),
            new_length,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<SteelVal>() as _;
        let offset = self.builder.ins().imul_imm(new_length, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let res = self
            .builder
            .ins()
            .load(types::I128, MemFlags::trusted(), slot_ptr, 0);

        // self.call_function_no_return("#%debug-stack-after");

        res
    }

    // Subtract one from the pop count, returns
    // the new pop count
    fn sub_one_pop_count(&mut self, vm_ctx: Value) -> Value {
        let current_pop_count = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            vm_ctx,
            offset_of!(VmCore, pop_count) as i32,
        );
        let sub_one = self.builder.ins().iadd_imm(current_pop_count, -1);
        self.builder.ins().store(
            MemFlags::trusted(),
            sub_one,
            vm_ctx,
            offset_of!(VmCore, pop_count) as i32,
        );
        sub_one
    }

    fn inline_handle_pop(&mut self, value: Value) {
        let vm_ctx = self.get_ctx();
        let new_pop_count = self.sub_one_pop_count(vm_ctx);
        let should_continue = self.pop_count_should_continue(new_pop_count);

        // TODO: Figure out what the return type is! We don't really need
        // to return anything. The result should just be assigned to the result
        // on the VM context; In the else case, we assign the result
        // on to the VM value. In the former case, we don't need to do anything.
        self.converging_if_no_value(
            should_continue,
            |ctx| {
                // This is the frame itself. We'll need to call drop
                // on it properly -> specifically on the function
                // and possibly the attachments, depending on whether
                // its null or not. If it is, we'll want to pass the frame
                // by value in, call drop on it, and close the things as needed
                // in order for drop to get called.
                let (popped_frame, fat_ptr) = ctx.inline_pop_from_stack_frames(vm_ctx);

                // first, we'll check if the attachments isn't null.
                // And then, we'll invoke drop on the frame for the attachments.

                let attachment_exists =
                    ctx.builder
                        .ins()
                        .icmp_imm(IntCC::NotEqual, popped_frame.attachments, 0);

                ctx.converging_if_no_else_no_value(
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
                        ctx.push_to_vm_stack(value);

                        let ip = ctx.builder.ins().uextend(types::I64, popped_frame.ip);
                        ctx.builder.ins().store(
                            MemFlags::trusted(),
                            ip,
                            vm_ctx,
                            offset_of!(VmCore, ip) as i32,
                        );

                        ctx.builder.ins().store(
                            MemFlags::trusted(),
                            popped_frame.instructions,
                            vm_ctx,
                            offset_of!(VmCore, instructions) as i32,
                        );

                        // let sp = ctx.read_last_sp(vm_ctx, Some(fat_ptr));
                        let sp = ctx.read_last_sp(vm_ctx, None);

                        // TODO: @Matt
                        // Call drop on the function here!
                        // We don't need to totally encode the function here like this
                        // let func =
                        //     ctx.encode_value(SteelVal::CLOSURE_TAG as _, popped_frame.function);
                        ctx.drop_biased_rc_unboxed_closure(popped_frame.function);

                        ctx.builder.ins().store(
                            MemFlags::trusted(),
                            sp,
                            vm_ctx,
                            offset_of!(VmCore, sp) as i32,
                        );
                    },
                );
            },
            |ctx| ctx.call_function_args_no_return("#%pop-slow-path-finish", &[value]),
        );
    }

    // Whether we should continue running:
    fn pop_count_should_continue(&mut self, pop_count: Value) -> Value {
        self.builder.ins().icmp_imm(IntCC::NotEqual, pop_count, 0)
    }

    fn update_last_stackframe(&mut self, vm_ctx: Value, fat_ptr: Value, function: Value) -> Value {
        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            vm_ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // Last thing
        let new_length = self.builder.ins().iadd_imm(stack_length, -1);

        let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<StackFrame>() as _;
        let offset = self.builder.ins().imul_imm(new_length, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        // Load the stack frame. We're going to use this later.
        let value = self.builder.ins().load(
            types::I32,
            MemFlags::trusted(),
            slot_ptr,
            offset_of!(StackFrame, sp) as i32,
        );

        //
        let old_function = self.builder.ins().load(
            types::I64,
            MemFlags::trusted(),
            slot_ptr,
            offset_of!(StackFrame, function) as i32,
        );

        self.drop_biased_rc_unboxed_closure(old_function);

        self.builder.ins().store(
            MemFlags::trusted(),
            fat_ptr,
            slot_ptr,
            offset_of!(StackFrame, instructions) as i32,
        );
        self.builder.ins().store(
            MemFlags::trusted(),
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
    fn read_last_sp(&mut self, vm_ctx: Value, fat_ptr: Option<(Value, Value)>) -> Value {
        // let (buf_ptr, new_length) = if let Some((buf_ptr, length)) = fat_ptr {
        //     let new_length = self.builder.ins().iadd_imm(length, -1);

        //     (buf_ptr, new_length)
        // } else {
        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            vm_ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        // (buf_ptr, new_length)
        // };

        let condition = self.builder.ins().icmp_imm(IntCC::Equal, stack_length, 0);

        self.converging_if(
            condition,
            |ctx| ctx.builder.ins().iconst(types::I64, 0),
            |ctx| {
                // Last thing
                let new_length = ctx.builder.ins().iadd_imm(stack_length, -1);

                let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlags::trusted(),
                    thread_pointer,
                    (stack_offset + ptr_offset) as i32,
                );

                let size: i64 = std::mem::size_of::<StackFrame>() as _;
                let offset = ctx.builder.ins().imul_imm(new_length, size);
                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                // Load the stack frame. We're going to use this later.
                let value = ctx.builder.ins().load(
                    types::I32,
                    MemFlags::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, sp) as i32,
                );

                ctx.builder.ins().uextend(types::I64, value)
            },
            types::I64,
        )
    }

    /// Pop the last stack frame off
    fn inline_pop_from_stack_frames(&mut self, vm_ctx: Value) -> (StackFrameRepr, (Value, Value)) {
        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            vm_ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack_frames);

        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let new_length = self.builder.ins().iadd_imm(stack_length, -1);

        self.builder.ins().store(
            MemFlags::trusted(),
            new_length,
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

        let buf_ptr = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + ptr_offset) as i32,
        );

        let size: i64 = std::mem::size_of::<StackFrame>() as _;
        let offset = self.builder.ins().imul_imm(new_length, size);
        let slot_ptr = self.builder.ins().iadd(buf_ptr, offset);

        let sp_offset = (buf_ptr, new_length);

        // Load the stack frame. We're going to use this later.
        (
            StackFrameRepr {
                sp: self.builder.ins().load(
                    types::I32,
                    MemFlags::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, sp) as i32,
                ),
                ip: self.builder.ins().load(
                    types::I32,
                    MemFlags::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, ip) as i32,
                ),
                instructions: self.builder.ins().load(
                    types::I128,
                    MemFlags::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, instructions) as i32,
                ),
                function: self.builder.ins().load(
                    types::I64,
                    MemFlags::trusted(),
                    slot_ptr,
                    offset_of!(StackFrame, function) as i32,
                ),
                attachments: self.builder.ins().load(
                    types::I64,
                    MemFlags::trusted(),
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
                    MemFlags::trusted(),
                    zero,
                    vm_ctx,
                    offset_of!(VmCore, is_native) as i32,
                );
            },
            |ctx| {
                let ip = ctx.builder.ins().iconst(types::I64, fallback_ip as i64);
                ctx.builder.ins().store(
                    MemFlags::trusted(),
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
        let is_unboxed = self.builder.ins().band_imm(shifted, 1);

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
        let is_standard_rc = self.builder.ins().band_imm(std_shifted, 1);

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

    /// Clone a standard RC value
    fn clone_rc_value(&mut self, value: Value) {
        self.call_function_args_no_context("#%clone-std-rc", &[value]);
    }

    /// Clone a biased rc value
    fn clone_biased_rc(&mut self, value: Value) {
        let ptr = self.unbox_value_to_pointer(value);
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
                let local_count =
                    ctx.builder
                        .ins()
                        .load(Type::int(32).unwrap(), MemFlags::new(), value, 8);

                let add_one = ctx.builder.ins().iadd_imm(local_count, 1);

                ctx.builder.ins().store(MemFlags::new(), add_one, value, 8);
            },
            |ctx| {
                // TODO: @Matt - we can inline this as well!
                // Slow path increment the counter
                ctx.call_function_args_no_context("raw-slow-increment-closure", &[value]);
            },
        );
    }

    // Note: The function _must_ have been cloned already
    // TODO: @Matt: we need a better representation for fat pointers
    // that is custom and has a stable abi in order to properly do this.
    fn push_stack_frame(
        &mut self,
        arity: i64,
        function: Value,
        instr_fat_ptr: Value,
        fallback_ip: usize,
    ) {
        let vm_ctx = self.get_ctx();
        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            vm_ctx,
            thread_offset as i32,
        );

        // Stack frame offset:
        let stack_frame_offset = offset_of!(SteelThread, stack_frames);

        let capacity_offset = steel_vec::Vec::<StackFrame>::capacity_offset();
        let len_offset = steel_vec::Vec::<StackFrame>::len_offset();

        // println!(
        //     "Offset of stackframe instuctions: {}",
        //     offset_of!(StackFrame, instructions)
        // );
        // println!("StackFrame size: {}", std::mem::size_of::<StackFrame>());

        // This is the actual stack, steel_vec::Vec<SteelVal>
        let stack_capacity = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_frame_offset + capacity_offset) as i32,
        );

        // self.call_function_args_no_context("#%debug-value", &[stack_capacity]);

        let stack_frame_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_frame_offset + len_offset) as i32,
        );

        // Stack values, for figuring out where we have to put the sp
        let stack_offset = offset_of!(SteelThread, stack);
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();
        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + len_offset) as i32,
        );

        let at_capacity = self
            .builder
            .ins()
            .icmp(IntCC::Equal, stack_capacity, stack_frame_length);

        self.converging_if_no_else_no_value(
            at_capacity,
            // If at capacity, call grow
            |ctx| ctx.call_function_no_return("slow-grow-frame-stack"),
            |ctx| {
                // Write the value:
                let ptr_offset = steel_vec::Vec::<StackFrame>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlags::trusted(),
                    thread_pointer,
                    (stack_frame_offset + ptr_offset) as i32,
                );

                // Okay so here, now we're going to move things around
                // such that we can snag values from things
                let size: i64 = std::mem::size_of::<StackFrame>() as _;
                let offset = ctx.builder.ins().imul_imm(stack_frame_length, size);
                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                let sp_offset = offset_of!(VmCore, sp);

                let arity = -arity;

                // TODO: Load the stack length, not the old sp!
                // let old_sp = ctx.builder.ins().load(
                //     types::I64,
                //     MemFlags::trusted(),
                //     vm_ctx,
                //     sp_offset as i32,
                // );
                let new_sp = ctx.builder.ins().iadd_imm(stack_length, arity as i64);

                // ctx.call_function_no_return("#%debug-stack");
                // ctx.call_function_args_no_context("#%debug-value", &[stack_length]);
                // ctx.call_function_args_no_context("#%debug-value", &[new_sp]);

                ctx.builder
                    .ins()
                    .store(MemFlags::trusted(), new_sp, vm_ctx, sp_offset as i32);

                // Reduce it before going in the stack frame
                let new_sp = ctx.builder.ins().ireduce(types::I32, new_sp);

                // Stack pointer:
                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    new_sp,
                    slot_ptr,
                    offset_of!(StackFrame, sp) as i32,
                );

                // Instruction pointer:

                /*
                let ip = ctx.builder.ins().load(
                    types::I64,
                    MemFlags::trusted(),
                    vm_ctx,
                    offset_of!(VmCore, ip) as i32,
                );

                // Reduce to 32 bits to store in the stack frame
                let ip = ctx.builder.ins().ireduce(types::I32, ip);
                */

                let ip_plus_one = ctx
                    .builder
                    .ins()
                    .iconst(types::I32, (fallback_ip + 1) as i64);

                // ctx.call_function_args_no_context("#%debug-count", &[ip_plus_one]);
                // println!("Calculated ip here: {}", ctx.ip);

                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    ip_plus_one,
                    slot_ptr,
                    offset_of!(StackFrame, ip) as i32,
                );

                // Instructions:

                let current_instructions = ctx.builder.ins().load(
                    types::I128,
                    MemFlags::trusted(),
                    vm_ctx,
                    offset_of!(VmCore, instructions) as i32,
                );

                // ctx.call_function_args_no_context("#%debug-instructions", &[current_instructions]);

                // ctx.call_function_args_no_context("#%debug-instructions2", &[instr_fat_ptr]);

                // TODO: This doesn't work correctly; we need to
                // construct a fat pointer here. So I need something
                // that is FFI safe in order to construct a fat
                // pointer to the *const [DenseInstruction]

                // Okay, now load the instructions from the function:
                // let instructions = ctx.builder.ins().load(
                //     types::I128,
                //     MemFlags::trusted(),
                //     function,
                //     offset_of!(ByteCodeLambda, body_exp) as i32,
                // );

                let instructions = instr_fat_ptr;

                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    current_instructions,
                    slot_ptr,
                    offset_of!(StackFrame, instructions) as i32,
                );

                // Store the instructions back to the VM pointer
                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    instructions,
                    vm_ctx,
                    offset_of!(VmCore, instructions) as i32,
                );

                // Store the function itself
                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    function,
                    slot_ptr,
                    offset_of!(StackFrame, function) as i32,
                );

                let null_pointer = ctx.builder.ins().iconst(types::I64, 0);

                // Null for the attachments
                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    null_pointer,
                    slot_ptr,
                    offset_of!(StackFrame, attachments) as i32,
                );

                let old_pop_count = ctx.builder.ins().load(
                    types::I64,
                    MemFlags::trusted(),
                    vm_ctx,
                    offset_of!(VmCore, pop_count) as i32,
                );

                // Increment pop count
                let new_pop_count = ctx.builder.ins().iadd_imm(old_pop_count, 1);

                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    new_pop_count,
                    vm_ctx,
                    offset_of!(VmCore, pop_count) as i32,
                );

                // Set ip to 0
                let zero = ctx.builder.ins().iconst(types::I64, 0);
                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    zero,
                    vm_ctx,
                    offset_of!(VmCore, ip) as i32,
                );

                // Add one to the length:
                let new_length = ctx.builder.ins().iadd_imm(stack_frame_length, 1);

                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    new_length,
                    thread_pointer,
                    (stack_frame_offset + len_offset) as i32,
                );
            },
        );
    }

    // Attempt to push this to the VM stack inline:
    fn push_to_vm_stack_let_var_new(&mut self, value: Value) {
        let ctx = self.get_ctx();
        let thread_offset = offset_of!(VmCore, thread);

        // This represents the first part of the thread
        let thread_pointer = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            ctx,
            thread_offset as i32,
        );

        // Stack offset:
        let stack_offset = offset_of!(SteelThread, stack);

        let capacity_offset = steel_vec::Vec::<SteelVal>::capacity_offset();
        let len_offset = steel_vec::Vec::<SteelVal>::len_offset();

        // This is the actual stack, steel_vec::Vec<SteelVal>
        let stack_capacity = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
            thread_pointer,
            (stack_offset + capacity_offset) as i32,
        );

        let stack_length = self.builder.ins().load(
            Type::int(64).unwrap(),
            MemFlags::trusted(),
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

        self.converging_if_no_else_no_value(
            at_capacity,
            // If at capacity, call grow
            |ctx| ctx.call_function_no_return("slow-grow-stack"),
            |ctx| {
                // Write the value:
                let ptr_offset = steel_vec::Vec::<SteelVal>::buf_offset();

                let buf_ptr = ctx.builder.ins().load(
                    Type::int(64).unwrap(),
                    MemFlags::trusted(),
                    thread_pointer,
                    (stack_offset + ptr_offset) as i32,
                );

                let size: i64 = std::mem::size_of::<SteelVal>() as _;
                let offset = ctx.builder.ins().imul_imm(stack_length, size);
                let slot_ptr = ctx.builder.ins().iadd(buf_ptr, offset);

                ctx.builder
                    .ins()
                    .store(MemFlags::trusted(), value, slot_ptr, 0);

                // Add one to the length:
                let new_length = ctx.builder.ins().iadd_imm(stack_length, 1);

                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    new_length,
                    thread_pointer,
                    (stack_offset + len_offset) as i32,
                );
            },
        );
    }

    /*
    fn push_to_vm_stack_let_var(&mut self, value: Value) {
        let name = "push-to-vm-stack-let-var";

        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();
        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let _call = self.builder.ins().call(local_callee, &arg_values);
        // let result = self.builder.inst_results(call)[0];
        // result
    }
    */

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
        let call = self.builder.ins().call(local_callee, &arg_values);
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

        if cfg!(target_os = "windows") {
            sig.call_conv = CallConv::SystemV;
        }

        sig.call_conv = CallConv::Tail;

        let name = format!("{}_inner", id);

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");

        self.module.declare_func_in_func(callee, self.builder.func)
    }

    /// Fetches a pointer to the VM context
    fn get_ctx(&mut self) -> Value {
        let ptr_type = self.module.target_config().pointer_type();
        self.builder.ins().global_value(ptr_type, self.vm_context)
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

        let sig_ref = self.builder.import_signature(sig);
        sig_ref
    }
}
