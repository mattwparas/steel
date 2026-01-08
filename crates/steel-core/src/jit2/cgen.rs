#![allow(unused)]
#![allow(improper_ctypes_definitions)]
#![allow(unpredictable_function_pointer_comparisons)]

use cranelift::{
    codegen::ir::{ArgumentPurpose, FuncRef, GlobalValue, StackSlot, Type},
    prelude::{isa::CallConv, *},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use std::collections::{HashMap, VecDeque};
use std::{collections::HashSet, slice};
use steel_gen::{opcode::OPCODES_ARRAY, OpCode};

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::{pretty_print_dense_instructions, DenseInstruction},
    primitives::{
        lists::steel_pair,
        ports::{eof_objectp_jit, read_char_single_ref, steel_eof_objectp, steel_read_char},
        strings::{char_equals_binop, steel_char_equals},
        vectors::steel_mut_vec_set,
    },
    rvals::FunctionSignature,
    steel_vm::{
        primitives::steel_eq,
        vm::{
            jit::{
                _push_to_vm_stack_function_spill, box_handler_c,
                call_global_function_deopt_no_arity_spilled, call_global_function_deopt_spilled,
                callglobal_handler_deopt_c, callglobal_tail_handler_deopt_spilled, car_handler_reg,
                car_handler_reg_no_check, car_handler_value, cdr_handler_mut_reg,
                cdr_handler_mut_reg_no_check, cdr_handler_reg, cdr_handler_reg_no_check,
                cdr_handler_value, check_callable, check_callable_spill, check_callable_tail,
                check_callable_value, check_callable_value_tail, cons_handler_value, drop_value,
                eq_reg_1, eq_reg_2, eq_value, equal_binop, extern_c_add_four, extern_c_add_three,
                extern_c_add_two, extern_c_add_two_binop_register,
                extern_c_add_two_binop_register_both, extern_c_div_two, extern_c_gt_two,
                extern_c_gte_two, extern_c_lt_two, extern_c_lt_two_int, extern_c_lte_two,
                extern_c_lte_two_int, extern_c_mult_three, extern_c_mult_two, extern_c_negate,
                extern_c_null_handler, extern_c_sub_three, extern_c_sub_two, extern_c_sub_two_int,
                extern_c_sub_two_int_reg, extern_handle_pop, handle_new_start_closure,
                handle_pure_function, if_handler_raw_value, if_handler_register, if_handler_value,
                is_pair_c_reg, let_end_scope_c, list_handler_c, list_ref_handler_c,
                move_read_local_0_value_c, move_read_local_1_value_c, move_read_local_2_value_c,
                move_read_local_3_value_c, move_read_local_any_value_c, not_handler_raw_value,
                num_equal_int, num_equal_value, num_equal_value_unboxed, pop_value,
                push_const_value_c, push_const_value_index_c, push_global, push_to_vm_stack,
                push_to_vm_stack_let_var, push_to_vm_stack_two, read_captured_c,
                read_local_0_value_c, read_local_1_value_c, read_local_2_value_c,
                read_local_3_value_c, read_local_any_value_c, self_tail_call_handler,
                self_tail_call_handler_loop, set_handler_c, set_local_any_c, setbox_handler_c,
                should_spill, should_spill_value, tcojmp_handler, unbox_handler_c, vec_handler_c,
                vector_ref_handler_c, vector_ref_handler_register, vector_ref_handler_register_two,
                vector_set_handler_register_one, vector_set_handler_register_three,
                vector_set_handler_register_two, vector_set_handler_stack, CallFunctionDefinitions,
                CallFunctionTailDefinitions, CallGlobalFunctionDefinitions,
                CallGlobalNoArityFunctionDefinitions, CallGlobalTailFunctionDefinitions,
                CallPrimitiveDefinitions, CallPrimitiveFixedDefinitions,
                CallPrimitiveMutDefinitions, CallSelfTailCallNoArityDefinitions,
                CallSelfTailCallNoArityLoopDefinitions, ListHandlerDefinitions,
            },
            VmCore,
        },
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

                let return_size = std::mem::size_of::<RET>();

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

                let return_size = std::mem::size_of::<RET>();

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

                let return_size = std::mem::size_of::<RET>();

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

                let return_size = std::mem::size_of::<RET>();

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
    Type::int(std::mem::size_of::<T>() as u16 * 8).unwrap()
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
                func: steel_read_char as i64,
                arity: 1,
                shape: &[CallKind::Ref],
            },
            read_char_single_ref as i64,
        );

        map.insert(
            PrimitiveSignature {
                func: steel_char_equals as i64,
                arity: 2,
                shape: &[CallKind::Value, CallKind::Value],
            },
            char_equals_binop as i64,
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

        map.add_func(
            "pair?",
            abi! { is_pair_c_reg as fn(*mut VmCore, usize) -> SteelVal },
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

        map.add_func(
            "cdr-reg-no-check",
            abi! { cdr_handler_reg_no_check as fn(*mut VmCore, usize) -> SteelVal },
        );
        map.add_func(
            "cdr-mut-reg-no-check",
            abi! { cdr_handler_mut_reg_no_check as fn(*mut VmCore, usize) -> SteelVal },
        );

        map.add_func("cons-handler-value", cons_handler_value as VmBinOp);

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

        map.add_func_hint2(
            "sub-binop-int",
            extern_c_sub_two_int as BinOp,
            InferredType::Number,
        );

        map.add_func_hint(
            "sub-binop-int-reg",
            abi! { extern_c_sub_two_int_reg
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

// Compile the bytecode assuming that things... work okay?
unsafe fn compile_bytecode(
    jit: &mut JIT,
    name: String,
    arity: u16,
    code: &[DenseInstruction],
    globals: &[SteelVal],
    constants: &ConstantMap,
    function_index: Option<usize>,
) -> Result<fn(&mut VmCore), String> {
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
    let code_ptr = jit.compile(name, arity, code, globals, constants, function_index)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = std::mem::transmute::<*const u8, fn(&mut VmCore)>(code_ptr);
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
    ) -> Result<fn(&mut VmCore), String> {
        unsafe { compile_bytecode(self, name, arity, code, globals, constants, function_index) }
    }
}

impl JIT {
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
    ) -> Result<*const u8, String> {
        // self.ctx.set_disasm(true);

        let (params, stmts) = (Default::default(), instructions);

        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(
            name,
            id,
            arity,
            params,
            stmts,
            globals,
            constants,
            function_index,
        )?;

        if let Err(e) = cranelift::codegen::verify_function(&self.ctx.func, self.module.isa()) {
            // println!("{:#?}", self.ctx.func);
            println!("{:#?}", e);
            self.module.clear_context(&mut self.ctx);
            return Err(format!("errors: {:#?}", e));
        }

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| {
                println!("error in defining function");
                e.to_string()
            })?;

        // let asm = self.ctx.compiled_code().map(|x| x.vcode.as_ref()).flatten();
        // if let Some(asm) = asm {
        //     println!("{}", asm);
        // }

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        // The steps here are analogous to `compile`, except that data is much
        // simpler than functions.
        self.data_description.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_description)
            .map_err(|e| e.to_string())?;
        self.data_description.clear();
        self.module.finalize_definitions().unwrap();
        let buffer = self.module.get_finalized_data(id);
        // TODO: Can we move the unsafe into cranelift?
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(
        &mut self,
        _name: String,
        _func_id: FuncId,
        arity: u16,
        params: Vec<String>,
        bytecode: &[DenseInstruction],
        globals: &[SteelVal], // stmts: Vec<Expr>,
        constants: &ConstantMap,
        _function_context: Option<usize>,
    ) -> Result<(), String> {
        // println!("----- Compiling function ----");

        // pretty_print_dense_instructions(bytecode);

        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        // let int = self.module.target_config().pointer_type();

        // Upgrade to 128 bit?
        let int = Type::int(128).unwrap();

        // Set up pointer type to be the first argument.
        let pointer = self.module.target_config().pointer_type();

        let mut param = AbiParam::new(pointer);
        param.purpose = ArgumentPurpose::VMContext;

        self.ctx.func.signature.params.push(param);

        // dbg!(&self.ctx.func.signature);

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

        // let vm_context = builder.create_sized_stack_slot(StackSlotData {
        //     kind: StackSlotKind::ExplicitSlot,
        //     size: pointer.bits(),
        //     align_shift: 8,
        // });

        // let vmctx_param = builder.block_params(entry_block)[0];

        // builder.ins().stack_store(vmctx_param, vm_context, 0);

        // TODO: Scan the bytecode
        // let variables = declare_variables(int, pointer, &mut builder, &params, entry_block);

        let vm_context = builder.create_global_value(GlobalValueData::VMContext);

        let variables = Default::default();

        // builder.ins().set_pinned_reg(addr)

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
            // stack: Vec::new(),
            shadow_stack: Vec::new(),
            arity,
            constants,
            // function_context,
            // id: func_id,
            // call_global_all_native: false,
            // name,
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
            queue: VecDeque::new(),
            if_stack: Vec::new(),
            if_bound: None,
            vm_context,
            // cloned_stack: false,
            // generators: Default::default(),
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
    // Could be either an i64 or a big int
    Int,
    // Is just straight up, unboxed, meaning
    // its represented by a u8 on the stack on not
    // a 128.
    UnboxedBool,

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
}

// #[derive(Debug, Clone, Copy)]
// struct GeneratorIndex(usize);

// #[derive(Default)]
// struct LazyInstructionGenerators {
//     funcs: Vec<Arc<dyn Fn(&mut FunctionTranslator) -> Value>>,
// }

// impl LazyInstructionGenerators {
//     fn take(&mut self, index: GeneratorIndex) -> Arc<dyn Fn(&mut FunctionTranslator) -> Value> {
//         self.funcs[index.0].clone()
//     }
// }

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
                let value = ctx.create_i128(encode(self.as_steelval()));
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

#[derive(Debug, Clone, Copy)]
enum Properties {
    // If we can call car on this list successfully, downstream of this
    // then we're both going to be listed as a proper list type,
    // and also this will successfully return without error.
    NonEmptyList,
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
    properties: HashMap<ValueOrRegister, Properties>,

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

    queue: VecDeque<(OpCode, usize, usize)>,

    depth: usize,

    if_bound: Option<usize>,

    if_stack: Vec<usize>,

    vm_context: GlobalValue,
    // vm_context: StackSlot,
    // generators: LazyInstructionGenerators,
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
    unsafe { std::mem::transmute(value) }
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
        // if self.depth > 100 {
        //     println!("Hit the depth limit, something went wrong");
        //     // println!("{:?}:{} @ {}", op, payload, self.ip);
        //     // println!("Previous path: {:#?}", self.queue);
        //     // let instr = self.instructions[self.ip];
        //     // let op = instr.op_code;
        //     // let payload = instr.payload_size.to_usize();
        //     // println!("{:?}:{} @ {}", op, payload, self.ip);
        //     // println!("Previous path: {:#?}", self.queue);

        //     let mut ips = HashSet::new();
        //     // let mut cycle = None;
        //     for (idx, v) in self.queue.iter().enumerate() {
        //         if !ips.insert(*v) {
        //             println!("Found a cycle at index: {} - {:?}", idx, v);
        //             // cycle = Some(idx);
        //             // break;
        //         }
        //     }
        //     println!("{:#?}", self.queue);
        //     println!("{}", std::backtrace::Backtrace::force_capture());
        //     // if let Some(cycle) = cycle {
        //     //     println!("Path up to cycle:");
        //     //     for item in self.queue.iter().take(cycle) {
        //     //         println!("{:?}", item);
        //     //     }
        //     // }
        //     panic!();
        // }

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
                    let void = SteelVal::Void;
                    let value = self.create_i128(encode(void));

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
                        let last_ref = self.shadow_stack.last().unwrap().into_value();

                        if last_ref.inferred_type == InferredType::UnboxedBool {
                            let false_instr = self.instructions[self.ip].payload_size;
                            let true_instr = self.ip + 1;

                            // Explicitly want the unboxed value here
                            let test_bool = last_ref.value;

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

                    if let Some(name) = name {
                        let v = self.call_function(arity, name, false);
                        self.push(v, InferredType::Any);
                    } else {
                        todo!("Implement spilled function call");
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
                    self.push_to_vm_stack_let_var(last);
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

                    // let _ = self.translate_tco_jmp_no_arity(payload);
                    // Jump to out of bounds so signal we're done
                    self.ip = self.instructions.len() + 1;

                    self.depth -= 1;

                    return false;
                }
                OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALTAILNOARITY
                | OpCode::CALLPRIMITIVETAIL => {
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();

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
                        let v =
                            self.call_global_function_spilled(arity, name, function_index, true);

                        self.push(v, InferredType::Any)
                    }

                    self.check_deopt();

                    self.ip = self.instructions.len() + 1;

                    // println!("------------------------> Returning");

                    self.depth -= 1;

                    return false;
                }
                OpCode::CALLGLOBALNOARITY => {
                    // First - find the index that we have to lookup.
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();
                    let name = CallGlobalNoArityFunctionDefinitions::arity_to_name(arity);

                    if let Some(name) = name {
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

                            if f == crate::primitives::strings::steel_char_equals
                                as FunctionSignature
                                && arity == 2
                                && false
                            {
                                self.char_equals(arity);
                            }
                            /*
                            else if f == steel_read_char as FunctionSignature
                                && arity == 1
                                && false
                            {
                                self.read_char(arity);
                            }
                            */
                            else if f == steel_eof_objectp as FunctionSignature
                                && arity == 1
                                && false
                            {
                                // Encode the object... Any others we can encode in this way?
                                self.eof_object()
                            } else if f == steel_mut_vec_set as FunctionSignature
                                && arity == 3
                                && false
                            {
                                self.vector_set()
                            } else if f == steel_eq as FunctionSignature && arity == 2 && false {
                                self.eq()
                            } else if f == steel_pair as FunctionSignature && arity == 1 && false {
                                self.is_pair()
                            } else {
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

                                    let result = self.call_function_returns_value_args(name, &args);
                                    self.push(result, InferredType::Any);
                                    self.ip += 1;
                                    self.check_deopt();
                                } else {
                                    self.ip -= 1;
                                    self.call_global_impl(payload);
                                }
                            }
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

                    self.let_var_stack.pop().unwrap();

                    // if last != payload {
                    //     pretty_print_dense_instructions(&self.instructions);
                    //     assert_eq!(last, payload);
                    // }

                    self.call_end_scope_handler(payload);
                }

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
                    let register = self.shadow_stack.pop().unwrap().into_index();

                    let register = self.builder.ins().iconst(types::I64, register as i64);

                    let args = [register, value.as_steelval(self)];
                    let result = self.call_function_returns_value_args("sub-binop-int-reg", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Number);

                    self.ip += 2;
                }

                OpCode::SUB
                    if payload == 2
                        && self.shadow_stack.last().and_then(|x| self.inferred_type(x))
                            == Some(InferredType::Int) =>
                {
                    // Call the func
                    self.func_ret_val_named("sub-binop-int", payload, 2, InferredType::Number);
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
                    let register = self.shadow_stack.pop().unwrap().into_index();

                    let register = self.builder.ins().iconst(types::I64, register as i64);

                    let args = [register, value.as_steelval(self)];
                    let result = self.call_function_returns_value_args("add-binop-reg", &args);

                    // Check the inferred type, if we know of it
                    self.push(result, InferredType::Number);

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
                OpCode::NULL => {
                    self.func_ret_val_named("null-handler", 1, 2, InferredType::Bool);
                }
                OpCode::CONS => {
                    self.func_ret_val(op, 2, 2, InferredType::List);
                }

                // Cdr reg no type check, should be faster
                OpCode::CDR => {
                    if let Some(last) = self.shadow_stack.last().copied() {
                        self.shadow_mark_local_type_from_var(last, InferredType::List);
                    }

                    match self.shadow_stack.last().unwrap().clone() {
                        MaybeStackValue::Register(reg) => {
                            let can_skip_bounds_check = matches!(
                                self.properties.get(&ValueOrRegister::Register(reg)),
                                Some(Properties::NonEmptyList)
                            );

                            self.shadow_stack.pop();
                            let reg = self.register_index(reg);

                            let func = if can_skip_bounds_check {
                                "cdr-reg-no-check"
                            } else {
                                "cdr-reg"
                            };

                            let res = self.call_function_returns_value_args(func, &[reg]);
                            self.push(res, InferredType::List);
                            self.ip += 2;
                        }

                        MaybeStackValue::MutRegister(reg) => {
                            let can_skip_bounds_check = matches!(
                                self.properties.get(&ValueOrRegister::Register(reg)),
                                Some(Properties::NonEmptyList)
                            );

                            self.shadow_stack.pop();

                            let func = if can_skip_bounds_check {
                                "cdr-mut-reg-no-check"
                            } else {
                                "cdr-mut-reg"
                            };

                            let reg = self.register_index(reg);
                            let res = self.call_function_returns_value_args(func, &[reg]);
                            self.push(res, InferredType::List);
                            self.ip += 2;
                        }

                        _ => {
                            self.func_ret_val(op, 1, 2, InferredType::List);
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
                        self.shadow_mark_local_type_from_var(last, InferredType::List);
                    }

                    match self.shadow_stack.last().unwrap().clone() {
                        MaybeStackValue::MutRegister(reg) | MaybeStackValue::Register(reg) => {
                            let can_skip_bounds_check = matches!(
                                self.properties.get(&ValueOrRegister::Register(reg)),
                                Some(Properties::NonEmptyList)
                            );

                            if can_skip_bounds_check {
                                self.shadow_stack.pop();
                                let reg = self.register_index(reg);
                                let res = self
                                    .call_function_returns_value_args("car-reg-unchecked", &[reg]);
                                self.push(res, InferredType::Any);
                                self.ip += 2;
                            } else {
                                // If its a non empty list, the next time we use it, we can skip bounds
                                // checks since we know that it has a cdr.
                                self.properties.insert(
                                    ValueOrRegister::Register(reg),
                                    Properties::NonEmptyList,
                                );

                                self.shadow_stack.pop();
                                let reg = self.register_index(reg);
                                let res = self.call_function_returns_value_args("car-reg", &[reg]);
                                self.push(res, InferredType::Any);
                                self.ip += 2;
                            }
                        }

                        _ => {
                            self.func_ret_val(op, 1, 2, InferredType::Any);
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
                    if let Some(last) = self.shadow_stack.last().copied() {
                        self.shadow_mark_local_type_from_var(last, InferredType::Box);
                    }

                    self.func_ret_val(op, 1, 2, InferredType::Any);
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
                    let last = self.shadow_stack.last().unwrap().into_value();

                    if last.inferred_type == InferredType::UnboxedBool {
                        let test = last.value;
                        let test = self.builder.ins().uextend(types::I64, test);
                        self.shadow_stack.pop();
                        let value = self.builder.ins().icmp_imm(IntCC::Equal, test, 0);
                        self.push(value, InferredType::UnboxedBool);
                        self.ip += 2;
                    } else if last.inferred_type == InferredType::Bool {
                        let (test, _) = self.shadow_pop();
                        // If this matches SteelVal::BoolV(false)
                        // exactly, then we're done.
                        let false_value = self.create_i128(encode(SteelVal::BoolV(false)));

                        let comparison = self.builder.ins().icmp(IntCC::Equal, test, false_value);
                        let res = self.builder.ins().uextend(types::I64, comparison);
                        let boolean =
                            self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
                        self.push(boolean, InferredType::Bool);
                        self.ip += 2;
                    } else {
                        self.func_ret_val(op, 1, 2, InferredType::Bool);
                    }

                    // let res = self.builder.ins().uextend(types::I64, comparison);
                    // let boolean =
                    //     self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);

                    // Do the thing.
                    // self.func_ret_val(op, 1, 2, InferredType::Bool);
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
            eof_objectp_jit as i64,
        );

        let mut args = vec![function, fallback_ip];
        args.extend(additional_args.into_iter().map(|x| x.0));

        let result = self.call_function_returns_value_args(name, &args);
        self.push(result, InferredType::Char);
        self.ip += 1;

        // Don't need to check deopt on predicates
    }

    fn gte(&mut self) {
        todo!()
    }

    // do the thing:
    fn is_pair(&mut self) {
        use MaybeStackValue::*;

        let last = self.shadow_stack.last().unwrap().clone();

        match last {
            // TODO: Encode the result of the evaluation into the
            // branching - if this is used in the test position
            // of an if statement, we should encode the type checking
            // through.
            Value(stack_value) => {
                self.shadow_stack.pop();
                // If we've already inferrred this type as a pair,
                // we can skip the code generation for checking the tags
                // and actually invoking the function since we know
                // it will be a pair.
                match stack_value.inferred_type {
                    InferredType::List | InferredType::Pair | InferredType::ListOrPair => {
                        let res = self.builder.ins().iconst(types::I64, 1);

                        let boolean =
                            self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);

                        self.push(boolean, InferredType::Bool);
                        self.ip += 1;

                        return;
                    }
                    _ => {}
                }

                let value = stack_value.as_steelval(self);

                // Encode this manually:
                let tag = self.get_tag(value);

                // TODO: Encode these tags in a better way besides manually
                // doing this here
                let pair_tag = self.tag(24);
                let list_tag = self.tag(23);

                // Compare these tags:
                let is_list = self.builder.ins().icmp(IntCC::Equal, tag, list_tag);
                let is_pair = self.builder.ins().icmp(IntCC::Equal, tag, pair_tag);

                let comparison = self.builder.ins().bor(is_list, is_pair);

                // let res = self.builder.ins().uextend(types::I64, comparison);
                // let boolean = self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
                // self.push(boolean, InferredType::Bool);

                self.push(comparison, InferredType::UnboxedBool);

                self.ip += 1;
            }
            MutRegister(p) | Register(p) => {
                let register = self.register_index(p);
                self.shadow_stack.pop();
                let res = self.call_function_returns_value_args("pair?", &[register]);

                self.push(res, InferredType::Bool);
                self.ip += 1;
            }

            // Depending on what the constant is, we can do this evaluation here
            // Constant(constant_value) => todo!(),
            _ => {
                todo!();
                // Fall back to checking is pair
            }
        }
    }

    // Pointer equality against constants can be inlined
    fn eq(&mut self) {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 2..)
            .unwrap();

        match args {
            // Okay, so for constants, we can wait to actually reify them
            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i)] => {
                let left = self.register_index(v);
                let right = self.register_index(i);

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args("eq?-reg-2", &[left, right]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            &[MutRegister(v) | Register(v), Value(_)] => {
                let left = self.register_index(v);
                let right = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args("eq?-reg-1", &[left, right.0]);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }

            // Spill all by value
            _ => {
                let args = self
                    .split_off(2)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_function_returns_value_args_no_context("eq?-args", &args);

                self.push(res, InferredType::UnboxedBool);
                self.ip += 1;
            }
        }
    }

    fn vector_set(&mut self) {
        use MaybeStackValue::*;

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - 3..)
            .unwrap();

        match args {
            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i), MutRegister(a) | Register(a)] =>
            {
                let vector = self.register_index(v);
                let index = self.register_index(i);
                let value = self.register_index(a);

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self
                    .call_function_returns_value_args("vector-set-reg-3", &[vector, index, value]);

                self.push(res, InferredType::Any);

                self.ip += 1;
                self.check_deopt();
            }

            &[MutRegister(v) | Register(v), MutRegister(i) | Register(i), Value(_)] => {
                let vector = self.register_index(v);
                let index = self.register_index(i);
                let value = self.shadow_pop();

                // Pop them off
                self.shadow_stack.pop();
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args(
                    "vector-set-reg-2",
                    &[vector, index, value.0],
                );

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
            &[MutRegister(v) | Register(v), Value(_), Value(_)] => {
                let value = self.shadow_pop();
                let index = self.shadow_pop();
                let vector = self.register_index(v);
                self.shadow_stack.pop();

                let res = self.call_function_returns_value_args(
                    "vector-set-reg-1",
                    &[vector, index.0, value.0],
                );

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }

            // Spill all by value
            _ => {
                let args = self
                    .split_off(3)
                    .into_iter()
                    .map(|x| x.0)
                    .collect::<Vec<_>>();

                let res = self.call_function_returns_value_args("vector-set-args", &args);

                self.push(res, InferredType::Any);
                self.ip += 1;
                self.check_deopt();
            }
        }
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

    fn char_equals(&mut self, arity: usize) {
        let name = CallPrimitiveFixedDefinitions::arity_to_name(arity).unwrap();

        let args = self
            .shadow_stack
            .get(self.shadow_stack.len() - arity..)
            .unwrap()
            .to_vec();

        // dbg!(args);

        // attempt to move forward with it
        let additional_args = self.split_off(arity);

        // dbg!(&additional_args);

        // let f = crate::primitives::ports::read_char_single
        //     as fn(SteelVal) -> Result<SteelVal, crate::SteelErr>;
        //

        let all_chars = additional_args.iter().all(|x| x.1 == InferredType::Char);

        if all_chars {
            // println!("Found all characters, applying equality");
            // Just... compare for equality?

            let left = additional_args[0].0;
            let right = additional_args[1].0;

            let left = self.unbox_value(left);
            let right = self.unbox_value(right);

            let left = self.builder.ins().ireduce(types::I8, left);
            let right = self.builder.ins().ireduce(types::I8, right);

            let comparison = self.builder.ins().icmp(IntCC::Equal, left, right);
            let res = self.builder.ins().uextend(types::I64, comparison);
            let boolean = self.encode_value(discriminant(&SteelVal::BoolV(true)) as i64, res);
            self.push(boolean, InferredType::Bool);
            self.ip += 1;

            // No need to check deopt here, we're good.
        } else {
            let function = self.builder.ins().iconst(
                self.module.target_config().pointer_type(),
                crate::primitives::strings::char_equals_binop as i64,
            );

            let fallback_ip = self
                .builder
                .ins()
                .iconst(Type::int(64).unwrap(), self.ip as i64);

            let mut args = vec![function, fallback_ip];

            args.extend(additional_args.into_iter().map(|x| x.0));

            let result = self.call_function_returns_value_args(name, &args);
            self.push(result, InferredType::Bool);
            self.ip += 1;
            self.check_deopt();
        }
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

        if payload < self.arity as _ {
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

    fn read_local_fixed_no_spill(&mut self, op: OpCode, payload: usize) -> (Value, InferredType) {
        assert!(payload < self.arity as _);

        // Replace this... with just reading from the vector?
        let value = self.call_func_or_immediate(op, payload);

        self.value_to_local_map.insert(value, payload);

        let inferred_type = if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
            *inferred_type
        } else {
            InferredType::Any
        };
        (value, inferred_type)
    }

    fn read_local_fixed(&mut self, op: OpCode, payload: usize) -> (Value, InferredType) {
        let let_var_offset: usize = self.let_var_stack.iter().sum();

        if payload > self.arity as usize + let_var_offset {
            let upper_bound = payload - self.arity as usize - let_var_offset;

            for i in 0..upper_bound {
                self.shadow_spill(i);
            }
        }

        // Replace this... with just reading from the vector?
        let value = self.call_func_or_immediate(op, payload);

        self.value_to_local_map.insert(value, payload);

        let inferred_type = if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
            *inferred_type
        } else {
            InferredType::Any
        };
        (value, inferred_type)
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
            // TODO: change this up
            // let value = if payload < 9 {
            //     let value =
            //         self.call_function_returns_value_args(op_to_name_payload(op, payload), &[]);
            //     value
            // } else {
            let index = self
                .builder
                .ins()
                .iconst(Type::int(64).unwrap(), payload as i64);
            let value =
                self.call_function_returns_value_args(op_to_name_payload(op, payload), &[index]);
            //     value
            // };

            self.value_to_local_map.insert(value, payload);

            let inferred_type = if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
                *inferred_type
            } else {
                InferredType::Any
            };

            // TODO: Should this advance the ip here as well?
            MaybeStackValue::Value(StackValue {
                value,
                inferred_type,
                spilled: false,
            })
        }
    }

    fn read_local_value_no_spill(&mut self, op: OpCode, payload: usize) -> (Value, InferredType) {
        assert!(payload < self.arity as _);

        // let index = self
        //     .builder
        //     .ins()
        //     .iconst(Type::int(64).unwrap(), payload as i64);

        // let value = if payload < 9 {
        //     let value = self.call_function_returns_value_args(op_to_name_payload(op, payload), &[]);
        //     value
        // } else {
        let index = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);
        let value =
            self.call_function_returns_value_args(op_to_name_payload(op, payload), &[index]);
        //     value
        // };

        self.value_to_local_map.insert(value, payload);

        let inferred_type = if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
            *inferred_type
        } else {
            InferredType::Any
        };

        (value, inferred_type)
    }

    fn read_local_value(&mut self, op: OpCode, payload: usize) -> (Value, InferredType) {
        let let_var_offset: usize = self.let_var_stack.iter().sum();

        if payload > self.arity as usize + let_var_offset {
            let upper_bound = payload - self.arity as usize - let_var_offset;

            for i in 0..upper_bound {
                self.shadow_spill(i);
            }
        }

        let index = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let value =
            self.call_function_returns_value_args(op_to_name_payload(op, payload), &[index]);

        self.value_to_local_map.insert(value, payload);

        let inferred_type = if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
            *inferred_type
        } else {
            InferredType::Any
        };

        (value, inferred_type)
    }

    fn call_global_impl(&mut self, payload: usize) {
        // First - find the index that we have to lookup.
        let function_index = payload;
        self.ip += 1;
        let arity = self.instructions[self.ip].payload_size.to_usize();

        let name = CallGlobalFunctionDefinitions::arity_to_name(arity);

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
        let sig = self.get_signature("pop-from-stack");

        let callee = self
            .module
            .declare_function("pop-from-stack", Linkage::Import, &sig)
            .expect("problem declaring function");

        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
        let ctx = self.get_ctx();

        let arg_values = vec![ctx];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
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

    // Let end scope handler - should instead pass the values in directly.
    fn call_end_scope_handler(&mut self, amount: usize) {
        let name = "let-end-scope-c";

        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();

        let amount_to_drop = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), amount as i64);

        let arg_values = [ctx, amount_to_drop];
        let _ = self.builder.ins().call(local_callee, &arg_values);
    }

    fn translate_tco_jmp_loop(&mut self, payload: usize) {
        todo!()
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

    fn _translate_tco_jmp_no_arity_loop(&mut self, payload: usize) {
        // for i in 0..self.stack.len() {
        //     self.spill(i);
        // }
        for i in 0..self.shadow_stack.len() {
            self.shadow_spill(i);
        }

        let local_callee = self.get_local_callee("self-tail-call-loop");
        let ctx = self.get_ctx();

        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let arg_values = [ctx, arity];
        let _call = self.builder.ins().call(local_callee, &arg_values);

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

    // If the values are left on the stack, we can do that?
    fn test_translate_tco_jmp_no_arity_loop_no_spill(&mut self, payload: usize, arity: usize) {
        let name = CallSelfTailCallNoArityLoopDefinitions::arity_to_name(payload).unwrap();

        let args_off_the_stack = self.split_off(payload);

        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();

        let arity = self
            .builder
            .ins()
            .iconst(Type::int(16).unwrap(), arity as i64);

        let mut arg_values = vec![ctx, arity];
        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));
        let _call = self.builder.ins().call(local_callee, &arg_values);

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

            if amount_dropped != 0 {
                return self.test_translate_tco_jmp_no_arity_loop_no_spill(
                    payload - amount_dropped,
                    payload,
                );
            }
        }

        let args_off_the_stack = self.split_off(payload);

        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();

        let arity = self
            .builder
            .ins()
            .iconst(Type::int(16).unwrap(), payload as i64);

        let mut arg_values = vec![ctx, arity];
        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));
        let _call = self.builder.ins().call(local_callee, &arg_values);

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

    fn _translate_tco_jmp_no_arity_without_spill(&mut self, payload: usize) {
        let name = CallSelfTailCallNoArityDefinitions::arity_to_name(payload).unwrap();

        // let mut args_off_the_stack = self
        //     .stack
        //     .drain(self.stack.len() - payload..)
        //     .collect::<Vec<_>>();

        // self.maybe_patch_from_stack(&mut args_off_the_stack);

        let args_off_the_stack = self.split_off(payload);

        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();

        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let mut arg_values = vec![ctx, arity];
        // arg_values.extend(args_off_the_stack.iter().map(|x| x.value));
        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));
        let _call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn _translate_tco_jmp_no_arity(&mut self, payload: usize) {
        // TODO: Don't spill to the stack right away
        // We can avoid a lot of movement since we can overwrite
        // the existing values on the stack before they're
        // pushed on.
        // for i in 0..self.stack.len() {
        //     self.spill(i);
        // }
        for i in 0..self.shadow_stack.len() {
            self.shadow_spill(i);
        }

        let local_callee = self.get_local_callee("self-tail-call");
        let ctx = self.get_ctx();

        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let arg_values = [ctx, arity];
        let _call = self.builder.ins().call(local_callee, &arg_values);
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

        /*

        // Check if its a function - otherwise, just spill the values to the stack.
        let is_function = self.check_callable(func, tail);

        {
            let then_block = self.builder.create_block();
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();

            self.builder.append_block_param(merge_block, self.int);

            self.builder
                .ins()
                .brif(is_function, then_block, &[], else_block, &[]);

            self.builder.switch_to_block(then_block);
            self.builder.seal_block(then_block);

            // Do nothing
            let then_return = BlockArg::Value(self.create_i128(0));

            // Check callable - TODO: Insert these checks elsewhere too!
            let should_spill = self.check_should_spill_value(func);

            let spill_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(should_spill, spill_block, &[], merge_block, &[then_return]);

            self.builder.switch_to_block(spill_block);
            self.builder.seal_block(spill_block);

            // for c in self.stack.clone() {
            //     if !c.spilled {
            //         self.push_to_vm_stack_function_spill(c.value);
            //     }
            // }

            if tail {
                self.spill_cloned_stack();
            } else {
                self.spill_stack();
            }

            self.builder.ins().jump(merge_block, &[then_return]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            // for c in self.stack.clone() {
            //     if !c.spilled {
            //         self.push_to_vm_stack(c.value);
            //     }
            // }

            // if tail {
            //     self.spill_cloned_stack();
            // } else {
            //     self.spill_stack();
            // }

            let else_return = BlockArg::Value(self.create_i128(0));

            self.builder.ins().jump(merge_block, &[else_return]);

            // Switch to the merge block for subsequent statements.
            self.builder.switch_to_block(merge_block);

            // We've now seen all the predecessors of the merge block.
            self.builder.seal_block(merge_block);
        }

        */

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
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

        // If any of these have been spilled, we have to pop them off
        // of the shadow stack and use them
        // let mut args_off_the_stack = self
        //     .stack
        //     .drain(self.stack.len() - arity..)
        //     .collect::<Vec<_>>();

        let args_off_the_stack = self.split_off(arity);

        // self.maybe_patch_from_stack(&mut args_off_the_stack);

        // for arg in &args_off_the_stack {
        //     assert!(!arg.spilled);
        // }

        // assert_eq!(args_off_the_stack.len(), arity);

        arg_values.extend(args_off_the_stack.iter().map(|x| x.0));

        /*
                let is_function = self.check_function(lookup_index, tail);
                {
                    let then_block = self.builder.create_block();
                    let else_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();

                    self.builder.append_block_param(merge_block, self.int);

                    self.builder
                        .ins()
                        .brif(is_function, then_block, &[], else_block, &[]);

                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);

                    // Do nothing
                    let then_return = BlockArg::Value(self.create_i128(0));

                    // Check callable - TODO: Insert these checks elsewhere too!
                    let should_spill = self.check_should_spill(lookup_index);

                    let spill_block = self.builder.create_block();

                    self.builder
                        .ins()
                        .brif(should_spill, spill_block, &[], merge_block, &[then_return]);

                    self.builder.switch_to_block(spill_block);
                    self.builder.seal_block(spill_block);

                    // for c in self.stack.clone() {
                    //     if !c.spilled {
                    //         self.push_to_vm_stack_function_spill(c.value);
                    //     }
                    // }

                    if tail {
                        self.spill_cloned_stack();
                    } else {
                        self.spill_stack();
                    }
                    // self.spill_cloned_stack();
                    // self.spill_stack();

                    self.builder.ins().jump(merge_block, &[then_return]);

                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);

                    // if tail {
                    //     self.spill_cloned_stack();
                    // } else {
                    //     self.spill_stack();
                    // }

                    let else_return = BlockArg::Value(self.create_i128(0));

                    self.builder.ins().jump(merge_block, &[else_return]);

                    // Switch to the merge block for subsequent statements.
                    self.builder.switch_to_block(merge_block);

                    // We've now seen all the predecessors of the merge block.
                    self.builder.seal_block(merge_block);
                }
        */

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

        // If any of these have been spilled, we have to pop them off
        // of the shadow stack and use them
        // let args_off_the_stack = self
        //     .stack
        //     .drain(self.stack.len() - arity..)
        //     .collect::<Vec<_>>();

        // let args_off_the_stack = self.split_off(arity);

        // self.maybe_patch_from_stack(&mut args_off_the_stack);

        // for arg in &args_off_the_stack {
        //     if !arg.spilled {
        //         self.push_to_vm_stack(arg.value);
        //     }
        // }

        // assert_eq!(args_off_the_stack.len(), arity);

        // arg_values.extend(args_off_the_stack.iter().map(|x| x.value));

        /*

        let is_function = self.check_function(lookup_index, tail);

        {
            let then_block = self.builder.create_block();
            let else_block = self.builder.create_block();
            let merge_block = self.builder.create_block();

            self.builder.append_block_param(merge_block, self.int);

            self.builder
                .ins()
                .brif(is_function, then_block, &[], else_block, &[]);

            self.builder.switch_to_block(then_block);
            self.builder.seal_block(then_block);

            // Do nothing
            let then_return = BlockArg::Value(self.create_i128(0));

            // Check callable - TODO: Insert these checks elsewhere too!
            let should_spill = self.check_should_spill(lookup_index);

            let spill_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(should_spill, spill_block, &[], merge_block, &[then_return]);

            self.builder.switch_to_block(spill_block);
            self.builder.seal_block(spill_block);

            // for c in self.stack.clone() {
            //     if !c.spilled {
            //         self.push_to_vm_stack_function_spill(c.value);
            //     }
            // }

            // self.spill_cloned_stack();

            if tail {
                self.spill_cloned_stack();
            } else {
                self.spill_stack();
            }

            self.builder.ins().jump(merge_block, &[then_return]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            // for c in self.stack.clone() {
            //     if !c.spilled {
            //         self.push_to_vm_stack_function_spill(c.value);
            //     }
            // }
            // self.spill_cloned_stack();

            // if tail {
            //     self.spill_cloned_stack();
            // } else {
            //     self.spill_stack();
            // }

            let else_return = BlockArg::Value(self.create_i128(0));

            self.builder.ins().jump(merge_block, &[else_return]);

            // Switch to the merge block for subsequent statements.
            self.builder.switch_to_block(merge_block);

            // We've now seen all the predecessors of the merge block.
            self.builder.seal_block(merge_block);
        }
        */

        if tail {
            self.spill_cloned_stack();
        } else {
            self.spill_stack();
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn check_callable(&mut self, callable: Value, tail: bool) -> Value {
        let name = if tail {
            "check-callable-value"
        } else {
            "check-callable-tail-value"
        };
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let call = self.builder.ins().call(local_callee, &[ctx, callable]);
        let result = self.builder.inst_results(call)[0];

        return result;
    }

    fn check_should_spill(&mut self, index: Value) -> Value {
        let name = "should-spill";
        // VmCore pointer
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let call = self.builder.ins().call(local_callee, &[ctx, index]);
        let result = self.builder.inst_results(call)[0];

        return result;
    }

    fn check_should_spill_value(&mut self, callable: Value) -> Value {
        let name = "should-spill-value";
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let call = self.builder.ins().call(local_callee, &[ctx, callable]);
        let result = self.builder.inst_results(call)[0];
        return result;
    }

    fn check_function(&mut self, index: Value, tail: bool) -> Value {
        // let mut sig = self.module.make_signature();
        let name = if tail {
            "check-callable-tail"
        } else {
            "check-callable"
        };
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let call = self.builder.ins().call(local_callee, &[ctx, index]);
        let result = self.builder.inst_results(call)[0];

        return result;
    }

    fn check_deopt_ptr_load(&mut self) -> Value {
        let ctx = self.get_ctx();
        let is_native = self
            .builder
            .ins()
            .load(Type::int(8).unwrap(), MemFlags::trusted(), ctx, 0);

        is_native
    }

    fn _check_deopt_new(&mut self) {
        let result = self.check_deopt_ptr_load();
        let then_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(result, then_block, &[], self.exit_block, &[]);

        // let else_block = self.builder.create_block();
        // let merge_block = self.builder.create_block();
        // self.builder
        //     .append_block_param(merge_block, Type::int(8).unwrap());
        // Do the thing.
        // self.builder
        //     .ins()
        //     .brif(result, then_block, &[], else_block, &[]);
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        // let then_return = BlockArg::Value(self.builder.ins().iconst(Type::int(8).unwrap(), 1));

        // Just... translate instructions?
        // self.stack_to_ssa();

        // if self.tco {
        //     self.ip = self.instructions.len() + 1;
        //     self.builder.ins().jump(self.exit_block, &[]);
        //     return;
        // }

        // Jump to the merge block, passing it the block return value.
        // self.builder.ins().jump(merge_block, &[then_return]);
        // self.builder.switch_to_block(else_block);
        // self.builder.seal_block(else_block);

        // Set the IP to the new spot:
        // {
        //     self.set_ctx_ip(self.ip);
        // }

        // // TODO: Update with the proper return value
        // let else_return = self.create_i128(0);
        // self.builder.ins().return_(&[]);
        // self.builder.switch_to_block(merge_block);
        // // // We've now seen all the predecessors of the merge block.
        // self.builder.seal_block(merge_block);
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

    fn check_deopt_working(&mut self) {
        let result = self.check_deopt_ptr_load();

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder
            .append_block_param(merge_block, Type::int(8).unwrap());
        // Do the thing.
        self.builder
            .ins()
            .brif(result, then_block, &[], else_block, &[]);
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        // let then_return = BlockArg::Value(self.builder.ins().iconst(Type::int(8).unwrap(), 1));
        let then_return = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

        // Just... translate instructions?
        self.stack_to_ssa();

        if self.tco {
            self.ip = self.instructions.len() + 1;
            self.builder.ins().jump(self.exit_block, &[]);
            return;
        }

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        // Set the IP to the new spot:
        // {
        //     self.set_ctx_ip(self.ip);
        // }

        // // TODO: Update with the proper return value
        // let else_return = self.create_i128(0);
        self.builder.ins().return_(&[]);
        self.builder.switch_to_block(merge_block);
        // // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);
        // // Read the value of the if-else by reading the merge block
        // // parameter.
        let _phi = self.builder.block_params(merge_block)[0];
        // phi
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
            self.push_to_vm_stack_spill(steelval);
        }

        Some(())
    }

    // fn spill(&mut self, index: usize) -> Option<()> {
    //     let guard = self.stack.get_mut(index)?;
    //     let mut spilled = false;
    //     if !guard.spilled {
    //         guard.spilled = true;
    //         spilled = true;
    //     }

    //     if spilled {
    //         self.push_to_vm_stack(self.stack[index].value);
    //     }

    //     Some(())
    // }

    // TODO: For spilling to the stack, we just _have_ to spill in order.
    // We have a cursor which will go through and mark if the value has already been pushed to the stack.
    // As long as we push the values in the right order, we're good.
    fn push(&mut self, value: Value, typ: InferredType) {
        // self.stack.push(StackValue {
        //     value,
        //     inferred_type: typ,
        //     spilled: false,
        // })

        self.shadow_stack.push(MaybeStackValue::Value(StackValue {
            value,
            inferred_type: typ,
            spilled: false,
        }))
    }

    fn shadow_push(&mut self, last: MaybeStackValue) {
        self.shadow_stack.push(last);
    }

    // fn pop(&mut self) -> (Value, InferredType) {
    //     let last = self.stack.pop().unwrap();

    //     assert!(!last.spilled);

    //     self.value_to_local_map.remove(&last.value);
    //     (last.value, last.inferred_type)
    // }

    fn spill_stack(&mut self) {
        // println!("Spilling stack: {:?}", self.shadow_stack);

        // assert!(!self.cloned_stack);
        for arg in 0..self.shadow_stack.len() {
            self.shadow_spill(arg);
        }

        // println!("Stack after spill: {:?}", self.shadow_stack);

        // self.spill_cloned_stack();
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
        let (value, _) = match p {
            0 => self.read_local_fixed_no_spill(OpCode::MOVEREADLOCAL0, p),
            1 => self.read_local_fixed_no_spill(OpCode::MOVEREADLOCAL1, p),
            2 => self.read_local_fixed_no_spill(OpCode::MOVEREADLOCAL2, p),
            3 => self.read_local_fixed_no_spill(OpCode::MOVEREADLOCAL3, p),
            _ => self.read_local_value_no_spill(OpCode::MOVEREADLOCAL, p),
        };

        (value, InferredType::Any)
    }

    fn immutable_register_to_value(&mut self, p: usize) -> (Value, InferredType) {
        let (value, _) = match p {
            0 => self.read_local_fixed_no_spill(OpCode::READLOCAL0, p),
            1 => self.read_local_fixed_no_spill(OpCode::READLOCAL1, p),
            2 => self.read_local_fixed_no_spill(OpCode::READLOCAL2, p),
            3 => self.read_local_fixed_no_spill(OpCode::READLOCAL3, p),
            _ => self.read_local_value_no_spill(OpCode::READLOCAL, p),
        };

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

    // fn shadow_split_off(&mut self, payload: usize) -> Vec<MaybeStackValue> {
    //     let mut args = self.shadow_stack.split_off(self.stack.len() - payload);

    //     // Patch the args if needed
    //     for arg in &args {
    //         // self.value_to_local_map.remove(&arg.value);

    //         if let MaybeStackValue::Value(v) = arg {
    //             self.value_to_local_map.remove(&v.value);
    //         }
    //     }

    //     self.shadow_maybe_patch_from_stack(&mut args);

    //     args
    // }

    // fn old_split_off(&mut self, payload: usize) -> Vec<(Value, InferredType)> {
    //     let mut args = self.stack.split_off(self.stack.len() - payload);

    //     // Patch the args if needed
    //     for arg in &args {
    //         self.value_to_local_map.remove(&arg.value);
    //     }

    //     self.maybe_patch_from_stack(&mut args);

    //     args.into_iter()
    //         .map(|x| (x.value, x.inferred_type))
    //         .collect()
    // }

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
                    SteelVal::BoolV(_) | SteelVal::IntV(_) | SteelVal::CharV(_) => {
                        self.create_i128(encode(constant))
                    }
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
        // let cloned_stack = self.cloned_stack;
        // let tco = self.tco;

        self.if_stack.push(start);

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
            self.create_i128(encode(SteelVal::IntV(12345)))

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
        self.let_var_stack = let_stack;
        self.shadow_stack = frozen_stack;

        // Set the if bound for the else case as well
        self.if_bound = else_offset;

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
            self.create_i128(encode(SteelVal::IntV(12345)))

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

                self.create_i128(encode(SteelVal::Void))
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

                let phi = self.builder.block_params(merge_block)[0];

                self.push(phi, InferredType::Any);

                self.stack_to_ssa();

                self.if_bound = last_bound;

                self.create_i128(encode(SteelVal::Void))
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

    fn vm_pop(&mut self, value: Value) {
        let name = "handle-pop!";
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let arg_values = [ctx, value];
        let _call = self.builder.ins().call(local_callee, &arg_values);
    }

    // fn push_to_vm_stack_two(&mut self, value: Value, value2: Value) {
    //     let mut sig = self.module.make_signature();
    //     let name = "push-to-vm-stack-2";

    //     sig.params
    //         .push(AbiParam::new(self.module.target_config().pointer_type()));

    //     sig.params.push(AbiParam::new(Type::int(128).unwrap()));
    //     sig.params.push(AbiParam::new(Type::int(128).unwrap()));

    //     // TODO: Streamline the API here?
    //     let callee = self
    //         .module
    //         .declare_function(&name, Linkage::Import, &sig)
    //         .expect("problem declaring function");
    //     let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

    //     // let mut arg_values = Vec::new();

    //     let variable = self.variables.get("vm-ctx").expect("variable not defined");
    //     let ctx = self.builder.use_var(*variable);
    //     let arg_values = [ctx, value, value2];

    //     // for arg in args {
    //     //     arg_values.push(self.translate_expr(arg))
    //     // }
    //     let call = self.builder.ins().call(local_callee, &arg_values);
    //     // let result = self.builder.inst_results(call)[0];
    //     // result
    // }

    fn push_to_vm_stack(&mut self, value: Value) {
        let name = "push-to-vm-stack";

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

    fn push_to_vm_stack_spill(&mut self, value: Value) {
        let name = "push-to-vm-stack-function-spill";

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

    fn push_to_vm_stack_function_spill(&mut self, value: Value) {
        let name = "push-to-vm-stack-function-spill";
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let arg_values = [ctx, value];
        let _call = self.builder.ins().call(local_callee, &arg_values);
    }

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

    fn get_signature(&self, name: &str) -> Signature {
        let mut sig = self.intrinsics.get_signature(name, &self.module);
        if cfg!(target_os = "windows") {
            sig.call_conv = CallConv::SystemV;
        }
        sig
    }

    fn get_local_callee(&mut self, name: &str) -> FuncRef {
        let sig = self.get_signature(name);

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        self.module.declare_func_in_func(callee, self.builder.func)
    }

    fn get_ctx(&mut self) -> Value {
        // let variable = self.variables.get("vm-ctx").expect("variable not defined");
        // let ctx = self.builder.use_var(*variable);
        // ctx

        // self.builder.ins().vmctx();

        let ptr_type = self.module.target_config().pointer_type();

        // self.builder.ins().stack_load(ptr_type, self.vm_context, 0)

        self.builder.ins().global_value(ptr_type, self.vm_context)
    }

    fn call_function_returns_value_args(&mut self, name: &str, args: &[Value]) -> Value {
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();

        let mut arg_values = vec![ctx];
        arg_values.extend(args.iter());
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_function_returns_value_args_no_context(&mut self, name: &str, args: &[Value]) -> Value {
        let local_callee = self.get_local_callee(name);
        let call = self.builder.ins().call(local_callee, &args);
        let result = self.builder.inst_results(call)[0];
        result
    }
}

// TODO: When setting up special functions, we'll
// do a different thing to create a function that we want
// that looks more like fn(&mut VmCore, args: &[SteelVal])
// fn declare_variables(
//     int: types::Type,
//     pointer: types::Type,
//     builder: &mut FunctionBuilder,
//     params: &[String],
//     entry_block: Block,
// ) -> HashMap<String, Variable> {
//     let mut variables = HashMap::new();
//     let mut index = 0;

//     // Leave the first one in place to pass the context in.
//     // {
//     //     let ctx = builder.block_params(entry_block)[0];
//     //     let var = declare_variable(pointer, builder, &mut variables, &mut index, "vm-ctx");
//     //     builder.def_var(var, ctx);
//     // }

//     for (i, name) in params.iter().enumerate() {
//         let val = builder.block_params(entry_block)[i + 1];
//         let var = declare_variable(int, builder, &mut variables, &mut index, name);
//         builder.def_var(var, val);
//     }

//     variables
// }

// Declare a single variable declaration.
// fn declare_variable(
//     int: types::Type,
//     builder: &mut FunctionBuilder,
//     variables: &mut HashMap<String, Variable>,
//     index: &mut usize,
//     name: &str,
// ) -> Variable {
//     if !variables.contains_key(name) {
//         let variable = builder.declare_var(int);
//         variables.insert(name.into(), variable.clone());
//         *index += 1;
//         variable
//     } else {
//         variables.get(name).unwrap().clone()
//     }
// }
