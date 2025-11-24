use cranelift::{
    codegen::ir::{BlockArg, Type},
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use std::collections::HashMap;
use std::slice;
// use steel_gen::opcode::OPCODES_ARRAY;
use steel_gen::{opcode::OPCODES_ARRAY, OpCode};

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::{u24, DenseInstruction},
    steel_vm::vm::{
        jit::{
            advance_ip, box_handler_c, call_function_deopt_0, call_function_deopt_1,
            call_function_deopt_2, call_function_deopt_3, call_global_function_deopt_0,
            call_global_function_deopt_0_func, call_global_function_deopt_0_no_arity,
            call_global_function_deopt_1, call_global_function_deopt_1_func,
            call_global_function_deopt_1_no_arity, call_global_function_deopt_2,
            call_global_function_deopt_2_func, call_global_function_deopt_2_no_arity,
            call_global_function_deopt_3, call_global_function_deopt_3_func,
            call_global_function_tail_deopt_0, call_global_function_tail_deopt_1,
            call_global_function_tail_deopt_2, call_global_function_tail_deopt_3,
            callglobal_handler_deopt_c, callglobal_tail_handler_deopt_3,
            callglobal_tail_handler_deopt_3_test, car_handler_value, cdr_handler_value,
            check_callable, check_callable_tail, check_callable_value, cons_handler_value,
            drop_value, equal_binop, extern_c_add_two, extern_c_div_two, extern_c_gt_two,
            extern_c_gte_two, extern_c_lt_two, extern_c_lte_two, extern_c_lte_two_int,
            extern_c_mult_two, extern_c_null_handler, extern_c_sub_two, extern_c_sub_two_int,
            extern_handle_pop, if_handler_raw_value, if_handler_value, let_end_scope_c,
            move_read_local_0_value_c, move_read_local_1_value_c, move_read_local_2_value_c,
            move_read_local_3_value_c, move_read_local_any_value_c, not_handler_raw_value,
            num_equal_value, num_equal_value_unboxed, pop_value, push_const_value_c,
            push_const_value_index_c, push_global, push_int_0, push_int_1, push_int_2,
            push_to_vm_stack, push_to_vm_stack_let_var, push_to_vm_stack_two, read_local_0_value_c,
            read_local_1_value_c, read_local_2_value_c, read_local_3_value_c,
            read_local_any_value_c, self_tail_call_handler, set_ctx_ip, set_handler_c,
            setbox_handler_c, should_continue, tcojmp_handler, trampoline, trampoline_no_arity,
            unbox_handler_c,
        },
        VmCore,
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

// Take a function, if its sufficiently hot, we can jit compile by going
// through and merging the handlers into one function, avoiding the dispatch
// cost. After that, we can do even better, but for now that should work.
// Blindly merging the op codes... that should work actually?

// TODO: This could help.
// // Forget the destructors.
// let reconstructed = unsafe { std::mem::transmute::<_, Arc<SValue>>(value.value()) };
// // If this is dropped, we're bad news bears
// let ptr = Arc::as_ptr(&reconstructed);
// unsafe { Arc::increment_strong_count(ptr) };
// // Horrendously unsafe, but waddya gonna do
// // let ptr = unsafe { std::mem::transmute::<_, Arc<SValue>>(value.value()) };
// println!("{:?}", reconstructed);
// drop(reconstructed);

// macro_rules! offset_of {
//     ($($tt:tt)*) => {
//         {
//             let base = $($tt)*(unsafe { ::std::mem::uninitialized() });
//             let offset = match base {
//                 $($tt)*(ref inner) => (inner as *const _ as usize) - (&base as *const _ as usize),
//                 _ => unreachable!(),
//             };
//             ::std::mem::forget(base);
//             offset
//         }
//     }
// }

// TODO: Implement some kind of handler -> signature generator?
// Maybe via macros?

// Turn functions from function signature, into cranelift signature for backend usage.
// TODO: Rename to an intrinsic map?
// Wire up with function signatures as well for automated unboxing?
struct FunctionMap<'a> {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    map2: HashMap<&'static str, Box<dyn FunctionToCranelift2 + Send + Sync + 'static>>,
    return_type_hints: HashMap<&'static str, InferredType>,
    builder: &'a mut JITBuilder,
}

struct OwnedFunctionMap {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    return_type_hints: HashMap<&'static str, InferredType>,
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

    pub fn get_signature(&self, name: &'static str, module: &JITModule) -> Signature {
        self.map
            .get(name)
            .map(|x| x.to_cranelift(module))
            .unwrap_or_else(|| self.map2.get(name).map(|x| x.to_cranelift(module)).unwrap())
    }
}

trait FunctionToCranelift {
    fn to_cranelift(&self, module: &JITModule) -> Signature;
    fn as_pointer(&self) -> *const u8;
}

trait FunctionToCranelift2 {
    fn to_cranelift(&self, module: &JITModule) -> Signature;
    fn as_pointer(&self) -> *const u8;
}

// TODO: How to set up the right args here?
// impl<T> FunctionToCranelift for extern "C-unwind" fn() -> T {
//     fn to_cranelift(&self, module: &JITModule) -> Signature {
//         todo!()
//     }
//     fn as_pointer(&self) -> *const u8 {
//         *self as _
//     }
// }

macro_rules! register_function_pointers_return {
    ($($typ:ident),*) => {
        impl<RET, $($typ),*> FunctionToCranelift for extern "C-unwind" fn(*mut VmCore, $($typ),*) -> RET {
            fn to_cranelift(&self, module: &JITModule) -> Signature {
                let mut sig = module.make_signature();

                // VmCore pointer
                sig.params
                    .push(AbiParam::new(module.target_config().pointer_type()));

                $(
                    sig.params.push(AbiParam::new(type_to_ir_type::<$typ>()));
                )*

                let return_size = std::mem::size_of::<RET>().min(1);

                if return_size != 0 {
                    sig.returns.push(AbiParam::new(type_to_ir_type::<RET>()));
                }

                sig
            }

            fn as_pointer(&self) -> *const u8 {
                *self as _
            }
        }

        impl<RET, $($typ),*> FunctionToCranelift2 for extern "C-unwind" fn($($typ),*) -> RET {
            fn to_cranelift(&self, module: &JITModule) -> Signature {
                let mut sig = module.make_signature();

                // VmCore pointer
                sig.params
                    .push(AbiParam::new(module.target_config().pointer_type()));

                $(
                    sig.params.push(AbiParam::new(type_to_ir_type::<$typ>()));
                )*

                let return_size = std::mem::size_of::<RET>().min(1);

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

fn type_to_ir_type<T>() -> Type {
    Type::int(std::mem::size_of::<T>() as u16 * 8).unwrap()
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
        builder.symbol("if-branch", if_handler_value as *const u8);

        builder.symbol("if-branch-value", if_handler_raw_value as *const u8);

        builder.symbol("not-value", not_handler_raw_value as *const u8);

        builder.symbol("call-global", callglobal_handler_deopt_c as *const u8);

        // Specialize the constant: Just look up the value itself:
        // This should be used for constants like #f, 1, 2, 3, etc.
        // Simply inline the value directly onto the stack, rather than
        // calling a function
        builder.symbol("push-int-0", push_int_0 as *const u8);
        builder.symbol("push-int-1", push_int_1 as *const u8);
        builder.symbol("push-int-2", push_int_2 as *const u8);

        builder.symbol(
            "call-global-tail-3",
            callglobal_tail_handler_deopt_3 as *const u8,
        );

        builder.symbol(
            "call-global-tail-3-test",
            callglobal_tail_handler_deopt_3_test as *const u8,
        );

        // Value functions:
        builder.symbol("num-equal-value", num_equal_value as *const u8);

        builder.symbol("equal-binop", equal_binop as *const u8);

        builder.symbol(
            "num-equal-value-unboxed",
            num_equal_value_unboxed as *const u8,
        );

        builder.symbol(
            "call-global-function-deopt-0-func",
            call_global_function_deopt_0_func as *const u8,
        );

        builder.symbol(
            "call-global-function-deopt-1-func",
            call_global_function_deopt_1_func as *const u8,
        );

        builder.symbol(
            "call-global-function-deopt-2-func",
            call_global_function_deopt_2_func as *const u8,
        );

        builder.symbol(
            "call-global-function-deopt-3-func",
            call_global_function_deopt_3_func as *const u8,
        );

        builder.symbol("let-end-scope-c", let_end_scope_c as *const u8);
        builder.symbol("set-ctx-ip!", set_ctx_ip as *const u8);

        let mut map = FunctionMap {
            map: HashMap::new(),
            map2: HashMap::new(),
            builder: &mut builder,
            return_type_hints: HashMap::new(),
        };

        map.add_func(
            "vm-increment-ip",
            advance_ip as extern "C-unwind" fn(*mut VmCore),
        );

        map.add_func(
            "drop-value",
            drop_value as extern "C-unwind" fn(*mut VmCore, SteelVal),
        );

        map.add_func(
            "pop-from-stack",
            pop_value as extern "C-unwind" fn(*mut VmCore) -> SteelVal,
        );

        map.add_func(
            "handle-pop!",
            extern_handle_pop as extern "C-unwind" fn(*mut VmCore, SteelVal),
        );

        // 0 => "call-func-deopt-0",
        // 1 => "call-func-deopt-1",
        // 2 => "call-func-deopt-2",
        // 3 => "call-func-deopt-3",

        map.add_func(
            "call-func-deopt-0",
            call_function_deopt_0 as extern "C-unwind" fn(*mut VmCore, SteelVal, usize) -> SteelVal,
        );
        map.add_func(
            "call-func-deopt-1",
            call_function_deopt_1
                as extern "C-unwind" fn(*mut VmCore, SteelVal, usize, SteelVal) -> SteelVal,
        );
        map.add_func(
            "call-func-deopt-2",
            call_function_deopt_2
                as extern "C-unwind" fn(
                    *mut VmCore,
                    SteelVal,
                    usize,
                    SteelVal,
                    SteelVal,
                ) -> SteelVal,
        );
        map.add_func(
            "call-func-deopt-3",
            call_function_deopt_3
                as extern "C-unwind" fn(
                    *mut VmCore,
                    SteelVal,
                    usize,
                    SteelVal,
                    SteelVal,
                    SteelVal,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-0",
            call_global_function_deopt_0
                as extern "C-unwind" fn(*mut VmCore, usize, usize) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-1",
            call_global_function_deopt_1
                as extern "C-unwind" fn(*mut VmCore, usize, usize, SteelVal) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-2",
            call_global_function_deopt_2
                as extern "C-unwind" fn(*mut VmCore, usize, usize, SteelVal, SteelVal) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-no-arity-0",
            call_global_function_deopt_0_no_arity
                as extern "C-unwind" fn(*mut VmCore, usize, usize) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-no-arity-1",
            call_global_function_deopt_1_no_arity
                as extern "C-unwind" fn(*mut VmCore, usize, usize, SteelVal) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-no-arity-2",
            call_global_function_deopt_2_no_arity
                as extern "C-unwind" fn(*mut VmCore, usize, usize, SteelVal, SteelVal) -> SteelVal,
        );

        map.add_func(
            "trampoline",
            trampoline as extern "C-unwind" fn(*mut VmCore, usize, usize) -> SteelVal,
        );

        map.add_func(
            "trampoline-no-arity",
            trampoline_no_arity as extern "C-unwind" fn(*mut VmCore, usize) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-3",
            call_global_function_deopt_3
                as extern "C-unwind" fn(
                    *mut VmCore,
                    usize,
                    usize,
                    SteelVal,
                    SteelVal,
                    SteelVal,
                ) -> SteelVal,
        );

        map.add_func(
            "push-global-value",
            push_global as extern "C-unwind" fn(ctx: *mut VmCore, index: usize) -> SteelVal,
        );

        // Check if the function at the global location is in fact the right one.
        map.add_func(
            "check-callable",
            check_callable as extern "C-unwind" fn(ctx: *mut VmCore, index: usize) -> bool,
        );

        map.add_func(
            "check-callable-tail",
            check_callable_tail as extern "C-unwind" fn(ctx: *mut VmCore, index: usize) -> bool,
        );

        map.add_func(
            "check-callable-value",
            check_callable_value as extern "C-unwind" fn(ctx: *mut VmCore, func: SteelVal) -> bool,
        );

        map.add_func(
            "push-to-vm-stack",
            push_to_vm_stack as extern "C-unwind" fn(ctx: *mut VmCore, value: SteelVal),
        );

        map.add_func(
            "push-to-vm-stack-let-var",
            push_to_vm_stack_let_var as extern "C-unwind" fn(ctx: *mut VmCore, value: SteelVal),
        );

        map.add_func(
            "push-to-vm-stack-function-spill",
            push_to_vm_stack_let_var as extern "C-unwind" fn(ctx: *mut VmCore, value: SteelVal),
        );

        map.add_func(
            "push-to-vm-stack-2",
            push_to_vm_stack_two
                as extern "C-unwind" fn(ctx: *mut VmCore, value: SteelVal, value2: SteelVal),
        );

        #[allow(improper_ctypes_definitions)]
        type Vm01 = extern "C-unwind" fn(*mut VmCore) -> SteelVal;

        type Vm01int = extern "C-unwind" fn(*mut VmCore) -> i128;

        #[allow(improper_ctypes_definitions)]
        type Vm02 = extern "C-unwind" fn(*mut VmCore, SteelVal) -> SteelVal;

        type Vm02int = extern "C-unwind" fn(*mut VmCore, i128) -> i128;

        #[allow(improper_ctypes_definitions)]
        type Vm0b = extern "C-unwind" fn(*mut VmCore) -> bool;

        #[allow(improper_ctypes_definitions)]
        type VmBinOp = extern "C-unwind" fn(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal;

        #[allow(improper_ctypes_definitions)]
        type BinOp = extern "C-unwind" fn(a: SteelVal, b: SteelVal) -> SteelVal;

        map.add_func("car-handler-value", car_handler_value as Vm02);
        map.add_func("cdr-handler-value", cdr_handler_value as Vm02);
        map.add_func("cons-handler-value", cons_handler_value as VmBinOp);

        map.add_func("box-handler", box_handler_c as Vm02);
        map.add_func("unbox-handler", unbox_handler_c as Vm02);
        map.add_func("set-box-handler", setbox_handler_c as VmBinOp);

        map.add_func("push-const", push_const_value_c as Vm01);
        map.add_func(
            "push-const-index",
            push_const_value_index_c as extern "C-unwind" fn(*mut VmCore, usize) -> SteelVal,
        );

        map.add_func_hint(
            "add-binop",
            extern_c_add_two as VmBinOp,
            InferredType::Number,
        );
        map.add_func_hint(
            "sub-binop",
            extern_c_sub_two as VmBinOp,
            InferredType::Number,
        );

        map.add_func_hint2(
            "sub-binop-int",
            extern_c_sub_two_int as BinOp,
            InferredType::Number,
        );

        map.add_func_hint("lt-binop", extern_c_lt_two as VmBinOp, InferredType::Bool);

        map.add_func_hint2("lte-binop", extern_c_lte_two as BinOp, InferredType::Bool);

        map.add_func_hint2(
            "lte-binop-int",
            extern_c_lte_two_int as BinOp,
            InferredType::Bool,
        );

        map.add_func_hint2(
            "null-handler",
            extern_c_null_handler as extern "C-unwind" fn(a: SteelVal) -> SteelVal,
            InferredType::Bool,
        );

        map.add_func_hint("gt-binop", extern_c_gt_two as VmBinOp, InferredType::Bool);
        map.add_func_hint("gte-binop", extern_c_gte_two as VmBinOp, InferredType::Bool);
        map.add_func_hint(
            "mult-two",
            extern_c_mult_two as VmBinOp,
            InferredType::Number,
        );
        map.add_func_hint("div-two", extern_c_div_two as VmBinOp, InferredType::Number);

        map.add_func(
            "call-global-function-tail-deopt-0",
            call_global_function_tail_deopt_0
                as extern "C-unwind" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function-tail-deopt-1",
            call_global_function_tail_deopt_1
                as extern "C-unwind" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                    arg0: SteelVal,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function-tail-deopt-2",
            call_global_function_tail_deopt_2
                as extern "C-unwind" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                    arg0: SteelVal,
                    arg1: SteelVal,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function-tail-deopt-3",
            call_global_function_tail_deopt_3
                as extern "C-unwind" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                    arg0: SteelVal,
                    arg1: SteelVal,
                    arg2: SteelVal,
                ) -> SteelVal,
        );

        // TODO: Pick up from here!
        map.add_func("read-local-0", read_local_0_value_c as Vm01);
        map.add_func("read-local-1", read_local_1_value_c as Vm01);
        map.add_func("read-local-2", read_local_2_value_c as Vm01);
        map.add_func("read-local-3", read_local_3_value_c as Vm01);

        map.add_func(
            "read-local-any",
            read_local_any_value_c
                as extern "C-unwind" fn(ctx: *mut VmCore, lookup_index: usize) -> SteelVal,
        );

        map.add_func("move-read-local-0", move_read_local_0_value_c as Vm01);
        map.add_func("move-read-local-1", move_read_local_1_value_c as Vm01);
        map.add_func("move-read-local-2", move_read_local_2_value_c as Vm01);
        map.add_func("move-read-local-3", move_read_local_3_value_c as Vm01);

        map.add_func(
            "move-read-local-any",
            move_read_local_any_value_c
                as extern "C-unwind" fn(ctx: *mut VmCore, lookup_index: usize) -> SteelVal,
        );

        map.add_func(
            "self-tail-call",
            self_tail_call_handler as extern "C-unwind" fn(*mut VmCore, usize),
        );

        map.add_func(
            "tco-jump",
            tcojmp_handler as extern "C-unwind" fn(*mut VmCore, usize),
        );

        map.add_func(
            "set-handler",
            set_handler_c as extern "C-unwind" fn(*mut VmCore, usize, SteelVal) -> SteelVal,
        );

        let function_map = OwnedFunctionMap {
            map: map.map,
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

#[test]
fn load_jit() {
    let mut jit = JIT::default();

    //     MOVEREADLOCAL0 : 0      ;; x
    // 1    MOVEREADLOCAL1 : 1      ;; y
    // 2    MOVEREADLOCAL2 : 2      ;; z
    // 3    CALLGLOBALTAIL : 260    ;; +
    // 4    TAILCALL       : 3      ;; +
    // 5    POPPURE        : 3

    jit.compile(
        "foo".to_string(),
        0,
        &[
            DenseInstruction {
                op_code: OpCode::MOVEREADLOCAL0,
                payload_size: u24::from_u32(0),
            },
            DenseInstruction {
                op_code: OpCode::MOVEREADLOCAL1,
                payload_size: u24::from_u32(0),
            },
            DenseInstruction {
                op_code: OpCode::MOVEREADLOCAL2,
                payload_size: u24::from_u32(0),
            },
            DenseInstruction {
                op_code: OpCode::CALLGLOBALTAIL,
                payload_size: u24::from_u32(0),
            },
        ],
        &[],
        &ConstantMap::default(),
        None,
    )
    .unwrap();
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
) -> Result<fn(&mut VmCore) -> bool, String> {
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
    let code_fn = std::mem::transmute::<*const u8, fn(&mut VmCore) -> bool>(code_ptr);
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
    ) -> Result<fn(&mut VmCore) -> bool, String> {
        unsafe { compile_bytecode(self, name, arity, code, globals, constants, function_index) }
    }
}

// // Compile the code to whatever inputs we want. In this case, I think
// // its going to follow the pattern that we want
// unsafe fn compile_code<I, O>(
//     jit: &mut JIT,
//     code: &[DenseInstruction],
// ) -> Result<fn(I) -> O, String> {
//     // Pass the string to the JIT, and it returns a raw pointer to machine code.
//     let code_ptr = jit.compile(code)?;
//     // Cast the raw pointer to a typed function pointer. This is unsafe, because
//     // this is the critical point where you have to trust that the generated code
//     // is safe to be called.
//     let code_fn = std::mem::transmute::<*const u8, fn(I) -> O>(code_ptr);
//     Ok(code_fn)
// }

// Supported OpCodes:
// Lets just start with the basics: Fuse opcodes together, call the handlers

// Just put them in sequence, subtract by the offset?
// Store them inline with the instructions, just double the size?
// Isn't that... wasteful? Probably?

impl JIT {
    /// Compile a string in the toy language into machine code.
    pub fn compile(
        &mut self,
        name: String,
        arity: u16,
        instructions: &[DenseInstruction],
        globals: &[SteelVal],
        constants: &ConstantMap,
        function_index: Option<usize>,
    ) -> Result<*const u8, String> {
        self.ctx.set_disasm(true);

        // First, parse the string, producing AST nodes.

        // TODO: Change this - we need to actually pass in the instructions
        // that we want to jit compile.
        let (params, the_return, stmts) = (Default::default(), Default::default(), instructions);
        // parser::function(input).map_err(|e| e.to_string())?;

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
            the_return,
            stmts,
            globals,
            constants,
            function_index,
        )?;

        // println!("{}", self.ctx.func);

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should
        // we have a version of `declare_function` that automatically declares
        // the function?

        if let Err(e) = cranelift::codegen::verify_function(&self.ctx.func, self.module.isa()) {
            println!("codegen error");

            // let mut s = String::new();

            // SsirWriter::new(self.ssir, &mut s)
            //     .fn_(&self.ssir.values[r].path, f)
            //     .unwrap();
            // println!("SSIR:\n{}", s);

            // s.clear();

            // codegen::write_function(&mut s, &func).unwrap();
            // println!("CLIF:\n{}", s);

            panic!("errors: {:#?}", e);
        }

        // cranelift::codegen::verify_function(func, )

        // if let Err(e) = codegen::verify_function(&func, &self.flags) {
        //     println!("codegen error");

        //     let mut s = String::new();

        //     SsirWriter::new(self.ssir, &mut s)
        //         .fn_(&self.ssir.values[r].path, f)
        //         .unwrap();
        //     println!("SSIR:\n{}", s);

        //     s.clear();

        //     codegen::write_function(&mut s, &func).unwrap();
        //     println!("CLIF:\n{}", s);

        //     panic!("errors: {:#?}", e);
        // }

        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| {
                println!("error in defining function");
                e.to_string()
            })?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions().unwrap();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        // println!("{:?}", self.ctx.compiled_code());

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
        name: String,
        func_id: FuncId,
        arity: u16,
        params: Vec<String>,
        the_return: String,
        bytecode: &[DenseInstruction],
        globals: &[SteelVal], // stmts: Vec<Expr>,
        constants: &ConstantMap,
        function_context: Option<usize>,
    ) -> Result<(), String> {
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        // let int = self.module.target_config().pointer_type();

        // Upgrade to 128 bit?
        let int = Type::int(128).unwrap();

        // Set up pointer type to be the first argument.
        let pointer = self.module.target_config().pointer_type();
        self.ctx.func.signature.params.push(AbiParam::new(pointer));

        // for _p in &params {
        //     self.ctx.func.signature.params.push(AbiParam::new(int));
        // }

        // Our toy language currently only supports one return value, though
        // Cranelift is designed to support more.
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(Type::int(8).unwrap()));

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        //
        // TODO: Streamline the API here.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // The toy language allows variables to be declared implicitly.
        // Walk the AST and declare all implicitly-declared variables.

        // TODO: Scan the bytecode
        let variables = declare_variables(
            int,
            &mut builder,
            &params,
            &the_return,
            bytecode,
            entry_block,
        );

        // Check if all of the functions that are getting
        // called are found to be machine code:

        let mut call_global_all_native = true;
        for instr in bytecode.iter() {
            match instr.op_code {
                OpCode::CALLGLOBAL
                | OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALTAILNOARITY
                | OpCode::CALLGLOBALNOARITY => {
                    let func = globals.get(instr.payload_size.to_usize());
                    if let Some(value) = func {
                        match value {
                            SteelVal::Closure(c) => {
                                if c.0.id.to_string() == name {
                                    // Then this is fine. Generate a trampoline call
                                    // without going through the runtime.
                                    continue;
                                } else if c.0.super_instructions.is_some() {
                                    continue;
                                } else {
                                    call_global_all_native = false;
                                    break;
                                }
                            }

                            SteelVal::BuiltIn(_) => {
                                call_global_all_native = false;
                                break;
                            }

                            _ => {}
                        }
                    }
                }

                OpCode::FUNC | OpCode::TAILCALL => {
                    call_global_all_native = false;
                    break;
                }
                _ => {}
            }
        }

        println!("All native functions: {}", call_global_all_native);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
            instructions: bytecode,
            ip: 0,
            sp: 0,
            globals,
            stack: Vec::new(),
            arity,
            constants,
            local_count: 0,
            patched_locals: Vec::new(),
            function_map: &self.function_map,
            function_context,
            id: func_id,
            native: false,
            call_global_all_native,
            name,
            value_to_local_map: HashMap::new(),
            local_to_value_map: HashMap::new(),
            stack_pointers: Vec::new(),
            let_var_stack: Vec::new(),
        };

        // trans.translate_instructions();

        trans.stack_to_ssa();

        // TODO:
        // for expr in stmts {
        //     trans.translate_expr(expr);
        // }

        // Set up the return variable of the function. Above, we declared a
        // variable to hold the return value. Here, we just do a use of that
        // variable.
        // let return_variable = trans.variables.get(&the_return).unwrap();
        // let return_value = trans.builder.use_var(*return_variable);

        let return_value = trans.builder.ins().iconst(Type::int(8).unwrap(), 1);

        // let return_value = trans.create_i128(1);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum InferredType {
    Int,
    // Is just straight up, unboxed
    UnboxedBool,
    Number,
    Bool,
    List,
    Any,
    Box,
    Void,
}

#[derive(Debug, Clone, Copy)]
struct StackValue {
    value: Value,
    inferred_type: InferredType,
    // Whether or not the value is spilled
    // to the stack
    spilled: bool,
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
    globals: &'a [SteelVal],

    // Local values - whenever things are locally read, we can start using them
    // here.
    stack: Vec<StackValue>,

    sp: usize,

    stack_pointers: Vec<usize>,

    // Local value mapping, can allow
    // us to elide type checks if we have them
    value_to_local_map: HashMap<Value, usize>,

    local_to_value_map: HashMap<usize, InferredType>,

    arity: u16,
    constants: &'a ConstantMap,

    local_count: usize,

    let_var_stack: Vec<usize>,

    patched_locals: Vec<bool>,

    function_map: &'a OwnedFunctionMap,

    function_context: Option<usize>,

    id: FuncId,

    native: bool,

    call_global_all_native: bool,

    name: String,
}

pub fn split_big(a: i128) -> [i64; 2] {
    [(a >> 64) as i64, a as i64]
}

// We should be able to read local values off the stack.
// Functions _will_ be called via reading from the stack, so the locals will be there,
// but any other values pushed to the stack do not necessarily have to get pushed on
// to the stack.

fn op_to_name(op: OpCode) -> &'static str {
    match op {
        OpCode::IF => "if-branch",
        OpCode::CALLGLOBAL => "call-global",
        OpCode::PUSHCONST => "push-const",
        OpCode::READLOCAL0 => "read-local-0",
        OpCode::READLOCAL1 => "read-local-1",
        OpCode::READLOCAL2 => "read-local-2",
        OpCode::READLOCAL3 => "read-local-3",

        OpCode::MOVEREADLOCAL0 => "move-read-local-0",
        OpCode::MOVEREADLOCAL1 => "move-read-local-1",
        OpCode::MOVEREADLOCAL2 => "move-read-local-2",
        OpCode::MOVEREADLOCAL3 => "move-read-local-3",

        OpCode::LOADINT0 => "push-int-0",
        OpCode::LOADINT1 => "push-int-1",
        OpCode::LOADINT2 => "push-int-2",
        OpCode::ADD => "add-binop",
        OpCode::SUB => "sub-binop",
        // OpCode::LT => "lt-binop",
        OpCode::LTE => "lte-binop",
        // OpCode::GT => "gt-binop",
        // OpCode::GTE => "gte-binop",
        OpCode::MUL => "mult-two",
        OpCode::DIV => "div-two",
        _ => panic!("couldn't match the name for the op code"),
    }
}

fn op_to_name_payload(op: OpCode, payload: usize) -> &'static str {
    match (op, payload) {
        (OpCode::IF, _) => "if-branch-value",
        (OpCode::CALLGLOBAL, _) => "call-global",
        (OpCode::PUSHCONST, _) => "push-const",
        (OpCode::READLOCAL0, _) => "read-local-0",
        (OpCode::READLOCAL1, _) => "read-local-1",
        (OpCode::READLOCAL2, _) => "read-local-2",
        (OpCode::READLOCAL3, _) => "read-local-3",

        (OpCode::READLOCAL, _) => "read-local-any",
        (OpCode::MOVEREADLOCAL, _) => "move-read-local-any",

        (OpCode::MOVEREADLOCAL0, _) => "move-read-local-0",
        (OpCode::MOVEREADLOCAL1, _) => "move-read-local-1",
        (OpCode::MOVEREADLOCAL2, _) => "move-read-local-2",
        (OpCode::MOVEREADLOCAL3, _) => "move-read-local-3",

        (OpCode::LOADINT0, _) => "push-int-0",
        (OpCode::LOADINT1, _) => "push-int-1",
        (OpCode::LOADINT2, _) => "push-int-2",
        (OpCode::ADD, 2) => "add-binop",
        (OpCode::SUB, 2) => "sub-binop",
        // (OpCode::LT, 2) => "lt-binop",
        (OpCode::LTE, 2) => "lte-binop",
        // (OpCode::GT, 2) => "gt-binop",
        // (OpCode::GTE, 2) => "gte-binop",
        (OpCode::MUL, 2) => "mult-two",
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
    fn call_global_handler(&mut self) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // This
        // sig.params.push(AbiParam::special(
        //     self.module.target_config().pointer_type(),
        //     codegen::ir::ArgumentPurpose::VMContext,
        // ));

        // Add a parameter for each argument.
        // for _arg in &args {
        //     sig.params.push(AbiParam::new(self.int));
        // }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function("call-global", Linkage::Import, &sig)
            .expect("problem declaring function");

        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");

        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];

        result
    }

    // Load the VM Context
    // Find the offset for the
    // fn move_local(&mut self) -> Value {
    //     let variable = self.variables.get("vm-ctx").expect("variable not defined");
    //     let ctx = self.builder.use_var(*variable);
    //     let sp = self
    //         .builder
    //         .ins()
    //         .load(Type::int(64).unwrap(), MemFlags::trusted(), ctx, 16);

    //     let thread = self.builder.ins().load(
    //         self.module.target_config().pointer_type(),
    //         MemFlags::trusted(),
    //         ctx,
    //         24,
    //     );

    //     todo!()

    //     // is_native
    // }

    // fn current_scope_locals_patched(&self) -> bool {
    //     self.patched_locals.last().copied().unwrap_or_default()
    // }

    // fn patch_up_to(&mut self) {
    //     self.patched_locals.iter_mut().for_each(|x| *x = true);
    // }

    // Read values off of the stack - push them on to
    // wherever they need to go.
    // Assuming the whole instruction set is translated and we also confirm
    // that _only_
    // native functions get used, we can
    // probably just rewrite the function
    // into a function pointer, and we don't need to thread the
    // context through at all.
    fn stack_to_ssa(&mut self) -> bool {
        while self.ip < self.instructions.len() {
            let instr = self.instructions[self.ip];
            let op = instr.op_code;
            println!("{:?} @ {}", op, self.ip);
            let payload = instr.payload_size.to_usize();
            match op {
                OpCode::LOADINT1POP
                | OpCode::BINOPADDTAIL
                | OpCode::NEWSCLOSURE
                | OpCode::TAILCALL => {
                    todo!("{:?}", op);
                }
                OpCode::POPJMP | OpCode::POPPURE => {
                    let last = self.stack.last().unwrap();

                    assert!(!last.spilled);

                    // Push the remaining value back on to the stack.
                    let value = self.pop();

                    // Should break here - just call `handle_pop_pure_value` and
                    // handle the return value / updating
                    // of various things here.

                    // if self.stack.len() > self.arity as _ {
                    // if !self.patched_locals {
                    println!("Stack at vm pop: {:?}", self.stack);
                    for value in self.stack.clone() {
                        if !value.spilled {
                            self.push_to_vm_stack(value.value);
                        }
                    }
                    // } else {
                    //     println!(
                    //         "Already patched locals, Stack at vm pop: {:?} - {}",
                    //         self.stack, self.arity
                    //     );
                    // }

                    self.vm_pop(value.0);

                    self.ip = self.instructions.len() + 1;
                    // self.ip + 1;

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
                    let abi_type = AbiParam::new(Type::int(64).unwrap());

                    let index = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), payload as i64);

                    // Push on to the stack to call
                    self.push(index, InferredType::Int);

                    // TODO: If we know what the global is, and we know that its not mutated,
                    // we probably can do something
                    // interesting here.
                    self.func_ret_val(
                        OpCode::PUSH,
                        payload,
                        1,
                        InferredType::Any,
                        abi_type,
                        AbiParam::new(Type::int(128).unwrap()),
                    );
                }

                // TODO: Still adjust the ip as needed
                OpCode::IF => {
                    // TODO: Type inference here! Change which function is called!
                    let (test, typ) = self.pop();
                    self.value_to_local_map.remove(&test);

                    let false_instr = self.instructions[self.ip].payload_size;
                    let true_instr = self.ip + 1;

                    // TODO: @Matt come back here and adjust IP correctly
                    let test_bool = if typ == InferredType::Bool {
                        let amount_to_shift = self.builder.ins().iconst(Type::int(64).unwrap(), 64);
                        let shift_right = self.builder.ins().sshr(test, amount_to_shift);
                        self.builder
                            .ins()
                            .ireduce(Type::int(8).unwrap(), shift_right)
                    } else {
                        self.call_test_handler(test)
                    };

                    let res =
                        self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());

                    self.push(res, InferredType::Any);
                }
                OpCode::JMP => {
                    self.ip = payload;
                }
                // Call func... lets see how this goes...
                OpCode::FUNC => {
                    let arity = payload;
                    let name = match arity {
                        0 => "call-func-deopt-0",
                        1 => "call-func-deopt-1",
                        2 => "call-func-deopt-2",
                        3 => "call-func-deopt-3",
                        other => todo!(),
                    };

                    let v = self.call_function(arity, name);
                    self.ip += 1;
                    self.push(v, InferredType::Any);
                    self.check_deopt();
                }
                OpCode::SCLOSURE => todo!(),
                OpCode::ECLOSURE => todo!(),
                OpCode::BIND => todo!(),
                OpCode::SDEF => todo!(),
                OpCode::EDEF => todo!(),
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
                OpCode::PASS => todo!(),
                OpCode::NDEFS => todo!(),
                OpCode::PANIC => todo!(),
                OpCode::SET => {
                    let value = self.pop();
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
                    if payload + 1 > self.arity as _ {
                        let locals_to_patch = self.local_count;

                        println!("-----------------");
                        println!("Patching locals: {}", locals_to_patch);
                        println!("Stack at local patching: {}", self.stack.len());
                        println!("payload: {}", payload);
                        println!("stack: {:#?}", self.stack);
                        println!("Arity: {}", self.arity);

                        let upper_bound = payload + 1 - self.arity as usize;
                        println!("upper bound: {}", upper_bound);

                        for i in 0..upper_bound {
                            // for i in 0..(payload + 1 - self.arity as usize) {
                            println!("Read local spill generic");
                            self.spill(i);
                        }
                    }

                    let index = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), payload as i64);

                    let value = self.call_function_returns_value_args(
                        op_to_name_payload(op, payload),
                        &[(index, AbiParam::new(Type::int(64).unwrap()))],
                        AbiParam::new(Type::int(128).unwrap()),
                    );

                    self.value_to_local_map.insert(value, payload);

                    let inferred_type =
                        if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
                            *inferred_type
                        } else {
                            InferredType::Any
                        };

                    self.ip += 1;
                    self.push(value, inferred_type);
                }
                OpCode::PUSHCONST => {
                    let payload = self.instructions[self.ip].payload_size.to_usize();
                    let value = self.call_func_or_immediate(op, payload);
                    self.ip += 1;
                    self.push(value, InferredType::Any);
                }
                OpCode::TRUE => {
                    let constant = SteelVal::BoolV(true);
                    let value = self.create_i128(encode(constant));
                    self.ip += 1;
                    // self.advance_ip();
                    self.push(value, InferredType::Bool);
                }
                OpCode::FALSE => {
                    let constant = SteelVal::BoolV(false);
                    let value = self.create_i128(encode(constant));
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

                    let spilled = self.stack.last().unwrap().spilled;

                    // self.spill(self.stack.len() - 1);

                    let (last, _) = self.pop();

                    *self.let_var_stack.last_mut().unwrap() += 1;

                    if !spilled {
                        self.push_to_vm_stack_let_var(last);
                    }
                }
                OpCode::READLOCAL0
                | OpCode::READLOCAL1
                | OpCode::READLOCAL2
                | OpCode::READLOCAL3
                | OpCode::MOVEREADLOCAL0
                | OpCode::MOVEREADLOCAL1
                | OpCode::MOVEREADLOCAL2
                | OpCode::MOVEREADLOCAL3 => {
                    // these are gonna get spilled anyway?
                    // TODO: Check... if we even need to spill things at all?
                    // We probably don't need to do at all, just need to encode
                    // move semantics into the stack itself to mark that we've
                    // read it?

                    let let_var_offset = self.let_var_stack.last().copied().unwrap_or(0);

                    if payload > self.arity as usize + let_var_offset {
                        let locals_to_patch = self.local_count;

                        println!("-----------------");
                        println!("Patching locals: {}", locals_to_patch);
                        println!("Stack at local patching: {}", self.stack.len());
                        println!("payload: {}", payload);
                        println!("stack: {:?}", self.stack);
                        println!("Arity: {}", self.arity);
                        println!("let var offset: {}", let_var_offset);

                        let upper_bound = payload - self.arity as usize - let_var_offset;

                        println!("Upper bound: {}", upper_bound);

                        for i in 0..upper_bound {
                            // for i in 0..(payload + 1 - self.arity as usize) {
                            // let value = self.stack.remove(i);
                            println!("Read local spill small: {}", self.ip);
                            // self.push_to_vm_stack(value.value);
                            self.spill(i);
                        }

                        println!("Stack after patching: {:#?}", self.stack);

                        // self.patch_up_to();
                    } else {
                        println!("Skipping patching locals: {:?}", self.patched_locals);
                        println!("{:?}", self.stack);
                        println!("arity: {}", self.arity);
                        println!("payload: {}", payload);
                    }

                    // Replace this... with just reading from the vector?
                    let value = self.call_func_or_immediate(op, payload);

                    self.value_to_local_map.insert(value, payload);

                    let inferred_type =
                        if let Some(inferred_type) = self.local_to_value_map.get(&payload) {
                            *inferred_type
                        } else {
                            InferredType::Any
                        };

                    self.ip += 1;
                    self.push(value, inferred_type);
                }
                OpCode::SETLOCAL => todo!(),
                OpCode::COPYCAPTURESTACK => todo!(),
                OpCode::COPYCAPTURECLOSURE => todo!(),
                OpCode::COPYHEAPCAPTURECLOSURE => todo!(),
                OpCode::FIRSTCOPYHEAPCAPTURECLOSURE => todo!(),
                OpCode::TCOJMP => {
                    self.translate_tco_jmp(payload);
                    self.ip = self.instructions.len() + 1;
                    println!("Returning...");
                    return false;
                }
                OpCode::SELFTAILCALLNOARITY => {
                    self.translate_tco_jmp_no_arity(payload);
                    // Jump to out of bounds so signal we're done
                    self.ip = self.instructions.len() + 1;
                    println!("Returning...");
                    return false;
                }
                OpCode::CALLGLOBALTAIL | OpCode::CALLGLOBALTAILNOARITY => {
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();
                    let name = match arity {
                        0 => "call-global-function-tail-deopt-0",
                        1 => "call-global-function-tail-deopt-1",
                        2 => "call-global-function-tail-deopt-2",
                        3 => "call-global-function-tail-deopt-3",
                        other => todo!("{}", other),
                    };

                    // TODO: @mparas
                    // Spill, and then call the function.
                    // for c in self.stack.clone() {
                    //     if !c.spilled {
                    //         self.push_to_vm_stack(c.value);
                    //     }
                    // }

                    // This function pushes back on to the stack, and then we should just
                    // return since we're done now.
                    let v = self.call_global_function(arity, name, function_index, true);

                    self.push(v, InferredType::Any);

                    self.check_deopt();

                    self.ip = self.instructions.len() + 1;
                }
                OpCode::CALLGLOBALNOARITY => {
                    // First - find the index that we have to lookup.
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();
                    let name = match arity {
                        0 => "call-global-function-deopt-no-arity-0",
                        1 => "call-global-function-deopt-no-arity-1",
                        2 => "call-global-function-deopt-no-arity-2",
                        3 => "call-global-function-deopt-no-arity-3",
                        other => todo!("{}", other),
                    };

                    let result = self.call_global_function(arity, name, function_index, false);

                    // Assuming this worked, we'll want to push this result on to the stack.
                    self.push(result, InferredType::Any);

                    // Then, we're gonna check the result and see if we should deopt
                    self.check_deopt();
                }
                OpCode::CALLGLOBAL => {
                    // First - find the index that we have to lookup.
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();
                    let name = match arity {
                        0 => "call-global-function-deopt-0",
                        1 => "call-global-function-deopt-1",
                        2 => "call-global-function-deopt-2",
                        3 => "call-global-function-deopt-3",
                        other => todo!("{}", other),
                    };

                    let result = self.call_global_function(arity, name, function_index, false);

                    // Assuming this worked, we'll want to push this result on to the stack.
                    self.push(result, InferredType::Any);

                    // Then, we're gonna check the result and see if we should deopt
                    self.check_deopt();
                }
                OpCode::READCAPTURED => todo!(),

                // Begin scope means we're starting
                // a let scope, which means we'll have some amount
                // of values that are retained on the stack for
                // further usage.
                OpCode::BEGINSCOPE => {
                    self.patched_locals.push(false);
                    self.stack_pointers.push(self.local_count);

                    self.let_var_stack.push(0);

                    for arg in 0..self.stack.len() {
                        self.spill(arg);
                    }

                    // let count = self.stack_pointers.len();

                    // Next n values should stick around on the stack.
                    for instr in &self.instructions[self.ip..] {
                        if instr.op_code == OpCode::LETENDSCOPE {
                            self.local_count = instr.payload_size.to_usize();
                            println!("Entering scope: {}", self.local_count);
                            break;
                        }
                    }

                    println!("Calling begin scope");

                    // self.begin_scope();

                    self.ip += 1;
                }
                OpCode::LETENDSCOPE => {
                    self.local_count = payload;
                    self.ip += 1;

                    self.let_var_stack.pop();

                    if self.stack.len() > self.arity as _ {
                        println!("Let end scope: {}", payload);
                        println!("Stack length: {}", self.stack.len());
                        println!("Arity: {}", self.arity);

                        // let last = self
                        //     .stack_pointers
                        //     .pop()
                        //     .unwrap()
                        //     .saturating_sub(self.arity as usize);

                        // let spilled = self
                        //     .stack
                        //     .drain(last..self.stack.len() - 1)
                        //     .collect::<Vec<_>>();

                        // for c in spilled {
                        //     if !c.spilled {
                        //         println!("-------------------> Spilling in let end scope: {:?}", c);
                        //         self.push_to_vm_stack(c.value);
                        //     }
                        // }
                        println!("Stack after let end scope: {:#?}", self.stack);
                    }

                    dbg!(self.local_count);

                    self.call_end_scope_handler(payload);

                    self.patched_locals.pop();
                    self.stack_pointers.pop();
                }
                OpCode::PUREFUNC => todo!(),

                OpCode::SUB
                    if payload == 2
                        && self.stack.last().map(|x| x.inferred_type)
                            == Some(InferredType::Int) =>
                {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    // Call the func
                    self.func_ret_val_named(
                        "sub-binop-int",
                        payload,
                        2,
                        InferredType::Number,
                        abi_type,
                        abi_type,
                    );
                }

                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    // Call the func
                    self.func_ret_val(op, payload, 2, InferredType::Number, abi_type, abi_type);

                    self.check_deopt();
                }

                OpCode::LTE
                    if payload == 2
                        && self.stack.last().map(|x| x.inferred_type)
                            == Some(InferredType::Int) =>
                {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val_named(
                        "lte-binop-int",
                        payload,
                        2,
                        InferredType::Bool,
                        abi_type,
                        abi_type,
                    );
                }

                OpCode::LTE if payload == 2 => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val_named(
                        "lte-binop",
                        payload,
                        2,
                        InferredType::Bool,
                        abi_type,
                        abi_type,
                    );
                }

                OpCode::EQUAL2 => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 2, 2, InferredType::Bool, abi_type, abi_type);
                }
                OpCode::EQUAL
                | OpCode::NUMEQUAL
                | OpCode::LTE
                | OpCode::GTE
                | OpCode::GT
                | OpCode::LT => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, payload, 2, InferredType::Bool, abi_type, abi_type);
                }
                OpCode::NULL => {
                    // todo!()
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val_named(
                        "null-handler",
                        1,
                        2,
                        InferredType::Bool,
                        abi_type,
                        abi_type,
                    );
                }
                OpCode::CONS => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 2, 2, InferredType::List, abi_type, abi_type);
                }
                OpCode::CDR => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 1, 2, InferredType::List, abi_type, abi_type);
                }
                OpCode::LIST => todo!(),
                OpCode::CAR => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());

                    if let Some(last) = self.stack.last() {
                        // Check if that value makes sense
                        if let Some(from_local) = self.value_to_local_map.get(&last.value) {
                            self.local_to_value_map
                                .insert(*from_local, InferredType::List);
                        }
                    }

                    self.func_ret_val(op, 1, 2, InferredType::Any, abi_type, abi_type);
                }
                OpCode::NEWBOX => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 1, 2, InferredType::Box, abi_type, abi_type);
                }
                OpCode::SETBOX => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 2, 2, InferredType::Any, abi_type, abi_type);
                }
                OpCode::UNBOX => {
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 1, 2, InferredType::Any, abi_type, abi_type);
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
                OpCode::NOT => {
                    // Do the thing.
                    let abi_type = AbiParam::new(Type::int(128).unwrap());
                    self.func_ret_val(op, 1, 2, InferredType::Bool, abi_type, abi_type);
                }
                OpCode::VEC => todo!(),
                OpCode::Apply => todo!(),
                OpCode::LOADINT0POP => todo!(),
                OpCode::LOADINT2POP => todo!(),
                OpCode::CaseLambdaDispatch => todo!(),
                OpCode::CGLOCALCONST => todo!(),
                OpCode::READLOCAL0CALLGLOBAL => todo!(),
                OpCode::READLOCAL1CALLGLOBAL => todo!(),
                OpCode::LISTREF => todo!(),
                OpCode::VECTORREF => todo!(),
                OpCode::NULLIF => todo!(),
                OpCode::UNBOXCALL => todo!(),
                OpCode::UNBOXTAIL => todo!(),
                OpCode::EQUALCONST => todo!(),
                OpCode::FUNCNOARITY => todo!(),
                OpCode::TAILCALLNOARITY => todo!(),
            }
        }

        return true;
    }

    fn call_set(&mut self, index: usize, value: Value) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        let p = AbiParam::new(codegen::ir::Type::int(128).unwrap());
        sig.params.push(p);

        let callee = self
            .module
            .declare_function("drop-value", Linkage::Import, &sig)
            .expect("problem declaring function");

        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

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
        let last = self.pop();

        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        let p = AbiParam::new(codegen::ir::Type::int(128).unwrap());
        sig.params.push(p);

        let callee = self
            .module
            .declare_function("drop-value", Linkage::Import, &sig)
            .expect("problem declaring function");

        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = vec![ctx, last.0];

        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn pop_value_from_vm_stack(&mut self) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        let p = AbiParam::new(codegen::ir::Type::int(128).unwrap());
        // sig.params.push(p);

        sig.returns.push(p);

        let callee = self
            .module
            .declare_function("pop-from-stack", Linkage::Import, &sig)
            .expect("problem declaring function");

        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = vec![ctx];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    /// Call a function by its name. If this function has been registered in the builder
    fn call_function_by_name(
        &mut self,
        name: &str,
        args: impl Iterator<Item = Value>,
    ) -> (Value, InferredType) {
        let obj = self.function_map.map.get(name).unwrap();
        let sig = obj.to_cranelift(&self.module);

        let callee = self
            .module
            .declare_function(name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

        // Unfortunate allocation here.
        let args = [ctx].into_iter().chain(args).collect::<Vec<_>>();

        let call = self.builder.ins().call(local_callee, &args);

        let result = self.builder.inst_results(call)[0];

        (
            result,
            *self.function_map.return_type_hints.get(name).unwrap(),
        )
    }

    fn call_test_handler(&mut self, test_value: Value) -> Value {
        // This is the call global `call_global_function_deopt`
        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // test
        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // Return value
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));

        let callee = self
            .module
            .declare_function("if-branch-value", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

        // Advance to the next thing
        self.ip += 1;

        let arg_values = [ctx, test_value];

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    // Let end scope handler - should instead pass the values in directly.
    fn call_end_scope_handler(&mut self, amount: usize) {
        // This is the call global `call_global_function_deopt`
        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        let name = "let-end-scope-c";

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

        let amount_to_drop = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), amount as i64);

        let arg_values = [ctx, amount_to_drop];
        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    // Call the function directly via the trampoline
    // fn call_trampoline(&mut self, arity: usize, function_index: usize) -> Value {
    //     let mut sig = self.module.make_signature();

    //     // VmCore pointer
    //     sig.params
    //         .push(AbiParam::new(self.module.target_config().pointer_type()));

    //     // Arity
    //     sig.params.push(AbiParam::new(Type::int(64).unwrap()));

    //     // lookup index
    //     sig.params.push(AbiParam::new(Type::int(64).unwrap()));

    //     // for _ in 0..arity {
    //     //     sig.params.push(AbiParam::new(Type::int(128).unwrap()));
    //     // }

    //     sig.returns.push(AbiParam::new(Type::int(128).unwrap()));

    //     let callee = self
    //         .module
    //         .declare_function("trampoline", Linkage::Import, &sig)
    //         .expect("problem declaring function");
    //     let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

    //     let variable = self.variables.get("vm-ctx").expect("variable not defined");
    //     let ctx = self.builder.use_var(*variable);

    //     let lookup_index = self
    //         .builder
    //         .ins()
    //         .iconst(Type::int(64).unwrap(), function_index as i64);

    //     let arity_value = self
    //         .builder
    //         .ins()
    //         .iconst(Type::int(64).unwrap(), arity as i64);

    //     self.ip += 1;

    //     let arg_values = vec![ctx, arity_value, lookup_index];

    //     // TODO: Revisit when bringing trampoline back
    //     let spilled = self
    //         .stack
    //         .drain(self.stack.len() - arity..)
    //         .collect::<Vec<_>>();

    //     if spilled.len() == 2 {
    //         let mut iter = spilled.into_iter();

    //         let first = iter.next().unwrap();
    //         let second = iter.next().unwrap();

    //         self.value_to_local_map.remove(&first.value);
    //         self.value_to_local_map.remove(&second.value);

    //         self.push_to_vm_stack_two(first.value, second.value);
    //     } else {
    //         for value in spilled {
    //             self.value_to_local_map.remove(&value.value);

    //             self.push_to_vm_stack(value.value);
    //         }
    //     }

    //     let call = self.builder.ins().call(local_callee, &arg_values);
    //     let result = self.builder.inst_results(call)[0];
    //     result

    //     // Just
    //     // arg_values.extend(self.stack.drain(self.stack.len() - arity..).map(|x| x.0));
    // }

    // fn call_trampoline_no_arity(&mut self, arity: usize, function_index: usize) -> Value {
    //     let mut sig = self.module.make_signature();

    //     // VmCore pointer
    //     sig.params
    //         .push(AbiParam::new(self.module.target_config().pointer_type()));

    //     // lookup index
    //     sig.params.push(AbiParam::new(Type::int(64).unwrap()));

    //     // for _ in 0..arity {
    //     //     sig.params.push(AbiParam::new(Type::int(128).unwrap()));
    //     // }

    //     sig.returns.push(AbiParam::new(Type::int(128).unwrap()));

    //     let callee = self
    //         .module
    //         .declare_function("trampoline-no-arity", Linkage::Import, &sig)
    //         .expect("problem declaring function");
    //     let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

    //     let variable = self.variables.get("vm-ctx").expect("variable not defined");
    //     let ctx = self.builder.use_var(*variable);

    //     let lookup_index = self
    //         .builder
    //         .ins()
    //         .iconst(Type::int(64).unwrap(), function_index as i64);

    //     self.ip += 1;

    //     let arg_values = vec![ctx, lookup_index];

    //     // TODO: Revisit when bringing trampoline back
    //     let spilled = self
    //         .stack
    //         .drain(self.stack.len() - arity..)
    //         .collect::<Vec<_>>();

    //     if spilled.len() == 2 {
    //         let mut iter = spilled.into_iter();

    //         let first = iter.next().unwrap();
    //         let second = iter.next().unwrap();

    //         self.value_to_local_map.remove(&first.value);
    //         self.value_to_local_map.remove(&second.value);

    //         self.push_to_vm_stack_two(first.value, second.value);
    //     } else {
    //         for value in spilled {
    //             self.value_to_local_map.remove(&value.value);

    //             self.push_to_vm_stack(value.value);
    //         }
    //     }

    //     let call = self.builder.ins().call(local_callee, &arg_values);
    //     let result = self.builder.inst_results(call)[0];
    //     result

    //     // Just
    //     // arg_values.extend(self.stack.drain(self.stack.len() - arity..).map(|x| x.0));
    // }

    fn translate_tco_jmp(&mut self, payload: usize) {
        // Translate to a while loop with calls to destructors?
        // Or just use the normal jump, where we jump to the
        // top of the instruction stack and reinvoke
        // the dyn super instruction?

        let args = self.split_off(payload);

        println!("Stack remaining at tco jump: {:?}", self.stack);

        for value in &self.stack {
            assert!(value.spilled);
        }

        // Translate to a while loop with calls to destructors?
        // Or just use the normal jump, where we jump to the
        // top of the instruction stack and reinvoke
        // the dyn super instruction?
        for c in args.chunks(2) {
            for v in c {
                self.value_to_local_map.remove(&v.0);
            }

            if c.len() == 2 {
                self.push_to_vm_stack_two(c[0].0, c[1].0);
            } else {
                self.push_to_vm_stack(c[0].0);
            }
        }

        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // arity
        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        let callee = self
            .module
            .declare_function("tco-jump", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let arg_values = [ctx, arity];
        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn translate_tco_jmp_no_arity(&mut self, payload: usize) {
        // Translate to a while loop with calls to destructors?
        // Or just use the normal jump, where we jump to the
        // top of the instruction stack and reinvoke
        // the dyn super instruction?
        println!("Stack at self tco jump: {:?}", self.stack);

        let args = self.split_off(payload);

        println!("Stack remaining at self tco jump: {:?}", self.stack);

        if self.stack.len() > self.arity as _ {
            for value in std::mem::take(&mut self.stack) {
                if !value.spilled {
                    self.push_to_vm_stack(value.value);
                }
            }
        }

        // Its just the arity that we want, not the full stack.
        for c in args.chunks(2) {
            if c.len() == 2 {
                self.push_to_vm_stack_two(c[0].0, c[1].0);

                self.value_to_local_map.remove(&c[0].0);
                self.value_to_local_map.remove(&c[1].0);
            } else {
                self.push_to_vm_stack(c[0].0);
                self.value_to_local_map.remove(&c[0].0);
            }
        }

        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // arity
        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        let callee = self
            .module
            .declare_function("self-tail-call", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let arg_values = [ctx, arity];
        let call = self.builder.ins().call(local_callee, &arg_values);

        // let ret = self.builder.ins().iconst(Type::int(8).unwrap(), 1);
        // self.builder.ins().return_(&[ret]);
    }

    fn call_function(&mut self, arity: usize, name: &str) -> Value {
        // This is the call global `call_global_function_deopt`
        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // Function
        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // instruction pointer
        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        for _ in 0..arity {
            sig.params.push(AbiParam::new(Type::int(128).unwrap()));
        }

        // Return value
        sig.returns.push(AbiParam::new(Type::int(128).unwrap()));

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

        let fallback_ip = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), self.ip as i64);

        let func = self.pop().0;

        let mut arg_values = vec![ctx, func, fallback_ip];
        arg_values.extend(
            self.stack
                .drain(self.stack.len() - arity..)
                .map(|x| x.value),
        );

        // Check if its a function - otherwise, just spill the values to the stack.
        let is_function = self.check_callable(func);

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

            self.builder.ins().jump(merge_block, &[then_return]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            // Spilling...
            // if !self.patched_locals.last().copied().unwrap_or_default() {
            println!(
                "Patching locals in function call: {:?} - {}",
                self.stack, self.local_count
            );
            for c in self.stack.clone() {
                if !c.spilled {
                    self.push_to_vm_stack(c.value);
                }
            }
            // }

            let else_return = BlockArg::Value(self.create_i128(0));

            self.builder.ins().jump(merge_block, &[else_return]);

            // Switch to the merge block for subsequent statements.
            self.builder.switch_to_block(merge_block);

            // We've now seen all the predecessors of the merge block.
            self.builder.seal_block(merge_block);
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
        // TODO: Call trampoline, but include the arity check as well
        // let func = self.globals.get(function_index).unwrap();
        // if let SteelVal::Closure(c) = func {
        //     if c.id.to_string() == self.name {
        //         println!("Calling trampoline instead of delegating to the runtime");

        //         if name.contains("no-arity") {
        //             return self.call_trampoline_no_arity(arity, function_index);
        //         } else {
        //             return self.call_trampoline(arity, function_index);
        //         }
        //     }
        // }

        // This is the call global `call_global_function_deopt`
        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // lookup index
        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        // instruction pointer
        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        for _ in 0..arity {
            sig.params.push(AbiParam::new(Type::int(128).unwrap()));
        }

        // Return value
        sig.returns.push(AbiParam::new(Type::int(128).unwrap()));

        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

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
        let mut args_off_the_stack = self
            .stack
            .drain(self.stack.len() - arity..)
            .collect::<Vec<_>>();

        dbg!(&args_off_the_stack);

        self.maybe_patch_from_stack(&mut args_off_the_stack);

        dbg!(&args_off_the_stack);

        for arg in &args_off_the_stack {
            assert!(!arg.spilled);
        }

        assert_eq!(args_off_the_stack.len(), arity);

        arg_values.extend(args_off_the_stack.iter().map(|x| x.value));

        // Check if its a function - otherwise, just spill the values to the stack.
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

            self.builder.ins().jump(merge_block, &[then_return]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            // if !self.current_scope_locals_patched() {

            // TODO: @matt
            // Investigate for call globals if we need to do this?
            // Spilling on the stack here seems suspect? Do we have to
            // spill the _whole_ stack? Or just part of it?
            println!(
                "-------- Fall back pushing to stack in tail call: {:#?} ---------",
                self.stack
            );
            println!("Stack length: {}", self.stack.len());
            println!("Stack pointer: {}", self.local_count);

            for c in self.stack.clone() {
                if !c.spilled {
                    println!("Spilling...: {}", c.value);
                    self.push_to_vm_stack_function_spill(c.value);
                }
            }

            // Only spill what hasn't been spilled already?
            // for value in self.stack.clone() {
            //     self.push_to_vm_stack(value.0);
            // }

            let else_return = BlockArg::Value(self.create_i128(0));

            self.builder.ins().jump(merge_block, &[else_return]);

            // Switch to the merge block for subsequent statements.
            self.builder.switch_to_block(merge_block);

            // We've now seen all the predecessors of the merge block.
            self.builder.seal_block(merge_block);
        }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn check_callable(&mut self, callable: Value) -> Value {
        let mut sig = self.module.make_signature();
        let name = "check-callable-value";
        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.params.push(AbiParam::new(Type::int(128).unwrap()));
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let call = self.builder.ins().call(local_callee, &[ctx, callable]);
        let result = self.builder.inst_results(call)[0];

        return result;
    }

    fn check_function(&mut self, index: Value, tail: bool) -> Value {
        let mut sig = self.module.make_signature();
        let name = if tail {
            "check-callable-tail"
        } else {
            "check-callable"
        };
        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.params.push(AbiParam::new(Type::int(64).unwrap()));
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let call = self.builder.ins().call(local_callee, &[ctx, index]);
        let result = self.builder.inst_results(call)[0];

        return result;
    }

    fn check_deopt_call(&mut self) -> Value {
        let mut sig = self.module.make_signature();
        let name = "vm-should-continue?";
        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let call = self.builder.ins().call(local_callee, &[ctx]);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn check_deopt_ptr_load(&mut self) -> Value {
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let is_native = self
            .builder
            .ins()
            .load(Type::int(8).unwrap(), MemFlags::trusted(), ctx, 0);

        is_native
    }

    fn advance_ip(&mut self) {
        let mut sig = self.module.make_signature();
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));
        let callee = self
            .module
            .declare_function("vm-increment-ip", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let call = self.builder.ins().call(local_callee, &[ctx]);
    }

    // Advance the pointer
    fn begin_scope(&mut self) {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        //
        // for (_, p) in args {
        // AbiParam::new(codegen::ir::Type::int(128).unwrap())
        // sig.params.push(*p);
        // }

        // For simplicity for now, just make all calls return a single I64.
        // sig.returns.push(return_type);

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function("vm-increment-ip", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

        // let mut arg_values = Vec::new();

        // let args = args.iter().map(|x| )

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &[ctx]);
    }

    fn check_deopt(&mut self) -> Value {
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
        let then_return = BlockArg::Value(self.builder.ins().iconst(Type::int(8).unwrap(), 1));
        // Just... translate instructions?
        self.stack_to_ssa();
        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        // Set the IP to the new spot:
        // {
        //     self.set_ctx_ip(self.ip);
        // }

        // // TODO: Update with the proper return value
        let else_return = self.builder.ins().iconst(Type::int(8).unwrap(), 1);
        // let else_return = self.create_i128(0);
        self.builder.ins().return_(&[else_return]);
        self.builder.switch_to_block(merge_block);
        // // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);
        // // Read the value of the if-else by reading the merge block
        // // parameter.
        let phi = self.builder.block_params(merge_block)[0];
        phi
    }

    fn func_ret_val_named(
        &mut self,
        function_name: &str,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
        abi_param_type: AbiParam,
        abi_return_type: AbiParam,
    ) {
        let args = self.split_off(payload);

        for arg in &args {
            self.value_to_local_map.remove(&arg.0);
        }

        println!("Args at {}: {:#?}", function_name, args);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args
            .into_iter()
            .map(|x| (x.0, abi_param_type))
            .collect::<Vec<_>>();

        let result =
            self.call_function_returns_value_args_no_context(function_name, &args, abi_return_type);

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);
        self.ip += ip_inc;
    }

    fn spill(&mut self, index: usize) -> Option<()> {
        let guard = self.stack.get_mut(index)?;
        let mut spilled = false;
        if !guard.spilled {
            guard.spilled = true;
            spilled = true;
        }

        if spilled {
            println!("Spilling in spill: {:?}", self.stack[index]);
            self.push_to_vm_stack(self.stack[index].value);
        }

        Some(())
    }

    // TODO: For spilling to the stack, we just _have_ to spill in order.
    // We have a cursor which will go through and mark if the value has already been pushed to the stack.
    // As long as we push the values in the right order, we're good.
    fn push(&mut self, value: Value, typ: InferredType) {
        self.stack.push(StackValue {
            value,
            inferred_type: typ,
            spilled: false,
        })
    }

    fn pop(&mut self) -> (Value, InferredType) {
        let last = self.stack.pop().unwrap();

        assert!(!last.spilled);

        self.value_to_local_map.remove(&last.value);
        (last.value, last.inferred_type)
    }

    fn maybe_patch_from_stack(&mut self, args_off_the_stack: &mut Vec<StackValue>) {
        let mut indices_to_get_from_shadow_stack = Vec::new();

        for (idx, arg) in args_off_the_stack.iter().enumerate() {
            if arg.spilled {
                indices_to_get_from_shadow_stack.push(idx);
            }
        }

        dbg!(&indices_to_get_from_shadow_stack);

        for idx in indices_to_get_from_shadow_stack.iter().rev() {
            let value = self.pop_value_from_vm_stack();

            args_off_the_stack[*idx] = StackValue {
                value,
                inferred_type: InferredType::Any,
                spilled: false,
            };
        }
    }

    fn split_off(&mut self, payload: usize) -> Vec<(Value, InferredType)> {
        let mut args = self.stack.split_off(self.stack.len() - payload);

        // Patch the args if needed
        for arg in &args {
            self.value_to_local_map.remove(&arg.value);
        }

        self.maybe_patch_from_stack(&mut args);

        args.into_iter()
            .map(|x| (x.value, x.inferred_type))
            .collect()
    }

    fn drain(&mut self) {
        todo!()
    }

    fn func_ret_val(
        &mut self,
        op: OpCode,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
        abi_param_type: AbiParam,
        abi_return_type: AbiParam,
    ) {
        println!("Func ret val: {:?} - {}", op, payload);
        let function_name = op_to_name_payload(op, payload);
        let args = self.split_off(payload);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args
            .into_iter()
            .map(|x| (x.0, abi_param_type))
            .collect::<Vec<_>>();

        let result = self.call_function_returns_value_args(function_name, &args, abi_return_type);

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);

        self.ip += ip_inc;
    }

    fn translate_instructions(&mut self) -> bool {
        // Fuse instructions together to avoid any dispatch cost.
        loop {
            // Get a window of 4 instructions just to see if this works:
            fn is_local_stack_pushable(op: OpCode) -> bool {
                use OpCode::*;
                matches!(
                    op,
                    PUSHCONST
                        | LOADINT0
                        | LOADINT1
                        | LOADINT2
                        | READLOCAL0
                        | READLOCAL1
                        | READLOCAL2
                        | READLOCAL3
                )
            }

            if self.ip + 4 < self.instructions.len() {
                let chain = &self.instructions[self.ip..self.ip + 4];
                match chain {
                    &[DenseInstruction {
                        op_code: op1,
                        payload_size: p1,
                    }, DenseInstruction {
                        op_code: op2,
                        payload_size: p2,
                    }, DenseInstruction {
                        op_code: op3,
                        payload_size: p3,
                    }, DenseInstruction {
                        op_code: OpCode::CALLGLOBALTAIL,
                        payload_size: funcp,
                    }] if is_local_stack_pushable(op1)
                        && is_local_stack_pushable(op2)
                        && is_local_stack_pushable(op3) =>
                    {
                        println!("--- Entering the special case ---");

                        let value1 = self.call_func_or_immediate(op1, p1.to_usize());
                        let value2 = self.call_func_or_immediate(op2, p2.to_usize());
                        let value3 = self.call_func_or_immediate(op3, p3.to_usize());
                        self.ip += 3;

                        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
                        let mut sig = self.module.make_signature();
                        let name = "call-global-tail-3-test";

                        sig.params
                            .push(AbiParam::new(self.module.target_config().pointer_type()));

                        // Function
                        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

                        // Three args
                        sig.params.push(AbiParam::new(Type::int(128).unwrap()));
                        sig.params.push(AbiParam::new(Type::int(128).unwrap()));
                        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

                        // offset
                        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

                        // For simplicity for now, just make all calls return a single I64.
                        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));

                        // TODO: Streamline the API here?
                        let callee = self
                            .module
                            .declare_function(&name, Linkage::Import, &sig)
                            .expect("problem declaring function");
                        let local_callee =
                            self.module.declare_func_in_func(callee, self.builder.func);

                        // let mut arg_values = Vec::new();

                        let variable = self.variables.get("vm-ctx").expect("variable not defined");
                        let ctx = self.builder.use_var(*variable);

                        let func = self.globals.get(funcp.to_usize()).unwrap().clone();

                        // Keep alive, forever?
                        let clone = func.clone();
                        std::mem::forget(clone);

                        let func_var = self.create_i128(encode(func));

                        let offset = self.builder.ins().iconst(Type::int(64).unwrap(), 3);

                        let arg_values = [ctx, func_var, value1, value2, value3, offset];

                        // for arg in args {
                        //     arg_values.push(self.translate_expr(arg))
                        // }
                        let call = self.builder.ins().call(local_callee, &arg_values);
                        let result = self.builder.inst_results(call)[0];
                        break;
                        // result

                        // Call global tail:
                        // "call-global-tail-3"
                    }

                    _ => {}
                }
            }

            match self.instructions[self.ip].op_code {
                OpCode::CALLGLOBALTAIL
                | OpCode::POPJMP
                | OpCode::POPPURE
                | OpCode::LOADINT1POP
                | OpCode::BINOPADDTAIL
                | OpCode::NEWSCLOSURE
                | OpCode::TAILCALL => {
                    return true;
                }

                // TODO: Figure out how to seamlessly
                // use either the local stack values, or the VM stack values,
                // or both. Calling a function should attempt to prefer
                // to use the local stack values instead - that way
                // we can avoid writing to the stack?
                /*

                OpCode::PUSHCONST => {
                    // Read off of the stack... where relevant?
                    let value = self.call_function_returns_value("push-const");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::LOADINT0 => {
                    let value = self.call_function_returns_value("push-int-0");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::LOADINT1 => {
                    let value = self.call_function_returns_value("push-int-1");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::LOADINT2 => {
                    let value = self.call_function_returns_value("push-int-2");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::READLOCAL0 => {
                    let value = self.call_function_returns_value("read-local-0");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::READLOCAL1 => {
                    let value = self.call_function_returns_value("read-local-1");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::READLOCAL2 => {
                    let value = self.call_function_returns_value("read-local-2");
                    self.stack.push(value);
                    self.ip += 1;
                }

                OpCode::READLOCAL3 => {
                    let value = self.call_function_returns_value("read-local-3");
                    self.stack.push(value);
                    self.ip += 1;
                }

                */
                OpCode::CALLGLOBAL => {
                    println!("{:?}", OpCode::CALLGLOBAL);
                    // Check if the function we're going to call is actually
                    // the right value first.

                    // Just... close the value over here?
                    // Grab the function, embed it, register the symbol, call it a day.
                    // Would save a lot of hassle with calling the function since we literally
                    // have the pointer right here?
                    let func = self
                        .globals
                        .get(self.instructions[self.ip].payload_size.to_usize())
                        .unwrap();

                    // Just bail out if its _not_ a native function.
                    match func {
                        SteelVal::Closure(_) | SteelVal::ContinuationFunction(_) => break,
                        _ => {}
                    };

                    // Call the global, but actually check the return value?
                    let result = self.call_global_handler();
                    self.ip += OpCode::CALLGLOBAL.width().unwrap();

                    // Return early otherwise:

                    let then_block = self.builder.create_block();
                    let else_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();

                    // // If-else constructs in the toy language have a return value.
                    // // In traditional SSA form, this would produce a PHI between
                    // // the then and else bodies. Cranelift uses block parameters,
                    // // so set up a parameter in the merge block, and we'll pass
                    // // the return values to it from the branches.
                    self.builder.append_block_param(merge_block, self.int);

                    // 0 return value -> continue?
                    // let compare = self
                    //     .builder
                    //     .ins()
                    //     .iconst(codegen::ir::Type::int(8).unwrap(), 1);

                    // let test = self.builder.ins().icmp(IntCC::Equal, compare, result);

                    // // Test the if condition and conditionally branch.
                    self.builder
                        .ins()
                        .brif(result, then_block, &[], else_block, &[]);

                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);

                    // // Update with the proper return value
                    let mut then_return =
                        BlockArg::Value(self.builder.ins().iconst(Type::int(8).unwrap(), 1));

                    // // Set the ip to the right spot:
                    // self.ip = then_start;
                    self.translate_instructions();

                    // Jump to the merge block, passing it the block return value.
                    self.builder.ins().jump(merge_block, &[then_return]);

                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);

                    // // TODO: Update with the proper return value
                    let mut else_return = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

                    self.builder.ins().return_(&[else_return]);

                    // self.ip = else_start;
                    // self.translate_instructions();

                    // // Jump to the merge block, passing it the block return value.

                    // // Switch to the merge block for subsequent statements.
                    self.builder.switch_to_block(merge_block);

                    // // We've now seen all the predecessors of the merge block.
                    self.builder.seal_block(merge_block);

                    // // Read the value of the if-else by reading the merge block
                    // // parameter.
                    let phi = self.builder.block_params(merge_block)[0];

                    // phi
                }

                // OpCode::JMP => {
                //     todo!()
                // }

                // Figure out... how to handle branching?
                OpCode::IF => {
                    // Make the block for each case, somehow figuring out which
                    // is true / false
                    let false_instr = self.instructions[self.ip].payload_size;
                    let true_instr = self.ip + 1;

                    let value = self.call_branch_handler();
                    self.translate_if_else(value, true_instr, false_instr.to_usize());

                    break;
                }

                op => {
                    let result = self.call_handler(op);
                    // Move forward by whatever the offset is:
                    let width = op.width();

                    println!("{:?}", op);

                    if let Some(width) = width {
                        self.ip += width;
                    } else {
                        panic!("Can't translate the width for: {:?}", op);
                    }
                    // Offset the instructions:
                    // match op {
                    // }
                    // todo!()
                    // }

                    // Lookup the associated handler function via the name

                    // Insert a call to the handler function, pass the handler in
                    // continue on with our lives?
                }
            }
        }

        return false;
    }

    fn call_func_or_immediate(&mut self, op1: OpCode, payload: usize) -> Value {
        match op1 {
            OpCode::LOADINT0 => self.create_i128(encode(SteelVal::INT_ZERO)),
            OpCode::LOADINT1 => self.create_i128(encode(SteelVal::INT_ONE)),
            OpCode::LOADINT2 => self.create_i128(encode(SteelVal::INT_TWO)),
            OpCode::PUSHCONST => {
                // Attempt to inline the constant, if it is something that can be inlined.
                // Assuming we know the type of it, we can get really fancy here since
                // we _should_ be able to do something with it if there are other types
                // in play - we can avoid the unboxing / boxing of the type if we know
                // what we're dealing with.

                let constant = self.constants.get(payload);

                match &constant {
                    SteelVal::BoolV(_) | SteelVal::IntV(_) => {
                        println!("Embedding immediate: {}", constant);

                        // self.advance_ip();

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

        let lhs = self.builder.ins().iconst(int, left);
        let rhs = self.builder.ins().iconst(int, right);

        // TODO:
        self.builder.ins().iconcat(rhs, lhs)
    }

    fn translate_if_else_value(
        &mut self,
        condition_value: Value,
        then_start: usize,
        else_start: usize,
    ) -> Value {
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
        self.builder.seal_block(then_block);

        // Update with the proper return value
        // let mut then_return = self
        //     .builder
        //     .ins()
        //     .iconst(codegen::ir::Type::int(128).unwrap(), 1);

        // Set the ip to the right spot:
        self.ip = then_start;

        let local_count = self.local_count;
        let frozen_patched = self.patched_locals.clone();
        let frozen_stack = self.stack.clone();

        self.stack_to_ssa();
        // println!("Done on then");
        // println!("Stack after then branch: {:?}", self.stack);

        // Unwrap or... must have been a tail call?
        let then_return = BlockArg::Value(
            // TODO: Replace this stack pop with the right one
            self.stack
                .pop()
                .map(|x| {
                    let value = x.value;
                    self.value_to_local_map.remove(&value);
                    value
                })
                // .unwrap(),
                .unwrap_or_else(|| dbg!(self.create_i128(encode(SteelVal::Void)))),
            // self.stack
            //     .pop()
            //     .map(|x| {
            //         let value = x.value;
            //         self.value_to_local_map.remove(&value);
            //         value
            //     })
            // self.create_i128(encode(SteelVal::Void)),
        );

        // let then_return = self.stack.pop().unwrap().0;
        // .map(|x| x.0)
        // .unwrap_or(self.create_i128(encode(SteelVal::Void)));

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        // TODO: Update with the proper return value
        // let mut else_return; = self
        //     .builder
        //     .ins()
        //     .iconst(codegen::ir::Type::int(8).unwrap(), 128);

        self.ip = else_start;

        self.local_count = local_count;
        self.patched_locals = frozen_patched;
        self.stack = frozen_stack;
        self.stack_to_ssa();

        // let else_return = self.stack.pop().unwrap().0;
        let else_return = BlockArg::Value(
            self.stack
                .pop()
                .map(|x| {
                    let value = x.value;
                    self.value_to_local_map.remove(&value);
                    value
                })
                // .unwrap(),
                .unwrap_or_else(|| dbg!(self.create_i128(encode(SteelVal::Void)))),
            // self.create_i128(encode(SteelVal::Void)),
        );

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

    // TODO: Actually store local variables on the stack!
    // fn push_alloc(&mut self, val: Value) {
    //     self.builder
    //         .ins()
    //         .stack_store(val, self.allocs, self.curr_allocs as i32 * 8);
    //     self.curr_allocs += 1;
    // }

    fn translate_if_else(
        &mut self,
        condition_value: Value,
        then_start: usize,
        else_start: usize,
    ) -> Value {
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
        self.builder.seal_block(then_block);

        // Update with the proper return value
        let mut then_return = BlockArg::Value(self.builder.ins().iconst(Type::int(8).unwrap(), 1));

        // Set the ip to the right spot:
        self.ip = then_start;
        self.translate_instructions();

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        // TODO: Update with the proper return value
        let mut else_return = BlockArg::Value(self.builder.ins().iconst(Type::int(8).unwrap(), 1));

        self.ip = else_start;
        self.translate_instructions();

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

    fn set_ctx_ip(&mut self, ip: usize) {
        let mut sig = self.module.make_signature();
        let name = "set-ctx-ip!";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        // For simplicity for now, just make all calls return a single I64.
        // sig.returns
        //     .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let ip = self.builder.ins().iconst(Type::int(64).unwrap(), ip as i64);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx, ip];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn vm_pop(&mut self, value: Value) {
        let mut sig = self.module.make_signature();
        let name = "handle-pop!";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // For simplicity for now, just make all calls return a single I64.
        // sig.returns
        //     .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn push_to_vm_stack_two(&mut self, value: Value, value2: Value) {
        let mut sig = self.module.make_signature();
        let name = "push-to-vm-stack-2";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(128).unwrap()));
        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx, value, value2];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        // let result = self.builder.inst_results(call)[0];
        // result
    }

    fn push_to_vm_stack(&mut self, value: Value) {
        let mut sig = self.module.make_signature();
        let name = "push-to-vm-stack";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        // let result = self.builder.inst_results(call)[0];
        // result
    }

    fn push_to_vm_stack_function_spill(&mut self, value: Value) {
        let mut sig = self.module.make_signature();
        let name = "push-to-vm-stack-function-spill";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        // let result = self.builder.inst_results(call)[0];
        // result
    }

    fn push_to_vm_stack_let_var(&mut self, value: Value) {
        let mut sig = self.module.make_signature();
        let name = "push-to-vm-stack-let-var";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(128).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        // let result = self.builder.inst_results(call)[0];
        // result
    }

    fn push_const_index(&mut self, index: usize) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params.push(AbiParam::new(Type::int(64).unwrap()));

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(Type::int(128).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function("push-const-index", Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);

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
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(Type::int(128).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_function_returns_value_args(
        &mut self,
        name: &str,
        args: &[(Value, AbiParam)],
        return_type: AbiParam,
    ) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        //
        for (_, p) in args {
            sig.params.push(*p);
        }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(return_type);

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let mut arg_values = vec![ctx];

        // let args = args.iter().map(|x| )

        arg_values.extend(args.iter().map(|x| x.0));

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_function_returns_value_args_no_context(
        &mut self,
        name: &str,
        args: &[(Value, AbiParam)],
        return_type: AbiParam,
    ) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        //
        for (_, p) in args {
            // AbiParam::new(codegen::ir::Type::int(128).unwrap())
            sig.params.push(*p);
        }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(return_type);

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let mut arg_values = vec![];

        // let args = args.iter().map(|x| )

        arg_values.extend(args.iter().map(|x| x.0));

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    // TODO: Call the branch handler, get the return value,
    // jump to where we need to go.
    fn call_branch_handler(&mut self) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();
        let name = "if-branch";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn call_handler(&mut self, op: OpCode) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        let name = format!("{:?}", op);

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // Add a parameter for each argument.
        // for _arg in &args {
        //     sig.params.push(AbiParam::new(self.int));
        // }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(Type::int(8).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        // let mut arg_values = Vec::new();

        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let arg_values = [ctx];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }
}

// impl FunctionTranslator<'_> {
//     /// When you write out instructions in Cranelift, you get back `Value`s. You
//     /// can then use these references in other instructions.
//     fn translate_expr(&mut self, expr: Expr) -> Value {
//         match expr {
//             Expr::Literal(literal) => {
//                 // TODO: Can we get this working?
//                 let imm: i32 = literal.parse().unwrap();
//                 let value: i64 = imm as _;
//                 self.create_i128_immediate(value)
//             }

//             Expr::Add(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().iadd(lhs, rhs)
//             }

//             Expr::Sub(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().isub(lhs, rhs)
//             }

//             Expr::Mul(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().imul(lhs, rhs)
//             }

//             Expr::Div(lhs, rhs) => {
//                 let lhs = self.translate_expr(*lhs);
//                 let rhs = self.translate_expr(*rhs);
//                 self.builder.ins().udiv(lhs, rhs)
//             }

//             Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
//             Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
//             Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
//             Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
//             Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
//             Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
//             Expr::Call(name, args) => self.translate_call(name, args),
//             Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
//             Expr::Identifier(name) => {
//                 // `use_var` is used to read the value of a variable.
//                 let variable = self.variables.get(&name).expect("variable not defined");
//                 self.builder.use_var(*variable)
//             }
//             Expr::Assign(name, expr) => self.translate_assign(name, *expr),
//             Expr::IfElse(condition, then_body, else_body) => {
//                 self.translate_if_else(*condition, then_body, else_body)
//             }
//             Expr::WhileLoop(condition, loop_body) => {
//                 self.translate_while_loop(*condition, loop_body)
//             }
//         }
//     }

//     fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
//         // `def_var` is used to write the value of a variable. Note that
//         // variables can have multiple definitions. Cranelift will
//         // convert them into SSA form for itself automatically.
//         let new_value = self.translate_expr(expr);
//         let variable = self.variables.get(&name).unwrap();
//         self.builder.def_var(*variable, new_value);
//         new_value
//     }

//     fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
//         let lhs = self.translate_expr(lhs);
//         let rhs = self.translate_expr(rhs);
//         self.builder.ins().icmp(cmp, lhs, rhs)
//     }

//     // Unbox integer from enum - Grab the discriminant from the function
//     // fn discriminant(&mut self, value: Value) -> Value {
//     //     // let shift = self.builder.ins().iconst(Type::int(32).unwrap(), 32);
//     //     // let shifted = self.builder.ins().rotr(value, shift);
//     //     self.builder.ins().ireduce(Type::int(8).unwrap(), value)
//     // }

//     fn create_i128_immediate(&mut self, value: i64) -> Value {
//         // let [left, right] = split_big(value);
//         // let int = codegen::ir::Type::int(64).unwrap();

//         // let lhs = self.builder.ins().iconst(int, left);
//         // let rhs = self.builder.ins().iconst(int, right);

//         // // TODO:
//         // self.builder.ins().iconcat(lhs, rhs)
//         let value = self.builder.ins().iconst(Type::int(64).unwrap(), value);
//         self.builder.ins().sextend(Type::int(128).unwrap(), value)
//     }

//     fn translate_if_else(
//         &mut self,
//         condition: Expr,
//         then_body: Vec<Expr>,
//         else_body: Vec<Expr>,
//     ) -> Value {
//         let condition_value = self.translate_expr(condition);

//         let then_block = self.builder.create_block();
//         let else_block = self.builder.create_block();
//         let merge_block = self.builder.create_block();

//         // If-else constructs in the toy language have a return value.
//         // In traditional SSA form, this would produce a PHI between
//         // the then and else bodies. Cranelift uses block parameters,
//         // so set up a parameter in the merge block, and we'll pass
//         // the return values to it from the branches.
//         self.builder.append_block_param(merge_block, self.int);

//         // Test the if condition and conditionally branch.
//         self.builder
//             .ins()
//             .brif(condition_value, then_block, &[], else_block, &[]);

//         self.builder.switch_to_block(then_block);
//         self.builder.seal_block(then_block);

//         // We're assuming this is 128, it should be 64?
//         // let mut then_return = self.builder.ins().iconst(self.int, 0);
//         let mut then_return = self.create_i128_immediate(0);

//         for expr in then_body {
//             then_return = self.translate_expr(expr);
//         }

//         // Jump to the merge block, passing it the block return value.
//         self.builder.ins().jump(merge_block, &[then_return]);

//         self.builder.switch_to_block(else_block);
//         self.builder.seal_block(else_block);
//         // let mut else_return = self.builder.ins().iconst(self.int, 0);
//         let mut else_return = self.create_i128_immediate(0);
//         for expr in else_body {
//             else_return = self.translate_expr(expr);
//         }

//         // Jump to the merge block, passing it the block return value.
//         self.builder.ins().jump(merge_block, &[else_return]);

//         // Switch to the merge block for subsequent statements.
//         self.builder.switch_to_block(merge_block);

//         // We've now seen all the predecessors of the merge block.
//         self.builder.seal_block(merge_block);

//         // Read the value of the if-else by reading the merge block
//         // parameter.
//         let phi = self.builder.block_params(merge_block)[0];

//         phi
//     }

//     fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
//         let header_block = self.builder.create_block();
//         let body_block = self.builder.create_block();
//         let exit_block = self.builder.create_block();

//         self.builder.ins().jump(header_block, &[]);
//         self.builder.switch_to_block(header_block);

//         let condition_value = self.translate_expr(condition);
//         self.builder
//             .ins()
//             .brif(condition_value, body_block, &[], exit_block, &[]);

//         self.builder.switch_to_block(body_block);
//         self.builder.seal_block(body_block);

//         for expr in loop_body {
//             self.translate_expr(expr);
//         }
//         self.builder.ins().jump(header_block, &[]);

//         self.builder.switch_to_block(exit_block);

//         // We've reached the bottom of the loop, so there will be no
//         // more backedges to the header to exits to the bottom.
//         self.builder.seal_block(header_block);
//         self.builder.seal_block(exit_block);

//         // Just return 0 for now.
//         // self.builder.ins().iconst(self.int, 0)

//         self.create_i128_immediate(0)
//     }

//     fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
//         let mut sig = self.module.make_signature();

//         // Add a parameter for each argument.
//         for _arg in &args {
//             sig.params.push(AbiParam::new(self.int));
//         }

//         // For simplicity for now, just make all calls return a single I64.
//         sig.returns.push(AbiParam::new(self.int));

//         // TODO: Streamline the API here?
//         let callee = self
//             .module
//             .declare_function(&name, Linkage::Import, &sig)
//             .expect("problem declaring function");
//         let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

//         let mut arg_values = Vec::new();
//         for arg in args {
//             arg_values.push(self.translate_expr(arg))
//         }
//         let call = self.builder.ins().call(local_callee, &arg_values);
//         self.builder.inst_results(call)[0]
//     }

//     fn translate_global_data_addr(&mut self, name: String) -> Value {
//         let sym = self
//             .module
//             .declare_data(&name, Linkage::Export, true, false)
//             .expect("problem declaring data object");
//         let local_id = self.module.declare_data_in_func(sym, self.builder.func);

//         // let pointer = self.module.target_config().pointer_type();
//         let int = codegen::ir::Type::int(128).unwrap();
//         self.builder.ins().symbol_value(int, local_id)
//     }
// }

fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
    the_return: &str,
    instructions: &[DenseInstruction],
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    // Leave the first one in place to pass the context in.
    {
        let ctx = builder.block_params(entry_block)[0];
        let var = declare_variable(
            Type::int(64).unwrap(),
            builder,
            &mut variables,
            &mut index,
            "vm-ctx",
        );
        builder.def_var(var, ctx);
    }

    for (i, name) in params.iter().enumerate() {
        // TODO: cranelift_frontend should really have an API to make it easy to set
        // up param variables.
        let val = builder.block_params(entry_block)[i + 1];
        dbg!(int);
        let var = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
    }

    // let zero = builder.ins().iconst(Type::int(64).unwrap(), 0);
    // let zero = builder.ins().sextend(Type::int(128).unwrap(), zero);

    // let one = builder.ins().iconst(Type::int(8).unwrap(), 1);

    // let return_variable = declare_variable(int, builder, &mut variables, &mut index, the_return);
    // builder.def_var(return_variable, one);

    // TODO:
    // for expr in stmts {
    //     declare_variables_in_stmt(int, builder, &mut variables, &mut index, expr);
    // }

    variables
}

// TODO: Stack2SSA here
// Recursively descend through the AST, translating all implicit
// variable declarations.
// fn declare_variables_in_stmt(
//     int: types::Type,
//     builder: &mut FunctionBuilder,
//     variables: &mut HashMap<String, Variable>,
//     index: &mut usize,
//     expr: &Expr,
// ) {
//     match *expr {
//         Expr::Assign(ref name, _) => {
//             declare_variable(int, builder, variables, index, name);
//         }
//         Expr::IfElse(ref _condition, ref then_body, ref else_body) => {
//             for stmt in then_body {
//                 declare_variables_in_stmt(int, builder, variables, index, stmt);
//             }
//             for stmt in else_body {
//                 declare_variables_in_stmt(int, builder, variables, index, stmt);
//             }
//         }
//         Expr::WhileLoop(ref _condition, ref loop_body) => {
//             for stmt in loop_body {
//                 declare_variables_in_stmt(int, builder, variables, index, stmt);
//             }
//         }
//         _ => (),
//     }
// }

/// Declare a single variable declaration.
fn declare_variable(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    if !variables.contains_key(name) {
        let variable = builder.declare_var(int);
        variables.insert(name.into(), variable.clone());
        *index += 1;
        variable
    } else {
        variables.get(name).unwrap().clone()
    }
}
