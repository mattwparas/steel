use cranelift::{
    codegen::ir::{BlockArg, FuncRef, Type},
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use std::collections::HashMap;
use std::slice;
use steel_gen::{opcode::OPCODES_ARRAY, OpCode};

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::DenseInstruction,
    steel_vm::vm::{
        jit::{
            box_handler_c, callglobal_handler_deopt_c, car_handler_value, cdr_handler_value,
            check_callable, check_callable_spill, check_callable_tail, check_callable_value,
            cons_handler_value, drop_value, equal_binop, extern_c_add_two, extern_c_div_two,
            extern_c_gt_two, extern_c_gte_two, extern_c_lt_two, extern_c_lte_two,
            extern_c_lte_two_int, extern_c_mult_two, extern_c_null_handler, extern_c_sub_two,
            extern_c_sub_two_int, extern_handle_pop, if_handler_raw_value, if_handler_value,
            let_end_scope_c, move_read_local_0_value_c, move_read_local_1_value_c,
            move_read_local_2_value_c, move_read_local_3_value_c, move_read_local_any_value_c,
            not_handler_raw_value, num_equal_int, num_equal_value, num_equal_value_unboxed,
            pop_value, push_const_value_c, push_const_value_index_c, push_global, push_to_vm_stack,
            push_to_vm_stack_let_var, push_to_vm_stack_two, read_local_0_value_c,
            read_local_1_value_c, read_local_2_value_c, read_local_3_value_c,
            read_local_any_value_c, self_tail_call_handler, set_handler_c, setbox_handler_c,
            should_continue, should_spill, should_spill_value, tcojmp_handler, trampoline,
            trampoline_no_arity, unbox_handler_c, CallFunctionDefinitions,
            CallGlobalFunctionDefinitions, CallGlobalNoArityFunctionDefinitions,
            CallGlobalTailFunctionDefinitions,
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

pub struct FunctionMap<'a> {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
    map2: HashMap<&'static str, Box<dyn FunctionToCranelift2 + Send + Sync + 'static>>,
    return_type_hints: HashMap<&'static str, InferredType>,
    builder: &'a mut JITBuilder,
}

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

        let mut map = FunctionMap {
            map: HashMap::new(),
            map2: HashMap::new(),
            builder: &mut builder,
            return_type_hints: HashMap::new(),
        };

        map.add_func(
            "if-branch",
            if_handler_value as extern "C-unwind" fn(*mut VmCore) -> bool,
        );

        map.add_func(
            "if-branch-value",
            if_handler_raw_value as extern "C-unwind" fn(*mut VmCore, i128) -> bool,
        );

        map.add_func(
            "not-value",
            not_handler_raw_value as extern "C-unwind" fn(*mut VmCore, i128) -> i128,
        );

        map.add_func(
            "call-global",
            callglobal_handler_deopt_c as extern "C-unwind" fn(*mut VmCore) -> u8,
        );

        // Value functions:
        map.add_func(
            "num-equal-value",
            num_equal_value as extern "C-unwind" fn(*mut VmCore, SteelVal, SteelVal) -> SteelVal,
        );

        map.add_func2(
            "num-equal-int",
            num_equal_int as extern "C-unwind" fn(SteelVal, SteelVal) -> SteelVal,
        );

        map.add_func(
            "equal-binop",
            equal_binop as extern "C-unwind" fn(*mut VmCore, SteelVal, SteelVal) -> SteelVal,
        );

        map.add_func(
            "vm-should-continue?",
            should_continue as extern "C-unwind" fn(*mut VmCore) -> bool,
        );

        map.add_func(
            "num-equal-value-unboxed",
            num_equal_value_unboxed as extern "C-unwind" fn(*mut VmCore, i128, i128) -> bool,
        );

        map.add_func(
            "let-end-scope-c",
            let_end_scope_c as extern "C-unwind" fn(*mut VmCore, usize),
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

        CallGlobalFunctionDefinitions::register(&mut map);
        CallGlobalNoArityFunctionDefinitions::register(&mut map);
        CallFunctionDefinitions::register(&mut map);
        CallGlobalTailFunctionDefinitions::register(&mut map);

        map.add_func(
            "trampoline",
            trampoline as extern "C-unwind" fn(*mut VmCore, usize, usize) -> SteelVal,
        );

        map.add_func(
            "trampoline-no-arity",
            trampoline_no_arity as extern "C-unwind" fn(*mut VmCore, usize) -> SteelVal,
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
            "should-spill",
            should_spill as extern "C-unwind" fn(ctx: *mut VmCore, index: usize) -> bool,
        );

        map.add_func(
            "should-spill-value",
            should_spill_value as extern "C-unwind" fn(ctx: *mut VmCore, value: SteelVal) -> bool,
        );

        map.add_func(
            "check-callable-spill",
            check_callable_spill as extern "C-unwind" fn(ctx: *mut VmCore, index: usize) -> u8,
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

        #[allow(improper_ctypes_definitions)]
        type Vm02 = extern "C-unwind" fn(*mut VmCore, SteelVal) -> SteelVal;

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

        map.add_func_hint("lte-binop", extern_c_lte_two as VmBinOp, InferredType::Bool);

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
            // &the_return,
            // bytecode,
            entry_block,
        );

        // Check if all of the functions that are getting
        // called are found to be machine code:

        // let mut call_global_all_native = true;
        // for instr in bytecode.iter() {
        //     match instr.op_code {
        //         OpCode::CALLGLOBAL
        //         | OpCode::CALLGLOBALTAIL
        //         | OpCode::CALLGLOBALTAILNOARITY
        //         | OpCode::CALLGLOBALNOARITY => {
        //             let func = globals.get(instr.payload_size.to_usize());
        //             if let Some(value) = func {
        //                 match value {
        //                     SteelVal::Closure(c) => {
        //                         if c.0.id.to_string() == name {
        //                             // Then this is fine. Generate a trampoline call
        //                             // without going through the runtime.
        //                             continue;
        //                         } else if c.0.super_instructions.is_some() {
        //                             continue;
        //                         } else {
        //                             call_global_all_native = false;
        //                             break;
        //                         }
        //                     }

        //                     SteelVal::BuiltIn(_) => {
        //                         call_global_all_native = false;
        //                         break;
        //                     }

        //                     _ => {}
        //                 }
        //             }
        //         }

        //         OpCode::FUNC | OpCode::TAILCALL => {
        //             call_global_all_native = false;
        //             break;
        //         }
        //         _ => {}
        //     }
        // }

        // println!("All native functions: {}", call_global_all_native);

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
            patched_locals: Vec::new(),
            function_map: &self.function_map,
            function_context,
            id: func_id,
            call_global_all_native: false,
            name,
            value_to_local_map: HashMap::new(),
            local_to_value_map: HashMap::new(),
            let_var_stack: Vec::new(),
            tco: None,
            intrinsics: &self.function_map,
        };

        trans.stack_to_ssa();

        let return_value = trans.builder.ins().iconst(Type::int(8).unwrap(), 1);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum InferredType {
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

    // Local value mapping, can allow
    // us to elide type checks if we have them
    value_to_local_map: HashMap<Value, usize>,

    local_to_value_map: HashMap<usize, InferredType>,

    arity: u16,
    constants: &'a ConstantMap,

    tco: Option<Value>,

    let_var_stack: Vec<usize>,

    patched_locals: Vec<bool>,

    function_map: &'a OwnedFunctionMap,

    function_context: Option<usize>,

    id: FuncId,

    call_global_all_native: bool,

    name: String,

    intrinsics: &'a OwnedFunctionMap,
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

        (OpCode::ADD, 2) => "add-binop",
        (OpCode::SUB, 2) => "sub-binop",
        // (OpCode::LT, 2) => "lt-binop",
        (OpCode::LTE, 2) => "lte-binop",
        (OpCode::GT, 2) => "gt-binop",
        (OpCode::GTE, 2) => "gte-binop",
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
    fn mark_local_type_from_var(&mut self, last: StackValue, typ: InferredType) {
        if let Some(from_local) = self.value_to_local_map.get(&last.value) {
            self.local_to_value_map.insert(*from_local, typ);
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
        while self.ip < self.instructions.len() {
            let instr = self.instructions[self.ip];
            let op = instr.op_code;
            // println!("{:?} @ {}", op, self.ip);
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

                    for value in self.stack.clone() {
                        if !value.spilled {
                            self.push_to_vm_stack(value.value);
                        }
                    }

                    self.vm_pop(value.0);

                    // let the_return = self.builder.ins().iconst(Type::int(8).unwrap(), 1);
                    // self.builder.ins().return_(&[the_return]);

                    self.ip = self.instructions.len() + 1;

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
                    let name = CallFunctionDefinitions::arity_to_name(arity).unwrap();

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
                        let upper_bound = payload + 1 - self.arity as usize;
                        for i in 0..upper_bound {
                            // for i in 0..(payload + 1 - self.arity as usize) {
                            // println!("Read local spill generic");
                            self.spill(i);
                        }
                    }

                    let index = self
                        .builder
                        .ins()
                        .iconst(Type::int(64).unwrap(), payload as i64);

                    let value = self.call_function_returns_value_args(
                        op_to_name_payload(op, payload),
                        &[index],
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
                        let upper_bound = payload - self.arity as usize - let_var_offset;

                        for i in 0..upper_bound {
                            // for i in 0..(payload + 1 - self.arity as usize) {
                            // let value = self.stack.remove(i);
                            // self.push_to_vm_stack(value.value);
                            self.spill(i);
                        }
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

                    return false;
                }
                OpCode::SELFTAILCALLNOARITY => {
                    let _ = self.translate_tco_jmp_no_arity(payload);
                    // self.tco = Some(res);
                    // Jump to out of bounds so signal we're done
                    self.ip = self.instructions.len() + 1;
                    return false;
                }
                OpCode::CALLGLOBALTAIL | OpCode::CALLGLOBALTAILNOARITY => {
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();

                    let name = CallGlobalTailFunctionDefinitions::arity_to_name(arity).unwrap();

                    // This function pushes back on to the stack, and then we should just
                    // return since we're done now.
                    let v = self.call_global_function(arity, name, function_index, true);

                    self.push(v, InferredType::Any);

                    self.check_deopt();
                }
                OpCode::CALLGLOBALNOARITY => {
                    // First - find the index that we have to lookup.
                    let function_index = payload;
                    self.ip += 1;
                    let arity = self.instructions[self.ip].payload_size.to_usize();
                    let name = CallGlobalNoArityFunctionDefinitions::arity_to_name(arity).unwrap();

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
                    let name = CallGlobalFunctionDefinitions::arity_to_name(arity).unwrap();

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

                    self.let_var_stack.push(0);

                    for arg in 0..self.stack.len() {
                        self.spill(arg);
                    }

                    self.ip += 1;
                }
                OpCode::LETENDSCOPE => {
                    // self.local_count = payload;
                    self.ip += 1;

                    self.let_var_stack.pop();

                    self.call_end_scope_handler(payload);

                    self.patched_locals.pop();
                }
                OpCode::PUREFUNC => todo!(),

                OpCode::SUB
                    if payload == 2
                        && self.stack.last().map(|x| x.inferred_type)
                            == Some(InferredType::Int) =>
                {
                    // Call the func
                    self.func_ret_val_named("sub-binop-int", payload, 2, InferredType::Number);
                }

                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    // Call the func
                    self.func_ret_val(op, payload, 2, InferredType::Number);
                    self.check_deopt();
                }

                OpCode::LTE
                    if payload == 2
                        && self.stack.last().map(|x| x.inferred_type)
                            == Some(InferredType::Int) =>
                {
                    self.func_ret_val_named("lte-binop-int", payload, 2, InferredType::Bool);
                }

                OpCode::NUMEQUAL
                    if payload == 2
                        && self.stack.last().map(|x| x.inferred_type)
                            == Some(InferredType::Int) =>
                {
                    self.func_ret_val_named("num-equal-int", payload, 2, InferredType::Bool);
                }

                OpCode::EQUAL
                | OpCode::NUMEQUAL
                | OpCode::LTE
                | OpCode::GTE
                | OpCode::GT
                | OpCode::LT
                | OpCode::EQUAL2 => {
                    self.func_ret_val(op, payload, 2, InferredType::Bool);
                }
                OpCode::NULL => {
                    self.func_ret_val_named("null-handler", 1, 2, InferredType::Bool);
                }
                OpCode::CONS => {
                    self.func_ret_val(op, 2, 2, InferredType::List);
                }
                OpCode::CDR => {
                    self.func_ret_val(op, 1, 2, InferredType::List);
                }
                OpCode::LIST => todo!(),
                OpCode::CAR => {
                    if let Some(last) = self.stack.last().copied() {
                        self.mark_local_type_from_var(last, InferredType::List);
                    }

                    self.func_ret_val(op, 1, 2, InferredType::Any);
                }
                OpCode::NEWBOX => {
                    self.func_ret_val(op, 1, 2, InferredType::Box);
                }
                OpCode::SETBOX => {
                    if let Some(last) = self.stack.get(self.stack.len() - 2).copied() {
                        self.mark_local_type_from_var(last, InferredType::Box);
                    }

                    self.func_ret_val(op, 2, 2, InferredType::Any);
                }
                OpCode::UNBOX => {
                    if let Some(last) = self.stack.last().copied() {
                        self.mark_local_type_from_var(last, InferredType::Box);
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
                OpCode::NOT => {
                    // Do the thing.
                    self.func_ret_val(op, 1, 2, InferredType::Bool);
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
        let last = self.pop();

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

    fn call_test_handler(&mut self, test_value: Value) -> Value {
        let local_callee = self.get_local_callee("if-branch-value");

        let ctx = self.get_ctx();

        // Advance to the next thing
        self.ip += 1;

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

    fn translate_tco_jmp(&mut self, payload: usize) {
        for i in 0..self.stack.len() {
            self.spill(i);
        }

        let local_callee = self.get_local_callee("tco-jump");

        let ctx = self.get_ctx();
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

        for i in 0..self.stack.len() {
            self.spill(i);
        }

        let local_callee = self.get_local_callee("self-tail-call");
        let ctx = self.get_ctx();

        let arity = self
            .builder
            .ins()
            .iconst(Type::int(64).unwrap(), payload as i64);

        let arg_values = [ctx, arity];
        let call = self.builder.ins().call(local_callee, &arg_values);

        // Tail call time!
        {
            // let pointer = self.module.target_config().pointer_type();
            // let mut signature = self.module.make_signature();
            // signature.params.push(AbiParam::new(pointer));
            // signature.returns.push(AbiParam::new(Type::int(8).unwrap()));

            // let func = self
            //     .module
            //     .declare_function(&self.name, Linkage::Import, &self.builder.func.signature)
            //     .unwrap();

            // let local_callee = self.module.declare_func_in_func(self.id, self.builder.func);

            // let token_return = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

            // let tail_call = self.builder.ins().return_call(local_callee, &[ctx]);
            // let tail_call = self.builder.ins().return_call(local_callee, &[ctx]);

            // token_return
        }

        // let ret = self.builder.ins().iconst(Type::int(8).unwrap(), 1);
        // self.builder.ins().return_(&[ret]);
    }

    fn call_function(&mut self, arity: usize, name: &str) -> Value {
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

            // Check callable - TODO: Insert these checks elsewhere too!
            let should_spill = self.check_should_spill_value(func);

            let spill_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(should_spill, spill_block, &[], merge_block, &[then_return]);

            self.builder.switch_to_block(spill_block);
            self.builder.seal_block(spill_block);

            for c in self.stack.clone() {
                if !c.spilled {
                    self.push_to_vm_stack_function_spill(c.value);
                }
            }

            self.builder.ins().jump(merge_block, &[then_return]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            for c in self.stack.clone() {
                if !c.spilled {
                    self.push_to_vm_stack(c.value);
                }
            }

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
        let mut args_off_the_stack = self
            .stack
            .drain(self.stack.len() - arity..)
            .collect::<Vec<_>>();

        self.maybe_patch_from_stack(&mut args_off_the_stack);

        for arg in &args_off_the_stack {
            assert!(!arg.spilled);
        }

        assert_eq!(args_off_the_stack.len(), arity);

        arg_values.extend(args_off_the_stack.iter().map(|x| x.value));

        // TODO: @Matt
        // Instead of being binary, this should return multiple values, and
        // we should check the conditions:
        //
        // 1. Is this a function
        // 2. If its a function, is it native. If its native, continue.
        // 3. If its a bytecode function, we should spill no matter what.
        // 4. If its not a bytecode function, we should bail.
        //
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

            // Check callable - TODO: Insert these checks elsewhere too!
            let should_spill = self.check_should_spill(lookup_index);

            let spill_block = self.builder.create_block();

            self.builder
                .ins()
                .brif(should_spill, spill_block, &[], merge_block, &[then_return]);

            self.builder.switch_to_block(spill_block);
            self.builder.seal_block(spill_block);

            for c in self.stack.clone() {
                if !c.spilled {
                    self.push_to_vm_stack_function_spill(c.value);
                }
            }

            self.builder.ins().jump(merge_block, &[then_return]);

            self.builder.switch_to_block(else_block);
            self.builder.seal_block(else_block);

            for c in self.stack.clone() {
                if !c.spilled {
                    self.push_to_vm_stack_function_spill(c.value);
                }
            }

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
        let name = "check-callable-value";
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

    fn check_call_spill(&mut self, index: Value) -> Value {
        let name = "check-callable-spill";
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let call = self.builder.ins().call(local_callee, &[ctx, index]);
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

    fn check_deopt(&mut self) {
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

        if self.tco.is_some() {
            self.ip = self.instructions.len() + 1;
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
        let else_return = self.builder.ins().iconst(Type::int(8).unwrap(), 1);
        // let else_return = self.create_i128(0);
        self.builder.ins().return_(&[else_return]);
        self.builder.switch_to_block(merge_block);
        // // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);
        // // Read the value of the if-else by reading the merge block
        // // parameter.
        let phi = self.builder.block_params(merge_block)[0];
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

    fn spill(&mut self, index: usize) -> Option<()> {
        let guard = self.stack.get_mut(index)?;
        let mut spilled = false;
        if !guard.spilled {
            guard.spilled = true;
            spilled = true;
        }

        if spilled {
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

    fn func_ret_val(
        &mut self,
        op: OpCode,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
    ) {
        let function_name = op_to_name_payload(op, payload);
        let args = self.split_off(payload);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args.into_iter().map(|x| x.0).collect::<Vec<_>>();

        let result = self.call_function_returns_value_args(function_name, &args);

        // Check the inferred type, if we know of it
        self.push(result, inferred_type);

        self.ip += ip_inc;
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

        let let_stack = self.let_var_stack.clone();
        // let local_count = self.local_count;
        let frozen_patched = self.patched_locals.clone();
        let frozen_stack = self.stack.clone();
        let tco = self.tco;

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
                .unwrap_or_else(|| self.create_i128(encode(SteelVal::Void))),
        );

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

        self.tco = None;
        self.let_var_stack = let_stack;
        // self.local_count = local_count;
        self.patched_locals = frozen_patched;
        self.stack = frozen_stack;
        let token_return_value = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

        self.stack_to_ssa();

        if let Some(value) = self.tco {
            // Set the return value to be the
            // tail called return value?
            self.ip = self.instructions.len() + 1;
            return token_return_value;
        }

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
                .unwrap_or_else(|| self.create_i128(encode(SteelVal::Void))),
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

    fn vm_pop(&mut self, value: Value) {
        let name = "handle-pop!";
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let arg_values = [ctx, value];
        let call = self.builder.ins().call(local_callee, &arg_values);
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
        let call = self.builder.ins().call(local_callee, &arg_values);
        // let result = self.builder.inst_results(call)[0];
        // result
    }

    fn push_to_vm_stack_function_spill(&mut self, value: Value) {
        let name = "push-to-vm-stack-function-spill";
        let local_callee = self.get_local_callee(name);
        let ctx = self.get_ctx();
        let arg_values = [ctx, value];
        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn push_to_vm_stack_let_var(&mut self, value: Value) {
        let name = "push-to-vm-stack-let-var";

        let local_callee = self.get_local_callee(name);

        let ctx = self.get_ctx();
        let arg_values = [ctx, value];

        // for arg in args {
        //     arg_values.push(self.translate_expr(arg))
        // }
        let call = self.builder.ins().call(local_callee, &arg_values);
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
        self.intrinsics.get_signature(name, &self.module)
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
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        ctx
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
fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
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
        let val = builder.block_params(entry_block)[i + 1];
        let var = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
    }

    variables
}

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
