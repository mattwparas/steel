use cranelift::{codegen::ir::BlockArg, prelude::*};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use std::collections::HashMap;
use std::slice;
// use steel_gen::opcode::OPCODES_ARRAY;
use steel_gen::{opcode::OPCODES_ARRAY, OpCode};

use crate::{
    compiler::constants::ConstantMap,
    core::instructions::{u24, DenseInstruction},
    steel_vm::vm::jit::{
        call_global_function_deopt_0, call_global_function_deopt_0_func,
        call_global_function_deopt_1, call_global_function_deopt_1_func,
        call_global_function_deopt_2, call_global_function_deopt_2_func,
        call_global_function_deopt_3, call_global_function_deopt_3_func,
        call_global_function_tail_deopt_0, call_global_function_tail_deopt_1,
        call_global_function_tail_deopt_2, call_global_function_tail_deopt_3,
        callglobal_handler_deopt_c, callglobal_tail_handler_deopt_3,
        callglobal_tail_handler_deopt_3_test, check_callable, extern_c_add_two, extern_c_div_two,
        extern_c_gt_two, extern_c_gte_two, extern_c_lt_two, extern_c_lte_two, extern_c_mult_two,
        extern_c_sub_two, extern_handle_pop, if_handler_raw_value, if_handler_value,
        let_end_scope_c, move_read_local_0_value_c, move_read_local_1_value_c,
        move_read_local_2_value_c, move_read_local_3_value_c, not_handler_raw_value,
        num_equal_value, num_equal_value_unboxed, push_const_value_c, push_global, push_int_0,
        push_int_1, push_int_2, push_to_vm_stack, read_local_0_value_c, read_local_1_value_c,
        read_local_2_value_c, read_local_3_value_c, set_ctx_ip, should_continue,
    },
    steel_vm::vm::VmCore,
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

// Set up ways to deconstruct this value, such that we can use a 16 byte value rather than an 8
// byte value.
#[repr(C, u8)]
enum SValue {
    Int(i64),
    Bool(bool),
    Pointer(Box<SValue>),
    Void,
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

macro_rules! offset_of {
    ($($tt:tt)*) => {
        {
            let base = $($tt)*(unsafe { ::std::mem::uninitialized() });
            let offset = match base {
                $($tt)*(ref inner) => (inner as *const _ as usize) - (&base as *const _ as usize),
                _ => unreachable!(),
            };
            ::std::mem::forget(base);
            offset
        }
    }
}

impl SValue {
    fn discriminant(&self) -> u8 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }

    fn value(&self) -> usize {
        unsafe { *<*const _>::from(self).byte_add(8).cast::<usize>() }
    }
}

// Any allocated values that are not primitives should be boxed, or set aside into some kind of
// jit compilation nursery, so we can avoid ref counts internally?

// Glue together op codes dynamically at runtime?
extern "C" fn handle_add(ctx: *mut VmContext) {
    println!("Calling vm context");
}

extern "C" fn handle_sub(ctx: *mut VmContext) {
    println!("Calling vm context");
}

extern "C" fn handle_pop(ctx: *mut VmContext) {
    println!("Calling vm context");
}

// fn handle_pop_test(ctx: &mut VmContext) {
//     println!("Calling vm context");
// }

// Set up handlers
extern "C" fn rustfunc(x: i128) -> i128 {
    println!("Hello from rustfunc x={}", x);
    x
}

// Set up the vm context to be a value
// when we're calling the function.
pub struct VmContext {}

// TODO: Implement some kind of handler -> signature generator?
// Maybe via macros?

// Turn functions from function signature, into cranelift signature for backend usage.
// TODO: Rename to an intrinsic map?
// Wire up with function signatures as well for automated unboxing?
struct FunctionMap<'a> {
    map: HashMap<&'static str, Box<dyn FunctionToCranelift + Send + Sync + 'static>>,
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

    pub fn add_func_hint(
        &mut self,
        name: &'static str,
        func: impl FunctionToCranelift + Send + Sync + 'static,
        return_type: InferredType,
    ) {
        self.add_func(name, func);
        self.return_type_hints.insert(name, return_type);
    }

    pub fn get_signature(&self, name: &'static str, module: &JITModule) -> Signature {
        self.map.get(name).unwrap().to_cranelift(module)
    }
}

trait FunctionToCranelift {
    fn to_cranelift(&self, module: &JITModule) -> Signature;
    fn as_pointer(&self) -> *const u8;
}

// TODO: How to set up the right args here?
impl<T> FunctionToCranelift for extern "C" fn() -> T {
    fn to_cranelift(&self, module: &JITModule) -> Signature {
        todo!()
    }
    fn as_pointer(&self) -> *const u8 {
        *self as _
    }
}

macro_rules! register_function_pointers_return {
    ($($typ:ident),*) => {
        impl<RET, $($typ),*> FunctionToCranelift for extern "C" fn(*mut VmCore, $($typ),*) -> RET {
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

fn type_to_ir_type<T>() -> codegen::ir::Type {
    codegen::ir::Type::int(std::mem::size_of::<T>() as u16 * 8).unwrap()
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
            builder: &mut builder,
            return_type_hints: HashMap::new(),
        };

        map.add_func(
            "handle-pop!",
            extern_handle_pop as extern "C" fn(*mut VmCore, SteelVal),
        );

        map.add_func(
            "call-global-function-deopt-0",
            call_global_function_deopt_0 as extern "C" fn(*mut VmCore, usize, usize) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-1",
            call_global_function_deopt_1
                as extern "C" fn(*mut VmCore, usize, usize, SteelVal) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-2",
            call_global_function_deopt_2
                as extern "C" fn(*mut VmCore, usize, usize, SteelVal, SteelVal) -> SteelVal,
        );

        map.add_func(
            "call-global-function-deopt-3",
            call_global_function_deopt_3
                as extern "C" fn(
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
            push_global as extern "C" fn(ctx: *mut VmCore, index: usize) -> SteelVal,
        );

        // Check if the function at the global location is in fact the right one.
        map.add_func(
            "check-callable",
            check_callable as extern "C" fn(ctx: *mut VmCore, index: usize) -> bool,
        );

        map.add_func(
            "push-to-vm-stack",
            push_to_vm_stack as extern "C" fn(ctx: *mut VmCore, value: SteelVal),
        );

        #[allow(improper_ctypes_definitions)]
        type Vm01 = extern "C" fn(*mut VmCore) -> SteelVal;

        #[allow(improper_ctypes_definitions)]
        type Vm0b = extern "C" fn(*mut VmCore) -> bool;

        #[allow(improper_ctypes_definitions)]
        type VmBinOp = extern "C" fn(ctx: *mut VmCore, a: SteelVal, b: SteelVal) -> SteelVal;

        map.add_func("push-const", push_const_value_c as Vm01);

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
        map.add_func_hint("lt-binop", extern_c_lt_two as VmBinOp, InferredType::Bool);
        map.add_func_hint("lte-binop", extern_c_lte_two as VmBinOp, InferredType::Bool);
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
                as extern "C" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function-tail-deopt-1",
            call_global_function_tail_deopt_1
                as extern "C" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                    arg0: SteelVal,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function-tail-deopt-2",
            call_global_function_tail_deopt_2
                as extern "C" fn(
                    ctx: *mut VmCore,
                    lookup_index: usize,
                    fallback_ip: usize,
                    arg0: SteelVal,
                    arg1: SteelVal,
                ) -> SteelVal,
        );

        map.add_func(
            "call-global-function0-tail-deopt-3",
            call_global_function_tail_deopt_3
                as extern "C" fn(
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
        map.add_func("move-read-local-0", move_read_local_0_value_c as Vm01);
        map.add_func("move-read-local-1", move_read_local_1_value_c as Vm01);
        map.add_func("move-read-local-2", move_read_local_2_value_c as Vm01);
        map.add_func("move-read-local-3", move_read_local_3_value_c as Vm01);

        map.add_func("vm-should-continue?", should_continue as Vm0b);

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
) -> Result<fn(&mut VmCore) -> bool, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile(name, arity, code, globals, constants)?;
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
    ) -> Result<fn(&mut VmCore) -> bool, String> {
        unsafe { compile_bytecode(self, name, arity, code, globals, constants) }
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
    ) -> Result<*const u8, String> {
        self.ctx.set_disasm(true);

        // First, parse the string, producing AST nodes.

        // TODO: Change this - we need to actually pass in the instructions
        // that we want to jit compile.
        let (params, the_return, stmts) = (Default::default(), Default::default(), instructions);
        // parser::function(input).map_err(|e| e.to_string())?;

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(arity, params, the_return, stmts, globals, constants)?;

        println!("{}", self.ctx.func);

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should
        // we have a version of `declare_function` that automatically declares
        // the function?
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

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
        arity: u16,
        params: Vec<String>,
        the_return: String,
        bytecode: &[DenseInstruction],
        globals: &[SteelVal], // stmts: Vec<Expr>,
        constants: &ConstantMap,
    ) -> Result<(), String> {
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        // let int = self.module.target_config().pointer_type();

        // Upgrade to 128 bit?
        let int = codegen::ir::Type::int(128).unwrap();

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
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

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

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
            instructions: bytecode,
            ip: 0,
            globals,
            stack: Vec::new(),
            arity,
            constants,
            local_count: 0,
            patched_locals: false,
            function_map: &self.function_map,
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

        let return_value = trans
            .builder
            .ins()
            .iconst(codegen::ir::Type::int(8).unwrap(), 1);

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
    stack: Vec<(Value, InferredType)>,

    arity: u16,
    constants: &'a ConstantMap,

    local_count: usize,

    patched_locals: bool,

    function_map: &'a OwnedFunctionMap,
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
        other => panic!(
            "couldn't match the name for the op code + payload: {:?}",
            other
        ),
    }
}

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
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

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

    // Read values off of the stack - push them on to wherever they need to go.
    // Assuming the whole instruction set is translated and we also confirm that _only_
    // native functions get used, we can probably just rewrite the function
    // into a function pointer, and we don't need to thread the context through at all.
    fn stack_to_ssa(&mut self) -> bool {
        while self.ip < self.instructions.len() {
            let instr = self.instructions[self.ip];
            let op = instr.op_code;
            println!("{:?}", op);
            let payload = instr.payload_size.to_usize();
            match op {
                // Exit points aren't handled quite yet
                OpCode::LOADINT1POP
                | OpCode::BINOPADDTAIL
                | OpCode::NEWSCLOSURE
                | OpCode::TAILCALL => {
                    todo!();
                }

                OpCode::POPJMP | OpCode::POPPURE => {
                    // Push the remaining value back on to the stack.
                    let value = self.stack.pop().unwrap();

                    // Should break here - just call `handle_pop_pure_value` and handle the return value / updating
                    // of various things here.
                    // self.push_to_vm_stack(value.0);
                    // self.set_ctx_ip(self.ip);

                    self.vm_pop(value.0);

                    self.ip += 1;

                    return false;
                }

                OpCode::VOID => {
                    // Push void onto stack?
                    let void = SteelVal::Void;
                    let value = self.create_i128(encode(void));
                    self.stack.push((value, InferredType::Void));
                    self.ip += 1;
                }
                // Handle global value
                OpCode::PUSH => {
                    // Let value to push:
                    let abi_type = AbiParam::new(codegen::ir::Type::int(64).unwrap());

                    let index = self
                        .builder
                        .ins()
                        .iconst(codegen::ir::Type::int(64).unwrap(), payload as i64);

                    // TODO: We could instead call it directly instead of pushing on to the stack!

                    // Push on to the stack to call
                    self.stack.push((index, InferredType::Int));

                    // Just...

                    // TODO: If we know what the global is, and we know that its not mutated, we probably can do something
                    // interesting here.
                    self.func_ret_val(
                        OpCode::PUSH,
                        payload,
                        1,
                        InferredType::Any,
                        abi_type,
                        AbiParam::new(codegen::ir::Type::int(128).unwrap()),
                    );
                }
                // Translate if? Correct the stack accordingly?
                OpCode::IF => {
                    // TODO: Type inference here! Change which function is called!
                    let (test, typ) = self.stack.pop().unwrap();

                    let false_instr = self.instructions[self.ip].payload_size;
                    let true_instr = self.ip + 1;

                    let test_bool = if typ == InferredType::Bool {
                        let amount_to_shift = self
                            .builder
                            .ins()
                            .iconst(codegen::ir::Type::int(64).unwrap(), 64);

                        // println!("Handling if with inferred type bool");
                        // println!(
                        //     "Split big: {:?}",
                        //     split_big(unsafe { std::mem::transmute(SteelVal::IntV(1)) })
                        // );

                        //
                        // self.builder.ins().ireduce(Type::int(8).unwrap(), test)

                        // self.builder.ins().

                        // self.builder.ins().ireduce(Int, x)

                        // self.builder.ins().rotr(test, amount_to_shift)

                        let shift_right = self.builder.ins().sshr(test, amount_to_shift);
                        self.builder
                            .ins()
                            .ireduce(Type::int(8).unwrap(), shift_right)
                    } else {
                        self.call_test_handler(test)
                    };

                    let res =
                        self.translate_if_else_value(test_bool, true_instr, false_instr.to_usize());

                    self.stack.push((res, InferredType::Any));
                }
                // Just... jump to that instruction?
                OpCode::JMP => {
                    self.ip = payload;
                }

                // Insert guards against the things.
                OpCode::FUNC => todo!(),
                OpCode::SCLOSURE => todo!(),
                OpCode::ECLOSURE => todo!(),
                OpCode::BIND => todo!(),
                OpCode::SDEF => todo!(),
                OpCode::EDEF => todo!(),
                OpCode::POPN => todo!(),
                OpCode::POPSINGLE => todo!(),
                OpCode::PASS => todo!(),
                OpCode::NDEFS => todo!(),
                OpCode::PANIC => todo!(),
                OpCode::SET => todo!(),
                OpCode::READLOCAL => todo!(),

                // Read a local from the local stack.
                OpCode::PUSHCONST | OpCode::LOADINT0 | OpCode::LOADINT1 | OpCode::LOADINT2 => {
                    let payload = self.instructions[self.ip].payload_size.to_usize();
                    let value = self.call_func_or_immediate(op, payload);
                    self.ip += 1;
                    self.stack.push((value, InferredType::Any));
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
                    if payload + 1 > self.arity as _ && !self.patched_locals {
                        let locals_to_patch = self.local_count;
                        println!("Patching locals: {}", locals_to_patch);

                        for i in 0..locals_to_patch {
                            let item = self.stack.get(i).unwrap();
                            self.push_to_vm_stack(item.0);
                        }

                        self.patched_locals = true;

                        // Push this on to the VM context stack
                        // let local_value = self.stack.get(payload + self.arity as usize);

                        // Push each of the pending locals to the VM stack?
                        // Call function to push onto local stack.
                    }

                    let value = self.call_func_or_immediate(op, payload);
                    self.ip += 1;
                    self.stack.push((value, InferredType::Any));
                }

                OpCode::SETLOCAL => todo!(),
                OpCode::COPYCAPTURESTACK => todo!(),
                OpCode::COPYCAPTURECLOSURE => todo!(),
                OpCode::COPYHEAPCAPTURECLOSURE => todo!(),
                OpCode::FIRSTCOPYHEAPCAPTURECLOSURE => todo!(),

                // TODO: This _should_ be changed to be a while loop probably.
                // We can get clever with this to avoid a lot of headache, but it will depend
                // on destructors getting run during the while loop. Probably just inline a call to drop?
                // Then can run a while loop no problem?
                // Something like:
                //
                // (define (loop x)
                //     (...)
                //     (loop (+ x 1))
                //
                // Would translate to:
                // while true:
                //    (...) - with explicit ret's at the tails?
                //
                // And each iteration of the loop prior to the jump back would
                // just call `drop(...)` on each of the args?
                OpCode::TCOJMP => todo!(),

                OpCode::CALLGLOBALTAIL => {
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

                    // This function pushes back on to the stack, and then we should just
                    // return since we're done now.
                    let v = self.call_global_function(arity, name, function_index);

                    // self.check_deopt();

                    self.stack.push((v, InferredType::Any));

                    // self.check_deopt();

                    // let return_value = self
                    //     .builder
                    //     .ins()
                    //     .iconst(codegen::ir::Type::int(8).unwrap(), 1);

                    // self.builder.ins().return_(&[return_value]);

                    // TODO: Push back on to the native stack, then deopt
                }

                // Call global value, with deopt down to auto adjust the stack?
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

                    // TODO: If the function that we're calling is not native,
                    // we need to push all of the values that we have here
                    // back on to the stack.
                    println!("stack at global: {:?}", self.stack);

                    let result = self.call_global_function(arity, name, function_index);

                    // Assuming this worked, we'll want to push this result on to the stack.
                    self.stack.push((result, InferredType::Any));

                    // Then, we're gonna check the result and see if we should deopt
                    self.check_deopt();
                }

                // Moving the value through to the function call means
                // just invalidating the previous reference on the stack.
                //
                // So move off of the stack.
                OpCode::MOVEREADLOCAL => todo!(),

                OpCode::READCAPTURED => todo!(),

                // Enter a let scope - This is unfortunately important.
                // This just lets us know now that the local variables from this
                // point onwards can be references _after_ whatever value we think
                // it is at.
                OpCode::BEGINSCOPE => {
                    // Next n values should stick around on the stack.
                    for instr in &self.instructions[self.ip..] {
                        if instr.op_code == OpCode::LETENDSCOPE {
                            self.local_count = instr.payload_size.to_usize();
                            println!("Entering scope: {}", self.local_count);
                            break;
                        }
                    }

                    self.ip += 1;
                }

                // Drop the previous values from the scope.
                OpCode::LETENDSCOPE => {
                    // TODO: Drain the values that are left on the stack
                    println!("Exiting scope: {}", payload);
                    self.local_count = payload;
                    self.ip += 1;
                    self.call_end_scope_handler(payload);

                    self.patched_locals = false;

                    // Drop the last n values off the stack

                    self.stack.drain(payload..self.stack.len() - 1);
                }
                OpCode::PUREFUNC => todo!(),

                // TODO: Roll up the bin ops into a function to make things easier
                OpCode::ADD | OpCode::SUB | OpCode::MUL | OpCode::DIV => {
                    let abi_type = AbiParam::new(codegen::ir::Type::int(128).unwrap());
                    // Call the func
                    self.func_ret_val(op, payload, 2, InferredType::Number, abi_type, abi_type);
                }

                // Should I just... convert this type to the native type?
                // Or deal with some kind of unboxing naively? Check the back half of it?
                OpCode::EQUAL
                | OpCode::NUMEQUAL
                | OpCode::LTE
                // | OpCode::GTE
                // | OpCode::GT
                // | OpCode::LT

                => {
                    let abi_type = AbiParam::new(codegen::ir::Type::int(128).unwrap());
                    self.func_ret_val(op, payload, 2, InferredType::Bool, abi_type, abi_type);
                }
                // OpCode::NUMEQUAL => todo!(),
                OpCode::NULL => todo!(),

                // Figure out how to handle heap allocated values like lists.
                OpCode::CONS => todo!(),
                OpCode::LIST => todo!(),
                OpCode::CAR => todo!(),
                OpCode::CDR => todo!(),
                OpCode::NEWBOX => todo!(),
                OpCode::SETBOX => todo!(),
                OpCode::UNBOX => todo!(),
                OpCode::ADDREGISTER => todo!(),
                OpCode::SUBREGISTER => todo!(),
                OpCode::LTEREGISTER => todo!(),
                OpCode::SUBREGISTER1 => todo!(),
                OpCode::ALLOC => todo!(),
                OpCode::READALLOC => todo!(),
                OpCode::SETALLOC => todo!(),
                OpCode::DynSuperInstruction => todo!(),
                OpCode::Arity => todo!(),
                OpCode::LetVar => todo!(),
                OpCode::ADDIMMEDIATE => todo!(),
                // Sub immediate:
                // What the heck is this op code?
                OpCode::SUBIMMEDIATE => todo!(),
                OpCode::LTEIMMEDIATE => todo!(),
                OpCode::BINOPADD => todo!(),
                OpCode::BINOPSUB => todo!(),
                OpCode::LTEIMMEDIATEIF => todo!(),
                // If the type is bool, just negate it
                OpCode::NOT => {
                    // Do the thing.
                    let abi_type = AbiParam::new(codegen::ir::Type::int(128).unwrap());
                    self.func_ret_val(op, 1, 2, InferredType::Bool, abi_type, abi_type);
                }
                OpCode::VEC => todo!(),
                OpCode::Apply => todo!(),
                // OpCode::CALLGLOBALTAILPOP => todo!(),
                OpCode::LOADINT0POP => todo!(),
                OpCode::LOADINT2POP => todo!(),
                OpCode::CaseLambdaDispatch => todo!(),
                _ => todo!()
            }
        }

        return true;
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
        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

        // Return value
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

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

        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(64).unwrap()));

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
            .iconst(codegen::ir::Type::int(64).unwrap(), amount as i64);

        let arg_values = [ctx, amount_to_drop];
        let call = self.builder.ins().call(local_callee, &arg_values);
    }

    fn call_global_function(&mut self, arity: usize, name: &str, function_index: usize) -> Value {
        // This is the call global `call_global_function_deopt`
        let mut sig = self.module.make_signature();

        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // lookup index
        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(64).unwrap()));

        // instruction pointer
        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(64).unwrap()));

        for _ in 0..arity {
            sig.params
                .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));
        }

        // Return value
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

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
            .iconst(codegen::ir::Type::int(64).unwrap(), function_index as i64);

        let fallback_ip = self
            .builder
            .ins()
            .iconst(codegen::ir::Type::int(64).unwrap(), self.ip as i64);

        // Advance to the next thing
        self.ip += 1;

        // TODO: Embed the function itself into the generated code?
        // How to tell if this slot is mutable?
        let func = self.globals.get(function_index).unwrap();

        let mut arg_values = vec![ctx, lookup_index, fallback_ip];
        arg_values.extend(self.stack.drain(self.stack.len() - arity..).map(|x| x.0));

        // Check if its a function - otherwise, just spill the values to the stack.
        let is_function = self.check_function(lookup_index);

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

            // Only spill what hasn't been spilled already?
            for value in self.stack.clone() {
                self.push_to_vm_stack(value.0);
            }

            let else_return = BlockArg::Value(self.create_i128(0));

            self.builder.ins().jump(merge_block, &[else_return]);

            // Switch to the merge block for subsequent statements.
            self.builder.switch_to_block(merge_block);

            // We've now seen all the predecessors of the merge block.
            self.builder.seal_block(merge_block);

            // Read the value of the if-else by reading the merge block
            // parameter.
            // let phi = self.builder.block_params(merge_block)[0];
        }

        // TODO:
        // Check if this is a native function before deopting? This _has_ to be encoded into the
        // function to call, otherwise we might not deopt correctly.
        // if let SteelVal::Closure(_) = func {
        //     println!("Deopting...");
        //     for value in self.stack.clone() {
        //         self.push_to_vm_stack(value.0);
        //     }
        // }

        let call = self.builder.ins().call(local_callee, &arg_values);
        let result = self.builder.inst_results(call)[0];
        result
    }

    fn check_function(&mut self, index: Value) -> Value {
        let mut sig = self.module.make_signature();
        let name = "check-callable";
        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(64).unwrap()));
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));
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

    fn check_deopt(&mut self) -> Value {
        let mut sig = self.module.make_signature();
        let name = "vm-should-continue?";
        // VmCore pointer
        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
        let variable = self.variables.get("vm-ctx").expect("variable not defined");
        let ctx = self.builder.use_var(*variable);
        let call = self.builder.ins().call(local_callee, &[ctx]);
        let result = self.builder.inst_results(call)[0];
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder
            .append_block_param(merge_block, codegen::ir::Type::int(8).unwrap());
        // Do the thing.
        self.builder
            .ins()
            .brif(result, then_block, &[], else_block, &[]);
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = BlockArg::Value(
            self.builder
                .ins()
                .iconst(codegen::ir::Type::int(8).unwrap(), 1),
        );
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
        let else_return = self
            .builder
            .ins()
            .iconst(codegen::ir::Type::int(8).unwrap(), 1);
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

    fn func_ret_val(
        &mut self,
        op: OpCode,
        payload: usize,
        ip_inc: usize,
        inferred_type: InferredType,
        abi_param_type: AbiParam,
        abi_return_type: AbiParam,
    ) {
        let function_name = op_to_name_payload(op, payload);
        let args = self.stack.split_off(self.stack.len() - payload);

        // TODO: Use the type hints! For now we're not going to for the sake
        // of getting something running
        let args = args
            .into_iter()
            .map(|x| (x.0, abi_param_type))
            .collect::<Vec<_>>();
        let result = self.call_function_returns_value_args(function_name, &args, abi_return_type);

        // Check the inferred type, if we know of it
        self.stack.push((result, inferred_type));
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
                        sig.params
                            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

                        // Three args
                        sig.params
                            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));
                        sig.params
                            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));
                        sig.params
                            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

                        // offset
                        sig.params
                            .push(AbiParam::new(codegen::ir::Type::int(64).unwrap()));

                        // For simplicity for now, just make all calls return a single I64.
                        sig.returns
                            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

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

                        let func_var = self.create_i128(unsafe { std::mem::transmute(func) });

                        let offset = self
                            .builder
                            .ins()
                            .iconst(codegen::ir::Type::int(64).unwrap(), 3);

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
                    let mut then_return = BlockArg::Value(
                        self.builder
                            .ins()
                            .iconst(codegen::ir::Type::int(8).unwrap(), 1),
                    );

                    // // Set the ip to the right spot:
                    // self.ip = then_start;
                    self.translate_instructions();

                    // Jump to the merge block, passing it the block return value.
                    self.builder.ins().jump(merge_block, &[then_return]);

                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);

                    // // TODO: Update with the proper return value
                    let mut else_return = self
                        .builder
                        .ins()
                        .iconst(codegen::ir::Type::int(8).unwrap(), 1);

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
            OpCode::LOADINT0 => {
                self.create_i128(unsafe { std::mem::transmute(SteelVal::INT_ZERO) })
            }
            OpCode::LOADINT1 => self.create_i128(unsafe { std::mem::transmute(SteelVal::INT_ONE) }),
            OpCode::LOADINT2 => self.create_i128(unsafe { std::mem::transmute(SteelVal::INT_TWO) }),
            OpCode::PUSHCONST => {
                // Attempt to inline the constant, if it is something that can be inlined.
                // Assuming we know the type of it, we can get really fancy here since
                // we _should_ be able to do something with it if there are other types
                // in play - we can avoid the unboxing / boxing of the type if we know
                // what we're dealing with.

                let constant = self.constants.get(payload);

                println!("Embedding immediate: {}", constant);

                match &constant {
                    SteelVal::BoolV(_) | SteelVal::IntV(_) => {
                        self.create_i128(unsafe { std::mem::transmute(constant) })
                    }
                    _ => self.call_function_returns_value(op_to_name_payload(op1, payload)),
                }
            }
            _ => self.call_function_returns_value(op_to_name_payload(op1, payload)),
        }
    }

    // Just... store as a i128, and hope for the best. Don't need to encode
    // it directly as anything really?
    fn create_i128(&mut self, value: i128) -> Value {
        let [left, right] = split_big(value);
        let int = codegen::ir::Type::int(64).unwrap();

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

        // let frozen_stack = self.stack.clone();
        // println!("Traversing then branch");
        // println!("Stack before then branch: {:?}", frozen_stack);
        self.stack_to_ssa();
        // println!("Done on then");
        // println!("Stack after then branch: {:?}", self.stack);

        // Unwrap or... must have been a tail call?
        let then_return = BlockArg::Value(
            self.stack
                .pop()
                .map(|x| x.0)
                .unwrap_or_else(|| dbg!(self.create_i128(encode(SteelVal::Void)))),
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

        // self.stack = frozen_stack;

        self.stack_to_ssa();

        // let else_return = self.stack.pop().unwrap().0;
        let else_return = BlockArg::Value(
            self.stack
                .pop()
                .map(|x| x.0)
                .unwrap_or_else(|| dbg!(self.create_i128(encode(SteelVal::Void)))),
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
        let mut then_return = BlockArg::Value(
            self.builder
                .ins()
                .iconst(codegen::ir::Type::int(8).unwrap(), 1),
        );

        // Set the ip to the right spot:
        self.ip = then_start;
        self.translate_instructions();

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        // TODO: Update with the proper return value
        let mut else_return = BlockArg::Value(
            self.builder
                .ins()
                .iconst(codegen::ir::Type::int(8).unwrap(), 1),
        );

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

        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(64).unwrap()));

        // For simplicity for now, just make all calls return a single I64.
        // sig.returns
        //     .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let ip = self
            .builder
            .ins()
            .iconst(codegen::ir::Type::int(64).unwrap(), ip as i64);

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

        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

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

    fn push_to_vm_stack(&mut self, value: Value) {
        let mut sig = self.module.make_signature();
        let name = "push-to-vm-stack";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        sig.params
            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

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
        // let result = self.builder.inst_results(call)[0];
        // result
    }

    fn call_function_returns_value(&mut self, name: &str) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // For simplicity for now, just make all calls return a single I64.
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(128).unwrap()));

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

    // TODO: Call the branch handler, get the return value,
    // jump to where we need to go.
    fn call_branch_handler(&mut self) -> Value {
        // fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();
        let name = "if-branch";

        sig.params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        // For simplicity for now, just make all calls return a single I64.
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

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
        sig.returns
            .push(AbiParam::new(codegen::ir::Type::int(8).unwrap()));

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
/// Recursively descend through the AST, translating all implicit
/// variable declarations.
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
