use crate::gc::Gc;
use crate::jit::ir::*;
use crate::jit::value::{
    decode, to_encoded_double, to_encoded_double_from_const_ptr, to_encoded_double_raw,
};
use crate::parser::ast::ExprKind;
use crate::rvals::ConsCell;
use crate::SteelVal;
use cranelift::prelude::types::{F64, I64};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use im_rc::Vector;
use std::collections::{HashMap, HashSet};
use std::slice;

// use lazy_static::lazy_static;
use std::cell::RefCell;
// use typed_arena::Arena;

use super::lower::lower_function;
use super::sig::{JitFunctionPointer, Sig};
use super::value::{encode_bool, from_i32, get_ref_from_double, FALSE_VALUE};

thread_local! {
    pub static MEMORY: RefCell<Vec<Gc<SteelVal>>> = RefCell::new(Vec::new());
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

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    /// Legal vars - functions that are currently deemed legal to reference
    legal_vars: HashSet<String>,
}

unsafe extern "C" fn length(value: f64) -> f64 {
    let lst = get_ref_from_double(value);

    if let SteelVal::Pair(_) = lst {
        let count = SteelVal::iter(lst).count() as isize;
        // to_encoded_double()
        // unimplemented!();
    }

    unimplemented!()
}

unsafe extern "C" fn empty_const() -> f64 {
    let empty_list = Gc::new(SteelVal::VectorV(Gc::new(Vector::new())));
    JIT::allocate(&empty_list);
    to_encoded_double(&empty_list)
}

// Prints a value used by the compiled code. Our JIT exposes this
// function to compiled code with the name "print".
// TODO audit the unsafe
unsafe extern "C" fn car(value: f64) -> f64 {
    // let lst: Box<SteelVal> = std::mem::transmute(value);

    // let lst = &*(value as *const SteelVal);

    let lst = get_ref_from_double(value);

    // println!("car address: {:p}", lst);

    if let SteelVal::Pair(c) = lst {
        let ret_value = &c.car;

        // println!("car output: {:?}", ret_value);
        to_encoded_double_raw(ret_value)

        // (ret_value as *const SteelVal) as isize
    } else {
        panic!("car expected a list, found: {:?}", lst);
    }

    // unimplemented!()
}

// TODO implement the cdr and see what happens
// Safety: The caller must assure that the value being passed in is a reference to
// a GC value
unsafe extern "C" fn cdr(value: f64) -> f64 {
    let lst = get_ref_from_double(value);

    if let SteelVal::Pair(c) = lst {
        let rest = c.cdr().as_ref().map(Gc::clone);

        if let Some(rest) = rest {
            let new_pair = Gc::new(SteelVal::Pair(rest));

            // We want to increment the life time of this so that references are valid later
            JIT::allocate(&new_pair);

            // println!("cdr output: {:?}", new_pair);

            to_encoded_double(&new_pair)
        } else {
            let empty_list = Gc::new(SteelVal::VectorV(Gc::new(Vector::new())));
            JIT::allocate(&empty_list);
            to_encoded_double(&empty_list)
        }
    } else {
        panic!("cdr expected a list");
    }
}

// TODO how to do booleans?
unsafe extern "C" fn empty(l: f64) -> f64 {
    let lst = decode(l);

    encode_bool(if let SteelVal::VectorV(v) = lst {
        v.is_empty()
    } else {
        false
    })
}

// TODO
// Implement values with a tag to know if they're a primitive or a reference
// type - would let me not have to register non values in memory
unsafe extern "C" fn cons(car: f64, cdr: f64) -> f64 {
    let car = decode(car);
    let cdr = decode(cdr);

    match &cdr {
        SteelVal::Pair(cdr) => {
            let new_value = Gc::new(SteelVal::Pair(Gc::new(ConsCell::new(
                car.clone(),
                Some(cdr.clone()),
            ))));

            // Register the allocated value so that it lives long enough
            JIT::allocate(&new_value);
            to_encoded_double(&new_value)
        }
        SteelVal::VectorV(l) => {
            let new_value = if l.is_empty() {
                Gc::new(SteelVal::Pair(Gc::new(ConsCell::new(car, None))))
            } else {
                Gc::new(SteelVal::Pair(Gc::new(ConsCell::new(
                    car,
                    Some(Gc::new(ConsCell::new(cdr, None))),
                ))))
            };

            JIT::allocate(&new_value);
            to_encoded_double(&new_value)
        }
        _ => panic!(
            "cons requires a list as the second argument, found: {}",
            cdr
        ),
    }
}

unsafe extern "C" fn _equals(left: f64, right: f64) -> f64 {
    let left = decode(left);
    let right = decode(right);

    encode_bool(left == right)
}

unsafe extern "C" fn _less_than_or_equals(left: f64, right: f64) -> f64 {
    let left = decode(left);
    let right = decode(right);

    encode_bool(left <= right)
}

// unsafe extern "C" fn addition(left: f64, right: f64) -> f64 {
//     let left = decode(left);
//     let right = decode(right);

//     if let (SteelVal::IntV(l), SteelVal::IntV(r)) = (left, right) {
//         to_encoded_double(SteelVal::IntV(l + r))
//     } else {
//         panic!("Addition expected two numbers")
//     }
// }

fn register_primitives(builder: &mut JITBuilder) {
    let addr: *const u8 = car as *const u8;
    builder.symbol("car", addr);

    let addr: *const u8 = cdr as *const u8;
    builder.symbol("cdr", addr);

    let addr: *const u8 = cons as *const u8;
    builder.symbol("cons", addr);

    let addr: *const u8 = empty as *const u8;
    builder.symbol("empty?", addr);
    builder.symbol("null?", addr);

    let addr: *const u8 = empty_const as *const u8;
    builder.symbol("empty", addr);

    let outer_value = "hello world!";

    // First convert function to this using register function
    let outer = move |args: &[SteelVal]| -> SteelVal {
        println!("{}", outer_value);
        SteelVal::BoolV(false)
    };

    unsafe extern "C" fn func(x: f64) -> f64 {
        unimplemented!()
        // to_encoded_double(decode(x))
    }

    // How do you take a closure and turn it into something not a closure without macros?
    // Unique type is unable to be encoded into a JIT callable function
    let addr: *const u8 = func as *const u8;
    builder.symbol("external-func", addr);
}

impl Default for JIT {
    fn default() -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());

        register_primitives(&mut builder);

        let module = JITModule::new(builder);

        let mut output = Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            legal_vars: HashSet::new(),
        };

        output.register_primitives();
        output
    }
}

impl JIT {
    // Wipes out the memory stores in the typed area
    pub fn new() -> Self {
        JIT::free();
        Self::default()
    }

    // Wipe out the memory
    pub fn free() {
        MEMORY.with(|x| {
            *x.borrow_mut() = Vec::new();
        });
    }

    // Keep track of the refs that get heap allocated
    pub(crate) fn allocate(value: &Gc<SteelVal>) {
        MEMORY.with(|x| x.borrow_mut().push(Gc::clone(value)))
    }

    fn register_primitives(&mut self) {
        self.register_symbol("car");
        self.register_symbol("cdr");
        self.register_symbol("cons");
        self.register_symbol("empty?");
        self.register_symbol("null?");
        self.register_symbol("empty");
    }

    fn register_symbol(&mut self, name: &str) {
        println!("Registering function: {}", name);

        self.legal_vars.insert(name.to_string());
    }

    // pub unsafe fn compile_and_transmute(
    //     &mut self,
    //     input: &ExprKind,
    //     sig: Sig,
    // ) -> Result, String> {
    //     let code_ptr = self.compile(input)?;
    //     let code_fn = std::mem::transmute::<_, T>(code_ptr);
    //     Ok(code_fn)
    // }

    /// Compile a string in the toy language into machine code.
    pub fn compile(&mut self, input: &ExprKind) -> Result<JitFunctionPointer, String> {
        // TOOD lift this out of this function
        // let mut legal_vars = HashSet::new();

        // Register the functions that are legal to reference inside the machine code
        // legal_vars.insert("car".to_string());
        // legal_vars.insert("cdr".to_string());
        // legal_vars.insert("cons".to_string());
        // legal_vars.insert("empty?".to_string());
        // legal_vars.insert("null?".to_string());

        // First, parse the string, producing AST nodes.
        let (name, params, the_return, stmts) = lower_function(input, &mut self.legal_vars)
            .ok_or_else(|| "Unable to lower the input AST".to_string())?;
        // type_check_please(&input).map_err(|e| e.to_string())?;

        // Get the arity from the number of parameters of the function we're compiling
        let arity = Sig::from_usize(params.len());

        println!("Function name: {}", name);
        println!("Params: {:?}", params);
        // println!("Return value: {}", the_return);
        // println!("Statements: {:?}", stmts);

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(params, the_return, stmts)?;

        println!("\n\n{}\n\n", self.ctx.func);

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

        // println!("Made it here!");

        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        self.module
            .define_function(
                id,
                &mut self.ctx,
                &mut codegen::binemit::NullTrapSink {},
                &mut codegen::binemit::NullStackMapSink {},
            )
            .map_err(|e| {
                println!("{:?}", e);
                e.to_string()
            })?;

        // println!("Defined function!");

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        // This _is_ a legal function now after the function is finalized
        self.register_symbol(&name);

        Ok(JitFunctionPointer::new(arity, code))
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        // The steps here are analogous to `compile`, except that data is much
        // simpler than functions.
        self.data_ctx.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_ctx)
            .map_err(|e| e.to_string())?;
        self.data_ctx.clear();
        self.module.finalize_definitions();
        let buffer = self.module.get_finalized_data(id);
        // TODO: Can we move the unsafe into cranelift?
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(
        &mut self,
        params: Vec<String>,
        the_return: String,
        stmts: Vec<Expr>,
    ) -> Result<(), String> {
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        // TODO come back to this to figure out the best way to handle different targets
        // let int = self.module.target_config().pointer_type();

        let int = F64;

        for _p in &params {
            self.ctx.func.signature.params.push(AbiParam::new(int));
        }

        // Our toy language currently only supports one return value, though
        // Cranelift is designed to support more.
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        println!("Function signature: {:?}", self.ctx.func.signature);

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
        let variables =
            declare_variables(int, &mut builder, &params, &the_return, &stmts, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
        };
        for expr in stmts {
            trans.translate_expr(expr);
        }

        // Set up the return variable of the function. Above, we declared a
        // variable to hold the return value. Here, we just do a use of that
        // variable.
        let return_variable = trans.variables.get(&the_return).unwrap();
        let return_value = trans.builder.use_var(*return_variable);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();

        // debug out what we can at this point
        // println!("\n{}", trans.builder.display(trans.module.isa()));

        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    // Comes in as a float
    // This unboxes the value first by applying the bitwise and
    // then casting to an int
    fn decode_float_to_int(&mut self, value: Value) -> Value {
        let bitmask: i64 = unsafe { std::mem::transmute(!super::value::INT32_TAG) };
        let cast = self.builder.ins().bitcast(I64, value);
        self.builder.ins().band_imm(cast, bitmask)
    }

    // TODO
    // currently decode float to int just takes an encoded value and blindly decodes to an integer
    fn decode_float_or_steelval_to_int(&mut self, value: Value) -> Value {
        // let bitmask
        unimplemented!()
    }

    // fn decode_bool(&mut self, value: Value) -> Value {
    //     self.builder.ins().isub_imm()
    // }

    // This takes in an int, then applies the bitwise and
    // then we cast this to a float when we're done
    fn encode_int_to_float(&mut self, value: Value) -> Value {
        let bitmask: i64 = unsafe { std::mem::transmute(super::value::INT32_TAG) };
        let encoded_int = self.builder.ins().bor_imm(value, bitmask);
        self.builder.ins().bitcast(F64, encoded_int)
    }

    /// When you write out instructions in Cranelift, you get back `Value`s. You
    /// can then use these references in other instructions.
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Literal(literal) => {
                let imm: i32 = literal.parse().unwrap();
                // Literal needs to be encoded as an int first
                // let value = self.builder.ins().iconst(self.int, i64::from(imm));

                // let value = self.builder.ins().iconst(I64, i64::from(imm));

                // println!("Encoding literal: {}", imm);
                // self.encode_int_to_float(value)

                let encoded = from_i32(imm);
                self.builder.ins().f64const(encoded)
            }

            Expr::Add(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                println!("About to decode the floats to ints");

                let decoded_lhs = self.decode_float_to_int(lhs);
                let decoded_rhs = self.decode_float_to_int(rhs);

                println!("Finished decoding ints");
                // println!("Successfully decoded ")

                // let output = self.builder.ins().iadd(lhs, rhs);
                let output = self.builder.ins().iadd(decoded_lhs, decoded_rhs);

                println!("Generated add instruction");

                let output = self.encode_int_to_float(output);

                println!("Encoding the result back to a float");

                output

                // self.builder.ins().xo
            }

            Expr::Sub(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);

                let decoded_lhs = self.decode_float_to_int(lhs);
                let decoded_rhs = self.decode_float_to_int(rhs);

                let output = self.builder.ins().isub(decoded_lhs, decoded_rhs);

                self.encode_int_to_float(output)
                // let output = self.

                // self.builder.ins().isub(lhs, rhs)
            }

            Expr::Mul(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                self.builder.ins().imul(lhs, rhs)
            }

            Expr::Div(lhs, rhs) => {
                let lhs = self.translate_expr(*lhs);
                let rhs = self.translate_expr(*rhs);
                self.builder.ins().udiv(lhs, rhs)
            }

            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),

            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
            // Expr::Le(lhs, rhs) => {
            //     // self.translate_call("<=".to_string(), vec![*lhs, *rhs])
            //     self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs)
            // }
            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),

            Expr::Call(name, args) => self.translate_call(name, args),
            Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
            Expr::Identifier(name) => {
                // `use_var` is used to read the value of a variable.
                let variable = self.variables.get(&name).expect("variable not defined");
                self.builder.use_var(*variable)
            }
            Expr::Assign(name, expr) => self.translate_assign(name, *expr),
            Expr::IfElse(condition, then_body, else_body) => {
                self.translate_if_else(*condition, then_body, else_body)
            }
            Expr::WhileLoop(condition, loop_body) => {
                self.translate_while_loop(*condition, loop_body)
            }
            Expr::Block(body) => self.translate_block(body),
        }
    }

    fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
        // `def_var` is used to write the value of a variable. Note that
        // variables can have multiple definitions. Cranelift will
        // convert them into SSA form for itself automatically.
        let new_value = self.translate_expr(expr);
        let variable = self.variables.get(&name).unwrap();
        self.builder.def_var(*variable, new_value);
        new_value
    }

    // icmp needs to keep track of the value
    // TODO remove using icmp
    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
        let lhs = self.translate_expr(lhs);
        let rhs = self.translate_expr(rhs);

        let encoded_lhs = self.decode_float_to_int(lhs);
        let encoded_rhs = self.decode_float_to_int(rhs);

        let c = self.builder.ins().icmp(cmp, encoded_lhs, encoded_rhs);

        // self.builder.ins().bint(IntTo, x)
        // let encoded = self.builder.ins.icmp(c)

        let one_or_zero = self.builder.ins().bint(I64, c);

        // Encode the resulting value back to the encoded constant
        let int_bool = self.builder.ins().iadd_imm(one_or_zero, FALSE_VALUE as i64);
        self.builder.ins().bitcast(F64, int_bool)
    }

    // Translate a block of instructions
    fn translate_block(&mut self, expr_block: Vec<Expr>) -> Value {
        let block = self.builder.create_block();

        // self.builder.append_block_param(block, self.int);

        self.builder.append_block_params_for_function_params(block);

        // self.builder.switch_to_block(block);
        self.builder.seal_block(block);
        // TODO
        // let mut block_return = self.builder.ins().iconst(self.int, 0);

        let mut block_return = self.builder.ins().f64const(0.0);
        for expr in expr_block {
            block_return = self.translate_expr(expr)
        }
        // block_return
        // self.builder.block_params(block)[0]
        block_return
    }

    fn translate_if_else(
        &mut self,
        condition: Expr,
        then_body: Vec<Expr>,
        else_body: Vec<Expr>,
    ) -> Value {
        let condition_value = self.translate_expr(condition);
        // Cast this back from a float to an int
        let decoded_value = self.builder.ins().bitcast(I64, condition_value);
        // let encoded_condition = self.decode_float_to_int(condition_value);

        // Decode the condition - translate whatever value we have back to 0
        // The _only_ value that will give us false here, is the FALSE_VALUE constant
        let condition_value = self
            .builder
            .ins()
            .iadd_imm(decoded_value, -(FALSE_VALUE as i64));

        // TODO insert way to decode value from boolean to value cranelift can understand?

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
        self.builder.ins().brz(condition_value, else_block, &[]);
        // Fall through to then block.
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        // let mut then_return = self.builder.ins().iconst(self.int, 0);

        let mut then_return = self.builder.ins().f64const(0.0);
        for expr in then_body {
            then_return = self.translate_expr(expr);
        }

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        // let mut else_return = self.builder.ins().iconst(self.int, 0);

        let mut else_return = self.builder.ins().f64const(0.0);
        for expr in else_body {
            else_return = self.translate_expr(expr);
        }

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

    fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_expr(condition);
        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        for expr in loop_body {
            self.translate_expr(expr);
        }
        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);

        // We've reached the bottom of the loop, so there will be no
        // more backedges to the header to exits to the bottom.
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        // Just return 0 for now.
        self.builder.ins().iconst(self.int, 0)
    }

    fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
        let mut sig = self.module.make_signature();

        // Add a parameter for each argument.
        for _arg in &args {
            sig.params.push(AbiParam::new(self.int));
        }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(self.int));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.translate_expr(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn translate_global_data_addr(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");
        let local_id = self
            .module
            .declare_data_in_func(sym, &mut self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }
}

fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
    the_return: &str,
    stmts: &[Expr],
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, name) in params.iter().enumerate() {
        // TODO: cranelift_frontend should really have an API to make it easy to set
        // up param variables.
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
    }
    let zero = builder.ins().f64const(0.0);
    let return_variable = declare_variable(int, builder, &mut variables, &mut index, the_return);
    builder.def_var(return_variable, zero);
    for expr in stmts {
        declare_variables_in_stmt(int, builder, &mut variables, &mut index, expr);
    }

    variables
}

/// Recursively descend through the AST, translating all implicit
/// variable declarations.
fn declare_variables_in_stmt(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    expr: &Expr,
) {
    match *expr {
        Expr::Assign(ref name, ref expr) => {
            declare_variable(int, builder, variables, index, name);
            // Declare variables on the right hand side as well
            declare_variables_in_stmt(int, builder, variables, index, expr);
        }
        Expr::IfElse(ref _condition, ref then_body, ref else_body) => {
            for stmt in then_body {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
            for stmt in else_body {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
        }
        Expr::WhileLoop(ref _condition, ref loop_body) => {
            for stmt in loop_body {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
        }
        Expr::Block(ref body) => {
            for stmt in body {
                declare_variables_in_stmt(int, builder, variables, index, &stmt);
            }
        }
        _ => (),
    }
}

/// Declare a single variable declaration.
fn declare_variable(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    println!("Declaring: {}", name);
    let var = Variable::new(*index);
    if !variables.contains_key(name) {
        variables.insert(name.into(), var);
        builder.declare_var(var, int);
        *index += 1;
    }
    var
}
