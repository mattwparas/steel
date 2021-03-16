use crate::stack::{CallStack, EnvStack, Stack, StackFrame};
use crate::{contracts::ContractedFunctionExt, heap::Heap, transducers::TransducerExt};
use steel::{
    contracts::ContractedFunction,
    core::{instructions::DenseInstruction, opcode::OpCode},
    rvals::FutureResult,
    steel_compiler::{
        constants::{ConstantMap, ConstantTable},
        program::Program,
    },
};

use std::{cell::RefCell, collections::HashMap, convert::TryFrom, iter::Iterator, rc::Rc, result};
use steel::{
    env::Env,
    gc::Gc,
    parser::{
        ast::ExprKind,
        parser::{ParseError, Parser},
        span::Span,
    },
    primitives::ListOperations,
    rerrs::{ErrorKind, SteelErr},
    rvals::{ByteCodeLambda, Result, SteelVal},
    stop,
    structs::SteelStruct,
};

use crate::evaluation_progress::EvaluationProgress;

pub type Callback = fn(usize) -> bool;

use log::error;

const STACK_LIMIT: usize = 100000;

// lazy_static! {
//     static ref CALL_STACK: Mutex<Vec<u8>> = Mutex::new(vec![]);
// }

pub struct VirtualMachineCore {
    global_env: Rc<RefCell<Env>>,
    global_heap: Heap,
    callback: EvaluationProgress,
}

impl VirtualMachineCore {
    pub fn new() -> VirtualMachineCore {
        VirtualMachineCore {
            global_env: Rc::new(RefCell::new(Env::root())),
            global_heap: Heap::new(),
            callback: EvaluationProgress::new(),
        }
    }

    pub fn insert_binding(&mut self, idx: usize, value: SteelVal) {
        self.global_env.borrow_mut().add_root_value(idx, value);
    }

    pub fn insert_bindings(&mut self, vals: Vec<(usize, SteelVal)>) {
        for (idx, value) in vals {
            self.global_env.borrow_mut().add_root_value(idx, value);
        }
    }

    pub fn extract_value(&self, idx: usize) -> Option<SteelVal> {
        self.global_env.borrow().extract(idx)
    }

    // pub fn new_with_meta() -> VirtualMachine {
    //     let mut vm = VirtualMachineCore::new();
    //     vm.insert_binding("*env*".to_string(), Env::constant_env_to_hashmap());
    //     vm
    // }

    // pub fn insert_binding(&mut self, name: String, value: SteelVal) {
    //     self.global_env
    //         .borrow_mut()
    //         .add_rooted_value(&mut self.ctx.symbol_map, (name.as_str(), value));
    // }

    // pub fn insert_gc_binding(&mut self, name: String, value: SteelVal) {
    //     self.global_env
    //         .borrow_mut()
    //         .add_rooted_gc_value(&mut self.ctx.symbol_map, (name.as_str(), value));
    // }

    // pub fn insert_bindings(&mut self, vals: Vec<(String, SteelVal)>) {
    //     self.global_env
    //         .borrow_mut()
    //         .repl_define_zipped_rooted(&mut self.ctx.symbol_map, vals.into_iter());
    // }

    pub fn on_progress(&mut self, callback: Callback) {
        &self.callback.with_callback(callback);
    }

    pub fn print_bindings(&self) {
        println!(
            "Env length: {}",
            self.global_env.borrow().bindings_map().len()
        );
        println!("{:?}", self.global_env.borrow().bindings_map());
    }

    pub fn roll_back(&mut self, _idx: usize) {
        unimplemented!()
    }

    // Read in the file from the given path and execute accordingly
    // Loads all the functions in from the given env
    // pub fn parse_and_execute_from_path<P: AsRef<Path>>(
    //     &mut self,
    //     path: P,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<SteelVal>> {
    //     let mut file = std::fs::File::open(path)?;
    //     let mut exprs = String::new();
    //     file.read_to_string(&mut exprs)?;
    //     self.parse_and_execute(exprs.as_str())
    // }

    // pub fn parse_and_execute_without_optimizations(
    //     &mut self,
    //     expr_str: &str,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<SteelVal>> {
    //     // let now = Instant::now();
    //     let gen_bytecode = self.emit_instructions(expr_str, false)?;

    //     gen_bytecode
    //         .into_iter()
    //         .map(|x| {
    //             let code = Rc::from(x.into_boxed_slice());
    //             let res = self.execute(code, true);
    //             res
    //         })
    //         .collect::<Result<Vec<SteelVal>>>()
    // }

    pub fn execute_program(&mut self, program: Program) -> Result<Vec<SteelVal>> {
        // unimplemented!()

        let Program {
            instructions,
            constant_map,
        } = program;

        let constant_map = ConstantMap::from_bytes(&constant_map)?;

        instructions
            .into_iter()
            .map(|x| {
                let code = Rc::from(x.into_boxed_slice());
                // let now = std::time::Instant::now();
                let res = self.execute(code, &constant_map);
                // println!("{:?}", now.elapsed());
                res
            })
            .collect()
    }

    pub fn execute_program_by_ref(&mut self, program: &Program) -> Result<Vec<SteelVal>> {
        // unimplemented!()

        let Program {
            instructions,
            constant_map,
        } = program;

        let constant_map = ConstantMap::from_bytes(&constant_map)?;
        let instructions: Vec<_> = instructions
            .clone()
            .into_iter()
            .map(|x| Rc::from(x.into_boxed_slice()))
            .collect();

        instructions
            .into_iter()
            .map(|code| {
                let res = self.execute(code, &constant_map);
                res
            })
            .collect()
    }

    // pub fn new_with_std

    // pub fn parse_and_execute(
    //     &mut self,
    //     expr_str: &str,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<SteelVal>> {
    //     // let now = Instant::now();
    //     let gen_bytecode = self.emit_instructions(expr_str, true)?;

    //     // previous size of the env
    //     // let length = self.global_env.borrow().len();

    //     // println!("Bytecode generated in: {:?}", now.elapsed());
    //     gen_bytecode
    //         .into_iter()
    //         .map(|x| {
    //             let code = Rc::from(x.into_boxed_slice());
    //             // let now = Instant::now();
    //             // let constant_map = &self.ctx.constant_map;
    //             // let repl = self.ctx.repl;
    //             // let mut heap = Vec::new();
    //             let res = self.execute(code);
    //             // println!("Time taken: {:?}", now.elapsed());
    //             res
    //         })
    //         .collect::<Result<Vec<SteelVal>>>()
    // }

    // pub fn optimize_exprs<I: IntoIterator<Item = Expr>>(
    //     exprs: I,
    //     // ctx: &mut Ctx<ConstantMap>,
    // ) -> Result<Vec<Expr>> {
    //     // println!("About to optimize the input program");

    //     let converted: Result<Vec<_>> = exprs
    //         .into_iter()
    //         .map(|x| SteelVal::try_from(x.clone()))
    //         .collect();

    //     // let converted = Gc::new(SteelVal::try_from(v[0].clone())?);
    //     let exprs = ListOperations::built_in_list_func_flat_non_gc(converted?)?;

    //     let mut vm = VirtualMachine::new_with_meta();
    //     vm.parse_and_execute_without_optimizations(crate::stdlib::PRELUDE)?;
    //     vm.insert_gc_binding("*program*".to_string(), exprs);
    //     let output = vm.parse_and_execute_without_optimizations(crate::stdlib::COMPILER)?;

    //     // println!("{:?}", output.last().unwrap());

    //     // if output.len()  1 {
    //     //     stop!(Generic => "panic! internal compiler error: output did not return a valid program");
    //     // }

    //     // TODO
    //     SteelVal::iter(Gc::clone(output.last().unwrap()))
    //         .into_iter()
    //         .map(|x| Expr::try_from(x.as_ref()).map_err(|x| SteelErr::Generic(x.to_string(), None)))
    //         .collect::<Result<Vec<Expr>>>()

    //     // unimplemented!()

    //     // self.emit_instructions_from_exprs(parsed)
    // }

    pub fn execute(
        &mut self,
        instructions: Rc<[DenseInstruction]>,
        constant_map: &ConstantMap,
        // heap: &mut Vec<Rc<RefCell<Env>>>,
        // repl: bool,
    ) -> Result<SteelVal> {
        let stack = StackFrame::new();
        let mut heap = Heap::new();

        // give access to the global root via this method
        heap.plant_root(Rc::downgrade(&self.global_env));

        let repl = true;
        // let repl = false;

        let result = vm(
            instructions,
            stack,
            &mut heap,
            Rc::clone(&self.global_env),
            constant_map,
            repl,
            &self.callback,
        );

        if self.global_env.borrow().is_binding_context() {
            self.global_heap.append(&mut heap);
            self.global_env.borrow_mut().set_binding_context(false);
        }

        heap.clear();
        heap.reset_limit();

        result
    }
}

#[derive(Debug)]
pub struct InstructionPointer(usize, Rc<[DenseInstruction]>);

impl InstructionPointer {
    pub fn new_raw() -> Self {
        InstructionPointer(0, Rc::from(Vec::new().into_boxed_slice()))
    }

    pub fn new(ip: usize, instrs: Rc<[DenseInstruction]>) -> Self {
        InstructionPointer(ip, instrs)
    }

    pub fn instrs_ref(&self) -> &Rc<[DenseInstruction]> {
        &self.1
    }

    #[inline(always)]
    pub fn instrs(self) -> Rc<[DenseInstruction]> {
        self.1
    }
}

struct VmCore<'a, CT: ConstantTable> {
    instructions: Rc<[DenseInstruction]>,
    stack: StackFrame,
    heap: &'a mut Heap,
    global_env: Rc<RefCell<Env>>,
    instruction_stack: Stack<InstructionPointer>,
    stacks: CallStack,
    repl: bool,
    callback: &'a EvaluationProgress,
    constants: &'a CT,
    ip: usize,
    pop_count: usize,
    env_stack: EnvStack,
}

impl<'a, CT: ConstantTable> VmCore<'a, CT> {
    fn new(
        instructions: Rc<[DenseInstruction]>,
        stack: StackFrame,
        heap: &'a mut Heap,
        global_env: Rc<RefCell<Env>>,
        constants: &'a CT,
        repl: bool,
        callback: &'a EvaluationProgress,
    ) -> Result<VmCore<'a, CT>> {
        if instructions.is_empty() {
            stop!(Generic => "empty stack!")
        }

        Ok(VmCore {
            instructions: Rc::clone(&instructions),
            stack,
            heap,
            global_env,
            instruction_stack: Stack::new(),
            stacks: Stack::new(),
            repl,
            callback,
            constants,
            ip: 0,
            pop_count: 1,
            env_stack: Stack::new(),
        })
    }

    fn vm(mut self) -> Result<SteelVal> {
        let mut cur_inst;

        while self.ip < self.instructions.len() {
            cur_inst = self.instructions[self.ip];

            match cur_inst.op_code {
                OpCode::PANIC => self.handle_panic(cur_inst.span)?,
                OpCode::EVAL => {
                    let _expr_to_eval = self.stack.pop().unwrap();
                    panic!("eval not yet supported - internal compiler error");
                }
                OpCode::PASS => {
                    self.ip += 1;
                }
                OpCode::VOID => {
                    self.stack.push(SteelVal::Void);
                    self.ip += 1;
                }
                OpCode::STRUCT => {
                    // For now, only allow structs at the top level
                    // In the future, allow structs to be also available in a nested scope
                    self.handle_struct(cur_inst.payload_size as usize)?;
                    self.stack.push(SteelVal::Void);
                    self.ip += 1;
                    // return Ok(SteelVal::Void);
                }
                OpCode::CALLCC => {
                    todo!("Handling call/cc not yet implemented");
                }
                OpCode::READ => self.handle_read(&cur_inst.span)?,
                OpCode::COLLECT => self.handle_collect(&cur_inst.span)?,
                OpCode::COLLECTTO => self.handle_collect_to(&cur_inst.span)?,
                OpCode::TRANSDUCE => self.handle_transduce(&cur_inst.span)?,
                OpCode::SET => self.handle_set(cur_inst.payload_size as usize)?,
                OpCode::PUSHCONST => {
                    let val = self.constants.get(cur_inst.payload_size as usize);
                    self.stack.push(val);
                    self.ip += 1;
                }
                OpCode::PUSH => self.handle_push(cur_inst.payload_size as usize)?,
                OpCode::APPLY => self.handle_apply(cur_inst.span)?,
                OpCode::CLEAR => {
                    self.ip += 1;
                }
                OpCode::FUNC => {
                    self.handle_function_call(cur_inst.payload_size as usize, &cur_inst.span)?;
                }
                // Tail call basically says "hey this function is exiting"
                // In the closure case, transfer ownership of the stack to the called function
                OpCode::TAILCALL => {
                    self.handle_tail_call(cur_inst.payload_size as usize, &cur_inst.span)?
                }
                OpCode::IF => {
                    // change to truthy...
                    if self.stack.pop().unwrap().is_truthy() {
                        self.ip = cur_inst.payload_size as usize;
                    } else {
                        self.ip += 1;
                    }
                }
                OpCode::JMP => {
                    self.ip = cur_inst.payload_size as usize;
                    // HACK
                    if self.ip == 0 && self.heap.len() > self.heap.limit() {
                        self.heap.collect_garbage();
                    }
                }
                OpCode::POP => {
                    self.pop_count -= 1;
                    if self.pop_count == 0 {
                        self.env_stack.clear();

                        if cur_inst.payload_size as usize == 1 {
                            self.global_env.borrow_mut().set_binding_context(true);
                        }

                        let ret_val = self.stack.try_pop().ok_or_else(|| {
                            SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
                                .with_span(cur_inst.span)
                        });

                        self.global_env.borrow_mut().set_binding_offset(false);

                        return ret_val;
                    } else {
                        let ret_val = self.stack.pop().unwrap();
                        let prev_state = self.instruction_stack.pop().unwrap();

                        if !prev_state.instrs_ref().is_empty() {
                            // println!("not empty case");
                            self.global_env = self.env_stack.pop().unwrap();
                            self.ip = prev_state.0;
                            self.instructions = prev_state.instrs();
                        } else {
                            // println!("################## empty case ##################");
                            self.ip += 1;
                        }

                        // Idk maybe?
                        // self.global_env.borrow_mut().pop_child();

                        // println!("inside here");

                        self.stack = self.stacks.pop().unwrap();
                        self.stack.push(ret_val);

                        // println!("stack length: {}", self.stack.len());
                    }
                }
                OpCode::BIND => self.handle_bind(cur_inst.payload_size as usize),
                OpCode::SCLOSURE => self.handle_start_closure(cur_inst.payload_size as usize),
                OpCode::SDEF => self.handle_start_def(),
                OpCode::EDEF => {
                    self.global_env.borrow_mut().set_binding_context(false);
                    self.ip += 1;
                }

                OpCode::LOOKUP => {}
                OpCode::ECLOSURE => {}
                OpCode::NDEFS => {}
                OpCode::METALOOKUP => {}
            }

            match self.callback.call_and_increment() {
                Some(b) if !b => stop!(Generic => "Callback forced quit of function!"),
                _ => {}
            }
        }

        error!(
            "Out of bounds instruction!: instruction pointer: {}, instruction length: {}",
            self.ip,
            self.instructions.len()
        );
        steel::core::instructions::pretty_print_dense_instructions(&self.instructions);
        panic!("Out of bounds instruction")
    }

    #[inline(always)]
    fn handle_transduce(&mut self, span: &Span) -> Result<()> {
        let list = self.stack.pop().unwrap();
        let initial_value = self.stack.pop().unwrap();
        let reducer = self.stack.pop().unwrap();
        let transducer = self.stack.pop().unwrap();

        if let SteelVal::IterV(transducer) = &transducer {
            let ret_val = transducer.transduce(
                list,
                initial_value,
                reducer,
                self.constants,
                span,
                self.repl,
                self.callback,
            );
            self.stack.push(ret_val?);
        } else {
            stop!(Generic => "Transduce must take an iterable");
        }
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_collect_to(&mut self, span: &Span) -> Result<()> {
        let output_type = self.stack.pop().unwrap();
        let list = self.stack.pop().unwrap();
        let transducer = self.stack.pop().unwrap();

        if let SteelVal::IterV(transducer) = &transducer {
            let ret_val = transducer.run(
                list,
                self.constants,
                span,
                self.repl,
                self.callback,
                Some(output_type),
            );
            self.stack.push(ret_val?);
        } else {
            stop!(Generic => "Transducer execute takes a list"; *span);
        }
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_collect(&mut self, span: &Span) -> Result<()> {
        let list = self.stack.pop().unwrap();
        let transducer = self.stack.pop().unwrap();

        if let SteelVal::IterV(transducer) = &transducer {
            let ret_val =
                transducer.run(list, self.constants, span, self.repl, self.callback, None);
            self.stack.push(ret_val?);
        } else {
            stop!(Generic => "Transducer execute takes a list"; *span);
        }
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_panic(&mut self, span: Span) -> Result<()> {
        let error_message = self.stack.pop().unwrap();
        stop!(Generic => error_message.to_string(); span);
    }

    #[inline(always)]
    fn handle_struct(&mut self, offset: usize) -> Result<()> {
        let val = self.constants.get(offset);
        let mut iter = SteelVal::iter(val);

        // List of indices e.g. '(25 26 27 28) to bind struct functions to
        let indices = iter.next().unwrap();

        // The name of the struct
        let name: String = if let SteelVal::StringV(s) = iter.next().unwrap() {
            s.to_string()
        } else {
            stop!( Generic => "ICE: Struct expected a string name")
        };

        // The fields of the structs
        let fields: Vec<Gc<String>> = iter
            .map(|x| {
                if let SteelVal::StringV(s) = x {
                    Ok(s.clone())
                } else {
                    stop!(Generic => "ICE: Struct encoded improperly with non string fields")
                }
            })
            .collect::<Result<Vec<_>>>()?;

        // Get them as &str for now
        let other_fields: Vec<&str> = fields.iter().map(|x| x.as_str()).collect();

        // Generate the functions, but they immediately override them with the names
        // Store them with the indices
        let funcs = SteelStruct::generate_from_name_fields(name.as_str(), &other_fields)?;

        for ((_, func), idx) in funcs.into_iter().zip(SteelVal::iter(indices)) {
            let idx = if let SteelVal::IntV(idx) = idx {
                idx as usize
            } else {
                stop!(Generic => "Index wrong in structs")
            };

            self.global_env.borrow_mut().repl_define_idx(idx, func);
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_read(&mut self, span: &Span) -> Result<()> {
        // this needs to be a string
        let expression_to_parse = self.stack.pop().unwrap();

        if let SteelVal::StringV(expr) = expression_to_parse {
            // dummy interning hashmap because the parser is bad
            // please don't judge I'm working on fixing it
            // TODO
            let mut intern = HashMap::new();

            let parsed: result::Result<Vec<ExprKind>, ParseError> =
                Parser::new(expr.as_str(), &mut intern).collect();

            match parsed {
                Ok(v) => {
                    let converted: Result<Vec<SteelVal>> = v
                        .into_iter()
                        .map(|x| SteelVal::try_from(x.clone()))
                        .collect();

                    // let converted = Gc::new(SteelVal::try_from(v[0].clone())?);
                    self.stack
                        .push(ListOperations::built_in_list_func_flat_non_gc(converted?)?);
                    self.ip += 1;
                }
                Err(e) => stop!(Generic => format!("{}", e); *span),
            }
        } else {
            stop!(TypeMismatch => "read expects a string"; *span)
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_set(&mut self, index: usize) -> Result<()> {
        let value_to_assign = self.stack.pop().unwrap();

        if self.repl {
            let value = self
                .global_env
                .borrow_mut()
                .repl_set_idx(index, value_to_assign)?;

            self.stack.push(value);
        } else {
            unimplemented!();
        }
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_push(&mut self, index: usize) -> Result<()> {
        // TODO future me figure out the annoying offset issue
        // awful awful awful hack to fix the repl environment noise
        // cur_inst.payload_size as usize

        let value = self.global_env.borrow().repl_lookup_idx(index)?;
        self.stack.push(value);

        // TODO handle the offset situation
        // if self.repl {
        //     let value = self.global_env.borrow().repl_lookup_idx(index)?;
        //     self.stack.push(value);
        // } else {
        //     let value = self.global_env.borrow().lookup_idx(index)?;
        //     self.stack.push(value);
        // }

        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_start_closure(&mut self, offset: usize) {
        self.ip += 1;
        let forward_jump = offset - 1;
        // Snag the number of definitions here
        let ndefs = self.instructions[self.ip].payload_size;
        self.ip += 1;
        // Construct the closure body using the offsets from the payload
        // used to be - 1, now - 2
        let closure_body = self.instructions[self.ip..(self.ip + forward_jump - 1)].to_vec();

        // snag the arity from the eclosure instruction
        let arity = self.instructions[self.ip + forward_jump - 1].payload_size;

        let capture_env = Rc::clone(&self.global_env);

        let mut closure_offset = self.global_env.borrow().len();
        // println!("%%%%%%%%%%% Env length: {} %%%%%%%%%%%", closure_offset);

        // println!("{:?}", global_env.borrow().string_bindings_vec());

        if self.global_env.borrow().is_binding_context()
            && !self.global_env.borrow().is_binding_offset()
        {
            self.global_env.borrow_mut().set_binding_offset(true);
            closure_offset += 1;
        };

        // set the number of definitions for the environment
        // capture_env.borrow_mut().set_ndefs(ndefs as usize);

        // println!("Adding the capture_env to the heap!");
        self.heap.add(Rc::clone(&capture_env));
        // inspect_heap(&heap);
        let constructed_lambda = ByteCodeLambda::new(
            closure_body,
            Rc::downgrade(&capture_env),
            closure_offset,
            arity as usize,
            ndefs as usize,
        );

        self.stack
            .push(SteelVal::Closure(Gc::new(constructed_lambda)));

        self.ip += forward_jump;
    }

    #[inline(always)]
    fn handle_bind(&mut self, payload_size: usize) {
        self.global_env
            .borrow_mut()
            .repl_define_idx(payload_size, self.stack.pop().unwrap());

        // TODO handle the offset situation
        // if self.repl {
        //     self.global_env
        //         .borrow_mut()
        //         .repl_define_idx(payload_size, self.stack.pop().unwrap());
        // } else {
        //     let offset = self.global_env.borrow().local_offset();

        //     self.global_env
        //         .borrow_mut()
        //         .define_idx(payload_size - offset, self.stack.pop().unwrap());
        // }

        self.ip += 1;
    }

    #[inline(always)]
    fn handle_tail_call(&mut self, payload_size: usize, span: &Span) -> Result<()> {
        use SteelVal::*;
        let stack_func = self.stack.pop().unwrap();

        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size, span)?,
            FuncV(f) => self.call_primitive_func(f, payload_size, span)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => self.call_contracted_function(cf, payload_size, span)?,
            Closure(closure) => {
                if self.stacks.len() == STACK_LIMIT {
                    println!("stacks at exit: {:?}", self.stacks);
                    println!("stack frame at exit: {:?}", self.stack);
                    stop!(Generic => "stack overflowed!"; *span);
                }

                if closure.arity() != payload_size {
                    stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
                }

                let args = self.stack.split_off(self.stack.len() - payload_size);

                let parent_env = closure.sub_expression_env();
                // TODO remove this unwrap
                let offset =
                    closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

                let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                    parent_env.clone(),
                    offset,
                )));

                // TODO perhaps don't add a child here
                // parent_env
                //     .upgrade()
                //     .unwrap()
                //     .borrow_mut()
                //     .add_child(Rc::downgrade(&inner_env));

                // TODO future me to figure out with offsets
                // inner_env
                //     .borrow_mut()
                //     .reserve_defs(if closure.ndef_body() > 0 {
                //         closure.ndef_body() - 1
                //     } else {
                //         0
                //     });

                self.heap
                    .gather_mark_and_sweep_2(&self.global_env, &inner_env);
                self.heap.collect_garbage();

                self.global_env = inner_env;
                self.instructions = closure.body_exp();
                self.stack = args.into();
                self.ip = 0;
            }
            _ => {
                stop!(BadSyntax => "TailCall - Application not a procedure or function type not supported"; *span);
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn call_boxed_func(
        &mut self,
        func: &Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        let result = func(self.stack.peek_range(self.stack.len() - payload_size..))
            .map_err(|x| x.set_span(*span))?;

        self.stack.truncate(self.stack.len() - payload_size);

        self.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn call_primitive_func(
        &mut self,
        f: &fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        let result = f(self.stack.peek_range(self.stack.len() - payload_size..))
            .map_err(|x| x.set_span(*span))?;

        self.stack.truncate(self.stack.len() - payload_size);

        self.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn call_contracted_function(
        &mut self,
        cf: &ContractedFunction,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        if cf.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", cf.arity(), payload_size); *span);
        }

        let args = self.stack.split_off(self.stack.len() - payload_size);

        let result = cf.apply(
            args,
            self.heap,
            self.constants,
            span,
            self.repl,
            self.callback,
        )?;

        self.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    // &Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>

    #[inline(always)]
    fn call_future_func(
        &mut self,
        f: &Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>,
        payload_size: usize,
    ) -> Result<()> {
        let result = SteelVal::FutureV(Gc::new(f(self
            .stack
            .peek_range(self.stack.len() - payload_size..))?));

        self.stack.truncate(self.stack.len() - payload_size);
        self.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_function_call(&mut self, payload_size: usize, span: &Span) -> Result<()> {
        use SteelVal::*;
        let stack_func = self.stack.pop().unwrap();

        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size, span)?,
            FuncV(f) => self.call_primitive_func(f, payload_size, span)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => self.call_contracted_function(cf, payload_size, span)?,
            Closure(closure) => {
                if closure.arity() != payload_size {
                    stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
                }

                if self.stacks.len() == STACK_LIMIT {
                    // println!("stacks at exit: {:?}", stacks);
                    println!("stack frame at exit: {:?}", self.stack);
                    stop!(Generic => "stack overflowed!"; *span);
                }

                // Use smallvec here?
                let args = self.stack.split_off(self.stack.len() - payload_size);

                let parent_env = closure.sub_expression_env();

                // TODO remove this unwrap
                // TODO see if this offset is even necessary
                // let offset =
                //     closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

                // let inner_env = Rc::new(RefCell::new(Env::new_subexpression(
                //     parent_env.clone(),
                //     offset,
                // )));

                let inner_env = Rc::new(RefCell::new(
                    Env::new_subexpression_with_capacity_without_offset(parent_env.clone()),
                ));

                // inner_env.borrow_mut().increment_weak_count();

                // Adds a pointer from parent -> child
                // weak reference taken by downgrading the strong reference on inner env

                // parent_env
                //     .upgrade()
                //     .unwrap()
                //     .borrow_mut()
                //     .add_child(Rc::downgrade(&inner_env));

                // TODO future me figure out offsets
                // Leave here until I figure out the offset problem
                // inner_env
                //     .borrow_mut()
                //     .reserve_defs(if closure.ndef_body() > 0 {
                //         closure.ndef_body() - 1
                //     } else {
                //         0
                //     });

                // self.heap
                //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
                // self.heap.collect_garbage();

                // let result =
                // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                // closure_stack.push(Rc::clone(&stack_func));
                // TODO this is where the memory leak is
                self.env_stack.push(Rc::clone(&self.global_env));

                self.global_env = inner_env;
                self.instruction_stack.push(InstructionPointer::new(
                    self.ip + 1,
                    Rc::clone(&self.instructions),
                ));
                self.pop_count += 1;
                // Move args into the stack, push stack onto stacks
                let stack = std::mem::replace(&mut self.stack, args.into());
                self.stacks.push(stack);
                self.instructions = closure.body_exp();
                self.ip = 0;
            }
            _ => {
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; *span);
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_start_def(&mut self) {
        self.ip += 1;

        self.global_env.borrow_mut().set_binding_context(true);
        self.global_env.borrow_mut().set_binding_offset(false);

        let stack = std::mem::replace(&mut self.stack, Stack::new());
        self.stacks.push(stack);

        // placeholder on the instruction_stack
        self.instruction_stack.push(InstructionPointer::new_raw());
        self.pop_count += 1;
    }

    #[inline(always)]
    fn handle_apply(&mut self, span: Span) -> Result<()> {
        let list = self.stack.pop().unwrap();
        let func = self.stack.pop().unwrap();

        let args = match ListOperations::collect_into_vec(&list) {
            Ok(args) => args,
            Err(_) => stop!(TypeMismatch => "apply expected a list"; span),
        };

        match &func {
            SteelVal::FuncV(f) => {
                let result = f(&args).map_err(|x| x.set_span(span))?;
                self.stack.push(result);
                self.ip += 1;
            }
            SteelVal::BoxedFunction(f) => {
                let result = f(&args).map_err(|x| x.set_span(span))?;
                self.stack.push(result);
                self.ip += 1;
            }
            SteelVal::Closure(closure) => {
                if self.stacks.len() == STACK_LIMIT {
                    // println!("stacks at exit: {:?}", stacks);
                    println!("stack frame at exit: {:?}", self.stack);
                    stop!(Generic => "stack overflowed!"; span);
                }

                // let args = stack.split_off(stack.len() - cur_inst.payload_size as usize);

                let parent_env = closure.sub_expression_env();

                // TODO remove this unwrap
                // TODO figure out the offset business
                // let offset =
                //     closure.offset() + parent_env.upgrade().unwrap().borrow().local_offset();

                let inner_env = Rc::new(RefCell::new(
                    Env::new_subexpression_with_capacity_without_offset(
                        parent_env.clone(),
                        // offset,
                    ),
                ));

                // add this closure to the list of children
                // parent_env
                //     .upgrade()
                //     .unwrap()
                //     .borrow_mut()
                //     .add_child(Rc::downgrade(&inner_env));

                // inner_env.borrow_mut().increment_weak_count();

                // TODO future me figure out offsets
                // More offset nonsense
                // inner_env
                //     .borrow_mut()
                //     .reserve_defs(if closure.ndef_body() > 0 {
                //         closure.ndef_body() - 1
                //     } else {
                //         0
                //     });

                // let result =
                // vm(closure.body_exp(), &mut args, heap, inner_env, constants)?;
                // closure_stack.push(Rc::clone(&stack_func));
                // TODO this is where the memory leak is
                self.env_stack.push(Rc::clone(&self.global_env));

                self.global_env = inner_env;
                self.instruction_stack.push(InstructionPointer::new(
                    self.ip + 1,
                    Rc::clone(&self.instructions),
                ));
                self.pop_count += 1;
                let stack = std::mem::replace(&mut self.stack, args.into());
                self.stacks.push(stack);
                self.instructions = closure.body_exp();
                self.ip = 0;
            }
            _ => {
                stop!(BadSyntax => "Apply - Application not a procedure or function type not supported"; span);
            }
        }
        Ok(())
    }
}

#[inline(always)]
pub(crate) fn vm<CT: ConstantTable>(
    instructions: Rc<[DenseInstruction]>,
    stack: StackFrame,
    heap: &mut Heap,
    global_env: Rc<RefCell<Env>>,
    constants: &CT,
    repl: bool,
    callback: &EvaluationProgress,
) -> Result<SteelVal> {
    VmCore::new(
        instructions,
        stack,
        heap,
        global_env,
        constants,
        repl,
        callback,
    )?
    .vm()
}
