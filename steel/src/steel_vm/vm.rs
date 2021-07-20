use super::contracts::ContractedFunctionExt;
use super::options::ApplyContract;
use super::options::ApplyContracts;
use super::options::UseCallback;
use super::options::UseCallbacks;
use super::{
    heap::UpValueHeap,
    stack::{Stack, StackFrame},
};
use crate::jit::code_gen::JIT;
use crate::jit::sig::JitFunctionPointer;
use crate::{
    compiler::{
        constants::{ConstantMap, ConstantTable},
        program::Program,
    },
    core::{instructions::DenseInstruction, opcode::OpCode},
    rvals::{FutureResult, UpValue},
    values::contracts::ContractedFunction,
};

use crate::{
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
    values::structs::SteelStruct,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryFrom,
    iter::Iterator,
    rc::{Rc, Weak},
    result,
};

use super::evaluation_progress::EvaluationProgress;

use log::error;

const STACK_LIMIT: usize = 1000;
const JIT_THRESHOLD: usize = 100;

pub struct VirtualMachineCore {
    global_env: Env,
    global_upvalue_heap: UpValueHeap,
    callback: EvaluationProgress,
    stack: StackFrame,
    function_stack: Vec<Gc<ByteCodeLambda>>,
    stack_index: Stack<usize>,
    #[cfg(feature = "jit")]
    jit: JIT,
}

impl VirtualMachineCore {
    pub fn new() -> VirtualMachineCore {
        VirtualMachineCore {
            global_env: Env::root(),
            global_upvalue_heap: UpValueHeap::new(),
            callback: EvaluationProgress::new(),
            stack: StackFrame::with_capacity(256),
            function_stack: Vec::with_capacity(64),
            stack_index: Stack::with_capacity(64),
            #[cfg(feature = "jit")]
            jit: JIT::default(),
        }
    }

    pub fn insert_binding(&mut self, idx: usize, value: SteelVal) {
        self.global_env.add_root_value(idx, value);
    }

    pub fn extract_value(&self, idx: usize) -> Option<SteelVal> {
        self.global_env.extract(idx)
    }

    pub fn on_progress<FN: Fn(usize) -> bool + 'static>(&mut self, callback: FN) {
        &self.callback.with_callback(Box::new(callback));
    }

    // fn vec_exprs_to_map(&mut self, exprs: Vec<ExprKind>) {}

    pub fn execute_program<U: UseCallbacks, A: ApplyContracts>(
        &mut self,
        program: Program,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<Vec<SteelVal>> {
        let Program {
            instructions,
            constant_map,
            ast,
        } = program;

        // TODO come back to this
        // Don't want to necessarily pre-compile _anything_ yet
        #[cfg(feature = "jit")]
        {
            // for (index, expr) in &ast {
            //     match self.jit.compile(&expr) {
            //         Ok(ptr) => {
            //             println!("Found JIT-able function at index: {}!", index)
            //         }
            //         Err(_) => {
            //             println!("Unable to compile function!");
            //         }
            //     }
            // }
        }

        // Add the new functions to the hashmap for the JIT
        self.global_env.add_hashmap(ast);

        let output = instructions
            .into_iter()
            .map(|x| {
                self.execute(
                    Rc::from(x.into_boxed_slice()),
                    &constant_map,
                    use_callbacks,
                    apply_contracts,
                )
            })
            .collect();

        // TODO
        self.global_env.print_diagnostics();

        output
    }

    pub fn _execute_program_by_ref(&mut self, program: &Program) -> Result<Vec<SteelVal>> {
        let Program {
            instructions,
            constant_map,
            ..
        } = program;

        let instructions: Vec<_> = instructions
            .clone()
            .into_iter()
            .map(|x| Rc::from(x.into_boxed_slice()))
            .collect();

        instructions
            .into_iter()
            .map(|code| self.execute(code, &constant_map, UseCallback, ApplyContract))
            .collect()
    }

    pub fn execute<U: UseCallbacks, A: ApplyContracts>(
        &mut self,
        instructions: Rc<[DenseInstruction]>,
        constant_map: &ConstantMap,
        use_callbacks: U,
        apply_contracts: A,
    ) -> Result<SteelVal> {
        let result = vm(
            instructions,
            &mut self.stack,
            &mut self.global_env,
            constant_map,
            &self.callback,
            &mut self.global_upvalue_heap,
            &mut self.function_stack,
            &mut self.stack_index,
            use_callbacks,
            apply_contracts,
            Some(&mut self.jit),
        );

        // Clean up
        self.stack.clear();
        self.stack_index.clear();
        self.function_stack.clear();

        result
    }
}

#[derive(Debug, Clone)]
pub struct InstructionPointer(usize, Rc<[DenseInstruction]>);

impl InstructionPointer {
    pub fn _new_raw() -> Self {
        InstructionPointer(0, Rc::from(Vec::new().into_boxed_slice()))
    }

    #[inline(always)]
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

#[derive(Clone, Debug)]
pub struct Continuation {
    pub(crate) stack: StackFrame,
    instructions: Rc<[DenseInstruction]>,
    instruction_stack: Stack<InstructionPointer>,
    stack_index: Stack<usize>,
    ip: usize,
    pop_count: usize,
    function_stack: Vec<Gc<ByteCodeLambda>>,
    upvalue_head: Option<Weak<RefCell<UpValue>>>,
}

#[inline(always)]
fn validate_closure_for_call_cc(function: &SteelVal, span: Span) -> Result<()> {
    match function {
        SteelVal::Closure(c) => {
            if c.arity() != 1 {
                stop!(Generic => "function arity in call/cc must be 1"; span)
            }
        }
        SteelVal::ContinuationFunction(_) => {}
        _ => {
            stop!(Generic => "call/cc expects a function"; span)
        }
    }

    Ok(())
}

pub(crate) struct VmCore<'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts> {
    pub(crate) instructions: Rc<[DenseInstruction]>,
    pub(crate) stack: &'a mut StackFrame,
    pub(crate) global_env: &'a mut Env,
    pub(crate) instruction_stack: Stack<InstructionPointer>,
    pub(crate) stack_index: &'a mut Stack<usize>,
    pub(crate) callback: &'a EvaluationProgress,
    pub(crate) constants: &'a CT,
    pub(crate) ip: usize,
    pub(crate) pop_count: usize,
    pub(crate) upvalue_head: Option<Weak<RefCell<UpValue>>>,
    pub(crate) upvalue_heap: &'a mut UpValueHeap,
    pub(crate) function_stack: &'a mut Vec<Gc<ByteCodeLambda>>,
    pub(crate) use_callbacks: U,
    pub(crate) apply_contracts: A,
    pub(crate) jit: Option<&'a mut JIT>,
}

impl<'a, CT: ConstantTable, U: UseCallbacks, A: ApplyContracts> VmCore<'a, CT, U, A> {
    fn new(
        instructions: Rc<[DenseInstruction]>,
        stack: &'a mut StackFrame,
        global_env: &'a mut Env,
        constants: &'a CT,
        callback: &'a EvaluationProgress,
        upvalue_heap: &'a mut UpValueHeap,
        function_stack: &'a mut Vec<Gc<ByteCodeLambda>>,
        stack_index: &'a mut Stack<usize>,
        use_callbacks: U,
        apply_contracts: A,
        jit: Option<&'a mut JIT>,
    ) -> Result<VmCore<'a, CT, U, A>> {
        if instructions.is_empty() {
            stop!(Generic => "empty stack!")
        }

        Ok(VmCore {
            instructions: Rc::clone(&instructions),
            stack,
            global_env,
            instruction_stack: Stack::new(),
            stack_index,
            callback,
            constants,
            ip: 0,
            pop_count: 1,
            upvalue_head: None,
            upvalue_heap,
            function_stack,
            use_callbacks,
            apply_contracts,
            jit,
        })
    }

    fn capture_upvalue(&mut self, local_idx: usize) -> Weak<RefCell<UpValue>> {
        let mut prev_up_value: Option<Weak<RefCell<UpValue>>> = None;
        let mut upvalue = self.upvalue_head.clone();

        while upvalue.is_some()
            && upvalue
                .as_ref()
                .unwrap()
                .upgrade()
                .expect("Upvalue freed too early")
                .borrow()
                .index()
                .map(|x| x > local_idx)
                .unwrap_or(false)
        {
            prev_up_value = upvalue.clone();
            upvalue = upvalue
                .map(|x| {
                    x.upgrade()
                        .expect("Upvalue freed too early")
                        .borrow()
                        .next
                        .clone()
                })
                .flatten();
        }

        if upvalue.is_some()
            && upvalue
                .as_ref()
                .unwrap()
                .upgrade()
                .expect("Upvalue freed too early")
                .borrow()
                .index()
                .map(|x| x == local_idx)
                .unwrap_or(false)
        {
            return upvalue.unwrap();
        }

        let created_up_value: Weak<RefCell<UpValue>> = self.upvalue_heap.new_upvalue(
            local_idx,
            upvalue,
            self.stack
                .0
                .iter()
                .chain(self.global_env.bindings_vec.iter()),
            self.function_stack.iter(),
        );

        if prev_up_value.is_none() {
            self.upvalue_head = Some(created_up_value.clone());
        } else {
            let prev_up_value = prev_up_value.unwrap().upgrade().unwrap();

            prev_up_value
                .borrow_mut()
                .set_next(created_up_value.clone());
        }

        created_up_value
    }

    fn close_upvalues(&mut self, last: usize) {
        while self.upvalue_head.is_some()
            && self
                .upvalue_head
                .as_ref()
                .unwrap()
                .upgrade()
                .unwrap()
                .borrow()
                .index()
                .map(|x| x >= last)
                .unwrap_or(false)
        {
            let upvalue = self.upvalue_head.as_ref().unwrap().upgrade().unwrap();
            let value = upvalue.borrow().get_value(&self.stack);
            upvalue.borrow_mut().set_value(value);
            self.upvalue_head = upvalue.borrow_mut().next.clone();
        }
    }

    #[inline(always)]
    fn new_continuation_from_state(&self) -> Continuation {
        Continuation {
            stack: self.stack.clone(),
            instructions: Rc::clone(&self.instructions),
            instruction_stack: self.instruction_stack.clone(),
            stack_index: self.stack_index.clone(),
            ip: self.ip,
            pop_count: self.pop_count,
            function_stack: self.function_stack.clone(),
            upvalue_head: self.upvalue_head.clone(),
        }
    }

    #[inline(always)]
    fn set_state_from_continuation(&mut self, continuation: Continuation) {
        *self.stack = continuation.stack;
        self.instructions = continuation.instructions;
        self.instruction_stack = continuation.instruction_stack;
        self.ip = continuation.ip;
        self.pop_count = continuation.pop_count;
        *self.stack_index = continuation.stack_index;
        *self.function_stack = continuation.function_stack;
        self.upvalue_head = continuation.upvalue_head;
    }

    #[inline(always)]
    fn construct_continuation_function(&self) -> SteelVal {
        let captured_continuation = self.new_continuation_from_state();
        SteelVal::ContinuationFunction(Gc::new(captured_continuation))
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
                    println!("Hitting a pass - this shouldn't happen");
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
                OpCode::INNERSTRUCT => {
                    self.handle_inner_struct(cur_inst.payload_size as usize)?;
                    self.stack.push(SteelVal::Void);
                    self.ip += 1;
                }
                OpCode::CALLCC => {
                    /*
                    - Construct the continuation
                    - Get the function that has been passed in (off the stack)
                    - Apply the function with the continuation
                    - Handle continuation function call separately in the handle_func_call
                    */
                    let function = self.stack.pop().unwrap();

                    validate_closure_for_call_cc(&function, cur_inst.span)?;

                    let continuation = self.construct_continuation_function();

                    match function {
                        SteelVal::Closure(closure) => {
                            if self.stack_index.len() == STACK_LIMIT {
                                println!("stack frame at exit: {:?}", self.stack);
                                stop!(Generic => "stack overflowed!"; cur_inst.span);
                            }

                            if closure.arity() != 1 {
                                stop!(Generic => "call/cc expects a function with arity 1");
                            }

                            self.stack_index.push(self.stack.len());

                            // Put the continuation as the argument
                            self.stack.push(continuation);

                            // self.global_env = inner_env;
                            self.instruction_stack.push(InstructionPointer::new(
                                self.ip + 1,
                                Rc::clone(&self.instructions),
                            ));
                            self.pop_count += 1;

                            self.instructions = closure.body_exp();
                            self.function_stack.push(closure);

                            self.ip = 0;
                        }
                        SteelVal::ContinuationFunction(cc) => {
                            self.set_state_from_continuation(cc.unwrap());
                            self.ip += 1;
                            self.stack.push(continuation);
                        }

                        _ => {
                            stop!(Generic => "call/cc expects a function");
                        }
                    }
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
                OpCode::READLOCAL => self.handle_local(cur_inst.payload_size as usize)?,
                OpCode::SETLOCAL => self.handle_set_local(cur_inst.payload_size as usize),
                OpCode::READUPVALUE => self.handle_upvalue(cur_inst.payload_size as usize),
                OpCode::SETUPVALUE => self.handle_set_upvalue(cur_inst.payload_size as usize),
                OpCode::APPLY => self.handle_apply(cur_inst.span)?,
                OpCode::CLEAR => {
                    self.ip += 1;
                }
                OpCode::LOADINT1 => {
                    self.stack.push(SteelVal::INT_ONE);
                    self.ip += 1;
                }
                OpCode::LOADINT2 => {
                    self.stack.push(SteelVal::INT_TWO);
                    self.ip += 1;
                }
                OpCode::CGLOCALCONST => {
                    let read_local = self.instructions[self.ip + 1];
                    let push_const = self.instructions[self.ip + 2];

                    // Snag the function
                    let func = self
                        .global_env
                        .repl_lookup_idx(cur_inst.payload_size as usize)?;

                    // get the local
                    let offset = self.stack_index.last().copied().unwrap_or(0);
                    let local_value = self.stack[read_local.payload_size as usize + offset].clone();

                    // get the const
                    let const_val = self.constants.get(push_const.payload_size as usize);

                    self.handle_lazy_function_call(func, local_value, const_val, &cur_inst.span)?;
                }
                OpCode::CALLGLOBAL => {
                    let next_inst = self.instructions[self.ip + 1];
                    self.handle_call_global(
                        cur_inst.payload_size as usize,
                        next_inst.payload_size as usize,
                        &next_inst.span,
                    )?;
                }
                OpCode::CALLGLOBALTAIL => {
                    let next_inst = self.instructions[self.ip + 1];
                    self.handle_tail_call_global(
                        cur_inst.payload_size as usize,
                        next_inst.payload_size as usize,
                        &next_inst.span,
                    )?;
                }
                OpCode::FUNC => {
                    let func = self.stack.pop().unwrap();
                    self.handle_function_call(
                        func,
                        cur_inst.payload_size as usize,
                        &cur_inst.span,
                    )?;
                }
                // Tail call basically says "hey this function is exiting"
                // In the closure case, transfer ownership of the stack to the called function
                OpCode::TAILCALL => {
                    let func = self.stack.pop().unwrap();
                    self.handle_tail_call(func, cur_inst.payload_size as usize, &cur_inst.span)?
                }
                OpCode::IF => {
                    // change to truthy...
                    if self.stack.pop().unwrap().is_truthy() {
                        self.ip = cur_inst.payload_size as usize;
                    } else {
                        self.ip = self.instructions[self.ip + 1].payload_size as usize
                        // self.ip += 1;
                    }
                }
                OpCode::TCOJMP => {
                    let current_arity = self.instructions[self.ip + 1].payload_size as usize;
                    self.ip = cur_inst.payload_size as usize;

                    let closure_arity = self.function_stack.last().unwrap().arity();

                    if current_arity != closure_arity {
                        stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure_arity, current_arity));
                    }

                    // HACK COME BACK TO THIS
                    // if self.ip == 0 && self.heap.len() > self.heap.limit() {
                    // TODO collect here
                    // self.heap.collect_garbage();
                    // }
                    let offset = self.stack_index.last().copied().unwrap_or(0);

                    // We should have arity at this point, drop the stack up to this point
                    // take the last arity off the stack, go back and replace those in order
                    let back = self.stack.len() - current_arity;
                    for i in 0..current_arity {
                        self.stack.set_idx(offset + i, self.stack[back + i].clone());
                    }

                    self.stack.truncate(offset + current_arity);
                }
                OpCode::JMP => {
                    self.ip = cur_inst.payload_size as usize;
                }
                OpCode::POP => {
                    if let Some(r) = self.handle_pop(cur_inst.payload_size, &cur_inst.span) {
                        return r;
                    }
                }
                OpCode::BIND => self.handle_bind(cur_inst.payload_size as usize),
                OpCode::SCLOSURE => self.handle_start_closure(cur_inst.payload_size as usize),
                OpCode::SDEF => self.handle_start_def(),
                OpCode::EDEF => {
                    self.ip += 1;
                }
                _ => {
                    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                    panic!("Unhandled opcode: {:?} @ {}", cur_inst.op_code, self.ip);
                }
            }

            // Put callbacks behind generic
            if self.use_callbacks.use_callbacks() {
                match self.callback.call_and_increment() {
                    Some(b) if !b => stop!(Generic => "Callback forced quit of function!"),
                    _ => {}
                }
            }
        }

        error!(
            "Out of bounds instruction!: instruction pointer: {}, instruction length: {}",
            self.ip,
            self.instructions.len()
        );
        panic!("Out of bounds instruction")
    }

    #[inline(always)]
    fn handle_pop(&mut self, payload: u32, span: &Span) -> Option<Result<SteelVal>> {
        self.pop_count -= 1;

        // unwrap just because we want to see if we have something here
        // rolling back the function stack
        self.function_stack.pop();

        if self.pop_count == 0 {
            let ret_val = self.stack.try_pop().ok_or_else(|| {
                SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string()).with_span(*span)
            });

            // Roll back if needed
            if let Some(rollback_index) = self.stack_index.pop() {
                self.stack.truncate(rollback_index);
            }

            Some(ret_val)
        } else {
            let ret_val = self.stack.pop().unwrap();

            let rollback_index = self.stack_index.pop().unwrap();

            // Snatch the value to close from the payload size
            let value_count_to_close = payload;

            // Move forward past the pop
            self.ip += 1;

            for i in 0..value_count_to_close {
                let instr = self.instructions[self.ip];
                match (instr.op_code, instr.payload_size) {
                    (OpCode::CLOSEUPVALUE, 1) => {
                        self.close_upvalues(rollback_index + i as usize);
                    }
                    (OpCode::CLOSEUPVALUE, 0) => {
                        // do nothing explicitly, just a normal pop
                    }
                    (op, _) => panic!(
                        "Closing upvalues failed with instruction: {:?} @ {}",
                        op, self.ip
                    ),
                }
                self.ip += 1;
            }

            self.stack.truncate(rollback_index);
            self.stack.push(ret_val);

            if !self
                .instruction_stack
                .last()
                .unwrap()
                .instrs_ref()
                .is_empty()
            {
                let prev_state = self.instruction_stack.pop().unwrap();
                self.ip = prev_state.0;
                self.instructions = prev_state.instrs();
            } else {
                self.ip += 1;
            }

            None
        }
    }

    #[inline(always)]
    fn handle_transduce(&mut self, span: &Span) -> Result<()> {
        let list = self.stack.pop().unwrap();
        let initial_value = self.stack.pop().unwrap();
        let reducer = self.stack.pop().unwrap();
        let transducer = self.stack.pop().unwrap();

        if let SteelVal::IterV(transducer) = &transducer {
            let ret_val = self.transduce(&transducer.ops, list, initial_value, reducer, span);
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
            let ret_val = self.run(&transducer.ops, list, Some(output_type), span);
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
            let ret_val = self.run(&transducer.ops, list, None, span);
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
                    Ok(s)
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

            self.global_env.repl_define_idx(idx, func);
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_inner_struct(&mut self, offset: usize) -> Result<()> {
        let val = self.constants.get(offset);
        let mut iter = SteelVal::iter(val);

        // List of indices e.g. '(25 26 27 28) to bind struct functions to
        let _ = iter.next().unwrap();

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
                    Ok(s)
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

        // We've mapped in the compiler _where_ locals are going to be (on the stack), just put them there
        for (_, func) in funcs {
            self.stack.push(func);
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
                    let converted: Result<Vec<SteelVal>> =
                        v.into_iter().map(SteelVal::try_from).collect();

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

        if let SteelVal::Closure(_) = &value_to_assign {
            // println!("Closing upvalue here");
            self.close_upvalues(*self.stack_index.last().unwrap_or(&0));
        }

        let value = self.global_env.repl_set_idx(index, value_to_assign)?;

        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_call_global(&mut self, index: usize, payload_size: usize, span: &Span) -> Result<()> {
        let func = self.global_env.repl_lookup_idx(index)?;
        self.ip += 1;
        // TODO - handle this a bit more elegantly
        // self.handle_function_call(func, payload_size, span)
        self.handle_global_function_call(func, payload_size, span, index)
    }

    #[inline(always)]
    fn handle_tail_call_global(
        &mut self,
        index: usize,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        let func = self.global_env.repl_lookup_idx(index)?;
        self.ip += 1;
        self.handle_tail_call(func, payload_size, span)
    }

    #[inline(always)]
    fn handle_push(&mut self, index: usize) -> Result<()> {
        let value = self.global_env.repl_lookup_idx(index)?;
        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_local(&mut self, index: usize) -> Result<()> {
        let offset = self.stack_index.last().copied().unwrap_or(0);
        let value = self.stack[index + offset].clone();
        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_upvalue(&mut self, index: usize) {
        let value = self
            .function_stack
            .last()
            .map(|x| {
                x.upvalues()[index]
                    .upgrade()
                    .expect("Upvalue dropped too early!")
                    .borrow()
                    .get_value(&self.stack)
            })
            .unwrap();

        self.stack.push(value);
        self.ip += 1;
    }

    #[inline(always)]
    fn handle_start_closure(&mut self, offset: usize) {
        self.ip += 1;

        let forward_jump = offset - 1;

        // Snag the number of upvalues here
        let ndefs = self.instructions[self.ip].payload_size;
        self.ip += 1;

        // TODO preallocate size
        let mut upvalues = Vec::with_capacity(ndefs as usize);

        // TODO clean this up a bit
        // hold the spot for where we need to jump aftwards
        let forward_index = self.ip + forward_jump;

        // Insert metadata
        for _ in 0..ndefs {
            let instr = self.instructions[self.ip];
            match (instr.op_code, instr.payload_size) {
                (OpCode::FILLUPVALUE, n) => {
                    upvalues.push(
                        self.function_stack
                            .last()
                            .map(|x| x.upvalues()[n as usize].clone())
                            .unwrap(),
                    );
                }
                (OpCode::FILLLOCALUPVALUE, n) => {
                    upvalues.push(
                        self.capture_upvalue(self.stack_index.last().unwrap_or(&0) + n as usize),
                    );
                }
                (l, _) => {
                    panic!(
                        "Something went wrong in closure construction!, found: {:?} @ {}",
                        l, self.ip,
                    );
                }
            }
            self.ip += 1;
        }

        // Construct the closure body using the offsets from the payload
        // used to be - 1, now - 2
        let closure_body = self.instructions[self.ip..(self.ip + forward_jump - 1)].to_vec();

        // snag the arity from the eclosure instruction
        let arity = self.instructions[forward_index - 1].payload_size;

        let constructed_lambda = ByteCodeLambda::new(closure_body, arity as usize, upvalues);

        self.stack
            .push(SteelVal::Closure(Gc::new(constructed_lambda)));

        self.ip = forward_index;
    }

    #[inline(always)]
    fn handle_bind(&mut self, payload_size: usize) {
        self.global_env
            .repl_define_idx(payload_size, self.stack.pop().unwrap());

        self.ip += 1;
    }

    #[inline(always)]
    fn handle_set_local(&mut self, index: usize) {
        let value_to_set = self.stack.pop().unwrap();
        let offset = self.stack_index.last().copied().unwrap_or(0);
        let old_value = self.stack[index + offset].clone();

        // Modify the stack and change the value to the new one
        self.stack.set_idx(index + offset, value_to_set);

        self.stack.push(old_value);
        self.ip += 1;
    }

    #[inline(always)]
    fn handle_set_upvalue(&mut self, index: usize) {
        let new = self.stack.pop().unwrap();
        let last_func = self.function_stack.last().unwrap();
        let upvalue = last_func.upvalues()[index].upgrade().unwrap();
        let value = upvalue.borrow_mut().mutate_value(&mut self.stack.0, new);

        self.stack.push(value);
        self.ip += 1;
    }

    #[inline(always)]
    fn handle_tail_call_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        closure.increment_call_count();

        // Snag the current functions arity & remove the last function call
        let current_executing = self.function_stack.pop();

        // TODO
        self.function_stack.push(Gc::clone(&closure));

        if self.stack_index.len() == STACK_LIMIT {
            // println!("stacks at exit: {:?}", self.stacks);
            println!("stack frame at exit: {:?}", self.stack);
            stop!(Generic => "stack overflowed!"; *span);
        }

        if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
        }

        // println!("stack index before: {:?}", self.stack_index);

        // jump back to the beginning at this point
        let offset = *(self.stack_index.last().unwrap_or(&0));

        if !current_executing
            .map(|x| x.upvalues().is_empty())
            .unwrap_or(true)
        {
            self.close_upvalues(offset);
        }

        // Find the new arity from the payload
        let new_arity = payload_size;

        // We should have arity at this point, drop the stack up to this point
        // take the last arity off the stack, go back and replace those in order
        let back = self.stack.len() - new_arity;
        for i in 0..new_arity {
            self.stack.set_idx(offset + i, self.stack[back + i].clone());
        }

        // TODO

        self.stack.truncate(offset + new_arity);

        // // TODO
        // self.heap
        // .gather_mark_and_sweep_2(&self.global_env, &inner_env);

        // self.heap.collect_garbage();

        // Added this one as well
        // self.heap.add(Rc::clone(&self.global_env));

        // self.global_env = inner_env;
        self.instructions = closure.body_exp();

        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn handle_tail_call(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        use SteelVal::*;
        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size, span)?,
            FuncV(f) => self.call_primitive_func(f, payload_size, span)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => {
                self.call_contracted_function_tail_call(cf, payload_size, span)?
            }
            CompiledFunction(function) => {
                self.call_compiled_function(function, payload_size, span)?
            }
            ContinuationFunction(cc) => self.call_continuation(cc)?,
            Closure(closure) => self.handle_tail_call_closure(closure, payload_size, span)?,
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
        // if cf.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", cf.arity(), payload_size); *span);
        // }

        if let Some(arity) = cf.arity() {
            if arity != payload_size {
                stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, payload_size); *span);
            }
        }

        if self.apply_contracts.enforce_contracts() {
            let args = self.stack.split_off(self.stack.len() - payload_size);

            let result = cf.apply(
                args,
                self.constants,
                span,
                self.callback,
                &mut self.upvalue_heap,
                self.global_env,
                &mut self.stack,
                &mut self.function_stack,
                &mut self.stack_index,
                self.use_callbacks,
                self.apply_contracts,
            )?;

            self.stack.push(result);
            self.ip += 1;
            Ok(())
        } else {
            self.handle_function_call(cf.function.clone(), payload_size, span)
        }
    }

    #[inline(always)]
    fn call_contracted_function_tail_call(
        &mut self,
        cf: &ContractedFunction,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        // if cf.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", cf.arity(), payload_size); *span);
        // }

        if let Some(arity) = cf.arity() {
            if arity != payload_size {
                stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, payload_size); *span);
            }
        }

        if self.apply_contracts.enforce_contracts() {
            let args = self.stack.split_off(self.stack.len() - payload_size);

            let result = cf.apply(
                args,
                self.constants,
                span,
                self.callback,
                &mut self.upvalue_heap,
                self.global_env,
                &mut self.stack,
                &mut self.function_stack,
                &mut self.stack_index,
                self.use_callbacks,
                self.apply_contracts,
            )?;

            self.stack.push(result);
            self.ip += 1;
            Ok(())
        } else {
            self.handle_tail_call(cf.function.clone(), payload_size, span)
        }
    }

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
    fn call_continuation(&mut self, continuation: &Continuation) -> Result<()> {
        let last = self
            .stack
            .pop()
            .ok_or_else(throw!(ArityMismatch => "continuation expected 1 argument, found none"))?;

        self.set_state_from_continuation(continuation.clone());

        self.ip += 1;
        self.stack.push(last);
        Ok(())
    }

    #[inline(always)]
    fn handle_lazy_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        local: SteelVal,
        const_value: SteelVal,
        span: &Span,
    ) -> Result<()> {
        // push them onto the stack if we need to
        self.stack.push(local);
        self.stack.push(const_value);

        // Push on the function stack so we have access to it later
        self.function_stack.push(Gc::clone(closure));

        if closure.arity() != 2 {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), 2); *span);
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_index.len() == STACK_LIMIT {
            println!("stack frame at exit: {:?}", self.stack);
            stop!(Generic => "stack overflowed!"; *span);
        }

        self.stack_index.push(self.stack.len() - 2);

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        self.instruction_stack.push(InstructionPointer::new(
            self.ip + 4,
            Rc::clone(&self.instructions),
        ));
        self.pop_count += 1;

        // Move args into the stack, push stack onto stacks
        // let stack = std::mem::replace(&mut self.stack, args.into());
        // self.stacks.push(stack);

        self.instructions = closure.body_exp();
        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn handle_lazy_function_call(
        &mut self,
        stack_func: SteelVal,
        local: SteelVal,
        const_value: SteelVal,
        span: &Span,
    ) -> Result<()> {
        use SteelVal::*;

        match &stack_func {
            BoxedFunction(f) => {
                self.stack
                    .push(f(&[local, const_value]).map_err(|x| x.set_span(*span))?);
                self.ip += 4;
            }
            FuncV(f) => {
                self.stack
                    .push(f(&[local, const_value]).map_err(|x| x.set_span(*span))?);
                self.ip += 4;
            }
            FutureFunc(f) => {
                let result = SteelVal::FutureV(Gc::new(
                    f(&[local, const_value]).map_err(|x| x.set_span(*span))?,
                ));

                self.stack.push(result);
                self.ip += 4;
            }
            ContractedFunction(cf) => {
                if let Some(arity) = cf.arity() {
                    if arity != 2 {
                        stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, 2); *span);
                    }
                }

                if self.apply_contracts.enforce_contracts() {
                    let result = cf.apply(
                        vec![local, const_value],
                        self.constants,
                        span,
                        self.callback,
                        &mut self.upvalue_heap,
                        self.global_env,
                        &mut self.stack,
                        &mut self.function_stack,
                        &mut self.stack_index,
                        self.use_callbacks,
                        self.apply_contracts,
                    )?;

                    self.stack.push(result);
                    self.ip += 4;
                } else {
                    self.handle_lazy_function_call(cf.function.clone(), local, const_value, span)?;
                }
            }
            ContinuationFunction(_cc) => {
                unimplemented!("calling continuation lazily not yet handled");
            }
            Closure(closure) => self.handle_lazy_closure(closure, local, const_value, span)?,
            _ => {
                println!("{:?}", stack_func);
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; *span);
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_function_call_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        // println!("Calling normal function");

        // Jit profiling
        closure.increment_call_count();

        // Push on the function stack so we have access to it later
        self.function_stack.push(Gc::clone(closure));

        if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_index.len() == STACK_LIMIT {
            println!("stack frame at exit: {:?}", self.stack);
            stop!(Generic => "stack overflowed!"; *span);
        }

        self.stack_index.push(self.stack.len() - payload_size);

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        self.instruction_stack.push(InstructionPointer::new(
            self.ip + 1,
            Rc::clone(&self.instructions),
        ));
        self.pop_count += 1;

        // Move args into the stack, push stack onto stacks
        // let stack = std::mem::replace(&mut self.stack, args.into());
        // self.stacks.push(stack);

        self.instructions = closure.body_exp();
        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn call_compiled_function(
        &mut self,
        function: &JitFunctionPointer,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        if function.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", function.arity(), payload_size); *span);
        }

        let result = function.call_func(self.stack);

        // println!("Calling function!");

        self.stack.push(result);
        self.ip += 1;

        Ok(())
    }

    // TODO improve this a bit
    #[inline(always)]
    fn handle_function_call_closure_jit(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
        span: &Span,
        ast_index: usize,
    ) -> Result<()> {
        // Jit profiling
        closure.increment_call_count();

        // TODO
        if closure.call_count() > JIT_THRESHOLD && !closure.has_attempted_to_be_compiled() {
            // unimplemented!();
            if let Some(jit) = &mut self.jit {
                if let Some(function_ast) = self.global_env.get_expr(ast_index) {
                    if let Ok(compiled_func) = jit.compile(function_ast) {
                        self.global_env.repl_define_idx(
                            ast_index,
                            SteelVal::CompiledFunction(compiled_func.clone()),
                        );

                        return self.call_compiled_function(&compiled_func, payload_size, span);
                    } else {
                        // Mark this function as being unable to be compiled
                        closure.set_cannot_be_compiled();
                    }
                }
            }
        }

        // Push on the function stack so we have access to it later
        self.function_stack.push(Gc::clone(closure));

        if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_index.len() == STACK_LIMIT {
            println!("stack frame at exit: {:?}", self.stack);
            stop!(Generic => "stack overflowed!"; *span);
        }

        self.stack_index.push(self.stack.len() - payload_size);

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        self.instruction_stack.push(InstructionPointer::new(
            self.ip + 1,
            Rc::clone(&self.instructions),
        ));
        self.pop_count += 1;

        // Move args into the stack, push stack onto stacks
        // let stack = std::mem::replace(&mut self.stack, args.into());
        // self.stacks.push(stack);

        self.instructions = closure.body_exp();
        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn handle_global_function_call(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
        span: &Span,
        ast_index: usize,
    ) -> Result<()> {
        use SteelVal::*;

        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size, span)?,
            FuncV(f) => self.call_primitive_func(f, payload_size, span)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => self.call_contracted_function(cf, payload_size, span)?,
            ContinuationFunction(cc) => self.call_continuation(cc)?,
            Closure(closure) => {
                self.handle_function_call_closure_jit(closure, payload_size, span, ast_index)?
            }
            CompiledFunction(function) => {
                self.call_compiled_function(function, payload_size, span)?
            }
            _ => {
                println!("{:?}", stack_func);
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; *span);
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_function_call(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
        span: &Span,
    ) -> Result<()> {
        use SteelVal::*;

        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size, span)?,
            FuncV(f) => self.call_primitive_func(f, payload_size, span)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => self.call_contracted_function(cf, payload_size, span)?,
            ContinuationFunction(cc) => self.call_continuation(cc)?,
            Closure(closure) => self.handle_function_call_closure(closure, payload_size, span)?,
            CompiledFunction(function) => {
                self.call_compiled_function(function, payload_size, span)?
            }
            _ => {
                println!("{:?}", stack_func);
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; *span);
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_start_def(&mut self) {
        self.ip += 1;
    }

    #[inline(always)]
    fn handle_apply(&mut self, span: Span) -> Result<()> {
        let list = self.stack.pop().unwrap();
        let func = self.stack.pop().unwrap();

        let mut args = match ListOperations::collect_into_vec(&list) {
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
                if self.stack_index.len() == STACK_LIMIT {
                    // println!("stacks at exit: {:?}", stacks);
                    println!("stack frame at exit: {:?}", self.stack);
                    stop!(Generic => "stack overflowed!"; span);
                }

                // self.global_env = inner_env;
                self.instruction_stack.push(InstructionPointer::new(
                    self.ip + 1,
                    Rc::clone(&self.instructions),
                ));
                self.pop_count += 1;

                self.function_stack.push(Gc::clone(closure));

                let payload_size = args.len();

                // Append the arguments to the function
                self.stack.append_vec(&mut args);

                self.stack_index.push(self.stack.len() - payload_size);

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
pub(crate) fn vm<CT: ConstantTable, U: UseCallbacks, A: ApplyContracts>(
    instructions: Rc<[DenseInstruction]>,
    stack: &mut StackFrame,
    global_env: &mut Env,
    constants: &CT,
    callback: &EvaluationProgress,
    upvalue_heap: &mut UpValueHeap,
    function_stack: &mut Vec<Gc<ByteCodeLambda>>,
    stack_index: &mut Stack<usize>,
    use_callbacks: U,
    apply_contracts: A,
    jit: Option<&mut JIT>,
) -> Result<SteelVal> {
    VmCore::new(
        instructions,
        stack,
        global_env,
        constants,
        callback,
        upvalue_heap,
        function_stack,
        stack_index,
        use_callbacks,
        apply_contracts,
        jit,
    )?
    .vm()
}
