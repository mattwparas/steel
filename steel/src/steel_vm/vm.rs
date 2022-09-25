use super::heap::UpValueHeap;

#[cfg(feature = "jit")]
use crate::jit::code_gen::JIT;
#[cfg(feature = "jit")]
use crate::jit::sig::JitFunctionPointer;
use crate::values::upvalue::UpValue;
use crate::values::{closed::Heap, contracts::ContractType};
use crate::{
    compiler::program::Executable,
    primitives::{add_primitive, divide_primitive, multiply_primitive, subtract_primitive},
    steel_vm::primitives::{equality_primitive, lte_primitive},
    values::transducers::Transducers,
};
use crate::{compiler::program::OpCodeOccurenceProfiler, values::transducers::Reducer};
use crate::{
    compiler::{constants::ConstantMap, program::Program},
    core::{instructions::DenseInstruction, opcode::OpCode},
    rvals::FutureResult,
    values::contracts::ContractedFunction,
};

use crate::{
    env::Env,
    gc::Gc,
    parser::span::Span,
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    stop,
    values::functions::ByteCodeLambda,
    values::structs::SteelStruct,
};
// use std::env::current_exe;
use std::{
    cell::RefCell,
    iter::Iterator,
    rc::{Rc, Weak},
};

use super::evaluation_progress::EvaluationProgress;

use fnv::FnvHashMap;
use im_lists::list::List;
use log::error;

use crate::rvals::IntoSteelVal;

const STACK_LIMIT: usize = 1000;
const _JIT_THRESHOLD: usize = 100;

#[derive(Debug, Clone)]
pub struct CallContext {
    span: Option<Span>,
    source: Option<usize>, // TODO intern file names
    function: Gc<ByteCodeLambda>,
}

impl CallContext {
    pub fn new(function: Gc<ByteCodeLambda>) -> Self {
        Self {
            span: None,
            source: None,
            function,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_source(mut self, source: usize) -> Self {
        self.source = Some(source);
        self
    }
}

#[derive(Default, Clone, Debug)]
pub struct CallStack {
    function_stack: Vec<CallContext>,
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            function_stack: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            function_stack: Vec::with_capacity(capacity),
        }
    }

    pub fn push(&mut self, call: CallContext) {
        self.function_stack.push(call)
    }

    pub fn pop(&mut self) -> Option<CallContext> {
        self.function_stack.pop()
    }

    pub fn function_iter(&self) -> impl Iterator<Item = &Gc<ByteCodeLambda>> {
        self.function_stack.iter().map(|x| &x.function)
    }

    pub fn last(&self) -> Option<&Gc<ByteCodeLambda>> {
        self.function_stack.last().map(|x| &x.function)
    }

    pub fn last_mut(&mut self) -> Option<&mut Gc<ByteCodeLambda>> {
        self.function_stack.last_mut().map(|x| &mut x.function)
    }

    pub fn clear(&mut self) {
        self.function_stack.clear()
    }

    pub fn is_empty(&self) -> bool {
        self.function_stack.is_empty()
    }
}

pub struct SteelThread {
    global_env: Env,
    global_upvalue_heap: UpValueHeap,
    callback: EvaluationProgress,
    stack: Vec<SteelVal>,
    function_stack: CallStack,
    stack_index: Vec<usize>,
    upvalue_head: Option<Weak<RefCell<UpValue>>>,
    profiler: OpCodeOccurenceProfiler,
    // TODO: make this not as bad
    closure_interner: FnvHashMap<usize, ByteCodeLambda>,
    pure_function_interner: FnvHashMap<usize, Gc<ByteCodeLambda>>,
    heap: Heap,
    // constants: ConstantMap,
    // If contracts are set to off - contract construction results in a no-op, so we don't
    // need generics on the thread
    contracts_on: bool,
    #[cfg(feature = "jit")]
    jit: JIT,
}

impl SteelThread {
    pub fn new() -> SteelThread {
        SteelThread {
            global_env: Env::root(),
            global_upvalue_heap: UpValueHeap::new(),
            callback: EvaluationProgress::new(),
            stack: Vec::with_capacity(256),
            function_stack: CallStack::with_capacity(64),
            stack_index: Vec::with_capacity(64),
            upvalue_head: None,
            profiler: OpCodeOccurenceProfiler::new(),
            closure_interner: FnvHashMap::default(),
            pure_function_interner: FnvHashMap::default(),
            heap: Heap::new(),
            // constants: ConstantMap::new(),
            contracts_on: true,
            #[cfg(feature = "jit")]
            jit: JIT::default(),
        }
    }

    // If you want to explicitly turn off contracts, you can do so
    pub fn with_contracts(&mut self, contracts: bool) -> &mut Self {
        self.contracts_on = contracts;
        self
    }

    pub fn insert_binding(&mut self, idx: usize, value: SteelVal) {
        self.global_env.add_root_value(idx, value);
    }

    pub fn extract_value(&self, idx: usize) -> Option<SteelVal> {
        self.global_env.extract(idx)
    }

    pub fn on_progress<FN: Fn(usize) -> bool + 'static>(&mut self, callback: FN) {
        self.callback.with_callback(Box::new(callback));
    }

    // Run the executable
    pub fn run_executable(&mut self, program: &Executable) -> Result<Vec<SteelVal>> {
        let Executable {
            instructions,
            constant_map,
            spans,
            // struct_functions,
            ..
        } = program;

        instructions
            .into_iter()
            .map(|x| self.execute(Rc::clone(&x), &constant_map, &spans))
            .collect()

        // TODO
        // self.global_env.print_diagnostics();

        // todo!("Initialize structs and build the program");
    }

    // fn vec_exprs_to_map(&mut self, exprs: Vec<ExprKind>) {}

    pub fn execute_program(&mut self, program: Program) -> Result<Vec<SteelVal>> {
        let Program {
            instructions,
            constant_map,
            ast,
        } = program;

        // TODO come back to this
        // Don't want to necessarily pre-compile _anything_ yet
        // #[cfg(feature = "jit")]
        // {
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
        // }

        // Add the new functions to the hashmap for the JIT
        self.global_env.add_hashmap(ast);

        instructions
            .into_iter()
            .map(|x| self.execute(Rc::from(x.into_boxed_slice()), &constant_map, &[]))
            .collect()

        // TODO
        // self.global_env.print_diagnostics();
    }

    pub(crate) fn call_function(
        &mut self,
        constant_map: &ConstantMap,
        function: SteelVal,
        args: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        let mut vm_instance = VmCore::new_unchecked(
            Rc::new([]),
            &mut self.stack,
            &mut self.global_env,
            constant_map,
            &self.callback,
            &mut self.global_upvalue_heap,
            &mut self.function_stack,
            &mut self.stack_index,
            self.upvalue_head.take(),
            &[],
            &mut self.profiler,
            &mut self.closure_interner,
            &mut self.pure_function_interner,
            &mut self.heap,
            self.contracts_on,
            #[cfg(feature = "jit")]
            Some(&mut self.jit),
        );

        vm_instance.call_func_or_else_many_args(
            &function,
            args,
            &Span::default(),
            throw!(TypeMismatch => format!("application not a procedure: {}", function)),
        )
    }

    pub fn execute(
        &mut self,
        instructions: Rc<[DenseInstruction]>,
        constant_map: &ConstantMap,
        spans: &[Span],
    ) -> Result<SteelVal> {
        self.profiler.reset();

        let mut vm_instance = VmCore::new(
            instructions,
            &mut self.stack,
            &mut self.global_env,
            constant_map,
            &self.callback,
            &mut self.global_upvalue_heap,
            &mut self.function_stack,
            &mut self.stack_index,
            self.upvalue_head.take(),
            spans,
            &mut self.profiler,
            &mut self.closure_interner,
            &mut self.pure_function_interner,
            &mut self.heap,
            self.contracts_on,
            #[cfg(feature = "jit")]
            Some(&mut self.jit),
        )?;

        // TODO: @Matt -> move the profiler out into the vm core type parameter and an argument
        // that way theres 0 cost to including a profiler vs not including a profiler

        let result = {
            let result = vm_instance.vm();
            self.upvalue_head = vm_instance.upvalue_head;
            result
        };

        // self.profiler.report();
        // self.profiler.report_time_spend();
        // self.profiler.report_basic_blocks();

        // self.upvalue_head = vm_instance.upvalue_head;

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

    // #[inline(always)]
    pub fn new(ip: usize, instrs: Rc<[DenseInstruction]>) -> Self {
        InstructionPointer(ip, instrs)
    }

    // #[inline(always)]
    pub fn instrs(self) -> Rc<[DenseInstruction]> {
        self.1
    }
}

#[derive(Clone, Debug)]
pub struct Continuation {
    pub(crate) stack: Vec<SteelVal>,
    instructions: Rc<[DenseInstruction]>,
    instruction_stack: Vec<InstructionPointer>,
    stack_index: Vec<usize>,
    ip: usize,
    pop_count: usize,
    pub(crate) function_stack: CallStack,
    upvalue_head: Option<Weak<RefCell<UpValue>>>,
}

pub trait VmContext {
    // This allows for some funky self calling business
    fn call_function_one_arg(&mut self, function: &SteelVal, arg: SteelVal) -> Result<SteelVal>;

    // Call with two args
    fn call_function_two_arg(
        &mut self,
        function: &SteelVal,
        arg1: SteelVal,
        arg2: SteelVal,
    ) -> Result<SteelVal>;

    fn call_function_many_args(
        &mut self,
        function: &SteelVal,
        args: List<SteelVal>,
    ) -> Result<SteelVal>;

    fn call_transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: Reducer,
        span: Option<Span>,
    ) -> Result<SteelVal>;
}

// For when we want a reference to the built in context as well -> In the event we want to call something
// See if this is even possible -> if you ever want to offload function calls to the
// pub type BuiltInSignature2 = fn(Vec<SteelVal>, &mut dyn VmContext) -> Result<SteelVal>;

// These reference the current existing thread
// TODO: Change this to refer directly to SteelThread in some way
pub type BuiltInSignature = for<'a, 'b> fn(&'a mut VmCore<'b>, &[SteelVal]) -> Result<SteelVal>;

impl<'a> VmContext for VmCore<'a> {
    fn call_function_one_arg(&mut self, function: &SteelVal, arg: SteelVal) -> Result<SteelVal> {
        let span = Span::default();
        self.call_func_or_else(
            function,
            arg,
            &span,
            throw!(TypeMismatch => format!("application not a procedure: {}", function)),
        )
    }

    fn call_function_two_arg(
        &mut self,
        function: &SteelVal,
        arg1: SteelVal,
        arg2: SteelVal,
    ) -> Result<SteelVal> {
        let span = Span::default();
        self.call_func_or_else_two_args(
            function,
            arg1,
            arg2,
            &span,
            throw!(TypeMismatch => format!("application not a procedure: {}", function)),
        )
    }

    fn call_function_many_args(
        &mut self,
        function: &SteelVal,
        args: List<SteelVal>,
    ) -> Result<SteelVal> {
        let span = Span::default();
        self.call_func_or_else_many_args(
            function,
            args,
            &span,
            throw!(TypeMismatch => format!("application not a procedure: {}", function)),
        )
    }

    fn call_transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: Reducer,
        span: Option<Span>,
    ) -> Result<SteelVal> {
        let span = span.unwrap_or_default();
        self.transduce(ops, root, reducer, &span)
    }
}

// This is the state that the VM should spin up and tear down at each invocation
// of the vm loop
struct InstructionState {
    instruction_stack: Vec<InstructionPointer>,
    instructions: Rc<[DenseInstruction]>,
    ip: usize,
    // TODO: this shouldn't be necessary - it should just be the count of the stack
    // eventually get rid of this I think
    pop_count: usize,
}

impl InstructionState {
    pub fn new(instructions: Rc<[DenseInstruction]>) -> Self {
        Self {
            instruction_stack: Vec::new(),
            instructions,
            ip: 0,
            pop_count: 1,
        }
    }
}

impl Default for InstructionState {
    fn default() -> Self {
        Self::new(Rc::new([]))
    }
}

pub struct VmCore<'a> {
    pub(crate) instructions: Rc<[DenseInstruction]>,
    pub(crate) stack: &'a mut Vec<SteelVal>,
    pub(crate) global_env: &'a mut Env,
    pub(crate) instruction_stack: Vec<InstructionPointer>,
    pub(crate) stack_index: &'a mut Vec<usize>,
    pub(crate) callback: &'a EvaluationProgress,
    pub(crate) constants: &'a ConstantMap,
    pub(crate) ip: usize,
    pub(crate) pop_count: usize,
    pub(crate) upvalue_head: Option<Weak<RefCell<UpValue>>>,
    pub(crate) upvalue_heap: &'a mut UpValueHeap,
    pub(crate) function_stack: &'a mut CallStack,
    pub(crate) spans: &'a [Span],
    pub(crate) profiler: &'a mut OpCodeOccurenceProfiler,
    pub(crate) closure_interner: &'a mut FnvHashMap<usize, ByteCodeLambda>,
    pub(crate) pure_function_interner: &'a mut FnvHashMap<usize, Gc<ByteCodeLambda>>,
    pub(crate) heap: &'a mut Heap,
    pub(crate) use_contracts: bool,
    #[cfg(feature = "jit")]
    pub(crate) jit: Option<&'a mut JIT>,
}

impl<'a> VmCore<'a> {
    fn new_unchecked(
        instructions: Rc<[DenseInstruction]>,
        stack: &'a mut Vec<SteelVal>,
        global_env: &'a mut Env,
        constants: &'a ConstantMap,
        callback: &'a EvaluationProgress,
        upvalue_heap: &'a mut UpValueHeap,
        function_stack: &'a mut CallStack,
        stack_index: &'a mut Vec<usize>,
        upvalue_head: Option<Weak<RefCell<UpValue>>>,
        spans: &'a [Span],
        profiler: &'a mut OpCodeOccurenceProfiler,
        closure_interner: &'a mut FnvHashMap<usize, ByteCodeLambda>,
        pure_function_interner: &'a mut FnvHashMap<usize, Gc<ByteCodeLambda>>,
        heap: &'a mut Heap,
        use_contracts: bool,
        #[cfg(feature = "jit")] jit: Option<&'a mut JIT>,
    ) -> VmCore<'a> {
        VmCore {
            instructions: Rc::clone(&instructions),
            stack,
            global_env,
            instruction_stack: Vec::new(),
            stack_index,
            callback,
            constants,
            ip: 0,
            pop_count: 1,
            upvalue_head,
            upvalue_heap,
            function_stack,
            spans,
            profiler,
            closure_interner,
            pure_function_interner,
            heap,
            use_contracts,
            #[cfg(feature = "jit")]
            jit,
        }
    }

    fn new(
        instructions: Rc<[DenseInstruction]>,
        stack: &'a mut Vec<SteelVal>,
        global_env: &'a mut Env,
        constants: &'a ConstantMap,
        callback: &'a EvaluationProgress,
        upvalue_heap: &'a mut UpValueHeap,
        function_stack: &'a mut CallStack,
        stack_index: &'a mut Vec<usize>,
        upvalue_head: Option<Weak<RefCell<UpValue>>>,
        spans: &'a [Span],
        profiler: &'a mut OpCodeOccurenceProfiler,
        closure_interner: &'a mut FnvHashMap<usize, ByteCodeLambda>,
        pure_function_interner: &'a mut FnvHashMap<usize, Gc<ByteCodeLambda>>,
        heap: &'a mut Heap,
        use_contracts: bool,
        #[cfg(feature = "jit")] jit: Option<&'a mut JIT>,
    ) -> Result<VmCore<'a>> {
        if instructions.is_empty() {
            stop!(Generic => "empty stack!")
        }

        Ok(VmCore {
            instructions: Rc::clone(&instructions),
            stack,
            global_env,
            instruction_stack: Vec::new(),
            stack_index,
            callback,
            constants,
            ip: 0,
            pop_count: 1,
            upvalue_head,
            upvalue_heap,
            function_stack,
            spans,
            profiler,
            closure_interner,
            pure_function_interner,
            heap,
            use_contracts,
            #[cfg(feature = "jit")]
            jit,
        })
    }

    // TODO this needs to be a sorted linked list
    // right now, its not a sorted linked list at all
    fn capture_upvalue(&mut self, local_idx: usize, offset: usize) -> Weak<RefCell<UpValue>> {
        let mut prev_up_value: Option<Weak<RefCell<UpValue>>> = None;
        let mut upvalue = self.upvalue_head.clone();

        // We've already adjusted for the stack index offset,
        // we want to make sure we're not duplicating work
        // let unoffset_index = local_idx - self.stack_index.last().copied().unwrap_or(0);

        let local_idx = local_idx + offset;

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
            upvalue = upvalue.and_then(|x| {
                x.upgrade()
                    .expect("Upvalue freed too early")
                    .borrow()
                    .next
                    .clone()
            });
        }

        // println!("Unoffset index: {:?}", unoffset_index);

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
            self.stack.iter().chain(self.global_env.bindings_vec.iter()),
            self.function_stack.function_iter(),
        );

        // println!(
        //     "Creating upvalue: {:?} at index: {:?}",
        //     created_up_value, local_idx
        // );
        // println!("Stack: {:?}", self.stack);

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
            // println!("Closing upvalue");
            let upvalue = self.upvalue_head.as_ref().unwrap().upgrade().unwrap();
            // println!("Getting the value");

            // println!("Closing upvalues with stack: {:?}", self.stack);
            // println!("Upvalue: {:?}", upvalue);
            let value = upvalue.borrow().get_value(&self.stack);

            upvalue.borrow_mut().set_value(value);

            // Do this scoping nonsense to avoid the borrow problem
            // let value = { upvalue.borrow().try_get_value(&self.stack) };

            // // TODO come back to this
            // if let Some(value) = value {
            //     // let value = upvalue.borrow().get_value(&self.stack);
            //     upvalue.borrow_mut().set_value(value);
            // }

            self.upvalue_head = upvalue.borrow_mut().next.clone();
        }
    }

    // #[inline(always)]
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

    // #[inline(always)]
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

    // #[inline(always)]
    fn construct_continuation_function(&self) -> SteelVal {
        let captured_continuation = self.new_continuation_from_state();
        SteelVal::ContinuationFunction(Gc::new(captured_continuation))
    }

    // Reset state FULLY
    fn call_with_instructions_and_reset_state(
        &mut self,
        closure: Rc<[DenseInstruction]>,
    ) -> Result<SteelVal> {
        let old_ip = self.ip;
        let old_instructions = std::mem::replace(&mut self.instructions, closure);
        let old_pop_count = self.pop_count;

        // let old_stack_index = self.stack_index;

        self.ip = 0;
        self.pop_count = 1;

        let res = self.vm();

        self.ip = old_ip;
        self.instructions = old_instructions;
        self.pop_count = old_pop_count;

        res
    }

    // #[inline(always)]
    pub(crate) fn call_func_or_else<F: FnOnce() -> SteelErr>(
        &mut self,
        func: &SteelVal,
        arg: SteelVal,
        cur_inst_span: &Span,
        err: F,
    ) -> Result<SteelVal> {
        match func {
            SteelVal::FuncV(func) => {
                let arg_vec = [arg];
                func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::BoxedFunction(func) => {
                let arg_vec = [arg];
                func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::ContractedFunction(cf) => {
                let arg_vec = vec![arg];
                cf.apply(arg_vec, cur_inst_span, self)
            }
            SteelVal::MutFunc(func) => {
                let mut arg_vec: Vec<_> = vec![arg];
                func(&mut arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::BuiltIn(func) => {
                let arg_vec = [arg];
                func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::Closure(closure) => self.call_with_one_arg(closure, arg),
            _ => Err(err()),
        }
    }

    // #[inline(always)]
    pub(crate) fn call_func_or_else_two_args<F: FnOnce() -> SteelErr>(
        &mut self,
        func: &SteelVal,
        arg1: SteelVal,
        arg2: SteelVal,
        cur_inst_span: &Span,
        err: F,
    ) -> Result<SteelVal> {
        match func {
            SteelVal::FuncV(func) => {
                let arg_vec = [arg1, arg2];
                func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::BoxedFunction(func) => {
                let arg_vec = [arg1, arg2];
                func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::ContractedFunction(cf) => {
                let arg_vec = vec![arg1, arg2];
                cf.apply(arg_vec, cur_inst_span, self)
            }
            SteelVal::MutFunc(func) => {
                let mut arg_vec: Vec<_> = vec![arg1, arg2];
                func(&mut arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::BuiltIn(func) => {
                let arg_vec = [arg1, arg2];
                func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::Closure(closure) => self.call_with_two_args(closure, arg1, arg2),
            _ => Err(err()),
        }
    }

    // #[inline(always)]
    pub(crate) fn call_func_or_else_many_args<F: FnOnce() -> SteelErr>(
        &mut self,
        func: &SteelVal,
        args: impl IntoIterator<Item = SteelVal>,
        cur_inst_span: &Span,
        err: F,
    ) -> Result<SteelVal> {
        match func {
            SteelVal::FuncV(func) => {
                let arg_vec: Vec<_> = args.into_iter().collect();
                func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::BoxedFunction(func) => {
                let arg_vec: Vec<_> = args.into_iter().collect();
                func(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::ContractedFunction(cf) => {
                let arg_vec: Vec<_> = args.into_iter().collect();
                cf.apply(arg_vec, cur_inst_span, self)
            }
            SteelVal::MutFunc(func) => {
                let mut arg_vec: Vec<_> = args.into_iter().collect();
                func(&mut arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::BuiltIn(func) => {
                let arg_vec: Vec<_> = args.into_iter().collect();
                func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            SteelVal::Closure(closure) => self.call_with_args(closure, args),
            _ => Err(err()),
        }
    }

    // Call with an arbitrary number of arguments
    pub(crate) fn call_with_args(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        args: impl IntoIterator<Item = SteelVal>,
    ) -> Result<SteelVal> {
        // println!("Call with args");
        // println!("Arity: {:?}", closure.arity());
        // println!("Multi arity: {:?}", closure.is_multi_arity);

        let prev_length = self.stack.len();
        self.stack_index.push(prev_length);

        let mut argument_count = 0;
        for arg in args {
            self.stack.push(arg);
            argument_count += 1;
        }

        // TODO: abstract the multi arity code into its own function
        // This is duplicated across like 3 different places
        if closure.is_multi_arity {
            if argument_count < closure.arity() - 1 {
                stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", closure.arity(), argument_count); self.current_span());
            }

            // (define (test x . y))
            // (test 1 2 3 4 5)
            // in this case, arity = 2 and payload size = 5
            // pop off the last 4, collect into a list
            let amount_to_remove = 1 + argument_count - closure.arity();

            let values = self.stack.split_off(self.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(List::from(values));

            self.stack.push(list);
        } else if closure.arity() != argument_count {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), argument_count); self.current_span());
        }

        self.function_stack
            .push(CallContext::new(Gc::clone(closure)));
        let result = self.call_with_instructions_and_reset_state(closure.body_exp());

        result
    }

    // Calling convention
    pub(crate) fn call_with_two_args(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        arg1: SteelVal,
        arg2: SteelVal,
    ) -> Result<SteelVal> {
        let prev_length = self.stack.len();
        self.stack_index.push(prev_length);
        self.stack.push(arg1);
        self.stack.push(arg2);
        self.function_stack
            .push(CallContext::new(Gc::clone(closure)));

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    // Calling convention
    pub(crate) fn call_with_one_arg(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        arg: SteelVal,
    ) -> Result<SteelVal> {
        let prev_length = self.stack.len();

        // println!("PUSHING NEW STACK INDEX ON");

        self.stack_index.push(prev_length);
        self.stack.push(arg);
        self.function_stack
            .push(CallContext::new(Gc::clone(closure)));

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    pub(crate) fn vm(&mut self) -> Result<SteelVal> {
        // let mut cur_inst;

        macro_rules! inline_primitive {
            ($name:tt, $payload_size:expr) => {{
                let last_index = self.stack.len() - $payload_size as usize;

                let result = match $name(&mut self.stack[last_index..]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                // This is the old way... lets see if the below way improves the speed
                self.stack.truncate(last_index);
                self.stack.push(result);

                self.ip += 2;
            }};
        }

        macro_rules! inline_register_primitive {
            ($name:tt) => {{
                let read_local = &self.instructions[self.ip];
                let push_const = &self.instructions[self.ip + 1];

                // get the local
                let offset = self.stack_index.last().copied().unwrap_or(0);
                let local_value = self.stack[read_local.payload_size as usize + offset].clone();

                // get the const
                let const_val = self.constants.get(push_const.payload_size as usize);

                let result = match $name(&[local_value, const_val]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                self.stack.push(result);

                self.ip += 2;
            }};
        }

        while self.ip < self.instructions.len() {
            // Process the op code
            // self.profiler.process_opcode(
            //     &self.instructions[self.ip].op_code,
            //     self.instructions[self.ip].payload_size as usize,
            // );

            // println!("{:?}", self.instructions[self.ip]);

            // let now = std::time::Instant::now();

            // TODO -> don't just copy the value from the instructions
            // We don't need to do that... Figure out a way to just take a reference to the value
            // Perhaps if the instructions are guaranteed to be immutable references that cannot be mutated
            // The cost of moving the instruction out of the vector is not what we want. Perhaps move instructions
            // into its own local array since we don't recur in this function?

            // cur_inst = self.instructions[self.ip];

            // Try to eliminate the current instruction variable
            // We can elide the reads, and instead opt to just go for values directly on the instructions
            // Otherwise, we're going to be copying the instruction _every_ time we iterate which is going to slow down the loop
            // We'd rather just reference the instruction and call it a day
            match self.instructions[self.ip] {
                DenseInstruction {
                    op_code: OpCode::PANIC,
                    ..
                } => self.handle_panic(self.current_span())?,
                DenseInstruction {
                    op_code: OpCode::PASS,
                    ..
                } => {
                    println!("Hitting a pass - this shouldn't happen");
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::SUBREGISTER1,
                    ..
                } => {
                    let read_local = &self.instructions[self.ip + 1];

                    // get the local
                    let offset = self.stack_index.last().copied().unwrap_or(0);
                    let local_value = self.stack[read_local.payload_size as usize + offset].clone();

                    let result = match subtract_primitive(&[local_value, SteelVal::IntV(1)]) {
                        Ok(value) => value,
                        Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    };

                    self.stack.push(result);

                    self.ip += 2;
                }
                DenseInstruction {
                    op_code: OpCode::ADDREGISTER,
                    ..
                } => {
                    inline_register_primitive!(add_primitive)
                }
                DenseInstruction {
                    op_code: OpCode::SUBREGISTER,
                    ..
                } => {
                    inline_register_primitive!(subtract_primitive)
                }
                DenseInstruction {
                    op_code: OpCode::LTEREGISTER,
                    ..
                } => {
                    inline_register_primitive!(lte_primitive)
                }
                DenseInstruction {
                    op_code: OpCode::ADD,
                    payload_size,
                    ..
                } => {
                    inline_primitive!(add_primitive, payload_size)
                }
                DenseInstruction {
                    op_code: OpCode::SUB,
                    payload_size,
                    ..
                } => {
                    inline_primitive!(subtract_primitive, payload_size)
                }
                DenseInstruction {
                    op_code: OpCode::MUL,
                    payload_size,
                    ..
                } => {
                    inline_primitive!(multiply_primitive, payload_size)
                }
                DenseInstruction {
                    op_code: OpCode::DIV,
                    payload_size,
                    ..
                } => inline_primitive!(divide_primitive, payload_size),

                DenseInstruction {
                    op_code: OpCode::EQUAL,
                    payload_size,
                    ..
                } => {
                    inline_primitive!(equality_primitive, payload_size);
                }

                DenseInstruction {
                    op_code: OpCode::LTE,
                    payload_size,
                    ..
                } => {
                    inline_primitive!(lte_primitive, payload_size);
                }

                DenseInstruction {
                    op_code: OpCode::VOID,
                    ..
                } => {
                    self.stack.push(SteelVal::Void);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::STRUCT,
                    payload_size,
                    ..
                } => {
                    // For now, only allow structs at the top level
                    // In the future, allow structs to be also available in a nested scope
                    self.handle_struct(payload_size as usize)?;
                    self.stack.push(SteelVal::Void);
                    self.ip += 1;
                    // return Ok(SteelVal::Void);
                }
                DenseInstruction {
                    op_code: OpCode::INNERSTRUCT,
                    payload_size,
                    ..
                } => {
                    self.handle_inner_struct(payload_size as usize)?;
                    self.stack.push(SteelVal::Void);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::SET,
                    payload_size,
                    ..
                } => self.handle_set(payload_size as usize)?,
                DenseInstruction {
                    op_code: OpCode::PUSHCONST,
                    payload_size,
                    ..
                } => {
                    let val = self.constants.get(payload_size as usize);
                    self.stack.push(val);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::PUSH,
                    payload_size,
                    ..
                } => self.handle_push(payload_size as usize)?,
                DenseInstruction {
                    op_code: OpCode::READLOCAL,
                    payload_size,
                    ..
                } => self.handle_local(payload_size as usize)?,
                DenseInstruction {
                    op_code: OpCode::READCAPTURED,
                    payload_size,
                    ..
                } => self.handle_read_captures(payload_size as usize)?,
                DenseInstruction {
                    op_code: OpCode::MOVEREADLOCAL,
                    payload_size,
                    ..
                } => self.handle_move_local(payload_size as usize)?,
                DenseInstruction {
                    op_code: OpCode::SETLOCAL,
                    payload_size,
                    ..
                } => self.handle_set_local(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::READUPVALUE,
                    payload_size,
                    ..
                } => self.handle_upvalue(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::MOVEREADUPVALUE,
                    payload_size,
                    ..
                } => self.handle_move_upvalue(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::SETUPVALUE,
                    payload_size,
                    ..
                } => self.handle_set_upvalue(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::CLEAR,
                    ..
                } => {
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::LOADINT0,
                    ..
                } => {
                    self.stack.push(SteelVal::INT_ZERO);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::LOADINT1,
                    ..
                } => {
                    self.stack.push(SteelVal::INT_ONE);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::LOADINT2,
                    ..
                } => {
                    self.stack.push(SteelVal::INT_TWO);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::CGLOCALCONST,
                    payload_size,
                    ..
                } => {
                    let read_local = &self.instructions[self.ip + 1];
                    let push_const = &self.instructions[self.ip + 2];

                    // Snag the function
                    let func = self.global_env.repl_lookup_idx(payload_size as usize);

                    // get the local
                    let offset = self.stack_index.last().copied().unwrap_or(0);
                    let local_value = self.stack[read_local.payload_size as usize + offset].clone();

                    // get the const
                    let const_val = self.constants.get(push_const.payload_size as usize);

                    self.handle_lazy_function_call(func, local_value, const_val)?;
                }
                DenseInstruction {
                    op_code: OpCode::MOVECGLOCALCONST,
                    payload_size,
                    ..
                } => {
                    let move_read_local = &self.instructions[self.ip + 1];
                    let push_const = &self.instructions[self.ip + 2];

                    // Snag the function
                    let func = self.global_env.repl_lookup_idx(payload_size as usize);

                    // get the local by moving its position
                    let offset = self.stack_index.last().copied().unwrap_or(0);
                    let local_value = std::mem::replace(
                        &mut self.stack[move_read_local.payload_size as usize + offset],
                        SteelVal::Void,
                    );

                    // get the const
                    let const_val = self.constants.get(push_const.payload_size as usize);

                    self.handle_lazy_function_call(func, local_value, const_val)?;
                }
                DenseInstruction {
                    op_code: OpCode::CALLGLOBAL,
                    payload_size,
                    ..
                } => {
                    self.ip += 1;
                    let next_inst = self.instructions[self.ip];
                    self.handle_call_global(
                        payload_size as usize,
                        next_inst.payload_size as usize,
                    )?;
                }
                DenseInstruction {
                    op_code: OpCode::CALLGLOBALTAIL,
                    payload_size,
                    ..
                } => {
                    // println!("calling global tail");
                    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                    let next_inst = self.instructions[self.ip + 1];
                    self.handle_tail_call_global(
                        payload_size as usize,
                        next_inst.payload_size as usize,
                    )?;
                }
                DenseInstruction {
                    op_code: OpCode::FUNC,
                    payload_size,
                    ..
                } => {
                    // TODO: @Matt -> don't pop the function off of the stack, just read it from there directly.
                    let func = self.stack.pop().unwrap();
                    self.handle_function_call(func, payload_size as usize)?;
                }
                // Tail call basically says "hey this function is exiting"
                // In the closure case, transfer ownership of the stack to the called function
                DenseInstruction {
                    op_code: OpCode::TAILCALL,
                    payload_size,
                    ..
                } => {
                    let func = self.stack.pop().unwrap();
                    self.handle_tail_call(func, payload_size as usize)?
                }
                DenseInstruction {
                    op_code: OpCode::IF,
                    payload_size,
                    ..
                } => {
                    // println!("If payload: {}", payload_size);
                    // println!(
                    //     "Jump payload: {}",
                    //     self.instructions[self.ip + 1].payload_size
                    // );

                    // change to truthy...
                    if self.stack.pop().unwrap().is_truthy() {
                        // TODO: Come back here
                        // println!("Current ip: {} - jump ip: {}", self.ip, payload_size);
                        // self.ip = payload_size as usize;
                        self.ip += 1;
                    } else {
                        self.ip = payload_size as usize;
                        // self.ip = self.instructions[self.ip + 1].payload_size as usize
                        // self.ip += 1;
                    }
                }
                DenseInstruction {
                    op_code: OpCode::TCOJMP,
                    payload_size,
                    ..
                } => {
                    // println!("At tco jump");

                    let current_arity = payload_size as usize;
                    self.ip = 0;

                    let closure_arity = self.function_stack.last().unwrap().arity();

                    if current_arity != closure_arity {
                        stop!(ArityMismatch => format!("tco: function expected {} arguments, found {}", closure_arity, current_arity));
                    }

                    // HACK COME BACK TO THIS
                    // if self.ip == 0 && self.heap.len() > self.heap.limit() {
                    // TODO collect here
                    // self.heap.collect_garbage();
                    // }
                    let offset = self.stack_index.last().copied().unwrap_or(0);

                    // We should have arity at this point, drop the stack up to this point
                    // take the last arity off the stack, go back and replace those in order
                    // [... arg1 arg2 arg3]
                    //      ^^^ <- back = this index
                    // offset = the start of the stack frame
                    // Copy the arg1 arg2 arg3 values to
                    // [... frame-start ... arg1 arg2 arg3]
                    //      ^^^^^^~~~~~~~~
                    let back = self.stack.len() - current_arity;
                    for i in 0..current_arity {
                        self.stack[offset + i] = self.stack[back + i].clone();
                    }

                    // self.stack.truncate(offset + current_arity);
                    self.stack.truncate(offset + current_arity);

                    // println!("stack after truncating: {:?}", self.stack);
                }
                DenseInstruction {
                    op_code: OpCode::JMP,
                    payload_size,
                    ..
                } => {
                    self.ip = payload_size as usize;
                }
                DenseInstruction {
                    op_code: OpCode::BEGINSCOPE,
                    ..
                } => {
                    // todo!()
                    self.ip += 1;
                    // self.stack_index.push(self.stack.len());
                }
                DenseInstruction {
                    op_code: OpCode::SETALLOC,
                    payload_size,
                    ..
                } => {
                    let value_to_assign = self.stack.pop().unwrap();

                    let old_value = self
                        .function_stack
                        .last()
                        .unwrap()
                        .heap_allocated()
                        .borrow_mut()[payload_size as usize]
                        .set(value_to_assign);

                    self.stack.push(old_value);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::READALLOC,
                    payload_size,
                    ..
                } => {
                    let value = self
                        .function_stack
                        .last()
                        .unwrap()
                        .heap_allocated()
                        .borrow()[payload_size as usize]
                        .get();

                    self.stack.push(value);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::ALLOC,
                    payload_size,
                    ..
                } => {
                    let offset =
                        self.stack_index.last().copied().unwrap_or(0) + payload_size as usize;

                    let allocated_var = self.heap.allocate(
                        self.stack[offset].clone(), // TODO: Could actually move off of the stack entirely
                        self.stack.iter(),
                        self.function_stack.function_iter(),
                        self.global_env.roots(),
                    );

                    self.function_stack
                        .last_mut()
                        .unwrap()
                        .heap_allocated
                        .borrow_mut()
                        .push(allocated_var);

                    self.ip += 1;

                    // todo!("Implement patching in vars from the stack to the heap");
                }
                DenseInstruction {
                    op_code: OpCode::LETENDSCOPE,
                    payload_size,
                    ..
                } => {
                    let beginning_scope = payload_size as usize;
                    let offset = self.stack_index.last().copied().unwrap_or(0);

                    // Move to the pop
                    self.ip += 1;

                    let rollback_index = beginning_scope + offset;

                    let last = self.stack.pop().expect("stack empty at pop");

                    self.stack.truncate(rollback_index);
                    self.stack.push(last);

                    /*

                    // todo!()

                    let beginning_scope = payload_size as usize;
                    let offset = self.stack_index.last().copied().unwrap_or(0);

                    // Move to the pop
                    self.ip += 1;

                    // See the count of local variables
                    let value_count_to_close = self.instructions[self.ip].payload_size;

                    // Move past the pop
                    self.ip += 1;

                    let rollback_index = beginning_scope + offset;

                    for i in 0..value_count_to_close {
                        let instr = self.instructions[self.ip];
                        match (instr.op_code, instr.payload_size) {
                            (OpCode::CLOSEUPVALUE, 1) => {
                                self.close_upvalues(rollback_index + i as usize);
                            }
                            (OpCode::CLOSEUPVALUE, 0) => {
                                // TODO -> understand if this is actually what I want to happen
                                // self.close_upvalues(rollback_index + i as usize);
                                // do nothing explicitly, just a normal pop
                            }
                            (op, _) => panic!(
                                "Closing upvalues failed with instruction: {:?} @ {}",
                                op, self.ip
                            ),
                        }
                        self.ip += 1;
                    }

                    println!("Prior to ending scope with rollback index: {rollback_index}");
                    println!("Stack: {:?}", self.stack);

                    let last = self.stack.pop().expect("Stack empty at pop");

                    self.stack.truncate(rollback_index);
                    self.stack.push(last);

                    // self.ip += 1;

                    println!("Ending scope, stack here: {:?}", self.stack);
                    println!("Current instruction: {:?}", self.instructions[self.ip]);
                    // let last = self.stack_index.pop().unwrap();
                    // self.stack.truncate(last);

                    */
                }
                DenseInstruction {
                    op_code: OpCode::POP,
                    payload_size,
                    ..
                } => {
                    if let Some(r) = self.handle_pop(payload_size) {
                        return r;
                    }
                }
                DenseInstruction {
                    op_code: OpCode::POP_PURE,
                    payload_size,
                    ..
                } => {
                    if let Some(r) = self.handle_pop_pure(payload_size) {
                        return r;
                    }
                }
                DenseInstruction {
                    op_code: OpCode::POPNEW,
                    payload_size,
                    ..
                } => {
                    if let Some(r) = self.handle_pop_pure(payload_size) {
                        return r;
                    }
                }
                DenseInstruction {
                    op_code: OpCode::BIND,
                    payload_size,
                    ..
                } => self.handle_bind(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::SCLOSURE,
                    payload_size,
                    ..
                } => self.handle_start_closure(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::NEWSCLOSURE,
                    payload_size,
                    ..
                } => self.handle_new_start_closure(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::PUREFUNC,
                    payload_size,
                    ..
                } => self.handle_pure_function(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::SDEF,
                    ..
                } => self.handle_start_def(),
                DenseInstruction {
                    op_code: OpCode::EDEF,
                    ..
                } => {
                    self.ip += 1;
                }
                _ => {
                    crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                    panic!(
                        "Unhandled opcode: {:?} @ {}",
                        self.instructions[self.ip], self.ip
                    );
                }
            }

            // Process the op code
            // self.profiler.add_time(
            //     &self.instructions[self.ip].op_code,
            //     self.instructions[self.ip].payload_size as usize,
            //     now.elapsed(),
            // );

            // TODO: @Matt Add a way to interrupt back thats not crappy
            // Put callbacks behind generic
            // if U::use_callbacks() {
            //     match self.callback.call_and_increment() {
            //         Some(b) if !b => stop!(Generic => "Callback forced quit of function!"),
            //         _ => {}
            //     }
            // }
        }

        error!(
            "Out of bounds instruction!: instruction pointer: {}, instruction length: {}",
            self.ip,
            self.instructions.len()
        );
        panic!("Out of bounds instruction")
    }

    // #[inline(always)]
    // TODO: This is definitely an issue - if the instruction stack is empty,
    // We will probably end up grabbing a garbage span
    fn current_span(&self) -> Span {
        self.spans
            .get(
                self.instructions
                    .get(self.ip)
                    .map(|x| x.span_index)
                    .unwrap_or_default(),
            )
            // .flatten()
            .copied()
            .unwrap_or_default()
    }

    fn enclosing_span(&self) -> Option<Span> {
        self.function_stack
            .function_stack
            .last()
            .and_then(|x| x.span)
    }

    fn handle_pop_pure(&mut self, payload: u32) -> Option<Result<SteelVal>> {
        // Check that the amoutn we're looking to pop and the function stack length are equivalent
        // otherwise we have a problem
        // assert_eq!(self.pop_count, self.function_stack.len());

        self.pop_count -= 1;

        // unwrap just because we want to see if we have something here
        // rolling back the function stack
        self.function_stack.pop();

        if self.pop_count == 0 {
            let ret_val = self.stack.pop().ok_or_else(|| {
                // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

                SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
                    .with_span(self.current_span())
            });

            // self.close_upvalues();

            // println!("ROLLING BACK STACK");
            // println!("BEFORE: {:?}", self.stack);
            // println!("index: {:?}", self.stack_index);

            // Roll back if needed
            if let Some(rollback_index) = self.stack_index.pop() {
                // TODO check if this is better / correct
                // self.close_upvalues(rollback_index);

                // Move forward past the pop
                self.ip += 1;

                self.stack.truncate(rollback_index);
            }

            Some(ret_val)
        } else {
            let ret_val = self.stack.pop().unwrap();

            // TODO fix this
            let rollback_index = self.stack_index.pop().unwrap();

            // Snatch the value to close from the payload size
            // Move forward past the pop
            self.ip += 1;

            // Close remaining values on the stack

            self.stack.truncate(rollback_index);
            self.stack.push(ret_val);

            // self.stack.drain_range(rollback_index..self.stack.len() - 1);

            // self.stack.truncate(rollback_index + 1);
            // *self.stack.last_mut().unwrap() = ret_val;

            // if !self
            //     .instruction_stack
            //     .last()
            //     .unwrap()
            //     .instrs_ref()
            //     .is_empty()
            // {
            let prev_state = self.instruction_stack.pop().unwrap();
            self.ip = prev_state.0;
            self.instructions = prev_state.instrs();
            // }

            None
        }
    }

    // #[inline(always)]
    fn handle_pop(&mut self, payload: u32) -> Option<Result<SteelVal>> {
        // Check that the amoutn we're looking to pop and the function stack length are equivalent
        // otherwise we have a problem
        // assert_eq!(self.pop_count, self.function_stack.len());

        self.pop_count -= 1;

        // unwrap just because we want to see if we have something here
        // rolling back the function stack
        self.function_stack.pop();

        if self.pop_count == 0 {
            let ret_val = self.stack.pop().ok_or_else(|| {
                // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

                SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
                    .with_span(self.current_span())
            });

            // self.close_upvalues();

            // println!("ROLLING BACK STACK");
            // println!("BEFORE: {:?}", self.stack);
            // println!("index: {:?}", self.stack_index);

            // Roll back if needed
            if let Some(rollback_index) = self.stack_index.pop() {
                // TODO check if this is better / correct
                // self.close_upvalues(rollback_index);

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
            }

            Some(ret_val)
        } else {
            let ret_val = self.stack.pop().unwrap();

            // TODO fix this
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

            // Close remaining values on the stack

            self.stack.truncate(rollback_index);
            self.stack.push(ret_val);

            // self.stack.drain_range(rollback_index..self.stack.len() - 1);

            // self.stack.truncate(rollback_index + 1);
            // *self.stack.last_mut().unwrap() = ret_val;

            // if !self
            //     .instruction_stack
            //     .last()
            //     .unwrap()
            //     .instrs_ref()
            //     .is_empty()
            // {
            let prev_state = self.instruction_stack.pop().unwrap();
            self.ip = prev_state.0;
            self.instructions = prev_state.instrs();
            // }

            None
        }
    }

    // #[inline(always)]
    fn handle_panic(&mut self, span: Span) -> Result<()> {
        let error_message = self.stack.pop().unwrap();
        stop!(Generic => error_message.to_string(); span);
    }

    // #[inline(always)]
    fn handle_struct(&mut self, offset: usize) -> Result<()> {
        let val = self.constants.get(offset);

        let mut iter = if let SteelVal::ListV(l) = val {
            l.into_iter()
        } else {
            stop!(Generic => "ICE: Struct expected a list");
        };

        // List of indices e.g. '(25 26 27 28) to bind struct functions to
        let indices = iter.next().unwrap();

        // The name of the struct
        let name: String = if let SteelVal::StringV(s) = iter.next().unwrap() {
            s.to_string()
        } else {
            stop!( Generic => "ICE: Struct expected a string name")
        };

        // The fields of the structs
        let fields: Vec<Rc<str>> = iter
            .map(|x| {
                if let SteelVal::StringV(s) = x {
                    Ok(s)
                } else {
                    stop!(Generic => "ICE: Struct encoded improperly with non string fields")
                }
            })
            .collect::<Result<Vec<_>>>()?;

        // Get them as &str for now
        let other_fields: Vec<&str> = fields.iter().map(|x| x.as_ref()).collect();

        // Generate the functions, but they immediately override them with the names
        // Store them with the indices
        let funcs = SteelStruct::generate_from_name_fields(name.as_str(), &other_fields)?;

        let index_iter = if let SteelVal::ListV(l) = indices {
            l.into_iter()
        } else {
            stop!(Generic => "ICE: Struct expected a list");
        };

        for ((_, func), idx) in funcs.into_iter().zip(index_iter) {
            let idx = if let SteelVal::IntV(idx) = idx {
                idx as usize
            } else {
                stop!(Generic => "Index wrong in structs")
            };

            self.global_env.repl_define_idx(idx, func);
        }
        Ok(())
    }

    // #[inline(always)]
    fn handle_inner_struct(&mut self, offset: usize) -> Result<()> {
        let val = self.constants.get(offset);

        let mut iter = if let SteelVal::ListV(l) = val {
            l.into_iter()
        } else {
            stop!(Generic => "ICE: Struct expected a list");
        };

        // List of indices e.g. '(25 26 27 28) to bind struct functions to
        let _ = iter.next().unwrap();

        // The name of the struct
        let name: String = if let SteelVal::StringV(s) = iter.next().unwrap() {
            s.to_string()
        } else {
            stop!( Generic => "ICE: Struct expected a string name")
        };

        // The fields of the structs
        let fields: Vec<Rc<str>> = iter
            .map(|x| {
                if let SteelVal::StringV(s) = x {
                    Ok(s)
                } else {
                    stop!(Generic => "ICE: Struct encoded improperly with non string fields")
                }
            })
            .collect::<Result<Vec<_>>>()?;

        // Get them as &str for now
        let other_fields: Vec<&str> = fields.iter().map(|x| x.as_ref()).collect();

        // Generate the functions, but they immediately override them with the names
        // Store them with the indices
        let funcs = SteelStruct::generate_from_name_fields(name.as_str(), &other_fields)?;

        // We've mapped in the compiler _where_ locals are going to be (on the stack), just put them there
        for (_, func) in funcs {
            self.stack.push(func);
        }

        Ok(())
    }

    // TODO -> this doesn't have to be an opcode
    // instead make this a built in function
    // #[inline(always)]
    // fn handle_read(&mut self) -> Result<()> {
    //     // this needs to be a string
    //     let expression_to_parse = self.stack.pop().unwrap();

    //     if let SteelVal::StringV(expr) = expression_to_parse {
    //         // dummy interning hashmap because the parser is bad
    //         // please don't judge I'm working on fixing it
    //         // TODO
    //         let mut intern = FnvHashMap::new();

    //         let parsed: result::Result<Vec<ExprKind>, ParseError> =
    //             Parser::new(expr.as_str(), &mut intern).collect();

    //         match parsed {
    //             Ok(v) => {
    //                 let converted: Result<List<SteelVal>> =
    //                     v.into_iter().map(SteelVal::try_from).collect();

    //                 self.stack.push(SteelVal::ListV(converted?));
    //                 self.ip += 1;
    //             }
    //             Err(e) => stop!(Generic => format!("{}", e); self.current_span()),
    //         }
    //     } else {
    //         stop!(TypeMismatch => "read expects a string"; self.current_span())
    //     }
    //     Ok(())
    // }

    // #[inline(always)]
    fn handle_set(&mut self, index: usize) -> Result<()> {
        let value_to_assign = self.stack.pop().unwrap();

        // if let SteelVal::Closure(_) = &value_to_assign {
        //     // println!("Closing upvalue in set");
        //     self.close_upvalues(*self.stack_index.last().unwrap_or(&0));
        // }

        let value = self.global_env.repl_set_idx(index, value_to_assign)?;

        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn handle_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
        let func = self.global_env.repl_lookup_idx(index);
        // TODO - handle this a bit more elegantly
        // self.handle_function_call(func, payload_size, span)
        self.handle_global_function_call(func, payload_size, index)
    }

    // #[inline(always)]
    fn handle_tail_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
        let func = self.global_env.repl_lookup_idx(index);
        self.ip += 1;
        self.handle_tail_call(func, payload_size)
    }

    // #[inline(always)]
    fn handle_push(&mut self, index: usize) -> Result<()> {
        let value = self.global_env.repl_lookup_idx(index);
        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn handle_local(&mut self, index: usize) -> Result<()> {
        // println!("Stack index: {:?}", self.stack_index);
        // println!("Stack index value: {:?}", index);
        // println!("Stack: {:?}", self.stack);
        let offset = self.stack_index.last().copied().unwrap_or(0);
        let value = self.stack[index + offset].clone();
        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    fn handle_read_captures(&mut self, index: usize) -> Result<()> {
        let value = self.function_stack.last().unwrap().captures()[index].clone();

        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn handle_move_local(&mut self, index: usize) -> Result<()> {
        let offset = self.stack_index.last().copied().unwrap_or(0);
        let value = std::mem::replace(&mut self.stack[index + offset], SteelVal::Void);

        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
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
                // .unwrap_or_else(|| {
                //     crate::core::instructions::pretty_print_dense_instructions(
                //         &self.instructions,
                //     );
                //     panic!("Known issue")
                // })
            })
            .unwrap();

        // println!("Getting upvalue: {}", value);

        self.stack.push(value);
        self.ip += 1;
    }

    // #[inline(always)]
    fn handle_move_upvalue(&mut self, index: usize) {
        let value = self.function_stack.last().unwrap().upvalues()[index]
            .upgrade()
            .expect("Upvalue dropped too early!")
            .borrow_mut()
            .try_move_value(&mut self.stack);

        self.stack.push(value);
        self.ip += 1;
    }

    fn handle_pure_function(&mut self, offset: usize) {
        // println!("Hitting start closure");

        // println!("Instruction: {:?}", self.instructions[self.ip]);

        // if self.instructions[self.ip].payload_size == 1 {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        let is_multi_arity = self.instructions[self.ip].payload_size == 1;

        self.ip += 1;

        // Check whether this is a let or a rooted function
        let closure_id = self.instructions[self.ip].payload_size as usize;

        // if is_multi_arity {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        // TODO - used to be offset - 2, now 3 with the multi arity
        let forward_jump = offset - 2;
        // let forward_jump = offset;

        // TODO clean this up a bit
        // hold the spot for where we need to jump aftwards
        let forward_index = self.ip + forward_jump;

        let constructed_lambda = if let Some(prototype) =
            self.pure_function_interner.get(&closure_id)
        {
            prototype.clone()
        } else {
            // debug_assert!(self.instructions[forward_index - 1].op_code == OpCode::ECLOSURE);

            // TODO -> this is probably quite slow
            // If extraneous lets are lifted, we probably don't need this
            // or if instructions get stored in some sort of shared memory so I'm not deep cloning the window

            // Construct the closure body using the offsets from the payload
            // used to be - 1, now - 2
            let closure_body = self.instructions[self.ip..(self.ip + forward_jump - 1)].to_vec();

            // snag the arity from the eclosure instruction
            let arity = self.instructions[forward_index - 1].payload_size;

            let constructed_lambda = Gc::new(ByteCodeLambda::new(
                closure_body,
                arity as usize,
                Vec::new(),
                false,
                is_multi_arity,
                Vec::new(),
                Vec::new(),
            ));

            self.pure_function_interner
                .insert(closure_id, Gc::clone(&constructed_lambda));

            constructed_lambda
        };

        self.stack.push(SteelVal::Closure(constructed_lambda));

        self.ip = forward_index;
    }

    // #[inline(always)]
    fn handle_start_closure(&mut self, offset: usize) {
        // println!("Hitting start closure");

        // println!("Instruction: {:?}", self.instructions[self.ip]);

        // if self.instructions[self.ip].payload_size == 1 {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        let is_multi_arity = self.instructions[self.ip].payload_size == 1;

        self.ip += 1;

        // Check whether this is a let or a rooted function
        let is_let = self.instructions[self.ip].payload_size == 1;

        // if is_multi_arity {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        // TODO - used to be offset - 2, now 3 with the multi arity
        let forward_jump = offset - 3;

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
                        // self.capture_upvalue(self.stack_index.last().unwrap_or(&0) + n as usize),
                        self.capture_upvalue(
                            n as usize,
                            self.stack_index.last().copied().unwrap_or(0),
                        ),
                        // self.capture_upvalue(n as usize),
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

        // TODO -> this is probably quite slow
        // If extraneous lets are lifted, we probably don't need this
        // or if instructions get stored in some sort of shared memory so I'm not deep cloning the window

        // Construct the closure body using the offsets from the payload
        // used to be - 1, now - 2
        let closure_body = self.instructions[self.ip..(self.ip + forward_jump - 1)].to_vec();

        // snag the arity from the eclosure instruction
        let arity = self.instructions[forward_index - 1].payload_size;

        let constructed_lambda = ByteCodeLambda::new(
            closure_body,
            arity as usize,
            upvalues,
            is_let,
            is_multi_arity,
            Vec::new(),
            Vec::new(),
        );

        self.stack
            .push(SteelVal::Closure(Gc::new(constructed_lambda)));

        self.ip = forward_index;
    }

    fn handle_new_start_closure(&mut self, offset: usize) {
        // println!("Hitting start closure");

        // println!("Instruction: {:?}", self.instructions[self.ip]);

        // if self.instructions[self.ip].payload_size == 1 {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        let is_multi_arity = self.instructions[self.ip].payload_size == 1;

        self.ip += 1;

        // Get the ID of the function
        let closure_id = self.instructions[self.ip].payload_size as usize;

        // if is_multi_arity {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        // TODO - used to be offset - 2, now 3 with the multi arity
        // let forward_jump = offset;
        let forward_jump = offset - 3;
        // println!("Forward jump: {}", forward_jump);

        // Snag the number of upvalues here
        let ndefs = self.instructions[self.ip].payload_size;
        self.ip += 1;

        // TODO preallocate size
        let mut captures = Vec::with_capacity(ndefs as usize);

        // TODO: This shouldn't be the same size as the captures
        let mut heap_vars = Vec::with_capacity(ndefs as usize);

        // TODO clean this up a bit
        // hold the spot for where we need to jump aftwards
        let forward_index = self.ip + forward_jump;

        // Insert metadata
        for _ in 0..ndefs {
            let instr = self.instructions[self.ip];
            match (instr.op_code, instr.payload_size) {
                (OpCode::COPYCAPTURESTACK, n) => {
                    let offset = self.stack_index.last().copied().unwrap_or(0);
                    let value = self.stack[n as usize + offset].clone();
                    captures.push(value);
                }
                (OpCode::COPYCAPTURECLOSURE, n) => {
                    debug_assert!(
                        !self.function_stack.is_empty(),
                        "Trying to capture from closure that doesn't exist",
                    );

                    debug_assert!(
                        (n as usize) < self.function_stack.last().unwrap().captures().len()
                    );

                    let value = self.function_stack.last().unwrap().captures()[n as usize].clone();

                    captures.push(value);
                }
                (OpCode::COPYHEAPCAPTURECLOSURE, n) => {
                    debug_assert!(
                        (n as usize)
                            < self
                                .function_stack
                                .last()
                                .unwrap()
                                .heap_allocated()
                                .borrow()
                                .len()
                    );

                    heap_vars.push(
                        self.function_stack
                            .last()
                            .unwrap()
                            .heap_allocated()
                            .borrow()[n as usize]
                            .clone(),
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

        let constructed_lambda = if let Some(prototype) = self.closure_interner.get(&closure_id) {
            log::info!("Fetching closure from cache");

            let mut prototype = prototype.clone();
            prototype.set_captures(captures);
            prototype.set_heap_allocated(heap_vars);
            prototype
        } else {
            log::info!("Constructing closure for the first time");

            debug_assert!(self.instructions[forward_index - 1].op_code == OpCode::ECLOSURE);

            // debug_assert!(self.ip + forward_jump - 1 <= self.instructions.len())

            if forward_index - 1 > self.instructions.len() {
                crate::core::instructions::pretty_print_dense_instructions(
                    self.instructions.as_ref(),
                );
                println!("Forward index: {}", self.ip + forward_jump - 1);
                println!("Length: {}", self.instructions.len());
                panic!("Out of bounds forward jump");
            }

            // Construct the closure body using the offsets from the payload
            // used to be - 1, now - 2
            let closure_body = self.instructions[self.ip..(forward_index - 1)].to_vec();

            // snag the arity from the eclosure instruction
            let arity = self.instructions[forward_index - 1].payload_size;

            let mut constructed_lambda = ByteCodeLambda::new(
                closure_body,
                arity as usize,
                Vec::new(),
                false,
                is_multi_arity,
                Vec::new(),
                Vec::new(),
            );

            self.closure_interner
                .insert(closure_id, constructed_lambda.clone());

            constructed_lambda.set_captures(captures);
            constructed_lambda.set_heap_allocated(heap_vars);

            constructed_lambda
        };

        self.stack
            .push(SteelVal::Closure(Gc::new(constructed_lambda)));

        self.ip = forward_index;
    }

    // #[inline(always)]
    fn handle_bind(&mut self, payload_size: usize) {
        self.global_env
            .repl_define_idx(payload_size, self.stack.pop().unwrap());

        self.ip += 1;
    }

    // #[inline(always)]
    fn handle_set_local(&mut self, index: usize) {
        let value_to_set = self.stack.pop().unwrap();
        let offset = self.stack_index.last().copied().unwrap_or(0);

        let old_index = index + offset;
        let old_value = self.stack[old_index].clone();

        // Modify the stack and change the value to the new one
        self.stack[old_index] = value_to_set;

        self.stack.push(old_value);
        self.ip += 1;
    }

    // #[inline(always)]
    fn handle_set_upvalue(&mut self, index: usize) {
        let new = self.stack.pop().unwrap();
        let last_func = self.function_stack.last().unwrap();
        let upvalue = last_func.upvalues()[index].upgrade().unwrap();
        let value = upvalue.borrow_mut().mutate_value(&mut self.stack, new);

        self.stack.push(value);
        self.ip += 1;
    }

    // Walk through a function and find if there are upvalues that need to be closed from it
    pub fn find_upvalues_to_be_closed(
        &mut self,
        rollback_index: usize,
        function: &Gc<ByteCodeLambda>,
    ) {
        // println!("Inside find upvalues to be closed");

        let last_pop = function
            .body_exp
            .iter()
            .rposition(|x| x.op_code == OpCode::POP)
            .expect("function missing pop instruction");

        // crate::core::instructions::pretty_print_dense_instructions(&function.body_exp);

        // println!("Last pop index: {}", last_pop);

        for (i, instr) in function.body_exp[last_pop + 1..].iter().enumerate() {
            // println!("INSTRUCTION: {:?}", instr);

            match (instr.op_code, instr.payload_size) {
                (OpCode::CLOSEUPVALUE, 1) => {
                    // println!("^^^^^^^^^^^Finding upvalues to be closed^^^^^^^^^^^^^^");
                    self.close_upvalues(rollback_index + i as usize);
                    // indices.push(i);
                }
                (OpCode::CLOSEUPVALUE, 0) => {
                    // println!("Found upvalues that do not need to be closed");
                    // do nothing explicitly, just a normal pop
                }
                _ => break,
            }
        }
    }

    // #[inline(always)]
    fn handle_tail_call_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        // println!("stack before: {:?}", self.stack);
        // println!("stack index: {:?}", self.stack_index);
        // println!(
        //     "Function stack length before: {}",
        //     self.function_stack.len()
        // );

        // closure.increment_call_count();

        // while let Some(current_executing) = self.function_stack.pop() {
        //     if !current_executing.is_let {}
        // }

        let mut offset;

        // self.close_upvalues(0);

        // let mut scopes_to_pop = Vec::new();

        loop {
            // We've already hit the root of our env
            if self.stack_index.len() == 1 {
                break;
            }

            // We've reached the root of wherever we are
            if let Some(last) = self.function_stack.last() {
                if !last.is_let {
                    // println!("Found root");
                    // self.pop_count -= 1;
                    // offset = self.stack_index.pop().unwrap_or(0);
                    break;
                }
            } else {
                break;
            }

            // println!("Rolling back function");
            // println!("Pop count: {}", self.pop_count);

            let current_executing = self.function_stack.pop().unwrap();

            // scopes_to_pop.push(current_executing);

            // self.close_upvalues(offset);

            // self.upvalue_heap.profile_heap();

            // if !current_executing.is_let {
            //     println!("Found root");
            //     // self.stack_index.pop();
            //     break;
            // }

            self.pop_count -= 1;
            offset = self.stack_index.pop().unwrap();
            self.instruction_stack.pop();

            self.find_upvalues_to_be_closed(offset, &current_executing.function);
        }

        offset = self.stack_index.last().copied().unwrap_or(0);

        // println!("Falling back to offset: {}", offset);
        // println!("Pop count before: {}", self.pop_count);

        // Wipe out the last one
        let current_executing = self.function_stack.pop().unwrap();

        // if !current_executing
        //     .map(|x| x.upvalues().is_empty())
        //     .unwrap_or(true)
        // {
        //     println!("CLOSING UPVALUES *******************");
        //     self.close_upvalues(offset);
        // }

        self.find_upvalues_to_be_closed(offset, &current_executing.function);

        // TODO
        self.function_stack
            .push(CallContext::new(Gc::clone(&closure)));

        // if self.stack_index.len() == STACK_LIMIT {
        //     // println!("stacks at exit: {:?}", self.stacks);
        //     crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
        //     println!("tail call - stack frame at exit: {:?}", self.stack);
        //     stop!(Generic => "stack overflowed!"; self.current_span());
        // }

        // if closure.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        // }

        if closure.is_multi_arity {
            if payload_size < closure.arity() - 1 {
                stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", closure.arity(), payload_size); self.current_span());
            }

            // (define (test x . y))
            // (test 1 2 3 4 5)
            // in this case, arity = 2 and payload size = 5
            // pop off the last 4, collect into a list
            let amount_to_remove = 1 + payload_size - closure.arity();

            let values = self.stack.split_off(self.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(List::from(values));

            self.stack.push(list);
        } else if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        }

        // Find the new arity from the payload
        let new_arity = payload_size;

        // We should have arity at this point, drop the stack up to this point
        // take the last arity off the stack, go back and replace those in order
        let back = self.stack.len() - new_arity;
        for i in 0..new_arity {
            self.stack[offset + i] = self.stack[back + i].clone();
        }

        self.stack.truncate(offset + new_arity);

        // self.stack_index.push(offset);

        // println!("stack after: {:?}", self.stack);
        // println!("stack index: {:?}", self.stack_index);
        // println!("Pop count after: {}", self.pop_count);

        // println!("Function stack length after: {}", self.function_stack.len());

        // Just collect here and minimize the heap size
        self.upvalue_heap.collect(
            self.stack.iter().chain(self.global_env.bindings_vec.iter()),
            self.function_stack.function_iter(),
        );

        // self.pop_count -= 1;

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

    fn new_handle_tail_call_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        // println!("stack before: {:?}", self.stack);
        // println!("stack index: {:?}", self.stack_index);
        // println!(
        //     "Function stack length before: {}",
        //     self.function_stack.len()
        // );

        // closure.increment_call_count();

        // while let Some(current_executing) = self.function_stack.pop() {
        //     if !current_executing.is_let {}
        // }

        // let ;

        // self.close_upvalues(0);

        // let mut scopes_to_pop = Vec::new();

        // println!("Pop count: {}", self.pop_count);

        self.function_stack.pop().unwrap();

        // self.pop_count -= 1;
        // let offset = self.stack_index.pop().unwrap();
        // self.instruction_stack.pop();

        let offset = self.stack_index.last().copied().unwrap_or(0);

        // println!("Falling back to offset: {}", offset);
        // println!("Pop count before: {}", self.pop_count);

        // Wipe out the last one
        // let current_executing = self.function_stack.pop().unwrap();

        // if !current_executing
        //     .map(|x| x.upvalues().is_empty())
        //     .unwrap_or(true)
        // {
        //     println!("CLOSING UPVALUES *******************");
        //     self.close_upvalues(offset);
        // }

        // TODO
        self.function_stack
            .push(CallContext::new(Gc::clone(&closure)).with_span(self.current_span()));

        // if self.stack_index.len() == STACK_LIMIT {
        //     // println!("stacks at exit: {:?}", self.stacks);
        //     crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
        //     println!("tail call - stack frame at exit: {:?}", self.stack);
        //     stop!(Generic => "stack overflowed!"; self.current_span());
        // }

        // if closure.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        // }

        let mut new_arity = payload_size;

        if closure.is_multi_arity {
            println!(
                "multi closure function, multi arity, arity: {:?}",
                closure.arity()
            );

            if payload_size < closure.arity() - 1 {
                stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", closure.arity(), payload_size); self.current_span());
            }

            // (define (test x . y))
            // (test 1 2 3 4 5)
            // in this case, arity = 2 and payload size = 5
            // pop off the last 4, collect into a list
            let amount_to_remove = 1 + payload_size - closure.arity();

            let values = self.stack.split_off(self.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(List::from(values));

            self.stack.push(list);

            new_arity = closure.arity();

            println!("Stack after list conversion: {:?}", self.stack);
        } else if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        }

        // Find the new arity from the payload

        // We should have arity at this point, drop the stack up to this point
        // take the last arity off the stack, go back and replace those in order
        let back = self.stack.len() - new_arity;
        for i in 0..new_arity {
            self.stack[offset + i] = self.stack[back + i].clone();
        }

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

    // #[inline(always)]
    fn handle_tail_call(&mut self, stack_func: SteelVal, payload_size: usize) -> Result<()> {
        use SteelVal::*;
        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size),
            FuncV(f) => self.call_primitive_func(f, payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            FutureFunc(f) => self.call_future_func(f, payload_size),
            ContractedFunction(cf) => self.call_contracted_function_tail_call(cf, payload_size),
            #[cfg(feature = "jit")]
            CompiledFunction(function) => self.call_compiled_function(function, payload_size),
            ContinuationFunction(cc) => self.call_continuation(cc),
            // TODO: Take this out when we move to the new code gen
            Closure(closure) => self.new_handle_tail_call_closure(closure, payload_size),
            // Closure(closure) => self.handle_tail_call_closure(closure, payload_size),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            _ => {
                println!("{:?}", self.stack);
                println!("{:?}", self.stack_index);
                crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                stop!(BadSyntax => format!("TailCall - Application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }
    }

    // #[inline(always)]
    fn call_boxed_func(
        &mut self,
        func: &Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>,
        payload_size: usize,
    ) -> Result<()> {
        let last_index = self.stack.len() - payload_size;

        let result =
            func(&self.stack[last_index..]).map_err(|x| x.set_span_if_none(self.current_span()))?;

        self.stack.truncate(last_index);

        self.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    // NOTE: Here, the last element on the stack _is_ the function we're referring to. In this case, just avoid
    // touching the last element and move on.
    fn call_boxed_func_on_stack(
        &mut self,
        func: Rc<dyn Fn(&[SteelVal]) -> Result<SteelVal>>,
        payload_size: usize,
    ) -> Result<()> {
        // stack is [args ... function]
        let len = self.stack.len();
        // This is the start of the arguments
        let last_index = len - payload_size - 1;

        // Peek the range for the [args ... function]
        //                        ~~~~~~~~~~
        // let result = func(self.stack.peek_range_double(last_index..len))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let result = match func(&self.stack[last_index..len]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way, but now given that the function is included on the stack, this should work...
        // self.stack.truncate(last_index);
        // self.stack.push(result);

        self.stack.truncate(last_index + 1);
        *self.stack.last_mut().unwrap() = result;

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_primitive_func_on_stack(
        &mut self,
        func: fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        // stack is [args ... function]
        let len = self.stack.len();
        // This is the start of the arguments
        let last_index = len - payload_size - 1;

        // Peek the range for the [args ... function]
        //                        ~~~~~~~~~~
        // let result = func(self.stack.peek_range_double(last_index..len))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let result = match func(&self.stack[last_index..len]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way, but now given that the function is included on the stack, this should work...
        // self.stack.truncate(last_index);
        // self.stack.push(result);

        self.stack.truncate(last_index + 1);
        *self.stack.last_mut().unwrap() = result;

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_builtin_func(&mut self, func: &BuiltInSignature, payload_size: usize) -> Result<()> {
        // Note: We Advance the pointer here. In the event we're calling a builtin that fusses with
        // the instruction pointer, we allow the function to override this. For example, call/cc will
        // advance the pointer - or perhaps, even fuss with the control flow.
        self.ip += 1;

        // TODO: Don't do this - just read directly from the stack
        let args = self.stack.split_off(self.stack.len() - payload_size);
        let result = func(self, &args).map_err(|x| {
            // TODO: @Matt 4/24/2022 -> combine this into one function probably
            if x.has_span() {
                x
            } else {
                x.set_span_if_none(self.current_span())
            }
            // x.set_span_if_none(self.current_span())
        })?;

        self.stack.push(result);
        Ok(())
    }

    // #[inline(always)]
    fn call_primitive_mut_func(
        &mut self,
        f: &fn(&mut [SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        // println!("Stack: {:?}", self.stack);

        let last_index = self.stack.len() - payload_size;

        let result = f(&mut self.stack[last_index..])
            .map_err(|x| x.set_span_if_none(self.current_span()))?;

        // TODO -> this can actually just be something like:
        // self.stack.truncate(self.stack.len() - payload_size + 1)
        // self.stack[self.stack.len() - 1] = result
        self.stack.truncate(last_index);
        self.stack.push(result);

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_primitive_func(
        &mut self,
        f: &fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        // let result = f(self.stack.peek_range(self.stack.len() - payload_size..))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let last_index = self.stack.len() - payload_size;

        let result = match f(&self.stack[last_index..]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // println!("Length to truncate to: {:?}", last_index);
        // println!("Stack at this point: {:?}", self.stack);

        // TODO: @Matt - This is a neat little optimization, although it only works for function calls > 1 argument
        // Function calls without args, this quits on.
        // Specialize function calls without arguments - this should be a fairly easy, free, speed up.

        // let truncate_len = last_index + 1;
        // if truncate_len > self.stack.len() {
        //     self.stack.truncate(last_index);
        //     self.stack.push(result);
        // } else {
        //     self.stack.truncate(truncate_len);
        //     *self.stack.last_mut().unwrap() = result;
        // }

        // if last_index + 1 > self.stack.len() {
        //     println!("Length to truncate to: {:?}", last_index);
        //     println!("Stack length at this point: {:?}", self.stack.len());
        //     println!("Stack at this point: {:?}", self.stack);
        //     panic!("Something is up here");
        // }

        // This is the old way... lets see if the below way improves the speed
        self.stack.truncate(last_index);
        self.stack.push(result);

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_contracted_function(
        &mut self,
        cf: &ContractedFunction,
        payload_size: usize,
    ) -> Result<()> {
        if let Some(arity) = cf.arity() {
            if arity != payload_size {
                stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, payload_size); self.current_span());
            }
        }

        // if A::enforce_contracts() {
        let args = self.stack.split_off(self.stack.len() - payload_size);

        let result = cf.apply(args, &self.current_span(), self)?;

        self.stack.push(result);
        self.ip += 1;
        Ok(())
        // } else {
        //     self.handle_function_call(cf.function.clone(), payload_size)
        // }
    }

    // #[inline(always)]
    fn call_contracted_function_tail_call(
        &mut self,
        cf: &ContractedFunction,
        payload_size: usize,
    ) -> Result<()> {
        if let Some(arity) = cf.arity() {
            if arity != payload_size {
                stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, payload_size); self.current_span());
            }
        }

        // if A::enforce_contracts() {
        let args = self.stack.split_off(self.stack.len() - payload_size);

        let result = cf.apply(args, &self.current_span(), self)?;

        self.stack.push(result);
        self.ip += 1;
        Ok(())
        // } else {
        // self.handle_tail_call(cf.function.clone(), payload_size)
        // }
    }

    fn call_future_func_on_stack(
        &mut self,
        func: Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>,
        payload_size: usize,
    ) -> Result<()> {
        // stack is [args ... function]
        let len = self.stack.len();
        // This is the start of the arguments
        let last_index = len - payload_size - 1;

        // Peek the range for the [args ... function]
        //                        ~~~~~~~~~~
        // let result = func(self.stack.peek_range_double(last_index..len))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let result = match func(&self.stack[last_index..len]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way, but now given that the function is included on the stack, this should work...
        // self.stack.truncate(last_index);
        // self.stack.push(result);

        self.stack.truncate(last_index + 1);
        *self.stack.last_mut().unwrap() = SteelVal::FutureV(Gc::new(result));

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_future_func(
        &mut self,
        f: &Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>,
        payload_size: usize,
    ) -> Result<()> {
        let last_index = self.stack.len() - payload_size;

        let result = SteelVal::FutureV(Gc::new(f(&self.stack[last_index..])?));

        self.stack.truncate(last_index);
        self.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
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

    // #[inline(always)]
    fn handle_lazy_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        local: SteelVal,
        const_value: SteelVal,
    ) -> Result<()> {
        // push them onto the stack if we need to
        self.stack.push(local);
        self.stack.push(const_value);
        // Push on the function stack so we have access to it later
        self.function_stack
            .push(CallContext::new(Gc::clone(closure)).with_span(self.current_span()));

        if closure.is_multi_arity {
            panic!("Calling lazy closure with multi arity");
        }

        if closure.arity() != 2 {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), 2); self.current_span());
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_index.len() == STACK_LIMIT {
            crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
            println!("lazy - stack frame at exit: {:?}", self.stack);
            stop!(Generic => "lazy closure: stack overflowed!"; self.current_span());
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

    // #[inline(always)]
    fn handle_lazy_function_call(
        &mut self,
        stack_func: SteelVal,
        local: SteelVal,
        const_value: SteelVal,
    ) -> Result<()> {
        use SteelVal::*;

        match &stack_func {
            BoxedFunction(f) => {
                self.stack.push(
                    f(&[local, const_value])
                        .map_err(|x| x.set_span_if_none(self.current_span()))?,
                );
                self.ip += 4;
            }
            FuncV(f) => {
                // self.stack
                //     .push(f(&[local, const_value]).map_err(|x| x.set_span_if_none(self.current_span()))?);
                // self.ip += 4;

                match f(&[local, const_value]) {
                    Ok(value) => self.stack.push(value),
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                }

                // self.stack
                // .push(f(&[local, const_value]).map_err(|x| x.set_span_if_none(self.current_span()))?);
                self.ip += 4;
            }
            FutureFunc(f) => {
                let result = SteelVal::FutureV(Gc::new(
                    f(&[local, const_value])
                        .map_err(|x| x.set_span_if_none(self.current_span()))?,
                ));

                self.stack.push(result);
                self.ip += 4;
            }
            ContractedFunction(cf) => {
                if let Some(arity) = cf.arity() {
                    if arity != 2 {
                        stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, 2); self.current_span());
                    }
                }

                // if A::enforce_contracts() {
                let result = cf.apply(vec![local, const_value], &self.current_span(), self)?;

                self.stack.push(result);
                self.ip += 4;
                // } else {
                // self.handle_lazy_function_call(cf.function.clone(), local, const_value)?;
                // }
            }
            // Contract(c) => self.call_contract(c, payload_size, span)?,
            ContinuationFunction(_cc) => {
                unimplemented!("calling continuation lazily not yet handled");
            }
            Closure(closure) => self.handle_lazy_closure(closure, local, const_value)?,
            MutFunc(func) => {
                let mut args = [local, const_value];
                self.stack
                    .push(func(&mut args).map_err(|x| x.set_span_if_none(self.current_span()))?);

                self.ip += 4;
            }
            BuiltIn(func) => {
                let args = [local, const_value];
                let result =
                    func(self, &args).map_err(|x| x.set_span_if_none(self.current_span()))?;
                self.stack.push(result);
                self.ip += 4;
            }
            _ => {
                println!("{:?}", stack_func);
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; self.current_span());
            }
        }
        Ok(())
    }

    // // #[inline(always)]
    fn handle_function_call_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

        // println!("Handling function call for multi arity function");

        // Jit profiling

        #[cfg(feature = "jit")]
        {
            closure.increment_call_count();
        }

        // Push on the function stack so we have access to it laters
        self.function_stack
            .push(CallContext::new(Gc::clone(closure)).with_span(self.current_span()));

        // if closure.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
        // }

        if closure.is_multi_arity {
            println!("Arity: {}", closure.arity());

            if payload_size < closure.arity() - 1 {
                stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", closure.arity(), payload_size); self.current_span());
            }

            // (define (test x . y))
            // (test 1 2 3 4 5)
            // in this case, arity = 2 and payload size = 5
            // pop off the last 4, collect into a list
            let amount_to_remove = 1 + payload_size - closure.arity();

            let values = self.stack.split_off(self.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(List::from(values));

            self.stack.push(list);
        } else if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_index.len() == STACK_LIMIT {
            crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
            println!("function call - stack frame at exit: {:?}", self.stack);
            stop!(Generic => "function call closure: stack overflowed!"; self.current_span());
        }

        self.stack_index.push(self.stack.len() - closure.arity());

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

    #[cfg(feature = "jit")]
    // #[inline(always)]
    fn call_compiled_function(
        &mut self,
        function: &JitFunctionPointer,
        payload_size: usize,
    ) -> Result<()> {
        if function.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", function.arity(), payload_size); self.current_span());
        }

        let result = function.call_func(self.stack);

        // println!("Calling function!");

        self.stack.push(result);
        self.ip += 1;

        Ok(())
    }

    // TODO improve this a bit
    // #[inline(always)]
    fn handle_function_call_closure_jit(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
        ast_index: usize,
    ) -> Result<()> {
        // Jit profiling
        // closure.increment_call_count();

        // if closure.is_multi_arity {
        //     println!("Calling multi arity function");
        // }

        // println!(
        //     "Calling function with arity: {:?}, payload size: {:?}",
        //     closure.arity(),
        //     payload_size
        // );

        // TODO take this out

        #[cfg(feature = "jit")]
        {
            // if closure.call_count() > JIT_THRESHOLD && !closure.has_attempted_to_be_compiled() {
            //     // unimplemented!();
            //     if let Some(jit) = &mut self.jit {
            //         if let Some(function_ast) = self.global_env.get_expr(ast_index) {
            //             if let Ok(compiled_func) = jit.compile(function_ast) {
            //                 self.global_env.repl_define_idx(
            //                     ast_index,
            //                     SteelVal::CompiledFunction(compiled_func.clone()),
            //                 );

            //                 return self.call_compiled_function(&compiled_func, payload_size, span);
            //             } else {
            //                 // Mark this function as being unable to be compiled
            //                 closure.set_cannot_be_compiled();
            //             }
            //         }
            //     }
            // }
        }

        // Push on the function stack so we have access to it later
        ;
        self.function_stack
            .push(CallContext::new(Gc::clone(closure)).with_span(self.current_span()));

        // TODO - this is unclear - need to pop values off of the stack, collect them as a list, then push it back in
        // If this is a multi arity function
        // then we should just
        if closure.is_multi_arity {
            // println!("Calling multi arity function");

            if payload_size < closure.arity() - 1 {
                stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", closure.arity(), payload_size); self.current_span());
            }

            // (define (test x . y))
            // (test 1 2 3 4 5)
            // in this case, arity = 2 and payload size = 5
            // pop off the last 4, collect into a list
            let amount_to_remove = 1 + payload_size - closure.arity();

            let values = self.stack.split_off(self.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(List::from(values));

            self.stack.push(list);
        } else if closure.arity() != payload_size {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_index.len() == STACK_LIMIT {
            crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
            println!("stack frame at exit: {:?}", self.stack);
            stop!(Generic => "function call closure jit: stack overflowed!"; self.current_span());
        }

        // closure arity here is the number of true arguments
        self.stack_index.push(self.stack.len() - closure.arity());

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        // std::mem::replace(x, y)

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

    // #[inline(always)]
    fn handle_global_function_call(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
        ast_index: usize,
    ) -> Result<()> {
        use SteelVal::*;

        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size)?,
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size)?,
            FuncV(f) => self.call_primitive_func(f, payload_size)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => self.call_contracted_function(cf, payload_size)?,
            ContinuationFunction(cc) => self.call_continuation(cc)?,
            Closure(closure) => {
                self.handle_function_call_closure_jit(closure, payload_size, ast_index)?
            }
            #[cfg(feature = "jit")]
            CompiledFunction(function) => self.call_compiled_function(function, payload_size)?,
            Contract(c) => self.call_contract(c, payload_size)?,
            BuiltIn(f) => self.call_builtin_func(f, payload_size)?,
            _ => {
                println!("{:?}", stack_func);
                println!("Stack: {:?}", self.stack);
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; self.current_span());
            }
        }
        Ok(())
    }

    // #[inline(always)]
    fn call_contract(&mut self, contract: &Gc<ContractType>, payload_size: usize) -> Result<()> {
        match contract.as_ref() {
            ContractType::Flat(f) => self.handle_function_call(f.predicate.clone(), payload_size),
            _ => {
                stop!(BadSyntax => "Function application not a procedure - cannot apply function contract to argument");
            }
        }
    }

    fn handle_function_call_on_stack(&mut self, payload_size: usize) -> Result<()> {
        use SteelVal::*;
        match self.stack.last().unwrap().clone() {
            BoxedFunction(f) => self.call_boxed_func_on_stack(f, payload_size)?,
            FuncV(f) => self.call_primitive_func_on_stack(f, payload_size)?,
            FutureFunc(f) => self.call_future_func_on_stack(f, payload_size)?,
            // ContractedFunction(cf) => self.call_contracted_function(&cf, payload_size)?,
            // ContinuationFunction(cc) => self.call_continuation(&cc)?,
            // Closure(closure) => self.handle_function_call_closure(&closure, payload_size)?,
            // #[cfg(feature = "jit")]
            // CompiledFunction(function) => self.call_compiled_function(&function, payload_size)?,
            // Contract(c) => self.call_contract(&c, payload_size)?,
            // BuiltIn(f) => self.call_builtin_func(&f, payload_size)?,
            _ => {
                todo!("Function application not a procedure");
                // println!("{:?}", stack_func);
                // println!("stack: {:?}", self.stack);
                // stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }

        Ok(())
    }

    // #[inline(always)]
    fn handle_function_call(&mut self, stack_func: SteelVal, payload_size: usize) -> Result<()> {
        use SteelVal::*;

        match &stack_func {
            BoxedFunction(f) => self.call_boxed_func(f, payload_size)?,
            FuncV(f) => self.call_primitive_func(f, payload_size)?,
            FutureFunc(f) => self.call_future_func(f, payload_size)?,
            ContractedFunction(cf) => self.call_contracted_function(cf, payload_size)?,
            ContinuationFunction(cc) => self.call_continuation(cc)?,
            Closure(closure) => self.handle_function_call_closure(closure, payload_size)?,
            #[cfg(feature = "jit")]
            CompiledFunction(function) => self.call_compiled_function(function, payload_size)?,
            Contract(c) => self.call_contract(c, payload_size)?,
            BuiltIn(f) => self.call_builtin_func(f, payload_size)?,
            _ => {
                println!("{:?}", stack_func);
                println!("stack: {:?}", self.stack);
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }
        Ok(())
    }

    // #[inline(always)]
    fn handle_start_def(&mut self) {
        self.ip += 1;
    }
}

pub fn current_function_span<'a, 'b>(
    ctx: &'a mut VmCore<'b>,
    args: &[SteelVal],
) -> Result<SteelVal> {
    if !args.is_empty() {
        stop!(ArityMismatch => format!("current-function-span requires no arguments, found {}", args.len()))
    }

    match ctx.enclosing_span() {
        Some(s) => Span::into_steelval(s),
        None => Ok(SteelVal::Void),
    }
}

pub fn call_cc<'a, 'b>(ctx: &'a mut VmCore<'b>, args: &[SteelVal]) -> Result<SteelVal> {
    /*
    - Construct the continuation
    - Get the function that has been passed in (off the stack)
    - Apply the function with the continuation
    - Handle continuation function call separately in the handle_func_call
    */

    // Roll back one because we advanced prior to entering the builtin

    // if std::env::var("CODE_GEN_V2").is_err() {
    ctx.ip -= 1;
    // }

    if args.len() != 1 {
        stop!(ArityMismatch => format!("call/cc expects one argument, found: {}", args.len()); ctx.current_span());
    }

    // let function = ctx.stack.pop().unwrap();

    let function = args[0].clone();

    // validate_closure_for_call_cc(&function, self.current_span())?;

    match &function {
        SteelVal::Closure(c) => {
            if c.arity() != 1 {
                stop!(Generic => "function arity in call/cc must be 1"; ctx.current_span())
            }
        }
        SteelVal::ContinuationFunction(_) => {}
        _ => {
            stop!(Generic => format!("call/cc expects a function, found: {}", function); ctx.current_span())
        }
    }

    // Ok(())

    // if std::env::var("CODE_GEN_V2").is_ok() {
    //     ctx.ip += 1;
    // }

    let continuation = ctx.construct_continuation_function();

    match function {
        SteelVal::Closure(closure) => {
            if ctx.stack_index.len() == STACK_LIMIT {
                println!("stack frame at exit: {:?}", ctx.stack);
                stop!(Generic => "call/cc: stack overflowed!"; ctx.current_span());
            }

            if closure.arity() != 1 {
                stop!(Generic => "call/cc expects a function with arity 1");
            }

            ctx.stack_index.push(ctx.stack.len());

            // Put the continuation as the argument
            // Previously we put the continuation directly on the stack ourselves, but instead we now return as an argument
            // ctx.stack.push(continuation);

            // self.global_env = inner_env;
            ctx.instruction_stack.push(InstructionPointer::new(
                ctx.ip + 1,
                Rc::clone(&ctx.instructions),
            ));
            ctx.pop_count += 1;

            ctx.instructions = closure.body_exp();
            ctx.function_stack
                .push(CallContext::new(closure).with_span(ctx.current_span()));

            ctx.ip = 0;
        }
        SteelVal::ContinuationFunction(cc) => {
            ctx.set_state_from_continuation(cc.unwrap());
            ctx.ip += 1;
            // ctx.stack.push(continuation);
        }

        _ => {
            stop!(Generic => format!("call/cc expects a function, found: {}", function));
        }
    }

    Ok(continuation)
}
