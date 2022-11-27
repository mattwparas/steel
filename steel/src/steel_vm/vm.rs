#[cfg(feature = "jit")]
use crate::jit::code_gen::JIT;
#[cfg(feature = "jit")]
use crate::jit::sig::JitFunctionPointer;
use crate::{
    compiler::constants::ConstantMap,
    core::{instructions::DenseInstruction, opcode::OpCode},
    rvals::FutureResult,
    values::contracts::ContractedFunction,
};
use crate::{
    compiler::program::Executable,
    primitives::{add_primitive, divide_primitive, multiply_primitive, subtract_primitive},
    steel_vm::primitives::{equality_primitive, lte_primitive},
    values::transducers::Transducers,
};
use crate::{compiler::program::OpCodeOccurenceProfiler, values::transducers::Reducer};
use crate::{
    core::utils::arity_check,
    values::{closed::Heap, contracts::ContractType},
};

use crate::{
    env::Env,
    gc::Gc,
    parser::span::Span,
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    stop,
    values::functions::ByteCodeLambda,
};
// use std::env::current_exe;
use std::{iter::Iterator, rc::Rc, time::Instant};

use super::evaluation_progress::EvaluationProgress;

use fnv::FnvHashMap;
use im_lists::list::List;
use log::{debug, error, log_enabled};

use crate::rvals::IntoSteelVal;

const STACK_LIMIT: usize = 1000;
const _JIT_THRESHOLD: usize = 100;

#[derive(Clone, Debug, Copy, PartialEq)]
pub struct DehydratedCallContext {
    span: Option<Span>,
}

impl DehydratedCallContext {
    pub fn span(&self) -> &Option<Span> {
        &self.span
    }
}

impl DehydratedCallContext {
    pub fn new(span: Option<Span>) -> Self {
        DehydratedCallContext { span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DehydratedStackTrace {
    stack_trace: Vec<DehydratedCallContext>,
}

impl DehydratedStackTrace {
    fn new(stack_trace: Vec<DehydratedCallContext>) -> Self {
        Self { stack_trace }
    }

    pub fn trace(&self) -> &[DehydratedCallContext] {
        &self.stack_trace
    }
}

// Eventually expand this to other kinds of continuations
#[derive(Debug, Clone, Copy)]
pub enum ContinuationMark {
    Default,
    Transducer,
}

// This should be the go to thing for handling basically everything we need
// Then - do I want to always reference the last one, or just refer to the current one?
// TODO: We'll need to add these functions to the GC as well

#[derive(Debug, Clone)]
pub struct StackFrame {
    index: usize,
    // This kind of by definition _does_ have to be a function. But for now, we'll just leave it as a
    // generic steel value
    handler: Option<SteelVal>,
    span: Option<Span>,
    // This should get added to the GC as well
    pub(crate) function: Gc<ByteCodeLambda>,
    instruction_pointer: InstructionPointer,
    // continuation_mark: ContinuationMark,
}

impl StackFrame {
    pub fn new(
        stack_index: usize,
        function: Gc<ByteCodeLambda>,
        instruction_pointer: InstructionPointer,
    ) -> Self {
        Self {
            index: stack_index,
            function,
            instruction_pointer,
            span: None,
            handler: None,
            // continuation_mark: ContinuationMark::Default,
        }
    }

    #[inline(always)]
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn set_function(&mut self, function: Gc<ByteCodeLambda>) {
        self.function = function;
    }

    pub fn set_span(&mut self, span: Span) {
        self.span = Some(span);
    }

    pub fn attach_handler(&mut self, handler: SteelVal) {
        self.handler = Some(handler);
    }

    pub fn with_handler(mut self, handler: SteelVal) -> Self {
        self.handler = Some(handler);
        self
    }
}

pub struct SteelThread {
    global_env: Env,
    callback: EvaluationProgress,
    stack: Vec<SteelVal>,
    // function_stack: CallStack,
    // stack_index: Vec<usize>,
    profiler: OpCodeOccurenceProfiler,
    // TODO: make this not as bad
    closure_interner: FnvHashMap<usize, ByteCodeLambda>,
    pure_function_interner: FnvHashMap<usize, Gc<ByteCodeLambda>>,

    heap: Heap,
    // If contracts are set to off - contract construction results in a no-op,
    // so we don't need generics on the thread
    contracts_on: bool,
    stack_frames: Vec<StackFrame>,
    // Bit of a funky way of handling things that we probably could do with delimited continuations
    // directly, but it could work
    #[cfg(feature = "jit")]
    jit: JIT,
}

impl SteelThread {
    pub fn new() -> SteelThread {
        SteelThread {
            global_env: Env::root(),
            callback: EvaluationProgress::new(),
            stack: Vec::with_capacity(256),
            // function_stack: CallStack::with_capacity(64),
            // stack_index: Vec::with_capacity(64),
            profiler: OpCodeOccurenceProfiler::new(),
            closure_interner: FnvHashMap::default(),
            pure_function_interner: FnvHashMap::default(),
            heap: Heap::new(),
            contracts_on: true,
            stack_frames: Vec::with_capacity(64),
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
            // &mut self.function_stack,
            // &mut self.stack_index,
            &mut self.stack_frames,
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

        let execution_time = Instant::now();

        let mut vm_instance = VmCore::new(
            instructions,
            &mut self.stack,
            &mut self.global_env,
            constant_map,
            &self.callback,
            // &mut self.function_stack,
            // &mut self.stack_index,
            &mut self.stack_frames,
            spans,
            &mut self.profiler,
            &mut self.closure_interner,
            &mut self.pure_function_interner,
            &mut self.heap,
            self.contracts_on,
            #[cfg(feature = "jit")]
            Some(&mut self.jit),
        )?;

        // This is our pseudo "dynamic unwind"
        // If we need to, we'll walk back on the stack and find any handlers to pop
        'outer: loop {
            // TODO: @Matt -> move the profiler out into the vm core type parameter and an argument
            // that way theres 0 cost to including a profiler vs not including a profiler

            let result = vm_instance
                .vm()
                .map_err(|error| error.with_stack_trace(vm_instance.snapshot_stack_trace()));

            // (let () (call-with-exception-handler (lambda (x) (displayln x)) (lambda () (+ 10 20 (error "oops!")))) (displayln "hi"))

            if let Err(e) = result {
                while let Some(last) = vm_instance.stack_frames.pop() {
                    // For whatever reason - if we're at the top, we shouldn't go down below 0
                    if vm_instance.pop_count == 0 {
                        return Err(e);
                    }

                    // Drop the pop count along with everything else we're doing
                    vm_instance.pop_count -= 1;

                    if let Some(handler) = last.handler.clone() {
                        // Drop the stack BACK to where it was on this level
                        vm_instance.stack.truncate(last.index);

                        vm_instance.stack.push(e.into_steelval()?);

                        // vm_instance
                        //     .stack
                        //     .push(SteelVal::StringV(Rc::from("APPLESAUCE")));

                        println!("Found handler!");

                        // println!("Stack here: {:?}", vm_instance.stack_frames);
                        // println!("pop count: {}", vm_instance.pop_count);

                        // panic!("Stopping");

                        // vm_instance.stack_frames.push(last);

                        // If we're at the top level, we need to handle this _slightly_ differently
                        // if vm_instance.stack_frames.is_empty() {
                        // Somehow update the main instruction group to _just_ be the new group
                        match handler {
                            SteelVal::Closure(closure) => {
                                if vm_instance.stack_frames.is_empty() {
                                    // Push on a dummy stack frame if we're at the top
                                    vm_instance.stack_frames.push(StackFrame::new(
                                        last.index,
                                        Gc::clone(&closure),
                                        InstructionPointer::new(0, Rc::from([])),
                                    ));
                                }

                                // vm_instance.pop_count += 1;

                                vm_instance.instructions = closure.body_exp();
                                // ctx.function_stack
                                //     .push(CallContext::new(closure).with_span(ctx.current_span()));

                                vm_instance.ip = 0;
                            }
                            _ => todo!("Unsupported"),
                        }

                        // } else {

                        // }

                        // This works, it just seems to then continue into the other function, which we don't want
                        // vm_instance
                        //     .stack
                        //     .push(SteelVal::StringV(Rc::from("APPLESAUCE")));

                        // // This is definitely wrong
                        // vm_instance.handle_function_call(handler, 1)?;

                        continue 'outer;

                        // todo!("Actually call the function handler")
                    }
                }

                return Err(e);
            } else {
                // self.profiler.report();
                // self.profiler.report_time_spend();
                // self.profiler.report_basic_blocks();

                // self.upvalue_head = vm_instance.upvalue_head;

                // Clean up
                self.stack.clear();
                // self.stack_index.clear();
                // self.function_stack.clear();

                if log_enabled!(target: "pipeline_time", log::Level::Debug) {
                    debug!(
                        target: "pipeline_time",
                        "VM Evaluation Time: {:?}",
                        execution_time.elapsed()
                    );
                };

                return result;
            }
        }
    }

    pub fn snapshot_stack_trace(&self) -> DehydratedStackTrace {
        DehydratedStackTrace::new(
            self.stack_frames
                .iter()
                .map(|x| DehydratedCallContext::new(x.span))
                .collect(),
        )
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
    // instruction_stack: Vec<InstructionPointer>,
    // stack_index: Vec<usize>,
    pub(crate) stack_frames: Vec<StackFrame>,
    ip: usize,
    pop_count: usize,
    // pub(crate) function_stack: CallStack,
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
pub type BuiltInSignature =
    for<'a, 'b> fn(&'a mut VmCore<'b>, &[SteelVal]) -> Option<Result<SteelVal>>;

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

pub struct VmCore<'a> {
    pub(crate) instructions: Rc<[DenseInstruction]>,
    pub(crate) stack: &'a mut Vec<SteelVal>,
    pub(crate) global_env: &'a mut Env,
    pub(crate) stack_frames: &'a mut Vec<StackFrame>,
    pub(crate) callback: &'a EvaluationProgress,
    pub(crate) constants: &'a ConstantMap,
    pub(crate) ip: usize,
    pub(crate) pop_count: usize,
    pub(crate) spans: &'a [Span],
    pub(crate) profiler: &'a mut OpCodeOccurenceProfiler,
    pub(crate) closure_interner: &'a mut FnvHashMap<usize, ByteCodeLambda>,
    pub(crate) pure_function_interner: &'a mut FnvHashMap<usize, Gc<ByteCodeLambda>>,
    pub(crate) heap: &'a mut Heap,
    pub(crate) use_contracts: bool,
    pub(crate) depth: usize,
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
        stack_frames: &'a mut Vec<StackFrame>,
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
            stack_frames,
            callback,
            constants,
            ip: 0,
            pop_count: 1,
            spans,
            profiler,
            closure_interner,
            pure_function_interner,
            heap,
            use_contracts,
            depth: 0,
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
        stack_frames: &'a mut Vec<StackFrame>,
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
            stack_frames,
            callback,
            constants,
            ip: 0,
            pop_count: 1,
            spans,
            profiler,
            closure_interner,
            pure_function_interner,
            heap,
            use_contracts,
            depth: 0,
            #[cfg(feature = "jit")]
            jit,
        })
    }

    // #[inline(always)]
    fn new_continuation_from_state(&self) -> Continuation {
        Continuation {
            stack: self.stack.clone(),
            instructions: Rc::clone(&self.instructions),
            stack_frames: self.stack_frames.clone(),
            ip: self.ip,
            pop_count: self.pop_count,
        }
    }

    pub fn snapshot_stack_trace(&self) -> DehydratedStackTrace {
        DehydratedStackTrace::new(
            self.stack_frames
                .iter()
                .map(|x| DehydratedCallContext::new(x.span))
                .collect(),
        )
    }

    // #[inline(always)]
    fn set_state_from_continuation(&mut self, continuation: Continuation) {
        *self.stack = continuation.stack;
        self.instructions = continuation.instructions;
        self.ip = continuation.ip;
        self.pop_count = continuation.pop_count;
        *self.stack_frames = continuation.stack_frames;
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
        // Force the execution to be done earlier
        self.pop_count = 1;

        self.depth += 1;

        let res = self.vm();

        self.depth -= 1;

        self.ip = old_ip;
        self.instructions = old_instructions;
        self.pop_count = old_pop_count;

        // self.stack_frames.pop();

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
            // SteelVal::BuiltIn(func) => {
            //     let arg_vec = [arg];
            //     func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            // }
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
            // SteelVal::BuiltIn(func) => {
            //     let arg_vec = [arg1, arg2];
            //     func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            // }
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
            // SteelVal::BuiltIn(func) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            // }
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
        // println!("Arity: {:?}", closure.arity());
        // println!("Multi arity: {:?}", closure.is_multi_arity);

        let prev_length = self.stack.len();
        // self.stack_index.push(prev_length);

        // if self.stack_frames

        // TODO:
        self.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            InstructionPointer::new(0, Rc::from([])),
        ));

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

        // self.function_stack
        // .push(CallContext::new(Gc::clone(closure)));
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
        // self.stack_index.push(prev_length);

        self.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            InstructionPointer::new(0, Rc::from([])),
        ));

        self.stack.push(arg1);
        self.stack.push(arg2);
        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)));

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    // Calling convention
    pub(crate) fn call_with_one_arg(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        arg: SteelVal,
    ) -> Result<SteelVal> {
        let prev_length = self.stack.len();

        self.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            InstructionPointer::new(0, Rc::from([])),
        ));

        // println!("PUSHING NEW STACK INDEX ON");

        // self.stack_index.push(prev_length);
        self.stack.push(arg);
        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)));

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    pub(crate) fn vm(&mut self) -> Result<SteelVal> {
        // let mut cur_inst;

        if self.depth > 64 {
            stop!(Generic => "stack overflow! The rust call stack is currently used for transducers, so we impose a hard recursion limit of 64"; self.current_span());
        }

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
                let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
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

        // let mut frame = self.stack_frames.last().unwrap();

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
                    // let offset = frame.index;
                    let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
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
                    op_code: OpCode::GIMMICK,
                    payload_size,
                    ..
                } => {
                    // Handle the local
                    self.handle_local(payload_size as usize)?;

                    // Load the int
                    self.stack.push(SteelVal::INT_TWO);
                    self.ip += 1;

                    let payload = self.instructions[self.ip].payload_size;

                    inline_primitive!(lte_primitive, payload);

                    let payload_size = self.instructions[self.ip].payload_size;

                    // Handle the if
                    if self.stack.pop().unwrap().is_truthy() {
                        self.ip += 1;
                    } else {
                        self.ip = payload_size as usize;
                    }
                }

                DenseInstruction {
                    op_code: OpCode::VOID,
                    ..
                } => {
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
                    let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
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
                    let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
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
                    // change to truthy...
                    if self.stack.pop().unwrap().is_truthy() {
                        self.ip += 1;
                    } else {
                        self.ip = payload_size as usize;
                    }
                }
                DenseInstruction {
                    op_code: OpCode::TCOJMP,
                    payload_size,
                    ..
                } => {
                    // println!("At tco jump");

                    let current_arity = payload_size as usize;
                    // This is the number of (local) functions we need to pop to get back to the place we want to be at
                    let depth = self.instructions[self.ip + 1].payload_size as usize;

                    // println!("Depth: {:?}", depth);
                    // println!("Function stack length: {:?}", self.function_stack.len());
                    // println!("Stack index: {:?}", self.stack_index);
                    // println!(
                    //     "Instruction stack length: {:?}",
                    //     self.instruction_stack.len()
                    // );

                    // for function in function_stack {

                    // }

                    for _ in 0..depth {
                        // println!("Popping");
                        // self.function_stack.pop();
                        // self.stack_index.pop();
                        self.stack_frames.pop();
                        // self.instruction_stack.pop();
                        self.pop_count -= 1;
                    }

                    let last_stack_frame = self.stack_frames.last().unwrap();

                    self.instructions = last_stack_frame.function.body_exp();

                    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

                    // panic!("Stopping");

                    self.ip = 0;

                    let closure_arity = last_stack_frame.function.arity();

                    if current_arity != closure_arity {
                        stop!(ArityMismatch => format!("tco: function expected {} arguments, found {}", closure_arity, current_arity); self.current_span());
                    }

                    // HACK COME BACK TO THIS
                    // if self.ip == 0 && self.heap.len() > self.heap.limit() {
                    // TODO collect here
                    // self.heap.collect_garbage();
                    // }
                    // let offset = self.stack_index.last().copied().unwrap_or(0);
                    let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

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
                        .stack_frames
                        .last()
                        .unwrap()
                        .function
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
                        .stack_frames
                        .last()
                        .unwrap()
                        .function
                        .heap_allocated()
                        .borrow()[payload_size as usize]
                        .get();

                    // println!(
                    //     "Reading heap allocated at index: {} {}",
                    //     payload_size, value
                    // );

                    self.stack.push(value);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::ALLOC,
                    payload_size,
                    ..
                } => {
                    let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

                    let allocated_var = self.heap.allocate(
                        self.stack[offset].clone(), // TODO: Could actually move off of the stack entirely
                        self.stack.iter(),
                        self.stack_frames.iter().map(|x| &x.function),
                        self.global_env.roots(),
                    );

                    self.stack_frames
                        .last_mut()
                        .unwrap()
                        .function
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
                    let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

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
                // DenseInstruction {
                //     op_code: OpCode::POP,
                //     payload_size,
                //     ..
                // } => {
                //     if let Some(r) = self.handle_pop(payload_size) {
                //         return r;
                //     }
                // }
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
                    op_code: OpCode::BIND,
                    payload_size,
                    ..
                } => self.handle_bind(payload_size as usize),
                // DenseInstruction {
                //     op_code: OpCode::SCLOSURE,
                //     payload_size,
                //     ..
                // } => self.handle_start_closure(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::NEWSCLOSURE,
                    payload_size,
                    span_index,
                    ..
                } => self.handle_new_start_closure(payload_size as usize, span_index)?,
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
        self.stack_frames.last().and_then(|x| x.span)
    }

    fn handle_pop_pure(&mut self, _payload: u32) -> Option<Result<SteelVal>> {
        // Check that the amount we're looking to pop and the function stack length are equivalent
        // otherwise we have a problem

        // if self.pop_count - 1 != self.stack_frames.len() {
        //     println!("{:?}", self.stack);
        //     println!("Pop count: {}", self.pop_count);
        //     println!("{:?}", self.stack_frames.len());

        //     panic!("Pop count and stack frames aren't aligned");
        // }

        // assert_eq!(self.pop_count - 1, self.stack_frames.len());

        // println!(
        //     "Pop count: {}, stack frames: {}",
        //     self.pop_count,
        //     self.stack_frames.len()
        // );

        self.pop_count -= 1;

        // unwrap just because we want to see if we have something here
        // rolling back the function stack
        // self.function_stack.pop();

        let last = self.stack_frames.pop();

        // let should_return = self.stack_frames.is_empty();
        let should_return = self.pop_count == 0;

        if should_return {
            let ret_val = self.stack.pop().ok_or_else(|| {
                SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
                    .with_span(self.current_span())
            });

            let rollback_index = last.map(|x| x.index).unwrap_or(0);

            // Move forward past the pop
            self.ip += 1;

            self.stack.truncate(rollback_index);

            Some(ret_val)
        } else {
            let last = last.unwrap();
            let ret_val = self.stack.pop().unwrap();

            let rollback_index = last.index;

            // Snatch the value to close from the payload size
            // Move forward past the pop
            self.ip += 1;

            // Close remaining values on the stack

            self.stack.truncate(rollback_index);
            self.stack.push(ret_val);

            let prev_state = last.instruction_pointer;
            self.ip = prev_state.0;
            self.instructions = prev_state.instrs();

            None
        }
    }

    // #[inline(always)]
    fn handle_panic(&mut self, span: Span) -> Result<()> {
        let error_message = self.stack.pop().unwrap();
        stop!(Generic => error_message.to_string(); span);
    }

    // #[inline(always)]
    fn handle_set(&mut self, index: usize) -> Result<()> {
        let value_to_assign = self.stack.pop().unwrap();

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
        let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
        let value = self.stack[index + offset].clone();
        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    fn handle_read_captures(&mut self, index: usize) -> Result<()> {
        let value = self.stack_frames.last().unwrap().function.captures()[index].clone();

        self.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn handle_move_local(&mut self, index: usize) -> Result<()> {
        let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
        let value = std::mem::replace(&mut self.stack[index + offset], SteelVal::Void);

        self.stack.push(value);
        self.ip += 1;
        Ok(())
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

    fn handle_new_start_closure(&mut self, offset: usize, span: usize) -> Result<()> {
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
        // TODO: We probably can get a bit better than this.
        // A lot of the captures are static, I'm not quite sure we necessarily need to patch down _every_ single one
        // each time, especially since each lambda is a standalone instance of this.

        let guard = self.stack_frames.last().unwrap();
        // let stack_index = self.stack_index.last().copied().unwrap_or(0);
        let stack_index = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

        for _ in 0..ndefs {
            let instr = self.instructions[self.ip];
            match (instr.op_code, instr.payload_size) {
                (OpCode::COPYCAPTURESTACK, n) => {
                    let offset = stack_index;
                    let value = self.stack[n as usize + offset].clone();
                    captures.push(value);
                }
                (OpCode::COPYCAPTURECLOSURE, n) => {
                    debug_assert!(
                        !self.stack_frames.is_empty(),
                        "Trying to capture from closure that doesn't exist",
                    );

                    debug_assert!((n as usize) < guard.function.captures().len());

                    let value = guard.function.captures()[n as usize].clone();

                    captures.push(value);
                }
                // TODO: Try adding these to the cache. i.e. -> if we're doing a copyheapcaptureclosure, we actually just don't
                // need to do this step at all each time.
                // Looks like all COPYHEAPCAPTURECLOSURE(s) happen at the start. So we should be able to store those
                // Directly
                (OpCode::COPYHEAPCAPTURECLOSURE, n) => {
                    heap_vars.push(guard.function.heap_allocated().borrow()[n as usize].clone());
                }
                (OpCode::FIRSTCOPYHEAPCAPTURECLOSURE, n) => {
                    heap_vars.push(guard.function.heap_allocated().borrow()[n as usize].clone());
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

        // TODO: Consider moving these captures into the interned closure directly
        // Its possible we're just doing a lot of extra capturing that way if we repeatedly copy things
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
        Ok(())
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
        let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

        let old_index = index + offset;
        let old_value = self.stack[old_index].clone();

        // Modify the stack and change the value to the new one
        self.stack[old_index] = value_to_set;

        self.stack.push(old_value);
        self.ip += 1;
    }

    fn new_handle_tail_call_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        // Something like:
        // let last_func = self.function_stack.last_mut().unwrap();
        // last_func.set_function(Gc::clone(&closure));
        // last_func.set_span(self.current_span());
        // let offset = last_func.index;

        // let last = self.stack_frames.pop().unwrap();

        // self.pop_count -= 1;
        // let offset = self.stack_index.pop().unwrap();
        // self.instruction_stack.pop();

        let current_span = self.current_span();

        let last = self.stack_frames.last_mut().unwrap();

        last.set_function(Gc::clone(closure));
        last.set_span(current_span);

        let offset = last.index;

        // self.stack_frames.last_mut().unwrap().set_function(function)

        // TODO
        // self.function_stack
        // .push(CallContext::new(Gc::clone(&closure)).with_span(self.current_span()));

        let mut new_arity = payload_size;

        if closure.is_multi_arity {
            // println!(
            //     "multi closure function, multi arity, arity: {:?}",
            //     closure.arity()
            // );

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

            // println!("Stack after list conversion: {:?}", self.stack);
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
                // println!("{:?}", self.stack);
                // println!("{:?}", self.stack_index);
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
        let result = func(self, &args).map(|x| {
            x.map_err(|x| {
                // TODO: @Matt 4/24/2022 -> combine this into one function probably
                if x.has_span() {
                    x
                } else {
                    x.set_span_if_none(self.current_span())
                }
                // x.set_span_if_none(self.current_span())
            })
        });

        if let Some(result) = result {
            self.stack.push(result?);
        }

        Ok(())
        // self.stack.push(result);
        // Ok(())
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
    // TODO: See if calling continuations can be implemented in terms of the core ABI
    // That way, we dont need a special "continuation" function
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

        // Push new stack frame
        self.stack_frames.push(
            StackFrame::new(
                self.stack.len() - 2,
                Gc::clone(closure),
                InstructionPointer::new(self.ip + 4, Rc::clone(&self.instructions)),
            )
            .with_span(self.current_span()),
        );

        // Push on the function stack so we have access to it later
        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)).with_span(self.current_span()));

        if closure.is_multi_arity {
            panic!("Calling lazy closure with multi arity");
        }

        if closure.arity() != 2 {
            stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), 2); self.current_span());
        }

        // self.current_arity = Some(closure.arity());

        if self.stack_frames.len() == STACK_LIMIT {
            crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
            println!("lazy - stack frame at exit: {:?}", self.stack);
            stop!(Generic => "lazy closure: stack overflowed!"; self.current_span());
        }

        // self.stack_index.push(self.stack.len() - 2);

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        // self.instruction_stack.push(InstructionPointer::new(
        //     self.ip + 4,
        //     Rc::clone(&self.instructions),
        // ));
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
            // BuiltIn(func) => {
            //     let args = [local, const_value];
            //     let result =
            //         func(self, &args).map_err(|x| x.set_span_if_none(self.current_span()))?;
            //     self.stack.push(result);
            //     self.ip += 4;
            // }
            _ => {
                println!("{:?}", stack_func);
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported, {}", stack_func); self.current_span());
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
        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)).with_span(self.current_span()));

        // if closure.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); *span);
        // }

        if closure.is_multi_arity {
            // println!("Arity: {}", closure.arity());

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

        self.stack_frames.push(
            StackFrame::new(
                self.stack.len() - closure.arity(),
                Gc::clone(closure),
                InstructionPointer::new(self.ip + 1, Rc::clone(&self.instructions)),
            )
            .with_span(self.current_span()),
        );

        // self.current_arity = Some(closure.arity());

        if self.stack_frames.len() == STACK_LIMIT {
            crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
            println!("function call - stack frame at exit: {:?}", self.stack);
            stop!(Generic => "function call closure: stack overflowed!"; self.current_span());
        }

        // self.stack_index.push(self.stack.len() - closure.arity());

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        // self.instruction_stack.push(InstructionPointer::new(
        //     self.ip + 1,
        //     Rc::clone(&self.instructions),
        // ));
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
        _ast_index: usize,
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

        // #[cfg(feature = "jit")]
        // {
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
        // }

        // Push on the function stack so we have access to it later

        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)).with_span(self.current_span()));

        // println!("Calling function");
        // println!("Multi arity: {}", closure.is_multi_arity);
        // crate::core::instructions::pretty_print_dense_instructions(&closure.body_exp);

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

        // Do this _after_ the multi arity business
        self.stack_frames.push(
            StackFrame::new(
                self.stack.len() - closure.arity(),
                Gc::clone(closure),
                InstructionPointer::new(self.ip + 1, Rc::clone(&self.instructions)),
            )
            .with_span(self.current_span()),
        );

        // self.current_arity = Some(closure.arity());

        if self.stack_frames.len() == STACK_LIMIT {
            crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
            println!("stack frame at exit: {:?}", self.stack);
            stop!(Generic => "function call closure jit: stack overflowed!"; self.current_span());
        }

        // closure arity here is the number of true arguments
        // self.stack_index.push(self.stack.len() - closure.arity());

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        // std::mem::replace(x, y)

        // self.instruction_stack.push(InstructionPointer::new(
        //     self.ip + 1,
        //     Rc::clone(&self.instructions),
        // ));
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
            BoxedFunction(f) => self.call_boxed_func_on_stack(*f, payload_size)?,
            FuncV(f) => self.call_primitive_func_on_stack(f, payload_size)?,
            FutureFunc(f) => self.call_future_func_on_stack(*f, payload_size)?,
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
) -> Option<Result<SteelVal>> {
    if !args.is_empty() {
        builtin_stop!(ArityMismatch => format!("current-function-span requires no arguments, found {}", args.len()))
    }

    match ctx.enclosing_span() {
        Some(s) => Some(Span::into_steelval(s)),
        None => Some(Ok(SteelVal::Void)),
    }
}

pub fn call_with_exception_handler<'a, 'b>(
    ctx: &'a mut VmCore<'b>,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    if args.len() != 2 {
        builtin_stop!(ArityMismatch => format!("with-handler expects two arguments, found: {}", args.len()); ctx.current_span());
    }

    let handler = args[0].clone();
    let thunk = args[1].clone();

    // let guard = ctx.stack_frames.last_mut().unwrap();
    // guard.attach_handler(handler);

    match thunk {
        SteelVal::Closure(closure) => {
            if ctx.stack_frames.len() == STACK_LIMIT {
                println!("stack frame at exit: {:?}", ctx.stack);
                builtin_stop!(Generic => "call/cc: stack overflowed!"; ctx.current_span());
            }

            if closure.arity() != 0 {
                builtin_stop!(Generic => "call-with-exception-handler expects a thunk with arity 0");
            }

            // Roll back one level
            ctx.ip -= 1;

            // Push the previous state on
            ctx.stack_frames.push(
                StackFrame::new(
                    ctx.stack.len(),
                    Gc::clone(&closure),
                    InstructionPointer::new(ctx.ip + 1, Rc::clone(&ctx.instructions)),
                )
                .with_span(ctx.current_span())
                .with_handler(handler),
            );

            // ctx.stack_index.push(ctx.stack.len());

            // Put the continuation as the argument
            // Previously we put the continuation directly on the stack ourselves, but instead we now return as an argument
            // ctx.stack.push(continuation);

            // self.global_env = inner_env;
            // ctx.instruction_stack.push(InstructionPointer::new(
            //     ctx.ip + 1,
            //     Rc::clone(&ctx.instructions),
            // ));
            ctx.pop_count += 1;

            ctx.instructions = closure.body_exp();
            // ctx.function_stack
            //     .push(CallContext::new(closure).with_span(ctx.current_span()));

            ctx.ip = 0;
        }

        _ => {
            builtin_stop!(TypeMismatch => format!("call-with-exception-handler expects a thunk as an argument, found: {}", thunk); ctx.current_span())
        }
    }

    Some(Ok(SteelVal::Void))
}

pub fn call_cc<'a, 'b>(ctx: &'a mut VmCore<'b>, args: &[SteelVal]) -> Option<Result<SteelVal>> {
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
        builtin_stop!(ArityMismatch => format!("call/cc expects one argument, found: {}", args.len()); ctx.current_span());
    }

    // let function = ctx.stack.pop().unwrap();

    let function = args[0].clone();

    // validate_closure_for_call_cc(&function, self.current_span())?;

    match &function {
        SteelVal::Closure(c) => {
            if c.arity() != 1 {
                builtin_stop!(Generic => "function arity in call/cc must be 1"; ctx.current_span())
            }
        }
        SteelVal::ContinuationFunction(_) => {}
        _ => {
            builtin_stop!(Generic => format!("call/cc expects a function, found: {}", function); ctx.current_span())
        }
    }

    // Ok(())

    // if std::env::var("CODE_GEN_V2").is_ok() {
    //     ctx.ip += 1;
    // }

    let continuation = ctx.construct_continuation_function();

    match function {
        SteelVal::Closure(closure) => {
            if ctx.stack_frames.len() == STACK_LIMIT {
                println!("stack frame at exit: {:?}", ctx.stack);
                builtin_stop!(Generic => "call/cc: stack overflowed!"; ctx.current_span());
            }

            if closure.arity() != 1 {
                builtin_stop!(Generic => "call/cc expects a function with arity 1");
            }

            ctx.stack_frames.push(
                StackFrame::new(
                    ctx.stack.len(),
                    Gc::clone(&closure),
                    InstructionPointer::new(ctx.ip + 1, Rc::clone(&ctx.instructions)),
                )
                .with_span(ctx.current_span()),
            );

            // ctx.stack_index.push(ctx.stack.len());

            // Put the continuation as the argument
            // Previously we put the continuation directly on the stack ourselves, but instead we now return as an argument
            // ctx.stack.push(continuation);

            // self.global_env = inner_env;
            // ctx.instruction_stack.push(InstructionPointer::new(
            //     ctx.ip + 1,
            //     Rc::clone(&ctx.instructions),
            // ));
            ctx.pop_count += 1;

            ctx.instructions = closure.body_exp();
            // ctx.function_stack
            //     .push(CallContext::new(closure).with_span(ctx.current_span()));

            ctx.ip = 0;
        }
        SteelVal::ContinuationFunction(cc) => {
            ctx.set_state_from_continuation(cc.unwrap());
            ctx.ip += 1;
            // ctx.stack.push(continuation);
        }

        _ => {
            builtin_stop!(Generic => format!("call/cc expects a function, found: {}", function));
        }
    }

    Some(Ok(continuation))
}

// TODO: This apply does not respect tail position
// Something like this: (define (loop) (apply loop '()))
// _should_ result in an infinite loop. In the current form, this is a Rust stack overflow.
// Similarly, care should be taken to check out transduce, because nested calls to that will
// result in a stack overflow with sufficient depth on the recursive calls
pub(crate) fn apply<'a, 'b>(
    ctx: &'a mut VmCore<'b>,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    // arity_check!(apply, args, 2);

    ctx.ip -= 1;

    if args.len() != 2 {
        builtin_stop!(ArityMismatch => "apply expected 2 arguments");
    }

    let mut arg_iter = args.iter();
    let arg1 = arg_iter.next().unwrap();
    let arg2 = arg_iter.next().unwrap();

    if let SteelVal::ListV(l) = arg2 {
        if arg1.is_function() {
            // println!("Calling apply with args: {:?}, {:?}", arg1, arg2);
            // ctx.call_function_many_args(&arg1, l.clone())

            match arg1 {
                SteelVal::Closure(closure) => {
                    for arg in l {
                        // println!("Arg: {:?}", arg);
                        ctx.stack.push(arg.clone());
                    }

                    // TODO: Fix this unwrap
                    ctx.handle_function_call_closure(closure, l.len()).unwrap();

                    None
                }
                SteelVal::ContinuationFunction(cc) => {
                    ctx.set_state_from_continuation(cc.unwrap());
                    ctx.ip += 1;

                    None
                    // ctx.stack.push(continuation);
                }
                SteelVal::FuncV(f) => {
                    let args = l.into_iter().cloned().collect::<Vec<_>>();

                    let result = f(&args).map_err(|e| e.set_span_if_none(ctx.current_span()));

                    Some(result)
                }

                _ => {
                    builtin_stop!(Generic => format!("apply expects a function, found: {}", arg1));
                }
            }
        } else {
            builtin_stop!(TypeMismatch => "apply expected a function, found: {}", arg1);
        }
    } else {
        builtin_stop!(TypeMismatch => "apply expects a list, found: {}", arg2);
    }
}
