#![allow(unused)]

use crate::core::instructions::pretty_print_dense_instructions;
use crate::primitives::lists::cons;
use crate::primitives::lists::new as new_list;
use crate::primitives::nums::special_add;
use crate::values::closed::Heap;
use crate::values::functions::SerializedLambda;
use crate::values::structs::UserDefinedStruct;
use crate::{
    compiler::constants::ConstantMap,
    core::{instructions::DenseInstruction, opcode::OpCode},
    rvals::FutureResult,
    // values::contracts::ContractedFunction,
};
use crate::{
    compiler::program::Executable,
    primitives::{add_primitive, divide_primitive, multiply_primitive, subtract_primitive},
    steel_vm::primitives::{equality_primitive, lte_primitive},
    values::transducers::Transducers,
};
use crate::{primitives::nums::add_primitive_faster, values::transducers::Reducer};

use crate::{
    env::Env,
    gc::Gc,
    parser::span::Span,
    rerrs::{ErrorKind, SteelErr},
    rvals::{Result, SteelVal},
    stop,
    values::functions::ByteCodeLambda,
};
use std::{cell::RefCell, collections::HashMap, iter::Iterator, rc::Rc};

use super::builtin::DocTemplate;

use crate::values::lists::List;

#[cfg(feature = "profiling")]
use log::{debug, log_enabled};
use num::ToPrimitive;
use once_cell::sync::Lazy;
use slotmap::DefaultKey;
#[cfg(feature = "profiling")]
use std::time::Instant;

use crate::rvals::{
    as_underlying_type, from_serializable_value, into_serializable_value, IntoSteelVal,
};

pub(crate) mod threads;
pub(crate) use threads::{spawn_thread, thread_join};

#[inline]
#[cold]
pub fn cold() {}

#[inline]
pub fn likely(b: bool) -> bool {
    if !b {
        cold()
    }
    b
}

#[inline]
pub fn unlikely(b: bool) -> bool {
    if b {
        cold()
    }
    b
}

// #[test]
// fn call_hello_world() {
//     println!("{:?}", message());
// }

const STACK_LIMIT: usize = 1000000;
const _JIT_THRESHOLD: usize = 100;

const USE_SUPER_INSTRUCTIONS: bool = false;
const CHECK_STACK_OVERFLOW: bool = false;

#[repr(C)]
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

#[repr(C)]
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

    pub fn push(&mut self, context: DehydratedCallContext) {
        self.stack_trace.push(context)
    }
}

// // Eventually expand this to other kinds of continuations
// #[derive(Debug, Clone, Copy)]
// pub enum ContinuationMark {
//     Default,
//     Transducer,
// }

// This should be the go to thing for handling basically everything we need
// Then - do I want to always reference the last one, or just refer to the current one?
// TODO: We'll need to add these functions to the GC as well

#[derive(Debug, Clone)]
pub struct StackFrame {
    sp: usize,
    // This _has_ to be a function
    handler: Option<DefaultKey>,
    // This should get added to the GC as well
    #[cfg(not(feature = "unsafe-internals"))]
    pub(crate) function: Gc<ByteCodeLambda>,
    // Whenever the StackFrame object leaves the context of _this_ VM, these functions
    // need to become rooted, otherwise we'll have an issue with use after free
    #[cfg(feature = "unsafe-internals")]
    pub(crate) function: crate::gc::unsafe_roots::MaybeRooted<ByteCodeLambda>,
    ip: usize,
    instructions: Rc<[DenseInstruction]>,
    // spans: Rc<[Span]>, // span_id: usize,
}

#[test]
fn check_sizes() {
    println!("{:?}", std::mem::size_of::<Option<SteelVal>>());
    println!("{:?}", std::mem::size_of::<StackFrame>());
    println!("{:?}", std::mem::size_of::<Rc<[DenseInstruction]>>());
    // println!("{:?}", std::mem::size_of::<Rc<[Span]>>());
    println!("{:?}", std::mem::size_of::<Gc<ByteCodeLambda>>());
    // println!("{:?}", std::mem::size_of::<Option<slotmap::DefaultKey>>());
}

impl StackFrame {
    pub fn new(
        stack_index: usize,
        function: Gc<ByteCodeLambda>,
        ip: usize,
        instructions: Rc<[DenseInstruction]>,
        // span_id: usize,
        // spans: Rc<[Span]>,
    ) -> Self {
        Self {
            sp: stack_index,
            #[cfg(feature = "unsafe-internals")]
            function: crate::gc::unsafe_roots::MaybeRooted::Reference(function),
            #[cfg(not(feature = "unsafe-internals"))]
            function,
            ip,
            instructions,
            // span: None,
            handler: None,
            // spans,
            // span_id,
        }
    }

    fn new_rooted(
        stack_index: usize,
        #[cfg(feature = "unsafe-internals")] function: crate::gc::unsafe_roots::MaybeRooted<
            ByteCodeLambda,
        >,
        #[cfg(not(feature = "unsafe-internals"))] function: Gc<ByteCodeLambda>,
        ip: usize,
        instructions: Rc<[DenseInstruction]>,
        // span_id: usize,
        // spans: Rc<[Span]>,
    ) -> Self {
        Self {
            sp: stack_index,
            #[cfg(feature = "unsafe-internals")]
            function,
            #[cfg(not(feature = "unsafe-internals"))]
            function,
            ip,
            instructions,
            // span: None,
            handler: None,
            // spans,
            // span_id,
        }
    }

    pub fn main() -> Self {
        let function = Gc::new(ByteCodeLambda::main(Vec::new()));
        StackFrame::new(0, function, 0, Rc::from([]))
    }

    // #[inline(always)]
    // pub fn with_span(mut self, span: Span) -> Self {
    //     self.span = Some(span);
    //     self
    // }

    #[inline(always)]
    pub fn set_function(&mut self, function: Gc<ByteCodeLambda>) {
        #[cfg(not(feature = "unsafe-internals"))]
        {
            self.function = function;
        }

        #[cfg(feature = "unsafe-internals")]
        {
            self.function = crate::gc::unsafe_roots::MaybeRooted::Reference(function);
        }
    }

    // pub fn set_span(&mut self, span: Span) {
    //     self.span = Some(span);
    // }

    // pub fn attach_handler(&mut self, handler: SteelVal) {
    //     self.handler = Some(handler);
    // }

    pub fn with_handler(mut self, handler: DefaultKey) -> Self {
        self.handler = Some(handler);
        self
    }
}

thread_local! {
    pub(crate) static DEFAULT_CONSTANT_MAP: ConstantMap = ConstantMap::new();
}

// Drain and move across the thread boundary, OR, enforce the restriction that only pure functions
// can move into a new thread... that might be the easiest way?
#[derive(Clone)]
pub struct SteelThread {
    pub(crate) global_env: Env,
    pub(crate) stack: Vec<SteelVal>,
    profiler: OpCodeOccurenceProfiler,
    function_interner: FunctionInterner,
    super_instructions: Vec<Rc<DynamicBlock>>,

    pub(crate) heap: Heap,
    // If contracts are set to off - contract construction results in a no-op,
    // so we don't need generics on the thread
    pub(crate) runtime_options: RunTimeOptions,
    pub(crate) current_frame: StackFrame,
    pub(crate) stack_frames: Vec<StackFrame>,
    pub(crate) constant_map: ConstantMap,
}

#[derive(Clone)]
pub(crate) struct RunTimeOptions {
    pub(crate) contracts_on: bool,
    pub(crate) test: bool,
}

impl RunTimeOptions {
    pub fn new() -> Self {
        Self {
            contracts_on: true,
            test: false,
        }
    }
}

struct InstructionChunk {
    start: usize,
    end: usize,
    id: usize,
}

#[derive(PartialEq)]
struct SpanId(usize);

#[derive(Default, Clone)]
pub struct FunctionInterner {
    closure_interner: fxhash::FxHashMap<usize, ByteCodeLambda>,
    pure_function_interner: fxhash::FxHashMap<usize, Gc<ByteCodeLambda>>,
    // Functions will store a reference to a slot here, rather than any other way
    // getting the span can be super late bound then, and we don't need to worry about
    // cache misses nearly as much
    // Rooted spans more or less are just spans from the top level getting passed in - we'll
    // take them, root them with some sort of gensym, and then free them on the way out.
    //
    // These should get GC'd eventually, by walking the alive set and seeing if there are
    // actually any references to this still in existence. Functions should probably hold a direct
    // reference to the existing thread in which it was created, and if passed in externally by
    // another run time, we can nuke it?
    spans: fxhash::FxHashMap<usize, Rc<[Span]>>,
    // Keep these around - each thread keeps track of the instructions on the bytecode object, but we shouldn't
    // need to dereference that until later? When we actually move to that
    instructions: fxhash::FxHashMap<usize, Rc<[DenseInstruction]>>,

    handlers: Rc<RefCell<slotmap::SlotMap<DefaultKey, SteelVal>>>,
}

impl SteelThread {
    pub fn new() -> SteelThread {
        SteelThread {
            global_env: Env::root(),
            stack: Vec::with_capacity(128),
            profiler: OpCodeOccurenceProfiler::new(),
            function_interner: FunctionInterner::default(),
            super_instructions: Vec::new(),

            heap: Heap::new(),
            runtime_options: RunTimeOptions::new(),
            stack_frames: Vec::with_capacity(128),
            current_frame: StackFrame::main(),
            // Should probably just have this be Option<ConstantMap> - but then every time we look up
            // something we'll have to deal with the fact that its wrapped in an option. Another options
            // Would just have all programs compiled in this thread just share a constant map. For now,
            // we'll have each thread default to an empty constant map, and replace it with the map bundled
            // with the executables
            constant_map: DEFAULT_CONSTANT_MAP.with(|x| x.clone()),
        }
    }

    // If you want to explicitly turn off contracts, you can do so
    pub fn with_contracts(&mut self, contracts: bool) -> &mut Self {
        self.runtime_options.contracts_on = contracts;
        self
    }

    pub fn insert_binding(&mut self, idx: usize, value: SteelVal) {
        self.global_env.add_root_value(idx, value);
    }

    pub fn extract_value(&self, idx: usize) -> Option<SteelVal> {
        self.global_env.extract(idx)
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

        self.constant_map = constant_map.clone();

        let result = instructions
            .iter()
            .zip(spans.iter())
            .map(|x| self.execute(Rc::clone(x.0), constant_map.clone(), Rc::clone(x.1)))
            .collect();

        self.constant_map = DEFAULT_CONSTANT_MAP.with(|x| x.clone());

        result

        // TODO
        // self.global_env.print_diagnostics();

        // todo!("Initialize structs and build the program");
    }

    pub(crate) fn call_function(
        &mut self,
        constant_map: ConstantMap,
        function: SteelVal,
        args: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        match function {
            SteelVal::FuncV(func) => {
                let arg_vec: Vec<_> = args.into_iter().collect();
                func(&arg_vec).map_err(|x| x.set_span_if_none(Span::default()))
            }
            SteelVal::BoxedFunction(func) => {
                let arg_vec: Vec<_> = args.into_iter().collect();
                func.func()(&arg_vec).map_err(|x| x.set_span_if_none(Span::default()))
            }
            // SteelVal::ContractedFunction(cf) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     cf.apply(arg_vec, cur_inst_span, self)
            // }
            SteelVal::MutFunc(func) => {
                let mut arg_vec: Vec<_> = args.into_iter().collect();
                func(&mut arg_vec).map_err(|x| x.set_span_if_none(Span::default()))
            }
            // SteelVal::BuiltIn(func) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            // }
            SteelVal::Closure(closure) => {
                // let prev_length = self.stack.len();

                // let frame = StackFrame::new(prev_length, Gc::clone(&closure), 0, Rc::from([]));

                // Create phony span vec
                let spans = closure.body_exp().iter().map(|_| Span::default()).collect();

                let mut vm_instance = VmCore::new_unchecked(
                    Rc::new([]),
                    constant_map,
                    // &mut self.function_stack,
                    // &mut self.stack_index,
                    // 0,
                    Rc::clone(&spans),
                    self,
                    &spans,
                );

                // vm_instance.call_func_or_else_many_args(
                //     &function,
                //     args,
                //     &Span::default(),
                //     throw!(TypeMismatch => format!("application not a procedure: {}", function)),
                // );

                vm_instance.call_with_args(&closure, args)
            }
            _ => {
                stop!(TypeMismatch => format!("application not a procedure: {function}"))
            }
        }
    }

    pub fn execute(
        &mut self,
        instructions: Rc<[DenseInstruction]>,
        constant_map: ConstantMap,
        spans: Rc<[Span]>,
    ) -> Result<SteelVal> {
        self.profiler.reset();

        #[cfg(feature = "profiling")]
        let execution_time = Instant::now();

        let handler_ref = Rc::clone(&self.function_interner.handlers);

        let mut vm_instance =
            VmCore::new(instructions, constant_map, Rc::clone(&spans), self, &spans)?;

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
                while let Some(mut last) = vm_instance.thread.stack_frames.pop() {
                    // For whatever reason - if we're at the top, we shouldn't go down below 0
                    if vm_instance.pop_count == 0 {
                        return Err(e);
                    }

                    // Drop the pop count along with everything else we're doing
                    vm_instance.pop_count -= 1;

                    if let Some(handler) = last.handler {
                        // Drop the stack BACK to where it was on this level
                        vm_instance.thread.stack.truncate(last.sp);

                        vm_instance.thread.stack.push(e.into_steelval()?);

                        let handler = &handler_ref.borrow()[handler];

                        // If we're at the top level, we need to handle this _slightly_ differently
                        // if vm_instance.stack_frames.is_empty() {
                        // Somehow update the main instruction group to _just_ be the new group
                        match handler {
                            SteelVal::Closure(closure) => {
                                if vm_instance.thread.stack_frames.is_empty() {
                                    vm_instance.sp = last.sp;

                                    // Push on a dummy stack frame if we're at the top
                                    vm_instance.thread.stack_frames.push(StackFrame::new(
                                        last.sp,
                                        Gc::clone(&closure),
                                        0,
                                        Rc::from([]),
                                        // Rc::from([]),
                                        // 0,
                                    ));
                                }

                                vm_instance.sp = last.sp;
                                vm_instance.instructions = closure.body_exp();
                                // vm_instance.spans = closure.spans();

                                last.handler = None;

                                #[cfg(not(feature = "unsafe-internals"))]
                                {
                                    last.function = closure.clone();
                                }

                                vm_instance.ip = 0;

                                // Put this back as the last stack frame
                                vm_instance.thread.stack_frames.push(last);

                                vm_instance.pop_count += 1;
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

                // println!(
                //     "------ Total trace samples: {} ------",
                //     self.profiler.sample_count
                // );

                // panic!("GETTING HERE")

                // println!("GETTING HERE");

                // while self.stack.pop().is_some() {}

                // Clean up
                self.stack.clear();

                #[cfg(feature = "profiling")]
                if log_enabled!(target: "pipeline_time", log::Level::Debug) {
                    debug!(
                        target: "pipeline_time",
                        "VM Evaluation Time: {:?}",
                        execution_time.elapsed()
                    );
                };

                // println!("FINISHED");

                return result;
            }
        }
    }

    // pub fn snapshot_stack_trace(&self) -> DehydratedStackTrace {
    //     DehydratedStackTrace::new(
    //         self.stack_frames
    //             .iter()
    //             .map(|x| DehydratedCallContext::new(x.span))
    //             .collect(),
    //     )
    // }
}

#[derive(Clone, Debug)]
pub struct Continuation {
    pub(crate) stack: Vec<SteelVal>,
    pub(crate) current_frame: StackFrame,
    instructions: Rc<[DenseInstruction]>,
    pub(crate) stack_frames: Vec<StackFrame>,
    ip: usize,
    sp: usize,
    pop_count: usize,
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
            throw!(TypeMismatch => format!("application not a procedure: {function}")),
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
            throw!(TypeMismatch => format!("application not a procedure: {function}")),
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
            throw!(TypeMismatch => format!("application not a procedure: {function}")),
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

// TODO: Make dynamic block use this
enum FunctionBlock {
    Specialized(for<'r> fn(&'r mut VmCore<'_>) -> Result<()>),
    Unrolled(Rc<[for<'r> fn(&'r mut VmCore<'_>) -> Result<()>]>),
}

// Construct a basic block for a series of instructions
// Note: A call to either apply or call/cc should invalidate this, as it fundamentally
// violates the principle of a basic block.
#[derive(Clone)]
pub struct DynamicBlock {
    basic_block: InstructionPattern,
    entry_inst: DenseInstruction,
    header_func: Option<for<'r> fn(&'r mut VmCore<'_>, usize) -> Result<()>>,
    handlers: Rc<[for<'r> fn(&'r mut VmCore<'_>) -> Result<()>]>,
    specialized: Option<for<'r> fn(&'r mut VmCore<'_>, usize) -> Result<()>>,
}

impl DynamicBlock {
    fn call(&self, context: &mut VmCore<'_>) -> Result<()> {
        // println!("---- Entering dynamic block ----");
        // println!("{:#?}", self.basic_block);

        if let Some(specialized) = self.specialized {
            specialized(context, self.entry_inst.payload_size as usize)?;
        } else {
            if let Some(header) = self.header_func {
                // println!("Calling special entry block");
                header(context, self.entry_inst.payload_size as usize)?;
            }

            for func in self.handlers.iter() {
                func(context)?;
            }
        }

        // println!("---- Exited dynamic block ----");
        // println!(
        // "Current op code: {:?}",
        // context.instructions[context.ip].op_code
        // );
        Ok(())
    }

    #[cfg(feature = "dynamic")]
    fn construct_basic_block(head: DenseInstruction, basic_block: InstructionPattern) -> Self {
        // TODO: Drop the first
        let mut handlers = basic_block.block.iter().peekable();
        // .map(|x| OP_CODE_TABLE[x as usize]);
        // .collect();

        let mut header_func = None;

        log::debug!(target: "super-instructions", "{basic_block:#?}");

        if let Some(first) = handlers.peek() {
            header_func = op_code_requires_payload(first.0);
        }

        if header_func.is_some() {
            handlers.next();
        }

        let op_codes: Vec<_> = handlers.clone().copied().collect();

        let specialized = dynamic::DYNAMIC_SUPER_PATTERNS.get(&op_codes);

        if specialized.is_some() {
            println!("Found specialized function!");
        }

        let handlers = handlers.map(|x| OP_CODE_TABLE[x.0 as usize]).collect();

        Self {
            basic_block,
            handlers,
            entry_inst: head,
            header_func,
            // TODO: Come back and add the specialized ones back in
            specialized,
        }
    }
}

pub struct VmCore<'a> {
    pub(crate) instructions: Rc<[DenseInstruction]>,
    pub(crate) constants: ConstantMap,
    pub(crate) ip: usize,
    pub(crate) sp: usize,
    pub(crate) pop_count: usize,
    // pub(crate) spans: Rc<[Span]>,
    // pub(crate) span_id: usize,
    pub(crate) depth: usize,
    pub(crate) thread: &'a mut SteelThread,
    pub(crate) root_spans: &'a [Span],
}

// TODO: Delete this entirely, and just have the run function live on top of the SteelThread.
//
impl<'a> VmCore<'a> {
    fn new_unchecked(
        instructions: Rc<[DenseInstruction]>,
        constants: ConstantMap,
        spans: Rc<[Span]>,
        // span_id: usize,
        thread: &'a mut SteelThread,
        root_spans: &'a [Span],
    ) -> VmCore<'a> {
        VmCore {
            instructions,
            constants,
            ip: 0,
            sp: 0,
            pop_count: 1,
            // spans,
            // span_id,
            depth: 0,
            thread,
            root_spans,
        }
    }

    fn new(
        instructions: Rc<[DenseInstruction]>,
        constants: ConstantMap,
        spans: Rc<[Span]>,
        // span_id: usize,
        thread: &'a mut SteelThread,
        root_spans: &'a [Span],
    ) -> Result<VmCore<'a>> {
        if instructions.is_empty() {
            stop!(Generic => "empty stack!")
        }

        // Set up the instruction pointers here
        // let function = Gc::new(ByteCodeLambda::main(instructions.iter().copied().collect()));

        // let current_frame = StackFrame::new(0, function, 0, Rc::clone(&instructions));

        Ok(VmCore {
            instructions,
            constants,
            ip: 0,
            sp: 0,
            pop_count: 1,
            // spans,
            // span_id,
            depth: 0,
            thread,
            root_spans,
        })
    }

    pub fn make_box(&mut self, value: SteelVal) -> SteelVal {
        let allocated_var = self.thread.heap.allocate(
            value,
            self.thread.stack.iter(),
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
        );

        SteelVal::HeapAllocated(allocated_var)
    }

    pub fn make_mutable_vector(&mut self, values: Vec<SteelVal>) -> SteelVal {
        let allocated_var = self.thread.heap.allocate_vector(
            values,
            self.thread.stack.iter(),
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
        );

        SteelVal::MutableVector(allocated_var)
    }

    fn gc_collect(&mut self) {
        self.thread.heap.collect(
            None,
            None,
            self.thread.stack.iter(),
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
        );
    }

    // #[inline(always)]
    fn new_continuation_from_state(&self) -> Continuation {
        Continuation {
            stack: self.thread.stack.clone(),
            instructions: Rc::clone(&self.instructions),
            current_frame: self.thread.current_frame.clone(),
            stack_frames: self.thread.stack_frames.clone(),
            ip: self.ip,
            sp: self.sp,
            pop_count: self.pop_count,
            // spans: Rc::clone(&self.spans),
        }
    }

    // Grab the continuation - but this continuation can only be played once
    fn new_oneshot_continuation_from_state(&mut self) -> Continuation {
        Continuation {
            stack: std::mem::take(&mut self.thread.stack),
            instructions: Rc::clone(&self.instructions),
            current_frame: self.thread.current_frame.clone(),
            stack_frames: std::mem::take(&mut self.thread.stack_frames),
            ip: self.ip,
            sp: self.sp,
            pop_count: self.pop_count,
        }
    }

    pub fn snapshot_stack_trace(&self) -> DehydratedStackTrace {
        DehydratedStackTrace::new(
            self.thread
                .stack_frames
                .iter()
                .map(|x| {
                    DehydratedCallContext::new(
                        self.thread
                            .function_interner
                            .spans
                            .get(&x.function.id)
                            .and_then(|x| x.get(self.ip))
                            .copied(),
                    )
                })
                .collect(),
        )
    }

    // #[inline(always)]
    fn set_state_from_continuation(&mut self, continuation: Continuation) {
        self.thread.stack = continuation.stack;
        self.instructions = continuation.instructions;
        // self.spans = continuation.spans;
        self.ip = continuation.ip;
        self.sp = continuation.sp;
        self.pop_count = continuation.pop_count;
        self.thread.stack_frames = continuation.stack_frames;
        self.thread.current_frame = continuation.current_frame;
    }

    // #[inline(always)]
    fn construct_continuation_function(&self) -> SteelVal {
        let captured_continuation = self.new_continuation_from_state();
        SteelVal::ContinuationFunction(Gc::new(captured_continuation))
    }

    fn construct_oneshot_continuation_function(&self) -> SteelVal {
        todo!()
    }

    // Reset state FULLY
    fn call_with_instructions_and_reset_state(
        &mut self,
        closure: Rc<[DenseInstruction]>,
        // spans: Rc<[Span]>,
    ) -> Result<SteelVal> {
        let old_ip = self.ip;
        let old_instructions = std::mem::replace(&mut self.instructions, closure);
        let old_pop_count = self.pop_count;
        // let old_spans = std::mem::replace(&mut self.spans, spans);

        // dbg!(self.sp);

        // dbg!(self.thread.stack_frames.last().map(|x| x.sp));
        // let old_sp = self.sp;

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
        // self.spans = old_spans;
        self.sp = self.thread.stack_frames.last().map(|x| x.sp).unwrap_or(0);

        // dbg!(self.sp);
        // dbg!(self.thread.stack_frames.last().map(|x| x.sp));

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
                func.func()(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            // SteelVal::ContractedFunction(cf) => {
            //     let arg_vec = vec![arg];
            //     cf.apply(arg_vec, cur_inst_span, self)
            // }
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
                func.func()(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            // SteelVal::ContractedFunction(cf) => {
            //     let arg_vec = vec![arg1, arg2];
            //     cf.apply(arg_vec, cur_inst_span, self)
            // }
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
                func.func()(&arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            }
            // SteelVal::ContractedFunction(cf) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     cf.apply(arg_vec, cur_inst_span, self)
            // }
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

        let prev_length = self.thread.stack.len();
        // self.stack_index.push(prev_length);

        // if self.stack_frames

        let instructions = closure.body_exp();
        // let spans = closure.spans();

        // TODO:
        self.thread.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            0,
            instructions.clone(),
            // spans.clone(),
        ));

        self.sp = prev_length;

        let mut argument_count = 0;
        for arg in args {
            self.thread.stack.push(arg);
            argument_count += 1;
        }

        self.adjust_stack_for_multi_arity(closure, argument_count, &mut 0)?;

        // self.function_stack
        // .push(CallContext::new(Gc::clone(closure)));

        self.call_with_instructions_and_reset_state(instructions)
    }

    // Calling convention
    pub(crate) fn call_with_two_args(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        arg1: SteelVal,
        arg2: SteelVal,
    ) -> Result<SteelVal> {
        let prev_length = self.thread.stack.len();
        // self.stack_index.push(prev_length);

        self.thread.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            0,
            Rc::from([]),
            // Rc::from([]),
        ));

        self.sp = prev_length;

        self.thread.stack.push(arg1);
        self.thread.stack.push(arg2);
        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)));

        self.adjust_stack_for_multi_arity(closure, 2, &mut 0)?;

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    // Calling convention
    pub(crate) fn call_with_one_arg(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        arg: SteelVal,
    ) -> Result<SteelVal> {
        let prev_length = self.thread.stack.len();

        self.thread.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            0,
            Rc::from([]),
            // Rc::from([]),
        ));

        self.sp = prev_length;

        // println!("PUSHING NEW STACK INDEX ON");

        // self.stack_index.push(prev_length);
        self.thread.stack.push(arg);
        // self.function_stack
        //     .push(CallContext::new(Gc::clone(closure)));

        self.adjust_stack_for_multi_arity(closure, 1, &mut 0)?;

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    // pub fn get_slice_of_size_two(&mut self) -> &mut [SteelVal] {
    //     if let &mut [.., last, two] = self.thread.stack.as_mut_slice() {
    //         &mut [last, two]
    //     } else {
    //         unreachable!()
    //     }
    // }

    pub(crate) fn vm(&mut self) -> Result<SteelVal> {
        // let mut cur_inst;

        if self.depth > 64 {
            // TODO: Unwind the callstack? Patch over to the VM call stack rather than continue to do recursive calls?
            stop!(Generic => "stack overflow! The rust call stack is currently used for transducers, so we impose a hard recursion limit of 64"; self.current_span());
        }

        macro_rules! inline_primitive {
            ($name:tt, $payload_size:expr) => {{
                let last_index = self.thread.stack.len() - $payload_size as usize;

                let result = match $name(&mut self.thread.stack[last_index..]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                // This is the old way... lets see if the below way improves the speed
                // self.thread.stack.truncate(last_index);
                // self.thread.stack.push(result);

                self.thread.stack.truncate(last_index + 1);
                *self.thread.stack.last_mut().unwrap() = result;
                // self.thread.stack.push(result);

                self.ip += 2;
            }};
        }

        macro_rules! inline_register_primitive {
            ($name:tt) => {{
                let read_local = &self.instructions[self.ip];
                let push_const = &self.instructions[self.ip + 1];

                // get the local
                // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                let offset = self.get_offset();
                let local_value =
                    self.thread.stack[read_local.payload_size as usize + offset].clone();

                // get the const
                let const_val = self.constants.get(push_const.payload_size as usize);

                let result = match $name(&[local_value, const_val]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                self.thread.stack.push(result);

                self.ip += 2;
            }};
        }

        // TODO: Directly call the binary operation with the value as an isize
        macro_rules! inline_register_primitive_immediate {
            ($name:tt) => {{
                let read_local = &self.instructions[self.ip];
                let push_const = &self.instructions[self.ip + 1];

                // get the local
                // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                let offset = self.get_offset();
                let local_value =
                    self.thread.stack[read_local.payload_size as usize + offset].clone();

                // get the const value, if it can fit into the value...
                let const_val = SteelVal::IntV(push_const.payload_size as isize);

                // sub_handler_none_int

                let result = match $name(&[local_value, const_val]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                self.thread.stack.push(result);

                self.ip += 2;
            }};
        }

        // let mut frame = self.stack_frames.last().unwrap();

        // while self.ip < self.instructions.len() {
        loop {
            // Process the op code
            // TODO: Just build up a slice, don't directly store the full vec of op codes

            // assert_eq!(self.spans.len(), self.instructions.len());

            #[cfg(feature = "dynamic")]
            if let Some(pat) = self.thread.profiler.process_opcode(
                &self.instructions[self.ip].op_code,
                self.ip,
                &self.instructions,
                // Grab the current stack frame
                self.thread.stack_frames.last().map(|x| x.function.as_ref()),
            ) {
                // if count > 1000 {
                log::debug!(target: "super-instructions", "Found a hot pattern, creating super instruction...");

                log::debug!(target: "super-instructions", "{:#?}", pat);

                if USE_SUPER_INSTRUCTIONS {
                    // Index of the starting opcode
                    let start = pat.pattern.start;

                    let id = self.thread.super_instructions.len();

                    let guard = self.thread.stack_frames.last_mut().unwrap();

                    // Next run should get the new sequence of opcodes
                    let (head, _) = guard.function.update_to_super_instruction(start, id);

                    let block = DynamicBlock::construct_basic_block(head, pat);

                    self.thread.super_instructions.push(Rc::new(block));
                }
                // self.thread.super_instructions.push(Rc::new(|ctx| {
                //     block.call(ctx);
                // }))

                // let
                // }
            }

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

            let instr = self.instructions[self.ip];

            match instr {
                DenseInstruction {
                    op_code: OpCode::DynSuperInstruction,
                    payload_size,
                    ..
                } => {
                    self.cut_sequence();

                    // TODO: Store in a different spot? So that we can avoid cloning on every iteration?
                    let super_instruction =
                        { self.thread.super_instructions[payload_size as usize].clone() };

                    super_instruction.call(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::POPN,
                    payload_size,
                    ..
                } => {
                    let last = self.thread.stack.pop().unwrap();

                    // println!("-- POP N: {} -- ", payload_size);

                    // println!("popping: {}", payload_size);
                    // println!("Stack length: {:?}", self.thread.stack.len());

                    // println!("{:#?}", self.thread.stack);

                    // if payload_size as usize > self.thread.stack.len() {
                    //     self.thread.stack.clear()
                    // } else {
                    //     self.thread
                    //         .stack
                    //         .truncate(self.thread.stack.len() - payload_size as usize);
                    // }

                    self.thread
                        .stack
                        .truncate(self.thread.stack.len() - payload_size as usize);

                    self.thread.stack.push(last);

                    self.ip += 1;

                    // todo!()
                }

                DenseInstruction {
                    op_code: OpCode::POPSINGLE,
                    ..
                } => {
                    self.thread.stack.pop();
                    self.ip += 1;
                }

                DenseInstruction {
                    op_code: OpCode::POPPURE,
                    ..
                } => {
                    // println!("-- POP PURE --");

                    if let Some(r) = self.handle_pop_pure() {
                        return r;
                    }
                }

                DenseInstruction {
                    op_code: OpCode::SUBREGISTER1,
                    ..
                } => {
                    let read_local = &self.instructions[self.ip + 1];

                    // get the local
                    // let offset = frame.index;
                    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                    let offset = self.get_offset();
                    let local_value =
                        self.thread.stack[read_local.payload_size as usize + offset].clone();

                    let result = match subtract_primitive(&[local_value, SteelVal::IntV(1)]) {
                        Ok(value) => value,
                        Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    };

                    self.thread.stack.push(result);

                    self.ip += 2;
                }

                // Specialization of specific library functions!
                DenseInstruction {
                    op_code: OpCode::CONS,
                    ..
                } => {
                    cons_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::LIST,
                    payload_size,
                    ..
                } => {
                    list_handler(self, payload_size as usize)?;
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
                    op_code: OpCode::ADDIMMEDIATE,
                    ..
                } => {
                    inline_register_primitive_immediate!(add_primitive)
                }
                DenseInstruction {
                    op_code: OpCode::SUBIMMEDIATE,
                    ..
                } => {
                    // inline_register_primitive_immediate!(subtract_primitive)

                    let read_local = &self.instructions[self.ip];
                    let push_const = &self.instructions[self.ip + 1];

                    // get the local
                    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                    let offset = self.get_offset();
                    let local_value =
                        self.thread.stack[read_local.payload_size as usize + offset].clone();

                    // get the const value, if it can fit into the value...
                    let const_val = push_const.payload_size as isize;

                    // sub_handler_none_int

                    let result = sub_handler_none_int(self, local_value, const_val)?;

                    // let result = match $name(&[local_value, const_val]) {
                    //     Ok(value) => value,
                    //     Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    // };

                    // let result

                    self.thread.stack.push(result);

                    self.ip += 2;
                }
                DenseInstruction {
                    op_code: OpCode::LTEIMMEDIATE,
                    ..
                } => {
                    // inline_register_primitive_immediate!(subtract_primitive)

                    let read_local = &self.instructions[self.ip];
                    let push_const = &self.instructions[self.ip + 1];

                    // get the local
                    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                    let offset = self.get_offset();
                    let local_value =
                        self.thread.stack[read_local.payload_size as usize + offset].clone();

                    // get the const value, if it can fit into the value...
                    let const_val = push_const.payload_size as isize;

                    // sub_handler_none_int

                    // TODO: Probably elide the stack push if the next inst is an IF
                    let result = lte_handler_none_int(self, local_value, const_val)?;

                    // let result = match $name(&[local_value, const_val]) {
                    //     Ok(value) => value,
                    //     Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    // };

                    // let result

                    self.thread.stack.push(SteelVal::BoolV(result));

                    self.ip += 2;
                }
                DenseInstruction {
                    op_code: OpCode::LTEIMMEDIATEIF,
                    ..
                } => {
                    // inline_register_primitive_immediate!(subtract_primitive)

                    let read_local = &self.instructions[self.ip];
                    let push_const = &self.instructions[self.ip + 1];

                    // get the local
                    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                    let offset = self.get_offset();
                    let local_value =
                        self.thread.stack[read_local.payload_size as usize + offset].clone();

                    // get the const value, if it can fit into the value...
                    let const_val = push_const.payload_size as isize;

                    // sub_handler_none_int

                    // TODO: Probably elide the stack push if the next inst is an IF
                    let result = lte_handler_none_int(self, local_value, const_val)?;

                    // let result = match $name(&[local_value, const_val]) {
                    //     Ok(value) => value,
                    //     Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    // };

                    // let result

                    self.ip += 2;

                    // change to truthy...
                    if result {
                        self.ip += 1;
                    } else {
                        self.ip = self.instructions[self.ip].payload_size as usize;
                    }
                }

                DenseInstruction {
                    op_code: OpCode::ADD,
                    payload_size,
                    ..
                } => {
                    add_handler_payload(self, payload_size as usize)?;
                    // inline_primitive!(add_primitive, payload_size)
                }
                DenseInstruction {
                    op_code: OpCode::BINOPADD,
                    ..
                } => {
                    // add_handler_payload(self, 2)?;

                    let last_index = self.thread.stack.len() - 2;

                    let right = self.thread.stack.pop().unwrap();
                    let left = self.thread.stack.last().unwrap();

                    let result = match add_handler_none_none(left, &right) {
                        Ok(value) => value,
                        Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    };

                    // let result = match $name(&mut $ctx.thread.stack[last_index..]) {
                    //     Ok(value) => value,
                    //     Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
                    // };

                    // This is the old way... lets see if the below way improves the speed
                    // $ctx.thread.stack.truncate(last_index);
                    // $ctx.thread.stack.push(result);

                    // self.thread.stack.truncate(last_index + 1);
                    // *self.thread.stack.last_mut().unwrap() = result;

                    *self.thread.stack.last_mut().unwrap() = result;

                    self.ip += 2;

                    // inline_primitive!(add_primitive, payload_size)
                }
                DenseInstruction {
                    op_code: OpCode::SUB,
                    payload_size,
                    ..
                } => {
                    sub_handler_payload(self, payload_size as usize)?;
                    // inline_primitive!(subtract_primitive, payload_size)
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
                    lte_handler_payload(self, payload_size as usize)?;
                    // inline_primitive!(lte_primitive, payload_size);
                }

                DenseInstruction {
                    op_code: OpCode::VOID,
                    ..
                } => {
                    self.thread.stack.push(SteelVal::Void);
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
                    self.thread.stack.push(val);
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
                    op_code: OpCode::READLOCAL0,
                    ..
                } => local_handler0(self)?,
                DenseInstruction {
                    op_code: OpCode::READLOCAL1,
                    ..
                } => local_handler1(self)?,
                DenseInstruction {
                    op_code: OpCode::READLOCAL2,
                    ..
                } => local_handler2(self)?,
                DenseInstruction {
                    op_code: OpCode::READLOCAL3,
                    ..
                } => local_handler3(self)?,
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
                // TODO: Introduce macro for this
                DenseInstruction {
                    op_code: OpCode::MOVEREADLOCAL0,
                    ..
                } => {
                    let offset = self.get_offset();
                    let value = self.move_from_stack(offset);

                    self.thread.stack.push(value);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::MOVEREADLOCAL1,
                    ..
                } => {
                    let offset = self.get_offset();
                    let value = self.move_from_stack(offset + 1);

                    self.thread.stack.push(value);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::MOVEREADLOCAL2,
                    ..
                } => {
                    let offset = self.get_offset();
                    let value = self.move_from_stack(offset + 2);

                    self.thread.stack.push(value);
                    self.ip += 1;
                }

                DenseInstruction {
                    op_code: OpCode::MOVEREADLOCAL3,
                    ..
                } => {
                    let offset = self.get_offset();
                    let value = self.move_from_stack(offset + 3);

                    self.thread.stack.push(value);
                    self.ip += 1;
                }

                // DenseInstruction {
                //     op_code: OpCode::MOVEREADLOCALCALLGLOBAL,
                //     payload_size,
                //     ..
                // } => {
                //     self.handle_move_local(payload_size as usize)?;

                //     // Move to the next iteration of the loop
                //     let next_inst = self.instructions[self.ip];
                //     self.ip += 1;
                //     let next_next_inst = self.instructions[self.ip];
                //     self.handle_call_global(
                //         next_inst.payload_size as usize,
                //         next_next_inst.payload_size as usize,
                //     )?;
                // }
                DenseInstruction {
                    op_code: OpCode::SETLOCAL,
                    payload_size,
                    ..
                } => self.handle_set_local(payload_size as usize),
                DenseInstruction {
                    op_code: OpCode::LOADINT0,
                    ..
                } => {
                    self.thread.stack.push(SteelVal::INT_ZERO);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::LOADINT1,
                    ..
                } => {
                    self.thread.stack.push(SteelVal::INT_ONE);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::LOADINT2,
                    ..
                } => {
                    self.thread.stack.push(SteelVal::INT_TWO);
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
                    let func = self
                        .thread
                        .global_env
                        .repl_lookup_idx(payload_size as usize);

                    // get the local
                    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                    let offset = self.get_offset();
                    let local_value =
                        self.thread.stack[read_local.payload_size as usize + offset].clone();

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
                        next_inst.payload_size as usize,
                        payload_size as usize,
                        // next_inst.payload_size as usize,
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
                        next_inst.payload_size as usize,
                        payload_size as usize,
                        // next_inst.payload_size as usize,
                    )?;
                }
                DenseInstruction {
                    op_code: OpCode::FUNC,
                    payload_size,
                    ..
                } => {
                    // TODO: @Matt -> don't pop the function off of the stack, just read it from there directly.
                    let func = self.thread.stack.pop().unwrap();
                    self.handle_function_call(func, payload_size as usize)?;
                }
                // Tail call basically says "hey this function is exiting"
                // In the closure case, transfer ownership of the stack to the called function
                DenseInstruction {
                    op_code: OpCode::TAILCALL,
                    payload_size,
                    ..
                } => {
                    let func = self.thread.stack.pop().unwrap();
                    self.handle_tail_call(func, payload_size as usize)?
                }
                DenseInstruction {
                    op_code: OpCode::IF,
                    payload_size,
                    ..
                } => {
                    // change to truthy...
                    if self.thread.stack.pop().unwrap().is_truthy() {
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
                        self.thread.stack_frames.pop();
                        // self.instruction_stack.pop();
                        self.pop_count -= 1;
                    }

                    let last_stack_frame = self.thread.stack_frames.last().unwrap();

                    #[cfg(feature = "dynamic")]
                    {
                        last_stack_frame.function.increment_call_count();
                    }

                    self.instructions = last_stack_frame.function.body_exp();
                    // self.spans = last_stack_frame.function.spans();
                    self.sp = last_stack_frame.sp;

                    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

                    // panic!("Stopping");

                    self.ip = 0;

                    let closure_arity = last_stack_frame.function.arity();

                    if current_arity != closure_arity {
                        stop!(ArityMismatch => format!("tco: function expected {closure_arity} arguments, found {current_arity}"); self.current_span());
                    }

                    // HACK COME BACK TO THIS
                    // if self.ip == 0 && self.heap.len() > self.heap.limit() {
                    // TODO collect here
                    // self.heap.collect_garbage();
                    // }
                    // let offset = self.stack_index.last().copied().unwrap_or(0);
                    let offset = last_stack_frame.sp;

                    // We should have arity at this point, drop the stack up to this point
                    // take the last arity off the stack, go back and replace those in order
                    // [... arg1 arg2 arg3]
                    //      ^^^ <- back = this index
                    // offset = the start of the stack frame
                    // Copy the arg1 arg2 arg3 values to
                    // [... frame-start ... arg1 arg2 arg3]
                    //      ^^^^^^~~~~~~~~
                    let back = self.thread.stack.len() - current_arity;
                    // for i in 0..current_arity {
                    //     self.stack[offset + i] = self.stack[back + i].clone();
                    // }

                    // self.stack.truncate(offset + current_arity);

                    let _ = self.thread.stack.drain(offset..back);

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
                    op_code: OpCode::LetVar,
                    ..
                } => {
                    // todo!()
                    self.ip += 1;
                    // self.stack_index.push(self.stack.len());
                }
                DenseInstruction {
                    op_code: OpCode::SETALLOC,
                    ..
                } => set_alloc_handler(self)?,
                DenseInstruction {
                    op_code: OpCode::READALLOC,
                    ..
                } => read_alloc_handler(self)?,

                DenseInstruction {
                    op_code: OpCode::ALLOC,
                    ..
                } => alloc_handler(self)?,
                // todo!("Implement patching in vars from the stack to the heap");
                DenseInstruction {
                    op_code: OpCode::LETENDSCOPE,
                    ..
                } => {
                    let_end_scope_handler(self)?;

                    // let beginning_scope = payload_size as usize;
                    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

                    // // Move to the pop
                    // self.ip += 1;

                    // let rollback_index = beginning_scope + offset;

                    // let last = self.stack.pop().expect("stack empty at pop");

                    // self.stack.truncate(rollback_index);
                    // self.stack.push(last);

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
                    ..
                } => self.handle_new_start_closure(payload_size as usize)?,
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
                DenseInstruction {
                    op_code: OpCode::Arity,
                    ..
                } => {
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::PANIC,
                    ..
                } => self.handle_panic(self.current_span())?,
                DenseInstruction {
                    op_code: OpCode::PASS,
                    ..
                } => {
                    // log::warn!("Hitting a pass - this shouldn't happen");
                    self.ip += 1;
                }

                // match_dynamic_super_instructions!()
                _ => {
                    #[cfg(feature = "dynamic")]
                    // TODO: Dispatch on the function here for super instructions!
                    dynamic::vm_match_dynamic_super_instruction(self, instr)?;

                    crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                    panic!(
                        "Unhandled opcode: {:?} @ {}",
                        self.instructions[self.ip], self.ip
                    );
                }
            }
        }
    }

    fn move_from_stack(&mut self, offset: usize) -> SteelVal {
        std::mem::replace(&mut self.thread.stack[offset], SteelVal::Void)
    }

    // #[inline(always)]
    // TODO: This is definitely an issue - if the instruction stack is empty,
    // We will probably end up grabbing a garbage span
    fn current_span(&self) -> Span {
        //// New way
        // self.thread
        //     .stack_frames
        //     .last()
        //     .and_then(|x| x.function.spans.get(self.ip))
        //     .copied()
        //     // .unwrap()
        //     .unwrap_or_default()

        // println!("Span vec: {:#?}", self.spans);

        // self.spans.get(self.ip).copied().unwrap_or_default()

        self.thread
            .stack_frames
            .last()
            .map(|x| x.function.id)
            .and_then(|x| {
                self.thread
                    .function_interner
                    .spans
                    .get(&x)
                    .and_then(|x| x.get(self.ip))
            })
            .or_else(|| self.root_spans.get(self.ip))
            .copied()
            .unwrap_or_default()

        // todo!()

        // self.spans
        //     .get(
        //         self.instructions
        //             .get(self.ip)
        //             .map(|x| x.span_index)
        //             .unwrap_or_default(),
        //     )
        //     // .flatten()
        //     .copied()
        //     .unwrap_or_default()

        // Span::default()
    }

    fn enclosing_span(&self) -> Option<Span> {
        if self.thread.stack_frames.len() > 1 {
            let back_two = self.thread.stack_frames.len() - 2;

            if let [second, last] = &self.thread.stack_frames[self.thread.stack_frames.len() - 2..]
            {
                // todo!();

                let id = second.function.id;
                let spans = self.thread.function_interner.spans.get(&second.function.id);

                spans
                    .and_then(|x| {
                        if last.ip > 2 {
                            x.get(last.ip - 2)
                        } else {
                            None
                        }
                    })
                    .copied()
            } else {
                todo!()
            }
        } else {
            self.thread
                .stack_frames
                .last()
                .and_then(|frame| {
                    if frame.ip > 2 {
                        self.root_spans.get(frame.ip - 2)
                    } else {
                        None
                    }
                })
                .copied()
        }
    }

    #[inline(always)]
    fn handle_pop_pure(&mut self) -> Option<Result<SteelVal>> {
        // Check that the amount we're looking to pop and the function stack length are equivalent
        // otherwise we have a problem

        self.pop_count -= 1;

        // unwrap just because we want to see if we have something here
        // rolling back the function stack
        // self.function_stack.pop();

        let last = self.thread.stack_frames.pop();

        // let should_return = self.stack_frames.is_empty();
        let should_continue = self.pop_count != 0;

        if should_continue {
            // #[cfg(feature = "unsafe-internals")]
            // let last = unsafe { last.unwrap_unchecked() };

            // #[cfg(not(feature = "unsafe-internals"))]
            let last = last.unwrap();

            // Update the current frame to be the last one
            //
            // self.current_frame = last.clone();

            // let ret_val = self.stack.pop().unwrap();

            let rollback_index = last.sp;

            // let ret_val = self.thread.stack.pop().unwrap();

            // for _ in rollback_index..self.thread.stack.len() {
            //     self.thread.stack.pop();
            // }

            // self.thread.stack.push(ret_val);

            let _ = self
                .thread
                .stack
                .drain(rollback_index..self.thread.stack.len() - 1);

            // for value in values {
            //     dbg!(value);
            // }

            // self.update_state_with_frame(last);

            self.ip = last.ip;
            self.instructions = last.instructions;

            self.sp = self.get_last_stack_frame_sp();

            // self.sp = last.index;

            // self.sp = rollback_index;

            None
        } else {
            let ret_val = self.thread.stack.pop().ok_or_else(|| {
                SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
                    .with_span(self.current_span())
            });

            let rollback_index = last.map(|x| x.sp).unwrap_or(0);

            // Move forward past the pop
            self.ip += 1;

            self.thread.stack.truncate(rollback_index);
            self.sp = 0;

            Some(ret_val)
        }
    }

    #[inline(always)]
    fn update_state_with_frame(&mut self, last: StackFrame) {
        self.ip = last.ip;
        self.instructions = last.instructions;
        // self.spans = last.spans;
    }

    #[inline(always)]
    fn get_last_stack_frame_sp(&self) -> usize {
        self.thread.stack_frames.last().map(|x| x.sp).unwrap_or(0)
    }

    // #[inline(always)]
    fn handle_panic(&mut self, span: Span) -> Result<()> {
        let error_message = self.thread.stack.pop().unwrap();
        stop!(Generic => error_message.to_string(); span);
    }

    // #[inline(always)]
    fn handle_set(&mut self, index: usize) -> Result<()> {
        let value_to_assign = self.thread.stack.pop().unwrap();

        let value = self
            .thread
            .global_env
            .repl_set_idx(index, value_to_assign)?;

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
        #[cfg(not(feature = "unsafe-internals"))]
        {
            let func = self.thread.global_env.repl_lookup_idx(index);
            // TODO - handle this a bit more elegantly
            // self.handle_function_call(func, payload_size, span)
            self.handle_global_function_call(func, payload_size)
        }
        #[cfg(feature = "unsafe-internals")]
        {
            let func = self.thread.global_env.repl_lookup_idx(index);
            // TODO - handle this a bit more elegantly
            // self.handle_function_call(func, payload_size, span)
            self.handle_global_function_call_by_reference(&func, payload_size)
        }
    }

    #[inline(always)]
    fn handle_tail_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
        let func = self.thread.global_env.repl_lookup_idx(index);
        self.ip += 1;
        self.handle_tail_call(func, payload_size)
    }

    #[inline(always)]
    fn handle_push(&mut self, index: usize) -> Result<()> {
        let value = self.thread.global_env.repl_lookup_idx(index);

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_local(&mut self, index: usize) -> Result<()> {
        // println!("STACK HERE: {:?}", self.thread.stack);
        // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
        let offset = self.get_offset();

        // if index + offset >= self.thread.stack.len() {
        // dbg!(&self.thread.stack.get(offset..));
        //     // dbg!(&self.current_span());

        //     pretty_print_dense_instructions(&self.instructions);
        //     dbg!(self.ip);
        // }

        let value = self.thread.stack[index + offset].clone();

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    fn handle_read_captures(&mut self, index: usize) -> Result<()> {
        let value = self.thread.stack_frames.last().unwrap().function.captures()[index].clone();

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn handle_move_local(&mut self, index: usize) -> Result<()> {
        // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
        // let offset = self.stack_frames.last().unwrap().index;
        let offset = self.get_offset();
        let value = self.move_from_stack(index + offset);

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    fn handle_pure_function(&mut self, offset: usize) {
        // println!("Hitting start closure");

        // println!("Instruction: {:?}", self.instructions[self.ip]);

        // if self.instructions[self.ip].payload_size == 1 {
        //     println!("Found multi arity function");
        // }

        assert!(self.ip < self.instructions.len());

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

        let constructed_lambda = if let Some(prototype) = self
            .thread
            .function_interner
            .pure_function_interner
            .get(&closure_id)
        {
            prototype.clone()
        } else {
            // debug_assert!(self.instructions[forward_index - 1].op_code == OpCode::ECLOSURE);

            // TODO -> this is probably quite slow
            // If extraneous lets are lifted, we probably don't need this
            // or if instructions get stored in some sort of shared memory so I'm not deep cloning the window

            let forward_jump_index = self.ip + forward_jump - 1;

            // Construct the closure body using the offsets from the payload
            // used to be - 1, now - 2
            let closure_body = self.instructions[self.ip..forward_jump_index].into();

            let spans = if let Some(spans) = self
                .thread
                .stack_frames
                .last()
                .and_then(|x| self.thread.function_interner.spans.get(&x.function.id))
            {
                if forward_jump_index >= spans.len() {
                    self.root_spans[self.ip..forward_jump_index].into()
                } else {
                    spans[self.ip..forward_jump_index].into()
                }
            } else {
                self.root_spans[self.ip..forward_jump_index].into()
            };

            // if let Some(spans) = self.thread.function_interner.spans.get()

            // let spans = self.spans[self.ip..forward_jump_index].into();

            // snag the arity from the eclosure instruction
            let arity = self.instructions[forward_index - 1].payload_size;

            let constructed_lambda = Gc::new(ByteCodeLambda::new(
                closure_id,
                closure_body,
                arity as usize,
                is_multi_arity,
                Vec::new(),
                Vec::new(),
                // Rc::clone(&spans),
            ));

            self.thread
                .function_interner
                .pure_function_interner
                .insert(closure_id, Gc::clone(&constructed_lambda));

            // Put the spans into the
            self.thread
                .function_interner
                .spans
                .insert(closure_id, spans);

            constructed_lambda
        };

        self.thread
            .stack
            .push(SteelVal::Closure(constructed_lambda));

        self.ip = forward_index;
    }

    fn handle_new_start_closure(&mut self, offset: usize) -> Result<()> {
        // println!("Hitting start closure");

        // println!("Instruction: {:?}", self.instructions[self.ip]);

        // if self.instructions[self.ip].payload_size == 1 {
        //     println!("Found multi arity function");
        // }

        assert!(self.ip < self.instructions.len());

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
        // let mut heap_vars = Vec::with_capacity(ndefs as usize);

        let mut heap_vars = Vec::new();

        // TODO clean this up a bit
        // hold the spot for where we need to jump aftwards
        let forward_index = self.ip + forward_jump;

        // Insert metadata
        // TODO: We probably can get a bit better than this.
        // A lot of the captures are static, I'm not quite sure we necessarily need to patch down _every_ single one
        // each time, especially since each lambda is a standalone instance of this.

        // Top level %plain-let with closures will panic here:
        // (%plain-let ((foo 10) (bar 20)) (lambda (+ foo bar)))
        // So, this should probably do something like this:

        if let Some(guard) = self.thread.stack_frames.last() {
            let guard = self.thread.stack_frames.last().unwrap();
            // let stack_index = self.stack_index.last().copied().unwrap_or(0);
            // let stack_index = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
            let stack_index = self.get_offset();

            for _ in 0..ndefs {
                let instr = self.instructions[self.ip];
                match (instr.op_code, instr.payload_size) {
                    (OpCode::COPYCAPTURESTACK, n) => {
                        let offset = stack_index;
                        let value = self.thread.stack[n as usize + offset].clone();
                        captures.push(value);
                    }
                    (OpCode::COPYCAPTURECLOSURE, n) => {
                        debug_assert!(
                            !self.thread.stack_frames.is_empty(),
                            "Trying to capture from closure that doesn't exist",
                        );

                        debug_assert!((n as usize) < guard.function.captures().len());

                        let value = guard.function.captures()[n as usize].clone();

                        captures.push(value);
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
        } else {
            // let stack_index = self.stack_index.last().copied().unwrap_or(0);
            // let stack_index = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
            let stack_index = self.get_offset();

            for _ in 0..ndefs {
                let instr = self.instructions[self.ip];
                match (instr.op_code, instr.payload_size) {
                    (OpCode::COPYCAPTURESTACK, n) => {
                        let offset = stack_index;
                        let value = self.thread.stack[n as usize + offset].clone();
                        captures.push(value);
                    }
                    // (OpCode::COPYCAPTURECLOSURE, n) => {
                    //     debug_assert!(
                    //         !self.thread.stack_frames.is_empty(),
                    //         "Trying to capture from closure that doesn't exist",
                    //     );

                    //     debug_assert!((n as usize) < guard.function.captures().len());

                    //     let value = guard.function.captures()[n as usize].clone();

                    //     captures.push(value);
                    // }
                    (l, _) => {
                        panic!(
                            "Something went wrong in closure construction!, found: {:?} @ {}",
                            l, self.ip,
                        );
                    }
                }
                self.ip += 1;
            }
        }

        // TODO: Consider moving these captures into the interned closure directly
        // Its possible we're just doing a lot of extra capturing that way if we repeatedly copy things
        let constructed_lambda = if let Some(prototype) = self
            .thread
            .function_interner
            .closure_interner
            .get(&closure_id)
        {
            // log::trace!("Fetching closure from cache");

            let mut prototype = prototype.clone();
            prototype.set_captures(captures);
            prototype.set_heap_allocated(heap_vars);
            prototype
        } else {
            // log::trace!("Constructing closure for the first time");

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

            let forward_jump_index = forward_index - 1;

            // Construct the closure body using the offsets from the payload
            // used to be - 1, now - 2
            let closure_body = self.instructions[self.ip..forward_jump_index].into();
            // let spans = self.spans[self.ip..forward_jump_index].into();

            let spans = if let Some(spans) = self
                .thread
                .stack_frames
                .last()
                .and_then(|x| self.thread.function_interner.spans.get(&x.function.id))
            {
                if forward_jump_index >= spans.len() {
                    self.root_spans[self.ip..forward_jump_index].into()
                } else {
                    spans[self.ip..forward_jump_index].into()
                }
            } else {
                self.root_spans[self.ip..forward_jump_index].into()
            };

            // snag the arity from the eclosure instruction
            let arity = self.instructions[forward_jump_index].payload_size;

            let mut constructed_lambda = ByteCodeLambda::new(
                closure_id,
                closure_body,
                arity as usize,
                is_multi_arity,
                Vec::new(),
                Vec::new(),
            );

            self.thread
                .function_interner
                .closure_interner
                .insert(closure_id, constructed_lambda.clone());

            // Put the spans into the interner
            self.thread
                .function_interner
                .spans
                .insert(closure_id, spans);

            constructed_lambda.set_captures(captures);
            constructed_lambda.set_heap_allocated(heap_vars);

            constructed_lambda
        };

        self.thread
            .stack
            .push(SteelVal::Closure(Gc::new(constructed_lambda)));

        self.ip = forward_index;
        Ok(())
    }

    // Enter a new thread, passing values that can be serialized
    // Resolve all references, attempt to instantiate a new engine on the other side?
    fn new_thread(&mut self, function: Gc<ByteCodeLambda>) {
        todo!()

        // Analyze the dependencies of the function, and see if its safe to be spawned on another thread
    }

    // #[inline(always)]
    fn handle_bind(&mut self, payload_size: usize) {
        self.thread
            .global_env
            .repl_define_idx(payload_size, self.thread.stack.pop().unwrap());

        self.ip += 1;
    }

    // #[inline(always)]
    fn handle_set_local(&mut self, index: usize) {
        let value_to_set = self.thread.stack.pop().unwrap();
        let offset = self.get_offset();
        // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);

        let old_index = index + offset;

        assert!(old_index < self.thread.stack.len());

        let old_value = self.thread.stack[old_index].clone();

        // Modify the stack and change the value to the new one
        self.thread.stack[old_index] = value_to_set;

        self.thread.stack.push(old_value);
        self.ip += 1;
    }

    fn new_handle_tail_call_closure(
        &mut self,
        closure: Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        self.cut_sequence();

        // Something like:
        // let last_func = self.function_stack.last_mut().unwrap();
        // last_func.set_function(Gc::clone(&closure));
        // last_func.set_span(self.current_span());
        // let offset = last_func.index;

        // let last = self.stack_frames.pop().unwrap();

        // self.pop_count -= 1;
        // let offset = self.stack_index.pop().unwrap();
        // self.instruction_stack.pop();

        // let current_span = self.current_span();

        // self.stack_frames.last_mut().unwrap().set_function(function)

        // TODO
        // self.function_stack
        // .push(CallContext::new(Gc::clone(&closure)).with_span(self.current_span()));

        let mut new_arity = payload_size;

        self.adjust_stack_for_multi_arity(&closure, payload_size, &mut new_arity)?;

        let last = self.thread.stack_frames.last_mut().unwrap();

        let offset = last.sp;

        // Find the new arity from the payload

        // We should have arity at this point, drop the stack up to this point
        // take the last arity off the stack, go back and replace those in order
        // let back = self.stack.len() - new_arity;
        // for i in 0..new_arity {
        //     self.stack[offset + i] = self.stack[back + i].clone();
        // }

        // self.stack.truncate(offset + new_arity);

        let back = self.thread.stack.len() - new_arity;
        let _ = self.thread.stack.drain(offset..back);

        // // TODO
        // self.heap
        // .gather_mark_and_sweep_2(&self.global_env, &inner_env);

        // self.heap.collect_garbage();

        // Added this one as well
        // self.heap.add(Rc::clone(&self.global_env));

        // self.global_env = inner_env;
        self.instructions = closure.body_exp();
        // self.spans = closure.spans();

        last.set_function(closure);
        // last.set_span(current_span);

        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn adjust_stack_for_multi_arity(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
        new_arity: &mut usize,
    ) -> Result<()> {
        if likely(!closure.is_multi_arity) {
            if unlikely(closure.arity() != payload_size) {
                stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
            }
        } else {
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

            let values = self
                .thread
                .stack
                .split_off(self.thread.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(List::from(values));

            self.thread.stack.push(list);

            *new_arity = closure.arity();

            // println!("Stack after list conversion: {:?}", self.stack);
        }

        // else if closure.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        // }

        Ok(())
    }

    fn cut_sequence(&mut self) {
        #[cfg(feature = "dynamic")]
        if let Some(pat) = self.thread.profiler.cut_sequence(
            &self.instructions,
            self.thread.stack_frames.last().map(|x| x.function.as_ref()),
        ) {
            log::debug!(target: "super-instructions", "Found a hot pattern, creating super instruction...");
            log::debug!(target: "super-instructions", "{:#?}", pat);

            if USE_SUPER_INSTRUCTIONS {
                // Index of the starting opcode
                let start = pat.pattern.start;

                let id = self.thread.super_instructions.len();

                let guard = self.thread.stack_frames.last_mut().unwrap();

                // We don't want to repeatedly thrash by calculating hashes for the block pattern, so
                // we mark the tail of the block directly on the function itself.
                // guard.function.mark_block_tail(self.ip);

                // Next run should get the new sequence of opcodes
                let (head, _) = guard.function.update_to_super_instruction(start, id);

                let block = DynamicBlock::construct_basic_block(head, pat);
                self.thread.super_instructions.push(Rc::new(block));
            }
        }
    }

    #[inline(always)]
    fn handle_tail_call(&mut self, stack_func: SteelVal, payload_size: usize) -> Result<()> {
        use SteelVal::*;
        match stack_func {
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
            FuncV(f) => self.call_primitive_func(f, payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            // ContractedFunction(cf) => self.call_contracted_function_tail_call(&cf, payload_size),
            ContinuationFunction(cc) => self.call_continuation(&cc),
            Closure(closure) => self.new_handle_tail_call_closure(closure, payload_size),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            CustomStruct(s) => self.call_custom_struct(&s, payload_size),
            _ => {
                // println!("{:?}", self.stack);
                // println!("{:?}", self.stack_index);
                crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                stop!(BadSyntax => format!("TailCall - Application not a procedure or function type not supported: {stack_func}"); self.current_span());
            }
        }
    }

    // #[inline(always)]
    fn call_boxed_func(
        &mut self,
        func: &dyn Fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        // println!("{:?}, {:?}", self.thread.stack, payload_size);

        let last_index = self.thread.stack.len() - payload_size;

        let result = func(&self.thread.stack[last_index..])
            .map_err(|x| x.set_span_if_none(self.current_span()))?;

        self.thread.stack.truncate(last_index);

        self.thread.stack.push(result);
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
        let len = self.thread.stack.len();
        // This is the start of the arguments
        let last_index = len - payload_size - 1;

        // Peek the range for the [args ... function]
        //                        ~~~~~~~~~~
        // let result = func(self.stack.peek_range_double(last_index..len))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let result = match func(&self.thread.stack[last_index..len]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way, but now given that the function is included on the stack, this should work...
        // self.stack.truncate(last_index);
        // self.stack.push(result);

        self.thread.stack.truncate(last_index + 1);
        *self.thread.stack.last_mut().unwrap() = result;

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
        let len = self.thread.stack.len();
        // This is the start of the arguments
        let last_index = len - payload_size - 1;

        // Peek the range for the [args ... function]
        //                        ~~~~~~~~~~
        // let result = func(self.stack.peek_range_double(last_index..len))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let result = match func(&self.thread.stack[last_index..len]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way, but now given that the function is included on the stack, this should work...
        // self.stack.truncate(last_index);
        // self.stack.push(result);

        self.thread.stack.truncate(last_index + 1);
        *self.thread.stack.last_mut().unwrap() = result;

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_builtin_func(&mut self, func: BuiltInSignature, payload_size: usize) -> Result<()> {
        // Note: We Advance the pointer here. In the event we're calling a builtin that fusses with
        // the instruction pointer, we allow the function to override this. For example, call/cc will
        // advance the pointer - or perhaps, even fuss with the control flow.
        self.ip += 1;

        // TODO: Don't do this - just read directly from the stack
        let args = self
            .thread
            .stack
            .split_off(self.thread.stack.len() - payload_size);

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
            self.thread.stack.push(result?);
        }

        Ok(())
        // self.stack.push(result);
        // Ok(())
    }

    // #[inline(always)]
    fn call_primitive_mut_func(
        &mut self,
        f: fn(&mut [SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        // println!("Stack: {:?}", self.stack);

        let last_index = self.thread.stack.len() - payload_size;

        let result = f(&mut self.thread.stack[last_index..])
            .map_err(|x| x.set_span_if_none(self.current_span()))?;

        // TODO -> this can actually just be something like:
        // self.stack.truncate(self.stack.len() - payload_size + 1)
        // self.stack[self.stack.len() - 1] = result
        self.thread.stack.truncate(last_index);
        self.thread.stack.push(result);

        self.ip += 1;
        Ok(())
    }

    // TODO: Clean up function calls and create a nice calling convention API?
    fn call_custom_struct(&mut self, s: &UserDefinedStruct, payload_size: usize) -> Result<()> {
        if let Some(procedure) = s.maybe_proc() {
            if let SteelVal::HeapAllocated(h) = procedure {
                self.handle_global_function_call(h.get(), payload_size)
            } else {
                self.handle_global_function_call(procedure.clone(), payload_size)
            }
        } else {
            stop!(Generic => "Attempted to call struct as a function - no procedure found!");
        }
    }

    // #[inline(always)]
    fn call_primitive_func(
        &mut self,
        f: fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        // let result = f(self.stack.peek_range(self.stack.len() - payload_size..))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let last_index = self.thread.stack.len() - payload_size;

        let result = match f(&self.thread.stack[last_index..]) {
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
        self.thread.stack.truncate(last_index);
        self.thread.stack.push(result);

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    // fn call_contracted_function(
    //     &mut self,
    //     cf: &ContractedFunction,
    //     payload_size: usize,
    // ) -> Result<()> {
    //     if let Some(arity) = cf.arity() {
    //         if arity != payload_size {
    //             stop!(ArityMismatch => format!("function expected {arity} arguments, found {payload_size}"); self.current_span());
    //         }
    //     }

    //     // if A::enforce_contracts() {
    //     let args = self
    //         .thread
    //         .stack
    //         .split_off(self.thread.stack.len() - payload_size);

    //     let result = cf.apply(args, &self.current_span(), self)?;

    //     self.thread.stack.push(result);
    //     self.ip += 1;
    //     Ok(())
    //     // } else {
    //     //     self.handle_function_call(cf.function.clone(), payload_size)
    //     // }
    // }

    // // #[inline(always)]
    // fn call_contracted_function_tail_call(
    //     &mut self,
    //     cf: &ContractedFunction,
    //     payload_size: usize,
    // ) -> Result<()> {
    //     if let Some(arity) = cf.arity() {
    //         if arity != payload_size {
    //             stop!(ArityMismatch => format!("function expected {arity} arguments, found {payload_size}"); self.current_span());
    //         }
    //     }

    //     // if A::enforce_contracts() {
    //     let args = self
    //         .thread
    //         .stack
    //         .split_off(self.thread.stack.len() - payload_size);

    //     let result = cf.apply(args, &self.current_span(), self)?;

    //     self.thread.stack.push(result);
    //     self.ip += 1;
    //     Ok(())
    //     // } else {
    //     // self.handle_tail_call(cf.function.clone(), payload_size)
    //     // }
    // }

    fn call_future_func_on_stack(
        &mut self,
        func: Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>,
        payload_size: usize,
    ) -> Result<()> {
        // stack is [args ... function]
        let len = self.thread.stack.len();
        // This is the start of the arguments
        let last_index = len - payload_size - 1;

        // Peek the range for the [args ... function]
        //                        ~~~~~~~~~~
        // let result = func(self.stack.peek_range_double(last_index..len))
        //     .map_err(|x| x.set_span_if_none(self.current_span()))?;

        let result = match func(&self.thread.stack[last_index..len]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way, but now given that the function is included on the stack, this should work...
        // self.stack.truncate(last_index);
        // self.stack.push(result);

        self.thread.stack.truncate(last_index + 1);
        *self.thread.stack.last_mut().unwrap() = SteelVal::FutureV(Gc::new(result));

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_future_func(
        &mut self,
        f: Box<Rc<dyn Fn(&[SteelVal]) -> Result<FutureResult>>>,
        payload_size: usize,
    ) -> Result<()> {
        let last_index = self.thread.stack.len() - payload_size;

        let result = SteelVal::FutureV(Gc::new(f(&self.thread.stack[last_index..])?));

        self.thread.stack.truncate(last_index);
        self.thread.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    // TODO: See if calling continuations can be implemented in terms of the core ABI
    // That way, we dont need a special "continuation" function
    fn call_continuation(&mut self, continuation: &Continuation) -> Result<()> {
        let last =
            self.thread.stack.pop().ok_or_else(
                throw!(ArityMismatch => "continuation expected 1 argument, found none"),
            )?;

        self.set_state_from_continuation(continuation.clone());

        self.ip += 1;

        self.thread.stack.push(last);
        Ok(())
    }

    // #[inline(always)]
    fn handle_lazy_closure(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        local: SteelVal,
        const_value: SteelVal,
    ) -> Result<()> {
        self.cut_sequence();

        let prev_length = self.thread.stack.len();

        // push them onto the stack if we need to
        self.thread.stack.push(local);
        self.thread.stack.push(const_value);

        // Push new stack frame
        self.thread.stack_frames.push(
            StackFrame::new(
                prev_length,
                Gc::clone(closure),
                self.ip + 4,
                Rc::clone(&self.instructions),
                // Rc::clone(&self.spans),
            ), // .with_span(self.current_span()),
        );

        // self.current_frame.sp = prev_length;

        // self.current_frame.ip += 4;

        // // Set the sp to be the current values on this
        // let mut current_frame = StackFrame::new(
        //     prev_length,
        //     Gc::clone(closure),
        //     self.ip + 4,
        //     Rc::clone(&self.instructions),
        // )
        // .with_span(self.current_span());

        // std::mem::swap(&mut current_frame, &mut self.current_frame);
        // self.stack_frames.push(current_frame);

        self.sp = prev_length;

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

        self.check_stack_overflow()?;
        self.pop_count += 1;

        self.instructions = closure.body_exp();
        // self.spans = closure.spans();
        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn check_stack_overflow(&self) -> Result<()> {
        if CHECK_STACK_OVERFLOW {
            if unlikely(self.thread.stack_frames.len() >= STACK_LIMIT) {
                crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                println!("stack frame at exit: {:?}", self.thread.stack);
                stop!(Generic => "stack overflowed!"; self.current_span());
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn get_offset(&self) -> usize {
        self.sp
        // self.stack_frames.last().map(|x| x.index).unwrap_or(0)
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
                self.thread.stack.push(
                    f.func()(&[local, const_value])
                        .map_err(|x| x.set_span_if_none(self.current_span()))?,
                );
                self.ip += 4;
            }
            FuncV(f) => {
                // self.stack
                //     .push(f(&[local, const_value]).map_err(|x| x.set_span_if_none(self.current_span()))?);
                // self.ip += 4;

                match f(&[local, const_value]) {
                    Ok(value) => self.thread.stack.push(value),
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

                self.thread.stack.push(result);
                self.ip += 4;
            }
            // ContractedFunction(cf) => {
            //     if let Some(arity) = cf.arity() {
            //         if arity != 2 {
            //             stop!(ArityMismatch => format!("function expected {} arguments, found {}", arity, 2); self.current_span());
            //         }
            //     }

            //     // if A::enforce_contracts() {
            //     let result = cf.apply(vec![local, const_value], &self.current_span(), self)?;

            //     self.thread.stack.push(result);
            //     self.ip += 4;
            //     // } else {
            //     // self.handle_lazy_function_call(cf.function.clone(), local, const_value)?;
            //     // }
            // }
            // Contract(c) => self.call_contract(c, payload_size, span)?,
            ContinuationFunction(_cc) => {
                unimplemented!("calling continuation lazily not yet handled");
            }
            Closure(closure) => self.handle_lazy_closure(closure, local, const_value)?,
            MutFunc(func) => {
                let mut args = [local, const_value];
                self.thread
                    .stack
                    .push(func(&mut args).map_err(|x| x.set_span_if_none(self.current_span()))?);

                self.ip += 4;
            }
            CustomStruct(s) => {
                if let Some(proc) = s.maybe_proc() {
                    return self.handle_lazy_function_call(proc.clone(), local, const_value);
                } else {
                    stop!(Generic => "attempted to call struct as function, but the struct does not have a function to call!")
                }
            }
            // BuiltIn(func) => {
            //     let args = [local, const_value];
            //     let result =
            //         func(self, &args).map_err(|x| x.set_span_if_none(self.current_span()))?;
            //     self.stack.push(result);
            //     self.ip += 4;
            // }
            _ => {
                log::error!("{stack_func:?}");
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported, {stack_func}"); self.current_span());
            }
        }
        Ok(())
    }

    // // #[inline(always)]
    fn handle_function_call_closure(
        &mut self,
        closure: Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        self.cut_sequence();

        // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

        // println!("Handling function call for multi arity function");

        // Jit profiling

        #[cfg(feature = "jit")]
        {
            closure.increment_call_count();
        }

        self.adjust_stack_for_multi_arity(&closure, payload_size, &mut 0)?;

        self.sp = self.thread.stack.len() - closure.arity();

        // {
        //     // Current frame already has the instructions
        //     self.current_frame.ip += 1;
        //     self.current_frame = self.stack.len() - closure.arity();

        //     self.stack_frames.push(self.current_frame);

        //     let instructions = closure.body_exp();

        //     self.current_frame = StackFrame::new(self.sp, closure, 0, instructions)
        // }

        let instructions = closure.body_exp();
        // let spans = closure.spans();

        self.thread.stack_frames.push(
            StackFrame::new(
                self.sp,
                closure,
                self.ip + 1,
                Rc::clone(&self.instructions),
                // Rc::clone(&self.spans),
            ), // .with_span(self.current_span()),
        );

        // self.current_arity = Some(closure.arity());

        self.check_stack_overflow()?;

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

        self.instructions = instructions;
        // self.spans = spans;

        self.ip = 0;
        Ok(())
    }

    // #[cfg(feature = "jit")]
    // // #[inline(always)]
    // fn call_compiled_function(
    //     &mut self,
    //     function: &JitFunctionPointer,
    //     payload_size: usize,
    // ) -> Result<()> {
    //     if function.arity() != payload_size {
    //         stop!(ArityMismatch => format!("function expected {} arguments, found {}", function.arity(), payload_size); self.current_span());
    //     }

    //     let result = function.call_func(&mut self.thread.stack);

    //     // println!("Calling function!");

    //     self.thread.stack.push(result);
    //     self.ip += 1;

    //     Ok(())
    // }

    // TODO improve this a bit
    #[inline(always)]
    fn handle_function_call_closure_jit_without_profiling(
        &mut self,
        mut closure: Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        self.adjust_stack_for_multi_arity(&closure, payload_size, &mut 0)?;

        self.sp = self.thread.stack.len() - closure.arity();

        let mut instructions = closure.body_exp();
        // let mut spans = closure.spans();

        std::mem::swap(&mut instructions, &mut self.instructions);
        // std::mem::swap(&mut spans, &mut self.spans);

        // Do this _after_ the multi arity business
        // TODO: can these rcs be avoided
        self.thread.stack_frames.push(
            StackFrame::new(self.sp, closure, self.ip + 1, instructions), // .with_span(self.current_span()),
        );

        // self.current_arity = Some(closure.arity());

        self.check_stack_overflow()?;

        // closure arity here is the number of true arguments
        // self.stack_index.push(self.stack.len() - closure.arity());

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        self.pop_count += 1;

        // self.instructions = instructions;
        // self.spans = spans;
        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn handle_function_call_closure_jit_without_profiling_ref(
        &mut self,
        mut closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        self.adjust_stack_for_multi_arity(closure, payload_size, &mut 0)?;

        self.sp = self.thread.stack.len() - closure.arity();

        let mut instructions = closure.body_exp();
        // let mut spans = closure.spans();

        std::mem::swap(&mut instructions, &mut self.instructions);
        // std::mem::swap(&mut spans, &mut self.spans);

        // Do this _after_ the multi arity business
        // TODO: can these rcs be avoided
        self.thread.stack_frames.push(
            StackFrame::new_rooted(
                self.sp,
                // Almost assuredly UB - there really just needs to be a runtime reference
                // on the value that gets passed around, or we just need to
                #[cfg(feature = "unsafe-internals")]
                crate::gc::unsafe_roots::MaybeRooted::from_root(closure),
                #[cfg(not(feature = "unsafe-internals"))]
                closure.clone(),
                self.ip + 1,
                instructions,
            ), // .with_span(self.current_span()),
        );

        // self.current_arity = Some(closure.arity());

        self.check_stack_overflow()?;

        // closure arity here is the number of true arguments
        // self.stack_index.push(self.stack.len() - closure.arity());

        // TODO use new heap
        // self.heap
        //     .gather_mark_and_sweep_2(&self.global_env, &inner_env);
        // self.heap.collect_garbage();

        self.pop_count += 1;

        // self.instructions = instructions;
        // self.spans = spans;
        self.ip = 0;
        Ok(())
    }

    // TODO improve this a bit
    // #[inline(always)]
    #[inline(always)]
    fn handle_function_call_closure_jit(
        &mut self,
        closure: Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        // Record the end of the existing sequence
        self.cut_sequence();

        // Jit profiling -> Make sure that we really only trace once we pass a certain threshold
        // For instance, if this function
        #[cfg(feature = "dynamic")]
        {
            closure.increment_call_count();
        }

        self.handle_function_call_closure_jit_without_profiling(closure, payload_size)
    }

    #[inline(always)]
    fn handle_function_call_closure_jit_ref(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        // Record the end of the existing sequence
        self.cut_sequence();

        // Jit profiling -> Make sure that we really only trace once we pass a certain threshold
        // For instance, if this function
        #[cfg(feature = "dynamic")]
        {
            closure.increment_call_count();
        }

        self.handle_function_call_closure_jit_without_profiling_ref(closure, payload_size)
    }

    #[inline(always)]
    fn handle_global_function_call_by_reference(
        &mut self,
        stack_func: &SteelVal,
        payload_size: usize,
    ) -> Result<()> {
        use SteelVal::*;

        match stack_func {
            Closure(closure) => self.handle_function_call_closure_jit_ref(closure, payload_size)?,
            FuncV(f) => self.call_primitive_func(*f, payload_size)?,
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size)?,
            MutFunc(f) => self.call_primitive_mut_func(*f, payload_size)?,
            FutureFunc(f) => self.call_future_func(f.clone(), payload_size)?,
            // ContractedFunction(cf) => self.call_contracted_function(&cf, payload_size)?,
            ContinuationFunction(cc) => self.call_continuation(&cc)?,
            // #[cfg(feature = "jit")]
            // CompiledFunction(function) => self.call_compiled_function(function, payload_size)?,
            // Contract(c) => self.call_contract(&c, payload_size)?,
            BuiltIn(f) => self.call_builtin_func(*f, payload_size)?,
            // CustomStruct(s) => self.call_custom_struct_global(&s.borrow(), payload_size)?,
            _ => {
                // Explicitly mark this as unlikely
                cold();
                log::error!("{stack_func:?}");
                log::error!("Stack: {:?}", self.thread.stack);
                stop!(BadSyntax => "Function application not a procedure or function type not supported"; self.current_span());
            }
        }
        Ok(())
    }

    #[inline(always)]
    fn handle_global_function_call(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
    ) -> Result<()> {
        use SteelVal::*;

        match stack_func {
            Closure(closure) => self.handle_function_call_closure_jit(closure, payload_size),
            FuncV(f) => self.call_primitive_func(f, payload_size),
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            FutureFunc(f) => self.call_future_func(f, payload_size),
            // ContractedFunction(cf) => self.call_contracted_function(&cf, payload_size),
            ContinuationFunction(cc) => self.call_continuation(&cc),
            // Contract(c) => self.call_contract(&c, payload_size),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            CustomStruct(s) => self.call_custom_struct(&s, payload_size),
            _ => {
                // Explicitly mark this as unlikely
                cold();
                log::error!("{stack_func:?}");
                log::error!("Stack: {:?}", self.thread.stack);
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }
    }

    #[inline(always)]
    fn handle_non_instr_global_function_call(
        &mut self,
        stack_func: SteelVal,
        args: &mut [SteelVal],
    ) -> Result<SteelVal> {
        use SteelVal::*;

        self.ip += 1;

        match &stack_func {
            BoxedFunction(f) => f.func()(args),
            MutFunc(f) => f(args),
            FuncV(f) => f(args),
            FutureFunc(f) => Ok(SteelVal::FutureV(Gc::new(f(args)?))),
            _ => {
                log::error!("{stack_func:?}");
                log::error!("Stack: {:?}", self.thread.stack);
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {stack_func}"); self.current_span());
            }
        }

        // Ok(())
    }

    #[inline(always)]
    fn handle_non_instr_global_function_call_lazy_push(
        &mut self,
        stack_func: SteelVal,
        args: &mut [SteelVal],
    ) -> Result<()> {
        use SteelVal::*;

        // self.ip += 1;

        match stack_func {
            BoxedFunction(f) => {
                self.ip += 1;
                self.thread.stack.push(f.func()(args)?)
            }
            MutFunc(f) => {
                self.ip += 1;
                self.thread.stack.push(f(args)?)
            }
            FuncV(f) => {
                self.ip += 1;
                self.thread.stack.push(f(args)?)
            }
            FutureFunc(f) => {
                self.ip += 1;
                self.thread.stack.push(SteelVal::FutureV(Gc::new(f(args)?)))
            }
            Closure(closure) => {
                let arity = args.len();

                self.thread.stack.reserve(arity);

                for arg in args {
                    self.thread.stack.push(arg.clone());
                }

                // If we're here, we're already done profiling, and don't need to profile anymore
                self.handle_function_call_closure_jit(closure, arity)?;
            }
            // BuiltIn(f) => f(self, args),
            _ => {
                log::error!("Lazy push: {stack_func:?}");
                log::error!("Stack: {:?}", self.thread.stack);
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {stack_func}"); self.current_span());
            }
        }

        Ok(())
    }

    // #[inline(always)]
    // fn call_contract(&mut self, contract: &Gc<ContractType>, payload_size: usize) -> Result<()> {
    //     match contract.as_ref() {
    //         ContractType::Flat(f) => self.handle_function_call(f.predicate.clone(), payload_size),
    //         _ => {
    //             stop!(BadSyntax => "Function application not a procedure - cannot apply function contract to argument");
    //         }
    //     }
    // }

    // #[inline(always)]
    fn handle_function_call(&mut self, stack_func: SteelVal, payload_size: usize) -> Result<()> {
        use SteelVal::*;

        match stack_func {
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
            FuncV(f) => self.call_primitive_func(f, payload_size),
            FutureFunc(f) => self.call_future_func(f, payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            // ContractedFunction(cf) => self.call_contracted_function(&cf, payload_size),
            ContinuationFunction(cc) => self.call_continuation(&cc),
            Closure(closure) => self.handle_function_call_closure(closure, payload_size),
            // #[cfg(feature = "jit")]
            // CompiledFunction(function) => self.call_compiled_function(function, payload_size)?,
            // Contract(c) => self.call_contract(&c, payload_size),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            CustomStruct(s) => self.call_custom_struct(&s, payload_size),
            _ => {
                log::error!("{stack_func:?}");
                log::error!("stack: {:?}", self.thread.stack);
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {stack_func}"); self.current_span());
            }
        }
    }

    // #[inline(always)]
    fn handle_start_def(&mut self) {
        self.ip += 1;
    }
}

pub fn current_function_span(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if !args.is_empty() {
        builtin_stop!(ArityMismatch => format!("current-function-span requires no arguments, found {}", args.len()))
    }

    // println!("Enclosing span: {:?}", ctx.enclosing_span());

    match ctx.enclosing_span() {
        Some(s) => Some(Span::into_steelval(s)),
        None => Some(Ok(SteelVal::Void)),
    }
}

/// Inspect the locals at the given function. Probably need to provide a way to
/// loop this back into the sources, in order to resolve any span information.
pub fn breakpoint(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let offset = ctx.get_offset();

    println!("----- Locals -----");
    for (slot, i) in (offset..ctx.thread.stack.len()).enumerate() {
        println!("x{} = {:?}", slot, &ctx.thread.stack[i]);
    }

    Some(Ok(SteelVal::Void))
}

pub fn call_with_exception_handler(
    ctx: &mut VmCore,
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
            if ctx.thread.stack_frames.len() == STACK_LIMIT {
                println!("stack frame at exit: {:?}", ctx.thread.stack);
                builtin_stop!(Generic => "call/cc: stack overflowed!"; ctx.current_span());
            }

            if closure.arity() != 0 {
                builtin_stop!(Generic => "call-with-exception-handler expects a thunk with arity 0");
            }

            // Roll back one level
            ctx.ip -= 1;

            ctx.sp = ctx.thread.stack.len();

            // dbg!(&ctx.thread.stack);

            let handler = ctx
                .thread
                .function_interner
                .handlers
                .borrow_mut()
                .insert(handler);

            // Push the previous state on
            ctx.thread.stack_frames.push(
                StackFrame::new(
                    ctx.sp,
                    Gc::clone(&closure),
                    ctx.ip + 1,
                    Rc::clone(&ctx.instructions),
                )
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
            // ctx.spans = closure.spans();
            // ctx.function_stack
            //     .push(CallContext::new(closure).with_span(ctx.current_span()));

            ctx.ip = 0;
        }

        _ => {
            builtin_stop!(TypeMismatch => format!("call-with-exception-handler expects a thunk as an argument, found: {thunk}"); ctx.current_span())
        }
    }

    // Some(Ok(SteelVal::Void))
    None
}

pub fn oneshot_call_cc(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    todo!("Create continuation that can only be used once!")
}

pub fn call_cc(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    /*
    - Construct the continuation
    - Get the function that has been passed in (off the stack)
    - Apply the function with the continuation
    - Handle continuation function call separately in the handle_func_call
    */

    // Roll back one because we advanced prior to entering the builtin
    ctx.ip -= 1;

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
        SteelVal::FuncV(_) => {}
        _ => {
            builtin_stop!(Generic => format!("call/cc expects a function, found: {function}"); ctx.current_span())
        }
    }

    let continuation = ctx.construct_continuation_function();

    match function {
        SteelVal::Closure(closure) => {
            if ctx.thread.stack_frames.len() == STACK_LIMIT {
                log::error!("stack frame at exit: {:?}", ctx.thread.stack);
                builtin_stop!(Generic => "call/cc: stack overflowed!"; ctx.current_span());
            }

            if closure.arity() != 1 {
                builtin_stop!(Generic => "call/cc expects a function with arity 1");
            }

            ctx.sp = ctx.thread.stack.len();

            ctx.thread.stack_frames.push(
                StackFrame::new(
                    ctx.sp,
                    Gc::clone(&closure),
                    ctx.ip + 1,
                    Rc::clone(&ctx.instructions),
                    // Rc::clone(&ctx.spans),
                ), // .with_span(ctx.current_span()),
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
            // ctx.spans = closure.spans();
            // ctx.function_stack
            //     .push(CallContext::new(closure).with_span(ctx.current_span()));

            ctx.ip = 0;
        }
        SteelVal::ContinuationFunction(cc) => {
            ctx.set_state_from_continuation(cc.unwrap());
            ctx.ip += 1;
            // ctx.stack.push(continuation);
        }
        SteelVal::FuncV(f) => return Some(f(&[continuation])),

        _ => {
            builtin_stop!(Generic => format!("call/cc expects a function, found: {function}"));
        }
    }

    Some(Ok(continuation))
}

// TODO: Come back and finish this
pub(crate) const APPLY_DOC: DocTemplate<'static> = DocTemplate {
    signature: "(apply function lst) -> any",
    params: &["function : function?", "lst : list?"],
    description: r#"Applies the given `function` with arguments as the contents of the `lst`."#,
    examples: &[
        ("λ > (apply + (list 1 2 3 4))", "=> 10"),
        ("λ > (apply list (list 1 2 3 4))", "=> '(1 2 3 4)"),
    ],
};

pub(crate) fn get_test_mode(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.thread.runtime_options.test.into()))
}

pub(crate) fn set_test_mode(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    ctx.thread.runtime_options.test = true;

    Some(Ok(ctx.thread.runtime_options.test.into()))
}

pub(crate) fn list_modules(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    use crate::rvals::AsRefSteelVal;
    use crate::steel_vm::builtin::BuiltInModule;

    let mut nursery = ();

    // Find all of the modules that are
    let modules = ctx
        .thread
        .global_env
        .roots()
        .filter(|x| BuiltInModule::as_ref(x, &mut nursery).is_ok())
        .cloned()
        .collect();

    Some(Ok(SteelVal::ListV(modules)))
}

// TODO: This apply does not respect tail position
// Something like this: (define (loop) (apply loop '()))
// _should_ result in an infinite loop. In the current form, this is a Rust stack overflow.
// Similarly, care should be taken to check out transduce, because nested calls to that will
// result in a stack overflow with sufficient depth on the recursive calls
pub(crate) fn apply(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
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
                        ctx.thread.stack.push(arg.clone());
                    }

                    // TODO: Fix this unwrap
                    let res = ctx.handle_function_call_closure(closure.clone(), l.len());

                    if res.is_err() {
                        // This is explicitly unreachable, since we're checking
                        // that this is an error variant
                        return Some(res.map(|_| unreachable!()));
                    }

                    None
                }
                SteelVal::ContinuationFunction(cc) => {
                    ctx.set_state_from_continuation(cc.unwrap());
                    ctx.ip += 1;

                    None
                    // ctx.stack.push(continuation);
                }
                // TODO: Reuse the allocation for apply
                SteelVal::FuncV(f) => {
                    let args = l.into_iter().cloned().collect::<Vec<_>>();

                    let result = f(&args).map_err(|e| e.set_span_if_none(ctx.current_span()));

                    Some(result)
                }
                SteelVal::MutFunc(f) => {
                    let mut args = l.into_iter().cloned().collect::<Vec<_>>();

                    let result = f(&mut args).map_err(|e| e.set_span_if_none(ctx.current_span()));

                    Some(result)
                }
                SteelVal::BoxedFunction(f) => {
                    let mut args = l.into_iter().cloned().collect::<Vec<_>>();

                    let result =
                        f.func()(&args).map_err(|e| e.set_span_if_none(ctx.current_span()));

                    Some(result)
                }

                // Calling a builtin here might involve a little recursion business
                SteelVal::BuiltIn(f) => {
                    let args = l.into_iter().cloned().collect::<Vec<_>>();

                    // let result = f(&args).map_err(|e| e.set_span_if_none(ctx.current_span()));

                    // Some(result)

                    ctx.ip += 1;

                    // TODO: Don't do this - just read directly from the stack

                    let result = f(ctx, &args).map(|x| {
                        x.map_err(|x| {
                            // TODO: @Matt 4/24/2022 -> combine this into one function probably
                            if x.has_span() {
                                x
                            } else {
                                x.set_span_if_none(ctx.current_span())
                            }
                            // x.set_span_if_none(self.current_span())
                        })
                    });

                    // TODO: Check if this is right - I think we really just want to return the value?
                    if let Some(result) = result {
                        match result {
                            Ok(value) => ctx.thread.stack.push(value),
                            e @ Err(_) => return Some(e),
                        }
                    }

                    // ctx.ip += 1;

                    None
                }

                // SteelVal::CustomStruct(s) => {
                //     let args = l.into_iter().cloned().collect::<Vec<_>>();

                // }
                _ => {
                    builtin_stop!(Generic => format!("apply expects a function, found: {arg1}"));
                }
            }
        } else {
            builtin_stop!(TypeMismatch => "apply expected a function, found: {}", arg1);
        }
    } else {
        builtin_stop!(TypeMismatch => "apply expects a list, found: {}", arg2);
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug)]
pub struct InstructionPattern {
    pub(crate) block: Rc<[(OpCode, usize)]>,
    pub(crate) pattern: BlockPattern,
}

impl InstructionPattern {
    pub fn new(block: Rc<[(OpCode, usize)]>, pattern: BlockPattern) -> Self {
        Self { block, pattern }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, PartialOrd, Ord, Debug)]
pub struct BlockPattern {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Default, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct BlockMetadata {
    count: usize,
    length: usize,
    created: bool,
}

#[derive(Clone)]
pub struct OpCodeOccurenceProfiler {
    occurrences: HashMap<(OpCode, usize), usize>,
    time: HashMap<(OpCode, usize), std::time::Duration>,
    starting_index: Option<usize>,
    ending_index: Option<usize>,
    sample_count: usize,
}

impl OpCodeOccurenceProfiler {
    pub fn new() -> Self {
        OpCodeOccurenceProfiler {
            occurrences: HashMap::new(),
            time: HashMap::new(),
            starting_index: None,
            ending_index: None,
            sample_count: 0,
        }
    }

    pub fn reset(&mut self) {
        self.occurrences.clear();
        self.time.clear();
    }

    // Process the op code and the associated payload
    // TODO: Get this to just use offsets, don't actually clone the instruction set directly
    #[cfg(feature = "dynamic")]
    pub fn process_opcode(
        &mut self,
        opcode: &OpCode,
        // payload: usize,
        index: usize,
        instructions: &[DenseInstruction],
        function: Option<&ByteCodeLambda>,
    ) -> Option<InstructionPattern> {
        // *self.occurrences.entry((*opcode, payload)).or_default() += 1;

        let function = function?;

        // Trace once it becomes hot
        let call_count = function.call_count();
        // If we're in the special zone, profile, otherwise don't
        if call_count < 1000 || call_count > 10000 {
            self.starting_index = None;
            self.ending_index = None;
            return None;
        }

        match opcode {
            OpCode::SDEF | OpCode::EDEF => return None,
            _ => {}
        }

        if self.starting_index.is_none() {
            self.starting_index = Some(index);
        }

        self.ending_index = Some(index);

        match opcode {
            OpCode::JMP
            | OpCode::IF
            // | OpCode::CALLGLOBAL
            | OpCode::CALLGLOBALTAIL
            | OpCode::TAILCALL
            | OpCode::TCOJMP
            | OpCode::POPPURE
            | OpCode::LTEIMMEDIATEIF
            // | OpCode::FUNC 
            => {

                let block_pattern = BlockPattern {
                    start: self.starting_index.unwrap(),
                    end: index
                };

                let mut guard = function.blocks.borrow_mut();


                if let Some((_, metadata)) = guard.iter_mut().find(|x| x.0 == block_pattern) {

                    // self.sample_count += 1;
                    // println!("Sampling on op code: {:?}", opcode);

                    metadata.count += 1;
                    metadata.length = index - block_pattern.start;
                    // self.last_sequence = Some(pattern);

                    if metadata.count > 1000 && !metadata.created {
                        metadata.created = true;

                        // println!("{} {}", block_pattern.start, index);

                        let sequence = instructions[block_pattern.start..=index]
                            .iter()
                            .map(|x| (x.op_code, x.payload_size as usize))
                            .filter(|x| !x.0.is_ephemeral_opcode() && x.0 != OpCode::POPPURE)
                            .collect();

                        self.starting_index = None;
                        self.ending_index = None;


                        // println!("Pattern finished");

                        return Some(InstructionPattern::new(sequence, block_pattern));
                    }

                } else if index - block_pattern.start > 2 {
                    guard.push((block_pattern, BlockMetadata::default()));
                }

                self.starting_index = None;
                self.ending_index = None;

                // println!("Pattern finished");
            }
            _ => {
                // println!("Updating end to be: {}", index);
                self.ending_index = Some(index);
                // self.sequence.push((*opcode, index));
            }
        }

        None
    }

    #[cfg(feature = "dynamic")]
    pub fn cut_sequence(
        &mut self,
        instructions: &[DenseInstruction],
        function: Option<&ByteCodeLambda>,
    ) -> Option<InstructionPattern> {
        // println!(
        //     "Cutting sequence: {:?} {:?} {:?}",
        //     function_id, self.starting_index, self.ending_index
        // );

        let function = function?;

        // Trace once it becomes hot
        let call_count = function.call_count();

        // If we're in the special zone, profile, otherwise don't
        if !(1000..=10000).contains(&call_count) {
            self.starting_index = None;
            self.ending_index = None;
            return None;
        }

        let start = self.starting_index?;
        let index = self.ending_index?;

        let block_pattern = BlockPattern { start, end: index };

        // if function.check_tail(&block_pattern) {
        //     self.starting_index = None;
        //     self.ending_index = None;
        //     return None;
        // }

        let mut guard = function.blocks.borrow_mut();

        if let Some((_, metadata)) = guard.iter_mut().find(|x| x.0 == block_pattern) {
            metadata.count += 1;
            metadata.length = index - block_pattern.start;
            // self.last_sequence = Some(pattern);

            // self.sample_count += 1;
            // println!("Sampling on op code: {:?}", instructions[index].op_code);

            if metadata.count > 1000 && !metadata.created {
                metadata.created = true;

                let sequence = instructions[block_pattern.start..=index]
                    .iter()
                    .map(|x| (x.op_code, x.payload_size as usize))
                    .filter(|x| !x.0.is_ephemeral_opcode() && x.0 != OpCode::POPPURE)
                    .collect();

                self.starting_index = None;
                self.ending_index = None;

                // println!("Pattern finished");

                return Some(InstructionPattern::new(sequence, block_pattern));
            }
        } else if index - block_pattern.start > 3 {
            guard.push((block_pattern, BlockMetadata::default()));
        }

        self.starting_index = None;
        self.ending_index = None;

        // println!("Pattern finished");

        None
    }

    pub fn add_time(&mut self, opcode: &OpCode, payload: usize, time: std::time::Duration) {
        *self.time.entry((*opcode, payload)).or_default() += time;
    }

    // pub fn report_basic_blocks(&self) {
    //     println!("--------------- Basic Blocks ---------------");

    //     let mut blocks = self
    //         .super_instructions
    //         .basic_blocks
    //         .iter()
    //         .collect::<Vec<_>>();

    //     blocks.sort_by_key(|x| x.1);
    //     blocks.reverse();

    //     for block in blocks.iter().take(10) {
    //         if block.1.count > 0 {
    //             println!("{:#?}", block)
    //         }
    //     }

    //     println!("--------------------------------------------")
    // }

    pub fn report_time_spend(&self) {
        let total_time: u128 = self.time.values().map(|x| x.as_micros()).sum();

        let mut counts = self
            .time
            .iter()
            .map(|x| (x.0, (x.1.as_micros() as f64 / total_time as f64) * 100.0))
            .filter(|x| !f64::is_nan(x.1))
            .collect::<Vec<(&(OpCode, usize), f64)>>();

        counts.sort_by(|x, y| y.1.partial_cmp(&x.1).unwrap());

        println!("------- Time Spent: Profiling Report -------");
        for row in counts {
            println!("{:?} => {:.2}%", row.0, row.1);
        }
        println!("--------------------------------------------")
    }

    pub fn report(&self) {
        let total: usize = self.occurrences.values().sum();

        let mut counts = self
            .occurrences
            .iter()
            .map(|x| (x.0, (*x.1 as f64 / total as f64) * 100.0))
            .collect::<Vec<(&(OpCode, usize), f64)>>();

        counts.sort_by(|x, y| y.1.partial_cmp(&x.1).unwrap());

        println!("------- Profiling Report -------");
        println!("Total instructions executed: {total}");
        for row in counts {
            println!("{:?} => {:.2}%", row.0, row.1);
        }
        println!("--------------------------------")

        // println!("{:#?}", counts);
    }
}

// If the op code requires the original payload from the instruction that we're overwriting, we should
// attach it to the basic block, because otherwise we'll have lost the payload
fn op_code_requires_payload(
    op_code: OpCode,
) -> Option<for<'r> fn(&'r mut VmCore<'_>, usize) -> Result<()>> {
    match op_code {
        OpCode::VOID => None,
        OpCode::PUSH => Some(push_handler_with_payload),
        OpCode::IF => todo!(),
        OpCode::JMP => todo!(),
        OpCode::FUNC => Some(func_handler_with_payload),
        OpCode::SCLOSURE => todo!(),
        OpCode::ECLOSURE => todo!(),
        OpCode::BIND => Some(bind_handler_with_payload),
        OpCode::SDEF => todo!(),
        OpCode::EDEF => todo!(),
        OpCode::POPPURE => todo!(),
        OpCode::PASS => todo!(),
        OpCode::PUSHCONST => Some(push_const_handler_with_payload),
        OpCode::NDEFS => todo!(),
        OpCode::PANIC => None,
        OpCode::TAILCALL => todo!(),
        OpCode::SET => Some(set_handler_with_payload),
        OpCode::READLOCAL => Some(local_handler_with_payload),
        OpCode::READLOCAL0 => None,
        OpCode::READLOCAL1 => None,
        OpCode::READLOCAL2 => None,
        OpCode::READLOCAL3 => None,
        OpCode::SETLOCAL => Some(set_local_handler_with_payload),
        OpCode::COPYCAPTURESTACK => todo!(),
        OpCode::COPYCAPTURECLOSURE => todo!(),
        OpCode::COPYHEAPCAPTURECLOSURE => todo!(),
        OpCode::FIRSTCOPYHEAPCAPTURECLOSURE => todo!(),
        OpCode::TCOJMP => todo!(),
        OpCode::CALLGLOBAL => Some(call_global_handler_with_payload),
        OpCode::CALLGLOBALTAIL => todo!(),
        OpCode::LOADINT0 => None,
        OpCode::LOADINT1 => None,
        OpCode::LOADINT2 => None,
        OpCode::CGLOCALCONST => todo!(),
        OpCode::MOVEREADLOCAL => Some(move_local_handler_with_payload),
        OpCode::MOVEREADLOCAL0 => None,
        OpCode::MOVEREADLOCAL1 => None,
        OpCode::MOVEREADLOCAL2 => None,
        OpCode::MOVEREADLOCAL3 => None,
        OpCode::READCAPTURED => Some(read_captured_handler_with_payload),
        OpCode::BEGINSCOPE => None,
        OpCode::LETENDSCOPE => Some(let_end_scope_handler_with_payload),
        OpCode::PUREFUNC => Some(pure_function_handler_with_payload),
        OpCode::ADD => Some(add_handler_payload),
        OpCode::SUB => Some(sub_handler_payload),
        OpCode::MUL => Some(multiply_handler_payload),
        OpCode::DIV => Some(division_handler_payload),
        OpCode::EQUAL => Some(equality_handler_payload),
        OpCode::LTE => Some(lte_handler_payload),
        OpCode::NEWSCLOSURE => Some(new_sclosure_handler_with_payload),
        OpCode::ADDREGISTER => todo!(),
        OpCode::SUBREGISTER => todo!(),
        OpCode::LTEREGISTER => todo!(),
        OpCode::SUBREGISTER1 => todo!(),
        OpCode::ALLOC => None,
        OpCode::READALLOC => Some(read_alloc_handler_with_payload),
        OpCode::SETALLOC => Some(set_alloc_handler_with_payload),
        // OpCode::GIMMICK => todo!(),
        // OpCode::MOVEREADLOCALCALLGLOBAL => Some(move_read_local_call_global_handler_payload),
        OpCode::DynSuperInstruction => todo!(),
        _ => None,
    }
}

// Table to map opcode discriminant directly to an individual handler function
// Why do we want this? When generating dynamic super instructions, we create
// basic blocks, and from there transfer contexts away from the core vm loop, and instead
// over to a basic block sequence of handler function, which we will call directly
// on the main VM context. In order to construct these sequences, we will need to be able
// to grab a basic block from the running sequence, and directly patch an instruction set
// on the fly, to transfer context over to that sequence.
static OP_CODE_TABLE: [for<'r> fn(&'r mut VmCore<'_>) -> Result<()>; 66] = [
    void_handler,
    push_handler,
    if_handler,   // If
    jump_handler, // jmp
    func_handler,
    dummy, // sclosure
    dummy, // eclosure
    bind_handler,
    dummy, // sdef
    dummy, // edef
    dummy, // pop
    dummy, // popn
    dummy, // pass
    push_const_handler,
    dummy, // ndefs,
    panic_handler,
    tail_call_handler, // tailcall
    set_handler,
    local_handler,
    local_handler0,
    local_handler1,
    local_handler2,
    local_handler3,
    set_local_handler,
    dummy,            // copycapturestack
    dummy,            // copycaptureclosure
    dummy,            // copyheapcaptureclosure
    dummy,            // firstcopyheapcaptureclosure
    tco_jump_handler, // tcojmp
    call_global_handler,
    call_global_tail_handler, // callglobaltail
    handle_load_int0,
    handle_load_int1,
    handle_load_int2,
    dummy, // cglocalconst
    move_local_handler,
    move_local_handler0,
    move_local_handler1,
    move_local_handler2,
    move_local_handler3,
    read_captured_handler,
    begin_scope_handler,
    let_end_scope_handler,
    pure_function_handler,
    add_handler,
    sub_handler,
    multiply_handler,
    division_handler,
    equality_handler,
    lte_handler,
    new_sclosure_handler,
    dummy, // addregister
    dummy, // subregister
    dummy, // lteregister
    dummy, // subregister
    alloc_handler,
    read_alloc_handler,
    set_alloc_handler,
    // dummy,                               // gimmick
    // move_read_local_call_global_handler, // movereadlocalcallglobal,
    dummy, // dynsuperinstruction,
    dummy,
    dummy,
    dummy,
    dummy,
    dummy,
    binop_add_handler,
    dummy,
];

macro_rules! opcode_to_function {
    (VOID) => {
        void_handler
    };
    (PUSH) => {
        push_handler
    };
    (FUNC) => {
        func_handler
    };
    (BIND) => {
        bind_handler
    };
    (PUSHCONST) => {
        push_const_handler
    };
    (PANIC) => {
        panic_handler
    };
    (SET) => {
        set_handler
    };
    (READLOCAL0) => {
        local_handler0
    };
    (LOADINT2) => {
        handle_load_int2
    };
    (LTE) => {
        lte_handler
    };
    (MOVEREADLOCAL0) => {
        move_local_handler0
    };
    (SUB) => {
        sub_handler
    };
    (LOADINT1) => {
        handle_load_int1
    };
    (MUL) => {
        multiply_handler
    };
    (MOVEREADLOCAL1) => {
        move_local_handler1
    };
    (READLOCAL1) => {
        local_handler1
    };
    (READLOCAL2) => {
        local_handler2
    };
    (READLOCAL3) => {
        local_handler3
    };
    (LOADINT0) => {
        handle_load_int0
    };
    (CALLGLOBAL) => {
        call_global_handler
    };
    (READCAPTURED) => {
        read_captured_handler
    };
    (IF) => {
        if_handler
    };
    (EQUAL) => {
        equality_handler
    };
    (JMP) => {
        jump_handler
    };
    (ADD) => {
        add_handler
    };
    (TAILCALL) => {
        tail_call_handler
    };
}

static SUPER_PATTERNS: Lazy<
    std::collections::HashMap<Vec<OpCode>, for<'r> fn(&'r mut VmCore<'_>) -> Result<()>>,
> = Lazy::new(|| create_super_instruction_map());

// lazy_static! {
//     static ref SUPER_PATTERNS: std::collections::HashMap<
//         Vec<(OpCode, Option<usize>)>,
//         for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
//     > = create_super_instruction_map();
// }

fn create_super_instruction_map(
) -> std::collections::HashMap<Vec<OpCode>, for<'r> fn(&'r mut VmCore<'_>) -> Result<()>> {
    use OpCode::*;

    let mut map = HashMap::new();

    macro_rules! create_super_pattern {
        ($($args:tt),*) => {

            // fn func(ctx: &mut VmCore<'_>) -> Result<()> {
            //     $(
            //         OP_CODE_TABLE[$args as usize](ctx)?;
            //     )*

            //     Ok(())
            // }

            // TODO: This isn't actually doing the correct mapping. Set up a const mapping instead using macros
            map.insert(vec![
                $($args,)*
            ], |ctx: &mut VmCore<'_>| -> Result<()> {
                $(
                    opcode_to_function!($args)(ctx)?;
                )*

                Ok(())
            } as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>);
        };
    }

    // Fib patterns identified from the benchmarks
    // yes, this is explicitly gaming the benchmarks. But the idea is sound,
    // and this is just a start.
    // create_super_pattern!(READLOCAL0, LOADINT2, LTE, IF);

    map.insert(
        vec![READLOCAL0, LOADINT2, LTE, IF],
        specialized_lte0 as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
    );

    map.insert(
        vec![MOVEREADLOCAL0, LOADINT2, SUB, CALLGLOBAL],
        specialized_sub02 as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
    );

    map.insert(
        vec![READLOCAL0, LOADINT1, SUB, CALLGLOBAL],
        specialized_sub01 as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
    );

    // create_super_pattern!(MOVEREADLOCAL0, LOADINT2, SUB, CALLGLOBAL);
    // create_super_pattern!(READLOCAL0, LOADINT1, SUB, CALLGLOBAL);

    // bin trees patterns
    create_super_pattern!(
        READLOCAL0,
        LOADINT2,
        MUL,
        MOVEREADLOCAL1,
        LOADINT1,
        SUB,
        READLOCAL2,
        LOADINT1,
        SUB,
        READLOCAL3,
        CALLGLOBAL
    );

    create_super_pattern!(READLOCAL1, LOADINT0, CALLGLOBAL, IF);
    create_super_pattern!(MOVEREADLOCAL0, LOADINT0, READCAPTURED, TAILCALL);
    create_super_pattern!(MOVEREADLOCAL0, LOADINT2, READCAPTURED, TAILCALL);

    // Ack patterns
    create_super_pattern!(READLOCAL0, LOADINT0, EQUAL, IF);

    create_super_pattern!(READLOCAL1, LOADINT0, EQUAL, IF);

    create_super_pattern!(
        READLOCAL0,
        LOADINT1,
        SUB,
        MOVEREADLOCAL0,
        MOVEREADLOCAL1,
        LOADINT1,
        SUB,
        CALLGLOBAL
    );

    create_super_pattern!(READLOCAL1, LOADINT1, ADD, JMP);

    map
}

/*

TODO:
* https://www3.hhu.de/stups/downloads/pdf/BoCuFiRi09_246.pdf
* https://github.com/playXE/stack2ssa/blob/main/src/lower.rs

^^ The above actually seems to generate an honest to god JIT backend. What I'm suggesting
is to still just use the runtime, but instead to profile guided optimization to constantly be
creating sequences of instructions that speed things up.

Develop a DSL for generating super instructions, via macros.

If the op code handler pushes a value to the stack, it should be declared like so:

fn returns_value(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    ...
}

if its a bin op, make it look like this:

fn bin_op(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    ...
}

if we know it returns a bool, have it just return a bool:

fn bool(ctx: &mut VmCore<'_>) -> Result<bool> {
    ...
}

Then, with a sequence of fairly silly macros transformations, we could define a sequence of meta
instructions with some funny macros:

super_instruction! {
    READLOCAL0,
    LOADINT1,
    SUB,
    MOVEREADLOCAL0,
    MOVEREADLOCAL1,
    LOADINT1,
    SUB,
    CALLGLOBAL
}

could translate directly to something like (where we have comp time fresh vars, somehow):

let x = read_local_0(ctx);
let y = 1;
let z = x - y // somehow

let foo = move_read_local_0(ctx);
let bar = move_read_local_1(ctx);
let baz = 1;

let quux = bar - baz;
let call_global = call_global(ctx, ...)

ret call_global

We would have to perform stack to ssa conversion, such that its implicitly possible to reconstruct
the call stack, and then we can "compile" this directly to the rust code that manipulates the state
of the VM context.

"Unlocking" the state by removing from the state of the

*/

fn specialized_lte0(ctx: &mut VmCore<'_>) -> Result<()> {
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let value = ctx.thread.stack[offset].clone();

    let result = lte_binop(value, 2);
    ctx.ip += 4;

    let payload_size = ctx.instructions[ctx.ip].payload_size;
    // change to truthy...
    if result {
        ctx.ip += 1;
    } else {
        ctx.ip = payload_size as usize;
    }

    Ok(())
}

fn specialized_sub02(ctx: &mut VmCore<'_>) -> Result<()> {
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    let value = ctx.thread.stack[offset].clone();

    ctx.thread.stack.push(sub_binop(value, 2)?);
    ctx.ip += 4;
    Ok(())
}

fn specialized_sub01(ctx: &mut VmCore<'_>) -> Result<()> {
    let offset = ctx.get_offset();
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let value = ctx.thread.stack[offset].clone();

    ctx.thread.stack.push(sub_binop(value, 1)?);
    ctx.ip += 4;

    call_global_handler(ctx)
}

pub fn lte_binop(l: SteelVal, r: isize) -> bool {
    match l {
        SteelVal::IntV(l) => l <= r,
        _ => false,
    }
}

pub fn sub_binop(l: SteelVal, r: isize) -> Result<SteelVal> {
    match l {
        SteelVal::IntV(l) => Ok(SteelVal::IntV(l - r)),
        SteelVal::NumV(l) => Ok(SteelVal::NumV(l - r as f64)),
        _ => stop!(TypeMismatch => "sub given wrong types"),
    }
}

fn dummy(_: &mut VmCore<'_>) -> Result<()> {
    panic!("Unimplemented op code handler!")
}

#[inline(always)]
fn handle_push_no_stack(ctx: &mut VmCore<'_>, index: usize) -> Result<SteelVal> {
    let value = ctx.thread.global_env.repl_lookup_idx(index);
    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_local_no_stack(ctx: &mut VmCore<'_>, index: usize) -> Result<SteelVal> {
    let offset = ctx.get_offset();
    let value = ctx.thread.stack[index + offset].clone();
    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_read_captures_no_stack(ctx: &mut VmCore<'_>, index: usize) -> Result<SteelVal> {
    let value = ctx.thread.stack_frames.last().unwrap().function.captures()[index].clone();

    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_move_local_no_stack(ctx: &mut VmCore<'_>, index: usize) -> Result<SteelVal> {
    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
    // let offset = ctx.stack_frames.last().unwrap().index;
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(index + offset);
    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_move_local_0_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
    // let offset = ctx.stack_frames.last().unwrap().index;
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(offset);
    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_move_local_1_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
    // let offset = ctx.stack_frames.last().unwrap().index;
    let offset = ctx.get_offset();
    let value = ctx.move_from_stack(offset + 1);
    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_local_0_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    let offset = ctx.get_offset();
    let value = ctx.thread.stack[offset].clone();
    ctx.ip += 1;
    Ok(value)
}

#[inline(always)]
fn handle_local_1_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    let offset = ctx.get_offset();
    let value = ctx.thread.stack[offset + 1].clone();
    ctx.ip += 1;
    Ok(value)
}

// OpCode::VOID
fn void_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::Void);
    ctx.ip += 1;
    Ok(())
}

// OpCode::PUSH
fn push_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_push(index as usize)
}

// OpCode::PUSH
fn push_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.handle_push(payload)
}

// OpCode::FUNC
fn func_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let func = ctx.thread.stack.pop().unwrap();
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_function_call(func, payload_size as usize)
}

// OpCode::FUNC
fn func_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    let func = ctx.thread.stack.pop().unwrap();
    ctx.handle_function_call(func, payload)
}

// OpCode::BIND
fn bind_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_bind(index as usize);
    Ok(())
}

// OpCode::BIND
fn bind_handler_with_payload(ctx: &mut VmCore<'_>, index: usize) -> Result<()> {
    ctx.handle_bind(index);
    Ok(())
}

// OpCode::PUSHCONST
fn push_const_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let val = ctx.constants.get(payload_size as usize);
    ctx.thread.stack.push(val);
    ctx.ip += 1;
    Ok(())
}

fn push_const_handler_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let val = ctx.constants.get(payload_size as usize);
    ctx.ip += 1;
    Ok(val)
}

// OpCode::PUSHCONST
fn push_const_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    let val = ctx.constants.get(payload);
    ctx.thread.stack.push(val);
    ctx.ip += 1;
    Ok(())
}

// OpCode::PANIC
fn panic_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_panic(ctx.current_span())
}

// OpCode::SET
fn set_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_set(index as usize)
}

// OpCode::SET
fn set_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.handle_set(payload)
}

// OpCode::READLOCAL
#[inline(always)]
fn local_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_local(index as usize)
}

// OpCode::READLOCAL
fn local_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.handle_local(payload)
}

// OpCode::READLOCAL0
#[inline(always)]
fn local_handler0(ctx: &mut VmCore<'_>) -> Result<()> {
    // let offset = ctx.get_offset();
    // dbg!(&ctx.thread.stack.get(offset..));
    ctx.handle_local(0)
}

// OpCode::READLOCAL1
fn local_handler1(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_local(1)
}

// OpCode::READLOCAL2
fn local_handler2(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_local(2)
}

// OpCode::READLOCAL3
fn local_handler3(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_local(3)
}

// OpCode::SETLOCAL
fn set_local_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let offset = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_set_local(offset as usize);
    Ok(())
}

// OpCode::SETLOCAL
fn set_local_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.handle_set_local(payload);
    Ok(())
}

// OpCode::CALLGLOBAL
fn call_global_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    // assert!(ctx.ip + 1 < ctx.instructions.len());
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.ip += 1;
    let next_inst = ctx.instructions[ctx.ip];

    // println!("{:?}, {:?}", next_inst.payload_size, payload_size);

    ctx.handle_call_global(next_inst.payload_size as usize, payload_size as usize)
}

// OpCode::CALLGLOBAL
fn call_global_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.ip += 1;
    let next_inst = ctx.instructions[ctx.ip];
    ctx.handle_call_global(next_inst.payload_size as usize, payload)
}

// TODO: Have a way to know the correct arity?
fn call_global_handler_no_stack(ctx: &mut VmCore<'_>, args: &mut [SteelVal]) -> Result<SteelVal> {
    ctx.ip += 1;
    let payload_size = ctx.instructions[ctx.ip].payload_size;

    // TODO: Track the op codes of the surrounding values as well
    // let next_inst = ctx.instructions[ctx.ip];

    // println!("Looking up a function at index: {}", payload_size as usize);

    let func = ctx.thread.global_env.repl_lookup_idx(payload_size as usize);
    ctx.handle_non_instr_global_function_call(func, args)
}

// Call a global function with the given arguments, but only push the args to the stack
// if we need to

#[inline(always)]
fn call_global_handler_with_args(ctx: &mut VmCore<'_>, args: &mut [SteelVal]) -> Result<()> {
    ctx.ip += 1;
    let payload_size = ctx.instructions[ctx.ip].payload_size;

    // TODO: Track the op codes of the surrounding values as well
    // let next_inst = ctx.instructions[ctx.ip];

    // println!("Looking up a function at index: {}", payload_size as usize);
    // panic!("Call global handler with args");

    let func = ctx.thread.global_env.repl_lookup_idx(payload_size as usize);
    ctx.handle_non_instr_global_function_call_lazy_push(func, args)
}

// OpCode::LOADINT0
fn handle_load_int0(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::INT_ZERO);
    ctx.ip += 1;
    Ok(())
}

// OpCode::LOADINT1
fn handle_load_int1(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::INT_ONE);
    ctx.ip += 1;
    Ok(())
}

// OpCode::LOADINT2
fn handle_load_int2(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::INT_TWO);
    ctx.ip += 1;
    Ok(())
}

// OpCode::MOVEREADLOCAL
fn move_local_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_move_local(index as usize)
}

// OpCode::MOVEREADLOCAL
fn move_local_handler_with_payload(ctx: &mut VmCore<'_>, index: usize) -> Result<()> {
    ctx.handle_move_local(index)
}

// OpCode::MOVEREADLOCAL0
fn move_local_handler0(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(0)
}

// OpCode::MOVEREADLOCAL1
fn move_local_handler1(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(1)
}

// OpCode::MOVEREADLOCAL2
fn move_local_handler2(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(2)
}

// OpCode::MOVEREADLOCAL3
fn move_local_handler3(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(3)
}

// OpCode::READCAPTURED
fn read_captured_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_read_captures(payload_size as usize)
}

// OpCode::READCAPTURED
fn read_captured_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    ctx.handle_read_captures(payload_size)
}

// OpCode::BEGINSCOPE
fn begin_scope_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.ip += 1;
    Ok(())
}

// OpCode::LETENDSCOPE
fn let_end_scope_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let beginning_scope = ctx.instructions[ctx.ip].payload_size as usize;
    let_end_scope_handler_with_payload(ctx, beginning_scope)
}

// OpCode::LETENDSCOPE
fn let_end_scope_handler_with_payload(ctx: &mut VmCore<'_>, beginning_scope: usize) -> Result<()> {
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();
    // let offset = ctx.sp;

    // Move to the pop
    ctx.ip += 1;

    let rollback_index = beginning_scope + offset;

    // let rollback_index = offset;

    // dbg!(beginning_scope, offset);
    // dbg!(rollback_index);
    // dbg!(ctx
    //     .thread
    //     .stack
    //     .get(rollback_index..ctx.thread.stack.len() - 1));
    // dbg!(ctx.thread.stack.len() - 1);

    // dbg!(&ctx.thread.stack);

    let _ = ctx
        .thread
        .stack
        .drain(rollback_index..ctx.thread.stack.len() - 1);

    // dbg!(dropped_locals.collect::<Vec<_>>());

    // let last = ctx.stack.pop().expect("stack empty at pop");

    // ctx.stack.truncate(rollback_index);
    // ctx.stack.push(last);

    Ok(())
}

// OpCode::PUREFUNC
fn pure_function_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size as usize;
    ctx.handle_pure_function(payload_size);
    Ok(())
}

// OpCode::PUREFUNC
fn pure_function_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    ctx.handle_pure_function(payload_size);
    Ok(())
}

macro_rules! handler_inline_primitive {
    ($ctx:expr, $name:tt) => {{
        let payload_size = $ctx.instructions[$ctx.ip].payload_size as usize;
        let last_index = $ctx.thread.stack.len() - payload_size as usize;

        let result = match $name(&mut $ctx.thread.stack[last_index..]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
        };

        // This is the old way... lets see if the below way improves the speed
        // $ctx.thread.stack.truncate(last_index);
        // $ctx.thread.stack.push(result);

        $ctx.thread.stack.truncate(last_index + 1);
        *$ctx.thread.stack.last_mut().unwrap() = result;

        $ctx.ip += 2;
    }};
}

macro_rules! handler_inline_primitive_payload {
    ($ctx:expr, $name:tt, $payload_size: expr) => {{
        let last_index = $ctx.thread.stack.len() - $payload_size as usize;

        let result = match $name(&mut $ctx.thread.stack[last_index..]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
        };

        // This is the old way... lets see if the below way improves the speed
        // $ctx.thread.stack.truncate(last_index);
        // $ctx.thread.stack.push(result);

        $ctx.thread.stack.truncate(last_index + 1);
        *$ctx.thread.stack.last_mut().unwrap() = result;

        $ctx.ip += 2;
    }};
}

// OpCode::ADD
fn add_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive!(ctx, add_primitive);
    Ok(())
}

fn binop_add_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let last_index = ctx.thread.stack.len() - 2;

    let right = ctx.thread.stack.pop().unwrap();
    let left = ctx.thread.stack.last().unwrap();

    let result = match add_handler_none_none(left, &right) {
        Ok(value) => value,
        Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
    };

    // let result = match $name(&mut $ctx.thread.stack[last_index..]) {
    //     Ok(value) => value,
    //     Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
    // };

    // This is the old way... lets see if the below way improves the speed
    // $ctx.thread.stack.truncate(last_index);
    // $ctx.thread.stack.push(result);

    // self.thread.stack.truncate(last_index + 1);
    // *self.thread.stack.last_mut().unwrap() = result;

    *ctx.thread.stack.last_mut().unwrap() = result;

    ctx.ip += 2;

    Ok(())
}

fn cons_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload!(ctx, cons, 2);
    Ok(())
}

fn cons_handler_no_stack(ctx: &mut VmCore<'_>) -> Result<()> {
    todo!()
}

fn list_handler(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, new_list, payload);
    Ok(())
}

fn list_handler_no_stack(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    todo!()
}

// OpCode::ADD
fn add_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, special_add, payload);
    Ok(())
}

// OpCode::SUB
#[inline(always)]
fn sub_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive!(ctx, subtract_primitive);
    Ok(())
}

// OpCode::SUB
#[inline(always)]
fn sub_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, subtract_primitive, payload);
    Ok(())
}

// OpCode::MULT
fn multiply_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive!(ctx, multiply_primitive);
    Ok(())
}

// OpCode::MULT
fn multiply_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, multiply_primitive, payload);
    Ok(())
}

// OpCode::DIV
#[inline(always)]
fn division_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive!(ctx, divide_primitive);
    Ok(())
}

// OpCode::DIV
fn division_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, divide_primitive, payload);
    Ok(())
}

// OpCode::EQUAL
#[inline(always)]
fn equality_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive!(ctx, equality_primitive);
    Ok(())
}

// OpCode::EQUAL
fn equality_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, equality_primitive, payload);
    Ok(())
}

// OpCode::LTE
#[inline(always)]
fn lte_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive!(ctx, lte_primitive);
    Ok(())
}

// OpCode::LTE
fn lte_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, lte_primitive, payload);
    Ok(())
}

// OpCode::ALLOC
fn alloc_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.get_offset();

    let allocated_var = ctx.thread.heap.allocate(
        ctx.thread.stack[offset].clone(), // TODO: Could actually move off of the stack entirely
        ctx.thread.stack.iter(),
        ctx.thread.stack_frames.iter().map(|x| x.function.as_ref()),
        ctx.thread.global_env.roots(),
    );

    ctx.thread
        .stack_frames
        .last_mut()
        .unwrap()
        .function
        .heap_allocated
        .borrow_mut()
        .push(allocated_var);

    ctx.ip += 1;

    Ok(())
}

// OpCode::READALLOC
#[inline(always)]
fn read_alloc_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size as usize;

    let value = ctx
        .thread
        .stack_frames
        .last()
        .unwrap()
        .function
        .heap_allocated()
        .borrow()[payload_size]
        .get();

    // dbg!(payload_size);

    // dbg!(ctx
    //     .thread
    //     .stack_frames
    //     .last()
    //     .unwrap()
    //     .function
    //     .heap_allocated()
    //     .borrow()
    //     .iter()
    //     .map(|x| x.get())
    //     .collect::<Vec<_>>());

    // dbg!(&value);

    ctx.thread.stack.push(value);
    ctx.ip += 1;

    Ok(())
}

// OpCode::READALLOC
#[inline(always)]
fn read_alloc_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    let value = ctx
        .thread
        .stack_frames
        .last()
        .unwrap()
        .function
        .heap_allocated()
        .borrow()[payload_size]
        .get();

    ctx.thread.stack.push(value);
    ctx.ip += 1;

    Ok(())
}

// OpCode::SETALLOC
#[inline(always)]
fn set_alloc_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size as usize;
    let value_to_assign = ctx.thread.stack.pop().unwrap();

    let old_value = ctx
        .thread
        .stack_frames
        .last()
        .unwrap()
        .function
        .heap_allocated()
        .borrow_mut()[payload_size]
        .set(value_to_assign);

    ctx.thread.stack.push(old_value);
    ctx.ip += 1;

    Ok(())
}

// OpCode::SETALLOC
#[inline(always)]
fn set_alloc_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    let value_to_assign = ctx.thread.stack.pop().unwrap();

    let old_value = ctx
        .thread
        .stack_frames
        .last()
        .unwrap()
        .function
        .heap_allocated()
        .borrow_mut()[payload_size]
        .set(value_to_assign);

    ctx.thread.stack.push(old_value);
    ctx.ip += 1;

    Ok(())
}

// OpCode::NEWSCLOSURE
fn new_sclosure_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size as usize;

    ctx.handle_new_start_closure(payload_size)
}

// OpCode::NEWSCLOSURE
fn new_sclosure_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    ctx.handle_new_start_closure(payload_size)
}

#[inline(always)]
fn jump_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.ip = payload_size as usize;
    Ok(())
}

#[inline(always)]
fn if_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    // change to truthy...
    if ctx.thread.stack.pop().unwrap().is_truthy() {
        ctx.ip += 1;
    } else {
        ctx.ip = payload_size as usize;
    }
    Ok(())
}

#[inline(always)]
fn raw_if_handler(ctx: &mut VmCore<'_>, arg: SteelVal) {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    // change to truthy...
    if arg.is_truthy() {
        ctx.ip += 1;
    } else {
        ctx.ip = payload_size as usize;
    }
}

#[inline(always)]
fn if_handler_with_bool(ctx: &mut VmCore<'_>, condition: bool) {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    // change to truthy...
    if condition {
        ctx.ip += 1;
    } else {
        ctx.ip = payload_size as usize;
    }
}

#[inline(always)]
fn call_global_tail_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    // assert!(ctx.ip + 1 < ctx.instructions.len());

    let payload_size = ctx.instructions[ctx.ip].payload_size;
    // ctx.ip += 1;
    let next_inst = ctx.instructions[ctx.ip + 1];

    // println!("{:?}, {:?}", payload_size, next_inst.payload_size);

    ctx.handle_tail_call_global(next_inst.payload_size as usize, payload_size as usize)

    // ctx.handle_tail_call_global(payload_size as usize, next_inst.payload_size as usize)
}

#[inline(always)]
fn tail_call_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    let func = ctx.thread.stack.pop().unwrap();
    ctx.handle_tail_call(func, payload_size as usize)
}

#[inline(always)]
fn tco_jump_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    // println!("At tco jump");

    let payload_size = ctx.instructions[ctx.ip].payload_size;

    let current_arity = payload_size as usize;
    // This is the number of (local) functions we need to pop to get back to the place we want to be at
    let depth = ctx.instructions[ctx.ip + 1].payload_size as usize;

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
        ctx.thread.stack_frames.pop();
        // self.instruction_stack.pop();
        ctx.pop_count -= 1;
    }

    let last_stack_frame = ctx.thread.stack_frames.last().unwrap();

    ctx.instructions = last_stack_frame.function.body_exp();
    // ctx.spans = last_stack_frame.function.spans();
    ctx.sp = last_stack_frame.sp;

    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

    // panic!("Stopping");

    ctx.ip = 0;

    let closure_arity = last_stack_frame.function.arity();

    if current_arity != closure_arity {
        stop!(ArityMismatch => format!("tco: function expected {closure_arity} arguments, found {current_arity}"); ctx.current_span());
    }

    // HACK COME BACK TO THIS
    // if self.ip == 0 && self.heap.len() > self.heap.limit() {
    // TODO collect here
    // self.heap.collect_garbage();
    // }
    // let offset = self.stack_index.last().copied().unwrap_or(0);
    // let offset = ctx.stack_frames.last().map(|x| x.index).unwrap_or(0);
    let offset = ctx.sp;

    // We should have arity at this point, drop the stack up to this point
    // take the last arity off the stack, go back and replace those in order
    // [... arg1 arg2 arg3]
    //      ^^^ <- back = this index
    // offset = the start of the stack frame
    // Copy the arg1 arg2 arg3 values to
    // [... frame-start ... arg1 arg2 arg3]
    //      ^^^^^^~~~~~~~~
    // let back = ctx.stack.len() - current_arity;
    // for i in 0..current_arity {
    //     ctx.stack[offset + i] = ctx.stack[back + i].clone();
    // }

    // // self.stack.truncate(offset + current_arity);
    // ctx.stack.truncate(offset + current_arity);

    let back = ctx.thread.stack.len() - current_arity;
    // for i in 0..current_arity {
    //     self.stack[offset + i] = self.stack[back + i].clone();
    // }

    // self.stack.truncate(offset + current_arity);

    let _ = ctx.thread.stack.drain(offset..back);

    Ok(())
}

// #[inline(always)]
// fn pop_pure_handler(ctx: &mut VmCore<'_>) ->

// | OpCode::CALLGLOBALTAIL
// | OpCode::TAILCALL
// | OpCode::TCOJMP
// | OpCode::POPPURE

// MUL | SUB | DIV

#[inline(always)]
fn add_handler_int_int(_: &mut VmCore<'_>, l: isize, r: isize) -> isize {
    l + r
}

#[inline(always)]
fn add_handler_int_float(_: &mut VmCore<'_>, l: isize, r: f64) -> f64 {
    l as f64 + r
}

#[inline(always)]
fn add_handler_float_int(_: &mut VmCore<'_>, l: f64, r: isize) -> f64 {
    l + r as f64
}

#[inline(always)]
fn add_handler_float_float(_: &mut VmCore<'_>, l: f64, r: f64) -> f64 {
    l + r
}

#[inline(always)]
fn sub_handler_int_int(_: &mut VmCore<'_>, l: isize, r: isize) -> isize {
    l - r
}

#[inline(always)]
fn sub_handler_int_none(_: &mut VmCore<'_>, l: isize, r: SteelVal) -> Result<SteelVal> {
    match r {
        SteelVal::IntV(n) => Ok(SteelVal::IntV(l - n)),
        SteelVal::NumV(r) => Ok(SteelVal::NumV(l as f64 - r)),
        _ => stop!(TypeMismatch => "sub expected a number, found: {}", r),
    }
}

#[inline(always)]
fn sub_handler_none_int(_: &mut VmCore<'_>, l: SteelVal, r: isize) -> Result<SteelVal> {
    match l {
        SteelVal::IntV(l) => Ok(SteelVal::IntV(l - r)),
        SteelVal::NumV(l) => Ok(SteelVal::NumV(l - r as f64)),
        _ => {
            cold();
            stop!(TypeMismatch => "sub expected a number, found: {}", l)
        }
    }
}

#[inline(always)]
fn sub_handler_int_float(_: &mut VmCore<'_>, l: isize, r: f64) -> f64 {
    l as f64 - r
}

#[inline(always)]
fn sub_handler_float_int(_: &mut VmCore<'_>, l: f64, r: isize) -> f64 {
    l - r as f64
}

#[inline(always)]
fn sub_handler_float_float(_: &mut VmCore<'_>, l: f64, r: f64) -> f64 {
    l - r
}

#[inline(always)]
fn multiply_handler_int_int(_: &mut VmCore<'_>, l: isize, r: isize) -> isize {
    l * r
}

#[inline(always)]
fn multiply_handler_int_float(_: &mut VmCore<'_>, l: isize, r: f64) -> f64 {
    l as f64 * r
}

#[inline(always)]
fn multiply_handler_float_int(_: &mut VmCore<'_>, l: f64, r: isize) -> f64 {
    l * r as f64
}

#[inline(always)]
fn multiply_handler_float_float(_: &mut VmCore<'_>, l: f64, r: f64) -> f64 {
    l * r
}

#[inline(always)]
fn multiply_handler_int_none(_: &mut VmCore<'_>, l: isize, r: SteelVal) -> Result<SteelVal> {
    match r {
        SteelVal::NumV(r) => Ok(SteelVal::NumV(l as f64 * r)),
        SteelVal::IntV(n) => {
            if let Some(res) = l.checked_mul(n) {
                Ok(SteelVal::IntV(res))
            } else {
                let mut res = num::BigInt::from(l);
                res *= n;
                Ok(SteelVal::BigNum(Gc::new(res)))
            }
        }
        _ => stop!(TypeMismatch => "multiply expected an number, found: {}", r),
    }
}

#[inline(always)]
fn div_handler_int_int(_: &mut VmCore<'_>, l: isize, r: isize) -> isize {
    l / r
}

#[inline(always)]
fn div_handler_int_float(_: &mut VmCore<'_>, l: isize, r: f64) -> f64 {
    l as f64 / r
}

#[inline(always)]
fn div_handler_float_int(_: &mut VmCore<'_>, l: f64, r: isize) -> f64 {
    l / r as f64
}

#[inline(always)]
fn div_handler_float_float(_: &mut VmCore<'_>, l: f64, r: f64) -> f64 {
    l / r
}

#[inline(always)]
fn lte_handler_none_int(_: &mut VmCore<'_>, l: SteelVal, r: isize) -> Result<bool> {
    match l {
        SteelVal::IntV(l) => Ok(l <= r),
        SteelVal::NumV(l) => Ok(l <= r as f64),
        _ => stop!(TypeMismatch => "lte expected an number, found: {}", r),
    }
}

#[inline(always)]
fn add_handler_none_none(l: &SteelVal, r: &SteelVal) -> Result<SteelVal> {
    match (l, r) {
        (SteelVal::IntV(l), SteelVal::IntV(r)) => {
            if let Some(res) = l.checked_add(*r) {
                Ok(SteelVal::IntV(res))
            } else {
                let mut big = num::BigInt::default();

                big += *l;
                big += *r;

                big.into_steelval()
            }
        }
        (SteelVal::IntV(l), SteelVal::NumV(r)) => Ok(SteelVal::NumV(*l as f64 + r)),
        (SteelVal::NumV(l), SteelVal::IntV(r)) => Ok(SteelVal::NumV(l + *r as f64)),
        (SteelVal::NumV(l), SteelVal::NumV(r)) => Ok(SteelVal::NumV(l + r)),

        (SteelVal::BigNum(l), SteelVal::BigNum(r)) => (l.as_ref() + r.as_ref()).into_steelval(),

        (SteelVal::IntV(l), SteelVal::BigNum(r)) => (r.as_ref() + *l).into_steelval(),

        (SteelVal::BigNum(l), SteelVal::IntV(r)) => (l.as_ref() + *r).into_steelval(),

        (SteelVal::NumV(l), SteelVal::BigNum(r)) => Ok(SteelVal::NumV(r.to_f64().unwrap() + *l)),
        (SteelVal::BigNum(l), SteelVal::NumV(r)) => Ok(SteelVal::NumV(l.to_f64().unwrap() + *r)),

        _ => stop!(TypeMismatch => "+ expected two numbers, found: {} and {}", l, r),
    }
}

#[cfg(feature = "dynamic")]
pub(crate) use dynamic::pattern_exists;

#[macro_use]
#[cfg(feature = "dynamic")]
mod dynamic {
    use super::*;

    #[macro_export]
    macro_rules! binop_opcode_to_ssa_handler {
        (ADD2, Int, Int) => {
            add_handler_int_int
        };

        (ADD2, Int, Float) => {
            add_handler_int_float
        };

        (ADD2, Float, Int) => {
            add_handler_int_float
        };

        (ADD2, Float, Float) => {
            add_handler_float_float
        };

        (MUL2, Int, Int) => {
            multiply_handler_int_int
        };

        (MUL2, Int, Float) => {
            multiply_handler_int_float
        };

        (MUL2, Float, Int) => {
            multiply_handler_int_float
        };

        (MUL2, Float, Float) => {
            multiply_handler_float_float
        };

        (SUB2, Int, Int) => {
            sub_handler_int_int
        };

        (SUB2, Int, None) => {
            sub_handler_int_none
        };

        (SUB2, None, Int) => {
            sub_handler_none_int
        };

        (SUB2, Int, Float) => {
            sub_handler_int_float
        };

        (SUB2, Float, Int) => {
            sub_handler_float_int
        };

        (SUB2, Float, Float) => {
            sub_handler_float_float
        };

        (DIV2, Int, Int) => {
            div_handler_int_int
        };

        (DIV2, Int, Float) => {
            div_handler_int_float
        };

        (DIV2, Float, Int) => {
            div_handler_float_int
        };

        (DIV2, Float, Float) => {
            div_handler_float_float
        };

        (LTE2, None, Int) => {
            lte_handler_none_int
        };
    }

    macro_rules! if_to_ssa_handler {
        (IF, Bool) => {
            if_handler_with_bool
        };
        (IF) => {
            raw_if_handler
        };
    }

    macro_rules! opcode_to_ssa_handler {
        (CALLGLOBAL) => {
            call_global_handler_no_stack
        };

        (CALLGLOBAL, Tail) => {
            call_global_handler_with_args
        };

        (PUSHCONST) => {
            push_const_handler_no_stack
        };

        (MOVEREADLOCAL0) => {
            handle_move_local_0_no_stack
        };

        (MOVEREADLOCAL1) => {
            handle_move_local_1_no_stack
        };

        (READLOCAL0) => {
            handle_local_0_no_stack
        };

        (READLOCAL1) => {
            handle_local_1_no_stack
        };
    }

    // Includes the module as a dependency, that being said - this should
    // probably get generated into some specific sub module directly?
    include!(concat!(env!("OUT_DIR"), "/dynamic.rs"));
}
