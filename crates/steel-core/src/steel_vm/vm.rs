use crate::compiler::compiler::Compiler;
use crate::core::instructions::u24;
use crate::env::SharedVectorWrapper;
use crate::gc::shared::{
    MutContainer, ShareableMut, Shared, StandardShared, StandardSharedMut, WeakShared,
    WeakSharedMut,
};
use crate::parser::expander::BindingKind;
use crate::parser::replace_idents::expand_template;
use crate::primitives::lists::car;
use crate::primitives::lists::cdr;
use crate::primitives::lists::is_empty;
use crate::primitives::lists::steel_cons;
use crate::primitives::lists::steel_list_ref;
use crate::primitives::numbers::add_two;
use crate::primitives::vectors::vec_ref;
use crate::rvals::as_underlying_type;
use crate::rvals::cycles::BreadthFirstSearchSteelValVisitor;
use crate::rvals::number_equality;
use crate::rvals::AsRefMutSteelVal;
use crate::rvals::AsRefSteelVal;
use crate::rvals::BoxedAsyncFunctionSignature;
use crate::rvals::FromSteelVal as _;
use crate::rvals::SteelString;
use crate::steel_vm::primitives::steel_set_box_mutable;
use crate::steel_vm::primitives::steel_unbox_mutable;
use crate::steel_vm::primitives::{gt_primitive, gte_primitive, lt_primitive, steel_not};
use crate::values::closed::Heap;
use crate::values::closed::MarkAndSweepContext;
use crate::values::functions::CaptureVec;
use crate::values::functions::RootedInstructions;
use crate::values::functions::SerializedLambda;
use crate::values::structs::UserDefinedStruct;
use crate::values::transducers::Reducer;
use crate::{
    compiler::constants::ConstantMap,
    core::{instructions::DenseInstruction, opcode::OpCode},
};
use crate::{
    compiler::program::Executable,
    primitives::{add_primitive, divide_primitive, multiply_primitive, subtract_primitive},
    steel_vm::primitives::{equality_primitive, lte_primitive},
    values::transducers::Transducers,
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
use std::io::Read as _;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::sync::Mutex;
use std::{cell::RefCell, collections::HashMap, iter::Iterator};

use super::engine::EngineId;

use crossbeam_utils::atomic::AtomicCell;
#[cfg(feature = "profiling")]
use log::{debug, log_enabled};
use num_bigint::BigInt;
use num_traits::CheckedSub;
use parking_lot::RwLock;
use smallvec::SmallVec;
#[cfg(feature = "profiling")]
use std::time::Instant;
use steel_parser::interner::InternedString;
use threads::ThreadHandle;

use crate::rvals::{into_serializable_value, IntoSteelVal};

pub(crate) mod threads;

#[cfg(feature = "jit2")]
pub mod jit;

pub use threads::{mutex_lock, mutex_unlock};

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

const STACK_LIMIT: usize = 10000000;
const _JIT_THRESHOLD: usize = 100;

const _USE_SUPER_INSTRUCTIONS: bool = false;
const CHECK_STACK_OVERFLOW: bool = true;

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

#[derive(Debug, Clone)]
pub struct StackFrameAttachments {
    pub(crate) handler: Option<SteelVal>,
    weak_continuation_mark: Option<WeakContinuation>,
}

// This should be the go to thing for handling basically everything we need
// Then - do I want to always reference the last one, or just refer to the current one?
// TODO: We'll need to add these functions to the GC as well

#[derive(Debug, Clone)]
pub struct StackFrame {
    sp: usize,

    pub(crate) function: Gc<ByteCodeLambda>,

    ip: usize,

    instructions: RootedInstructions,

    pub(crate) attachments: Option<Box<StackFrameAttachments>>,
}

impl Eq for StackFrame {}

impl PartialEq for StackFrame {
    fn eq(&self, other: &Self) -> bool {
        self.sp == other.sp
            && self.attachments.as_ref().map(|x| &x.handler)
                == other.attachments.as_ref().map(|x| &x.handler)
            && self.ip == other.ip
            && self.instructions == other.instructions
            && self.function == other.function
    }
}

#[test]
fn check_sizes() {
    println!("stack frame: {:?}", std::mem::size_of::<StackFrame>());
    println!(
        "option rc steelval: {:?}",
        std::mem::size_of::<Option<std::rc::Rc<SteelVal>>>()
    );
    println!(
        "option box steelval: {:?}",
        std::mem::size_of::<Option<Box<SteelVal>>>()
    );
    println!(
        "option steelval: {:?}",
        std::mem::size_of::<Option<SteelVal>>()
    );
}

thread_local! {
    static THE_EMPTY_INSTRUCTION_SET: StandardShared<[DenseInstruction]> = StandardShared::from([]);
}

impl StackFrame {
    #[inline(always)]
    pub fn new(
        stack_index: usize,
        function: Gc<ByteCodeLambda>,
        ip: usize,
        instructions: RootedInstructions,
    ) -> Self {
        Self {
            sp: stack_index,
            function,
            ip,
            instructions,
            attachments: None,
        }
    }

    fn with_continuation_mark(mut self, continuation_mark: Continuation) -> Self {
        // self.weak_continuation_mark = Some(WeakContinuation::from_strong(&continuation_mark));
        match &mut self.attachments {
            Some(attachments) => {
                attachments.weak_continuation_mark =
                    Some(WeakContinuation::from_strong(&continuation_mark));
            }

            None => {
                self.attachments = Some(Box::new(StackFrameAttachments {
                    handler: None,
                    weak_continuation_mark: Some(WeakContinuation::from_strong(&continuation_mark)),
                }))
            }
        }

        self
    }

    pub fn main() -> Self {
        let function = Gc::new(ByteCodeLambda::main(Vec::new()));
        StackFrame::new(
            0,
            function,
            0,
            RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
        )
    }

    #[inline(always)]
    pub fn set_function(&mut self, function: Gc<ByteCodeLambda>) {
        self.function = function;
    }

    #[inline(always)]
    pub fn set_continuation(&mut self, continuation: &Continuation) {
        // self.weak_continuation_mark = Some(WeakContinuation::from_strong(&continuation));

        match &mut self.attachments {
            Some(attachments) => {
                attachments.weak_continuation_mark =
                    Some(WeakContinuation::from_strong(continuation));
            }

            None => {
                self.attachments = Some(Box::new(StackFrameAttachments {
                    handler: None,
                    weak_continuation_mark: Some(WeakContinuation::from_strong(continuation)),
                }))
            }
        }
    }

    pub fn with_handler(mut self, handler: SteelVal) -> Self {
        // self.handler = Some(Shared::new(handler));

        match &mut self.attachments {
            Some(attachments) => {
                attachments.handler = Some(handler);
            }

            None => {
                self.attachments = Some(Box::new(StackFrameAttachments {
                    handler: Some(handler),
                    weak_continuation_mark: None,
                }))
            }
        }
        self
    }
}

thread_local! {
    pub(crate) static DEFAULT_CONSTANT_MAP: ConstantMap = ConstantMap::new();
}

#[derive(Copy, Clone, Default, Debug)]
pub enum ThreadState {
    #[default]
    Running,
    Interrupted,
    Suspended,
    PausedAtSafepoint,
}

// Multiple buffers
// / When each one fills, send it to the back
// static BACKGROUND_DROPPER: LazyLock<DropBufferPool> = LazyLock::new(|| {
//     let (forward_sender, forward_receiver) = crossbeam_channel::bounded::<Vec<_>>(6);
//     let (backward_sender, backward_receiver) = crossbeam_channel::bounded(6);

//     std::thread::spawn(move || {
//         // Have many ready in the queue
//         backward_sender.send(Vec::with_capacity(1024)).unwrap();
//         backward_sender.send(Vec::with_capacity(1024)).unwrap();
//         backward_sender.send(Vec::with_capacity(1024)).unwrap();
//         backward_sender.send(Vec::with_capacity(1024)).unwrap();
//         backward_sender.send(Vec::with_capacity(1024)).unwrap();

//         for mut vec in forward_receiver {
//             vec.clear();

//             backward_sender.send(vec).unwrap()
//         }
//     });

//     DropBufferPool {
//         forward_sender,
//         backward_receiver,
//     }
// });

// struct DropBufferPool {
//     forward_sender: Sender<Vec<SteelVal>>,
//     backward_receiver: Receiver<Vec<SteelVal>>,
// }

// #[derive(Clone)]
// pub(crate) struct DelayedDropper {
//     buffer: Vec<SteelVal>,
// }

// impl DelayedDropper {
//     // Attempt to just write to a local buffer
//     // to avoid drops.
//     fn new() -> Self {
//         Self {
//             buffer: Vec::with_capacity(1024),
//         }
//     }

//     // Extend with a drain?
//     pub fn push(&mut self, value: SteelVal) {
//         // If we're going to reallocate,
//         // request a new buffer from the back.
//         if self.buffer.len() == 1024 {
//             // Atomic swap with a background thread?

//             BACKGROUND_DROPPER
//                 .forward_sender
//                 .send(std::mem::take(&mut self.buffer))
//                 .unwrap();
//             self.buffer = BACKGROUND_DROPPER.backward_receiver.recv().unwrap();
//         } else {
//             self.buffer.push(value);
//         }
//     }
// }

/// The thread execution context
#[derive(Clone)]
#[repr(C)]
pub struct SteelThread {
    // TODO: Figure out how to best broadcast changes
    // to the rest of the world? Right now pausing threads
    // means we can get away with one environment that is
    // shared, but in reality this should just be
    pub(crate) global_env: Env,
    pub(crate) stack: Vec<SteelVal>,

    #[cfg(feature = "dynamic")]
    profiler: OpCodeOccurenceProfiler,

    pub(crate) function_interner: FunctionInterner,
    pub(crate) heap: Arc<Mutex<Heap>>,
    pub(crate) runtime_options: RunTimeOptions,
    pub(crate) current_frame: StackFrame,
    pub(crate) stack_frames: Vec<StackFrame>,
    pub(crate) constant_map: ConstantMap,
    pub(crate) interrupted: Option<Arc<AtomicBool>>,
    pub(crate) synchronizer: Synchronizer,
    // This will be static, for the thread.
    pub(crate) thread_local_storage: Vec<SteelVal>,

    // Store... more stuff here
    pub(crate) compiler: std::sync::Arc<RwLock<Compiler>>,

    pub(crate) id: EngineId,

    pub(crate) safepoints_enabled: bool,

    #[cfg(feature = "jit2")]
    pub(crate) jit: Arc<Mutex<crate::jit2::gen::JIT>>,
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

// TODO: This object probably needs to be shared as well
#[derive(Default, Clone)]
pub struct FunctionInterner {
    closure_interner: rustc_hash::FxHashMap<u32, ByteCodeLambda>,
    pub(crate) pure_function_interner: rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,
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
    pub(crate) spans: rustc_hash::FxHashMap<u32, Shared<[Span]>>,

    #[cfg(feature = "jit2")]
    jit_funcs: rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,
}

#[derive(Clone, Default)]
pub struct ThreadStateController {
    paused: Arc<AtomicBool>,
    state: Arc<AtomicCell<ThreadState>>,
}

impl ThreadStateController {
    pub fn suspend(&self) {
        self.paused
            .store(true, std::sync::atomic::Ordering::Relaxed);
        self.state.store(ThreadState::Suspended)
    }

    pub fn pause_for_safepoint(&self) {
        self.paused
            .store(true, std::sync::atomic::Ordering::Relaxed);
        self.state.store(ThreadState::PausedAtSafepoint)
    }

    pub fn resume(&self) {
        self.paused
            .store(false, std::sync::atomic::Ordering::Relaxed);
        self.state.store(ThreadState::Running)
    }

    pub fn interrupt(&self) {
        self.paused
            .store(true, std::sync::atomic::Ordering::Relaxed);
        self.state.store(ThreadState::Interrupted)
    }
}

#[derive(Clone)]
pub(crate) struct ThreadContext {
    pub(crate) ctx: std::sync::Weak<AtomicCell<Option<*mut SteelThread>>>,
    pub(crate) handle: SteelVal,
}

#[derive(Clone)]
pub struct Synchronizer {
    // All of the threads that have been created
    // from the root of the runtime. Since we're now operating
    // in a world in which these kinds of threads might basically
    // share memory space, we probably need to handle this
    pub(crate) threads: Arc<Mutex<Vec<ThreadContext>>>,
    // The signal to actually tell all the threads to stop
    pub(crate) state: ThreadStateController,

    // If we're at a safe point, then this will include a _live_ pointer
    // to the context. Once we exit the safe point, we're done.
    pub(crate) ctx: Arc<AtomicCell<Option<*mut SteelThread>>>,

    spawned_via_make_thread: bool,
}

// TODO: Until I figure out how to note have this be the case
unsafe impl Sync for Synchronizer {}
unsafe impl Send for Synchronizer {}

impl Synchronizer {
    pub fn new() -> Self {
        Self {
            threads: Arc::new(Mutex::new(Vec::new())),
            state: ThreadStateController {
                paused: Arc::new(AtomicBool::new(false)),
                state: Arc::new(AtomicCell::new(ThreadState::Running)),
            },
            ctx: Arc::new(AtomicCell::new(None)),
            spawned_via_make_thread: false,
        }
    }

    pub(crate) unsafe fn maybe_call_per_ctx(
        &self,
        mut func: impl FnMut(&mut SteelThread),
        timeout_ms: u128,
    ) {
        let guard = self.threads.lock().unwrap();

        // IMPORTANT - This needs to be all threads except the currently
        // executing one.
        for ThreadContext { ctx, handle } in guard.iter() {
            if let Some(ctx) = ctx.upgrade() {
                if Arc::ptr_eq(&ctx, &self.ctx) {
                    continue;
                }

                let now = std::time::Instant::now();

                // TODO: Have to use a condvar
                while now.elapsed().as_millis() < timeout_ms {
                    if let Some(ctx) = ctx.load() {
                        unsafe {
                            let live_ctx = &mut (*ctx);
                            (func)(live_ctx)
                        }

                        break;
                    } else {
                        if let SteelVal::Custom(c) = &handle {
                            if let Some(inner) =
                                as_underlying_type::<ThreadHandle>(c.read().as_ref())
                            {
                                if let Some(forked_thread_handle) = &inner.forked_thread_handle {
                                    if let Some(upgraded) = forked_thread_handle.upgrade() {
                                        if let Ok(mut live_ctx) = upgraded.try_lock() {
                                            (func)(&mut live_ctx);

                                            break;
                                        }
                                    }
                                }
                            }
                        }

                        // TODO:
                        // If the thread was spawned via the make_thread function,
                        // then we need to check if that thread is even running to begin with.

                        // println!("Waiting for thread...")
                    }
                }
            } else {
                continue;
            }
        }
    }

    pub(crate) unsafe fn call_per_ctx(&self, mut func: impl FnMut(&mut SteelThread)) {
        let guard = self.threads.lock().unwrap();

        // IMPORTANT - This needs to be all threads except the currently
        // executing one.
        for ThreadContext { ctx, handle } in guard.iter() {
            if let Some(ctx) = ctx.upgrade() {
                if Arc::ptr_eq(&ctx, &self.ctx) {
                    continue;
                }

                // TODO: Have to use a condvar
                loop {
                    if let Some(ctx) = ctx.load() {
                        // println!("Broadcasting `set!` operation");

                        unsafe {
                            let live_ctx = &mut (*ctx);
                            (func)(live_ctx)
                        }

                        break;
                    } else {
                        if let SteelVal::Custom(c) = &handle {
                            if let Some(inner) =
                                as_underlying_type::<ThreadHandle>(c.read().as_ref())
                            {
                                if let Some(forked_thread_handle) = &inner.forked_thread_handle {
                                    if let Some(upgraded) = forked_thread_handle.upgrade() {
                                        if let Ok(mut live_ctx) = upgraded.try_lock() {
                                            (func)(&mut live_ctx);

                                            break;
                                        }
                                    }
                                }
                            }
                        }

                        // TODO:
                        // If the thread was spawned via the make_thread function,
                        // then we need to check if that thread is even running to begin with.

                        // println!("Waiting for thread...")
                    }
                }
            } else {
                continue;
            }
        }
    }

    pub(crate) unsafe fn enumerate_stacks(&mut self, context: &mut MarkAndSweepContext) {
        // TODO: Continue...
        let guard = self.threads.lock().unwrap();

        // Wait for all the threads to be legal
        for ThreadContext { ctx, handle } in guard.iter() {
            if let Some(ctx) = ctx.upgrade() {
                // Don't pause myself, enter safepoint from main thread?
                if Arc::ptr_eq(&ctx, &self.ctx) {
                    continue;
                }

                // TODO: Have to use a condvar
                loop {
                    if let Some(ctx) = ctx.load() {
                        log::debug!("Sweeping other threads");

                        unsafe {
                            let live_ctx = &(*ctx);
                            for value in &live_ctx.stack {
                                context.push_back(value.clone());
                            }

                            for frame in &live_ctx.stack_frames {
                                for value in frame.function.captures() {
                                    context.push_back(value.clone());
                                }
                            }

                            for value in live_ctx.current_frame.function.captures() {
                                context.push_back(value.clone());
                            }

                            for value in &live_ctx.thread_local_storage {
                                context.push_back(value.clone());
                            }

                            context.visit();
                        }

                        break;
                    } else {
                        log::debug!("Waiting for thread...");

                        if let SteelVal::Custom(c) = &handle {
                            if let Some(inner) =
                                as_underlying_type::<ThreadHandle>(c.read().as_ref())
                            {
                                if let Some(forked_thread_handle) = &inner.forked_thread_handle {
                                    if let Some(upgraded) = forked_thread_handle.upgrade() {
                                        if let Ok(live_ctx) = upgraded.try_lock() {
                                            for value in &live_ctx.stack {
                                                context.push_back(value.clone());
                                            }

                                            for frame in &live_ctx.stack_frames {
                                                for value in frame.function.captures() {
                                                    context.push_back(value.clone());
                                                }
                                            }

                                            for value in live_ctx.current_frame.function.captures()
                                            {
                                                context.push_back(value.clone());
                                            }

                                            for value in &live_ctx.thread_local_storage {
                                                context.push_back(value.clone());
                                            }

                                            context.visit();

                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                continue;
            }
        }
    }

    /// Stops all threads within the context of this virtual machine, and
    /// waits for all of those threads to stop before continuing on.
    pub fn stop_threads(&mut self) {
        self.state.pause_for_safepoint();

        // Stop other threads, wait until we've gathered acknowledgements
        self.threads.lock().unwrap().iter().for_each(|x| {
            if let SteelVal::Custom(c) = &x.handle {
                if let Some(inner) = as_underlying_type::<ThreadHandle>(c.read().as_ref()) {
                    inner.thread_state_manager.pause_for_safepoint();
                }
            }
        });
    }

    pub fn resume_threads(&mut self) {
        self.state.resume();

        // Go through, and resume all of the threads
        self.threads.lock().unwrap().iter().for_each(|x| {
            if let SteelVal::Custom(c) = &x.handle {
                if let Some(inner) = as_underlying_type::<ThreadHandle>(c.read().as_ref()) {
                    inner.thread_state_manager.resume();
                    inner.thread.unpark();
                }
            }
        });
    }
}

impl SteelThread {
    pub fn new(compiler: std::sync::Arc<RwLock<Compiler>>) -> SteelThread {
        let synchronizer = Synchronizer::new();
        let weak_ctx = Arc::downgrade(&synchronizer.ctx);

        // Get a handle to the current thread?
        let handle = ThreadHandle {
            handle: Mutex::new(None),
            thread: std::thread::current(),
            thread_state_manager: synchronizer.state.clone(),
            forked_thread_handle: None,
        }
        .into_steelval()
        .unwrap();

        // TODO: Entering safepoint should happen often
        // for the main thread?
        synchronizer.threads.lock().unwrap().push(ThreadContext {
            ctx: weak_ctx,
            handle,
        });

        SteelThread {
            global_env: Env::root(),
            stack: Vec::with_capacity(128),

            #[cfg(feature = "dynamic")]
            profiler: OpCodeOccurenceProfiler::new(),

            function_interner: FunctionInterner::default(),
            // _super_instructions: Vec::new(),
            heap: Arc::new(Mutex::new(Heap::new())),
            runtime_options: RunTimeOptions::new(),
            stack_frames: Vec::with_capacity(128),
            current_frame: StackFrame::main(),
            // Should probably just have this be Option<ConstantMap> - but then every time we look up
            // something we'll have to deal with the fact that its wrapped in an option. Another options
            // Would just have all programs compiled in this thread just share a constant map. For now,
            // we'll have each thread default to an empty constant map, and replace it with the map bundled
            // with the executables
            constant_map: DEFAULT_CONSTANT_MAP.with(|x| x.clone()),
            interrupted: Default::default(),
            synchronizer,
            thread_local_storage: Vec::new(),
            compiler,
            id: EngineId::new(),

            // Only incur the cost of the actual safepoint behavior
            // if multiple threads are enabled
            safepoints_enabled: false,

            #[cfg(feature = "jit2")]
            jit: Arc::new(Mutex::new(crate::jit2::gen::JIT::default())),
            // delayed_dropper: DelayedDropper::new(),
        }
    }

    #[inline(always)]
    fn local_set(&mut self, idx: usize, val: SteelVal) -> Result<SteelVal> {
        self.global_env.repl_set_idx(idx, val)
    }

    #[cfg(feature = "sync")]
    pub(crate) fn with_locked_env<T, F: FnOnce(&mut Self, &mut SharedVectorWrapper) -> T>(
        &mut self,
        thunk: F,
    ) -> T {
        // log::info!("Stopping threads...");
        self.synchronizer.stop_threads();
        // log::info!("Stopped threads.");

        let mut env = self.global_env.drain_env();

        unsafe {
            self.synchronizer.call_per_ctx(|thread| {
                thread.global_env.default_env();
            });
        }

        let out = thunk(self, &mut env);

        unsafe {
            self.synchronizer.call_per_ctx(|thread| {
                thread.global_env.update_env(env.clone());
            });
        }

        self.global_env.update_env(env);

        // log::info!("Resuming threads...");

        // Resume.
        // Apply these to all of the things.
        self.synchronizer.resume_threads();
        // log::info!("Threads resumed.");

        out
    }

    #[cfg(not(feature = "sync"))]
    pub(crate) fn with_locked_env<T, F: FnOnce(&mut Self) -> T>(&mut self, thunk: F) -> T {
        thunk(self)
    }

    // Allow this thread to be available for garbage collection
    // during the duration of the provided thunk
    #[inline(always)]
    pub fn enter_safepoint(
        &mut self,
        mut finish: impl FnMut(&SteelThread) -> Result<SteelVal>,
    ) -> Result<SteelVal> {
        // TODO:
        // Only need to actually enter the safepoint if another
        // thread exists

        if cfg!(feature = "sync") && self.safepoints_enabled {
            let ptr = self as _;
            self.synchronizer.ctx.store(Some(ptr));
        }

        let res = finish(self);

        if cfg!(feature = "sync") && self.safepoints_enabled {
            // Just block here until we're out - this only applies if we're not the main thread and
            // not in garbage collection
            while self
                .synchronizer
                .state
                .paused
                .load(std::sync::atomic::Ordering::Relaxed)
            {
                if let ThreadState::Interrupted = self.synchronizer.state.state.load() {
                    break;
                }

                // Only park the thread if we actually have the ability to unpark it
                if !self.synchronizer.spawned_via_make_thread {
                    std::thread::park();
                }
            }

            self.synchronizer.ctx.store(None);
        }

        res
    }

    pub fn with_interrupted(&mut self, interrupted: Arc<AtomicBool>) -> &mut Self {
        self.interrupted = Some(interrupted);
        self
    }

    // If you want to explicitly turn off contracts, you can do so
    pub fn with_contracts(&mut self, contracts: bool) -> &mut Self {
        self.runtime_options.contracts_on = contracts;
        self
    }

    pub fn insert_binding(&mut self, idx: usize, value: SteelVal) {
        // TODO: Do the thing where we also pause the threads
        // to broadcast all the values!

        #[cfg(feature = "sync")]
        self.with_locked_env(move |_, env| {
            env.repl_define_idx(idx, value);
        });

        #[cfg(not(feature = "sync"))]
        self.with_locked_env(move |this| {
            this.global_env.repl_define_idx(idx, value);
        });
    }

    pub fn extract_value(&self, idx: usize) -> Option<SteelVal> {
        // self.global_env.extract(idx)
        self.global_env.repl_maybe_lookup_idx(idx)
    }

    // Run the executable
    pub fn run_executable(&mut self, program: &Executable) -> Result<Vec<SteelVal>> {
        let Executable {
            instructions,
            constant_map,
            spans,
            ..
        } = program;

        self.constant_map = constant_map.clone();

        // dbg!(&self.stack);
        // dbg!(&self.stack_frames);
        // dbg!(&self.current_frame);

        let result = instructions
            .iter()
            .zip(spans.iter())
            .map(|x| {
                self.execute(
                    StandardShared::clone(x.0),
                    constant_map.clone(),
                    Shared::clone(x.1),
                )
            })
            .collect();

        self.constant_map = DEFAULT_CONSTANT_MAP.with(|x| x.clone());

        #[cfg(feature = "op-code-profiling")]
        super::profiling::profiling_report();

        result
    }

    #[allow(unused)]
    pub fn call_fn_from_mut_slice(
        &mut self,
        function: SteelVal,
        args: &mut [SteelVal],
    ) -> Result<SteelVal> {
        let constants = self.compiler.read().constant_map.clone();

        self.call_function_from_mut_slice(constants, function, args)
    }

    pub(crate) fn call_function_from_mut_slice(
        &mut self,
        constant_map: ConstantMap,
        function: SteelVal,
        args: &mut [SteelVal],
    ) -> Result<SteelVal> {
        match function {
            SteelVal::FuncV(func) => func(args).map_err(|x| x.set_span_if_none(Span::default())),
            SteelVal::BoxedFunction(func) => {
                func.func()(args).map_err(|x| x.set_span_if_none(Span::default()))
            }
            // SteelVal::ContractedFunction(cf) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     cf.apply(arg_vec, cur_inst_span, self)
            // }
            SteelVal::MutFunc(func) => func(args).map_err(|x| x.set_span_if_none(Span::default())),
            // SteelVal::BuiltIn(func) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            // }
            SteelVal::CustomStruct(ref s) => {
                if let Some(procedure) = s.maybe_proc() {
                    if let SteelVal::HeapAllocated(h) = procedure {
                        self.call_function_from_mut_slice(constant_map, h.get(), args)
                    } else {
                        self.call_function_from_mut_slice(constant_map, procedure.clone(), args)
                    }
                } else {
                    stop!(TypeMismatch => format!("application not a procedure: {function}"))
                }
            }

            SteelVal::Closure(closure) => {
                // Create phony span vec
                let spans = closure
                    .body_exp()
                    .iter()
                    .map(|_| Span::default())
                    .collect::<Vec<_>>();

                let mut vm_instance = VmCore::new_unchecked(
                    // Shared::new([]),
                    RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
                    constant_map,
                    self,
                    &spans,
                );

                vm_instance.call_with_args(&closure, args.iter().cloned())
            }
            _ => {
                stop!(TypeMismatch => format!("application not a procedure: {function}"))
            }
        }
    }

    pub(crate) fn call_function(
        &mut self,
        constant_map: ConstantMap,
        function: SteelVal,
        mut args: Vec<SteelVal>,
    ) -> Result<SteelVal> {
        match function {
            SteelVal::FuncV(func) => func(&args).map_err(|x| x.set_span_if_none(Span::default())),
            SteelVal::BoxedFunction(func) => {
                func.func()(&args).map_err(|x| x.set_span_if_none(Span::default()))
            }
            // SteelVal::ContractedFunction(cf) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     cf.apply(arg_vec, cur_inst_span, self)
            // }
            SteelVal::MutFunc(func) => {
                func(&mut args).map_err(|x| x.set_span_if_none(Span::default()))
            }
            // SteelVal::BuiltIn(func) => {
            //     let arg_vec: Vec<_> = args.into_iter().collect();
            //     func(self, &arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
            // }
            SteelVal::CustomStruct(ref s) => {
                if let Some(procedure) = s.maybe_proc() {
                    if let SteelVal::HeapAllocated(h) = procedure {
                        self.call_function(constant_map, h.get(), args)
                    } else {
                        self.call_function(constant_map, procedure.clone(), args)
                    }
                } else {
                    stop!(TypeMismatch => format!("application not a procedure: {function}"))
                }
            }
            SteelVal::ContinuationFunction(c) => {
                let mut vm_instance = VmCore::new_unchecked(
                    RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
                    constant_map,
                    self,
                    &[],
                );

                vm_instance.call_cont_with_args(c, args)
            }
            SteelVal::Closure(closure) => {
                // TODO: Revisit if we need this phony span vec!
                let spans = closure
                    .body_exp()
                    .iter()
                    .map(|_| Span::default())
                    .collect::<Vec<_>>();

                let mut vm_instance = VmCore::new_unchecked(
                    RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
                    constant_map,
                    self,
                    &spans,
                );

                vm_instance.call_with_args(&closure, args)
            }
            _ => {
                stop!(TypeMismatch => format!("application not a procedure: {function}"))
            }
        }
    }

    pub fn execute(
        &mut self,
        instructions: StandardShared<[DenseInstruction]>,
        constant_map: ConstantMap,
        spans: Shared<[Span]>,
    ) -> Result<SteelVal> {
        #[cfg(feature = "dynamic")]
        self.profiler.reset();

        #[cfg(feature = "profiling")]
        let execution_time = Instant::now();

        let keep_alive = instructions.clone();

        self.current_frame
            .set_function(Gc::new(ByteCodeLambda::rooted(keep_alive.clone())));

        let raw_keep_alive = StandardShared::into_raw(keep_alive);

        // TODO: Figure out how to keep the first set of instructions around
        // during a continuation? Does it get allocated into something? If its the
        // root one, we should move it into a function?
        let mut vm_instance = VmCore::new(
            RootedInstructions::new(instructions),
            constant_map,
            self,
            &spans,
        )?;

        // This is our pseudo "dynamic unwind"
        // If we need to, we'll walk back on the stack and find any handlers to pop
        'outer: loop {
            let result = vm_instance.vm().map_err(|error| {
                error
                    .set_span_if_none(vm_instance.current_span())
                    .with_stack_trace(vm_instance.snapshot_stack_trace())
            });

            if let Err(e) = result {
                while let Some(mut last) = vm_instance.thread.stack_frames.pop() {
                    // Unwind the stack, close continuation marks here!
                    // For whatever reason - if we're at the top, we shouldn't go down below 0
                    if vm_instance.pop_count == 0 {
                        return Err(e);
                    }

                    // Drop the pop count along with everything else we're doing
                    vm_instance.pop_count -= 1;

                    if last
                        .attachments
                        .as_mut()
                        .and_then(|x| x.weak_continuation_mark.take())
                        .is_some()
                    {
                        vm_instance.thread.stack.truncate(last.sp as _);
                        vm_instance.ip = last.ip;
                        vm_instance.sp = vm_instance.get_last_stack_frame_sp();

                        // vm_instance.instructions = Rc::clone(&last.instructions);
                        vm_instance.instructions = last.instructions.clone();
                        vm_instance.close_continuation_marks(&last);
                    }

                    if let Some(handler) = last.attachments.as_mut().and_then(|x| x.handler.take())
                    {
                        // Drop the stack BACK to where it was on this level
                        vm_instance.thread.stack.truncate(last.sp);
                        vm_instance.thread.stack.push(e.into_steelval()?);

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
                                        RootedInstructions::new(
                                            THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone()),
                                        ),
                                    ));
                                }

                                vm_instance.sp = last.sp;
                                vm_instance.instructions = closure.body_exp();

                                if let Some(attachments) = &mut last.attachments {
                                    attachments.handler = None;
                                } else {
                                    panic!("This shouldn't happen")
                                }

                                last.function = closure.clone();

                                vm_instance.ip = 0;

                                // Put this back as the last stack frame
                                vm_instance.thread.stack_frames.push(last);

                                vm_instance.pop_count += 1;
                            }
                            _ => {
                                stop!(TypeMismatch => "expected a function for the exception handler, found: {}", handler)
                            }
                        }

                        continue 'outer;
                    }
                }

                self.stack.clear();
                unsafe { StandardShared::from_raw(raw_keep_alive) };

                return Err(e);
            } else {
                for frame in &vm_instance.thread.stack_frames {
                    Continuation::close_marks(&vm_instance, frame);
                }

                // Clean up
                self.stack.clear();
                unsafe { StandardShared::from_raw(raw_keep_alive) };

                return result;
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpenContinuationMark {
    // Lazily capture the frames we need to?
    pub(crate) current_frame: StackFrame,
    pub(crate) stack_frame_offset: usize,
    instructions: RootedInstructions,

    // Captured at creation, everything on the stack
    // from the current frame
    pub(crate) current_stack_values: Vec<SteelVal>,

    ip: usize,
    sp: usize,
    pop_count: usize,

    #[cfg(debug_assertions)]
    closed_continuation: ClosedContinuation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
// TODO: This should replace the continuation value.
pub enum ContinuationMark {
    Closed(ClosedContinuation),
    Open(OpenContinuationMark),
}

impl ContinuationMark {
    pub fn close(&mut self, ctx: &VmCore<'_>) {
        match self {
            ContinuationMark::Closed(_) => {}
            ContinuationMark::Open(open) => {
                let mut continuation = ctx.new_closed_continuation_from_state();

                // continuation.stack.truncate(open.stack_frame_offset);
                continuation.stack.truncate(open.sp);
                continuation.stack.append(&mut open.current_stack_values);

                continuation.ip = open.ip;
                continuation.sp = open.sp;
                continuation.pop_count = open.pop_count;
                // continuation.instructions = Shared::clone(&open.instructions);
                continuation.instructions = open.instructions.clone();

                continuation.current_frame = open.current_frame.clone();

                #[cfg(debug_assertions)]
                {
                    debug_assert_eq!(
                        open.closed_continuation.stack.len(),
                        continuation.stack.len()
                    );
                    debug_assert_eq!(
                        open.closed_continuation.stack_frames.len(),
                        continuation.stack_frames.len()
                    );
                    debug_assert_eq!(
                        open.closed_continuation.stack_frames,
                        continuation.stack_frames
                    );
                }

                *self = ContinuationMark::Closed(continuation);
            }
        }
    }

    fn into_open_mark(self) -> Option<OpenContinuationMark> {
        if let Self::Open(open) = self {
            Some(open)
        } else {
            None
        }
    }

    fn into_closed(self) -> Option<ClosedContinuation> {
        if let Self::Closed(closed) = self {
            Some(closed)
        } else {
            None
        }
    }
}

impl Continuation {
    #[inline(always)]
    pub fn close_marks(ctx: &VmCore<'_>, stack_frame: &StackFrame) -> bool {
        // if let Some(cont_mark) = stack_frame
        //     .weak_continuation_mark
        //     .as_ref()
        //     .and_then(|x| WeakShared::upgrade(&x.inner))
        // {

        if let Some(cont_mark) = stack_frame.attachments.as_ref().and_then(|x| {
            x.weak_continuation_mark
                .as_ref()
                .and_then(|x| WeakShared::upgrade(&x.inner))
        }) {
            cont_mark.write().close(ctx);

            return true;
        }

        false
    }

    pub fn set_state_from_continuation(ctx: &mut VmCore<'_>, this: Self) {
        // Check if this is an open
        let maybe_open_mark = (*this.inner.read()).clone().into_open_mark();

        if let Some(open) = maybe_open_mark {
            let strong_count = StandardShared::strong_count(&this.inner);
            let weak_count = StandardShared::weak_count(&this.inner);

            while let Some(stack_frame) = ctx.thread.stack_frames.pop() {
                ctx.pop_count -= 1;

                // if let Some(mark) = &stack_frame
                //     .weak_continuation_mark
                //     .as_ref()
                //     .and_then(|x| WeakShared::upgrade(&x.inner))
                // {

                if let Some(mark) = &stack_frame.attachments.as_ref().and_then(|x| {
                    x.weak_continuation_mark
                        .as_ref()
                        .and_then(|x| WeakShared::upgrade(&x.inner))
                }) {
                    if StandardShared::ptr_eq(mark, &this.inner) {
                        if weak_count == 1
                            && strong_count > 1
                            && Self::close_marks(ctx, &stack_frame)
                        {
                            // TODO: We shouldn't have to both close the frame and also
                            // set state from the continuation in both spots. There is a nefarious
                            // bug here that I haven't yet resolved.
                            let definitely_closed =
                                this.inner.read().clone().into_closed().unwrap();

                            ctx.set_state_from_continuation(definitely_closed);

                            return;
                        }

                        ctx.sp = open.sp;
                        ctx.ip = open.ip;
                        // ctx.instructions = Shared::clone(&open.instructions);

                        ctx.instructions = open.instructions.clone();

                        ctx.thread.stack.truncate(open.sp);

                        // TODO: Probably move the pointer for the stack frame as well?
                        ctx.thread.stack.extend(open.current_stack_values.clone());

                        #[cfg(debug_assertions)]
                        {
                            debug_assert_eq!(ctx.sp, open.closed_continuation.sp);
                            debug_assert_eq!(ctx.ip, open.closed_continuation.ip);
                            debug_assert_eq!(
                                ctx.instructions,
                                open.closed_continuation.instructions
                            );
                            debug_assert_eq!(
                                ctx.thread.stack.len(),
                                open.closed_continuation.stack.len()
                            );

                            debug_assert_eq!(ctx.pop_count, open.closed_continuation.pop_count);

                            debug_assert_eq!(
                                ctx.thread.stack_frames,
                                open.closed_continuation.stack_frames
                            );
                        }

                        return;
                    }
                }

                ctx.thread.stack.truncate(stack_frame.sp);
                ctx.ip = stack_frame.ip;
                ctx.sp = ctx.get_last_stack_frame_sp();
                // ctx.instructions = Shared::clone(&stack_frame.instructions);

                ctx.instructions = stack_frame.instructions.clone();

                if Self::close_marks(ctx, &stack_frame) {
                    // println!("CLOSING MARKS WHEN SETTING STATE FROM CONTINUATION");
                }
            }

            panic!("Failed to find an open continuation on the stack");
        } else {
            match StandardShared::try_unwrap(this.inner).map(|x| x.into_inner()) {
                Ok(cont) => {
                    ctx.set_state_from_continuation(cont.into_closed().unwrap());
                }
                Err(e) => {
                    let definitely_closed = e.read().clone().into_closed().unwrap();

                    ctx.set_state_from_continuation(definitely_closed);
                }
            }
        }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        StandardShared::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Clone, Debug)]
pub struct Continuation {
    // TODO: This _might_ need to be a weak reference. We'll see!
    pub(crate) inner: StandardSharedMut<ContinuationMark>,
}

impl PartialEq for Continuation {
    fn eq(&self, other: &Self) -> bool {
        *(self.inner.read()) == *(other.inner.read())
    }
}

impl Eq for Continuation {}

#[derive(Clone, Debug)]
struct WeakContinuation {
    pub(crate) inner: WeakSharedMut<ContinuationMark>,
}

impl WeakContinuation {
    fn from_strong(cont: &Continuation) -> Self {
        Self {
            inner: StandardShared::downgrade(&cont.inner),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosedContinuation {
    pub(crate) stack: Vec<SteelVal>,
    pub(crate) current_frame: StackFrame,
    instructions: RootedInstructions,
    pub(crate) stack_frames: Vec<StackFrame>,
    ip: usize,
    sp: usize,
    pop_count: usize,

    // #[cfg(feature = "rooted-instructions")]
    // rooted_instructions: Vec<Shared<[DenseInstruction]>>,
    #[cfg(debug_assertions)]
    closed_continuation: Option<Box<ClosedContinuation>>,
}

pub trait VmContext {
    fn call_transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: &Reducer,
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
    fn call_transduce(
        &mut self,
        ops: &[Transducers],
        root: SteelVal,
        reducer: &Reducer,
        span: Option<Span>,
    ) -> Result<SteelVal> {
        let span = span.unwrap_or_default();
        self.transduce(ops, root, reducer, &span)
    }
}

// TODO: Make dynamic block use this
// enum FunctionBlock {
//     Specialized(for<'r> fn(&'r mut VmCore<'_>) -> Result<()>),
//     Unrolled(Rc<[for<'r> fn(&'r mut VmCore<'_>) -> Result<()>]>),
// }

// Construct a basic block for a series of instructions
// Note: A call to either apply or call/cc should invalidate this, as it fundamentally
// violates the principle of a basic block.
// #[derive(Clone)]
// pub struct DynamicBlock {
//     basic_block: InstructionPattern,
//     entry_inst: DenseInstruction,
//     header_func: Option<for<'r> fn(&'r mut VmCore<'_>, usize) -> Result<()>>,
//     handlers: Rc<[for<'r> fn(&'r mut VmCore<'_>) -> Result<()>]>,
//     specialized: Option<for<'r> fn(&'r mut VmCore<'_>, usize) -> Result<()>>,
// }

// impl DynamicBlock {
// fn call(&self, context: &mut VmCore<'_>) -> Result<()> {
//     // println!("---- Entering dynamic block ----");
//     // println!("{:#?}", self.basic_block);

//     if let Some(specialized) = self.specialized {
//         specialized(context, self.entry_inst.payload_size.to_usize())?;
//     } else {
//         if let Some(header) = self.header_func {
//             // println!("Calling special entry block");
//             header(context, self.entry_inst.payload_size.to_usize())?;
//         }

//         for func in self.handlers.iter() {
//             func(context)?;
//         }
//     }

//     Ok(())
// }

//     #[cfg(feature = "dynamic")]
//     fn construct_basic_block(head: DenseInstruction, basic_block: InstructionPattern) -> Self {
//         // TODO: Drop the first
//         let mut handlers = basic_block.block.iter().peekable();
//         // .map(|x| OP_CODE_TABLE[x.to_usize()]);
//         // .collect();

//         let mut header_func = None;

//         log::debug!(target: "super-instructions", "{basic_block:#?}");

//         if let Some(first) = handlers.peek() {
//             header_func = op_code_requires_payload(first.0);
//         }

//         if header_func.is_some() {
//             handlers.next();
//         }

//         let op_codes: Vec<_> = handlers.clone().copied().collect();

//         let specialized = dynamic::DYNAMIC_SUPER_PATTERNS.get(&op_codes);

//         if specialized.is_some() {
//             println!("Found specialized function!");
//         }

//         let handlers = handlers.map(|x| OP_CODE_TABLE[x.0.to_usize()]).collect();

//         Self {
//             basic_block,
//             handlers,
//             entry_inst: head,
//             header_func,
//             // TODO: Come back and add the specialized ones back in
//             specialized,
//         }
//     }
// }

#[repr(C)]
pub struct VmCore<'a> {
    pub(crate) is_native: bool,
    pub(crate) ip: usize,
    pub(crate) sp: usize,
    pub(crate) thread: &'a mut SteelThread,
    pub(crate) instructions: RootedInstructions,
    // TODO: Replace this with a thread local constant map!
    // that way reads are fast - and any updates to it are
    // broadcast from the shared constant map.
    pub(crate) constants: ConstantMap,
    pub(crate) pop_count: usize,
    pub(crate) depth: usize,
    pub(crate) root_spans: &'a [Span],
    pub(crate) return_value: Option<SteelVal>,
    // TODO: This means we've entered the native section of the code
    pub(crate) result: Option<Result<SteelVal>>,
}

// TODO: Delete this entirely, and just have the run function live on top of the SteelThread.
//
impl<'a> VmCore<'a> {
    fn new_unchecked(
        instructions: RootedInstructions,
        constants: ConstantMap,
        thread: &'a mut SteelThread,
        root_spans: &'a [Span],
    ) -> VmCore<'a> {
        VmCore {
            instructions,
            constants,
            ip: 0,
            sp: 0,
            pop_count: 1,
            depth: 0,
            thread,
            root_spans,
            return_value: None,
            is_native: false,
            result: None,
        }
    }

    fn new(
        instructions: RootedInstructions,
        constants: ConstantMap,
        thread: &'a mut SteelThread,
        root_spans: &'a [Span],
    ) -> Result<VmCore<'a>> {
        if instructions.is_empty() {
            stop!(Generic => "empty stack!")
        }

        Ok(VmCore {
            instructions,
            constants,
            ip: 0,
            sp: 0,
            pop_count: 1,
            depth: 0,
            thread,
            root_spans,
            return_value: None,
            is_native: false,
            result: None,
        })
    }

    #[cfg(feature = "sync")]
    pub fn steel_function_to_rust_function(
        &self,
        func: SteelVal,
    ) -> Box<dyn Fn(&mut [SteelVal]) -> Result<SteelVal> + Send + Sync + 'static> {
        let thread = self.make_thread();
        let rooted = func.as_rooted();

        Box::new(move |args: &mut [SteelVal]| {
            let func = rooted.value();

            let mut guard = thread.lock().unwrap();
            guard.call_fn_from_mut_slice(func.clone(), args)
        })
    }

    // Copy the thread of execution. This just blindly copies the thread, and closes
    // the continuations found.
    // TODO: Add this thread to the parent VM thread handler -> this is necessary
    // for safepoints to work correctly
    #[cfg(feature = "sync")]
    pub fn make_thread(&self) -> Arc<Mutex<SteelThread>> {
        let mut thread = self.thread.clone();

        let controller = ThreadStateController::default();
        thread.synchronizer.state = controller.clone();
        // This thread needs its own context
        thread.synchronizer.ctx = Arc::new(AtomicCell::new(None));

        thread.synchronizer.spawned_via_make_thread = true;

        let weak_ctx = Arc::downgrade(&thread.synchronizer.ctx);

        thread.id = EngineId::new();

        let forked_thread = Arc::new(Mutex::new(thread));

        let value = ThreadHandle {
            handle: Mutex::new(None),
            thread: std::thread::current(),
            thread_state_manager: controller,
            forked_thread_handle: Some(Arc::downgrade(&forked_thread)),
        }
        .into_steelval()
        .unwrap();

        self.thread
            .synchronizer
            .threads
            .lock()
            .unwrap()
            .push(ThreadContext {
                ctx: weak_ctx,
                handle: value.clone(),
            });

        for frame in &self.thread.stack_frames {
            self.close_continuation_marks(frame);
        }
        self.close_continuation_marks(&self.thread.current_frame);

        forked_thread
    }

    fn park_thread_while_paused(&self) {
        // TODO: Consider using condvar instead here
        while self
            .thread
            .synchronizer
            .state
            .paused
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            if !self.thread.synchronizer.spawned_via_make_thread {
                std::thread::park();
            }
        }
    }

    #[inline(always)]
    pub fn safepoint_or_interrupt(&mut self) -> Result<()> {
        // Check if we need to be paused?
        if self
            .thread
            .synchronizer
            .state
            .paused
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            match self.thread.synchronizer.state.state.load() {
                ThreadState::Interrupted => {
                    stop!(Generic => format!("Thread: {:?} - Interrupted by user", std::thread::current().id()); self.current_span());
                }
                ThreadState::Suspended => {
                    self.park_thread_while_paused();
                }
                ThreadState::PausedAtSafepoint => {
                    // TODO:
                    // Insert the code to do the stack things here
                    let ptr = self.thread as _;
                    self.thread.synchronizer.ctx.store(Some(ptr));
                    self.park_thread_while_paused();
                    self.thread.synchronizer.ctx.store(None);
                }
                ThreadState::Running => {}
            }
        }

        Ok(())
    }

    pub fn make_box(&mut self, value: SteelVal) -> SteelVal {
        let allocated_var = self.thread.heap.lock().unwrap().allocate(
            value,
            &self.thread.stack,
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
            &self.thread.thread_local_storage,
            &mut self.thread.synchronizer,
        );

        SteelVal::HeapAllocated(allocated_var)
    }

    // TODO: Accept a slice instead, or an iterator with a known size.
    // That way we can take advantage of a pre allocation.
    pub fn make_mutable_vector(&mut self, values: Vec<SteelVal>) -> SteelVal {
        let allocated_var = self.thread.heap.lock().unwrap().allocate_vector(
            values,
            &self.thread.stack,
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
            &self.thread.thread_local_storage,
            &mut self.thread.synchronizer,
        );

        SteelVal::MutableVector(allocated_var)
    }

    pub fn make_mutable_vector_iter(
        &mut self,
        values: impl Iterator<Item = SteelVal> + Clone,
    ) -> SteelVal {
        let allocated_var = self.thread.heap.lock().unwrap().allocate_vector_iter(
            values,
            &self.thread.stack,
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
            &self.thread.thread_local_storage,
            &mut self.thread.synchronizer,
        );

        SteelVal::MutableVector(allocated_var)
    }

    pub(crate) fn gc_collect(&mut self) {
        self.thread.heap.lock().unwrap().collection(
            &self.thread.stack,
            self.thread.stack_frames.iter().map(|x| x.function.as_ref()),
            self.thread.global_env.roots(),
            &self.thread.thread_local_storage,
            &mut self.thread.synchronizer,
            true,
        );
    }

    // TODO: Expose a function for this! Or alternatively, set up an incremental
    // way of doing things
    // pub fn weak_collection(&mut self) {
    //     todo!()
    //     // self.thread.heap.lock().unwrap().weak_collection();
    // }

    fn new_open_continuation_from_state(&self) -> Continuation {
        let offset = self.get_offset();
        Continuation {
            inner: StandardShared::new(MutContainer::new(ContinuationMark::Open(
                OpenContinuationMark {
                    current_frame: self.thread.stack_frames.last().unwrap().clone(),
                    stack_frame_offset: self.thread.stack.len(),
                    current_stack_values: self.thread.stack[offset..].to_vec(),
                    instructions: self.instructions.clone(),
                    ip: self.ip,
                    sp: self.sp,
                    pop_count: self.pop_count,
                    #[cfg(debug_assertions)]
                    closed_continuation: self.new_closed_continuation_from_state(),
                },
            ))),
        }
    }

    // Could be neat at some point: https://docs.rs/stacker/latest/stacker/
    fn new_closed_continuation_from_state(&self) -> ClosedContinuation {
        ClosedContinuation {
            stack: self.thread.stack.clone(),
            instructions: self.instructions.clone(),
            current_frame: self.thread.stack_frames.last().unwrap().clone(),
            stack_frames: self.thread.stack_frames.clone(),
            ip: self.ip,
            sp: self.sp,
            pop_count: self.pop_count,
            #[cfg(debug_assertions)]
            closed_continuation: None,
        }
    }

    // Grab the continuation - but this continuation can only be played once.
    // The way this should probably be implemented is by checking that the continuation
    // is open; In this scheme, it doesn't appear that we'll be able to continue computation
    // correctly after this is created. We should leave it open - and then when closing the
    // continuation, move the values in, rather than immediately closing it. This _might_ be
    // how the existing call/cc implementation works already, which would be nice. However -
    // when _replaying_ the continuation, we should also assume that it can only be replayed
    // once to avoid copying the whole thing.
    pub fn new_oneshot_continuation_from_state(&mut self) -> ClosedContinuation {
        ClosedContinuation {
            stack: std::mem::take(&mut self.thread.stack),
            instructions: self.instructions.clone(),
            current_frame: self.thread.current_frame.clone(),
            stack_frames: std::mem::take(&mut self.thread.stack_frames),
            ip: self.ip,
            sp: self.sp,
            pop_count: self.pop_count,
            #[cfg(debug_assertions)]
            closed_continuation: None,
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
    fn set_state_from_continuation(&mut self, continuation: ClosedContinuation) {
        // dbg!(&continuation.stack);

        // Linked list of frames perhaps?
        // TODO: This, unfortunately, will close any open continuations even if they're within the same subset.
        // So what we really should do is keep a set of sub continuations inside each open continuation, to see
        // if there are currently open continuations inside of those?
        //
        // Or maybe a List<MaybeContinuation> inside of each continuation? Something like that
        //
        // That way we can in relatively quick amount of time scan for the continuations and delay
        // copying everything over.

        let mut marks_still_open = rustc_hash::FxHashSet::default();

        for frame in &continuation.stack_frames {
            // if let Some(cont_mark) = frame
            //     .weak_continuation_mark
            //     .as_ref()
            //     .and_then(|x| WeakShared::upgrade(&x.inner))
            // {

            if let Some(cont_mark) = frame.attachments.as_ref().and_then(|x| {
                x.weak_continuation_mark
                    .as_ref()
                    .and_then(|x| WeakShared::upgrade(&x.inner))
            }) {
                marks_still_open.insert(StandardShared::as_ptr(&cont_mark) as usize);
            }
        }

        while let Some(frame) = self.thread.stack_frames.pop() {
            // if let Some(cont_mark) = frame
            //     .weak_continuation_mark
            //     .as_ref()
            //     .and_then(|x| WeakShared::upgrade(&x.inner))
            // {
            if let Some(cont_mark) = frame.attachments.as_ref().and_then(|x| {
                x.weak_continuation_mark
                    .as_ref()
                    .and_then(|x| WeakShared::upgrade(&x.inner))
            }) {
                // Close frame if the new continuation doesn't have it
                if !marks_still_open.contains(&(StandardShared::as_ptr(&cont_mark) as usize)) {
                    self.thread.stack.truncate(frame.sp);
                    self.ip = frame.ip;
                    self.sp = self.get_last_stack_frame_sp();
                    // self.instructions = Shared::clone(&frame.instructions);

                    self.instructions = frame.instructions.clone();

                    self.close_continuation_marks(&frame);
                    continue;
                }
            }
        }

        self.thread.stack = continuation.stack;
        self.instructions = continuation.instructions;
        self.ip = continuation.ip;
        self.sp = continuation.sp;
        self.pop_count = continuation.pop_count;
        self.thread.stack_frames = continuation.stack_frames;
        self.thread.current_frame = continuation.current_frame;
    }

    // #[inline(always)]
    fn construct_continuation_function(&self) -> Continuation {
        self.new_open_continuation_from_state()
    }

    // Reset state FULLY
    pub(crate) fn call_with_instructions_and_reset_state(
        &mut self,
        closure: RootedInstructions,
    ) -> Result<SteelVal> {
        let old_ip = self.ip;
        let old_instructions = std::mem::replace(&mut self.instructions, closure);
        let old_pop_count = self.pop_count;

        self.ip = 0;
        // Force the execution to be done earlier
        self.pop_count = 1;

        self.depth += 1;

        let res;

        'outer: loop {
            let result = self
                .vm()
                .map_err(|error| error.with_stack_trace(self.snapshot_stack_trace()));

            if let Err(e) = result {
                while let Some(mut last) = self.thread.stack_frames.pop() {
                    // Unwind the stack, close continuation marks here!
                    // For whatever reason - if we're at the top, we shouldn't go down below 0
                    if self.pop_count == 0 {
                        return Err(e);
                    }

                    // Drop the pop count along with everything else we're doing
                    self.pop_count -= 1;

                    // if last.weak_continuation_mark.is_some() {

                    if last
                        .attachments
                        .as_mut()
                        .and_then(|x| x.weak_continuation_mark.take())
                        .is_some()
                    {
                        self.thread.stack.truncate(last.sp);
                        self.ip = last.ip;
                        self.sp = self.get_last_stack_frame_sp();
                        // self.instructions = Shared::clone(&last.instructions);

                        self.instructions = last.instructions.clone();

                        self.close_continuation_marks(&last);
                    }

                    if let Some(handler) = last.attachments.as_mut().and_then(|x| x.handler.take())
                    {
                        // Drop the stack BACK to where it was on this level
                        self.thread.stack.truncate(last.sp);

                        self.thread.stack.push(e.into_steelval()?);

                        // If we're at the top level, we need to handle this _slightly_ differently
                        // if vm_instance.stack_frames.is_empty() {
                        // Somehow update the main instruction group to _just_ be the new group
                        match handler {
                            SteelVal::Closure(closure) => {
                                if self.thread.stack_frames.is_empty() {
                                    self.sp = last.sp;

                                    // Push on a dummy stack frame if we're at the top
                                    self.thread.stack_frames.push(StackFrame::new(
                                        last.sp,
                                        Gc::clone(&closure),
                                        0,
                                        RootedInstructions::new(
                                            THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone()),
                                        ),
                                    ));
                                }

                                self.sp = last.sp;
                                self.instructions = closure.body_exp();

                                last.function = closure.clone();

                                self.ip = 0;

                                // Put this back as the last stack frame
                                self.thread.stack_frames.push(last);

                                self.pop_count += 1;
                            }
                            _ => {
                                stop!(TypeMismatch => "expected a function for the exception handler, found: {}", handler)
                            }
                        }

                        continue 'outer;
                    }
                }

                // self.thread.stack.clear();

                res = Err(e);
                break;
            } else {
                // TODO: Do we need to close frames here?
                // for frame in &self.thread.stack_frames {
                //     Continuation::close_marks(&self, &frame);
                // }

                // self.thread.stack.truncate(self.sp);

                res = result;
                break;
            }
        }

        self.depth -= 1;

        self.ip = old_ip;
        self.instructions = old_instructions;
        self.pop_count = old_pop_count;
        // self.spans = old_spans;
        self.sp = self.thread.stack_frames.last().map(|x| x.sp).unwrap_or(0);

        // println!("After: {:?}", self.thread.stack_frames.len());
        // self.thread.stack.truncate(self.sp);

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
            SteelVal::MutFunc(func) => {
                let mut arg_vec = [arg];
                func(&mut arg_vec).map_err(|x| x.set_span_if_none(*cur_inst_span))
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
    pub(crate) fn call_cont_with_args(
        &mut self,
        cont: Continuation,
        args: impl IntoIterator<Item = SteelVal>,
    ) -> Result<SteelVal> {
        for arg in args {
            self.thread.stack.push(arg);
        }

        self.call_continuation(cont)?;

        self.thread
            .stack
            .pop()
            .ok_or_else(throw!(Generic => "stack empty at pop!"))
    }

    // pub(crate) fn eval_executable(&mut self, executable: &Executable) -> Result<Vec<SteelVal>> {
    //     // let prev_length = self.thread.stack.len();

    //     // let prev_stack_frames = std::mem::take(&mut self.thread.stack_frames);

    //     // let mut results = Vec::new();

    //     // for instr in executable.instructions {
    //     //     self.call_with_instructions_and_reset_state(Arc::clone(instr))
    //     // }

    //     let Executable {
    //         instructions,
    //         constant_map,
    //         spans,
    //         ..
    //     } = executable;

    //     // let res = instructions
    //     //     .iter()
    //     //     .zip(spans.iter())
    //     //     .map(|x| {
    //     //         pretty_print_dense_instructions(&x.0);

    //     //         self.call_with_instructions_and_reset_state(
    //     //             Shared::clone(x.0),
    //     //             // constant_map.clone(),
    //     //             // Shared::clone(x.1),
    //     //         )
    //     //     })
    //     //     .collect::<Result<Vec<_>>>();

    //     // self.thread.stack.truncate(prev_length);

    //     // self.thread.stack_frames = prev_stack_frames;

    //     res
    // }

    // Call with an arbitrary number of arguments
    pub(crate) fn call_with_args(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        args: impl IntoIterator<Item = SteelVal>,
    ) -> Result<SteelVal> {
        let prev_length = self.thread.stack.len();
        // let prev_stack_frame = self.thread.current_frame.clone();
        // let stack_frame_len = self.thread.stack_frames.len();

        let instructions = closure.body_exp();

        // TODO:
        self.thread.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            0,
            instructions.clone(),
        ));

        self.sp = prev_length;

        let mut argument_count = 0;
        for arg in args {
            self.thread.stack.push(arg);
            argument_count += 1;
        }

        self.adjust_stack_for_multi_arity(closure, argument_count, &mut 0)?;

        let res = self.call_with_instructions_and_reset_state(instructions);

        // Clean up the stack now
        self.thread.stack.truncate(prev_length);
        // self.thread.current_frame = prev_stack_frame;
        // self.thread.stack_frames.truncate(stack_frame_len);

        res
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
            RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
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
            RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
        ));

        self.sp = prev_length;

        self.thread.stack.push(arg);

        self.adjust_stack_for_multi_arity(closure, 1, &mut 0)?;
        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    pub(crate) fn call_with_one_arg_test<const M: bool>(
        &mut self,
        closure: &Gc<ByteCodeLambda>,
        arg: SteelVal,
    ) -> Result<SteelVal> {
        let prev_length = self.thread.stack.len();

        self.thread.stack_frames.push(StackFrame::new(
            prev_length,
            Gc::clone(closure),
            0,
            RootedInstructions::new(THE_EMPTY_INSTRUCTION_SET.with(|x| x.clone())),
        ));

        self.sp = prev_length;

        self.thread.stack.push(arg);

        if M {
            self.adjust_stack_for_multi_arity(closure, 1, &mut 0)?;
        }

        self.call_with_instructions_and_reset_state(closure.body_exp())
    }

    pub(crate) fn vm(&mut self) -> Result<SteelVal> {
        // if self.depth > 1024 {
        if self.depth > 1024 * 128 {
            // TODO: Unwind the callstack? Patch over to the VM call stack rather than continue to do recursive calls?
            stop!(Generic => "stack overflow! The rust call stack is currently used for transducers, so we impose a hard recursion limit of 1024"; self.current_span());
        }

        macro_rules! inline_primitive {
            ($name:tt, $payload_size:expr) => {{
                let last_index = self.thread.stack.len() - $payload_size.to_usize();

                let result = match $name(&mut self.thread.stack[last_index..]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                self.thread.stack.truncate(last_index + 1);
                *self.thread.stack.last_mut().unwrap() = result;

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
                    self.thread.stack[read_local.payload_size.to_usize() + offset].clone();

                // get the const
                let const_val = self.constants.get_value(push_const.payload_size.to_usize());

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
                    self.thread.stack[read_local.payload_size.to_usize() + offset].clone();

                // get the const value, if it can fit into the value...
                let const_val = SteelVal::IntV(push_const.payload_size.to_usize() as isize);

                // sub_handler_none_int

                let result = match $name(&[local_value, const_val]) {
                    Ok(value) => value,
                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                };

                self.thread.stack.push(result);

                self.ip += 2;
            }};
        }

        loop {
            self.safepoint_or_interrupt()?;

            #[cfg(feature = "op-code-profiling")]
            crate::steel_vm::profiling::record_start_op(self.instructions[self.ip]);

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

                // if USE_SUPER_INSTRUCTIONS {
                //     // Index of the starting opcode
                //     let start = pat.pattern.start;

                //     let id = self.thread.super_instructions.len();

                //     let guard = self.thread.stack_frames.last_mut().unwrap();

                //     // Next run should get the new sequence of opcodes
                //     let (head, _) = guard.function.update_to_super_instruction(start, id);

                //     let block = DynamicBlock::construct_basic_block(head, pat);

                //     self.thread.super_instructions.push(Rc::new(block));
                // }
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

            // dbg!(&instr);

            match instr {
                #[cfg(feature = "jit2")]
                DenseInstruction {
                    op_code: OpCode::DynSuperInstruction,
                    ..
                } => {
                    // Entering the context of the native code.
                    // If at any point we deopt, we should check this flag on the
                    // runtime, which tells the function to just return and let
                    // the runtime do the thing now.
                    self.is_native = true;

                    self.thread
                        .stack_frames
                        .last()
                        .unwrap()
                        .function
                        .super_instructions
                        .as_ref()
                        .unwrap()(self);

                    self.is_native = false;

                    if let Some(res) = self.result.take() {
                        println!("Found result: {:?}", res);

                        return res;
                    }
                }

                // DenseInstruction {
                //     op_code: OpCode::DynSuperInstruction,
                //     payload_size,
                //     ..
                // } => {
                //     self.cut_sequence();

                //     // TODO: Store in a different spot? So that we can avoid cloning on every iteration?
                //     let super_instruction =
                //         { self.thread.super_instructions[payload_size.to_usize()].clone() };

                //     super_instruction.call(self)?;
                // }
                DenseInstruction {
                    op_code: OpCode::POPN,
                    payload_size,
                    ..
                } => {
                    let last = self.thread.stack.pop().unwrap();
                    self.thread
                        .stack
                        .truncate(self.thread.stack.len() - payload_size.to_usize());
                    self.thread.stack.push(last);
                    self.ip += 1;
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
                        self.thread.stack[read_local.payload_size.to_usize() + offset].clone();

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
                    op_code: OpCode::Apply,
                    // payload_size,
                    ..
                } => {
                    todo!()
                }

                DenseInstruction {
                    op_code: OpCode::LIST,
                    payload_size,
                    ..
                } => {
                    list_handler(self, payload_size.to_usize())?;
                }

                DenseInstruction {
                    op_code: OpCode::VECTORREF,
                    ..
                } => {
                    let index = self.thread.stack.pop().unwrap();
                    let last_mut = self.thread.stack.last_mut().unwrap();

                    let result = match vec_ref(last_mut, &index) {
                        Ok(value) => value,
                        Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    };

                    *last_mut = result;

                    self.ip += 2;
                }

                DenseInstruction {
                    op_code: OpCode::LISTREF,
                    ..
                } => {
                    listref_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::NEWBOX,
                    ..
                } => {
                    new_box_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::UNBOX,
                    ..
                } => {
                    // unbox_handler(self)?;

                    let last = self.thread.stack.last_mut().unwrap();
                    if let SteelVal::HeapAllocated(var) = last {
                        *last = var.get();
                    } else {
                        stop!(TypeMismatch => format!("Unable to unbox non box: {}", last); self.current_span())
                    }
                    self.ip += 2;
                }

                DenseInstruction {
                    op_code: OpCode::UNBOXCALL,
                    payload_size,
                    ..
                } => {
                    let mut last = self.thread.stack.pop().unwrap();
                    if let SteelVal::HeapAllocated(var) = last {
                        last = var.get();
                    } else {
                        stop!(TypeMismatch => format!("Unable to unbox non box: {}", last); self.current_span())
                    }
                    self.ip += 2;
                    self.handle_function_call(last, payload_size.to_usize())?;
                }

                DenseInstruction {
                    op_code: OpCode::UNBOXTAIL,
                    payload_size,
                    ..
                } => {
                    let mut last = self.thread.stack.pop().unwrap();
                    if let SteelVal::HeapAllocated(var) = last {
                        last = var.get();
                    } else {
                        stop!(TypeMismatch => format!("Unable to unbox non box: {}", last); self.current_span())
                    }
                    self.ip += 2;
                    self.handle_tail_call(last, payload_size.to_usize())?
                }

                DenseInstruction {
                    op_code: OpCode::SETBOX,
                    ..
                } => {
                    setbox_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::CAR,
                    ..
                } => {
                    car_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::NOT,
                    ..
                } => {
                    not_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::CDR,
                    ..
                } => {
                    cdr_handler(self)?;
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
                    let l = &self.thread.stack[read_local.payload_size.to_usize() + offset];

                    // get the const value, if it can fit into the value...
                    let r = push_const.payload_size.to_usize() as isize;

                    let result = match l {
                        // Fast path with an integer, otherwise slow path
                        SteelVal::IntV(l) => {
                            match l.checked_sub(&r) {
                                Some(r) => SteelVal::IntV(r),
                                // Slow path
                                None => SteelVal::BigNum(Gc::new(BigInt::from(*l) - r)),
                            }
                        }

                        SteelVal::NumV(_)
                        | SteelVal::Rational(_)
                        | SteelVal::BigNum(_)
                        | SteelVal::BigRational(_) => {
                            // TODO: Create a specialized version of this!
                            subtract_primitive(&[l.clone(), SteelVal::IntV(r)])
                                .map_err(|x| x.set_span_if_none(self.current_span()))?
                        }
                        _ => {
                            cold();
                            stop!(TypeMismatch => "sub expected a number, found: {}", l)
                        }
                    };

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
                    let l = &self.thread.stack[read_local.payload_size.to_usize() + offset];

                    // get the const value, if it can fit into the value...
                    let r = push_const.payload_size.to_usize() as isize;

                    // sub_handler_none_int

                    // TODO: Probably elide the stack push if the next inst is an IF
                    // let result = lte_handler_none_int(self, local_value, const_val)?;

                    let result = match l {
                        SteelVal::IntV(_)
                        | SteelVal::NumV(_)
                        | SteelVal::Rational(_)
                        | SteelVal::BigNum(_)
                        | SteelVal::BigRational(_) => l.clone() <= SteelVal::IntV(r),
                        _ => {
                            stop!(TypeMismatch => format!("lte expected an number, found: {}", l); self.current_span())
                        }
                    };

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
                    let l = &self.thread.stack[read_local.payload_size.to_usize() + offset];

                    // get the const value, if it can fit into the value...
                    let r = push_const.payload_size.to_usize() as isize;

                    // sub_handler_none_int

                    // TODO: Probably elide the stack push if the next inst is an IF
                    // let result = lte_handler_none_int(self, local_value, const_val)?;

                    let result = match l {
                        SteelVal::IntV(_)
                        | SteelVal::NumV(_)
                        | SteelVal::Rational(_)
                        | SteelVal::BigNum(_)
                        | SteelVal::BigRational(_) => l.clone() <= SteelVal::IntV(r),
                        _ => {
                            stop!(TypeMismatch => format!("lte expected an number, found: {}", l); self.current_span())
                        }
                    };

                    self.ip += 2;

                    // change to truthy...
                    if result {
                        self.ip += 1;
                    } else {
                        self.ip = self.instructions[self.ip].payload_size.to_usize();
                    }
                }

                DenseInstruction {
                    op_code: OpCode::ADD,
                    payload_size,
                    ..
                } => {
                    add_handler_payload(self, payload_size.to_usize())?;
                    // inline_primitive!(add_primitive, payload_size)
                }
                DenseInstruction {
                    op_code: OpCode::BINOPADD,
                    ..
                } => {
                    // add_handler_payload(self, 2)?;

                    let right = self.thread.stack.pop().unwrap();
                    let left = self.thread.stack.last_mut().unwrap();

                    let result = match add_two(left, &right) {
                        Ok(value) => value,
                        Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    };

                    *left = result;

                    self.ip += 2;
                }

                DenseInstruction {
                    op_code: OpCode::BINOPADDTAIL,
                    ..
                } => {
                    let right = self.thread.stack.pop().unwrap();
                    let left = self.thread.stack.pop().unwrap();

                    let result = match handlers::add_handler_none_none(&left, &right) {
                        Ok(value) => value,
                        Err(e) => return Err(e.set_span_if_none(self.current_span())),
                    };

                    // *left = result;

                    if let Some(r) = self.handle_pop_pure_value(result) {
                        return r;
                    }

                    // self.ip += 2;
                }

                DenseInstruction {
                    op_code: OpCode::SUB,
                    payload_size,
                    ..
                } => {
                    sub_handler_payload(self, payload_size.to_usize())?;
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
                    op_code: OpCode::EQUAL2,
                    ..
                } => {
                    let top = self.thread.stack.pop().unwrap();
                    let last = self.thread.stack.last_mut().unwrap();
                    *last = SteelVal::BoolV(last == &top);
                    self.ip += 2;
                }

                DenseInstruction {
                    op_code: OpCode::EQUALCONST,
                    payload_size,
                } => {
                    self.constants
                        .get_map(payload_size.to_usize(), |const_value: &SteelVal| {
                            let last = self.thread.stack.last_mut().unwrap();
                            let res = last == const_value;
                            *last = SteelVal::BoolV(res);
                        });

                    self.ip += 3;
                }

                DenseInstruction {
                    op_code: OpCode::NUMEQUAL,
                    ..
                } => {
                    number_equality_handler(self)?;
                }

                DenseInstruction {
                    op_code: OpCode::NULL,
                    ..
                } => {
                    // Simply fast path case for checking null or empty
                    let last = self.thread.stack.last_mut().unwrap();
                    let result = is_empty(last);
                    *last = SteelVal::BoolV(result);
                    self.ip += 2;
                }

                DenseInstruction {
                    op_code: OpCode::NULLIF,
                    ..
                } => {
                    // Simply fast path case for checking null or empty
                    let last = self.thread.stack.pop().unwrap();
                    let result = is_empty(&last);
                    self.ip += 2;
                    if result {
                        self.ip += 1;
                    } else {
                        self.ip = self.instructions[self.ip].payload_size.to_usize();
                    }
                }

                DenseInstruction {
                    op_code: OpCode::LTE,
                    payload_size,
                    ..
                } => {
                    lte_handler_payload(self, payload_size.to_usize())?;
                    // inline_primitive!(lte_primitive, payload_size);
                }

                DenseInstruction {
                    op_code: OpCode::LT,
                    payload_size,
                    ..
                } => {
                    // #[cfg(feature = "jit2")]
                    lt_handler_payload(self, payload_size.to_usize())?;
                }

                DenseInstruction {
                    op_code: OpCode::GT,
                    payload_size,
                    ..
                } => {
                    // #[cfg(feature = "jit2")]
                    gt_handler_payload(self, payload_size.to_usize())?;
                }

                DenseInstruction {
                    op_code: OpCode::GTE,
                    payload_size,
                    ..
                } => {
                    // #[cfg(feature = "jit2")]
                    gte_handler_payload(self, payload_size.to_usize())?;
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
                } => self.handle_set(payload_size.to_usize())?,
                DenseInstruction {
                    op_code: OpCode::PUSHCONST,
                    payload_size,
                    ..
                } => {
                    let val = self.constants.get_value(payload_size.to_usize());
                    self.thread.stack.push(val);
                    self.ip += 1;
                }
                DenseInstruction {
                    op_code: OpCode::PUSH,
                    payload_size,
                    ..
                } => self.handle_push(payload_size.to_usize())?,
                DenseInstruction {
                    op_code: OpCode::READLOCAL,
                    payload_size,
                    ..
                } => self.handle_local(payload_size.to_usize())?,
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
                } => self.handle_read_captures(payload_size.to_usize())?,
                DenseInstruction {
                    op_code: OpCode::MOVEREADLOCAL,
                    payload_size,
                    ..
                } => self.handle_move_local(payload_size.to_usize())?,
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

                DenseInstruction {
                    op_code: OpCode::SETLOCAL,
                    payload_size,
                    ..
                } => self.handle_set_local(payload_size.to_usize()),

                DenseInstruction {
                    op_code: OpCode::TRUE,
                    ..
                } => {
                    self.thread.stack.push(SteelVal::BoolV(true));
                    self.ip += 1;
                }

                DenseInstruction {
                    op_code: OpCode::FALSE,
                    ..
                } => {
                    self.thread.stack.push(SteelVal::BoolV(false));
                    self.ip += 1;
                }

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
                    op_code: OpCode::LOADINT1POP,
                    ..
                } => {
                    // self.thread.stack.push(SteelVal::INT_TWO);
                    self.ip += 1;
                    if let Some(r) = self.handle_pop_pure_value(SteelVal::INT_ONE) {
                        return r;
                    }
                }

                // DenseInstruction {
                //     op_code: OpCode::CGLOCALCONST,
                //     payload_size,
                //     ..
                // } => {
                //     let read_local = &self.instructions[self.ip + 1];
                //     let push_const = &self.instructions[self.ip + 2];

                //     // Snag the function
                //     let func = self
                //         .thread
                //         .global_env
                //         .repl_lookup_idx(payload_size.to_usize());

                //     // get the local
                //     // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
                //     let offset = self.get_offset();
                //     let local_value =
                //         self.thread.stack[read_local.payload_size.to_usize() + offset].clone();

                //     // get the const
                //     let const_val = self.constants.get(push_const.payload_size.to_usize());

                //     self.handle_lazy_function_call(func, local_value, const_val)?;
                // }
                DenseInstruction {
                    op_code: OpCode::READLOCAL0CALLGLOBAL,
                    // payload_size,
                    ..
                } => {
                    // Handling the local
                    let offset = self.get_offset();
                    let value = self.thread.stack[offset].clone();
                    self.thread.stack.push(value);
                    self.ip += 1;

                    //
                    let payload_size = self.instructions[self.ip].payload_size;
                    self.ip += 1;
                    let next_inst = self.instructions[self.ip];

                    // TODO: Handle lazy function call instead to avoid pushing on
                    // to the stack if necessary
                    self.handle_call_global(
                        payload_size.to_usize(),
                        next_inst.payload_size.to_usize(),
                    )?;
                }

                DenseInstruction {
                    op_code: OpCode::READLOCAL1CALLGLOBAL,
                    // payload_size,
                    ..
                } => {
                    let offset = self.get_offset();
                    let value = self.thread.stack[offset + 1].clone();
                    self.thread.stack.push(value);
                    self.ip += 1;

                    // assert!(self.ip + 2 < self.instructions.len());

                    let payload_size = self.instructions[self.ip].payload_size;
                    self.ip += 1;
                    let next_inst = self.instructions[self.ip];

                    self.handle_call_global(
                        payload_size.to_usize(),
                        next_inst.payload_size.to_usize(),
                    )?;
                }

                DenseInstruction {
                    op_code: OpCode::CALLGLOBAL,
                    payload_size,
                    ..
                } => {
                    self.ip += 1;
                    let next_inst = self.instructions[self.ip];
                    self.handle_call_global(
                        payload_size.to_usize(),
                        next_inst.payload_size.to_usize(),
                    )?;
                }

                DenseInstruction {
                    op_code: OpCode::CALLGLOBALNOARITY,
                    payload_size,
                    ..
                } => {
                    self.ip += 1;
                    let next_inst = self.instructions[self.ip];
                    self.handle_call_global_no_arity(
                        payload_size.to_usize(),
                        next_inst.payload_size.to_usize(),
                    )?;
                }

                /*
                DenseInstruction {
                    op_code: OpCode::CALLGLOBALIF,
                    payload_size,
                    ..
                } => {
                    self.ip += 1;
                    let next_inst = self.instructions[self.ip];
                    let res = self.handle_call_global_no_stack(
                        payload_size.to_usize(),
                        next_inst.payload_size.to_usize(),
                    )?;

                    // If there is something, then we can move on to the if. Otherwise
                    // we've yielded control back to the calling function.
                    if let Some(value) = res {
                        if value.is_truthy() {
                            self.ip += 1;
                        } else {
                            self.ip = self.instructions[self.ip].payload_size.to_usize();
                        }
                    }
                }
                */
                DenseInstruction {
                    op_code: OpCode::CALLGLOBALTAIL,
                    payload_size,
                    ..
                } => {
                    let next_inst = self.instructions[self.ip + 1];
                    // self.handle_tail_call_global(
                    //     payload_size.to_usize(),
                    //     next_inst.payload_size.to_usize(),
                    // )?;

                    let stack_func = self
                        .thread
                        .global_env
                        .repl_lookup_idx(payload_size.to_usize());
                    self.ip += 1;
                    let payload_size = next_inst.payload_size.to_usize();
                    // self.handle_tail_call(stack_func, next_inst.payload_size.to_usize());

                    use SteelVal::*;

                    match stack_func {
                        FuncV(f) => {
                            let last_index = self.thread.stack.len() - payload_size;
                            let result =
                                match self.thread.enter_safepoint(move |ctx: &SteelThread| {
                                    f(&ctx.stack[last_index..])
                                }) {
                                    Ok(v) => v,
                                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                                };

                            // This is the old way... lets see if the below way improves the speed
                            self.thread.stack.truncate(last_index);
                            if let Some(r) = self.handle_pop_pure_value(result) {
                                return r;
                            }
                            Ok(())
                        }
                        MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
                        BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
                        Closure(closure) => {
                            self.new_handle_tail_call_closure(closure, payload_size)
                        }
                        BuiltIn(f) => self.call_builtin_func(f, payload_size),
                        CustomStruct(s) => self.call_custom_struct(&s, payload_size),
                        ContinuationFunction(cc) => self.call_continuation(cc),
                        _ => {
                            // println!("{:?}", self.stack);
                            // println!("{:?}", self.stack_index);
                            // println!("Bad tail call");
                            // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                            stop!(BadSyntax => format!("TailCall - Application not a procedure or function type
                    not supported: {stack_func}"); self.current_span());
                        }
                    }?
                }

                DenseInstruction {
                    op_code: OpCode::CALLGLOBALTAILNOARITY,
                    payload_size,
                    ..
                } => {
                    let next_inst = self.instructions[self.ip + 1];
                    // self.handle_tail_call_global(
                    //     payload_size.to_usize(),
                    //     next_inst.payload_size.to_usize(),
                    // )?;

                    let stack_func = self
                        .thread
                        .global_env
                        .repl_lookup_idx(payload_size.to_usize());
                    self.ip += 1;
                    let payload_size = next_inst.payload_size.to_usize();
                    // self.handle_tail_call(stack_func, next_inst.payload_size.to_usize());

                    use SteelVal::*;

                    match stack_func {
                        FuncV(f) => {
                            let last_index = self.thread.stack.len() - payload_size;
                            let result =
                                match self.thread.enter_safepoint(move |ctx: &SteelThread| {
                                    f(&ctx.stack[last_index..])
                                }) {
                                    Ok(v) => v,
                                    Err(e) => return Err(e.set_span_if_none(self.current_span())),
                                };

                            // This is the old way... lets see if the below way improves the speed
                            self.thread.stack.truncate(last_index);
                            if let Some(r) = self.handle_pop_pure_value(result) {
                                return r;
                            }
                            Ok(())
                        }
                        MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
                        BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
                        Closure(closure) => {
                            self.new_handle_tail_call_closure(closure, payload_size)
                        }
                        BuiltIn(f) => self.call_builtin_func(f, payload_size),
                        CustomStruct(s) => self.call_custom_struct(&s, payload_size),
                        ContinuationFunction(cc) => self.call_continuation(cc),
                        _ => {
                            // println!("{:?}", self.stack);
                            // println!("{:?}", self.stack_index);
                            // println!("Bad tail call");
                            // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                            stop!(BadSyntax => format!("TailCall - Application not a procedure or function type
                    not supported: {stack_func}"); self.current_span());
                        }
                    }?
                }

                DenseInstruction {
                    op_code: OpCode::FUNC | OpCode::FUNCNOARITY,
                    payload_size,
                    ..
                } => {
                    // TODO: @Matt -> don't pop the function off of the stack, just read it from there directly.
                    let func = self.thread.stack.pop().unwrap();
                    self.handle_function_call(func, payload_size.to_usize())?;
                }
                // Tail call basically says "hey this function is exiting"
                // In the closure case, transfer ownership of the stack to the called function
                DenseInstruction {
                    op_code: OpCode::TAILCALL | OpCode::TAILCALLNOARITY,
                    payload_size,
                    ..
                } => {
                    let func = self.thread.stack.pop().unwrap();
                    self.handle_tail_call(func, payload_size.to_usize())?
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
                        self.ip = payload_size.to_usize();
                    }
                }

                DenseInstruction {
                    op_code: OpCode::SELFTAILCALLNOARITY,
                    payload_size,
                } => {
                    // println!("self tail cail: {:?}", self.thread.stack);
                    let current_arity = payload_size.to_usize();

                    // let last_stack_frame = self.thread.stack_frames.last().unwrap();

                    // #[cfg(feature = "dynamic")]
                    // {
                    //     last_stack_frame.function.increment_call_count();
                    // }

                    // self.instructions = last_stack_frame.function.body_exp();
                    // dbg!(last_stack_frame.sp);
                    // dbg!(self.sp);

                    // self.sp = last_stack_frame.sp;

                    self.ip = 0;

                    // TODO: Adjust the stack for multiple arity functions
                    // let offset = last_stack_frame.sp;

                    // We should have arity at this point, drop the stack up to this point
                    // take the last arity off the stack, go back and replace those in order
                    // [... arg1 arg2 arg3]
                    //      ^^^ <- back = this index
                    // offset = the start of the stack frame
                    // Copy the arg1 arg2 arg3 values to
                    // [... frame-start ... arg1 arg2 arg3]
                    //      ^^^^^^~~~~~~~~
                    let back = self.thread.stack.len() - current_arity;
                    let _ = self.thread.stack.drain(self.sp..back);
                }

                DenseInstruction {
                    op_code: OpCode::TCOJMP,
                    payload_size,
                    ..
                } => {
                    // println!("tcojmp: {:?}", self.thread.stack);
                    let mut current_arity = payload_size.to_usize();

                    let last_stack_frame = self.thread.stack_frames.last().unwrap();

                    // #[cfg(feature = "dynamic")]
                    // {
                    //     last_stack_frame.function.increment_call_count();
                    // }

                    // self.instructions = last_stack_frame.function.body_exp();
                    // self.spans = last_stack_frame.function.spans();
                    // self.sp = last_stack_frame.sp;

                    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);

                    // panic!("Stopping");

                    self.ip = 0;

                    // TODO: Adjust the stack for multiple arity functions
                    let is_multi_arity = last_stack_frame.function.is_multi_arity;
                    let original_arity = last_stack_frame.function.arity();
                    let payload_size = current_arity;
                    // let new_arity = &mut closure_arity;

                    // TODO: Reuse the original list allocation, if it exists.
                    if likely(!is_multi_arity) {
                        if unlikely(original_arity != payload_size) {
                            stop!(ArityMismatch => format!("function expected {} arguments, found {}", original_arity, payload_size); self.current_span());
                        }
                    } else {
                        // println!(
                        //     "multi closure function, multi arity, arity: {:?} - called with: {:?}",
                        //     original_arity, payload_size
                        // );

                        if payload_size < original_arity - 1 {
                            stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", original_arity - 1, payload_size); self.current_span());
                        }

                        // (define (test x . y))
                        // (test 1 2 3 4 5)
                        // in this case, arity = 2 and payload size = 5
                        // pop off the last 4, collect into a list
                        let amount_to_remove = 1 + payload_size - original_arity;

                        let values = self
                            .thread
                            .stack
                            .drain(self.thread.stack.len() - amount_to_remove..)
                            .collect();

                        let list = SteelVal::ListV(values);

                        self.thread.stack.push(list);

                        current_arity = original_arity;
                    }

                    // HACK COME BACK TO THIS
                    // if self.ip == 0 && self.heap.len() > self.heap.limit() {
                    // TODO collect here
                    // self.heap.collect_garbage();
                    // }
                    // let offset = self.stack_index.last().copied().unwrap_or(0);
                    // let offset = last_stack_frame.sp;

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

                    let _ = self.thread.stack.drain(self.sp..back);
                }

                // Blindly go to the next instructions
                DenseInstruction {
                    op_code: OpCode::JMP,
                    payload_size,
                    ..
                } => {
                    self.ip = payload_size.to_usize();
                }

                // If the JMP points to a pop, just pop
                DenseInstruction {
                    op_code: OpCode::POPJMP,
                    ..
                } => {
                    // self.ip = payload_size.to_usize();
                    if let Some(r) = self.handle_pop_pure() {
                        return r;
                    }
                }

                DenseInstruction {
                    op_code: OpCode::BEGINSCOPE,
                    ..
                } => {
                    self.ip += 1;
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
                DenseInstruction {
                    op_code: OpCode::LETENDSCOPE,
                    ..
                } => {
                    let_end_scope_handler(self)?;
                }
                DenseInstruction {
                    op_code: OpCode::BIND,
                    payload_size,
                    ..
                } => self.handle_bind(payload_size.to_usize()),
                DenseInstruction {
                    op_code: OpCode::NEWSCLOSURE,
                    payload_size,
                    ..
                } => self.handle_new_start_closure(payload_size.to_usize())?,
                DenseInstruction {
                    op_code: OpCode::PUREFUNC,
                    payload_size,
                    ..
                } => self.handle_pure_function(payload_size.to_usize()),
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
                    self.ip += 1;
                }

                DenseInstruction {
                    op_code: OpCode::VEC,
                    payload_size,
                } => {
                    let payload = payload_size.to_usize();
                    let len = payload / 2;
                    let bytes = payload % 2 != 0;

                    let args = self.thread.stack.split_off(self.thread.stack.len() - len);

                    let val = if bytes {
                        let buffer: Vec<_> = args
                            .into_iter()
                            .flat_map(|val| {
                                let int = val.int_or_else(|| "unexpected non integer");

                                debug_assert!(int.is_ok());

                                int.ok()
                            })
                            .flat_map(|int| {
                                let byte = u8::try_from(int);

                                debug_assert!(byte.is_ok());
                                byte.ok()
                            })
                            .collect();

                        SteelVal::ByteVector(crate::rvals::SteelByteVector::new(buffer))
                    } else {
                        SteelVal::VectorV(crate::rvals::SteelVector(Gc::new(args.into())))
                    };

                    self.thread.stack.push(val);
                    self.ip += 1;
                }

                // DenseInstruction {
                //     op_code: OpCode::ECLOSURE,
                //     ..
                // } => {
                //     self.ip += 1;
                // }

                // match_dynamic_super_instructions!()
                _ => {
                    #[cfg(feature = "dynamic")]
                    // TODO: Dispatch on the function here for super instructions!
                    dynamic::vm_match_dynamic_super_instruction(self, instr)?;

                    // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                    // panic!(
                    //     "Unhandled opcode: {:?} @ {}",
                    //     self.instructions[self.ip], self.ip
                    // );
                }
            }

            #[cfg(feature = "op-code-profiling")]
            crate::steel_vm::profiling::record_next_op(self.instructions[self.ip]);
        }
    }

    fn move_from_stack(&mut self, offset: usize) -> SteelVal {
        std::mem::replace(&mut self.thread.stack[offset], SteelVal::Void)
    }

    pub(crate) fn current_span_for_index(&self, ip: usize) -> Span {
        self.thread
            .stack_frames
            .last()
            .map(|x| x.function.id)
            .and_then(|x| {
                self.thread
                    .function_interner
                    .spans
                    .get(&x)
                    .and_then(|x| x.get(ip))
            })
            .or_else(|| self.root_spans.get(ip))
            .copied()
            .unwrap_or_default()
    }

    // #[inline(always)]
    // TODO: This is definitely an issue - if the instruction stack is empty,
    // We will probably end up grabbing a garbage span
    fn current_span(&self) -> Span {
        self.current_span_for_index(self.ip)
    }

    pub(crate) fn previous_span(&self) -> Span {
        self.current_span_for_index(self.ip - 1)
    }

    // TODO: Tail calls see to obfuscate the proper span information.
    // These will need to be rewritten, somehow, assuming we have knowledge
    // if the last function was called in tail position.
    fn enclosing_span(&self) -> Option<Span> {
        if self.thread.stack_frames.len() > 1 {
            if let [second, last] = &self.thread.stack_frames[self.thread.stack_frames.len() - 2..]
            {
                let spans = self.thread.function_interner.spans.get(&second.function.id);

                spans
                    .and_then(|x| {
                        if last.ip > 1 {
                            x.get(last.ip - 1)
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
                    if frame.ip > 1 {
                        self.root_spans.get(frame.ip - 1)
                    } else {
                        None
                    }
                })
                .copied()
        }
    }

    // TODO: Anytime we genuinely close a continuation mark, we should check that we
    // are not in a recursive installment of the VM. Any recursive call sharing the
    // stack will need to be instrumented with the point in time that we are at
    // with respect to the existing continuation.
    #[inline(always)]
    fn close_continuation_marks(&self, last: &StackFrame) -> bool {
        // TODO: @Matt - continuation marks should actually do something here
        // What we'd like: This marks the stack frame going out of scope. Since it is going out of scope,
        // the stack frame should check if there are marks here, specifying that we should grab
        // the values out of the existing frame, and "close" the open continuation. That way the continuation (if called)
        // does not need to actually copy the entire frame eagerly, but rather can do so lazily.
        Continuation::close_marks(self, last)
    }

    #[inline(always)]
    fn handle_pop_pure_value(&mut self, value: SteelVal) -> Option<Result<SteelVal>> {
        // println!("calling pop: {}", value);
        // Check that the amount we're looking to pop and the function stack length are equivalent
        // otherwise we have a problem
        // println!("{} - {}", self.pop_count, self.thread.stack_frames.len());
        self.pop_count -= 1;

        let last = self.thread.stack_frames.pop();

        // let should_return = self.stack_frames.is_empty();
        let should_continue = self.pop_count != 0;

        if should_continue {
            let last = last.unwrap();
            // let last = unsafe { last.unwrap_unchecked() };

            let rollback_index = last.sp;

            self.close_continuation_marks(&last);

            // let _ = self
            //     .thread
            //     .stack
            //     .drain(rollback_index..self.thread.stack.len() - 1);

            self.thread.stack.truncate(rollback_index);
            self.thread.stack.push(value);

            self.ip = last.ip;
            self.instructions = last.instructions;

            self.sp = self.get_last_stack_frame_sp();

            None
        } else {
            // let ret_val = self.thread.stack.pop().ok_or_else(|| {
            //     SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
            //         .with_span(self.current_span())
            // });

            let ret_val = Ok(value);

            let rollback_index = last
                .map(|x| {
                    self.close_continuation_marks(&x);
                    x.sp
                })
                .unwrap_or(0);

            // Move forward past the pop
            self.ip += 1;

            self.thread.stack.truncate(rollback_index);
            self.sp = 0;

            Some(ret_val)
        }
    }

    #[inline(always)]
    fn handle_pop_pure(&mut self) -> Option<Result<SteelVal>> {
        // Check that the amount we're looking to pop and the function stack length are equivalent
        // otherwise we have a problem
        // println!("{} - {}", self.pop_count, self.thread.stack_frames.len());
        self.pop_count -= 1;

        let last = self.thread.stack_frames.pop();

        // let should_return = self.stack_frames.is_empty();
        let should_continue = self.pop_count != 0;

        if should_continue {
            let last = last.unwrap();
            // let last = unsafe { last.unwrap_unchecked() };

            let rollback_index = last.sp;

            self.close_continuation_marks(&last);

            let _ = self
                .thread
                .stack
                .drain(rollback_index..self.thread.stack.len() - 1);

            // for value in other {
            //     self.thread.delayed_dropper.push(value);
            // }

            // TODO: Delay running the destructors until a safepoint?
            // for value in values {
            //     match value {
            //         SteelVal::ListV(_) => {
            //             DROP_THREAD.send(value).unwrap();
            //         }
            //         _ => {}
            //     }
            // }

            self.ip = last.ip;
            self.instructions = last.instructions;

            self.sp = self.get_last_stack_frame_sp();

            None
        } else {
            let ret_val = self.thread.stack.pop().ok_or_else(|| {
                SteelErr::new(ErrorKind::Generic, "stack empty at pop".to_string())
                    .with_span(self.current_span())
            });

            let rollback_index = last
                .map(|x| {
                    self.close_continuation_marks(&x);
                    x.sp
                })
                .unwrap_or(0);

            // Move forward past the pop
            self.ip += 1;

            self.thread.stack.truncate(rollback_index);
            self.sp = 0;

            Some(ret_val)
        }
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

    // TODO: One neat thing to do here would be to
    // put a guard to move the value out of the top level before
    // the expression for the set, in an effort to try to
    // get in place mutation.
    // Basically a `MoveReadGlobal`
    fn handle_set(&mut self, index: usize) -> Result<()> {
        let value_to_assign = self.thread.stack.pop().unwrap();

        let value = if self.thread.safepoints_enabled {
            #[cfg(feature = "sync")]
            {
                self.thread
                    .with_locked_env(|_, env| env.set_idx(index, value_to_assign))
            }

            #[cfg(not(feature = "sync"))]
            self.thread
                .with_locked_env(|this| this.global_env.repl_set_idx(index, value_to_assign))?
        } else {
            self.thread.local_set(index, value_to_assign).unwrap()
        };

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
        // TODO: Lazily fetch the function. Avoid cloning where relevant.
        // Boxed functions probably _should_ be rooted in the modules?
        let func = self.thread.global_env.repl_lookup_idx(index);
        self.handle_global_function_call(func, payload_size)
    }

    #[inline(always)]
    fn handle_call_global_no_arity(&mut self, index: usize, payload_size: usize) -> Result<()> {
        // TODO: Lazily fetch the function. Avoid cloning where relevant.
        // Boxed functions probably _should_ be rooted in the modules?
        let func = self.thread.global_env.repl_lookup_idx(index);
        self.handle_global_function_call_no_arity(func, payload_size)
    }

    #[allow(unused)]
    #[inline(always)]
    fn handle_call_global_no_stack(
        &mut self,
        index: usize,
        payload_size: usize,
    ) -> Result<Option<SteelVal>> {
        // TODO: Lazily fetch the function. Avoid cloning where relevant.
        // Boxed functions probably _should_ be rooted in the modules?
        let func = self.thread.global_env.repl_lookup_idx(index);
        self.handle_global_function_call_no_stack(func, payload_size)
    }

    #[inline(always)]
    fn handle_tail_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
        let stack_func = self.thread.global_env.repl_lookup_idx(index);
        self.ip += 1;
        self.handle_tail_call(stack_func, payload_size)
    }

    // #[inline(always)]
    // fn handle_tail_call_global(&mut self, index: usize, payload_size: usize) -> Result<()> {
    //     let func = self.thread.global_env.repl_lookup_idx(index);
    //     self.ip += 1;

    //     // self.handle_tail_call(func, payload_size)

    // }

    #[inline(always)]
    fn handle_push(&mut self, index: usize) -> Result<()> {
        // let value = self.thread.global_env.repl_lookup_idx(index);
        let value = self
            .thread
            .global_env
            .repl_maybe_lookup_idx(index)
            .ok_or_else(throw!(Generic => "free identifier"))?;

        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    #[inline(always)]
    fn handle_local(&mut self, index: usize) -> Result<()> {
        // println!("STACK HERE: {:?}", self.thread.stack);
        // let offset = self.stack_frames.last().map(|x| x.index).unwrap_or(0);
        let offset = self.get_offset();

        let value = self.thread.stack[index + offset].clone();
        self.thread.stack.push(value);
        self.ip += 1;
        Ok(())
    }

    // How many captures?
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
        // println!("Instruction: {:?}", self.instructions[self.ip]);

        // if self.instructions[self.ip].payload_size == 1 {
        //     println!("Found multi arity function");
        // }

        assert!(self.ip < self.instructions.len());

        self.ip += 1;

        let is_multi_arity = self.instructions[self.ip].payload_size.to_u32() == 1;

        self.ip += 1;

        // Check whether this is a let or a rooted function
        let closure_id = self.instructions[self.ip].payload_size.to_u32();

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
                    // TODO: This is a bug! We can panic here if we are
                    // not running on the root?
                    // self.root_spans[self.ip..forward_jump_index].into()
                    if let Some(span_range) = self.root_spans.get(self.ip..forward_jump_index) {
                        span_range.to_vec().into()
                    } else {
                        Shared::from(Vec::new())
                    }
                } else {
                    spans[self.ip..forward_jump_index].into()
                }
            } else {
                // TODO: Under certain circumstances, it is possible we're doing some funky
                // call/cc business, and the rooted spans don't make it back on during the call/cc
                // section of the evaluation. In this case, we should try to optimistically store
                // all of the spans that we have, rather than doing it lazily like we're doing now.
                // See this body of code for an offending edge case:
                /*

                λ > (define the-empty-cont #f)
                λ > (call/cc (lambda (k) (set! the-empty-cont k)))
                => #false
                λ > the-empty-cont
                => #<procedure>
                λ > (+ 10 20 30 40 (call/cc (k) (the-empty-cont k) k))
                error[E02]: FreeIdentifier
                  ┌─ :1:26
                  │
                1 │ (+ 10 20 30 40 (call/cc (k) (the-empty-cont k) k))
                  │                          ^ k

                λ > (+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k)))
                => #<procedure>
                λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))) 100)
                => #<procedure>
                λ > (+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k)))
                => #<procedure>
                λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))))
                => #<procedure>
                λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))) 10)
                => #<procedure>
                λ > ((+ 10 20 30 40 (call/cc (lambda (k) (the-empty-cont k) k))) 100)
                => #<procedure>
                λ > (define the-next-cont #f)
                λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) k)))
                error[E03]: TypeMismatch
                  ┌─ :1:2
                  │
                1 │ (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) k)))
                  │  ^ + expects a number, found: (Continuation)

                λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))))

                => #false
                λ > the-next-cont
                => #<procedure>
                λ > (the-next-cont 10)
                => 110
                λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))) (begin (displayln "hello world") 100))
                => #false
                λ > (the-next-cont 10)
                hello world
                => 210
                // λ > (+ 10 20 30 40 (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))) (begin (displayln "hello world") (call/cc (lambda (k) (set! the-next-cont k) (the-empty-cont #f))) 100))
                => #false
                λ > (the-next-cont 10)
                hello world
                thread 'main' panicked at crates/steel-core/src/steel_vm/vm.rs:2699:32:
                range end index 31 out of range for slice of length 4
                note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

                                */
                // self.root_spans[self.ip..forward_jump_index].into()
                if let Some(span_range) = self.root_spans.get(self.ip..forward_jump_index) {
                    span_range.to_vec().into()
                } else {
                    Shared::from(Vec::new())
                }
            };

            // snag the arity from the eclosure instruction
            let arity = self.instructions[forward_index - 1].payload_size;

            let constructed_lambda = Gc::new(ByteCodeLambda::new(
                closure_id,
                closure_body,
                arity.to_usize(),
                is_multi_arity,
                CaptureVec::new(),
            ));

            self.thread
                .function_interner
                .pure_function_interner
                .insert(closure_id, Gc::clone(&constructed_lambda));

            // Put the spans into the interner as well
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

        let is_multi_arity = self.instructions[self.ip].payload_size.to_u32() == 1;

        self.ip += 1;

        // Get the ID of the function
        let closure_id = self.instructions[self.ip].payload_size.to_u32();

        // if is_multi_arity {
        //     println!("Found multi arity function");
        // }

        self.ip += 1;

        // TODO - used to be offset - 2, now 3 with the multi arity
        // let forward_jump = offset;
        let forward_jump = offset - 3;
        // println!("Forward jump: {}", forward_jump);

        // Snag the number of upvalues here
        let ndefs = self.instructions[self.ip].payload_size.to_usize();
        self.ip += 1;

        // TODO preallocate size
        let mut captures = CaptureVec::with_capacity(ndefs);

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
            let stack_index = self.get_offset();

            for _ in 0..ndefs {
                let instr = self.instructions[self.ip];
                match (instr.op_code, instr.payload_size) {
                    (OpCode::COPYCAPTURESTACK, n) => {
                        let offset = stack_index;
                        let value = self.thread.stack[n.to_usize() + offset].clone();
                        captures.push(value);
                    }
                    (OpCode::COPYCAPTURECLOSURE, n) => {
                        debug_assert!(
                            !self.thread.stack_frames.is_empty(),
                            "Trying to capture from closure that doesn't exist",
                        );

                        debug_assert!((n.to_usize()) < guard.function.captures().len());

                        let value = guard.function.captures()[n.to_usize()].clone();

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
                        let value = self.thread.stack[n.to_usize() + offset].clone();
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
            // prototype.set_heap_allocated(heap_vars);
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
                    // self.root_spans[self.ip..forward_jump_index].into()

                    if let Some(span_range) = self.root_spans.get(self.ip..forward_jump_index) {
                        span_range.to_vec().into()
                    } else {
                        Shared::from(Vec::new())
                    }
                } else {
                    spans[self.ip..forward_jump_index].into()
                }
            } else {
                // TODO: This seems to be causing an error
                // self.root_spans[self.ip..forward_jump_index].into()

                // For now, lets go ahead and use this... hack to get us going
                if let Some(span_range) = self.root_spans.get(self.ip..forward_jump_index) {
                    span_range.to_vec().into()
                } else {
                    Shared::from(Vec::new())
                }
            };

            // snag the arity from the eclosure instruction
            let arity = self.instructions[forward_jump_index].payload_size;

            let mut constructed_lambda = ByteCodeLambda::new(
                closure_id,
                closure_body,
                arity.to_usize(),
                is_multi_arity,
                CaptureVec::new(),
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

            constructed_lambda
        };

        self.thread
            .stack
            .push(SteelVal::Closure(Gc::new(constructed_lambda)));

        self.ip = forward_index;
        Ok(())
    }

    // #[inline(always)]
    fn handle_bind(&mut self, payload_size: usize) {
        let value = self.thread.stack.pop().unwrap();

        #[cfg(feature = "sync")]
        self.thread.with_locked_env(move |_, env| {
            env.repl_define_idx(payload_size, value);
        });

        #[cfg(not(feature = "sync"))]
        self.thread.with_locked_env(move |this| {
            this.global_env.repl_define_idx(payload_size, value);
        });

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

    // Calls the given function in tail position.
    fn new_handle_tail_call_closure(
        &mut self,
        closure: Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        self.cut_sequence();

        let mut new_arity = payload_size;

        self.adjust_stack_for_multi_arity(&closure, payload_size, &mut new_arity)?;

        // TODO: Try to figure out if we need to close this frame
        // self.close_continuation_marks(self.thread.stack_frames.last().unwrap());

        let last = self.thread.stack_frames.last_mut().unwrap();

        let offset = last.sp;

        // We should have arity at this point, drop the stack up to this point
        // take the last arity off the stack, go back and replace those in order
        let back = self.thread.stack.len() - new_arity;

        // println!("{}..{}", offset, back);
        // println!("{:?}", self.thread.stack);
        // println!("{}..{}", offset, back);
        // println!("{:?}", self.thread.stack);

        let _ = self.thread.stack.drain(offset..back);

        // TODO: Perhaps add call to minor collection here?
        // Could be a good way to avoid running the whole mark and sweep,
        // since values will have left the scope by now.

        self.instructions = closure.body_exp();

        last.set_function(closure);

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
                stop!(ArityMismatch => format!("function expected at least {} arguments, found {}", closure.arity() - 1, payload_size); self.current_span());
            }

            // (define (test x . y))
            // (test 1 2 3 4 5)
            // in this case, arity = 2 and payload size = 5
            // pop off the last 4, collect into a list
            let amount_to_remove = 1 + payload_size - closure.arity();

            let values = self
                .thread
                .stack
                .drain(self.thread.stack.len() - amount_to_remove..)
                .collect();
            // .split_off(self.thread.stack.len() - amount_to_remove);

            let list = SteelVal::ListV(values);

            self.thread.stack.push(list);

            *new_arity = closure.arity();

            // println!("Stack after list conversion: {:?}", self.stack);
        }

        // else if closure.arity() != payload_size {
        //     stop!(ArityMismatch => format!("function expected {} arguments, found {}", closure.arity(), payload_size); self.current_span());
        // }

        Ok(())
    }

    #[inline(always)]
    fn cut_sequence(&mut self) {
        #[cfg(feature = "dynamic")]
        if let Some(pat) = self.thread.profiler.cut_sequence(
            &self.instructions,
            self.thread.stack_frames.last().map(|x| x.function.as_ref()),
        ) {
            log::debug!(target: "super-instructions", "Found a hot pattern, creating super instruction...");
            log::debug!(target: "super-instructions", "{:#?}", pat);

            // if USE_SUPER_INSTRUCTIONS {
            //     // Index of the starting opcode
            //     let start = pat.pattern.start;

            //     let id = self.thread.super_instructions.len();

            //     let guard = self.thread.stack_frames.last_mut().unwrap();

            //     // We don't want to repeatedly thrash by calculating hashes for the block pattern, so
            //     // we mark the tail of the block directly on the function itself.
            //     // guard.function.mark_block_tail(self.ip);

            //     // Next run should get the new sequence of opcodes
            //     let (head, _) = guard.function.update_to_super_instruction(start, id);

            //     let block = DynamicBlock::construct_basic_block(head, pat);
            //     self.thread.super_instructions.push(Rc::new(block));
            // }
        }
    }

    #[inline(always)]
    fn handle_tail_call(&mut self, stack_func: SteelVal, payload_size: usize) -> Result<()> {
        use SteelVal::*;

        match stack_func {
            FuncV(f) => self.call_primitive_func(f, payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
            Closure(closure) => self.new_handle_tail_call_closure(closure, payload_size),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            CustomStruct(s) => self.call_custom_struct(&s, payload_size),
            ContinuationFunction(cc) => self.call_continuation(cc),
            _ => {
                // println!("{:?}", self.stack);
                // println!("{:?}", self.stack_index);
                // println!("Bad tail call");
                // crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
                stop!(BadSyntax => format!("TailCall - Application not a procedure or function type
                    not supported: {stack_func}"); self.current_span());
            }
        }
    }

    // #[inline(always)]
    fn call_boxed_func(
        &mut self,
        func: &dyn Fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        let last_index = self.thread.stack.len() - payload_size;

        let result = self
            .thread
            .enter_safepoint(|ctx| func(&ctx.stack[last_index..]))
            .map_err(|x| x.set_span_if_none(self.current_span()))?;

        // TODO: Drain, and push onto another thread to drop?
        self.thread.stack.truncate(last_index);
        self.thread.stack.push(result);
        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_builtin_func(&mut self, func: BuiltInSignature, payload_size: usize) -> Result<()> {
        // println!("Calling builtin function");

        // Note: We Advance the pointer here. In the event we're calling a builtin that fusses with
        // the instruction pointer, we allow the function to override this. For example, call/cc will
        // advance the pointer - or perhaps, even fuss with the control flow.
        self.ip += 1;

        // TODO: Don't do this - just read directly from the stack
        let args = self
            .thread
            .stack
            .drain(self.thread.stack.len() - payload_size..)
            .collect::<SmallVec<[_; 4]>>();

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
        let last_index = self.thread.stack.len() - payload_size;

        // These kinds of functions aren't valid for a safepoint.
        let result = f(&mut self.thread.stack[last_index..])
            .map_err(|x| x.set_span_if_none(self.current_span()))?;

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

    #[inline(always)]
    fn call_primitive_func(
        &mut self,
        f: fn(&[SteelVal]) -> Result<SteelVal>,
        payload_size: usize,
    ) -> Result<()> {
        let last_index = self.thread.stack.len() - payload_size;

        let result = match self
            .thread
            .enter_safepoint(move |ctx: &SteelThread| f(&ctx.stack[last_index..]))
        {
            Ok(v) => v,
            Err(e) => return Err(e.set_span_if_none(self.current_span())),
        };

        // This is the old way... lets see if the below way improves the speed
        self.thread.stack.truncate(last_index);
        self.thread.stack.push(result);

        self.ip += 1;
        Ok(())
    }

    // #[inline(always)]
    fn call_future_func(
        &mut self,
        // f: Shared<Box<dyn Fn(&[SteelVal]) -> Result<FutureResult>>>,
        f: BoxedAsyncFunctionSignature,
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
    fn call_continuation(&mut self, continuation: Continuation) -> Result<()> {
        let last =
            self.thread.stack.pop().ok_or_else(
                throw!(ArityMismatch => "continuation expected 1 argument, found none"),
            )?;

        // println!("Calling continuation...");

        Continuation::set_state_from_continuation(self, continuation);

        // match Gc::try_unwrap(continuation) {
        //     Ok(cont) => {
        //         self.set_state_from_continuation(cont);
        //     }

        //     Err(unable_to_unwrap) => {
        //         self.set_state_from_continuation(unable_to_unwrap.unwrap());
        //     }
        // }

        // self.set_state_from_continuation(continuation.clone());

        self.ip += 1;

        self.thread.stack.push(last);
        Ok(())
    }

    #[inline(always)]
    fn check_stack_overflow(&self) -> Result<()> {
        if CHECK_STACK_OVERFLOW {
            if unlikely(self.thread.stack_frames.len() >= STACK_LIMIT) {
                // let last_id = self.thread.stack_frames.last().unwrap().function.id;

                // last 100
                // let last = self
                //     .thread
                //     .stack_frames
                //     .get(self.thread.stack_frames.len() - 10000..self.thread.stack_frames.len())
                //     .unwrap();

                // let last = last.iter().map(|x| x.function.id).collect::<Vec<_>>();

                // dbg!(last);

                crate::core::instructions::pretty_print_dense_instructions(&self.instructions);
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

    // // #[inline(always)]
    pub(crate) fn handle_function_call_closure(
        &mut self,
        closure: Gc<ByteCodeLambda>,
        payload_size: usize,
    ) -> Result<()> {
        self.cut_sequence();

        #[cfg(feature = "jit")]
        {
            closure.increment_call_count();
        }

        self.adjust_stack_for_multi_arity(&closure, payload_size, &mut 0)?;

        self.sp = self.thread.stack.len() - closure.arity();

        let instructions = closure.body_exp();

        self.thread.stack_frames.push(StackFrame::new(
            self.sp,
            closure,
            self.ip + 1,
            std::mem::replace(&mut self.instructions, instructions),
        ));

        self.check_stack_overflow()?;

        self.pop_count += 1;

        // self.instructions = instructions;

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
        self.adjust_stack_for_multi_arity(&closure, payload_size, &mut 0)?;
        self.sp = self.thread.stack.len() - closure.arity();

        #[cfg(not(feature = "rooted-instructions"))]
        {
            let mut instructions = closure.body_exp();

            std::mem::swap(&mut instructions, &mut self.instructions);

            // Do this _after_ the multi arity business
            // TODO: can these rcs be avoided
            self.thread.stack_frames.push(
                StackFrame::new(self.sp, closure, self.ip + 1, instructions), // .with_span(self.current_span()),
            );
        }

        #[cfg(feature = "rooted-instructions")]
        {
            let frame = StackFrame {
                sp: self.sp,
                function: closure,
                ip: self.ip + 1,
                instructions: self.instructions,
                attachments: None,
            };
            self.instructions = frame.function.body_exp();
            self.thread.stack_frames.push(frame);
        }

        self.check_stack_overflow()?;
        self.pop_count += 1;
        self.ip = 0;
        Ok(())
    }

    #[inline(always)]
    fn handle_function_call_closure_jit_no_arity(
        &mut self,
        closure: Gc<ByteCodeLambda>,
    ) -> Result<()> {
        self.sp = self.thread.stack.len() - closure.arity();

        #[cfg(not(feature = "rooted-instructions"))]
        {
            let mut instructions = closure.body_exp();

            std::mem::swap(&mut instructions, &mut self.instructions);

            // Do this _after_ the multi arity business
            // TODO: can these rcs be avoided
            self.thread.stack_frames.push(
                StackFrame::new(self.sp, closure, self.ip + 1, instructions), // .with_span(self.current_span()),
            );
        }

        #[cfg(feature = "rooted-instructions")]
        {
            let frame = StackFrame {
                sp: self.sp,
                function: closure,
                ip: self.ip + 1,
                instructions: self.instructions,
                attachments: None,
            };
            self.instructions = frame.function.body_exp();
            self.thread.stack_frames.push(frame);
        }

        self.check_stack_overflow()?;
        self.pop_count += 1;
        self.ip = 0;
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
            ContinuationFunction(cc) => self.call_continuation(cc),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            CustomStruct(s) => self.call_custom_struct(&s, payload_size),
            _ => {
                cold();
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }
    }

    #[inline(always)]
    fn handle_global_function_call_no_arity(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
    ) -> Result<()> {
        use SteelVal::*;

        match stack_func {
            Closure(closure) => self.handle_function_call_closure_jit_no_arity(closure),
            FuncV(f) => self.call_primitive_func(f, payload_size),
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            FutureFunc(f) => self.call_future_func(f, payload_size),
            ContinuationFunction(cc) => self.call_continuation(cc),
            BuiltIn(f) => self.call_builtin_func(f, payload_size),
            CustomStruct(s) => self.call_custom_struct(&s, payload_size),
            _ => {
                cold();
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }
    }
    #[inline(always)]
    fn handle_global_function_call_no_stack(
        &mut self,
        stack_func: SteelVal,
        payload_size: usize,
    ) -> Result<Option<SteelVal>> {
        use SteelVal::*;

        match stack_func {
            Closure(closure) => {
                self.handle_function_call_closure_jit(closure, payload_size)?;
                Ok(None)
            }
            FuncV(f) => {
                let this = &mut *self;
                let last_index = this.thread.stack.len() - payload_size;

                let result = match this
                    .thread
                    .enter_safepoint(move |ctx: &SteelThread| f(&ctx.stack[last_index..]))
                {
                    Ok(v) => v,
                    Err(e) => return Err(e.set_span_if_none(this.current_span())),
                };

                // This is the old way... lets see if the below way improves the speed
                this.thread.stack.truncate(last_index);
                this.ip += 1;
                Ok(Some(result))
            }
            BoxedFunction(f) => {
                let this = &mut *self;
                let func: &dyn Fn(&[SteelVal]) -> Result<SteelVal> = f.func();
                let last_index = this.thread.stack.len() - payload_size;

                let result = this
                    .thread
                    .enter_safepoint(|ctx| func(&ctx.stack[last_index..]))
                    .map_err(|x| x.set_span_if_none(this.current_span()))?;

                // TODO: Drain, and push onto another thread to drop?
                this.thread.stack.truncate(last_index);
                this.ip += 1;
                Ok(Some(result))
            }
            MutFunc(f) => {
                let this = &mut *self;
                let last_index = this.thread.stack.len() - payload_size;

                // These kinds of functions aren't valid for a safepoint.
                let result = f(&mut this.thread.stack[last_index..])
                    .map_err(|x| x.set_span_if_none(this.current_span()))?;

                this.thread.stack.truncate(last_index);
                this.ip += 1;
                Ok(Some(result))
            }
            FutureFunc(f) => {
                let this = &mut *self;
                let last_index = this.thread.stack.len() - payload_size;

                let result = SteelVal::FutureV(Gc::new(f(&this.thread.stack[last_index..])?));

                this.thread.stack.truncate(last_index);
                this.ip += 1;
                Ok(Some(result))
            }
            ContinuationFunction(cc) => {
                self.call_continuation(cc)?;
                Ok(None)
            }
            BuiltIn(f) => {
                let this = &mut *self;
                // println!("Calling builtin function");

                // Note: We Advance the pointer here. In the event we're calling a builtin that fusses with
                // the instruction pointer, we allow the function to override this. For example, call/cc will
                // advance the pointer - or perhaps, even fuss with the control flow.
                this.ip += 1;

                // TODO: Don't do this - just read directly from the stack
                let args = this
                    .thread
                    .stack
                    .drain(this.thread.stack.len() - payload_size..)
                    .collect::<SmallVec<[_; 4]>>();

                let result = f(this, &args).map(|x| {
                    x.map_err(|x| {
                        if x.has_span() {
                            x
                        } else {
                            x.set_span_if_none(this.current_span())
                        }
                    })
                });

                if let Some(result) = result {
                    // this.thread.stack.push(result?);

                    Ok(Some(result?))
                } else {
                    Ok(None)
                }

                // self.stack.push(result);
                // Ok(())
            }
            CustomStruct(s) => {
                let this = &mut *self;
                let s: &UserDefinedStruct = &s;
                if let Some(procedure) = s.maybe_proc() {
                    if let SteelVal::HeapAllocated(h) = procedure {
                        this.handle_global_function_call_no_stack(h.get(), payload_size)
                    } else {
                        this.handle_global_function_call_no_stack(procedure.clone(), payload_size)
                    }
                } else {
                    stop!(Generic => "Attempted to call struct as a function - no procedure found!");
                }
            }
            _ => {
                cold();
                stop!(BadSyntax => format!("Function application not a procedure or function type not supported: {}", stack_func); self.current_span());
            }
        }
    }

    // #[inline(always)]
    fn handle_function_call(&mut self, stack_func: SteelVal, payload_size: usize) -> Result<()> {
        use SteelVal::*;

        match stack_func {
            FuncV(f) => self.call_primitive_func(f, payload_size),
            MutFunc(f) => self.call_primitive_mut_func(f, payload_size),
            Closure(closure) => self.handle_function_call_closure(closure, payload_size),
            BoxedFunction(f) => self.call_boxed_func(f.func(), payload_size),
            FutureFunc(f) => self.call_future_func(f, payload_size),
            ContinuationFunction(cc) => self.call_continuation(cc),
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

// TODO: This is gonna cause issues assuming this was called in tail call.
pub fn current_function_span(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if !args.is_empty() {
        builtin_stop!(ArityMismatch => format!("current-function-span requires no arguments, found {}", args.len()))
    }

    match ctx.enclosing_span() {
        Some(s) => Some(Span::into_steelval(s)),
        None => Some(Ok(SteelVal::Void)),
    }
}

fn inspect_impl(ctx: &VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let guard = &ctx.thread.compiler.read().sources.sources;

    if let Some(SteelVal::Closure(c)) = args.first() {
        let spans = ctx.thread.function_interner.spans.get(&c.id);

        let instructions = c.body_exp();

        let first_column_width = instructions.len().to_string().len();
        let second_column_width = instructions
            .iter()
            .map(|x| format!("{:?}", x.op_code).len())
            .max()
            .unwrap();
        let third_column_width = instructions
            .iter()
            .map(|x| x.payload_size.to_string().len())
            .max()
            .unwrap();

        let mut buffer = String::new();

        for (i, instruction) in c.body_exp().iter().enumerate() {
            let span = spans.and_then(|x| x.get(i));

            let index = i.to_string();

            buffer.push_str(index.as_str());
            for _ in 0..(first_column_width - index.len()) {
                buffer.push(' ');
            }

            buffer.push_str("    ");

            let op_code = format!("{:?}", instruction.op_code);
            buffer.push_str(op_code.as_str());
            for _ in 0..(second_column_width - op_code.len()) {
                buffer.push(' ');
            }

            buffer.push_str(" : ");

            let payload_size = instruction.payload_size.to_string();
            buffer.push_str(payload_size.as_str());
            for _ in 0..(third_column_width - payload_size.len()) {
                buffer.push(' ');
            }

            buffer.push_str("    ");

            if let Some(source_id) = span.and_then(|x| x.source_id()) {
                let source = guard.get(source_id);
                if let Some(span) = span {
                    if let Some(source) = source {
                        let range = source.get(span.start as usize..span.end as usize);

                        if let Some(range) =
                            range.and_then(|x| if x.len() > 30 { x.get(0..30) } else { Some(x) })
                        {
                            buffer.push_str(";; ");
                            buffer.push_str(range);
                        }
                    }
                }
            }

            println!("{}", buffer);
            buffer.clear();
        }
    }

    Some(Ok(SteelVal::Void))
}

#[steel_derive::context(name = "inspect", arity = "Exact(1)")]
pub fn inspect(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    inspect_impl(ctx, args)
}

/// Inspect the locals at the given function. Probably need to provide a way to
/// loop this back into the sources, in order to resolve any span information.
pub fn breakpoint(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let offset = ctx.get_offset();

    // Wait for user input to continue...
    // -> Evaluation context, but lets first see if we can resolve what everything is on the stack

    let guard = &ctx.thread.compiler.read().sources.sources;

    // Determine the globals at the current spot, use the span in order to resolve
    // what they're looking for, into something more interesting.
    // - Note: This can probably be done externally, as a library, in order to avoid
    // having to bundle read line into steel-core
    for (index, instr) in ctx.instructions.iter().enumerate() {
        match instr {
            DenseInstruction {
                op_code:
                    OpCode::READLOCAL
                    | OpCode::MOVEREADLOCAL
                    | OpCode::MOVEREADLOCAL0
                    | OpCode::MOVEREADLOCAL1
                    | OpCode::MOVEREADLOCAL2
                    | OpCode::MOVEREADLOCAL3
                    | OpCode::READLOCAL0
                    | OpCode::READLOCAL1
                    | OpCode::READLOCAL2
                    | OpCode::READLOCAL3,
                payload_size,
            } => {
                let span = ctx.current_span_for_index(index);

                if let Some(source_id) = span.source_id() {
                    let source = guard.get(source_id);

                    if let Some(source) = source {
                        let range = source.get(span.start as usize..span.end as usize);

                        println!(
                            "x{} = {:?} = {}",
                            payload_size,
                            range,
                            ctx.thread.stack[payload_size.to_usize() + offset]
                        );
                    }
                }
            }

            DenseInstruction {
                op_code: OpCode::READCAPTURED,
                payload_size,
            } => {
                let span = ctx.current_span_for_index(index);

                if let Some(source_id) = span.source_id() {
                    let source = guard.get(source_id);

                    if let Some(source) = source {
                        let range = source.get(span.start as usize..span.end as usize);

                        let value = ctx.thread.stack_frames.last().unwrap().function.captures()
                            [payload_size.to_usize()]
                        .clone();

                        println!("x(captured){} = {:?} = {}", payload_size, range, value);
                    }
                }
            }

            _ => {}
        }
    }

    Some(Ok(SteelVal::Void))
}

#[steel_derive::context(name = "call-with-exception-handler", arity = "Exact(2)")]
pub fn call_with_exception_handler(
    ctx: &mut VmCore,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    let handler = args[0].clone();
    let thunk = args[1].clone();

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

            // Push the previous state on
            ctx.thread.stack_frames.push(
                StackFrame::new(
                    ctx.sp,
                    Gc::clone(&closure),
                    ctx.ip + 1,
                    // Shared::clone(&ctx.instructions),
                    ctx.instructions.clone(),
                )
                .with_handler(handler),
            );

            ctx.pop_count += 1;

            ctx.instructions = closure.body_exp();

            ctx.ip = 0;
        }

        _ => {
            builtin_stop!(TypeMismatch => format!("call-with-exception-handler expects a thunk as an argument, found: {thunk}"); ctx.current_span())
        }
    }

    None
}

#[steel_derive::context(name = "call/cc", arity = "Exact(1)")]
pub fn call_cc(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    /*
    - Construct the continuation
    - Get the function that has been passed in (off the stack)
    - Apply the function with the continuation
    - Handle continuation function call separately in the handle_func_call
    */

    // Roll back one because we advanced prior to entering the builtin
    ctx.ip -= 1;

    let function = args[0].clone();

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
                    // Shared::clone(&ctx.instructions),
                    ctx.instructions.clone(),
                )
                .with_continuation_mark(continuation.clone()),
            );

            ctx.pop_count += 1;

            ctx.instructions = closure.body_exp();

            ctx.ip = 0;
        }
        SteelVal::ContinuationFunction(cc) => {
            Continuation::set_state_from_continuation(ctx, cc);

            ctx.ip += 1;
        }
        SteelVal::FuncV(f) => return Some(f(&[SteelVal::ContinuationFunction(continuation)])),

        _ => {
            builtin_stop!(Generic => format!("call/cc expects a function, found: {function}"));
        }
    }

    Some(Ok(SteelVal::ContinuationFunction(continuation)))
}

fn eval_impl(ctx: &mut crate::steel_vm::vm::VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let expr = crate::parser::ast::TryFromSteelValVisitorForExprKind::root(&args[0])?;
    // TODO: Looks like this isn't correctly parsing / pushing down macros!
    // This needs to extract macros

    let res = ctx
        .thread
        .compiler
        .write()
        .compile_executable_from_expressions(vec![expr]);

    // Gc the sources, if possible
    ctx.thread
        .compiler
        .write()
        .gc_sources(ctx.thread.function_interner.spans.values());

    match res {
        Ok(program) => {
            let result = program.build(
                "eval-context".to_string(),
                &mut ctx.thread.compiler.write().symbol_map,
            )?;

            eval_program(result, ctx)?;

            Ok(SteelVal::Void)
        }
        Err(e) => Err(e),
    }
}

fn eval_program(program: crate::compiler::program::Executable, ctx: &mut VmCore) -> Result<()> {
    let current_instruction = ctx.instructions[ctx.ip - 1];

    let tail_call = matches!(
        current_instruction.op_code,
        OpCode::TAILCALL | OpCode::CALLGLOBALTAIL | OpCode::CALLGLOBALTAILNOARITY
    );

    let Executable {
        instructions,
        spans,
        ..
    } = program;
    let mut bytecode = Vec::new();
    let mut new_spans = Vec::new();

    // let mut global_offset = 0;

    // Rewrite relative jumps at the top level into absolute jumps.
    for (instr, span) in instructions.into_iter().zip(spans) {
        let mut depth = 0;

        let offset = bytecode.len();

        new_spans.extend_from_slice(&span);
        bytecode.extend_from_slice(&instr);

        bytecode
            .last_mut()
            .ok_or_else(throw!(Generic => "Compilation error: empty expression"))?
            .op_code = OpCode::POPSINGLE;

        for instruction in &mut bytecode[offset..] {
            match instruction {
                DenseInstruction {
                    op_code: OpCode::JMP | OpCode::IF,
                    payload_size,
                } => {
                    if depth == 0 {
                        *payload_size = *payload_size + u24::from_usize(offset);
                    }
                }
                DenseInstruction {
                    op_code: OpCode::BIND,
                    payload_size,
                } => {
                    let mut compiler_guard = ctx.thread.compiler.write();
                    if compiler_guard
                        .symbol_map
                        .free_list
                        .recently_freed
                        .contains(&payload_size.to_usize())
                    {
                        compiler_guard
                            .symbol_map
                            .free_list
                            .recently_freed
                            .remove(&payload_size.to_usize());
                    }
                }
                DenseInstruction {
                    op_code:
                        OpCode::PUSH
                        | OpCode::CALLGLOBAL
                        | OpCode::CALLGLOBALTAIL
                        | OpCode::CALLGLOBALNOARITY
                        | OpCode::CALLGLOBALTAILNOARITY,
                    payload_size,
                } => {
                    if depth == 0 {
                        // if payload_size.to_usize() >= ctx.thread.global_env.len() + global_offset {
                        //     let guard = ctx.thread.compiler.read();
                        //     let value = guard
                        //         .symbol_map
                        //         .values()
                        //         .get(payload_size.to_usize())
                        //         .map(|x| x.resolve());

                        //     let recently_freed = guard
                        //         .symbol_map
                        //         .free_list
                        //         .recently_freed
                        //         .contains(&payload_size.to_usize());

                        //     stop!(Generic => "Free identifier: eval referenced an identifier before it was bound: {:?} - {} - {} - {} - {}",
                        //           value, payload_size.to_usize(), ctx.thread.global_env.len() + global_offset, recently_freed, global_offset)
                        // }

                        let compiler_guard = ctx.thread.compiler.read();

                        // TODO: Figure out how to make the recently freed list
                        // move down in size. Eval is the only way that we could
                        // reclaim these slots, so assuming there isn't any
                        // eval, we'll want to make sure this goes down in size.
                        if compiler_guard
                            .symbol_map
                            .free_list
                            .recently_freed
                            .contains(&payload_size.to_usize())
                            && ctx
                                .thread
                                .global_env
                                .repl_maybe_lookup_idx(payload_size.to_usize())
                                == Some(SteelVal::Void)
                        {
                            stop!(Generic => "Free identifier: eval (maybe) referenced an identifier before it was bound");
                        }
                    }
                }
                DenseInstruction {
                    op_code: OpCode::NEWSCLOSURE | OpCode::PUREFUNC,
                    ..
                } => {
                    depth += 1;
                }
                DenseInstruction {
                    op_code: OpCode::ECLOSURE,
                    ..
                } => {
                    depth -= 1;
                }
                _ => {}
            }
        }
    }

    // TODO: Fix the unwrap here
    if let Some(last) = bytecode.last_mut() {
        last.op_code = OpCode::POPPURE;
    } else {
        // Push an op code void?
        bytecode.push(DenseInstruction {
            op_code: OpCode::VOID,
            payload_size: crate::core::instructions::u24::from_u32(0),
        });
        bytecode.push(DenseInstruction {
            op_code: OpCode::POPPURE,
            payload_size: crate::core::instructions::u24::from_u32(0),
        });
    }

    let function_id = crate::compiler::code_gen::fresh_function_id();
    let function = Gc::new(ByteCodeLambda::new(
        function_id as _,
        StandardShared::from(bytecode),
        0,
        false,
        CaptureVec::new(),
    ));
    ctx.thread
        .function_interner
        .spans
        .insert(function_id as _, Shared::from(new_spans));

    if tail_call {
        ctx.new_handle_tail_call_closure(function, 0).unwrap();
    } else {
        ctx.ip -= 1;
        ctx.handle_function_call_closure(function, 0).unwrap();
    }

    Ok(())
}

#[steel_derive::function(name = "emit-expanded", arity = "Exact(1)")]
fn emit_expanded_file(path: String) {
    let mut engine = crate::steel_vm::engine::Engine::new();

    let contents = std::fs::read_to_string(&path).unwrap();

    engine.expand_to_file(contents, std::path::PathBuf::from(path))
}

#[steel_derive::function(name = "load-expanded", arity = "Exact(1)")]
fn load_expanded_file(path: String) {
    let mut engine = crate::steel_vm::engine::Engine::new();
    engine.load_from_expanded_file(&path)
}

#[steel_derive::context(name = "eval", arity = "Exact(1)")]
fn eval(ctx: &mut crate::steel_vm::vm::VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    match eval_impl(ctx, args) {
        Ok(_) => None,
        Err(e) => Some(Err(e)),
    }
}

#[steel_derive::context(name = "load", arity = "Exact(1)")]
fn eval_file(ctx: &mut crate::steel_vm::vm::VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    match eval_file_impl(ctx, args) {
        Ok(_) => None,
        Err(e) => Some(Err(e)),
    }
}

#[steel_derive::context(name = "eval-string", arity = "Exact(1)")]
fn eval_string(
    ctx: &mut crate::steel_vm::vm::VmCore,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    match eval_string_impl(ctx, args) {
        Ok(_) => None,
        Err(e) => Some(Err(e)),
    }
}

#[steel_derive::context(name = "#%expand", arity = "Exact(1)")]
fn expand_syntax_objects(
    ctx: &mut crate::steel_vm::vm::VmCore,
    args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    Some(expand_impl(ctx, args))
}

// Expand syntax objects?
fn expand_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    // Syntax Objects -> Expr, expand, put back to syntax objects.
    let expr = crate::parser::ast::TryFromSteelValVisitorForExprKind::root(&args[0])?;

    let res = ctx
        .thread
        .compiler
        .write()
        .lower_expressions_impl(vec![expr], None)?;

    crate::parser::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(
        res.into_iter().next().unwrap(),
    )
}

fn eval_file_impl(ctx: &mut crate::steel_vm::vm::VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let path = SteelString::from_steelval(&args[0])?;

    let mut file = std::fs::File::open(path.as_str())?;

    let mut exprs = String::new();
    file.read_to_string(&mut exprs)?;

    let res = ctx
        .thread
        .compiler
        .write()
        .compile_executable(exprs, Some(std::path::PathBuf::from(path.as_str())));

    ctx.thread
        .compiler
        .write()
        .gc_sources(ctx.thread.function_interner.spans.values());

    match res {
        Ok(program) => {
            let result = program.build(
                "eval-context".to_string(),
                &mut ctx.thread.compiler.write().symbol_map,
            )?;

            eval_program(result, ctx)?;

            Ok(SteelVal::Void)
        }
        Err(e) => Err(e),
    }
}

fn eval_string_impl(ctx: &mut crate::steel_vm::vm::VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let string = SteelString::from_steelval(&args[0])?;

    let res = ctx
        .thread
        .compiler
        .write()
        .compile_executable(string.to_string(), None);

    ctx.thread
        .compiler
        .write()
        .gc_sources(ctx.thread.function_interner.spans.values());

    match res {
        Ok(program) => {
            let result = program.build(
                "eval-context".to_string(),
                &mut ctx.thread.compiler.write().symbol_map,
            )?;

            eval_program(result, ctx)?;

            Ok(SteelVal::Void)
        }
        Err(e) => Err(e),
    }
}
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

    // Find all of the modules that are
    let modules = ctx
        .thread
        .global_env
        .roots()
        .iter()
        .filter(|x| BuiltInModule::as_ref(x).is_ok())
        .cloned()
        .collect();

    Some(Ok(SteelVal::ListV(modules)))
}

pub(crate) fn environment_offset(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(ctx.thread.global_env.len().into_steelval().unwrap()))
}

// Should really change this to be:
// Snag values, then expand them, then convert back? The constant conversion
// back and forth will probably hamper performance significantly. That being said,
// it is entirely at compile time, so probably _okay_
pub(crate) fn expand_syntax_case_impl(_ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let mut bindings: rustc_hash::FxHashMap<_, _> = if let SteelVal::HashMapV(h) = &args[1] {
        h.iter()
            .map(|(k, v)| match (k, v) {
                (SteelVal::SymbolV(k), _e) => Ok((
                    InternedString::from_str(k.as_str()),
                    crate::parser::ast::TryFromSteelValVisitorForExprKind::root(v)?,
                )),
                _ => stop!(TypeMismatch => "#%expand-template error"),
            })
            .collect::<Result<_>>()?
    } else {
        stop!(TypeMismatch => "#%expand-template expected a map of bindings")
    };

    let mut binding_kind: rustc_hash::FxHashMap<_, _> = if let SteelVal::HashMapV(h) = &args[2] {
        h.iter()
            .map(|(k, v)| match (k, v) {
                (SteelVal::SymbolV(k), e) => Ok((
                    InternedString::from_str(k.as_str()),
                    if usize::from_steelval(e)? == 1 {
                        BindingKind::Many
                    } else {
                        BindingKind::Single
                    },
                )),
                _ => stop!(TypeMismatch => "#%expand-template error"),
            })
            .collect::<Result<_>>()?
    } else {
        stop!(TypeMismatch => "#%expand-template expected a map of bindings")
    };

    if bindings.is_empty() && binding_kind.is_empty() {
        // If its a quoted expr, convert it.
        if let SteelVal::SyntaxObject(_) = &args[0] {
            return Ok(args[0].clone());
        } else {
            let template = crate::parser::ast::TryFromSteelValVisitorForExprKind::root(&args[0])?;
            return crate::parser::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(
                template,
            );
        }
    }

    let mut template = crate::parser::ast::TryFromSteelValVisitorForExprKind::root(&args[0])?;

    expand_template(&mut template, &mut bindings, &mut binding_kind)?;

    crate::parser::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(template)
}

#[steel_derive::context(name = "#%expand-syntax-case", arity = "Exact(3)")]
pub(crate) fn expand_syntax_case(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(expand_syntax_case_impl(ctx, args))
}

pub(crate) fn match_syntax_case_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let macro_name: InternedString = String::from_steelval(&args[0])?.into();
    let guard = ctx.thread.compiler.read();
    let macro_object = guard.macro_env.get(&macro_name).ok_or_else(
        throw!(Generic => format!("unable to find macro: {}", &macro_name); ctx.current_span()),
    )?;
    let expr = crate::parser::ast::TryFromSteelValVisitorForExprKind::root(&args[1])?;

    let list = expr.list().unwrap();

    let (case, index) = macro_object.match_case_index(list)?;

    let (bindings, binding_kind) = case.gather_bindings(list.clone())?;

    let map = bindings
        .into_iter()
        .map(|(k, v)| {
            (
                SteelVal::SymbolV(k.resolve().trim_start_matches("##").into()),
                crate::parser::tryfrom_visitor::SyntaxObjectFromExprKind::try_from_expr_kind(v)
                    .unwrap(),
            )
        })
        .collect::<crate::values::HashMap<_, _>>();

    let kind = binding_kind
        .into_iter()
        .map(|(k, v)| {
            (
                SteelVal::SymbolV(k.resolve().trim_start_matches("##").into()),
                match v {
                    crate::parser::expander::BindingKind::Single => 0.into_steelval().unwrap(),
                    crate::parser::expander::BindingKind::Many => 1.into_steelval().unwrap(),
                },
            )
        })
        .collect::<crate::values::HashMap<_, _>>();

    Ok(crate::list![
        // The index of the case that matched
        index.into_steelval()?,
        SteelVal::HashMapV(Gc::new(map).into()),
        SteelVal::HashMapV(Gc::new(kind).into())
    ])
}

fn macro_case_bindings_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let macro_name = String::from_steelval(&args[0])?.into();
    let guard = ctx.thread.compiler.read();
    let macro_object = guard.macro_env.get(&macro_name).unwrap();

    Ok(SteelVal::ListV(
        macro_object
            .cases
            .iter()
            .map(|x| {
                SteelVal::ListV(
                    x.all_bindings()
                        .into_iter()
                        .map(|x| SteelVal::SymbolV(x.trim_start_matches("##").into()))
                        .collect(),
                )
            })
            .collect(),
    ))
}

#[steel_derive::context(name = "#%macro-case-bindings", arity = "Exact(1)")]
pub(crate) fn macro_case_bindings(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(macro_case_bindings_impl(ctx, args))
}

#[steel_derive::context(name = "#%match-syntax-case", arity = "Exact(2)")]
pub(crate) fn match_syntax_case(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(match_syntax_case_impl(ctx, args))
}

pub struct CallStackCapture {
    timer: HashMap<u32, usize>,

    // Map the name of the function to the name, if we have it
    names: HashMap<u32, InternedString>,
}

#[steel_derive::function(name = "dump-profiler")]
pub fn dump_profiler(value: &SteelVal) -> Result<SteelVal> {
    let capture = CallStackCapture::as_ref(value)?;

    let mut values = Vec::new();
    let mut total_count = 0;

    for (id, count) in &capture.timer {
        let name = capture.names.get(id);
        total_count += count;

        if let Some(name) = name {
            values.push((name.resolve().to_string(), count));
            // println!("{} : {}", name, count);
        } else {
            values.push((id.to_string(), count));
            // println!("{} : {}", id, count);
        }
    }

    values.sort_by(|a, b| b.1.cmp(a.1));

    for (key, count) in values {
        println!("{} | {}", key, (*count as f64 / total_count as f64) * 100.0);
    }

    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "make-callstack-profiler")]
pub fn make_callstack_profiler() -> Result<SteelVal> {
    CallStackCapture {
        timer: HashMap::default(),
        names: HashMap::default(),
    }
    .into_steelval()
}

#[steel_derive::context(name = "callstack-hydrate-names", arity = "Exact(1)")]
pub fn callstack_hydrate_names(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let mut capture = CallStackCapture::as_mut_ref(&args[0]).unwrap();

    // Map the name back to the value
    for (idx, global) in ctx.thread.global_env.roots().iter().enumerate() {
        // Find the name for this function, if possible?
        if let SteelVal::Closure(c) = global {
            if let Some(name) = ctx.thread.compiler.read().symbol_map.values().get(idx) {
                capture.names.insert(c.id, *name);
            }
        }
    }

    Some(Ok(SteelVal::Void))
}

impl crate::rvals::Custom for CallStackCapture {}

#[steel_derive::context(name = "#%snapshot-stacks", arity = "Exact(1)")]
pub(crate) fn sample_stacks(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let mut capture = CallStackCapture::as_mut_ref(&args[0]).unwrap();

    // if let Ok(_) = ctx.thread.heap.try_lock() {
    log::info!("Stopping threads...");
    ctx.thread.synchronizer.stop_threads();
    log::info!("Stopped threads.");
    unsafe {
        ctx.thread.synchronizer.maybe_call_per_ctx(
            |thread| {
                // Can we snag the values without locking...
                for frame in thread.stack_frames.iter() {
                    capture
                        .timer
                        .entry(frame.function.id)
                        .and_modify(|x| *x += 1)
                        .or_default();
                }
            },
            100,
        );
    }

    // Resume.
    // Apply these to all of the things.
    ctx.thread.synchronizer.resume_threads();
    log::info!("Threads resumed.");
    // }
    // out

    Some(Ok(SteelVal::Void))
}

/// Applies the given `function` with arguments as the contents of the `list`.
///
/// (apply function lst) -> any?
///
/// * function : function?
/// * list: list?
///
/// # Examples
/// ```scheme
/// > (apply + (list 1 2 3 4)) ;; => 10
/// > (apply list (list 1 2 3 4)) ;; => '(1 2 3 4)
///```
#[steel_derive::context(name = "apply", arity = "Exact(2)")]
pub(crate) fn apply(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let current_instruction = ctx.instructions[ctx.ip - 1];

    let tail_call = matches!(
        current_instruction.op_code,
        OpCode::TAILCALL | OpCode::CALLGLOBALTAIL | OpCode::CALLGLOBALTAILNOARITY
    );

    let mut arg_iter = args.iter();
    let arg1 = arg_iter.next().unwrap();
    let arg2 = arg_iter.next().unwrap();

    if let SteelVal::ListV(l) = arg2 {
        if arg1.is_function() {
            match arg1 {
                SteelVal::Closure(closure) => {
                    for arg in l {
                        ctx.thread.stack.push(arg.clone());
                    }

                    let res = if tail_call {
                        ctx.new_handle_tail_call_closure(closure.clone(), l.len())
                    } else {
                        ctx.ip -= 1;
                        ctx.handle_function_call_closure(closure.clone(), l.len())
                    };

                    if res.is_err() {
                        // This is explicitly unreachable, since we're checking
                        // that this is an error variant
                        return Some(res.map(|_| unreachable!()));
                    }

                    None
                }
                SteelVal::ContinuationFunction(cc) => {
                    Continuation::set_state_from_continuation(ctx, cc.clone());

                    ctx.ip += 1;

                    None
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
                    let args = l.into_iter().cloned().collect::<Vec<_>>();

                    let result =
                        f.func()(&args).map_err(|e| e.set_span_if_none(ctx.current_span()));

                    Some(result)
                }

                // Calling a builtin here might involve a little recursion business
                SteelVal::BuiltIn(f) => {
                    // println!("Calling a builtin with apply!");

                    let args = l.into_iter().cloned().collect::<Vec<_>>();

                    // ctx.ip += 1;

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
#[cfg(feature = "dynamic")]
pub struct InstructionPattern {
    pub(crate) block: std::rc::Rc<[(OpCode, usize)]>,
    pub(crate) pattern: BlockPattern,
}

#[cfg(feature = "dynamic")]
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

#[cfg(feature = "dynamic")]
#[derive(Clone)]
pub struct OpCodeOccurenceProfiler {
    occurrences: HashMap<(OpCode, usize), usize>,
    time: HashMap<(OpCode, usize), std::time::Duration>,
    starting_index: Option<usize>,
    ending_index: Option<usize>,
    sample_count: usize,
}

#[cfg(feature = "dynamic")]
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
                            .map(|x| (x.op_code, x.payload_size.to_usize()))
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
                    .map(|x| (x.op_code, x.payload_size.to_usize()))
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
// fn op_code_requires_payload(
//     op_code: OpCode,
// ) -> Option<for<'r> fn(&'r mut VmCore<'_>, usize) -> Result<()>> {
//     match op_code {
//         OpCode::VOID => None,
//         OpCode::PUSH => Some(push_handler_with_payload),
//         OpCode::IF => todo!(),
//         OpCode::JMP => todo!(),
//         OpCode::FUNC => Some(func_handler_with_payload),
//         OpCode::SCLOSURE => todo!(),
//         OpCode::ECLOSURE => todo!(),
//         OpCode::BIND => Some(bind_handler_with_payload),
//         OpCode::SDEF => todo!(),
//         OpCode::EDEF => todo!(),
//         OpCode::POPPURE => todo!(),
//         OpCode::PASS => todo!(),
//         OpCode::PUSHCONST => Some(push_const_handler_with_payload),
//         OpCode::NDEFS => todo!(),
//         OpCode::PANIC => None,
//         OpCode::TAILCALL => todo!(),
//         OpCode::SET => Some(set_handler_with_payload),
//         OpCode::READLOCAL => Some(local_handler_with_payload),
//         OpCode::READLOCAL0 => None,
//         OpCode::READLOCAL1 => None,
//         OpCode::READLOCAL2 => None,
//         OpCode::READLOCAL3 => None,
//         OpCode::SETLOCAL => Some(set_local_handler_with_payload),
//         OpCode::COPYCAPTURESTACK => todo!(),
//         OpCode::COPYCAPTURECLOSURE => todo!(),
//         OpCode::COPYHEAPCAPTURECLOSURE => todo!(),
//         OpCode::FIRSTCOPYHEAPCAPTURECLOSURE => todo!(),
//         OpCode::TCOJMP => todo!(),
//         OpCode::CALLGLOBAL => Some(call_global_handler_with_payload),
//         OpCode::CALLGLOBALTAIL => todo!(),
//         OpCode::LOADINT0 => None,
//         OpCode::LOADINT1 => None,
//         OpCode::LOADINT2 => None,
//         OpCode::CGLOCALCONST => todo!(),
//         OpCode::MOVEREADLOCAL => Some(move_local_handler_with_payload),
//         OpCode::MOVEREADLOCAL0 => None,
//         OpCode::MOVEREADLOCAL1 => None,
//         OpCode::MOVEREADLOCAL2 => None,
//         OpCode::MOVEREADLOCAL3 => None,
//         OpCode::READCAPTURED => Some(read_captured_handler_with_payload),
//         OpCode::BEGINSCOPE => None,
//         OpCode::LETENDSCOPE => Some(let_end_scope_handler_with_payload),
//         OpCode::PUREFUNC => Some(pure_function_handler_with_payload),
//         OpCode::ADD => Some(add_handler_payload),
//         OpCode::SUB => Some(sub_handler_payload),
//         OpCode::MUL => Some(multiply_handler_payload),
//         OpCode::DIV => Some(division_handler_payload),
//         OpCode::EQUAL => Some(equality_handler_payload),
//         OpCode::LTE => Some(lte_handler_payload),
//         OpCode::NEWSCLOSURE => Some(new_sclosure_handler_with_payload),
//         OpCode::ADDREGISTER => todo!(),
//         OpCode::SUBREGISTER => todo!(),
//         OpCode::LTEREGISTER => todo!(),
//         OpCode::SUBREGISTER1 => todo!(),
//         OpCode::ALLOC => None,
//         OpCode::READALLOC => Some(read_alloc_handler_with_payload),
//         OpCode::SETALLOC => Some(set_alloc_handler_with_payload),
//         // OpCode::GIMMICK => todo!(),
//         // OpCode::MOVEREADLOCALCALLGLOBAL => Some(move_read_local_call_global_handler_payload),
//         OpCode::DynSuperInstruction => todo!(),
//         _ => None,
//     }
// }

// Table to map opcode discriminant directly to an individual handler function
// Why do we want this? When generating dynamic super instructions, we create
// basic blocks, and from there transfer contexts away from the core vm loop, and instead
// over to a basic block sequence of handler function, which we will call directly
// on the main VM context. In order to construct these sequences, we will need to be able
// to grab a basic block from the running sequence, and directly patch an instruction set
// on the fly, to transfer context over to that sequence.
// static OP_CODE_TABLE: [for<'r> fn(&'r mut VmCore<'_>) -> Result<()>; 66] = [
//     void_handler,
//     push_handler,
//     if_handler,   // If
//     jump_handler, // jmp
//     func_handler,
//     dummy, // sclosure
//     dummy, // eclosure
//     bind_handler,
//     dummy, // sdef
//     dummy, // edef
//     dummy, // pop
//     dummy, // popn
//     dummy, // pass
//     push_const_handler,
//     dummy, // ndefs,
//     panic_handler,
//     tail_call_handler, // tailcall
//     set_handler,
//     local_handler,
//     local_handler0,
//     local_handler1,
//     local_handler2,
//     local_handler3,
//     set_local_handler,
//     dummy,            // copycapturestack
//     dummy,            // copycaptureclosure
//     dummy,            // copyheapcaptureclosure
//     dummy,            // firstcopyheapcaptureclosure
//     tco_jump_handler, // tcojmp
//     call_global_handler,
//     call_global_tail_handler, // callglobaltail
//     handle_load_int0,
//     handle_load_int1,
//     handle_load_int2,
//     dummy, // cglocalconst
//     move_local_handler,
//     move_local_handler0,
//     move_local_handler1,
//     move_local_handler2,
//     move_local_handler3,
//     read_captured_handler,
//     begin_scope_handler,
//     let_end_scope_handler,
//     pure_function_handler,
//     add_handler,
//     sub_handler,
//     multiply_handler,
//     division_handler,
//     equality_handler,
//     lte_handler,
//     new_sclosure_handler,
//     dummy, // addregister
//     dummy, // subregister
//     dummy, // lteregister
//     dummy, // subregister
//     alloc_handler,
//     read_alloc_handler,
//     set_alloc_handler,
//     // dummy,                               // gimmick
//     // move_read_local_call_global_handler, // movereadlocalcallglobal,
//     dummy, // dynsuperinstruction,
//     dummy,
//     dummy,
//     dummy,
//     dummy,
//     dummy,
//     binop_add_handler,
//     dummy,
// ];

// macro_rules! opcode_to_function {
//     (VOID) => {
//         void_handler
//     };
//     (PUSH) => {
//         push_handler
//     };
//     (FUNC) => {
//         func_handler
//     };
//     (BIND) => {
//         bind_handler
//     };
//     (PUSHCONST) => {
//         push_const_handler
//     };
//     (PANIC) => {
//         panic_handler
//     };
//     (SET) => {
//         set_handler
//     };
//     (READLOCAL0) => {
//         local_handler0
//     };
//     (LOADINT2) => {
//         handle_load_int2
//     };
//     (LTE) => {
//         lte_handler
//     };
//     (MOVEREADLOCAL0) => {
//         move_local_handler0
//     };
//     (SUB) => {
//         sub_handler
//     };
//     (LOADINT1) => {
//         handle_load_int1
//     };
//     (MUL) => {
//         multiply_handler
//     };
//     (MOVEREADLOCAL1) => {
//         move_local_handler1
//     };
//     (READLOCAL1) => {
//         local_handler1
//     };
//     (READLOCAL2) => {
//         local_handler2
//     };
//     (READLOCAL3) => {
//         local_handler3
//     };
//     (LOADINT0) => {
//         handle_load_int0
//     };
//     (CALLGLOBAL) => {
//         call_global_handler
//     };
//     (READCAPTURED) => {
//         read_captured_handler
//     };
//     (IF) => {
//         if_handler
//     };
//     (EQUAL) => {
//         equality_handler
//     };
//     (JMP) => {
//         jump_handler
//     };
//     (ADD) => {
//         add_handler
//     };
//     (TAILCALL) => {
//         tail_call_handler
//     };
// }

// static SUPER_PATTERNS: Lazy<
//     std::collections::HashMap<Vec<OpCode>, for<'r> fn(&'r mut VmCore<'_>) -> Result<()>>,
// > = Lazy::new(|| create_super_instruction_map());

// lazy_static! {
//     static ref SUPER_PATTERNS: std::collections::HashMap<
//         Vec<(OpCode, Option<usize>)>,
//         for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
//     > = create_super_instruction_map();
// }

// fn create_super_instruction_map(
// ) -> std::collections::HashMap<Vec<OpCode>, for<'r> fn(&'r mut VmCore<'_>) -> Result<()>> {
//     use OpCode::*;

//     let mut map = HashMap::new();

//     macro_rules! create_super_pattern {
//         ($($args:tt),*) => {

//             // fn func(ctx: &mut VmCore<'_>) -> Result<()> {
//             //     $(
//             //         OP_CODE_TABLE[$args.to_usize()](ctx)?;
//             //     )*

//             //     Ok(())
//             // }

//             // TODO: This isn't actually doing the correct mapping. Set up a const mapping instead using macros
//             map.insert(vec![
//                 $($args,)*
//             ], |ctx: &mut VmCore<'_>| -> Result<()> {
//                 $(
//                     opcode_to_function!($args)(ctx)?;
//                 )*

//                 Ok(())
//             } as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>);
//         };
//     }

//     // Fib patterns identified from the benchmarks
//     // yes, this is explicitly gaming the benchmarks. But the idea is sound,
//     // and this is just a start.
//     // create_super_pattern!(READLOCAL0, LOADINT2, LTE, IF);

//     map.insert(
//         vec![READLOCAL0, LOADINT2, LTE, IF],
//         specialized_lte0 as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
//     );

//     map.insert(
//         vec![MOVEREADLOCAL0, LOADINT2, SUB, CALLGLOBAL],
//         specialized_sub02 as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
//     );

//     map.insert(
//         vec![READLOCAL0, LOADINT1, SUB, CALLGLOBAL],
//         specialized_sub01 as for<'r> fn(&'r mut VmCore<'_>) -> Result<()>,
//     );

//     // create_super_pattern!(MOVEREADLOCAL0, LOADINT2, SUB, CALLGLOBAL);
//     // create_super_pattern!(READLOCAL0, LOADINT1, SUB, CALLGLOBAL);

//     // bin trees patterns
//     create_super_pattern!(
//         READLOCAL0,
//         LOADINT2,
//         MUL,
//         MOVEREADLOCAL1,
//         LOADINT1,
//         SUB,
//         READLOCAL2,
//         LOADINT1,
//         SUB,
//         READLOCAL3,
//         CALLGLOBAL
//     );

//     create_super_pattern!(READLOCAL1, LOADINT0, CALLGLOBAL, IF);
//     create_super_pattern!(MOVEREADLOCAL0, LOADINT0, READCAPTURED, TAILCALL);
//     create_super_pattern!(MOVEREADLOCAL0, LOADINT2, READCAPTURED, TAILCALL);

//     // Ack patterns
//     create_super_pattern!(READLOCAL0, LOADINT0, EQUAL, IF);

//     create_super_pattern!(READLOCAL1, LOADINT0, EQUAL, IF);

//     create_super_pattern!(
//         READLOCAL0,
//         LOADINT1,
//         SUB,
//         MOVEREADLOCAL0,
//         MOVEREADLOCAL1,
//         LOADINT1,
//         SUB,
//         CALLGLOBAL
//     );

//     create_super_pattern!(READLOCAL1, LOADINT1, ADD, JMP);

//     map
// }

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

#[cfg(feature = "dynamic")]
// OpCode::SETLOCAL
fn set_local_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let offset = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_set_local(offset.to_usize());
    Ok(())
}

#[cfg(feature = "dynamic")]
// OpCode::SETLOCAL
fn set_local_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.handle_set_local(payload);
    Ok(())
}

#[cfg(feature = "dynamic")]
// OpCode::CALLGLOBAL
fn call_global_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    // assert!(ctx.ip + 1 < ctx.instructions.len());
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.ip += 1;
    let next_inst = ctx.instructions[ctx.ip];

    ctx.handle_call_global(payload_size.to_usize(), next_inst.payload_size.to_usize())
}

#[cfg(feature = "dynamic")]
// OpCode::CALLGLOBAL
// TODO: Fix this!
fn call_global_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    ctx.ip += 1;
    let next_inst = ctx.instructions[ctx.ip];
    ctx.handle_call_global(payload, next_inst.payload_size.to_usize())
}

// // TODO: Have a way to know the correct arity?
// fn call_global_handler_no_stack(
//     ctx: &mut VmCore<'_>,
//     args: &mut [SteelVal],
// ) -> Result<Option<SteelVal>> {
//     // ctx.ip += 1;
//     let payload_size = ctx.instructions[ctx.ip].payload_size;
//     ctx.ip += 1;
//     // TODO: Track the op codes of the surrounding values as well
//     // let next_inst = ctx.instructions[ctx.ip];
//     // println!("Looking up a function at index: {}", payload_size.to_usize());
//     let func = ctx
//         .thread
//         .global_env
//         .repl_lookup_idx(payload_size.to_usize());
//     ctx.handle_non_instr_global_function_call(func, args)
// }

#[cfg(feature = "dynamic")]
fn num_equal_handler_no_stack(_ctx: &mut VmCore<'_>, l: SteelVal, r: SteelVal) -> Result<bool> {
    if let SteelVal::BoolV(b) = number_equality(&l, &r)? {
        Ok(b)
    } else {
        unreachable!()
    }
}

#[cfg(feature = "dynamic")]
// OpCode::LOADINT0
fn handle_load_int0(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::INT_ZERO);
    ctx.ip += 1;
    Ok(())
}

#[cfg(feature = "dynamic")]
// OpCode::LOADINT1
fn handle_load_int1(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::INT_ONE);
    ctx.ip += 1;
    Ok(())
}

#[cfg(feature = "dynamic")]
// OpCode::LOADINT2
fn handle_load_int2(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.thread.stack.push(SteelVal::INT_TWO);
    ctx.ip += 1;
    Ok(())
}

#[cfg(feature = "dynamic")]
// OpCode::MOVEREADLOCAL
fn move_local_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let index = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_move_local(index.to_usize())
}

#[cfg(feature = "dynamic")]
// OpCode::MOVEREADLOCAL
fn move_local_handler_with_payload(ctx: &mut VmCore<'_>, index: usize) -> Result<()> {
    ctx.handle_move_local(index)
}

#[cfg(feature = "dynamic")]
// OpCode::MOVEREADLOCAL0
fn move_local_handler0(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(0)
}

#[cfg(feature = "dynamic")]
// OpCode::MOVEREADLOCAL1
fn move_local_handler1(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(1)
}

#[cfg(feature = "dynamic")]
// OpCode::MOVEREADLOCAL2
fn move_local_handler2(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(2)
}

#[cfg(feature = "dynamic")]
// OpCode::MOVEREADLOCAL3
fn move_local_handler3(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.handle_move_local(3)
}

#[cfg(feature = "dynamic")]
// OpCode::READCAPTURED
fn read_captured_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size;
    ctx.handle_read_captures(payload_size.to_usize())
}

#[cfg(feature = "dynamic")]
// OpCode::READCAPTURED
fn read_captured_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    ctx.handle_read_captures(payload_size)
}

#[cfg(feature = "dynamic")]
// OpCode::BEGINSCOPE
fn begin_scope_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    ctx.ip += 1;
    Ok(())
}

// OpCode::LETENDSCOPE
fn let_end_scope_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let beginning_scope = ctx.instructions[ctx.ip].payload_size.to_usize();
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

#[cfg(feature = "dynamic")]
// OpCode::PUREFUNC
fn pure_function_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let payload_size = ctx.instructions[ctx.ip].payload_size.to_usize();
    ctx.handle_pure_function(payload_size);
    Ok(())
}

#[cfg(feature = "dynamic")]
// OpCode::PUREFUNC
fn pure_function_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
    ctx.handle_pure_function(payload_size);
    Ok(())
}

macro_rules! handler_inline_primitive {
    ($ctx:expr, $name:tt) => {{
        let payload_size = $ctx.instructions[$ctx.ip].payload_size.to_usize();
        let last_index = $ctx.thread.stack.len() - payload_size;

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
        let last_index = $ctx.thread.stack.len() - $payload_size;

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

macro_rules! handler_inline_primitive_payload_1 {
    ($ctx:expr, $name:tt) => {{
        let last_index = $ctx.thread.stack.len() - 1;

        let result = match $name(&mut $ctx.thread.stack[last_index..]) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
        };

        *$ctx.thread.stack.last_mut().unwrap() = result;

        $ctx.ip += 2;
    }};
}

macro_rules! handler_inline_primitive_payload_1_single {
    ($ctx:expr, $name:tt) => {{
        let last = $ctx.thread.stack.last_mut().unwrap();

        let result = match $name(last) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
        };

        *last = result;

        $ctx.ip += 2;
    }};
}

macro_rules! handler_inline_primitive_payload_2 {
    ($ctx:expr, $name:tt) => {{
        let mut last = $ctx.thread.stack.pop().unwrap();
        let second_last = $ctx.thread.stack.last_mut().unwrap();

        let result = match $name(second_last, &mut last) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none($ctx.current_span())),
        };

        *second_last = result;

        $ctx.ip += 2;
    }};
}

// OpCode::ADD
fn cons_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload!(ctx, steel_cons, 2);
    Ok(())
}

fn new_box_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    let last = ctx.thread.stack.pop().unwrap();

    let allocated_var = ctx.thread.heap.lock().unwrap().allocate(
        last,
        &ctx.thread.stack,
        ctx.thread.stack_frames.iter().map(|x| x.function.as_ref()),
        ctx.thread.global_env.roots(),
        &ctx.thread.thread_local_storage,
        &mut ctx.thread.synchronizer,
    );

    let result = SteelVal::HeapAllocated(allocated_var);

    ctx.thread.stack.push(result);

    ctx.ip += 2;
    Ok(())
}

#[allow(unused)]
fn unbox_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload!(ctx, steel_unbox_mutable, 1);
    Ok(())
}

fn setbox_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload!(ctx, steel_set_box_mutable, 2);
    Ok(())
}

fn car_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload_1_single!(ctx, car);
    Ok(())
}

fn not_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload_1!(ctx, steel_not);
    Ok(())
}

fn cdr_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload_1_single!(ctx, cdr);
    Ok(())
}

fn number_equality_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload_2!(ctx, number_equality);
    Ok(())
}

fn listref_handler(ctx: &mut VmCore<'_>) -> Result<()> {
    handler_inline_primitive_payload!(ctx, steel_list_ref, 2);
    Ok(())
}

fn list_handler(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    // handler_inline_primitive_payload!(ctx, new_list, payload);
    let last_index = ctx.thread.stack.len() - payload;
    let remaining = ctx.thread.stack.split_off(last_index);
    let list = SteelVal::ListV(remaining.into());
    ctx.thread.stack.push(list);

    ctx.ip += 2;

    Ok(())
}

// OpCode::ADD
fn add_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, add_primitive, payload);
    Ok(())
}

// // OpCode::SUB
// #[inline(always)]
// fn sub_handler(ctx: &mut VmCore<'_>) -> Result<()> {
//     handler_inline_primitive!(ctx, subtract_primitive);
//     Ok(())
// }

// OpCode::SUB
#[inline(always)]
fn sub_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, subtract_primitive, payload);
    Ok(())
}

// OpCode::LTE
fn lte_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, lte_primitive, payload);
    Ok(())
}

// OpCode::ALLOC
fn alloc_handler(_ctx: &mut VmCore<'_>) -> Result<()> {
    panic!("Deprecated now - this shouldn't be hit");

    /*

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

    */
}

// OpCode::READALLOC
#[inline(always)]
fn read_alloc_handler(_ctx: &mut VmCore<'_>) -> Result<()> {
    panic!("Deprecated - this shouldn't be hit")

    /*
    let payload_size = ctx.instructions[ctx.ip].payload_size.to_usize();

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
    */
}

// OpCode::SETALLOC
#[inline(always)]
fn set_alloc_handler(_ctx: &mut VmCore<'_>) -> Result<()> {
    panic!("Deprecated - this shouldn't be hit")

    /*
    let payload_size = ctx.instructions[ctx.ip].payload_size.to_usize();
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
    */
}

pub(super) fn lt_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, lt_primitive, payload);
    Ok(())
}

pub(super) fn gt_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, gt_primitive, payload);
    Ok(())
}

pub(super) fn gte_handler_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
    handler_inline_primitive_payload!(ctx, gte_primitive, payload);
    Ok(())
}

#[allow(unused)]
mod handlers {

    use super::*;

    fn add_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        handler_inline_primitive!(ctx, add_primitive);
        Ok(())
    }

    fn binop_add_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let last_index = ctx.thread.stack.len() - 2;

        let right = ctx.thread.stack.pop().unwrap();
        let left = ctx.thread.stack.last().unwrap();

        let result = match handlers::add_handler_none_none(left, &right) {
            Ok(value) => value,
            Err(e) => return Err(e.set_span_if_none(ctx.current_span())),
        };

        *ctx.thread.stack.last_mut().unwrap() = result;

        ctx.ip += 2;

        Ok(())
    }

    // OpCode::READALLOC
    #[inline(always)]
    fn read_alloc_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
        panic!("Deprecated - this shouldn't be hit")

        /*
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
        */
    }

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
            ctx.ip = payload_size.to_usize();
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

    #[cfg(feature = "dynamic")]
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
    pub fn handle_local_0_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
        let offset = ctx.get_offset();
        let value = ctx.thread.stack[offset].clone();
        ctx.ip += 1;
        Ok(value)
    }

    #[inline(always)]
    pub fn handle_local_1_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
        let offset = ctx.get_offset();
        let value = ctx.thread.stack[offset + 1].clone();
        ctx.ip += 1;
        Ok(value)
    }

    #[inline(always)]
    pub fn handle_local_2_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
        let offset = ctx.get_offset();
        let value = ctx.thread.stack[offset + 2].clone();
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
        ctx.handle_push(index.to_usize())
    }

    // OpCode::PUSH
    fn push_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
        ctx.handle_push(payload)
    }

    // OpCode::FUNC
    fn func_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let func = ctx.thread.stack.pop().unwrap();
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        ctx.handle_function_call(func, payload_size.to_usize())
    }

    // OpCode::FUNC
    fn func_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
        let func = ctx.thread.stack.pop().unwrap();
        ctx.handle_function_call(func, payload)
    }

    // OpCode::BIND
    fn bind_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let index = ctx.instructions[ctx.ip].payload_size;
        ctx.handle_bind(index.to_usize());
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
        let val = ctx.constants.get_value(payload_size.to_usize());
        ctx.thread.stack.push(val);
        ctx.ip += 1;
        Ok(())
    }

    pub fn push_const_handler_no_stack(ctx: &mut VmCore<'_>) -> Result<SteelVal> {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        let val = ctx.constants.get_value(payload_size.to_usize());
        ctx.ip += 1;
        Ok(val)
    }

    // OpCode::PUSHCONST
    fn push_const_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
        let val = ctx.constants.get_value(payload);
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
        ctx.handle_set(index.to_usize())
    }

    // OpCode::SET
    fn set_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
        ctx.handle_set(payload)
    }

    // OpCode::READLOCAL
    #[inline(always)]
    fn local_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let index = ctx.instructions[ctx.ip].payload_size;
        ctx.handle_local(index.to_usize())
    }

    // OpCode::READLOCAL
    fn local_handler_with_payload(ctx: &mut VmCore<'_>, payload: usize) -> Result<()> {
        ctx.handle_local(payload)
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

    // OpCode::SETALLOC
    #[inline(always)]
    fn set_alloc_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
        panic!("Deprecated - this shouldn't be hit")
        /*
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

        */
    }

    // OpCode::NEWSCLOSURE
    fn new_sclosure_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let payload_size = ctx.instructions[ctx.ip].payload_size.to_usize();

        ctx.handle_new_start_closure(payload_size)
    }

    // OpCode::NEWSCLOSURE
    fn new_sclosure_handler_with_payload(ctx: &mut VmCore<'_>, payload_size: usize) -> Result<()> {
        ctx.handle_new_start_closure(payload_size)
    }

    #[inline(always)]
    fn jump_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        ctx.ip = payload_size.to_usize();
        Ok(())
    }

    #[inline(always)]
    fn if_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        // change to truthy...
        if ctx.thread.stack.pop().unwrap().is_truthy() {
            ctx.ip += 1;
        } else {
            ctx.ip = payload_size.to_usize();
        }
        Ok(())
    }

    #[inline(always)]
    pub fn raw_if_handler(ctx: &mut VmCore<'_>, arg: SteelVal) {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        // change to truthy...
        if arg.is_truthy() {
            ctx.ip += 1;
        } else {
            ctx.ip = payload_size.to_usize();
        }
    }

    #[inline(always)]
    pub fn if_handler_with_bool(ctx: &mut VmCore<'_>, condition: bool) {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        // change to truthy...
        if condition {
            ctx.ip += 1;
        } else {
            ctx.ip = payload_size.to_usize();
        }
    }

    #[inline(always)]
    fn call_global_tail_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        let next_inst = ctx.instructions[ctx.ip + 1];

        ctx.handle_tail_call_global(payload_size.to_usize(), next_inst.payload_size.to_usize())
    }

    #[inline(always)]
    fn tail_call_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        let payload_size = ctx.instructions[ctx.ip].payload_size;
        let func = ctx.thread.stack.pop().unwrap();
        ctx.handle_tail_call(func, payload_size.to_usize())
    }

    #[inline(always)]
    fn tco_jump_handler(ctx: &mut VmCore<'_>) -> Result<()> {
        // TODO: Handle multiple arity for TCO

        let payload_size = ctx.instructions[ctx.ip].payload_size;

        let current_arity = payload_size.to_usize();
        let last_stack_frame = ctx.thread.stack_frames.last().unwrap();

        ctx.instructions = last_stack_frame.function.body_exp();
        ctx.sp = last_stack_frame.sp;

        ctx.ip = 0;

        let closure_arity = last_stack_frame.function.arity();

        if current_arity != closure_arity {
            stop!(ArityMismatch => format!("tco: function expected {closure_arity} arguments, found {current_arity}"); ctx.current_span());
        }
        let offset = ctx.sp;

        let back = ctx.thread.stack.len() - current_arity;
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
                    let mut res = BigInt::from(l);
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
    pub(crate) fn add_handler_none_none(l: &SteelVal, r: &SteelVal) -> Result<SteelVal> {
        add_two(l, r)
    }
}

#[cfg(feature = "dynamic")]
pub(crate) use dynamic::pattern_exists;

#[macro_use]
#[cfg(feature = "dynamic")]
mod dynamic {
    use super::*;

    use handlers::{
        handle_local_0_no_stack, handle_local_1_no_stack, handle_local_2_no_stack,
        if_handler_with_bool, push_const_handler_no_stack, raw_if_handler,
    };

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

        (NUMEQUAL) => {
            num_equal_handler_no_stack
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

        (READLOCAL2) => {
            handle_local_2_no_stack
        };
    }

    // Includes the module as a dependency, that being said - this should
    // probably get generated into some specific sub module directly?
    include!(concat!(env!("OUT_DIR"), "/dynamic.rs"));
}
