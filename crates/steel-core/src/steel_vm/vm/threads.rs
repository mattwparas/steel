use std::collections::HashSet;

use fxhash::FxHashMap;
use parking_lot::RwLock;
use steel_derive::function;

use crate::{
    rvals::{
        AsRefMutSteelVal, AsRefSteelVal as _, Custom, HeapSerializer, SerializableSteelVal,
        SerializedHeapRef,
    },
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    values::{functions::SerializedLambdaPrototype, structs::VTable},
};

use super::*;

// TODO: Do proper logging here for thread spawning
macro_rules! time {
    ($label:expr, $e:expr) => {{
        #[cfg(feature = "profiling")]
        let now = std::time::Instant::now();

        let e = $e;

        #[cfg(feature = "profiling")]
        log::debug!(target: "threads", "{}: {:?}", $label, now.elapsed());

        e
    }};
}

pub struct ThreadHandle {
    // If this can hold a native steelerr object that would be nice
    pub(crate) handle: Option<std::thread::JoinHandle<std::result::Result<(), String>>>,
    pub(crate) thread_state_manager: ThreadStateController,
}

/// Check if the given thread is finished running.
#[steel_derive::function(name = "thread-finished?")]
pub fn thread_finished(handle: &SteelVal) -> Result<SteelVal> {
    Ok(ThreadHandle::as_ref(handle)?
        .handle
        .as_ref()
        .map(|x| x.is_finished())
        .unwrap_or(true))
    .map(SteelVal::BoolV)
}

impl crate::rvals::Custom for ThreadHandle {}

pub struct SteelMutex {
    mutex: Arc<parking_lot::Mutex<()>>,
    guard: AtomicCell<Option<parking_lot::ArcMutexGuard<parking_lot::RawMutex, ()>>>,
}

impl crate::rvals::Custom for SteelMutex {}

impl SteelMutex {
    pub fn new() -> Self {
        Self {
            mutex: Arc::new(parking_lot::Mutex::new(())),
            guard: AtomicCell::new(None),
        }
    }

    // Attempt to lock it before killing the other one
    pub fn lock(&self) {
        // Acquire the lock first
        let guard = self.mutex.lock_arc();
        self.guard.store(Some(guard));
    }

    pub fn unlock(&self) {
        self.guard.store(None);
    }
}

/// Construct a new mutex
#[steel_derive::function(name = "mutex")]
pub fn new_mutex() -> Result<SteelVal> {
    SteelMutex::new().into_steelval()
}

/// Lock the given mutex. Note, this is most likely used as a building block
/// with the `lock!` function.
#[steel_derive::function(name = "lock-acquire!")]
pub fn mutex_lock(mutex: &SteelVal) -> Result<SteelVal> {
    SteelMutex::as_ref(mutex)?.lock();
    Ok(SteelVal::Void)
}

/// Unlock the given mutex.
#[steel_derive::function(name = "lock-release!")]
pub fn mutex_unlock(mutex: &SteelVal) -> Result<SteelVal> {
    SteelMutex::as_ref(mutex)?.unlock();
    Ok(SteelVal::Void)
}

/// Block until this thread finishes.
#[steel_derive::function(name = "thread-join!")]
pub fn thread_join(handle: &SteelVal) -> Result<SteelVal> {
    ThreadHandle::as_mut_ref(handle)
        .and_then(|mut x| thread_join_impl(&mut x))
        .map(|_| SteelVal::Void)
}

pub(crate) fn thread_join_impl(handle: &mut ThreadHandle) -> Result<()> {
    if let Some(handle) = handle.handle.take() {
        handle
            .join()
            .map_err(|_| SteelErr::new(ErrorKind::Generic, "thread panicked!".to_string()))?
            .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
    } else {
        stop!(ContractViolation => "thread handle has already been joined!");
    }
}

/// Suspend the thread. Note, this will _not_ interrupt any native code that is
/// potentially running in the thread, and will attempt to block at the next
/// bytecode instruction that is running.
#[steel_derive::function(name = "thread-suspend")]
pub(crate) fn thread_suspend(handle: &SteelVal) -> Result<SteelVal> {
    ThreadHandle::as_mut_ref(handle)?
        .thread_state_manager
        .suspend();

    Ok(SteelVal::Void)
}

/// Resume a suspended thread. This does nothing if the thread is already joined.
#[steel_derive::function(name = "thread-resume")]
pub(crate) fn thread_resume(handle: &SteelVal) -> Result<SteelVal> {
    let mut handle = ThreadHandle::as_mut_ref(handle)?;
    handle.thread_state_manager.resume();
    if let Some(handle) = handle.handle.as_mut() {
        handle.thread().unpark();
    }
    Ok(SteelVal::Void)
}

/// Interrupts the thread. Note, this will _not_ interrupt any native code
/// that is potentially running in the thread, and will attempt to block
/// at the next bytecode instruction that is running.
#[steel_derive::function(name = "thread-interrupt")]
pub(crate) fn thread_interrupt(handle: &SteelVal) -> Result<SteelVal> {
    ThreadHandle::as_mut_ref(handle)?
        .thread_state_manager
        .interrupt();
    Ok(SteelVal::Void)
}

thread_local! {
    static CACHED_CLOSURES: RefCell<FxHashMap<u32, SerializedLambdaPrototype>> = RefCell::new(FxHashMap::default());
}

pub fn closure_into_serializable(
    c: &ByteCodeLambda,
    serializer: &mut std::collections::HashMap<usize, SerializableSteelVal>,
    visited: &mut std::collections::HashSet<usize>,
) -> Result<SerializedLambda> {
    if let Some(prototype) = CACHED_CLOSURES.with(|x| x.borrow().get(&c.id).cloned()) {
        let mut prototype = SerializedLambda {
            id: prototype.id,
            body_exp: prototype.body_exp,
            arity: prototype.arity,
            is_multi_arity: prototype.is_multi_arity,
            captures: Vec::new(),
        };

        prototype.captures = c
            .captures
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, serializer, visited))
            .collect::<Result<_>>()?;

        Ok(prototype)
    } else {
        let prototype = SerializedLambdaPrototype {
            id: c.id,

            #[cfg(not(feature = "dynamic"))]
            body_exp: c.body_exp.into_iter().cloned().collect(),

            #[cfg(feature = "dynamic")]
            body_exp: c.body_exp.borrow().iter().cloned().collect(),

            arity: c.arity as _,
            is_multi_arity: c.is_multi_arity,
        };

        CACHED_CLOSURES.with(|x| x.borrow_mut().insert(c.id, prototype.clone()));

        let mut prototype = SerializedLambda {
            id: prototype.id,
            body_exp: prototype.body_exp,
            arity: prototype.arity,
            is_multi_arity: prototype.is_multi_arity,
            captures: Vec::new(),
        };

        prototype.captures = c
            .captures
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, serializer, visited))
            .collect::<Result<_>>()?;

        Ok(prototype)
    }
}

struct MovableThread {
    constants: Vec<SerializableSteelVal>,
    global_env: Vec<SerializableSteelVal>,
    function_interner: MovableFunctionInterner,
    runtime_options: RunTimeOptions,
}

struct MovableFunctionInterner {
    closure_interner: fxhash::FxHashMap<u32, SerializedLambda>,
    pure_function_interner: fxhash::FxHashMap<u32, SerializedLambda>,
    spans: fxhash::FxHashMap<u32, Vec<Span>>,
    instructions: fxhash::FxHashMap<u32, Vec<DenseInstruction>>,
}

#[allow(unused)]
/// This will naively deep clone the environment, by attempting to translate every value into a `SerializableSteelVal`
/// While this does work, it does result in a fairly hefty deep clone of the environment. It does _not_ smartly attempt
/// to keep track of what values this function could touch - rather it assumes every value is possible to be touched
/// by the child thread.
fn spawn_thread_result(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    use crate::rvals::SerializableSteelVal;

    #[cfg(feature = "profiling")]
    let now = std::time::Instant::now();

    // Need a new:
    // Stack
    // Heap
    // global env - This we can do (hopefully) lazily. Only clone the values that actually
    // get referenced. We can also just straight up reject any closures that cannot be moved
    // across threads

    if args.len() != 1 {
        stop!(ArityMismatch => "spawn-thread! accepts one argument, found: {}", args.len())
    }

    let mut initial_map = HashMap::new();
    let mut visited = HashSet::new();

    // If it is a native function, theres no reason we can't just call it on a new thread, most likely.
    // There might be some funny business with thread local values, but for now we'll just accept it.
    let function: SerializedLambda = match &args[0] {
        SteelVal::FuncV(f) => {
            let func = *f;

            let handle =
                std::thread::spawn(move || func(&[]).map(|_| ()).map_err(|e| e.to_string()));

            return ThreadHandle {
                handle: Some(handle),
                thread_state_manager: ThreadStateController::default(),
            }
            .into_steelval();

            // todo!()
        }
        SteelVal::MutFunc(f) => {
            let func = *f;

            let handle =
                std::thread::spawn(move || func(&mut []).map(|_| ()).map_err(|e| e.to_string()));

            return ThreadHandle {
                handle: Some(handle),
                thread_state_manager: ThreadStateController::default(),
            }
            .into_steelval();
        }

        // Probably rename unwrap to something else
        SteelVal::Closure(f) => closure_into_serializable(&f, &mut initial_map, &mut visited)?,
        illegal => {
            stop!(TypeMismatch => "Cannot spawn value on another thread: {}", illegal);
        }
    };

    let constants = time!("Constant map serialization", {
        let constants = ctx
            .thread
            .constant_map
            .to_serializable_vec(&mut initial_map, &mut visited);

        constants
    });

    let sources = ctx.thread.sources.clone();

    let thread = MovableThread {
        constants,

        // Void in this case, is a poisoned value. We need to trace the closure
        // (and all of its references) - to find any / all globals that _could_ be
        // referenced.
        #[cfg(feature = "sync")]
        global_env: time!(
            "Global env serialization",
            ctx.thread
                .global_env
                .bindings_vec
                .read()
                .unwrap()
                .iter()
                .cloned()
                .map(|x| into_serializable_value(x, &mut initial_map, &mut visited))
                .map(|x| x.unwrap_or(SerializableSteelVal::Void))
                .collect()
        ),

        #[cfg(not(feature = "sync"))]
        global_env: time!(
            "Global env serialization",
            ctx.thread
                .global_env
                .bindings_vec
                .iter()
                .cloned()
                .map(|x| into_serializable_value(x, &mut initial_map, &mut visited))
                .map(|x| x.unwrap_or(SerializableSteelVal::Void))
                .collect()
        ),

        // Populate with the values after moving into the thread, spawn accordingly
        // TODO: Move this out of here
        function_interner: time!(
            "Function interner serialization",
            MovableFunctionInterner {
                closure_interner: ctx
                    .thread
                    .function_interner
                    .closure_interner
                    .iter()
                    .map(|(k, v)| {
                        let v_prime: SerializedLambda =
                            closure_into_serializable(v, &mut initial_map, &mut visited)
                                .expect("This shouldn't fail!");
                        (*k, v_prime)
                    })
                    .collect(),
                pure_function_interner: ctx
                    .thread
                    .function_interner
                    .pure_function_interner
                    .iter()
                    .map(|(k, v)| {
                        let v_prime: SerializedLambda =
                            closure_into_serializable(v, &mut initial_map, &mut visited)
                                .expect("This shouldn't fail!");
                        (*k, v_prime)
                    })
                    .collect(),
                spans: ctx
                    .thread
                    .function_interner
                    .spans
                    .iter()
                    .map(|(k, v)| (*k, v.iter().copied().collect()))
                    .collect(),
                instructions: ctx
                    .thread
                    .function_interner
                    .instructions
                    .iter()
                    .map(|(k, v)| (*k, v.iter().copied().collect()))
                    .collect(),
            }
        ),

        runtime_options: ctx.thread.runtime_options.clone(),
    };

    let sendable_vtable_entries = VTable::sendable_entries(&mut initial_map, &mut visited)?;

    // TODO: Spawn a bunch of threads at the start to handle requests. That way we don't need to do this
    // the whole time they're in there.
    let handle = std::thread::spawn(move || {
        let heap = time!("Heap Creation", Arc::new(Mutex::new(Heap::new())));

        // Move across threads?
        let mut mapping = initial_map
            .into_iter()
            .map(|(key, value)| (key, SerializedHeapRef::Serialized(Some(value))))
            .collect();

        let mut patcher = HashMap::new();
        let mut built_functions = HashMap::new();

        let mut heap_guard = heap.lock().unwrap();

        let mut serializer = HeapSerializer {
            heap: &mut heap_guard,
            fake_heap: &mut mapping,
            values_to_fill_in: &mut patcher,
            built_functions: &mut built_functions,
        };

        // Moved over the thread. We now have
        let closure: ByteCodeLambda = ByteCodeLambda::from_serialized(&mut serializer, function);

        VTable::initialize_new_thread(sendable_vtable_entries, &mut serializer);

        let constant_map = time!(
            "Constant map deserialization",
            ConstantMap::from_vec(
                thread
                    .constants
                    .into_iter()
                    .map(|x| from_serializable_value(&mut serializer, x))
                    .collect(),
            )
        );

        #[cfg(feature = "sync")]
        let global_env = time!(
            "Global env creation",
            Env {
                bindings_vec: Arc::new(std::sync::RwLock::new(
                    thread
                        .global_env
                        .into_iter()
                        .map(|x| from_serializable_value(&mut serializer, x))
                        .collect()
                )),
                // TODO:
                thread_local_bindings: Vec::new(),
            }
        );

        #[cfg(not(feature = "sync"))]
        let global_env = time!(
            "Global env creation",
            Env {
                bindings_vec: thread
                    .global_env
                    .into_iter()
                    .map(|x| from_serializable_value(&mut serializer, x))
                    .collect(),
            }
        );

        let function_interner = time!(
            "Function interner time",
            FunctionInterner {
                closure_interner: thread
                    .function_interner
                    .closure_interner
                    .into_iter()
                    .map(|(k, v)| (k, ByteCodeLambda::from_serialized(&mut serializer, v)))
                    .collect(),
                pure_function_interner: thread
                    .function_interner
                    .pure_function_interner
                    .into_iter()
                    .map(|(k, v)| (
                        k,
                        if let Some(exists) = serializer.built_functions.get(&v.id) {
                            exists.clone()
                        } else {
                            Gc::new(ByteCodeLambda::from_serialized(&mut serializer, v))
                        }
                    ))
                    .collect(),
                spans: thread
                    .function_interner
                    .spans
                    .into_iter()
                    .map(|(k, v)| (k, v.into()))
                    .collect(),
                instructions: thread
                    .function_interner
                    .instructions
                    .into_iter()
                    .map(|(k, v)| (k, v.into()))
                    .collect(),
            }
        );

        // Patch over the values in the final heap!

        time!("Patching over heap values", {
            for (key, value) in serializer.values_to_fill_in {
                if let Some(cycled) = serializer.fake_heap.get(key) {
                    match cycled {
                        SerializedHeapRef::Serialized(_) => todo!(),
                        // Patch over the cycle
                        SerializedHeapRef::Closed(c) => {
                            value.set(c.get());
                        }
                    }
                } else {
                    todo!()
                }
            }
        });

        drop(heap_guard);

        // New thread! It will result in a run time error if the function references globals that cannot be shared
        // between threads. This is a bit of an unfortunate occurrence - we probably _should_ just have the engine share
        // as much as possible between threads.
        let mut thread = SteelThread {
            global_env,
            sources,
            stack: Vec::with_capacity(64),

            #[cfg(feature = "dynamic")]
            profiler: OpCodeOccurenceProfiler::new(),

            function_interner,
            heap,
            runtime_options: thread.runtime_options,
            current_frame: StackFrame::main(),
            stack_frames: Vec::with_capacity(32),
            constant_map,
            interrupted: Default::default(),
            synchronizer: Synchronizer::new(),
            thread_local_storage: Vec::new(),
            // TODO: Fix this
            compiler: todo!(),
            id: EngineId::new(),
            safepoints_enabled: false,
        };

        #[cfg(feature = "profiling")]
        log::info!(target: "threads", "Time taken to spawn thread: {:?}", now.elapsed());

        // Call the function!
        thread
            .call_function(
                thread.constant_map.clone(),
                SteelVal::Closure(Gc::new(closure)),
                Vec::new(),
            )
            .map(|_| ())
            .map_err(|e| e.to_string())
    });

    return ThreadHandle {
        handle: Some(handle),
        thread_state_manager: ThreadStateController::default(),
    }
    .into_steelval();
}

pub struct SteelReceiver {
    receiver: crossbeam::channel::Receiver<SteelVal>,
}

pub struct SteelSender {
    sender: crossbeam::channel::Sender<SteelVal>,
}

pub struct Channels {
    sender: SteelVal,
    receiver: SteelVal,
}

impl Custom for SteelReceiver {}
impl Custom for SteelSender {}
impl Custom for Channels {}

impl Channels {
    pub fn new() -> Self {
        let (sender, receiver) = crossbeam::channel::unbounded();

        Self {
            sender: SteelSender { sender }.into_steelval().unwrap(),
            receiver: SteelReceiver { receiver }.into_steelval().unwrap(),
        }
    }

    pub fn sender(&self) -> SteelVal {
        self.sender.clone()
    }

    pub fn receiver(&self) -> SteelVal {
        self.receiver.clone()
    }
}

/// Blocks until one of the channels passed in is ready to receive.
/// Returns the index of the channel arguments passed in which is ready.
///
/// Using this directly is not recommended.
#[steel_derive::native(name = "receivers-select", arity = "AtLeast(0)")]
pub fn select(values: &[SteelVal]) -> Result<SteelVal> {
    let mut selector = crossbeam::channel::Select::new();

    let borrows = values
        .iter()
        .map(|x| SteelReceiver::as_ref(x))
        .collect::<Result<smallvec::SmallVec<[_; 8]>>>()?;

    for channel in &borrows {
        selector.recv(&channel.receiver);
    }

    // Grab the index of the one that is ready first
    selector.ready().into_steelval()
}

#[steel_derive::function(name = "channels/new")]
pub fn new_channels() -> Channels {
    Channels::new()
}

#[steel_derive::function(name = "channels-sender")]
pub fn channels_sender(value: &SteelVal) -> Result<SteelVal> {
    Channels::as_ref(value).map(|x| x.sender())
}

#[steel_derive::function(name = "channels-receiver")]
pub fn channels_receiver(value: &SteelVal) -> Result<SteelVal> {
    Channels::as_ref(value).map(|x| x.receiver())
}

#[steel_derive::function(name = "channel/send")]
pub fn channel_send(sender: &SteelVal, value: SteelVal) -> Result<SteelVal> {
    SteelSender::as_ref(sender)?
        .sender
        .send(value)
        .map_err(|e| {
            throw!(Generic => "channel disconnected - 
            unable to send value across channel: {:?}", e.0)()
        })
        .map(|_| SteelVal::Void)
}

#[steel_derive::function(name = "channel/recv")]
pub fn channel_recv(receiver: &SteelVal) -> Result<SteelVal> {
    SteelReceiver::as_ref(receiver)?
        .receiver
        .recv()
        .map_err(|_| {
            throw!(Generic => "Unable to receive on the channel. 
                The channel is empty and disconnected")()
        })
}

// Need singletons to use for "empty"

#[steel_derive::function(name = "channel/try-recv")]
pub fn channel_try_recv(receiver: &SteelVal) -> Result<SteelVal> {
    let value = SteelReceiver::as_ref(receiver)?.receiver.try_recv();

    match value {
        Ok(v) => Ok(v),
        Err(crossbeam::channel::TryRecvError::Empty) => Ok(empty_channel()),
        Err(crossbeam::channel::TryRecvError::Disconnected) => Ok(disconnected_channel()),
    }
}

#[cfg(not(feature = "sync"))]
thread_local! {
    static EMPTY_CHANNEL_OBJECT: once_cell::unsync::Lazy<(SteelVal, crate::values::structs::StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        crate::values::structs::make_struct_singleton("#%empty-channel".into())
    });

    static DISCONNECTED_CHANNEL_OBJECT: once_cell::unsync::Lazy<(SteelVal, crate::values::structs::StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        crate::values::structs::make_struct_singleton("#%disconnected-channel".into())
    });
}

#[cfg(feature = "sync")]
pub static EMPTY_CHANNEL_OBJECT: once_cell::sync::Lazy<(
    SteelVal,
    crate::values::structs::StructTypeDescriptor,
)> = once_cell::sync::Lazy::new(|| {
    crate::values::structs::make_struct_singleton("#%empty-channel".into())
});

#[cfg(feature = "sync")]
pub static DISCONNECTED_CHANNEL_OBJECT: once_cell::sync::Lazy<(
    SteelVal,
    crate::values::structs::StructTypeDescriptor,
)> = once_cell::sync::Lazy::new(|| {
    crate::values::structs::make_struct_singleton("#%empty-channel".into())
});

/// Returns `#t` if the value is an empty-channel object.
///
/// (empty-channel-object? any/c) -> bool?
#[function(name = "empty-channel-object?")]
pub fn empty_channel_objectp(value: &SteelVal) -> bool {
    let SteelVal::CustomStruct(struct_) = value else {
        return false;
    };

    #[cfg(feature = "sync")]
    {
        struct_.type_descriptor == EMPTY_CHANNEL_OBJECT.1
    }

    #[cfg(not(feature = "sync"))]
    {
        EMPTY_CHANNEL_OBJECT.with(|eof| struct_.type_descriptor == eof.1)
    }
}

pub fn empty_channel() -> SteelVal {
    #[cfg(feature = "sync")]
    {
        EMPTY_CHANNEL_OBJECT.0.clone()
    }

    #[cfg(not(feature = "sync"))]
    {
        EMPTY_CHANNEL_OBJECT.with(|eof| eof.0.clone())
    }
}

/// Returns `#t` if the value is an disconnected-channel object.
///
/// (eof-object? any/c) -> bool?
#[function(name = "disconnected-channel-object?")]
pub fn disconnected_channel_objectp(value: &SteelVal) -> bool {
    let SteelVal::CustomStruct(struct_) = value else {
        return false;
    };

    #[cfg(feature = "sync")]
    {
        struct_.type_descriptor == DISCONNECTED_CHANNEL_OBJECT.1
    }

    #[cfg(not(feature = "sync"))]
    {
        DISCONNECTED_CHANNEL_OBJECT.with(|eof| struct_.type_descriptor == eof.1)
    }
}

pub fn disconnected_channel() -> SteelVal {
    #[cfg(feature = "sync")]
    {
        DISCONNECTED_CHANNEL_OBJECT.0.clone()
    }

    #[cfg(not(feature = "sync"))]
    {
        DISCONNECTED_CHANNEL_OBJECT.with(|eof| eof.0.clone())
    }
}

#[steel_derive::context(name = "current-thread-id", arity = "Exact(0)")]
pub fn engine_id(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(SteelVal::IntV(ctx.thread.id.0 as _)))
}

#[cfg(not(feature = "sync"))]
#[steel_derive::context(name = "spawn-native-thread", arity = "Exact(1)")]
pub(crate) fn spawn_native_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    builtin_stop!(Generic => "the feature needed for spawn-native-thread is not enabled.")
}

/// Spawns the given `func` on another thread. It is required that the arity of the
/// given function be 0. If the arity of the given function cannot be checked until runtime,
/// the thread will be spawned and the function will fail to execute.
///
/// # Examples
///
/// ```scheme
/// (define thread (spawn-native-thread (lambda () (displayln "Hello world!"))))
/// ```
#[cfg(feature = "sync")]
#[steel_derive::context(name = "spawn-native-thread", arity = "Exact(1)")]
pub(crate) fn spawn_native_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    // We are now in a world in which we have to support safe points
    ctx.thread.safepoints_enabled = true;

    let thread_time = std::time::Instant::now();

    // Do this here?
    let mut thread = ctx.thread.clone();

    // println!("Created thread");

    // let interrupt = Arc::new(AtomicBool::new(false));
    // Let this thread have its own interrupt handler
    let controller = ThreadStateController::default();
    thread.synchronizer.state = controller.clone();
    // This thread needs its own context
    thread.synchronizer.ctx = Arc::new(AtomicCell::new(None));

    thread.id = EngineId::new();

    let weak_ctx = Arc::downgrade(&thread.synchronizer.ctx);

    let func = args[0].clone();

    // Try closing open continuations?
    // TODO: See if we can close them here?

    // Anything open, should be closed. And we'll be okay... I think.
    // This could be an unreasonably negative performance hit, however in
    // spirit I'd imagine its for the best in order to make sure that open
    // continuations are legal to be called across other threads.

    let now = std::time::Instant::now();
    for frame in &ctx.thread.stack_frames {
        ctx.close_continuation_marks(frame);
    }
    log::debug!(target: "threads", "Time to close continuations: {:?}", now.elapsed());

    // Meta continuations should actually be captured?
    ctx.close_continuation_marks(&ctx.thread.current_frame);

    // thread.stack = Vec::new();
    // thread.stack_frames = Vec::new();

    // The whole stack above and below... should be able to be dropped
    // from this context? - basically, unwind the stack since we're no
    // longer going to be in this context.

    let handle = std::thread::spawn(move || {
        let constant_map = thread.compiler.read().constant_map.clone();

        // TODO: We have to use the `execute` function in vm.rs - this sets up
        // the proper dynamic wind stuff that is built in. Otherwise, it seems
        // like we're not getting it installed correctly, and things are dying
        thread
            .call_function(constant_map, func, Vec::new())
            .map(|_| ())
            .map_err(|e| e.to_string())

        // thread.execute(func, , )
    });

    let value = ThreadHandle {
        handle: Some(handle),
        thread_state_manager: controller,
    }
    .into_steelval()
    .unwrap();

    // Store for the shared runtime
    ctx.thread
        .synchronizer
        .threads
        .lock()
        .unwrap()
        .push(ThreadContext {
            ctx: weak_ctx,
            handle: value.clone(),
        });

    log::debug!(target: "threads", "Time to spawn thread: {:?}", thread_time.elapsed());

    Some(Ok(value))
}

// Use internal spawn_thread function
pub(crate) fn spawn_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(spawn_thread_result(ctx, args))
}

// Move values back and forth across threads!
impl Custom for std::sync::mpsc::Sender<SerializableSteelVal> {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        Some(SerializableSteelVal::Custom(Box::new(self.clone())))
    }
}

struct SReceiver {
    receiver: Option<std::sync::mpsc::Receiver<SerializableSteelVal>>,
}

// TODO: @Matt - Revisit this!
unsafe impl Sync for SReceiver {}

impl Custom for SReceiver {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        let inner = self.receiver.take();

        let new_channel = SReceiver { receiver: inner };

        Some(SerializableSteelVal::Custom(Box::new(new_channel)))
    }
}

impl Custom for std::thread::ThreadId {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("#<{:?}>", self)))
    }
}

pub struct ThreadLocalStorage(usize);
impl crate::rvals::Custom for ThreadLocalStorage {}

/// Creates a thread local storage slot. These slots are static, and will _not_ be reclaimed.
///
/// When spawning a new thread, the value inside will be shared into that slot, however
/// future updates to the slot will be local to that thread.
#[steel_derive::context(name = "make-tls", arity = "Exact(0)")]
pub(crate) fn make_tls(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let index = ctx.thread.thread_local_storage.len();
    ctx.thread.thread_local_storage.push(args[0].clone());
    Some(ThreadLocalStorage(index).into_steelval())
}

/// Get the value out of the thread local storage slot.
#[steel_derive::context(name = "get-tls", arity = "Exact(1)")]
pub(crate) fn get_tls(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if let SteelVal::Custom(c) = &args[0] {
        if let Some(tls_index) = as_underlying_type::<ThreadLocalStorage>(c.read().as_ref()) {
            ctx.thread
                .thread_local_storage
                .get(tls_index.0)
                .map(|x| Ok(x.clone()))
        } else {
            todo!()
        }
    } else {
        builtin_stop!(Generic => "get-tls expects a thread local storage handler, found: {:?}", &args[0])
    }
}

/// Set the value in the the thread local storage. Only this thread will see the updates associated
/// with this TLS.
#[steel_derive::context(name = "set-tls!", arity = "Exact(2)")]
pub(crate) fn set_tls(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    if let SteelVal::Custom(c) = &args[0] {
        if let Some(tls_index) = as_underlying_type::<ThreadLocalStorage>(c.read().as_ref()) {
            ctx.thread.thread_local_storage[tls_index.0] = args[1].clone();

            Some(Ok(SteelVal::Void))
        } else {
            todo!()
        }
    } else {
        todo!()
    }
}

// TODO: Document these
pub fn threading_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/threads");

    module.register_native_fn_definition(SPAWN_NATIVE_THREAD_DEFINITION);

    module
        .register_value(
            "spawn-thread!",
            SteelVal::BuiltIn(crate::steel_vm::vm::spawn_thread),
        )
        .register_native_fn_definition(THREAD_JOIN_DEFINITION)
        .register_native_fn_definition(THREAD_INTERRUPT_DEFINITION)
        .register_native_fn_definition(THREAD_SUSPEND_DEFINITION)
        .register_native_fn_definition(THREAD_RESUME_DEFINITION)
        .register_native_fn_definition(THREAD_FINISHED_DEFINITION)
        .register_native_fn_definition(NEW_MUTEX_DEFINITION)
        .register_native_fn_definition(MUTEX_LOCK_DEFINITION)
        .register_native_fn_definition(MUTEX_UNLOCK_DEFINITION)
        .register_native_fn_definition(MAKE_TLS_DEFINITION)
        .register_native_fn_definition(SET_TLS_DEFINITION)
        .register_native_fn_definition(GET_TLS_DEFINITION)
        .register_native_fn_definition(NEW_CHANNELS_DEFINITION)
        .register_native_fn_definition(CHANNELS_SENDER_DEFINITION)
        .register_native_fn_definition(CHANNELS_RECEIVER_DEFINITION)
        .register_native_fn_definition(CHANNEL_SEND_DEFINITION)
        .register_native_fn_definition(CHANNEL_RECV_DEFINITION)
        .register_native_fn_definition(CHANNEL_TRY_RECV_DEFINITION)
        .register_native_fn_definition(SELECT_DEFINITION)
        .register_native_fn_definition(EMPTY_CHANNEL_OBJECTP_DEFINITION)
        .register_native_fn_definition(DISCONNECTED_CHANNEL_OBJECTP_DEFINITION)
        .register_native_fn_definition(ENGINE_ID_DEFINITION)
        .register_fn("make-channels", || {
            let (left, right) = std::sync::mpsc::channel::<SerializableSteelVal>();

            crate::list![
                left,
                SReceiver {
                    receiver: Some(right)
                }
            ]
        })
        .register_fn(
            "channel->send",
            |channel: &std::sync::mpsc::Sender<SerializableSteelVal>,
             val: SteelVal|
             -> Result<()> {
                let mut map = HashMap::new();
                let mut visited = HashSet::new();

                // TODO: Handle this here somehow, we don't want to use an empty map
                let serializable =
                    crate::rvals::into_serializable_value(val, &mut map, &mut visited)?;

                if !map.is_empty() {
                    stop!(Generic => "Unable to send mutable variable over a channel");
                }

                channel
                    .send(serializable)
                    .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))
            },
        )
        // TODO: These need to be fucntions that take the context
        .register_fn("channel->recv", |channel: &SReceiver| -> Result<SteelVal> {
            let receiver = channel
                .receiver
                .as_ref()
                .expect("Channel should not be dropped here!");

            let value = receiver
                .recv()
                .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))?;

            let mut heap = Heap::new_empty();
            let mut fake_heap = HashMap::new();
            let mut patcher = HashMap::new();
            let mut built_functions = HashMap::new();
            let mut serializer = HeapSerializer {
                heap: &mut heap,
                fake_heap: &mut fake_heap,
                values_to_fill_in: &mut patcher,
                built_functions: &mut built_functions,
            };

            let value = crate::rvals::from_serializable_value(&mut serializer, value);

            Ok(value)
        })
        .register_fn(
            "channel->try-recv",
            |channel: &SReceiver| -> Result<Option<SteelVal>> {
                let receiver = channel
                    .receiver
                    .as_ref()
                    .expect("Channel should not be dropped here!");

                let value = receiver.try_recv();

                let mut heap = Heap::new_empty();
                let mut fake_heap = HashMap::new();
                let mut patcher = HashMap::new();
                let mut built_functions = HashMap::new();
                let mut serializer = HeapSerializer {
                    heap: &mut heap,
                    fake_heap: &mut fake_heap,
                    values_to_fill_in: &mut patcher,
                    built_functions: &mut built_functions,
                };

                match value {
                    Ok(v) => Ok(Some(crate::rvals::from_serializable_value(
                        &mut serializer,
                        v,
                    ))),
                    Err(std::sync::mpsc::TryRecvError::Empty) => Ok(None),
                    Err(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
                }
            },
        )
        .register_fn("thread::current/id", || std::thread::current().id())
        .register_fn("thread/available-parallelism", || {
            std::thread::available_parallelism().map(|x| x.get()).ok()
        });
    module
}
