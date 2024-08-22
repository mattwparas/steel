use std::collections::HashSet;

use fxhash::FxHashMap;
use parking_lot::RwLock;

use crate::{
    rvals::{Custom, HeapSerializer, SerializableSteelVal, SerializedHeapRef},
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

impl ThreadHandle {
    pub fn is_finished(&self) -> bool {
        self.handle
            .as_ref()
            .map(|x| x.is_finished())
            .unwrap_or(true)
    }
}

impl crate::rvals::Custom for ThreadHandle {}

// pub struct Mutex

pub(crate) fn thread_join(handle: &mut ThreadHandle) -> Result<()> {
    if let Some(handle) = handle.handle.take() {
        handle
            .join()
            .map_err(|_| SteelErr::new(ErrorKind::Generic, "thread panicked!".to_string()))?
            .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
    } else {
        stop!(ContractViolation => "thread handle has already been joined!");
    }
}

pub(crate) fn thread_suspend(handle: &mut ThreadHandle) {
    handle.thread_state_manager.suspend();
}

pub(crate) fn thread_resume(handle: &mut ThreadHandle) {
    handle.thread_state_manager.resume();
    if let Some(handle) = handle.handle.as_mut() {
        handle.thread().unpark();
    }
}

pub(crate) fn thread_interrupt(handle: &mut ThreadHandle) {
    handle.thread_state_manager.interrupt();
}

thread_local! {
    static CACHED_CLOSURES: RefCell<FxHashMap<u32, SerializedLambdaPrototype>> = RefCell::new(FxHashMap::default());
}

pub fn closure_into_serializable(
    c: &ByteCodeLambda,
    serializer: &mut std::collections::HashMap<usize, SerializableSteelVal>,
    visited: &mut std::collections::HashSet<usize>,
) -> Result<SerializedLambda> {
    if let Some(mut prototype) = CACHED_CLOSURES.with(|x| x.borrow().get(&c.id).cloned()) {
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
        let mut prototype = SerializedLambdaPrototype {
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

    let thread = MovableThread {
        constants,

        // Void in this case, is a poisoned value. We need to trace the closure
        // (and all of its references) - to find any / all globals that _could_ be
        // referenced.
        global_env: time!(
            "Global env serialization",
            ctx.thread
                .global_env
                .bindings_vec
                .read()
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
        let mut heap = time!("Heap Creation", Arc::new(Mutex::new(Heap::new())));

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
                bindings_vec: Arc::new(RwLock::new(
                    thread
                        .global_env
                        .into_iter()
                        .map(|x| from_serializable_value(&mut serializer, x))
                        .collect()
                )),
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
            stack: Vec::with_capacity(64),
            profiler: OpCodeOccurenceProfiler::new(),
            function_interner,
            heap,
            runtime_options: thread.runtime_options,
            current_frame: StackFrame::main(),
            stack_frames: Vec::with_capacity(32),
            constant_map,
            interrupted: Default::default(),
            synchronizer: Synchronizer::new(),
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

// See... if this works...?
pub(crate) fn spawn_native_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let mut thread = ctx.thread.clone();
    let interrupt = Arc::new(AtomicBool::new(false));
    // Let this thread have its own interrupt handler
    let controller = ThreadStateController::default();
    thread.synchronizer.state = controller.clone();
    // This thread needs its own context
    thread.synchronizer.ctx = Arc::new(AtomicCell::new(None));

    let weak_ctx = Arc::downgrade(&thread.synchronizer.ctx);

    let func = args[0].clone();

    let handle = std::thread::spawn(move || {
        thread
            .call_function(thread.constant_map.clone(), func, Vec::new())
            .map(|_| ())
            .map_err(|e| e.to_string())
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

// TODO: Document these
pub fn threading_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/threads");

    module
        .register_value(
            "spawn-thread!",
            SteelVal::BuiltIn(crate::steel_vm::vm::spawn_thread),
        )
        .register_value(
            "spawn-native-thread",
            SteelVal::BuiltIn(crate::steel_vm::vm::spawn_native_thread),
        )
        .register_fn("thread-join!", crate::steel_vm::vm::thread_join)
        .register_fn("thread-interrupt", thread_interrupt)
        .register_fn("thread-suspend", thread_suspend)
        .register_fn("thread-resume", thread_resume)
        .register_fn("thread-finished?", ThreadHandle::is_finished)
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
        .register_fn("thread::current/id", || std::thread::current().id());
    module
}
