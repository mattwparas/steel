use crate::{
    rvals::{Custom, SerializableSteelVal},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

use super::*;

// TODO: Do proper logging here for thread spawning
macro_rules! time {
    ($label:expr, $e:expr) => {{
        let now = std::time::Instant::now();

        let e = $e;

        log::debug!(target: "threads", "{}: {:?}", $label, now.elapsed());

        e
    }};
}

pub struct ThreadHandle {
    // If this can hold a native steelerr object that would be nice
    handle: Option<std::thread::JoinHandle<std::result::Result<(), String>>>,
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

/// This will naively deep clone the environment, by attempting to translate every value into a `SerializableSteelVal`
/// While this does work, it does result in a fairly hefty deep clone of the environment. It does _not_ smartly attempt
/// to keep track of what values this function could touch - rather it assumes every value is possible to be touched
/// by the child thread. In addition - closures which capture mutable variables are unable to be moved across threads.
/// Only pure functions and/or functions which capture immutable can be moved.
fn spawn_thread_result(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    use crate::rvals::SerializableSteelVal;

    let now = std::time::Instant::now();

    // Need a new:
    // Stack
    // Heap
    // global env - This we can do (hopefully) lazily. Only clone the values that actually
    // get referenced. We can also just straight up reject any closures that cannot be moved
    // across threads
    struct MovableThread {
        constants: Vec<SerializableSteelVal>,
        global_env: Vec<SerializableSteelVal>,
        function_interner: MovableFunctionInterner,
        runtime_options: RunTimeOptions,
    }

    struct MovableFunctionInterner {
        closure_interner: fxhash::FxHashMap<usize, SerializedLambda>,
        pure_function_interner: fxhash::FxHashMap<usize, SerializedLambda>,
        spans: fxhash::FxHashMap<usize, Vec<Span>>,
        instructions: fxhash::FxHashMap<usize, Vec<DenseInstruction>>,
    }

    if args.len() != 1 {
        stop!(ArityMismatch => "spawn-thread! accepts one argument, found: {}", args.len())
    }

    // If it is a native function, theres no reason we can't just call it on a new thread, most likely.
    // There might be some funny business with thread local values, but for now we'll just accept it.
    let function: SerializedLambda = match &args[0] {
        SteelVal::FuncV(f) => {
            let func = *f;

            let handle =
                std::thread::spawn(move || func(&[]).map(|_| ()).map_err(|e| e.to_string()));

            return ThreadHandle {
                handle: Some(handle),
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
            }
            .into_steelval();
        }

        // Probably rename unwrap to something else
        SteelVal::Closure(f) => f.unwrap().try_into()?,
        illegal => {
            stop!(TypeMismatch => "Cannot spawn value on another thread: {}", illegal);
        }
    };

    let thread = MovableThread {
        constants: time!(
            "Constant map serialization",
            ctx.thread.constant_map.to_serializable_vec()
        ),

        // Void in this case, is a poisoned value. We need to trace the closure
        // (and all of its references) - to find any / all globals that _could_ be
        // referenced.
        global_env: time!(
            "Global env serialization",
            ctx.thread
                .global_env
                .bindings_vec
                .iter()
                .cloned()
                .map(into_serializable_value)
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
                            v.clone().try_into().expect("This shouldn't fail!");
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
                            v.unwrap().try_into().expect("This shouldn't fail!");
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

    // TODO: Spawn a bunch of threads at the start to handle requests. That way we don't need to do this
    // the whole time they're in there.
    let handle = std::thread::spawn(move || {
        // Moved over the thread. We now have
        let closure: ByteCodeLambda = function.into();

        // New thread! It will result in a run time error if the function references globals that cannot be shared
        // between threads. This is a bit of an unfortunate occurrence - we probably _should_ just have the engine share
        // as much as possible between threads.
        let mut thread = SteelThread {
            global_env: time!(
                "Global env creation",
                Env {
                    bindings_vec: thread
                        .global_env
                        .into_iter()
                        .map(from_serializable_value)
                        .collect(),
                }
            ),

            stack: Vec::with_capacity(64),
            profiler: OpCodeOccurenceProfiler::new(),
            function_interner: time!(
                "Function interner time",
                FunctionInterner {
                    closure_interner: thread
                        .function_interner
                        .closure_interner
                        .into_iter()
                        .map(|(k, v)| (k, v.into()))
                        .collect(),
                    pure_function_interner: thread
                        .function_interner
                        .pure_function_interner
                        .into_iter()
                        .map(|(k, v)| (k, Gc::new(v.into())))
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
                    handlers: Rc::new(RefCell::new(slotmap::SlotMap::default())),
                }
            ),
            super_instructions: Vec::new(),
            heap: Heap::new(),
            runtime_options: thread.runtime_options,
            current_frame: StackFrame::main(),
            stack_frames: Vec::with_capacity(32),
            constant_map: time!(
                "Constant map deserialization",
                ConstantMap::from_vec(
                    thread
                        .constants
                        .into_iter()
                        .map(from_serializable_value)
                        .collect(),
                )
            ),
        };

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
    }
    .into_steelval();
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
        .register_fn("thread-join!", crate::steel_vm::vm::thread_join)
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
                let serializable = crate::rvals::into_serializable_value(val)?;

                channel
                    .send(serializable)
                    .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))
            },
        )
        .register_fn("channel->recv", |channel: &SReceiver| -> Result<SteelVal> {
            let receiver = channel
                .receiver
                .as_ref()
                .expect("Channel should not be dropped here!");

            let value = receiver
                .recv()
                .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))?;

            let value = crate::rvals::from_serializable_value(value);

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

                match value {
                    Ok(v) => Ok(Some(crate::rvals::from_serializable_value(v))),
                    Err(std::sync::mpsc::TryRecvError::Empty) => Ok(None),
                    Err(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
                }
            },
        )
        .register_fn("thread::current/id", || std::thread::current().id())
        .register_fn("current-os!", || std::env::consts::OS);
    module
}
