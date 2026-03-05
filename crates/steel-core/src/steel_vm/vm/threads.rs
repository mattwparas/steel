use core::ops::Deref;
use std::collections::HashSet;

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use steel_derive::function;

use crate::rvals::{from_serializable_value, SerializedHeapRefVector};
use crate::{
    compiler::modules::{BUILT_INS, MANGLER_PREFIX, MANGLER_SEPARATOR},
    rvals::{
        AsRefMutSteelVal, AsRefSteelVal as _, Custom, HeapSerializer, NativeRefSpec,
        SerializableSteelVal, SerializationContext, SerializedHeapRef, SteelByteVector,
    },
    steel_vm::{builtin::BuiltInModule, engine::ModuleContainer, register_fn::RegisterFn},
    values::{
        functions::SerializedLambdaPrototype,
        structs::{SendableVTableEntry, VTable},
    },
};
use crate::{steel_deserialize, steel_serialize};

use super::*;

pub struct ThreadHandle {
    pub(crate) handle:
        Mutex<Option<std::thread::JoinHandle<core::result::Result<SteelVal, String>>>>,

    pub(crate) thread: std::thread::Thread,

    pub(crate) thread_state_manager: ThreadStateController,

    pub(crate) forked_thread_handle: Option<std::sync::Weak<Mutex<SteelThread>>>,
}

/// Check if the given thread is finished running.
#[steel_derive::function(name = "thread-finished?")]
pub fn thread_finished(handle: &SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(
        ThreadHandle::as_ref(handle)?
            .handle
            .lock()
            .unwrap()
            .as_ref()
            .map(|x| x.is_finished())
            .unwrap_or(true),
    ))
}

impl crate::rvals::Custom for ThreadHandle {}

pub struct SteelMutex {
    mutex: Arc<parking_lot::Mutex<()>>,
}

impl crate::rvals::Custom for SteelMutex {}

pub struct MutexGuard {
    guard: AtomicCell<Option<parking_lot::ArcMutexGuard<parking_lot::RawMutex, ()>>>,
}

impl crate::rvals::Custom for MutexGuard {}

impl SteelMutex {
    pub fn new() -> Self {
        Self {
            mutex: Arc::new(parking_lot::Mutex::new(())),
        }
    }

    // Attempt to lock it before killing the other one
    pub fn lock(&self) -> SteelVal {
        // Acquire the lock first
        MutexGuard {
            guard: AtomicCell::new(Some(self.mutex.lock_arc())),
        }
        .into_steelval()
        .unwrap()
    }
}

impl MutexGuard {
    pub fn unlock(&self) {
        drop(self.guard.take());
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
    Ok(SteelMutex::as_ref(mutex)?.lock())
}

/// Unlock the given mutex.
#[steel_derive::function(name = "lock-release!")]
pub fn mutex_unlock(mutex: &SteelVal) -> Result<SteelVal> {
    MutexGuard::as_ref(mutex)?.unlock();
    Ok(SteelVal::Void)
}

/// Block until this thread finishes.
#[steel_derive::function(name = "thread-join!")]
pub fn thread_join(handle: &SteelVal) -> Result<SteelVal> {
    ThreadHandle::as_ref(handle).and_then(|mut x| thread_join_impl(&mut x))
}

pub(crate) fn thread_join_impl(handle: &ThreadHandle) -> Result<SteelVal> {
    if let Some(handle) = handle.handle.lock().unwrap().take() {
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
    let handle = ThreadHandle::as_mut_ref(handle)?;
    handle.thread_state_manager.resume();
    handle.thread.unpark();
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
    ctx: &mut SerializationContext,
) -> Result<SerializedLambda> {
    if let Some(prototype) = CACHED_CLOSURES.with(|x| x.borrow().get(&c.id).cloned()) {
        let mut prototype = SerializedLambda {
            id: prototype.id,
            body_exp: prototype.body_exp,
            arity: prototype.arity,
            is_multi_arity: prototype.is_multi_arity,
            captures: Vec::new(),
            constants: HashMap::new(),
        };

        prototype.captures = c
            .captures
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, ctx))
            .collect::<Result<_>>()?;

        Ok(prototype)
    } else {
        let mut constants = HashMap::new();

        for instr in c.body_exp.iter() {
            if instr.op_code == OpCode::PUSHCONST {
                let index = instr.payload_size.to_usize();
                let value = ctx.constants.get(index);
                constants.insert(index, into_serializable_value(value, ctx).unwrap());
            }

            match instr.op_code {
                // If this instruction touches this global variable,
                // then we want to mark it as possibly referenced here.
                OpCode::CALLGLOBAL
                | OpCode::CALLPRIMITIVE
                | OpCode::PUSH
                | OpCode::CALLGLOBALTAIL
                | OpCode::CALLGLOBALNOARITY
                | OpCode::CALLGLOBALTAILNOARITY => {
                    let idx = instr.payload_size.to_usize();
                    ctx.reachable_globals.insert(idx);
                }
                _ => {}
            }
        }

        let mut prototype = SerializedLambdaPrototype {
            id: c.id,

            body_exp: c.body_exp.iter().cloned().collect(),

            arity: c.arity as _,
            is_multi_arity: c.is_multi_arity,
            constants,
        };

        if let Some(header) = c.header {
            prototype.body_exp[0].op_code = header;
        }

        CACHED_CLOSURES.with(|x| x.borrow_mut().insert(c.id, prototype.clone()));

        let mut prototype = SerializedLambda {
            id: prototype.id,
            body_exp: prototype.body_exp,
            arity: prototype.arity,
            is_multi_arity: prototype.is_multi_arity,
            captures: Vec::new(),
            constants: prototype.constants,
        };

        prototype.captures = c
            .captures
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, ctx))
            .collect::<Result<_>>()?;

        Ok(prototype)
    }
}

#[allow(unused)]
struct MovableThread {
    constants: Vec<SerializableSteelVal>,
    global_env: Vec<SerializableSteelVal>,
    function_interner: MovableFunctionInterner,
    _runtime_options: RunTimeOptions,
}

#[allow(unused)]
struct MovableFunctionInterner {
    closure_interner: FxHashMap<u32, SerializedLambda>,
    pure_function_interner: FxHashMap<u32, SerializedLambda>,
    spans: FxHashMap<u32, Vec<Span>>,
}

#[allow(unused)]
struct EngineImage {
    vtable: Vec<SendableVTableEntry>,
    heap_map: HashMap<usize, SerializedHeapRef>,
    thread: MovableThread,
}

#[steel_derive::context(name = "deserialize-value", arity = "Exact(1)")]
fn deserialize_value(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(deserialize_individual_value_impl(ctx, args))
}

#[steel_derive::context(name = "serialize-value", arity = "Exact(1)")]
fn serialize_value(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(serialize_individual_value_impl(ctx, args))
}

// #[steel_derive::context(name = "serialize-thread", arity = "Exact(0)")]
// fn serialize_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
//     Some(serialize_thread_impl(ctx, args))
// }

// Create a native ref spec from the builtin modules.
// This should in theory be all we need in order to then reconstruct this
// native value reference on the other side.
//
// When deconstructing the value, we'll attempt to find the key for it
// and provide the builtin modules. Assuming it exists, we can locate
// the value and move on with our lives.
pub(crate) fn create_native_ref(ctx: &ModuleContainer, v: SteelVal) -> Option<NativeRefSpec> {
    // Not good, but lets just see if it even works:
    let module_map = ctx.inner();

    for (mkey, module) in module_map.iter() {
        let map = module.inner_map();

        for (key, value) in map.iter() {
            // Just check each item, and drain from our list if its it?

            if value == &v {
                return Some(NativeRefSpec {
                    module: mkey.to_string(),
                    key: key.to_string(),
                });
            }
        }
    }

    None
}

#[derive(Debug, Serialize, Deserialize)]
struct SerializedValue {
    value: SerializableSteelVal,
    // Map the index of something to the name of it, so that we can
    // build a new reverse mapping when deserializing the bytecode
    symbol_index_map: HashMap<usize, InternedString>,

    referenced_globals: HashMap<usize, SerializableSteelVal>,

    vtable_entries: Vec<SendableVTableEntry>,

    serialized_heap: HashMap<usize, SerializableSteelVal>,

    serialized_vec_heap: HashMap<usize, Vec<SerializableSteelVal>>,
}

impl Custom for SerializedValue {}

fn deserialize_individual_value_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let value = args[0].clone();

    let mut inner = SerializedValue::as_mut_ref(&value)?;

    let mut mapping = std::mem::take(&mut inner.serialized_heap)
        .into_iter()
        .map(|(key, value)| (key, SerializedHeapRef::Serialized(Some(value))))
        .collect();

    let mut vector_mapping = std::mem::take(&mut inner.serialized_vec_heap)
        .into_iter()
        .map(|(key, value)| (key, SerializedHeapRefVector::Serialized(Some(value))))
        .collect();

    // Have these values point to the new place
    let mut patcher = HashMap::new();
    let mut vector_patcher = HashMap::new();
    let mut built_functions = HashMap::new();

    let mut serializer = HeapSerializer {
        fake_heap: &mut mapping,
        values_to_fill_in: &mut patcher,
        built_functions: &mut built_functions,
        thread: &mut ctx.thread,
        function_mapping: HashMap::new(),
        global_mapping: HashMap::new(),
        fake_vector_heap: &mut vector_mapping,
        vectors_to_fill_in: &mut vector_patcher,
        struct_map: HashMap::new(),
    };

    // Map the struct type descriptors between the two

    for entry in std::mem::take(&mut inner.vtable_entries) {
        // Assert that the things in the vtable are the same.

        let old = entry.desc;

        let new = VTable::get_by_sendable_entry(&entry).ok_or_else(
            throw!(Generic => "Struct not defined yet in this environment: {:?}", entry),
        )?;

        serializer.struct_map.insert(old, new);
    }

    // Populate the symbol maps, using all of the values
    // that exist.
    for (index, name) in &inner.symbol_index_map {
        if let Some(SerializableSteelVal::BuiltinSteelModuleRef(m, k)) =
            inner.referenced_globals.get(index)
        {
            // Find the existing index
            let key = format!(
                "{}{}{}{}",
                MANGLER_PREFIX,
                InternedString::from_str(m.as_str()).get().into_inner(),
                MANGLER_SEPARATOR,
                k
            );

            let interned = InternedString::from_string(key);
            let existing_index = serializer
                .thread
                .compiler
                .read()
                .symbol_map
                .get(&interned)?;

            serializer.global_mapping.insert(*index, existing_index);

            continue;
        }

        let idx = serializer.thread.compiler.write().symbol_map.add(&name);
        serializer.global_mapping.insert(*index, idx);
    }

    for (index, global) in inner.referenced_globals.drain() {
        if let SerializableSteelVal::BuiltinSteelModuleRef(_, _) = global {
            continue;
        }

        let deserialized = from_serializable_value(&mut serializer, global)?;
        let idx = serializer.global_mapping[&index];

        // Make the new global point to the new, deserialized value
        serializer.thread.insert_binding(idx, deserialized);
    }

    // let original_value = std::mem::replace(&mut inner.value, SerializableSteelVal::Void);
    // let deserialized = from_serializable_value(&mut serializer, original_value);

    for (key, value) in serializer.values_to_fill_in.iter_mut() {
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

    for (key, value) in serializer.vectors_to_fill_in.iter_mut() {
        if let Some(cycled) = serializer.fake_vector_heap.get(key) {
            match cycled {
                SerializedHeapRefVector::Serialized(_) => todo!(),
                // Patch over the cycle
                SerializedHeapRefVector::Closed(c) => {
                    value.set(c.get());
                }
            }
        } else {
            todo!()
        }
    }

    let original_value = std::mem::replace(&mut inner.value, SerializableSteelVal::Void);
    let deserialized = from_serializable_value(&mut serializer, original_value)?;

    Ok(deserialized)
}

#[steel_derive::function(name = "serialized->bytes")]
fn serialized_to_bytes(value: &SteelVal) -> Result<SteelVal> {
    let inner = SerializedValue::as_ref(value)?;
    let bytes = steel_serialize(inner.deref())?;
    Ok(SteelVal::ByteVector(crate::rvals::SteelByteVector::new(
        bytes,
    )))
}

#[steel_derive::function(name = "bytes->serialized")]
fn bytes_to_serialized(value: &SteelByteVector) -> Result<SteelVal> {
    let serde = steel_deserialize::<SerializedValue>(value.vec.read().as_slice())?;
    serde.into_steelval()
}

// Serialize one individual value.
fn serialize_individual_value_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let value = args[0].clone();

    let mut initial_map = HashMap::new();
    let mut initial_vector_map = HashMap::new();
    let mut visited = HashSet::new();
    let _sources = ctx.thread.compiler.read().sources.clone();
    let compiler_guard = ctx.thread.compiler.read();
    let builtin_modules = compiler_guard.builtin_modules.clone();
    let mut sctx = SerializationContext {
        builtin_modules: &builtin_modules,
        serialized_heap: &mut initial_map,
        serialized_heap_vectors: &mut initial_vector_map,
        visited: &mut visited,
        globals: ctx.thread.global_env.roots(),
        symbol_map: &compiler_guard.symbol_map,
        constants: &compiler_guard.constant_map,
        reachable_globals: HashSet::new(),
        reachable_structs: HashSet::new(),
        compiled_modules: &compiler_guard.module_manager,
    };

    let serialized_value = into_serializable_value(value, &mut sctx)?;

    // Somehow, figure out structs as well
    // println!("Succeeded in serializing the value");
    // println!("Going through the reachable globals");

    let mut symbol_index_map = HashMap::new();

    let mut old_mapping = HashMap::new();

    let mut entries = Vec::new();

    loop {
        for idx in std::mem::take(&mut sctx.reachable_globals) {
            let ident = sctx.symbol_map.values()[idx];

            if old_mapping.contains_key(&idx) {
                continue;
            }

            let resolved = ident.resolve();

            // This refers to a built in
            if resolved.starts_with(MANGLER_PREFIX) {
                let split = resolved
                    .strip_prefix(MANGLER_PREFIX)
                    .and_then(|x| x.split_once(MANGLER_SEPARATOR));

                if let Some((module, name)) = split {
                    let id = module.parse::<usize>();
                    if let Ok(id) = id {
                        let path = InternedString::new(id - 1);

                        let global = sctx.globals[idx].clone();

                        if matches!(global, SteelVal::Custom(_) | SteelVal::PortV(_)) {
                            println!(
                                "Found unserializable value @ path: {} -> {} - {}",
                                module,
                                path.resolve(),
                                name
                            );

                            // If this is a built in, then just record the fact that we should
                            // resolve to the original built in (or something)
                            if BUILT_INS.iter().find(|x| x.0 == path.resolve()).is_some() {
                                old_mapping.insert(
                                    idx,
                                    SerializableSteelVal::BuiltinSteelModuleRef(
                                        path.resolve().to_string(),
                                        name.to_string(),
                                    ),
                                );

                                symbol_index_map.insert(idx, ident);
                                continue;
                            }
                        }
                    }
                }
            }

            let global = sctx.globals[idx].clone();

            old_mapping.insert(idx, into_serializable_value(global, &mut sctx)?);

            // Take this value, and make sure that the values are aligned.
            symbol_index_map.insert(idx, ident);
        }

        // println!("Finished serializing globals");
        // println!(
        //     "Remaining globals to check: {}",
        //     sctx.reachable_globals.len()
        // );

        let reachable_structs = std::mem::take(&mut sctx.reachable_structs);

        // println!("Checking structs: {}", reachable_structs.len());

        let local_entries = VTable::sendable_entries_for(&mut sctx, reachable_structs)?;

        entries.extend(local_entries);

        // println!("Entries: {}", entries.len());

        // println!("Checking if we need to do another pass...");
        // println!(
        //     "Remaining globals to check: {}",
        //     sctx.reachable_globals.len()
        // );
        // println!("Checking structs: {}", sctx.reachable_structs.len());

        if sctx.reachable_globals.len() == 0 && sctx.reachable_structs.len() == 0 {
            break;
        }
    }

    // println!("Done");

    let value = SerializedValue {
        value: serialized_value,
        symbol_index_map,
        referenced_globals: old_mapping,
        vtable_entries: entries,
        serialized_heap: initial_map,
        serialized_vec_heap: initial_vector_map,
    };

    // dbg!(&value);

    value.into_steelval()
}

/*
fn serialize_thread_impl(ctx: &mut VmCore, _args: &[SteelVal]) -> Result<SteelVal> {
    // use crate::rvals::SerializableSteelVal;

    #[cfg(feature = "profiling")]
    let now = std::time::Instant::now();

    // Need a new:
    // Stack
    // Heap
    // global env - This we can do (hopefully) lazily. Only clone the values that actually
    // get referenced. We can also just straight up reject any closures that cannot be moved
    // across threads

    let mut initial_map = HashMap::new();
    let mut visited = HashSet::new();
    let _sources = ctx.thread.compiler.read().sources.clone();
    let mut compiler_guard = ctx.thread.compiler.write();
    let builtin_modules = compiler_guard.builtin_modules.clone();
    let mut sctx = SerializationContext {
        builtin_modules: &builtin_modules,
        serialized_heap: &mut initial_map,
        visited: &mut visited,
        globals: ctx.thread.global_env.roots(),
        symbol_map: &compiler_guard.symbol_map,
        constants: &compiler_guard.constant_map,
        reachable_globals: HashSet::new(),
        reachable_structs: HashSet::new(),
        compiled_modules: &compiler_guard.module_manager,
    };
    let constants = ctx.thread.constant_map.to_serializable_vec(&mut sctx);

    let thread = MovableThread {
        constants,

        // Void in this case, is a poisoned value. We need to trace the closure
        // (and all of its references) - to find any / all globals that _could_ be
        // referenced.
        #[cfg(feature = "sync")]
        global_env: ctx
            .thread
            .global_env
            .roots()
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, &mut sctx).unwrap())
            .collect(),

        #[cfg(not(feature = "sync"))]
        global_env: ctx
            .thread
            .global_env
            .bindings_vec
            .iter()
            .cloned()
            .map(|x| into_serializable_value(x, &mut sctx))
            .collect(),

        // Populate with the values after moving into the thread, spawn accordingly
        // TODO: Move this out of here
        function_interner: MovableFunctionInterner {
            closure_interner: ctx
                .thread
                .function_interner
                .closure_interner
                .iter()
                .map(|(k, v)| {
                    let v_prime: SerializedLambda =
                        closure_into_serializable(v, &mut sctx).expect("This shouldn't fail!");
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
                        closure_into_serializable(v, &mut sctx).expect("This shouldn't fail!");
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
        },

        _runtime_options: ctx.thread.runtime_options.clone(),
    };

    let sendable_vtable_entries = VTable::sendable_entries(&mut sctx)?;

    // TODO: Spawn a bunch of threads at the start to handle requests. That way we don't need to do this
    // the whole time they're in there.
    let heap = Arc::new(Mutex::new(Heap::new()));

    // Move across threads?
    let mut mapping = initial_map
        .into_iter()
        .map(|(key, value)| (key, SerializedHeapRef::Serialized(Some(value))))
        .collect();

    println!("Finished serializing values");

    let mut patcher = HashMap::new();
    let mut built_functions = HashMap::new();
    let mut heap_guard = heap.lock().unwrap();

    let mut serializer = HeapSerializer {
        heap: &mut heap_guard,
        fake_heap: &mut mapping,
        values_to_fill_in: &mut patcher,
        built_functions: &mut built_functions,
        modules: compiler_guard.builtin_modules.clone(),
        globals: &mut ctx.thread.global_env,
        function_mapping: HashMap::new(),
        compiler: &mut compiler_guard,
        global_mapping: HashMap::new(),
    };

    // Moved over the thread. We now have
    // let closure: ByteCodeLambda = ByteCodeLambda::from_serialized(&mut serializer, function);

    VTable::initialize_new_thread(sendable_vtable_entries, &mut serializer);

    println!("Initialized vtable.");

    let _constant_map = ConstantMap::from_vec(
        thread
            .constants
            .into_iter()
            .map(|x| from_serializable_value(&mut serializer, x))
            .collect(),
    );

    println!("Initialized constant map");

    #[cfg(feature = "sync")]
    let _global_env = Env::new(
        &thread
            .global_env
            .into_iter()
            .map(|x| from_serializable_value(&mut serializer, x))
            .collect::<Vec<_>>(),
    );

    #[cfg(not(feature = "sync"))]
    let global_env = Env {
        bindings_vec: thread
            .global_env
            .into_iter()
            .map(|x| from_serializable_value(&mut serializer, x))
            .collect(),
    };

    println!("Initialized global env");

    let _function_interner = FunctionInterner {
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
            .map(|(k, v)| {
                (
                    k,
                    if let Some(exists) = serializer.built_functions.get(&v.id) {
                        exists.clone()
                    } else {
                        Gc::new(ByteCodeLambda::from_serialized(&mut serializer, v))
                    },
                )
            })
            .collect(),
        spans: thread
            .function_interner
            .spans
            .into_iter()
            .map(|(k, v)| (k, v.into()))
            .collect(),
        #[cfg(feature = "jit2")]
        jit_funcs: HashMap::default(),
    };

    println!("Initialized function interner");

    // Patch over the values in the final heap!

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

    println!("Patched heap");

    drop(heap_guard);

    println!("Finished.");

    Ok(SteelVal::Void)
}
*/

pub struct SteelReceiver {
    receiver: crossbeam_channel::Receiver<SteelVal>,
}

pub struct SteelSender {
    sender: crossbeam_channel::Sender<SteelVal>,
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
        let (sender, receiver) = crossbeam_channel::unbounded();

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
    let mut selector = crossbeam_channel::Select::new();

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
        Err(crossbeam_channel::TryRecvError::Empty) => Ok(empty_channel()),
        Err(crossbeam_channel::TryRecvError::Disconnected) => Ok(disconnected_channel()),
    }
}

#[cfg(not(feature = "sync"))]
thread_local! {
    static EMPTY_CHANNEL_OBJECT: once_cell::unsync::Lazy<(SteelVal, crate::values::structs::StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        crate::values::structs::make_struct_singleton("#%empty-channel")
    });

    static DISCONNECTED_CHANNEL_OBJECT: once_cell::unsync::Lazy<(SteelVal, crate::values::structs::StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        crate::values::structs::make_struct_singleton("#%disconnected-channel")
    });
}

#[cfg(feature = "sync")]
pub static EMPTY_CHANNEL_OBJECT: once_cell::sync::Lazy<(
    SteelVal,
    crate::values::structs::StructTypeDescriptor,
)> =
    once_cell::sync::Lazy::new(|| crate::values::structs::make_struct_singleton("#%empty-channel"));

#[cfg(feature = "sync")]
pub static DISCONNECTED_CHANNEL_OBJECT: once_cell::sync::Lazy<(
    SteelVal,
    crate::values::structs::StructTypeDescriptor,
)> =
    once_cell::sync::Lazy::new(|| crate::values::structs::make_struct_singleton("#%empty-channel"));

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
pub(crate) fn spawn_native_thread(
    _ctx: &mut VmCore,
    _args: &[SteelVal],
) -> Option<Result<SteelVal>> {
    builtin_stop!(Generic => "the feature needed for spawn-native-thread is not enabled.")
}

/// Spawns the given `func` on another thread. It is required that the arity of the
/// given function be 0. If the arity of the given function cannot be checked until runtime,
/// the thread will be spawned and the function will fail to execute.
///
/// ```scheme
/// (spawn-native-thread func)
/// ```
///
/// func : (-> any?) ;; Function with no arguments, returns anything
///
/// # Examples
///
/// ```scheme
/// (define thread (spawn-native-thread (lambda () (displayln "Hello world!"))))
/// ```
#[cfg(feature = "sync")]
#[steel_derive::context(
    name = "spawn-native-thread",
    arity = "Exact(1)",
    alias = "spawn-thread"
)]
pub(crate) fn spawn_native_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    // We are now in a world in which we have to support safe points
    ctx.thread.safepoints_enabled = true;

    let thread_time = crate::time::Instant::now();

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

    // TODO: Continuations should not cross thread barriers.
    // Install continuation barriers here?

    // Make sure that this thread is certainly registered with the current system,
    // and otherwise we can install the runtime system to associate with living threads
    // with something.

    #[cfg(feature = "biased")]
    {
        steel_rc::register_thread();
    }

    #[cfg(feature = "biased")]
    let handle = steel_rc::with_explicit_merge(move || {
        let constant_map = thread.compiler.read().constant_map.clone();

        // TODO: We have to use the `execute` function in vm.rs - this sets up
        // the proper dynamic wind stuff that is built in. Otherwise, it seems
        // like we're not getting it installed correctly, and things are dying
        thread
            .call_function(constant_map, func, Vec::new())
            .map_err(|e| e.to_string())
    });

    #[cfg(not(feature = "biased"))]
    let handle = std::thread::spawn(move || {
        let constant_map = thread.compiler.read().constant_map.clone();

        // TODO: We have to use the `execute` function in vm.rs - this sets up
        // the proper dynamic wind stuff that is built in. Otherwise, it seems
        // like we're not getting it installed correctly, and things are dying
        thread
            .call_function(constant_map, func, Vec::new())
            .map_err(|e| e.to_string())
    });

    let thread = handle.thread().clone();

    let value = ThreadHandle {
        handle: Mutex::new(Some(handle)),
        thread,
        thread_state_manager: controller,
        forked_thread_handle: None,
    }
    .into_steelval()
    .unwrap();

    ctx.thread.enter_safepoint(|thread| {
        // Store for the shared runtime
        thread
            .synchronizer
            .threads
            .lock()
            .unwrap()
            .push(ThreadContext {
                ctx: weak_ctx.clone(),
                handle: value.clone(),
            });
    });

    log::debug!(target: "threads", "Time to spawn thread: {:?}", thread_time.elapsed());

    Some(Ok(value))
}

impl Custom for std::thread::ThreadId {
    fn fmt(&self) -> Option<core::result::Result<String, core::fmt::Error>> {
        Some(Ok(format!("#<{:?}>", self)))
    }
}

#[derive(Clone)]
pub struct ThreadLocalStorage(usize);
impl crate::rvals::Custom for ThreadLocalStorage {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        // TODO: This probably should have a different implementation?
        // Some(SerializableSteelVal::Custom(Box::new(self.clone())))
        None
    }
}

/// Creates a thread local storage slot. These slots are static, and will _not_ be reclaimed.
///
/// When spawning a new thread, the value inside will be shared into that slot, however
/// future updates to the slot will be local to that thread.
#[steel_derive::context(name = "make-tls", arity = "Exact(1)")]
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
        // .register_fn("make-channels", || {
        //     let (left, right) = std::sync::mpsc::channel::<SerializableSteelVal>();
        //     crate::list![
        //         left,
        //         SReceiver {
        //             receiver: Some(right)
        //         }
        //     ]
        // })
        // .register_fn(
        //     "channel->send",
        //     |_channel: &std::sync::mpsc::Sender<SerializableSteelVal>,
        //      _val: SteelVal|
        //      -> Result<()> {
        //         todo!()
        //         // let mut map = HashMap::new();
        //         // let mut visited = HashSet::new();
        //         // // TODO: Handle this here somehow, we don't want to use an empty map
        //         // let serializable =
        //         //     crate::rvals::into_serializable_value(val, &mut map, &mut visited)?;
        //         // if !map.is_empty() {
        //         //     stop!(Generic => "Unable to send mutable variable over a channel");
        //         // }
        //         // channel
        //         //     .send(serializable)
        //         //     .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))
        //     },
        // )
        // // TODO: These need to be fucntions that take the context
        // .register_fn(
        //     "channel->recv",
        //     |_channel: &SReceiver| -> Result<SteelVal> {
        //         // let receiver = channel
        //         //     .receiver
        //         //     .as_ref()
        //         //     .expect("Channel should not be dropped here!");
        //         // let value = receiver
        //         //     .recv()
        //         //     .map_err(|e| SteelErr::new(ErrorKind::Generic, e.to_string()))?;
        //         // let mut heap = Heap::new_empty();
        //         // let mut fake_heap = HashMap::new();
        //         // let mut patcher = HashMap::new();
        //         // let mut built_functions = HashMap::new();
        //         // let mut serializer = HeapSerializer {
        //         //     heap: &mut heap,
        //         //     fake_heap: &mut fake_heap,
        //         //     values_to_fill_in: &mut patcher,
        //         //     built_functions: &mut built_functions,
        //         // };
        //         // let value = crate::rvals::from_serializable_value(&mut serializer, value);
        //         // Ok(value)
        //         todo!()
        //     },
        // )
        // .register_fn(
        //     "channel->try-recv",
        //     |_channel: &SReceiver| -> Result<Option<SteelVal>> {
        //         todo!()
        //         // let receiver = channel
        //         //     .receiver
        //         //     .as_ref()
        //         //     .expect("Channel should not be dropped here!");
        //         // let value = receiver.try_recv();
        //         // let mut heap = Heap::new_empty();
        //         // let mut fake_heap = HashMap::new();
        //         // let mut patcher = HashMap::new();
        //         // let mut built_functions = HashMap::new();
        //         // let mut serializer = HeapSerializer {
        //         //     heap: &mut heap,
        //         //     fake_heap: &mut fake_heap,
        //         //     values_to_fill_in: &mut patcher,
        //         //     built_functions: &mut built_functions,
        //         // };
        //         // match value {
        //         //     Ok(v) => Ok(Some(crate::rvals::from_serializable_value(
        //         //         &mut serializer,
        //         //         v,
        //         //     ))),
        //         //     Err(std::sync::mpsc::TryRecvError::Empty) => Ok(None),
        //         //     Err(e) => Err(SteelErr::new(ErrorKind::Generic, e.to_string())),
        //         // }
        //     },
        // )
        .register_fn("thread::current/id", || std::thread::current().id())
        .register_fn("thread/available-parallelism", || {
            std::thread::available_parallelism().map(|x| x.get()).ok()
        });
    module
}
