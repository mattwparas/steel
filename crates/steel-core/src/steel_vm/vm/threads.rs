use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use steel_derive::function;

use crate::rvals::{from_serializable_value, SerializedId};
use crate::{
    compiler::modules::{BUILT_INS, MANGLER_PREFIX, MANGLER_SEPARATOR},
    rvals::{
        AsRefMutSteelVal, AsRefSteelVal as _, Custom, HeapSerializer, NativeRefSpec,
        SerializableSteelVal, SerializationContext, SteelByteVector,
    },
    steel_vm::{builtin::BuiltInModule, engine::ModuleContainer, register_fn::RegisterFn},
    values::structs::{SendableVTableEntry, VTable},
};
use crate::{steel_deserialize, steel_serialize};

use super::*;

pub struct ThreadHandle {
    pub(crate) handle:
        Mutex<Option<std::thread::JoinHandle<core::result::Result<SteelVal, String>>>>,

    pub(crate) thread: std::thread::Thread,

    pub(crate) thread_state_manager: ThreadStateController,

    pub(crate) forked_thread_handle: Option<std::sync::Weak<Mutex<SteelThread>>>,

    // What the thread returned, until it is joined. A collection can't see
    // inside the join handle.
    pub(crate) result: Arc<Mutex<Option<SteelVal>>>,
}

/// Check if the given thread is finished running.
#[steel_derive::function(name = "thread-finished?", returns = Bool)]
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

impl crate::rvals::Custom for ThreadHandle {
    fn gc_visit_children(&self, context: &mut crate::values::closed::MarkAndSweepContext) {
        if let Some(value) = self.result.lock().unwrap().as_ref() {
            context.push_back(value.clone());
        }
    }
}

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
#[steel_derive::function(name = "mutex", args(), raises = Never)]
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
#[steel_derive::function(name = "lock-release!", returns = Void)]
pub fn mutex_unlock(mutex: &SteelVal) -> Result<SteelVal> {
    MutexGuard::as_ref(mutex)?.unlock();
    Ok(SteelVal::Void)
}

/// Block until this thread finishes.
#[steel_derive::function(name = "thread-join!")]
pub fn thread_join(handle: &SteelVal) -> Result<SteelVal> {
    // The handle is not kept borrowed while waiting, since a collection
    // reads it.
    let (join_handle, result) = {
        let handle = ThreadHandle::as_ref(handle)?;
        let join_handle = handle.handle.lock().unwrap().take();
        (join_handle, handle.result.clone())
    };

    let Some(join_handle) = join_handle else {
        stop!(ContractViolation => "thread handle has already been joined!");
    };

    let joined = join_handle.join();

    // The value is only in hand from here on.
    crate::values::closed::leave_safepoint_for_cells();
    result.lock().unwrap().take();

    joined
        .map_err(|_| SteelErr::new(ErrorKind::Generic, "thread panicked!".to_string()))?
        .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
}

pub(crate) fn thread_join_impl(handle: &ThreadHandle) -> Result<SteelVal> {
    if let Some(join_handle) = handle.handle.lock().unwrap().take() {
        let joined = join_handle.join();
        crate::values::closed::leave_safepoint_for_cells();
        handle.result.lock().unwrap().take();
        joined
            .map_err(|_| SteelErr::new(ErrorKind::Generic, "thread panicked!".to_string()))?
            .map_err(|x| SteelErr::new(ErrorKind::Generic, x.to_string()))
    } else {
        stop!(ContractViolation => "thread handle has already been joined!");
    }
}

/// Suspend the thread. Note, this will _not_ interrupt any native code that is
/// potentially running in the thread, and will attempt to block at the next
/// bytecode instruction that is running.
#[steel_derive::function(name = "thread-suspend", returns = Void)]
pub(crate) fn thread_suspend(handle: &SteelVal) -> Result<SteelVal> {
    ThreadHandle::as_ref(handle)?.thread_state_manager.suspend();

    Ok(SteelVal::Void)
}

/// Resume a suspended thread. This does nothing if the thread is already joined.
#[steel_derive::function(name = "thread-resume", returns = Void)]
pub(crate) fn thread_resume(handle: &SteelVal) -> Result<SteelVal> {
    let handle = ThreadHandle::as_ref(handle)?;
    handle.thread_state_manager.unsuspend();
    handle.thread.unpark();
    Ok(SteelVal::Void)
}

/// Interrupts the thread. Note, this will _not_ interrupt any native code
/// that is potentially running in the thread, and will attempt to block
/// at the next bytecode instruction that is running.
#[steel_derive::function(name = "thread-interrupt", returns = Void)]
pub(crate) fn thread_interrupt(handle: &SteelVal) -> Result<SteelVal> {
    let handle = ThreadHandle::as_ref(handle)?;
    handle.thread_state_manager.interrupt();
    // It may be parked, suspended.
    handle.thread.unpark();
    Ok(SteelVal::Void)
}

pub fn closure_into_serializable(
    c: &ByteCodeLambda,
    ctx: &mut SerializationContext,
) -> Result<SerializedLambda> {
    let mut constants = HashMap::new();

    for (position, instr) in c.body_exp.iter().enumerate() {
        // The first instruction is sent as what it was before the function
        // was compiled, which is kept in the header.
        let op_code = match c.header {
            Some(header) if position == 0 => header,
            _ => instr.op_code,
        };

        match op_code {
            OpCode::PUSHCONST => {
                let index = instr.payload_size.to_usize();
                let value = ctx.constants.get(index);
                constants.insert(index, ctx.place(value)?);
            }
            // The globals it refers to have to be sent with it.
            OpCode::CALLGLOBAL
            | OpCode::CALLPRIMITIVE
            | OpCode::PUSH
            | OpCode::CALLGLOBALTAIL
            | OpCode::CALLGLOBALNOARITY
            | OpCode::CALLGLOBALTAILNOARITY => {
                ctx.reachable_globals.insert(instr.payload_size.to_usize());
            }
            _ => {}
        }
    }

    let mut body_exp: Vec<_> = c.body_exp.iter().cloned().collect();
    if let (Some(header), Some(first)) = (c.header, body_exp.first_mut()) {
        first.op_code = header;
    }

    let mut captures = Vec::with_capacity(c.captures.len());
    for capture in c.captures.iter() {
        captures.push(ctx.place(capture.clone())?);
    }

    Ok(SerializedLambda {
        id: c.id,
        body_exp,
        arity: c.arity as _,
        is_multi_arity: c.is_multi_arity,
        captures,
        constants,
    })
}

#[steel_derive::context(name = "deserialize-value", arity = Exact(1))]
fn deserialize_value(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(deserialize_individual_value_impl(ctx, args))
}

#[steel_derive::context(name = "serialize-value", arity = Exact(1))]
fn serialize_value(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(serialize_individual_value_impl(ctx, args))
}

// #[steel_derive::context(name = "serialize-thread", arity = Exact(0))]
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

// A value as it is sent: the table of everything it is made of, which entry
// of that is the value itself, and what a function among them needs to be
// found where it is read.
#[derive(Debug, Serialize, Deserialize)]
struct SerializedValue {
    nodes: Vec<SerializableSteelVal>,
    value: SerializedId,
    // Map the index of something to the name of it, so that we can
    // build a new reverse mapping when deserializing the bytecode
    symbol_index_map: HashMap<usize, InternedString>,

    // The globals the functions refer to, by the index they have here.
    referenced_globals: HashMap<usize, SerializedId>,

    vtable_entries: Vec<SendableVTableEntry>,
}

impl Custom for SerializedValue {}

// What the bytes of a serialized value start with. A table whose entries
// name each other by place came in with 2. What was written before that
// nested them, and is not read.
const SERIALIZED_FORMAT: u8 = 2;

// The struct type here that a struct sent from elsewhere belongs to.
//
// Struct types are kept in one table for the whole process, and two engines
// that each define a struct of one name have a type each in it. So the one
// this engine means by the name is asked for first, through the `struct:`
// binding a struct definition makes, and only failing that is the table
// searched.
fn struct_type_here(
    thread: &SteelThread,
    entry: &SendableVTableEntry,
) -> Option<crate::values::structs::StructTypeDescriptor> {
    use crate::rvals::FromSteelVal;
    use crate::values::structs::StructTypeDescriptor;

    let binding = InternedString::from_string(format!("struct:{}", entry.name.resolve()));
    let index = thread.compiler.read().symbol_map.get(&binding).ok();
    let known = index
        .and_then(|index| thread.global_env.roots().get(index).cloned())
        .and_then(|value| StructTypeDescriptor::from_steelval(&value).ok())
        .filter(|descriptor| VTable::matches_sendable_entry(*descriptor, entry));

    known.or_else(|| VTable::get_by_sendable_entry(entry))
}

fn deserialize_individual_value_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let value = args[0].clone();

    // Read and not taken apart, so that it can be deserialized again.
    let inner = SerializedValue::as_ref(&value)?;

    let mut built_functions = FxHashMap::default();

    // Waited for at a safepoint, since the thread that has it may be
    // collecting and waiting for this one to stop.
    let heap = ctx.thread.lock_heap_holding(&[]);

    let mut serializer =
        HeapSerializer::new(&inner.nodes, heap, &mut built_functions, &mut ctx.thread);

    // Map the struct type descriptors between the two

    for entry in inner.vtable_entries.iter() {
        // Assert that the things in the vtable are the same.

        let old = entry.desc;

        let new = struct_type_here(serializer.thread, entry).ok_or_else(
            throw!(Generic => "Struct not defined yet in this environment: {:?}", entry),
        )?;

        serializer.struct_map.insert(old, new);
    }

    let mut already_here = FxHashSet::default();

    // Populate the symbol maps, using all of the values
    // that exist.
    for (index, name) in &inner.symbol_index_map {
        let sent = match inner.referenced_globals.get(index) {
            Some(id) => Some(serializer.node(*id)?),
            None => None,
        };

        if let Some(SerializableSteelVal::BuiltinSteelModuleRef(m, k)) = sent {
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
            already_here.insert(*index);

            continue;
        }

        // A global this engine has already is the one that is used. What
        // was sent with a function is what it referred to where it came
        // from, and binding that here would change the global under
        // everything else that uses it: a function read back by the engine
        // that wrote it would leave that engine's globals pointing at
        // copies. Two programs with the same definitions get each their
        // own, the struct types among them.
        let existing = serializer.thread.compiler.read().symbol_map.get(name).ok();
        let existing = existing.filter(|idx| serializer.thread.extract_value(*idx).is_some());
        if let Some(existing) = existing {
            serializer.global_mapping.insert(*index, existing);
            already_here.insert(*index);
            continue;
        }

        let idx = serializer.thread.compiler.write().symbol_map.add(&name);
        serializer.global_mapping.insert(*index, idx);
    }

    for (index, global) in inner.referenced_globals.iter() {
        if already_here.contains(index) {
            continue;
        }
        let Some(idx) = serializer.global_mapping.get(index).copied() else {
            stop!(Generic => "a global was sent without its name");
        };

        let deserialized = from_serializable_value(&mut serializer, *global)?;

        // Make the new global point to the new, deserialized value. The
        // heap's lock is held already.
        serializer
            .thread
            .with_locked_env_holding_heap(move |_, env| env.repl_define_idx(idx, deserialized));
    }

    from_serializable_value(&mut serializer, inner.value)
}

#[steel_derive::function(name = "serialized->bytes", returns = ByteVector)]
fn serialized_to_bytes(value: &SteelVal) -> Result<SteelVal> {
    let inner = SerializedValue::as_ref(value)?;

    let mut bytes = vec![SERIALIZED_FORMAT];
    bytes.extend(steel_serialize(&*inner)?);

    Ok(SteelVal::ByteVector(crate::rvals::SteelByteVector::new(
        bytes,
    )))
}

#[steel_derive::function(name = "bytes->serialized", args(ByteVector))]
fn bytes_to_serialized(value: &SteelByteVector) -> Result<SteelVal> {
    let guard = value.vec.read();

    let Some((format, table)) = guard.split_first() else {
        stop!(Generic => "no bytes are not a serialized value");
    };
    if *format != SERIALIZED_FORMAT {
        stop!(Generic => "these bytes are not a serialized value that this can read");
    }

    steel_deserialize::<SerializedValue>(table)?.into_steelval()
}

// Serialize one individual value.
fn serialize_individual_value_impl(ctx: &mut VmCore, args: &[SteelVal]) -> Result<SteelVal> {
    let value = args[0].clone();

    let compiler_guard = ctx.thread.compiler.read();
    let builtin_modules = compiler_guard.builtin_modules.clone();
    let mut sctx = SerializationContext::new(
        &builtin_modules,
        ctx.thread.global_env.roots(),
        &compiler_guard.symbol_map,
        &compiler_guard.constant_map,
        &compiler_guard.module_manager,
    );

    let serialized_value = into_serializable_value(value, &mut sctx)?;

    let mut symbol_index_map = HashMap::new();

    let mut old_mapping = HashMap::new();

    let mut entries = Vec::new();

    // A function names the globals it uses, a global can be a function,
    // and either can hold a struct, so this goes around until nothing new
    // turns up.
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

                        // If this is a built in that can't be sent, then
                        // just record the fact that we should resolve to
                        // the original built in.
                        if matches!(global, SteelVal::Custom(_) | SteelVal::PortV(_))
                            && BUILT_INS.iter().any(|x| x.0 == path.resolve())
                        {
                            let reference =
                                sctx.push(SerializableSteelVal::BuiltinSteelModuleRef(
                                    path.resolve().to_string(),
                                    name.to_string(),
                                ))?;
                            old_mapping.insert(idx, reference);
                            symbol_index_map.insert(idx, ident);
                            continue;
                        }
                    }
                }
            }

            let global = sctx.globals[idx].clone();

            old_mapping.insert(idx, into_serializable_value(global, &mut sctx)?);

            // Take this value, and make sure that the values are aligned.
            symbol_index_map.insert(idx, ident);
        }

        let reachable_structs = std::mem::take(&mut sctx.reachable_structs);
        let local_entries = VTable::sendable_entries_for(&mut sctx, reachable_structs)?;
        entries.extend(local_entries);

        if sctx.reachable_globals.is_empty() && sctx.reachable_structs.is_empty() {
            break;
        }
    }

    SerializedValue {
        nodes: sctx.nodes,
        value: serialized_value,
        symbol_index_map,
        referenced_globals: old_mapping,
        vtable_entries: entries,
    }
    .into_steelval()
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

// What has been sent and not yet received, in order, for a collection to
// find. It can't look inside the channel.
type InTransit = Arc<Mutex<std::collections::VecDeque<SteelVal>>>;

fn visit_in_transit(
    in_transit: &InTransit,
    context: &mut crate::values::closed::MarkAndSweepContext,
) {
    for value in in_transit.lock().unwrap().iter() {
        context.push_back(value.clone());
    }
}

pub struct SteelReceiver {
    receiver: crossbeam_channel::Receiver<SteelVal>,
    in_transit: InTransit,
}

impl SteelReceiver {
    // Called with a value just taken off the channel.
    fn received(&self, value: &SteelVal) {
        crate::values::closed::leave_safepoint_for_cells();

        let mut in_transit = self.in_transit.lock().unwrap();
        // Another receiver may have taken the front one in the meantime
        match in_transit
            .iter()
            .position(|sent| SteelVal::ptr_eq(sent, value))
        {
            Some(position) => {
                in_transit.remove(position);
            }
            None => {
                in_transit.pop_front();
            }
        }
    }
}

pub struct SteelSender {
    sender: crossbeam_channel::Sender<SteelVal>,
    in_transit: InTransit,
}

pub struct Channels {
    sender: SteelVal,
    receiver: SteelVal,
}

impl Custom for SteelReceiver {
    fn gc_visit_children(&self, context: &mut crate::values::closed::MarkAndSweepContext) {
        visit_in_transit(&self.in_transit, context);
    }
}

impl Custom for SteelSender {
    fn gc_visit_children(&self, context: &mut crate::values::closed::MarkAndSweepContext) {
        visit_in_transit(&self.in_transit, context);
    }
}
impl Custom for Channels {}

impl Channels {
    pub fn new() -> Self {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let in_transit = InTransit::default();

        Self {
            sender: SteelSender {
                sender,
                in_transit: in_transit.clone(),
            }
            .into_steelval()
            .unwrap(),
            receiver: SteelReceiver {
                receiver,
                in_transit,
            }
            .into_steelval()
            .unwrap(),
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
#[steel_derive::native(name = "receivers-select", arity = AtLeast(0), returns = Number)]
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

#[steel_derive::function(name = "channels/new", args(), raises = Never)]
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

#[steel_derive::function(name = "channel/send", returns = Void)]
pub fn channel_send(sender: &SteelVal, value: SteelVal) -> Result<SteelVal> {
    let sender = SteelSender::as_ref(sender)?;

    // Held across the send so the two stay in the same order.
    let mut in_transit = sender.in_transit.lock().unwrap();
    sender.sender.send(value.clone()).map_err(|e| {
        throw!(Generic => "channel disconnected -
            unable to send value across channel: {:?}", e.0)()
    })?;
    in_transit.push_back(value);

    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "channel/recv")]
pub fn channel_recv(receiver: &SteelVal) -> Result<SteelVal> {
    let receiver = SteelReceiver::as_ref(receiver)?;
    let value = receiver.receiver.recv().map_err(|_| {
        throw!(Generic => "Unable to receive on the channel.
                The channel is empty and disconnected")()
    })?;
    receiver.received(&value);

    Ok(value)
}

// Need singletons to use for "empty"

#[steel_derive::function(name = "channel/try-recv")]
pub fn channel_try_recv(receiver: &SteelVal) -> Result<SteelVal> {
    let receiver = SteelReceiver::as_ref(receiver)?;
    let value = receiver.receiver.try_recv();

    match value {
        Ok(v) => {
            receiver.received(&v);
            Ok(v)
        }
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
#[function(name = "empty-channel-object?", args(Any), raises = Never)]
pub fn empty_channel_objectp(value: &SteelVal) -> bool {
    let SteelVal::CustomStruct(struct_) = value else {
        return false;
    };

    #[cfg(feature = "sync")]
    {
        struct_.descriptor() == EMPTY_CHANNEL_OBJECT.1
    }

    #[cfg(not(feature = "sync"))]
    {
        EMPTY_CHANNEL_OBJECT.with(|eof| struct_.descriptor() == eof.1)
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
#[function(name = "disconnected-channel-object?", args(Any), raises = Never)]
pub fn disconnected_channel_objectp(value: &SteelVal) -> bool {
    let SteelVal::CustomStruct(struct_) = value else {
        return false;
    };

    #[cfg(feature = "sync")]
    {
        struct_.descriptor() == DISCONNECTED_CHANNEL_OBJECT.1
    }

    #[cfg(not(feature = "sync"))]
    {
        DISCONNECTED_CHANNEL_OBJECT.with(|eof| struct_.descriptor() == eof.1)
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

#[steel_derive::context(
    name = "current-thread-id",
    arity = Exact(0),
    returns = Int,
    args(),
    raises = Never
)]
pub fn engine_id(ctx: &mut VmCore, _args: &[SteelVal]) -> Option<Result<SteelVal>> {
    Some(Ok(SteelVal::IntV(ctx.thread.id.0 as _)))
}

#[cfg(not(feature = "sync"))]
#[steel_derive::context(name = "spawn-native-thread", arity = Exact(1))]
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
/// Holds a new thread at a safepoint until it has been registered. A
/// collection only scans registered threads.
#[cfg(feature = "sync")]
fn wait_to_be_registered(
    thread: &mut SteelThread,
    func: &SteelVal,
    registered: &(Mutex<bool>, std::sync::Condvar),
) {
    thread.enter_safepoint_holding(core::slice::from_ref(func), |_| {
        let mut open = registered.0.lock().unwrap();
        while !*open {
            open = registered.1.wait(open).unwrap();
        }
    });
}

#[cfg(feature = "sync")]
#[steel_derive::context(
    name = "spawn-native-thread",
    arity = Exact(1),
    alias = "spawn-thread",
    args(Any),
    raises = Never
)]
pub(crate) fn spawn_native_thread(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    // We are now in a world in which we have to support safe points
    ctx.thread.safepoints_enabled = true;

    let thread_time = crate::time::Instant::now();

    // Do this here?
    let mut thread = ctx.thread.clone();
    // The limit is where this thread's stack ends, and the new thread has a
    // stack of its own. Left as it is, every jitted call on the new thread
    // would find no room and run in the interpreter.
    #[cfg(feature = "jit2")]
    {
        thread.native_stack_limit = 0;
    }

    thread.stack.clear();
    thread.stack_frames.clear();
    thread.current_frame = StackFrame::main();

    // let interrupt = Arc::new(AtomicBool::new(false));
    // Let this thread have its own interrupt handler
    let controller = ThreadStateController::default();
    thread.synchronizer.state = controller.clone();
    // This thread needs its own context
    thread.synchronizer.ctx = Arc::new(std::sync::atomic::AtomicPtr::new(core::ptr::null_mut()));

    thread.id = EngineId::new();

    let weak_ctx = Arc::downgrade(&thread.synchronizer.ctx);

    let func = args[0].clone();

    // The new thread can reach what this one can, so from here on nothing
    // uses a cell without its lock. This has to come before the thread starts.
    crate::values::closed::share_cells();

    // See `Synchronizer::announce`
    ctx.thread.synchronizer.announce();

    // TODO: Continuations should not cross thread barriers.
    // Install continuation barriers here?

    // Make sure that this thread is certainly registered with the current system,
    // and otherwise we can install the runtime system to associate with living threads
    // with something.

    #[cfg(feature = "biased")]
    {
        steel_rc::register_thread();
    }

    // See `wait_to_be_registered`
    let registered = Arc::new((Mutex::new(false), std::sync::Condvar::new()));
    let opened = registered.clone();
    let result = Arc::new(Mutex::new(None));
    let returned = result.clone();

    #[cfg(feature = "biased")]
    let handle = steel_rc::with_explicit_merge(move || {
        let mut thread = thread;
        wait_to_be_registered(&mut thread, &func, &registered);

        let constant_map = thread.compiler.read().constant_map.clone();

        // TODO: We have to use the `execute` function in vm.rs - this sets up
        // the proper dynamic wind stuff that is built in. Otherwise, it seems
        // like we're not getting it installed correctly, and things are dying
        let returned = thread
            .call_function(constant_map, func, Vec::new())
            .map_err(|e| e.to_string());
        // Before the thread is gone, while a collection still waits on it.
        if let Ok(value) = &returned {
            *result.lock().unwrap() = Some(value.clone());
        }
        returned
    });

    #[cfg(not(feature = "biased"))]
    let handle = std::thread::spawn(move || {
        let mut thread = thread;
        wait_to_be_registered(&mut thread, &func, &registered);

        let constant_map = thread.compiler.read().constant_map.clone();

        // TODO: We have to use the `execute` function in vm.rs - this sets up
        // the proper dynamic wind stuff that is built in. Otherwise, it seems
        // like we're not getting it installed correctly, and things are dying
        let returned = thread
            .call_function(constant_map, func, Vec::new())
            .map_err(|e| e.to_string());
        // Before the thread is gone, while a collection still waits on it.
        if let Ok(value) = &returned {
            *result.lock().unwrap() = Some(value.clone());
        }
        returned
    });

    let thread = handle.thread().clone();

    let value = ThreadHandle {
        handle: Mutex::new(Some(handle)),
        thread,
        thread_state_manager: controller,
        forked_thread_handle: None,
        result: returned,
    }
    .into_steelval()
    .unwrap();

    ctx.thread.enter_safepoint(|thread| {
        // Store for the shared runtime
        thread.synchronizer.register_announced(ThreadContext {
            ctx: weak_ctx.clone(),
            handle: value.clone(),
        });
    });

    *opened.0.lock().unwrap() = true;
    opened.1.notify_all();

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
#[steel_derive::context(name = "make-tls", arity = Exact(1), args(Any), raises = Never)]
pub(crate) fn make_tls(ctx: &mut VmCore, args: &[SteelVal]) -> Option<Result<SteelVal>> {
    let index = ctx.thread.thread_local_storage.len();
    ctx.thread.thread_local_storage.push(args[0].clone());
    Some(ThreadLocalStorage(index).into_steelval())
}

/// Get the value out of the thread local storage slot.
#[steel_derive::context(name = "get-tls", arity = Exact(1))]
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
#[steel_derive::context(name = "set-tls!", arity = Exact(2), returns = Void)]
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
