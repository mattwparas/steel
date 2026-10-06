//! Values in a form that can be sent to another thread, engine or process.
//!
//! A serialized value is a table. Each entry is one value, and where a value
//! holds others it names them by their place in the table, so nothing is
//! nested inside anything else. That is what lets both directions work
//! through a list of what is left to do instead of going down the stack once
//! for each level of the value: a chain of a million pairs is a million
//! entries, and takes no more stack than three. Copying a table, dropping
//! one and turning one to and from bytes are loops over it for the same
//! reason.
//!
//! A value that is referred to from two places is one entry named twice, so
//! it comes back as one value. A box, a mutable vector or a mutable struct
//! that is part of a cycle is an entry that, some way down, names itself.

use super::*;

use crate::values::functions::SerializedLambda;

/// A value in a serialized value's table, by its place there.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SerializedId(u32);

impl SerializedId {
    fn index(self) -> usize {
        self.0 as usize
    }
}

/// One entry of the table.
#[derive(Clone, Serialize, Deserialize)]
pub enum SerializableSteelVal {
    Closure(SerializedLambda),
    BoolV(bool),
    NumV(f64),
    IntV(isize),
    CharV(char),
    Void,
    StringV(String),
    HashMapV(Vec<(SerializedId, SerializedId)>),
    HashSet(Vec<SerializedId>),
    ListV(Vec<SerializedId>),
    Pair(SerializedId, SerializedId),
    VectorV(Vec<SerializedId>),
    FlatVector(Vec<SerializedId>),
    ByteVectorV(Vec<u8>),
    BigNum(BigInt),
    BigRational(BigRational),
    Complex(SerializedId, SerializedId),
    SymbolV(String),
    CustomStruct(SerializableUserDefinedStruct),
    // A box, and what it holds.
    HeapAllocated(SerializedId),
    // A mutable vector, and its elements.
    HeapAllocatedVector(Vec<SerializedId>),
    MutableStruct(SerializableUserDefinedStruct),
    Port(SendablePort),
    Rational(Rational32),
    Stream(SerializableStream),
    NativeRef(NativeRefSpec),
    StructConstructorSpec(StructConstructorRefSpec),
    ModuleSpec(String),
    GlobalRef(String),
    BuiltinSteelModuleRef(String, String),
    NativeStruct(String, Vec<u8>),
}

impl std::fmt::Debug for SerializableSteelVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerializableSteelVal::Closure(serialized_lambda) => {
                write!(f, "{:?}", serialized_lambda)
            }
            SerializableSteelVal::BoolV(x) => write!(f, "{}", x),
            SerializableSteelVal::NumV(x) => write!(f, "{}", x),
            SerializableSteelVal::IntV(x) => write!(f, "{}", x),
            SerializableSteelVal::CharV(x) => write!(f, "{}", x),
            SerializableSteelVal::Void => write!(f, "SteelVal::Void"),
            SerializableSteelVal::StringV(x) => write!(f, "{}", x),
            SerializableSteelVal::HashMapV(x) => write!(f, "#<hash {:?}>", x),
            SerializableSteelVal::HashSet(x) => write!(f, "#<hashset {:?}>", x),
            SerializableSteelVal::ListV(x) => write!(f, "#<list {:?}>", x),
            SerializableSteelVal::Pair(car, cdr) => write!(f, "#<pair {:?} {:?}>", car, cdr),
            SerializableSteelVal::VectorV(x) => write!(f, "#<vector {:?}>", x),
            SerializableSteelVal::FlatVector(x) => write!(f, "#<flat-vector {:?}>", x),
            SerializableSteelVal::BigNum(x) => write!(f, "{:?}", x),
            SerializableSteelVal::BigRational(x) => write!(f, "{:?}", x),
            SerializableSteelVal::Complex(re, im) => write!(f, "#<complex {:?} {:?}>", re, im),
            SerializableSteelVal::ByteVectorV(items) => write!(f, "{:?}", items),
            SerializableSteelVal::SymbolV(x) => write!(f, "{:?}", x),
            SerializableSteelVal::MutableStruct(x) => write!(f, "#<mutable-struct {:?}>", x),
            SerializableSteelVal::CustomStruct(x) => write!(f, "{:?}", x),
            SerializableSteelVal::HeapAllocated(x) => write!(f, "#<box {:?}>", x),
            SerializableSteelVal::Rational(x) => write!(f, "{:?}", x),
            SerializableSteelVal::Stream(x) => write!(f, "{:?}", x),
            SerializableSteelVal::NativeRef(x) => write!(f, "{:?}", x),
            SerializableSteelVal::StructConstructorSpec(struct_constructor_ref_spec) => {
                write!(f, "{:?}", struct_constructor_ref_spec)
            }
            SerializableSteelVal::ModuleSpec(x) => write!(f, "#<module:{}>", x),
            SerializableSteelVal::GlobalRef(x) => write!(f, "#<global:{}>", x),
            SerializableSteelVal::BuiltinSteelModuleRef(m, n) => {
                write!(f, "#<builtin-module:{}:{}>", m, n)
            }
            SerializableSteelVal::Port(sendable_port) => write!(f, "#<port:{:?}>", sendable_port),
            SerializableSteelVal::NativeStruct(n, _) => write!(f, "#<native:{}>", n),
            SerializableSteelVal::HeapAllocatedVector(x) => write!(f, "#<mutable-vector {:?}>", x),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NativeRefSpec {
    pub module: String,
    pub key: String,
    // This should also include some metadata about what the type was _before_
    // going in, and what it will be after
    // pub pointer_addr: Option<usize>,
}

/// What reading a serialized value needs: the table, what has been made from
/// it so far, and where the pieces that aren't in the table are found.
pub struct HeapSerializer<'a> {
    pub nodes: &'a [SerializableSteelVal],
    // The value made from each entry, once it has been.
    built: Vec<Option<SteelVal>>,
    // Entries that making has started on. One that is reached again before
    // it is done is part of a cycle.
    started: Vec<bool>,

    // Held for as long as values are being built. Nothing refers to what
    // has been allocated so far but this, so a collection in the meantime,
    // by this thread or another, would free it.
    pub heap: parking_lot::ArcMutexGuard<parking_lot::RawMutex, crate::values::closed::Heap>,

    // Cache the functions that get built
    pub built_functions: &'a mut rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,

    pub thread: &'a mut SteelThread,

    pub function_mapping: rustc_hash::FxHashMap<u32, u32>,

    pub global_mapping: rustc_hash::FxHashMap<usize, usize>,

    pub struct_map: rustc_hash::FxHashMap<StructTypeDescriptor, StructTypeDescriptor>,
}

impl<'a> HeapSerializer<'a> {
    pub fn new(
        nodes: &'a [SerializableSteelVal],
        heap: parking_lot::ArcMutexGuard<parking_lot::RawMutex, crate::values::closed::Heap>,
        built_functions: &'a mut rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,
        thread: &'a mut SteelThread,
    ) -> Self {
        Self {
            nodes,
            built: vec![None; nodes.len()],
            started: vec![false; nodes.len()],
            heap,
            built_functions,
            thread,
            function_mapping: rustc_hash::FxHashMap::default(),
            global_mapping: rustc_hash::FxHashMap::default(),
            struct_map: rustc_hash::FxHashMap::default(),
        }
    }

    /// The entry `id` names. Bytes from elsewhere can name anything.
    pub fn node(&self, id: SerializedId) -> Result<&'a SerializableSteelVal> {
        match self.nodes.get(id.index()) {
            Some(node) => Ok(node),
            None => {
                stop!(Generic => "a serialized value refers to a part of itself that isn't there")
            }
        }
    }

    /// The value made from the entry `id` names, which has to have been
    /// made already.
    pub fn built(&self, id: SerializedId) -> Result<SteelVal> {
        match self.built.get(id.index()) {
            Some(Some(value)) => Ok(value.clone()),
            _ => stop!(Generic => "a serialized value uses a part of itself before it is made"),
        }
    }

    fn built_all(&self, ids: &[SerializedId]) -> Result<Vec<SteelVal>> {
        ids.iter().map(|id| self.built(*id)).collect()
    }

    // The struct type a struct that was sent belongs to here. The number it
    // was sent with is the sender's.
    fn struct_type_here(&self, sent: StructTypeDescriptor) -> Result<StructTypeDescriptor> {
        match self.struct_map.get(&sent) {
            Some(here) => Ok(*here),
            None => stop!(Generic => "a struct was sent without its struct type"),
        }
    }
}

// What is left to do to make a value.
enum Step {
    // Make the entry, or see to it that what it holds is made first.
    Visit(SerializedId),
    // The same, for something a box, mutable vector or mutable struct
    // holds. Those are made before what they hold, so finding one of their
    // contents already under way is a cycle through them, which is allowed.
    VisitContents(SerializedId),
    // Everything the entry holds has been made. Put it together.
    Finish(SerializedId),
}

/// Makes the value the entry `id` names, and every value it holds.
pub fn from_serializable_value(ctx: &mut HeapSerializer, id: SerializedId) -> Result<SteelVal> {
    ctx.node(id)?;

    // Boxes, mutable vectors and mutable structs made here, to be given
    // their contents once everything is made.
    let mut to_fill = Vec::new();
    let mut steps = vec![Step::Visit(id)];

    while let Some(step) = steps.pop() {
        match step {
            Step::Visit(id) | Step::VisitContents(id) => {
                let node = ctx.node(id)?;
                let index = id.index();

                if ctx.built[index].is_some() {
                    continue;
                }
                if ctx.started[index] {
                    if matches!(step, Step::VisitContents(_)) {
                        continue;
                    }
                    stop!(Generic => "a serialized value holds itself other than through a box, a mutable vector or a mutable struct");
                }
                ctx.started[index] = true;

                match node {
                    // Made empty and filled in afterwards, so that what
                    // they hold can refer back to them.
                    SerializableSteelVal::HeapAllocated(contents) => {
                        let allocation = ctx.heap.allocate_without_collection(SteelVal::Void);
                        ctx.built[index] = Some(SteelVal::HeapAllocated(allocation));
                        to_fill.push(id);
                        steps.push(Step::VisitContents(*contents));
                    }
                    SerializableSteelVal::HeapAllocatedVector(elements) => {
                        let allocation =
                            ctx.heap
                                .allocate_vec_without_collection(HeapVec::with_capacity(
                                    elements.len(),
                                ));
                        ctx.built[index] = Some(SteelVal::MutableVector(allocation));
                        to_fill.push(id);
                        steps.extend(elements.iter().map(|id| Step::VisitContents(*id)));
                    }
                    SerializableSteelVal::MutableStruct(record) => {
                        let type_descriptor = ctx.struct_type_here(record.type_descriptor)?;
                        let handle = ctx.heap.allocate_mutable_struct_without_collection(
                            type_descriptor,
                            core::iter::repeat_n(SteelVal::Void, record.fields.len()),
                        );
                        ctx.built[index] = Some(SteelVal::MutableStruct(handle));
                        to_fill.push(id);
                        steps.extend(record.fields.iter().map(|id| Step::VisitContents(*id)));
                    }

                    // Everything else is made after what it holds.
                    _ => {
                        steps.push(Step::Finish(id));
                        for_each_held(node, |held| steps.push(Step::Visit(held)));
                    }
                }
            }

            Step::Finish(id) => {
                let value = finish(ctx, ctx.node(id)?)?;
                ctx.built[id.index()] = Some(value);
            }
        }
    }

    for id in to_fill {
        match (ctx.node(id)?, ctx.built(id)?) {
            (SerializableSteelVal::HeapAllocated(contents), SteelVal::HeapAllocated(cell)) => {
                cell.set_and_return(ctx.built(*contents)?);
            }
            (
                SerializableSteelVal::HeapAllocatedVector(elements),
                SteelVal::MutableVector(cell),
            ) => {
                let elements = ctx.built_all(elements)?;
                cell.strong_ptr().write().value.extend(elements);
            }
            (SerializableSteelVal::MutableStruct(record), SteelVal::MutableStruct(handle)) => {
                for (index, field) in record.fields.iter().enumerate() {
                    handle.set_index(index, ctx.built(*field)?);
                }
            }
            _ => unreachable!("only these are put down to be filled"),
        }
    }

    ctx.built(id)
}

// Calls `visit` with each entry that `node` holds and has to be made before
// it. A box, mutable vector or mutable struct is made first instead, and is
// not asked.
fn for_each_held(node: &SerializableSteelVal, mut visit: impl FnMut(SerializedId)) {
    match node {
        SerializableSteelVal::Closure(lambda) => {
            lambda.captures.iter().copied().for_each(&mut visit);
            lambda.constants.values().copied().for_each(&mut visit);
        }
        SerializableSteelVal::HashMapV(entries) => {
            for (key, value) in entries {
                visit(*key);
                visit(*value);
            }
        }
        SerializableSteelVal::HashSet(ids)
        | SerializableSteelVal::ListV(ids)
        | SerializableSteelVal::VectorV(ids)
        | SerializableSteelVal::FlatVector(ids) => ids.iter().copied().for_each(&mut visit),
        SerializableSteelVal::Pair(a, b) | SerializableSteelVal::Complex(a, b) => {
            visit(*a);
            visit(*b);
        }
        SerializableSteelVal::CustomStruct(record) => {
            record.fields.iter().copied().for_each(&mut visit)
        }
        SerializableSteelVal::Stream(stream) => {
            visit(stream.initial_value);
            visit(stream.stream_thunk);
        }
        _ => {}
    }
}

// Puts together the value for `node`, whose contents have all been made.
fn finish(ctx: &mut HeapSerializer, node: &SerializableSteelVal) -> Result<SteelVal> {
    match node {
        SerializableSteelVal::Closure(c) => {
            if c.captures.is_empty() {
                if let Some(already_made) = ctx
                    .function_mapping
                    .get(&c.id)
                    .and_then(|x| ctx.built_functions.get(x))
                {
                    Ok(SteelVal::Closure(already_made.clone()))
                } else {
                    let value = Gc::new(ByteCodeLambda::from_serialized(ctx, c)?);
                    let id = value.id;

                    // Save those as well
                    // Probably need to just do this for all
                    ctx.built_functions.insert(id, value.clone());
                    Ok(SteelVal::Closure(value))
                }
            } else {
                Ok(SteelVal::Closure(Gc::new(ByteCodeLambda::from_serialized(
                    ctx, c,
                )?)))
            }
        }
        SerializableSteelVal::BoolV(b) => Ok(SteelVal::BoolV(*b)),
        SerializableSteelVal::NumV(n) => Ok(SteelVal::NumV(*n)),
        SerializableSteelVal::IntV(i) => Ok(SteelVal::IntV(*i)),
        SerializableSteelVal::CharV(c) => Ok(SteelVal::CharV(*c)),
        SerializableSteelVal::Void => Ok(SteelVal::Void),
        SerializableSteelVal::Rational(r) => Ok(SteelVal::Rational(*r)),
        SerializableSteelVal::StringV(s) => Ok(SteelVal::StringV(s.as_str().into())),
        SerializableSteelVal::HashMapV(entries) => {
            let mut map = HashMap::new();
            for (key, value) in entries {
                map.insert(ctx.built(*key)?, ctx.built(*value)?);
            }
            Ok(SteelVal::HashMapV(Gc::new(map).into()))
        }
        SerializableSteelVal::HashSet(ids) => Ok(SteelVal::HashSetV(
            Gc::new(ctx.built_all(ids)?.into_iter().collect::<HashSet<_>>()).into(),
        )),
        SerializableSteelVal::ListV(ids) => {
            Ok(SteelVal::ListV(ctx.built_all(ids)?.into_iter().collect()))
        }
        SerializableSteelVal::VectorV(ids) => Ok(SteelVal::VectorV(SteelVector(Gc::new(
            ctx.built_all(ids)?.into_iter().collect(),
        )))),
        // Interned, so that it is `eq?` to the same symbol made here.
        SerializableSteelVal::SymbolV(s) => {
            let mut guard = ctx.thread.compiler.write();
            let interned = guard
                .constant_map
                .add_or_get(SteelVal::SymbolV(s.as_str().into()));
            Ok(guard.constant_map.get(interned))
        }
        SerializableSteelVal::FlatVector(ids) => {
            let mut values = steel_vec::Vec::with_capacity(ids.len());
            for id in ids {
                values.push(ctx.built(*id)?);
            }
            Ok(SteelVal::FlatVector(Gc::new(values)))
        }
        SerializableSteelVal::BigNum(n) => Ok(SteelVal::BigNum(Gc::new(n.clone()))),
        SerializableSteelVal::BigRational(r) => Ok(SteelVal::BigRational(Gc::new(r.clone()))),
        SerializableSteelVal::Complex(re, im) => Ok(SteelVal::Complex(Gc::new(SteelComplex {
            re: ctx.built(*re)?,
            im: ctx.built(*im)?,
        }))),
        SerializableSteelVal::CustomStruct(s) => {
            let type_descriptor = ctx.struct_type_here(s.type_descriptor)?;
            let mut fields = steel_vec::Vec::with_capacity(s.fields.len());
            for field in &s.fields {
                fields.push(ctx.built(*field)?);
            }
            Ok(SteelVal::CustomStruct(StructRef::new(UserDefinedStruct {
                fields,
                type_descriptor,
            })))
        }
        SerializableSteelVal::Pair(car, cdr) => {
            Ok(crate::values::lists::Pair::cons(ctx.built(*car)?, ctx.built(*cdr)?).into())
        }
        SerializableSteelVal::Stream(value) => Ok(SteelVal::StreamV(Gc::new(LazyStream {
            initial_value: ctx.built(value.initial_value)?,
            stream_thunk: ctx.built(value.stream_thunk)?,
            empty_stream: value.empty_stream,
        }))),
        SerializableSteelVal::NativeRef(s) => {
            let guard = ctx.thread.compiler.read();
            let module_map = guard.builtin_modules.inner();

            if let Some(m) = module_map.get(s.module.as_str()) {
                return Ok(m.get(s.key.clone()));
            }

            stop!(Generic => "no module {} here to find {} in", s.module, s.key);
        }
        SerializableSteelVal::ByteVectorV(bytes) => {
            Ok(SteelVal::ByteVector(SteelByteVector::new(bytes.clone())))
        }
        SerializableSteelVal::StructConstructorSpec(spec) => {
            let mut spec = spec.clone();
            // First, we need to patch any existing type descriptors to the new one.
            let Some(descriptor) = ctx.struct_map.get(&spec.descriptor) else {
                stop!(Generic => "a struct function was sent without its struct type");
            };
            spec.descriptor = *descriptor;
            match fetch_from_type_map(spec) {
                Some(value) => Ok(value),
                None => {
                    stop!(Generic => "a struct function was sent that its struct type doesn't have")
                }
            }
        }
        SerializableSteelVal::ModuleSpec(m) => {
            let module = ctx
                .thread
                .compiler
                .read()
                .builtin_modules
                .inner()
                .get(m.as_str())
                .cloned();
            match module {
                Some(module) => module.into_steelval(),
                None => stop!(Generic => "no module {} here", m),
            }
        }
        SerializableSteelVal::GlobalRef(s) => {
            let interned = s.as_str().into();
            // Find the index of this thing:
            let idx = ctx.thread.compiler.read().symbol_map.get(&interned)?;
            match ctx.thread.global_env.roots().get(idx) {
                Some(value) => Ok(value.clone()),
                None => stop!(Generic => "a global was sent that is not defined here"),
            }
        }
        SerializableSteelVal::BuiltinSteelModuleRef(module, name) => {
            stop!(Generic => "{} from {} can only be sent as a global", name, module)
        }
        SerializableSteelVal::Port(sendable_port) => Ok(SteelVal::PortV(
            SteelPort::from_sendable_port(sendable_port.clone()),
        )),
        SerializableSteelVal::NativeStruct(name, items) => {
            match call_deserializer_by_name(ctx, name, items) {
                Some(value) => value,
                None => stop!(Generic => "no deserializer here for {}", name),
            }
        }
        SerializableSteelVal::HeapAllocated(_)
        | SerializableSteelVal::HeapAllocatedVector(_)
        | SerializableSteelVal::MutableStruct(_) => {
            unreachable!("made before what they hold, and filled in afterwards")
        }
    }
}

/// What serializing a value needs: the table being built, and where to look
/// up what a value refers to without holding.
pub struct SerializationContext<'a> {
    pub builtin_modules: &'a ModuleContainer,
    pub globals: &'a [SteelVal],
    pub symbol_map: &'a SymbolMap,
    pub constants: &'a ConstantMap,
    pub reachable_globals: rustc_hash::FxHashSet<usize>,
    pub reachable_structs: rustc_hash::FxHashSet<StructTypeDescriptor>,
    pub compiled_modules: &'a ModuleManager,

    // The table.
    pub(crate) nodes: Vec<SerializableSteelVal>,
    // Where each value that can be told apart from its like has been put,
    // so that it is put there once.
    seen: rustc_hash::FxHashMap<(u8, usize), SerializedId>,
    // Values with a place in the table and nothing in it yet.
    waiting: Vec<(SerializedId, SteelVal)>,
}

impl<'a> SerializationContext<'a> {
    pub fn new(
        builtin_modules: &'a ModuleContainer,
        globals: &'a [SteelVal],
        symbol_map: &'a SymbolMap,
        constants: &'a ConstantMap,
        compiled_modules: &'a ModuleManager,
    ) -> Self {
        Self {
            builtin_modules,
            globals,
            symbol_map,
            constants,
            reachable_globals: rustc_hash::FxHashSet::default(),
            reachable_structs: rustc_hash::FxHashSet::default(),
            compiled_modules,
            nodes: Vec::new(),
            seen: rustc_hash::FxHashMap::default(),
            waiting: Vec::new(),
        }
    }

    /// Gives an entry that is already whole a place in the table.
    pub fn push(&mut self, node: SerializableSteelVal) -> Result<SerializedId> {
        let Ok(id) = u32::try_from(self.nodes.len()) else {
            stop!(Generic => "this value has too many parts to serialize");
        };
        self.nodes.push(node);
        Ok(SerializedId(id))
    }

    /// The place in the table of `value`, which is put down to be filled in
    /// if this is the first it has been seen.
    pub(crate) fn place(&mut self, value: SteelVal) -> Result<SerializedId> {
        let identity = identity(&value);
        if let Some(id) = identity.and_then(|identity| self.seen.get(&identity)) {
            return Ok(*id);
        }

        let id = self.push(SerializableSteelVal::Void)?;
        if let Some(identity) = identity {
            self.seen.insert(identity, id);
        }
        self.waiting.push((id, value));
        Ok(id)
    }

    fn place_all(
        &mut self,
        values: impl IntoIterator<Item = SteelVal>,
    ) -> Result<Vec<SerializedId>> {
        values.into_iter().map(|value| self.place(value)).collect()
    }
}

// What a value that can be referred to from more than one place is told
// apart by: its kind and where it lives. Two references to one of these are
// one entry in the table. A list is left out, since a list and its tail can
// live in the same place.
fn identity(value: &SteelVal) -> Option<(u8, usize)> {
    Some(match value {
        SteelVal::Closure(gc) => (0, gc.as_ptr() as usize),
        SteelVal::VectorV(vector) => (1, vector.0.as_ptr() as usize),
        SteelVal::HashMapV(map) => (2, map.0.as_ptr() as usize),
        SteelVal::HashSetV(set) => (3, set.0.as_ptr() as usize),
        SteelVal::CustomStruct(s) => (4, s.as_ptr() as usize),
        SteelVal::MutableStruct(s) => (5, s.as_ptr() as usize),
        SteelVal::StreamV(gc) => (6, gc.as_ptr() as usize),
        SteelVal::Pair(gc) => (7, gc.as_ptr() as usize),
        SteelVal::MutableVector(heap_ref) => (8, heap_ref.as_ptr_usize()),
        SteelVal::HeapAllocated(heap_ref) => (9, heap_ref.as_ptr_usize()),
        SteelVal::FlatVector(gc) => (10, gc.as_ptr() as usize),
        _ => return None,
    })
}

/// Puts `val` and everything it holds in the table, and gives its place.
pub fn into_serializable_value(
    val: SteelVal,
    ctx: &mut SerializationContext,
) -> Result<SerializedId> {
    let id = ctx.place(val)?;

    // This also takes whatever else is waiting, which is the same work
    // whoever put it down would have gone on to do.
    while let Some((id, value)) = ctx.waiting.pop() {
        let node = node_for(value, ctx)?;
        ctx.nodes[id.index()] = node;
    }

    Ok(id)
}

// The entry for `val`. What it holds is given a place and left waiting.
fn node_for(val: SteelVal, ctx: &mut SerializationContext) -> Result<SerializableSteelVal> {
    match val {
        SteelVal::Closure(c) => {
            closure_into_serializable(&c, ctx).map(SerializableSteelVal::Closure)
        }
        SteelVal::BoolV(b) => Ok(SerializableSteelVal::BoolV(b)),
        SteelVal::NumV(n) => Ok(SerializableSteelVal::NumV(n)),
        SteelVal::IntV(n) => Ok(SerializableSteelVal::IntV(n)),
        SteelVal::CharV(c) => Ok(SerializableSteelVal::CharV(c)),
        SteelVal::Void => Ok(SerializableSteelVal::Void),
        SteelVal::StringV(s) => Ok(SerializableSteelVal::StringV(s.to_string())),
        SteelVal::FuncV(_)
        | SteelVal::BuiltIn(_)
        | SteelVal::MutFunc(_)
        | SteelVal::BoxedFunction(_) => {
            // A native function is sent as where to find it: in a builtin
            // module, as one of a struct type's functions, or failing those
            // under the global that holds it.
            if let Some(spec) =
                crate::steel_vm::vm::threads::create_native_ref(ctx.builtin_modules, val.clone())
            {
                return Ok(SerializableSteelVal::NativeRef(spec));
            }

            if let SteelVal::BoxedFunction(_) = &val {
                if let Some(spec) = create_struct_spec(val.clone()) {
                    // Its struct type has to be sent with it.
                    ctx.reachable_structs.insert(spec.descriptor);
                    return Ok(SerializableSteelVal::StructConstructorSpec(spec));
                }
            }

            for (idx, v) in ctx.globals.iter().enumerate() {
                if v.ptr_eq(&val) {
                    let name = ctx.symbol_map.values()[idx];
                    return Ok(SerializableSteelVal::GlobalRef(name.resolve().to_string()));
                }
            }

            stop!(Generic => "a native function was found that can't be named to be sent: {}", val);
        }
        SteelVal::ListV(l) => Ok(SerializableSteelVal::ListV(ctx.place_all(l)?)),
        SteelVal::Pair(pair) => Ok(SerializableSteelVal::Pair(
            ctx.place(pair.car())?,
            ctx.place(pair.cdr())?,
        )),
        SteelVal::SymbolV(s) => Ok(SerializableSteelVal::SymbolV(s.to_string())),
        SteelVal::HashMapV(v) => {
            let mut entries = Vec::with_capacity(v.len());
            for (key, value) in v.iter() {
                entries.push((ctx.place(key.clone())?, ctx.place(value.clone())?));
            }
            Ok(SerializableSteelVal::HashMapV(entries))
        }

        SteelVal::Custom(c) => {
            let mut guard = c.write();

            if let Some(output) = guard.as_serializable_steelval() {
                Ok(output)
            } else
            // Check the tags that exist for serialization:
            if let Some(value) = guard.as_serializable_steelval_with_ctx(ctx) {
                value
            } else {
                stop!(Generic => "Custom type not allowed to be moved across threads!: {}", guard.name())
            }
        }

        SteelVal::CustomStruct(s) => {
            // Mark that we visited this, and that it will need to be
            // present on the other side.
            ctx.reachable_structs.insert(s.descriptor());

            Ok(SerializableSteelVal::CustomStruct(
                SerializableUserDefinedStruct {
                    fields: ctx.place_all(s.fields().iter().cloned())?,
                    type_descriptor: s.descriptor(),
                },
            ))
        }

        SteelVal::PortV(p) => SendablePort::from_port(p).map(SerializableSteelVal::Port),
        SteelVal::MutableStruct(s) => {
            ctx.reachable_structs.insert(s.descriptor());

            Ok(SerializableSteelVal::MutableStruct(
                SerializableUserDefinedStruct {
                    fields: ctx.place_all(s.fields())?,
                    type_descriptor: s.descriptor(),
                },
            ))
        }

        SteelVal::HeapAllocated(h) => Ok(SerializableSteelVal::HeapAllocated(ctx.place(h.get())?)),

        SteelVal::VectorV(vector) => Ok(SerializableSteelVal::VectorV(
            ctx.place_all(vector.iter().cloned())?,
        )),

        SteelVal::ByteVector(bytes) => {
            Ok(SerializableSteelVal::ByteVectorV(bytes.vec.read().to_vec()))
        }

        SteelVal::Rational(r) => Ok(SerializableSteelVal::Rational(r)),

        SteelVal::StreamV(s) => Ok(SerializableSteelVal::Stream(SerializableStream {
            initial_value: ctx.place(s.initial_value.clone())?,
            stream_thunk: ctx.place(s.stream_thunk.clone())?,
            empty_stream: s.empty_stream,
        })),

        SteelVal::HashSetV(s) => Ok(SerializableSteelVal::HashSet(
            ctx.place_all(s.iter().cloned())?,
        )),

        SteelVal::MutableVector(v) => {
            // Copied out, so the vector's lock isn't held for longer.
            let elements: Vec<SteelVal> = {
                let cell = v.strong_ptr();
                let guard = cell.lock();
                guard.value.iter().cloned().collect()
            };
            Ok(SerializableSteelVal::HeapAllocatedVector(
                ctx.place_all(elements)?,
            ))
        }

        SteelVal::FlatVector(v) => Ok(SerializableSteelVal::FlatVector(
            ctx.place_all(v.iter().cloned())?,
        )),
        SteelVal::BigNum(n) => Ok(SerializableSteelVal::BigNum(n.as_ref().clone())),
        SteelVal::BigRational(r) => Ok(SerializableSteelVal::BigRational(r.as_ref().clone())),
        SteelVal::Complex(c) => Ok(SerializableSteelVal::Complex(
            ctx.place(c.re.clone())?,
            ctx.place(c.im.clone())?,
        )),

        illegal => stop!(Generic => "Type not allowed to be moved across threads!: {}", illegal),
    }
}
