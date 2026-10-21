use super::*;

use crate::compiler::modules::{ModuleOrigins, ModuleRoot, MANGLER_PREFIX, MANGLER_SEPARATOR};
use crate::parser::interner::InternedString;
use crate::values::functions::SerializedLambda;
use crate::values::structs::{SendableVTableEntry, VTable};

/// A value in a serialized value's table, by its place there.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SerializedId(u32);

impl SerializedId {
    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Whether `id` names an entry of a table with `len` entries.
pub(crate) fn serialized_id_in(id: SerializedId, len: usize) -> bool {
    id.index() < len
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
    NativeRef(NativeRefSpec),
    StructConstructorSpec(StructConstructorRefSpec),
    ModuleSpec(String),
    GlobalRef(GlobalName),
    // The key the constructor and accessors of a mutable struct name its
    // type by.
    StructTypeKey(StructTypeDescriptor),
    BuiltinSteelModuleRef(String, String),
    NativeStruct(String, Vec<u8>),
    // These elements consed onto the list at the entry.
    ListOnto(Vec<SerializedId>, SerializedId),
    // The list at the entry without its first n elements.
    ListTail(SerializedId, u32),
}

impl SerializableSteelVal {
    /// Whether every entry this one names is in a table with `len` entries.
    pub(crate) fn is_within(&self, len: usize) -> bool {
        let mut within = true;
        let mut check = |id: SerializedId| within &= id.index() < len;
        for_each_held(self, &mut check);
        for_each_in_cell(self, &mut check);
        within
    }
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
            SerializableSteelVal::NativeRef(x) => write!(f, "{:?}", x),
            SerializableSteelVal::StructConstructorSpec(struct_constructor_ref_spec) => {
                write!(f, "{:?}", struct_constructor_ref_spec)
            }
            SerializableSteelVal::ModuleSpec(x) => write!(f, "#<module:{}>", x),
            SerializableSteelVal::GlobalRef(x) => {
                write!(f, "#<global:{}>", x.here(&ModuleOrigins::default()))
            }
            SerializableSteelVal::StructTypeKey(x) => write!(f, "#<struct-type-key:{:?}>", x),
            SerializableSteelVal::BuiltinSteelModuleRef(m, n) => {
                write!(f, "#<builtin-module:{}:{}>", m, n)
            }
            SerializableSteelVal::Port(sendable_port) => write!(f, "#<port:{:?}>", sendable_port),
            SerializableSteelVal::NativeStruct(n, _) => write!(f, "#<native:{}>", n),
            SerializableSteelVal::HeapAllocatedVector(x) => write!(f, "#<mutable-vector {:?}>", x),
            SerializableSteelVal::ListOnto(x, rest) => write!(f, "#<list {:?} onto {:?}>", x, rest),
            SerializableSteelVal::ListTail(list, skipped) => {
                write!(f, "#<tail {:?} of {:?}>", skipped, list)
            }
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

/// A number that tells this process from any other that writes serialized
/// values. Function ids, the numbers in the names of module members and the
/// names the compiler makes up only mean something in the process they come
/// from.
pub(crate) fn process_origin() -> u64 {
    use std::hash::{BuildHasher, Hasher};

    static ORIGIN: std::sync::OnceLock<u64> = std::sync::OnceLock::new();
    *ORIGIN.get_or_init(|| {
        // Seeded at random for each process.
        let mut hasher = std::collections::hash_map::RandomState::new().build_hasher();
        hasher.write_u32(std::process::id());
        hasher.finish()
    })
}

/// The name of a global in a form that means the same in every process.
///
/// A member of a module is named with the number its module's path was
/// interned as, which is whatever the interner had got to in the process
/// that compiled it. Here each such number is the path itself, and the name
/// this process has for the global is put back together from that.
///
/// The path is the sender's. The root the module was found under and its
/// relative path go with it, so a receiver with the module somewhere else
/// can find it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct GlobalName(Vec<NamePart>);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum NamePart {
    Text(String),
    // A module prefix, as the path of the module.
    Module {
        path: String,
        origin: Option<(ModuleRoot, String)>,
    },
}

impl GlobalName {
    /// Takes apart a name as it is in this process.
    pub fn new(name: &str, origins: &ModuleOrigins) -> Self {
        let mut parts = Vec::new();
        let mut text = String::new();
        let mut rest = name;

        while let Some(at) = rest.find(MANGLER_PREFIX) {
            let after = &rest[at + MANGLER_PREFIX.len()..];
            let digits = after.bytes().take_while(u8::is_ascii_digit).count();

            let path = after[digits..]
                .strip_prefix(MANGLER_SEPARATOR)
                .and_then(|_| after[..digits].parse::<usize>().ok())
                .and_then(|id| id.checked_sub(1))
                .and_then(<lasso::Spur as lasso::Key>::try_from_usize)
                .and_then(|key| crate::parser::interner::get_interner()?.try_resolve(&key));

            match path {
                Some(path) => {
                    text.push_str(&rest[..at]);
                    if !text.is_empty() {
                        parts.push(NamePart::Text(std::mem::take(&mut text)));
                    }
                    parts.push(NamePart::Module {
                        path: path.to_string(),
                        origin: origins
                            .by_prefix_path(path)
                            .map(|origin| (origin.root, origin.relative.clone())),
                    });
                    rest = &after[digits + MANGLER_SEPARATOR.len()..];
                }
                None => {
                    text.push_str(&rest[..at + MANGLER_PREFIX.len()]);
                    rest = after;
                }
            }
        }

        text.push_str(rest);
        if !text.is_empty() {
            parts.push(NamePart::Text(text));
        }

        GlobalName(parts)
    }

    /// The name as it is in this process.
    pub fn here(&self, origins: &ModuleOrigins) -> String {
        let mut name = String::new();
        for part in &self.0 {
            match part {
                NamePart::Text(text) => name.push_str(text),
                NamePart::Module { path, origin } => {
                    // Use the sender's path if this engine hasn't loaded
                    // the module.
                    let path = origin
                        .as_ref()
                        .and_then(|(root, relative)| origins.prefix_path_for(*root, relative))
                        .unwrap_or(path);

                    name.push_str(MANGLER_PREFIX);
                    name.push_str(
                        &InternedString::from_str(path)
                            .get()
                            .into_inner()
                            .to_string(),
                    );
                    name.push_str(MANGLER_SEPARATOR);
                }
            }
        }
        name
    }

    // The name as written with the sender's module path, for errors.
    fn described(&self) -> String {
        match self.0.as_slice() {
            [NamePart::Module { path, .. }, NamePart::Text(name)] => {
                format!("{}, defined in {} where it was sent from", name, path)
            }
            _ => self.here(&ModuleOrigins::default()),
        }
    }

    /// The path of the module and the name in it, for a member of a module.
    pub(crate) fn module_member(&self) -> Option<(&str, &str)> {
        match self.0.as_slice() {
            [NamePart::Module { path, .. }, NamePart::Text(name)] => Some((path, name)),
            _ => None,
        }
    }

    // Whether the name was made up, by the compiler for a function it
    // lifted out of another or by `gensym`. Such a name is numbered from a
    // counter, so two engines use the same one for different things.
    fn is_generated(&self) -> bool {
        self.0.iter().any(|part| match part {
            NamePart::Text(text) => {
                text.contains("##__lifted_pure_function")
                    || text.contains("##lambda-lifting")
                    || text.contains("-closure-lifting-")
                    || text.contains("##gensym")
            }
            NamePart::Module { .. } => false,
        })
    }

    /// Whether this is the global a struct definition keeps the key of a
    /// mutable struct type in.
    pub(crate) fn is_struct_type_key(&self) -> bool {
        matches!(self.0.last(), Some(NamePart::Text(text)) if text.contains("##struct-key##gensym"))
    }
}

/// A global a function that was sent refers to, as it is here.
#[derive(Clone, Copy)]
pub(crate) struct GlobalHere {
    pub(crate) index: usize,
    // Whether the function behind it is known to be the one the sender
    // compiled its calls against. Only then can a call that was compiled to
    // skip the arity check go on skipping it.
    pub(crate) as_sent: bool,
}

/// What reading a serialized value needs: the table, what has been made from
/// it so far, and where the pieces that aren't in the table are found.
pub struct HeapSerializer<'a> {
    pub nodes: &'a [SerializableSteelVal],
    // The value made from each entry, once it has been.
    built: Vec<Option<SteelVal>>,
    // Entries that making has started on and not finished.
    started: Vec<bool>,
    // Boxes, mutable vectors and mutable structs that exist and have not
    // been given their contents.
    unfilled: Vec<bool>,
    // Entries that hold, however far down, one of those that is part of a
    // cycle. They can't be hashed.
    cyclic: Vec<bool>,

    // Held for as long as values are being built. Nothing refers to what
    // has been allocated so far but this, so a collection in the meantime,
    // by this thread or another, would free it.
    pub heap: parking_lot::ArcMutexGuard<parking_lot::RawMutex, crate::values::closed::Heap>,

    // Cache the functions that get built
    pub built_functions: &'a mut rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,
    // The functions built so far, one per body, with the id each was sent
    // under.
    pub made_functions: Vec<(u32, Gc<ByteCodeLambda>)>,
    // The first closure built for each id. Later ones with that id share
    // its body.
    shared_bodies: rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,

    pub thread: &'a mut SteelThread,

    pub function_mapping: rustc_hash::FxHashMap<u32, u32>,

    pub(crate) global_mapping: rustc_hash::FxHashMap<usize, GlobalHere>,

    pub struct_map: rustc_hash::FxHashMap<StructTypeDescriptor, StructTypeDescriptor>,

    // Whether the value was written by this process.
    same_process: bool,
    origin: u64,
    // The globals the functions refer to, by the index the sender had for
    // them: their names, and their values where those were sent.
    global_names: &'a std::collections::HashMap<usize, GlobalName>,
    sent_globals: &'a std::collections::HashMap<usize, SerializedId>,
    // Globals that are not defined here, and have to be from what was sent:
    // the sender's index and the one given to them here.
    globals_to_define: Vec<(usize, usize)>,
}

impl<'a> HeapSerializer<'a> {
    pub fn new(
        nodes: &'a [SerializableSteelVal],
        heap: parking_lot::ArcMutexGuard<parking_lot::RawMutex, crate::values::closed::Heap>,
        built_functions: &'a mut rustc_hash::FxHashMap<u32, Gc<ByteCodeLambda>>,
        thread: &'a mut SteelThread,
        origin: u64,
        global_names: &'a std::collections::HashMap<usize, GlobalName>,
        sent_globals: &'a std::collections::HashMap<usize, SerializedId>,
    ) -> Self {
        Self {
            nodes,
            built: vec![None; nodes.len()],
            started: vec![false; nodes.len()],
            unfilled: vec![false; nodes.len()],
            cyclic: vec![false; nodes.len()],
            heap,
            built_functions,
            made_functions: Vec::new(),
            shared_bodies: rustc_hash::FxHashMap::default(),
            thread,
            function_mapping: rustc_hash::FxHashMap::default(),
            global_mapping: rustc_hash::FxHashMap::default(),
            struct_map: rustc_hash::FxHashMap::default(),
            same_process: origin == process_origin(),
            origin,
            global_names,
            sent_globals,
            globals_to_define: Vec::new(),
        }
    }

    /// The function built for `sent` by an earlier value. It has no captures.
    fn received_before(&self, sent: &SerializedLambda) -> Option<Gc<ByteCodeLambda>> {
        self.thread
            .received_functions
            .get(&(self.origin, sent.id))
            .filter(|(made, captures)| same_shape(made, sent) && *captures == sent.captures.len())
            .map(|(made, _)| made.clone())
    }

    /// The id here for the function sent as `sent`. The same within one value.
    ///
    /// Always a fresh id. Compiled code is cached per process by function id,
    /// and another engine or process has different globals and constants.
    pub(crate) fn function_id_here(&mut self, sent: u32) -> u32 {
        let here = self.fresh_function_id_for(sent);
        // Copy the span over for errors when the sender is in this process.
        if self.same_process {
            let spans = &mut self.thread.function_interner.spans;
            if let Some(known) = spans.get(&sent).cloned() {
                spans.entry(here).or_insert(known);
            }
        }
        here
    }

    /// A new id for the function that had `sent`, the same each time it
    /// comes up in this value.
    pub(crate) fn fresh_function_id_for(&mut self, sent: u32) -> u32 {
        *self
            .function_mapping
            .entry(sent)
            .or_insert_with(|| crate::compiler::code_gen::fresh_function_id() as u32)
    }

    /// The global here that a function refers to by the index `sent`.
    ///
    /// A global this engine has already is the one that is used. What was
    /// sent with a function is what it referred to where it came from, and
    /// binding that here would change the global under everything else that
    /// uses it. One this engine lacks is put down to be defined from what
    /// was sent.
    pub(crate) fn global_here(&mut self, sent: usize) -> Result<GlobalHere> {
        if let Some(here) = self.global_mapping.get(&sent) {
            return Ok(*here);
        }

        let Some(name) = self.global_names.get(&sent) else {
            stop!(Generic => "a function was sent without a global it refers to");
        };
        let value = match self.sent_globals.get(&sent) {
            Some(id) => Some(self.node(*id)?),
            None => None,
        };

        let here = if let Some(SerializableSteelVal::BuiltinSteelModuleRef(module, key)) = value {
            let name = GlobalName(vec![
                NamePart::Module {
                    path: module.clone(),
                    origin: None,
                },
                NamePart::Text(key.clone()),
            ]);
            let interned = InternedString::from_string(self.name_here(&name));
            let index = self.thread.compiler.read().symbol_map.get(&interned)?;
            GlobalHere {
                index,
                as_sent: false,
            }
        } else {
            let by_name = InternedString::from_string(self.name_here(name));
            let here = self.defined_global(&by_name);

            // What tells the global apart where it was written, if it is
            // one of the kinds that anything does.
            let identity = match value {
                Some(SerializableSteelVal::Closure(sent)) => Some(format!("fn{}", sent.id)),
                Some(SerializableSteelVal::StructTypeKey(sent)) => {
                    Some(format!("key{}", sent.key()))
                }
                _ => None,
            };
            // In one process a function id names one function and a key one
            // struct type.
            let same = self.same_process
                && match (value, here.as_ref().map(|(_, value)| value)) {
                    (Some(SerializableSteelVal::Closure(sent)), Some(SteelVal::Closure(here))) => {
                        sent.id == here.id
                    }
                    // The key of the struct type this engine reads the
                    // sender's as.
                    (
                        Some(SerializableSteelVal::StructTypeKey(sent)),
                        Some(SteelVal::IntV(here)),
                    ) => self
                        .struct_map
                        .get(sent)
                        .is_some_and(|mapped| mapped.key() as isize == *here),
                    _ => false,
                };

            // A made up name is no guide to what is behind it here. The
            // global of that name is used if it is known to be the same
            // thing, or within one process if there is no telling. Failing
            // that the global is kept under a name of its own.
            let made_up =
                name.is_generated() && !same && !(self.same_process && identity.is_none());

            if !made_up {
                match here {
                    // A global the sender brought earlier is the sender's,
                    // and takes what it has now.
                    Some((index, _))
                        if !same && value.is_some() && self.brought_by_sender(index, &by_name) =>
                    {
                        self.globals_to_define.push((sent, index));
                        GlobalHere {
                            index,
                            as_sent: false,
                        }
                    }
                    Some((index, _)) => GlobalHere {
                        index,
                        as_sent: same,
                    },
                    // It can be given again, so calls to it check.
                    None => GlobalHere {
                        as_sent: false,
                        ..self.global_to_define(sent, &by_name, value.is_some())?
                    },
                }
            } else {
                static APART: core::sync::atomic::AtomicUsize =
                    core::sync::atomic::AtomicUsize::new(0);

                let apart = InternedString::from_string(match identity {
                    Some(identity) => format!("##sent-{:016x}-{}", self.origin, identity),
                    None => format!(
                        "##sent-{:016x}-{}-{}",
                        self.origin,
                        APART.fetch_add(1, core::sync::atomic::Ordering::Relaxed),
                        by_name.resolve()
                    ),
                });
                // One that is here already was left by an earlier value
                // from the same process. A function is the same function
                // still. A key is given again, in case the struct type has
                // been defined anew here since.
                match self.defined_global(&apart) {
                    Some((index, _)) => {
                        if let Some(SerializableSteelVal::StructTypeKey(_)) = value {
                            self.globals_to_define.push((sent, index));
                        }
                        GlobalHere {
                            index,
                            as_sent: true,
                        }
                    }
                    None => self.global_to_define(sent, &apart, value.is_some())?,
                }
            }
        };

        self.global_mapping.insert(sent, here);
        Ok(here)
    }

    fn name_here(&self, name: &GlobalName) -> String {
        name.here(&self.thread.compiler.read().module_manager.origins)
    }

    // Whether the global at `index` was defined from what this sender sent
    // with an earlier function.
    fn brought_by_sender(&self, index: usize, name: &InternedString) -> bool {
        self.thread
            .received_globals
            .get(&index)
            .is_some_and(|(brought, origin)| brought == name && *origin == self.origin)
    }

    // The index and value of the global `name`, if this engine has it.
    fn defined_global(&self, name: &InternedString) -> Option<(usize, SteelVal)> {
        let index = self.thread.compiler.read().symbol_map.get(name).ok()?;
        Some((index, self.thread.extract_value(index)?))
    }

    // Puts `name` down to be defined from what was sent.
    fn global_to_define(
        &mut self,
        sent: usize,
        name: &InternedString,
        was_sent: bool,
    ) -> Result<GlobalHere> {
        if !was_sent {
            let described = match self.global_names.get(&sent) {
                Some(name) => name.described(),
                None => name.resolve().to_string(),
            };
            stop!(Generic => "a function refers to {}, which is not defined here and was not sent", described);
        }
        let index = self.thread.compiler.write().symbol_map.add(name);
        self.thread
            .received_globals
            .insert(index, (*name, self.origin));
        self.globals_to_define.push((sent, index));
        Ok(GlobalHere {
            index,
            as_sent: true,
        })
    }

    /// The next global that has to be defined from what was sent: where
    /// its value is in the table, and its index here.
    pub(crate) fn next_global_to_define(&mut self) -> Result<Option<(SerializedId, usize)>> {
        let Some((sent, index)) = self.globals_to_define.pop() else {
            return Ok(None);
        };
        match self.sent_globals.get(&sent) {
            Some(id) => Ok(Some((*id, index))),
            None => stop!(Generic => "a global was sent without its value"),
        }
    }

    /// Finds the struct type here for one that was sent, and checks that the
    /// two are laid out alike.
    pub(crate) fn map_struct_type(
        &mut self,
        entry: &SendableVTableEntry,
        here: StructTypeDescriptor,
    ) -> Result<()> {
        let name = entry.name.resolve();
        let Some(shape) = VTable::shape(here) else {
            stop!(Generic => "the struct type {} can't be used here", name);
        };

        if shape.mutable != entry.mutable {
            stop!(Generic => "the struct type {} is {} where it was sent from and {} here",
                name,
                if entry.mutable { "mutable" } else { "immutable" },
                if shape.mutable { "mutable" } else { "immutable" });
        }
        if let (Some(sent), Some(here)) = (entry.field_count, shape.field_count) {
            if sent != here {
                stop!(Generic => "the struct type {} has {} fields where it was sent from and {} here", name, sent, here);
            }
        }
        if let (Some(sent), Some(here)) = (&entry.fields, &shape.fields) {
            if sent != here {
                stop!(Generic => "the struct type {} has the fields {:?} where it was sent from and {:?} here", name, sent, here);
            }
        }

        // Two types the sender tells apart can't both be this one.
        if self
            .struct_map
            .iter()
            .any(|(sent, mapped)| *mapped == here && *sent != entry.desc)
        {
            stop!(Generic => "two struct types named {} were sent, and they can't be told apart here", name);
        }

        self.struct_map.insert(entry.desc, here);
        Ok(())
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

    // Whether the entry is, or holds, a box, mutable vector or mutable
    // struct that is part of a cycle.
    fn in_cycle(&self, id: SerializedId) -> bool {
        let index = id.index();
        self.cyclic.get(index).copied().unwrap_or(false)
            || self.unfilled.get(index).copied().unwrap_or(false)
    }

    // A key of a hash map or member of a hash set. Hashing a value that
    // holds itself would not end.
    fn built_key(&self, id: SerializedId) -> Result<SteelVal> {
        if self.in_cycle(id) {
            stop!(Generic => "a serialized hash map or set has a key that holds itself");
        }
        self.built(id)
    }

    // The struct type a struct that was sent belongs to here. The number it
    // was sent with is the sender's. The struct has to be of the kind the
    // type makes, with as many fields.
    fn struct_type_here(
        &self,
        record: &SerializableUserDefinedStruct,
        mutable: bool,
    ) -> Result<StructTypeDescriptor> {
        let Some(here) = self.struct_map.get(&record.type_descriptor).copied() else {
            stop!(Generic => "a struct was sent without its struct type");
        };
        let Some(shape) = VTable::shape(here) else {
            stop!(Generic => "a struct was sent whose struct type can't be used here");
        };
        if shape.mutable != mutable || shape.field_count.is_some_and(|n| n != record.fields.len()) {
            stop!(Generic => "a struct was sent that doesn't fit its struct type {}", here.name().resolve());
        }
        Ok(here)
    }
}

// What is left to do to make a value.
enum Step {
    // Make the entry, after what it holds.
    Visit(SerializedId),
    // Everything the entry holds has been made. Put it together.
    Finish(SerializedId),
    // The entry is a box, a mutable vector or a mutable struct, which is
    // made before what it holds. That has been made now. Put it in.
    Fill(SerializedId),
}

/// Makes the value the entry `id` names, and every value it holds.
///
/// A box, a mutable vector or a mutable struct is made empty and filled as
/// soon as what it holds has been made, so that a hash map made afterwards
/// with it in a key hashes what it will hold. A cycle is found by coming
/// back to an entry that is not finished. The box, vector or struct nearest
/// to that on the way down is then left empty and set aside, what was begun
/// beneath it is given up, and it is filled once everything else is made.
pub fn from_serializable_value(ctx: &mut HeapSerializer, id: SerializedId) -> Result<SteelVal> {
    ctx.node(id)?;

    let mut set_aside = Vec::new();
    let mut was_set_aside = rustc_hash::FxHashSet::default();
    let mut steps = vec![Step::Visit(id)];

    loop {
        let Some(step) = steps.pop() else {
            match set_aside.pop() {
                Some(id) => {
                    steps.push(Step::Fill(id));
                    for_each_in_cell(ctx.node(id)?, |held| steps.push(Step::Visit(held)));
                    continue;
                }
                None => break,
            }
        };

        match step {
            Step::Visit(id) => {
                let node = ctx.node(id)?;
                let index = id.index();

                if ctx.built[index].is_some() {
                    continue;
                }

                if ctx.started[index] {
                    // A cycle. Go back up to the nearest box, mutable
                    // vector or mutable struct and set it aside.
                    loop {
                        match steps.pop() {
                            Some(Step::Fill(cell)) if was_set_aside.insert(cell) => {
                                set_aside.push(cell);
                                break;
                            }
                            Some(Step::Finish(above)) if above != id => {
                                ctx.started[above.index()] = false;
                            }
                            Some(Step::Visit(_)) => {}
                            Some(Step::Finish(_)) | Some(Step::Fill(_)) | None => {
                                stop!(Generic => "a serialized value holds itself other than through a box, a mutable vector or a mutable struct");
                            }
                        }
                    }
                    continue;
                }

                match node {
                    SerializableSteelVal::HeapAllocated(_) => {
                        let allocation = ctx.heap.allocate_without_collection(SteelVal::Void);
                        ctx.built[index] = Some(SteelVal::HeapAllocated(allocation));
                    }
                    SerializableSteelVal::HeapAllocatedVector(elements) => {
                        let allocation =
                            ctx.heap
                                .allocate_vec_without_collection(HeapVec::with_capacity(
                                    elements.len(),
                                ));
                        ctx.built[index] = Some(SteelVal::MutableVector(allocation));
                    }
                    SerializableSteelVal::MutableStruct(record) => {
                        let type_descriptor = ctx.struct_type_here(record, true)?;
                        let handle = ctx.heap.allocate_mutable_struct_without_collection(
                            type_descriptor,
                            core::iter::repeat_n(SteelVal::Void, record.fields.len()),
                        );
                        ctx.built[index] = Some(SteelVal::MutableStruct(handle));
                    }

                    // Everything else is made after what it holds.
                    _ => {
                        ctx.started[index] = true;
                        steps.push(Step::Finish(id));
                        for_each_held(node, |held| steps.push(Step::Visit(held)));
                        continue;
                    }
                }

                ctx.unfilled[index] = true;
                steps.push(Step::Fill(id));
                for_each_in_cell(node, |held| steps.push(Step::Visit(held)));
            }

            Step::Finish(id) => {
                let node = ctx.node(id)?;
                let mut cyclic = false;
                for_each_held(node, |held| cyclic |= ctx.in_cycle(held));
                ctx.cyclic[id.index()] = cyclic;

                let value = finish(ctx, node)?;
                ctx.started[id.index()] = false;
                ctx.built[id.index()] = Some(value);
            }

            Step::Fill(id) => {
                let node = ctx.node(id)?;
                // What is still unfilled here is this entry or one that
                // holds it.
                let mut cyclic = false;
                for_each_in_cell(node, |held| cyclic |= ctx.in_cycle(held));
                ctx.cyclic[id.index()] = cyclic;

                match (node, ctx.built(id)?) {
                    (
                        SerializableSteelVal::HeapAllocated(contents),
                        SteelVal::HeapAllocated(cell),
                    ) => {
                        cell.set_and_return(ctx.built(*contents)?);
                    }
                    (
                        SerializableSteelVal::HeapAllocatedVector(elements),
                        SteelVal::MutableVector(cell),
                    ) => {
                        let elements = ctx.built_all(elements)?;
                        cell.strong_ptr().write().value.extend(elements);
                    }
                    (
                        SerializableSteelVal::MutableStruct(record),
                        SteelVal::MutableStruct(handle),
                    ) => {
                        for (index, field) in record.fields.iter().enumerate() {
                            handle.set_index(index, ctx.built(*field)?);
                        }
                    }
                    _ => unreachable!("only these are put down to be filled"),
                }
                ctx.unfilled[id.index()] = false;
            }
        }
    }

    ctx.built(id)
}

// Calls `visit` with each entry a box, mutable vector or mutable struct
// holds.
fn for_each_in_cell(node: &SerializableSteelVal, mut visit: impl FnMut(SerializedId)) {
    match node {
        SerializableSteelVal::HeapAllocated(contents) => visit(*contents),
        SerializableSteelVal::HeapAllocatedVector(ids) => ids.iter().copied().for_each(&mut visit),
        SerializableSteelVal::MutableStruct(record) => {
            record.fields.iter().copied().for_each(&mut visit)
        }
        _ => {}
    }
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
        SerializableSteelVal::ListOnto(ids, rest) => {
            ids.iter().copied().for_each(&mut visit);
            visit(*rest);
        }
        SerializableSteelVal::ListTail(list, _) => visit(*list),
        SerializableSteelVal::CustomStruct(record) => {
            record.fields.iter().copied().for_each(&mut visit)
        }
        _ => {}
    }
}

// Whether `sent` can reuse the body built for `made`.
fn same_shape(made: &ByteCodeLambda, sent: &SerializedLambda) -> bool {
    made.arity() == sent.arity
        && made.is_multi_arity() == sent.is_multi_arity
        && made.body_exp.len() == sent.body_exp.len()
}

// Puts together the value for `node`, whose contents have all been made.
fn finish(ctx: &mut HeapSerializer, node: &SerializableSteelVal) -> Result<SteelVal> {
    match node {
        SerializableSteelVal::Closure(c) => {
            // A function that closes over nothing is made once, however
            // many entries describe it.
            if c.captures.is_empty() {
                let id = ctx.function_id_here(c.id);
                if let Some(already_made) = ctx.built_functions.get(&id) {
                    return Ok(SteelVal::Closure(already_made.clone()));
                }
                if let Some(received_before) = ctx.received_before(c) {
                    c.refresh_globals(ctx)?;
                    ctx.built_functions.insert(id, received_before.clone());
                    return Ok(SteelVal::Closure(received_before));
                }
                let value = Gc::new(ByteCodeLambda::from_serialized(ctx, c)?);
                ctx.made_functions.push((c.id, value.clone()));
                ctx.built_functions.insert(id, value.clone());
                Ok(SteelVal::Closure(value))
            } else {
                let id = ctx.function_id_here(c.id);
                // Closures of one function share a body, so it is compiled
                // once.
                let first = ctx
                    .shared_bodies
                    .get(&id)
                    .filter(|first| {
                        same_shape(first, c) && first.captures().len() == c.captures.len()
                    })
                    .cloned()
                    .or_else(|| ctx.received_before(c));
                if let Some(first) = first {
                    c.validate()?;
                    c.refresh_globals(ctx)?;
                    let captures = c
                        .captures
                        .iter()
                        .map(|capture| ctx.built(*capture))
                        .collect::<Result<_>>()?;
                    let value = ByteCodeLambda::from_prototype(&first, captures);
                    return Ok(SteelVal::Closure(Gc::new(value)));
                }
                let value = Gc::new(ByteCodeLambda::from_serialized(ctx, c)?);
                ctx.made_functions.push((c.id, value.clone()));
                ctx.shared_bodies.insert(id, value.clone());
                Ok(SteelVal::Closure(value))
            }
        }
        SerializableSteelVal::BoolV(b) => Ok(SteelVal::BoolV(*b)),
        SerializableSteelVal::NumV(n) => Ok(SteelVal::NumV(*n)),
        SerializableSteelVal::IntV(i) => Ok(SteelVal::IntV(*i)),
        SerializableSteelVal::CharV(c) => Ok(SteelVal::CharV(*c)),
        SerializableSteelVal::Void => Ok(SteelVal::Void),
        SerializableSteelVal::Rational(r) => {
            // Arithmetic only produces rationals in lowest terms.
            if *r.denom() <= 1 || r.reduced().denom() != r.denom() {
                stop!(Generic => "a serialized rational is not in lowest terms");
            }
            Ok(SteelVal::Rational(*r))
        }
        SerializableSteelVal::StringV(s) => Ok(SteelVal::StringV(s.as_str().into())),
        SerializableSteelVal::HashMapV(entries) => {
            let mut map = HashMap::new();
            for (key, value) in entries {
                map.insert(ctx.built_key(*key)?, ctx.built(*value)?);
            }
            Ok(SteelVal::HashMapV(Gc::new(map).into()))
        }
        SerializableSteelVal::HashSet(ids) => {
            let mut set = HashSet::new();
            for id in ids {
                set.insert(ctx.built_key(*id)?);
            }
            Ok(SteelVal::HashSetV(Gc::new(set).into()))
        }
        SerializableSteelVal::ListV(ids) => {
            Ok(SteelVal::ListV(ctx.built_all(ids)?.into_iter().collect()))
        }
        SerializableSteelVal::ListOnto(ids, rest) => {
            let SteelVal::ListV(mut list) = ctx.built(*rest)? else {
                stop!(Generic => "a serialized list goes on into something that is not a list");
            };
            // The table also holds the tail, so cons onto it without
            // copying.
            for element in ctx.built_all(ids)?.into_iter().rev() {
                list.cons_mut(element);
            }
            Ok(SteelVal::ListV(list))
        }
        SerializableSteelVal::ListTail(list, skipped) => {
            let SteelVal::ListV(list) = ctx.built(*list)? else {
                stop!(Generic => "a serialized tail is of something that is not a list");
            };
            match list.tail(*skipped as usize) {
                Some(tail) => Ok(SteelVal::ListV(tail)),
                None => stop!(Generic => "a serialized tail is of a list too short to have it"),
            }
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
        // As arithmetic would give them. One that fits is not a big number.
        SerializableSteelVal::BigNum(n) => n.clone().into_steelval(),
        SerializableSteelVal::BigRational(r) => {
            if num_traits::Zero::is_zero(r.denom()) {
                stop!(Generic => "a serialized rational has a denominator of zero");
            }
            num_rational::BigRational::new(r.numer().clone(), r.denom().clone()).into_steelval()
        }
        SerializableSteelVal::Complex(re, im) => {
            let (re, im) = (ctx.built(*re)?, ctx.built(*im)?);
            let real = |part: &SteelVal| {
                matches!(
                    part,
                    SteelVal::IntV(_)
                        | SteelVal::NumV(_)
                        | SteelVal::Rational(_)
                        | SteelVal::BigNum(_)
                        | SteelVal::BigRational(_)
                )
            };
            if !real(&re) || !real(&im) {
                stop!(Generic => "a serialized complex number has a part that is not a real number");
            }
            Ok(SteelVal::Complex(Gc::new(SteelComplex { re, im })))
        }
        SerializableSteelVal::CustomStruct(s) => {
            let type_descriptor = ctx.struct_type_here(s, false)?;
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
        SerializableSteelVal::NativeRef(s) => {
            let guard = ctx.thread.compiler.read();
            let module_map = guard.builtin_modules.inner();

            match module_map.get(s.module.as_str()) {
                Some(module) => match module.try_get_ref(s.key.as_str()) {
                    Some(value) => Ok(value),
                    None => stop!(Generic => "the module {} has no {} here", s.module, s.key),
                },
                None => stop!(Generic => "no module {} here to find {} in", s.module, s.key),
            }
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
            let interned = InternedString::from_string(ctx.name_here(s));
            // Find the index of this thing:
            let idx = ctx.thread.compiler.read().symbol_map.get(&interned)?;
            match ctx.thread.global_env.roots().get(idx) {
                Some(value) => Ok(value.clone()),
                None => stop!(Generic => "a global was sent that is not defined here"),
            }
        }
        SerializableSteelVal::StructTypeKey(sent) => match ctx.struct_map.get(sent) {
            Some(here) => Ok(SteelVal::IntV(here.key() as isize)),
            None => stop!(Generic => "the key of a struct type was sent without its struct type"),
        },
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
    seen: rustc_hash::FxHashMap<(u8, usize, usize), SerializedId>,
    // Those values, kept so that the place one lives in is not given to
    // another while the table is being made.
    kept: Vec<SteelVal>,
    // Values with a place in the table and nothing in it yet.
    waiting: Vec<(SerializedId, SteelVal)>,
    // The table entry of each list.
    lists: rustc_hash::FxHashMap<ListPlace, SerializedId>,
    // The longest list written from each run, and how many of the run's
    // elements it has.
    longest: rustc_hash::FxHashMap<(usize, usize), (usize, SerializedId)>,
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
            kept: Vec::new(),
            waiting: Vec::new(),
            lists: rustc_hash::FxHashMap::default(),
            longest: rustc_hash::FxHashMap::default(),
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

    /// Drops every entry from `len` on, as if they had never been made.
    pub(crate) fn rewind(&mut self, len: usize) {
        self.nodes.truncate(len);
        self.seen.retain(|_, id| id.index() < len);
        self.lists.retain(|_, id| id.index() < len);
        self.longest.retain(|_, (_, id)| id.index() < len);
        self.waiting.retain(|(id, _)| id.index() < len);
    }

    /// The place in the table of `value`, which is put down to be filled in
    /// if this is the first it has been seen.
    pub(crate) fn place(&mut self, value: SteelVal) -> Result<SerializedId> {
        if let Some(place) = list_place(&value) {
            if let Some(id) = self.lists.get(&place) {
                return Ok(*id);
            }
            let id = self.push(SerializableSteelVal::Void)?;
            self.lists.insert(place, id);
            self.kept.push(value.clone());
            self.waiting.push((id, value));
            return Ok(id);
        }

        let identity = identity(&value);
        if let Some(id) = identity.and_then(|identity| self.seen.get(&identity)) {
            return Ok(*id);
        }

        let id = self.push(SerializableSteelVal::Void)?;
        if let Some(identity) = identity {
            self.seen.insert(identity, id);
            self.kept.push(value.clone());
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

// The kind and address of a value that can be shared. Two references to
// the same one are one entry in the table.
fn identity(value: &SteelVal) -> Option<(u8, usize, usize)> {
    let (kind, address) = match value {
        SteelVal::Closure(gc) => (0, gc.as_ptr() as usize),
        SteelVal::VectorV(vector) => (1, vector.0.as_ptr() as usize),
        SteelVal::HashMapV(map) => (2, map.0.as_ptr() as usize),
        SteelVal::HashSetV(set) => (3, set.0.as_ptr() as usize),
        SteelVal::CustomStruct(s) => (4, s.as_ptr() as usize),
        SteelVal::MutableStruct(s) => (5, s.as_ptr() as usize),
        SteelVal::Pair(gc) => (7, gc.as_ptr() as usize),
        SteelVal::MutableVector(heap_ref) => (8, heap_ref.as_ptr_usize()),
        SteelVal::HeapAllocated(heap_ref) => (9, heap_ref.as_ptr_usize()),
        SteelVal::FlatVector(gc) => (10, gc.as_ptr() as usize),
        SteelVal::ByteVector(bytes) => (11, bytes.vec.as_ptr() as usize),
        _ => return None,
    };
    Some((kind, address, 0))
}

// Identifies a list. A list keeps its elements in runs, each followed by the
// rest of the list. Lists that share a tail share its runs, and a list and
// its tail can start in the same run. So a list is its first run, how many
// of that run's elements it has, and what follows the run.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ListPlace {
    run: usize,
    rest: usize,
    count: usize,
}

fn list_place(value: &SteelVal) -> Option<ListPlace> {
    match value {
        SteelVal::ListV(list) if !list.is_empty() => {
            let (run, count) = list.identity_tuple();
            Some(ListPlace {
                run,
                rest: list.next_ptr_as_usize().unwrap_or(0),
                count,
            })
        }
        _ => None,
    }
}

// The entry for a list. Only its first run is written. What follows the run
// is its own entry, shared by every list with that tail. A list in a run
// that a longer list was written from is a tail of it, and a longer list
// adds its elements onto the previous one.
fn list_node(
    id: SerializedId,
    list: crate::values::lists::List<SteelVal>,
    ctx: &mut SerializationContext,
) -> Result<SerializableSteelVal> {
    let Some(place) = list_place(&SteelVal::ListV(list.clone())) else {
        return Ok(SerializableSteelVal::ListV(Vec::new()));
    };
    let run = (place.run, place.rest);
    let longest = ctx.longest.get(&run).copied();

    let (written, rest) = match longest {
        Some((count, longer)) if place.count <= count => {
            let Ok(skipped) = u32::try_from(count - place.count) else {
                stop!(Generic => "this list is too long to serialize");
            };
            return Ok(SerializableSteelVal::ListTail(longer, skipped));
        }
        Some((count, shorter)) => (count, Some(shorter)),
        None if place.rest == 0 => (0, None),
        // The cdr of the run's last cell is the following list as is.
        // `tail(count)` would copy its first cell.
        None => match list.tail(place.count - 1).and_then(|last| last.cdr()) {
            Some(rest) => (0, Some(ctx.place(SteelVal::ListV(rest))?)),
            None => (0, None),
        },
    };
    ctx.longest.insert(run, (place.count, id));

    let elements = list
        .current_node_iter()
        .take(place.count - written)
        .cloned()
        .collect::<Vec<_>>();
    let elements = ctx.place_all(elements)?;
    Ok(match rest {
        Some(rest) => SerializableSteelVal::ListOnto(elements, rest),
        None => SerializableSteelVal::ListV(elements),
    })
}

/// Puts `val` and everything it holds in the table, and gives its place.
pub fn into_serializable_value(
    val: SteelVal,
    ctx: &mut SerializationContext,
) -> Result<SerializedId> {
    let id = ctx.place(val)?;

    // This also takes whatever else is waiting. Lists are left until nothing
    // else is waiting and then taken longest first, so a list is written
    // before its tails when possible.
    let mut lists = Vec::new();
    loop {
        while let Some((id, value)) = ctx.waiting.pop() {
            match value {
                SteelVal::ListV(list) if !list.is_empty() => {
                    lists.push((list.cursor_index(), id, list));
                }
                value => {
                    let node = node_for(id, value, ctx)?;
                    ctx.nodes[id.index()] = node;
                }
            }
        }
        if lists.is_empty() {
            break;
        }
        lists.sort_by_key(|(count, ..)| core::cmp::Reverse(*count));
        for (_, id, list) in lists.drain(..) {
            let node = list_node(id, list, ctx)?;
            ctx.nodes[id.index()] = node;
        }
    }

    Ok(id)
}

// The entry for `val`. What it holds is given a place and left waiting.
fn node_for(
    id: SerializedId,
    val: SteelVal,
    ctx: &mut SerializationContext,
) -> Result<SerializableSteelVal> {
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
                    return Ok(SerializableSteelVal::GlobalRef(GlobalName::new(
                        name.resolve(),
                        ctx.compiled_modules.origins(),
                    )));
                }
            }

            stop!(Generic => "a native function was found that can't be named to be sent: {}", val);
        }
        SteelVal::ListV(l) => list_node(id, l, ctx),
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
