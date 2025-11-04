#![allow(unused)]
#![allow(clippy::type_complexity)]

use crate::collections::{HashMap, MutableHashMap, MutableHashSet};
use crate::steel_vm::primitives::{steel_unbox_mutable, unbox_mutable};
#[cfg(feature = "sync")]
use crate::sync::RwLock;
use alloc::{
    boxed::Box, collections::VecDeque, format, rc::Rc, string::String, string::ToString, sync::Arc,
    vec, vec::Vec,
};
use once_cell::sync::Lazy;
use smallvec::SmallVec;

use crate::compiler::map::SymbolMap;
use crate::parser::interner::InternedString;
use crate::rerrs::ErrorKind;
use crate::rvals::{
    from_serializable_value, into_serializable_value, Custom, HeapSerializer, SerializableSteelVal,
    SerializedHeapRef, SteelHashMap,
};
use crate::rvals::{FromSteelVal, IntoSteelVal};
use crate::steel_vm::register_fn::RegisterFn;
use crate::throw;
use crate::{
    gc::Gc,
    rvals::{AsRefSteelVal, SRef, SteelString},
};
use crate::{
    rvals::{Result, SteelVal},
    SteelErr,
};
use crate::{steel_vm::builtin::BuiltInModule, stop};
use core::{
    cell::{Ref, RefCell},
    hash::Hash,
    ops::Deref,
};
use fxhash::FxBuildHasher;

use super::closed::Heap;
use super::functions::BoxedDynFunction;
use super::lists::List;
use super::recycler::Recycle;

enum StringOrMagicNumber {
    String(Rc<String>),
    Magic(usize),
}

// #[derive(Debug)]
pub struct VTableEntry {
    pub(crate) name: InternedString,
    pub(crate) properties: Gc<HashMap<SteelVal, SteelVal>>,
    pub(crate) proc: Option<usize>,
    pub(crate) transparent: bool,
    pub(crate) mutable: bool,
}

pub(crate) struct SendableVTableEntry {
    pub(crate) name: InternedString,
    pub(crate) properties: Vec<(SerializableSteelVal, SerializableSteelVal)>,
    pub(crate) proc: Option<usize>,
    pub(crate) transparent: bool,
    pub(crate) mutable: bool,
}

impl VTableEntry {
    pub fn new(name: InternedString, proc: Option<usize>) -> Self {
        Self {
            name,
            proc,
            properties: DEFAULT_PROPERTIES.with(|x| x.clone()),
            transparent: false,
            mutable: false,
        }
    }
}

// If they're built in, we want to package the values alongside the
#[derive(Debug, Clone)]
pub enum Properties {
    BuiltIn,
    Local(Gc<HashMap<SteelVal, SteelVal>>),
}

impl Properties {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Local(l), Self::Local(r)) => Gc::ptr_eq(l, r),
            (Self::BuiltIn, Self::BuiltIn) => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
// Wrap the usize, store this and this only. We use this as an index into the VTable.
pub struct StructTypeDescriptor(usize);

impl Custom for StructTypeDescriptor {
    fn into_serializable_steelval(&mut self) -> Option<SerializableSteelVal> {
        Some(SerializableSteelVal::Custom(Box::new(*self)))
    }
}

impl StructTypeDescriptor {
    #[cfg(not(feature = "sync"))]
    fn name(&self) -> InternedString {
        VTABLE.with(|x| x.borrow().entries[self.0].name)
    }

    #[cfg(feature = "sync")]
    fn name(&self) -> InternedString {
        STATIC_VTABLE.read().entries[self.0].name
    }

    // TODO: Use inline reference to avoid reference count when getting the fields
    #[cfg(not(feature = "sync"))]
    fn fields(&self) -> SteelVal {
        FIELDS_KEY.with(|key| {
            VTABLE.with(|x| {
                x.borrow().entries[self.0]
                    .properties
                    .get(key)
                    .cloned()
                    .expect("#:fields entry missing on struct properties")
            })
        })
    }

    #[cfg(feature = "sync")]
    fn fields(&self) -> SteelVal {
        STATIC_VTABLE.read().entries[self.0]
            .properties
            .get(&STATIC_FIELDS_KEY)
            .cloned()
            .expect("#:fields entry missing on struct properties")
    }
}

pub struct SerializableUserDefinedStruct {
    pub(crate) fields: Vec<SerializableSteelVal>,

    pub(crate) type_descriptor: StructTypeDescriptor,
}

#[derive(Clone, Debug)]
pub struct UserDefinedStruct {
    // pub(crate) fields: Recycle<Vec<SteelVal>>,
    pub(crate) fields: Recycle<SmallVec<[SteelVal; 4]>>,
    // pub(crate) fields: SmallVec<[SteelVal; 4]>,

    // Type Descriptor. Use this as an index into the VTable to find anything that we need.
    pub(crate) type_descriptor: StructTypeDescriptor,
}

impl UserDefinedStruct {
    pub fn name(&self) -> InternedString {
        self.type_descriptor.name()
    }

    pub fn is_transparent(&self) -> bool {
        self.get(&TRANSPARENT_KEY.with(|x| x.clone()))
            .and_then(|x| x.as_bool())
            .unwrap_or_default()
    }

    pub fn get_index(&self, index: usize) -> Option<&SteelVal> {
        self.fields.get(index)
    }

    pub fn get_mut_index(&self, index: usize) -> Option<SteelVal> {
        self.fields
            .get(index)
            .cloned()
            .map(|x| steel_unbox_mutable(&[x]).unwrap())
    }

    // This only works if the underlying value is a box
    pub fn set_index(&self, index: usize, value: SteelVal) {
        let inner = self.fields.get(index);

        if let Some(SteelVal::HeapAllocated(s)) = inner {
            s.set_and_return(value);
        }
    }
}

// TODO: This could blow the stack for big trees...
impl PartialEq for UserDefinedStruct {
    fn eq(&self, other: &Self) -> bool {
        self.type_descriptor == other.type_descriptor && self.fields.deref() == other.fields.deref()
    }
}

impl Hash for UserDefinedStruct {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.type_descriptor.hash(state);
        self.fields.deref().hash(state);
    }
}

impl core::fmt::Display for UserDefinedStruct {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self
            .get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
            .is_some()
        {
            write!(f, "({}", self.type_descriptor.name())?;
            // for i in 0..self.len - 1 {
            //     write!(f, " {}", self.fields[i])?;
            // }

            for i in self.fields.iter() {
                write!(f, " {}", i)?;
            }

            write!(f, ")")
        } else {
            write!(f, "({})", self.name())
        }
    }
}

impl UserDefinedStruct {
    fn new(type_descriptor: StructTypeDescriptor, raw_fields: &[SteelVal]) -> Self {
        // let mut fields: Recycle<Vec<_>> = Recycle::new();
        let mut fields: Recycle<SmallVec<[SteelVal; 4]>> = Recycle::new();
        // fields.extend_from_slice(raw_fields);
        fields.extend(raw_fields.iter().cloned());

        // let fields = raw_fields.into_iter().cloned().collect();

        Self {
            fields,
            type_descriptor,
        }
    }

    #[cfg(not(feature = "sync"))]
    pub(crate) fn get(&self, val: &SteelVal) -> Option<SteelVal> {
        VTABLE.with(|x| {
            x.borrow().entries[self.type_descriptor.0]
                .properties
                .get(val)
                .cloned()
        })
    }

    #[cfg(feature = "sync")]
    pub(crate) fn get(&self, val: &SteelVal) -> Option<SteelVal> {
        STATIC_VTABLE.read().entries[self.type_descriptor.0]
            .properties
            .get(val)
            .cloned()
    }

    #[inline(always)]
    fn new_ok<T: IntoSteelVal>(value: T) -> Result<SteelVal> {
        if cfg!(feature = "sync") {
            UserDefinedStruct::constructor_thunk(1, *STATIC_OK_DESCRIPTOR)(
                &[value.into_steelval()?],
            )
        } else {
            OK_CONSTRUCTOR.with(|x| x(&[value.into_steelval()?]))
        }
    }

    #[inline(always)]
    fn new_err<T: IntoSteelVal>(value: T) -> Result<SteelVal> {
        if cfg!(feature = "sync") {
            UserDefinedStruct::constructor_thunk(1, *STATIC_ERR_DESCRIPTOR)(&[
                value.into_steelval()?
            ])
        } else {
            ERR_CONSTRUCTOR.with(|x| x(&[value.into_steelval()?]))
        }
    }

    // TODO: This doesn't particularly play nice with dynamic libraries. Should probably just assign some IDs
    // to these structs and use them that way
    #[inline(always)]
    fn is_ok(&self) -> bool {
        self.type_descriptor.name() == *OK_RESULT_LABEL
    }

    #[inline(always)]
    fn is_err(&self) -> bool {
        self.type_descriptor.name() == *ERR_RESULT_LABEL
    }

    #[cfg(not(feature = "sync"))]
    pub(crate) fn maybe_proc(&self) -> Option<&SteelVal> {
        VTABLE.with(|x| {
            x.borrow().entries[self.type_descriptor.0]
                .proc
                .as_ref()
                .map(|s| &self.fields[*s])
        })
    }

    #[cfg(feature = "sync")]
    pub(crate) fn maybe_proc(&self) -> Option<&SteelVal> {
        STATIC_VTABLE.read().entries[self.type_descriptor.0]
            .proc
            .as_ref()
            .map(|s| &self.fields[*s])
    }

    fn new_with_options(
        properties: Properties,
        type_descriptor: StructTypeDescriptor,
        rest: &[SteelVal],
    ) -> Self {
        // let mut fields: Recycle<Vec<_>> = Recycle::new_with_capacity(rest.len());
        // fields.extend_from_slice(rest);

        let mut fields: Recycle<SmallVec<[_; 4]>> = Recycle::new_with_capacity(rest.len());
        fields.extend(rest.iter().cloned());

        // let fields = rest.into_iter().cloned().collect();

        Self {
            fields,
            type_descriptor,
        }
    }

    fn constructor_thunk(
        len: usize,
        descriptor: StructTypeDescriptor,
    ) -> impl Fn(&[SteelVal]) -> Result<SteelVal> {
        move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != len {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    descriptor.name(),
                    len,
                    args.len(),
                );
                stop!(ArityMismatch => error_message);
            }

            let new_struct =
                UserDefinedStruct::new_with_options(Properties::BuiltIn, descriptor, args);

            Ok(SteelVal::CustomStruct(Gc::new(new_struct)))
        }
    }

    fn constructor_with_options(len: usize, descriptor: StructTypeDescriptor) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != len {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    descriptor.name().clone(),
                    len,
                    args.len(),
                );
                stop!(ArityMismatch => error_message);
            }

            let new_struct =
                UserDefinedStruct::new_with_options(Properties::BuiltIn, descriptor, args);

            Ok(SteelVal::CustomStruct(Gc::new(new_struct)))
        };

        SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            Some(Arc::new(String::from(descriptor.name().resolve()))),
            Some(len as _),
        )))
    }

    fn constructor(
        name: InternedString,
        len: usize,
        type_descriptor: StructTypeDescriptor,
    ) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != len {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    name.clone(),
                    len,
                    args.len(),
                );
                stop!(ArityMismatch => error_message);
            }

            // Definitely use interned symbols for these. Otherwise we're going to be doing A LOT of
            // arc cloning, and we don't want that.
            let new_struct = UserDefinedStruct::new(type_descriptor, args);

            Ok(SteelVal::CustomStruct(Gc::new(new_struct)))
        };

        SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            Some(Arc::new(String::from(name.resolve()))),
            Some(len as _),
        )))
    }

    fn predicate(descriptor: StructTypeDescriptor) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                let error_message = format!(
                    "{}? expected one argument, found {}",
                    descriptor.name(),
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }
            Ok(SteelVal::BoolV(match &args[0] {
                SteelVal::CustomStruct(my_struct) if my_struct.type_descriptor == descriptor => {
                    true
                }
                _ => false,
            }))
        };

        SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            Some(Arc::new(String::from(descriptor.name().resolve()))),
            Some(1),
        )))
    }

    fn getter_prototype(descriptor: StructTypeDescriptor) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected two arguments", descriptor.name()));
            }

            let steel_struct = &args[0];
            let idx = &args[1];

            match (steel_struct, idx) {
                (SteelVal::CustomStruct(s), SteelVal::IntV(idx)) => {
                    if s.type_descriptor != descriptor {
                        stop!(TypeMismatch => format!("Struct getter expected {}, found {:?}, {:?}", descriptor.name(), &s, &steel_struct));
                    }

                    if *idx < 0 {
                        stop!(Generic => "struct-ref expected a non negative index");
                    }

                    s.fields
                        .get(*idx as usize)
                        .cloned()
                        .ok_or_else(throw!(Generic => "struct-ref: index out of bounds"))
                }
                _ => {
                    let error_message = format!(
                        "{} accessor expected a struct and an int, found: {} and {}",
                        descriptor.name(),
                        steel_struct,
                        idx
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            Some(Arc::new(String::from(descriptor.name().resolve()))),
            Some(2),
        )))
    }

    fn getter_prototype_index(descriptor: StructTypeDescriptor, index: usize) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "struct-ref expected one argument");
            }

            let steel_struct = &args[0];

            match &steel_struct {
                SteelVal::CustomStruct(s) => {
                    if s.type_descriptor != descriptor {
                        stop!(TypeMismatch => format!("Struct getter expected {}, found {:?}, {:?}", descriptor.name(), &s, &steel_struct));
                    }

                    s.fields
                        .get(index)
                        .cloned()
                        .ok_or_else(throw!(Generic => "struct-ref: index out of bounds"))
                }
                _ => {
                    let error_message = format!(
                        "{} expected a struct and an int, found: {} and {}",
                        descriptor.name(),
                        steel_struct,
                        index
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            Some(Arc::new(String::from(descriptor.name().resolve()))),
            Some(1),
        )))
    }
}

// Update the given struct in place, without having to allocate a new one
// This in practice should yield some nice performance
pub fn struct_update_primitive(args: &mut [SteelVal]) -> Result<SteelVal> {
    if let Some((SteelVal::CustomStruct(s), fields)) = args.split_first_mut() {
        let mut fields = fields.iter_mut();

        let struct_fields = s.type_descriptor.fields();

        let struct_fields_list = struct_fields
            .list()
            .ok_or_else(throw!(TypeMismatch => "struct fields are not a list!"))?;

        // Fast path, we have less than 5 pairs of keys to change. Past that
        // we'll have to use some heap allocations
        let mut fields_to_update = smallvec::SmallVec::<[(usize, &mut SteelVal); 5]>::new();

        match Gc::get_mut(s) {
            Some(s) => {
                populate_fields_offsets(fields, struct_fields_list, &mut fields_to_update)?;

                for (idx, value) in fields_to_update {
                    core::mem::swap(&mut s.fields[idx], value);
                }

                Ok(core::mem::replace(&mut args[0], SteelVal::Void))
            }

            None => {
                let mut s = s.unwrap();
                populate_fields_offsets(fields, struct_fields_list, &mut fields_to_update)?;
                for (idx, value) in fields_to_update {
                    core::mem::swap(&mut s.fields[idx], value);
                }

                Ok(SteelVal::CustomStruct(Gc::new(s)))
            }
        }
    } else {
        stop!(TypeMismatch => "struct-copy expects a struct in the first position, found: {:?}", args);
    }
}

fn populate_fields_offsets<'a>(
    mut fields: core::slice::IterMut<'a, SteelVal>,
    struct_fields_list: &List<SteelVal>,
    fields_to_update: &mut smallvec::SmallVec<[(usize, &'a mut SteelVal); 5]>,
) -> Result<()> {
    loop {
        match (fields.next(), fields.next()) {
            (Some(key), Some(value)) => {
                // check all of the struct offsets first, otherwise roll back the applied changes to the struct?
                let struct_offset = struct_fields_list
                    .iter()
                    .enumerate()
                    .find(|(index, element)| *element == key)
                    .ok_or_else(throw!(Generic => "field not found on struct: {}", key))?;

                fields_to_update.push((struct_offset.0, value));
            }
            (None, None) => break,
            _ => {
                stop!(ArityMismatch => "struct-update must have a value for every key!");
            }
        }
    }

    Ok(())
}

pub fn make_struct_type(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "make-struct-type expects 2 args, found: {}", args.len())
    }

    // Convert the string into an Arc'd string - this now makes the generated functions
    // thread safe.
    let name = match &args[0] {
        SteelVal::SymbolV(name) => name.clone(),
        SteelVal::ListV(list) => {
            let mut iter = list.iter();
            match (iter.next(), iter.next(), iter.next()) {
                (Some(SteelVal::SymbolV(sym)), Some(SteelVal::SymbolV(name)), None)
                    if sym.as_str() == "quote" =>
                {
                    name.clone()
                }
                _ => stop!(TypeMismatch => format!(
                    "make-struct-type expected a symbol for the name, found: {}",
                    &args[0]
                )),
            }
        }
        _ => stop!(TypeMismatch => format!(
            "make-struct-type expected a symbol for the name, found: {}",
            &args[0]
        )),
    };

    let SteelVal::IntV(field_count) = &args[1] else {
        stop!(TypeMismatch => format!("make-struct-type expected an integer for the field count, found: {}", &args[0]));
    };

    let (
        struct_type_descriptor,
        struct_constructor,
        struct_predicate,
        getter_prototype,
        getter_prototypes,
    ) = make_struct_type_inner(name.as_str(), *field_count as usize);

    Ok(SteelVal::ListV(
        vec![
            // Convert this into a descriptor before we're done
            struct_type_descriptor.into_steelval().unwrap(),
            struct_constructor,
            struct_predicate,
            getter_prototype,
            getter_prototypes,
            // struct_type_descriptor,
        ]
        .into(),
    ))
}

pub fn make_struct_singleton(name: &str) -> (SteelVal, StructTypeDescriptor) {
    let (descriptor, _, _, _, _) = make_struct_type_inner(name, 0);

    let instance = UserDefinedStruct::new(descriptor, &[]);

    (SteelVal::CustomStruct(Gc::new(instance)), descriptor)
}

fn make_struct_type_inner(
    name: &str,
    field_count: usize,
) -> (StructTypeDescriptor, SteelVal, SteelVal, SteelVal, SteelVal) {
    let name = InternedString::from(name);

    // Make a slot in the VTable for this struct
    let struct_type_descriptor = VTable::new_entry(name, None);

    // Build out the constructor and the predicate
    let struct_constructor =
        UserDefinedStruct::constructor(name, field_count, struct_type_descriptor);
    let struct_predicate = UserDefinedStruct::predicate(struct_type_descriptor);

    let getter_prototype = UserDefinedStruct::getter_prototype(struct_type_descriptor);

    let mut getter_prototypes = Vec::new();

    for i in 0..field_count {
        getter_prototypes.push(UserDefinedStruct::getter_prototype_index(
            struct_type_descriptor,
            i,
        ));
    }

    (
        struct_type_descriptor,
        struct_constructor,
        struct_predicate,
        getter_prototype,
        getter_prototypes.into_steelval().unwrap(),
    )
}

// Implement internal thing here?
struct SteelTrait {
    name: InternedString,
    method_names: Vec<InternedString>,
}

struct SteelTraitImplementation {}

// Thread local v-table reference.
// Rather than have structs hold their options directly, we will include a map which
// is just a weak reference to the original arc. Then, in order to access the vtable, we use the Arc'd
// name as a key, and use that to grab the properties. Under any circumstance that I am aware of,
// the entry in the vtable should be alive for as long as the struct is legally allowed to be accessed.
pub struct VTable {
    map: fxhash::FxHashMap<InternedString, Gc<HashMap<SteelVal, SteelVal>>>,

    traits: fxhash::FxHashMap<InternedString, fxhash::FxHashMap<InternedString, Vec<SteelVal>>>,

    entries: Vec<VTableEntry>,
}

impl VTable {
    fn insert(name: InternedString, options: Gc<HashMap<SteelVal, SteelVal>>) {
        VTABLE.with(|x| x.borrow_mut().map.insert(name, options));
    }

    fn get(name: &InternedString) -> Option<Gc<HashMap<SteelVal, SteelVal>>> {
        VTABLE.with(|x| x.borrow().map.get(name).cloned())
    }

    pub(crate) fn sendable_entries(
        serializer: &mut MutableHashMap<usize, SerializableSteelVal>,
        visited: &mut MutableHashSet<usize>,
    ) -> Result<Vec<SendableVTableEntry>> {
        VTABLE.with(|x| {
            x.borrow()
                .entries
                .iter()
                .map(|entry| {
                    Ok(SendableVTableEntry {
                        name: entry.name,
                        proc: entry.proc,
                        transparent: entry.transparent,
                        mutable: entry.mutable,
                        properties: entry
                            .properties
                            .iter()
                            .map(|(key, value)| {
                                Ok((
                                    into_serializable_value(key.clone(), serializer, visited)?,
                                    into_serializable_value(value.clone(), serializer, visited)?,
                                ))
                            })
                            .collect::<Result<Vec<_>>>()?,
                    })
                })
                .collect()
        })
    }

    pub(crate) fn initialize_new_thread(
        values: Vec<SendableVTableEntry>,
        heap: &mut HeapSerializer,
    ) {
        for (index, entry) in values.into_iter().enumerate() {
            Self::new_entry(entry.name, entry.proc);

            let properties = Gc::new(
                entry
                    .properties
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            from_serializable_value(heap, k),
                            from_serializable_value(heap, v),
                        )
                    })
                    .collect(),
            );

            Self::set_entry(&StructTypeDescriptor(index), entry.proc, properties);
        }
    }

    // Returns a type descriptor, in this case it is just a usize
    #[cfg(not(feature = "sync"))]
    pub fn new_entry(name: InternedString, proc: Option<usize>) -> StructTypeDescriptor {
        VTABLE.with(|x| {
            let mut guard = x.borrow_mut();
            let length = guard.entries.len();

            guard.entries.push(VTableEntry::new(name, proc));

            StructTypeDescriptor(length)
        })
    }

    #[cfg(feature = "sync")]
    pub fn new_entry(name: InternedString, proc: Option<usize>) -> StructTypeDescriptor {
        let mut guard = STATIC_VTABLE.write();
        let length = guard.entries.len();
        guard.entries.push(VTableEntry::new(name, proc));
        StructTypeDescriptor(length)
    }

    // Updates the entry with the now available property information
    #[cfg(not(feature = "sync"))]
    pub fn set_entry(
        descriptor: &StructTypeDescriptor,
        proc: Option<usize>,
        properties: Gc<HashMap<SteelVal, SteelVal>>,
    ) {
        VTABLE.with(|x| {
            let mut guard = x.borrow_mut();

            let index = descriptor.0;

            let value = &mut guard.entries[index];

            value.proc = proc;

            // TODO: Lift these strings to the thread local
            value.transparent = properties
                .get(&TRANSPARENT_KEY.with(|x| x.clone()))
                .and_then(|x| x.as_bool())
                .unwrap_or_default();
            value.mutable = properties
                .get(&MUTABLE_KEY.with(|x| x.clone()))
                .and_then(|x| x.as_bool())
                .unwrap_or_default();

            value.properties = properties;
        })
    }

    #[cfg(feature = "sync")]
    pub fn set_entry(
        descriptor: &StructTypeDescriptor,
        proc: Option<usize>,
        properties: Gc<HashMap<SteelVal, SteelVal>>,
    ) {
        let mut guard = STATIC_VTABLE.write();

        let index = descriptor.0;

        let value = &mut guard.entries[index];

        value.proc = proc;

        // TODO: Lift these strings to the thread local
        value.transparent = properties
            .get(&STATIC_TRANSPARENT_KEY)
            .and_then(|x| x.as_bool())
            .unwrap_or_default();
        value.mutable = properties
            .get(&STATIC_MUTABLE_KEY)
            .and_then(|x| x.as_bool())
            .unwrap_or_default();

        value.properties = properties;
    }

    // fn define_trait()
}

pub static OK_RESULT_LABEL: Lazy<InternedString> = Lazy::new(|| "Ok".into());
pub static SOME_OPTION_LABEL: Lazy<InternedString> = Lazy::new(|| "Some".into());
pub static ERR_RESULT_LABEL: Lazy<InternedString> = Lazy::new(|| "Err".into());
pub static NONE_OPTION_LABEL: Lazy<InternedString> = Lazy::new(|| "None".into());
pub static TYPE_ID: Lazy<InternedString> = Lazy::new(|| "TypeId".into());

#[cfg(feature = "sync")]
#[cfg(feature = "sync")]
pub static STRUCT_DEFINITIONS: Lazy<Arc<RwLock<SymbolMap>>> =
    Lazy::new(|| Arc::new(RwLock::new(SymbolMap::default())));

#[cfg(not(feature = "sync"))]
thread_local! {
    pub static STRUCT_DEFINITIONS: RefCell<SymbolMap> = RefCell::new(SymbolMap::default());
}

#[cfg(feature = "sync")]
pub static STATIC_VTABLE: Lazy<RwLock<VTable>> = Lazy::new(|| {
    let mut map = fxhash::FxHashMap::default();

    #[cfg(feature = "imbl")]
    let result_options = Gc::new(imbl::hashmap! {
        SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    #[cfg(not(feature = "imbl"))]
    let result_options = Gc::new(im::hashmap! {
        SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    map.insert("Ok".into(), result_options.clone());
    map.insert("Err".into(), result_options.clone());
    map.insert("Some".into(), result_options.clone());
    map.insert("None".into(), result_options.clone());
    map.insert("TypeId".into(), result_options.clone());

    RwLock::new(VTable {
        map,
        traits: fxhash::FxHashMap::default(),
        entries: Vec::new(),
    })
});

#[cfg(feature = "sync")]
pub static STATIC_TRANSPARENT_KEY: Lazy<SteelVal> =
    Lazy::new(|| SteelVal::SymbolV("#:transparent".into()));
#[cfg(feature = "sync")]
pub static STATIC_MUTABLE_KEY: Lazy<SteelVal> =
    Lazy::new(|| SteelVal::SymbolV("#:transparent".into()));
#[cfg(feature = "sync")]
pub static STATIC_FIELDS_KEY: Lazy<SteelVal> =
    Lazy::new(|| SteelVal::SymbolV("#:transparent".into()));

pub static STATIC_OK_DESCRIPTOR: Lazy<StructTypeDescriptor> =
    Lazy::new(|| VTable::new_entry(*OK_RESULT_LABEL, None));
pub static STATIC_ERR_DESCRIPTOR: Lazy<StructTypeDescriptor> =
    Lazy::new(|| VTable::new_entry(*ERR_RESULT_LABEL, None));
pub static STATIC_SOME_DESCRIPTOR: Lazy<StructTypeDescriptor> =
    Lazy::new(|| VTable::new_entry(*SOME_OPTION_LABEL, None));
pub static STATIC_NONE_DESCRIPTOR: Lazy<StructTypeDescriptor> =
    Lazy::new(|| VTable::new_entry(*NONE_OPTION_LABEL, None));

// TODO: Just make these Arc'd and lazy static instead of thread local.
thread_local! {

    pub static TRANSPARENT_KEY: SteelVal = SteelVal::SymbolV("#:transparent".into());
    pub static MUTABLE_KEY: SteelVal = SteelVal::SymbolV("#:mutable".into());
    pub static FIELDS_KEY: SteelVal = SteelVal::SymbolV("#:fields".into());

    // Consult this to get values. It is possible, the vtable is _not_ populated for a given thread.
    // The only way that can happen is if a struct is constructed on another thread?
    // The value inside should explicitly be a thread safe value.
    pub static VTABLE: Rc<RefCell<VTable>> = {

        let mut map = fxhash::FxHashMap::default();

        #[cfg(all(feature = "sync", not(feature = "imbl")))]
        let result_options = Gc::new(im::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
        });

        #[cfg(all(feature = "sync", feature = "imbl"))]
        let result_options = Gc::new(imbl::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
        });

        #[cfg(all(not(feature = "sync"), feature = "std"))]
        let result_options = Gc::new(im_rc::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
        });

        #[cfg(all(not(feature = "sync"), not(feature = "std")))]
        let result_options = {
            let mut map = HashMap::<SteelVal, SteelVal>::default();
            map.insert(
                SteelVal::SymbolV("#:transparent".into()),
                SteelVal::BoolV(true),
            );
            Gc::new(map)
        };

        map.insert("Ok".into(), result_options.clone());
        map.insert("Err".into(), result_options.clone());
        map.insert("Some".into(), result_options.clone());
        map.insert("None".into(), result_options.clone());
        map.insert("TypeId".into(), result_options.clone());

        Rc::new(RefCell::new(VTable {
            map,
            traits: fxhash::FxHashMap::default(),
            entries: Vec::new(),
        }))
    };

    pub static DEFAULT_PROPERTIES: Gc<HashMap<SteelVal, SteelVal>> =
        Gc::new(HashMap::<SteelVal, SteelVal>::default());

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    pub static STANDARD_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = Gc::new(im::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });


    #[cfg(all(feature = "sync", feature = "imbl"))]
    pub static STANDARD_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = Gc::new(imbl::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    #[cfg(all(not(feature = "sync"), feature = "std"))]
    pub static STANDARD_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = Gc::new(im_rc::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    #[cfg(all(not(feature = "sync"), not(feature = "std")))]
    pub static STANDARD_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = {
        let mut map = HashMap::<SteelVal, SteelVal>::default();
        map.insert(
            SteelVal::SymbolV("#:transparent".into()),
            SteelVal::BoolV(true),
        );
        Gc::new(map)
    };

    pub static OK_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*OK_RESULT_LABEL, None);
    pub static ERR_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*ERR_RESULT_LABEL, None);
    pub static SOME_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*SOME_OPTION_LABEL, None);
    pub static NONE_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*NONE_OPTION_LABEL, None);


    pub static OK_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
            Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            1,
            OK_DESCRIPTOR.with(|x| *x),
        )))
    };

    pub static ERR_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
        Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            1,
            ERR_DESCRIPTOR.with(|x| *x),
        )))
    };

    #[cfg(all(not(feature = "sync"), feature = "std"))]
    pub static OPTION_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = Gc::new(im_rc::hashmap! {
        SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    #[cfg(all(not(feature = "sync"), not(feature = "std")))]
    pub static OPTION_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = {
        let mut map = HashMap::<SteelVal, SteelVal>::default();
        map.insert(
            SteelVal::SymbolV("#:transparent".into()),
            SteelVal::BoolV(true),
        );
        Gc::new(map)
    };

    #[cfg(all(feature = "sync", not(feature = "imbl")))]
    pub static OPTION_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = Gc::new(im::hashmap! {
        SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    #[cfg(all(feature = "sync", feature = "imbl"))]
    pub static OPTION_OPTIONS: Gc<HashMap<SteelVal, SteelVal>> = Gc::new(imbl::hashmap! {
        SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });

    pub static SOME_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
        Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            1,
            SOME_DESCRIPTOR.with(|x| *x),
        )))
    };

    pub static NONE_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
        Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            0,
            NONE_DESCRIPTOR.with(|x| *x),
        )))
    };
}

pub(crate) fn build_type_id_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/core/types");

    let name = *TYPE_ID;

    let type_descriptor = VTable::new_entry(name, None);

    // Build the getter for the first index
    let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);
    let predicate = UserDefinedStruct::predicate(type_descriptor);

    let constructor = Arc::new(UserDefinedStruct::constructor_thunk(2, type_descriptor));

    module
        .register_fn("#%vtable-update-entry!", VTable::set_entry)
        .register_value("TypeId?", predicate);

    module
}

pub(crate) fn build_result_structs() -> BuiltInModule {
    // Build module
    let mut module = BuiltInModule::new("steel/core/result");

    #[cfg(not(feature = "sync"))]
    {
        let name = *OK_RESULT_LABEL;

        let type_descriptor = OK_DESCRIPTOR.with(|x| *x);

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        VTable::set_entry(
            &OK_DESCRIPTOR.with(|x| *x),
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );

        VTable::set_entry(
            &ERR_DESCRIPTOR.with(|x| *x),
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );

        module
            .register_value(
                "Ok",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        1,
                        OK_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(1),
                ))),
            )
            .register_value("Ok?", predicate)
            .register_value("Ok->value", getter);
    }

    #[cfg(feature = "sync")]
    {
        let name = *OK_RESULT_LABEL;

        let type_descriptor = *STATIC_OK_DESCRIPTOR;

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        VTable::set_entry(
            &STATIC_OK_DESCRIPTOR,
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );

        VTable::set_entry(
            &STATIC_ERR_DESCRIPTOR,
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );

        module
            .register_value(
                "Ok",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        1,
                        *STATIC_OK_DESCRIPTOR,
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(1),
                ))),
            )
            .register_value("Ok?", predicate)
            .register_value("Ok->value", getter);
    }

    #[cfg(not(feature = "sync"))]
    {
        // let name = ERR_RESULT_LABEL.with(|x| Rc::clone(x));
        let name = *ERR_RESULT_LABEL;

        let type_descriptor = ERR_DESCRIPTOR.with(|x| *x);

        // let constructor = UserDefinedStruct::constructor(Rc::clone(&name), 1);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);

        module
            .register_value(
                "Err",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        1,
                        ERR_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(1),
                ))),
            )
            .register_value("Err?", predicate)
            .register_value("Err->value", getter);
    }

    #[cfg(feature = "sync")]
    {
        // let name = ERR_RESULT_LABEL.with(|x| Rc::clone(x));
        let name = *ERR_RESULT_LABEL;

        let type_descriptor = *STATIC_ERR_DESCRIPTOR;

        // let constructor = UserDefinedStruct::constructor(Rc::clone(&name), 1);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);

        module
            .register_value(
                "Err",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        1,
                        *STATIC_ERR_DESCRIPTOR,
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(1),
                ))),
            )
            .register_value("Err?", predicate)
            .register_value("Err->value", getter);
    }

    module
}

pub(crate) fn build_option_structs() -> BuiltInModule {
    // Build module
    let mut module = BuiltInModule::new("steel/core/option");

    if cfg!(feature = "sync") {
        VTable::set_entry(
            &STATIC_SOME_DESCRIPTOR,
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );

        VTable::set_entry(
            &STATIC_NONE_DESCRIPTOR,
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );
    } else {
        VTable::set_entry(
            &SOME_DESCRIPTOR.with(|x| *x),
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );

        VTable::set_entry(
            &NONE_DESCRIPTOR.with(|x| *x),
            None,
            STANDARD_OPTIONS.with(|x| x.clone()),
        );
    }

    #[cfg(not(feature = "sync"))]
    {
        // let name = SOME_OPTION_LABEL.with(|x| Rc::clone(x));
        let name = *SOME_OPTION_LABEL;
        let type_descriptor = SOME_DESCRIPTOR.with(|x| *x);

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        module
            .register_value(
                "Some",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        // Rc::clone(&name),
                        1,
                        SOME_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(1),
                ))),
            )
            .register_value("Some?", predicate)
            .register_value("Some->value", getter);
    }

    #[cfg(feature = "sync")]
    {
        // let name = SOME_OPTION_LABEL.with(|x| Rc::clone(x));
        let name = *SOME_OPTION_LABEL;
        let type_descriptor = *STATIC_SOME_DESCRIPTOR;

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(type_descriptor, 0);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        module
            .register_value(
                "Some",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        // Rc::clone(&name),
                        1,
                        *STATIC_SOME_DESCRIPTOR,
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(1),
                ))),
            )
            .register_value("Some?", predicate)
            .register_value("Some->value", getter);
    }

    #[cfg(not(feature = "sync"))]
    {
        // let name = NONE_LABEL.with(|x| Rc::clone(x));
        let name = *NONE_OPTION_LABEL;
        let type_descriptor = NONE_DESCRIPTOR.with(|x| *x);
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        module
            .register_value(
                "None",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        0,
                        NONE_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(0),
                ))),
            )
            .register_value("None?", predicate);
    }

    #[cfg(feature = "sync")]
    {
        // let name = NONE_LABEL.with(|x| Rc::clone(x));
        let name = *NONE_OPTION_LABEL;
        let type_descriptor = *STATIC_NONE_DESCRIPTOR;
        let predicate = UserDefinedStruct::predicate(type_descriptor);

        module
            .register_value(
                "None",
                SteelVal::BoxedFunction(Gc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        0,
                        *STATIC_NONE_DESCRIPTOR,
                    )),
                    Some(Arc::new(String::from(name.resolve()))),
                    Some(0),
                ))),
            )
            .register_value("None?", predicate);
    }

    module
}

pub struct RecoverableResult<T, E>(core::result::Result<T, E>);

impl<T: IntoSteelVal, E: IntoSteelVal> IntoSteelVal for RecoverableResult<T, E> {
    #[inline(always)]
    fn into_steelval(self) -> Result<SteelVal> {
        match self.0 {
            Ok(s) => s.into_steelval(),
            Err(e) => UserDefinedStruct::new_err(e.into_steelval()?),
        }
    }
}

impl<T: IntoSteelVal, E: IntoSteelVal> From<RecoverableResult<T, E>>
    for core::result::Result<T, E>
{
    fn from(value: RecoverableResult<T, E>) -> Self {
        value.0
    }
}

impl<T: IntoSteelVal, E: IntoSteelVal> From<core::result::Result<T, E>>
    for RecoverableResult<T, E>
{
    fn from(value: core::result::Result<T, E>) -> Self {
        RecoverableResult(value)
    }
}

/// Result type that automatically maps into the equivalent Result type within Steel.
/// For example, `Ok(10)` will map to `(Ok 10)`, and `Err(10)` will map to `(Err 10)`.
pub struct SteelResult<T, E>(core::result::Result<T, E>);
impl<T: IntoSteelVal, E: IntoSteelVal> IntoSteelVal for SteelResult<T, E> {
    #[inline(always)]
    fn into_steelval(self) -> Result<SteelVal> {
        match self.0 {
            Ok(s) => UserDefinedStruct::new_ok(s),
            Err(e) => UserDefinedStruct::new_err(e.into_steelval()?),
        }
    }
}

impl<T: IntoSteelVal, E: IntoSteelVal> From<SteelResult<T, E>> for core::result::Result<T, E> {
    fn from(value: SteelResult<T, E>) -> Self {
        value.0
    }
}

impl<T: IntoSteelVal, E: IntoSteelVal> From<core::result::Result<T, E>> for SteelResult<T, E> {
    fn from(value: core::result::Result<T, E>) -> Self {
        SteelResult(value)
    }
}

// By default, the standard result type will automatically unwrap ok values, and raise errors
// if they occur as genuine steel errors. If you'd like to catch these, you can set up an exception handler.
// The runtime cost for this is relatively low.
impl<T: IntoSteelVal, E: IntoSteelVal> IntoSteelVal for core::result::Result<T, E> {
    fn into_steelval(self) -> Result<SteelVal> {
        match self {
            Ok(s) => s.into_steelval(),
            Err(e) => match e.as_error() {
                Ok(e) => Err(e),
                Err(e) => Err(SteelErr::new(
                    ErrorKind::Generic,
                    e.into_steelval()?.to_string(),
                )),
            },
        }
    }
}

impl<T: FromSteelVal, E: FromSteelVal> FromSteelVal for core::result::Result<T, E> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::CustomStruct(s) = val {
            if s.is_ok() {
                Ok(Ok(T::from_steelval(s.fields.first().unwrap())?))
            } else if s.is_err() {
                Ok(Err(E::from_steelval(s.fields.first().unwrap())?))
            } else {
                stop!(ConversionError => format!("Failed attempting to convert an instance of a steelval into a result type: {val:?}"))
            }
        } else {
            stop!(ConversionError => format!("Failed attempting to convert an instance of a steelval into a result type - value is not a struct: {val:?}"));
        }
    }
}
