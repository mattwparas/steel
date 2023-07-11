#![allow(unused)]
#![allow(clippy::type_complexity)]

use im_rc::HashMap;

use crate::compiler::map::SymbolMap;
use crate::parser::interner::InternedString;
use crate::rvals::Custom;
use crate::steel_vm::register_fn::RegisterFn;
use crate::throw;
use crate::{
    core::utils::Boxed,
    rvals::{FromSteelVal, IntoSteelVal},
};
use crate::{
    gc::Gc,
    rvals::{AsRefSteelVal, SRef, SteelString},
};
use crate::{
    rvals::{Result, SteelVal},
    SteelErr,
};
use crate::{steel_vm::builtin::BuiltInModule, stop};
use std::sync::Arc;
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use super::functions::BoxedDynFunction;

enum StringOrMagicNumber {
    String(Rc<String>),
    Magic(usize),
}

pub struct VTableEntry {
    name: InternedString,
    properties: Gc<im_rc::HashMap<SteelVal, SteelVal>>,
    proc: Option<usize>,
    transparent: bool,
    mutable: bool,
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
#[derive(Debug, Clone, Hash)]
pub enum Properties {
    BuiltIn,
    Local(Gc<im_rc::HashMap<SteelVal, SteelVal>>),
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

impl Custom for StructTypeDescriptor {}

#[derive(Clone, Debug, Hash)]
pub struct UserDefinedStruct {
    pub(crate) name: InternedString,

    // TODO: Consider using... just a vec here.
    pub(crate) fields: smallvec::SmallVec<[SteelVal; 5]>,

    // Type Descriptor. Use this as an index into the VTable to find anything that we need.
    pub(crate) type_descriptor: StructTypeDescriptor,
}

// TODO: This could blow the stack for big trees...
impl PartialEq for UserDefinedStruct {
    fn eq(&self, other: &Self) -> bool {
        self.type_descriptor == other.type_descriptor
            && self.name == other.name
            && self.fields == other.fields
        // && Properties::ptr_eq(&self.properties, &other.properties)
    }
}

impl std::fmt::Display for UserDefinedStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self
            .get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
            .is_some()
        {
            write!(f, "({}", self.name)?;
            // for i in 0..self.len - 1 {
            //     write!(f, " {}", self.fields[i])?;
            // }

            for i in &self.fields {
                write!(f, " {}", i)?;
            }

            write!(f, ")")
        } else {
            write!(f, "({})", self.name)
        }
    }
}

enum StructBacking {
    Immutable(ImmutableMaybeHeapVec),
    Mutable(MaybeHeapVec),
}

impl StructBacking {
    fn get(&self, index: usize) -> Option<SteelVal> {
        match self {
            Self::Immutable(v) => v.get(index),
            Self::Mutable(v) => v.get(index),
        }
    }
}

#[test]
fn check_sizes() {
    println!("MaybeHeapVec: {}", std::mem::size_of::<MaybeHeapVec>());
    println!(
        "ImmutableMaybeHeapVec: {}",
        std::mem::size_of::<ImmutableMaybeHeapVec>()
    );
    println!("StructBacking: {}", std::mem::size_of::<StructBacking>());
}

/// Array backed fixed size vector
/// Structs should be fixed size - they don't need to grow once they are created, and for
/// unit or single values, they shouldn't need to allocate an array at all. For structs up to
/// length 6, we keep these as individual enum variants, so that we can simple switch on the kind.
/// beyond length 6, we spill into a normal vector.
#[derive(Clone, Debug)]
pub(crate) enum MaybeHeapVec {
    Unit,
    One(SteelVal),
    Two([SteelVal; 2]),
    Three([SteelVal; 3]),
    Four([SteelVal; 4]),
    Five([SteelVal; 5]),
    Spilled(Vec<SteelVal>),
}

impl MaybeHeapVec {
    pub fn from_slice(args: &[SteelVal]) -> Self {
        match args {
            [] => Self::Unit,
            [one] => Self::One(one.clone()),
            [one, two] => Self::Two([one.clone(), two.clone()]),
            [one, two, three] => Self::Three([one.clone(), two.clone(), three.clone()]),
            [one, two, three, four] => {
                Self::Four([one.clone(), two.clone(), three.clone(), four.clone()])
            }
            [one, two, three, four, five] => Self::Five([
                one.clone(),
                two.clone(),
                three.clone(),
                four.clone(),
                five.clone(),
            ]),
            _ => Self::Spilled(args.to_vec()),
        }
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<SteelVal> {
        match self {
            MaybeHeapVec::Unit => None,
            MaybeHeapVec::One(v) => Some(v.clone()),
            MaybeHeapVec::Two(a) => Some(a[index].clone()),
            MaybeHeapVec::Three(a) => Some(a[index].clone()),
            MaybeHeapVec::Four(a) => Some(a[index].clone()),
            MaybeHeapVec::Five(a) => Some(a[index].clone()),
            MaybeHeapVec::Spilled(a) => Some(a[index].clone()),
        }
    }

    #[inline(always)]
    pub fn set(&mut self, index: usize, mut value: SteelVal) -> SteelVal {
        match self {
            MaybeHeapVec::Unit => panic!("Tried to get the 0th index of a unit struct"),
            MaybeHeapVec::One(v) => {
                let old = v.clone();
                *v = value;
                old
            }
            MaybeHeapVec::Two(a) => {
                std::mem::swap(&mut a[index], &mut value);
                value
            }
            MaybeHeapVec::Three(a) => {
                std::mem::swap(&mut a[index], &mut value);
                value
            }
            MaybeHeapVec::Four(a) => {
                std::mem::swap(&mut a[index], &mut value);
                value
            }
            MaybeHeapVec::Five(a) => {
                std::mem::swap(&mut a[index], &mut value);
                value
            }
            MaybeHeapVec::Spilled(a) => {
                std::mem::swap(&mut a[index], &mut value);
                value
            }
        }
    }
}

// TODO: This is an immutable struct, which can has sole ownership of the values
// underneath. Could be useful to separate mutable structs vs immutable structs, since
// that way we can safely access this stuff
pub(crate) enum ImmutableMaybeHeapVec {
    Unit,
    One(SteelVal),
    Two(Rc<[SteelVal; 2]>),
    Three(Rc<[SteelVal; 3]>),
    Four(Rc<[SteelVal; 4]>),
    Five(Rc<[SteelVal; 5]>),
    Spilled(Rc<[SteelVal]>),
}

impl ImmutableMaybeHeapVec {
    pub fn from_slice(args: &[SteelVal]) -> Self {
        match args {
            [] => Self::Unit,
            [one] => Self::One(one.clone()),
            [one, two] => Self::Two(Rc::new([one.clone(), two.clone()])),
            [one, two, three] => Self::Three(Rc::new([one.clone(), two.clone(), three.clone()])),
            [one, two, three, four] => Self::Four(Rc::new([
                one.clone(),
                two.clone(),
                three.clone(),
                four.clone(),
            ])),
            [one, two, three, four, five] => Self::Five(Rc::new([
                one.clone(),
                two.clone(),
                three.clone(),
                four.clone(),
                five.clone(),
            ])),
            _ => Self::Spilled(args.iter().cloned().collect()),
        }
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<SteelVal> {
        match self {
            ImmutableMaybeHeapVec::Unit => None,
            ImmutableMaybeHeapVec::One(v) => Some(v.clone()),
            ImmutableMaybeHeapVec::Two(a) => Some(a[index].clone()),
            ImmutableMaybeHeapVec::Three(a) => Some(a[index].clone()),
            ImmutableMaybeHeapVec::Four(a) => Some(a[index].clone()),
            ImmutableMaybeHeapVec::Five(a) => Some(a[index].clone()),
            ImmutableMaybeHeapVec::Spilled(a) => Some(a[index].clone()),
        }
    }
}

impl UserDefinedStruct {
    fn new(
        name: InternedString,
        type_descriptor: StructTypeDescriptor,
        fields: &[SteelVal],
    ) -> Self {
        // let (options, rest) = fields.split_first().ok_or_else(
        //     throw!(ArityMismatch => "struct constructor expects at least one argument"),
        // )?;

        // let (proc, rest) = rest.split_first().ok_or_else(
        //     throw!(ArityMismatch => "struct constructor expects at least one argument"),
        // )?;

        // todo!()

        Self {
            name,
            fields: fields.into(),
            // properties: Properties::BuiltIn,
            // proc: None,
            type_descriptor,
        }

        // Ok(Self {

        // })

        // TODO: Don't use a hashmap for these properties. Probably best to have some kind of fixed purpose
        // data structure? A hashmap is also fine, but something that is required to be constructed with only
        // constants could be helpful.
        // if let SteelVal::HashMapV(properties) = options.clone() {
        // Don't do this - we don't want to have to do a hash every time?
        // let proc = PROC_PROPERTY.with(|property_keyword| {
        //     properties
        //         .get(property_keyword)
        //         .and_then(|value| value.as_usize())
        // });

        // Ok(Self {
        // name,
        // fields: rest.into(),
        // len: fields.len(),
        // properties: Properties::Local(properties),
        // proc: proc.as_usize(),
        // type_descriptor,
        // })
        // } else {
        // stop!(TypeMismatch => format!("struct constructor expected a hashmap, found: {options}"))
        // }
    }

    pub(crate) fn get(&self, val: &SteelVal) -> Option<SteelVal> {
        VTABLE.with(|x| {
            x.borrow().entries[self.type_descriptor.0]
                .properties
                .get(val)
                .cloned()
        })

        // match &self.properties {
        // Properties::BuiltIn => VTable::get(&self.name).and_then(|x| x.get(val).cloned()),
        // Properties::Local(p) => p.get(val).cloned(),
        // }
    }

    #[inline(always)]
    fn new_ok<T: IntoSteelVal>(value: T) -> Result<SteelVal> {
        OK_CONSTRUCTOR.with(|x| x(&[value.into_steelval()?]))
    }

    #[inline(always)]
    fn new_err<T: IntoSteelVal>(value: T) -> Result<SteelVal> {
        ERR_CONSTRUCTOR.with(|x| x(&[value.into_steelval()?]))
    }

    // TODO: This doesn't particularly play nice with dynamic libraries. Should probably just assign some IDs
    // to these structs and use them that way
    #[inline(always)]
    fn is_ok(&self) -> bool {
        // todo!()
        self.name == *OK_RESULT_LABEL
        // Arc::ptr_eq(&self.name, &OK_RESULT_LABEL.with(|x| Rc::clone(x)))
        //     || self.name == OK_RESULT_LABEL.with(|x| Rc::clone(x))
    }

    #[inline(always)]
    fn is_err(&self) -> bool {
        // todo!()
        self.name == *ERR_RESULT_LABEL
        // Arc::ptr_eq(&self.name, &ERR_RESULT_LABEL.with(|x| Rc::clone(x)))
        //     || self.name == ERR_RESULT_LABEL.with(|x| Rc::clone(x))
    }

    pub(crate) fn maybe_proc(&self) -> Option<&SteelVal> {
        // self.proc.as_ref().map(|s| &self.fields[*s])

        VTABLE.with(|x| {
            x.borrow().entries[self.type_descriptor.0]
                .proc
                .as_ref()
                .map(|s| &self.fields[*s])
        })
    }

    fn new_with_options(
        name: InternedString,
        properties: Properties,
        type_descriptor: StructTypeDescriptor,
        rest: &[SteelVal],
    ) -> Self {
        Self {
            name,
            fields: rest.into(),
            // len: rest.len() + 1,
            // properties,
            // proc: None,
            type_descriptor,
        }
    }

    fn constructor_thunk(
        name: InternedString,
        // options: Properties,
        len: usize,
        descriptor: StructTypeDescriptor,
    ) -> impl Fn(&[SteelVal]) -> Result<SteelVal> {
        move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != len {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    name.clone(),
                    args.len(),
                    len
                );
                stop!(ArityMismatch => error_message);
            }

            let new_struct =
                UserDefinedStruct::new_with_options(name, Properties::BuiltIn, descriptor, args);

            Ok(SteelVal::CustomStruct(Gc::new(RefCell::new(new_struct))))
        }
    }

    fn constructor_with_options(
        name: InternedString,
        // options: Gc<HashMap<SteelVal, SteelVal>>,
        // options: Properties,
        len: usize,
        descriptor: StructTypeDescriptor,
    ) -> SteelVal {
        // let out_name = Arc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != len {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    name.clone(),
                    args.len(),
                    len
                );
                stop!(ArityMismatch => error_message);
            }

            let new_struct =
                UserDefinedStruct::new_with_options(name, Properties::BuiltIn, descriptor, args);

            Ok(SteelVal::CustomStruct(Gc::new(RefCell::new(new_struct))))
        };

        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            Some(name.resolve().to_string().into()),
            Some(len),
        )))
    }

    fn constructor(
        name: InternedString,
        len: usize,
        type_descriptor: StructTypeDescriptor,
    ) -> SteelVal {
        // let out_name = Arc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != len {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    name.clone(),
                    args.len(),
                    len
                );
                stop!(ArityMismatch => error_message);
            }

            // Definitely use interned symbols for these. Otherwise we're going to be doing A LOT of
            // arc cloning, and we don't want that.
            let new_struct = UserDefinedStruct::new(name, type_descriptor, args);

            Ok(SteelVal::CustomStruct(Gc::new(RefCell::new(new_struct))))
        };

        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            // Some(out_name),
            Some(name.resolve().to_string().into()),
            Some(len),
        )))
    }

    fn predicate(name: InternedString) -> SteelVal {
        // let out_name = Arc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                let error_message =
                    format!("{}? expected one argument, found {}", name, args.len());
                stop!(ArityMismatch => error_message);
            }
            Ok(SteelVal::BoolV(match &args[0] {
                SteelVal::CustomStruct(my_struct) if my_struct.borrow().name == name => true,
                // SteelVal::CustomStruct(my_struct) if my_struct.name == name => true,
                _ => false,
            }))
        };

        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            // Some(out_name),
            Some(name.resolve().to_string().into()),
            Some(1),
        )))
    }

    fn getter_prototype(name: InternedString) -> SteelVal {
        // let out_name = Arc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => format!("{} expected two arguments", name));
            }

            let steel_struct = &args[0].clone();
            let idx = &args[1].clone();

            match (&steel_struct, &idx) {
                (SteelVal::CustomStruct(s), SteelVal::IntV(idx)) => {
                    if s.borrow().name != name {
                        // println!("{}, {}", s.borrow().name.resolve(), name.resolve());

                        stop!(TypeMismatch => format!("Struct getter expected {}, found {:?}, {:?}", name, &s, &steel_struct));
                    }

                    if *idx < 0 {
                        stop!(Generic => "struct-ref expected a non negative index");
                    }

                    s.borrow()
                        .fields
                        .get(*idx as usize)
                        .cloned()
                        .ok_or_else(throw!(Generic => "struct-ref: index out of bounds"))
                }
                _ => {
                    let error_message = format!(
                        "{name} expected a struct and an int, found: {steel_struct} and {idx}"
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            // Some(out_name),
            Some(name.resolve().to_string().into()),
            Some(2),
        )))
    }

    fn getter_prototype_index(name: InternedString, index: usize) -> SteelVal {
        // let out_name = Arc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "struct-ref expected one argument");
            }

            let steel_struct = &args[0].clone();

            match &steel_struct {
                SteelVal::CustomStruct(s) => {
                    // println!("{}, {}", s.borrow().name.resolve(), name.resolve());

                    if s.borrow().name != name {
                        stop!(TypeMismatch => format!("Struct getter expected {}, found {:?}, {:?}", name, &s, &steel_struct));
                    }

                    s.borrow()
                        .fields
                        .get(index)
                        .cloned()
                        .ok_or_else(throw!(Generic => "struct-ref: index out of bounds"))
                }
                _ => {
                    let error_message = format!(
                        "{name} expected a struct and an int, found: {steel_struct} and {index}"
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            // Some(out_name),
            Some(name.resolve().to_string().into()),
            Some(1),
        )))
    }

    fn setter_prototype(name: InternedString) -> SteelVal {
        // let out_name = Arc::clone(&name);

        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 3 {
                stop!(ArityMismatch => "struct-ref expected 3 arguments");
            }

            let steel_struct = &args[0].clone();
            let idx = &args[1].clone();
            let arg = &args[2].clone();

            match (&steel_struct, &idx) {
                (SteelVal::CustomStruct(s), SteelVal::IntV(idx)) => {
                    if s.borrow().name != name {
                        stop!(TypeMismatch => format!("Struct setter expected {}, found {}", name, &s.borrow().name));
                    }

                    if *idx < 0 {
                        stop!(Generic => "struct-ref expected a non negative index");
                    }
                    if *idx as usize >= s.borrow().fields.len() {
                        stop!(Generic => "struct-ref: index out of bounds");
                    }

                    let mut guard = s.borrow_mut();

                    let old = guard.fields[*idx as usize].clone();

                    guard.fields[*idx as usize] = arg.clone();

                    Ok(old)
                }
                _ => {
                    let error_message = format!(
                        "struct-ref expected a struct and an int, found: {steel_struct} and {idx}"
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
            Arc::new(f),
            // Some(out_name),
            Some(name.resolve().to_string().into()),
            Some(3),
        )))
    }

    // pub fn properties(&self) -> SteelVal {
    // SteelVal::HashMapV(self.properties.clone())
    // }
}

pub fn make_struct_type(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "make-struct-type expects 2 args, found: {}", args.len())
    }

    // Convert the string into an Arc'd string - this now makes the generated functions
    // thread safe.
    let name: InternedString = if let SteelVal::SymbolV(s) = &args[0] {
        Ok::<_, SteelErr>(s)
    } else {
        stop!(TypeMismatch => format!("make-struct-type expected a symbol for the name, found: {}", &args[0]));
    }?.as_str().into();

    let field_count = if let SteelVal::IntV(i) = &args[1] {
        Ok::<_, SteelErr>(i)
    } else {
        stop!(TypeMismatch => format!("make-struct-type expected an integer for the field count, found: {}", &args[0]));
    }?;

    // Make a slot in the VTable for this struct
    let struct_type_descriptor = VTable::new_entry(name, None);

    // Build out the constructor and the predicate
    let struct_constructor =
        UserDefinedStruct::constructor(name, *field_count as usize, struct_type_descriptor);
    let struct_predicate = UserDefinedStruct::predicate(name);

    let getter_prototype = UserDefinedStruct::getter_prototype(name);
    let setter_prototype = UserDefinedStruct::setter_prototype(name);

    // We do not have the properties yet. Should probably intern the
    // let struct_type_id = new_type_id(name, address_or_name)

    Ok(SteelVal::ListV(im_lists::list![
        // Convert this into a descriptor before we're done
        struct_type_descriptor.into_steelval().unwrap(),
        struct_constructor,
        struct_predicate,
        getter_prototype,
        setter_prototype,
        // struct_type_descriptor,
    ]))
}

/*
TODO:

Create something like a type-id - dispatch from there to the proper implementation by consulting
the VTable.

Implementing a trait for a type should error if the trait is already implemented, and the predicate
for checking if a trait is implemented should just consult the table.

There should be a way to move things over


*/

// Implement internal thing here?
struct SteelTrait {
    name: InternedString,
    method_names: Vec<InternedString>,
}

struct SteelTraitImplementation {}

// // Return the method associated
// fn dispatch(t: &SteelTrait, value: &SteelVal) -> SteelVal {
//     match value {
//         SteelVal::CustomStruct(s) => {
//             todo!()
//         }
//         _ => {
//             todo!()
//         }
//     }
// }

// fn new_type_id(type_name: InternedString, address_or_name: SteelVal) -> Result<SteelVal> {
//     UserDefinedStruct::constructor_thunk(*TYPE_ID, 2)(&[
//         type_name.as_u32().into_steelval().unwrap(),
//         address_or_name,
//     ])
// }

// #[steel_derive::function(name = "value->type-id")]
// fn custom_type_id(value: &SteelVal) -> Result<SteelVal> {
//     match value {
//         SteelVal::CustomStruct(s) => {
//             let guard = s.borrow();

//             match &guard.properties {
//                 Properties::BuiltIn => {
//                     // Return a special struct representing the type id for the type id
//                     new_type_id(
//                         guard.name,
//                         SteelVal::IntV((-1 * (guard.name.as_u32() as isize))),
//                     )
//                 }
//                 Properties::Local(p) => {
//                     // Return a special struct representing the type id for the type id
//                     new_type_id(guard.name, SteelVal::IntV((p.as_ptr() as usize) as isize))
//                 }
//             }
//         }
//         _ => todo!(),
//     }
// }

// Thread local v-table reference.
// Rather than have structs hold their options directly, we will include a map which
// is just a weak reference to the original arc. Then, in order to access the vtable, we use the Arc'd
// name as a key, and use that to grab the properties. Under any circumstance that I am aware of,
// the entry in the vtable should be alive for as long as the struct is legally allowed to be accessed.
pub struct VTable {
    map: fxhash::FxHashMap<InternedString, Gc<im_rc::HashMap<SteelVal, SteelVal>>>,

    traits: fxhash::FxHashMap<InternedString, fxhash::FxHashMap<InternedString, Vec<SteelVal>>>,

    entries: Vec<VTableEntry>,
}

impl VTable {
    fn insert(name: InternedString, options: Gc<im_rc::HashMap<SteelVal, SteelVal>>) {
        VTABLE.with(|x| x.borrow_mut().map.insert(name, options));
    }

    fn get(name: &InternedString) -> Option<Gc<im_rc::HashMap<SteelVal, SteelVal>>> {
        VTABLE.with(|x| x.borrow().map.get(name).cloned())
    }

    // Returns a type descriptor, in this case it is just a usize
    fn new_entry(name: InternedString, proc: Option<usize>) -> StructTypeDescriptor {
        VTABLE.with(|x| {
            let mut guard = x.borrow_mut();
            let length = guard.entries.len();

            guard.entries.push(VTableEntry::new(name, proc));

            StructTypeDescriptor(length)
        })
    }

    // Updates the entry with the now available property information
    fn set_entry(
        descriptor: &StructTypeDescriptor,
        proc: Option<usize>,
        properties: Gc<im_rc::HashMap<SteelVal, SteelVal>>,
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

    // fn define_trait()
}

// Probably just... intern the strings instead? I have an interner, it might be useful to
// just use that directly. Resolve shouldn't be that difficult in this case.
lazy_static::lazy_static! {
    pub static ref OK_RESULT_LABEL: InternedString = "Ok".into();
    pub static ref ERR_RESULT_LABEL: InternedString = "Err".into();
    pub static ref SOME_OPTION_LABEL: InternedString = "Some".into();
    pub static ref NONE_OPTION_LABEL: InternedString = "None".into();
    pub static ref TYPE_ID: InternedString = "TypeId".into();


    pub static ref STRUCT_DEFINITIONS: Arc<std::sync::RwLock<SymbolMap>> = Arc::new(std::sync::RwLock::new(SymbolMap::default()));
}

// TODO: Just make these Arc'd and lazy static instead of thread local.
thread_local! {

    pub static TRANSPARENT_KEY: SteelVal = SteelVal::SymbolV("#:transparent".into());
    pub static MUTABLE_KEY: SteelVal = SteelVal::SymbolV("#:mutable".into());

    // Consult this to get values. It is possible, the vtable is _not_ populated for a given thread.
    // The only way that can happen is if a struct is constructed on another thread?
    // The value inside should explicitly be a thread safe value.
    pub static VTABLE: Rc<RefCell<VTable>> = {

        let mut map = fxhash::FxHashMap::default();

        let result_options = Gc::new(im_rc::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
        });

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

    pub static DEFAULT_PROPERTIES: Gc<im_rc::HashMap<SteelVal, SteelVal>> = Gc::new(im_rc::HashMap::new());
    pub static STANDARD_OPTIONS: Gc<im_rc::HashMap<SteelVal, SteelVal>> = Gc::new(im_rc::hashmap! {
            SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });


    pub static OK_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*OK_RESULT_LABEL, None);
    pub static ERR_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*ERR_RESULT_LABEL, None);
    pub static SOME_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*SOME_OPTION_LABEL, None);
    pub static NONE_DESCRIPTOR: StructTypeDescriptor = VTable::new_entry(*NONE_OPTION_LABEL, None);


    // pub static OK_RESULT_LABEL: Rc<String> = Rc::new("Ok".into());
    // pub static ERR_RESULT_LABEL: Rc<String> = Rc::new("Err".into());
    // pub static RESULT_OPTIONS: Gc<im_rc::HashMap<SteelVal, SteelVal>> = Gc::new(im_rc::hashmap! {
        // SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    // });
    pub static OK_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
            Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            *OK_RESULT_LABEL,
            // RESULT_OPTIONS.with(|x| Gc::clone(x)),
            1,
            OK_DESCRIPTOR.with(|x| *x),
        )))
    };

    pub static ERR_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
        // let name = ERR_RESULT_LABEL.with(|x| Rc::clone(x));
        Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            // Rc::clone(&name),
            *ERR_RESULT_LABEL,
            // RESULT_OPTIONS.with(|x| Gc::clone(x)),
            1,
            ERR_DESCRIPTOR.with(|x| *x),
        )))
    };

    // pub static SOME_OPTION_LABEL: Rc<String> = Rc::new("Some".into());
    // pub static NONE_LABEL: Rc<String> = Rc::new("None".into());
    pub static OPTION_OPTIONS: Gc<im_rc::HashMap<SteelVal, SteelVal>> = Gc::new(im_rc::hashmap! {
        SteelVal::SymbolV("#:transparent".into()) => SteelVal::BoolV(true),
    });
    pub static SOME_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
        // let name = SOME_OPTION_LABEL.with(|x| Rc::clone(x));
        Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            // Rc::clone(&name),
            *SOME_OPTION_LABEL,
            // OPTION_OPTIONS.with(|x| Gc::clone(x)),
            1,
            SOME_DESCRIPTOR.with(|x| *x),
        )))
    };

    pub static NONE_CONSTRUCTOR: Rc<Box<dyn Fn(&[SteelVal]) -> Result<SteelVal>>> = {
        // let name = NONE_LABEL.with(|x| Rc::clone(x));
        Rc::new(Box::new(UserDefinedStruct::constructor_thunk(
            // Rc::clone(&name),
            *NONE_OPTION_LABEL,
            // OPTION_OPTIONS.with(|x| Gc::clone(x)),
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
    let getter = UserDefinedStruct::getter_prototype_index(name, 0);
    let predicate = UserDefinedStruct::predicate(name);

    let constructor = Arc::new(UserDefinedStruct::constructor_thunk(
        name,
        2,
        type_descriptor,
    ));

    module
        .register_fn("#%vtable-update-entry!", VTable::set_entry)
        // .register_value(
        //     "TypeId",
        //     SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
        //         constructor,
        //         Some(name.resolve().to_string().into()),
        //         Some(2),
        //     ))),
        // )
        .register_value("TypeId?", predicate);
    // .register_native_fn_definition(CUSTOM_TYPE_ID_DEFINITION);

    module
}

pub(crate) fn build_result_structs() -> BuiltInModule {
    // Build module
    let mut module = BuiltInModule::new("steel/core/result".to_string());

    {
        let name = *OK_RESULT_LABEL;

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(name, 0);
        let predicate = UserDefinedStruct::predicate(name);

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
        // VTable::set_entry(OK_DESCRIPTOR, None, )

        module
            .register_value(
                "Ok",
                SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        // Rc::clone(&name),
                        name, // RESULT_OPTIONS.with(|x| Gc::clone(x)),
                        1,
                        OK_DESCRIPTOR.with(|x| *x),
                    )),
                    // Some(Rc::clone(&name)),
                    Some(name.resolve().to_string().into()),
                    Some(1),
                ))),
            )
            .register_value("Ok?", predicate)
            .register_value("Ok->value", getter);
    }

    {
        // let name = ERR_RESULT_LABEL.with(|x| Rc::clone(x));
        let name = *ERR_RESULT_LABEL;
        // let constructor = UserDefinedStruct::constructor(Rc::clone(&name), 1);
        let predicate = UserDefinedStruct::predicate(name);

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(name, 0);

        module
            .register_value(
                "Err",
                SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        // Rc::clone(&name),
                        name, // RESULT_OPTIONS.with(|x| Gc::clone(x)),
                        1,
                        ERR_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(name.resolve().to_string().into()),
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
    let mut module = BuiltInModule::new("steel/core/option".to_string());

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

    {
        // let name = SOME_OPTION_LABEL.with(|x| Rc::clone(x));
        let name = *SOME_OPTION_LABEL;

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(name, 0);
        let predicate = UserDefinedStruct::predicate(name);

        module
            .register_value(
                "Some",
                SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        // Rc::clone(&name),
                        name, // OPTION_OPTIONS.with(|x| Gc::clone(x)),
                        1,
                        SOME_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(name.resolve().to_string().into()),
                    Some(1),
                ))),
            )
            .register_value("Some?", predicate)
            .register_value("Some->value", getter);
    }

    {
        // let name = NONE_LABEL.with(|x| Rc::clone(x));
        let name = *NONE_OPTION_LABEL;
        // let constructor = UserDefinedStruct::constructor(Rc::clone(&name), 1);
        let predicate = UserDefinedStruct::predicate(name);

        module
            .register_value(
                "None",
                SteelVal::BoxedFunction(Rc::new(BoxedDynFunction::new_owned(
                    Arc::new(UserDefinedStruct::constructor_thunk(
                        name, // OPTION_OPTIONS.with(|x| Gc::clone(x)),
                        1,
                        NONE_DESCRIPTOR.with(|x| *x),
                    )),
                    Some(name.resolve().to_string().into()),
                    Some(0),
                ))),
            )
            .register_value("None?", predicate);
    }

    module
}

// impl<T: IntoSteelVal, E: std::fmt::Debug> IntoSteelVal for Result<T, E> {
//     fn into_steelval(self) -> Result<SteelVal> {
//         match self {
//             Ok(s) => Ok()
//         }
//     }
// }

// impl<T: IntoSteelVal, E: std::fmt::Debug> IntoSteelVal for std::result::Result<T, E> {
//     fn into_steelval(self) -> Result<SteelVal> {
//         match self {
//             Ok(s) => UserDefinedStruct::new_ok(s),
//             Err(e) => UserDefinedStruct::new_err(format!("{:?}", e)),
//         }
//     }
// }

impl<T: IntoSteelVal, E: IntoSteelVal> IntoSteelVal for std::result::Result<T, E> {
    fn into_steelval(self) -> Result<SteelVal> {
        match self {
            Ok(s) => UserDefinedStruct::new_ok(s),
            Err(e) => UserDefinedStruct::new_err(e.into_steelval()?),
        }
    }
}

impl<T: FromSteelVal, E: FromSteelVal> FromSteelVal for std::result::Result<T, E> {
    fn from_steelval(val: &SteelVal) -> Result<Self> {
        if let SteelVal::CustomStruct(s) = val {
            if s.borrow().is_ok() {
                Ok(Ok(T::from_steelval(s.borrow().fields.get(0).unwrap())?))
            } else if s.borrow().is_err() {
                Ok(Err(E::from_steelval(s.borrow().fields.get(0).unwrap())?))
            } else {
                stop!(ConversionError => format!("Failed attempting to convert an instance of a steelval into a result type: {val:?}"))
            }
        } else {
            stop!(ConversionError => format!("Failed attempting to convert an instance of a steelval into a result type - value is not a struct: {val:?}"));
        }
    }
}

// #[cfg(test)]
// mod struct_tests {

//     use super::*;

//     fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
//         let func = func
//             .boxed_func_or_else(throw!(BadSyntax => "string tests"))
//             .unwrap();

//         func(&args)
//     }

//     #[test]
//     fn constructor_normal() {
//         let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
//         let res = apply_function(constructor(Rc::from("Promise"), 2), args);
//         let expected = SteelVal::StructV(Gc::new(SteelStruct {
//             name: Rc::from("Promise"),
//             fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
//         }));
//         assert_eq!(res.unwrap(), expected)
//     }

//     #[test]
//     fn setter_position_0() {
//         let args = vec![
//             SteelVal::StructV(Gc::new(SteelStruct {
//                 name: Rc::from("Promise"),
//                 fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
//             })),
//             SteelVal::IntV(100),
//         ];

//         let res = apply_function(setter(Rc::from("Promise"), 0), args);
//         let expected = SteelVal::StructV(Gc::new(SteelStruct {
//             name: Rc::from("Promise"),
//             fields: vec![SteelVal::IntV(100), SteelVal::IntV(2)],
//         }));
//         assert_eq!(res.unwrap(), expected);
//     }

//     #[test]
//     fn setter_position_1() {
//         let args = vec![
//             SteelVal::StructV(Gc::new(SteelStruct {
//                 name: Rc::from("Promise"),
//                 fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
//             })),
//             SteelVal::IntV(100),
//         ];

//         let res = apply_function(setter(Rc::from("Promise"), 1), args);
//         let expected = SteelVal::StructV(Gc::new(SteelStruct {
//             name: Rc::from("Promise"),
//             fields: vec![SteelVal::IntV(1), SteelVal::IntV(100)],
//         }));
//         assert_eq!(res.unwrap(), expected);
//     }

//     #[test]
//     fn getter_position_0() {
//         let args = vec![SteelVal::StructV(Gc::new(SteelStruct {
//             name: Rc::from("Promise"),
//             fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
//         }))];

//         let res = apply_function(getter(Rc::from("Promise"), 0), args);
//         let expected = SteelVal::IntV(1);
//         assert_eq!(res.unwrap(), expected);
//     }
// }

// #[test]
// fn small_vec_size() {
//     println!(
//         "{:?}",
//         std::mem::size_of::<smallvec::SmallVec<[SteelVal; 5]>>()
//     );
//     println!("{:?}", std::mem::size_of::<MaybeHeapVec>())
// }
