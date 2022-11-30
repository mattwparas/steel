use crate::{core::utils::Boxed, rvals::MAGIC_STRUCT_SYMBOL};
use crate::{
    gc::Gc,
    rvals::{AsRefSteelVal, SRef, SteelString},
    steel_vm::register_fn::RegisterFn,
};
use crate::{rvals::Custom, throw};
use crate::{
    rvals::{Result, SteelVal},
    SteelErr,
};
use crate::{steel_vm::builtin::BuiltInModule, stop};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub struct UserDefinedStruct {
    pub(crate) name: Rc<String>,
    pub(crate) fields: MaybeHeapVec,
    pub(crate) len: usize,
    pub(crate) properties: Gc<im_rc::HashMap<SteelVal, SteelVal>>,
}

impl AsRefSteelVal for UserDefinedStruct {
    fn as_ref<'b, 'a: 'b>(val: &'a SteelVal) -> Result<SRef<'b, Self>> {
        if let SteelVal::CustomStruct(s) = val {
            Ok(SRef::Temporary(s))
        } else {
            stop!(TypeMismatch => format!("Value cannot be referenced as a struct: {}", val))
        }
    }
}

impl std::fmt::Display for UserDefinedStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self
            .properties
            .get(&SteelVal::SymbolV(SteelString::from("#:transparent")))
            .is_some()
        {
            match &self.fields {
                MaybeHeapVec::Unit => write!(f, "({})", self.name),
                MaybeHeapVec::One(v) => write!(f, "({} {})", self.name, v.borrow()),
                MaybeHeapVec::Two(a) => {
                    let guard = a.borrow();
                    write!(f, "({} {} {})", self.name, guard[0], guard[1])
                }
                MaybeHeapVec::Three(a) => {
                    let guard = a.borrow();
                    write!(f, "({} {} {} {})", self.name, guard[0], guard[1], guard[2])
                }
                MaybeHeapVec::Four(a) => {
                    let guard = a.borrow();
                    write!(
                        f,
                        "({} {} {} {} {})",
                        self.name, guard[0], guard[1], guard[2], guard[3]
                    )
                }
                MaybeHeapVec::Five(a) => {
                    let guard = a.borrow();
                    write!(
                        f,
                        "({} {} {} {} {} {})",
                        self.name, guard[0], guard[1], guard[2], guard[3], guard[4]
                    )
                }
                MaybeHeapVec::Spilled(v) => {
                    let guard = v.borrow();
                    write!(f, "({})", self.name)?;
                    for item in guard.iter() {
                        write!(f, "{}", item)?;
                    }
                    write!(f, ")")
                }
            }
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
    One(Rc<RefCell<SteelVal>>),
    Two(Rc<RefCell<[SteelVal; 2]>>),
    Three(Rc<RefCell<[SteelVal; 3]>>),
    Four(Rc<RefCell<[SteelVal; 4]>>),
    Five(Rc<RefCell<[SteelVal; 5]>>),
    Spilled(Rc<RefCell<Vec<SteelVal>>>),
}

impl MaybeHeapVec {
    pub fn from_slice(args: &[SteelVal]) -> Self {
        match &args {
            &[] => Self::Unit,
            &[one] => Self::One(one.clone().rc_refcell()),
            &[one, two] => Self::Two([one.clone(), two.clone()].rc_refcell()),
            &[one, two, three] => {
                Self::Three([one.clone(), two.clone(), three.clone()].rc_refcell())
            }
            &[one, two, three, four] => {
                Self::Four([one.clone(), two.clone(), three.clone(), four.clone()].rc_refcell())
            }
            &[one, two, three, four, five] => Self::Five(
                [
                    one.clone(),
                    two.clone(),
                    three.clone(),
                    four.clone(),
                    five.clone(),
                ]
                .rc_refcell(),
            ),
            _ => Self::Spilled(Rc::new(RefCell::new(args.iter().cloned().collect()))),
        }
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<SteelVal> {
        match self {
            MaybeHeapVec::Unit => None,
            MaybeHeapVec::One(v) => Some(v.borrow().clone()),
            MaybeHeapVec::Two(a) => Some(a.borrow()[index].clone()),
            MaybeHeapVec::Three(a) => Some(a.borrow()[index].clone()),
            MaybeHeapVec::Four(a) => Some(a.borrow()[index].clone()),
            MaybeHeapVec::Five(a) => Some(a.borrow()[index].clone()),
            MaybeHeapVec::Spilled(a) => Some(a.borrow()[index].clone()),
        }
    }

    #[inline(always)]
    pub fn set(&self, index: usize, mut value: SteelVal) -> SteelVal {
        match self {
            MaybeHeapVec::Unit => panic!("Tried to get the 0th index of a unit struct"),
            MaybeHeapVec::One(v) => {
                let mut guard = v.borrow_mut();
                let old = guard.clone();
                *guard = value;
                old
            }
            MaybeHeapVec::Two(a) => {
                let mut guard = a.borrow_mut();
                std::mem::swap(&mut guard[index], &mut value);
                value
            }
            MaybeHeapVec::Three(a) => {
                let mut guard = a.borrow_mut();
                std::mem::swap(&mut guard[index], &mut value);
                value
            }
            MaybeHeapVec::Four(a) => {
                let mut guard = a.borrow_mut();
                std::mem::swap(&mut guard[index], &mut value);
                value
            }
            MaybeHeapVec::Five(a) => {
                let mut guard = a.borrow_mut();
                std::mem::swap(&mut guard[index], &mut value);
                value
            }
            MaybeHeapVec::Spilled(a) => {
                let mut guard = a.borrow_mut();
                std::mem::swap(&mut guard[index], &mut value);
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
        match &args {
            &[] => Self::Unit,
            &[one] => Self::One(one.clone()),
            &[one, two] => Self::Two(Rc::new([one.clone(), two.clone()])),
            &[one, two, three] => Self::Three(Rc::new([one.clone(), two.clone(), three.clone()])),
            &[one, two, three, four] => Self::Four(Rc::new([
                one.clone(),
                two.clone(),
                three.clone(),
                four.clone(),
            ])),
            &[one, two, three, four, five] => Self::Five(Rc::new([
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
    fn new(name: Rc<String>, fields: &[SteelVal]) -> Result<Self> {
        let (options, rest) = fields.split_first().ok_or_else(
            throw!(ArityMismatch => "struct constructor expects at least one argument"),
        )?;

        if let SteelVal::HashMapV(properties) = options.clone() {
            Ok(Self {
                name,
                fields: MaybeHeapVec::from_slice(rest),
                len: fields.len(),
                properties,
            })
        } else {
            stop!(TypeMismatch => format!("struct constructor expected a hashmap, found: {}", options))
        }
    }

    fn constructor(name: Rc<String>, len: usize) -> SteelVal {
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

            let new_struct = UserDefinedStruct::new(Rc::clone(&name), args)?;

            Ok(SteelVal::CustomStruct(Gc::new(new_struct)))
        };

        SteelVal::BoxedFunction(Box::new(Rc::new(f)))
    }

    fn predicate(name: Rc<String>) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                let error_message =
                    format!("{}? expected one argument, found {}", name, args.len());
                stop!(ArityMismatch => error_message);
            }
            Ok(SteelVal::BoolV(match &args[0] {
                SteelVal::CustomStruct(my_struct) if Rc::ptr_eq(&my_struct.name, &name) => true,
                // SteelVal::CustomStruct(my_struct) if my_struct.name == name => true,
                _ => false,
            }))
        };

        SteelVal::BoxedFunction(Box::new(Rc::new(f)))
    }

    fn getter_prototype(name: Rc<String>) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "struct-ref expected two arguments");
            }

            let steel_struct = &args[0].clone();
            let idx = &args[1].clone();

            match (&steel_struct, &idx) {
                (SteelVal::CustomStruct(s), SteelVal::IntV(idx)) => {
                    if !Rc::ptr_eq(&s.name, &name) {
                        stop!(TypeMismatch => format!("Struct getter expected {}, found {:p}, {:?}", name, &s, &steel_struct));
                    }

                    if *idx < 0 {
                        stop!(Generic => "struct-ref expected a non negative index");
                    }

                    s.fields
                        .get(*idx as usize)
                        .ok_or_else(throw!(Generic => "struct-ref: index out of bounds"))
                }
                _ => {
                    let error_message = format!(
                        "struct-ref expected a struct and an int, found: {} and {}",
                        steel_struct, idx
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Box::new(Rc::new(f)))
    }

    fn getter_prototype_index(name: Rc<String>, index: usize) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "struct-ref expected two arguments");
            }

            let steel_struct = &args[0].clone();

            match &steel_struct {
                SteelVal::CustomStruct(s) => {
                    if !Rc::ptr_eq(&s.name, &name) {
                        stop!(TypeMismatch => format!("Struct getter expected {}, found {:p}, {:?}", name, &s, &steel_struct));
                    }

                    s.fields
                        .get(index)
                        .ok_or_else(throw!(Generic => "struct-ref: index out of bounds"))
                }
                _ => {
                    let error_message = format!(
                        "struct-ref expected a struct and an int, found: {} and {}",
                        steel_struct, index
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Box::new(Rc::new(f)))
    }

    fn setter_prototype(name: Rc<String>) -> SteelVal {
        let f = move |args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "struct-ref expected two arguments");
            }

            let steel_struct = &args[0].clone();
            let idx = &args[1].clone();
            let arg = &args[2].clone();

            match (&steel_struct, &idx) {
                (SteelVal::CustomStruct(s), SteelVal::IntV(idx)) => {
                    if !Rc::ptr_eq(&s.name, &name) {
                        stop!(TypeMismatch => format!("Struct setter expected {}, found {}", name, &s.name));
                    }

                    if *idx < 0 {
                        stop!(Generic => "struct-ref expected a non negative index");
                    }
                    if *idx as usize >= s.len {
                        stop!(Generic => "struct-ref: index out of bounds");
                    }

                    // if let StructBacking::Mutable(m) = s.fields {
                    Ok(s.fields.set(0, arg.clone()))
                    // } else {
                    // stop!(TypeMismatch => "attempted to set an immutable struct fields")
                    // }

                    // Ok(s.fields[*idx as usize].clone())

                    // s.fields.borrow_mut()[*idx as usize] = arg.clone();
                }
                _ => {
                    let error_message = format!(
                        "struct-ref expected a struct and an int, found: {} and {}",
                        steel_struct, idx
                    );
                    stop!(TypeMismatch => error_message)
                }
            }
        };

        SteelVal::BoxedFunction(Box::new(Rc::new(f)))
    }

    pub fn properties(&self) -> SteelVal {
        SteelVal::HashMapV(self.properties.clone())
    }
}

pub fn make_struct_type(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "make-struct-type expects 2 args, found: {}", args.len())
    }

    let name = if let SteelVal::SymbolV(s) = &args[0] {
        Ok::<_, SteelErr>(s)
    } else {
        stop!(TypeMismatch => format!("make-struct-type expected a symbol for the name, found: {}", &args[0]));
    }?;

    let field_count = if let SteelVal::IntV(i) = &args[1] {
        Ok::<_, SteelErr>(i)
    } else {
        stop!(TypeMismatch => format!("make-struct-type expected an integer for the field count, found: {}", &args[0]));
    }?;

    // Build out the constructor and the predicate
    let struct_constructor = UserDefinedStruct::constructor(Rc::clone(name), *field_count as usize);
    let struct_predicate = UserDefinedStruct::predicate(Rc::clone(name));

    let getter_prototype = UserDefinedStruct::getter_prototype(Rc::clone(name));
    let setter_prototype = UserDefinedStruct::setter_prototype(Rc::clone(name));

    Ok(SteelVal::ListV(im_lists::list![
        struct_constructor,
        struct_predicate,
        getter_prototype,
        setter_prototype
    ]))
}

#[derive(Debug, Clone)]
struct AlternativeOk(SteelVal);

impl AlternativeOk {
    pub fn new(value: SteelVal) -> Self {
        Self(value)
    }

    pub fn value(&self) -> SteelVal {
        self.0.clone()
    }
}

impl Custom for AlternativeOk {}

#[derive(Debug, Clone)]
struct AlternativeErr(SteelVal);

impl AlternativeErr {
    pub fn new(value: SteelVal) -> Self {
        Self(value)
    }

    pub fn value(&self) -> SteelVal {
        self.0.clone()
    }
}

impl Custom for AlternativeErr {}

pub(crate) fn build_result_structs() -> BuiltInModule {
    // Build module
    let mut module = BuiltInModule::new("steel/result".to_string());

    {
        let name = Rc::new("Ok".into());

        // Don't put any options in it?
        let constructor = UserDefinedStruct::constructor(Rc::clone(&name), 1);
        let predicate = UserDefinedStruct::predicate(Rc::clone(&name));

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(Rc::clone(&name), 0);

        module
            .register_value("Ok", constructor)
            .register_value("Ok?", predicate)
            .register_value("Ok->value", getter);
    }

    {
        let name = Rc::new("Err".into());
        let constructor = UserDefinedStruct::constructor(Rc::clone(&name), 1);
        let predicate = UserDefinedStruct::predicate(Rc::clone(&name));

        // Build the getter for the first index
        let getter = UserDefinedStruct::getter_prototype_index(Rc::clone(&name), 0);

        module
            .register_value("Err", constructor)
            .register_value("Err?", predicate)
            .register_value("Err->value", getter);
    }

    module
        .register_type::<AlternativeOk>("Alternative-Ok?")
        .register_type::<AlternativeErr>("Alternative-Err?")
        .register_fn("Alternative-Ok", AlternativeOk::new)
        .register_fn("Alternative-Err", AlternativeErr::new)
        .register_fn("Alternative-Ok->value", AlternativeOk::value)
        .register_fn("Alternative-Err->value", AlternativeErr::value);

    module
}

pub(crate) fn is_custom_struct() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 1 {
            stop!(ArityMismatch => "struct? expected one argument");
        }

        let steel_struct = &args[0].clone();

        if let SteelVal::MutableVector(v) = &steel_struct {
            if let Some(magic_value) = v.borrow().get(0) {
                Ok(SteelVal::BoolV(
                    magic_value.ptr_eq(&MAGIC_STRUCT_SYMBOL.with(|x| x.clone())),
                ))
            } else {
                Ok(SteelVal::BoolV(false))
            }
        } else {
            Ok(SteelVal::BoolV(false))
        }
    })
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
