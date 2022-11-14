use crate::{gc::Gc, parser::ast::ExprKind, steel_vm::register_fn::RegisterFn};
use crate::{primitives::VectorOperations, rvals::MAGIC_STRUCT_SYMBOL};
use crate::{rvals::Custom, throw};
use crate::{
    rvals::{Result, SteelVal},
    SteelErr,
};
use crate::{steel_vm::builtin::BuiltInModule, stop};
use std::{cell::RefCell, fmt::write, rc::Rc};

use im_rc::hashmap;
use serde::{Deserialize, Serialize};

use crate::parser::ast::Struct;

/*
A vtable would need to be consulted to understand how to go from struct instance -> interface invocation

That could easily live in the VM state -> consulting the VM context for class information would be like:

(define-interface
    (define method1)
    (define method2)
    (define method3))

(implement interface for StructName
    (define method1 ...)
    (define method2 ...)
    (define method3 ...))

Register struct/class definition with the runtime, consult the vtable to map the method name + struct -> method call

functions for structs should be registered separately -> function name + struct together

TODO: structs should actually just be implemented as vectors in Steel
Make a defmacro style macro that expands into the necessary code to do so

Also attempt to create wrapper object that obfuscates the type of an object, so that vector
operations don't work explicitly on structs

i.e. Implement a tagged pointer to delineate the type of the object for the struct
its important that structs are treated differently than vectors on their own

*/

#[derive(Clone, Debug)]
pub struct UserDefinedStruct {
    pub(crate) name: Rc<str>,
    pub(crate) fields: Rc<RefCell<Vec<SteelVal>>>,
}

// impl std::fmt::Display for UserDefinedStruct {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write(f, "UserDefinedStruct {}", )
//     }
// }

impl UserDefinedStruct {
    fn new(name: Rc<str>, fields: Rc<RefCell<Vec<SteelVal>>>) -> Self {
        Self { name, fields }
    }

    fn constructor(name: Rc<str>, len: usize) -> SteelVal {
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

            let fields = args.into_iter().cloned().collect::<Vec<_>>();

            let new_struct =
                UserDefinedStruct::new(Rc::clone(&name), Rc::new(RefCell::new(fields)));

            Ok(SteelVal::CustomStruct(Gc::new(new_struct)))
        };

        SteelVal::BoxedFunction(Rc::new(f))
    }

    fn predicate(name: Rc<str>) -> SteelVal {
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

        SteelVal::BoxedFunction(Rc::new(f))
    }

    fn getter_prototype(name: Rc<str>) -> SteelVal {
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
                    if *idx as usize >= s.fields.borrow().len() {
                        stop!(Generic => "struct-ref: index out of bounds");
                    }
                    Ok(s.fields.borrow()[*idx as usize].clone())
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

        SteelVal::BoxedFunction(Rc::new(f))
    }

    fn getter_prototype_index(name: Rc<str>, index: usize) -> SteelVal {
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

                    Ok(s.fields.borrow()[index as usize].clone())
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

        SteelVal::BoxedFunction(Rc::new(f))
    }

    fn setter_prototype(name: Rc<str>) -> SteelVal {
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
                    if *idx as usize >= s.fields.borrow().len() {
                        stop!(Generic => "struct-ref: index out of bounds");
                    }

                    let old_value = {
                        let mut guard = s.fields.borrow_mut();
                        let old = guard[*idx as usize].clone();
                        guard[*idx as usize] = arg.clone();
                        old
                    };

                    Ok(old_value)
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

        SteelVal::BoxedFunction(Rc::new(f))
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
        let name = Rc::from("Ok");

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
        let name = Rc::from("Err");
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

// / An instance of an immutable struct in Steel
// / In order to override the display of this struct, the struct definition would need to have
// / a generic marker on it to call with the value
// #[derive(Clone, Debug, PartialEq)]
// pub struct SteelStruct {
//     pub(crate) name: Rc<str>,
//     pub(crate) fields: Vec<SteelVal>,
// }

// impl SteelStruct {
//     pub fn iter(&self) -> impl Iterator<Item = &SteelVal> {
//         self.fields.iter()
//     }
// }

// pub struct StructBuilders {
//     pub builders: Vec<StructFuncBuilderConcrete>,
// }

// impl StructBuilders {
//     pub fn new() -> Self {
//         Self {
//             builders: Vec::new(),
//         }
//     }

//     pub fn extract_structs_for_executable(
//         &mut self,
//         exprs: Vec<ExprKind>,
//     ) -> Result<Vec<ExprKind>> {
//         let mut non_structs = Vec::new();
//         // let mut struct_instructions = Vec::new();
//         for expr in exprs {
//             if let ExprKind::Struct(s) = expr {
//                 let builder = StructFuncBuilder::generate_from_ast(&s)?.into_concrete();

//                 self.builders.push(builder);

//                 // // Add the eventual function names to the symbol map
//                 // let indices = self.symbol_map.insert_struct_function_names(&builder);

//                 // // Get the value we're going to add to the constant map for eventual use
//                 // // Throw the bindings in as well
//                 // let constant_values = builder.to_constant_val(indices);
//                 // let idx = self.constant_map.add_or_get(constant_values);

//                 // struct_instructions
//                 //     .push(vec![Instruction::new_struct(idx), Instruction::new_pop()]);
//             } else {
//                 non_structs.push(expr);
//             }
//         }

//         // for instruction_set in struct_instructions {
//         //     results.push(instruction_set)
//         // }

//         Ok(non_structs)
//     }
// }

// #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
// pub struct StructFuncBuilderConcrete {
//     pub name: String,
//     pub fields: Vec<String>,
// }

// impl StructFuncBuilderConcrete {
//     pub fn new(name: String, fields: Vec<String>) -> Self {
//         Self { name, fields }
//     }

//     pub fn to_struct_function_names(&self) -> Vec<String> {
//         StructFuncBuilder::new(&self.name, self.fields.iter().map(|x| x.as_str()).collect())
//             .to_struct_function_names()
//     }

//     pub fn to_constant_val(&self, indices: Vec<usize>) -> SteelVal {
//         StructFuncBuilder::new(&self.name, self.fields.iter().map(|x| x.as_str()).collect())
//             .to_constant_val(indices)
//     }

//     // pub fn
// }

// #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
// pub struct StructFuncBuilder<'a> {
//     pub name: &'a str,
//     pub fields: Vec<&'a str>,
// }

// impl<'a> StructFuncBuilder<'a> {
//     pub fn generate_from_ast(s: &Struct) -> Result<StructFuncBuilder> {
//         let name = s.name.atom_identifier_or_else(throw!(TypeMismatch => "struct definition expected an identifier as the first argument"))?;

//         let field_names_as_strs: Vec<&str> = s
//             .fields
//             .iter()
//             .map(|x| {
//                 x.atom_identifier_or_else(throw!(TypeMismatch => "struct expected identifiers"))
//             })
//             .collect::<Result<_>>()?;

//         Ok(StructFuncBuilder::new(name, field_names_as_strs))
//     }

//     pub fn into_concrete(self) -> StructFuncBuilderConcrete {
//         StructFuncBuilderConcrete::new(
//             self.name.to_string(),
//             self.fields.into_iter().map(|x| x.to_string()).collect(),
//         )
//     }

//     pub fn new(name: &'a str, fields: Vec<&'a str>) -> Self {
//         StructFuncBuilder { name, fields }
//     }

//     pub fn to_struct_function_names(&self) -> Vec<String> {
//         // collect the functions
//         // for each field there are going to be 2 functions
//         // add 2 for the constructor and the predicate
//         let mut func_names = Vec::with_capacity(&self.fields.len() * 2 + 2);
//         // generate constructor
//         // let cons = constructor(name, field_names_as_strs.len());
//         // generate predicate
//         func_names.push((&self.name).to_string());
//         func_names.push(format!("{}?", &self.name));
//         // generate getters and setters
//         for field in &self.fields {
//             func_names.push(format!("{}-{}", &self.name, field));
//             func_names.push(format!("set-{}-{}!", &self.name, field));
//         }
//         func_names
//     }

//     // This needs to return something that can be consumed at runtime from the constant map
//     // Effectively, we need a list with the form '(name fields ...)
//     // Let's make it happen
//     pub fn to_constant_val(&self, indices: Vec<usize>) -> SteelVal {
//         let indices: Vec<_> = indices
//             .into_iter()
//             .map(|x| SteelVal::IntV(x as isize))
//             .collect();

//         let mut name = vec![
//             SteelVal::ListV(indices.into_iter().collect()),
//             SteelVal::StringV(self.name.into()),
//         ];

//         let fields: Vec<_> = self
//             .fields
//             .iter()
//             .map(|x| SteelVal::StringV((*x).into()))
//             .collect();

//         name.extend(fields);

//         // TODO who knows if this actually works
//         SteelVal::ListV(name.into())
//     }

//     pub fn to_func_vec(&self) -> Result<Vec<(String, SteelVal)>> {
//         SteelStruct::generate_from_name_fields(self.name, &self.fields)
//     }
// }

// impl SteelStruct {
//     pub fn new(name: Rc<str>, fields: Vec<SteelVal>) -> Self {
//         SteelStruct { name, fields }
//     }

//     // This will blow up the stack with a sufficiently large recursive struct
//     pub fn pretty_print(&self) -> String {
//         format!("{}", self.name)
//     }
// }

// impl SteelStruct {
//     pub fn generate_from_name_fields(
//         name: &str,
//         field_names_as_strs: &[&str],
//     ) -> Result<Vec<(String, SteelVal)>> {
//         // collect the functions
//         // for each field there are going to be 2 functions
//         // add 2 for the constructor and the predicate
//         let mut funcs = Vec::with_capacity(field_names_as_strs.len() * 2 + 2);
//         let name = Rc::from(name);
//         // generate constructor
//         let cons = constructor(Rc::clone(&name), field_names_as_strs.len());
//         funcs.push((name.to_string(), cons));
//         // generate predicate
//         funcs.push((format!("{}?", name), predicate(Rc::clone(&name))));
//         // generate getters and setters
//         for (idx, field) in field_names_as_strs.iter().enumerate() {
//             funcs.push((format!("{}-{}", name, field), getter(Rc::clone(&name), idx)));
//             funcs.push((
//                 format!("set-{}-{}!", name, field),
//                 setter(Rc::clone(&name), idx),
//             ));
//         }
//         Ok(funcs)
//     }
// }

// initialize hashmap to be field_names -> void
// just do arity check before inserting to make sure things check out
// that way field names as a vec are no longer necessary
// fn constructor(name: Rc<str>, len: usize) -> SteelVal {
//     let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != len {
//             let error_message = format!(
//                 "{} expected {} arguments, found {}",
//                 name.clone(),
//                 args.len(),
//                 len
//             );
//             stop!(ArityMismatch => error_message);
//         }

//         let mut new_struct = SteelStruct::new(Rc::clone(&name), vec![SteelVal::Void; len]);

//         for (idx, arg) in args.iter().enumerate() {
//             let key = new_struct
//                 .fields
//                 .get_mut(idx)
//                 .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
//             *key = arg.clone();
//         }

//         Ok(SteelVal::StructV(Gc::new(new_struct)))
//     };

//     SteelVal::BoxedFunction(Rc::new(f))
// }

// fn predicate(name: Rc<str>) -> SteelVal {
//     let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 1 {
//             let error_message = format!("{}? expected one argument, found {}", name, args.len());
//             stop!(ArityMismatch => error_message);
//         }
//         Ok(SteelVal::BoolV(match &args[0] {
//             SteelVal::StructV(my_struct) if my_struct.name.as_ref() == name.as_ref() => true,
//             _ => false,
//         }))
//     };

//     SteelVal::BoxedFunction(Rc::new(f))
// }

// fn getter(name: Rc<str>, idx: usize) -> SteelVal {
//     let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 1 {
//             let error_message = format!(
//                 "{} getter expected one argument, found {}",
//                 name,
//                 args.len()
//             );
//             stop!(ArityMismatch => error_message);
//         }

//         let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

//         if &my_struct.name != &name {
//             stop!(TypeMismatch => format!("Struct getter expected {}, found {}", name, &my_struct.name));
//         }

//         if let Some(ret_val) = my_struct.fields.get(idx) {
//             Ok(ret_val.clone())
//         } else {
//             stop!(TypeMismatch => "Couldn't find that field in the struct")
//         }
//     };

//     SteelVal::BoxedFunction(Rc::new(f))
// }

// fn setter(name: Rc<str>, idx: usize) -> SteelVal {
//     let f = move |args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 2 {
//             let error_message = format!(
//                 "{} setter expected two arguments, found {}",
//                 name,
//                 args.len()
//             );
//             stop!(ArityMismatch => error_message);
//         }

//         let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

//         if &my_struct.name != &name {
//             stop!(TypeMismatch => format!("Struct setter expected {}, found {}", name, &my_struct.name));
//         }

//         let value = args[1].clone();

//         let mut new_struct = my_struct.clone();
//         let key = new_struct
//             .fields
//             .get_mut(idx)
//             .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
//         *key = value;
//         Ok(SteelVal::StructV(Gc::new(new_struct)))
//     };

//     SteelVal::BoxedFunction(Rc::new(f))
// }

// pub fn struct_ref() -> SteelVal {
//     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 2 {
//             stop!(ArityMismatch => "struct-ref expected two arguments");
//         }

//         let steel_struct = &args[0].clone();
//         let idx = &args[1].clone();

//         match (&steel_struct, &idx) {
//             (SteelVal::StructV(s), SteelVal::IntV(idx)) => {
//                 if *idx < 0 {
//                     stop!(Generic => "struct-ref expected a non negative index");
//                 }
//                 if *idx as usize >= s.fields.len() {
//                     stop!(Generic => "struct-ref: index out of bounds");
//                 }
//                 Ok(s.fields[*idx as usize].clone())
//             }
//             _ => {
//                 let error_message = format!(
//                     "struct-ref expected a struct and an int, found: {} and {}",
//                     steel_struct, idx
//                 );
//                 stop!(TypeMismatch => error_message)
//             }
//         }
//     })
// }

// pub fn struct_to_list() -> SteelVal {
//     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 1 {
//             stop!(ArityMismatch => "struct->list expected one argument");
//         }

//         let steel_struct = &args[0].clone();

//         if let SteelVal::StructV(s) = &steel_struct {
//             let name = SteelVal::SymbolV(s.name.to_string().into());

//             Ok(SteelVal::ListV(
//                 std::iter::once(name)
//                     .chain(s.fields.iter().cloned())
//                     .collect(),
//             ))
//         } else {
//             let e = format!("struct->list expected a struct, found: {}", steel_struct);
//             stop!(TypeMismatch => e);
//         }
//     })
// }

// pub fn struct_to_vector() -> SteelVal {
//     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 1 {
//             stop!(ArityMismatch => "struct->list expected one argument");
//         }

//         let steel_struct = &args[0].clone();

//         if let SteelVal::StructV(s) = &steel_struct {
//             let name = SteelVal::SymbolV(s.name.to_string().into());
//             VectorOperations::vec_construct_iter_normal(
//                 vec![name].into_iter().chain(s.fields.iter().cloned()),
//             )
//         } else {
//             let e = format!("struct->list expected a struct, found: {}", steel_struct);
//             stop!(TypeMismatch => e);
//         }
//     })
// }

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

// pub(crate) fn get_struct_name() -> SteelVal {
//     SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
//         if args.len() != 1 {
//             stop!(ArityMismatch => "get-struct-name? expected one argument");
//         }

//         let steel_struct = &args[0].clone();

//         if let SteelVal::MutableVector(v) = &steel_struct {
//             if let Some(magic_value) = v.borrow().get(0) {
//                 Ok(SteelVal::BoolV(
//                     magic_value.ptr_eq(&MAGIC_STRUCT_SYMBOL.with(|x| x.clone())),
//                 ))
//             } else {
//                 Ok(SteelVal::BoolV(false))
//             }
//         } else {
//             let e = format!("struct? expected a struct, found: {}", steel_struct);
//             stop!(TypeMismatch => e);
//         }
//     })
// }

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
