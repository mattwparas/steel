use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::throw;
use crate::{
    env::{FALSE, TRUE, VOID},
    rvals::StructClosureSignature,
};
use std::fmt;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::parser::ast::Struct;

#[derive(Clone, Debug, PartialEq)]
pub enum StructFunctionType {
    Constructor,
    Getter(usize),
    Setter(usize),
    Predicate(String),
}

#[derive(Clone)]
pub struct StructClosure {
    pub factory: SteelStruct,
    pub func: StructClosureSignature,
}

// impl fmt::Display for StructClosure {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         // at the top level, print a ' if we are
//         // trying to print a symbol or list
//         write!("Struct function: {:?}", factory);
//     }
// }

// impl fmt::Debug for StructClosure {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         // at the top level, print a ' if we are
//         // trying to print a symbol or list
//         write!("")
//     }
// }

impl StructClosure {
    pub fn new(factory: SteelStruct, func: StructClosureSignature) -> Self {
        StructClosure { factory, func }
    }

    pub fn new_box(factory: SteelStruct, func: StructClosureSignature) -> Box<Self> {
        Box::new(Self::new(factory, func))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SteelStruct {
    name: Rc<str>,
    fields: Vec<SteelVal>,
    function_purpose: StructFunctionType,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StructFuncBuilder<'a> {
    pub name: &'a str,
    pub fields: Vec<&'a str>,
}

impl<'a> StructFuncBuilder<'a> {
    pub fn new(name: &'a str, fields: Vec<&'a str>) -> Self {
        StructFuncBuilder { name, fields }
    }

    pub fn to_struct_function_names(&self) -> Vec<String> {
        // collect the functions
        // for each field there are going to be 2 functions
        // add 2 for the constructor and the predicate
        let mut func_names = Vec::with_capacity(&self.fields.len() * 2 + 2);
        // generate constructor
        // let cons = constructor(name, field_names_as_strs.len());
        // generate predicate
        func_names.push(format!("{}?", &self.name));
        // generate getters and setters
        for field in &self.fields {
            func_names.push(format!("{}-{}", &self.name, field));
            func_names.push(format!("set-{}-{}!", &self.name, field));
        }
        func_names.push((&self.name).to_string());
        func_names
    }

    // This needs to return something that can be consumed at runtime from the constant map
    // Effectively, we need a list with the form '(name fields ...)
    // Let's make it happen
    pub fn to_constant_val(&self, indices: Vec<usize>) -> SteelVal {
        let indices: Vec<_> = indices
            .into_iter()
            .map(|x| SteelVal::IntV(x as isize))
            .collect();

        let mut name = vec![
            crate::primitives::ListOperations::built_in_list_normal_iter_non_result(
                indices.into_iter(),
            ),
            SteelVal::StringV(self.name.into()),
        ];

        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|x| SteelVal::StringV((*x).into()))
            .collect();

        name.extend(fields);

        // TODO who knows if this actually works
        crate::primitives::ListOperations::built_in_list_normal_iter_non_result(name.into_iter())
    }

    pub fn to_func_vec(&self) -> Result<Vec<(String, SteelVal)>> {
        SteelStruct::generate_from_name_fields(self.name, &self.fields)
    }
}

// Housekeeping (just in case there are cyclical references)
impl Drop for SteelStruct {
    fn drop(&mut self) {
        self.fields.clear();
    }
}

impl SteelStruct {
    pub fn new(name: Rc<str>, fields: Vec<SteelVal>, function_purpose: StructFunctionType) -> Self {
        SteelStruct {
            name,
            fields,
            function_purpose,
        }
    }

    // This will blow up the stack with a sufficiently large recursive struct
    pub fn pretty_print(&self) -> String {
        format!("{}: {:#?}", self.name, self.fields)
    }
}

impl SteelStruct {
    pub fn generate_from_ast(s: &Struct) -> Result<StructFuncBuilder> {
        let name = s.name.atom_identifier_or_else(throw!(TypeMismatch => "struct definition expected an identifier as the first argument"))?;

        let field_names_as_strs: Vec<&str> = s
            .fields
            .iter()
            .map(|x| {
                x.atom_identifier_or_else(throw!(TypeMismatch => "struct expected identifiers"))
            })
            .collect::<Result<_>>()?;

        Ok(StructFuncBuilder::new(name, field_names_as_strs))
    }

    pub fn generate_from_name_fields(
        name: &str,
        field_names_as_strs: &[&str],
    ) -> Result<Vec<(String, SteelVal)>> {
        // collect the functions
        // for each field there are going to be 2 functions
        // add 2 for the constructor and the predicate
        let mut funcs = Vec::with_capacity(field_names_as_strs.len() * 2 + 2);
        // generate constructor
        let cons = constructor(name, field_names_as_strs.len());
        funcs.push((name.to_string(), cons));
        // generate predicate
        funcs.push((format!("{}?", name), predicate(&name)));
        // generate getters and setters
        for (idx, field) in field_names_as_strs.iter().enumerate() {
            funcs.push((format!("{}-{}", name, field), getter(&name, idx)));
            funcs.push((format!("set-{}-{}!", name, field), setter(&name, idx)));
        }
        Ok(funcs)
    }
}

// initialize hashmap to be field_names -> void
// just do arity check before inserting to make sure things check out
// that way field names as a vec are no longer necessary
fn constructor(name: &str, len: usize) -> SteelVal {
    let factory: SteelStruct = SteelStruct::new(
        Rc::from(name),
        vec![SteelVal::Void; len],
        StructFunctionType::Constructor,
    );

    SteelVal::StructClosureV(Box::new(StructClosure::new(
        factory,
        |args: &[SteelVal], factory: &SteelStruct| -> Result<SteelVal> {
            if args.len() != factory.fields.len() {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    factory.name.clone(),
                    args.len(),
                    factory.fields.len()
                );
                stop!(ArityMismatch => error_message);
            }
            let mut new_struct = factory.clone();

            for (idx, arg) in args.into_iter().enumerate() {
                let key = new_struct
                    .fields
                    .get_mut(idx)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = arg.clone();
            }

            Ok(SteelVal::StructV(Gc::new(new_struct)))
        },
    )))
}

fn predicate(name: &str) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::from(name),
        Vec::new(),
        StructFunctionType::Predicate(name.to_string()),
    );
    SteelVal::StructClosureV(Box::new(StructClosure::new(
        factory,
        |args: &[SteelVal], factory: &SteelStruct| -> Result<SteelVal> {
            if args.len() != 1 {
                let error_message = format!(
                    "{}? expected one argument, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            match &args[0] {
                SteelVal::StructV(my_struct) => {
                    if let StructFunctionType::Predicate(name_huh) = &factory.function_purpose {
                        if my_struct.name.as_ref() == name_huh {
                            Ok(SteelVal::BoolV(true))
                        } else {
                            Ok(SteelVal::BoolV(false))
                        }
                    } else {
                        stop!(TypeMismatch => "something went wrong with struct predicate")
                    }
                }
                _ => Ok(SteelVal::BoolV(false)),
            }
        },
    )))
}

fn getter(name: &str, idx: usize) -> SteelVal {
    let factory = SteelStruct::new(Rc::from(name), Vec::new(), StructFunctionType::Getter(idx));
    SteelVal::StructClosureV(Box::new(StructClosure::new(
        factory,
        |args: &[SteelVal], factory: &SteelStruct| -> Result<SteelVal> {
            if args.len() != 1 {
                let error_message = format!(
                    "{} getter expected one argument, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

            if let StructFunctionType::Getter(idx) = &factory.function_purpose {
                if let Some(ret_val) = my_struct.fields.get(*idx) {
                    Ok(ret_val.clone())
                } else {
                    stop!(TypeMismatch => "Couldn't find that field in the struct")
                }
            } else {
                stop!(TypeMismatch => "something went wrong with struct getter")
            }
        },
    )))
}

fn setter(name: &str, idx: usize) -> SteelVal {
    let factory = SteelStruct::new(Rc::from(name), Vec::new(), StructFunctionType::Setter(idx));
    SteelVal::StructClosureV(Box::new(StructClosure::new(
        factory,
        |args: &[SteelVal], factory: &SteelStruct| -> Result<SteelVal> {
            if args.len() != 2 {
                let error_message = format!(
                    "{} setter expected two arguments, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;
            let value = args[1].clone();

            if let StructFunctionType::Setter(idx) = &factory.function_purpose {
                let mut new_struct = my_struct.clone();
                let key = new_struct
                    .fields
                    .get_mut(*idx)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = value;
                Ok(SteelVal::StructV(Gc::new(new_struct)))
            } else {
                stop!(TypeMismatch => "something went wrong with struct setter")
            }
        },
    )))
}

#[cfg(test)]
mod struct_tests {

    use super::*;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        let (factory, func) = func
            .struct_func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap();

        func(&args, factory)
    }

    #[test]
    fn constructor_normal() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = apply_function(constructor("Promise", 2), args);
        let expected = SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected)
    }

    #[test]
    fn setter_position_0() {
        let args = vec![
            SteelVal::StructV(Gc::new(SteelStruct {
                name: Rc::from("Promise"),
                fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
                function_purpose: StructFunctionType::Constructor,
            })),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter("Promise", 0), args);
        let expected = SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(100), SteelVal::IntV(2)],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn setter_position_1() {
        let args = vec![
            SteelVal::StructV(Gc::new(SteelStruct {
                name: Rc::from("Promise"),
                fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
                function_purpose: StructFunctionType::Constructor,
            })),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter("Promise", 1), args);
        let expected = SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(1), SteelVal::IntV(100)],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn getter_position_0() {
        let args = vec![SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
            function_purpose: StructFunctionType::Constructor,
        }))];

        let res = apply_function(getter("Promise", 0), args);
        let expected = SteelVal::IntV(1);
        assert_eq!(res.unwrap(), expected);
    }
}
