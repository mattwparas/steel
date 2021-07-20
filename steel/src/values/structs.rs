use crate::gc::Gc;
use crate::primitives::ListOperations;
use crate::primitives::VectorOperations;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::throw;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::parser::ast::Struct;

#[derive(Clone, Debug, PartialEq)]
pub struct SteelStruct {
    name: Rc<str>,
    fields: Vec<SteelVal>,
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
        func_names.push((&self.name).to_string());
        func_names.push(format!("{}?", &self.name));
        // generate getters and setters
        for field in &self.fields {
            func_names.push(format!("{}-{}", &self.name, field));
            func_names.push(format!("set-{}-{}!", &self.name, field));
        }
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

// // Housekeeping (just in case there are cyclical references)
// impl Drop for SteelStruct {
//     fn drop(&mut self) {
//         self.fields.clear();
//     }
// }

impl SteelStruct {
    pub fn new(name: Rc<str>, fields: Vec<SteelVal>) -> Self {
        SteelStruct { name, fields }
    }

    // This will blow up the stack with a sufficiently large recursive struct
    pub fn pretty_print(&self) -> String {
        format!("{}", self.name)
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
        let name = Rc::from(name);
        // generate constructor
        let cons = constructor(Rc::clone(&name), field_names_as_strs.len());
        funcs.push((name.to_string(), cons));
        // generate predicate
        funcs.push((format!("{}?", name), predicate(Rc::clone(&name))));
        // generate getters and setters
        for (idx, field) in field_names_as_strs.iter().enumerate() {
            funcs.push((format!("{}-{}", name, field), getter(Rc::clone(&name), idx)));
            funcs.push((
                format!("set-{}-{}!", name, field),
                setter(Rc::clone(&name), idx),
            ));
        }
        Ok(funcs)
    }
}

// initialize hashmap to be field_names -> void
// just do arity check before inserting to make sure things check out
// that way field names as a vec are no longer necessary
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

        let mut new_struct = SteelStruct::new(Rc::clone(&name), vec![SteelVal::Void; len]);

        for (idx, arg) in args.iter().enumerate() {
            let key = new_struct
                .fields
                .get_mut(idx)
                .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
            *key = arg.clone();
        }

        Ok(SteelVal::StructV(Gc::new(new_struct)))
    };

    SteelVal::BoxedFunction(Rc::new(f))
}

fn predicate(name: Rc<str>) -> SteelVal {
    let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 1 {
            let error_message = format!("{}? expected one argument, found {}", name, args.len());
            stop!(ArityMismatch => error_message);
        }
        match &args[0] {
            SteelVal::StructV(my_struct) => {
                if my_struct.name.as_ref() == name.as_ref() {
                    Ok(SteelVal::BoolV(true))
                } else {
                    Ok(SteelVal::BoolV(false))
                }
            }
            _ => Ok(SteelVal::BoolV(false)),
        }
    };

    SteelVal::BoxedFunction(Rc::new(f))
}

fn getter(name: Rc<str>, idx: usize) -> SteelVal {
    let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 1 {
            let error_message = format!(
                "{} getter expected one argument, found {}",
                name,
                args.len()
            );
            stop!(ArityMismatch => error_message);
        }

        let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

        if &my_struct.name != &name {
            stop!(TypeMismatch => format!("Struct getter expected {}, found {}", name, &my_struct.name));
        }

        if let Some(ret_val) = my_struct.fields.get(idx) {
            Ok(ret_val.clone())
        } else {
            stop!(TypeMismatch => "Couldn't find that field in the struct")
        }
    };

    SteelVal::BoxedFunction(Rc::new(f))
}

fn setter(name: Rc<str>, idx: usize) -> SteelVal {
    let f = move |args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 2 {
            let error_message = format!(
                "{} setter expected two arguments, found {}",
                name,
                args.len()
            );
            stop!(ArityMismatch => error_message);
        }

        let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

        if &my_struct.name != &name {
            stop!(TypeMismatch => format!("Struct setter expected {}, found {}", name, &my_struct.name));
        }

        let value = args[1].clone();

        let mut new_struct = my_struct.clone();
        let key = new_struct
            .fields
            .get_mut(idx)
            .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
        *key = value;
        Ok(SteelVal::StructV(Gc::new(new_struct)))
    };

    SteelVal::BoxedFunction(Rc::new(f))
}

pub fn struct_ref() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 2 {
            stop!(ArityMismatch => "struct-ref expected two arguments");
        }

        let steel_struct = &args[0].clone();
        let idx = &args[1].clone();

        match (&steel_struct, &idx) {
            (SteelVal::StructV(s), SteelVal::IntV(idx)) => {
                if *idx < 0 {
                    stop!(Generic => "struct-ref expected a non negative index");
                }
                if *idx as usize >= s.fields.len() {
                    stop!(Generic => "struct-ref: index out of bounds");
                }
                Ok(s.fields[*idx as usize].clone())
            }
            _ => {
                let error_message = format!(
                    "struct-ref expected a struct and an int, found: {} and {}",
                    steel_struct, idx
                );
                stop!(TypeMismatch => error_message)
            }
        }
    })
}

pub fn struct_to_list() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 1 {
            stop!(ArityMismatch => "struct->list expected one argument");
        }

        let steel_struct = &args[0].clone();

        if let SteelVal::StructV(s) = &steel_struct {
            let name = SteelVal::SymbolV(s.name.to_string().into());

            Ok(ListOperations::built_in_list_normal_iter_non_result(
                vec![name].into_iter().chain(s.fields.iter().cloned()),
            ))
        } else {
            let e = format!("struct->list expected a struct, found: {}", steel_struct);
            stop!(TypeMismatch => e);
        }
    })
}

pub fn struct_to_vector() -> SteelVal {
    SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
        if args.len() != 1 {
            stop!(ArityMismatch => "struct->list expected one argument");
        }

        let steel_struct = &args[0].clone();

        if let SteelVal::StructV(s) = &steel_struct {
            let name = SteelVal::SymbolV(s.name.to_string().into());
            VectorOperations::vec_construct_iter_normal(
                vec![name].into_iter().chain(s.fields.iter().cloned()),
            )
        } else {
            let e = format!("struct->list expected a struct, found: {}", steel_struct);
            stop!(TypeMismatch => e);
        }
    })
}

#[cfg(test)]
mod struct_tests {

    use super::*;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        let func = func
            .boxed_func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap();

        func(&args)
    }

    #[test]
    fn constructor_normal() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = apply_function(constructor(Rc::from("Promise"), 2), args);
        let expected = SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
        }));
        assert_eq!(res.unwrap(), expected)
    }

    #[test]
    fn setter_position_0() {
        let args = vec![
            SteelVal::StructV(Gc::new(SteelStruct {
                name: Rc::from("Promise"),
                fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
            })),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter(Rc::from("Promise"), 0), args);
        let expected = SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(100), SteelVal::IntV(2)],
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn setter_position_1() {
        let args = vec![
            SteelVal::StructV(Gc::new(SteelStruct {
                name: Rc::from("Promise"),
                fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
            })),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter(Rc::from("Promise"), 1), args);
        let expected = SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(1), SteelVal::IntV(100)],
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn getter_position_0() {
        let args = vec![SteelVal::StructV(Gc::new(SteelStruct {
            name: Rc::from("Promise"),
            fields: vec![SteelVal::IntV(1), SteelVal::IntV(2)],
        }))];

        let res = apply_function(getter(Rc::from("Promise"), 0), args);
        let expected = SteelVal::IntV(1);
        assert_eq!(res.unwrap(), expected);
    }
}
