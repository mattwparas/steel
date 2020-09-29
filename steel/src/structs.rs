use crate::env::{FALSE, TRUE, VOID};
use crate::gc::Gc;
use crate::parser::Expr;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::throw;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum StructFunctionType {
    Constructor,
    Getter(usize),
    Setter(usize),
    Predicate(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SteelStruct {
    name: Rc<String>,
    fields: Vec<Gc<SteelVal>>,
    function_purpose: StructFunctionType,
}

// Housekeeping (just in case there are cyclical references)
impl Drop for SteelStruct {
    fn drop(&mut self) {
        self.fields.clear();
    }
}

impl SteelStruct {
    pub fn new(
        name: Rc<String>,
        fields: Vec<Gc<SteelVal>>,
        function_purpose: StructFunctionType,
    ) -> Self {
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
    pub fn generate_from_tokens(list_of_tokens: &[Expr]) -> Result<Vec<(String, SteelVal)>> {
        let (name, list_of_tokens) = list_of_tokens.split_first().ok_or_else(
            throw!(ArityMismatch => "struct definition requires a name and a list of field names"),
        )?;

        let name = name.atom_identifier_or_else(throw!(TypeMismatch => "struct definition expected an identifier as the first argument"))?;

        if list_of_tokens.len() != 1 {
            stop!(ArityMismatch => "Struct definition requires list of field names")
        }
        let field_names = list_of_tokens[0].vector_val_or_else(
            throw!(ArityMismatch => "struct requires list of identifiers for the field names"),
        )?;

        let field_names_as_strs: Vec<&str> = field_names
            .iter()
            .map(|x| {
                x.atom_identifier_or_else(throw!(TypeMismatch => "struct expected identifiers"))
            })
            .collect::<Result<_>>()?;

        // collect the functions
        // for each field there are going to be 2 functions
        // add 2 for the constructor and the predicate
        let mut funcs = Vec::with_capacity(field_names_as_strs.len() * 2 + 2);
        // generate constructor
        let cons = constructor(name.to_string(), field_names_as_strs.len());
        // generate predicate
        funcs.push((format!("{}?", name), predicate(&name)));
        // generate getters and setters
        for (idx, field) in field_names_as_strs.into_iter().enumerate() {
            funcs.push((format!("{}-{}", name, field), getter(&name, idx)));
            funcs.push((format!("set-{}-{}!", name, field), setter(&name, idx)));
        }
        funcs.push((name.to_string(), cons));
        Ok(funcs)
    }
}

// initialize hashmap to be field_names -> void
// just do arity check before inserting to make sure things check out
// that way field names as a vec are no longer necessary
fn constructor(name: String, len: usize) -> SteelVal {
    let factory: SteelStruct = SteelStruct::new(
        Rc::new(name),
        vec![VOID.with(|f| Gc::clone(f)); len],
        StructFunctionType::Constructor,
    );

    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
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
                *key = arg;
            }

            Ok(Gc::new(SteelVal::StructV(new_struct)))
        },
    )
}

fn predicate(name: &str) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::new(name.to_string()),
        Vec::new(),
        StructFunctionType::Predicate(name.to_string()),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                let error_message = format!(
                    "{}? expected one argument, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            match args[0].as_ref() {
                SteelVal::StructV(my_struct) => {
                    if let StructFunctionType::Predicate(name_huh) = &factory.function_purpose {
                        if my_struct.name.as_ref() == name_huh {
                            Ok(TRUE.with(|f| Gc::clone(f)))
                        } else {
                            Ok(FALSE.with(|f| Gc::clone(f)))
                        }
                    } else {
                        stop!(TypeMismatch => "something went wrong with struct predicate")
                    }
                }
                _ => Ok(FALSE.with(|f| Gc::clone(f))),
            }
        },
    )
}

fn getter(name: &str, idx: usize) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::new(name.to_string()),
        Vec::new(),
        StructFunctionType::Getter(idx),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
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
                    Ok(Gc::clone(ret_val))
                } else {
                    stop!(TypeMismatch => "Couldn't find that field in the struct")
                }
            } else {
                stop!(TypeMismatch => "something went wrong with struct getter")
            }
        },
    )
}

fn setter(name: &str, idx: usize) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::new(name.to_string()),
        Vec::new(),
        StructFunctionType::Setter(idx),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Gc<SteelVal>>, factory: &SteelStruct| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                let error_message = format!(
                    "{} setter expected two arguments, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;
            let value = Gc::clone(&args[1]);

            if let StructFunctionType::Setter(idx) = &factory.function_purpose {
                let mut new_struct = my_struct.clone();
                let key = new_struct
                    .fields
                    .get_mut(*idx)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = value;
                Ok(Gc::new(SteelVal::StructV(new_struct)))
            } else {
                stop!(TypeMismatch => "something went wrong with struct setter")
            }
        },
    )
}

#[cfg(test)]
mod struct_tests {

    use super::*;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        let (factory, func) = func
            .struct_func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap();

        func(args, factory)
    }

    #[test]
    fn constructor_normal() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = apply_function(constructor("Promise".to_string(), 2), args);
        let expected = Gc::new(SteelVal::StructV(SteelStruct {
            name: Rc::new("Promise".to_string()),
            fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected)
    }

    #[test]
    fn setter_position_0() {
        let args = vec![
            SteelVal::StructV(SteelStruct {
                name: Rc::new("Promise".to_string()),
                fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
                function_purpose: StructFunctionType::Constructor,
            }),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter("Promise", 0), args);
        let expected = Gc::new(SteelVal::StructV(SteelStruct {
            name: Rc::new("Promise".to_string()),
            fields: vec![Gc::new(SteelVal::IntV(100)), Gc::new(SteelVal::IntV(2))],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn setter_position_1() {
        let args = vec![
            SteelVal::StructV(SteelStruct {
                name: Rc::new("Promise".to_string()),
                fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
                function_purpose: StructFunctionType::Constructor,
            }),
            SteelVal::IntV(100),
        ];

        let res = apply_function(setter("Promise", 1), args);
        let expected = Gc::new(SteelVal::StructV(SteelStruct {
            name: Rc::new("Promise".to_string()),
            fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(100))],
            function_purpose: StructFunctionType::Constructor,
        }));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn getter_position_0() {
        let args = vec![SteelVal::StructV(SteelStruct {
            name: Rc::new("Promise".to_string()),
            fields: vec![Gc::new(SteelVal::IntV(1)), Gc::new(SteelVal::IntV(2))],
            function_purpose: StructFunctionType::Constructor,
        })];

        let res = apply_function(getter("Promise", 0), args);
        let expected = Gc::new(SteelVal::IntV(1));
        assert_eq!(res.unwrap(), expected);
    }
}
