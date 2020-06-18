// TODO
// pub struct SteelStruct {
//     name: String,
//     fields: HashMap<String, Rc<SteelVal>>,
// }

// generate getters, setters, predicates, constructor here
// with one functions and/or macro
// impl SteelStruct {}

use crate::rvals::SteelVal;
// use im_rc::HashMap;
use crate::parser::Expr;
use crate::rerrs::SteelErr;
use std::collections::HashMap;
// use crate::rvals::FunctionSignature;
use crate::rvals::Result;
use crate::stop;
use crate::throw;
use std::rc::Rc;

use crate::env::{FALSE, TRUE, VOID};

// use std::time::Instant;

#[derive(Clone, Debug)]
pub enum StructFunctionType {
    Constructor,
    Getter(String),
    Setter(String),
    Predicate(String),
}

#[derive(Clone, Debug)]
pub struct SteelStruct {
    name: Rc<String>,
    field_names: Rc<Vec<String>>,
    fields: HashMap<String, Rc<SteelVal>>,
    function_purpose: StructFunctionType,
}

// Housekeeping
impl Drop for SteelStruct {
    fn drop(&mut self) {
        self.fields.clear();
    }
}

impl SteelStruct {
    pub fn new(
        name: Rc<String>,
        field_names: Rc<Vec<String>>,
        fields: HashMap<String, Rc<SteelVal>>,
        function_purpose: StructFunctionType,
    ) -> Self {
        SteelStruct {
            name,
            field_names,
            fields,
            function_purpose,
        }
    }

    // This will blow up the stack with a sufficiently large recursive struct
    pub fn pretty_print(&self) -> String {
        format!("{}: {:#?}", self.name, self.fields)

        // let params = self.params_exp().join(" ");
        // format!(
        //     "(lambda ({}) {})",
        //     params.to_string(),
        //     self.body_exp().to_string()
        // )
    }
}

impl SteelStruct {
    pub fn generate_from_tokens(list_of_tokens: &[Rc<Expr>]) -> Result<Vec<(String, SteelVal)>> {
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

        println!("{:?}", field_names);

        let field_names_as_strs: Vec<String> = field_names
            .iter()
            .map(|x| {
                x.atom_identifier_or_else(throw!(TypeMismatch => "struct expected identifiers"))
                    .map(|x| x.to_string())
            })
            .collect::<Result<Vec<String>>>()?;
        let mut funcs = Vec::new();
        // generate constructor
        let cons = constructor(name.to_string(), field_names_as_strs.clone());
        // generate predicate
        funcs.push((format!("{}?", name), predicate(&name)));
        // generate getters and setters
        for field in field_names_as_strs {
            funcs.push((format!("{}-{}", name, field), getter(&name, &field)));
            funcs.push((format!("set-{}-{}!", name, field), setter(&name, &field)));
        }
        funcs.push((name.to_string(), cons));
        Ok(funcs)
    }
}

// initialize hashmap to be field_names -> void
// just do arity check before inserting to make sure things check out
// that way field names as a vec are no longer necessary
fn constructor(name: String, field_names: Vec<String>) -> SteelVal {
    let mut hm = HashMap::new();
    for field in &field_names {
        hm.insert(field.to_string(), VOID.with(|f| Rc::clone(f)));
    }

    let factory: SteelStruct = SteelStruct::new(
        Rc::new(name),
        Rc::new(field_names),
        hm,
        StructFunctionType::Constructor,
    );

    SteelVal::StructClosureV(
        factory,
        |args: Vec<Rc<SteelVal>>, factory: &SteelStruct| -> Result<Rc<SteelVal>> {
            // println!("Calling a constructor");
            // let now = Instant::now();

            // let factory = factory.clone();
            if args.len() != factory.fields.len() {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    factory.name.clone(),
                    args.len(),
                    factory.fields.len()
                );
                stop!(ArityMismatch => error_message);
            }

            // let args_field_iter = ;

            let mut new_struct = factory.clone();

            for (field_name, arg) in factory.field_names.iter().zip(args.into_iter()) {
                // new_struct.fields.insert(field_name.clone(), arg);
                let key = new_struct
                    .fields
                    .get_mut(field_name)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = arg;
            }

            Ok(Rc::new(SteelVal::StructV(new_struct)))

            // ret_val
        },
    )
}

fn predicate(name: &str) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::new(name.to_string()),
        Rc::new(Vec::new()),
        HashMap::new(),
        StructFunctionType::Predicate(name.to_string()),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Rc<SteelVal>>, factory: &SteelStruct| -> Result<Rc<SteelVal>> {
            // println!("Checking predicate!");

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
                            Ok(TRUE.with(|f| Rc::clone(f)))
                        } else {
                            Ok(FALSE.with(|f| Rc::clone(f)))
                        }
                    } else {
                        stop!(TypeMismatch => "something went wrong with struct predicate")
                    }
                }
                _ => Ok(FALSE.with(|f| Rc::clone(f))),
            }

            // let my_struct = args[0].struct_or_else(|| {
            //     return Ok(FALSE.with(|f| Rc::clone(f)));
            // })?;

            // if let StructFunctionType::Predicate(name_huh) = &factory.function_purpose {
            //     if my_struct.name.as_ref() == name_huh {
            //         Ok(TRUE.with(|f| Rc::clone(f)))
            //     } else {
            //         Ok(FALSE.with(|f| Rc::clone(f)))
            //     }
            // } else {
            //     stop!(TypeMismatch => "something went wrong with struct predicate")
            // }
        },
    )
}

fn getter(name: &str, field: &str) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::new(name.to_string()),
        Rc::new(Vec::new()),
        HashMap::new(),
        StructFunctionType::Getter(field.to_string()),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Rc<SteelVal>>, factory: &SteelStruct| -> Result<Rc<SteelVal>> {
            // let now = Instant::now();
            // let res = interpreter.evaluate(&line);
            // it prints '2'
            if args.len() != 1 {
                let error_message = format!(
                    "{} getter expected one argument, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

            if let StructFunctionType::Getter(field_name) = &factory.function_purpose {
                if let Some(ret_val) = my_struct.fields.get(field_name) {
                    // println!("Getting from a struct!: {:?}", now.elapsed());
                    Ok(Rc::clone(ret_val))
                } else {
                    stop!(TypeMismatch => "Couldn't find that field in the struct")
                }
            } else {
                stop!(TypeMismatch => "something went wrong with struct predicate")
            }
        },
    )
}

fn setter(name: &str, field: &str) -> SteelVal {
    let factory = SteelStruct::new(
        Rc::new(name.to_string()),
        Rc::new(Vec::new()),
        HashMap::new(),
        StructFunctionType::Setter(field.to_string()),
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Rc<SteelVal>>, factory: &SteelStruct| -> Result<Rc<SteelVal>> {
            if args.len() != 2 {
                let error_message = format!(
                    "{} setter expected two arguments, found {}",
                    factory.name,
                    args.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;
            let value = Rc::clone(&args[1]);

            if let StructFunctionType::Setter(ref field_name) = &factory.function_purpose {
                let mut new_struct = my_struct.clone();
                let key = new_struct
                    .fields
                    .get_mut(field_name)
                    .ok_or_else(throw!(TypeMismatch => "Couldn't find that field in the struct"))?;
                *key = value;
                Ok(Rc::new(SteelVal::StructV(new_struct)))

            // if let Some(ret_val) = my_struct.fields.get(field_name) {
            //     let new_struct = my_struct.clone();
            //     new_struct.fields.entry(field_name).unwrap().insert(value);
            //     Ok(Rc::clone(new_struct))
            // } else {
            //     stop!(TypeMismatch => "Couldn't find that field in the struct")
            // }
            } else {
                stop!(TypeMismatch => "something went wrong with struct predicate")
            }
        },
    )
}
