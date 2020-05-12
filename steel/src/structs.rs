// TODO
// pub struct SteelStruct {
//     name: String,
//     fields: HashMap<String, Rc<SteelVal>>,
// }

// generate getters, setters, predicates, constructor here
// with one functions and/or macro
// impl SteelStruct {}

use crate::rvals::SteelVal;
use im_rc::HashMap;
// use std::collections::HashMap;
use crate::parser::Expr;
use crate::rerrs::SteelErr;
// use crate::rvals::FunctionSignature;
use crate::rvals::Result;
use crate::stop;
use crate::throw;
use std::rc::Rc;

use crate::env::{FALSE, TRUE};

use std::time::Instant;

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
        let field_names_as_strs: Vec<String> = field_names
            .into_iter()
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
            // funcs.push((format!("set-{}-{}!", name, field), setter(&name, &field)));
        }
        funcs.push((name.to_string(), cons));
        Ok(funcs)
    }
}

// actually just return an expression to evaluate
// because thats basically what I want anyway
fn constructor(name: String, field_names: Vec<String>) -> SteelVal {
    let factory: SteelStruct = SteelStruct::new(
        Rc::new(name),
        Rc::new(field_names),
        HashMap::new(),
        StructFunctionType::Constructor,
    );
    SteelVal::StructClosureV(
        factory,
        |args: Vec<Rc<SteelVal>>, factory: &SteelStruct| -> Result<Rc<SteelVal>> {
            // println!("Calling a constructor");
            // let now = Instant::now();

            // let factory = factory.clone();
            if args.len() != factory.field_names.len() {
                let error_message = format!(
                    "{} expected {} arguments, found {}",
                    factory.name.clone(),
                    args.len(),
                    factory.field_names.len()
                );
                stop!(ArityMismatch => error_message);
            }

            let args_field_iter = factory.field_names.iter().zip(args.into_iter());
            let mut fields = HashMap::new();
            for (field_name, arg) in args_field_iter {
                fields.insert(field_name.clone(), arg);
            }

            // println!("Cloning: {:?}", factory.field_names);

            let ret_val = Ok(Rc::new(SteelVal::StructV(SteelStruct::new(
                Rc::clone(&factory.name),
                Rc::clone(&factory.field_names), // TODO
                fields,
                StructFunctionType::Constructor,
            ))));

            // println!("Constructing a struct!: {:?}", now.elapsed());

            ret_val
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

            let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

            if let StructFunctionType::Predicate(name_huh) = &factory.function_purpose {
                if my_struct.name.as_ref() == name_huh {
                    Ok(TRUE.with(|f| Rc::clone(f)))
                } else {
                    Ok(FALSE.with(|f| Rc::clone(f)))
                }
            } else {
                stop!(TypeMismatch => "something went wrong with struct predicate")
            }
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

// fn setter(name: &str, field: &str) -> SteelVal {
//     let factory = SteelStruct::new(
//         name.to_string(),
//         Vec::new(),
//         HashMap::new(),
//         StructFunctionType::Setter(field.to_string()),
//     );
//     SteelVal::StructClosureV(
//         factory,
//         |args: Vec<Rc<SteelVal>>, factory: SteelStruct| -> Result<Rc<SteelVal>> {
//             if args.len() != 1 {
//                 let error_message = format!(
//                     "{} getter expected one argument, found {}",
//                     factory.name,
//                     args.len()
//                 );
//                 stop!(ArityMismatch => error_message);
//             }

//             let my_struct = args[0].struct_or_else(throw!(TypeMismatch => "expected struct"))?;

//             if let StructFunctionType::Getter(field_name) = factory.function_purpose {
//                 if let Some(ret_val) = my_struct.fields.get(&field_name) {
//                     Ok(Rc::clone(ret_val))
//                 } else {
//                     stop!(TypeMismatch => "Couldn't find that field in the struct")
//                 }
//             } else {
//                 stop!(TypeMismatch => "something went wrong with struct predicate")
//             }
//         },
//     )
// }
