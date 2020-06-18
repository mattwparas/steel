// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::rc::Rc;

// use crate::primitives::lists::ListOperations;

pub struct SymbolOperations {}
impl SymbolOperations {
    pub fn concat_symbols() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut new_symbol = String::new();

            for arg in args {
                if let SteelVal::SymbolV(quoted_value) = arg.as_ref() {
                    new_symbol.push_str(quoted_value);
                } else {
                    let error_message =
                        format!("concat-symbol expected only symbols, found {}", arg);
                    stop!(TypeMismatch => error_message);
                }
            }

            return Ok(Rc::new(SteelVal::SymbolV(new_symbol)));

            // if args.len() == 2 {
            //     if let (SteelVal::StringV(l), SteelVal::StringV(r)) =
            //         (&args[0].as_ref(), &args[1].as_ref())
            //     {
            //         let new_string = l.clone() + &r.clone();
            //         Ok(Rc::new(SteelVal::StringV(new_string)))
            //     } else {
            //         stop!(TypeMismatch => "string-append expected two strings")
            //     }
            // } else {
            //     stop!(ArityMismatch => "string-append takes two arguments")
            // }
        })
    }
}
