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
        })
    }

    pub fn symbol_to_string() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::SymbolV(quoted_value) = args[0].as_ref() {
                    return Ok(Rc::new(SteelVal::SymbolV(quoted_value.clone())));
                } else {
                    let error_message = format!(
                        "symbol->string expected a symbol, found {}",
                        args[0].as_ref()
                    );
                    stop!(TypeMismatch => error_message)
                }
            } else {
                stop!(ArityMismatch => "symbol->string expects only one argument")
            }
        })
    }
}
