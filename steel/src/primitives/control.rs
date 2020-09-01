// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::gc::Gc;
use crate::rvals::{Result, SteelVal};
use crate::stop;

// use crate::primitives::lists::ListOperations;

pub struct ControlOperations {}
impl ControlOperations {
    pub fn error() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut error_message = String::new();

            if args.len() > 0 {
                for arg in args {
                    let error_val = arg.to_string();
                    error_message.push(' ');
                    error_message.push_str(error_val.trim_matches('\"'));
                }

                stop!(Generic => error_message);
            } else {
                stop!(ArityMismatch => "error takes at least one argument");
            }
        })
    }
}
