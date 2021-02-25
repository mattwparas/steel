// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;

// use crate::primitives::lists::ListOperations;

pub struct ControlOperations {}
impl ControlOperations {
    pub fn error() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let mut error_message = String::new();

            if !args.is_empty() {
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
