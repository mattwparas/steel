// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::env::VOID;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::rc::Rc;

use crate::rvals::rc_get_size;
use crate::rvals::MemSize;

// use crate::primitives::lists::ListOperations;

pub struct MetaOperations {}
impl MetaOperations {
    pub fn inspect_bytecode() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            // let mut error_message = String::new();

            if args.len() == 1 {
                if let SteelVal::Closure(bytecode_lambda) = args[0].as_ref() {
                    crate::vm::pretty_print_dense_instructions(&bytecode_lambda.body_exp());
                    Ok(VOID.with(|f| Rc::clone(f)))
                } else {
                    stop!(TypeMismatch => "inspect-bytecode expects a closure object");
                }
            } else {
                stop!(ArityMismatch => "inspect-bytecode takes only one argument");
            }
        })
    }

    pub fn size_of() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            // let mut error_message = String::new();
            if args.len() == 1 {
                Ok(Rc::new(SteelVal::IntV(
                    rc_get_size(Rc::clone(&args[0])) as isize
                )))

            // if let SteelVal::Closure(bytecode_lambda) = args[0].as_ref() {
            //     crate::vm::pretty_print_dense_instructions(&bytecode_lambda.body_exp());
            //     Ok(VOID.with(|f| Rc::clone(f)))
            // } else {
            //     stop!(TypeMismatch => "inspect-bytecode expects a closure object");
            // }
            } else {
                stop!(ArityMismatch => "sizeof takes only one argument");
            }
        })
    }
}
