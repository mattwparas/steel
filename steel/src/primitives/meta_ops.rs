// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::env::VOID;
use crate::gc::Gc;
// use crate::rvals::gc_get_size;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use crate::rvals::MemSize;

// use crate::primitives::lists::ListOperations;

pub struct MetaOperations {}
impl MetaOperations {
    pub fn inspect_bytecode() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            // let mut error_message = String::new();

            if args.len() == 1 {
                if let SteelVal::Closure(bytecode_lambda) = args[0].as_ref() {
                    crate::vm::pretty_print_dense_instructions(&bytecode_lambda.body_exp());
                    Ok(VOID.with(|f| Gc::clone(f)))
                } else {
                    stop!(TypeMismatch => "inspect-bytecode expects a closure object");
                }
            } else {
                stop!(ArityMismatch => "inspect-bytecode takes only one argument");
            }
        })
    }

    pub fn memory_address() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "memory address takes one address")
            }

            let memory_address = format!("{:p}", &args[0].as_ptr());

            Ok(Gc::new(SteelVal::StringV(memory_address)))
        })
    }

    // pub fn size_of() -> SteelVal {
    //     SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
    //         // let mut error_message = String::new();
    //         if args.len() == 1 {
    //             Ok(Gc::new(SteelVal::IntV(
    //                 gc_get_size(Gc::clone(&args[0])) as isize
    //             )))

    //         // if let SteelVal::Closure(bytecode_lambda) = args[0].as_ref() {
    //         //     crate::vm::pretty_print_dense_instructions(&bytecode_lambda.body_exp());
    //         //     Ok(VOID.with(|f| Rc::clone(f)))
    //         // } else {
    //         //     stop!(TypeMismatch => "inspect-bytecode expects a closure object");
    //         // }
    //         } else {
    //             stop!(ArityMismatch => "sizeof takes only one argument");
    //         }
    //     })
    // }
}
