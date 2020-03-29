use crate::env::VOID;
use crate::evaluator::Result;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::stop;
use std::rc::Rc;

pub struct IoFunctions {}
impl IoFunctions {
    pub fn display() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                let print_val = (*args[0]).clone();
                print!("{:?}", print_val);
                // Ok(Rc::new(SteelVal::Void))
                Ok(VOID.with(|f| Rc::clone(f)))
            } else {
                stop!(ArityMismatch => "display takes one argument");
            }
        })
    }

    pub fn newline() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 0 {
                println!("");
                // Ok(Rc::new(SteelVal::Void))
                Ok(VOID.with(|f| Rc::clone(f)))
            } else {
                stop!(ArityMismatch => "newline takes no arguments");
            }
        })
    }
}
