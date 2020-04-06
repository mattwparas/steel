// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use im_rc::Vector;
use std::rc::Rc;

pub struct StringOperations {}
impl StringOperations {
    pub fn string_append() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 2 {
                if let (SteelVal::StringV(l), SteelVal::StringV(r)) =
                    (&args[0].as_ref(), &args[1].as_ref())
                {
                    let new_string = l.clone() + &r.clone();
                    return Ok(Rc::new(SteelVal::StringV(new_string)));
                } else {
                    stop!(TypeMismatch => "string-append expected two strings")
                }
            } else {
                stop!(ArityMismatch => "string-append takes two arguments")
            }
        })
    }
}
