// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use im_rc::Vector;
use std::rc::Rc;

use crate::primitives::lists::ListOperations;

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

    pub fn string_to_list() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let chars: Vec<Rc<SteelVal>> =
                        s.chars().map(|x| Rc::new(SteelVal::CharV(x))).collect();
                    ListOperations::built_in_list_func()(chars)
                } else {
                    stop!(TypeMismatch => "string->list expected a string")
                }
            } else {
                stop!(ArityMismatch => "string->list takes one argument")
            }
        })
    }

    pub fn string_to_upper() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let upper = s.to_uppercase();
                    Ok(Rc::new(SteelVal::StringV(upper)))
                } else {
                    stop!(TypeMismatch => "string-upcase expected a string")
                }
            } else {
                stop!(ArityMismatch => "string-upcase takes one argument")
            }
        })
    }

    pub fn string_to_lower() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let lower = s.to_lowercase();
                    Ok(Rc::new(SteelVal::StringV(lower)))
                } else {
                    stop!(TypeMismatch => "string-lowercase expected a string")
                }
            } else {
                stop!(ArityMismatch => "string-lowercase takes one argument")
            }
        })
    }

    pub fn trim() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim();
                    Ok(Rc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim takes one argument")
            }
        })
    }

    pub fn trim_start() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim_start();
                    Ok(Rc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim-start expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim-start takes one argument")
            }
        })
    }

    pub fn trim_end() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim_end();
                    Ok(Rc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim-end expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim-end takes one argument")
            }
        })
    }

    pub fn split_whitespace() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let split: Vec<Rc<SteelVal>> = s
                        .split_whitespace()
                        .map(|x| Rc::new(SteelVal::StringV(x.to_string())))
                        .collect();
                    ListOperations::built_in_list_func()(split)
                } else {
                    stop!(TypeMismatch => "split-whitespace expected a string")
                }
            } else {
                stop!(ArityMismatch => "split-whitespace takes one argument")
            }
        })
    }
}
