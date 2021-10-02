use colored::Colorize;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::io;
// use std::rc::Rc;

// mod primitives;

pub struct IoFunctions {}
impl IoFunctions {
    pub fn display() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                let print_val = &args[0];

                match &print_val {
                    SteelVal::StringV(s) => print!("{}", s),
                    _ => print!("{}", print_val),
                }

                Ok(SteelVal::Void)
            } else {
                stop!(ArityMismatch => "display takes one argument");
            }
        })
    }

    pub fn display_color() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 2 {
                let print_val = &args[0];
                let color = &args[1];

                match (&print_val, &color) {
                    (SteelVal::StringV(s), SteelVal::SymbolV(c)) => match c.as_ref() {
                        "green" | "Green" => print!("{}", s.to_string().bright_green()),
                        "blue" | "Blue" => print!("{}", s.to_string().bright_blue()),
                        "red" | "Red" => print!("{}", s.to_string().red()),
                        _ => print!("{}", s),
                    },
                    (_, SteelVal::StringV(c)) | (_, SteelVal::SymbolV(c)) => match c.as_ref() {
                        "green" | "Green" => print!("{}", print_val.to_string().bright_green()),
                        "blue" | "Blue" => print!("{}", print_val.to_string().bright_blue()),
                        "red" | "Red" => print!("{}", print_val.to_string().red()),
                        _ => print!("{}", print_val),
                    },
                    (_, _) => {
                        stop!(TypeMismatch => "display-color expected a symbol as the second argument")
                    }
                }
                Ok(SteelVal::Void)
            } else {
                stop!(ArityMismatch => "display-color takes two arguments");
            }
        })
    }

    pub fn newline() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                println!();
                Ok(SteelVal::Void)
            } else {
                stop!(ArityMismatch => "newline takes no arguments");
            }
        })
    }

    pub fn read_to_string() -> SteelVal {
        SteelVal::FuncV(|_args: &[SteelVal]| -> Result<SteelVal> {
            let mut input_text = String::new();
            io::stdin().read_line(&mut input_text)?;
            Ok(SteelVal::StringV(input_text.trim_end().into()))
        })
    }
}
