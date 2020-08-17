// use crate::env::VOID;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use std::io;
use std::rc::Rc;

// mod primitives;

// use std::fs

use std::path::Path;

use crate::primitives::lists::ListOperations;

use std::env::current_dir;

// use crate::lists::ListOperations

pub struct FsFunctions {}
impl FsFunctions {
    pub fn path_exists() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Rc::new(SteelVal::BoolV(Path::new(s).exists())))
                } else {
                    stop!(TypeMismatch => "path-exists? expects a string")
                }
            } else {
                stop!(ArityMismatch => "path-exists? takes one argument")
            }
        })
    }

    pub fn is_file() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Rc::new(SteelVal::BoolV(Path::new(s).is_file())))
                } else {
                    stop!(TypeMismatch => "is-file? expects a string")
                }
            } else {
                stop!(ArityMismatch => "is-file? takes one argument")
            }
        })
    }

    pub fn is_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Rc::new(SteelVal::BoolV(Path::new(s).is_dir())))
                } else {
                    stop!(TypeMismatch => "is-dir? expects a string")
                }
            } else {
                stop!(ArityMismatch => "is-dir? takes one argument")
            }
        })
    }

    pub fn file_name() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Rc::new(SteelVal::StringV(
                        Path::new(s)
                            .file_name()
                            .map(|x| x.to_str())
                            .flatten()
                            .unwrap_or("")
                            .to_string(),
                    )))
                } else {
                    stop!(TypeMismatch => "is-dir? expects a string")
                }
            } else {
                stop!(ArityMismatch => "file-name takes one argument")
            }
        })
    }

    pub fn read_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = args[0].as_ref() {
                    let p = Path::new(s);
                    if p.is_dir() {
                        let iter = p.read_dir();
                        match iter {
                            Ok(i) => {
                                ListOperations::built_in_list_normal_iter(i.into_iter().map(|x| {
                                    match x?.path().to_str() {
                                        Some(s) => Ok(Rc::new(SteelVal::StringV(s.to_string()))),
                                        None => Ok(Rc::new(SteelVal::BoolV(false))),
                                    }
                                }))
                            }
                            Err(e) => stop!(Generic => e.to_string()),
                        }
                    } else {
                        stop!(TypeMismatch => "read-dir expected a dir, found a file")
                    }
                } else {
                    stop!(TypeMismatch => "read-dir expects a string")
                }
            } else {
                stop!(ArityMismatch => "read-dir takes one argument")
            }
        })
    }

    pub fn current_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 0 {
                let path = current_dir()?;
                Ok(Rc::new(SteelVal::StringV(
                    path.to_str().unwrap_or("").to_string(),
                )))
            // println!("The current directory is {}", path.display());
            // Ok(())
            } else {
                stop!(ArityMismatch => "current-directory takes no arguments")
            }
        })
    }
}

// pub fn display() -> SteelVal {
//     SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
//         if args.len() == 1 {
//             let print_val = (*args[0]).clone();
//             print!("{:?}", print_val);
//             Ok(VOID.with(|f| Rc::clone(f)))
//         } else {
//             stop!(ArityMismatch => "display takes one argument");
//         }
//     })
// }

// pub fn newline() -> SteelVal {
//     SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
//         if args.is_empty() {
//             println!();
//             Ok(VOID.with(|f| Rc::clone(f)))
//         } else {
//             stop!(ArityMismatch => "newline takes no arguments");
//         }
//     })
// }

// pub fn read_to_string() -> SteelVal {
//     SteelVal::FuncV(|_args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
//         let mut input_text = String::new();
//         io::stdin().read_line(&mut input_text)?;
//         Ok(Rc::new(SteelVal::StringV(
//             input_text.trim_end().to_string(),
//         )))
//     })
// }

// pub fn read_file_to_string() -> SteelVal {
//     SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
//         let mut
//     })
// }
// }
