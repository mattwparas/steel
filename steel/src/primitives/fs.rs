// use crate::env::VOID;
use crate::rerrs::SteelErr;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use std::io;
// use std::rc::Rc;

use crate::gc::Gc;

// mod primitives;

// use std::fs

use std::path::Path;

use crate::primitives::lists::ListOperations;

use std::env::current_dir;

// use crate::lists::ListOperations

pub struct FsFunctions {}
impl FsFunctions {
    pub fn path_exists() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Gc::new(SteelVal::BoolV(Path::new(s).exists())))
                } else {
                    stop!(TypeMismatch => "path-exists? expects a string")
                }
            } else {
                stop!(ArityMismatch => "path-exists? takes one argument")
            }
        })
    }

    pub fn is_file() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Gc::new(SteelVal::BoolV(Path::new(s).is_file())))
                } else {
                    stop!(TypeMismatch => "is-file? expects a string")
                }
            } else {
                stop!(ArityMismatch => "is-file? takes one argument")
            }
        })
    }

    pub fn is_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Gc::new(SteelVal::BoolV(Path::new(s).is_dir())))
                } else {
                    stop!(TypeMismatch => "is-dir? expects a string")
                }
            } else {
                stop!(ArityMismatch => "is-dir? takes one argument")
            }
        })
    }

    pub fn file_name() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = args[0].as_ref() {
                    Ok(Gc::new(SteelVal::StringV(
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
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
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
                                        Some(s) => Ok(Gc::new(SteelVal::StringV(s.to_string()))),
                                        None => Ok(Gc::new(SteelVal::BoolV(false))),
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
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 0 {
                let path = current_dir()?;
                Ok(Gc::new(SteelVal::StringV(
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
