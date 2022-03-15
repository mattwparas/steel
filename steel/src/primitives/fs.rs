use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::env::current_dir;
use std::path::Path;

pub struct FsFunctions {}
impl FsFunctions {
    pub fn path_exists() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0] {
                    Ok(SteelVal::BoolV(Path::new(s).exists()))
                } else {
                    stop!(TypeMismatch => "path-exists? expects a string")
                }
            } else {
                stop!(ArityMismatch => "path-exists? takes one argument")
            }
        })
    }

    pub fn is_file() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = &args[0] {
                    Ok(SteelVal::BoolV(Path::new(s).is_file()))
                } else {
                    stop!(TypeMismatch => "is-file? expects a string")
                }
            } else {
                stop!(ArityMismatch => "is-file? takes one argument")
            }
        })
    }

    pub fn is_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = &args[0] {
                    Ok(SteelVal::BoolV(Path::new(s).is_dir()))
                } else {
                    stop!(TypeMismatch => "is-dir? expects a string")
                }
            } else {
                stop!(ArityMismatch => "is-dir? takes one argument")
            }
        })
    }

    pub fn file_name() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0] {
                    Ok(SteelVal::StringV(
                        Path::new(s)
                            .file_name()
                            .map(|x| x.to_str())
                            .flatten()
                            .unwrap_or("")
                            .into(),
                    ))
                } else {
                    stop!(TypeMismatch => "is-dir? expects a string")
                }
            } else {
                stop!(ArityMismatch => "file-name takes one argument")
            }
        })
    }

    pub fn read_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = &args[0] {
                    let p = Path::new(s);
                    if p.is_dir() {
                        let iter = p.read_dir();
                        match iter {
                            Ok(i) => Ok(SteelVal::ListV(
                                i.into_iter()
                                    .map(|x| match x?.path().to_str() {
                                        Some(s) => Ok(SteelVal::StringV(s.into())),
                                        None => Ok(SteelVal::BoolV(false)),
                                    })
                                    .collect::<Result<_>>()?,
                            )),
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
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                let path = current_dir()?;
                Ok(SteelVal::StringV(path.to_str().unwrap_or("").into()))
            // println!("The current directory is {}", path.display());
            // Ok(())
            } else {
                stop!(ArityMismatch => "current-directory takes no arguments")
            }
        })
    }
}
