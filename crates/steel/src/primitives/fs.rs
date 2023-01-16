use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::env::current_dir;
use std::path::Path;

use std::fs;
use std::io;

fn get_extension_from_filename(filename: &str) -> Option<&str> {
    Path::new(filename)
        .extension()
        .and_then(std::ffi::OsStr::to_str)
}

/// Copy files from source to destination recursively.
/// from https://nick.groenen.me/notes/recursively-copy-files-in-rust/
pub fn copy_recursively(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> io::Result<()> {
    fs::create_dir_all(&destination)?;
    for entry in fs::read_dir(source)? {
        let entry = entry?;
        let filetype = entry.file_type()?;
        if filetype.is_dir() {
            copy_recursively(entry.path(), destination.as_ref().join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), destination.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}

pub struct FsFunctions {}
impl FsFunctions {
    pub fn copy_directory_recursively() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 2 {
                let source = if let SteelVal::StringV(s) = &args[0] {
                    s
                } else {
                    stop!(TypeMismatch => format!("copy-directory-recursively! expects a string, found: {}", &args[0]))
                };

                let destination = if let SteelVal::StringV(s) = &args[1] {
                    s
                } else {
                    stop!(TypeMismatch => format!("copy-directory-recursively! expects a string, found: {}", &args[0]))
                };

                copy_recursively(source.as_str(), destination.as_str())?;

                Ok(SteelVal::Void)
            } else {
                stop!(ArityMismatch => format!("copy-directory-recursively! takes two arguments, found: {}", args.len()))
            }
        })
    }

    pub fn path_exists() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0] {
                    Ok(SteelVal::BoolV(Path::new(s.as_ref()).exists()))
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
                    Ok(SteelVal::BoolV(Path::new(s.as_ref()).is_file()))
                } else {
                    stop!(TypeMismatch => format!("is-file? expects a string, found: {}", &args[0]))
                }
            } else {
                stop!(ArityMismatch => "is-file? takes one argument")
            }
        })
    }

    pub fn get_extension(args: &[SteelVal]) -> Result<SteelVal> {
        if args.len() == 1 {
            if let SteelVal::StringV(s) = &args[0] {
                if let Some(ext) = get_extension_from_filename(&s) {
                    Ok(SteelVal::StringV(ext.into()))
                } else {
                    stop!(Generic => format!("path->extension expects a path that exists, found: {}", s))
                }
            } else {
                stop!(TypeMismatch => format!("path->extension expects a string, found: {}", &args[0]))
            }
        } else {
            stop!(ArityMismatch => format!("path->extension takes one argument, found: {}", args.len()))
        }
    }

    pub fn is_dir() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                // let path =

                if let SteelVal::StringV(s) = &args[0] {
                    Ok(SteelVal::BoolV(Path::new(&s.to_string()).is_dir()))
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
                        Path::new(s.as_str())
                            .file_name()
                            .and_then(|x| x.to_str())
                            .unwrap_or("")
                            .into(),
                    ))
                } else {
                    stop!(TypeMismatch => "file-name expects a string")
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
                    let p = Path::new(s.as_ref());
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
