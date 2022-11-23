use std::rc::Rc;

use crate::gc::Gc;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::values::port::SteelPort;

pub struct PortOperations {}
impl PortOperations {
    pub fn open_input_file() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::StringV(path) = &args[0] {
                    let new_port = SteelPort::new_textual_file_input(&*path)?;
                    Ok(SteelVal::PortV(Gc::new(new_port)))
                } else {
                    stop!(TypeMismatch => "open-input-file expects a path")
                }
            } else {
                stop!(ArityMismatch => "open-input-file expected one argument")
            }
        })
    }

    pub fn open_output_file() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::StringV(path) = &args[0] {
                    let new_port = SteelPort::new_textual_file_output(&*path)?;
                    Ok(SteelVal::PortV(Gc::new(new_port)))
                } else {
                    stop!(TypeMismatch => "open-output-file expects a path")
                }
            } else {
                stop!(ArityMismatch => "open-output-file expected one argument")
            }
        })
    }

    pub fn read_port_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::PortV(port) = &args[0] {
                    // let new_port = SteelPort::new_textual_file_input(&*path)?;
                    // Ok(Gc::new(SteelVal::PortV(new_port)))
                    let (_, result) = port.read_all_str()?;
                    Ok(SteelVal::StringV(result.into()))
                } else {
                    stop!(TypeMismatch => "read-port-to-string expected a port")
                }
            } else {
                stop!(ArityMismatch => "read-port-to-string expected one argument")
            }
        })
    }

    pub fn read_line_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                if let SteelVal::PortV(port) = &args[0] {
                    // let new_port = SteelPort::new_textual_file_input(&*path)?;
                    // Ok(Gc::new(SteelVal::PortV(new_port)))

                    let res = port.read_line();

                    if let Ok((size, result)) = res {
                        if size == 0 {
                            Ok(SteelVal::SymbolV("eof".into()))
                        } else {
                            Ok(SteelVal::StringV(result.into()))
                        }
                    } else {
                        // bit of a hack for now we'll see
                        res.map(|_| unreachable!())
                    }
                } else {
                    stop!(TypeMismatch => "read-line-to-string expected a port")
                }
            } else {
                stop!(ArityMismatch => "read-line-to-string expected one argument")
            }
        })
    }

    pub fn write_line() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 2 {
                if let SteelVal::PortV(port) = &args[0] {
                    let line = args[1].to_string();
                    let res = port.write_string_line(line.as_str());

                    if res.is_ok() {
                        Ok(SteelVal::Void)
                    } else {
                        stop!(Generic => "unable to write string to file");
                    }
                } else {
                    stop!(TypeMismatch => "write-line expects a port")
                }
            } else {
                stop!(ArityMismatch => "write-line expects one argument")
            }
        })
    }
}
