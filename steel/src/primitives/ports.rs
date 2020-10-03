// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::gc::Gc;
use crate::port::SteelPort;
use crate::rvals::{Result, SteelVal};
use crate::stop;

pub struct PortOperations {}
impl PortOperations {
    pub fn open_input_file() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(path) = &args[0].as_ref() {
                    let new_port = SteelPort::new_textual_file_input(&*path)?;
                    Ok(Gc::new(SteelVal::PortV(new_port)))
                } else {
                    stop!(TypeMismatch => "open-input-file expects a path")
                }
            } else {
                stop!(ArityMismatch => "open-input-file expected one argument")
            }
        })
    }

    pub fn read_port_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::PortV(port) = &args[0].as_ref() {
                    // let new_port = SteelPort::new_textual_file_input(&*path)?;
                    // Ok(Gc::new(SteelVal::PortV(new_port)))
                    let (_, result) = port.read_all_str()?;
                    Ok(Gc::new(SteelVal::StringV(result)))
                } else {
                    stop!(TypeMismatch => "read-port-to-string expected a port")
                }
            } else {
                stop!(ArityMismatch => "read-port-to-string expected one argument")
            }
        })
    }

    pub fn read_line_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::PortV(port) = &args[0].as_ref() {
                    // let new_port = SteelPort::new_textual_file_input(&*path)?;
                    // Ok(Gc::new(SteelVal::PortV(new_port)))

                    let res = port.read_line();

                    if let Ok((size, result)) = res {
                        if size == 0 {
                            Ok(Gc::new(SteelVal::SymbolV("eof".to_string())))
                        } else {
                            Ok(Gc::new(SteelVal::StringV(result)))
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
}
