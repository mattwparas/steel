// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::port::SteelPort;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use std::rc::Rc;

pub struct PortOperations {}
impl PortOperations {
    pub fn open_input_file() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(path) = &args[0].as_ref() {
                    let new_port = SteelPort::new_textual_file_input(&*path)?;
                    Ok(Rc::new(SteelVal::PortV(new_port)))
                } else {
                    stop!(TypeMismatch => "open-input-file expects a path")
                }
            } else {
                stop!(ArityMismatch => "open-input-file expected one argument")
            }
        })
    }

    pub fn read_port_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::PortV(port) = &args[0].as_ref() {
                    // let new_port = SteelPort::new_textual_file_input(&*path)?;
                    // Ok(Rc::new(SteelVal::PortV(new_port)))
                    let (_, result) = port.read_all_str()?;
                    Ok(Rc::new(SteelVal::StringV(result)))
                } else {
                    stop!(TypeMismatch => "read-port-to-string expected a port")
                }
            } else {
                stop!(ArityMismatch => "read-port-to-string expected one argument")
            }
        })
    }

    pub fn read_line_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[Rc<SteelVal>]| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::PortV(port) = &args[0].as_ref() {
                    // let new_port = SteelPort::new_textual_file_input(&*path)?;
                    // Ok(Rc::new(SteelVal::PortV(new_port)))
                    let (_, result) = port.read_line()?;
                    Ok(Rc::new(SteelVal::StringV(result)))
                } else {
                    stop!(TypeMismatch => "read-port-to-string expected a port")
                }
            } else {
                stop!(ArityMismatch => "read-port-to-string expected one argument")
            }
        })
    }
}
