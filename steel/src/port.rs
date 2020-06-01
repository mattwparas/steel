use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::io::{BufReader, BufWriter, Stdin, Stdout};

// use serr::{SErr, SResult};
// use utils::chars::Chars;
// use utils::{new_rc_ref_cell, RcRefCell};

use crate::rerrs::SteelErr;
use crate::rvals::Result;

// use crate::rvals::{new_rc_ref_cell, RcRefSteelVal};

use std::cell::RefCell;
use std::rc::Rc;

// pub<T> type RcRefCell: Rc<RcRefCell<T>>;

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

#[derive(Debug, Clone)]
pub enum SteelPort {
    FileInput(String, RcRefCell<BufReader<File>>),
    FileOutput(String, RcRefCell<BufWriter<File>>),
    StdInput(RcRefCell<Stdin>),
    StdOutput(RcRefCell<Stdout>),
    // StringInput(String, RcRefCell<BufReader<&[u8]>>),
    // StringOutput(String, RcRefCell<BufWriter<&[u8]>>),
    Closed,
}

// impl SteelPort {

// }

// pub fnum Input

// #[derive(Debug, Clone)]
// pub enum PortData {
//     FileInput(String, RcRefCell<BufReader<File>>),
//     FileOutput(String, RcRefCell<BufWriter<File>>),
//     StringInput(String, RcRefCell<BufReader<File>>),
//     StringOutput(String, RcRefCell<BufWriter<File>>),
//     StdInput(RcRefCell<Stdin>),
//     StdOutput(RcRefCell<Stdout>),
//     Closed,
// }

// impl PartialEq for SteelPort {
//     fn eq(&self, rhs: &Self) -> bool {
//         match (self, rhs) {
//             (PortData::FileInput(s, r), PortData::FileInput(rs, rr))
//             | (PortData::StringInput(s, r), PortData::StringInput(rs, rr)) => {
//                 s == rs && &*r as *const _ == &*rr as *const _
//             }
//             (PortData::FileOutput(s, r), PortData::FileOutput(rs, rr))
//             | (PortData::StringOutput(s, r), PortData::StringOutput(rs, rr)) => {
//                 s == rs && &*r as *const _ == &*rr as *const _
//             }
//             (PortData::StdInput(r), PortData::StdInput(rr)) => &*r as *const _ == &*rr as *const _,
//             (PortData::StdOutput(r), PortData::StdOutput(rr)) => {
//                 &*r as *const _ == &*rr as *const _
//             }
//             _ => false,
//         }
//     }
// }

#[macro_export]
macro_rules! port_read_str_fn(
    ($br: ident, $fn: ident) => {{
        let br = &mut *$br.borrow_mut();
        let mut result = String::new();
        let size = br.$fn(&mut result)?;
        Ok((size, result))
    }};
);

impl SteelPort {
    pub fn new_textual_file_input(path: &str) -> Result<SteelPort> {
        let file = OpenOptions::new().read(true).open(path)?;

        Ok(SteelPort::FileInput(
            path.to_string(),
            new_rc_ref_cell(BufReader::new(file)),
        ))
    }

    pub fn new_textual_file_output(path: &str) -> Result<SteelPort> {
        let file = OpenOptions::new().create_new(true).write(true).open(path)?;

        Ok(SteelPort::FileOutput(
            path.to_string(),
            new_rc_ref_cell(BufWriter::new(file)),
        ))
    }

    // pub fn new_binary_file_input(path: &str) -> Result<SteelPort> {
    //     let file = OpenOptions::new().read(true).open(path)?;

    //     Ok(SteelPort::StringInput(
    //         path.to_string(),
    //         new_rc_ref_cell(BufReader::new(file)),
    //     ))
    // }

    // pub fn new_binary_file_output(path: &str) -> Result<SteelPort> {
    //     let file = OpenOptions::new().create_new(true).write(true).open(path)?;

    //     Ok(SteelPort::StringOutput(
    //         path.to_string(),
    //         new_rc_ref_cell(BufWriter::new(file)),
    //     ))
    // }

    //
    // Read functions
    //
    pub fn read_line(&self) -> Result<(usize, String)> {
        match self {
            SteelPort::FileInput(_, br) => port_read_str_fn!(br, read_line),
            SteelPort::StdInput(br) => port_read_str_fn!(br, read_line),
            // FIXME: fix this and the functions below
            _x => stop!(Generic => "read-line"),
        }
    }

    pub fn read_all_str(&self) -> Result<(usize, String)> {
        match self {
            SteelPort::FileInput(_, br) => port_read_str_fn!(br, read_to_string),
            SteelPort::StdInput(br) => port_read_str_fn!(br, read_to_string),
            _x => stop!(Generic => "read-all-str"),
        }
    }

    pub fn read_char(&mut self) -> Result<(usize, char)> {
        // FIXME: this only reads 1 u8 and casts it to char
        macro_rules! port_read_chr(
            ($br: ident) => {{
                let br = &mut *$br.borrow_mut();
                let mut chr = [0; 1];
                br.read_exact(&mut chr)?;
                Ok((1, chr[0] as char))
            }};
        );

        match self {
            SteelPort::FileInput(_, br) => port_read_chr!(br),
            SteelPort::StdInput(br) => port_read_chr!(br),
            _x => stop!(Generic => "read-char"),
        }
    }

    // pub fn read_u8(&mut self) -> Result<(usize, u8)> {
    //     match self {
    //         SteelPort::StringInput(_, br) => {
    //             let br = &mut *br.borrow_mut();
    //             let mut u8s = [0; 1];
    //             br.read_exact(&mut u8s)?;

    //             Ok((1, u8s[0]))
    //         }
    //         _x => stop!(Generic => "read-u8"),
    //     }
    // }

    // pub fn read_all_u8(&mut self) -> Result<(usize, Vec<u8>)> {
    //     match self {
    //         SteelPort::StringInput(_, br) => {
    //             let br = &mut *br.borrow_mut();
    //             let mut u8s = vec![];
    //             let size = br.read_to_end(&mut u8s)?;

    //             Ok((size, u8s))
    //         }
    //         _x => stop!(Generic => "read-all-u8"),
    //     }
    // }

    // pub fn with_chars<F, T>(&mut self, f: F) -> SResult<T>
    // where
    //     F: FnOnce(&mut Iterator<Item = char>) -> SResult<T>,
    // {
    //     macro_rules! with_chars(
    //         ($br: ident) => {{
    //             let br = &mut *$br.borrow_mut();
    //             let mut chars = Chars::new(br);
    //             f(&mut chars)
    //         }};
    //     );

    //     match self {
    //         SteelPort::FileInput(_, br) => with_chars!(br),
    //         SteelPort::StdInput(br) => with_chars!(br),
    //         _x => stop!(Generic => "chars", "TODO:PORT_NAME_HERE"),
    //     }
    // }

    //
    // Write functions
    //
    pub fn write_string(&mut self, string: &str) -> Result<()> {
        macro_rules! write_string(
            ($br: ident) => {{
                let br = &mut *$br.borrow_mut();
                write!(br, "{}", string)?;
                br.flush()?;
            }};
        );

        match self {
            SteelPort::FileOutput(_, br) => write_string!(br),
            SteelPort::StdOutput(br) => write_string!(br),
            _x => stop!(Generic => "write-string"),
        };

        Ok(())
    }

    //
    // Checks
    //
    pub fn is_input(&self) -> bool {
        match self {
            SteelPort::FileInput(_, _) => true,
            // SteelPort::StringInput(_, _) => true,
            SteelPort::StdInput(_) => true,
            _ => false,
        }
    }

    pub fn is_output(&self) -> bool {
        match self {
            SteelPort::FileOutput(_, _) => true,
            // SteelPort::StringOutput(_, _) => true,
            SteelPort::StdOutput(_) => true,
            _ => false,
        }
    }

    pub fn is_textual(&self) -> bool {
        match self {
            SteelPort::FileInput(_, _) => true,
            SteelPort::FileOutput(_, _) => true,
            SteelPort::StdOutput(_) => true,
            SteelPort::StdInput(_) => true,
            _ => false,
        }
    }

    // pub fn is_binary(&self) -> bool {
    //     match self {
    //         SteelPort::StringInput(_, _) => true,
    //         SteelPort::StringOutput(_, _) => true,
    //         _ => false,
    //     }
    // }

    pub fn default_current_input_port() -> Self {
        SteelPort::StdInput(new_rc_ref_cell(io::stdin()))
    }

    pub fn default_current_output_port() -> Self {
        SteelPort::StdOutput(new_rc_ref_cell(io::stdout()))
    }
}

// pub fn default_current_input_port() -> SteelPort {
//     // TODO: current_input should be changable
//     SteelPort::StdInput(new_rc_ref_cell(io::stdin()))
// }

// pub fn default_current_output_port() -> SteelPort {
//     // TODO: current_output should be changable
//     SteelPort::StdOutput(new_rc_ref_cell(io::stdout()))
// }
