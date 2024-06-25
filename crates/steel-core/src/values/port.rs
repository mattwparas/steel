use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::io::Cursor;
use std::io::{BufReader, BufWriter, Stdin, Stdout};
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::sync::Arc;
use std::sync::Mutex;

// use serr::{SErr, SResult};
// use utils::chars::Chars;
// use utils::{new_rc_ref_cell, RcRefCell};

use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::gc::GcMut;
use crate::rerrs;
use crate::rvals::Result;
use crate::SteelErr;

// use crate::rvals::{new_rc_ref_cell, RcRefSteelVal};

use std::cell::RefCell;
use std::rc::Rc;

// pub<T> type RcRefCell: Rc<RcRefCell<T>>;

pub type RcRefCell<T> = Rc<RefCell<T>>;
pub fn new_rc_ref_cell<T>(x: T) -> RcRefCell<T> {
    Rc::new(RefCell::new(x))
}

thread_local! {
    // TODO: This needs to be per engine, not global, and functions should accept the port they use
    // Probably by boxing up the port that gets used
    pub static DEFAULT_OUTPUT_PORT: GcMut<SteelPort> = Gc::new_mut(SteelPort { port: Gc::new_mut(SteelPortRepr::StdOutput(io::stdout())) } );
    pub static CAPTURED_OUTPUT_PORT: GcMut<BufWriter<Vec<u8>>> = Gc::new_mut(BufWriter::new(Vec::new()));
}

#[derive(Debug, Clone)]
pub struct SteelPort {
    pub(crate) port: GcMut<SteelPortRepr>,
}

// #[derive(Debug)]
pub enum SteelPortRepr {
    FileInput(String, BufReader<File>),
    FileOutput(String, BufWriter<File>),
    StdInput(Stdin),
    StdOutput(Stdout),
    ChildStdOutput(BufReader<ChildStdout>),
    ChildStdInput(BufWriter<ChildStdin>),
    StringInput(BufReader<Cursor<Vec<u8>>>),
    StringOutput(BufWriter<Vec<u8>>),
    DynWriter(Arc<Mutex<dyn Write + Send + Sync>>),
    // DynReader(Box<dyn Read>),
    Closed,
}

impl std::fmt::Debug for SteelPortRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SteelPortRepr::FileInput(name, w) => {
                f.debug_tuple("FileInput").field(name).field(w).finish()
            }
            SteelPortRepr::FileOutput(name, w) => {
                f.debug_tuple("FileOutput").field(name).field(w).finish()
            }
            SteelPortRepr::StdInput(s) => f.debug_tuple("StdInput").field(s).finish(),
            SteelPortRepr::StdOutput(s) => f.debug_tuple("StdOutput").field(s).finish(),
            SteelPortRepr::ChildStdOutput(s) => f.debug_tuple("ChildStdOutput").field(s).finish(),
            SteelPortRepr::ChildStdInput(s) => f.debug_tuple("ChildStdInput").field(s).finish(),
            SteelPortRepr::StringInput(s) => f.debug_tuple("StringInput").field(s).finish(),
            SteelPortRepr::StringOutput(s) => f.debug_tuple("StringOutput").field(s).finish(),
            SteelPortRepr::DynWriter(_) => f.debug_tuple("DynWriter").field(&"#<opaque>").finish(),
            SteelPortRepr::Closed => f.debug_tuple("Closed").finish(),
        }
    }
}

pub enum SendablePort {
    StdInput(Stdin),
    StdOutput(Stdout),
    BoxDynWriter(Arc<Mutex<dyn Write + Send + Sync>>),
    Closed,
}

impl SendablePort {
    fn from_port_repr(value: &SteelPortRepr) -> Result<SendablePort> {
        match value {
            SteelPortRepr::StdInput(_) => Ok(SendablePort::StdInput(io::stdin())),
            SteelPortRepr::StdOutput(_) => Ok(SendablePort::StdOutput(io::stdout())),
            SteelPortRepr::Closed => Ok(SendablePort::Closed),
            _ => {
                stop!(Generic => "Unable to send port across threads: {:?}", value)
            }
        }
    }

    pub fn from_port(value: SteelPort) -> Result<SendablePort> {
        Self::from_port_repr(&value.port.read())
    }
}

impl SteelPort {
    pub fn from_sendable_port(value: SendablePort) -> Self {
        match value {
            SendablePort::StdInput(s) => SteelPort {
                port: Gc::new_mut(SteelPortRepr::StdInput(s)),
            },
            SendablePort::StdOutput(s) => SteelPort {
                port: Gc::new_mut(SteelPortRepr::StdOutput(s)),
            },
            SendablePort::Closed => SteelPort {
                port: Gc::new_mut(SteelPortRepr::Closed),
            },
            SendablePort::BoxDynWriter(w) => SteelPort {
                port: Gc::new_mut(SteelPortRepr::DynWriter(w)),
            },
        }
    }
}

// TODO: Probably replace this with dynamic dispatch over writers?
// #[derive(Debug, Clone)]
// pub enum SteelPort {
//     FileInput(String, RcRefCell<BufReader<File>>),
//     FileOutput(String, RcRefCell<BufWriter<File>>),
//     StdInput(RcRefCell<Stdin>),
//     StdOutput(RcRefCell<Stdout>),
//     ChildStdOutput(RcRefCell<BufReader<ChildStdout>>),
//     ChildStdInput(RcRefCell<BufWriter<ChildStdin>>),
//     // StringInput(RcRefCell<BufReader<&[u8]>>),
//     StringOutput(RcRefCell<BufWriter<Vec<u8>>>),
//     // DynWriter(Rc<RefCell<Box<dyn Write>>>),
//     // DynReader(Rc<RefCell<Box<dyn Read>>>),
//     Closed,
// }

#[macro_export]
macro_rules! port_read_str_fn(
    ($br: ident, $fn: ident) => {{
        let mut result = String::new();
        let size = $br.$fn(&mut result)?;
        Ok((size, result))
    }};
);

impl SteelPortRepr {
    pub fn read_line(&mut self) -> Result<(usize, String)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br, read_line),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br, read_line),

            SteelPortRepr::ChildStdOutput(br) => {
                port_read_str_fn!(br, read_line)
            }

            // SteelPort::ChildStdOutput(br) => port_read_str_fn!(br, read_line),
            // FIXME: fix this and the functions below
            _x => stop!(Generic => "read-line"),
        }
    }

    pub fn flush(&mut self) -> Result<()> {
        match self {
            SteelPortRepr::FileOutput(_, s) => Ok(s.flush()?),
            SteelPortRepr::StdOutput(s) => Ok(s.flush()?),
            SteelPortRepr::ChildStdInput(s) => Ok(s.flush()?),
            SteelPortRepr::StringOutput(s) => Ok(s.flush()?),
            SteelPortRepr::DynWriter(s) => Ok(s.lock().unwrap().flush()?),
            SteelPortRepr::Closed => Ok(()),
            _ => stop!(TypeMismatch => "expected an output port, found: {:?}", self),
        }
    }

    pub fn read_all_str(&mut self) -> Result<(usize, String)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br, read_to_string),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br, read_to_string),
            _x => stop!(Generic => "read-all-str"),
        }
    }

    pub fn read_char(&mut self) -> Result<(usize, char)> {
        // FIXME: this only reads 1 u8 and casts it to char
        macro_rules! port_read_chr(
            ($br: ident) => {{
                let mut chr = [0; 1];
                $br.read_exact(&mut chr)?;
                Ok((1, chr[0] as char))
            }};
        );

        match self {
            SteelPortRepr::FileInput(_, br) => port_read_chr!(br),
            SteelPortRepr::StdInput(br) => port_read_chr!(br),
            _x => stop!(Generic => "read-char"),
        }
    }

    pub fn write_char(&mut self, c: char) -> Result<()> {
        macro_rules! write_string(
            ($br: ident) => {{
                write!($br, "{}", c)?;
                $br.flush()?;
            }};
        );

        match self {
            SteelPortRepr::FileOutput(_, br) => write_string!(br),
            SteelPortRepr::StringOutput(br) => write_string!(br),
            SteelPortRepr::StdOutput(out) => {
                let mut br = out.lock();
                write!(br, "{}", c)?;
                br.flush()?;
            }
            SteelPortRepr::DynWriter(o) => {
                let mut br = o.lock().unwrap();
                write!(br, "{}", c)?;
                br.flush()?;
            }
            _x => stop!(Generic => "write-car"),
        };

        Ok(())
    }

    pub fn write_string(&mut self, string: &str) -> Result<()> {
        macro_rules! write_string(
            ($br: ident) => {{
                write!($br, "{}", string)?;
                $br.flush()?;
            }};
        );

        match self {
            SteelPortRepr::FileOutput(_, br) => write_string!(br),
            SteelPortRepr::StdOutput(out) => {
                let mut br = out.lock();
                write!(br, "{}", string)?;
                br.flush()?;
            }
            SteelPortRepr::DynWriter(o) => {
                let mut br = o.lock().unwrap();
                write!(br, "{}", string)?;
                br.flush()?;
            }
            SteelPortRepr::StringOutput(br) => write_string!(br),
            _x => stop!(Generic => "write-string"),
        };

        Ok(())
    }

    pub fn write_string_line(&mut self, string: &str) -> Result<()> {
        macro_rules! write_string(
            ($br: ident) => {{
                write!($br, "{}\n", string)?;
                $br.flush()?;
            }};
        );

        match self {
            SteelPortRepr::FileOutput(_, br) => write_string!(br),
            SteelPortRepr::StdOutput(br) => write_string!(br),
            SteelPortRepr::ChildStdInput(br) => write_string!(br),
            SteelPortRepr::StringOutput(br) => write_string!(br),
            SteelPortRepr::DynWriter(br) => {
                let mut br = br.lock().unwrap();
                write_string!(br)
            }
            _x => stop!(Generic => "write-string"),
        };

        Ok(())
    }

    pub fn is_input(&self) -> bool {
        matches!(
            self,
            SteelPortRepr::FileInput(_, _) | SteelPortRepr::StdInput(_)
        )
    }

    pub fn is_output(&self) -> bool {
        matches!(
            self,
            SteelPortRepr::FileOutput(_, _)
                | SteelPortRepr::StdOutput(_)
                | SteelPortRepr::DynWriter(_)
        )
    }

    pub fn is_textual(&self) -> bool {
        matches!(
            self,
            SteelPortRepr::FileInput(_, _)
                | SteelPortRepr::FileOutput(_, _)
                | SteelPortRepr::StdOutput(_)
                | SteelPortRepr::StdInput(_)
        )
    }

    pub fn get_output_string(&mut self) -> Result<String> {
        if let SteelPortRepr::StringOutput(s) = self {
            // Ensure that this is flushed
            s.flush()?;

            String::from_utf8(s.get_ref().to_vec())
                .map_err(|err| SteelErr::new(rerrs::ErrorKind::Generic, err.to_string()))
        } else {
            stop!(TypeMismatch => "get-output-string expects an output port, found: {:?}", self);
        }
    }

    pub fn close_output_port(&mut self) -> Result<()> {
        match self {
            SteelPortRepr::FileOutput(_, _) | SteelPortRepr::StdOutput(_) => {
                *self = SteelPortRepr::Closed;
                Ok(())
            }
            _ => {
                stop!(TypeMismatch => "close-output-port expects an output port, found: {:?}", self)
            }
        }
    }
}

impl SteelPort {
    pub fn new_textual_file_input(path: &str) -> Result<SteelPort> {
        let file = OpenOptions::new().read(true).open(path)?;

        Ok(SteelPort {
            port: Gc::new_mut(SteelPortRepr::FileInput(
                path.to_string(),
                BufReader::new(file),
            )),
        })
    }

    pub fn new_textual_file_output(path: &str) -> Result<SteelPort> {
        let file = OpenOptions::new()
            .truncate(true)
            .write(true)
            .create(true)
            .open(path)?;

        Ok(SteelPort {
            port: Gc::new_mut(SteelPortRepr::FileOutput(
                path.to_string(),
                BufWriter::new(file),
            )),
        })
    }

    pub fn new_input_port_string(string: String) -> SteelPort {
        SteelPort {
            port: Gc::new_mut(SteelPortRepr::StringInput(BufReader::new(Cursor::new(
                string.into_bytes(),
            )))),
        }
    }

    pub fn new_output_port() -> SteelPort {
        SteelPort {
            port: Gc::new_mut(SteelPortRepr::StringOutput(BufWriter::new(Vec::new()))),
        }
    }

    //
    // Read functions
    //
    pub fn read_line(&self) -> Result<(usize, String)> {
        self.port.write().read_line()
    }

    // TODO: Implement the rest of the flush methods
    pub fn flush(&self) -> Result<()> {
        self.port.write().flush()
    }

    pub fn read_all_str(&self) -> Result<(usize, String)> {
        self.port.write().read_all_str()
    }

    pub fn read_char(&self) -> Result<(usize, char)> {
        self.port.write().read_char()
    }

    pub fn write_char(&self, c: char) -> Result<()> {
        self.port.write().write_char(c)
    }

    //
    // Write functions
    //
    pub fn write_string(&self, string: &str) -> Result<()> {
        self.port.write().write_string(string)
    }

    pub fn write_string_line(&self, string: &str) -> Result<()> {
        self.port.write().write_string_line(string)
    }

    //
    // Checks
    //
    pub fn is_input(&self) -> bool {
        self.port.read().is_input()
    }

    pub fn is_output(&self) -> bool {
        self.port.read().is_output()
    }

    pub fn is_textual(&self) -> bool {
        self.port.read().is_textual()
    }

    pub fn default_current_input_port() -> Self {
        SteelPort {
            port: Gc::new_mut(SteelPortRepr::StdInput(io::stdin())),
        }
    }

    pub fn default_current_output_port() -> Self {
        if cfg!(test) {
            // Write out to thread safe port
            SteelPort {
                port: Gc::new_mut(SteelPortRepr::DynWriter(Arc::new(Mutex::new(
                    BufWriter::new(Vec::new()),
                )))),
            }
        } else {
            SteelPort {
                port: Gc::new_mut(SteelPortRepr::StdOutput(io::stdout())),
            }
        }
    }

    pub fn get_output_string(&self) -> Result<String> {
        self.port.write().get_output_string()
    }

    pub fn close_output_port(&self) -> Result<()> {
        self.port.write().close_output_port()
    }
}
