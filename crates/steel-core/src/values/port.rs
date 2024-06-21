use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::io::Cursor;
use std::io::Stderr;
use std::io::{BufReader, BufWriter, Stdin, Stdout};
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::sync::Arc;
use std::sync::Mutex;

// use serr::{SErr, SResult};
// use utils::chars::Chars;
// use utils::{new_rc_ref_cell, RcRefCell};

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
    pub static DEFAULT_OUTPUT_PORT: RcRefCell<SteelPort> = new_rc_ref_cell(SteelPort { port: new_rc_ref_cell(SteelPortRepr::StdOutput(io::stdout())) } );
    pub static CAPTURED_OUTPUT_PORT: RcRefCell<BufWriter<Vec<u8>>> = new_rc_ref_cell(BufWriter::new(Vec::new()));

    // pub static STANDARD_OUT: SteelPort = SteelPort::StringOutput(Rc::new(RefCell::new(BufWriter::new(Vec::new()))));
}

#[derive(Debug, Clone)]
pub struct SteelPort {
    pub(crate) port: RcRefCell<SteelPortRepr>,
}

// #[derive(Debug)]
pub enum SteelPortRepr {
    FileInput(String, BufReader<File>),
    FileOutput(String, BufWriter<File>),
    StdInput(Stdin),
    StdOutput(Stdout),
    StdError(Stderr),
    ChildStdOutput(BufReader<ChildStdout>),
    ChildStdInput(BufWriter<ChildStdin>),
    StringInput(Cursor<Vec<u8>>),
    StringOutput(Vec<u8>),
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
            SteelPortRepr::StdError(s) => f.debug_tuple("StdError").field(s).finish(),
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
    StdError(Stderr),
    BoxDynWriter(Arc<Mutex<dyn Write + Send + Sync>>),
    Closed,
}

impl SendablePort {
    fn from_port_repr(value: &SteelPortRepr) -> Result<SendablePort> {
        match value {
            SteelPortRepr::StdInput(_) => Ok(SendablePort::StdInput(io::stdin())),
            SteelPortRepr::StdOutput(_) => Ok(SendablePort::StdOutput(io::stdout())),
            SteelPortRepr::StdError(_) => Ok(SendablePort::StdError(io::stderr())),
            SteelPortRepr::Closed => Ok(SendablePort::Closed),
            _ => {
                stop!(Generic => "Unable to send port across threads: {:?}", value)
            }
        }
    }

    pub fn from_port(value: SteelPort) -> Result<SendablePort> {
        Self::from_port_repr(&value.port.borrow())
    }
}

impl SteelPort {
    pub fn from_sendable_port(value: SendablePort) -> Self {
        match value {
            SendablePort::StdInput(s) => SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::StdInput(s)),
            },
            SendablePort::StdOutput(s) => SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::StdOutput(s)),
            },
            SendablePort::StdError(s) => SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::StdError(s)),
            },
            SendablePort::Closed => SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::Closed),
            },
            SendablePort::BoxDynWriter(w) => SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::DynWriter(w)),
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
                // let buf_reader = BufReader::new(br.borrow_mut().as_mut());

                port_read_str_fn!(br, read_line)

                // todo!()

                // buf_reader
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

    pub fn read_char(&mut self) -> Result<Option<char>> {
        let mut buf = [0; 4];

        for i in 0..4 {
            let result = self.read_byte()?;

            let b = match result {
                Some(b) => b,
                None => {
                    if i == 0 {
                        return Ok(None);
                    } else {
                        stop!(ConversionError => "unable to decode character, found {:?}", &buf[0..=i]);
                    }
                }
            };

            buf[i] = b;

            match std::str::from_utf8(&buf[0..=i]) {
                Ok(s) => return Ok(s.chars().next()),
                Err(err) if err.error_len().is_some() => {
                    stop!(ConversionError => "unable to decode character, found {:?}", &buf[0..=i]);
                }
                _ => {}
            }
        }

        stop!(ConversionError => "unable to decode character, found {:?}", buf);
    }

    pub fn read_byte(&mut self) -> Result<Option<u8>> {
        let mut byte = [0];

        let result = match self {
            SteelPortRepr::FileInput(_, reader) => reader.read_exact(&mut byte),
            SteelPortRepr::StdInput(stdin) => stdin.read_exact(&mut byte),
            SteelPortRepr::ChildStdOutput(output) => output.read_exact(&mut byte),
            SteelPortRepr::StringInput(reader) => reader.read_exact(&mut byte),
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => stop!(ContractViolation => "expected input-port?"),
            SteelPortRepr::Closed => return Ok(None),
        };

        if let Err(err) = result {
            if err.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(None);
            }

            return Err(err.into());
        }

        Ok(Some(byte[0]))
    }

    pub fn peek_byte(&mut self) -> Result<Option<u8>> {
        let mut buf = [0];

        let result = self.peek(&mut buf)?;

        if result == 0 {
            Ok(None)
        } else {
            Ok(Some(buf[0]))
        }
    }

    // This would not work for `peek_char`, since a `BufRead` will not work for this purpose.
    // We need a way to force-fill the internal buffer up to 4 bytes. `fill_buf()` only tries to
    // fill _if_ the internal buffer is empty, and without any guarantees of returned size.
    fn peek(&mut self, buf: &mut [u8]) -> Result<usize> {
        let copy = |src: &[u8]| {
            let len = src.len().min(buf.len());

            buf.copy_from_slice(&src[0..len]);

            len
        };

        let result = match self {
            SteelPortRepr::FileInput(_, reader) => reader.fill_buf().map(copy),
            SteelPortRepr::StdInput(stdin) => {
                let mut lock = stdin.lock();
                lock.fill_buf().map(copy)
            }
            SteelPortRepr::ChildStdOutput(output) => output.fill_buf().map(copy),
            SteelPortRepr::StringInput(reader) => reader.fill_buf().map(copy),
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => stop!(ContractViolation => "expected input-port?"),
            SteelPortRepr::Closed => return Ok(0),
        }?;

        Ok(result)
    }

    pub fn write_char(&mut self, c: char) -> Result<()> {
        let mut buf = [0; 4];

        let s = c.encode_utf8(&mut buf);

        let _ = self.write(s.as_bytes())?;

        Ok(())
    }

    pub fn write_string_line(&mut self, string: &str) -> Result<()> {
        let _ = self.write(string.as_bytes())?;
        let _ = self.write(b"\n")?;

        Ok(())
    }

    pub fn is_input(&self) -> bool {
        matches!(
            self,
            SteelPortRepr::FileInput(_, _)
                | SteelPortRepr::StdInput(_)
                | SteelPortRepr::ChildStdOutput(_)
                | SteelPortRepr::StringInput(_)
        )
    }

    pub fn is_output(&self) -> bool {
        matches!(
            self,
            SteelPortRepr::FileOutput(_, _)
                | SteelPortRepr::StdOutput(_)
                | SteelPortRepr::DynWriter(_)
                | SteelPortRepr::ChildStdInput(_)
                | SteelPortRepr::StringOutput(_)
        )
    }

    pub fn get_output_string(&mut self) -> Result<String> {
        let buf: &mut Vec<u8> = if let SteelPortRepr::StringOutput(s) = self {
            s
        } else {
            stop!(TypeMismatch => "get-output-string expects an output port, found: {:?}", self);
        };

        String::from_utf8(buf.clone())
            .map_err(|err| SteelErr::new(rerrs::ErrorKind::Generic, err.to_string()))
    }

    pub fn close_output_port(&mut self) -> Result<()> {
        if self.is_output() {
            *self = SteelPortRepr::Closed;
            Ok(())
        } else {
            stop!(TypeMismatch => "close-output-port expects an output port, found: {:?}", self)
        }
    }

    pub fn write(&mut self, buf: &[u8]) -> Result<usize> {
        macro_rules! write_and_flush(
            ($br: expr) => {{
                let result = $br.write(buf)?;
                $br.flush()?;
                result
            }};
        );

        let result = match self {
            SteelPortRepr::FileOutput(_, writer) => write_and_flush![writer],
            SteelPortRepr::StdOutput(writer) => write_and_flush![writer],
            SteelPortRepr::StdError(writer) => write_and_flush![writer],
            SteelPortRepr::ChildStdInput(writer) => write_and_flush![writer],
            SteelPortRepr::StringOutput(writer) => write_and_flush![writer],
            SteelPortRepr::DynWriter(writer) => write_and_flush![writer.lock().unwrap()],
            SteelPortRepr::FileInput(_, _)
            | SteelPortRepr::StdInput(_)
            | SteelPortRepr::ChildStdOutput(_)
            | SteelPortRepr::StringInput(_) => stop!(ContractViolation => "expected output-port?"),
            SteelPortRepr::Closed => stop!(Io => "port is closed"),
        };

        Ok(result)
    }
}

impl SteelPort {
    pub fn new_textual_file_input(path: &str) -> Result<SteelPort> {
        let file = OpenOptions::new().read(true).open(path)?;

        Ok(SteelPort {
            port: new_rc_ref_cell(SteelPortRepr::FileInput(
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
            port: new_rc_ref_cell(SteelPortRepr::FileOutput(
                path.to_string(),
                BufWriter::new(file),
            )),
        })
    }

    pub fn new_input_port_string(string: String) -> SteelPort {
        SteelPort {
            port: new_rc_ref_cell(SteelPortRepr::StringInput(Cursor::new(string.into_bytes()))),
        }
    }

    pub fn new_output_port_string() -> SteelPort {
        SteelPort {
            port: new_rc_ref_cell(SteelPortRepr::StringOutput(Vec::new())),
        }
    }

    //
    // Read functions
    //
    pub fn read_line(&self) -> Result<(usize, String)> {
        self.port.borrow_mut().read_line()
    }

    // TODO: Implement the rest of the flush methods
    pub fn flush(&self) -> Result<()> {
        self.port.borrow_mut().flush()
    }

    pub fn read_all_str(&self) -> Result<(usize, String)> {
        self.port.borrow_mut().read_all_str()
    }

    pub fn read_char(&self) -> Result<Option<char>> {
        self.port.borrow_mut().read_char()
    }

    pub fn read_byte(&self) -> Result<Option<u8>> {
        self.port.borrow_mut().read_byte()
    }

    pub fn peek_byte(&self) -> Result<Option<u8>> {
        self.port.borrow_mut().peek_byte()
    }

    //
    // Write functions
    //
    pub fn write_char(&self, c: char) -> Result<()> {
        self.port.borrow_mut().write_char(c)
    }

    pub fn write(&self, buf: &[u8]) -> Result<()> {
        let _ = self.port.borrow_mut().write(buf)?;

        Ok(())
    }

    pub fn write_string_line(&self, string: &str) -> Result<()> {
        self.port.borrow_mut().write_string_line(string)
    }

    //
    // Checks
    //
    pub fn is_input(&self) -> bool {
        self.port.borrow().is_input()
    }

    pub fn is_output(&self) -> bool {
        self.port.borrow().is_output()
    }

    pub fn default_current_input_port() -> Self {
        SteelPort {
            port: new_rc_ref_cell(SteelPortRepr::StdInput(io::stdin())),
        }
    }

    pub fn default_current_output_port() -> Self {
        if cfg!(test) {
            // Write out to thread safe port
            SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::DynWriter(Arc::new(Mutex::new(
                    BufWriter::new(Vec::new()),
                )))),
            }
        } else {
            SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::StdOutput(io::stdout())),
            }
        }
    }

    pub fn default_current_error_port() -> Self {
        if cfg!(test) {
            // Write out to thread safe port
            SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::DynWriter(Arc::new(Mutex::new(
                    BufWriter::new(Vec::new()),
                )))),
            }
        } else {
            SteelPort {
                port: new_rc_ref_cell(SteelPortRepr::StdError(io::stderr())),
            }
        }
    }

    pub fn get_output_string(&self) -> Result<String> {
        self.port.borrow_mut().get_output_string()
    }

    pub fn close_output_port(&self) -> Result<()> {
        self.port.borrow_mut().close_output_port()
    }
}
