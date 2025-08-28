use std::fs::File;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::io::Cursor;
use std::io::Stderr;
use std::io::{BufReader, BufWriter, Stdin, Stdout};
use std::net::TcpStream;
use std::process::ChildStderr;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::sync::Arc;
use std::sync::Mutex;

use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::gc::GcMut;
use crate::rvals::Result;
use crate::SteelVal;

// use crate::rvals::{new_rc_ref_cell, RcRefSteelVal};

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

// pub trait PortLike {
//     fn as_any_ref(&self) -> &dyn Any;
//     fn into_port(self) -> SteelVal;
// }

// impl<T: Write + Send + Sync + 'static> PortLike for T {
//     fn as_any_ref(&self) -> &dyn Any {
//         self as &dyn Any
//     }

//     //
//     fn into_port(self) -> SteelVal {}
// }

// #[derive(Debug)]
pub enum SteelPortRepr {
    FileInput(String, BufReader<File>),
    FileOutput(String, BufWriter<File>),
    StdInput(Stdin),
    StdOutput(Stdout),
    StdError(Stderr),
    ChildStdOutput(BufReader<ChildStdout>),
    ChildStdError(BufReader<ChildStderr>),
    ChildStdInput(BufWriter<ChildStdin>),
    StringInput(Cursor<Vec<u8>>),
    StringOutput(Vec<u8>),

    // TODO: This does not need to be Arc<Mutex<dyn ...>> - it can
    // get away with just Box<dyn ...> - and also it should be dyn Portlike
    // with blanket trait impls to do the thing otherwise.
    DynWriter(Arc<Mutex<dyn Write + Send + Sync>>),
    DynReader(BufReader<Box<dyn Read + Send + Sync>>),
    TcpStream(TcpStream),
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
            SteelPortRepr::ChildStdError(s) => f.debug_tuple("ChildStdError").field(s).finish(),
            SteelPortRepr::ChildStdInput(s) => f.debug_tuple("ChildStdInput").field(s).finish(),
            SteelPortRepr::StringInput(s) => f.debug_tuple("StringInput").field(s).finish(),
            SteelPortRepr::StringOutput(s) => f.debug_tuple("StringOutput").field(s).finish(),
            SteelPortRepr::DynWriter(_) => f.debug_tuple("DynWriter").field(&"#<opaque>").finish(),
            SteelPortRepr::DynReader(_) => f
                .debug_tuple("DynReader")
                .field(&"#<opaque-reader>")
                .finish(),
            SteelPortRepr::TcpStream(_) => f.debug_tuple("TcpStream").finish(),
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
            SendablePort::StdError(s) => SteelPort {
                port: Gc::new_mut(SteelPortRepr::StdError(s)),
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
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s, read_line),

            SteelPortRepr::ChildStdOutput(br) => {
                port_read_str_fn!(br, read_line)
            }

            SteelPortRepr::DynReader(br) => port_read_str_fn!(br, read_line),

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
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br, read_to_string),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br, read_to_string),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br, read_to_string),
            SteelPortRepr::StringInput(br) => port_read_str_fn!(br, read_to_string),
            x => stop!(Generic => "read-all-str: {:?}", x),
        }
    }

    pub fn read_char(&mut self) -> Result<MaybeBlocking<Option<char>>> {
        let mut buf = [0; 4];

        for i in 0..4 {
            let result = self.read_byte()?;

            let b = match result {
                MaybeBlocking::Nonblocking(Some(b)) => b,
                MaybeBlocking::Nonblocking(None) => {
                    if i == 0 {
                        return Ok(MaybeBlocking::Nonblocking(None));
                    } else {
                        stop!(ConversionError => "unable to decode character, found {:?}", &buf[0..=i]);
                    }
                }
                MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
            };

            buf[i] = b;

            match std::str::from_utf8(&buf[0..=i]) {
                Ok(s) => return Ok(MaybeBlocking::Nonblocking(s.chars().next())),
                Err(err) if err.error_len().is_some() => {
                    stop!(ConversionError => "unable to decode character, found {:?}", &buf[0..=i]);
                }
                _ => {}
            }
        }

        stop!(ConversionError => "unable to decode character, found {:?}", buf);
    }

    pub fn read_bytes_amt(&mut self, buf: &mut [u8]) -> Result<MaybeBlocking<(usize, bool)>> {
        let result = match self {
            SteelPortRepr::FileInput(_, reader) => reader.read(buf),
            SteelPortRepr::StdInput(stdin) => stdin.read(buf),
            SteelPortRepr::ChildStdOutput(output) => output.read(buf),
            SteelPortRepr::ChildStdError(output) => output.read(buf),
            SteelPortRepr::StringInput(reader) => reader.read(buf),
            SteelPortRepr::DynReader(reader) => reader.read(buf),
            SteelPortRepr::TcpStream(t) => t.read(buf),
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => stop!(ContractViolation => "expected input-port?"),
            SteelPortRepr::Closed => return Ok(MaybeBlocking::Nonblocking((0, true))),
        };

        if let Err(err) = result {
            if err.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(MaybeBlocking::Nonblocking((0, false)));
            }

            if err.kind() == io::ErrorKind::WouldBlock {
                // TODO: If this would block, do something
                return Ok(MaybeBlocking::WouldBlock);
            }

            return Err(err.into());
        }

        Ok(MaybeBlocking::Nonblocking((result?, true)))
    }

    pub fn read_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        let mut byte = [0];

        let result = match self {
            SteelPortRepr::FileInput(_, reader) => reader.read_exact(&mut byte),
            SteelPortRepr::StdInput(stdin) => stdin.read_exact(&mut byte),
            SteelPortRepr::ChildStdOutput(output) => output.read_exact(&mut byte),
            SteelPortRepr::ChildStdError(output) => output.read_exact(&mut byte),
            SteelPortRepr::StringInput(reader) => reader.read_exact(&mut byte),
            SteelPortRepr::DynReader(reader) => reader.read_exact(&mut byte),
            SteelPortRepr::TcpStream(t) => {
                let amount = t.read(&mut byte)?;

                if amount == 0 {
                    stop!(Generic => "unexpected eof");
                } else {
                    Ok(())
                }
            }
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => stop!(ContractViolation => "expected input-port?"),
            SteelPortRepr::Closed => return Ok(MaybeBlocking::Nonblocking(None)),
        };

        if let Err(err) = result {
            if err.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(MaybeBlocking::Nonblocking(None));
            }

            return Err(err.into());
        }

        Ok(MaybeBlocking::Nonblocking(Some(byte[0])))
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
            SteelPortRepr::ChildStdError(output) => output.fill_buf().map(copy),
            SteelPortRepr::StringInput(reader) => reader.fill_buf().map(copy),
            SteelPortRepr::DynReader(reader) => reader.fill_buf().map(copy),
            SteelPortRepr::TcpStream(tcp) => tcp.peek(buf),
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

        let _ = self.write_without_flush(s.as_bytes())?;

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
                | SteelPortRepr::ChildStdError(_)
                | SteelPortRepr::StringInput(_)
        )
    }

    pub fn is_string_input(&self) -> bool {
        matches!(self, SteelPortRepr::StringInput(_))
    }

    pub fn is_file_input(&self) -> bool {
        matches!(self, SteelPortRepr::FileInput(_, _))
    }

    pub fn is_output(&self) -> bool {
        matches!(
            self,
            SteelPortRepr::FileOutput(_, _)
                | SteelPortRepr::StdOutput(_)
                | SteelPortRepr::StdError(_)
                | SteelPortRepr::DynWriter(_)
                | SteelPortRepr::ChildStdInput(_)
                | SteelPortRepr::StringOutput(_)
        )
    }

    pub fn get_output(&self) -> Result<Option<Vec<u8>>> {
        let buf: &Vec<u8> = if let SteelPortRepr::StringOutput(s) = self {
            s
        } else {
            return Ok(None);
        };

        Ok(Some(buf.clone()))
    }

    pub fn close_port(&mut self) {
        *self = SteelPortRepr::Closed;
    }

    pub fn close_output_port(&mut self) -> Result<()> {
        if self.is_output() {
            *self = SteelPortRepr::Closed;
            Ok(())
        } else {
            stop!(TypeMismatch => "close-output-port expects an output port, found: {:?}", self)
        }
    }

    pub fn close_input_port(&mut self) -> Result<()> {
        if self.is_input() {
            *self = SteelPortRepr::Closed;
            Ok(())
        } else {
            stop!(TypeMismatch => "close-input-port expects an input port, found: {:?}", self)
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
            // TODO: Should tcp streams be both input and output ports?
            SteelPortRepr::TcpStream(tcp) => tcp.write(buf)?,
            SteelPortRepr::FileInput(_, _)
            | SteelPortRepr::StdInput(_)
            | SteelPortRepr::DynReader(_)
            | SteelPortRepr::ChildStdOutput(_)
            | SteelPortRepr::ChildStdError(_)
            | SteelPortRepr::StringInput(_) => stop!(ContractViolation => "expected output-port?"),
            SteelPortRepr::Closed => stop!(Io => "port is closed"),
        };

        Ok(result)
    }

    pub fn write_without_flush(&mut self, buf: &[u8]) -> Result<usize> {
        macro_rules! write_and_flush(
            ($br: expr) => {{
                let result = $br.write(buf)?;
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
            // TODO: Should tcp streams be both input and output ports?
            SteelPortRepr::TcpStream(tcp) => tcp.write(buf)?,
            SteelPortRepr::FileInput(_, _)
            | SteelPortRepr::StdInput(_)
            | SteelPortRepr::DynReader(_)
            | SteelPortRepr::ChildStdOutput(_)
            | SteelPortRepr::ChildStdError(_)
            | SteelPortRepr::StringInput(_) => stop!(ContractViolation => "expected output-port?"),
            SteelPortRepr::Closed => stop!(Io => "port is closed"),
        };

        Ok(result)
    }
}

pub enum MaybeBlocking<T> {
    Nonblocking(T),
    WouldBlock,
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

    pub fn new_textual_file_output_with_options(
        path: &str,
        open_options: OpenOptions,
    ) -> Result<SteelPort> {
        let file = open_options.open(path)?;

        Ok(SteelPort {
            port: Gc::new_mut(SteelPortRepr::FileOutput(
                path.to_string(),
                BufWriter::new(file),
            )),
        })
    }

    pub fn new_input_port_string(string: String) -> SteelPort {
        SteelPort {
            port: Gc::new_mut(SteelPortRepr::StringInput(Cursor::new(string.into_bytes()))),
        }
    }

    pub fn new_input_port_bytevector(vec: Vec<u8>) -> SteelPort {
        SteelPort {
            port: Gc::new_mut(SteelPortRepr::StringInput(Cursor::new(vec))),
        }
    }

    pub fn new_output_port_string() -> SteelPort {
        SteelPort {
            port: Gc::new_mut(SteelPortRepr::StringOutput(Vec::new())),
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

    pub fn read_char(&self) -> Result<MaybeBlocking<Option<char>>> {
        self.port.write().read_char()
    }

    pub fn read_byte(&self) -> Result<MaybeBlocking<Option<u8>>> {
        self.port.write().read_byte()
    }

    pub fn read_bytes(&self, amount: usize) -> Result<MaybeBlocking<Vec<u8>>> {
        // TODO: This is going to allocate unnecessarily
        let mut buf = vec![0; amount];
        match self.port.write().read_bytes_amt(&mut buf)? {
            MaybeBlocking::Nonblocking((amount_read, _)) => {
                buf.truncate(amount_read);
                Ok(MaybeBlocking::Nonblocking(buf))
            }
            MaybeBlocking::WouldBlock => Ok(MaybeBlocking::WouldBlock),
        }
    }

    pub fn read_bytes_into_buf(&self, buf: &mut [u8]) -> Result<MaybeBlocking<(usize, bool)>> {
        self.port.write().read_bytes_amt(buf)
    }

    pub fn peek_byte(&self) -> Result<Option<u8>> {
        self.port.write().peek_byte()
    }

    //
    // Write functions
    //
    pub fn write_char(&self, c: char) -> Result<()> {
        self.port.write().write_char(c)
    }

    pub fn write(&self, buf: &[u8]) -> Result<()> {
        let _ = self.port.write().write(buf)?;

        Ok(())
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

    pub fn is_string_input(&self) -> bool {
        self.port.read().is_string_input()
    }

    pub fn is_file_input(&self) -> bool {
        self.port.read().is_file_input()
    }

    pub fn is_output(&self) -> bool {
        self.port.read().is_output()
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

    pub fn default_current_error_port() -> Self {
        if cfg!(test) {
            // Write out to thread safe port
            SteelPort {
                port: Gc::new_mut(SteelPortRepr::DynWriter(Arc::new(Mutex::new(
                    BufWriter::new(Vec::new()),
                )))),
            }
        } else {
            SteelPort {
                port: Gc::new_mut(SteelPortRepr::StdError(io::stderr())),
            }
        }
    }

    pub fn get_output(&self) -> Result<Option<Vec<u8>>> {
        self.port.write().get_output()
    }

    pub fn close_port(&self) {
        self.port.write().close_port()
    }

    pub fn close_output_port(&self) -> Result<()> {
        self.port.write().close_output_port()
    }

    pub fn close_input_port(&self) -> Result<()> {
        self.port.write().close_input_port()
    }
}

#[cfg(not(feature = "sync"))]
thread_local! {
    pub static WOULD_BLOCK_OBJECT: once_cell::unsync::Lazy<(crate::SteelVal,
        super::structs::StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        super::structs::make_struct_singleton("would-block")
    });
}

#[cfg(feature = "sync")]
pub static WOULD_BLOCK_OBJECT: once_cell::sync::Lazy<(
    crate::SteelVal,
    super::structs::StructTypeDescriptor,
)> = once_cell::sync::Lazy::new(|| super::structs::make_struct_singleton("eof"));

pub fn would_block() -> SteelVal {
    #[cfg(feature = "sync")]
    {
        WOULD_BLOCK_OBJECT.0.clone()
    }

    #[cfg(not(feature = "sync"))]
    {
        WOULD_BLOCK_OBJECT.with(|eof| eof.0.clone())
    }
}
