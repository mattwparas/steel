use std::fs::File;
use std::fs::OpenOptions;
use std::hash::Hash;
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

use crate::gc::shared::GcLock;
use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::gc::GcMut;
use crate::rvals::Result;
use crate::SteelVal;

// use crate::rvals::{new_rc_ref_cell, RcRefSteelVal};

thread_local! {
    // TODO: This needs to be per engine, not global, and functions should accept the port they use
    // Probably by boxing up the port that gets used
    pub static DEFAULT_OUTPUT_PORT: GcMut<SteelPort> = Gc::new_mut(SteelPort { port: Gc::new_lock(SteelPortRepr::StdOutput(io::stdout())) } );
    pub static CAPTURED_OUTPUT_PORT: GcMut<BufWriter<Vec<u8>>> = Gc::new_mut(BufWriter::new(Vec::new()));
}

#[derive(Debug, Clone)]
pub struct SteelPort {
    // TODO: Convert this to be a GcMut but with exclusive locks instead?
    // pub(crate) port: GcMut<SteelPortRepr>,
    pub(crate) port: GcLock<SteelPortRepr>,
}

impl PartialEq for SteelPort {
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(&self.port, &other.port)
    }
}

impl Eq for SteelPort {}

impl Hash for SteelPort {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Gc::as_ptr(&self.port).hash(state);
    }
}

impl std::fmt::Display for SteelPort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.port.read())
    }
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

#[derive(Debug)]
pub struct Peekable<R> {
    inner: R,
    peek: [u8; 4],
    idx: usize,
}

impl<T> Peekable<T> {
    pub fn new(inner: T) -> Self {
        Peekable {
            inner,
            peek: [0; 4],
            idx: 0,
        }
    }
}

impl<R: Read> Peekable<R> {
    fn read_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        if self.idx > 0 {
            let peek = self.peek[0];

            self.peek[..].rotate_left(1);
            self.idx -= 1;

            return Ok(MaybeBlocking::Nonblocking(Some(peek)));
        }

        let mut byte = [0];
        if let Err(err) = self.inner.read_exact(&mut byte) {
            if err.kind() == io::ErrorKind::UnexpectedEof {
                return Ok(MaybeBlocking::Nonblocking(None));
            }

            if err.kind() == io::ErrorKind::WouldBlock {
                return Ok(MaybeBlocking::WouldBlock);
            }

            return Err(err.into());
        }

        Ok(MaybeBlocking::Nonblocking(Some(byte[0])))
    }

    fn peek_char(&mut self) -> Result<MaybeBlocking<Option<char>>> {
        match read_full(&mut self.inner, &mut self.peek[self.idx..])? {
            MaybeBlocking::Nonblocking(n) => self.idx += n,
            MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
        };

        if self.idx == 0 {
            return Ok(MaybeBlocking::Nonblocking(None));
        }

        match std::str::from_utf8(&self.peek[0..self.idx]) {
            Ok(str) => Ok(MaybeBlocking::Nonblocking(str.chars().next())),
            Err(err) => {
                if err.valid_up_to() > 0 {
                    let s = std::str::from_utf8(&self.peek[0..err.valid_up_to()]).unwrap();
                    Ok(MaybeBlocking::Nonblocking(s.chars().next()))
                } else if let Some(len) = err.error_len() {
                    self.idx -= len;
                    self.peek[..].rotate_left(len);
                    Ok(MaybeBlocking::Nonblocking(Some(
                        char::REPLACEMENT_CHARACTER,
                    )))
                } else {
                    // if error_len is None, it means that there should be more
                    // bytes coming after, but we tried to fill the buf to a len
                    // of 4, the maximum, so that just means we got incomplete utf8
                    self.idx = 0;
                    Ok(MaybeBlocking::Nonblocking(Some(
                        char::REPLACEMENT_CHARACTER,
                    )))
                }
            }
        }
    }

    fn peek_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        if self.idx > 0 {
            Ok(MaybeBlocking::Nonblocking(Some(self.peek[0])))
        } else {
            match self.read_byte()? {
                MaybeBlocking::Nonblocking(None) => Ok(MaybeBlocking::Nonblocking(None)),
                MaybeBlocking::Nonblocking(Some(peek)) => {
                    self.peek[0] = peek;
                    self.idx += 1;
                    Ok(MaybeBlocking::Nonblocking(Some(peek)))
                }
                MaybeBlocking::WouldBlock => Ok(MaybeBlocking::WouldBlock),
            }
        }
    }

    pub fn read_bytes_amt(&mut self, mut buf: &mut [u8]) -> Result<MaybeBlocking<usize>> {
        let mut amt = 0;

        if !buf.is_empty() && self.idx > 0 {
            let len = usize::min(buf.len(), self.idx);
            for i in 0..len {
                buf[i] = self.peek[i];
            }

            amt += len;
            buf = &mut buf[len..];

            self.idx -= len;
            self.peek[..].rotate_left(len);
        }

        match read_full(&mut self.inner, buf)? {
            MaybeBlocking::Nonblocking(n) => amt += n,
            MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
        };

        Ok(MaybeBlocking::Nonblocking(amt))
    }
}

fn read_full<R: Read>(mut reader: R, mut buf: &mut [u8]) -> Result<MaybeBlocking<usize>> {
    let mut amt = 0;
    while !buf.is_empty() {
        match reader.read(buf) {
            Ok(0) => break,
            Ok(n) => {
                amt += n;
                buf = &mut buf[n..];
            }
            Err(err) if err.kind() == io::ErrorKind::Interrupted => {}
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {
                if amt == 0 {
                    return Ok(MaybeBlocking::WouldBlock);
                } else {
                    break;
                }
            }
            Err(err) => return Err(err.into()),
        }
    }

    Ok(MaybeBlocking::Nonblocking(amt))
}

// #[derive(Debug)]
pub enum SteelPortRepr {
    FileInput(String, Peekable<BufReader<File>>),
    FileOutput(String, BufWriter<File>),
    StdInput(Peekable<Stdin>),
    StdOutput(Stdout),
    StdError(Stderr),
    ChildStdOutput(Peekable<BufReader<ChildStdout>>),
    ChildStdError(Peekable<BufReader<ChildStderr>>),
    ChildStdInput(BufWriter<ChildStdin>),
    StringInput(Peekable<Cursor<Vec<u8>>>),
    StringOutput(Vec<u8>),

    // TODO: This does not need to be Arc<Mutex<dyn ...>> - it can
    // get away with just Box<dyn ...> - and also it should be dyn Portlike
    // with blanket trait impls to do the thing otherwise.
    DynWriter(Arc<Mutex<dyn Write + Send + Sync>>),
    DynReader(Peekable<BufReader<Box<dyn Read + Send + Sync>>>),
    TcpStream(Peekable<TcpStream>),
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

impl std::fmt::Display for SteelPortRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SteelPortRepr::FileInput(file, _) => write!(f, "#<input-port:{file}>"),
            SteelPortRepr::FileOutput(file, _) => write!(f, "#<output-port:{file}>"),
            SteelPortRepr::StdInput(_) => write!(f, "#<input-port:stdin>"),
            SteelPortRepr::StdOutput(_) => write!(f, "#<output-port:stdout>"),
            SteelPortRepr::StdError(_) => write!(f, "#<output-port:stderr>"),
            SteelPortRepr::ChildStdOutput(_) => write!(f, "#<input-port:child-stdout>"),
            SteelPortRepr::ChildStdError(_) => write!(f, "#<input-port:child-stderr>"),
            SteelPortRepr::ChildStdInput(_) => write!(f, "#<output-port:child-stdin>"),
            SteelPortRepr::StringInput(_) => write!(f, "#<input-port:string>"),
            SteelPortRepr::StringOutput(_) => write!(f, "#<output-port:string>"),
            SteelPortRepr::DynWriter(_) => write!(f, "#<output-port:opaque>"),
            SteelPortRepr::DynReader(_) => write!(f, "#<input-port:opaque>"),
            SteelPortRepr::TcpStream(_) => write!(f, "#<port:tcp>"),
            SteelPortRepr::Closed => write!(f, "#<port:closed>"),
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
            _ => stop!(Generic => "Unable to send port across threads: {}", value),
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
                port: Gc::new_lock(SteelPortRepr::StdInput(Peekable::new(s))),
            },
            SendablePort::StdOutput(s) => SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdOutput(s)),
            },
            SendablePort::StdError(s) => SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdError(s)),
            },
            SendablePort::Closed => SteelPort {
                port: Gc::new_lock(SteelPortRepr::Closed),
            },
            SendablePort::BoxDynWriter(w) => SteelPort {
                port: Gc::new_lock(SteelPortRepr::DynWriter(w)),
            },
        }
    }
}

macro_rules! port_read_str_fn {
    ($br: expr, $fn: ident) => {{
        let mut result = String::new();
        let size = $br.$fn(&mut result)?;
        Ok((size, result))
    }};
}

impl SteelPortRepr {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, SteelPortRepr::Closed)
    }

    pub fn as_stdio(self) -> std::result::Result<std::process::Stdio, Self> {
        match self {
            SteelPortRepr::FileInput(_, peekable) => {
                let guard = peekable.inner.get_ref().try_clone().unwrap();
                Ok(guard.into())
            }
            SteelPortRepr::FileOutput(_, buf_writer) => {
                let guard = buf_writer.get_ref().try_clone().unwrap();
                Ok(guard.into())
            }
            SteelPortRepr::StdOutput(_) => Ok(std::io::stdout().into()),
            SteelPortRepr::StdError(_) => Ok(std::io::stderr().into()),
            SteelPortRepr::ChildStdOutput(peekable) => Ok(peekable.inner.into_inner().into()),
            SteelPortRepr::ChildStdError(peekable) => Ok(peekable.inner.into_inner().into()),
            SteelPortRepr::ChildStdInput(buf_writer) => Ok(buf_writer.into_inner().unwrap().into()),
            SteelPortRepr::Closed => todo!(),
            _ => Err(self),
        }
    }

    pub fn read_line(&mut self) -> Result<(usize, String)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br.inner, read_line),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br.inner, read_line),
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s.inner, read_line),
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br.inner, read_line),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br.inner, read_line),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br.inner, read_line),
            // FIXME: fix this and the functions below
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
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
            _ => stop!(ContractViolation => "expected output-port?, found {}", self),
        }
    }

    pub fn read_all_str(&mut self) -> Result<(usize, String)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br.inner, read_to_string),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br.inner, read_to_string),
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s.inner, read_to_string),
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br.inner, read_to_string),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br.inner, read_to_string),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br.inner, read_to_string),
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
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
                        return Ok(MaybeBlocking::Nonblocking(Some(
                            char::REPLACEMENT_CHARACTER,
                        )));
                    }
                }
                MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
            };

            buf[i] = b;

            match std::str::from_utf8(&buf[0..=i]) {
                Ok(s) => return Ok(MaybeBlocking::Nonblocking(s.chars().next())),
                Err(err) if err.error_len().is_some() => {
                    return Ok(MaybeBlocking::Nonblocking(Some(
                        char::REPLACEMENT_CHARACTER,
                    )));
                }
                _ => {}
            }
        }

        Ok(MaybeBlocking::Nonblocking(Some(
            char::REPLACEMENT_CHARACTER,
        )))
    }

    pub fn peek_char(&mut self) -> Result<MaybeBlocking<Option<char>>> {
        match self {
            SteelPortRepr::FileInput(_, br) => br.peek_char(),
            SteelPortRepr::StdInput(br) => br.peek_char(),
            SteelPortRepr::StringInput(s) => s.peek_char(),
            SteelPortRepr::ChildStdOutput(br) => br.peek_char(),
            SteelPortRepr::ChildStdError(br) => br.peek_char(),
            SteelPortRepr::DynReader(br) => br.peek_char(),
            _ => stop!(TypeMismatch => "expected an input port"),
        }
    }

    pub fn read_bytes_amt(&mut self, buf: &mut [u8]) -> Result<MaybeBlocking<usize>> {
        match self {
            SteelPortRepr::FileInput(_, reader) => reader.read_bytes_amt(buf),
            SteelPortRepr::StdInput(stdin) => stdin.read_bytes_amt(buf),
            SteelPortRepr::ChildStdOutput(output) => output.read_bytes_amt(buf),
            SteelPortRepr::ChildStdError(output) => output.read_bytes_amt(buf),
            SteelPortRepr::StringInput(reader) => reader.read_bytes_amt(buf),
            SteelPortRepr::DynReader(reader) => reader.read_bytes_amt(buf),
            SteelPortRepr::TcpStream(t) => t.read_bytes_amt(buf),
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => {
                stop!(ContractViolation => "expected input-port?, found {}", self)
            }
            SteelPortRepr::Closed => stop!(ContractViolation => "input port is closed"),
        }
    }

    pub fn read_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        match self {
            SteelPortRepr::FileInput(_, reader) => reader.read_byte(),
            SteelPortRepr::StdInput(stdin) => stdin.read_byte(),
            SteelPortRepr::ChildStdOutput(output) => output.read_byte(),
            SteelPortRepr::ChildStdError(output) => output.read_byte(),
            SteelPortRepr::StringInput(reader) => reader.read_byte(),
            SteelPortRepr::DynReader(reader) => reader.read_byte(),
            SteelPortRepr::TcpStream(t) => t.read_byte(),
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => {
                stop!(ContractViolation => "expected input-port?, found {}", self)
            }
            SteelPortRepr::Closed => Ok(MaybeBlocking::Nonblocking(None)),
        }
    }

    pub fn peek_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        match self {
            SteelPortRepr::FileInput(_, reader) => reader.peek_byte(),
            SteelPortRepr::StdInput(stdin) => stdin.peek_byte(),
            SteelPortRepr::ChildStdOutput(output) => output.peek_byte(),
            SteelPortRepr::ChildStdError(output) => output.peek_byte(),
            SteelPortRepr::StringInput(reader) => reader.peek_byte(),
            SteelPortRepr::DynReader(reader) => reader.peek_byte(),
            SteelPortRepr::TcpStream(t) => t.peek_byte(),
            SteelPortRepr::FileOutput(_, _)
            | SteelPortRepr::StdOutput(_)
            | SteelPortRepr::StdError(_)
            | SteelPortRepr::ChildStdInput(_)
            | SteelPortRepr::StringOutput(_)
            | SteelPortRepr::DynWriter(_) => {
                stop!(ContractViolation => "expected input-port?, found {}", self)
            }
            SteelPortRepr::Closed => Ok(MaybeBlocking::Nonblocking(None)),
        }
    }

    pub fn write_char(&mut self, c: char) -> Result<()> {
        let mut buf = [0; 4];
        let s = c.encode_utf8(&mut buf);
        self.write(s.as_bytes())
    }

    pub fn write_string_line(&mut self, string: &str) -> Result<()> {
        self.write(string.as_bytes())?;
        self.write(b"\n")
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
            stop!(TypeMismatch => "expected output-port?, found {}", self)
        }
    }

    pub fn close_input_port(&mut self) -> Result<()> {
        if self.is_input() {
            *self = SteelPortRepr::Closed;
            Ok(())
        } else {
            stop!(TypeMismatch => "expected input-port?, found {}", self)
        }
    }

    pub fn write(&mut self, buf: &[u8]) -> Result<()> {
        match self {
            SteelPortRepr::FileOutput(_, writer) => writer.write_all(buf)?,
            SteelPortRepr::StdOutput(writer) => writer.write_all(buf)?,
            SteelPortRepr::StdError(writer) => writer.write_all(buf)?,
            SteelPortRepr::ChildStdInput(writer) => writer.write_all(buf)?,
            SteelPortRepr::StringOutput(writer) => writer.write_all(buf)?,
            SteelPortRepr::DynWriter(writer) => writer.lock().unwrap().write_all(buf)?,
            // TODO: Should tcp streams be both input and output ports?
            SteelPortRepr::TcpStream(tcp) => tcp.inner.write_all(buf)?,
            SteelPortRepr::FileInput(_, _)
            | SteelPortRepr::StdInput(_)
            | SteelPortRepr::DynReader(_)
            | SteelPortRepr::ChildStdOutput(_)
            | SteelPortRepr::ChildStdError(_)
            | SteelPortRepr::StringInput(_) => {
                stop!(ContractViolation => "expected output-port?, found {}", self)
            }
            SteelPortRepr::Closed => stop!(Io => "port is closed"),
        }

        Ok(())
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
            port: Gc::new_lock(SteelPortRepr::FileInput(
                path.to_string(),
                Peekable::new(BufReader::new(file)),
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
            port: Gc::new_lock(SteelPortRepr::FileOutput(
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
            port: Gc::new_lock(SteelPortRepr::FileOutput(
                path.to_string(),
                BufWriter::new(file),
            )),
        })
    }

    pub fn new_input_port_string(string: String) -> SteelPort {
        SteelPort {
            port: Gc::new_lock(SteelPortRepr::StringInput(Peekable::new(Cursor::new(
                string.into_bytes(),
            )))),
        }
    }

    pub fn new_input_port_bytevector(vec: Vec<u8>) -> SteelPort {
        SteelPort {
            port: Gc::new_lock(SteelPortRepr::StringInput(Peekable::new(Cursor::new(vec)))),
        }
    }

    pub fn new_output_port_string() -> SteelPort {
        SteelPort {
            port: Gc::new_lock(SteelPortRepr::StringOutput(Vec::new())),
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

    pub fn peek_char(&self) -> Result<MaybeBlocking<Option<char>>> {
        self.port.write().peek_char()
    }

    pub fn read_byte(&self) -> Result<MaybeBlocking<Option<u8>>> {
        self.port.write().read_byte()
    }

    pub fn read_bytes(&self, amount: usize) -> Result<MaybeBlocking<Vec<u8>>> {
        // TODO: This is going to allocate unnecessarily
        let mut buf = vec![0; amount];
        match self.port.write().read_bytes_amt(&mut buf)? {
            MaybeBlocking::Nonblocking(amount_read) => {
                buf.truncate(amount_read);
                Ok(MaybeBlocking::Nonblocking(buf))
            }
            MaybeBlocking::WouldBlock => Ok(MaybeBlocking::WouldBlock),
        }
    }

    pub fn read_bytes_into_buf(&self, buf: &mut [u8]) -> Result<MaybeBlocking<usize>> {
        self.port.write().read_bytes_amt(buf)
    }

    pub fn peek_byte(&self) -> Result<MaybeBlocking<Option<u8>>> {
        self.port.write().peek_byte()
    }

    //
    // Write functions
    //
    pub fn write_char(&self, c: char) -> Result<()> {
        self.port.write().write_char(c)
    }

    pub fn write(&self, buf: &[u8]) -> Result<()> {
        self.port.write().write(buf)
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
            port: Gc::new_lock(SteelPortRepr::StdInput(Peekable::new(io::stdin()))),
        }
    }

    pub fn default_current_output_port() -> Self {
        if cfg!(test) {
            // Write out to thread safe port
            SteelPort {
                port: Gc::new_lock(SteelPortRepr::DynWriter(Arc::new(Mutex::new(
                    BufWriter::new(Vec::new()),
                )))),
            }
        } else {
            SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdOutput(io::stdout())),
            }
        }
    }

    pub fn default_current_error_port() -> Self {
        if cfg!(test) {
            // Write out to thread safe port
            SteelPort {
                port: Gc::new_lock(SteelPortRepr::DynWriter(Arc::new(Mutex::new(
                    BufWriter::new(Vec::new()),
                )))),
            }
        } else {
            SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdError(io::stderr())),
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
