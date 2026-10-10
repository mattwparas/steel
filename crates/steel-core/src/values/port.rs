use alloc::sync::Arc;
use core::hash::Hash;
use serde::Deserialize;
use serde::Serialize;
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
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        Gc::as_ptr(&self.port).hash(state);
    }
}

impl core::fmt::Display for SteelPort {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

const MIN_CHUNK: usize = 8 * 1024;
const MAX_STREAM_CHUNK: usize = 64 * 1024;

#[derive(Debug)]
pub struct Peekable<R> {
    inner: R,
    buf: Vec<u8>,
    pos: usize,
    chunk: usize,
    consumed_any: bool,
}

impl<T> Peekable<T> {
    pub fn new(inner: T) -> Self {
        Peekable {
            inner,
            buf: Vec::new(),
            pos: 0,
            chunk: MIN_CHUNK,
            consumed_any: false,
        }
    }

    pub fn at_source_start(&self) -> bool {
        !self.consumed_any
    }

    pub fn pending(&self) -> &[u8] {
        &self.buf[self.pos..]
    }

    pub fn pending_is_empty(&self) -> bool {
        self.pos >= self.buf.len()
    }

    pub fn consume(&mut self, amt: usize) {
        self.consumed_any |= amt > 0;
        self.chunk = MIN_CHUNK;
        self.pos = usize::min(self.pos + amt, self.buf.len());

        if self.pos >= self.buf.len() {
            self.buf.clear();
            self.pos = 0;
        } else if self.pos >= self.buf.len() / 2 {
            self.buf.drain(..self.pos);
            self.pos = 0;
        }
    }

    fn take_pending(&mut self, out: &mut Vec<u8>) {
        self.consumed_any |= self.pos < self.buf.len();
        out.extend_from_slice(&self.buf[self.pos..]);
        self.buf.clear();
        self.pos = 0;
    }
}

impl<R: Read> Peekable<R> {
    fn fill(&mut self, amt: usize) -> Result<MaybeBlocking<usize>> {
        let start = self.buf.len();
        self.buf.resize(start + amt, 0);

        let filled = match read_full(&mut self.inner, &mut self.buf[start..]) {
            Ok(MaybeBlocking::Nonblocking(n)) => n,
            Ok(MaybeBlocking::WouldBlock) => {
                self.buf.truncate(start);
                return Ok(MaybeBlocking::WouldBlock);
            }
            Err(err) => {
                self.buf.truncate(start);
                return Err(err);
            }
        };

        self.buf.truncate(start + filled);
        Ok(MaybeBlocking::Nonblocking(filled))
    }

    pub fn fill_chunk(&mut self, max: usize) -> Result<MaybeBlocking<usize>> {
        let unfinished = !self.pending_is_empty();
        let start = self.buf.len();
        self.buf.resize(start + usize::min(self.chunk, max), 0);

        let result = loop {
            match self.inner.read(&mut self.buf[start..]) {
                Err(err) if err.kind() == io::ErrorKind::Interrupted => {}
                other => break other,
            }
        };

        match result {
            Ok(amt) => {
                self.buf.truncate(start + amt);

                if unfinished && amt > 0 {
                    self.chunk = usize::min(self.chunk.saturating_mul(2), max);
                }

                Ok(MaybeBlocking::Nonblocking(amt))
            }
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {
                self.buf.truncate(start);
                Ok(MaybeBlocking::WouldBlock)
            }
            Err(err) => {
                self.buf.truncate(start);
                Err(err.into())
            }
        }
    }

    fn read_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        if !self.pending_is_empty() {
            let byte = self.buf[self.pos];
            self.consume(1);
            return Ok(MaybeBlocking::Nonblocking(Some(byte)));
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
        loop {
            let upto = usize::min(self.pending().len(), 4);

            if upto > 0 {
                let pending = self.pending();

                match core::str::from_utf8(&pending[..upto]) {
                    Ok(str) => return Ok(MaybeBlocking::Nonblocking(str.chars().next())),
                    Err(err) if err.valid_up_to() > 0 => {
                        let valid = core::str::from_utf8(&pending[..err.valid_up_to()]).unwrap();

                        return Ok(MaybeBlocking::Nonblocking(valid.chars().next()));
                    }
                    Err(err) if err.error_len().is_some() || upto == 4 => {
                        self.consume(err.error_len().unwrap_or(1));

                        return Ok(MaybeBlocking::Nonblocking(Some(
                            char::REPLACEMENT_CHARACTER,
                        )));
                    }
                    Err(_) => {}
                }
            }

            match self.fill(4 - upto)? {
                MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
                MaybeBlocking::Nonblocking(0) => {
                    if upto == 0 {
                        return Ok(MaybeBlocking::Nonblocking(None));
                    }

                    self.consume(upto);

                    return Ok(MaybeBlocking::Nonblocking(Some(
                        char::REPLACEMENT_CHARACTER,
                    )));
                }
                MaybeBlocking::Nonblocking(_) => {}
            }
        }
    }

    fn peek_byte(&mut self) -> Result<MaybeBlocking<Option<u8>>> {
        if !self.pending_is_empty() {
            return Ok(MaybeBlocking::Nonblocking(Some(self.buf[self.pos])));
        }

        match self.fill(1)? {
            MaybeBlocking::WouldBlock => Ok(MaybeBlocking::WouldBlock),
            MaybeBlocking::Nonblocking(0) => Ok(MaybeBlocking::Nonblocking(None)),
            MaybeBlocking::Nonblocking(_) => {
                Ok(MaybeBlocking::Nonblocking(Some(self.buf[self.pos])))
            }
        }
    }

    pub fn read_bytes_amt(&mut self, mut buf: &mut [u8]) -> Result<MaybeBlocking<usize>> {
        let mut amt = 0;

        if !buf.is_empty() && !self.pending_is_empty() {
            let len = usize::min(buf.len(), self.pending().len());
            buf[..len].copy_from_slice(&self.buf[self.pos..self.pos + len]);

            amt += len;
            buf = &mut buf[len..];
            self.consume(len);
        }

        match read_full(&mut self.inner, buf)? {
            MaybeBlocking::Nonblocking(n) => amt += n,
            MaybeBlocking::WouldBlock => {
                if amt == 0 {
                    return Ok(MaybeBlocking::WouldBlock);
                }
            }
        };

        Ok(MaybeBlocking::Nonblocking(amt))
    }

    pub fn read_to_end_peeked(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        let start = buf.len();
        self.take_pending(buf);
        self.inner.read_to_end(buf)?;
        Ok(buf.len() - start)
    }

    pub fn read_to_string_peeked(&mut self, buf: &mut String) -> io::Result<usize> {
        if self.pending_is_empty() {
            return self.inner.read_to_string(buf);
        }

        let mut bytes = Vec::new();
        self.take_pending(&mut bytes);
        self.inner.read_to_end(&mut bytes)?;

        let amt = bytes.len();
        buf.push_str(&into_utf8(bytes)?);
        Ok(amt)
    }

    fn line_from_pending(&mut self, bytes: &mut Vec<u8>) -> bool {
        match self.pending().iter().position(|b| *b == b'\n') {
            Some(pos) => {
                let taken = pos + 1;
                bytes.extend_from_slice(&self.buf[self.pos..self.pos + taken]);
                self.consume(taken);
                true
            }
            None => {
                self.take_pending(bytes);
                false
            }
        }
    }
}

pub enum StdioError {
    Unsupported(SteelPortRepr),
    Buffered(SteelPortRepr),
}

impl StdioError {
    pub fn into_port(self) -> SteelPortRepr {
        match self {
            StdioError::Unsupported(port) | StdioError::Buffered(port) => port,
        }
    }

    pub fn is_buffered(&self) -> bool {
        matches!(self, StdioError::Buffered(_))
    }
}

impl<R> Peekable<BufReader<R>> {
    fn buffered(&self) -> usize {
        self.pending().len() + self.inner.buffer().len()
    }
}

impl<R: BufRead> Peekable<R> {
    pub fn read_line_peeked(&mut self, buf: &mut String) -> io::Result<usize> {
        if self.pending_is_empty() {
            return self.inner.read_line(buf);
        }

        let mut bytes = Vec::new();
        if !self.line_from_pending(&mut bytes) {
            self.inner.read_until(b'\n', &mut bytes)?;
        }

        let amt = bytes.len();
        buf.push_str(&into_utf8(bytes)?);
        Ok(amt)
    }
}

impl Peekable<Stdin> {
    pub fn read_line_peeked_stdin(&mut self, buf: &mut String) -> io::Result<usize> {
        if self.pending_is_empty() {
            return self.inner.read_line(buf);
        }

        let mut bytes = Vec::new();
        if !self.line_from_pending(&mut bytes) {
            self.inner.lock().read_until(b'\n', &mut bytes)?;
        }

        let amt = bytes.len();
        buf.push_str(&into_utf8(bytes)?);
        Ok(amt)
    }
}

fn into_utf8(bytes: Vec<u8>) -> io::Result<String> {
    String::from_utf8(bytes).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "stream did not contain valid UTF-8",
        )
    })
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

impl core::fmt::Debug for SteelPortRepr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

impl core::fmt::Display for SteelPortRepr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SendablePort {
    StdInput,
    StdOutput,
    StdError,
    // BoxDynWriter(Arc<Mutex<dyn Write + Send + Sync>>),
    Closed,
}

impl SendablePort {
    fn from_port_repr(value: &SteelPortRepr) -> Result<SendablePort> {
        match value {
            SteelPortRepr::StdInput(_) => Ok(SendablePort::StdInput),
            SteelPortRepr::StdOutput(_) => Ok(SendablePort::StdOutput),
            SteelPortRepr::StdError(_) => Ok(SendablePort::StdError),
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
            SendablePort::StdInput => SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdInput(Peekable::new(io::stdin()))),
            },
            SendablePort::StdOutput => SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdOutput(io::stdout())),
            },
            SendablePort::StdError => SteelPort {
                port: Gc::new_lock(SteelPortRepr::StdError(io::stderr())),
            },
            SendablePort::Closed => SteelPort {
                port: Gc::new_lock(SteelPortRepr::Closed),
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

macro_rules! port_read_vec_fn {
    ($br: expr, $fn: ident) => {{
        let mut result = Vec::new();
        let size = $br.$fn(&mut result)?;
        Ok((size, result))
    }};
}

impl SteelPortRepr {
    pub fn take(&mut self) -> Self {
        core::mem::replace(self, SteelPortRepr::Closed)
    }

    pub fn as_stdio(self) -> core::result::Result<std::process::Stdio, StdioError> {
        match self {
            SteelPortRepr::FileInput(name, peekable) => {
                let buffered = peekable.buffered();

                let Ok(mut file) = peekable.inner.get_ref().try_clone() else {
                    return Err(StdioError::Unsupported(SteelPortRepr::FileInput(
                        name, peekable,
                    )));
                };

                if buffered == 0 {
                    return Ok(file.into());
                }

                match file.seek(io::SeekFrom::Current(-(buffered as i64))) {
                    Ok(_) => Ok(file.into()),
                    Err(_) => Err(StdioError::Buffered(SteelPortRepr::FileInput(
                        name, peekable,
                    ))),
                }
            }
            SteelPortRepr::FileOutput(_, buf_writer) => {
                let guard = buf_writer.get_ref().try_clone().unwrap();
                Ok(guard.into())
            }
            SteelPortRepr::StdOutput(_) => Ok(std::io::stdout().into()),
            SteelPortRepr::StdError(_) => Ok(std::io::stderr().into()),
            SteelPortRepr::ChildStdOutput(peekable) => {
                if peekable.buffered() > 0 {
                    return Err(StdioError::Buffered(SteelPortRepr::ChildStdOutput(
                        peekable,
                    )));
                }

                Ok(peekable.inner.into_inner().into())
            }
            SteelPortRepr::ChildStdError(peekable) => {
                if peekable.buffered() > 0 {
                    return Err(StdioError::Buffered(SteelPortRepr::ChildStdError(peekable)));
                }

                Ok(peekable.inner.into_inner().into())
            }
            SteelPortRepr::ChildStdInput(buf_writer) => Ok(buf_writer.into_inner().unwrap().into()),
            SteelPortRepr::Closed => todo!(),
            _ => Err(StdioError::Unsupported(self)),
        }
    }

    pub fn read_line(&mut self) -> Result<(usize, String)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br, read_line_peeked_stdin),
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s, read_line_peeked),
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br, read_line_peeked),
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

    pub fn pending(&self) -> &[u8] {
        match self {
            SteelPortRepr::FileInput(_, br) => br.pending(),
            SteelPortRepr::StdInput(br) => br.pending(),
            SteelPortRepr::StringInput(s) => s.pending(),
            SteelPortRepr::ChildStdOutput(br) => br.pending(),
            SteelPortRepr::ChildStdError(br) => br.pending(),
            SteelPortRepr::DynReader(br) => br.pending(),
            SteelPortRepr::TcpStream(t) => t.pending(),
            _ => &[],
        }
    }

    pub fn at_source_start(&self) -> bool {
        match self {
            SteelPortRepr::FileInput(_, br) => br.at_source_start(),
            SteelPortRepr::StdInput(br) => br.at_source_start(),
            SteelPortRepr::StringInput(s) => s.at_source_start(),
            SteelPortRepr::ChildStdOutput(br) => br.at_source_start(),
            SteelPortRepr::ChildStdError(br) => br.at_source_start(),
            SteelPortRepr::DynReader(br) => br.at_source_start(),
            SteelPortRepr::TcpStream(t) => t.at_source_start(),
            _ => false,
        }
    }

    pub fn consume_pending(&mut self, amt: usize) {
        match self {
            SteelPortRepr::FileInput(_, br) => br.consume(amt),
            SteelPortRepr::StdInput(br) => br.consume(amt),
            SteelPortRepr::StringInput(s) => s.consume(amt),
            SteelPortRepr::ChildStdOutput(br) => br.consume(amt),
            SteelPortRepr::ChildStdError(br) => br.consume(amt),
            SteelPortRepr::DynReader(br) => br.consume(amt),
            SteelPortRepr::TcpStream(t) => t.consume(amt),
            _ => {}
        }
    }

    pub fn fill_more(&mut self) -> Result<MaybeBlocking<usize>> {
        match self {
            SteelPortRepr::FileInput(_, br) => br.fill_chunk(usize::MAX),
            SteelPortRepr::StringInput(s) => {
                let left = (s.inner.get_ref().len()).saturating_sub(s.inner.position() as usize);
                s.fill_chunk(left)
            }
            SteelPortRepr::StdInput(br) => br.fill_chunk(MAX_STREAM_CHUNK),
            SteelPortRepr::ChildStdOutput(br) => br.fill_chunk(MAX_STREAM_CHUNK),
            SteelPortRepr::ChildStdError(br) => br.fill_chunk(MAX_STREAM_CHUNK),
            SteelPortRepr::DynReader(br) => br.fill_chunk(MAX_STREAM_CHUNK),
            SteelPortRepr::TcpStream(t) => t.fill_chunk(MAX_STREAM_CHUNK),
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
        }
    }

    pub fn read_all_str(&mut self) -> Result<(usize, String)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s, read_to_string_peeked),
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br, read_to_string_peeked),
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
        }
    }

    pub fn read_all_bytes(&mut self) -> Result<(usize, Vec<u8>)> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::StdInput(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::StringInput(s) => port_read_vec_fn!(s, read_to_end_peeked),
            SteelPortRepr::ChildStdOutput(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::ChildStdError(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::DynReader(br) => port_read_vec_fn!(br, read_to_end_peeked),
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

            match core::str::from_utf8(&buf[0..=i]) {
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

    pub fn read_all_bytes(&self) -> Result<(usize, Vec<u8>)> {
        self.port.write().read_all_bytes()
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

    pub fn default_input_port() -> Self {
        SteelPort {
            port: Gc::new_lock(SteelPortRepr::StdInput(Peekable::new(io::stdin()))),
        }
    }

    pub fn default_output_port() -> Self {
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

    pub fn default_error_port() -> Self {
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

#[cfg(test)]
mod port_tests {
    use super::*;
    use std::collections::VecDeque;

    struct Chunks {
        chunks: VecDeque<Vec<u8>>,
    }

    impl Chunks {
        fn new(chunks: &[&[u8]]) -> Self {
            Chunks {
                chunks: chunks.iter().map(|chunk| chunk.to_vec()).collect(),
            }
        }
    }

    impl Read for Chunks {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            let Some(chunk) = self.chunks.pop_front() else {
                return Ok(0);
            };

            let amt = usize::min(chunk.len(), buf.len());
            buf[..amt].copy_from_slice(&chunk[..amt]);

            if amt < chunk.len() {
                self.chunks.push_front(chunk[amt..].to_vec());
            }

            Ok(amt)
        }
    }

    struct ThenBlocks {
        first: Option<&'static [u8]>,
    }

    impl Read for ThenBlocks {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            match self.first.take() {
                Some(bytes) => {
                    buf[..bytes.len()].copy_from_slice(bytes);
                    Ok(bytes.len())
                }
                None => Err(io::Error::new(io::ErrorKind::WouldBlock, "nothing ready")),
            }
        }
    }

    struct Interrupts {
        interrupted: bool,
    }

    impl Read for Interrupts {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            if !self.interrupted {
                self.interrupted = true;
                return Err(io::Error::new(io::ErrorKind::Interrupted, "signal"));
            }

            buf[..2].copy_from_slice(b"ok");
            Ok(2)
        }
    }

    fn nonblocking<T>(value: MaybeBlocking<T>) -> T {
        match value {
            MaybeBlocking::Nonblocking(value) => value,
            MaybeBlocking::WouldBlock => panic!("expected the read to go through"),
        }
    }

    fn filled(bytes: &[u8]) -> Peekable<Cursor<Vec<u8>>> {
        let mut peekable = Peekable::new(Cursor::new(bytes.to_vec()));
        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        peekable
    }

    #[test]
    fn nothing_is_pending_before_a_fill() {
        let peekable = Peekable::new(Cursor::new(b"abc".to_vec()));

        assert!(peekable.pending_is_empty());
        assert_eq!(peekable.pending(), b"");
    }

    #[test]
    fn consume_advances_through_the_buffer() {
        let mut peekable = filled(b"0123456789");
        assert_eq!(peekable.pending(), b"0123456789");

        peekable.consume(3);
        assert_eq!(peekable.pending(), b"3456789");
    }

    #[test]
    fn consume_past_the_end_clamps_and_clears() {
        let mut peekable = filled(b"abc");

        peekable.consume(99);

        assert!(peekable.pending_is_empty());
        assert_eq!(peekable.buf.len(), 0, "a drained buffer is released");
        assert_eq!(peekable.pos, 0);
    }

    #[test]
    fn consume_compacts_once_the_buffer_is_mostly_drained() {
        let mut peekable = filled(b"0123456789");

        peekable.consume(3);
        assert_eq!(peekable.pos, 3, "under half, so the front is kept");

        peekable.consume(3);
        assert_eq!(peekable.pos, 0, "over half, so the front is dropped");
        assert_eq!(peekable.pending(), b"6789");
    }

    #[test]
    fn at_source_start_holds_until_something_is_handed_out() {
        let mut peekable = Peekable::new(Cursor::new(b"abc".to_vec()));
        assert!(peekable.at_source_start());

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert!(peekable.at_source_start(), "a fill hands nothing out");

        peekable.consume(0);
        assert!(
            peekable.at_source_start(),
            "consuming nothing hands nothing out"
        );

        peekable.consume(1);
        assert!(!peekable.at_source_start());
    }

    #[test]
    fn take_pending_drains_the_buffer() {
        let mut peekable = filled(b"abc");
        let mut out = Vec::new();

        peekable.take_pending(&mut out);

        assert_eq!(out, b"abc");
        assert!(peekable.pending_is_empty());
        assert!(!peekable.at_source_start(), "the bytes were handed out");
    }

    #[test]
    fn the_request_grows_while_a_datum_is_unfinished() {
        let mut peekable = Peekable::new(Chunks::new(&[b"ab", b"cd", b"ef"]));
        assert_eq!(peekable.chunk, MIN_CHUNK);

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(
            peekable.chunk, MIN_CHUNK,
            "nothing was pending, so no growth"
        );

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.chunk, MIN_CHUNK * 2);

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.chunk, MIN_CHUNK * 4);
        assert_eq!(peekable.pending(), b"abcdef");
    }

    #[test]
    fn the_request_resets_once_a_datum_is_consumed() {
        let mut peekable = Peekable::new(Chunks::new(&[b"ab", b"cd"]));

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.chunk, MIN_CHUNK * 2);

        peekable.consume(1);
        assert_eq!(peekable.chunk, MIN_CHUNK);
    }

    #[test]
    fn the_request_never_exceeds_the_cap() {
        let mut peekable = Peekable::new(Chunks::new(&[b"ab", b"cd", b"ef"]));

        nonblocking(peekable.fill_chunk(4).unwrap());
        nonblocking(peekable.fill_chunk(4).unwrap());

        assert_eq!(peekable.chunk, 4);
    }

    #[test]
    fn an_interrupted_read_is_retried() {
        let mut peekable = Peekable::new(Interrupts { interrupted: false });

        let amt = nonblocking(peekable.fill_chunk(usize::MAX).unwrap());

        assert_eq!(amt, 2);
        assert_eq!(peekable.pending(), b"ok");
    }

    #[test]
    fn a_source_with_nothing_ready_leaves_the_buffer_alone() {
        let mut peekable = Peekable::new(ThenBlocks {
            first: Some(b"abc"),
        });

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.pending(), b"abc");

        assert!(matches!(
            peekable.fill_chunk(usize::MAX).unwrap(),
            MaybeBlocking::WouldBlock
        ));
        assert_eq!(peekable.pending(), b"abc", "the pending bytes survive");
    }

    #[test]
    fn peek_char_does_not_consume() {
        let mut peekable = Peekable::new(Cursor::new("hé".as_bytes().to_vec()));

        assert_eq!(nonblocking(peekable.peek_char().unwrap()), Some('h'));
        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some('h'),
            "peeking twice gives the same character"
        );

        assert_eq!(nonblocking(peekable.read_byte().unwrap()), Some(b'h'));
        assert_eq!(nonblocking(peekable.peek_char().unwrap()), Some('é'));
    }

    #[test]
    fn peek_char_waits_for_a_character_split_across_reads() {
        let mut peekable = Peekable::new(Chunks::new(&[&[0xC3], &[0xA9]]));

        assert_eq!(nonblocking(peekable.peek_char().unwrap()), Some('é'));
    }

    #[test]
    fn peek_char_reports_bytes_that_are_not_utf8_and_moves_past_them() {
        let mut peekable = Peekable::new(Cursor::new(vec![0xFF, b'a']));

        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some('a'),
            "the bad byte is not peeked forever"
        );
    }

    #[test]
    fn peek_char_reports_a_character_the_source_cut_short() {
        let mut peekable = Peekable::new(Cursor::new(vec![0xC3]));

        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            None,
            "and the stub is not peeked forever either"
        );
    }

    #[test]
    fn peek_char_at_the_end_is_nothing() {
        let mut peekable = Peekable::new(Cursor::new(Vec::new()));

        assert_eq!(nonblocking(peekable.peek_char().unwrap()), None);
    }

    #[test]
    fn read_byte_drains_the_buffer_before_the_source() {
        let mut peekable = Peekable::new(Chunks::new(&[b"ab", b"cd"]));
        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());

        assert_eq!(nonblocking(peekable.read_byte().unwrap()), Some(b'a'));
        assert_eq!(nonblocking(peekable.read_byte().unwrap()), Some(b'b'));
        assert_eq!(nonblocking(peekable.read_byte().unwrap()), Some(b'c'));
    }

    #[test]
    fn read_bytes_amt_spans_the_buffer_and_the_source() {
        let mut peekable = Peekable::new(Chunks::new(&[b"abc", b"def"]));
        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.pending(), b"abc");

        let mut buf = [0; 5];
        let amt = nonblocking(peekable.read_bytes_amt(&mut buf).unwrap());

        assert_eq!(amt, 5);
        assert_eq!(&buf, b"abcde");
    }

    #[test]
    fn read_to_end_includes_what_was_already_buffered() {
        let mut peekable = Peekable::new(Chunks::new(&[b"abc", b"def"]));
        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());

        let mut out = Vec::new();
        let amt = peekable.read_to_end_peeked(&mut out).unwrap();

        assert_eq!(amt, 6);
        assert_eq!(out, b"abcdef");
    }

    #[test]
    fn read_to_string_joins_a_character_split_across_the_boundary() {
        let mut peekable = Peekable::new(Cursor::new("é".as_bytes().to_vec()));
        nonblocking(peekable.fill(1).unwrap());
        assert_eq!(peekable.pending(), &[0xC3]);

        let mut out = String::new();
        let amt = peekable.read_to_string_peeked(&mut out).unwrap();

        assert_eq!(amt, 2);
        assert_eq!(out, "é");
    }

    #[test]
    fn read_line_takes_a_line_that_is_already_buffered() {
        let mut peekable = filled(b"ab\ncd");

        let mut out = String::new();
        let amt = peekable.read_line_peeked(&mut out).unwrap();

        assert_eq!(amt, 3);
        assert_eq!(out, "ab\n");
        assert_eq!(peekable.pending(), b"cd", "the rest stays put");
    }

    #[test]
    fn read_line_spans_the_buffer_and_the_source() {
        let mut peekable = Peekable::new(Cursor::new(b"ab\ncd".to_vec()));
        nonblocking(peekable.fill(1).unwrap());
        assert_eq!(peekable.pending(), b"a");

        let mut out = String::new();
        let amt = peekable.read_line_peeked(&mut out).unwrap();

        assert_eq!(amt, 3);
        assert_eq!(out, "ab\n");
    }

    #[test]
    fn read_line_returns_a_last_line_without_a_newline() {
        let mut peekable = filled(b"cd");

        let mut out = String::new();
        let amt = peekable.read_line_peeked(&mut out).unwrap();

        assert_eq!(amt, 2);
        assert_eq!(out, "cd");
    }

    #[test]
    fn read_line_at_the_end_is_zero() {
        let mut peekable = Peekable::new(Cursor::new(Vec::new()));

        let mut out = String::new();

        assert_eq!(peekable.read_line_peeked(&mut out).unwrap(), 0);
        assert_eq!(out, "");
    }

    #[test]
    fn a_string_port_never_asks_for_more_than_is_left() {
        let mut port = SteelPortRepr::StringInput(Peekable::new(Cursor::new(b"abc".to_vec())));

        assert!(port.at_source_start());
        assert_eq!(nonblocking(port.fill_more().unwrap()), 3);
        assert_eq!(port.pending(), b"abc");

        port.consume_pending(1);
        assert_eq!(port.pending(), b"bc");
        assert!(!port.at_source_start());

        assert_eq!(
            nonblocking(port.fill_more().unwrap()),
            0,
            "the source is spent"
        );
    }

    #[test]
    fn as_stdio_rewinds_what_was_buffered_but_not_consumed() {
        let path = std::env::temp_dir().join(format!("steel-as-stdio-{}.txt", std::process::id()));
        std::fs::write(&path, b"(a) remainder").unwrap();

        let file = File::open(&path).unwrap();
        let mut observer = file.try_clone().unwrap();
        let mut peekable = Peekable::new(BufReader::new(file));

        peekable.fill_chunk(usize::MAX).unwrap();
        assert_eq!(peekable.buffered(), 13);

        peekable.consume(3);
        assert_eq!(peekable.buffered(), 10);

        let port = SteelPortRepr::FileInput(path.display().to_string(), peekable);
        assert!(port.as_stdio().is_ok());

        let mut rest = String::new();
        observer.read_to_string(&mut rest).unwrap();
        assert_eq!(rest, " remainder");

        std::fs::remove_file(&path).unwrap();
    }

    #[test]
    fn as_stdio_rewinds_bytes_the_bufreader_is_holding_too() {
        let path =
            std::env::temp_dir().join(format!("steel-as-stdio-buf-{}.txt", std::process::id()));
        std::fs::write(&path, b"(a) remainder").unwrap();

        let file = File::open(&path).unwrap();
        let mut observer = file.try_clone().unwrap();
        let mut peekable = Peekable::new(BufReader::new(file));

        nonblocking(peekable.fill_chunk(4).unwrap());
        assert_eq!(peekable.pending(), b"(a) ");
        assert!(
            !peekable.inner.buffer().is_empty(),
            "a small request leaves the rest inside the BufReader"
        );
        assert_eq!(peekable.buffered(), 13, "both buffers count");

        peekable.consume(4);
        assert_eq!(peekable.buffered(), 9);

        let port = SteelPortRepr::FileInput(path.display().to_string(), peekable);
        assert!(port.as_stdio().is_ok());

        let mut rest = String::new();
        observer.read_to_string(&mut rest).unwrap();
        assert_eq!(rest, "remainder");

        std::fs::remove_file(&path).unwrap();
    }

    #[test]
    fn as_stdio_hands_over_an_untouched_file_port_whole() {
        let path =
            std::env::temp_dir().join(format!("steel-as-stdio-cold-{}.txt", std::process::id()));
        std::fs::write(&path, b"whole file").unwrap();

        let file = File::open(&path).unwrap();
        let mut observer = file.try_clone().unwrap();
        let port = SteelPortRepr::FileInput(
            path.display().to_string(),
            Peekable::new(BufReader::new(file)),
        );

        assert!(port.as_stdio().is_ok());

        let mut rest = String::new();
        observer.read_to_string(&mut rest).unwrap();
        assert_eq!(rest, "whole file", "nothing was buffered, so nothing moved");

        std::fs::remove_file(&path).unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn as_stdio_refuses_a_file_port_that_cannot_seek() {
        use std::os::unix::io::{FromRawFd, IntoRawFd};

        let (mut writer, reader) = std::os::unix::net::UnixStream::pair().unwrap();
        writer.write_all(b"(a) remainder").unwrap();
        drop(writer);

        // A socket is not seekable, so there is nowhere to put bytes already read
        let file = unsafe { File::from_raw_fd(reader.into_raw_fd()) };
        let mut peekable = Peekable::new(BufReader::new(file));

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert!(peekable.buffered() > 0);
        peekable.consume(4);

        let port = SteelPortRepr::FileInput("socket".to_string(), peekable);
        let Err(err) = port.as_stdio() else {
            panic!("a port that cannot seek must not hand over buffered bytes");
        };

        assert!(err.is_buffered());
    }

    #[test]
    fn as_stdio_refuses_a_port_that_is_not_a_child_handle() {
        let port = SteelPortRepr::StringInput(Peekable::new(Cursor::new(b"abc".to_vec())));

        let Err(err) = port.as_stdio() else {
            panic!("a string port has no handle to give a child");
        };

        assert!(!err.is_buffered());
        assert!(matches!(err.into_port(), SteelPortRepr::StringInput(_)));
    }
}
