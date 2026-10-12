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

        let mut filled = 0;
        let result = read_full(&mut self.inner, &mut self.buf[start..], &mut filled);

        self.buf.truncate(start + filled);

        match result? {
            MaybeBlocking::Nonblocking(()) => Ok(MaybeBlocking::Nonblocking(filled)),
            MaybeBlocking::WouldBlock => Ok(MaybeBlocking::WouldBlock),
        }
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
        Ok(match self.peek_char_pending()? {
            MaybeBlocking::Nonblocking(peeked) => {
                MaybeBlocking::Nonblocking(peeked.map(|(c, _)| c))
            }
            MaybeBlocking::WouldBlock => MaybeBlocking::WouldBlock,
        })
    }

    fn read_char(&mut self) -> Result<MaybeBlocking<Option<char>>> {
        Ok(match self.peek_char_pending()? {
            MaybeBlocking::Nonblocking(Some((c, pending))) => {
                self.consume(pending);

                MaybeBlocking::Nonblocking(Some(c))
            }
            MaybeBlocking::Nonblocking(None) => MaybeBlocking::Nonblocking(None),
            MaybeBlocking::WouldBlock => MaybeBlocking::WouldBlock,
        })
    }

    fn peek_char_pending(&mut self) -> Result<MaybeBlocking<Option<(char, usize)>>> {
        loop {
            let upto = usize::min(self.pending().len(), 4);

            if upto > 0 {
                let pending = self.pending();

                match core::str::from_utf8(&pending[..upto]) {
                    Ok(str) => return Ok(first_char(str)),
                    Err(err) if err.valid_up_to() > 0 => {
                        let valid = core::str::from_utf8(&pending[..err.valid_up_to()]).unwrap();

                        return Ok(first_char(valid));
                    }
                    Err(err) if err.error_len().is_some() || upto == 4 => {
                        return Ok(replacement_character(err.error_len().unwrap_or(1)));
                    }
                    Err(_) => {}
                }
            }

            match self.fill(1)? {
                MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
                MaybeBlocking::Nonblocking(0) => {
                    if upto == 0 {
                        return Ok(MaybeBlocking::Nonblocking(None));
                    }

                    return Ok(replacement_character(upto));
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

    pub fn read_bytes_amt(&mut self, buf: &mut [u8]) -> Result<MaybeBlocking<usize>> {
        let at_source_start = self.at_source_start();
        let mut amt = 0;

        if !buf.is_empty() && !self.pending_is_empty() {
            let len = usize::min(buf.len(), self.pending().len());
            buf[..len].copy_from_slice(&self.buf[self.pos..self.pos + len]);

            amt = len;
            self.consume(len);
        }

        let peeked = amt;

        match read_full(&mut self.inner, &mut buf[peeked..], &mut amt) {
            Ok(MaybeBlocking::Nonblocking(())) => Ok(MaybeBlocking::Nonblocking(amt)),
            Ok(MaybeBlocking::WouldBlock) => Ok(MaybeBlocking::WouldBlock),
            Err(err) => {
                self.unconsume(&buf[..amt], at_source_start);

                Err(err)
            }
        }
    }

    pub fn read_to_end_peeked(&mut self, buf: &mut Vec<u8>) -> Result<MaybeBlocking<usize>> {
        let at_source_start = self.at_source_start();
        let start = buf.len();

        self.take_pending(buf);

        if let Err(err) = self.inner.read_to_end(buf) {
            self.put_back(buf.split_off(start), at_source_start);

            if blocked(&err) {
                return Ok(MaybeBlocking::WouldBlock);
            }

            return Err(err.into());
        }

        Ok(MaybeBlocking::Nonblocking(buf.len() - start))
    }

    pub fn read_to_string_peeked(&mut self, buf: &mut String) -> Result<MaybeBlocking<usize>> {
        let at_source_start = self.at_source_start();
        let mut bytes = Vec::new();

        self.take_pending(&mut bytes);

        if let Err(err) = self.inner.read_to_end(&mut bytes) {
            self.put_back(bytes, at_source_start);

            if blocked(&err) {
                return Ok(MaybeBlocking::WouldBlock);
            }

            return Err(err.into());
        }

        let amt = bytes.len();

        match String::from_utf8(bytes) {
            Ok(str) => {
                buf.push_str(&str);

                Ok(MaybeBlocking::Nonblocking(amt))
            }
            Err(err) => {
                self.put_back(err.into_bytes(), at_source_start);

                Err(invalid_utf8().into())
            }
        }
    }

    fn unconsume(&mut self, bytes: &[u8], at_source_start: bool) {
        debug_assert!(self.pending_is_empty());

        self.buf.splice(..self.pos, bytes.iter().copied());
        self.pos = 0;
        self.consumed_any = !at_source_start;
    }

    fn put_back(&mut self, bytes: Vec<u8>, at_source_start: bool) {
        debug_assert!(self.buf.is_empty());

        self.buf = bytes;
        self.pos = 0;
        self.consumed_any = !at_source_start;
    }

    fn read_line_peeked_unbuffered(&mut self, buf: &mut String) -> Result<MaybeBlocking<usize>> {
        let mut scanned = 0;

        loop {
            if let Some(pos) = self.pending()[scanned..].iter().position(|b| *b == b'\n') {
                return self.take_line(scanned + pos + 1, buf);
            }

            scanned = self.pending().len();

            match self.fill_chunk(MAX_STREAM_CHUNK)? {
                MaybeBlocking::Nonblocking(0) => return self.take_line(self.pending().len(), buf),
                MaybeBlocking::Nonblocking(_) => {}
                MaybeBlocking::WouldBlock => return Ok(MaybeBlocking::WouldBlock),
            }
        }
    }

    fn take_line(&mut self, amt: usize, buf: &mut String) -> Result<MaybeBlocking<usize>> {
        let line = self.pending()[..amt].to_vec();
        self.consume(amt);
        buf.push_str(&into_utf8(line)?);

        Ok(MaybeBlocking::Nonblocking(amt))
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
    pub fn read_line_peeked(&mut self, buf: &mut String) -> Result<MaybeBlocking<usize>> {
        if let Some(pos) = self.pending().iter().position(|b| *b == b'\n') {
            return self.take_line(pos + 1, buf);
        }

        if let Err(err) = self.inner.read_until(b'\n', &mut self.buf) {
            if blocked(&err) {
                return Ok(MaybeBlocking::WouldBlock);
            }

            return Err(err.into());
        }

        self.take_line(self.pending().len(), buf)
    }
}

impl Peekable<Stdin> {
    pub fn read_line_peeked_stdin(&mut self, buf: &mut String) -> Result<MaybeBlocking<usize>> {
        if let Some(pos) = self.pending().iter().position(|b| *b == b'\n') {
            return self.take_line(pos + 1, buf);
        }

        if let Err(err) = self.inner.lock().read_until(b'\n', &mut self.buf) {
            if blocked(&err) {
                return Ok(MaybeBlocking::WouldBlock);
            }

            return Err(err.into());
        }

        self.take_line(self.pending().len(), buf)
    }
}

fn first_char(str: &str) -> MaybeBlocking<Option<(char, usize)>> {
    MaybeBlocking::Nonblocking(str.chars().next().map(|c| (c, c.len_utf8())))
}

fn replacement_character(amt: usize) -> MaybeBlocking<Option<(char, usize)>> {
    MaybeBlocking::Nonblocking(Some((char::REPLACEMENT_CHARACTER, amt)))
}

fn blocked(err: &io::Error) -> bool {
    err.kind() == io::ErrorKind::WouldBlock
}

fn invalid_utf8() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        "stream did not contain valid UTF-8",
    )
}

fn into_utf8(bytes: Vec<u8>) -> io::Result<String> {
    String::from_utf8(bytes).map_err(|_| invalid_utf8())
}

// `amt` is in/out, and is updated on the error path too
fn read_full<R: Read>(
    mut reader: R,
    mut buf: &mut [u8],
    amt: &mut usize,
) -> Result<MaybeBlocking<()>> {
    while !buf.is_empty() {
        match reader.read(buf) {
            Ok(0) => break,
            Ok(n) => {
                *amt += n;
                buf = &mut buf[n..];
            }
            Err(err) if err.kind() == io::ErrorKind::Interrupted => {}
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {
                if *amt == 0 {
                    return Ok(MaybeBlocking::WouldBlock);
                } else {
                    break;
                }
            }
            Err(err) => return Err(err.into()),
        }
    }

    Ok(MaybeBlocking::Nonblocking(()))
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

        Ok(size.map(|size| (size, result)))
    }};
}

macro_rules! port_read_vec_fn {
    ($br: expr, $fn: ident) => {{
        let mut result = Vec::new();
        let size = $br.$fn(&mut result)?;

        Ok(size.map(|size| (size, result)))
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
            _ => Err(StdioError::Unsupported(self)),
        }
    }

    pub fn read_line(&mut self) -> Result<MaybeBlocking<(usize, String)>> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br, read_line_peeked_stdin),
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s, read_line_peeked),
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br, read_line_peeked),
            SteelPortRepr::TcpStream(t) => port_read_str_fn!(t, read_line_peeked_unbuffered),
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
        }
    }

    pub fn flush(&mut self) -> Result<()> {
        match self {
            SteelPortRepr::FileOutput(_, s) => Ok(s.flush()?),
            SteelPortRepr::StdOutput(s) => Ok(s.flush()?),
            SteelPortRepr::StdError(s) => Ok(s.flush()?),
            SteelPortRepr::ChildStdInput(s) => Ok(s.flush()?),
            SteelPortRepr::StringOutput(s) => Ok(s.flush()?),
            SteelPortRepr::DynWriter(s) => Ok(s.lock().unwrap().flush()?),
            SteelPortRepr::TcpStream(t) => Ok(t.inner.flush()?),
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

    pub fn read_all_str(&mut self) -> Result<MaybeBlocking<(usize, String)>> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::StdInput(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::StringInput(s) => port_read_str_fn!(s, read_to_string_peeked),
            SteelPortRepr::ChildStdOutput(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::ChildStdError(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::DynReader(br) => port_read_str_fn!(br, read_to_string_peeked),
            SteelPortRepr::TcpStream(t) => port_read_str_fn!(t, read_to_string_peeked),
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
        }
    }

    pub fn read_all_bytes(&mut self) -> Result<MaybeBlocking<(usize, Vec<u8>)>> {
        match self {
            SteelPortRepr::FileInput(_, br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::StdInput(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::StringInput(s) => port_read_vec_fn!(s, read_to_end_peeked),
            SteelPortRepr::ChildStdOutput(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::ChildStdError(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::DynReader(br) => port_read_vec_fn!(br, read_to_end_peeked),
            SteelPortRepr::TcpStream(t) => port_read_vec_fn!(t, read_to_end_peeked),
            _ => stop!(ContractViolation => "expected input-port?, found {}", self),
        }
    }

    pub fn read_char(&mut self) -> Result<MaybeBlocking<Option<char>>> {
        match self {
            SteelPortRepr::FileInput(_, reader) => reader.read_char(),
            SteelPortRepr::StdInput(stdin) => stdin.read_char(),
            SteelPortRepr::ChildStdOutput(output) => output.read_char(),
            SteelPortRepr::ChildStdError(output) => output.read_char(),
            SteelPortRepr::StringInput(reader) => reader.read_char(),
            SteelPortRepr::DynReader(reader) => reader.read_char(),
            SteelPortRepr::TcpStream(t) => t.read_char(),
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

    pub fn peek_char(&mut self) -> Result<MaybeBlocking<Option<char>>> {
        match self {
            SteelPortRepr::FileInput(_, br) => br.peek_char(),
            SteelPortRepr::StdInput(br) => br.peek_char(),
            SteelPortRepr::StringInput(s) => s.peek_char(),
            SteelPortRepr::ChildStdOutput(br) => br.peek_char(),
            SteelPortRepr::ChildStdError(br) => br.peek_char(),
            SteelPortRepr::DynReader(br) => br.peek_char(),
            SteelPortRepr::TcpStream(t) => t.peek_char(),
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
                | SteelPortRepr::DynReader(_)
                | SteelPortRepr::TcpStream(_)
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
                | SteelPortRepr::TcpStream(_)
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

#[derive(Debug, PartialEq, Eq)]
pub enum MaybeBlocking<T> {
    Nonblocking(T),
    WouldBlock,
}

impl<T> MaybeBlocking<T> {
    pub fn map<U>(self, op: impl FnOnce(T) -> U) -> MaybeBlocking<U> {
        match self {
            MaybeBlocking::Nonblocking(value) => MaybeBlocking::Nonblocking(op(value)),
            MaybeBlocking::WouldBlock => MaybeBlocking::WouldBlock,
        }
    }
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
    pub fn read_line(&self) -> Result<MaybeBlocking<(usize, String)>> {
        self.port.write().read_line()
    }

    // TODO: Implement the rest of the flush methods
    pub fn flush(&self) -> Result<()> {
        self.port.write().flush()
    }

    pub fn read_all_str(&self) -> Result<MaybeBlocking<(usize, String)>> {
        self.port.write().read_all_str()
    }

    pub fn read_all_bytes(&self) -> Result<MaybeBlocking<(usize, Vec<u8>)>> {
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
)> = once_cell::sync::Lazy::new(|| super::structs::make_struct_singleton("would-block"));

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
    use std::net::TcpListener;

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

    struct Stalls {
        reads: VecDeque<Option<&'static [u8]>>,
    }

    impl Stalls {
        fn new(reads: &[Option<&'static [u8]>]) -> Self {
            Stalls {
                reads: reads.iter().copied().collect(),
            }
        }
    }

    impl Read for Stalls {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            match self.reads.pop_front() {
                Some(Some(bytes)) => {
                    buf[..bytes.len()].copy_from_slice(bytes);
                    Ok(bytes.len())
                }
                Some(None) => Err(io::Error::new(io::ErrorKind::WouldBlock, "nothing ready")),
                None => Ok(0),
            }
        }
    }

    struct Breaks {
        before: Option<&'static [u8]>,
    }

    impl Read for Breaks {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            let Some(bytes) = self.before.take() else {
                return Err(io::Error::new(io::ErrorKind::ConnectionReset, "reset"));
            };

            let amt = usize::min(bytes.len(), buf.len());
            buf[..amt].copy_from_slice(&bytes[..amt]);

            Ok(amt)
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
        let mut peekable = Peekable::new(Stalls::new(&[Some(b"abc"), None]));

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
    fn peek_char_reports_bytes_that_are_not_utf8_and_leaves_them() {
        let mut peekable = Peekable::new(Cursor::new(vec![0xFF, b'a']));

        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER),
            "peeking again reports the same character"
        );
        assert_eq!(
            nonblocking(peekable.read_byte().unwrap()),
            Some(0xFF),
            "and the byte is still there to be read"
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
            Some(char::REPLACEMENT_CHARACTER),
            "peeking again reports the same character"
        );
        assert_eq!(
            nonblocking(peekable.read_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(
            nonblocking(peekable.read_char().unwrap()),
            None,
            "reading is what moves past the stub"
        );
    }

    #[test]
    fn peek_char_at_the_end_is_nothing() {
        let mut peekable = Peekable::new(Cursor::new(Vec::new()));

        assert_eq!(nonblocking(peekable.peek_char().unwrap()), None);
    }

    #[test]
    fn peek_char_leaves_a_byte_that_is_not_utf8_for_a_later_read_char() {
        let mut peekable = Peekable::new(Cursor::new(vec![0xFF, b'a']));

        assert_eq!(
            nonblocking(peekable.peek_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(
            nonblocking(peekable.read_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(nonblocking(peekable.read_char().unwrap()), Some('a'));
    }

    #[test]
    fn read_char_keeps_what_it_has_when_the_source_stalls() {
        let mut peekable = Peekable::new(Stalls::new(&[Some(&[0xCE]), None, Some(&[0xBB])]));

        assert!(matches!(
            peekable.read_char().unwrap(),
            MaybeBlocking::WouldBlock
        ));
        assert_eq!(peekable.pending(), &[0xCE]);
        assert_eq!(nonblocking(peekable.read_char().unwrap()), Some('\u{3bb}'));
    }

    #[test]
    fn read_char_takes_only_the_bytes_of_the_character_it_returns() {
        let mut peekable = Peekable::new(Chunks::new(&[b"a", b"bc"]));

        assert_eq!(nonblocking(peekable.read_char().unwrap()), Some('a'));
        assert_eq!(
            peekable.pending(),
            b"",
            "the bytes after the character are left with the source"
        );
    }

    #[test]
    fn read_char_joins_a_character_split_across_reads() {
        let mut peekable = Peekable::new(Chunks::new(&[&[0xC3], &[0xA9], b"!"]));

        assert_eq!(nonblocking(peekable.read_char().unwrap()), Some('\u{e9}'));
        assert_eq!(nonblocking(peekable.read_char().unwrap()), Some('!'));
    }

    #[test]
    fn read_char_moves_past_bytes_that_are_not_utf8() {
        let mut peekable = Peekable::new(Cursor::new(vec![0xFF, b'a']));

        assert_eq!(
            nonblocking(peekable.read_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(nonblocking(peekable.read_char().unwrap()), Some('a'));
    }

    #[test]
    fn read_char_reports_a_character_the_source_cut_short() {
        let mut peekable = Peekable::new(Cursor::new(vec![0xC3]));

        assert_eq!(
            nonblocking(peekable.read_char().unwrap()),
            Some(char::REPLACEMENT_CHARACTER)
        );
        assert_eq!(nonblocking(peekable.read_char().unwrap()), None);
    }

    #[test]
    fn read_char_at_the_end_is_nothing() {
        let mut peekable = Peekable::new(Cursor::new(Vec::new()));

        assert_eq!(nonblocking(peekable.read_char().unwrap()), None);
    }

    #[test]
    fn read_char_on_a_closed_port_is_the_end_of_the_source() {
        let mut port = SteelPortRepr::StringInput(Peekable::new(Cursor::new(b"abc".to_vec())));
        port.close_port();

        assert_eq!(nonblocking(port.read_char().unwrap()), None);
    }

    #[test]
    fn peek_char_on_a_closed_port_is_the_end_of_the_source() {
        let mut port = SteelPortRepr::StringInput(Peekable::new(Cursor::new(b"abc".to_vec())));
        port.close_port();

        assert_eq!(nonblocking(port.peek_char().unwrap()), None);
    }

    #[test]
    fn peek_char_on_an_output_port_is_refused() {
        let mut port = SteelPortRepr::StringOutput(Vec::new());

        assert!(port.peek_char().is_err());
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
        let amt = nonblocking(peekable.read_to_end_peeked(&mut out).unwrap());

        assert_eq!(amt, 6);
        assert_eq!(out, b"abcdef");
    }

    #[test]
    fn a_read_that_breaks_partway_reports_what_it_already_took() {
        let mut reader = Breaks {
            before: Some(b"abc"),
        };

        let mut buf = [0; 8];
        let mut amt = 0;
        assert!(read_full(&mut reader, &mut buf, &mut amt).is_err());
        assert_eq!(amt, 3);
        assert_eq!(&buf[..amt], b"abc");
    }

    #[test]
    fn filling_keeps_the_bytes_it_read_before_the_source_broke() {
        let mut peekable = Peekable::new(Breaks {
            before: Some(b"abc"),
        });

        assert!(peekable.fill(8).is_err());
        assert_eq!(peekable.pending(), b"abc");
        assert!(peekable.at_source_start());
    }

    #[test]
    fn read_bytes_keeps_the_bytes_it_took_before_the_source_broke() {
        let mut peekable = Peekable::new(Breaks {
            before: Some(b"abc"),
        });

        let mut buf = [0; 8];
        assert!(peekable.read_bytes_amt(&mut buf).is_err());
        assert_eq!(
            peekable.pending(),
            b"abc",
            "the bytes are not handed out and dropped"
        );
        assert!(peekable.at_source_start());
    }

    #[test]
    fn read_bytes_keeps_the_peeked_prefix_when_the_source_breaks() {
        let mut peekable = Peekable::new(Breaks {
            before: Some(b"abc"),
        });

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.pending(), b"abc");

        let mut buf = [0; 8];
        assert!(peekable.read_bytes_amt(&mut buf).is_err());
        assert_eq!(peekable.pending(), b"abc");
        assert!(peekable.at_source_start());

        assert_eq!(
            nonblocking(peekable.read_bytes_amt(&mut buf[..3]).unwrap()),
            3
        );
        assert_eq!(&buf[..3], b"abc");
    }

    #[test]
    fn read_to_end_keeps_what_it_has_when_the_source_stalls() {
        let mut peekable = Peekable::new(Stalls::new(&[Some(b"abc"), None, Some(b"def")]));

        let mut bytes = Vec::new();
        assert_eq!(
            peekable.read_to_end_peeked(&mut bytes).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(peekable.pending(), b"abc");
        assert!(peekable.at_source_start());

        bytes.clear();
        assert_eq!(
            nonblocking(peekable.read_to_end_peeked(&mut bytes).unwrap()),
            6
        );
        assert_eq!(bytes, b"abcdef");
    }

    #[test]
    fn read_to_end_keeps_the_bytes_it_had_already_buffered() {
        let mut peekable = Peekable::new(Stalls::new(&[Some(b"abc"), None]));

        nonblocking(peekable.fill_chunk(usize::MAX).unwrap());
        assert_eq!(peekable.pending(), b"abc");

        let mut bytes = Vec::new();
        assert_eq!(
            peekable.read_to_end_peeked(&mut bytes).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(
            peekable.pending(),
            b"abc",
            "the buffered bytes are not handed out and dropped"
        );
    }

    #[test]
    fn read_to_string_keeps_what_it_has_when_the_source_stalls() {
        let mut peekable = Peekable::new(Stalls::new(&[Some(b"abc"), None, Some(b"def")]));

        let mut str = String::new();
        assert_eq!(
            peekable.read_to_string_peeked(&mut str).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(str, "");
        assert_eq!(peekable.pending(), b"abc");

        assert_eq!(
            nonblocking(peekable.read_to_string_peeked(&mut str).unwrap()),
            6
        );
        assert_eq!(str, "abcdef");
    }

    #[test]
    fn read_to_string_keeps_bytes_that_are_not_utf8() {
        let mut peekable = Peekable::new(Cursor::new(vec![b'a', 0xFF]));

        let mut str = String::new();
        assert!(peekable.read_to_string_peeked(&mut str).is_err());
        assert_eq!(str, "");

        let mut bytes = Vec::new();
        assert_eq!(
            nonblocking(peekable.read_to_end_peeked(&mut bytes).unwrap()),
            2
        );
        assert_eq!(
            bytes,
            vec![b'a', 0xFF],
            "what could not be decoded is still readable as bytes"
        );
    }

    #[test]
    fn read_to_string_joins_a_character_split_across_the_boundary() {
        let mut peekable = Peekable::new(Cursor::new("é".as_bytes().to_vec()));
        nonblocking(peekable.fill(1).unwrap());
        assert_eq!(peekable.pending(), &[0xC3]);

        let mut out = String::new();
        let amt = nonblocking(peekable.read_to_string_peeked(&mut out).unwrap());

        assert_eq!(amt, 2);
        assert_eq!(out, "é");
    }

    #[test]
    fn read_line_takes_a_line_that_is_already_buffered() {
        let mut peekable = filled(b"ab\ncd");

        let mut out = String::new();
        let amt = nonblocking(peekable.read_line_peeked(&mut out).unwrap());

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
        let amt = nonblocking(peekable.read_line_peeked(&mut out).unwrap());

        assert_eq!(amt, 3);
        assert_eq!(out, "ab\n");
    }

    #[test]
    fn read_line_returns_a_last_line_without_a_newline() {
        let mut peekable = filled(b"cd");

        let mut out = String::new();
        let amt = nonblocking(peekable.read_line_peeked(&mut out).unwrap());

        assert_eq!(amt, 2);
        assert_eq!(out, "cd");
    }

    #[test]
    fn read_line_keeps_the_peeked_prefix_when_the_source_stalls() {
        let mut peekable = Peekable::new(BufReader::new(Stalls::new(&[
            Some(b"half "),
            None,
            Some(b"a line\n"),
        ])));

        assert_eq!(nonblocking(peekable.peek_char().unwrap()), Some('h'));

        let mut line = String::new();
        assert_eq!(
            peekable.read_line_peeked(&mut line).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(line, "");

        assert_eq!(
            nonblocking(peekable.read_line_peeked(&mut line).unwrap()),
            "half a line\n".len()
        );
        assert_eq!(line, "half a line\n");
    }

    #[test]
    fn read_line_keeps_a_partial_line_nothing_had_peeked_yet() {
        let mut peekable = Peekable::new(BufReader::new(Stalls::new(&[
            Some(b"half "),
            None,
            Some(b"a line\n"),
        ])));

        let mut line = String::new();
        assert_eq!(
            peekable.read_line_peeked(&mut line).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(line, "");

        assert_eq!(
            nonblocking(peekable.read_line_peeked(&mut line).unwrap()),
            "half a line\n".len()
        );
        assert_eq!(line, "half a line\n");
    }

    #[test]
    fn read_line_moves_the_port_past_the_source_start() {
        let mut peekable = Peekable::new(BufReader::new(Cursor::new(b"one\ntwo\n".to_vec())));

        let mut line = String::new();
        assert_eq!(
            nonblocking(peekable.read_line_peeked(&mut line).unwrap()),
            4
        );
        assert!(
            !peekable.at_source_start(),
            "a line that was handed out is no longer the start of the source"
        );
    }

    #[test]
    fn read_line_at_the_end_is_zero() {
        let mut peekable = Peekable::new(Cursor::new(Vec::new()));

        let mut out = String::new();

        assert_eq!(nonblocking(peekable.read_line_peeked(&mut out).unwrap()), 0);
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

    #[test]
    fn the_would_block_object_is_not_the_end_of_the_source() {
        let blocked = would_block();

        assert!(crate::primitives::ports::would_block_objectp(&blocked));
        assert!(!crate::primitives::ports::eof_objectp(&blocked));
        assert_eq!(blocked.to_string(), "(would-block)");
    }

    #[test]
    fn as_stdio_refuses_a_closed_port() {
        let Err(err) = SteelPortRepr::Closed.as_stdio() else {
            panic!("a closed port has no handle to give a child");
        };

        assert!(!err.is_buffered());
        assert!(matches!(err.into_port(), SteelPortRepr::Closed));
    }

    #[test]
    fn read_line_without_buffering_spans_several_reads() {
        let mut peekable = Peekable::new(Chunks::new(&[b"abc", b"def\nrest"]));

        let mut line = String::new();
        assert_eq!(
            nonblocking(peekable.read_line_peeked_unbuffered(&mut line).unwrap()),
            "abcdef\n".len()
        );
        assert_eq!(line, "abcdef\n");

        let mut rest = String::new();
        nonblocking(peekable.read_line_peeked_unbuffered(&mut rest).unwrap());
        assert_eq!(rest, "rest");
    }

    #[test]
    fn read_line_without_buffering_ends_with_the_source() {
        let mut peekable = Peekable::new(Chunks::new(&[b"no newline here"]));

        let mut line = String::new();
        nonblocking(peekable.read_line_peeked_unbuffered(&mut line).unwrap());
        assert_eq!(line, "no newline here");

        let mut nothing = String::new();
        assert_eq!(
            nonblocking(peekable.read_line_peeked_unbuffered(&mut nothing).unwrap()),
            0
        );
        assert_eq!(nothing, "");
    }

    #[test]
    fn read_line_without_buffering_reports_a_source_that_blocks() {
        let mut peekable = Peekable::new(Stalls::new(&[Some(b"half a line"), None]));

        let mut line = String::new();
        assert_eq!(
            peekable.read_line_peeked_unbuffered(&mut line).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(line, "");
        assert_eq!(peekable.pending(), b"half a line");
    }

    #[test]
    fn read_line_without_buffering_moves_past_a_line_that_is_not_utf8() {
        let mut peekable = Peekable::new(Chunks::new(&[b"bad \xff\n", b"good\n"]));

        let mut line = String::new();
        assert!(peekable.read_line_peeked_unbuffered(&mut line).is_err());

        assert_eq!(
            nonblocking(peekable.read_line_peeked_unbuffered(&mut line).unwrap()),
            "good\n".len()
        );
        assert_eq!(line, "good\n");
    }

    #[test]
    fn read_line_without_buffering_finishes_a_line_the_source_stalled_on() {
        let mut peekable = Peekable::new(Stalls::new(&[Some(b"half "), None, Some(b"a line\n")]));

        let mut line = String::new();
        assert_eq!(
            peekable.read_line_peeked_unbuffered(&mut line).unwrap(),
            MaybeBlocking::WouldBlock
        );
        assert_eq!(line, "");

        assert_eq!(
            nonblocking(peekable.read_line_peeked_unbuffered(&mut line).unwrap()),
            "half a line\n".len()
        );
        assert_eq!(line, "half a line\n");
    }

    #[test]
    fn read_line_without_buffering_spans_many_chunks() {
        let long = vec![b'x'; MAX_STREAM_CHUNK * 3];
        let mut source = long.clone();
        source.extend_from_slice(b"\nnext\n");

        let mut peekable = Peekable::new(Cursor::new(source));

        let mut line = String::new();
        assert_eq!(
            nonblocking(peekable.read_line_peeked_unbuffered(&mut line).unwrap()),
            long.len() + 1
        );
        assert_eq!(line.len(), long.len() + 1);
        assert!(line.ends_with("x\n"));

        line.clear();
        assert_eq!(
            nonblocking(peekable.read_line_peeked_unbuffered(&mut line).unwrap()),
            5
        );
        assert_eq!(line, "next\n");
    }

    fn tcp_pair() -> (TcpStream, TcpStream) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let client = TcpStream::connect(listener.local_addr().unwrap()).unwrap();
        let (server, _) = listener.accept().unwrap();

        (client, server)
    }

    fn tcp_port(bytes: &[u8]) -> SteelPortRepr {
        let (client, mut server) = tcp_pair();
        server.write_all(bytes).unwrap();
        drop(server);

        SteelPortRepr::TcpStream(Peekable::new(client))
    }

    #[test]
    fn a_tcp_stream_port_is_an_input_and_an_output_port() {
        let port = tcp_port(b"abc");

        assert!(port.is_input());
        assert!(port.is_output());
    }

    #[test]
    fn a_tcp_stream_port_reads_lines() {
        let mut port = tcp_port(b"first\nsecond\n");

        assert_eq!(
            nonblocking(port.read_line().unwrap()),
            (6, "first\n".to_string())
        );
        assert_eq!(
            nonblocking(port.read_line().unwrap()),
            (7, "second\n".to_string())
        );
        assert_eq!(nonblocking(port.read_line().unwrap()), (0, String::new()));
    }

    #[test]
    fn a_tcp_stream_port_peeks_a_char_back_to_back_with_a_read() {
        let mut port = tcp_port("λ rest".as_bytes());

        assert_eq!(nonblocking(port.peek_char().unwrap()), Some('λ'));
        assert_eq!(nonblocking(port.peek_char().unwrap()), Some('λ'));
        assert_eq!(nonblocking(port.read_char().unwrap()), Some('λ'));
        assert_eq!(nonblocking(port.read_all_str().unwrap()).1, " rest");
    }

    #[test]
    fn a_tcp_stream_port_reads_the_bytes_left_after_a_peek() {
        let mut port = tcp_port(b"abc");

        assert_eq!(nonblocking(port.peek_byte().unwrap()), Some(b'a'));
        assert_eq!(
            nonblocking(port.read_all_bytes().unwrap()),
            (3, b"abc".to_vec())
        );
    }

    #[test]
    fn a_tcp_stream_port_writes_to_the_other_end() {
        let (client, server) = tcp_pair();
        let mut port = SteelPortRepr::TcpStream(Peekable::new(client));

        port.write(b"to the peer\n").unwrap();
        port.flush().unwrap();

        let mut line = String::new();
        BufReader::new(server).read_line(&mut line).unwrap();
        assert_eq!(line, "to the peer\n");
    }

    #[test]
    fn a_dyn_reader_port_is_an_input_port() {
        let reader: Box<dyn Read + Send + Sync> = Box::new(Cursor::new(b"first\nsecond".to_vec()));
        let mut port = SteelPortRepr::DynReader(Peekable::new(BufReader::new(reader)));

        assert!(port.is_input());
        assert!(!port.is_output());
        assert_eq!(
            nonblocking(port.read_line().unwrap()),
            (6, "first\n".to_string())
        );
    }

    #[test]
    fn the_standard_error_port_flushes() {
        let mut port = SteelPortRepr::StdError(io::stderr());

        assert!(port.is_output());
        assert!(port.flush().is_ok());
    }
}
