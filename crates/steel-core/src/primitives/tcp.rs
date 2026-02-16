use std::io::BufReader;
use std::net::{TcpListener, TcpStream};

use steel_derive::function;

use crate::gc::Gc;
use crate::rvals::{AsRefSteelVal, Custom, IntoSteelVal, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::values::lists::Pair;
use crate::values::port::{Peekable, SteelPort};
use crate::values::SteelPortRepr;

impl Custom for TcpStream {}
impl Custom for TcpListener {}

// TODO: Maybe include the following as builtins:
// - https://crates.io/crates/httparse
// - https://github.com/hyperium/http
// - Perhaps include hyper as well

/// Opens a TCP connection to a remote host.
///
/// `addr` is an address of the remote host as a string
///
/// If `addr` yields multiple addresses, `tcp-connect` will be attempted with
/// each of the addresses until a connection is successful. If none of
/// the addresses result in a successful connection, the error returned from
/// the last connection attempt (the last address) is returned.
///
/// # Examples
///
/// Open a TCP connection to `127.0.0.1:8080`:
///
/// ```scheme
/// (tcp-connect "127.0.0.1:8080")
/// ```
#[function(name = "tcp-connect")]
pub fn tcp_connect(addr: SteelString) -> Result<SteelVal> {
    TcpStream::connect(addr.as_str())?.into_steelval()
}

/// Shuts down both halves of this tcp connection.
///
/// This function will cause all pending and future I/O on the specified
/// portions to return immediately with either an error or an okay result,
/// dependong on if it was the write or read half.
///
/// # Platform-specific behavior
///
/// Calling this function multiple times may result in different behavior,
/// depending on the operating system. On Linux, the second call will
/// return `Ok(())`, but on macOS, it will return an error wrapping the rust error
/// `ErrorKind::NotConnected`. This may change in the future.
///
/// ```scheme
/// (tcp-shutdown! stream) ;; stream : TcpStream?
/// ```
#[function(name = "tcp-shutdown!")]
pub fn tcp_close(stream: &SteelVal) -> Result<SteelVal> {
    let writer = TcpStream::as_ref(stream)?.try_clone().unwrap();
    writer.shutdown(std::net::Shutdown::Both)?;
    Ok(SteelVal::Void)
}

/// Get the writer half of this tcp stream as a port.
///
/// ```scheme
/// (define my-port (tcp-connect "127.0.0.1:8080"))
/// (tcp-stream-writer my-port) ;; => port?
/// ```
#[function(name = "tcp-stream-writer")]
pub fn tcp_input_port(stream: &SteelVal) -> Result<SteelVal> {
    let writer = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::new_dyn_writer_port(writer))
}

/// Get the reader half of this tcp stream as a port.
///
/// ```scheme
/// (define my-port (tcp-connect "127.0.0.1:8080"))
/// (tcp-stream-reader my-port) ;; => port?
/// ```
#[function(name = "tcp-stream-reader")]
pub fn tcp_output_port(stream: &SteelVal) -> Result<SteelVal> {
    let reader = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::PortV(SteelPort {
        port: Gc::new_lock(SteelPortRepr::TcpStream(
            stream.clone(),
            Peekable::new(reader),
        )),
    }))
}

/// Get the reader half of this tcp stream as a buffered reader port.
///
/// ```scheme
/// (define my-port (tcp-connect "127.0.0.1:8080"))
/// (tcp-stream-buffered-reader my-port) ;; => port?
/// ```
#[function(name = "tcp-stream-buffered-reader")]
pub fn tcp_buffered_output_port(stream: &SteelVal) -> Result<SteelVal> {
    let reader = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::PortV(SteelPort {
        port: Gc::new_lock(SteelPortRepr::DynReader(Peekable::new(BufReader::new(
            Box::new(reader),
        )))),
    }))
}

/// Creates a new `TcpListener?` which will be bound to the specified
/// address.
///
/// The returned listener is ready for accepting connections.
///
/// ```scheme
/// (tcp-listen addr) -> TcpListener?
/// ```
///
/// Binding with a port number of 0 will request that the OS assigns a port
/// to this listener. The port allocated can be queried via the
/// `tcp-listener-local-addr` method.
///
/// If `addr` yields multiple addresses, `bind` will be attempted with
/// each of the addresses until one succeeds and returns the listener. If
/// none of the addresses succeed in creating a listener, the error returned
/// from the last attempt (the last address) is returned.
///
/// # Examples
///
/// Creates a TCP listener bound to `127.0.0.1:80`:
///
/// ```scheme
/// (tcp-listen "127.0.0.1:80")
/// ```
///
#[function(name = "tcp-listen")]
pub fn tcp_listen(addr: SteelString) -> Result<SteelVal> {
    TcpListener::bind(addr.as_str())?.into_steelval()
}

/// Returns the local socket address of this listener.
///
/// ```scheme
/// (tcp-listener-local-addr listener) -> string?
/// ```
///
/// listener : TcpListener?
#[function(name = "tcp-listener-local-addr")]
pub fn tcp_listener_local_addr(value: &SteelVal) -> Result<SteelVal> {
    TcpListener::as_ref(value)?
        .local_addr()?
        .to_string()
        .into_steelval()
}

/// Accept a new incoming connection from this listener.
///
/// This function will block the calling thread until a new TCP connection
/// is established. When established, the corresponding `TcpStream?` will be
/// returned
///
/// # Examples
///
/// ```scheme
/// (define listener (tcp-listen "127.0.0.1:8080"))
///
/// (tcp-accept listener) ;; => TcpStream?
/// ```
#[function(name = "tcp-accept")]
pub fn tcp_accept(value: &SteelVal) -> Result<SteelVal> {
    TcpListener::as_ref(value)?.accept()?.0.into_steelval()
}

/// Accept a new incoming connection from this listener.
///
/// This function will block the calling thread until a new TCP connection
/// is established. When established, the corresponding `TcpStream?` will be
/// returned with the remote address in a pair
///
/// # Examples
///
/// ```scheme
/// (define listener (tcp-listen "127.0.0.1:8080"))
///
/// (tcp-accept listener) ;; => (cons TcpStream? string?)
/// ```
#[function(name = "tcp-accept-with-addr")]
pub fn tcp_accept_addr(value: &SteelVal) -> Result<SteelVal> {
    let res = TcpListener::as_ref(value)?.accept()?;

    Ok(SteelVal::Pair(Gc::new(Pair::cons(
        res.0.into_steelval()?,
        res.1.to_string().into_steelval()?,
    ))))
}

#[function(name = "tcp-listener-set-non-blocking!")]
pub fn tcp_listener_set_non_blocking(value: &SteelVal) -> Result<SteelVal> {
    TcpListener::as_ref(value)?.set_nonblocking(true)?;
    Ok(SteelVal::Void)
}

#[function(name = "tcp-stream-set-non-blocking!")]
pub fn tcp_stream_set_non_blocking(value: &SteelVal) -> Result<SteelVal> {
    TcpStream::as_ref(value)?.set_nonblocking(true)?;
    Ok(SteelVal::Void)
}

pub fn tcp_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/tcp".to_string());

    module
        .register_native_fn_definition(TCP_CONNECT_DEFINITION)
        .register_native_fn_definition(TCP_CLOSE_DEFINITION)
        .register_native_fn_definition(TCP_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(TCP_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(TCP_BUFFERED_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(TCP_LISTEN_DEFINITION)
        .register_native_fn_definition(TCP_ACCEPT_DEFINITION)
        .register_native_fn_definition(TCP_ACCEPT_ADDR_DEFINITION)
        .register_native_fn_definition(TCP_LISTENER_SET_NON_BLOCKING_DEFINITION)
        .register_native_fn_definition(TCP_STREAM_SET_NON_BLOCKING_DEFINITION);

    module
}
