use std::io::BufReader;
use std::net::{TcpListener, TcpStream};

use steel_derive::function;

use crate::gc::Gc;
use crate::rvals::{AsRefSteelVal, Custom, IntoSteelVal, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::values::lists::Pair;
use crate::values::port::SteelPort;
use crate::values::SteelPortRepr;

impl Custom for TcpStream {}
impl Custom for TcpListener {}

// TODO: Maybe include the following as builtins:
// - https://crates.io/crates/httparse
// - https://github.com/hyperium/http
// - Perhaps include hyper as well

#[function(name = "tcp-connect")]
pub fn tcp_connect(addr: SteelString) -> Result<SteelVal> {
    TcpStream::connect(addr.as_str())?.into_steelval()
}

#[function(name = "tcp-shutdown!")]
pub fn tcp_close(stream: &SteelVal) -> Result<SteelVal> {
    let writer = TcpStream::as_ref(stream)?.try_clone().unwrap();
    writer.shutdown(std::net::Shutdown::Both)?;
    Ok(SteelVal::Void)
}

#[function(name = "tcp-stream-writer")]
pub fn tcp_input_port(stream: &SteelVal) -> Result<SteelVal> {
    let writer = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::new_dyn_writer_port(writer))
}

#[function(name = "tcp-stream-reader")]
pub fn tcp_output_port(stream: &SteelVal) -> Result<SteelVal> {
    let reader = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::PortV(SteelPort {
        port: Gc::new_mut(SteelPortRepr::TcpStream(reader)),
    }))
}

#[function(name = "tcp-stream-buffered-reader")]
pub fn tcp_buffered_output_port(stream: &SteelVal) -> Result<SteelVal> {
    let reader = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::PortV(SteelPort {
        port: Gc::new_mut(SteelPortRepr::DynReader(BufReader::new(Box::new(reader)))),
    }))
}

#[function(name = "tcp-listen")]
pub fn tcp_listen(addr: SteelString) -> Result<SteelVal> {
    TcpListener::bind(addr.as_str())?.into_steelval()
}

#[function(name = "tcp-accept")]
pub fn tcp_accept(value: &SteelVal) -> Result<SteelVal> {
    TcpListener::as_ref(value)?.accept()?.0.into_steelval()
}

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
