use std::io::{BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use steel_derive::function;

use crate::gc::Gc;
use crate::rvals::{AsRefSteelVal, Custom, IntoSteelVal, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
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

#[function(name = "tcp-stream-input-port")]
pub fn tcp_input_port(stream: &SteelVal) -> Result<SteelVal> {
    let writer = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::new_dyn_writer_port(writer))
}

#[function(name = "tcp-stream-output-port")]
pub fn tcp_output_port(stream: &SteelVal) -> Result<SteelVal> {
    let reader = TcpStream::as_ref(stream)?.try_clone().unwrap();
    Ok(SteelVal::PortV(SteelPort {
        port: Gc::new_mut(SteelPortRepr::TcpStream(reader)),
    }))
}

#[function(name = "tcp-stream-buffered-output-port")]
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

pub fn tcp_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/tcp".to_string());

    module
        .register_native_fn_definition(TCP_CONNECT_DEFINITION)
        .register_native_fn_definition(TCP_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(TCP_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(TCP_BUFFERED_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(TCP_LISTEN_DEFINITION)
        .register_native_fn_definition(TCP_ACCEPT_DEFINITION);

    module
}
