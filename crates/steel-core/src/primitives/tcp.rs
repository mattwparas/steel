use std::any::Any;
use std::io::Write;
use std::net::TcpStream;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use steel_derive::function;

use crate::rvals::{Custom, IntoSteelVal, Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;

impl Custom for TcpStream {}

// use crate::values::port::PortLike;

#[function(name = "tcp-connect")]
pub fn connect(addr: SteelString) -> Result<SteelVal> {
    TcpStream::connect(addr.as_str())?.into_steelval()

    // How do I get out of a port...?
}

// Convert out of a value to the thing
// pub fn port_to_tcpstream(value: Arc<Mutex<dyn PortLike>>) {
//     let guard = value.lock().unwrap();

//     // Port like, needs its own thing. Assuming it _also_ implements custom type,
//     // then we should be able to downcast into the type no problem with some magic.
//     let inner = guard.deref().as_any_ref().downcast_ref::<TcpStream>();

//     todo!()

//     // How to call generic write methods on the trait object?
//     // if let Some(tcp) = (guard as &dyn Any).downcast_ref::<TcpStream>() {
//     //     todo!()
//     // } else {
//     //     todo!()
//     // }
// }

pub fn tcp_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/tcp".to_string());

    todo!()
}
