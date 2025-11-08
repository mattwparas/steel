use std::net::TcpStream;

use abi_stable::std_types::{RBoxError, RResult};
use steel::steel_vm::ffi::{as_underlying_ffi_type, CustomRef, FFIArg, FFIValue};
use tungstenite::{
    connect, handshake::client::Response, stream::MaybeTlsStream, Message, WebSocket,
};

use steel::{rvals::Custom, steel_vm::ffi::IntoFFIVal};

use steel::{
    declare_module,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

struct SteelWebSocket(WebSocket<MaybeTlsStream<TcpStream>>);
impl Custom for SteelWebSocket {}

impl SteelWebSocket {
    fn read_message(&mut self) -> Result<WebSocketMessage, WebSocketError> {
        self.0
            .read_message()
            .map(WebSocketMessage)
            .map_err(WebSocketError)
    }

    fn write_message(&mut self, message: FFIArg) -> RResult<FFIValue, RBoxError> {
        if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = message {
            if let Some(inner) = as_underlying_ffi_type::<WebSocketMessage>(custom.get_mut()) {
                let result = self.0.write_message(std::mem::replace(
                    &mut inner.0,
                    Message::Text(String::new()),
                ));

                match result {
                    Ok(_) => true.into_ffi_val(),
                    Err(e) => RResult::RErr(RBoxError::new(e)),
                }
            } else {
                false.into_ffi_val()
            }
        } else {
            false.into_ffi_val()
        }
    }
}

#[derive(Clone)]
struct WebSocketMessage(Message);

impl Custom for WebSocketMessage {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", self.0)))
    }
}

impl WebSocketMessage {
    fn is_ping(&self) -> bool {
        self.0.is_ping()
    }

    fn is_pong(&self) -> bool {
        self.0.is_pong()
    }

    fn is_text(&self) -> bool {
        self.0.is_text()
    }
}

#[allow(dead_code)]
struct WebSocketResponse(Response);
impl Custom for WebSocketResponse {}

#[derive(Debug)]
struct WebSocketError(tungstenite::Error);

impl Custom for WebSocketError {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", self.0)))
    }
}

/// In lieu of a direct bytes type in Steel, since we need to typically respond to a ping with a pong
/// containing the matching payload, we can skip the deserializing into the Steel type and directly
/// construct the pong here
fn ping_to_pong(message: &WebSocketMessage) -> Option<WebSocketMessage> {
    if let Message::Ping(payload) = &message.0 {
        Some(WebSocketMessage(Message::Pong(payload.clone())))
    } else {
        None
    }
}

/// Extracts the payload from a given message
fn text_payload(message: &WebSocketMessage) -> Option<String> {
    if let Message::Text(text) = &message.0 {
        Some(text.clone())
    } else {
        None
    }
}

// TODO: @Matt -> perhaps explore this and/or explore using tokio tungstenite for this
fn _make_non_blocking(socket: &mut SteelWebSocket) -> std::result::Result<(), std::io::Error> {
    match socket.0.get_mut() {
        MaybeTlsStream::Plain(s) => s.set_nonblocking(true),
        MaybeTlsStream::Rustls(s) => s.get_mut().set_nonblocking(true),
        _ => todo!(),
    }
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/web/ws");

    module
        .register_fn("ws/message-ping?", WebSocketMessage::is_ping)
        .register_fn("ws/message-pong?", WebSocketMessage::is_pong)
        .register_fn("ws/message-text?", WebSocketMessage::is_text)
        .register_fn("ws/message-text", |text: String| {
            WebSocketMessage(Message::Text(text))
        })
        .register_fn("ws/message-ping->pong", ping_to_pong)
        .register_fn("ws/message->text-payload", text_payload)
        .register_fn("ws/connect", |url: String| {
            connect(url)
                .map(|(socket, resp)| {
                    vec![
                        SteelWebSocket(socket).into_ffi_val().unwrap(),
                        WebSocketResponse(resp).into_ffi_val().unwrap(),
                    ]
                })
                .map_err(WebSocketError)
        })
        .register_fn("ws/read-message!", SteelWebSocket::read_message)
        .register_fn("ws/write-message!", SteelWebSocket::write_message);

    module
}
