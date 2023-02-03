use std::net::TcpStream;

use tungstenite::{
    connect, handshake::client::Response, stream::MaybeTlsStream, Message, WebSocket,
};

use crate::{
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
};

type SteelWebSocket = WebSocket<MaybeTlsStream<TcpStream>>;

impl Custom for SteelWebSocket {}
impl Custom for Message {
    fn fmt(&self) -> Option<std::result::Result<String, std::fmt::Error>> {
        Some(Ok(format!("{:?}", self)))
    }
}
impl Custom for Response {}

/// In lieu of a direct bytes type in Steel, since we need to typically respond to a ping with a pong
/// containing the matching payload, we can skip the deserializing into the Steel type and directly
/// construct the pong here
fn ping_to_pong(message: &Message) -> Option<Message> {
    if let Message::Ping(payload) = message {
        Some(Message::Pong(payload.clone()))
    } else {
        None
    }
}

/// Extracts the payload from a given message
pub fn text_payload(message: &Message) -> Option<String> {
    if let Message::Text(text) = &message {
        Some(text.clone())
    } else {
        None
    }
}

pub fn websockets_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/web/ws".to_string());

    module
        .register_fn("ws/message-ping?", Message::is_ping)
        .register_fn("ws/message-pog?", Message::is_pong)
        .register_fn("ws/message-text?", Message::is_text)
        .register_fn("ws/message-text", Message::Text)
        .register_fn("ws/message-ping->pong", ping_to_pong)
        .register_fn("ws/message->text-payload", text_payload)
        .register_fn("ws/connect", connect::<String>)
        .register_fn("ws/read-message!", SteelWebSocket::read_message)
        .register_fn("ws/write-message!", SteelWebSocket::write_message);

    module
}
