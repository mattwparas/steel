use std::net::TcpStream;

use tungstenite::{connect, stream::MaybeTlsStream, Message, WebSocket};
use url::Url;

use crate::{
    rerrs::ErrorKind,
    rvals::Custom,
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    stop, SteelErr, SteelVal,
};

#[derive(Debug)]
struct SocketWrapper {
    socket: WebSocket<MaybeTlsStream<TcpStream>>,
}

impl Custom for SocketWrapper {}

impl SocketWrapper {
    pub fn connect(url: String) -> Result<Self, SteelErr> {
        let (socket, response) = connect(Url::parse(&url).map_err(|_| {
            SteelErr::new(
                ErrorKind::Generic,
                "Unable to parse url into valid url string".to_string(),
            )
        })?)
        .map_err(|_| {
            SteelErr::new(
                ErrorKind::Generic,
                "Unable to connect to ws server".to_string(),
            )
        })?;

        println!("Connected to the server");
        println!("Response HTTP code: {}", response.status());
        println!("Response contains the following headers:");
        for (ref header, _value) in response.headers() {
            println!("* {}", header);
        }

        Ok(Self { socket })
    }

    pub fn read_message(&mut self) -> Result<WrappedMessage, SteelErr> {
        self.socket.read_message().map(WrappedMessage).map_err(|x| {
            SteelErr::new(
                ErrorKind::Generic,
                format!("Unable to read message from socket: {}", x),
            )
        })
    }

    pub fn write_message(&mut self, message: WrappedMessage) -> Result<(), SteelErr> {
        self.socket.write_message(message.0).map_err(|x| {
            SteelErr::new(
                ErrorKind::Generic,
                format!("Unable to write message to socket: {}", x),
            )
        })
    }
}

#[derive(Clone, Debug)]
struct WrappedMessage(Message);

impl WrappedMessage {
    pub fn is_ping(&self) -> bool {
        self.0.is_ping()
    }

    pub fn is_text(&self) -> bool {
        self.0.is_text()
    }

    pub fn new_text(contents: String) -> WrappedMessage {
        WrappedMessage(Message::Text(contents))
    }

    pub fn text_payload(&self) -> Option<String> {
        if let Message::Text(text) = &self.0 {
            Some(text.clone())
        } else {
            None
        }
    }

    pub fn pong_from_ping(&self) -> Option<WrappedMessage> {
        if let Message::Ping(payload) = &self.0 {
            Some(WrappedMessage(Message::Pong(payload.clone())))
        } else {
            None
        }
    }
}

impl Custom for WrappedMessage {}

pub fn requests_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/web/ws".to_string());

    // module
    //     .register_fn("request/client", Client::new)
    //     .register_fn("get", get::<String>)
    //     .register_fn("post", post_wrapper)
    //     .register_fn("request/json", SteelRequestBuilder::json)
    //     .register_fn("request/query", SteelRequestBuilder::query)
    //     .register_fn("request/send", SteelRequestBuilder::send)
    //     .register_fn("request/header", SteelRequestBuilder::header)
    //     .register_fn("request/basic-auth", SteelRequestBuilder::basic_auth)
    //     .register_fn("request/bearer-auth", SteelRequestBuilder::bearer_auth)
    //     .register_fn("response/status", SteelResponse::status)
    //     .register_fn("status-code->int", status_code_to_int)
    //     .register_fn("status-code/success?", StatusCode::is_success)
    //     .register_fn("status-code/redirect?", StatusCode::is_redirection)
    //     .register_fn("status-code/client-error?", StatusCode::is_client_error)
    //     .register_fn("status-code/server-error?", StatusCode::is_server_error)
    //     .register_type::<SteelRequestBuilder>("request/builder?")
    //     .register_type::<StatusCode>("status-code?")
    //     .register_type::<Client>("request/client?")
    //     .register_fn("response?", SteelResponse::identity)
    //     .register_fn("response->json", SteelResponse::json);

    module
}
