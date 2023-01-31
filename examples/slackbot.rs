use reqwest::blocking::Client;
use serde_json::Value;
use std::{io::Read, net::TcpStream, path::PathBuf};

use tungstenite::{connect, stream::MaybeTlsStream, Message, WebSocket};
use url::Url;

use steel::{
    rerrs::ErrorKind,
    rvals::Custom,
    steel_vm::{
        builtin::BuiltInModule, engine::Engine, primitives::register_builtin_modules,
        register_fn::RegisterFn,
    },
    stop, SteelErr, SteelVal,
};

#[derive(Clone)]
struct HttpClient {
    client: reqwest::blocking::Client,
}

impl std::fmt::Debug for HttpClient {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HttpClient")
    }
}

impl HttpClient {
    pub fn new() -> Self {
        HttpClient {
            client: Client::new(),
        }
    }

    pub fn post(
        &self,
        url: String,
        headers: Vec<Vec<String>>,
        body: SteelVal,
    ) -> Result<SteelVal, SteelErr> {
        let mut request = self.client.post(url);

        for pair in headers {
            match pair.as_slice() {
                [desc, content_type] if desc == "content-type" => {
                    request = request.header(reqwest::header::CONTENT_TYPE, content_type)
                }
                [desc, authorization] if desc == "authorization" => {
                    request = request.header(reqwest::header::AUTHORIZATION, authorization)
                }
                other => stop!(Generic => "Unhandled header type: {:?}", other),
            }
        }

        let json_body = Value::try_from(body)?;

        request = request.json(&json_body);

        request
            .send()
            .map_err(|e| SteelErr::new(ErrorKind::Generic, format!("Unable to send request: {e}")))?
            .json::<Value>()
            .map_err(|_| {
                SteelErr::new(
                    ErrorKind::Generic,
                    "Unable to deserialize json response".to_string(),
                )
            })
            .map(SteelVal::try_from)?
    }
}

impl Custom for HttpClient {}

fn main() -> Result<(), std::io::Error> {
    let mut engine = Engine::new_raw();

    register_builtin_modules(&mut engine);

    engine
        .compile_and_run_raw_program(steel::steel_vm::primitives::ALL_MODULES)
        .unwrap();

    let mut module = BuiltInModule::new("steel/requests".to_string());

    module
        // .register_type::<WebSocketResponse>("WebSocketResponse?")
        .register_type::<HttpClient>("Client?")
        .register_fn("Client", HttpClient::new)
        .register_fn("post", HttpClient::post)
        .register_fn("ws/connect", SocketWrapper::connect)
        .register_fn("ws/read-message", SocketWrapper::read_message)
        .register_fn("ws/write-message", SocketWrapper::write_message)
        .register_fn("message-ping?", WrappedMessage::is_ping)
        .register_fn("message-text?", WrappedMessage::is_text)
        .register_fn("new-message-text", WrappedMessage::new_text)
        .register_fn("message-text", WrappedMessage::text_payload)
        .register_fn("ping->pong", WrappedMessage::pong_from_ping);

    engine.register_module(module);

    let core_libraries = [
        steel::stdlib::PRELUDE,
        steel::stdlib::CONTRACTS,
        steel::stdlib::DISPLAY,
    ];

    for core in core_libraries.into_iter() {
        engine.compile_and_run_raw_program(core).unwrap();
    }

    let path = "examples/scripts/slack.rkt";

    let path_buf = PathBuf::from(path);
    let mut exprs = String::new();

    {
        let mut file = std::fs::File::open(path)?;
        file.read_to_string(&mut exprs)?;
    }

    let res = engine.compile_and_run_raw_program_with_path(&exprs, path_buf);

    if let Err(e) = res {
        e.emit_result(path, &exprs);
    }

    Ok(())
}

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
            println!("* {header}");
        }

        Ok(Self { socket })
    }

    pub fn read_message(&mut self) -> Result<WrappedMessage, SteelErr> {
        self.socket.read_message().map(WrappedMessage).map_err(|x| {
            SteelErr::new(
                ErrorKind::Generic,
                format!("Unable to read message from socket: {x}"),
            )
        })
    }

    pub fn write_message(&mut self, message: WrappedMessage) -> Result<(), SteelErr> {
        self.socket.write_message(message.0).map_err(|x| {
            SteelErr::new(
                ErrorKind::Generic,
                format!("Unable to write message to socket: {x}"),
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
