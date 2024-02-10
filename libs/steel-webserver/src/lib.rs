use axum::{extract::Query, http::StatusCode, routing::get, Router};
use fxhash::FxHashMap;
use std::net::SocketAddr;
use steel::{
    rvals::{Custom, SerializableSteelVal},
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

use crossbeam::channel::{unbounded, Receiver, Sender};

use axum::extract::Path;

#[derive(Copy, Clone, PartialEq, Eq)]
enum RequestType {
    // Post,
    Get,
}

#[derive(Clone, PartialEq, Eq)]
struct Request {
    typ: RequestType,
    path: String,
    body: Option<String>,
    query_parameters: FxHashMap<String, String>,
}

impl Custom for Request {}

impl Request {
    fn get_type(&self) -> isize {
        match self.typ {
            // RequestType::Post => 0,
            RequestType::Get => 1,
        }
    }

    fn get_path(&self) -> String {
        self.path.clone()
    }

    fn get_body(&self) -> Option<String> {
        self.body.clone()
    }

    fn get_query_parameters(&self) -> FFIValue {
        if self.query_parameters.is_empty() {
            FFIValue::BoolV(false)
        } else {
            FFIValue::HashMap(
                self.query_parameters
                    .clone()
                    .into_iter()
                    .map(|(key, value)| {
                        (key.into_ffi_val().unwrap(), value.into_ffi_val().unwrap())
                    })
                    .collect(),
            )
        }
    }
}

#[derive(Clone)]
struct CommandMessenger {
    sender: Sender<Request>,
    receiver: Receiver<RequestResult>,
}

impl Custom for CommandMessenger {
    fn into_serializable_steelval(&mut self) -> Option<steel::rvals::SerializableSteelVal> {
        Some(steel::rvals::SerializableSteelVal::Custom(Box::new(
            self.clone(),
        )))
    }
}

impl CommandMessenger {
    pub fn new(sender: Sender<Request>, receiver: Receiver<RequestResult>) -> Self {
        Self { sender, receiver }
    }

    pub fn send_request(&self, request: Request) {
        // Send the message to the VM context to parse
        match self.sender.send(request) {
            Ok(_) => {}
            Err(e) => {
                println!("Error: {e:?}")
            }
        };
    }
}

#[derive(Clone)]
struct WrappedReceiver {
    receiver: Receiver<Request>,
}

impl WrappedReceiver {
    fn recv(&self) -> Request {
        self.receiver.recv().unwrap()
    }
}

impl Custom for WrappedReceiver {
    fn into_serializable_steelval(&mut self) -> Option<steel::rvals::SerializableSteelVal> {
        Some(steel::rvals::SerializableSteelVal::Custom(Box::new(
            self.clone(),
        )))
    }
}

#[derive(Clone)]
struct WrappedSender {
    sender: Sender<RequestResult>,
}

impl WrappedSender {
    fn send(&self, value: RequestResult) {
        self.sender.send(value).unwrap()
    }
}

impl Custom for WrappedSender {
    fn into_serializable_steelval(&mut self) -> Option<steel::rvals::SerializableSteelVal> {
        Some(steel::rvals::SerializableSteelVal::Custom(Box::new(
            self.clone(),
        )))
    }
}

fn setup_channels() -> Vec<FFIValue> {
    let (command_sender, vm_receiver) = unbounded();
    let (vm_sender, command_receiver) = unbounded();

    let command_messenger = CommandMessenger::new(command_sender, command_receiver);

    vec![
        WrappedSender { sender: vm_sender }.into_ffi_val().unwrap(),
        WrappedReceiver {
            receiver: vm_receiver,
        }
        .into_ffi_val()
        .unwrap(),
        command_messenger.into_ffi_val().unwrap(),
    ]
}

steel::declare_module!(build_module);

#[derive(Clone)]
enum RequestResult {
    Ok(String),
    Err(String),
}

impl Custom for RequestResult {
    fn into_serializable_steelval(&mut self) -> Option<steel::rvals::SerializableSteelVal> {
        Some(SerializableSteelVal::Custom(Box::new(self.clone())))
    }
}

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/steel/webserver");

    module
        .register_fn("start-server!", spawn_server)
        .register_fn("setup-channels", setup_channels)
        .register_fn("receiver/recv", WrappedReceiver::recv)
        .register_fn("sender/send", WrappedSender::send)
        .register_fn("thread/join", WrappedJoinHandler::join)
        .register_fn("request-path", Request::get_path)
        .register_fn("request-body", Request::get_body)
        .register_fn("request-type", Request::get_type)
        .register_fn("request-query-params", Request::get_query_parameters)
        .register_fn("Response-Ok", RequestResult::Ok)
        .register_fn("Response-Err", RequestResult::Err);

    module
}

struct WrappedJoinHandler {
    handle: Option<std::thread::JoinHandle<()>>,
}

impl WrappedJoinHandler {
    fn join(&mut self) {
        self.handle.take().unwrap().join().unwrap()
    }
}

impl Custom for WrappedJoinHandler {}

fn spawn_server(
    command_messenger: CommandMessenger,
    routes: Vec<String>,
    port: usize,
) -> WrappedJoinHandler {
    let handle = std::thread::spawn(move || {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                let mut app = Router::new();

                for route in routes {
                    let messenger = command_messenger.clone();

                    app = app.route(
                        &route,
                        get(
                            |Path(path): Path<String>,
                             Query(params): Query<FxHashMap<String, String>>,
                             body: Option<String>| async move {
                                let request = Request {
                                    typ: RequestType::Get,
                                    path,
                                    body,
                                    query_parameters: params,
                                };

                                messenger.send_request(request);

                                match messenger.receiver.recv() {
                                    Ok(RequestResult::Ok(v)) => Ok(v),
                                    Ok(RequestResult::Err(_)) => {
                                        Err(StatusCode::INTERNAL_SERVER_ERROR)
                                    }
                                    Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
                                }
                            },
                        ),
                    );
                }

                // run it
                let addr = SocketAddr::from(([127, 0, 0, 1], port as u16));
                println!("listening on {addr}");
                axum::Server::bind(&addr)
                    .serve(app.into_make_service())
                    .await
                    .unwrap();
            })
    });

    WrappedJoinHandler {
        handle: Some(handle),
    }
}
