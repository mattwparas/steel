#![allow(unused)]

use axum::{extract::Query, http::StatusCode, routing::get, Json, Router};
use std::{collections::HashMap, net::SocketAddr};
use steel::{
    rvals::Custom,
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
    SteelVal,
};

use crossbeam::channel::{unbounded, Receiver, Sender};

use axum::extract::Path;

thread_local! {
    static POST: SteelVal = SteelVal::SymbolV("POST".into());
    static GET: SteelVal = SteelVal::SymbolV("GET".into());
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum RequestType {
    Post,
    Get,
}

#[derive(Clone, PartialEq, Eq)]
struct Request {
    typ: RequestType,
    path: String,
    body: Option<serde_json::Value>,
}

impl Custom for Request {}

impl Request {
    fn get_type(&self) -> SteelVal {
        match self.typ {
            RequestType::Post => POST.with(|x| x.clone()),
            RequestType::Get => GET.with(|x| x.clone()),
        }
    }

    fn get_path(&self) -> String {
        self.path.clone()
    }

    fn get_body(&self) -> Option<serde_json::Value> {
        self.body.clone()
    }
}

#[derive(Clone)]
struct CommandMessenger {
    sender: Sender<Request>,
    receiver: Receiver<Result<String, String>>,
}

impl Custom for CommandMessenger {}

impl CommandMessenger {
    pub fn new(sender: Sender<Request>, receiver: Receiver<Result<String, String>>) -> Self {
        Self { sender, receiver }
    }

    pub fn send_request(&self, request: Request) {
        // todo!()

        // Send the message to the VM context to parse
        match self.sender.send(request) {
            Ok(_) => {}
            Err(e) => {
                println!("Error: {e:?}")
            }
        };
        // .expect("Failed to send message to VM");
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

impl Custom for WrappedReceiver {}

#[derive(Clone)]
struct WrappedSender {
    sender: Sender<Result<String, String>>,
}

impl WrappedSender {
    fn send(&self, value: Result<String, String>) {
        self.sender.send(value).unwrap()
    }
}

impl Custom for WrappedSender {}

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

// thread_local! {
//     static MODULE: Rc<BuiltInModule> = create_module();
// }

// #[no_mangle]
// fn create_module() -> Box<BuiltInModule> {
//     let mut module = BuiltInModule::new("dylib/steel/webserver".to_string());

//     // module.register_fn("toml->value", SteelTomlValue::as_value);

//     module
//         .register_fn("start-server!", spawn_server)
//         .register_fn("setup-channels", setup_channels)
//         .register_fn("receiver/recv", WrappedReceiver::recv)
//         .register_fn("sender/send", WrappedSender::send)
//         .register_fn("thread/join", WrappedJoinHandler::join)
//         .register_fn("request-type", Request::get_type)
//         .register_fn("request-path", Request::get_path)
//         .register_fn("request-body", Request::get_body)
//         .register_value("request-type/POST", POST.with(|x| x.clone()))
//         .register_value("request-type/GET", GET.with(|x| x.clone()));

//     // module.register_type::<SteelTomlValue>("toml?");

//     // module.register_

//     // module.register_value("outside-value", SteelVal::StringV("Hello world!".into()));
//     // module.register_fn("hidden-function", hidden_function);

//     // Rc::new(module)
//     Box::new(module)
// }

steel::declare_module!(build_module);

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/steel/webserver");

    module
        .register_fn("start-server!", spawn_server)
        .register_fn("setup-channels", setup_channels)
        .register_fn("receiver/recv", WrappedReceiver::recv)
        // .register_fn("sender/send", WrappedSender::send)
        .register_fn("thread/join", WrappedJoinHandler::join)
        // .register_fn("request-type", Request::get_type)
        .register_fn("request-path", Request::get_path)
        // .register_fn("request-body", Request::get_body)
    ;

    //     .register_value("request-type/POST", POST.with(|x| x.clone()))
    //     .register_value("request-type/GET", GET.with(|x| x.clone()));

    module
}

struct WrappedJoinHandler {
    handle: Option<std::thread::JoinHandle<()>>,
}

impl WrappedJoinHandler {
    fn join(&mut self) {
        self.handle.take().unwrap().join().unwrap()

        // self.handle.join().unwrap()
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
                // let (command_sender, vm_receiver) = unbounded();
                // let (vm_sender, command_receiver) = unbounded();

                // let command_messenger = CommandMessenger::new(command_sender, command_receiver);

                // let post_messenger = command_messenger.clone();

                let mut app = Router::new();

                for route in routes {
                    let messenger = command_messenger.clone();

                    app = app.route(
                        &route,
                        get(
                            |Path(path): Path<String>,
                             Path(_params): Path<std::collections::HashMap<String, String>>,
                             Query(_query): Query<HashMap<String, String>>,
                             _json: Option<Json<serde_json::Value>>| async move {
                                // println!("Receiving request");

                                // println!("path parameters: {:?}", params);
                                // println!("Query parameters: {:?}", query);
                                // println!("Body: {:?}", json);

                                let request = Request {
                                    typ: RequestType::Get,
                                    path,
                                    body: None,
                                };

                                messenger.send_request(request);

                                match messenger.receiver.recv() {
                                    Ok(Ok(v)) => Ok(v),
                                    Ok(Err(_)) => Err(StatusCode::INTERNAL_SERVER_ERROR),
                                    Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
                                }
                                // messenger.receiver.recv().unwrap()
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

// async fn handler() -> String {
//     let mut engine = Engine::new();
//     let results = engine
//         .compile_and_run_raw_program(
//             "(define x 10) (define y 100) (define z (int->string (+ x y))) z",
//         )
//         .unwrap();
//     // println!("{:?}", results);
//     "Hello world".to_string()
// }
