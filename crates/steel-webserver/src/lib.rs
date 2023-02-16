use axum::{routing::get, Router};
use std::net::SocketAddr;
use steel::{
    rvals::{Custom, IntoSteelVal},
    steel_vm::{builtin::BuiltInModule, register_fn::RegisterFn},
    SteelVal,
};

use crossbeam::channel::{unbounded, Receiver, Sender};

use axum::extract::Path;

#[derive(Clone)]
struct CommandMessenger {
    sender: Sender<String>,
    receiver: Receiver<String>,
}

impl Custom for CommandMessenger {}

impl CommandMessenger {
    pub fn new(sender: Sender<String>, receiver: Receiver<String>) -> Self {
        Self { sender, receiver }
    }

    pub fn send_request(&self, path: String) {
        // todo!()

        // Send the message to the VM context to parse
        match self.sender.send(path) {
            Ok(_) => {}
            Err(e) => {
                println!("Error: {:?}", e)
            }
        };
        // .expect("Failed to send message to VM");
    }
}

#[derive(Clone)]
struct WrappedReceiver {
    receiver: Receiver<String>,
}

impl WrappedReceiver {
    fn recv(&self) -> String {
        self.receiver.recv().unwrap()
    }
}

impl Custom for WrappedReceiver {}

#[derive(Clone)]
struct WrappedSender {
    sender: Sender<String>,
}

impl WrappedSender {
    fn send(&self, value: String) {
        self.sender.send(value).unwrap()
    }
}

impl Custom for WrappedSender {}

fn setup_channels() -> Vec<SteelVal> {
    let (command_sender, vm_receiver) = unbounded();
    let (vm_sender, command_receiver) = unbounded();

    let command_messenger = CommandMessenger::new(command_sender, command_receiver);

    vec![
        WrappedSender { sender: vm_sender }.into_steelval().unwrap(),
        WrappedReceiver {
            receiver: vm_receiver,
        }
        .into_steelval()
        .unwrap(),
        command_messenger.into_steelval().unwrap(),
    ]
}

#[no_mangle]
fn generate_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("dylib/steel/webserver".to_string());

    // module.register_fn("toml->value", SteelTomlValue::as_value);

    module
        .register_fn("start-server!", spawn_server)
        .register_fn("setup-channels", setup_channels)
        .register_fn("receiver/recv", WrappedReceiver::recv)
        .register_fn("sender/send", WrappedSender::send)
        .register_fn("thread/join", WrappedJoinHandler::join);

    // module.register_type::<SteelTomlValue>("toml?");

    // module.register_

    // module.register_value("outside-value", SteelVal::StringV("Hello world!".into()));
    // module.register_fn("hidden-function", hidden_function);

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

fn spawn_server(command_messenger: CommandMessenger) -> WrappedJoinHandler {
    let handle = std::thread::spawn(|| {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap()
            .block_on(async {
                // let (command_sender, vm_receiver) = unbounded();
                // let (vm_sender, command_receiver) = unbounded();

                // let command_messenger = CommandMessenger::new(command_sender, command_receiver);

                // build our application with a route
                let app = Router::new().route(
                    "/*route",
                    get(|Path(path): Path<String>| async move {
                        // println!("Receiving request");
                        command_messenger.send_request(path);
                        command_messenger.receiver.recv().unwrap()
                    }),
                );

                // run it
                let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
                println!("listening on {}", addr);
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
