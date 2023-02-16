use axum::{routing::get, Router};
use std::net::SocketAddr;
use steel::{
    rvals::Custom,
    steel_vm::{engine::Engine, register_fn::RegisterFn},
};

use crossbeam::channel::{unbounded, Receiver, Sender};

// use std::thread;

use axum::extract::Path;

// struct Handler {
//     messenger: Arc<Mutex<CommandMessenger>>,
// }

// impl Handler {
//     pub fn new(messenger: Arc<Mutex<CommandMessenger>>) -> Self {
//         Handler { messenger }
//     }
// }

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

// TODO: More or less, just make the web server the background thread
// have the main loop blocking on requests coming from the server background thread
// And have the objects get passed over the boundary as necessary
//
// Just use message passing as needed via the channels
// #[tokio::main]
fn main() {
    let (command_sender, vm_receiver) = unbounded();
    let (vm_sender, command_receiver) = unbounded();

    let command_messenger = CommandMessenger::new(command_sender, command_receiver);

    // Engine thread where processing occurs for messages that come through
    // let _vm_thread = thread::spawn(move || {
    let mut vm = Engine::new();

    vm.register_fn("start-server", spawn_server);
    vm.register_external_value("command-channel", command_messenger)
        .unwrap();
    // vm.register_external_value("")

    // Warmup script

    vm.register_external_value(
        "vm-receiver",
        WrappedReceiver {
            receiver: vm_receiver,
        },
    )
    .unwrap();
    vm.register_external_value("vm-sender", WrappedSender { sender: vm_sender })
        .unwrap();

    vm.register_fn("receiver/recv", WrappedReceiver::recv)
        .register_fn("sender/send", WrappedSender::send)
        .register_fn("thread/join", WrappedJoinHandler::join);

    //

    vm.compile_and_run_raw_program(include_str!("../server.scm"))
        .unwrap();

    // vm.compile_and_run

    // TODO change this to not just be strings
    // for route in vm_receiver {
    //     // todo!()

    //     // vm.parse_

    //     let result = vm.compile_and_run_raw_program("z");
    //     let parsed = match result {
    //         Ok(v) => v.last().map(|x| x.to_string()),
    //         Err(e) => Some(e.to_string()),
    //     };

    //     vm_sender
    //         .send(parsed.unwrap())
    //         .expect("Unable to send message back from VM");
    // }
    // });

    // todo!()

    // // build our application with a route
    // let app = Router::new().route(
    //     "/*route",
    //     get(|_: Path<String>| async move {
    //         command_messenger.send_request();
    //         command_messenger.receiver.recv().unwrap()
    //     }),
    // );

    // // run it
    // let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    // println!("listening on {}", addr);
    // axum::Server::bind(&addr)
    //     .serve(app.into_make_service())
    //     .await
    //     .unwrap();
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
