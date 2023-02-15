use axum::{routing::get, Router};
use std::net::SocketAddr;
use steel::steel_vm::engine::Engine;

use std::sync::{Arc, Mutex};

use crossbeam_channel::{unbounded, Receiver, Sender};

use std::thread;

use axum::extract::Path;

struct Handler {
    messenger: Arc<Mutex<CommandMessenger>>,
}

impl Handler {
    pub fn new(messenger: Arc<Mutex<CommandMessenger>>) -> Self {
        Handler { messenger }
    }
}

#[derive(Clone)]
struct CommandMessenger {
    sender: Sender<()>,
    receiver: Receiver<String>,
}

impl CommandMessenger {
    pub fn new(sender: Sender<()>, receiver: Receiver<String>) -> Self {
        Self { sender, receiver }
    }

    pub fn send_request(&self) {
        // todo!()

        // Send the message to the VM context to parse
        match self.sender.send(()) {
            Ok(_) => {}
            Err(e) => {
                println!("Error: {:?}", e)
            }
        };
        // .expect("Failed to send message to VM");
    }
}

#[tokio::main]
async fn main() {
    let (command_sender, vm_receiver) = unbounded();
    let (vm_sender, command_receiver) = unbounded();

    let command_messenger = CommandMessenger::new(command_sender, command_receiver);

    // Engine thread where processing occurs for messages that come through
    let _vm_thread = thread::spawn(move || {
        let mut vm = Engine::new();

        // Warmup script
        vm.compile_and_run_raw_program(
            "(define x 10) (define y 100) (define z (int->string (+ x y))) z",
        )
        .unwrap();

        // TODO change this to not just be strings
        for () in vm_receiver {
            // todo!()

            // vm.parse_

            let result = vm.compile_and_run_raw_program("z");
            let parsed = match result {
                Ok(v) => v.last().map(|x| x.to_string()),
                Err(e) => Some(e.to_string()),
            };

            vm_sender
                .send(parsed.unwrap())
                .expect("Unable to send message back from VM");
        }
    });

    // build our application with a route
    let app = Router::new().route(
        "/*route",
        get(|_: Path<String>| async move {
            command_messenger.send_request();
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
