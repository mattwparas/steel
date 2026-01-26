use std::{
    sync::{atomic::AtomicBool, Arc},
    time::Duration,
};

use crate::steel_vm::{engine::Engine, ThreadStateController};

pub struct InterruptHandler {
    controller: ThreadStateController,
    running: Arc<AtomicBool>,
    handle: std::thread::JoinHandle<()>,
    done: crossbeam_channel::Sender<()>,
}

impl InterruptHandler {
    pub fn new(engine: &mut Engine, timeout: Duration) -> Self {
        let controller = engine.get_thread_state_controller();
        let running = Arc::new(AtomicBool::new(false));

        let controller_clone = controller.clone();
        let running_clone = running.clone();

        let (sender, receiver) = crossbeam_channel::bounded::<()>(16);

        let thread_handle = std::thread::spawn(move || {
            let controller = controller_clone;
            let running = running_clone;

            // TODO: Don't just want an infinite loop, also figure out a way to
            // kill this thread.
            loop {
                std::thread::park();

                while running.load(std::sync::atomic::Ordering::Relaxed) {
                    // Either, sleep or receive event. Whichever comes first.

                    match receiver.recv_timeout(timeout) {
                        Ok(_) => break,
                        Err(_) => {
                            controller.interrupt();

                            while receiver.try_recv().is_ok() {
                                // Drain the messages
                                // for the next batch
                            }
                            break;
                        }
                    }
                }
            }
        });

        Self {
            controller,
            running,
            handle: thread_handle,
            done: sender,
        }
    }

    pub fn run_with_timeout<F, R>(&self, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let handler = self;
        handler
            .running
            .store(true, std::sync::atomic::Ordering::Relaxed);

        handler.handle.thread().unpark();

        let res = (f)();

        self.done.send(()).unwrap();

        handler.controller.resume();
        handler
            .running
            .store(false, std::sync::atomic::Ordering::Relaxed);

        res
    }
}

#[test]
fn check_engine_timeout_infinite_loop() {
    let mut engine = Engine::new();

    let interrupt_handler = InterruptHandler::new(&mut engine, Duration::from_millis(500));

    let res = interrupt_handler.run_with_timeout(|| {
        engine.run(
            r#"

(define (loop)
    (loop))
(loop)
            "#,
        )
    });

    assert!(res.is_err());
}

#[test]
fn check_engine_does_not_timeout() {
    let mut engine = Engine::new();

    let interrupt_handler = InterruptHandler::new(&mut engine, Duration::from_millis(500));

    let res = interrupt_handler.run_with_timeout(|| {
        engine.run(
            r#"
            (list 10 20 30 40)
            "#,
        )
    });

    assert!(res.is_ok());
}
