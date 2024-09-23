use polling::{Event, Events, Poller};
use std::{cell::RefCell, net::TcpListener, sync::atomic::AtomicUsize};

use crate::{
    rvals::{AsRefSteelVal, Custom, IntoSteelVal, Result},
    steel_vm::builtin::BuiltInModule,
    SteelVal,
};

impl Custom for Poller {}
impl Custom for Events {}

static KEY_ID: AtomicUsize = AtomicUsize::new(0);

#[steel_derive::function(name = "fresh-event-id")]
fn fresh_key() -> Result<SteelVal> {
    KEY_ID
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        .into_steelval()
}

#[steel_derive::function(name = "make-poller")]
pub fn new_poller() -> Result<SteelVal> {
    Poller::new()?.into_steelval()
}

#[steel_derive::function(name = "add-event-interest-read")]
pub fn add_read(poller: &SteelVal, socket: &SteelVal, key: usize) -> Result<SteelVal> {
    unsafe {
        Poller::as_ref(poller)?.add(&*(TcpListener::as_ref(socket)?), Event::readable(key))?;
        Ok(SteelVal::Void)
    }
}

#[steel_derive::function(name = "add-event-interest-write")]
pub fn add_write(poller: &SteelVal, socket: &SteelVal, key: usize) -> Result<SteelVal> {
    unsafe {
        Poller::as_ref(poller)?.add(&*(TcpListener::as_ref(socket)?), Event::writable(key))?;
        Ok(SteelVal::Void)
    }
}

#[steel_derive::function(name = "add-event-interest-all")]
pub fn add_all(poller: &SteelVal, socket: &SteelVal, key: usize) -> Result<SteelVal> {
    unsafe {
        Poller::as_ref(poller)?.add(&*(TcpListener::as_ref(socket)?), Event::all(key))?;
        Ok(SteelVal::Void)
    }
}

// Probably, just need to have a thread local events?
thread_local! {
    pub static EVENTS: RefCell<Events> = RefCell::new(Events::new());
}

#[steel_derive::function(name = "events-clear!")]
pub fn clear_events() {
    EVENTS.with(|x| x.borrow_mut().clear());
}

#[steel_derive::function(name = "poller-wait")]
pub fn poller_wait(poller: &SteelVal) -> Result<SteelVal> {
    let poller = Poller::as_ref(poller)?;
    EVENTS.with(|x| poller.wait(&mut x.borrow_mut(), None))?;
    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "events->list")]
pub fn events() -> Result<SteelVal> {
    Ok(SteelVal::ListV(EVENTS.with(|x| {
        x.borrow()
            .iter()
            .map(|x| SteelVal::IntV(x.key as _))
            .collect::<crate::values::lists::List<_>>()
    })))
}

#[steel_derive::function(name = "modify-event-interest-read!")]
pub fn modify_read(poller: &SteelVal, socket: &SteelVal, key: usize) -> Result<SteelVal> {
    Poller::as_ref(poller)?.modify(&*(TcpListener::as_ref(socket)?), Event::readable(key))?;
    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "modify-event-interest-write!")]
pub fn modify_write(poller: &SteelVal, socket: &SteelVal, key: usize) -> Result<SteelVal> {
    Poller::as_ref(poller)?.modify(&*(TcpListener::as_ref(socket)?), Event::writable(key))?;
    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "modify-event-interest-all!")]
pub fn modify_all(poller: &SteelVal, socket: &SteelVal, key: usize) -> Result<SteelVal> {
    Poller::as_ref(poller)?.modify(&*(TcpListener::as_ref(socket)?), Event::all(key))?;
    Ok(SteelVal::Void)
}

#[steel_derive::function(name = "poller-delete!")]
pub fn delete_interest(poller: &SteelVal, socket: &SteelVal) -> Result<SteelVal> {
    Poller::as_ref(poller)?.delete(&*(TcpListener::as_ref(socket)?))?;
    Ok(SteelVal::Void)
}

pub fn polling_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/polling".to_string());

    module
        .register_native_fn_definition(FRESH_KEY_DEFINITION)
        .register_native_fn_definition(NEW_POLLER_DEFINITION)
        .register_native_fn_definition(ADD_READ_DEFINITION)
        .register_native_fn_definition(ADD_WRITE_DEFINITION)
        .register_native_fn_definition(ADD_ALL_DEFINITION)
        .register_native_fn_definition(CLEAR_EVENTS_DEFINITION)
        .register_native_fn_definition(POLLER_WAIT_DEFINITION)
        .register_native_fn_definition(EVENTS_DEFINITION)
        .register_native_fn_definition(CLEAR_EVENTS_DEFINITION)
        .register_native_fn_definition(MODIFY_READ_DEFINITION)
        .register_native_fn_definition(MODIFY_WRITE_DEFINITION)
        .register_native_fn_definition(MODIFY_ALL_DEFINITION)
        .register_native_fn_definition(DELETE_INTEREST_DEFINITION);

    module
}

// unsafe fn test() -> std::result::Result<(), Box<dyn Error>> {
//     // Create a TCP listener.
//     let socket = TcpListener::bind("127.0.0.1:8000")?;
//     socket.set_nonblocking(true)?;
//     let key = 7; // Arbitrary key identifying the socket.

//     // Create a poller and register interest in readability on the socket.
//     let poller = Poller::new()?;
//     poller.add(&socket, Event::readable(key))?;

//     // The event loop.
//     let mut events = Events::new();
//     loop {
//         // Wait for at least one I/O event.
//         events.clear();
//         poller.wait(&mut events, None)?;

//         for ev in events.iter() {
//             // Map the key to the event properly
//             if ev.key == key {
//                 // Perform a non-blocking accept operation.
//                 socket.accept()?;
//                 // Set interest in the next readability event.
//                 poller.modify(&socket, Event::readable(key))?;
//             }
//         }
//     }
// }
