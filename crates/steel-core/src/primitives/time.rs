use crate::gc::Gc;
use crate::SteelVal;
use crate::{rvals::Custom, steel_vm::builtin::MarkdownDoc};
use chrono::Local;
use std::time::Duration;
use std::time::Instant;
use steel_derive::function;

use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

pub(crate) const TIME_MODULE_DOC: MarkdownDoc<'static> = MarkdownDoc(
    r#"
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

"#,
);

/// Returns a string representation of a duration
///
/// (duration->string dur)
///
/// * dur : duration?
#[function(name = "duration->string")]
fn duration_to_string(duration: Duration) -> String {
    format!("{duration:?}")
}

/// Returns the local time in the format given by the input string (using `chrono::Local::format`).
///
/// (local-time/now! fmt) -> string?
///
/// * fmt : string?
#[function(name = "local-time/now!")]
fn current_time_formatted(format_string: String) -> String {
    Local::now().format(&format_string).to_string()
}

/// Sleeps the thread for a given number of milliseconds.
///
/// (time/sleep-ms ms)
///
/// * ms : int?
#[function(name = "time/sleep-ms")]
fn sleep_millis(millis: usize) {
    std::thread::sleep(Duration::from_millis(millis.try_into().unwrap()))
}

/// Returns the number of milliseconds since the Unix epoch as an integer.
///
/// (current-milliseconds) -> int?
#[function(name = "current-milliseconds")]
fn current_milliseconds() -> SteelVal {
    use std::time::{SystemTime, UNIX_EPOCH};

    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => {
            let ms = n.as_millis();
            match isize::try_from(ms) {
                Ok(inner) => SteelVal::IntV(inner),
                _ => SteelVal::BigNum(Gc::new(num::BigInt::from(ms))),
            }
        }
        Err(_) => panic!("SystemTime before UNIX EPOCH!"),
    }
}

/// Returns the number of seconds since the Unix epoch as an integer.
///
/// (current-second) -> int?
#[function(name = "current-second")]
fn current_seconds() -> SteelVal {
    use std::time::{SystemTime, UNIX_EPOCH};

    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => {
            let ms = n.as_secs();
            match isize::try_from(ms) {
                Ok(inner) => SteelVal::IntV(inner),
                _ => SteelVal::BigNum(Gc::new(num::BigInt::from(ms))),
            }
        }
        Err(_) => panic!("SystemTime before UNIX EPOCH!"),
    }
}

/// Returns the number of milliseconds since the Unix epoch as an inexact number.
///
/// (current-inexact-milliseconds) -> inexact?
#[function(name = "current-inexact-milliseconds")]
fn current_inexact_milliseconds() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};

    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => n.as_secs_f64() * 1000.0,
        Err(_) => panic!("SystemTime before UNIX EPOCH!"),
    }
}

pub fn time_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/time".to_string());

    module.register_doc("steel/time", TIME_MODULE_DOC);

    module
        .register_fn("instant/now", Instant::now)
        .register_fn("instant/elapsed", Instant::elapsed)
        .register_fn("duration-since", Instant::duration_since)
        .register_fn("duration->string", duration_to_string)
        .register_fn("duration->seconds", Duration::as_secs)
        .register_native_fn_definition(CURRENT_TIME_FORMATTED_DEFINITION)
        .register_native_fn_definition(SLEEP_MILLIS_DEFINITION)
        .register_native_fn_definition(CURRENT_MILLISECONDS_DEFINITION)
        .register_native_fn_definition(CURRENT_SECONDS_DEFINITION)
        .register_native_fn_definition(CURRENT_INEXACT_MILLISECONDS_DEFINITION);

    module
}

impl Custom for Instant {}
impl Custom for Duration {}
