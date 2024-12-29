use crate::gc::Gc;
use crate::SteelVal;
use crate::{rvals::Custom, steel_vm::builtin::MarkdownDoc};
use chrono::{Datelike, Local, NaiveDate, NaiveDateTime};
use std::time::Duration;
use std::time::Instant;
use steel_derive::function;

use crate::steel_vm::builtin::BuiltInModule;
use crate::steel_vm::register_fn::RegisterFn;

pub(crate) const TIME_MODULE_DOC: MarkdownDoc<'static> = MarkdownDoc::from_str(
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

// enum TimeZones {
//     Local(Local),
//     Utc(Utc),
//     FixedOffset(FixedOffset),
// }

// pub struct SteelDateTime {
//     datetime: NaiveDateTime,
//     timezone: TimeZones,
// }

impl Custom for NaiveDateTime {}
impl Custom for NaiveDate {}

#[function(name = "naive-current-date-local")]
fn naive_current_date() -> NaiveDate {
    Local::now().date_naive()
}

#[function(name = "naive-date-ymd")]
fn naive_date(year: i32, month: u32, day: u32) -> Option<NaiveDate> {
    NaiveDate::from_ymd_opt(year, month, day)
}

#[function(name = "naive-date-and-hms")]
fn with_time(date: NaiveDate, hour: u32, minute: u32, second: u32) -> Option<NaiveDateTime> {
    date.and_hms_opt(hour, minute, second)
}

#[function(name = "naive-date-year")]
fn date_year(date: NaiveDate) -> i32 {
    date.year()
}

#[function(name = "naive-date-month")]
fn date_month(date: NaiveDate) -> u32 {
    date.month()
}

#[function(name = "naive-date-day")]
fn date_day(date: NaiveDate) -> u32 {
    date.day()
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
        .register_fn("duration->seconds", Duration::as_secs)
        .register_native_fn_definition(DURATION_TO_STRING_DEFINITION)
        .register_native_fn_definition(CURRENT_TIME_FORMATTED_DEFINITION)
        .register_native_fn_definition(SLEEP_MILLIS_DEFINITION)
        .register_native_fn_definition(CURRENT_MILLISECONDS_DEFINITION)
        .register_native_fn_definition(CURRENT_SECONDS_DEFINITION)
        .register_native_fn_definition(CURRENT_INEXACT_MILLISECONDS_DEFINITION)
        .register_native_fn_definition(NAIVE_DATE_DEFINITION)
        .register_native_fn_definition(WITH_TIME_DEFINITION)
        .register_native_fn_definition(DATE_YEAR_DEFINITION)
        .register_native_fn_definition(DATE_MONTH_DEFINITION)
        .register_native_fn_definition(DATE_DAY_DEFINITION)
        .register_native_fn_definition(NAIVE_CURRENT_DATE_DEFINITION);

    module
}

impl Custom for Instant {}
impl Custom for Duration {}
