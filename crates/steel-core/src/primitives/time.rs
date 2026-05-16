use crate::gc::Gc;
use crate::rvals::{as_underlying_type, IntoSteelVal};
use crate::time::Instant;
use crate::time::{Duration, SystemTime};
use crate::SteelVal;
use crate::{rvals::Custom, steel_vm::builtin::MarkdownDoc};
use chrono::{Datelike, Local, NaiveDate, NaiveDateTime};
use steel_derive::function;

use crate::steel_vm::builtin::BuiltInModule;

pub(crate) const TIME_MODULE_DOC: MarkdownDoc<'static> = MarkdownDoc::from_str(
    r#"
Contains direct wrappers around the Rust `crate::time::Instant` and `crate::time::Duration` modules. 
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
impl Custom for SystemTime {
    fn equality_hint(&self, other: &dyn crate::rvals::CustomType) -> bool {
        if let Some(other) = as_underlying_type::<SystemTime>(other) {
            self == other
        } else {
            false
        }
    }
}

/// Returns the current date in the local time zone as a naive date (a date
/// without any time zone information attached).
///
/// (naive-current-date-local) -> naive-date?
#[function(name = "naive-current-date-local")]
fn naive_current_date() -> NaiveDate {
    Local::now().date_naive()
}

/// Constructs a naive date from a year, month, and day. Returns `#false` if the
/// given values do not form a valid calendar date.
///
/// (naive-date-ymd year month day) -> naive-date?
///
/// * year : int?
/// * month : int? - the month, from 1 to 12
/// * day : int? - the day of the month, from 1 to 31
#[function(name = "naive-date-ymd")]
fn naive_date(year: i32, month: u32, day: u32) -> Option<NaiveDate> {
    NaiveDate::from_ymd_opt(year, month, day)
}

/// Combines a naive date with an hour, minute, and second to produce a naive
/// date-time. Returns `#false` if the time values are out of range.
///
/// (naive-date-and-hms date hour minute second) -> naive-date-time?
///
/// * date : naive-date?
/// * hour : int? - the hour, from 0 to 23
/// * minute : int? - the minute, from 0 to 59
/// * second : int? - the second, from 0 to 59
#[function(name = "naive-date-and-hms")]
fn with_time(date: NaiveDate, hour: u32, minute: u32, second: u32) -> Option<NaiveDateTime> {
    date.and_hms_opt(hour, minute, second)
}

/// Returns the year of the given naive date.
///
/// (naive-date-year date) -> int?
///
/// * date : naive-date?
#[function(name = "naive-date-year")]
fn date_year(date: NaiveDate) -> i32 {
    date.year()
}

/// Returns the month of the given naive date, from 1 to 12.
///
/// (naive-date-month date) -> int?
///
/// * date : naive-date?
#[function(name = "naive-date-month")]
fn date_month(date: NaiveDate) -> u32 {
    date.month()
}

/// Returns the day of the month of the given naive date, from 1 to 31.
///
/// (naive-date-day date) -> int?
///
/// * date : naive-date?
#[function(name = "naive-date-day")]
fn date_day(date: NaiveDate) -> u32 {
    date.day()
}

/// Returns `#true` if the first system time is strictly later than the second.
///
/// (system-time>? left right) -> bool?
///
/// * left : system-time?
/// * right : system-time?
#[function(name = "system-time>?")]
fn system_time_gt(left: SystemTime, right: SystemTime) -> bool {
    left > right
}

/// Returns `#true` if the first system time is strictly earlier than the second.
///
/// (system-time<? left right) -> bool?
///
/// * left : system-time?
/// * right : system-time?
#[function(name = "system-time<?")]
fn system_time_lt(left: SystemTime, right: SystemTime) -> bool {
    left < right
}

/// Returns `#true` if the first system time is later than or equal to the second.
///
/// (system-time>= left right) -> bool?
///
/// * left : system-time?
/// * right : system-time?
#[function(name = "system-time>=")]
fn system_time_gte(left: SystemTime, right: SystemTime) -> bool {
    left >= right
}

/// Returns `#true` if the first system time is earlier than or equal to the second.
///
/// (system-time<= left right) -> bool?
///
/// * left : system-time?
/// * right : system-time?
#[function(name = "system-time<=")]
fn system_time_lte(left: SystemTime, right: SystemTime) -> bool {
    left <= right
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
    use crate::time::{SystemTime, UNIX_EPOCH};

    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => {
            let ms = n.as_millis();
            match isize::try_from(ms) {
                Ok(inner) => SteelVal::IntV(inner),
                _ => SteelVal::BigNum(Gc::new(num_bigint::BigInt::from(ms))),
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
    use crate::time::{SystemTime, UNIX_EPOCH};

    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => {
            let ms = n.as_secs();
            match isize::try_from(ms) {
                Ok(inner) => SteelVal::IntV(inner),
                _ => SteelVal::BigNum(Gc::new(num_bigint::BigInt::from(ms))),
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
    use crate::time::{SystemTime, UNIX_EPOCH};

    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => n.as_secs_f64() * 1000.0,
        Err(_) => panic!("SystemTime before UNIX EPOCH!"),
    }
}

/// Returns the current `SystemTime`.
///
/// (system-time/now) -> SystemTime?
#[function(name = "system-time/now")]
fn system_time_now() -> SystemTime {
    SystemTime::now()
}

/// Gets the duration between two system times.
///
/// (system-time-duration-since time earlier)
#[function(name = "system-time-duration-since")]
fn system_time_duration_since(
    left: SystemTime,
    right: SystemTime,
) -> crate::rvals::Result<SteelVal> {
    left.duration_since(right)
        .map(|x| x.into_steelval().unwrap())
        .map_err(|x| crate::throw!(Generic => format!("{:?}", x))())
}

/// Returns the current instant from a monotonic clock. Pair with
/// `instant/elapsed` or `duration-since` to measure how much time has passed.
///
/// (instant/now) -> instant?
#[function(name = "instant/now")]
fn instant_now() -> Instant {
    Instant::now()
}

/// Returns the duration that has elapsed since the given instant was created.
///
/// (instant/elapsed instant) -> duration?
///
/// * instant : instant?
#[function(name = "instant/elapsed")]
fn instant_elapsed(instant: Instant) -> Duration {
    instant.elapsed()
}

/// Returns the duration of time that elapsed from `earlier` to `instant`.
///
/// (duration-since instant earlier) -> duration?
///
/// * instant : instant?
/// * earlier : instant? - an instant created no later than `instant`
#[function(name = "duration-since")]
fn duration_since(instant: Instant, earlier: Instant) -> Duration {
    instant.duration_since(earlier)
}

/// Returns the number of whole seconds contained in the given duration.
///
/// (duration->seconds dur) -> int?
///
/// * dur : duration?
#[function(name = "duration->seconds")]
fn duration_to_seconds(duration: Duration) -> u64 {
    duration.as_secs()
}

/// Returns the total number of whole milliseconds contained in the given duration.
///
/// (duration->millis dur) -> int?
///
/// * dur : duration?
#[function(name = "duration->millis")]
fn duration_to_millis(duration: Duration) -> u128 {
    duration.as_millis()
}

/// Returns the total number of whole microseconds contained in the given duration.
///
/// (duration->micros dur) -> int?
///
/// * dur : duration?
#[function(name = "duration->micros")]
fn duration_to_micros(duration: Duration) -> u128 {
    duration.as_micros()
}

/// Returns the total number of nanoseconds contained in the given duration.
///
/// (duration->nanos dur) -> int?
///
/// * dur : duration?
#[function(name = "duration->nanos")]
fn duration_to_nanos(duration: Duration) -> u128 {
    duration.as_nanos()
}

pub fn time_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/time".to_string());

    module.register_doc("steel/time", TIME_MODULE_DOC);

    module
        .register_native_fn_definition(INSTANT_NOW_DEFINITION)
        .register_native_fn_definition(INSTANT_ELAPSED_DEFINITION)
        .register_native_fn_definition(DURATION_SINCE_DEFINITION)
        .register_native_fn_definition(DURATION_TO_SECONDS_DEFINITION)
        .register_native_fn_definition(DURATION_TO_MILLIS_DEFINITION)
        .register_native_fn_definition(DURATION_TO_MICROS_DEFINITION)
        .register_native_fn_definition(DURATION_TO_NANOS_DEFINITION)
        .register_native_fn_definition(SYSTEM_TIME_DURATION_SINCE_DEFINITION)
        .register_native_fn_definition(SYSTEM_TIME_NOW_DEFINITION)
        .register_native_fn_definition(SYSTEM_TIME_GTE_DEFINITION)
        .register_native_fn_definition(SYSTEM_TIME_GT_DEFINITION)
        .register_native_fn_definition(SYSTEM_TIME_LTE_DEFINITION)
        .register_native_fn_definition(SYSTEM_TIME_LT_DEFINITION)
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
