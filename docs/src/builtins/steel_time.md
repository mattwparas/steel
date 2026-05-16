# steel/time

Contains direct wrappers around the Rust `crate::time::Instant` and `crate::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

### **current-inexact-milliseconds**
Returns the number of milliseconds since the Unix epoch as an inexact number.

(current-inexact-milliseconds) -> inexact?
### **current-milliseconds**
Returns the number of milliseconds since the Unix epoch as an integer.

(current-milliseconds) -> int?
### **current-second**
Returns the number of seconds since the Unix epoch as an integer.

(current-second) -> int?
### **duration->micros**
Returns the total number of whole microseconds contained in the given duration.

(duration->micros dur) -> int?

* dur : duration?
### **duration->millis**
Returns the total number of whole milliseconds contained in the given duration.

(duration->millis dur) -> int?

* dur : duration?
### **duration->nanos**
Returns the total number of nanoseconds contained in the given duration.

(duration->nanos dur) -> int?

* dur : duration?
### **duration->seconds**
Returns the number of whole seconds contained in the given duration.

(duration->seconds dur) -> int?

* dur : duration?
### **duration->string**
Returns a string representation of a duration

(duration->string dur)

* dur : duration?
### **duration-since**
Returns the duration of time that elapsed from `earlier` to `instant`.

(duration-since instant earlier) -> duration?

* instant : instant?
* earlier : instant? - an instant created no later than `instant`
### **instant/elapsed**
Returns the duration that has elapsed since the given instant was created.

(instant/elapsed instant) -> duration?

* instant : instant?
### **instant/now**
Returns the current instant from a monotonic clock. Pair with
`instant/elapsed` or `duration-since` to measure how much time has passed.

(instant/now) -> instant?
### **local-time/now!**
Returns the local time in the format given by the input string (using `chrono::Local::format`).

(local-time/now! fmt) -> string?

* fmt : string?
### **naive-current-date-local**
Returns the current date in the local time zone as a naive date (a date
without any time zone information attached).

(naive-current-date-local) -> naive-date?
### **naive-date-and-hms**
Combines a naive date with an hour, minute, and second to produce a naive
date-time. Returns `#false` if the time values are out of range.

(naive-date-and-hms date hour minute second) -> naive-date-time?

* date : naive-date?
* hour : int? - the hour, from 0 to 23
* minute : int? - the minute, from 0 to 59
* second : int? - the second, from 0 to 59
### **naive-date-day**
Returns the day of the month of the given naive date, from 1 to 31.

(naive-date-day date) -> int?

* date : naive-date?
### **naive-date-month**
Returns the month of the given naive date, from 1 to 12.

(naive-date-month date) -> int?

* date : naive-date?
### **naive-date-year**
Returns the year of the given naive date.

(naive-date-year date) -> int?

* date : naive-date?
### **naive-date-ymd**
Constructs a naive date from a year, month, and day. Returns `#false` if the
given values do not form a valid calendar date.

(naive-date-ymd year month day) -> naive-date?

* year : int?
* month : int? - the month, from 1 to 12
* day : int? - the day of the month, from 1 to 31
### **system-time-duration-since**
Gets the duration between two system times.

(system-time-duration-since time earlier)
### **system-time/now**
Returns the current `SystemTime`.

(system-time/now) -> SystemTime?
### **system-time<=**
Returns `#true` if the first system time is earlier than or equal to the second.

(system-time<= left right) -> bool?

* left : system-time?
* right : system-time?
### **system-time<?**
Returns `#true` if the first system time is strictly earlier than the second.

(system-time<? left right) -> bool?

* left : system-time?
* right : system-time?
### **system-time>=**
Returns `#true` if the first system time is later than or equal to the second.

(system-time>= left right) -> bool?

* left : system-time?
* right : system-time?
### **system-time>?**
Returns `#true` if the first system time is strictly later than the second.

(system-time>? left right) -> bool?

* left : system-time?
* right : system-time?
### **time/sleep-ms**
Sleeps the thread for a given number of milliseconds.

(time/sleep-ms ms)

* ms : int?
