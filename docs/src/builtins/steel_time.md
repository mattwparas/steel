# steel/time

Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
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
### **duration->string**
Returns a string representation of a duration

(duration->string dur)

* dur : duration?
### **local-time/now!**
Returns the local time in the format given by the input string (using `chrono::Local::format`).

(local-time/now! fmt) -> string?

* fmt : string?
### **time/sleep-ms**
Sleeps the thread for a given number of milliseconds.

(time/sleep-ms ms)

* ms : int?
### **duration->seconds**
### **duration-since**
### **instant/elapsed**
### **instant/now**
### **naive-current-date-local**
### **naive-date-and-hms**
### **naive-date-day**
### **naive-date-month**
### **naive-date-year**
### **naive-date-ymd**
### **system-time<=**
### **system-time<?**
### **system-time>=**
### **system-time>?**
