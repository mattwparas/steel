# steel/time


#### steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

### **current-inexact-milliseconds**
### **current-milliseconds**
### **current-second**
### **duration->seconds**
### **duration->string**
### **duration-since**
### **instant/elapsed**
### **instant/now**
### **local-time/now!**
### **time/sleep-ms**
