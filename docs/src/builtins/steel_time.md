# steel/time


#### steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

### **local-time/now!**
### **time/sleep-ms**
### **duration->seconds**
### **duration->string**
### **current-milliseconds**
### **duration-since**
### **instant/elapsed**
### **current-second**
### **instant/now**
### **current-inexact-milliseconds**
