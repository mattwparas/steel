# steel/time
### **steel/time**


#### steel/time
    
Contains direct wrappers around the Rust `std::time::Instant` and `std::time::Duration` modules. 
For example, to measure the time something takes:

```scheme
(define t (instant/now))
(displayln "Hello world")
(displayln (instant/elapsed t))
```

