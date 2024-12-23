# steel/threads
### **disconnected-channel-object?**
Returns `#t` if the value is an disconnected-channel object.

(eof-object? any/c) -> bool?
### **empty-channel-object?**
Returns `#t` if the value is an empty-channel object.

(empty-channel-object? any/c) -> bool?
### **get-tls**
Get the value out of the thread local storage slot.
### **lock-acquire!**
Lock the given mutex. Note, this is most likely used as a building block
with the `lock!` function.
### **lock-release!**
Unlock the given mutex.
### **make-tls**
Creates a thread local storage slot. These slots are static, and will _not_ be reclaimed.

When spawning a new thread, the value inside will be shared into that slot, however
future updates to the slot will be local to that thread.
### **mutex**
Construct a new mutex
### **receivers-select**
Blocks until one of the channels passed in is ready to receive.
Returns the index of the channel arguments passed in which is ready.

Using this directly is not recommended.
### **set-tls!**
Set the value in the the thread local storage. Only this thread will see the updates associated
with this TLS.
### **spawn-native-thread**
Spawns the given `func` on another thread. It is required that the arity of the
given function be 0. If the arity of the given function cannot be checked until runtime,
the thread will be spawned and the function will fail to execute.

#### Examples

```scheme
(define thread (spawn-native-thread (lambda () (displayln "Hello world!"))))
```
### **thread-finished?**
Check if the given thread is finished running.
### **thread-interrupt**
Interrupts the thread. Note, this will _not_ interrupt any native code
that is potentially running in the thread, and will attempt to block
at the next bytecode instruction that is running.
### **thread-join!**
Block until this thread finishes.
### **thread-resume**
Resume a suspended thread. This does nothing if the thread is already joined.
### **thread-suspend**
Suspend the thread. Note, this will _not_ interrupt any native code that is
potentially running in the thread, and will attempt to block at the next
bytecode instruction that is running.
### **channel->recv**
### **channel->send**
### **channel->try-recv**
### **channel/recv**
### **channel/send**
### **channel/try-recv**
### **channels-receiver**
### **channels-sender**
### **channels/new**
### **current-thread-id**
### **make-channels**
### **spawn-thread!**
### **thread/available-parallelism**
### **thread::current/id**
