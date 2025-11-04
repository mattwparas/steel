# steel/meta
### **command-line**
Returns the command line passed to this process,
including the command name as first argument.
### **error-object-message**
Returns the message of an error object.

(error-object-message error?) -> string?
### **make-weak-box**
Allocates a weak box.

A weak box is similar to a box, but when the garbage collector can prove
that the value of a weak box is only reachable through weak references,
the weak box value will always return #false.

In other words, a weak box does not keep the value contained alive through
a gc collection.
### **weak-box-value**
Returns the value contained in the weak box.
If the garbage collector has proven that the previous content
value of weak-box was reachable only through a weak reference,
then default-value (which defaults to #f) is returned.

```scheme
(define value (make-weak-box 10))
(weak-box-value value) ;; => 10
(set! value #f) ;; Wipe out the previous value
(#%gc-collect)
(weak-box-value value) ;; => #false
```
### **%#interner-memory-usage**
### **%iterator?**
### **Engine::add-module**
### **Engine::clone**
### **Engine::modules->list**
### **Engine::new**
### **Engine::raise_error**
### **active-object-count**
### **arity-object->list**
### **arity?**
### **assert!**
### **attach-contract-struct!**
### **block-on**
### **box**
### **box-strong**
### **breakpoint!**
### **call-with-current-continuation**
### **call-with-exception-handler**
### **call/cc**
### **callstack-hydrate-names**
### **current-function-span**
### **current-os!**
### **dump-profiler**
### **emit-expanded**
### **env-var**
### **error-with-span**
### **eval**
### **eval!**
### **eval-string**
### **expand!**
### **feature-dylib-build?**
### **function-arity**
### **function-name**
### **futures-join-all**
### **get-contract-struct**
### **get-test-mode**
### **inspect**
### **iter-next!**
### **join!**
### **load**
### **load-expanded**
### **local-executor/block-on**
### **make-callstack-profiler**
### **make-struct-type**
### **make-will-executor**
### **maybe-get-env-var**
### **memory-address**
### **multi-arity?**
### **path-separator**
### **platform-dll-extension!**
### **platform-dll-prefix!**
### **poll!**
### **raise-error**
### **raise-error-with-span**
### **read!**
### **run!**
### **set-box!**
### **set-env-var!**
### **set-strong-box!**
### **set-test-mode!**
### **steel-home-location**
### **struct->list**
### **target-arch!**
### **unbox**
### **unbox-strong**
### **value->iterator**
### **value->string**
### **will-execute**
### **will-register**
