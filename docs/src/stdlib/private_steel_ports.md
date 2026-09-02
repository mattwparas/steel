# #%private/steel/ports
**this module is in the prelude and therefore automatically available when running steel.**

### **call-with-input-file**
Calls the given *proc* with an input port obtained opening *file*.
If *proc* returns, then the temporary port will be closed and the return value of *proc* returned.

(call-with-input-file file proc) -> any/c

- file : string?
- proc : procedure?
### **call-with-input-string**
Calls the given *proc* with an input string port created by opening the given *string* with `open-input-string`.
If *proc* returns, then the return value of *proc* returned.

(call-with-input-string proc) -> any/c

- proc : procedure?
### **call-with-output-file**
Calls the given *proc* with an output port obtained opening *file*.
If *proc* returns, then the temporary port will be closed and the return value of *proc* returned.

(call-with-output-file file proc) -> any/c

- file : string?
- proc : procedure?
### **call-with-output-string**
Calls the given *proc* with an output string port created with `open-output-string`.
If *proc* returns, then the content from the string port will be returned.

(call-with-output-string proc) -> string?

- proc : procedure?
### **call-with-port**
Calls the given *proc* with the *port*.
If *proc* returns, then the port will be closed and the return value of *proc* returned.

(call-with-port port proc) -> any/c

- port : port?
- proc : procedure?
### **peek-byte**
Peeks the next byte from an input port.

(peek-byte [port]) -> byte?

* port : input-port? = (current-input-port)
### **peek-char**
Peeks the next character from an input port.

(peek-char [port]) -> char?

* port : input-port? = (current-input-port)
### **peek-u8**
Alias of `peek-byte`.
### **port->bytes**
Alias of `read-port-to-bytes`.
### **port->string**
Alias of `read-port-to-string`.
### **read-byte**
Reads a single byte from an input port.

(read-byte [port]) -> byte?

* port : input-port? = (current-input-port)
### **read-bytes**
Reads bytes from an input port.

(read-bytes amt [port]) -> bytes?

* amt : (and positive? int?)
* port : input-port? = (current-input-port)
### **read-bytes!**
Reads *end* - *start* bytes from an input port into a given buffer.

(read-bytes! buf [port] [start] [end])

* buf : bytes?
* port : input-port? = (current-input-port)
* start : (and positive? int?) = 0
* end : (and positive? int?) = (bytes-length buf)
### **read-bytes-into-buf**
Reads *amt* bytes from an input port into a given buffer.

(read-bytes-into-buf buf amt [port]) -> int?

* buf : bytes?
* amt : (and positive? int?)
* port : input-port? = (current-input-port)
### **read-bytevector**
Alias of `read-bytes`.
### **read-bytevector!**
Alias of `read-bytes!`.
### **read-char**
Reads the next character from an input port.

(read-char [port]) -> char?

* port : input-port? = (current-input-port)
### **read-line**
Reads a line from an input port.

(read-line [port]) -> string?

* port : input-port? = (current-input-port)
### **read-port-to-bytes**
Reads the entire content of an input port into a byte vector.

(read-port-to-bytes [port]) -> string?

* [port] : input-port? = (current-input-port)
### **read-port-to-string**
Reads the entire content of an input port into a string.

(read-port-to-string [port]) -> string?

* [port] : input-port? = (current-input-port)
### **read-u8**
Alias of `read-byte`.
### **with-input-from-file**
Similar to `call-with-input-file`, but installs the newly opened port as the `current-input-port` instead of passing it as an argument.
If *thunk* returns, then the temporary port will be closed and the return value of *thunk* returned.

(with-input-from-file file proc) > any/c

- file : string?
- thunk : procedure?
### **with-input-from-string**
Similar to `call-with-output-string`, but installs the newly opened port as the `current-input-port` instead of passing it as an argument.
If *thunk* returns, then the return value of *thunk* returned.

(with-input-from-string string thunk) -> any/c

- string : string?
- thunk : procedure?
### **with-output-to-file**
Similar to `call-with-output-file`, but installs the newly opened port as the `current-output-port` instead of passing it as an argument.
If *thunk* returns, then the temporary port will be closed and the return value of *thunk* returned.

(with-output-to-file file proc) > any/c

- file : string?
- thunk : procedure?
### **with-output-to-string**
Similar to `call-with-output-string`, but installs the newly opened port as the `current-input-port` instead of passing it as an argument.
If *thunk* returns, then the content from the string port will be returned.

(with-output-to-string thunk) -> string?

- thunk : procedure?
### **write-byte**
Writes a single byte to an output port.

(write-byte b [port])

* b : byte?
* port : output-port? = (current-output-port)
### **write-bytes**
Writes the contents of a bytevector into an output port.

(write-bytes buf [port])

* buf : bytes?
* port : output-port? = (current-output-port)
### **write-bytevector**
Alias of `write-bytes`.
### **write-u8**
Alias of `write-byte`.
