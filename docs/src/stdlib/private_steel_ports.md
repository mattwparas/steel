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
### **call-with-port**
Calls the given *proc* with the *port*.
If *proc* returns, then the port will be closed and the return value of *proc* returned.

(call-with-port port proc) -> any/c

- port : port?
- proc : procedure?
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
### **call-with-output-string**
