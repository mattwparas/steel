# steel/ports
### **eof-object**
Returns an EOF object.

(eof-object) -> eof-object?
### **get-output-bytevector**
Extracts the contents from a port created with `open-output-bytevector`.

(get-output-bytevector port?) -> bytes?
### **get-output-string**
Extracts the string contents from a port created with `open-output-string`.

(get-output-string port?) -> string?
### **input-port?**
Checks if a given value is an input port

(input-port? any/c) -> bool?

#### Examples

```scheme
> (input-port? (stdin)) ;; => #true
> (input-port? "foo") ;; => #false
```
### **open-input-bytevector**
Creates an input port from a bytevector, that will return the bytevector contents.

(open-input-bytevector bytes?) -> input-port?
### **open-input-file**
Takes a filename `path` referring to an existing file and returns an input port. Raises an error
if the file does not exist

(open-input-file string?) -> input-port?

#### Examples
```scheme
> (open-input-file "foo-bar.txt") ;; => #<port>
> (open-input-file "file-does-not-exist.txt")
error[E08]: Io
┌─ :1:2
│
1 │ (open-input-file "foo-bar.txt")
│  ^^^^^^^^^^^^^^^ No such file or directory (os error 2)
```
### **open-input-string**
Creates an input port from a string, that will return the string contents.

(open-input-string string?) -> input-port?
### **open-output-bytevector**
Creates an output port that accumulates what is written into a bytevector.
These bytes can be recovered calling `get-output-bytevector`.

(open-output-bytevector) -> output-port?

#### Examples
```scheme
(define out (open-output-bytevector))


(write-byte 30 out)
(write-byte 250 out)

(get-output-bytevector out) ;; => (bytes 30 250)
```
### **open-output-file**
Takes a filename `path` referring to a file to be created and returns an output port.

(open-output-file string?) -> output-port?

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<port>
```
### **open-output-string**
Creates an output port that accumulates what is written into a string.
This string can be recovered calling `get-output-string`.

(open-output-string) -> output-port?

#### Examples
```scheme
(define out (open-output-string))


(write-char "α" out)
(write-char "ω" out)

(get-output-string out) ;; => "αω"
```
### **output-port?**
Checks if a given value is an output port

(output-port? any/c) -> bool?

#### Examples

```scheme
> (define output (open-output-file "foo.txt"))
> (output-port? output) ;; => #true
```
### **peek-byte**
Peeks the next byte from an input port.

(peek-byte [port]) -> byte?

* port : input-port? = (current-input-port)
### **read-byte**
Reads a single byte from an input port.

(read-byte [port]) -> byte?

* port : input-port? = (current-input-port)
### **read-bytes**
Reads bytes from an input port.

(read-bytes amt [port]) -> bytes?

* amt : (and positive? int?)
* port : input-port? = (current-input-port)
### **read-bytes-into-buf**
Reads bytes from an input port into a given buffer.

(read-bytes-into-buf buf amt [port]) -> bytes?

* buf : bytes?
* amt : (and positive? int?)
* port : input-port? = (current-input-port)
### **read-char**
Reads the next character from an input port.

(read-char [port]) -> char?

* port : input-port? = (current-input-port)
### **read-port-to-string**
Takes a port and reads the entire content into a string

(read-port-to-string port) -> string?

* port : input-port?
### **stdin**
Gets the port handle to stdin

(stdin) -> input-port?

#### Examples

```scheme
> (stdin) ;; => #<port>
```
### **would-block-object?**
Returns `#t` if the value is an EOF object.

(eof-object? any/c) -> bool?
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
### **close-output-port**
### **flush-output-port**
### **read-line-from-port**
### **stdout**
### **would-block**
### **write-line!**
