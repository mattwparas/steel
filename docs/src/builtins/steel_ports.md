# steel/ports
### **close-input-port**
Close an input port. If the port is a file, the file will be closed.

(close-input-port input-port?) -> void
### **close-output-port**
Close an output port. If the port is a file, the file will be closed.

(close-output-port output-port?) -> void
### **close-port**
Close a port. If the port is a file, the file will be closed.

(close-port port?) -> void
### **eof-object**
Returns an EOF object.

(eof-object) -> eof-object?
### **get-output-bytevector**
Extracts the contents from a port created with `open-output-bytevector`.

(get-output-bytevector port?) -> bytes?
### **get-output-string**
Extracts the string contents from a port created with `open-output-string`.
Bytes written to the port that are not valid UTF-8 read back as the
replacement character.

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
> (open-input-file "foo-bar.txt") ;; => #<input-port:foo-bar.txt>
> (open-input-file "file-does-not-exist.txt")
error[E08]: Io
  ┌─ :1:2
  │
1 │ (open-input-file "foo-bar.txt")
  │  ^^^^^^^^^^^^^^^ No such file or directory (os error 2)
```
### **open-input-output-file**
Takes a filename `path` and returns a port that both reads and writes that file. The
two directions share one position: a write lands where the last read left off, and a
read picks up after the last write. `exists` is as it is on `open-output-file`, except
that it defaults to `'can-update`, which keeps a file that is already there.

(open-input-output-file path [#:exists exists]) -> port?

* path : string?
* exists : symbol? = 'can-update

#### Examples
```scheme
> (open-input-output-file "foo-bar.txt") ;; => #<input-output-port:foo-bar.txt>
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
`exists` says what to do when the file is already there: `'error` raises, `'truncate` drops
what was in it, `'append` writes at the end, `'update` keeps it and positions the port at the
start, and `'can-update` does the same but also creates the file when it is missing.

(open-output-file path [#:exists exists]) -> output-port?

* path : string?
* exists : symbol? = 'error

#### Examples
```scheme
> (open-output-file "foo-bar.txt") ;; => #<output-port:foo-bar.txt>
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
### **port-has-port-position?**
Returns `#t` if a port can report a position.

(port-has-port-position? port) -> bool?

* port : port?
### **port-has-set-port-position!?**
Returns `#t` if a port can be moved to a position.

(port-has-set-port-position!? port) -> bool?

* port : port?
### **port-position**
Returns where the next read or write on a port will land, in bytes from the
start of the stream. Only file ports and string ports have a position, and a
file opened with `#:exists 'append` reports zero until something is written.

(port-position port) -> int?

* port : port?
### **port-sync!**
Flushes a port, and for a file port waits for the os to put the bytes on the
disk.

(port-sync! port) -> void?

* port : output-port?
### **port-truncate!**
Sets the length of the file behind an output port. Bytes past `length` are
dropped, and a `length` past the end reads back as zeros.

(port-truncate! port length) -> void?

* port : output-port?
* length : int?
### **read-line-from-port**
Reads a line from the given port, including the '\n' at the end.

Use of this procedure is discouraged in favor of the (read-line) procedure,
which is included in the scheme spec and therefore more portable.

(read-line-from-port port?) -> string?
### **seek**
Moves a port and returns where it ended up. `whence` says what `offset` counts from:
`SEEK_SET` the start of the stream, `SEEK_CUR` where the port is now, and `SEEK_END`
the end of the stream.

(seek port offset whence) -> int?

* port : port?
* offset : int?
* whence : int?

#### Examples
```scheme
> (seek port 0 SEEK_END) ;; => 4096
> (seek port -16 SEEK_CUR) ;; => 4080
```
### **set-port-position!**
Moves a port to `position` bytes from the start of the stream, or to the end
if given the eof object. A file opened with `#:exists 'append` is written at
the end no matter where its port is positioned.

(set-port-position! port position) -> void?

* port : port?
* position : (or/c int? eof-object?)
### **stdin**
Gets the port handle to stdin

(stdin) -> input-port?

#### Examples

```scheme
> (stdin) ;; => #<input-port:stdin>
```
### **would-block**
Returns a would-block object. This is what the `*` readers return when a
port has nothing ready to read.

(would-block) -> would-block-object?
### **would-block-object?**
Returns `#t` if the value is a would-block object.

(would-block-object? any/c) -> bool?
### **SEEK_CUR**
### **SEEK_END**
### **SEEK_SET**
### **flush-output-port**
### **stdout**
### **write-line!**
