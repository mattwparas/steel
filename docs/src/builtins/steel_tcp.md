# steel/tcp
### **tcp-accept**
Accept a new incoming connection from this listener.

This function will block the calling thread until a new TCP connection
is established. When established, the corresponding `TcpStream?` will be
returned

#### Examples

```scheme
(define listener (tcp-listen "127.0.0.1:8080"))

(tcp-accept listener) ;; => TcpStream?
```
### **tcp-accept-with-addr**
Accept a new incoming connection from this listener.

This function will block the calling thread until a new TCP connection
is established. When established, the corresponding `TcpStream?` will be
returned with the remote address in a pair

#### Examples

```scheme
(define listener (tcp-listen "127.0.0.1:8080"))

(tcp-accept listener) ;; => (cons TcpStream? string?)
```
### **tcp-connect**
Opens a TCP connection to a remote host.

`addr` is an address of the remote host as a string

If `addr` yields multiple addresses, `tcp-connect` will be attempted with
each of the addresses until a connection is successful. If none of
the addresses result in a successful connection, the error returned from
the last connection attempt (the last address) is returned.

#### Examples

Open a TCP connection to `127.0.0.1:8080`:

```scheme
(tcp-connect "127.0.0.1:8080")
```
### **tcp-listen**
Creates a new `TcpListener?` which will be bound to the specified
address.

The returned listener is ready for accepting connections.

```scheme
(tcp-listen addr) -> TcpListener?
```

Binding with a port number of 0 will request that the OS assigns a port
to this listener. The port allocated can be queried via the
`tcp-listener-local-addr` method.

If `addr` yields multiple addresses, `bind` will be attempted with
each of the addresses until one succeeds and returns the listener. If
none of the addresses succeed in creating a listener, the error returned
from the last attempt (the last address) is returned.

#### Examples

Creates a TCP listener bound to `127.0.0.1:80`:

```scheme
(tcp-listen "127.0.0.1:80")
```
### **tcp-shutdown!**
Shuts down both halves of this tcp connection.

This function will cause all pending and future I/O on the specified
portions to return immediately with either an error or an okay result,
dependong on if it was the write or read half.

#### Platform-specific behavior

Calling this function multiple times may result in different behavior,
depending on the operating system. On Linux, the second call will
return `Ok(())`, but on macOS, it will return an error wrapping the rust error
`ErrorKind::NotConnected`. This may change in the future.

```scheme
(tcp-shutdown! stream) ;; stream : TcpStream?
```
### **tcp-stream-buffered-reader**
Get the reader half of this tcp stream as a buffered reader port.

```scheme
(define my-port (tcp-connect "127.0.0.1:8080"))
(tcp-stream-buffered-reader my-port) ;; => port?
```
### **tcp-stream-reader**
Get the reader half of this tcp stream as a port.

```scheme
(define my-port (tcp-connect "127.0.0.1:8080"))
(tcp-stream-reader my-port) ;; => port?
```
### **tcp-stream-writer**
Get the writer half of this tcp stream as a port.

```scheme
(define my-port (tcp-connect "127.0.0.1:8080"))
(tcp-stream-writer my-port) ;; => port?
```
### **tcp-listener-set-non-blocking!**
### **tcp-stream-set-non-blocking!**
