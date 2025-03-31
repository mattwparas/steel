use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::rvals::{RestArgsIter, Result, SteelByteVector, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;
use crate::values::port::{would_block, SteelPort, SteelPortRepr, WOULD_BLOCK_OBJECT};
use crate::values::structs::{make_struct_singleton, StructTypeDescriptor};

use steel_derive::function;

#[cfg(not(feature = "sync"))]
thread_local! {
    static EOF_OBJECT: once_cell::unsync::Lazy<(SteelVal, StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        make_struct_singleton("eof".into())
    });
}

#[cfg(feature = "sync")]
pub static EOF_OBJECT: once_cell::sync::Lazy<(SteelVal, StructTypeDescriptor)> =
    once_cell::sync::Lazy::new(|| make_struct_singleton("eof".into()));

pub fn port_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_native_fn_definition(OPEN_STDIN_DEFINITION)
        .register_native_fn_definition(OPEN_STDOUT_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_FILE_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_FILE_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(WRITE_LINE_DEFINITION)
        .register_native_fn_definition(WRITE_STRING_DEFINITION)
        .register_native_fn_definition(WRITE_DEFINITION)
        .register_native_fn_definition(WRITE_CHAR_DEFINITION)
        .register_native_fn_definition(FLUSH_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_STRING_DEFINITION)
        .register_native_fn_definition(READ_LINE_TO_STRING_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(IS_INPUT_DEFINITION)
        .register_native_fn_definition(IS_OUTPUT_DEFINITION)
        .register_native_fn_definition(DEFAULT_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(DEFAULT_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(CLOSE_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(CLOSE_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(CLOSE_PORT_DEFINITION)
        .register_native_fn_definition(DEFAULT_ERROR_PORT_DEFINITION)
        .register_native_fn_definition(EOF_OBJECT_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_STRING_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(READ_BYTE_DEFINITION)
        .register_native_fn_definition(READ_CHAR_DEFINITION)
        .register_native_fn_definition(WRITE_BYTE_DEFINITION)
        .register_native_fn_definition(WRITE_BYTES_DEFINITION)
        .register_native_fn_definition(PEEK_BYTE_DEFINITION)
        .register_native_fn_definition(READ_BYTES_DEFINITION)
        .register_native_fn_definition(READ_BYTES_INTO_BUF_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECTP_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECT_DEFINITION);
    module
}

pub fn port_module_without_filesystem() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_native_fn_definition(OPEN_STDIN_DEFINITION)
        .register_native_fn_definition(OPEN_STDOUT_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_FILE_SANDBOXED_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_FILE_SANDBOXED_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(WRITE_LINE_DEFINITION)
        .register_native_fn_definition(WRITE_STRING_DEFINITION)
        .register_native_fn_definition(WRITE_DEFINITION)
        .register_native_fn_definition(WRITE_CHAR_DEFINITION)
        .register_native_fn_definition(FLUSH_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_STRING_DEFINITION)
        .register_native_fn_definition(READ_LINE_TO_STRING_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(IS_INPUT_DEFINITION)
        .register_native_fn_definition(IS_OUTPUT_DEFINITION)
        .register_native_fn_definition(DEFAULT_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(DEFAULT_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(CLOSE_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(CLOSE_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(CLOSE_PORT_DEFINITION)
        .register_native_fn_definition(DEFAULT_ERROR_PORT_DEFINITION)
        .register_native_fn_definition(EOF_OBJECT_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_STRING_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(READ_BYTE_DEFINITION)
        .register_native_fn_definition(READ_CHAR_DEFINITION)
        .register_native_fn_definition(WRITE_BYTE_DEFINITION)
        .register_native_fn_definition(WRITE_BYTES_DEFINITION)
        .register_native_fn_definition(PEEK_BYTE_DEFINITION)
        .register_native_fn_definition(READ_BYTES_DEFINITION)
        .register_native_fn_definition(READ_BYTES_INTO_BUF_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECTP_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECT_DEFINITION);
    module
}

// TODO: implement textual-port? and binary-port?

/// Gets the port handle to stdin
///
/// (stdin) -> input-port?
///
/// # Examples
///
/// ```scheme
/// > (stdin) ;; => #<port>
/// ```
#[function(name = "stdin")]
pub fn open_stdin() -> SteelVal {
    SteelVal::PortV(SteelPort {
        port: Gc::new_mut(SteelPortRepr::StdInput(std::io::stdin())),
    })
}

#[function(name = "stdout")]
pub fn open_stdout() -> SteelVal {
    SteelVal::PortV(SteelPort {
        port: Gc::new_mut(SteelPortRepr::StdOutput(std::io::stdout())),
    })
}

/// Takes a filename `path` referring to an existing file and returns an input port. Raises an error
/// if the file does not exist
///
/// (open-input-file string?) -> input-port?
///
/// # Examples
/// ```scheme
/// > (open-input-file "foo-bar.txt") ;; => #<port>
/// > (open-input-file "file-does-not-exist.txt")
/// error[E08]: Io
///   ┌─ :1:2
///   │
/// 1 │ (open-input-file "foo-bar.txt")
///   │  ^^^^^^^^^^^^^^^ No such file or directory (os error 2)
/// ```
#[function(name = "open-input-file")]
pub fn open_input_file(path: &SteelString) -> Result<SteelVal> {
    SteelPort::new_textual_file_input(path).map(SteelVal::PortV)
}

/// Takes a filename `path` referring to a file to be created and returns an output port.
///
/// (open-output-file string?) -> output-port?
///
/// # Examples
/// ```scheme
/// > (open-output-file "foo-bar.txt") ;; => #<port>
/// ```
#[function(name = "open-output-file")]
pub fn open_output_file(path: &SteelString) -> Result<SteelVal> {
    SteelPort::new_textual_file_output(path).map(SteelVal::PortV)
}

/// Takes a filename `path` referring to an existing file and returns an input port. Raises an error
/// if the file does not exist
///
/// (open-input-file string?) -> input-port?
///
/// # Examples
/// ```scheme
/// > (open-input-file "foo-bar.txt") ;; => #<port>
/// > (open-input-file "file-does-not-exist.txt")
/// error[E08]: Io
///   ┌─ :1:2
///   │
/// 1 │ (open-input-file "foo-bar.txt")
///   │  ^^^^^^^^^^^^^^^ No such file or directory (os error 2)
/// ```
#[function(name = "open-input-file")]
pub fn open_input_file_sandboxed(_: &SteelString) -> Result<SteelVal> {
    stop!(Generic => "A sandboxed engine cannot open the file system.")
}

/// Takes a filename `path` referring to a file to be created and returns an output port.
///
/// (open-output-file string?) -> output-port?
///
/// # Examples
/// ```scheme
/// > (open-output-file "foo-bar.txt") ;; => #<port>
/// ```
#[function(name = "open-output-file")]
pub fn open_output_file_sandboxed(_: &SteelString) -> Result<SteelVal> {
    stop!(Generic => "A sandboxed engine cannot open the file system.")
}

/// Creates an output port that accumulates what is written into a string.
/// This string can be recovered calling `get-output-string`.
///
/// (open-output-string) -> output-port?
///
/// # Examples
/// ```scheme
/// (define out (open-output-string))
///
///
/// (write-char "α" out)
/// (write-char "ω" out)
///
/// (get-output-string out) ;; => "αω"
/// ```
#[function(name = "open-output-string")]
pub fn open_output_string() -> SteelVal {
    SteelVal::PortV(SteelPort::new_output_port_string())
}

/// Creates an output port that accumulates what is written into a bytevector.
/// These bytes can be recovered calling `get-output-bytevector`.
///
/// (open-output-bytevector) -> output-port?
///
/// # Examples
/// ```scheme
/// (define out (open-output-bytevector))
///
///
/// (write-byte 30 out)
/// (write-byte 250 out)
///
/// (get-output-bytevector out) ;; => (bytes 30 250)
/// ```
#[function(name = "open-output-bytevector")]
pub fn open_output_bytevector() -> SteelVal {
    SteelVal::PortV(SteelPort::new_output_port_string())
}

/// Creates an input port from a string, that will return the string contents.
///
/// (open-input-string string?) -> input-port?
#[function(name = "open-input-string")]
pub fn open_input_string(s: &SteelString) -> SteelVal {
    SteelVal::PortV(SteelPort::new_input_port_string(s.to_string()))
}

/// Creates an input port from a bytevector, that will return the bytevector contents.
///
/// (open-input-bytevector bytes?) -> input-port?
#[function(name = "open-input-bytevector")]
pub fn open_input_bytevector(bytes: &SteelByteVector) -> SteelVal {
    let vec: &Vec<u8> = &*bytes.vec.read();
    SteelVal::PortV(SteelPort::new_input_port_bytevector(vec.clone()))
}
/// Takes a port and reads the entire content into a string
///
/// (read-port-to-string port) -> string?
///
/// * port : input-port?
#[function(name = "read-port-to-string")]
pub fn read_port_to_string(port: &SteelPort) -> Result<SteelVal> {
    let (_, result) = port.read_all_str()?;
    Ok(SteelVal::StringV(result.into()))
}

/// Checks if a given value is an input port
///
/// (input-port? any/c) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (input-port? (stdin)) ;; => #true
/// > (input-port? "foo") ;; => #false
/// ```
#[function(name = "input-port?")]
pub fn is_input(maybe_port: &SteelVal) -> bool {
    if let SteelVal::PortV(port) = maybe_port {
        port.is_input()
    } else {
        false
    }
}

/// Checks if a given value is an output port
///
/// (output-port? any/c) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (define output (open-output-file "foo.txt"))
/// > (output-port? output) ;; => #true
/// ```
#[function(name = "output-port?")]
pub fn is_output(maybe_port: &SteelVal) -> bool {
    if let SteelVal::PortV(port) = maybe_port {
        port.is_output()
    } else {
        false
    }
}

#[function(name = "read-line-from-port")]
pub fn read_line_to_string(port: &SteelPort) -> Result<SteelVal> {
    let res = port.read_line();

    if let Ok((size, result)) = res {
        if size == 0 {
            Ok(eof())
        } else {
            Ok(SteelVal::StringV(result.into()))
        }
    } else {
        // bit of a hack for now we'll see
        res.map(|_| unreachable!())
    }
}

#[function(name = "write-line!")]
pub fn write_line(port: &SteelPort, line: &SteelVal) -> Result<SteelVal> {
    let line = line.to_string();
    let res = port.write_string_line(line.as_str());

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to file");
    }
}

#[function(name = "#%raw-write")]
pub fn write(line: &SteelVal, rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = output_args(rest)?;
    let line = line.to_string();
    let res = port.write(line.as_str().as_bytes());

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to port");
    }
}

#[function(name = "#%raw-write-char")]
pub fn write_char(character: char, rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = output_args(rest)?;
    let res = port.write_char(character);

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to port");
    }
}

// TODO: support start and end
#[function(name = "#%raw-write-string")]
pub fn write_string(line: &SteelVal, rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = output_args(rest)?;

    let res = if let SteelVal::StringV(s) = line {
        port.write(s.as_str().as_bytes())
    } else {
        port.write(line.to_string().as_str().as_bytes())
    };

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to port");
    }
}

/// Extracts the string contents from a port created with `open-output-string`.
///
/// (get-output-string port?) -> string?
#[function(name = "get-output-string")]
pub fn get_output_string(port: &SteelPort) -> Result<SteelVal> {
    let Some(bytes) = port.get_output()? else {
        stop!(TypeMismatch => "get-output-string expects an output port created with open-output-string");
    };

    let Ok(s) = String::from_utf8(bytes) else {
        stop!(Generic => "Port contents are not valid UTF-8");
    };

    Ok(s.into())
}

/// Extracts the contents from a port created with `open-output-bytevector`.
///
/// (get-output-bytevector port?) -> bytes?
#[function(name = "get-output-bytevector")]
pub fn get_output_bytevector(port: &SteelPort) -> Result<SteelVal> {
    let Some(bytes) = port.get_output()? else {
        stop!(TypeMismatch => "get-output-bytevector expects an output port created with open-output-bytevector");
    };

    Ok(SteelVal::ByteVector(SteelByteVector::new(bytes)))
}

#[function(name = "flush-output-port")]
pub fn flush_output_port(port: &SteelPort) -> Result<SteelVal> {
    port.flush().map(|_| SteelVal::Void)
}

#[function(name = "#%default-input-port")]
pub fn default_input_port() -> SteelVal {
    SteelVal::PortV(SteelPort::default_current_input_port())
}

#[function(name = "#%default-output-port")]
pub fn default_output_port() -> SteelVal {
    SteelVal::PortV(SteelPort::default_current_output_port())
}

#[function(name = "#%default-error-port")]
pub fn default_error_port() -> SteelVal {
    SteelVal::PortV(SteelPort::default_current_error_port())
}

/// Close a port. If the port is a file, the file will be closed.
///
/// (close-port port?) -> void
#[function(name = "close-port")]
pub fn close_port(port: &SteelPort) -> SteelVal {
    port.close_port();
    SteelVal::Void
}

/// Close an output port. If the port is a file, the file will be closed.
///
/// (close-port output-port?) -> void
#[function(name = "close-output-port")]
pub fn close_output_port(port: &SteelPort) -> Result<SteelVal> {
    port.close_output_port().map(|_| SteelVal::Void)
}

/// Close an input port. If the port is a file, the file will be closed.
///
/// (close-port input-port?) -> void
#[function(name = "close-input-port")]
pub fn close_input_port(port: &SteelPort) -> Result<SteelVal> {
    port.close_input_port().map(|_| SteelVal::Void)
}

/// Returns `#t` if the value is an EOF object.
///
/// (eof-object? any/c) -> bool?
#[function(name = "eof-object?")]
pub fn eof_objectp(value: &SteelVal) -> bool {
    let SteelVal::CustomStruct(struct_) = value else {
        return false;
    };

    #[cfg(feature = "sync")]
    {
        struct_.type_descriptor == EOF_OBJECT.1
    }

    #[cfg(not(feature = "sync"))]
    {
        EOF_OBJECT.with(|eof| struct_.type_descriptor == eof.1)
    }
}

/// Returns an EOF object.
///
/// (eof-object) -> eof-object?
#[function(name = "eof-object")]
pub fn eof_object() -> SteelVal {
    eof()
}

#[function(name = "would-block")]
pub fn would_block_object() -> SteelVal {
    would_block()
}

/// Returns `#t` if the value is an EOF object.
///
/// (eof-object? any/c) -> bool?
#[function(name = "would-block-object?")]
pub fn would_block_objectp(value: &SteelVal) -> bool {
    let SteelVal::CustomStruct(struct_) = value else {
        return false;
    };

    #[cfg(feature = "sync")]
    {
        struct_.type_descriptor == WOULD_BLOCK_OBJECT.1
    }

    #[cfg(not(feature = "sync"))]
    {
        WOULD_BLOCK_OBJECT.with(|eof| struct_.type_descriptor == eof.1)
    }
}

/// Reads a single byte from an input port.
///
/// (read-byte [port]) -> byte?
///
/// * port : input-port? = (current-input-port)
#[function(name = "read-byte")]
pub fn read_byte(rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = input_args(rest)?;

    let maybe_byte = port.read_byte()?;

    match maybe_byte {
        crate::values::port::MaybeBlocking::Nonblocking(b) => {
            Ok(b.map(|b| SteelVal::IntV(b.into())).unwrap_or_else(eof))
        }
        crate::values::port::MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }

    // Ok(port
    //     .read_byte()?
    //     .map(|b| SteelVal::IntV(b.into()))
    //     .unwrap_or_else(eof))
}

/// Reads bytes from an input port.
///
/// (read-bytes amt [port]) -> bytes?
///
/// * amt : (and positive? int?)
/// * port : input-port? = (current-input-port)
#[function(name = "read-bytes")]
pub fn read_bytes(amt: usize, rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = input_args(rest)?;

    let bytes = port.read_bytes(amt)?;

    match bytes {
        crate::values::port::MaybeBlocking::Nonblocking(b) => {
            Ok(SteelVal::ByteVector(SteelByteVector::new(b).into()))
        }
        crate::values::port::MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

/// Reads bytes from an input port into a given buffer.
///
/// (read-bytes-into-buf buf amt [port]) -> bytes?
///
/// * buf : bytes?
/// * amt : (and positive? int?)
/// * port : input-port? = (current-input-port)
#[function(name = "read-bytes-into-buf")]
pub fn read_bytes_into_buf(
    buf: &SteelByteVector,
    amt: usize,
    rest: RestArgsIter<&SteelPort>,
) -> Result<SteelVal> {
    let port = input_args(rest)?;

    let mut guard = buf.vec.write();

    if guard.len() < amt {
        stop!(ContractViolation => "read-bytes-into-buf expects a buffer with the capacity to fill the buffer with the specified amount");
    }

    port.read_bytes_into_buf(&mut guard)?;

    Ok(SteelVal::Void)
}

/// Writes a single byte to an output port.
///
/// (write-byte b [port])
///
/// * b : byte?
/// * port : output-port? = (current-output-port)
#[function(name = "write-byte")]
pub fn write_byte(byte: u8, rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = output_args(rest)?;
    port.write(&[byte])?;

    Ok(SteelVal::Void)
}

/// Writes the contents of a bytevector into an output port.
///
/// (write-bytes buf [port])
///
/// * buf : bytes?
/// * port : output-port? = (current-output-port)
#[function(name = "write-bytes")]
pub fn write_bytes(bytes: &SteelByteVector, rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = output_args(rest)?;
    port.write(&*bytes.vec.read())?;

    Ok(SteelVal::Void)
}

/// Peeks the next byte from an input port.
///
/// (peek-byte [port]) -> byte?
///
/// * port : input-port? = (current-input-port)
#[function(name = "peek-byte")]
pub fn peek_byte(rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = input_args(rest)?;

    Ok(port
        .peek_byte()?
        .map(|b| SteelVal::IntV(b.into()))
        .unwrap_or_else(eof))
}

/// Reads the next character from an input port.
///
/// (read-char [port]) -> char?
///
/// * port : input-port? = (current-input-port)
#[function(name = "read-char")]
pub fn read_char(rest: RestArgsIter<&SteelPort>) -> Result<SteelVal> {
    let port = input_args(rest)?;

    let char = port.read_char()?;

    match char {
        crate::values::port::MaybeBlocking::Nonblocking(c) => {
            Ok(c.map(SteelVal::CharV).unwrap_or_else(eof))
        }
        crate::values::port::MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

fn input_args(args: RestArgsIter<&SteelPort>) -> Result<SteelPort> {
    Ok(io_args(1, args)?.unwrap_or_else(SteelPort::default_current_input_port))
}

fn output_args(args: RestArgsIter<&SteelPort>) -> Result<SteelPort> {
    Ok(io_args(2, args)?.unwrap_or_else(SteelPort::default_current_output_port))
}

fn io_args(max: usize, mut args: RestArgsIter<&SteelPort>) -> Result<Option<SteelPort>> {
    let port = args.next().transpose()?.cloned();

    if args.next().is_some() {
        stop!(ArityMismatch => "expected at most {} arguments", max);
    }

    Ok(port)
}

pub fn eof() -> SteelVal {
    #[cfg(feature = "sync")]
    {
        EOF_OBJECT.0.clone()
    }

    #[cfg(not(feature = "sync"))]
    {
        EOF_OBJECT.with(|eof| eof.0.clone())
    }
}
