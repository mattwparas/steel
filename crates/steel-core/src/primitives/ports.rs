use std::fs::OpenOptions;

use crate::gc::shared::ShareableMut;
use crate::gc::Gc;
use crate::primitives::lists::plist_get_impl;
use crate::rvals::{FromSteelVal, Result, SteelByteVector, SteelString, SteelVal, SteelValDisplay};
use crate::steel_vm::builtin::BuiltInModule;
use crate::values::port::{
    would_block, MaybeBlocking, Peekable, SteelPort, SteelPortRepr, WOULD_BLOCK_OBJECT,
};
use crate::values::structs::{make_struct_singleton, StructTypeDescriptor};
use crate::{stop, throw};

use steel_derive::{function, native};

#[cfg(not(feature = "sync"))]
thread_local! {
    static EOF_OBJECT: once_cell::unsync::Lazy<(SteelVal, StructTypeDescriptor)>= once_cell::unsync::Lazy::new(|| {
        make_struct_singleton("eof")
    });
}

#[cfg(feature = "sync")]
pub static EOF_OBJECT: once_cell::sync::Lazy<(SteelVal, StructTypeDescriptor)> =
    once_cell::sync::Lazy::new(|| make_struct_singleton("eof"));

pub fn port_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_native_fn_definition(DISPLAY_DEFINITION)
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
        .register_native_fn_definition(READ_PORT_TO_STRING_READY_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_BYTES_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_BYTES_READY_DEFINITION)
        .register_native_fn_definition(READ_LINE_DEFINITION)
        .register_native_fn_definition(READ_LINE_READY_DEFINITION)
        .register_native_fn_definition(READ_LINE_FROM_PORT_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_BYTEVECTOR_DEFINITION)
        .register_native_fn_definition(IS_INPUT_DEFINITION)
        .register_native_fn_definition(IS_STRING_INPUT_DEFINITION)
        .register_native_fn_definition(IS_FILE_INPUT_DEFINITION)
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
        .register_native_fn_definition(PEEK_CHAR_DEFINITION)
        .register_native_fn_definition(WRITE_BYTE_DEFINITION)
        .register_native_fn_definition(WRITE_BYTES_DEFINITION)
        .register_native_fn_definition(PEEK_BYTE_DEFINITION)
        .register_native_fn_definition(READ_BYTES_DEFINITION)
        .register_native_fn_definition(READ_BYTES_INTO_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECTP_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECT_DEFINITION);
    module
}

pub fn port_module_without_filesystem() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_native_fn_definition(DISPLAY_DEFINITION)
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
        .register_native_fn_definition(READ_PORT_TO_STRING_READY_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_BYTES_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_BYTES_READY_DEFINITION)
        .register_native_fn_definition(READ_LINE_DEFINITION)
        .register_native_fn_definition(READ_LINE_READY_DEFINITION)
        .register_native_fn_definition(READ_LINE_FROM_PORT_DEFINITION)
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
        .register_native_fn_definition(PEEK_CHAR_DEFINITION)
        .register_native_fn_definition(WRITE_BYTE_DEFINITION)
        .register_native_fn_definition(WRITE_BYTES_DEFINITION)
        .register_native_fn_definition(PEEK_BYTE_DEFINITION)
        .register_native_fn_definition(READ_BYTES_DEFINITION)
        .register_native_fn_definition(READ_BYTES_INTO_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECTP_DEFINITION)
        .register_native_fn_definition(WOULD_BLOCK_OBJECT_DEFINITION)
        .register_native_fn_definition(IS_STRING_INPUT_DEFINITION)
        .register_native_fn_definition(IS_FILE_INPUT_DEFINITION);
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
/// > (stdin) ;; => #<input-port:stdin>
/// ```
#[function(name = "stdin")]
pub fn open_stdin() -> SteelVal {
    SteelVal::PortV(SteelPort {
        port: Gc::new_lock(SteelPortRepr::StdInput(Peekable::new(std::io::stdin()))),
    })
}

#[function(name = "stdout")]
pub fn open_stdout() -> SteelVal {
    SteelVal::PortV(SteelPort {
        port: Gc::new_lock(SteelPortRepr::StdOutput(std::io::stdout())),
    })
}

/// Takes a filename `path` referring to an existing file and returns an input port. Raises an error
/// if the file does not exist
///
/// (open-input-file string?) -> input-port?
///
/// # Examples
/// ```scheme
/// > (open-input-file "foo-bar.txt") ;; => #<input-port:foo-bar.txt>
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
/// > (open-output-file "foo-bar.txt") ;; => #<output-port:foo-bar.txt>
/// ```
#[native(name = "open-output-file", arity = "AtLeast(1)")]
pub fn open_output_file(args: &[SteelVal]) -> Result<SteelVal> {
    let path = SteelString::from_steelval(&args[0])?;
    let opts = create_open_options(&args[1..])?;
    SteelPort::new_textual_file_output_with_options(&path, opts).map(SteelVal::PortV)
}

pub fn create_open_options(args: &[SteelVal]) -> Result<OpenOptions> {
    #[cfg(feature = "sync")]
    static EXISTS_FLAG: once_cell::sync::Lazy<SteelVal> =
        once_cell::sync::Lazy::new(|| SteelVal::SymbolV("#:exists".into()));

    #[cfg(feature = "sync")]
    let exists_flag = plist_get_impl(args.iter(), &EXISTS_FLAG).ok();

    #[cfg(not(feature = "sync"))]
    thread_local! {
        static EXISTS_FLAG: SteelVal = SteelVal::SymbolV("#:exists".into());
    }

    #[cfg(not(feature = "sync"))]
    let exists_flag = EXISTS_FLAG.with(|x| plist_get_impl(args.iter(), x)).ok();

    let mut options = OpenOptions::new();

    // We want to write the file no matter what in this context
    options.write(true).create(true);

    match exists_flag {
        Some(flag) => {
            let flag = flag.symbol_or_else(
                throw!(Generic => "#:exists flag expects a symbol, found: {}", flag),
            )?;

            match flag {
                "append" => {
                    options.append(true);
                    options.truncate(false)
                }
                "truncate" => options.truncate(true),
                // Default behavior
                "error" => options.create_new(true),
                _ => stop!(Generic => "unexpected option provided to open options"),
            };
        }
        None => {
            options.create_new(true);
        }
    }

    Ok(options)
}

/// Takes a filename `path` referring to an existing file and returns an input port. Raises an error
/// if the file does not exist
///
/// (open-input-file string?) -> input-port?
///
/// # Examples
/// ```scheme
/// > (open-input-file "foo-bar.txt") ;; => #<input-port:foo-bar.txt>
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
/// > (open-output-file "foo-bar.txt") ;; => #<output-port:foo-bar.txt>
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
/// Reads the entire content of an input port into a string.
///
/// (read-port-to-string [port]) -> string?
///
/// * [port] : input-port? = (current-input-port)
#[function(name = "#%read-port-to-string")]
pub fn read_port_to_string(port: &SteelPort) -> Result<SteelVal> {
    let (_, result) = nonblocking(port.read_all_str()?)?;

    Ok(SteelVal::StringV(result.into()))
}

/// Reads the entire content of an input port into a string, without waiting.
///
/// (read-port-to-string* [port]) -> (or string? would-block-object?)
///
/// * [port] : input-port? = (current-input-port)
#[function(name = "#%read-port-to-string*")]
pub fn read_port_to_string_ready(port: &SteelPort) -> Result<SteelVal> {
    match port.read_all_str()? {
        MaybeBlocking::Nonblocking((_, result)) => Ok(SteelVal::StringV(result.into())),
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

/// Reads the entire content of an input port into a byte vector.
///
/// (read-port-to-bytes [port]) -> string?
///
/// * [port] : input-port? = (current-input-port)
#[function(name = "#%read-port-to-bytes")]
pub fn read_port_to_bytes(port: &SteelPort) -> Result<SteelVal> {
    let (_, result) = nonblocking(port.read_all_bytes()?)?;

    Ok(SteelVal::ByteVector(SteelByteVector::new(result)))
}

/// Reads the entire content of an input port into a byte vector, without waiting.
///
/// (read-port-to-bytes* [port]) -> (or bytes? would-block-object?)
///
/// * [port] : input-port? = (current-input-port)
#[function(name = "#%read-port-to-bytes*")]
pub fn read_port_to_bytes_ready(port: &SteelPort) -> Result<SteelVal> {
    match port.read_all_bytes()? {
        MaybeBlocking::Nonblocking((_, result)) => {
            Ok(SteelVal::ByteVector(SteelByteVector::new(result)))
        }
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
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

/// Checks if a given value is a string input port
///
/// (string-input-port? any/c) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (string-input-port? (stdin)) ;; => #false
/// > (string-input-port? (open-input-string "foo")) ;; => #true
/// ```
#[function(name = "#%string-input-port?")]
pub fn is_string_input(maybe_port: &SteelVal) -> bool {
    if let SteelVal::PortV(port) = maybe_port {
        port.is_string_input()
    } else {
        false
    }
}

/// Checks if a given value is a file input port
///
/// (file-input-port? any/c) -> bool?
///
/// # Examples
///
/// ```scheme
/// > (file-input-port? (stdin)) ;; => #false
/// > (file-input-port? (open-input-file "foo.scm")) ;; => #true
/// ```
#[function(name = "#%file-input-port?")]
pub fn is_file_input(maybe_port: &SteelVal) -> bool {
    if let SteelVal::PortV(port) = maybe_port {
        port.is_file_input()
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

fn line(size: usize, mut result: String) -> SteelVal {
    if size == 0 {
        return eof();
    }

    // r7rs read-line doesn't return the trailing \n
    if result.ends_with('\n') {
        result.pop();
    }

    SteelVal::StringV(result.into())
}

/// Reads a line from an input port.
///
/// (read-line [port]) -> string?
///
/// * port : input-port? = (current-input-port)
#[function(name = "#%read-line")]
pub fn read_line(port: &SteelPort) -> Result<SteelVal> {
    let (size, result) = nonblocking(port.read_line()?)?;

    Ok(line(size, result))
}

/// Reads a line from an input port, without waiting.
///
/// (read-line* [port]) -> (or string? would-block-object?)
///
/// * port : input-port? = (current-input-port)
#[function(name = "#%read-line*")]
pub fn read_line_ready(port: &SteelPort) -> Result<SteelVal> {
    match port.read_line()? {
        MaybeBlocking::Nonblocking((size, result)) => Ok(line(size, result)),
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

/// Reads a line from the given port, including the '\n' at the end.
///
/// Use of this procedure is discouraged in favor of the (read-line) procedure,
/// which is included in the scheme spec and therefore more portable.
///
/// (read-line-from-port port?) -> string?
#[function(name = "read-line-from-port")]
pub fn read_line_from_port(port: &SteelPort) -> Result<SteelVal> {
    let (size, result) = nonblocking(port.read_line()?)?;

    if size == 0 {
        Ok(eof())
    } else {
        Ok(SteelVal::StringV(result.into()))
    }
}

#[function(name = "write-line!")]
pub fn write_line(port: &SteelPort, line: &SteelVal) -> Result<SteelVal> {
    let line = line.to_string();
    if let Err(err) = port.write_string_line(line.as_str()) {
        stop!(Generic => "unable to write string to file: {}", err.message());
    }

    Ok(SteelVal::Void)
}

#[function(name = "#%raw-write")]
pub fn write(line: &SteelVal, port: &SteelPort) -> Result<SteelVal> {
    if let Err(err) = port.write(line.to_string().as_bytes()) {
        stop!(Generic => "unable to write string to port: {}", err.message());
    }

    Ok(SteelVal::Void)
}

#[function(name = "#%raw-display")]
pub fn display(line: &SteelVal, port: &SteelPort) -> Result<SteelVal> {
    let line = SteelValDisplay(line).to_string();
    if let Err(err) = port.write(line.as_bytes()) {
        stop!(Generic => "unable to write string to port: {}", err.message());
    }

    Ok(SteelVal::Void)
}

#[function(name = "#%raw-write-char")]
pub fn write_char(character: char, port: &SteelPort) -> Result<SteelVal> {
    if let Err(err) = port.write_char(character) {
        stop!(Generic => "unable to write string to port: {}", err.message());
    }

    Ok(SteelVal::Void)
}

// TODO: support start and end
#[function(name = "#%raw-write-string")]
pub fn write_string(line: &SteelVal, port: &SteelPort) -> Result<SteelVal> {
    let res = if let SteelVal::StringV(s) = line {
        port.write(s.as_bytes())
    } else {
        port.write(line.to_string().as_bytes())
    };

    if let Err(err) = res {
        stop!(Generic => "unable to write string to port: {}", err.message());
    }

    Ok(SteelVal::Void)
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
    SteelVal::PortV(SteelPort::default_input_port())
}

#[function(name = "#%default-output-port")]
pub fn default_output_port() -> SteelVal {
    SteelVal::PortV(SteelPort::default_output_port())
}

#[function(name = "#%default-error-port")]
pub fn default_error_port() -> SteelVal {
    SteelVal::PortV(SteelPort::default_error_port())
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
/// (close-output-port output-port?) -> void
#[function(name = "close-output-port")]
pub fn close_output_port(port: &SteelPort) -> Result<SteelVal> {
    port.close_output_port().map(|_| SteelVal::Void)
}

/// Close an input port. If the port is a file, the file will be closed.
///
/// (close-input-port input-port?) -> void
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

pub fn eof_objectp_jit(value: SteelVal) -> Result<SteelVal> {
    Ok(SteelVal::BoolV(eof_objectp(&value)))
}

/// Returns an EOF object.
///
/// (eof-object) -> eof-object?
#[function(name = "eof-object")]
pub fn eof_object() -> SteelVal {
    eof()
}

/// Returns a would-block object, which a read reports when it has nothing
/// ready and the port would otherwise have to wait.
///
/// (would-block) -> would-block-object?
#[function(name = "would-block")]
pub fn would_block_object() -> SteelVal {
    would_block()
}

/// Returns `#t` if the value is a would-block object.
///
/// (would-block-object? any/c) -> bool?
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
        WOULD_BLOCK_OBJECT.with(|would_block| struct_.type_descriptor == would_block.1)
    }
}

// Every waiting read raises this, so callers have one message to match
fn nonblocking<T>(value: MaybeBlocking<T>) -> Result<T> {
    match value {
        MaybeBlocking::Nonblocking(value) => Ok(value),
        MaybeBlocking::WouldBlock => {
            stop!(Io => "the port has nothing ready and this read would block")
        }
    }
}

/// Reads a single byte from an input port.
///
/// (read-byte [port]) -> byte?
///
/// * port : input-port? = (current-input-port)
#[function(name = "#%read-byte")]
pub fn read_byte(port: &SteelPort) -> Result<SteelVal> {
    match port.read_byte()? {
        MaybeBlocking::Nonblocking(b) => {
            Ok(b.map(|b| SteelVal::IntV(b.into())).unwrap_or_else(eof))
        }
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

/// Reads bytes from an input port.
///
/// (read-bytes amt [port]) -> bytes?
///
/// * amt : (and positive? int?)
/// * port : input-port? = (current-input-port)
#[function(name = "#%read-bytes")]
pub fn read_bytes(amt: usize, port: &SteelPort) -> Result<SteelVal> {
    let bytes = port.read_bytes(amt)?;

    match bytes {
        MaybeBlocking::Nonblocking(b) => {
            if b.is_empty() {
                Ok(eof_object())
            } else {
                Ok(SteelVal::ByteVector(SteelByteVector::new(b)))
            }
        }
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

#[function(name = "#%read-bytes!")]
pub fn read_bytes_into(
    buf: &SteelByteVector,
    port: &SteelPort,
    start: usize,
    end: usize,
) -> Result<SteelVal> {
    let mut guard = buf.vec.write();
    let Some(slice) = guard.get_mut(start..end) else {
        stop!(ContractViolation => "start and end indices are not valid for the provided buffer");
    };

    let res = port.read_bytes_into_buf(slice)?;
    match res {
        MaybeBlocking::Nonblocking(0) => Ok(eof_object()),
        MaybeBlocking::Nonblocking(amt) => Ok(SteelVal::IntV(amt as _)),
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

/// Writes a single byte to an output port.
///
/// (write-byte b [port])
///
/// * b : byte?
/// * port : output-port? = (current-output-port)
#[function(name = "#%write-byte")]
pub fn write_byte(byte: u8, port: &SteelPort) -> Result<SteelVal> {
    port.write(&[byte])?;

    Ok(SteelVal::Void)
}

/// Writes the contents of a bytevector into an output port.
///
/// (write-bytes buf [port])
///
/// * buf : bytes?
/// * port : output-port? = (current-output-port)
#[function(name = "#%write-bytes")]
pub fn write_bytes(bytes: &SteelByteVector, port: &SteelPort) -> Result<SteelVal> {
    port.write(&*bytes.vec.read())?;

    Ok(SteelVal::Void)
}

/// Peeks the next byte from an input port.
///
/// (peek-byte [port]) -> byte?
///
/// * port : input-port? = (current-input-port)
#[function(name = "#%peek-byte")]
pub fn peek_byte(port: &SteelPort) -> Result<SteelVal> {
    match port.peek_byte()? {
        MaybeBlocking::Nonblocking(b) => {
            Ok(b.map(|b| SteelVal::IntV(b.into())).unwrap_or_else(eof))
        }
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

/// Reads the next character from an input port.
///
/// (read-char [port]) -> char?
///
/// * port : input-port? = (current-input-port)
#[function(name = "#%read-char")]
pub fn read_char(port: &SteelPort) -> Result<SteelVal> {
    match port.read_char()? {
        MaybeBlocking::Nonblocking(c) => Ok(c.map(SteelVal::CharV).unwrap_or_else(eof)),
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
}

pub fn read_char_single(port: SteelVal) -> Result<SteelVal> {
    if let SteelVal::PortV(port) = port {
        match port.read_char()? {
            MaybeBlocking::Nonblocking(c) => Ok(c.map(SteelVal::CharV).unwrap_or_else(eof)),
            MaybeBlocking::WouldBlock => Ok(would_block_object()),
        }
    } else {
        stop!(TypeMismatch => "...")
    }
}

// Can we assume there isn't a check here? Just skip the port itself?
pub fn read_char_single_ref(port: &SteelVal) -> Result<SteelVal> {
    if let SteelVal::PortV(port) = port {
        match port.read_char()? {
            MaybeBlocking::Nonblocking(c) => Ok(c.map(SteelVal::CharV).unwrap_or_else(eof)),
            MaybeBlocking::WouldBlock => Ok(would_block_object()),
        }
    } else {
        stop!(TypeMismatch => "...")
    }
}

/// Peeks the next character from an input port.
///
/// (peek-char [port]) -> char?
///
/// * port : input-port? = (current-input-port)
#[function(name = "#%peek-char")]
pub fn peek_char(port: &SteelPort) -> Result<SteelVal> {
    match port.peek_char()? {
        MaybeBlocking::Nonblocking(c) => Ok(c.map(SteelVal::CharV).unwrap_or_else(eof)),
        MaybeBlocking::WouldBlock => Ok(would_block_object()),
    }
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

#[cfg(test)]
mod would_block_tests {
    use super::*;
    use crate::gc::Gc;
    use crate::rvals::SteelByteVector;
    use std::io::{BufReader, Read, Write};
    use std::net::{TcpListener, TcpStream};

    struct NothingReady;

    impl Read for NothingReady {
        fn read(&mut self, _: &mut [u8]) -> std::io::Result<usize> {
            Err(std::io::Error::new(
                std::io::ErrorKind::WouldBlock,
                "nothing ready",
            ))
        }
    }

    // What a socket with a read timeout reports on windows, where the deadline is
    // not the same error as a read that would block
    struct RanOutOfTime;

    impl Read for RanOutOfTime {
        fn read(&mut self, _: &mut [u8]) -> std::io::Result<usize> {
            Err(std::io::Error::new(
                std::io::ErrorKind::TimedOut,
                "timed out",
            ))
        }
    }

    fn port_over(reader: Box<dyn Read + Send + Sync>) -> SteelPort {
        SteelPort {
            port: Gc::new_lock(SteelPortRepr::DynReader(Peekable::new(BufReader::new(
                reader,
            )))),
        }
    }

    fn blocked_port() -> SteelPort {
        port_over(Box::new(NothingReady))
    }

    fn timed_out_port() -> SteelPort {
        port_over(Box::new(RanOutOfTime))
    }

    // The client end comes back so the connection stays open, not closed
    fn blocked_tcp_port() -> (SteelPort, TcpStream) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let client = TcpStream::connect(listener.local_addr().unwrap()).unwrap();
        let (server, _) = listener.accept().unwrap();
        server.set_nonblocking(true).unwrap();

        let port = SteelPort {
            port: Gc::new_lock(SteelPortRepr::TcpStream(Peekable::new(server))),
        };

        (port, client)
    }

    // The same, except that the socket waits for a deadline rather than refusing
    // to wait at all
    fn timed_out_tcp_port() -> (SteelPort, TcpStream) {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let client = TcpStream::connect(listener.local_addr().unwrap()).unwrap();
        let (server, _) = listener.accept().unwrap();
        server
            .set_read_timeout(Some(std::time::Duration::from_millis(50)))
            .unwrap();

        let port = SteelPort {
            port: Gc::new_lock(SteelPortRepr::TcpStream(Peekable::new(server))),
        };

        (port, client)
    }

    #[test]
    fn every_read_reports_a_blocked_port_the_same_way() {
        let buf = SteelByteVector::new(vec![0; 4]);

        let reads: Vec<(&str, SteelVal)> = vec![
            ("read-byte", read_byte(&blocked_port()).unwrap()),
            ("read-bytes", read_bytes(4, &blocked_port()).unwrap()),
            (
                "read-bytes!",
                read_bytes_into(&buf, &blocked_port(), 0, 4).unwrap(),
            ),
            ("read-line*", read_line_ready(&blocked_port()).unwrap()),
            (
                "read-port-to-string*",
                read_port_to_string_ready(&blocked_port()).unwrap(),
            ),
            (
                "read-port-to-bytes*",
                read_port_to_bytes_ready(&blocked_port()).unwrap(),
            ),
        ];

        for (name, value) in reads {
            assert!(
                would_block_objectp(&value),
                "{name} reported a blocked port as {value}"
            );
            assert!(
                !eof_objectp(&value),
                "{name} reported a blocked port as eof"
            );
        }
    }

    #[test]
    fn every_waiting_read_raises_the_same_thing() {
        let raises: Vec<(&str, String)> = vec![
            ("read-line", read_line(&blocked_port()).unwrap_err()),
            (
                "read-line-from-port",
                read_line_from_port(&blocked_port()).unwrap_err(),
            ),
            (
                "read-port-to-string",
                read_port_to_string(&blocked_port()).unwrap_err(),
            ),
            (
                "read-port-to-bytes",
                read_port_to_bytes(&blocked_port()).unwrap_err(),
            ),
        ]
        .into_iter()
        .map(|(name, err)| (name, err.message().to_string()))
        .collect();

        for (name, message) in &raises {
            assert_eq!(
                message, &raises[0].1,
                "{name} describes a blocked port differently from {}",
                raises[0].0
            );
        }
    }

    #[test]
    fn a_waiting_read_raises_the_same_thing_whatever_backs_the_port() {
        let (tcp, _client) = blocked_tcp_port();

        assert_eq!(
            read_line(&tcp).unwrap_err().message(),
            read_line(&blocked_port()).unwrap_err().message(),
            "read-line describes a blocked socket differently depending on the reader"
        );
    }

    #[test]
    fn a_read_that_runs_out_of_time_reports_the_same_whichever_error_the_os_gives() {
        assert!(
            would_block_objectp(&read_line_ready(&timed_out_port()).unwrap()),
            "read-line* reported a timed out read as something other than would-block"
        );

        assert_eq!(
            read_line(&timed_out_port()).unwrap_err().message(),
            read_line(&blocked_port()).unwrap_err().message(),
            "read-line describes a timed out read differently from a would-block"
        );

        let reads: Vec<(&str, SteelVal)> = vec![
            ("read-byte", read_byte(&timed_out_port()).unwrap()),
            ("read-bytes", read_bytes(4, &timed_out_port()).unwrap()),
            (
                "read-port-to-string*",
                read_port_to_string_ready(&timed_out_port()).unwrap(),
            ),
            (
                "read-port-to-bytes*",
                read_port_to_bytes_ready(&timed_out_port()).unwrap(),
            ),
        ];

        for (name, value) in reads {
            assert!(
                would_block_objectp(&value),
                "{name} reported a timed out read as {value}"
            );
        }
    }

    #[test]
    fn a_read_that_runs_out_of_time_reports_what_a_non_blocking_one_does() {
        let (timed_out, _client) = timed_out_tcp_port();

        assert!(
            would_block_objectp(&read_line_ready(&timed_out).unwrap()),
            "read-line* reported a read timeout as something other than would-block"
        );

        let (timed_out, _client) = timed_out_tcp_port();
        let (non_blocking, _other) = blocked_tcp_port();

        assert_eq!(
            read_line(&timed_out).unwrap_err().message(),
            read_line(&non_blocking).unwrap_err().message(),
            "read-line describes a read timeout differently from a would-block"
        );
    }

    #[test]
    fn a_read_that_runs_out_of_time_keeps_what_already_arrived() {
        let (timed_out, mut client) = timed_out_tcp_port();

        client.write_all(b"ab").unwrap();
        client.flush().unwrap();

        let value = read_bytes(4, &timed_out).unwrap();

        assert_eq!(
            value.to_string(),
            SteelVal::ByteVector(SteelByteVector::new(b"ab".to_vec())).to_string(),
            "a read that timed out after two of the four bytes gave back {value}"
        );
    }
}
