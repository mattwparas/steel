use crate::gc::Gc;
use crate::rvals::{Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;
use crate::values::port::new_rc_ref_cell;
use crate::values::port::{SteelPort, SteelPortRepr};

use steel_derive::function;

thread_local! {
    pub static EOF_OBJECT: SteelString = "eof".into();
}

pub fn port_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_native_fn_definition(OPEN_STDIN_DEFINITION)
        .register_native_fn_definition(OPEN_STDOUT_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_FILE_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_FILE_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(WRITE_LINE_DEFINITION)
        .register_native_fn_definition(WRITE_STRING_DEFINITION)
        .register_native_fn_definition(WRITE_DEFINITION)
        .register_native_fn_definition(WRITE_CHAR_DEFINITION)
        .register_native_fn_definition(FLUSH_OUTPUT_PORT_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_STRING_DEFINITION)
        .register_native_fn_definition(READ_LINE_TO_STRING_DEFINITION)
        .register_native_fn_definition(GET_OUTPUT_STRING_DEFINITION)
        .register_native_fn_definition(IS_INPUT_DEFINITION)
        .register_native_fn_definition(IS_OUTPUT_DEFINITION)
        .register_native_fn_definition(DEFAULT_INPUT_PORT_DEFINITION)
        .register_native_fn_definition(DEFAULT_OUTPUT_PORT_DEFINITION);
    module
}

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

#[function(name = "open-output-string")]
pub fn open_output_string() -> SteelVal {
    SteelVal::PortV(SteelPort::new_output_port())
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
            Ok(SteelVal::SymbolV(EOF_OBJECT.with(|x| x.clone())))
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

#[function(name = "raw-write")]
pub fn write(port: &SteelPort, line: &SteelVal) -> Result<SteelVal> {
    let line = line.to_string();
    let res = port.write_string(line.as_str());

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to port");
    }
}

#[function(name = "raw-write-char")]
pub fn write_char(port: &SteelPort, character: char) -> Result<SteelVal> {
    let res = port.write_char(character);

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to port");
    }
}

#[function(name = "raw-write-string")]
pub fn write_string(port: &SteelPort, line: &SteelVal) -> Result<SteelVal> {
    let res = if let SteelVal::StringV(s) = line {
        port.write_string(s.as_str())
    } else {
        port.write_string(line.to_string().as_str())
    };

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to port");
    }
}

#[function(name = "get-output-string")]
pub fn get_output_string(port: &SteelPort) -> Result<SteelVal> {
    port.get_output_string().map(SteelVal::from)
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

// TODO: In order for this to work, ports have to get refactored - the mutability needs to be
// on the outside, rather than the inside.
#[function(name = "close-output-port")]
pub fn close_output_port(port: &SteelPort) -> Result<SteelVal> {
    port.close_output_port().map(|_| SteelVal::Void)
}
