use crate::rvals::{Result, SteelString, SteelVal};
use crate::steel_vm::builtin::BuiltInModule;
use crate::stop;
use crate::values::port::SteelPort;
use crate::{gc::Gc, values::port::new_rc_ref_cell};

use steel_derive::function;

thread_local! {
    pub static EOF_OBJECT: SteelString = "eof".into();
}

pub fn port_module() -> BuiltInModule {
    let mut module = BuiltInModule::new("steel/ports");
    module
        .register_native_fn_definition(OPEN_STDIN_DEFINITION)
        .register_native_fn_definition(OPEN_INPUT_FILE_DEFINITION)
        .register_native_fn_definition(OPEN_OUTPUT_FILE_DEFINITION)
        .register_native_fn_definition(WRITE_LINE_DEFINITION)
        .register_native_fn_definition(READ_PORT_TO_STRING_DEFINITION)
        .register_native_fn_definition(READ_LINE_TO_STRING_DEFINITION)
        .register_native_fn_definition(OPEN_STDIN_DEFINITION)
        .register_native_fn_definition(IS_INPUT_DEFINITION)
        .register_native_fn_definition(IS_OUTPUT_DEFINITION);
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
    SteelVal::PortV(Gc::new(SteelPort::StdInput(new_rc_ref_cell(
        std::io::stdin(),
    ))))
}

#[function(name = "open-input-file")]
pub fn open_input_file(path: &SteelString) -> Result<SteelVal> {
    let port = SteelPort::new_textual_file_input(path)?;
    Ok(SteelVal::PortV(Gc::new(port)))
}

#[function(name = "open-output-file")]
pub fn open_output_file(path: &SteelString) -> Result<SteelVal> {
    let new_port = SteelPort::new_textual_file_output(path)?;
    Ok(SteelVal::PortV(Gc::new(new_port)))
}

#[function(name = "read-port-to-string")]
pub fn read_port_to_string(port: &Gc<SteelPort>) -> Result<SteelVal> {
    let (_, result) = port.read_all_str()?;
    Ok(SteelVal::StringV(result.into()))
}

#[function(name = "input-port?")]
pub fn is_input(port: &Gc<SteelPort>) -> bool {
    port.is_input()
}

#[function(name = "output-port?")]
pub fn is_output(port: &Gc<SteelPort>) -> bool {
    port.is_output()
}

#[function(name = "read-line-to-string")]
pub fn read_line_to_string(port: &Gc<SteelPort>) -> Result<SteelVal> {
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
pub fn write_line(port: &Gc<SteelPort>, line: &SteelVal) -> Result<SteelVal> {
    let line = line.to_string();
    let res = port.write_string_line(line.as_str());

    if res.is_ok() {
        Ok(SteelVal::Void)
    } else {
        stop!(Generic => "unable to write string to file");
    }
}
