use std::error::Error;
use std::fmt::Display;

use abi_stable::std_types::{RHashMap, RVec};

use steel::rvals::Custom;
use toml::Value;

use steel::declare_module;
use steel::steel_vm::ffi::{FFIModule, FFIValue, RegisterFFIFn};

struct SteelTomlValue(Value);

impl SteelTomlValue {
    fn as_ffi_value(&self) -> FFIValue {
        as_ffi_value(&self.0)
    }
}

impl Custom for SteelTomlValue {}

fn as_ffi_value(value: &Value) -> FFIValue {
    match value {
        Value::String(s) => s.clone().into(),
        Value::Integer(i) => (*i as isize).into(),
        Value::Float(f) => (*f).into(),
        Value::Boolean(b) => (*b).into(),
        Value::Array(a) => a.iter().map(as_ffi_value).collect::<RVec<_>>().into(),
        Value::Table(a) => FFIValue::HashMap(
            a.into_iter()
                .map(|x| (FFIValue::from(x.0.to_owned()), as_ffi_value(x.1)))
                .collect::<RHashMap<FFIValue, FFIValue>>(),
        ),
        // Just send back as a string if it is a datetime
        _ => toml::to_string(value).unwrap().into(),
    }
}

#[derive(Debug)]
struct TomlError(toml::de::Error);

impl Error for TomlError {}
impl Display for TomlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Custom for TomlError {}

fn string_to_toml(string: &str) -> Result<SteelTomlValue, TomlError> {
    toml::from_str(string)
        .map(SteelTomlValue)
        .map_err(TomlError)
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/toml");

    module
        .register_fn("string->toml", string_to_toml)
        .register_fn("toml->value", SteelTomlValue::as_ffi_value);
    module
}
