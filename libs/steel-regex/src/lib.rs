use abi_stable::std_types::{RBoxError, RResult};
use steel::{
    rvals::Custom,
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

use regex::Regex;

struct SteelRegex(Regex);
struct RegexError(regex::Error);

impl Custom for SteelRegex {}
impl Custom for RegexError {}

impl From<regex::Error> for RegexError {
    fn from(value: regex::Error) -> Self {
        Self(value)
    }
}

impl SteelRegex {
    fn new(re: String) -> RResult<FFIValue, RBoxError> {
        match Regex::new(re.as_str()) {
            Ok(v) => SteelRegex(v).into_ffi_val(),
            Err(e) => RResult::RErr(RBoxError::new(e)),
        }

        // Ok(SteelRegex(Regex::new(re.as_str())?))
    }

    // TODO: Add string ref arguments so that these can take &str
    // to the FFI library so that these don't have to be copies.
    fn is_match(&self, re: &str) -> bool {
        self.0.is_match(re)
    }
}

steel::declare_module!(build_module);

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/steel/regex");

    module
        .register_fn("regex", SteelRegex::new)
        .register_fn("regex/match?", SteelRegex::is_match);

    module
}
