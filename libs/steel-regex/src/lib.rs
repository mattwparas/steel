use abi_stable::std_types::{RBoxError, RResult, RString, RVec};
use steel::{
    rvals::Custom,
    steel_vm::ffi::{FFIModule, FFIValue, IntoFFIVal, RegisterFFIFn},
};

use regex::Regex;
use std::error::Error;

struct SteelRegex(Regex);

#[allow(unused)]
#[derive(Debug)]
struct RegexError(regex::Error);

impl Custom for SteelRegex {}
impl Custom for RegexError {}

impl Error for RegexError {}
impl std::fmt::Display for RegexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<regex::Error> for RegexError {
    fn from(value: regex::Error) -> Self {
        Self(value)
    }
}

impl SteelRegex {
    fn new(re: String) -> RResult<FFIValue, RBoxError> {
        match Regex::new(re.as_str()) {
            Ok(v) => SteelRegex(v).into_ffi_val(),
            Err(e) => RResult::RErr(RBoxError::new(RegexError(e))),
        }
    }

    fn is_match(&self, haystack: &str) -> bool {
        self.0.is_match(haystack)
    }

    // Use this to find the matches themselves.
    // Unfortunately, it will clone the value since we don't
    // have a great way of producing a slice of the input
    // _back_ to the caller
    fn find(&self, haystack: &str) -> Option<FFIValue> {
        let matches = self.0.find(haystack)?;
        Some(FFIValue::StringV(RString::from(matches.as_str())))
    }

    fn find_all(&self, haystack: &str) -> Option<FFIValue> {
        let matches = self.0.find_iter(haystack);
        let mut values = RVec::new();
        for m in matches {
            values.push(FFIValue::StringV(RString::from(m.as_str())));
        }
        Some(FFIValue::Vector(values))
    }
}

steel::declare_module!(build_module);

pub fn build_module() -> FFIModule {
    let mut module = FFIModule::new("dylib/steel/regex");

    module
        .register_fn("regex", SteelRegex::new)
        .register_fn("regex/match?", SteelRegex::is_match)
        .register_fn("regex/find", SteelRegex::find)
        .register_fn("regex/find*", SteelRegex::find_all);

    module
}
