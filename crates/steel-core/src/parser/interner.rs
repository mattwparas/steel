pub use steel_parser::interner::*;

use crate::{rvals::SteelString, SteelVal};

impl From<InternedString> for SteelVal {
    fn from(value: InternedString) -> Self {
        SteelVal::StringV(value.into())
    }
}

impl From<InternedString> for SteelString {
    fn from(value: InternedString) -> Self {
        value.resolve().into()
    }
}
