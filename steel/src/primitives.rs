mod contracts;
mod control;
mod fs;
mod hashmaps;
mod hashsets;
mod io;
pub mod lists;
mod meta_ops;
mod nums;
mod ports;
mod streams;
mod strings;
mod symbols;
mod transducers;
mod utils;
mod vectors;

pub use contracts::ContractOperations;
pub use control::ControlOperations;
pub use fs::FsFunctions;
pub use hashmaps::HashMapOperations;
pub use hashsets::HashSetOperations;
pub use io::IoFunctions;
pub use meta_ops::MetaOperations;
pub use nums::NumOperations;
pub use ports::PortOperations;
pub use streams::StreamOperations;
pub use strings::StringOperations;
pub use symbols::SymbolOperations;
pub use transducers::TransducerOperations;
pub use vectors::VectorOperations;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::{
    rvals::{create_result_ok_struct, FunctionSignature, SteelVal},
    stop,
};
use im_rc::Vector;

use std::convert::TryFrom;
use std::result;

use crate::rvals::{FromSteelVal, IntoSteelVal};

use crate::gc::Gc;

macro_rules! try_from_impl {
    ($type:ident => $($body:ty),*) => {
        $(
            impl TryFrom<SteelVal> for $body {
                type Error = SteelErr;
                fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::new(ErrorKind::ConversionError, "Expected number".to_string())),
                    }
                }
            }

            impl TryFrom<&SteelVal> for $body {
                type Error = SteelErr;
                fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::new(ErrorKind::ConversionError, "Expected number".to_string())),
                    }
                }
            }

            impl FromSteelVal for $body {
                fn from_steelval(value: &SteelVal) -> result::Result<Self, SteelErr> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::new(ErrorKind::ConversionError, "Expected number".to_string())),
                    }
                }
            }

        )*
    };
}

macro_rules! from_f64 {
    ($($body:ty),*) => {
        $(
            impl From<$body> for SteelVal {
                fn from(val: $body) -> SteelVal {
                    SteelVal::NumV(val as f64)
                }
            }

            impl IntoSteelVal for $body {
                fn into_steelval(self) -> Result<SteelVal, SteelErr> {
                    Ok(SteelVal::NumV(self as f64))
                }
            }
        )*
    };
}

macro_rules! from_for_isize {
    ($($body:ty),*) => {
        $(
            impl From<$body> for SteelVal {
                fn from(val: $body) -> SteelVal {
                    SteelVal::IntV(val as isize)
                }
            }

            impl IntoSteelVal for $body {
                fn into_steelval(self) -> Result<SteelVal, SteelErr> {
                    Ok(SteelVal::IntV(self as isize))
                }
            }
        )*
    };
}

impl From<char> for SteelVal {
    fn from(val: char) -> SteelVal {
        SteelVal::CharV(val)
    }
}

impl IntoSteelVal for char {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::CharV(self))
    }
}

impl FromSteelVal for char {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if let SteelVal::CharV(c) = val {
            Ok(*c)
        } else {
            Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected character".to_string(),
            ))
        }
    }
}

impl<T: Into<SteelVal>> From<Option<T>> for SteelVal {
    fn from(val: Option<T>) -> SteelVal {
        if let Some(s) = val {
            s.into()
        } else {
            SteelVal::BoolV(true)
        }
    }
}

impl<T: IntoSteelVal> IntoSteelVal for Option<T> {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        if let Some(s) = self {
            s.into_steelval()
        } else {
            Ok(SteelVal::BoolV(false))
        }
    }
}

impl<T: FromSteelVal> FromSteelVal for Option<T> {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if val.is_truthy() {
            Ok(Some(T::from_steelval(val)?))
        } else {
            Ok(None)
        }
    }
}

impl FromSteelVal for SteelVal {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        Ok(val.clone())
    }
}

// TODO make intosteelval return a result type
// This allows errors to propagate
// @Matt - TODO: 4/29/22 -> Decide how natively Result and Option types should be integrated
// Directly into Steel. At the moment we _could_ bail out and just rely entirely on Rust
// types, but then we may have to more carefully integrate with functions in the std library
// in order to interact. We also could convert directly into a Steel representation, i.e.
// (make-struct Ok (x))
// (make-struct Err (x))
// This could make it easier to integrate natively, but also opaquely wrapping it allows for
// perhaps better performance
// impl<T: IntoSteelVal, E: IntoSteelVal> Custom for Result<T, E> {}

impl<T: IntoSteelVal, E: std::fmt::Debug> IntoSteelVal for Result<T, E> {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        match self {
            Ok(s) => Ok(create_result_ok_struct(s.into_steelval()?)),
            Err(e) => crate::stop!(Generic => format!("{:?}", e)),
        }
    }
}

impl<T: FromSteelVal, E: FromSteelVal> FromSteelVal for Result<T, E> {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if val.is_struct() {
            if let SteelVal::MutableVector(v) = val {
                let lock = v.borrow();
                let name = lock.get(1);
                let inner = lock.get(2);

                if let Some(SteelVal::SymbolV(name)) = name {
                    match name.as_ref() {
                        "Ok" => Ok(Ok(T::from_steelval(inner.unwrap())?)),
                        "Err" => Ok(Err(E::from_steelval(inner.unwrap())?)),
                        _ => {
                            stop!(ConversionError => format!("Failed converting an instance of a steel struct into a Rust result type: found an instance of a struct with the name: {:?}, expecting either `Ok` or `Err`", name))
                        }
                    }
                } else {
                    stop!(ConversionError => format!("Failed attempting to convert an instance of a steelval into a result type, found an instance of a struct without a name - expected a name and found: {:?}", name))
                }
            } else {
                unreachable!()
            }
        } else {
            stop!(ConversionError => format!("Failed attempting to convert an instance of a steelval into a result type: {:?}", val));
        }
    }
}

impl FromSteelVal for () {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        if let SteelVal::Void = val {
            Ok(())
        } else {
            crate::stop!(ConversionError => "could not convert value to unit type")
        }
    }
}

impl IntoSteelVal for () {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::Void)
    }
}

impl From<()> for SteelVal {
    fn from(_: ()) -> SteelVal {
        SteelVal::Void
    }
}

from_f64!(f64, f32);
from_for_isize!(i32, i16, i8, u8, u16, u32, u64, usize, isize);
try_from_impl!(NumV => f64, f32);
try_from_impl!(IntV => i64, i32, i16, i8, u8, u16, u32, u64, usize, isize);

impl TryFrom<SteelVal> for String {
    type Error = SteelErr;
    fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(ref x) => Ok(x.to_string()),
            SteelVal::SymbolV(ref x) => Ok(x.to_string()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected string".to_string(),
            )),
        }
    }
}

impl From<SteelVal> for Gc<SteelVal> {
    fn from(val: SteelVal) -> Self {
        Gc::new(val)
    }
}

impl From<Gc<SteelVal>> for SteelVal {
    fn from(val: Gc<SteelVal>) -> Self {
        (*val).clone()
    }
}

impl FromSteelVal for String {
    fn from_steelval(val: &SteelVal) -> Result<Self, SteelErr> {
        match val {
            SteelVal::StringV(s) | SteelVal::SymbolV(s) => Ok(s.to_string()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                format!("Expected string, found: {}", val),
            )),
        }
    }
}

impl TryFrom<&SteelVal> for String {
    type Error = SteelErr;
    fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(x) => Ok(x.to_string()),
            SteelVal::SymbolV(x) => Ok(x.to_string()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected string".to_string(),
            )),
        }
    }
}

impl From<String> for SteelVal {
    fn from(val: String) -> SteelVal {
        SteelVal::StringV(val.into())
    }
}

impl IntoSteelVal for String {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::StringV(self.into()))
    }
}

impl From<String> for Gc<SteelVal> {
    fn from(val: String) -> Gc<SteelVal> {
        Gc::new(val.into())
    }
}

impl From<bool> for SteelVal {
    fn from(val: bool) -> SteelVal {
        SteelVal::BoolV(val)
    }
}

impl IntoSteelVal for bool {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::BoolV(self))
    }
}

impl From<Vector<SteelVal>> for SteelVal {
    fn from(val: Vector<SteelVal>) -> SteelVal {
        SteelVal::VectorV(Gc::new(val))
    }
}

impl From<FunctionSignature> for SteelVal {
    fn from(val: FunctionSignature) -> SteelVal {
        SteelVal::FuncV(val)
    }
}

#[cfg(test)]
mod try_from_tests {

    use super::*;

    #[test]
    fn from_char() {
        assert_eq!(SteelVal::from('c'), SteelVal::CharV('c'));
    }

    #[test]
    fn from_steelval_char() {
        assert_eq!(char::from_steelval(&SteelVal::CharV('c')).unwrap(), 'c')
    }

    #[test]
    fn into_steelval_char() {
        assert_eq!('c'.into_steelval().unwrap(), SteelVal::CharV('c'))
    }

    #[test]
    fn from_steelval_usize() {
        assert_eq!(usize::from_steelval(&SteelVal::IntV(10)).unwrap(), 10)
    }

    #[test]
    fn from_steelval_i32() {
        assert_eq!(i32::from_steelval(&SteelVal::IntV(32)).unwrap(), 32)
    }

    #[test]
    fn into_steelval_i32() {
        assert_eq!(32.into_steelval().unwrap(), SteelVal::IntV(32))
    }

    #[test]
    fn from_bool() {
        assert_eq!(SteelVal::from(true), SteelVal::BoolV(true));
    }

    #[test]
    fn try_from_steelval_string() {
        let expected = "foo".to_string();
        let input = SteelVal::StringV("foo".into());

        let res = String::try_from(input);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn try_from_steelval_ref_string() {
        let expected = "foo".to_string();
        let input = SteelVal::StringV("foo".into());

        let res = String::try_from(&input);
        assert_eq!(res.unwrap(), expected);
    }
}
