mod contracts;
mod control;
mod fs;
mod hashmaps;
mod hashsets;
mod io;
mod lists;
mod meta_ops;
mod nums;
mod ports;
mod streams;
mod strings;
mod symbols;
mod transducers;
mod vectors;

pub use contracts::ContractOperations;
pub use control::ControlOperations;
pub use fs::FsFunctions;
pub use hashmaps::HashMapOperations;
pub use hashsets::HashSetOperations;
pub use io::IoFunctions;
pub use lists::ListOperations;
pub use meta_ops::MetaOperations;
pub use nums::NumOperations;
pub use ports::PortOperations;
pub use streams::StreamOperations;
pub use strings::StringOperations;
pub use symbols::SymbolOperations;
pub use transducers::TransducerOperations;
pub use vectors::VectorOperations;

use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{FunctionSignature, SteelVal};
use im_rc::Vector;

use std::convert::TryFrom;
use std::convert::TryInto;
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
                fn from_steelval(value: SteelVal) -> result::Result<Self, SteelErr> {
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
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        if let SteelVal::CharV(c) = val {
            Ok(c)
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
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        if val.is_truthy() {
            Ok(Some(T::from_steelval(val)?))
        } else {
            Ok(None)
        }
    }
}

// TODO make intosteelval return a result type
// This allows errors to propagate

impl<T: IntoSteelVal, E: std::fmt::Debug> IntoSteelVal for Result<T, E> {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        match self {
            Ok(s) => s.into_steelval(),
            Err(e) => crate::stop!(Generic => format!("{:?}", e)),
        }
    }
}

impl FromSteelVal for () {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
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

// TODO add explicit result type for handling results?
// impl<T: Into<SteelVal>, E> From<std::result::Result<T>, E> for SteelVal {
//     fn from(val: std::result::Result<T, E>) -> SteelVal {
//         if let Ok(s) = val {
//             s.into()
//         } else {
//             SteelVal::BoolV(true)
//         }
//     }
// }

impl<T: TryInto<SteelVal>> TryFrom<Vec<T>> for SteelVal {
    type Error = SteelErr;
    fn try_from(val: Vec<T>) -> result::Result<Self, Self::Error> {
        let vec_vals: Result<Vec<Self>, <T as std::convert::TryInto<SteelVal>>::Error> =
            val.into_iter().map(|x| x.try_into()).collect();

        match vec_vals {
            Ok(l) => ListOperations::built_in_list_func_flat(&l),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert vector of values to SteelVal list".to_string(),
            )),
        }
    }
}

impl<T: IntoSteelVal> IntoSteelVal for Vec<T> {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        let vec_vals: Result<Vec<SteelVal>, SteelErr> =
            self.into_iter().map(|x| x.into_steelval()).collect();

        match vec_vals {
            Ok(l) => ListOperations::built_in_list_func_flat(&l),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert vector of values to SteelVal list".to_string(),
            )),
        }
    }
}

impl<T: FromSteelVal> FromSteelVal for Vec<T> {
    fn from_steelval(val: SteelVal) -> Result<Self, SteelErr> {
        match val {
            SteelVal::Pair(_) => {
                // let vec_vals = collect_pair_into_vector(&val);
                let result_vec_vals: Result<Self, SteelErr> = SteelVal::iter(val.clone())
                    .into_iter()
                    .map(FromSteelVal::from_steelval)
                    .collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            }
            SteelVal::VectorV(v) => {
                let result_vec_vals: Result<Self, SteelErr> = v
                    .iter()
                    .map(|x| FromSteelVal::from_steelval(x.clone()))
                    .collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            } // TODO
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal list to Vector of values".to_string(),
            )),
        }
    }
}

impl<T: TryFrom<SteelVal>> TryFrom<SteelVal> for Vec<T> {
    type Error = SteelErr;
    fn try_from(val: SteelVal) -> result::Result<Self, Self::Error> {
        match val {
            SteelVal::Pair(_) => {
                // let vec_vals = collect_pair_into_vector(&val);
                let result_vec_vals: Result<Self, <T as std::convert::TryFrom<SteelVal>>::Error> =
                    SteelVal::iter(val.clone())
                        .into_iter()
                        .map(T::try_from)
                        .collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            }
            SteelVal::VectorV(v) => {
                let result_vec_vals: Result<Self, <T as std::convert::TryFrom<SteelVal>>::Error> =
                    v.iter().map(|x| T::try_from(x.clone())).collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            } // TODO
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal list to Vector of values".to_string(),
            )),
        }
    }
}

impl<T: TryFrom<SteelVal>> TryFrom<&SteelVal> for Vec<T> {
    type Error = SteelErr;
    fn try_from(val: &SteelVal) -> result::Result<Self, Self::Error> {
        match val {
            SteelVal::Pair(_) => {
                // let vec_vals = collect_pair_into_vector(&val);
                let result_vec_vals: Result<Self, <T as std::convert::TryFrom<SteelVal>>::Error> =
                    SteelVal::iter(val.clone())
                        .into_iter()
                        .map(T::try_from)
                        .collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            }
            SteelVal::VectorV(v) => {
                let result_vec_vals: Result<Self, <T as std::convert::TryFrom<SteelVal>>::Error> =
                    v.iter().map(|x| T::try_from(x.clone())).collect();
                match result_vec_vals {
                    Ok(x) => Ok(x),
                    _ => Err(SteelErr::new(
                        ErrorKind::ConversionError,
                        "Could not convert SteelVal list to Vector of values".to_string(),
                    )),
                }
            } // TODO
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Could not convert SteelVal list to Vector of values".to_string(),
            )),
        }
    }
}

// fn collect_pair_into_vector(p: &SteelVal) -> Vec<SteelVal> {
//     SteelVal::iter(p.clone()).collect()
// }

// from_f64!(f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);

from_f64!(f64, f32);

from_for_isize!(i32, i16, i8, u8, u16, u32, u64, usize, isize);

// from_usize!(u64, u32);

// from_f64!(f64, f32);
try_from_impl!(NumV => f64, f32);
try_from_impl!(IntV => i64, i32, i16, i8, u8, u16, u32, u64, usize, isize);

impl TryFrom<SteelVal> for String {
    type Error = SteelErr;
    fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(ref x) => Ok(x.unwrap()),
            SteelVal::SymbolV(ref x) => Ok(x.unwrap()),
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

impl TryFrom<&Gc<SteelVal>> for String {
    type Error = SteelErr;
    fn try_from(value: &Gc<SteelVal>) -> result::Result<Self, Self::Error> {
        match value.as_ref() {
            SteelVal::StringV(x) => Ok(x.unwrap()),
            SteelVal::SymbolV(x) => Ok(x.unwrap()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected string".to_string(),
            )),
        }
    }
}

impl TryFrom<&SteelVal> for String {
    type Error = SteelErr;
    fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(x) => Ok(x.unwrap()),
            SteelVal::SymbolV(x) => Ok(x.unwrap()),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected string".to_string(),
            )),
        }
    }
}

pub struct VecNumbers(Vec<f64>);
impl TryFrom<Vec<SteelVal>> for VecNumbers {
    type Error = SteelErr;
    fn try_from(value: Vec<SteelVal>) -> Result<Self, Self::Error> {
        let num_matcher = |val| match val {
            SteelVal::NumV(x) => Ok(x),
            _ => Err(SteelErr::new(
                ErrorKind::ConversionError,
                "Expected number in vec".to_string(),
            )),
        };
        let val_iter = value.into_iter();
        let converted: Result<Vec<f64>, Self::Error> = val_iter.map(num_matcher).collect();
        converted.map(Self)
    }
}

impl From<String> for SteelVal {
    fn from(val: String) -> SteelVal {
        SteelVal::StringV(val.into())
    }
}

impl IntoSteelVal for String {
    fn into_steelval(self) -> Result<SteelVal, SteelErr> {
        Ok(SteelVal::StringV(Gc::new(self)))
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
    use crate::rvals::ConsCell;
    use im_rc::vector;

    #[test]
    fn try_from_vec_usize() {
        let input: Vec<usize> = vec![0, 1];
        let res = SteelVal::try_from(input);
        let expected = SteelVal::Pair(Gc::new(ConsCell::new(
            SteelVal::IntV(0),
            Some(Gc::new(ConsCell::new(SteelVal::IntV(1), None))),
        )));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn try_from_steelval_list_to_vec_usize() {
        let input = SteelVal::Pair(Gc::new(ConsCell::new(
            SteelVal::IntV(0),
            Some(Gc::new(ConsCell::new(SteelVal::IntV(1), None))),
        )));
        let res = <Vec<usize>>::try_from(input);
        let expected: Vec<usize> = vec![0, 1];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn try_from_steelval_list_to_vec_bad() {
        let input = SteelVal::StringV("foo".into());
        let res = <Vec<usize>>::try_from(input);
        assert!(res.is_err());
    }

    #[test]
    fn try_from_steelval_list_ref_to_vec_bad() {
        let input = SteelVal::StringV("foo".into());
        let res = <Vec<usize>>::try_from(&input);
        assert!(res.is_err());
    }

    #[test]
    fn try_from_steelval_vec_to_vec_usize() {
        let input = SteelVal::VectorV(Gc::new(vector![SteelVal::IntV(0), SteelVal::IntV(1)]));
        let res = <Vec<usize>>::try_from(input);
        let expected: Vec<usize> = vec![0, 1];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn try_from_ref_steelval_list_to_vec_usize() {
        let input = SteelVal::Pair(Gc::new(ConsCell::new(
            SteelVal::IntV(0),
            Some(Gc::new(ConsCell::new(SteelVal::IntV(1), None))),
        )));
        let res = <Vec<usize>>::try_from(&input);
        let expected: Vec<usize> = vec![0, 1];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn try_from_steelval_ref_vec_to_vec_usize() {
        let input = SteelVal::VectorV(Gc::new(vector![SteelVal::IntV(0), SteelVal::IntV(1)]));
        let res = <Vec<usize>>::try_from(&input);
        let expected: Vec<usize> = vec![0, 1];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn from_char() {
        assert_eq!(SteelVal::from('c'), SteelVal::CharV('c'));
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
