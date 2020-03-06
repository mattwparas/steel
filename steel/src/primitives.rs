// use crate::converter::SteelFunctor;
use crate::rerrs::SteelErr;
use crate::rvals::{SteelLambda, SteelVal};
use std::convert::TryFrom;
use std::result;

// the conversion layer works like
// Vec<SteelVal> -> your struct -> call the function -> output -> Steelval output
// maybe TryFrom Error type should be something else?

pub trait SteelFunctor<U, V>
where
    U: TryFrom<Vec<SteelVal>, Error = SteelErr>,
    V: Into<SteelVal>,
{
    fn new_func() -> fn(args: Vec<SteelVal>) -> Result<SteelVal, SteelErr> {
        |args: Vec<SteelVal>| {
            let input = Self::in_convert(args)?;
            let res = Self::call(input)?;
            Ok(res.into())
        }
    }
    fn call(input: U) -> Result<V, SteelErr>;
    fn in_convert(input: Vec<SteelVal>) -> Result<U, SteelErr> {
        U::try_from(input)
    }
}

#[macro_export]
macro_rules! try_from_impl {
    ($type:ident => $($body:ty),*) => {
        $(
            impl TryFrom<SteelVal> for $body {
                type Error = SteelErr;
                fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x as $body),
                        _ => Err(SteelErr::ConversionError("Expected number".to_string())),
                    }
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! from_f64 {
    ($($body:ty),*) => {
        $(
            impl From<$body> for SteelVal {
                fn from(val: $body) -> SteelVal {
                    SteelVal::NumV(val as f64)
                }
            }
        )*
    };
}

from_f64!(f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);

try_from_impl!(NumV => f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);
try_from_impl!(StringV => String);

pub struct VecNumbers(Vec<f64>);
impl TryFrom<Vec<SteelVal>> for VecNumbers {
    type Error = SteelErr;
    fn try_from(value: Vec<SteelVal>) -> Result<Self, Self::Error> {
        let num_matcher = |val| match val {
            SteelVal::NumV(x) => Ok(x),
            _ => Err(SteelErr::ConversionError("Expected number".to_string())),
        };
        let val_iter = value.into_iter();
        let converted: Result<Vec<f64>, Self::Error> = val_iter.map(num_matcher).collect();
        converted.map(|x| Self(x))
    }
}

// impl From<f64> for SteelVal {
//     fn from(val: f64) -> SteelVal {
//         SteelVal::NumV(val)
//     }
// }

impl From<String> for SteelVal {
    fn from(val: String) -> SteelVal {
        SteelVal::StringV(val)
    }
}

impl From<bool> for SteelVal {
    fn from(val: bool) -> SteelVal {
        SteelVal::BoolV(val)
    }
}

impl From<Vec<SteelVal>> for SteelVal {
    fn from(val: Vec<SteelVal>) -> SteelVal {
        SteelVal::ListV(val)
    }
}

impl From<fn(Vec<SteelVal>) -> Result<SteelVal, SteelErr>> for SteelVal {
    fn from(val: fn(Vec<SteelVal>) -> Result<SteelVal, SteelErr>) -> SteelVal {
        SteelVal::FuncV(val)
    }
}

impl From<SteelLambda> for SteelVal {
    fn from(val: SteelLambda) -> SteelVal {
        SteelVal::LambdaV(val)
    }
}

pub struct Adder;
impl SteelFunctor<VecNumbers, f64> for Adder {
    fn call(input: VecNumbers) -> Result<f64, SteelErr> {
        Ok(input.0.iter().fold(0.0, |sum, x| sum + x))
    }
}

pub struct Multiplier;
impl SteelFunctor<VecNumbers, f64> for Multiplier {
    fn call(input: VecNumbers) -> Result<f64, SteelErr> {
        Ok(input.0.iter().fold(1.0, |sum, x| sum * x))
    }
}

pub struct Subtractor;
impl SteelFunctor<VecNumbers, f64> for Subtractor {
    fn call(input: VecNumbers) -> Result<f64, SteelErr> {
        let mut v = input.0.into_iter();
        if let Some(first) = v.next() {
            Ok(v.fold(first, |acc, x| acc - x))
        } else {
            Err(SteelErr::ArityMismatch(
                "'-' expects at least one number".to_string(),
            ))
        }
    }
}

pub struct Divider;
impl SteelFunctor<VecNumbers, f64> for Divider {
    fn call(input: VecNumbers) -> Result<f64, SteelErr> {
        let mut v = input.0.into_iter();
        if let Some(first) = v.next() {
            Ok(v.fold(first, |acc, x| acc / x))
        } else {
            Err(SteelErr::ArityMismatch(
                "'\' expects at least one number".to_string(),
            ))
        }
    }
}

// impl Default for Adder {
//     fn default() -> Self {
//         Adder {}
//     }
// }
// impl Default for Subtractor {
//     fn default() -> Self {
//         Subtractor {}
//     }
// }
// impl Default for Multiplier {
//     fn default() -> Self {
//         Multiplier {}
//     }
// }
// impl Default for Divider {
//     fn default() -> Self {
//         Divider {}
//     }
// }
