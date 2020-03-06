use crate::converter::ConversionError;
use crate::converter::RucketFunctor;
use crate::rerrs::RucketErr;
use crate::rvals::{RucketLambda, RucketVal};
use std::convert::TryFrom;
use std::result;

// impl TryFrom<RucketVal> for f64 {
//     type Error = ConversionError;
//     fn try_from(value: RucketVal) -> Result<Self, Self::Error> {
//         match value {
//             RucketVal::NumV(x) => Ok(x),
//             _ => Err(ConversionError::Generic("Expected number".to_string())),
//         }
//     }
// }

#[macro_export]
macro_rules! try_from_impl {
    ($type:ident => $($body:ty),*) => {
        $(
            impl TryFrom<RucketVal> for $body {
                type Error = RucketErr;
                fn try_from(value: RucketVal) -> result::Result<Self, Self::Error> {
                    match value {
                        RucketVal::$type(x) => Ok(x as $body),
                        _ => Err(RucketErr::ConversionError("Expected number".to_string())),
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
            impl From<$body> for RucketVal {
                fn from(val: $body) -> RucketVal {
                    RucketVal::NumV(val as f64)
                }
            }
        )*
    };
}

from_f64!(f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);

try_from_impl!(NumV => f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);
try_from_impl!(StringV => String);

pub struct VecNumbers(Vec<f64>);
impl TryFrom<Vec<RucketVal>> for VecNumbers {
    type Error = ConversionError;
    fn try_from(value: Vec<RucketVal>) -> Result<Self, Self::Error> {
        let num_matcher = |val| match val {
            RucketVal::NumV(x) => Ok(x),
            _ => Err(ConversionError::Generic("Expected number".to_string())),
        };
        let val_iter = value.into_iter();
        let converted: Result<Vec<f64>, Self::Error> = val_iter.map(num_matcher).collect();
        converted.map(|x| Self(x))
    }
}

// impl From<f64> for RucketVal {
//     fn from(val: f64) -> RucketVal {
//         RucketVal::NumV(val)
//     }
// }

impl From<String> for RucketVal {
    fn from(val: String) -> RucketVal {
        RucketVal::StringV(val)
    }
}

impl From<bool> for RucketVal {
    fn from(val: bool) -> RucketVal {
        RucketVal::BoolV(val)
    }
}

impl From<Vec<RucketVal>> for RucketVal {
    fn from(val: Vec<RucketVal>) -> RucketVal {
        RucketVal::ListV(val)
    }
}

impl From<fn(Vec<RucketVal>) -> Result<RucketVal, RucketErr>> for RucketVal {
    fn from(val: fn(Vec<RucketVal>) -> Result<RucketVal, RucketErr>) -> RucketVal {
        RucketVal::FuncV(val)
    }
}

impl From<RucketLambda> for RucketVal {
    fn from(val: RucketLambda) -> RucketVal {
        RucketVal::LambdaV(val)
    }
}

pub struct Adder;
impl RucketFunctor<VecNumbers, f64> for Adder {
    fn call(input: VecNumbers) -> Result<f64, RucketErr> {
        Ok(input.0.iter().fold(0.0, |sum, x| sum + x))
    }
}

pub struct Multiplier;
impl RucketFunctor<VecNumbers, f64> for Multiplier {
    fn call(input: VecNumbers) -> Result<f64, RucketErr> {
        Ok(input.0.iter().fold(1.0, |sum, x| sum * x))
    }
}

pub struct Subtractor;
impl RucketFunctor<VecNumbers, f64> for Subtractor {
    fn call(input: VecNumbers) -> Result<f64, RucketErr> {
        let mut v = input.0.into_iter();
        if let Some(first) = v.next() {
            Ok(v.fold(first, |acc, x| acc - x))
        } else {
            Err(RucketErr::ArityMismatch(
                "'-' expects at least one number".to_string(),
            ))
        }
    }
}

pub struct Divider;
impl RucketFunctor<VecNumbers, f64> for Divider {
    fn call(input: VecNumbers) -> Result<f64, RucketErr> {
        let mut v = input.0.into_iter();
        if let Some(first) = v.next() {
            Ok(v.fold(first, |acc, x| acc / x))
        } else {
            Err(RucketErr::ArityMismatch(
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
