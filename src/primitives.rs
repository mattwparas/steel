use crate::converter::ConversionError;
use crate::converter::RucketFunctor;
use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use std::convert::TryFrom;

// impl TryFrom<RucketVal> for f64 {
//     type Error = ConversionError;
//     fn try_from(value: RucketVal) -> Result<Self, Self::Error> {
//         match value {
//             RucketVal::NumV(x) => Ok(x),
//             _ => Err(ConversionError::Generic("Expected number".to_string())),
//         }
//     }
// }

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

impl From<f64> for RucketVal {
    fn from(val: f64) -> RucketVal {
        RucketVal::NumV(val)
    }
}

impl From<String> for RucketVal {
    fn from(val: String) -> RucketVal {
        RucketVal::StringV(val)
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
