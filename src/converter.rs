use crate::rerrs::RucketErr;
use crate::rvals::RucketVal;
use std::convert::TryFrom;
// the conversion layer works like
// Vec<RucketVal> -> your struct -> call the function -> output -> Rucketval output
// maybe TryFrom Error type should be something else?
trait RucketFunctor<U, V>
where
    U: TryFrom<Vec<RucketVal>, Error = &'static str>,
    V: Into<RucketVal>,
{
    fn new_func(name: String) -> (String, fn(Vec<RucketVal>) -> Result<RucketVal, RucketErr>) {
        let wrapped = |args: Vec<RucketVal>| -> Result<RucketVal, RucketErr> {
            let input = Self::in_convert(args)?;
            let res = Self::call(input)?;
            Ok(res.into())
        };
        (name, wrapped)
    }
    fn call(input: U) -> Result<V, RucketErr>;
    fn in_convert(input: Vec<RucketVal>) -> Result<U, RucketErr> {
        U::try_from(input).map_err(|e| RucketErr::ConversionError(e.to_string()))
    }
}

struct VecNumbers(Vec<f64>);
impl TryFrom<Vec<RucketVal>> for VecNumbers {
    type Error = &'static str;
    fn try_from(value: Vec<RucketVal>) -> Result<Self, Self::Error> {
        let num_matcher = |val| match val {
            RucketVal::NumV(x) => Ok(x),
            _ => Err("Expected number"),
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
