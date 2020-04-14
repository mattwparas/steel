mod io;
mod lists;
mod strings;
mod vectors;

pub use io::IoFunctions;
pub use lists::ListOperations;
pub use strings::StringOperations;
pub use vectors::VectorOperations;

// use crate::converter::SteelFunctor;
use crate::rerrs::SteelErr;
use crate::rvals::{FunctionSignature, SteelLambda, SteelVal};
use im_rc::Vector;
// use std::collections::Vector;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::result;

use std::rc::Rc;

// pub struct List(pub Vec<T>);

// the conversion layer works like
// Vec<SteelVal> -> your struct -> call the function -> output -> Steelval output
// maybe TryFrom Error type should be something else?

pub trait SteelFunctor<U, V>
where
    U: TryFrom<Vec<SteelVal>, Error = SteelErr>,
    V: Into<SteelVal>,
{
    fn new_func() -> FunctionSignature {
        |args: Vec<Rc<SteelVal>>| {
            let args = args.into_iter().map(|x| (*x).clone()).collect();
            let input = Self::in_convert(args)?;
            let res = Self::call(input)?;
            Ok(Rc::new(res.into())) // TODO
        }
    }
    fn call(input: U) -> Result<V, SteelErr>;
    fn in_convert(input: Vec<SteelVal>) -> Result<U, SteelErr> {
        U::try_from(input)
    }
}

macro_rules! try_from_impl {
    ($type:ident => $($body:ty),*) => {
        $(
            impl TryFrom<SteelVal> for $body {
                type Error = SteelErr;
                fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::ConversionError("Expected number".to_string())),
                    }
                }
            }

            impl TryFrom<&SteelVal> for $body {
                type Error = SteelErr;
                fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
                    match value {
                        SteelVal::$type(x) => Ok(x.clone() as $body),
                        _ => Err(SteelErr::ConversionError("Expected number".to_string())),
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
        )*
    };
}

impl<T: TryInto<SteelVal>> TryFrom<Vec<T>> for SteelVal {
    type Error = SteelErr;
    fn try_from(val: Vec<T>) -> result::Result<Self, Self::Error> {
        let vec_vals: Result<Vec<Self>, <T as std::convert::TryInto<SteelVal>>::Error> =
            val.into_iter().map(|x| x.try_into()).collect();

        match vec_vals {
            Ok(l) => Ok(vec_to_list(l)),
            _ => Err(SteelErr::ConversionError(
                "Could not convert vector of values to SteelVal list".to_string(),
            )),
        }
    }
}

impl<T: TryFrom<SteelVal>> TryFrom<SteelVal> for Vec<T> {
    type Error = SteelErr;
    fn try_from(val: SteelVal) -> result::Result<Self, Self::Error> {
        if let SteelVal::Pair(_, _) = val {
            let vec_vals = collect_pair_into_vector(&val);
            let result_vec_vals: Result<Self, <T as std::convert::TryFrom<SteelVal>>::Error> =
                vec_vals.into_iter().map(|x| T::try_from(x)).collect();
            match result_vec_vals {
                Ok(x) => Ok(x),
                _ => Err(SteelErr::ConversionError(
                    "Could not convert SteelVal list to Vector of values".to_string(),
                )),
            }
        } else {
            Err(SteelErr::ConversionError(
                "Could not convert SteelVal list to Vector of values".to_string(),
            ))
        }
    }
}

impl<T: TryFrom<SteelVal>> TryFrom<&SteelVal> for Vec<T> {
    type Error = SteelErr;
    fn try_from(val: &SteelVal) -> result::Result<Self, Self::Error> {
        if let SteelVal::Pair(_, _) = val {
            let vec_vals = collect_pair_into_vector(&val);
            let result_vec_vals: Result<Self, <T as std::convert::TryFrom<SteelVal>>::Error> =
                vec_vals.into_iter().map(|x| T::try_from(x)).collect();
            match result_vec_vals {
                Ok(x) => Ok(x),
                _ => Err(SteelErr::ConversionError(
                    "Could not convert SteelVal list to Vector of values".to_string(),
                )),
            }
        } else {
            Err(SteelErr::ConversionError(
                "Could not convert SteelVal list to Vector of values".to_string(),
            ))
        }
    }
}

fn collect_pair_into_vector(mut p: &SteelVal) -> Vec<SteelVal> {
    let mut lst = Vec::new();
    loop {
        if let SteelVal::Pair(cons, cdr) = p {
            lst.push((**cons).clone());
            match cdr.as_ref() {
                Some(rest) => match rest.as_ref() {
                    SteelVal::Pair(_, _) => p = rest,
                    _ => {
                        lst.push((**rest).clone());
                        return lst;
                    }
                },
                None => return lst,
            }
        }
    }
}

fn vec_to_list(args: Vec<SteelVal>) -> SteelVal {
    let mut args = args.into_iter().rev();
    let mut pairs = Vec::new();
    match (args.next(), args.next()) {
        (cdr, Some(car)) => {
            pairs.push(Rc::new(SteelVal::Pair(
                Rc::new(car),
                cdr.map(|x| Rc::new(x)),
            )));
        }
        (Some(cdr), None) => {
            pairs.push(Rc::new(SteelVal::Pair(Rc::new(cdr), None)));
        }
        (_, _) => {
            return SteelVal::VectorV(Vector::new());
        }
    }

    for (i, val) in args.enumerate() {
        pairs.push(Rc::new(SteelVal::Pair(
            Rc::new(val),
            Some(Rc::clone(&pairs[i])),
        )));
    }
    (*pairs.pop().unwrap()).clone()
}

from_f64!(f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);
try_from_impl!(NumV => f64, f32, i32, i16, i8, u8, u16, u32, u64, usize, isize);

impl TryFrom<SteelVal> for String {
    type Error = SteelErr;
    fn try_from(value: SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(ref x) => Ok(x.clone()),
            _ => Err(SteelErr::ConversionError("Expected number".to_string())),
        }
    }
}

impl TryFrom<&SteelVal> for String {
    type Error = SteelErr;
    fn try_from(value: &SteelVal) -> result::Result<Self, Self::Error> {
        match value {
            SteelVal::StringV(x) => Ok(x.clone()),
            _ => Err(SteelErr::ConversionError("Expected number".to_string())),
        }
    }
}

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
        converted.map(Self)
    }
}

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

impl From<Vector<SteelVal>> for SteelVal {
    fn from(val: Vector<SteelVal>) -> SteelVal {
        SteelVal::VectorV(val)
    }
}

impl From<FunctionSignature> for SteelVal {
    fn from(val: FunctionSignature) -> SteelVal {
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
