use std::ops::Neg;

use num::{BigInt, BigRational, CheckedAdd, CheckedMul, Integer, Rational32, ToPrimitive};

use crate::rvals::{IntoSteelVal, Result, SteelVal};
use crate::stop;

fn ensure_args_are_numbers(op: &str, args: &[SteelVal]) -> Result<()> {
    for arg in args {
        match arg {
            SteelVal::NumV(_)
            | SteelVal::FractV(_)
            | SteelVal::IntV(_)
            | SteelVal::BigNum(_)
            | SteelVal::BigFract(_) => {}
            v => stop!(TypeMismatch => "{op} expects a number, found: {:?}", v),
        };
    }
    Ok(())
}

/// # Precondition
/// - `x` and `y` must be valid numerical types.
fn multiply_2_impl(x: &SteelVal, y: &SteelVal) -> Result<SteelVal> {
    match (x, y) {
        (SteelVal::NumV(x), SteelVal::NumV(y)) => (x * y).into_steelval(),
        (SteelVal::NumV(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::NumV(x)) => {
            (x * *y as f64).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigNum(y)) | (SteelVal::BigNum(y), SteelVal::NumV(x)) => {
            (x * y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::FractV(y)) | (SteelVal::FractV(y), SteelVal::NumV(x)) => {
            (x * y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigFract(y)) | (SteelVal::BigFract(y), SteelVal::NumV(x)) => {
            (x * y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::IntV(x), SteelVal::IntV(y)) => match x.checked_mul(y) {
            Some(res) => res.into_steelval(),
            None => {
                let mut res = BigInt::from(*x);
                res *= *y;
                res.into_steelval()
            }
        },
        (SteelVal::IntV(x), SteelVal::BigNum(y)) | (SteelVal::BigNum(y), SteelVal::IntV(x)) => {
            (y.as_ref() * x).into_steelval()
        }
        (SteelVal::IntV(x), SteelVal::FractV(y)) | (SteelVal::FractV(y), SteelVal::IntV(x)) => {
            match i32::try_from(*x) {
                Ok(x) => match y.checked_mul(&Rational32::new(x, 1)) {
                    Some(res) => res.into_steelval(),
                    None => {
                        let mut res =
                            BigRational::new(BigInt::from(*y.numer()), BigInt::from(*y.denom()));
                        res *= BigInt::from(x);
                        res.into_steelval()
                    }
                },
                Err(_) => {
                    let mut res =
                        BigRational::new(BigInt::from(*y.numer()), BigInt::from(*y.denom()));
                    res *= BigInt::from(*x);
                    res.into_steelval()
                }
            }
        }
        (SteelVal::IntV(x), SteelVal::BigFract(y)) | (SteelVal::BigFract(y), SteelVal::IntV(x)) => {
            let mut res = y.as_ref().clone();
            res *= BigInt::from(*x);
            res.into_steelval()
        }
        (SteelVal::FractV(x), SteelVal::FractV(y)) => match x.checked_mul(y) {
            Some(res) => res.into_steelval(),
            None => {
                let mut res = BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()));
                res *= BigRational::new(BigInt::from(*y.numer()), BigInt::from(*y.denom()));
                res.into_steelval()
            }
        },
        (SteelVal::FractV(x), SteelVal::BigNum(y)) | (SteelVal::BigNum(y), SteelVal::FractV(x)) => {
            let mut res = BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()));
            res *= y.as_ref();
            res.into_steelval()
        }
        (SteelVal::BigFract(x), SteelVal::BigFract(y)) => (x.as_ref() + y.as_ref()).into_steelval(),
        (SteelVal::BigFract(x), SteelVal::BigNum(y))
        | (SteelVal::BigNum(y), SteelVal::BigFract(x)) => (x.as_ref() + y.as_ref()).into_steelval(),
        (SteelVal::BigNum(x), SteelVal::BigNum(y)) => (x.as_ref() + y.as_ref()).into_steelval(),
        _ => unreachable!(),
    }
}

/// # Precondition
/// All types in `args` must be numerical types.
fn multiply_primitive_impl(args: &[SteelVal]) -> Result<SteelVal> {
    match args {
        [] => 1.into_steelval(),
        [x] => x.clone().into_steelval(),
        [x, y] => multiply_2_impl(x, y).into_steelval(),
        [x, y, zs @ ..] => {
            let mut res = multiply_2_impl(x, y)?;
            for z in zs {
                // TODO: This use case could be optimized to reuse state instead of creating a new
                // object each time.
                res = multiply_2_impl(&res, &z)?;
            }
            res.into_steelval()
        }
    }
}

pub fn multiply_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("*", args)?;
    multiply_primitive_impl(args)
}

pub fn quotient(l: isize, r: isize) -> isize {
    l / r
}

pub fn divide_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("/", args)?;
    let recip = |x: &SteelVal| -> Result<SteelVal> {
        match x {
            SteelVal::IntV(n) => match i32::try_from(*n) {
                Ok(n) => Rational32::new(1, n).into_steelval(),
                Err(_) => BigRational::new(BigInt::from(1), BigInt::from(*n)).into_steelval(),
            },
            SteelVal::NumV(n) => n.recip().into_steelval(),
            SteelVal::FractV(f) => f.recip().into_steelval(),
            SteelVal::BigFract(f) => f.recip().into_steelval(),
            SteelVal::BigNum(n) => BigRational::new(1.into(), n.as_ref().clone()).into_steelval(),
            unexpected => {
                stop!(TypeMismatch => "/ expects a number, but found: {:?}", unexpected)
            }
        }
    };
    match &args {
        [] => stop!(ArityMismatch => "/ requires at least one argument"),
        [x] => recip(x),
        // TODO: Provide custom implementation to optimize by joining the multiply and recip calls.
        [x, y] => multiply_2_impl(x, &recip(y)?),
        [x, ys @ ..] => {
            let d = multiply_primitive_impl(ys)?;
            multiply_2_impl(&x, &recip(&d)?)
        }
    }
}

#[inline(always)]
pub fn subtract_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("/", args)?;
    let negate = |x: &SteelVal| match x {
        SteelVal::NumV(x) => (-x).into_steelval(),
        SteelVal::IntV(x) => match x.checked_neg() {
            Some(res) => res.into_steelval(),
            None => BigInt::from(*x).neg().into_steelval(),
        },
        SteelVal::FractV(x) => match 0i32.checked_sub(*x.numer()) {
            Some(n) => Rational32::new(n, *x.denom()).into_steelval(),
            None => BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()))
                .neg()
                .into_steelval(),
        },
        SteelVal::BigFract(x) => x.as_ref().neg().into_steelval(),
        SteelVal::BigNum(x) => x.as_ref().clone().neg().into_steelval(),
        _ => unreachable!(),
    };
    match args {
        [] => stop!(TypeMismatch => "- requires at least one argument"),
        [x] => negate(x),
        [x, ys @ ..] => {
            let y = negate(&add_primitive(ys)?)?;
            add_primitive(&[x.clone(), y])
        }
    }
}

#[steel_derive::native(name = "+", constant = true, arity = "AtLeast(0)")]
pub fn add_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("+", args)?;
    let add = |x: &SteelVal, y: &SteelVal| match (x, y) {
        // Simple integer case. Probably very common.
        (SteelVal::IntV(x), SteelVal::IntV(y)) => match x.checked_add(y) {
            Some(res) => res.into_steelval(),
            None => {
                let mut res = BigInt::from(*x);
                res += *y;
                res.into_steelval()
            }
        },
        // Cases that return an `f64`.
        (SteelVal::NumV(x), SteelVal::NumV(y)) => (x + y).into_steelval(),
        (SteelVal::NumV(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::NumV(x)) => {
            (x + *y as f64).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigNum(y)) | (SteelVal::BigNum(y), SteelVal::NumV(x)) => {
            (x + y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::FractV(y)) | (SteelVal::FractV(y), SteelVal::NumV(x)) => {
            (x + y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigFract(y)) | (SteelVal::BigFract(y), SteelVal::NumV(x)) => {
            (x + y.to_f64().unwrap()).into_steelval()
        }
        // Cases that interact with `FractV`.
        (SteelVal::FractV(x), SteelVal::FractV(y)) => (x + y).into_steelval(),
        (SteelVal::FractV(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::FractV(x)) => {
            match i32::try_from(*y) {
                Ok(y) => match x.checked_add(&Rational32::new(y, 1)) {
                    Some(res) => res.into_steelval(),
                    None => {
                        let res =
                            BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()))
                                * BigInt::from(y);
                        res.into_steelval()
                    }
                },
                Err(_) => {
                    let res = BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()))
                        * BigInt::from(*y);
                    res.into_steelval()
                }
            }
        }
        (SteelVal::FractV(x), SteelVal::BigNum(y)) | (SteelVal::BigNum(y), SteelVal::FractV(x)) => {
            let res =
                BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom())) * y.as_ref();
            res.into_steelval()
        }
        // Cases that interact with `BigFract`. Hopefully not too common, for performance reasons.
        (SteelVal::BigFract(x), SteelVal::BigFract(y)) => (x.as_ref() + y.as_ref()).into_steelval(),
        (SteelVal::BigFract(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::BigFract(x)) => {
            (x.as_ref() + BigInt::from(*y)).into_steelval()
        }
        (SteelVal::BigFract(x), SteelVal::BigNum(y))
        | (SteelVal::BigNum(y), SteelVal::BigFract(x)) => (x.as_ref() * y.as_ref()).into_steelval(),
        // Remaining cases that interact with `BigNum`. Probably not too common.
        (SteelVal::BigNum(x), SteelVal::BigNum(y)) => {
            let mut res = x.as_ref().clone();
            res += y.as_ref();
            res.into_steelval()
        }
        (SteelVal::BigNum(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::BigNum(x)) => {
            let mut res = x.as_ref().clone();
            res += *y;
            res.into_steelval()
        }
        _ => unreachable!(),
    };
    match args {
        [] => 0.into_steelval(),
        [x] => x.clone().into_steelval(),
        [x, y] => add(x, y),
        [x, y, zs @ ..] => {
            let mut res = add(x, y)?;
            for z in zs {
                res = add(&res, z)?;
            }
            res.into_steelval()
        }
    }
}

pub struct NumOperations {}
impl NumOperations {
    pub fn arithmetic_shift() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "arithmetic-shift takes 2 arguments")
            }
            let n = args[0].clone();
            let m = args[1].clone();

            match (n, m) {
                (SteelVal::IntV(n), SteelVal::IntV(m)) => {
                    if m >= 0 {
                        Ok(SteelVal::IntV(n << m))
                    } else {
                        Ok(SteelVal::IntV(n >> -m))
                    }
                }
                _ => stop!(TypeMismatch => "arithmetic-shift expected 2 integers"),
            }
        })
    }

    pub fn even() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "even? takes one argument")
            }

            match &args[0] {
                SteelVal::IntV(n) => Ok(SteelVal::BoolV(n & 1 == 0)),
                SteelVal::BigNum(n) => Ok(SteelVal::BoolV(n.is_even())),
                SteelVal::NumV(n) if n.fract() == 0.0 => (*n as i64).is_even().into_steelval(),
                _ => {
                    stop!(TypeMismatch => format!("even? requires an integer, found: {:?}", &args[0]))
                }
            }
        })
    }

    pub fn odd() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "odd? takes one argument")
            }

            match &args[0] {
                SteelVal::IntV(n) => Ok(SteelVal::BoolV(n & 1 == 1)),
                SteelVal::BigNum(n) => Ok(SteelVal::BoolV(n.is_odd())),
                SteelVal::NumV(n) if n.fract() == 0.0 => (*n as i64).is_odd().into_steelval(),
                _ => {
                    stop!(TypeMismatch => format!("odd? requires an integer, found: {:?}", &args[0]))
                }
            }
        })
    }

    pub fn integer_add() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                stop!(ArityMismatch => "+ requires at least one argument")
            }

            let mut sum = 0;

            for arg in args {
                if let SteelVal::IntV(n) = arg {
                    sum += n;
                } else {
                    stop!(TypeMismatch => "+ expected a number, found {:?}", arg);
                }
            }

            Ok(SteelVal::IntV(sum))
        })
    }

    pub fn integer_sub() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                stop!(ArityMismatch => "+ requires at least one argument")
            }

            let mut sum = if let SteelVal::IntV(n) = &args[0] {
                *n
            } else {
                stop!(TypeMismatch => "- expected a number, found {:?}", &args[0])
            };

            for arg in &args[1..] {
                if let SteelVal::IntV(n) = arg {
                    sum -= n;
                } else {
                    stop!(TypeMismatch => "+ expected a number, found {:?}", arg);
                }
            }

            Ok(SteelVal::IntV(sum))
        })
    }

    pub fn float_add() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.is_empty() {
                stop!(ArityMismatch => "+ requires at least one argument")
            }

            let mut sum = 0.0;

            for arg in args {
                if let SteelVal::NumV(n) = arg {
                    sum += n;
                } else {
                    stop!(TypeMismatch => "+ expected a number, found {:?}", arg);
                }
            }

            Ok(SteelVal::NumV(sum))
        })
    }

    pub fn adder() -> SteelVal {
        SteelVal::FuncV(add_primitive)
    }

    pub fn multiply() -> SteelVal {
        SteelVal::FuncV(multiply_primitive)
    }

    // TODO implement the full numerical tower
    // For now, only support division into floats
    pub fn divide() -> SteelVal {
        SteelVal::FuncV(divide_primitive)
    }

    pub fn subtract() -> SteelVal {
        SteelVal::FuncV(subtract_primitive)
    }
}

impl IntoSteelVal for BigInt {
    fn into_steelval(self) -> Result<SteelVal> {
        match self.to_isize() {
            Some(i) => i.into_steelval(),
            None => Ok(SteelVal::BigNum(crate::gc::Gc::new(self))),
        }
    }
}

#[cfg(test)]
mod num_op_tests {
    use super::*;
    use crate::rvals::SteelVal::*;
    use crate::throw;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        func.func_or_else(throw!(BadSyntax => "num op tests"))
            .unwrap()(&args)
    }

    #[test]
    fn division_test() {
        let args = vec![IntV(10), IntV(2)];

        let output = apply_function(NumOperations::divide(), args).unwrap();
        let expected = IntV(5);
        assert_eq!(output.to_string(), expected.to_string());
    }

    #[test]
    fn multiplication_test() {
        let args = vec![IntV(10), IntV(2)];

        let output = apply_function(NumOperations::multiply(), args).unwrap();
        let expected = IntV(20);
        assert_eq!(output, expected);
    }

    #[test]
    fn multiplication_different_types() {
        let args = vec![IntV(10), NumV(2.0)];

        let output = apply_function(NumOperations::multiply(), args).unwrap();
        let expected = NumV(20.0);
        assert_eq!(output.to_string(), expected.to_string());
    }

    #[test]
    fn addition_different_types() {
        let args = vec![IntV(10), NumV(2.0)];

        let output = apply_function(NumOperations::adder(), args).unwrap();
        let expected = NumV(12.0);
        assert_eq!(output.to_string(), expected.to_string());
    }

    #[test]
    fn subtraction_different_types() {
        let args = vec![IntV(10), NumV(2.0)];

        let output = apply_function(NumOperations::subtract(), args).unwrap();
        let expected = NumV(8.0);
        assert_eq!(output.to_string(), expected.to_string());
    }

    #[test]
    fn test_integer_add() {
        let args = vec![IntV(10), IntV(2)];

        let output = apply_function(NumOperations::integer_add(), args).unwrap();
        let expected = IntV(12);
        assert_eq!(output, expected);
    }

    #[test]
    fn test_integer_sub() {
        let args = vec![IntV(10), IntV(2)];

        let output = apply_function(NumOperations::integer_sub(), args).unwrap();
        let expected = IntV(8);
        assert_eq!(output, expected);
    }
}
