use crate::rvals::{IntoSteelVal, Result, SteelComplex, SteelVal};
use crate::steel_vm::primitives::{numberp, realp};
use crate::stop;
use num::{BigInt, BigRational, CheckedAdd, CheckedMul, Integer, Rational32, ToPrimitive};
use std::ops::Neg;

fn ensure_args_are_numbers(op: &str, args: &[SteelVal]) -> Result<()> {
    for arg in args {
        if !numberp(arg) {
            stop!(TypeMismatch => "{op} expects a number, found: {:?}", arg)
        }
    }
    Ok(())
}

/// Multiplies `x` and `y` without any type checking.
///
/// # Precondition
/// - `x` and `y` must be valid numerical types.
fn multiply_two(x: &SteelVal, y: &SteelVal) -> Result<SteelVal> {
    match (x, y) {
        (SteelVal::NumV(x), SteelVal::NumV(y)) => (x * y).into_steelval(),
        (SteelVal::NumV(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::NumV(x)) => {
            (x * *y as f64).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigNum(y)) | (SteelVal::BigNum(y), SteelVal::NumV(x)) => {
            (x * y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::Rational(y)) | (SteelVal::Rational(y), SteelVal::NumV(x)) => {
            (x * y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigRational(y))
        | (SteelVal::BigRational(y), SteelVal::NumV(x)) => {
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
        (SteelVal::IntV(x), SteelVal::Rational(y)) | (SteelVal::Rational(y), SteelVal::IntV(x)) => {
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
        (SteelVal::IntV(x), SteelVal::BigRational(y))
        | (SteelVal::BigRational(y), SteelVal::IntV(x)) => {
            let mut res = y.as_ref().clone();
            res *= BigInt::from(*x);
            res.into_steelval()
        }
        (SteelVal::Rational(x), SteelVal::Rational(y)) => match x.checked_mul(y) {
            Some(res) => res.into_steelval(),
            None => {
                let mut res = BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()));
                res *= BigRational::new(BigInt::from(*y.numer()), BigInt::from(*y.denom()));
                res.into_steelval()
            }
        },
        (SteelVal::Rational(x), SteelVal::BigNum(y))
        | (SteelVal::BigNum(y), SteelVal::Rational(x)) => {
            let mut res = BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()));
            res *= y.as_ref();
            res.into_steelval()
        }
        (SteelVal::BigRational(x), SteelVal::BigRational(y)) => {
            (x.as_ref() * y.as_ref()).into_steelval()
        }
        (SteelVal::BigRational(x), SteelVal::BigNum(y))
        | (SteelVal::BigNum(y), SteelVal::BigRational(x)) => {
            (x.as_ref() * y.as_ref()).into_steelval()
        }
        (SteelVal::BigNum(x), SteelVal::BigNum(y)) => (x.as_ref() * y.as_ref()).into_steelval(),
        // Complex numbers.
        (SteelVal::Complex(x), SteelVal::Complex(y)) => multiply_complex(x, y),
        (SteelVal::Complex(x), y) | (y, SteelVal::Complex(x)) => {
            let y = SteelComplex::new(y.clone(), SteelVal::IntV(0));
            multiply_complex(x, &y)
        }
        _ => unreachable!(),
    }
}

/// # Precondition
/// All types in `args` must be numerical types.
fn multiply_primitive_impl(args: &[SteelVal]) -> Result<SteelVal> {
    match args {
        [] => 1.into_steelval(),
        [x] => x.clone().into_steelval(),
        [x, y] => multiply_two(x, y).into_steelval(),
        [x, y, zs @ ..] => {
            let mut res = multiply_two(x, y)?;
            for z in zs {
                // TODO: This use case could be optimized to reuse state instead of creating a new
                // object each time.
                res = multiply_two(&res, &z)?;
            }
            res.into_steelval()
        }
    }
}

#[steel_derive::native(name = "*", constant = true, arity = "AtLeast(0)")]
pub fn multiply_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("*", args)?;
    multiply_primitive_impl(args)
}

pub fn quotient(l: isize, r: isize) -> isize {
    l / r
}

#[cold]
fn complex_reciprocal(c: &SteelComplex) -> Result<SteelVal> {
    println!("start: {c:?}");
    let denominator = add_two(&multiply_two(&c.re, &c.re)?, &multiply_two(&c.im, &c.im)?)?;
    println!("{denominator:?}");
    let re = divide_primitive(&[c.re.clone(), denominator.clone()])?;
    let neg_im = divide_primitive(&[c.re.clone(), denominator])?;
    SteelComplex::new(re, subtract_primitive(&[neg_im])?).into_steelval()
}

#[steel_derive::native(name = "/", constant = true, arity = "AtLeast(1)")]
pub fn divide_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("/", args)?;
    let recip = |x: &SteelVal| -> Result<SteelVal> {
        match x {
            SteelVal::IntV(n) => match i32::try_from(*n) {
                Ok(n) => Rational32::new(1, n).into_steelval(),
                Err(_) => BigRational::new(BigInt::from(1), BigInt::from(*n)).into_steelval(),
            },
            SteelVal::NumV(n) => n.recip().into_steelval(),
            SteelVal::Rational(r) => r.recip().into_steelval(),
            SteelVal::BigRational(r) => r.recip().into_steelval(),
            SteelVal::BigNum(n) => BigRational::new(1.into(), n.as_ref().clone()).into_steelval(),
            SteelVal::Complex(c) => complex_reciprocal(c),
            unexpected => {
                stop!(TypeMismatch => "/ expects a number, but found: {:?}", unexpected)
            }
        }
    };
    match &args {
        [] => stop!(ArityMismatch => "/ requires at least one argument"),
        [x] => recip(x),
        // TODO: Provide custom implementation to optimize by joining the multiply and recip calls.
        [x, y] => multiply_two(x, &recip(y)?),
        [x, ys @ ..] => {
            let d = multiply_primitive_impl(ys)?;
            multiply_two(&x, &recip(&d)?)
        }
    }
}

/// Negate a number.
///
/// # Precondition
/// `value` must be a number.
fn negate(value: &SteelVal) -> Result<SteelVal> {
    match value {
        SteelVal::NumV(x) => (-x).into_steelval(),
        SteelVal::IntV(x) => match x.checked_neg() {
            Some(res) => res.into_steelval(),
            None => BigInt::from(*x).neg().into_steelval(),
        },
        SteelVal::Rational(x) => match 0i32.checked_sub(*x.numer()) {
            Some(n) => Rational32::new(n, *x.denom()).into_steelval(),
            None => BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom()))
                .neg()
                .into_steelval(),
        },
        SteelVal::BigRational(x) => x.as_ref().neg().into_steelval(),
        SteelVal::BigNum(x) => x.as_ref().clone().neg().into_steelval(),
        SteelVal::Complex(x) => negate_complex(x),
        _ => unreachable!(),
    }
}

#[steel_derive::native(name = "-", constant = true, arity = "AtLeast(1)")]
pub fn subtract_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("-", args)?;
    match args {
        [] => stop!(TypeMismatch => "- requires at least one argument"),
        [x] => negate(x),
        [x, ys @ ..] => {
            let y = negate(&add_primitive(ys)?)?;
            add_two(x, &y)
        }
    }
}

/// Adds two numbers.
///
/// # Precondition
/// x and y must be valid numbers.
pub fn add_two(x: &SteelVal, y: &SteelVal) -> Result<SteelVal> {
    match (x, y) {
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
        (SteelVal::NumV(x), SteelVal::Rational(y)) | (SteelVal::Rational(y), SteelVal::NumV(x)) => {
            (x + y.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(x), SteelVal::BigRational(y))
        | (SteelVal::BigRational(y), SteelVal::NumV(x)) => {
            (x + y.to_f64().unwrap()).into_steelval()
        }
        // Cases that interact with `Rational`.
        (SteelVal::Rational(x), SteelVal::Rational(y)) => (x + y).into_steelval(),
        (SteelVal::Rational(x), SteelVal::IntV(y)) | (SteelVal::IntV(y), SteelVal::Rational(x)) => {
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
        (SteelVal::Rational(x), SteelVal::BigNum(y))
        | (SteelVal::BigNum(y), SteelVal::Rational(x)) => {
            let res =
                BigRational::new(BigInt::from(*x.numer()), BigInt::from(*x.denom())) * y.as_ref();
            res.into_steelval()
        }
        // Cases that interact with `BigRational`. For the sake of performance, hopefully not too
        // common.
        (SteelVal::BigRational(x), SteelVal::BigRational(y)) => {
            (x.as_ref() + y.as_ref()).into_steelval()
        }
        (SteelVal::BigRational(x), SteelVal::IntV(y))
        | (SteelVal::IntV(y), SteelVal::BigRational(x)) => {
            (x.as_ref() + BigInt::from(*y)).into_steelval()
        }
        (SteelVal::BigRational(x), SteelVal::BigNum(y))
        | (SteelVal::BigNum(y), SteelVal::BigRational(x)) => {
            (x.as_ref() * y.as_ref()).into_steelval()
        }
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
        // Complex numbers
        (SteelVal::Complex(x), SteelVal::Complex(y)) => add_complex(x, y),
        (SteelVal::Complex(x), y) | (y, SteelVal::Complex(x)) => {
            debug_assert!(realp(y));
            add_complex(x, &SteelComplex::new(y.clone(), SteelVal::IntV(0)))
        }
        _ => unreachable!(),
    }
}

#[steel_derive::native(name = "+", constant = true, arity = "AtLeast(0)")]
pub fn add_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("+", args)?;
    match args {
        [] => 0.into_steelval(),
        [x] => x.clone().into_steelval(),
        [x, y] => add_two(x, y),
        [x, y, zs @ ..] => {
            let mut res = add_two(x, y)?;
            for z in zs {
                res = add_two(&res, z)?;
            }
            res.into_steelval()
        }
    }
}

#[cold]
fn multiply_complex(x: &SteelComplex, y: &SteelComplex) -> Result<SteelVal> {
    // TODO: Optimize the implementation if needed.
    let real = add_two(
        &multiply_two(&x.re, &y.re)?,
        &negate(&multiply_two(&x.im, &y.im)?)?,
    )?;
    let im = add_two(&multiply_two(&x.re, &y.im)?, &multiply_two(&x.im, &y.re)?)?;
    SteelComplex::new(real, im).into_steelval()
}

#[cold]
fn negate_complex(x: &SteelComplex) -> Result<SteelVal> {
    // TODO: Optimize the implementation if needed.
    SteelComplex::new(negate(&x.re)?, negate(&x.im)?).into_steelval()
}

#[cold]
fn add_complex(x: &SteelComplex, y: &SteelComplex) -> Result<SteelVal> {
    // TODO: Optimize the implementation if needed.
    SteelComplex::new(add_two(&x.re, &y.re)?, add_two(&x.im, &y.im)?).into_steelval()
}

#[steel_derive::function(name = "exact?", constant = true)]
pub fn exactp(value: &SteelVal) -> bool {
    match value {
        SteelVal::IntV(_)
        | SteelVal::BigNum(_)
        | SteelVal::Rational(_)
        | SteelVal::BigRational(_) => true,
        SteelVal::Complex(x) => exactp(&x.re) && exactp(&x.im),
        _ => false,
    }
}

#[steel_derive::function(name = "inexact?", constant = true)]
pub fn inexactp(value: &SteelVal) -> bool {
    match value {
        SteelVal::NumV(_) => true,
        SteelVal::Complex(x) => inexactp(&x.re) || inexactp(&x.im),
        _ => false,
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
}

#[cfg(test)]
mod num_op_tests {
    use super::*;
    use crate::{gc::Gc, rvals::SteelVal::*};
    use std::str::FromStr;

    #[test]
    fn division_test() {
        assert_eq!(
            divide_primitive(&[IntV(10), IntV(2)]).unwrap().to_string(),
            IntV(5).to_string()
        );
    }

    #[test]
    fn division_on_single_integer_returns_reciprocal_rational() {
        assert_eq!(
            divide_primitive(&[IntV(10)]).unwrap().to_string(),
            Rational(Rational32::new(1, 10)).to_string()
        );
    }

    #[test]
    fn division_on_single_rational_returns_reciprocal_rational() {
        assert_eq!(
            divide_primitive(&[Rational32::new(2, 5).into_steelval().unwrap()])
                .unwrap()
                .to_string(),
            Rational(Rational32::new(5, 2)).to_string()
        );
    }

    #[test]
    fn division_on_rational_with_numerator_one_returns_integer() {
        assert_eq!(
            divide_primitive(&[Rational32::new(1, 5).into_steelval().unwrap()])
                .unwrap()
                .to_string(),
            IntV(5).to_string()
        );
    }

    #[test]
    fn division_on_bignum_returns_bigrational() {
        assert_eq!(
            divide_primitive(
                &([BigInt::from_str("18446744073709551616")
                    .unwrap()
                    .into_steelval()
                    .unwrap(),])
            )
            .unwrap()
            .to_string(),
            BigRational(Gc::new(num::BigRational::new(
                BigInt::from(1),
                BigInt::from_str("18446744073709551616").unwrap()
            )))
            .to_string()
        );
    }

    #[test]
    fn multiplication_test() {
        let args = [IntV(10), IntV(2)];
        let got = multiply_primitive(&args).unwrap();
        let expected = IntV(20);
        assert_eq!(got, expected);
    }

    #[test]
    fn multiplication_different_types() {
        let args = [IntV(10), NumV(2.0)];
        let got = multiply_primitive(&args).unwrap();
        let expected = NumV(20.0);
        assert_eq!(got.to_string(), expected.to_string());
    }

    #[test]
    fn multiply_multiple_numbers() {
        assert_eq!(
            multiply_primitive(&[IntV(16), NumV(2.0), Rational(Rational32::new(1, 4))])
                .unwrap()
                .to_string(),
            NumV(8.0).to_string(),
        );
    }

    #[test]
    fn adding_exact_with_inexact_returns_inexact() {
        assert_eq!(
            add_primitive(&([IntV(10), NumV(2.0)])).unwrap().to_string(),
            NumV(12.0).to_string()
        );
        assert_eq!(
            add_primitive(
                &([
                    BigInt::from_str("18446744073709551616")
                        .unwrap()
                        .into_steelval()
                        .unwrap(),
                    NumV(18446744073709551616.0),
                ])
            )
            .unwrap()
            .to_string(),
            NumV(18446744073709551616.0 * 2.0).to_string()
        );
        assert_eq!(
            add_primitive(
                &([
                    BigInt::from_str("18446744073709551616")
                        .unwrap()
                        .into_steelval()
                        .unwrap(),
                    NumV(18446744073709551616.0),
                ])
            )
            .unwrap()
            .to_string(),
            NumV(18446744073709551616.0 * 2.0).to_string()
        );
        assert_eq!(
            add_primitive(&([Rational32::new(1, 2).into_steelval().unwrap(), NumV(0.5),]))
                .unwrap()
                .to_string(),
            NumV(1.0).to_string()
        );
    }

    #[test]
    fn subtraction_different_types() {
        let args = [IntV(10), NumV(2.0)];
        let got = subtract_primitive(&args).unwrap();
        let expected = NumV(8.0);
        assert_eq!(got.to_string(), expected.to_string());
    }

    #[test]
    fn test_integer_add() {
        let args = [IntV(10), IntV(2)];
        let got = add_primitive(&args).unwrap();
        let expected = IntV(12);
        assert_eq!(got, expected);
    }

    #[test]
    fn test_integer_sub() {
        let args = [IntV(10), IntV(2)];
        let got = subtract_primitive(&args).unwrap();
        let expected = IntV(8);
        assert_eq!(got, expected);
    }
}
