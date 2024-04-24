use crate::rvals::{IntoSteelVal, Result, SteelComplex, SteelVal};
use crate::{steelerr, stop};
use num::Zero;
use num::{
    pow::Pow, BigInt, BigRational, CheckedAdd, CheckedMul, Integer, Rational32, Signed, ToPrimitive,
};
use std::ops::Neg;

#[steel_derive::function(name = "number?", constant = true)]
pub fn numberp(value: &SteelVal) -> bool {
    matches!(
        value,
        SteelVal::IntV(_)
            | SteelVal::BigNum(_)
            | SteelVal::Rational(_)
            | SteelVal::BigRational(_)
            | SteelVal::NumV(_)
            | SteelVal::Complex(_)
    )
}

#[steel_derive::function(name = "complex?", constant = true)]
pub fn complexp(value: &SteelVal) -> bool {
    numberp(value)
}

#[steel_derive::function(name = "real?", constant = true)]
pub fn realp(value: &SteelVal) -> bool {
    matches!(
        value,
        SteelVal::IntV(_)
            | SteelVal::BigNum(_)
            | SteelVal::Rational(_)
            | SteelVal::BigRational(_)
            | SteelVal::NumV(_)
    )
}

/// Returns #t if obj is a rational number, #f otherwise.
/// Rational numbers are numbers that can be expressed as the quotient of two numbers.
/// For example, 3/4, -5/2, 0.25, and 0 are rational numbers, while
///
/// (rational? value) -> bool?
///
/// Examples:
/// ```scheme
///   (rational? (/ 0.0)) ⇒ #f
///   (rational? 3.5)     ⇒ #t
///   (rational? 6/10)    ⇒ #t
///   (rational? 6/3)     ⇒ #t
/// ```
#[steel_derive::function(name = "rational?", constant = true)]
fn rationalp(value: &SteelVal) -> bool {
    match value {
        SteelVal::IntV(_)
        | SteelVal::BigNum(_)
        | SteelVal::Rational(_)
        | SteelVal::BigRational(_) => true,
        SteelVal::NumV(n) => n.is_finite(),
        _ => false,
    }
}

#[steel_derive::function(name = "int?", constant = true)]
fn intp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::IntV(_) | SteelVal::BigNum(_))
}

#[steel_derive::function(name = "integer?", constant = true)]
fn integerp(value: &SteelVal) -> bool {
    intp(value)
}

#[steel_derive::function(name = "exact-integer?", constant = true)]
fn exact_integerp(value: &SteelVal) -> bool {
    intp(value)
}

#[steel_derive::function(name = "float?", constant = true)]
fn floatp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::NumV(_))
}

/// Returns `#t` if the real number is Nan.
///
/// ```scheme
/// (nan? +nan.0) => #t
/// (nan? 100000) => #f
/// ```
#[steel_derive::function(name = "nan?", constant = true)]
fn nanp(value: &SteelVal) -> Result<SteelVal> {
    match value {
        SteelVal::NumV(n) => n.is_nan().into_steelval(),
        // The following types are numbers but can not be nan.
        SteelVal::IntV(_)
        | SteelVal::Rational(_)
        | SteelVal::BigNum(_)
        | SteelVal::BigRational(_) => false.into_steelval(),
        _ => steelerr!(TypeMismatch => "nan? expected real number"),
    }
}

/// Returns `#t` if the real number is 0 or 0.0.
///
/// ```scheme
/// (zero? 0  ) => #f
/// (zero? 0.0) => #t
/// (zero? 0.1) => #f
/// ```
#[steel_derive::function(name = "zero?", constant = true)]
fn zerop(value: &SteelVal) -> Result<SteelVal> {
    match value {
        SteelVal::NumV(x) => x.is_zero().into_steelval(),
        SteelVal::IntV(0) => true.into_steelval(),
        // The following types are numbers, but are casted to NumV or IntV if they are 0 by their
        // into_steelval implementation.
        SteelVal::IntV(_)
        | SteelVal::Rational(_)
        | SteelVal::BigNum(_)
        | SteelVal::BigRational(_)
        | SteelVal::Complex(_) => false.into_steelval(),
        _ => steelerr!(TypeMismatch => "zero? expected number"),
    }
}

/// Returns `#t` if the real number is positive.
///
/// ```scheme
/// (positive?  0) => #f
/// (positive?  1) => #t
/// (positive? -1) => #f
/// ```
#[steel_derive::function(name = "positive?", constant = true)]
fn positivep(value: &SteelVal) -> Result<SteelVal> {
    match value {
        SteelVal::NumV(n) => n.is_positive().into_steelval(),
        SteelVal::IntV(n) => n.is_positive().into_steelval(),
        SteelVal::Rational(n) => n.is_positive().into_steelval(),
        SteelVal::BigNum(n) => n.is_positive().into_steelval(),
        SteelVal::BigRational(n) => n.is_positive().into_steelval(),
        _ => steelerr!(TypeMismatch => "positive? expected real number"),
    }
}

/// Returns `#t` if the real number is negative.
///
/// ```scheme
/// (negative?  0) => #f
/// (negative?  1) => #f
/// (negative? -1) => #t
/// ```
#[steel_derive::function(name = "negative?", constant = true)]
fn negativep(value: &SteelVal) -> Result<SteelVal> {
    match value {
        SteelVal::NumV(n) => n.is_negative().into_steelval(),
        SteelVal::IntV(n) => n.is_negative().into_steelval(),
        SteelVal::Rational(n) => n.is_negative().into_steelval(),
        SteelVal::BigNum(n) => n.is_negative().into_steelval(),
        SteelVal::BigRational(n) => n.is_negative().into_steelval(),
        _ => steelerr!(TypeMismatch => "negative? expected real number"),
    }
}

#[steel_derive::native(name = "-", constant = true, arity = "AtLeast(1)")]
pub fn subtract_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("-", args)?;
    match args {
        [] => steelerr!(TypeMismatch => "- requires at least one argument"),
        [x] => negate(x),
        [x, ys @ ..] => {
            let y = negate(&add_primitive(ys)?)?;
            add_two(x, &y)
        }
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

#[steel_derive::native(name = "*", constant = true, arity = "AtLeast(0)")]
pub fn multiply_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("*", args)?;
    multiply_primitive_impl(args)
}

#[steel_derive::native(name = "quotient", constant = true, arity = "Exact(2)")]
pub fn quotient(args: &[SteelVal]) -> Result<SteelVal> {
    match &args {
        [l, r] => match (l, r) {
            (SteelVal::IntV(l), SteelVal::IntV(r)) => (l / r).into_steelval(),
            _ => steelerr!(TypeMismatch => "quotient only supports integers"),
        },
        _ => steelerr!(ArityMismatch => "quotient requires 2 arguments"),
    }
}

#[steel_derive::native(name = "/", constant = true, arity = "AtLeast(1)")]
pub fn divide_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("/", args)?;
    let recip = |x: &SteelVal| -> Result<SteelVal> {
        match x {
            SteelVal::IntV(n) => match i32::try_from(*n) {
                Ok(0) => {
                    stop!(Generic => "/: division by zero")
                }
                Ok(n) => Rational32::new(1, n).into_steelval(),
                Err(_) => BigRational::new(BigInt::from(1), BigInt::from(*n)).into_steelval(),
            },
            SteelVal::NumV(n) => n.recip().into_steelval(),
            SteelVal::Rational(r) => r.recip().into_steelval(),
            SteelVal::BigRational(r) => r.recip().into_steelval(),
            SteelVal::BigNum(n) => BigRational::new(1.into(), n.as_ref().clone()).into_steelval(),
            SteelVal::Complex(c) => complex_reciprocal(c),
            unexpected => {
                steelerr!(TypeMismatch => "/ expects a number, but found: {:?}", unexpected)
            }
        }
    };
    match &args {
        [] => steelerr!(ArityMismatch => "/ requires at least one argument"),
        [x] => recip(x),
        // TODO: Provide custom implementation to optimize by joining the multiply and recip calls.
        [x, y] => multiply_two(x, &recip(y)?),
        [x, ys @ ..] => {
            let d = multiply_primitive_impl(ys)?;
            multiply_two(&x, &recip(&d)?)
        }
    }
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

fn number_to_float(number: &SteelVal) -> Result<f64> {
    let res = match number {
        SteelVal::IntV(i) => *i as f64,
        SteelVal::Rational(f) => f.to_f64().unwrap(),
        SteelVal::BigRational(f) => f.to_f64().unwrap(),
        SteelVal::NumV(n) => *n,
        SteelVal::BigNum(n) => n.to_f64().unwrap(),
        _ => stop!(TypeMismatch => "number->float expects a real number, found: {}", number),
    };
    Ok(res)
}

#[steel_derive::function(name = "exact->inexact", constant = true)]
fn exact_to_inexact(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(i) => (*i as f64).into_steelval(),
        SteelVal::Rational(f) => f.to_f64().unwrap().into_steelval(),
        SteelVal::BigRational(f) => f.to_f64().unwrap().into_steelval(),
        SteelVal::NumV(n) => n.into_steelval(),
        SteelVal::BigNum(n) => Ok(SteelVal::NumV(n.to_f64().unwrap())),
        SteelVal::Complex(x) => {
            SteelComplex::new(exact_to_inexact(&x.re)?, exact_to_inexact(&x.im)?).into_steelval()
        }
        _ => steelerr!(TypeMismatch => "exact->inexact expects a number type, found: {}", number),
    }
}

#[steel_derive::function(name = "inexact->exact", constant = true)]
fn inexact_to_exact(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(x) => x.into_steelval(),
        SteelVal::Rational(x) => x.into_steelval(),
        SteelVal::BigRational(x) => SteelVal::BigRational(x.clone()).into_steelval(),
        SteelVal::NumV(x) => {
            let x_isize = *x as isize;
            if x_isize as f64 == *x {
                return x_isize.into_steelval();
            }
            BigRational::from_float(*x).into_steelval()
        }
        SteelVal::BigNum(x) => SteelVal::BigNum(x.clone()).into_steelval(),
        SteelVal::Complex(x) => {
            SteelComplex::new(inexact_to_exact(&x.re)?, inexact_to_exact(&x.im)?).into_steelval()
        }
        _ => steelerr!(TypeMismatch => "exact->inexact expects a number type, found: {}", number),
    }
}

fn finitep_impl(number: &SteelVal) -> Result<bool> {
    match number {
        SteelVal::NumV(x) if x.is_nan() || x.is_infinite() => Ok(false),
        SteelVal::IntV(_)
        | SteelVal::NumV(_)
        | SteelVal::BigNum(_)
        | SteelVal::Rational(_)
        | SteelVal::BigRational(_) => Ok(true),
        SteelVal::Complex(x) => Ok(finitep_impl(&x.re)? && finitep_impl(&x.im)?),
        _ => steelerr!(TypeMismatch => "finite? expects a number, found: {}", number),
    }
}

#[steel_derive::function(name = "finite?", constant = true)]
fn finitep(number: &SteelVal) -> Result<SteelVal> {
    finitep_impl(number).into_steelval()
}

fn infinitep_impl(number: &SteelVal) -> Result<bool> {
    match number {
        SteelVal::NumV(x) if x.is_infinite() => Ok(true),
        SteelVal::IntV(_)
        | SteelVal::NumV(_)
        | SteelVal::BigNum(_)
        | SteelVal::Rational(_)
        | SteelVal::BigRational(_) => Ok(false),
        SteelVal::Complex(n) => Ok(infinitep_impl(&n.re)? || infinitep_impl(&n.im)?),
        _ => steelerr!(TypeMismatch => "exact->inexact expects a real number, found: {}", number),
    }
}

#[steel_derive::function(name = "infinite?", constant = true)]
fn infinitep(number: &SteelVal) -> Result<SteelVal> {
    infinitep_impl(number)?.into_steelval()
}

/// Returns the absolute value of the given input
#[steel_derive::function(name = "abs", constant = true)]
fn abs(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(i) => Ok(SteelVal::IntV(i.abs())),
        SteelVal::NumV(n) => Ok(SteelVal::NumV(n.abs())),
        SteelVal::Rational(f) => f.abs().into_steelval(),
        SteelVal::BigRational(f) => f.abs().into_steelval(),
        SteelVal::BigNum(n) => n.as_ref().abs().into_steelval(),
        _ => steelerr!(TypeMismatch => "abs expects a real number, found: {}", number),
    }
}

#[steel_derive::function(name = "ceiling", constant = true)]
fn ceiling(number: &SteelVal) -> Result<SteelVal> {
    match number {
        n @ SteelVal::IntV(_) | n @ SteelVal::BigNum(_) => Ok(n.clone()),
        SteelVal::NumV(n) => Ok(SteelVal::NumV(n.ceil())),
        SteelVal::Rational(f) => f.ceil().into_steelval(),
        SteelVal::BigRational(f) => f.ceil().into_steelval(),
        _ => steelerr!(TypeMismatch => "ceiling expects a real number, found: {}", number),
    }
}

#[steel_derive::function(name = "denominator", constant = true)]
fn denominator(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(_) | SteelVal::BigNum(_) => 1.into_steelval(),
        SteelVal::NumV(_) => {
            steelerr!(TypeMismatch => "denominator not supported for number {}", number)
        }
        SteelVal::Rational(f) => f.denom().into_steelval(),
        SteelVal::BigRational(f) => f.denom().clone().into_steelval(),
        _ => steelerr!(TypeMismatch => "ceiling expects a real number, found: {}", number),
    }
}

// TODO: Add support for BigNum.
#[steel_derive::function(name = "expt", constant = true)]
fn expt(left: &SteelVal, right: &SteelVal) -> Result<SteelVal> {
    match (left, right) {
        (SteelVal::IntV(l), SteelVal::IntV(r)) => match u32::try_from(*r) {
            Ok(r) => l.pow(r).into_steelval(),
            Err(_) => (*l as f64).powf(*r as f64).into_steelval(),
        },
        (SteelVal::IntV(l), SteelVal::NumV(r)) => (*l as f64).powf(*r).into_steelval(),
        (SteelVal::IntV(l), SteelVal::Rational(r)) => {
            (*l as f64).powf(r.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::IntV(l), SteelVal::BigNum(r)) => {
            (*l as f64).powf(r.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::IntV(l), SteelVal::BigRational(r)) => {
            (*l as f64).powf(r.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::NumV(l), SteelVal::NumV(r)) => Ok(SteelVal::NumV(l.powf(*r))),
        (SteelVal::NumV(l), SteelVal::IntV(r)) => match i32::try_from(*r) {
            Ok(r) => l.powi(r).into_steelval(),
            Err(_) => l.powf(*r as f64).into_steelval(),
        },
        (SteelVal::NumV(l), SteelVal::Rational(r)) => l.powf(r.to_f64().unwrap()).into_steelval(),
        (SteelVal::NumV(l), SteelVal::BigNum(r)) => l.powf(r.to_f64().unwrap()).into_steelval(),
        (SteelVal::NumV(l), SteelVal::BigRational(r)) => {
            l.powf(r.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::Rational(l), SteelVal::Rational(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::Rational(l), SteelVal::NumV(r)) => l.to_f64().unwrap().powf(*r).into_steelval(),
        (SteelVal::Rational(l), SteelVal::IntV(r)) => match i32::try_from(*r) {
            Ok(r) => l.pow(r).into_steelval(),
            Err(_) => {
                let base = BigRational::new(BigInt::from(*l.numer()), BigInt::from(*l.denom()));
                let exp = BigInt::from(*r);
                base.pow(exp).into_steelval()
            }
        },
        (SteelVal::Rational(l), SteelVal::BigNum(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::Rational(l), SteelVal::BigRational(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::BigNum(l), SteelVal::BigNum(r)) => match r.as_ref().sign() {
            num::bigint::Sign::NoSign | num::bigint::Sign::Plus => l
                .as_ref()
                .clone()
                .pow(r.as_ref().magnitude())
                .into_steelval(),
            num::bigint::Sign::Minus => Ok(SteelVal::NumV(
                l.to_f64().unwrap().powf(r.to_f64().unwrap()),
            )),
        },
        (SteelVal::BigNum(l), SteelVal::IntV(r)) => match *r {
            0 => 1.into_steelval(),
            r if r < 0 => Ok(SteelVal::NumV(
                l.to_f64().unwrap().powf(r.to_f64().unwrap()),
            )),
            r => l.as_ref().clone().pow(r as usize).into_steelval(),
        },
        (SteelVal::BigNum(l), SteelVal::NumV(r)) => l.to_f64().unwrap().powf(*r).into_steelval(),
        (SteelVal::BigNum(l), SteelVal::Rational(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::BigNum(l), SteelVal::BigRational(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::BigRational(l), SteelVal::Rational(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::BigRational(l), SteelVal::NumV(r)) => {
            l.to_f64().unwrap().powf(*r).into_steelval()
        }
        (SteelVal::BigRational(l), SteelVal::IntV(r)) => match i32::try_from(*r) {
            Ok(r) => l.as_ref().pow(r).into_steelval(),
            Err(_) => {
                let exp = BigInt::from(*r);
                l.as_ref().clone().pow(exp).into_steelval()
            }
        },
        (SteelVal::BigRational(l), SteelVal::BigNum(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (SteelVal::BigRational(l), SteelVal::BigRational(r)) => l
            .to_f64()
            .unwrap()
            .powf(r.to_f64().unwrap())
            .into_steelval(),
        (l, r) => {
            steelerr!(TypeMismatch => "expt expected two numbers but found {} and {}", l, r)
        }
    }
}

/// Returns Euler's number raised to the power of z.
#[steel_derive::function(name = "exp", constant = true)]
fn exp(left: &SteelVal) -> Result<SteelVal> {
    match left {
        SteelVal::IntV(0) => Ok(SteelVal::IntV(1)),
        SteelVal::IntV(l) if *l < i32::MAX as isize => {
            Ok(SteelVal::NumV(std::f64::consts::E.powi(*l as i32)))
        }
        maybe_number => match number_to_float(maybe_number) {
            Ok(n) => Ok(SteelVal::NumV(std::f64::consts::E.powf(n))),
            Err(_) => steelerr!(Generic => "exp expected a real number"),
        },
    }
}

#[steel_derive::function(name = "floor", constant = true)]
fn floor(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::NumV(x) => Ok(SteelVal::NumV(x.floor())),
        SteelVal::IntV(x) => x.into_steelval(),
        SteelVal::Rational(x) => x.floor().into_steelval(),
        SteelVal::BigNum(x) => Ok(SteelVal::BigNum(x.clone())),
        SteelVal::BigRational(x) => x.floor().into_steelval(),
        _ => steelerr!(Generic => "floor expected a real number"),
    }
}

#[steel_derive::function(name = "numerator", constant = true)]
fn numerator(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(x) => x.into_steelval(),
        SteelVal::Rational(x) => (*x.numer() as isize).into_steelval(),
        SteelVal::BigNum(x) => Ok(SteelVal::BigNum(x.clone())),
        SteelVal::BigRational(x) => (x.numer().clone()).into_steelval(),
        _ => steelerr!(Generic => "numerator expects a integer or rational number"),
    }
}

// Docs from racket:
// (round x) → (or/c integer? +inf.0 -inf.0 +nan.0)
//   x : real?
// Returns the integer closest to x, resolving ties in favor of an even number, but +inf.0, -inf.0, and +nan.0 round to themselves.
#[steel_derive::function(name = "round", constant = true)]
fn round(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(i) => i.into_steelval(),
        SteelVal::NumV(n) => n.round().into_steelval(),
        SteelVal::Rational(f) => f.round().into_steelval(),
        SteelVal::BigRational(f) => f.round().into_steelval(),
        SteelVal::BigNum(n) => Ok(SteelVal::BigNum(n.clone())),
        _ => steelerr!(TypeMismatch => "round expects a real number, found: {}", number),
    }
}

/// Squares a number. This is equivalent to `(* x x)`
#[steel_derive::function(name = "square", constant = true)]
fn square(number: &SteelVal) -> Result<SteelVal> {
    if !numberp(number) {
        stop!(TypeMismatch => "square expects a number, found: {:?}", number)
    }
    multiply_two(&number, &number)
}

/// Takes a number and returns the square root. If the number is negative, then a complex number may
/// be returned.
///
/// ```scheme
/// (sqrt  -1)   => 0+1i
/// (sqrt   4)   => 2
/// (sqrt   2)   => 1.414..
/// (sqrt 4/9)   => 2/3
/// (sqrt -3-4i) => 1-2i
/// ```
#[steel_derive::function(name = "sqrt", constant = true)]
fn sqrt(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::NumV(x) => {
            if x.is_negative() {
                let imag = x.neg().sqrt();
                SteelComplex::new(0.0.into_steelval()?, imag.into_steelval()?).into_steelval()
            } else {
                x.sqrt().into_steelval()
            }
        }
        SteelVal::IntV(x) => {
            if x.is_negative() {
                let sqrt = (*x as f64).abs().sqrt();
                if sqrt as isize as f64 == sqrt {
                    SteelComplex::new(0.into_steelval()?, (sqrt as isize).into_steelval()?)
                        .into_steelval()
                } else {
                    SteelComplex::new(0.into_steelval()?, sqrt.into_steelval()?).into_steelval()
                }
            } else {
                let sqrt = (*x as f64).sqrt();
                if sqrt as isize as f64 == sqrt {
                    (sqrt as isize).into_steelval()
                } else {
                    sqrt.into_steelval()
                }
            }
        }
        SteelVal::Rational(x) => {
            let n = x.numer().abs();
            let d = *x.denom();
            let n_sqrt = (n as f64).sqrt();
            let d_sqrt = (d as f64).sqrt();
            let sqrt = if n_sqrt as i32 as f64 == n_sqrt && d_sqrt as i32 as f64 == d_sqrt {
                Rational32::new(n_sqrt as i32, d_sqrt as i32).into_steelval()?
            } else {
                (n_sqrt / d_sqrt).into_steelval()?
            };
            if x.is_negative() {
                let re = if exactp(&sqrt) {
                    0.into_steelval()?
                } else {
                    0.0.into_steelval()?
                };
                SteelComplex::new(re, sqrt).into_steelval()
            } else {
                Ok(sqrt)
            }
        }
        SteelVal::BigNum(n) => {
            let sqrt = n.as_ref().to_f64().unwrap().sqrt();
            if n.as_ref().is_negative() {
                SteelComplex::new(0.0.into_steelval()?, sqrt.into_steelval()?).into_steelval()
            } else {
                sqrt.into_steelval()
            }
        }
        SteelVal::BigRational(n) => {
            let sqrt = n.as_ref().to_f64().unwrap().sqrt();
            if n.as_ref().is_negative() {
                SteelComplex::new(0.0.into_steelval()?, sqrt.into_steelval()?).into_steelval()
            } else {
                sqrt.into_steelval()
            }
        }
        SteelVal::Complex(n) => {
            let z_mag = magnitude(number)?;
            let half = Rational32::new(1, 2).into_steelval()?;
            let re = sqrt(&multiply_two(&add_two(&z_mag, &n.re)?, &half)?)?;
            let im = sqrt(&multiply_two(&add_two(&z_mag, &negate(&n.re)?)?, &half)?)?;
            if negativep(&n.im)? == SteelVal::BoolV(true) {
                SteelComplex::new(re, negate(&im)?).into_steelval()
            } else {
                SteelComplex::new(re, im).into_steelval()
            }
        }
        _ => steelerr!(TypeMismatch => "sqrt expected a number"),
    }
}

/// Returns the magnitude of the number. For real numbers, this is equvalent to `(abs x)`. For
/// complex numbers this returns its distance from `(0, 0)` in the complex plane.
///
/// ```scheme
/// (magnitude -1/3) => 1/3
/// (magnitude 3+4i) => 5
/// ```
#[steel_derive::function(name = "magnitude", constant = true)]
fn magnitude(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::NumV(x) => x.abs().into_steelval(),
        SteelVal::IntV(x) => x.abs().into_steelval(),
        SteelVal::Rational(x) => x.abs().into_steelval(),
        SteelVal::BigNum(x) => x.as_ref().abs().into_steelval(),
        SteelVal::BigRational(x) => x.as_ref().abs().into_steelval(),
        SteelVal::Complex(x) => {
            let c_squared = add_two(&square(&x.re)?, &square(&x.im)?)?;
            sqrt(&c_squared)
        }
        _ => steelerr!(TypeMismatch => "magnitude expects a number, found {number}"),
    }
}

#[steel_derive::native(name = "log", arity = "AtLeast(1)")]
fn log(args: &[SteelVal]) -> Result<SteelVal> {
    if args.len() > 2 {
        stop!(ArityMismatch => "log expects one or two arguments, found: {}", args.len());
    }

    let first = &args[0];
    let base = args
        .get(1)
        .cloned()
        .unwrap_or(SteelVal::NumV(std::f64::consts::E));

    match (first, &base) {
        (SteelVal::IntV(1), _) => Ok(SteelVal::IntV(0)),
        (SteelVal::IntV(_) | SteelVal::NumV(_), SteelVal::IntV(1)) => {
            steelerr!(Generic => "log: divide by zero with args: {} and {}", first, base)
        }
        (SteelVal::IntV(arg), SteelVal::NumV(n)) => Ok(SteelVal::NumV((*arg as f64).log(*n))),
        (SteelVal::IntV(arg), SteelVal::IntV(base)) => Ok(SteelVal::IntV(arg.ilog(*base) as isize)),
        (SteelVal::NumV(arg), SteelVal::NumV(n)) => Ok(SteelVal::NumV(arg.log(*n))),
        (SteelVal::NumV(arg), SteelVal::IntV(base)) => Ok(SteelVal::NumV(arg.log(*base as f64))),
        // TODO: Support BigNum, Rational, and BigRational.
        _ => {
            steelerr!(TypeMismatch => "log expects one or two numbers, found: {} and {}", first, base)
        }
    }
}

/// Returns an integer that is closest (but not greater than) the square root of an integer and the
/// remainder.
///
/// ```scheme
/// (exact-integer-sqrt x) => '(root rem)
/// (equal? x (+ (square root) rem)) => #t
/// ```
#[steel_derive::function(name = "exact-integer-sqrt", constant = true)]
fn exact_integer_sqrt(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(x) if *x >= 0 => {
            let (ans, rem) = exact_integer_impl(x);
            (ans.into_steelval()?, rem.into_steelval()?).into_steelval()
        }
        SteelVal::BigNum(x) if !x.is_negative() => {
            let (ans, rem) = exact_integer_impl(x.as_ref());
            (ans.into_steelval()?, rem.into_steelval()?).into_steelval()
        }
        _ => {
            steelerr!(TypeMismatch => "exact-integer-sqrt expects a non-negative integer but found {number}")
        }
    }
}

fn exact_integer_impl<'a, N>(target: &'a N) -> (N, N)
where
    N: num::integer::Roots + Clone,
    &'a N: std::ops::Mul<&'a N, Output = N>,
    N: std::ops::Sub<N, Output = N>,
{
    let x = target.sqrt();
    let x_sq = x.clone() * x.clone();
    let rem = target.clone() - x_sq;
    (x, rem)
}

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

#[cold]
fn complex_reciprocal(c: &SteelComplex) -> Result<SteelVal> {
    let denominator = add_two(&multiply_two(&c.re, &c.re)?, &multiply_two(&c.im, &c.im)?)?;
    let re = divide_primitive(&[c.re.clone(), denominator.clone()])?;
    let neg_im = divide_primitive(&[c.re.clone(), denominator])?;
    SteelComplex::new(re, subtract_primitive(&[neg_im])?).into_steelval()
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
                _ => steelerr!(TypeMismatch => "arithmetic-shift expected 2 integers"),
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
                    steelerr!(TypeMismatch => format!("even? requires an integer, found: {:?}", &args[0]))
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
                    steelerr!(TypeMismatch => format!("odd? requires an integer, found: {:?}", &args[0]))
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
    fn dvision_by_integer_zero_returns_positive_infinity() {
        // assert_eq!(
        //     divide_primitive(&[IntV(1), IntV(0)]).unwrap().to_string(),
        //     NumV(f64::INFINITY).to_string()
        // )

        assert!(divide_primitive(&[IntV(1), IntV(0)]).is_err())
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

    #[test]
    fn test_exact_integer_sqrt() {
        assert_eq!(
            exact_integer_sqrt(&0.into()),
            (0.into_steelval().unwrap(), 0.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&1.into()),
            (1.into_steelval().unwrap(), 0.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&2.into()),
            (1.into_steelval().unwrap(), 1.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&2.into()),
            (1.into_steelval().unwrap(), 1.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&3.into()),
            (1.into_steelval().unwrap(), 2.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&4.into()),
            (2.into_steelval().unwrap(), 0.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&5.into()),
            (2.into_steelval().unwrap(), 1.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&6.into()),
            (2.into_steelval().unwrap(), 2.into_steelval().unwrap()).into_steelval()
        );
        assert_eq!(
            exact_integer_sqrt(&7.into()),
            (2.into_steelval().unwrap(), 3.into_steelval().unwrap()).into_steelval()
        );
    }

    #[test]
    fn test_exact_integer_sqrt_fails_on_negative_or_noninteger() {
        assert!(exact_integer_sqrt(&(-7).into()).is_err());
        assert!(exact_integer_sqrt(&Rational32::new(-1, 2).into_steelval().unwrap()).is_err());
        assert!(exact_integer_sqrt(
            &BigInt::from_str("-10000000000000000000000000000000000001")
                .unwrap()
                .into_steelval()
                .unwrap()
        )
        .is_err());
        assert!(exact_integer_sqrt(
            &num::BigRational::new(
                BigInt::from_str("-10000000000000000000000000000000000001").unwrap(),
                BigInt::from_str("2").unwrap()
            )
            .into_steelval()
            .unwrap()
        )
        .is_err());
        assert!(exact_integer_sqrt(&(1.0).into()).is_err());
        assert!(exact_integer_sqrt(
            &SteelComplex::new(1.into(), 1.into())
                .into_steelval()
                .unwrap()
        )
        .is_err());
    }

    #[test]
    fn test_sqrt() {
        assert_eq!(sqrt(&4isize.into()).unwrap(), 2isize.into());
        assert_eq!(
            sqrt(
                &SteelComplex::new(0.into(), 2.into())
                    .into_steelval()
                    .unwrap()
            )
            .unwrap(),
            SteelComplex::new(1.into(), 1.into())
                .into_steelval()
                .unwrap()
        );
        assert_eq!(
            sqrt(
                &SteelComplex::new((-3).into(), (-4).into())
                    .into_steelval()
                    .unwrap()
            )
            .unwrap(),
            SteelComplex::new(1.into(), (-2).into())
                .into_steelval()
                .unwrap()
        );
    }
}
