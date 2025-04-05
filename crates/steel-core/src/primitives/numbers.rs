use crate::rvals::{IntoSteelVal, Result, SteelComplex, SteelVal};
use crate::{steelerr, stop};
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::{BigRational, Rational32};
use num_traits::{pow::Pow, CheckedAdd, CheckedMul, Signed, ToPrimitive, Zero};
use std::ops::Neg;

/// Checks if the given value is a number
///
/// (number? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (number? 42) ;; => #t
/// > (number? "hello") ;; => #f
/// > (number? 'symbol) ;; => #f
/// ```
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

/// Checks if the given value is a complex number
///
/// (complex? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (complex? 3+4i) ;; => #t
/// > (complex? 42) ;; => #t
/// > (complex? "hello") ;; => #f
/// ```
#[steel_derive::function(name = "complex?", constant = true)]
pub fn complexp(value: &SteelVal) -> bool {
    numberp(value)
}

/// Checks if the given value is a real number
///
/// (real? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (real? 42) ;; => #t
/// > (real? 3+4i) ;; => #f
/// > (real? "hello") ;; => #f
/// ```
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
/// For example, 3/4, -5/2, 0.25, and 0 are rational numbers.
///
/// (rational? value) -> bool?
///
/// * value : any - The value to check
///
/// Examples:
/// ```scheme
/// > (rational? (/ 0.0)) ;; => #f
/// > (rational? 3.5) ;; => #t
/// > (rational? 6/10) ;; => #t
/// > (rational? +nan.0) ;; => #f
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

/// Checks if the given value is an integer, an alias for `integer?`
///
/// (int? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (int? 42) ;; => #t
/// > (int? 3.14) ;; => #f
/// > (int? "hello") ;; => #f
/// ```
#[steel_derive::function(name = "int?", constant = true)]
fn intp(value: &SteelVal) -> bool {
    match value {
        SteelVal::IntV(_) | SteelVal::BigNum(_) => true,
        SteelVal::NumV(n) if n.fract() == 0.0 => true,
        _ => false,
    }
}

/// Checks if the given value is an integer, an alias for `int?`
///
/// (integer? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (integer? 42) ;; => #t
/// > (integer? 3.14) ;; => #f
/// > (integer? "hello") ;; => #f
/// ```
#[steel_derive::function(name = "integer?", constant = true)]
fn integerp(value: &SteelVal) -> bool {
    intp(value)
}

/// Checks if the given value is an exact integer
///
/// (exact-integer? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (exact-integer? 42) ;; => #t
/// > (exact-integer? -42) ;; => #t
/// > (exact-integer? 4.0) ;; => #f
/// ```
#[steel_derive::function(name = "exact-integer?", constant = true)]
fn exact_integerp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::IntV(_) | SteelVal::BigNum(_))
}

/// Checks if the given value is a floating-point number
///
/// (float? value) -> boolean?
///
/// * value : any - The value to check
///
/// # Examples
/// ```scheme
/// > (float? 42) ;; => #f
/// > (float? 3.14) ;; => #t
/// > (float? #t) ;; => #f
/// ```
#[steel_derive::function(name = "float?", constant = true)]
fn floatp(value: &SteelVal) -> bool {
    matches!(value, SteelVal::NumV(_))
}

/// Returns `#t` if the real number is Nan.
///
/// (nan? value) -> boolean?
///
/// * value : real? - The value to check
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

/// Checks if the given real number is zero.
///
/// (zero? num) -> boolean?
///
/// * num : real? - The number to check for zero.
///
/// # Examples
/// ```scheme
/// > (zero? 0) ;; => #t
/// > (zero? 0.0) ;; => #t
/// > (zero? 0.1) ;; => #f
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

/// Checks if the given real number is positive.
///
/// (positive? num) -> boolean?
///
/// * num : real? - The real number to check for positivity.
///
/// # Examples
/// ```scheme
/// > (positive? 0) ;; => #f
/// > (positive? 1) ;; => #t
/// > (positive? -1) ;; => #f
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

/// Checks if the given real number is negative.
///
/// (negative? num) -> boolean?
///
/// * num : real? - The real number to check for negativity.
///
/// # Examples
/// ```scheme
/// > (negative? 0) ;; => #f
/// > (negative? 1) ;; => #f
/// > (negative? -1) ;; => #t
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

/// Subtracts the given numbers.
///
/// (- . nums) -> number?
///
/// * nums : number? - The numbers to subtract. Must have at least one number.
///
/// # Examples
/// ```scheme
/// > (- 5 3) ;; => 2
/// > (- 10 3 2) ;; => 5
/// > (- -5) ;; => 5
/// ```
#[steel_derive::native(name = "-", constant = true, arity = "AtLeast(1)")]
pub fn subtract_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("-", args)?;
    match args {
        [] => steelerr!(TypeMismatch => "- requires at least one argument"),
        [x] => negate(x),
        [x, ys @ ..] => {
            let y = negate(&add_primitive_no_check(ys)?)?;
            add_two(x, &y)
        }
    }
}

#[inline(always)]
fn add_primitive_no_check(args: &[SteelVal]) -> Result<SteelVal> {
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

/// Adds the given numbers.
///
/// (+ . nums) -> number?
///
/// * nums : number? - The numbers to add. Can have any number of arguments including zero.
///
/// # Examples
/// ```scheme
/// > (+ 5 3) ;; => 8
/// > (+ 10 3 2) ;; => 15
/// > (+) ;; => 0
/// ```
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

/// Multiplies the given numbers.
///
/// (* . nums) -> number?
///
/// * nums : number? - The numbers to multiply. Can have any number of arguments including zero.
///
/// # Examples
/// ```scheme
/// > (* 5 3) ;; => 15
/// > (* 10 3 2) ;; => 60
/// > (*) ;; => 1
/// ```
#[steel_derive::native(name = "*", constant = true, arity = "AtLeast(0)")]
pub fn multiply_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    ensure_args_are_numbers("*", args)?;
    multiply_primitive_impl(args)
}

/// Returns quotient of dividing numerator by denomintator.
///
/// (quotient numerator denominator) -> integer?
///
/// * numerator : integer? - The numerator.
/// * denominator : integer? - The denominator.
///
/// # Examples
/// ```scheme
/// > (quotient 11 2) ;; => 5
/// > (quotient 10 2) ;; => 5
/// > (quotient -10 2) ;; => -5
/// ```
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

/// Returns the euclidean remainder of the division of the first number by the second
/// This differs from the remainder operator when using negative numbers.
///
/// (modulo n m) -> integer?
///
/// * n : integer?
/// * m : integer?
///
/// # Examples
/// ```scheme
/// > (modulo 10 3) ;; => 1
/// > (modulo -10 3) ;; => 2
/// > (modulo 10 -3) ;; => -2
/// > (module -10 -3) ;; => -1
/// ```
#[steel_derive::native(name = "modulo", constant = true, arity = "Exact(2)")]
pub fn modulo(args: &[SteelVal]) -> Result<SteelVal> {
    match &args {
        [l, r] => match (l, r) {
            (SteelVal::IntV(l), SteelVal::IntV(r)) => ((l % r + r) % r).into_steelval(),
            _ => steelerr!(TypeMismatch => "modulo only supports integers"),
        },
        _ => steelerr!(ArityMismatch => "modulo requires 2 arguments"),
    }
}

/// Returns the arithmetic remainder of the division of the first number by the second.
/// This differs from the modulo operator when using negative numbers.
///
/// (remainder n m) -> integer?
///
/// * n : integer?
/// * m : integer?
///
/// # Examples
/// ```scheme
/// > (remainder 10 3) ;; => 1
/// > (remainder -10 3) ;; => -1
/// > (remainder 10 -3) ;; => 1
/// > (remainder -10 -3) ;; => -1
/// ```
#[steel_derive::native(name = "remainder", constant = true, arity = "Exact(2)")]
pub fn remainder(args: &[SteelVal]) -> Result<SteelVal> {
    match &args {
        [l, r] => match (l, r) {
            (SteelVal::IntV(l), SteelVal::IntV(r)) => (l % r).into_steelval(),
            _ => steelerr!(TypeMismatch => "remainder only supports integers"),
        },
        _ => steelerr!(ArityMismatch => "remainder requires 2 arguments"),
    }
}

/// Returns the sine value of the input angle, measured in radians.
///
/// (sin n) -> number?
///
/// * n : number? - The input angle, in radians.
///
/// # Examples
/// ```scheme
/// > (sin 0) ;; => 0
/// > (sin 1) ;; => 0.8414709848078965
/// > (sin 2.0) ;; => 0.9092974268256817
/// > (sin 3.14) ;; => 0.0015926529164868282
/// ```
#[steel_derive::function(name = "sin", constant = true)]
pub fn sin(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(i) => (*i as f64).sin(),
        SteelVal::BigNum(i) => i.to_f64().unwrap().sin(),
        SteelVal::NumV(n) => n.sin(),
        SteelVal::Rational(r) => (*r.numer() as f32 / *r.denom() as f32).sin() as f64,
        _ => stop!(TypeMismatch => "sin expects a number, found: {}", arg),
    }
    .into_steelval()
}

/// Returns the cosine value of the input angle, measured in radians.
///
/// (cos n) -> number?
///
/// * n : number? - The input angle, in radians.
///
/// # Examples
/// ```scheme
/// > (cos 0) ;; => 1
/// > (cos 1) ;; => 0.5403023058681398
/// > (cos 2.0) ;; => -0.4161468365471424
/// > (cos 3.14) ;; => -0.9999987317275395
/// ```
#[steel_derive::function(name = "cos", constant = true)]
pub fn cos(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(i) => (*i as f64).cos(),
        SteelVal::BigNum(i) => i.to_f64().unwrap().cos(),
        SteelVal::NumV(n) => n.cos(),
        SteelVal::Rational(r) => (*r.numer() as f32 / *r.denom() as f32).cos() as f64,
        _ => stop!(TypeMismatch => "cos expects a number, found: {}", arg),
    }
    .into_steelval()
}

/// Returns the tangent value of the input angle, measured in radians.
///
/// (tan n) -> number?
///
/// * n : number? - The input angle, in radians.
///
/// # Examples
/// ```scheme
/// > (tan 0) ;; => 0
/// > (tan 1) ;; => 1.557407724654902
/// > (tan 2.0) ;; => -2.185039863261519
/// > (tan 3.14) ;; => -0.0015926549364072232
/// ```
#[steel_derive::function(name = "tan", constant = true)]
pub fn tan(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(i) => (*i as f64).tan(),
        SteelVal::BigNum(i) => i.to_f64().unwrap().tan(),
        SteelVal::NumV(n) => n.tan(),
        SteelVal::Rational(r) => (*r.numer() as f32 / *r.denom() as f32).tan() as f64,
        _ => stop!(TypeMismatch => "tan expects a number, found: {}", arg),
    }
    .into_steelval()
}

/// Returns the arcsine, or inverse sine, of a value; output is in radians.
///
/// (asin n) -> number?
///
/// * n : number? - The input value is the sine of the angle you want and must be from -1 to 1.
///
/// # Examples
/// ```scheme
/// > (asin -1) ;; => -1.5707963267948966
/// > (asin 0) ;; => 0
/// > (asin 0.5) ;; => 0.5235987755982988
/// > (asin 2) ;; => +nan.0
/// ```
#[steel_derive::function(name = "asin", constant = true)]
pub fn asin(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(i) => (*i as f64).asin(),
        SteelVal::BigNum(i) => i.to_f64().unwrap().asin(),
        SteelVal::NumV(n) => n.asin(),
        SteelVal::Rational(r) => (*r.numer() as f32 / *r.denom() as f32).asin() as f64,
        _ => stop!(TypeMismatch => "asin expects a number, found: {}", arg),
    }
    .into_steelval()
}

/// Returns the arccosine, or inverse cosine, of a value; output is in radians.
///
/// (acos n) -> number?
///
/// * n : number? - The input value is the cosine of the angle you want and must be from -1 to 1.
///
/// # Examples
/// ```scheme
/// > (acos -1) ;; => 3.141592653589793
/// > (acos 0) ;; => 1.5707963267948966
/// > (acos 0.5) ;; => 1.0471975511965976
/// > (acos 2) ;; => +nan.0
/// ```
#[steel_derive::function(name = "acos", constant = true)]
pub fn acos(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(i) => (*i as f64).acos(),
        SteelVal::BigNum(i) => i.to_f64().unwrap().acos(),
        SteelVal::NumV(n) => n.acos(),
        SteelVal::Rational(r) => (*r.numer() as f32 / *r.denom() as f32).acos() as f64,
        _ => stop!(TypeMismatch => "acos expects a number, found: {}", arg),
    }
    .into_steelval()
}

/// Returns the arctangent, or inverse tangent, of a value; output is in radians.
///
/// (atan n) -> number?
///
/// * n : number? - The input value is the tangent of the angle you want.
///
/// # Examples
/// ```scheme
/// > (atan -1) ;; => -0.7853981633974483
/// > (atan 0) ;; => 0
/// > (atan 0.5) ;; => 0.46364760900080615
/// > (atan 2) ;; => 1.1071487177940906
/// ```
#[steel_derive::function(name = "atan", constant = true)]
pub fn atan(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(i) => (*i as f64).atan(),
        SteelVal::BigNum(i) => i.to_f64().unwrap().atan(),
        SteelVal::NumV(n) => n.atan(),
        SteelVal::Rational(r) => (*r.numer() as f32 / *r.denom() as f32).atan() as f64,
        _ => stop!(TypeMismatch => "atan expects a number, found: {}", arg),
    }
    .into_steelval()
}

/// Divides the given numbers.
///
/// (/ . nums) -> number?
///
/// * nums : number? - The numbers to divide. Must have at least one number.
///
/// # Examples
/// ```scheme
/// > (/ 10 2) ;; => 5
/// > (/ 10 2 2.0) ;; => 2.5
/// > (/ 1 3.0) ;; => 0.3333333333333333
/// > (/ 1 3) ;; => 1/3
/// ```
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

/// Checks if the given value is exact.
///
/// (exact? val) -> boolean?
///
/// * val : any - The value to check for exactness.
///
/// # Examples
/// ```scheme
/// > (exact? 42) ;; => #t
/// > (exact? 3.14) ;; => #f
/// > (exact? "hello") ;; => #f
/// ```
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

/// Checks if the given value is inexact.
///
/// (inexact? val) -> boolean?
///
/// * val : any - The value to check for inexactness.
///
/// # Examples
/// ```scheme
/// > (inexact? 42) ;; => #f
/// > (inexact? 3.14) ;; => #t
/// ```
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

/// Converts a number to an inexact number.
///
/// (inexact num) -> number?
///
/// * num : number? - The number to convert from exact to inexact.
///
/// # Examples
/// ```scheme
/// > (inexact 10) ;; => 10
/// > (inexact 1/2) ;; => 0.5
/// > (inexact 1+2i) ;; => 1+2i
/// ```
#[steel_derive::function(name = "inexact", constant = true)]
fn inexact(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(i) => (*i as f64).into_steelval(),
        SteelVal::Rational(f) => f.to_f64().unwrap().into_steelval(),
        SteelVal::BigRational(f) => f.to_f64().unwrap().into_steelval(),
        SteelVal::NumV(n) => n.into_steelval(),
        SteelVal::BigNum(n) => Ok(SteelVal::NumV(n.to_f64().unwrap())),
        SteelVal::Complex(x) => SteelComplex::new(inexact(&x.re)?, inexact(&x.im)?).into_steelval(),
        _ => steelerr!(TypeMismatch => "exact->inexact expects a number type, found: {}", number),
    }
}

/// Converts an exact number to an inexact number.
///
/// (exact->inexact num) -> number?
///
/// * num : number? - The number to convert from exact to inexact.
///
/// # Examples
/// ```scheme
/// > (exact->inexact 10) ;; => 10
/// > (exact->inexact 1/2) ;; => 0.5
/// > (exact->inexact 1+2i) ;; => 1+2i
/// ```
#[steel_derive::function(name = "exact->inexact", constant = true)]
fn exact_to_inexact(number: &SteelVal) -> Result<SteelVal> {
    inexact(number)
}

/// Converts a number to an exact number.
///
/// (exact num) -> number?
///
/// * num : number? - The value to convert to exact.
///
/// # Examples
/// ```scheme
/// > (exact 10.0) ;; => 10
/// > (exact 1.5) ;; => 3/2
/// > (exact 1.5+2.5i) ;; => 3/2+5/2i
/// ```
#[steel_derive::function(name = "exact", constant = true)]
pub fn exact(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(_)
        | SteelVal::Rational(_)
        | SteelVal::BigRational(_)
        | SteelVal::BigNum(_) => Ok(number.clone()),
        SteelVal::NumV(x) => {
            if x.fract() == 0. {
                (*x as isize).into_steelval()
            } else {
                BigRational::from_float(*x).into_steelval()
            }
        }
        SteelVal::Complex(x) => SteelComplex::new(exact(&x.re)?, exact(&x.im)?).into_steelval(),
        _ => steelerr!(TypeMismatch => "exact->inexact expects a number type, found: {}", number),
    }
}

/// Converts an inexact number to an exact number.
///
/// (inexact->exact num) -> number?
///
/// * num : number? - The number to convert from inexact to exact.
///
/// # Examples
/// ```scheme
/// > (inexact->exact 10.0) ;; => 10
/// > (inexact->exact 1.5) ;; => 3/2
/// > (inexact->exact 1.5+2.5i) ;; => 3/2+5/2i
/// ```
#[steel_derive::function(name = "inexact->exact", constant = true)]
fn inexact_to_exact(number: &SteelVal) -> Result<SteelVal> {
    exact(number)
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

/// Returns `#t` if the given number is finite.
///
/// (finite? number) -> boolean?
///
/// * number : number? - The number to check for finiteness.
///
/// # Examples
/// ```scheme
/// > (finite? 42) ;; => #t
/// > (finite? 0.1) ;; => #t
/// > (finite? +inf.0) ;; => #f
/// > (finite? -inf.0) ;; => #f
/// > (finite? +nan.0) ;; => #f
/// ```
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

/// Returns `#t` if the given number is infinite.
///
/// (infinite? number) -> boolean?
///
/// * number : number? - The number to check for infiniteness.
///
/// # Examples
/// ```scheme
/// > (infinite? 42) ;; => #f
/// > (infinite? -nan.0) ;; => #f
/// > (infinite? +inf.0) ;; => #t
/// ```
#[steel_derive::function(name = "infinite?", constant = true)]
fn infinitep(number: &SteelVal) -> Result<SteelVal> {
    infinitep_impl(number)?.into_steelval()
}

/// Computes the absolute value of the given number.
///
/// (abs number) -> number?
///
/// * number : number? - The number to compute the absolute value of.
///
/// # Examples
/// ```scheme
/// > (abs 42) ;; => 42
/// > (abs -42) ;; => 42
/// > (abs 0) ;; => 0
/// ```
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

/// Rounds the given number up to the nearest integer not less than it.
///
/// (ceiling number) -> integer?
///
/// * number : number? - The number to round up.
///
/// # Examples
/// ```scheme
/// > (ceiling 42) ;; => 42
/// > (ceiling 42.1) ;; => 43
/// > (ceiling -42.1) ;; => -42
/// ```
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

/// Retrieves the denominator of the given rational number.
///
/// (denominator number) -> integer?
///
/// * number : number? - The rational number to retrieve the denominator from.
///
/// # Examples
/// ```scheme
/// > (denominator 1/2) ;; => 2
/// > (denominator 3/4) ;; => 4
/// > (denominator 4) ;; => 1
/// ```
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
/// Raises the left operand to the power of the right operand.
///
/// (expt base exponent) -> number?
///
/// * base : number? - The base number.
/// * exponent : number? - The exponent to raise the base to.
///
/// # Examples
/// ```scheme
/// > (expt 2 3) ;; => 8
/// > (expt 2.0 0.5) ;; => 1.4142135623730951
/// > (expt 9 0.5) ;; => 3
/// ```
#[steel_derive::function(name = "expt", constant = true)]
fn expt(left: &SteelVal, right: &SteelVal) -> Result<SteelVal> {
    match (left, right) {
        (SteelVal::IntV(l), SteelVal::IntV(r)) if *r >= 0 => {
            match u32::try_from(*r).ok().and_then(|r| l.checked_pow(r)) {
                Some(val) => val.into_steelval(),
                None => BigInt::from(*l).pow(*r as usize).into_steelval(),
            }
        }
        // r is negative here
        (SteelVal::IntV(l), SteelVal::IntV(r)) => {
            if l.is_zero() {
                stop!(Generic => "expt: 0 cannot be raised to a negative power");
            }

            let r = r.unsigned_abs();
            // a^-b = 1/(a^b)
            match (u32::try_from(r).ok())
                .and_then(|r| l.checked_pow(r))
                .and_then(|l| i32::try_from(l).ok())
            {
                Some(val) => Rational32::new_raw(1, val).into_steelval(),
                None => {
                    BigRational::new_raw(BigInt::from(1), BigInt::from(*l).pow(r)).into_steelval()
                }
            }
        }
        (SteelVal::IntV(l), SteelVal::NumV(r)) => (*l as f64).powf(*r).into_steelval(),
        (SteelVal::IntV(l), SteelVal::Rational(r)) => {
            (*l as f64).powf(r.to_f64().unwrap()).into_steelval()
        }
        (SteelVal::IntV(l), SteelVal::BigNum(r)) => {
            if l.is_zero() {
                stop!(Generic => "expt: 0 cannot be raised to a negative power");
            }

            let expt = BigInt::from(*l).pow(r.magnitude());
            match r.sign() {
                num_bigint::Sign::Plus | num_bigint::Sign::NoSign => expt.into_steelval(),
                num_bigint::Sign::Minus => {
                    BigRational::new_raw(BigInt::from(1), expt).into_steelval()
                }
            }
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
        (SteelVal::BigNum(l), SteelVal::BigNum(r)) => {
            let expt = l.as_ref().clone().pow(r.magnitude());
            match r.sign() {
                num_bigint::Sign::NoSign | num_bigint::Sign::Plus => expt.into_steelval(),
                num_bigint::Sign::Minus => {
                    BigRational::new_raw(BigInt::from(1), expt).into_steelval()
                }
            }
        }
        (SteelVal::BigNum(l), SteelVal::IntV(r)) => match *r {
            0 => 1.into_steelval(),
            r if r < 0 => {
                BigRational::new_raw(BigInt::from(1), l.as_ref().clone().pow(r.unsigned_abs()))
                    .into_steelval()
            }
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

/// Returns Euler’s number raised to the power of z.
///
/// (exp z) -> number?
///
/// * z : number? - The number to raise e to the power of.
///
/// # Examples
/// ```scheme
/// > (exp 0) ;; => 1
/// > (exp 2) ;; => 7.38905609893065
/// > (exp 1.5) ;; => 4.4816890703380645
/// ```
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

/// Computes the largest integer less than or equal to the given number.
///
/// (floor number) -> number?
///
/// * number : number? - The number to compute the floor for.
///
/// # Examples
/// ```scheme
/// > (floor 3.14) ;; => 3
/// > (floor 4.99) ;; => 4
/// > (floor -2.5) ;; => -3
/// ```
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

/// Retrieves the numerator of the given rational number.
///
/// (numerator number) -> number?
///
/// * number : number? - The rational number to retrieve the numerator from.
///
/// # Examples
/// ```scheme
/// > (numerator 3/4) ;; => 3
/// > (numerator 5/2) ;; => 5
/// > (numerator -2) ;; => -2
/// ```
#[steel_derive::function(name = "numerator", constant = true)]
fn numerator(number: &SteelVal) -> Result<SteelVal> {
    match number {
        SteelVal::IntV(x) => x.into_steelval(),
        SteelVal::Rational(x) => (*x.numer() as isize).into_steelval(),
        SteelVal::BigNum(x) => Ok(SteelVal::BigNum(x.clone())),
        SteelVal::BigRational(x) => (x.numer().clone()).into_steelval(),
        _ => steelerr!(Generic => "numerator expects an integer or rational number"),
    }
}

/// Rounds the given number to the nearest integer.
///
/// (round number) -> number?
///
/// * number : number? - The number to round.
///
/// # Examples
/// ```scheme
/// > (round 3.14) ;; => 3
/// > (round 4.6) ;; => 5
/// > (round -2.5) ;; => -3
/// ```
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

/// Computes the square of the given number.
///
/// (square number) -> number?
///
/// * number : number? - The number to square.
///
/// # Examples
/// ```scheme
/// > (square 5) ;; => 25
/// > (square -3) ;; => 9
/// > (square 2.5) ;; => 6.25
/// ```
#[steel_derive::function(name = "square", constant = true)]
fn square(number: &SteelVal) -> Result<SteelVal> {
    if !numberp(number) {
        stop!(TypeMismatch => "square expects a number, found: {:?}", number)
    }
    multiply_two(&number, &number)
}

/// Computes the square root of the given number.
///
/// (sqrt number) -> number?
///
/// * number : number? - The number to compute the square root for.
///
/// # Examples
/// ```scheme
/// > (sqrt 4) ;; => 2
/// > (sqrt 2) ;; => 1.4142135623730951
/// > (sqrt -1) ;; => 0+1i
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

/// Create a complex number with `re` as the real part and `im` as the imaginary part.
///
/// (make-rectangular re im) -> number?
///
/// - re : real?
/// - im : real?
#[steel_derive::function(name = "make-rectangular", constant = true)]
pub fn make_rectangular(re: &SteelVal, im: &SteelVal) -> Result<SteelVal> {
    ensure_arg_is_real("make-rectangular", re)?;
    ensure_arg_is_real("make-rectangular", im)?;

    SteelComplex {
        re: re.clone(),
        im: im.clone(),
    }
    .into_steelval()
}

/// Make a complex number out of a magnitude `r` and an angle `θ`, so that the result is `r * (cos θ + i sin θ)`
///
/// (make-polar r θ) -> number?
///
/// - r : real?
/// - theta : real?
#[steel_derive::function(name = "make-polar", constant = true)]
pub fn make_polar(r: &SteelVal, theta: &SteelVal) -> Result<SteelVal> {
    ensure_arg_is_real("make-polar", r)?;
    ensure_arg_is_real("make-polar", theta)?;

    let re = multiply_primitive(&[r.clone(), cos(&theta)?])?;
    let im = multiply_primitive(&[r.clone(), sin(&theta)?])?;
    SteelComplex { re, im }.into_steelval()
}

fn ensure_arg_is_real(op: &str, arg: &SteelVal) -> Result<()> {
    if !realp(arg) {
        stop!(TypeMismatch => "{op} expects a real number, found: {:?}", arg);
    } else {
        Ok(())
    }
}

/// Returns the real part of a number
///
/// (real-part number) -> number?
///
/// # Examples
/// ```scheme
/// > (real-part 3+4i) ;; => 3
/// > (real-part 42) ;; => 42
/// ```
#[steel_derive::function(name = "real-part", constant = true)]
pub fn real_part(value: &SteelVal) -> Result<SteelVal> {
    match value {
        val @ SteelVal::IntV(_)
        | val @ SteelVal::BigNum(_)
        | val @ SteelVal::Rational(_)
        | val @ SteelVal::BigRational(_)
        | val @ SteelVal::NumV(_) => Ok(val.clone()),
        SteelVal::Complex(complex) => Ok(complex.re.clone()),
        _ => steelerr!(TypeMismatch => "real-part expected number"),
    }
}

/// Returns the imaginary part of a number
///
/// (imag-part number) -> number?
///
/// # Examples
/// ```scheme
/// > (imag-part 3+4i) ;; => 4
/// > (imag-part 42) ;; => 0
/// ```
#[steel_derive::function(name = "imag-part", constant = true)]
pub fn imag_part(value: &SteelVal) -> Result<SteelVal> {
    match value {
        SteelVal::IntV(_)
        | SteelVal::BigNum(_)
        | SteelVal::Rational(_)
        | SteelVal::BigRational(_)
        | SteelVal::NumV(_) => Ok(SteelVal::IntV(0)),
        SteelVal::Complex(complex) => Ok(complex.im.clone()),
        _ => steelerr!(TypeMismatch => "imag-part expected number"),
    }
}

/// Computes the magnitude of the given number.
///
/// (magnitude number) -> number?
///
/// * number : number? - The number to compute the magnitude for.
///
/// # Examples
/// ```scheme
/// > (magnitude 3+4i) ;; => 5
/// > (magnitude 5) ;; => 5
/// > (magnitude -5) ;; => 5
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

/// Computes the angle `θ` of a complex number `z` where `z = r * (cos θ + i sin θ)` and `r` is the magnitude.
///
/// (angle number) -> number?
///
/// - number : number?
#[steel_derive::function(name = "angle", constant = true)]
pub fn angle(number: &SteelVal) -> Result<SteelVal> {
    let (re, im) = match number {
        re @ SteelVal::NumV(_)
        | re @ SteelVal::IntV(_)
        | re @ SteelVal::Rational(_)
        | re @ SteelVal::BigNum(_) => (re, &SteelVal::IntV(0)),
        SteelVal::Complex(complex) => (&complex.re, &complex.im),
        _ => stop!(TypeMismatch => "angle expects a number, found {number}"),
    };

    atan2(im, re)
}

/// Computes the quadratic arctan of `x` and `y`
fn atan2(x: &SteelVal, y: &SteelVal) -> Result<SteelVal> {
    let as_f64 = |arg: &_| match arg {
        SteelVal::NumV(arg) => Ok(*arg),
        SteelVal::IntV(arg) => Ok(*arg as f64),
        SteelVal::Rational(arg) => Ok(*arg.numer() as f64 / *arg.denom() as f64),
        SteelVal::BigNum(arg) => Ok(arg.to_f64().unwrap()),
        _ => steelerr!(TypeMismatch => "atan2 expects a number, found {arg}"),
    };

    let x = as_f64(x)?;
    let y = as_f64(y)?;
    if x == 0. && y == 0. {
        // as this is currently only used for `angle`, make the error
        // message a little better by saying `angle` instead of `atan2`
        stop!(Generic => "angle: undefined for zero");
    }

    f64::atan2(x, y).into_steelval()
}

/// Computes the natural logarithm of the given number.
///
/// (log number [base]) -> number?
///
/// * number : number? - The number to compute the logarithm for.
/// * base : number? - The base of the logarithm. If not provided, defaults to Euler's number (e).
///
/// # Examples
/// ```scheme
/// > (log 10) ;; => 2.302585092994046
/// > (log 100 10) ;; => 2
/// > (log 27 3) ;; => 3
/// ```
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

/// Computes the integer square root of the given non-negative integer.
///
/// (exact-integer-sqrt number) -> (integer? integer?)
///
/// * number : (and/c integer? positive?) - The non-negative integer to compute the square root for.
///
/// # Examples
/// ```scheme
/// > (exact-integer-sqrt 25) ;; => (5 0)
/// > (exact-integer-sqrt 35) ;; => (5 10)
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
    N: num_integer::Roots + Clone,
    &'a N: std::ops::Mul<&'a N, Output = N>,
    N: std::ops::Sub<N, Output = N>,
{
    let x = target.sqrt();
    let x_sq = x.clone() * x.clone();
    let rem = target.clone() - x_sq;
    (x, rem)
}

/// Performs a bitwise arithmetic shift using the given 2 numbers
///
/// (arithmetic-shift n m) -> integer?
///
/// * n : integer? - The number to shift.
/// * m : integer? - The number by which to shift.
///
/// # Examples
/// ```scheme
/// > (arithmetic-shift 10 1) ;; => 20
/// > (arithmetic-shift 20 1) ;; => 40
/// > (arithmetic-shift 40 -2) ;; => 10
/// ```
#[steel_derive::native(name = "arithmetic-shift", constant = true, arity = "Exact(2)")]
pub fn arithmetic_shift(args: &[SteelVal]) -> Result<SteelVal> {
    match &args {
        [n, m] => match (n, m) {
            (SteelVal::IntV(n), SteelVal::IntV(m)) => {
                if *m >= 0 {
                    Ok(SteelVal::IntV(n << m))
                } else {
                    Ok(SteelVal::IntV(n >> -m))
                }
            }
            _ => stop!(TypeMismatch => "arithmetic-shift expected 2 integers"),
        },
        _ => stop!(ArityMismatch => "arithmetic-shift takes 2 arguments"),
    }
}

/// Checks if the given number is even
///
/// (even? n) -> bool?
///
/// * n : number? - The number to check for evenness.
///
/// # Examples
/// ```scheme
/// > (even? 2) ;; => #true
/// > (even? 3) ;; => #false
/// > (even? 4.0) ;; => #true
/// ```
#[steel_derive::function(name = "even?", constant = true)]
pub fn even(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(n) => Ok(SteelVal::BoolV(n & 1 == 0)),
        SteelVal::BigNum(n) => Ok(SteelVal::BoolV(n.is_even())),
        SteelVal::NumV(n) if n.fract() == 0.0 => (*n as i64).is_even().into_steelval(),
        _ => steelerr!(TypeMismatch => "even? requires an integer, found: {:?}", arg),
    }
}

/// Checks if the given number is odd
///
/// (odd? n) -> bool?
///
/// * n : number? - The number to check for oddness.
///
/// # Examples
/// ```scheme
/// > (odd? 2) ;; => #false
/// > (odd? 3) ;; => #true
/// > (odd? 5.0) ;; => #true
/// ```
#[steel_derive::function(name = "odd?", constant = true)]
pub fn odd(arg: &SteelVal) -> Result<SteelVal> {
    match arg {
        SteelVal::IntV(n) => Ok(SteelVal::BoolV(n & 1 == 1)),
        SteelVal::BigNum(n) => Ok(SteelVal::BoolV(n.is_odd())),
        SteelVal::NumV(n) if n.fract() == 0.0 => (*n as i64).is_odd().into_steelval(),
        _ => {
            steelerr!(TypeMismatch => "odd? requires an integer, found: {:?}", arg)
        }
    }
}

/// Sums all given floats
///
/// (f+ nums) -> number?
///
/// * nums : float? - The floats to sum up.
///
/// # Examples
/// ```scheme
/// > (f+ 5.5) ;; => 5.5
/// > (f+ 1.1 2.2) ;; => 3.3
/// > (f+ 3.3 3.3 3.3) ;; => 9.9
/// ```
#[steel_derive::native(name = "f+", constant = true, arity = "AtLeast(1)")]
pub fn float_add(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "f+ requires at least one argument")
    }
    let mut sum = 0.0;

    for arg in args {
        if let SteelVal::NumV(n) = arg {
            sum += n;
        } else {
            stop!(TypeMismatch => "f+ expected a float, found {:?}", arg);
        }
    }

    Ok(SteelVal::NumV(sum))
}

fn ensure_args_are_numbers(op: &str, args: &[SteelVal]) -> Result<()> {
    for arg in args {
        if !numberp(arg) {
            stop!(TypeMismatch => "{op} expects a number, found: {:?}", arg);
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
        (SteelVal::BigRational(x), SteelVal::Rational(y)) => {
            let mut res = BigRational::new(
                BigInt::from(x.numer().clone()),
                BigInt::from(x.denom().clone()),
            );
            res *= BigRational::new(BigInt::from(*y.numer()), BigInt::from(*y.denom()));
            res.into_steelval()
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
#[inline(always)]
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
#[inline(always)]
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
            BigRational(Gc::new(num_rational::BigRational::new(
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
            &num_rational::BigRational::new(
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
