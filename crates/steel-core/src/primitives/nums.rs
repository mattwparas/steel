use num::{Integer, Rational32, ToPrimitive, Zero};

use crate::rvals::{Custom, IntoSteelVal, Result, SteelVal};
use crate::stop;

pub fn multiply_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        // stop!(ArityMismatch => "* requires at least one argument")

        return Ok(SteelVal::IntV(1));
    }

    let mut sum_int = 1;
    let mut sum_float = 1.0;
    let mut bignum = num::BigInt::default();
    let mut found_float = false;
    let mut found_bignum = false;

    for arg in args {
        match arg {
            SteelVal::IntV(n) => {
                if found_float {
                    sum_float *= *n as f64;
                } else if found_bignum {
                    bignum *= *n;
                } else if let Some(res) = isize::checked_mul(sum_int, *n) {
                    sum_int = res
                } else {
                    found_bignum = true;

                    bignum += sum_int;
                    bignum *= *n;
                }
            }
            SteelVal::NumV(n) => {
                if !found_float {
                    sum_float = sum_int as f64;
                    found_float = true
                }
                sum_float *= n;
            }
            SteelVal::BigNum(n) => {
                // If we have a float already, just stay in float
                if found_float {
                    sum_float *= n.to_f64().unwrap();
                // If we haven't found a float, but we have promoted to bignum, we can stay
                // in bignum
                } else if found_bignum {
                    bignum *= n.as_ref();
                } else {
                    // Promote to bignum
                    bignum = n.unwrap();
                    bignum *= sum_int;

                    found_bignum = true;
                }
            }
            _ => stop!(TypeMismatch => "* expected a number"),
        }
    }

    if found_float {
        Ok(SteelVal::NumV(sum_float))
    } else if found_bignum {
        bignum.into_steelval()
    } else {
        Ok(SteelVal::IntV(sum_int))
    }
}

pub fn quotient(l: isize, r: isize) -> isize {
    l / r
}

pub fn divide_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "/ requires at least one argument")
    }

    if args.len() == 1 {
        return match &args[0] {
            SteelVal::IntV(n) => match i32::try_from(*n) {
                Ok(n) => Rational32::new(1, n).into_steelval(),
                Err(_) => todo!(),
            },
            SteelVal::NumV(n) => n.recip().into_steelval(),
            SteelVal::FractV(f) => f.recip().into_steelval(),
            unexpected => {
                stop!(TypeMismatch => "division expects a number, found: {:?}", unexpected)
            }
        };
    }

    let mut no_floats = true;
    let floats: Result<Vec<f64>> = args
        .iter()
        .map(|x| match x {
            SteelVal::IntV(n) => Ok(*n as f64),
            SteelVal::NumV(n) => {
                no_floats = false;
                Ok(*n)
            }
            _ => stop!(TypeMismatch => "division expects a number"),
        })
        .collect();

    let mut floats = floats?.into_iter();
    // Unwrapping is ok as we checked the arity which ensures at least 1 float.
    let first = floats.next().unwrap();
    let result = floats.fold(first, |acc, x| acc / x);
    if no_floats && result.fract() == 0.0 {
        Ok(SteelVal::IntV(result as isize))
    } else {
        Ok(SteelVal::NumV(result))
    }
}

#[inline(always)]
pub fn subtract_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    if args.is_empty() {
        stop!(ArityMismatch => "- requires at least one argument")
    }

    if args.len() == 1 {
        match &args[0] {
            SteelVal::IntV(n) => {
                if let Some(res) = 0isize.checked_sub(*n) {
                    return Ok(SteelVal::IntV(res));
                } else {
                    let mut zero = num::BigInt::from(0);

                    zero -= *n;

                    return zero.into_steelval();
                }
            }
            SteelVal::NumV(n) => {
                return Ok(SteelVal::NumV(0.0 - n));
            }
            SteelVal::BigNum(n) => {
                return (0isize - n.as_ref()).into_steelval();
            }
            _ => {
                stop!(TypeMismatch => format!("'-' expected a number type, found: {}", &args[0]))
            }
        }
    }

    let mut sum_int = 0;
    let mut sum_float = 0.0;
    let mut found_float = false;

    let mut args = args.iter();

    if let Some(first) = args.next() {
        match first {
            SteelVal::IntV(n) => {
                sum_int = *n;

                // sum_float = *n as f64;
            }
            SteelVal::NumV(n) => {
                found_float = true;
                sum_float = *n;
            }
            _ => {
                stop!(TypeMismatch => format!("'-' expected a number type, found: {first}"))
            }
        }
    }

    for arg in args {
        match arg {
            SteelVal::IntV(n) => {
                if found_float {
                    sum_float -= *n as f64;
                } else if let Some(res) = isize::checked_sub(sum_int, *n) {
                    sum_int = res
                } else {
                    found_float = true;
                    sum_float -= *n as f64;
                }
            }
            SteelVal::NumV(n) => {
                if !found_float {
                    sum_float = sum_int as f64;
                    found_float = true
                }
                sum_float -= n;
            }
            _ => {
                stop!(TypeMismatch => "- expected a number")
            }
        }
    }

    if found_float {
        Ok(SteelVal::NumV(sum_float))
    } else {
        Ok(SteelVal::IntV(sum_int))
    }
}

pub fn add_primitive_faster(args: &[SteelVal]) -> Result<SteelVal> {
    let mut sum_int = 0;
    let mut sum_float = 0.0;
    let mut found_float = false;

    for arg in args {
        match arg {
            SteelVal::IntV(n) => {
                sum_int += n;
            }
            SteelVal::NumV(n) => {
                sum_float += n;
                found_float = true;
            }
            _ => {
                crate::steel_vm::vm::cold();
                let e = format!("+ expected a number, found {arg:?}");
                stop!(TypeMismatch => e);
            }
        }
    }

    if found_float {
        Ok(SteelVal::NumV(sum_float + sum_int as f64))
    } else {
        Ok(SteelVal::IntV(sum_int))
    }
}

pub fn add_primitive(args: &[SteelVal]) -> Result<SteelVal> {
    let mut sum_int = 0;
    let mut sum_float = 0.0;
    let mut found_float = false;

    for arg in args {
        match arg {
            SteelVal::IntV(n) => {
                if found_float {
                    sum_float += *n as f64;
                } else if let Some(res) = isize::checked_add(sum_int, *n) {
                    sum_int = res
                } else {
                    found_float = true;
                    sum_float += *n as f64;
                }
            }
            SteelVal::NumV(n) => {
                if !found_float {
                    sum_float = sum_int as f64;
                    found_float = true
                }
                sum_float += n;
            }
            _ => {
                let e = format!("+ expected a number, found {arg:?}, all args: {args:?}");
                stop!(TypeMismatch => e);
            }
        }
    }

    if found_float {
        Ok(SteelVal::NumV(sum_float))
    } else {
        Ok(SteelVal::IntV(sum_int))
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
        // SteelVal::FuncV(add_primitive_faster)
        SteelVal::FuncV(special_add)
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

impl Custom for num::BigRational {}

impl IntoSteelVal for num::BigInt {
    fn into_steelval(self) -> Result<SteelVal> {
        Ok(SteelVal::BigNum(crate::gc::Gc::new(self)))
    }
}

#[steel_derive::native(name = "+", constant = true, arity = "AtLeast(0)")]
pub fn special_add(args: &[SteelVal]) -> Result<SteelVal> {
    let mut sum_int: isize = 0;
    let mut sum_float = 0.0;
    let mut found_float = false;
    let mut big_int_sum = num::BigInt::default();

    for arg in args {
        match arg {
            SteelVal::IntV(n) => {
                if let Some(right_side) = sum_int.checked_add(*n) {
                    sum_int = right_side;
                } else {
                    big_int_sum += *n;
                }
            }
            SteelVal::NumV(n) => {
                sum_float += n;
                found_float = true;
            }

            SteelVal::BigNum(b) => {
                big_int_sum += b.as_ref();
            }
            _ => {
                crate::steel_vm::vm::cold();
                let e = format!("+ expected a number, found {arg:?}");
                stop!(TypeMismatch => e);
            }
        }
    }

    if found_float {
        (sum_float + sum_int as f64 + big_int_sum.to_f64().unwrap()).into_steelval()
    } else if !big_int_sum.is_zero() {
        big_int_sum += sum_int;
        big_int_sum.into_steelval()
    } else {
        sum_int.into_steelval()
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
