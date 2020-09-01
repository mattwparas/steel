// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use rand::Rng;

use crate::env::{FALSE, TRUE};

use crate::gc::Gc;

pub struct NumOperations {}
impl NumOperations {
    pub fn random_int() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "random-int requires an upper bound");
            }

            if args.len() > 1 {
                stop!(ArityMismatch => "random-int takes one argument")
            }

            if let SteelVal::IntV(upper_bound) = args[0].as_ref() {
                let mut rng = rand::thread_rng();
                return Ok(Gc::new(SteelVal::IntV(rng.gen_range(0, upper_bound))));
            } else {
                stop!(TypeMismatch => "random-int requires an integer upper bound");
            }
        })
    }

    pub fn even() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "even? takes one argument")
            }

            if let SteelVal::IntV(n) = args[0].as_ref() {
                // let is_odd = |x: i32| x & 1 == 1;
                // let is_even = |x: i32| x & 1 == 0;
                if n & 1 == 0 {
                    Ok(TRUE.with(|f| Gc::clone(f)))
                } else {
                    Ok(FALSE.with(|f| Gc::clone(f)))
                }
            } else {
                stop!(TypeMismatch => "even? requires an integer")
            }
        })
    }

    pub fn odd() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "even? takes one argument")
            }

            if let SteelVal::IntV(n) = args[0].as_ref() {
                // let is_odd = |x: i32| x & 1 == 1;
                // let is_even = |x: i32| x & 1 == 0;
                if n & 1 == 1 {
                    Ok(TRUE.with(|f| Gc::clone(f)))
                } else {
                    Ok(FALSE.with(|f| Gc::clone(f)))
                }
            } else {
                stop!(TypeMismatch => "odd? requires an integer")
            }
        })
    }

    pub fn integer_add() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "+ requires at least one argument")
            }

            let mut sum = 0;

            for arg in args {
                if let SteelVal::IntV(n) = arg.as_ref() {
                    sum += n;
                } else {
                    stop!(TypeMismatch => "+ expected a number, found {:?}", arg);
                }
            }

            Ok(Gc::new(SteelVal::IntV(sum)))
        })
    }

    pub fn float_add() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "+ requires at least one argument")
            }

            let mut sum = 0.0;

            for arg in args {
                if let SteelVal::NumV(n) = arg.as_ref() {
                    sum += n;
                } else {
                    stop!(TypeMismatch => "+ expected a number, found {:?}", arg);
                }
            }

            Ok(Gc::new(SteelVal::NumV(sum)))
        })
    }

    pub fn adder() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "+ requires at least one argument")
            }

            let mut sum_int = 0;
            let mut sum_float = 0.0;
            let mut found_float = false;

            for arg in args {
                match arg.as_ref() {
                    SteelVal::IntV(n) => {
                        if found_float {
                            sum_float += *n as f64;
                        } else {
                            if let Some(res) = isize::checked_add(sum_int, *n) {
                                sum_int = res
                            } else {
                                found_float = true;
                                sum_float += *n as f64;
                            }
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
                        let e = format!("+ expected a number, found {:?}", arg);
                        stop!(TypeMismatch => e);
                    }
                }
            }

            if found_float {
                Ok(Gc::new(SteelVal::NumV(sum_float)))
            } else {
                Ok(Gc::new(SteelVal::IntV(sum_int)))
            }
        })
    }

    pub fn multiply() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "* requires at least one argument")
            }

            let mut sum_int = 1;
            let mut sum_float = 1.0;
            let mut found_float = false;

            for arg in args {
                match arg.as_ref() {
                    SteelVal::IntV(n) => {
                        if found_float {
                            sum_float *= *n as f64;
                        } else {
                            if let Some(res) = isize::checked_mul(sum_int, *n) {
                                sum_int = res
                            } else {
                                found_float = true;
                                sum_float *= *n as f64;
                            }
                        }
                    }
                    SteelVal::NumV(n) => {
                        if !found_float {
                            sum_float = sum_int as f64;
                            found_float = true
                        }
                        sum_float *= n;
                    }
                    _ => stop!(TypeMismatch => "* expected a number"),
                }
            }

            if found_float {
                Ok(Gc::new(SteelVal::NumV(sum_float)))
            } else {
                Ok(Gc::new(SteelVal::IntV(sum_int)))
            }
        })
    }

    // TODO implement the full numerical tower
    // For now, only support division into floats
    pub fn divide() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "/ requires at least one argument")
            }

            let floats: Result<Vec<f64>> = args
                .into_iter()
                .map(|x| match x.as_ref() {
                    SteelVal::IntV(n) => Ok(*n as f64),
                    SteelVal::NumV(n) => Ok(*n),
                    _ => stop!(TypeMismatch => "division expects a number"),
                })
                .collect();

            let mut floats = floats?.into_iter();

            if let Some(first) = floats.next() {
                Ok(Gc::new(SteelVal::NumV(
                    floats.fold(first, |acc, x| acc / x),
                )))
            } else {
                stop!(ArityMismatch => "division requires at least one argument")
            }
        })
    }

    pub fn subtract() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.is_empty() {
                stop!(ArityMismatch => "- requires at least one argument")
            }

            let mut sum_int = 0;
            let mut sum_float = 0.0;
            let mut found_float = false;

            let mut args = args.into_iter();

            if let Some(first) = args.next() {
                match first.as_ref() {
                    SteelVal::IntV(n) => {
                        sum_int = *n;
                        // sum_float = *n as f64;
                    }
                    SteelVal::NumV(n) => {
                        found_float = true;
                        sum_float = *n;
                    }
                    _ => stop!(TypeMismatch => "'-' expected a number type"),
                }
            }

            for arg in args {
                match arg.as_ref() {
                    SteelVal::IntV(n) => {
                        if found_float {
                            sum_float -= *n as f64;
                        } else {
                            if let Some(res) = isize::checked_sub(sum_int, *n) {
                                sum_int = res
                            } else {
                                found_float = true;
                                sum_float -= *n as f64;
                            }
                        }
                    }
                    SteelVal::NumV(n) => {
                        if !found_float {
                            sum_float = sum_int as f64;
                            found_float = true
                        }
                        sum_float -= n;
                    }
                    _ => stop!(TypeMismatch => "- expected a number"),
                }
            }

            if found_float {
                Ok(Gc::new(SteelVal::NumV(sum_float)))
            } else {
                Ok(Gc::new(SteelVal::IntV(sum_int)))
            }
        })
    }
}
