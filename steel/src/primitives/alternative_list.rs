use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::{Result, SteelVal};
use crate::{stop, throw};
use im_lists::{list, list::List};

// Generates a macro that expands to code snippet to do arity checks
// example usage:
// arity_check!(length, args, 1);
// Expands to:
// if args.len() != 1 {
//     stop!(ArityMismatch => format!("length expected only one argument, found {}", args.len()))
// }
// assert!(args.len() == 1);
macro_rules! arity_check_generator {
    ($($arity:tt),*) => {
        macro_rules! arity_check {
            ($name:tt, $args:expr, 1) => {
                if $args.len() != 1 {
                    stop!(ArityMismatch => format!(stringify!($name expected only one argument, found {}), $args.len()))
                }
                assert!($args.len() == 1);
            };

            $ (
                ($name:tt, $args:expr, $arity) => {
                    if $args.len() != $arity {
                        stop!(ArityMismatch => format!(stringify!($name expected two arguments, found {}), $args.len()))
                    }
                    assert!($args.len() == $arity);
                };
            ) *
        }
    }
}

arity_check_generator!(2, 3, 4, 5, 6, 7, 8);

// Declares a const for a function that takes an immutable slice to the arguments
// e.g.
//      declare_const_ref_functions! { LENGTH => length }
// expands into
//      const LENGTH: SteelVal = SteelVal::FuncV(length)
macro_rules! declare_const_ref_functions {
    ($($name:tt => $func_name:tt),* $(,)? ) => {
        $ (
            pub(crate) const $name: SteelVal = SteelVal::FuncV($func_name);
        ) *
    }
}

// Declares a const for a function that takes an immutable slice to the arguments
// e.g.
//      declare_const_mut_ref_functions! { CONS => cons }
// expands into
//      const LENGTH: SteelVal = SteelVal::MutFunc(length)
macro_rules! declare_const_mut_ref_functions {
    ($($name:tt => $func_name:tt),* $(,)? ) => {
        $ (
            pub(crate) const $name: SteelVal = SteelVal::MutFunc($func_name);
        ) *
    }
}

declare_const_ref_functions! {
    LIST => new,
    LENGTH => length,
    NEW => new,
    REVERSE => reverse,
    LAST => last,
    TAKE => take,
    LIST_REF => list_ref,
    RANGE => range,
}

declare_const_mut_ref_functions! {
    CONS => cons,
    FIRST => first,
    REST => rest,
    APPEND => append,
}

// TODO replace all usages with const
// const LENGTH: SteelVal = SteelVal::FuncV(length);
// const NEW: SteelVal = SteelVal::FuncV(new);

fn new(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::ListV(args.iter().cloned().collect()))
}

fn cons(args: &mut [SteelVal]) -> Result<SteelVal> {
    if args.len() != 2 {
        stop!(ArityMismatch => "cons takes only two arguments")
    }
    match (args[0].clone(), &mut args[1]) {
        (left, SteelVal::ListV(right)) => {
            // println!("Strong count: {:?}", right.strong_count());
            right.cons_mut(left);
            Ok(SteelVal::ListV(right.clone()))
        }
        (left, right) => Ok(SteelVal::ListV(list![left, right.clone()])),
    }

    // match (&args[1].clone(), &mut args[0]) {
    //     (right, SteelVal::ListV(left)) => {
    //         println!("Strong count: {:?}", left.strong_count());
    //         right.cons_mut(left);
    //         Ok(SteelVal::ListV(right.clone()))
    //     }
    //     (right, left) => Ok(SteelVal::ListV(list![left, right])),
    // }
}

fn range(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(new_range, args, 2);

    if let (SteelVal::IntV(lower), SteelVal::IntV(upper)) = (&args[0], &args[1]) {
        if *lower < 0 {
            stop!(Generic => "range expects a positive integer");
        }

        if *upper < 0 {
            stop!(Generic => "range expects a positive integer");
        }

        Ok(SteelVal::ListV(
            (*lower as usize..*upper as usize)
                .into_iter()
                .map(|x| SteelVal::IntV(x as isize))
                .collect(),
        ))
    } else {
        stop!(ArityMismatch => "range takes two integers")
    }
}

fn length(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(length, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(l.len().into())
    } else {
        stop!(TypeMismatch => "length expects a list")
    }
}

fn reverse(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(reverse, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(SteelVal::ListV(l.clone().reverse()))
    } else {
        stop!(TypeMismatch => "reverse expects a list")
    }
}

fn last(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(last, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        l.last()
            .cloned()
            .ok_or_else(throw!(Generic => "last resulted in an error - empty list"))
    } else {
        stop!(TypeMismatch => "last expects a list")
    }
}

fn first(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(first, args, 1);

    if let SteelVal::ListV(l) = &mut args[0] {
        l.pop_front()
            .ok_or_else(throw!(Generic => "first resulted in an error - empty list"))
    } else {
        stop!(TypeMismatch => "first expects a list")
    }
}

fn rest(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(rest, args, 1);

    if let SteelVal::ListV(l) = &mut args[0] {
        match l.rest_mut() {
            Some(l) => Ok(SteelVal::ListV(l.clone())),
            None => {
                if l.is_empty() {
                    stop!(Generic => "rest expects a non empty list");
                } else {
                    Ok(SteelVal::ListV(l.clone()))
                }
            }
        }
    } else {
        stop!(TypeMismatch => "rest expects a list")
    }
}

fn take(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(take, args, 2);

    if let (SteelVal::ListV(l), SteelVal::IntV(n)) = (&args[0], &args[1]) {
        if *n < 0 {
            stop!(Generic => "take expects a positive integer")
        } else {
            Ok(SteelVal::ListV(l.take(*n as usize)))
        }
    } else {
        stop!(TypeMismatch => "take expects a list in the first position, and a positive integer in the second position")
    }
}

fn append(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(append, args, 2);

    if let (SteelVal::ListV(r), SteelVal::ListV(l)) = (args[1].clone(), &mut args[0]) {
        l.append_mut(r);
        Ok(SteelVal::ListV(l.clone()))
    } else {
        stop!(TypeMismatch => "append expects two lists");
    }
}

fn list_ref(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(list_ref, args, 2);

    // todo!()
    if let (SteelVal::ListV(lst), SteelVal::IntV(n)) = (&args[0], &args[1]) {
        if *n < 0 {
            stop!(Generic => "list-ref expects a positive integer")
        } else {
            lst.get(*n as usize)
                .cloned()
                .ok_or_else(throw!(Generic => format!("out of bounds index in list-ref - list length: {}, index: {}", lst.len(), n)))
        }
    } else {
        stop!(TypeMismatch => "list expects a list and an integer")
    }
}

impl From<List<SteelVal>> for SteelVal {
    fn from(l: List<SteelVal>) -> Self {
        SteelVal::ListV(l)
    }
}
