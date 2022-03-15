use crate::rvals::{Result, SteelVal};
use crate::steel_vm::vm::VmContext;
use crate::{stop, throw};
use im_lists::{list, list::List};

use crate::core::utils::{
    arity_check, declare_const_mut_ref_functions, declare_const_ref_functions,
};

declare_const_ref_functions! {
    LIST => new,
    LENGTH => length,
    NEW => new,
    REVERSE => reverse,
    LAST => last,
    TAKE => take,
    LIST_REF => list_ref,
    RANGE => range,
    IS_EMPTY => is_empty,
    CAR => car,
    LIST_TO_STRING => list_to_string,
    FIRST => car,
    PAIR => pair,
}

declare_const_mut_ref_functions! {
    CONS => cons,
    // FIRST => first,
    REST => rest,
    CDR => cdr,
    APPEND => append,
    PUSH_BACK => push_back,
}

pub(crate) const TEST_MAP: SteelVal = SteelVal::BuiltIn(test_map);
pub(crate) const TEST_APPLY: SteelVal = SteelVal::BuiltIn(apply);

// TODO replace all usages with const
// const LENGTH: SteelVal = SteelVal::FuncV(length);
// const NEW: SteelVal = SteelVal::FuncV(new);

fn test_map(args: Vec<SteelVal>, ctx: &mut dyn VmContext) -> Result<SteelVal> {
    arity_check!(test_map, args, 2);

    let mut arg_iter = args.into_iter();
    let arg1 = arg_iter.next().unwrap();
    let arg2 = arg_iter.next().unwrap();

    if let SteelVal::ListV(l) = arg2 {
        if arg1.is_function() {
            // unimplemented!()

            Ok(SteelVal::ListV(
                l.into_iter()
                    .map(|x| ctx.call_function_one_arg(&arg1, x))
                    .collect::<Result<_>>()?,
            ))

            // ctx.call_function_one_arg_or_else(function, arg)
        } else {
            stop!(TypeMismatch => "test-map expected a function")
        }
    } else {
        stop!(TypeMismatch => "test-map expects a list")
    }
}

fn apply(args: Vec<SteelVal>, ctx: &mut dyn VmContext) -> Result<SteelVal> {
    arity_check!(apply, args, 2);

    let mut arg_iter = args.into_iter();
    let arg1 = arg_iter.next().unwrap();
    let arg2 = arg_iter.next().unwrap();

    if let SteelVal::ListV(l) = arg2 {
        if arg1.is_function() {
            ctx.call_function_many_args(&arg1, l)
        } else {
            stop!(TypeMismatch => "apply expected a function")
        }
    } else {
        stop!(TypeMismatch => "apply expects a list")
    }
}

fn new(args: &[SteelVal]) -> Result<SteelVal> {
    Ok(SteelVal::ListV(args.iter().cloned().collect()))
}

fn is_empty(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(is_empty, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(l.is_empty().into())
    } else {
        stop!(TypeMismatch => "empty? expects a list")
    }
}

fn pair(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(pair, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        Ok(l.iter().next().is_some().into())
    } else {
        stop!(TypeMismatch => format!("pair expects a list, found: {}", &args[0]))
    }
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

pub fn last(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(last, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        l.last()
            .cloned()
            .ok_or_else(throw!(Generic => "last resulted in an error - empty list"))
    } else {
        stop!(TypeMismatch => "last expects a list")
    }
}

// fn first(args: &mut [SteelVal]) -> Result<SteelVal> {
//     arity_check!(first, args, 1);

//     if let SteelVal::ListV(l) = &mut args[0] {
//         l.pop_front()
//             .ok_or_else(throw!(Generic => "first resulted in an error - empty list"))
//     } else {
//         stop!(TypeMismatch => "first expects a list")
//     }
// }

fn car(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(car, args, 1);
    if let SteelVal::ListV(l) = &args[0] {
        l.car()
            .ok_or_else(throw!(Generic => "first resulted in an error - empty list"))
    } else {
        stop!(TypeMismatch => format!("first expects a list, found: {:?}", &args[0]))
    }
}

fn cdr(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(rest, args, 1);

    if let SteelVal::ListV(l) = &mut args[0] {
        if l.is_empty() {
            stop!(Generic => "cdr expects a non empty list");
        }

        match l.rest_mut() {
            Some(l) => Ok(SteelVal::ListV(l.clone())),
            None => Ok(SteelVal::ListV(l.clone())),
        }
    } else {
        stop!(TypeMismatch => format!("cdr expects a list, found: {}", &args[0]))
    }
}

fn rest(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(rest, args, 1);

    if let SteelVal::ListV(l) = &mut args[0] {
        if l.is_empty() {
            stop!(Generic => "rest expects a non empty list");
        }

        match l.rest_mut() {
            Some(l) => Ok(SteelVal::ListV(l.clone())),
            None => Ok(SteelVal::ListV(l.clone())),
        }
    } else {
        stop!(TypeMismatch => "rest expects a list")
    }
}

pub fn take(args: &[SteelVal]) -> Result<SteelVal> {
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

pub fn list_ref(args: &[SteelVal]) -> Result<SteelVal> {
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

fn list_to_string(args: &[SteelVal]) -> Result<SteelVal> {
    arity_check!(list_to_string, args, 1);

    if let SteelVal::ListV(l) = &args[0] {
        let collected_string = l
            .iter()
            .map(|x| {
                x.char_or_else(throw!(TypeMismatch => "list->string expected a list of characters"))
            })
            .collect::<Result<String>>()?;

        Ok(SteelVal::StringV(collected_string.into()))
    } else {
        stop!(TypeMismatch => "first expects a list")
    }
}

// TODO this could be broken using &mut
// need to think about it more
fn push_back(args: &mut [SteelVal]) -> Result<SteelVal> {
    arity_check!(push_back, args, 2);

    let arg = args[1].clone();

    if let SteelVal::ListV(l) = &mut args[0] {
        l.push_back(arg);

        Ok(args[0].clone())
    } else {
        stop!(TypeMismatch => "push-back expects a list")
    }
}

impl From<List<SteelVal>> for SteelVal {
    fn from(l: List<SteelVal>) -> Self {
        SteelVal::ListV(l)
    }
}

#[cfg(test)]
mod list_operation_tests {

    use super::*;
    use crate::rerrs::ErrorKind;

    #[test]
    fn cons_test_normal_input() {
        let mut args = [SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = cons(&mut args);
        let expected = SteelVal::ListV(list![SteelVal::IntV(1), SteelVal::IntV(2)]);

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_single_input() {
        let mut args = [SteelVal::IntV(1)];
        let res = cons(&mut args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cons_no_input() {
        let res = cons(&mut []);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cons_with_empty_list() {
        let mut args = [SteelVal::IntV(1), SteelVal::ListV(List::new())];
        let res = cons(&mut args);
        // let expected = SteelVal::ListV(list![SteelVal::IntV(1)]);

        let expected = crate::list![1i32];

        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_with_non_empty_vector() {
        let mut args = [SteelVal::IntV(1), SteelVal::ListV(list![SteelVal::IntV(2)])];
        let res = cons(&mut args);
        let expected = crate::list![1i32, 2i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_normal_input() {
        let args = [crate::list![1i32, 2i32]];
        let res = car(&args);
        let expected = SteelVal::IntV(1);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_bad_input() {
        let args = [SteelVal::IntV(1)];
        let res = car(&args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn car_too_many_args() {
        let args = [SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = car(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_normal_input_2_elements() {
        let mut args = [crate::list![1i32, 2i32]];
        let res = cdr(&mut args);
        let expected = crate::list![2i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_normal_input_3_elements() {
        let mut args = [crate::list![1i32, 2i32, 3i32]];
        let res = cdr(&mut args);
        let expected = crate::list![2i32, 3i32];
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_bad_input() {
        let mut args = [SteelVal::IntV(1)];
        let res = cdr(&mut args);
        let expected = ErrorKind::TypeMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_too_many_args() {
        let mut args = [SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = cdr(&mut args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn cdr_single_element_list() {
        let mut args = [SteelVal::ListV(list![SteelVal::NumV(1.0)])];
        let res = cdr(&mut args);
        let expected = SteelVal::ListV(List::new());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn range_tests_arity_too_few() {
        let args = [SteelVal::IntV(1)];
        let res = range(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn range_test_arity_too_many() {
        let args = [
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0),
        ];
        let res = range(&args);
        let expected = ErrorKind::ArityMismatch;
        assert_eq!(res.unwrap_err().kind(), expected);
    }

    #[test]
    fn range_test_normal_input() {
        let args = [SteelVal::IntV(0), SteelVal::IntV(3)];
        let res = range(&args);
        let expected = SteelVal::ListV(list![
            SteelVal::IntV(0),
            SteelVal::IntV(1),
            SteelVal::IntV(2)
        ]);
        assert_eq!(res.unwrap(), expected);
    }
}
