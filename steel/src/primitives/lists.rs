use crate::rerrs::SteelErr;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::throw;
use im_rc::Vector;

use crate::gc::Gc;

pub struct ListOperations {}
impl ListOperations {
    pub fn cons() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "cons takes only two arguments")
            }
            let mut args = args.into_iter().map(Gc::clone);
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => match lst.as_ref() {
                    SteelVal::VectorV(l) => {
                        if l.is_empty() {
                            Ok(Gc::new(SteelVal::Pair(elem, None)))
                        } else {
                            Ok(Gc::new(SteelVal::Pair(elem, Some(lst))))
                        }
                    }
                    _ => Ok(Gc::new(SteelVal::Pair(elem, Some(lst)))),
                },
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn car() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "car takes one argument");
            }
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(car, _) => Ok(Gc::clone(car)),
                    e => {
                        stop!(TypeMismatch => "car takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "car takes one argument");
            }
        })
    }

    pub fn cdr() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "cdr takes one argument");
            }
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(_, cdr) => match cdr {
                        Some(rest) => match rest.as_ref() {
                            Pair(_, _) => Ok(Gc::clone(rest)),
                            _ => Ok(Gc::new(SteelVal::Pair(Gc::clone(rest), None))), // Ok(Gc::clone(rest))
                        },
                        None => Ok(Gc::new(SteelVal::VectorV(Vector::new()))), // TODO
                    },
                    e => {
                        stop!(TypeMismatch => "cdr takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "cdr takes one argument");
            }
        })
    }

    pub fn list() -> SteelVal {
        SteelVal::FuncV(Self::built_in_list_func())
    }

    // TODO fix this
    pub fn range() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "range takes two arguments")
            }
            let mut args = args.into_iter().map(|x| (**x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (IntV(lower), IntV(upper)) = (elem, lst) {
                        // let size = (upper - lower) as usize;
                        // let mut res = Vec::with_capacity(size);

                        Ok(Self::built_in_list_normal_iter_non_result(
                            (lower as usize..upper as usize)
                                .into_iter()
                                .map(|x| Gc::new(SteelVal::IntV(x as isize))),
                        ))

                    // for i in lower as usize..upper as usize {
                    //     res.push(Gc::new(SteelVal::IntV(i as isize)));
                    // }
                    // Self::built_in_list_func()(&res)
                    } else {
                        stop!(TypeMismatch => "range expected number")
                    }
                }
                _ => stop!(ArityMismatch => "range takes two arguments"),
            }
        })
    }

    // TODO fix the VectorV case
    pub fn reverse() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                match &args[0].as_ref() {
                    SteelVal::Pair(_, _) => {
                        let mut lst = Self::collect_into_vec(&args[0])?;
                        lst.reverse();
                        Self::built_in_list_func()(&lst)
                    }
                    SteelVal::VectorV(v) => Ok(Gc::new(SteelVal::BoolV(v.is_empty()))),
                    _ => Ok(Gc::new(SteelVal::BoolV(false))),
                }
            } else {
                stop!(ArityMismatch => "reverse takes one argument");
            }
        })
    }

    pub fn list_to_string() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "list->string takes one argument");
            }
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(_, _) => {
                        let collected_string = SteelVal::iter(Gc::clone(first)).map(|x| {
                            x.char_or_else(throw!(TypeMismatch => "list->string expected a list of characters"))
                        })
                        .collect::<Result<String>>()?;

                        Ok(Gc::new(SteelVal::StringV(collected_string)))
                    }
                    VectorV(v) if v.len() == 0 => Ok(Gc::new(SteelVal::StringV("".to_string()))),
                    e => {
                        stop!(TypeMismatch => "list->string takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "list->string takes one argument");
            }
        })
    }

    pub fn push_back() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            // let mut lst = Vec::new();
            if args.len() != 2 {
                stop!(ArityMismatch => "push-back expected 2 arguments");
            }

            match args[0].as_ref() {
                SteelVal::Pair(_, _) => {
                    let mut lst: Vec<_> = SteelVal::iter(Gc::clone(&args[0])).collect();
                    lst.push(Gc::clone(&args[1]));
                    ListOperations::built_in_list_func()(&lst)
                }
                SteelVal::VectorV(v) => {
                    if v.is_empty() {
                        let lst = vec![Gc::clone(&args[1])];
                        ListOperations::built_in_list_func()(&lst)
                    } else {
                        stop!(TypeMismatch => "push-back requires a list as the first argument")
                    }
                }
                _ => {
                    stop!(TypeMismatch => "push-back requires a list as the first argument");
                }
            }
        })
    }

    pub fn append() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut lst = Vec::new();
            for arg in args {
                match arg.as_ref() {
                    SteelVal::Pair(_, _) => {
                        for value in SteelVal::iter(Gc::clone(arg)) {
                            // println!("{:?}", value);
                            lst.push(value);
                        }
                    }
                    SteelVal::VectorV(v) => {
                        // unimplemented!();
                        // println!("{:?}", v);
                        if v.is_empty() {
                            continue;
                        }
                    }
                    _ => {
                        let error_msg =
                            format!("append expected a list, found: {}", arg.to_string());
                        stop!(TypeMismatch => error_msg);
                    }
                }
            }

            // let lst = args
            //     .map(|x| {
            //         if let SteelVal::Pair(_, _) = x.as_ref() {
            //             Ok(SteelVal::iter(x))
            //         } else {
            //             stop!(TypeMismatch => "append expected a list");
            //         }
            //     })
            //     .flatten()
            //     .collect::<Result<Vec<Gc<SteelVal>>>>();

            Self::built_in_list_func()(&lst)
        })
    }

    // pub fn take() -> SteelVal {
    //     SteelVal::FuncV(|args: Vec<Gc<SteelVal>>| -> Result<Gc<SteelVal>> {
    //         if args.len() == 1 {
    //             match &args[0].as_ref() {
    //                 SteelVal::Pair(_, _) => {
    //                     let mut lst = Self::collect_into_vec(&args[0])?;
    //                     lst.reverse();
    //                     Self::built_in_list_func()(lst)
    //                 }
    //                 SteelVal::VectorV(v) => Ok(Gc::new(SteelVal::BoolV(v.is_empty()))),
    //                 _ => Ok(Gc::new(SteelVal::BoolV(false))),
    //             }
    //         } else {
    //             stop!(ArityMismatch => "reverse takes one argument");
    //         }
    //     })
    // }

    // pub fn flatten() -> SteelVal {
    //     SteelVal::FuncV(|args: Vec<Gc<SteelVal>>| -> Result<Gc<SteelVal>> {
    //         let flattened_vec = args
    //             .into_iter()
    //             .map(|x| Self::collect_into_vec(&x))
    //             .collect::<Result<Vec<Vec<Gc<SteelVal>>>>>()?
    //             .into_iter()
    //             .flatten()
    //             .collect::<Vec<Gc<SteelVal>>>();

    //         Self::built_in_list_func()(flattened_vec)

    //         // unimplemented!()
    //     })
    // }

    pub fn list_to_vec() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::Pair(..) = &args[0].as_ref() {
                    // let iter = SteelVal::iter(Gc::clone(&args[0])).collect()

                    // let collected: Vec<SteelVal> = Self::collect_into_vec(&args[0])?
                    //     .into_iter()
                    //     .map(|x| (*x).clone())
                    //     .collect();
                    // let im_vec: Vector<SteelVal> = collected.into();
                    Ok(Gc::new(SteelVal::VectorV(
                        SteelVal::iter(Gc::clone(&args[0])).collect(),
                    )))
                } else {
                    stop!(TypeMismatch => "list->vector expected list")
                }
            } else {
                stop!(ArityMismatch => "list->vector takes one argument");
            }
        })
    }

    pub fn collect_into_vec(mut p: &Gc<SteelVal>) -> Result<Vec<Gc<SteelVal>>> {
        let mut lst = Vec::new();
        loop {
            match p.as_ref() {
                SteelVal::Pair(cons, cdr) => {
                    lst.push(Gc::clone(cons));
                    match cdr.as_ref() {
                        Some(rest) => match rest.as_ref() {
                            Pair(_, _) => p = rest,
                            _ => {
                                lst.push(Gc::clone(rest));
                                break;
                            }
                        },
                        None => break,
                    }
                }
                _ => stop!(TypeMismatch => "collect into vec expected a list"),
            }
        }

        Ok(lst)
    }

    pub fn built_in_list_normal_iter_non_result<I>(args: I) -> Gc<SteelVal>
    where
        I: Iterator<Item = Gc<SteelVal>>,
    {
        let mut pairs: Vec<Gc<SteelVal>> =
            args.map(|car| Gc::new(SteelVal::Pair(car, None))).collect();

        // let mut rev_iter = pairs.iter_mut().rev().enumerate();

        let mut rev_iter = (0..pairs.len()).into_iter().rev();
        rev_iter.next();

        for i in rev_iter {
            let prev = pairs.pop().unwrap();
            if let Some(SteelVal::Pair(_, cdr)) = pairs.get_mut(i).map(Gc::get_mut).flatten() {
                *cdr = Some(prev)
            } else {
                unreachable!()
            }
        }

        pairs.pop().unwrap()
    }

    pub fn built_in_list_normal_iter<I>(args: I) -> Result<Gc<SteelVal>>
    where
        I: Iterator<Item = Result<Gc<SteelVal>>>,
    {
        // unimplemented!();

        // let mut pairs = Vec::new();

        let mut pairs: Vec<Gc<SteelVal>> = args
            .map(|car| Ok(Gc::new(SteelVal::Pair(car?, None))))
            .collect::<Result<Vec<Gc<SteelVal>>>>()?;

        // let mut rev_iter = pairs.iter_mut().rev().enumerate();

        let mut rev_iter = (0..pairs.len()).into_iter().rev();
        rev_iter.next();

        for i in rev_iter {
            let prev = pairs.pop().unwrap();
            if let Some(SteelVal::Pair(_, cdr)) = pairs.get_mut(i).map(Gc::get_mut).flatten() {
                *cdr = Some(prev)
            } else {
                unreachable!()
            }
        }

        pairs
            .pop()
            .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))

        // rev_iter.next();

        // for (i, val) in rev_iter {

        // }

        // unimplemented!()
    }

    pub fn built_in_list_func_iter<I>(args: I) -> Result<Gc<SteelVal>>
    where
        I: DoubleEndedIterator<Item = Gc<SteelVal>>,
    {
        let mut args = args.rev();
        let mut pairs = Vec::new();
        match args.next() {
            Some(car) => {
                pairs.push(Gc::new(SteelVal::Pair(car, None)));
            }
            _ => {
                return Ok(Gc::new(SteelVal::VectorV(Vector::new())));
            }
        }

        for (i, val) in args.enumerate() {
            pairs.push(Gc::new(SteelVal::Pair(val, Some(Gc::clone(&pairs[i])))));
        }
        pairs
            .pop()
            .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))
        // unimplemented!()
    }

    pub fn built_in_list_func_iter_result<I>(args: I) -> Result<Gc<SteelVal>>
    where
        I: DoubleEndedIterator<Item = Result<Gc<SteelVal>>>,
    {
        let mut args = args.rev();
        let mut pairs = Vec::new();
        match args.next() {
            Some(car) => {
                pairs.push(Gc::new(SteelVal::Pair(car?, None)));
            }
            _ => {
                return Ok(Gc::new(SteelVal::VectorV(Vector::new())));
            }
        }

        for (i, val) in args.enumerate() {
            pairs.push(Gc::new(SteelVal::Pair(val?, Some(Gc::clone(&pairs[i])))));
        }
        pairs
            .pop()
            .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))
        // unimplemented!()
    }

    // pub fn built_is_list_func_normal_iter<I>(args: I) -> Result<Gc<SteelVal>>
    // where
    //     I: Iterator<Item = Gc<SteelVal>>,
    // {
    //     // unimplemented!()
    //     let mut args = args;
    //     let mut pairs = Vec::new();

    //     match (args.next(), args.next()) {
    //         (cdr, Some(car)) => {
    //             pairs.push(Gc::new(SteelVal::Pair(car, cdr)));
    //         }
    //         (Some(cdr), None) => {
    //             pairs.push(Gc::new(SteelVal::Pair(cdr, None)));
    //         }
    //         (_, _) => {
    //             return Ok(Gc::new(SteelVal::VectorV(Vector::new())));
    //         }
    //     }

    //     for (i, val) in args.enumerate() {
    //         pairs.push(Gc::new(SteelVal::Pair(val, Some(Gc::clone(&pairs[i])))));
    //     }
    //     pairs
    //         .pop()
    //         .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))
    //     // unimplemented!()
    // }

    // TODO add a function that does the same but takes an iterator
    // This definitely needs to get looked at
    // This could lead to nasty speed ups for map/filter
    pub fn built_in_list_func() -> fn(&[Gc<SteelVal>]) -> Result<Gc<SteelVal>> {
        |args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let mut args = args.into_iter().rev().map(Gc::clone);
            let mut pairs = Vec::new();

            match args.next() {
                Some(car) => {
                    pairs.push(Gc::new(SteelVal::Pair(car, None)));
                }
                _ => {
                    return Ok(Gc::new(SteelVal::VectorV(Vector::new())));
                }
            }

            for (i, val) in args.enumerate() {
                pairs.push(Gc::new(SteelVal::Pair(val, Some(Gc::clone(&pairs[i])))));
            }

            pairs
                .pop()
                .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))
        }
    }

    pub fn built_in_list_func_flat(args: &[Gc<SteelVal>]) -> Result<Gc<SteelVal>> {
        let mut args = args.into_iter().rev().map(Gc::clone);
        let mut pairs = Vec::new();

        match args.next() {
            Some(car) => {
                pairs.push(Gc::new(SteelVal::Pair(car, None)));
            }
            _ => {
                return Ok(Gc::new(SteelVal::VectorV(Vector::new())));
            }
        }

        for (i, val) in args.enumerate() {
            pairs.push(Gc::new(SteelVal::Pair(val, Some(Gc::clone(&pairs[i])))));
        }

        pairs
            .pop()
            .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))
    }

    pub fn built_in_list_func_flat_non_gc(args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let mut args = args.into_iter().rev().map(Gc::new);
        let mut pairs = Vec::new();

        match args.next() {
            Some(car) => {
                pairs.push(Gc::new(SteelVal::Pair(car, None)));
            }
            _ => {
                return Ok(Gc::new(SteelVal::VectorV(Vector::new())));
            }
        }

        for (i, val) in args.enumerate() {
            pairs.push(Gc::new(SteelVal::Pair(val, Some(Gc::clone(&pairs[i])))));
        }

        pairs
            .pop()
            .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string(), None))
    }
}

#[cfg(test)]
mod list_operation_tests {

    use super::*;
    use crate::throw;
    use im_rc::vector;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "list tests"))
            .unwrap()(&args)
    }

    #[test]
    fn cons_test_normal_input() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = apply_function(ListOperations::cons(), args);
        let expected = Gc::new(SteelVal::Pair(
            Gc::new(SteelVal::IntV(1)),
            Some(Gc::new(SteelVal::IntV(2))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_single_input() {
        let args = vec![SteelVal::IntV(1)];
        let res = apply_function(ListOperations::cons(), args);
        let expected = SteelErr::ArityMismatch("cons takes only two arguments".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cons_no_input() {
        let args = vec![];
        let res = apply_function(ListOperations::cons(), args);
        let expected = SteelErr::ArityMismatch("cons takes only two arguments".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cons_with_empty_list() {
        let args = vec![SteelVal::IntV(1), SteelVal::VectorV(Vector::new())];
        let res = apply_function(ListOperations::cons(), args);
        let expected = Gc::new(SteelVal::Pair(Gc::new(SteelVal::IntV(1)), None));
        assert_eq!(res.unwrap(), expected);
    }
    #[test]
    fn cons_with_non_empty_vector() {
        let args = vec![
            SteelVal::IntV(1),
            SteelVal::VectorV(vector![Gc::new(SteelVal::IntV(2))]),
        ];
        let res = apply_function(ListOperations::cons(), args);
        let expected = Gc::new(SteelVal::Pair(
            Gc::new(SteelVal::IntV(1)),
            Some(Gc::new(SteelVal::VectorV(vector![Gc::new(
                SteelVal::IntV(2)
            )]))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_normal_input() {
        let args = vec![SteelVal::Pair(
            Gc::new(SteelVal::IntV(1)),
            Some(Gc::new(SteelVal::IntV(2))),
        )];
        let res = apply_function(ListOperations::car(), args);
        let expected = Gc::new(SteelVal::IntV(1));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_bad_input() {
        let args = vec![SteelVal::IntV(1)];
        let res = apply_function(ListOperations::car(), args);
        let expected = SteelErr::TypeMismatch("car takes a list, given: 1".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn car_too_many_args() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2)];
        let res = apply_function(ListOperations::car(), args);
        let expected = SteelErr::ArityMismatch("car takes one argument".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cdr_normal_input_2_elements() {
        let args = vec![SteelVal::Pair(
            Gc::new(SteelVal::IntV(1)),
            Some(Gc::new(SteelVal::IntV(2))),
        )];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = Gc::new(Pair(Gc::new(SteelVal::IntV(2)), None));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_normal_input_3_elements() {
        let args = vec![SteelVal::Pair(
            Gc::new(SteelVal::IntV(1)),
            Some(Gc::new(SteelVal::Pair(
                Gc::new(SteelVal::IntV(2)),
                Some(Gc::new(SteelVal::IntV(3))),
            ))),
        )];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = Gc::new(Pair(
            Gc::new(SteelVal::IntV(2)),
            Some(Gc::new(SteelVal::IntV(3))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_bad_input() {
        let args = vec![SteelVal::IntV(1)];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = SteelErr::TypeMismatch("cdr takes a list, given: 1".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }
    #[test]
    fn cdr_too_many_args() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = SteelErr::ArityMismatch("cdr takes one argument".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cdr_single_element_list() {
        let args = vec![SteelVal::Pair(Gc::new(SteelVal::NumV(1.0)), None)];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = Gc::new(SteelVal::VectorV(Vector::new()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn range_tests_arity_too_few() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::range(), args);
        let expected = SteelErr::ArityMismatch("range takes two arguments".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn range_test_arity_too_many() {
        let args = vec![
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0),
        ];
        let res = apply_function(ListOperations::range(), args);
        let expected = SteelErr::ArityMismatch("range takes two arguments".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn range_test_normal_input() {
        let args = vec![SteelVal::IntV(0), SteelVal::IntV(3)];
        let res = apply_function(ListOperations::range(), args);
        let expected = Gc::new(SteelVal::Pair(
            Gc::new(SteelVal::IntV(0)),
            Some(Gc::new(SteelVal::Pair(
                Gc::new(SteelVal::IntV(1)),
                Some(Gc::new(SteelVal::IntV(2))),
            ))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_to_vec_arity_too_few() {
        let args = vec![];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = SteelErr::ArityMismatch("list->vector takes one argument".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn list_to_vec_arity_too_many() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = SteelErr::ArityMismatch("list->vector takes one argument".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn list_to_vec_bad_arg() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = SteelErr::TypeMismatch("list->vector expected list".to_string(), None);
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn list_to_vec_normal() {
        let args = vec![SteelVal::Pair(
            Gc::new(SteelVal::IntV(1)),
            Some(Gc::new(SteelVal::IntV(2))),
        )];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![SteelVal::IntV(1), SteelVal::IntV(2)]
                .into_iter()
                .map(Gc::new)
                .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }
}
