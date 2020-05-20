use crate::rerrs::SteelErr;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use crate::throw;
use im_rc::Vector;
use std::rc::Rc;
// mod primitives;

pub struct ListOperations {}
impl ListOperations {
    pub fn cons() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "cons takes only two arguments")
            }
            let mut args = args.into_iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => match lst.as_ref() {
                    SteelVal::VectorV(l) => {
                        if l.is_empty() {
                            Ok(Rc::new(SteelVal::Pair(elem, None)))
                        } else {
                            Ok(Rc::new(SteelVal::Pair(elem, Some(lst))))
                        }
                    }
                    _ => Ok(Rc::new(SteelVal::Pair(elem, Some(lst)))),
                },
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn car() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "car takes one argument");
            }
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(car, _) => Ok(Rc::clone(car)),
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
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "cdr takes one argument");
            }
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(_, cdr) => match cdr {
                        Some(rest) => match rest.as_ref() {
                            Pair(_, _) => Ok(Rc::clone(rest)),
                            _ => Ok(Rc::new(SteelVal::Pair(Rc::clone(rest), None))), // Ok(Rc::clone(rest))
                        },
                        None => Ok(Rc::new(SteelVal::VectorV(Vector::new()))), // TODO
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

    pub fn range() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "range takes two arguments")
            }
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (NumV(lower), NumV(upper)) = (elem, lst) {
                        let mut res = Vec::new();
                        for i in lower as usize..upper as usize {
                            res.push(Rc::new(SteelVal::NumV(i as f64)));
                        }
                        Self::built_in_list_func()(res)
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
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                match &args[0].as_ref() {
                    SteelVal::Pair(_, _) => {
                        let mut lst = Self::collect_into_vec(&args[0])?;
                        lst.reverse();
                        Self::built_in_list_func()(lst)
                    }
                    SteelVal::VectorV(v) => Ok(Rc::new(SteelVal::BoolV(v.is_empty()))),
                    _ => Ok(Rc::new(SteelVal::BoolV(false))),
                }
            } else {
                stop!(ArityMismatch => "reverse takes one argument");
            }
        })
    }

    pub fn list_to_string() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "list->string takes one argument");
            }
            if let Some(first) = args.into_iter().next() {
                match first.as_ref() {
                    Pair(_, _) => {
                        let collected_string = SteelVal::iter(first).map(|x| {
                            x.char_or_else(throw!(TypeMismatch => "list->string expected a list of characters"))
                        })
                        .collect::<Result<String>>()?;

                        // let lst = Self::collect_into_vec(&first)?;
                        // let collected_string = lst
                        //     .into_iter()
                        // .map(|x| {
                        //     x.char_or_else(throw!(TypeMismatch => "list->string expected a list of characters"))
                        // })
                        // .collect::<Result<String>>()?;

                        Ok(Rc::new(SteelVal::StringV(collected_string)))

                        // Ok(Rc::clone(car))
                    }
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
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            // let mut lst = Vec::new();
            if args.len() != 2 {
                stop!(ArityMismatch => "push-back expected 2 arguments");
            }

            match args[0].as_ref() {
                SteelVal::Pair(_, _) => {
                    let mut lst: Vec<_> = SteelVal::iter(Rc::clone(&args[0])).collect();
                    lst.push(Rc::clone(&args[1]));
                    ListOperations::built_in_list_func()(lst)
                }
                SteelVal::VectorV(v) => {
                    if v.is_empty() {
                        let lst = vec![Rc::clone(&args[1])];
                        ListOperations::built_in_list_func()(lst)
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
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut lst = Vec::new();
            for arg in args {
                match arg.as_ref() {
                    SteelVal::Pair(_, _) => {
                        for value in SteelVal::iter(arg) {
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
            //     .collect::<Result<Vec<Rc<SteelVal>>>>();

            Self::built_in_list_func()(lst)
        })
    }

    // pub fn take() -> SteelVal {
    //     SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
    //         if args.len() == 1 {
    //             match &args[0].as_ref() {
    //                 SteelVal::Pair(_, _) => {
    //                     let mut lst = Self::collect_into_vec(&args[0])?;
    //                     lst.reverse();
    //                     Self::built_in_list_func()(lst)
    //                 }
    //                 SteelVal::VectorV(v) => Ok(Rc::new(SteelVal::BoolV(v.is_empty()))),
    //                 _ => Ok(Rc::new(SteelVal::BoolV(false))),
    //             }
    //         } else {
    //             stop!(ArityMismatch => "reverse takes one argument");
    //         }
    //     })
    // }

    // pub fn flatten() -> SteelVal {
    //     SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
    //         let flattened_vec = args
    //             .into_iter()
    //             .map(|x| Self::collect_into_vec(&x))
    //             .collect::<Result<Vec<Vec<Rc<SteelVal>>>>>()?
    //             .into_iter()
    //             .flatten()
    //             .collect::<Vec<Rc<SteelVal>>>();

    //         Self::built_in_list_func()(flattened_vec)

    //         // unimplemented!()
    //     })
    // }

    pub fn list_to_vec() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::Pair(..) = &args[0].as_ref() {
                    let collected: Vec<SteelVal> = Self::collect_into_vec(&args[0])?
                        .into_iter()
                        .map(|x| (*x).clone())
                        .collect();
                    let im_vec: Vector<SteelVal> = collected.into();
                    Ok(Rc::new(SteelVal::VectorV(im_vec)))
                } else {
                    stop!(TypeMismatch => "list->vector expected list")
                }
            } else {
                stop!(ArityMismatch => "list->vector takes one argument");
            }
        })
    }

    pub fn collect_into_vec(mut p: &Rc<SteelVal>) -> Result<Vec<Rc<SteelVal>>> {
        let mut lst = Vec::new();
        loop {
            match p.as_ref() {
                SteelVal::Pair(cons, cdr) => {
                    lst.push(Rc::clone(cons));
                    match cdr.as_ref() {
                        Some(rest) => match rest.as_ref() {
                            Pair(_, _) => p = rest,
                            _ => {
                                lst.push(Rc::clone(rest));
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

    pub fn built_in_list_func() -> fn(Vec<Rc<SteelVal>>) -> Result<Rc<SteelVal>> {
        |args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().rev();
            let mut pairs = Vec::new();
            match (args.next(), args.next()) {
                (cdr, Some(car)) => {
                    pairs.push(Rc::new(SteelVal::Pair(car, cdr)));
                }
                (Some(cdr), None) => {
                    pairs.push(Rc::new(SteelVal::Pair(cdr, None)));
                }
                (_, _) => {
                    return Ok(Rc::new(SteelVal::VectorV(Vector::new())));
                }
            }

            for (i, val) in args.enumerate() {
                pairs.push(Rc::new(SteelVal::Pair(val, Some(Rc::clone(&pairs[i])))));
            }
            pairs
                .pop()
                .ok_or_else(|| SteelErr::ContractViolation("list-pair broke".to_string()))
        }
    }
}

#[cfg(test)]
mod list_operation_tests {

    use super::*;
    use crate::throw;
    use im_rc::vector;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Rc<SteelVal>> {
        let args = args.into_iter().map(|x| Rc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "list tests"))
            .unwrap()(args)
    }

    #[test]
    fn cons_test_normal_input() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = apply_function(ListOperations::cons(), args);
        let expected = Rc::new(SteelVal::Pair(
            Rc::new(SteelVal::NumV(1.0)),
            Some(Rc::new(SteelVal::NumV(2.0))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cons_single_input() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::cons(), args);
        let expected = SteelErr::ArityMismatch("cons takes only two arguments".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cons_no_input() {
        let args = vec![];
        let res = apply_function(ListOperations::cons(), args);
        let expected = SteelErr::ArityMismatch("cons takes only two arguments".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cons_with_empty_list() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::VectorV(Vector::new())];
        let res = apply_function(ListOperations::cons(), args);
        let expected = Rc::new(SteelVal::Pair(Rc::new(SteelVal::NumV(1.0)), None));
        assert_eq!(res.unwrap(), expected);
    }
    #[test]
    fn cons_with_non_empty_vector() {
        let args = vec![
            SteelVal::NumV(1.0),
            SteelVal::VectorV(vector![SteelVal::NumV(2.0)]),
        ];
        let res = apply_function(ListOperations::cons(), args);
        let expected = Rc::new(SteelVal::Pair(
            Rc::new(SteelVal::NumV(1.0)),
            Some(Rc::new(SteelVal::VectorV(vector![SteelVal::NumV(2.0)]))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_normal_input() {
        let args = vec![SteelVal::Pair(
            Rc::new(SteelVal::NumV(1.0)),
            Some(Rc::new(SteelVal::NumV(2.0))),
        )];
        let res = apply_function(ListOperations::car(), args);
        let expected = Rc::new(SteelVal::NumV(1.0));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn car_bad_input() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::car(), args);
        let expected = SteelErr::TypeMismatch("car takes a list, given: 1".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn car_too_many_args() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = apply_function(ListOperations::car(), args);
        let expected = SteelErr::ArityMismatch("car takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cdr_normal_input_2_elements() {
        let args = vec![SteelVal::Pair(
            Rc::new(SteelVal::NumV(1.0)),
            Some(Rc::new(SteelVal::NumV(2.0))),
        )];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = Rc::new(Pair(Rc::new(SteelVal::NumV(2.0)), None));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_normal_input_3_elements() {
        let args = vec![SteelVal::Pair(
            Rc::new(SteelVal::NumV(1.0)),
            Some(Rc::new(SteelVal::Pair(
                Rc::new(SteelVal::NumV(2.0)),
                Some(Rc::new(SteelVal::NumV(3.0))),
            ))),
        )];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = Rc::new(Pair(
            Rc::new(SteelVal::NumV(2.0)),
            Some(Rc::new(SteelVal::NumV(3.0))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn cdr_bad_input() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = SteelErr::TypeMismatch("cdr takes a list, given: 1".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }
    #[test]
    fn cdr_too_many_args() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = SteelErr::ArityMismatch("cdr takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn cdr_single_element_list() {
        let args = vec![SteelVal::Pair(Rc::new(SteelVal::NumV(1.0)), None)];
        let res = apply_function(ListOperations::cdr(), args);
        let expected = Rc::new(SteelVal::VectorV(Vector::new()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn range_tests_arity_too_few() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::range(), args);
        let expected = SteelErr::ArityMismatch("range takes two arguments".to_string());
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
        let expected = SteelErr::ArityMismatch("range takes two arguments".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn range_test_normal_input() {
        let args = vec![SteelVal::NumV(0.0), SteelVal::NumV(3.0)];
        let res = apply_function(ListOperations::range(), args);
        let expected = Rc::new(SteelVal::Pair(
            Rc::new(SteelVal::NumV(0.0)),
            Some(Rc::new(SteelVal::Pair(
                Rc::new(SteelVal::NumV(1.0)),
                Some(Rc::new(SteelVal::NumV(2.0))),
            ))),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_to_vec_arity_too_few() {
        let args = vec![];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = SteelErr::ArityMismatch("list->vector takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn list_to_vec_arity_too_many() {
        let args = vec![SteelVal::NumV(1.0), SteelVal::NumV(2.0)];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = SteelErr::ArityMismatch("list->vector takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn list_to_vec_bad_arg() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = SteelErr::TypeMismatch("list->vector expected list".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn list_to_vec_normal() {
        let args = vec![SteelVal::Pair(
            Rc::new(SteelVal::NumV(1.0)),
            Some(Rc::new(SteelVal::NumV(2.0))),
        )];
        let res = apply_function(ListOperations::list_to_vec(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0)
        ]));
        assert_eq!(res.unwrap(), expected);
    }
}
