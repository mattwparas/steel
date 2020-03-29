use crate::evaluator::Result;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal;
use crate::rvals::SteelVal::*;
use crate::stop;
use im_rc::Vector;
use std::rc::Rc;

// mod primitives;

pub struct ListOperations {}
impl ListOperations {
    pub fn cons() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
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
                _ => stop!(ArityMismatch => "cons-pair takes two arguments"),
            }
        })
    }

    pub fn car() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
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
                _ => stop!(TypeMismatch => "reverse expected a list"),
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
