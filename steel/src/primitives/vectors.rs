use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use im_rc::Vector;
use std::rc::Rc;

// mod primitives;

pub struct VectorOperations {}
impl VectorOperations {
    pub fn vec_construct() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            Ok(Rc::new(SteelVal::VectorV(
                args.into_iter().map(|x| (*x).clone()).collect(),
            )))
        })
    }

    pub fn vec_append() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let lsts: Vector<SteelVal> =
                unwrap_list_of_lists(args.into_iter().map(|x| (*x).clone()).collect())?
                    .into_iter()
                    .flatten()
                    .collect();
            Ok(Rc::new(SteelVal::VectorV(lsts)))
        })
    }

    pub fn vec_range() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (NumV(lower), NumV(upper)) = (elem, lst) {
                        let mut res = Vector::new();
                        for i in lower as usize..upper as usize {
                            res.push_back(SteelVal::NumV(i as f64));
                        }
                        Ok(Rc::new(SteelVal::VectorV(res)))
                    } else {
                        stop!(TypeMismatch => "range expected number")
                    }
                }
                _ => stop!(ArityMismatch => "range takes two arguments"),
            }
        })
    }

    pub fn vec_push() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(ref l) = lst {
                        let mut l = l.clone();
                        l.push_back(elem);
                        Ok(Rc::new(SteelVal::VectorV(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(elem);
                        new.push_front(lst);
                        Ok(Rc::new(SteelVal::VectorV(new)))
                    }
                }
                _ => stop!(ArityMismatch => "push takes two arguments"),
            }
        })
    }

    pub fn vec_cons() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            let mut args = args.into_iter().map(|x| (*x).clone());
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(ref l) = lst {
                        let mut l = l.clone();
                        l.push_front(elem);
                        Ok(Rc::new(SteelVal::VectorV(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(lst);
                        new.push_front(elem);
                        Ok(Rc::new(SteelVal::VectorV(new)))
                    }
                }
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn vec_car() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.into_iter().map(|x| (*x).clone()).next() {
                match first {
                    SteelVal::VectorV(ref e) => {
                        let mut e = e.clone();
                        match e.pop_front() {
                            Some(e) => Ok(Rc::new(e)),
                            None => stop!(ContractViolation => "car expects a non empty list"),
                        }
                    }
                    e => {
                        stop!(TypeMismatch => "car takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "car takes one argument");
            }
        })
    }

    pub fn vec_cdr() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if let Some(first) = args.into_iter().map(|x| (*x).clone()).next() {
                match first {
                    SteelVal::VectorV(ref e) => {
                        let mut e = e.clone();
                        if !e.is_empty() {
                            e.pop_front();
                            Ok(Rc::new(SteelVal::VectorV(e)))
                        } else {
                            stop!(ContractViolation => "cdr expects a non empty list")
                        }
                    }
                    e => {
                        stop!(TypeMismatch => "cdr takes a list, given: {}", e);
                    }
                }
            } else {
                stop!(ArityMismatch => "cdr takes one argument");
            }
        })
    }

    pub fn list_vec_null() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                match &args[0].as_ref() {
                    SteelVal::VectorV(v) => {
                        if v.is_empty() {
                            Ok(TRUE.with(|f| Rc::clone(f)))
                        } else {
                            Ok(FALSE.with(|f| Rc::clone(f)))
                        }
                    }
                    _ => Ok(FALSE.with(|f| Rc::clone(f))),
                }
            } else {
                stop!(ArityMismatch => "null? takes one argument");
            }
        })
    }
}

fn unwrap_list_of_lists(args: Vec<SteelVal>) -> Result<Vec<Vector<SteelVal>>> {
    args.iter().map(unwrap_single_list).collect()
}

fn unwrap_single_list(exp: &SteelVal) -> Result<Vector<SteelVal>> {
    match exp {
        SteelVal::VectorV(lst) => Ok(lst.clone()),
        _ => stop!(TypeMismatch => "expected a list"),
    }
}
