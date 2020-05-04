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
            if args.len() != 2 {
                stop!(ArityMismatch => "range takes two arguments");
            }
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
            if args.len() != 2 {
                stop!(ArityMismatch => "push takes two arguments");
            }
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
            if args.len() != 2 {
                stop!(ArityMismatch => "cons takes two arguments")
            }
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
            if args.len() != 1 {
                stop!(ArityMismatch => "car takes one argument");
            }
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
            if args.len() != 1 {
                stop!(ArityMismatch => "cdr takes one argument");
            }
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

#[cfg(test)]
mod vector_prim_tests {
    use super::*;
    use crate::throw;
    use im_rc::vector;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Rc<SteelVal>> {
        let args = args.into_iter().map(|x| Rc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(args)
    }

    #[test]
    fn vec_construct_test() {
        let args = vec![
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0),
        ];
        let res = apply_function(VectorOperations::vec_construct(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0)
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_good_inputs() {
        let args = vec![
            SteelVal::VectorV(vector![
                SteelVal::NumV(1.0),
                SteelVal::NumV(2.0),
                SteelVal::NumV(3.0)
            ]),
            SteelVal::VectorV(vector![
                SteelVal::NumV(1.0),
                SteelVal::NumV(2.0),
                SteelVal::NumV(3.0)
            ]),
            SteelVal::VectorV(vector![
                SteelVal::NumV(1.0),
                SteelVal::NumV(2.0),
                SteelVal::NumV(3.0)
            ]),
        ];

        let res = apply_function(VectorOperations::vec_append(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0),
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0),
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0)
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_bad_inputs() {
        let args = vec![
            SteelVal::VectorV(vector![
                SteelVal::NumV(1.0),
                SteelVal::NumV(2.0),
                SteelVal::NumV(3.0)
            ]),
            SteelVal::StringV("foo".to_string()),
            SteelVal::VectorV(vector![
                SteelVal::NumV(1.0),
                SteelVal::NumV(2.0),
                SteelVal::NumV(3.0)
            ]),
        ];
        let res = apply_function(VectorOperations::vec_append(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_arity_too_few() {
        let args = vec![SteelVal::NumV(1.0)];

        let res = apply_function(VectorOperations::vec_range(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_arity_too_many() {
        let args = vec![
            SteelVal::NumV(1.0),
            SteelVal::NumV(1.0),
            SteelVal::NumV(1.0),
        ];

        let res = apply_function(VectorOperations::vec_range(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_bad_input() {
        let args = vec![SteelVal::StringV("1".to_string()), SteelVal::NumV(1.0)];

        let res = apply_function(VectorOperations::vec_range(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_normal() {
        let args = vec![SteelVal::NumV(0.0), SteelVal::NumV(4.0)];

        let res = apply_function(VectorOperations::vec_range(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::NumV(0.0),
            SteelVal::NumV(1.0),
            SteelVal::NumV(2.0),
            SteelVal::NumV(3.0)
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_arity_too_few() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(VectorOperations::vec_push(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("foo".to_string()),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_test_good_input_pair() {
        let args = vec![
            SteelVal::StringV("baz".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("baz".to_string()),
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_test_good_input() {
        let args = vec![
            SteelVal::StringV("baz".to_string()),
            SteelVal::VectorV(vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string())
            ]),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("baz".to_string())
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cons_test_arity_too_few() {
        let args = vec![];
        let res = apply_function(VectorOperations::vec_cons(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_test_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("foo".to_string()),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_pair() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string())
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cons_elem_vector() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::VectorV(vector![
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string())
            ]),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("baz".to_string())
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_car_arity_too_few() {
        let args = vec![];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_bad_input() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_normal_input() {
        let args = vec![SteelVal::VectorV(vector![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string())
        ])];
        let res = apply_function(VectorOperations::vec_car(), args);
        let expected = Rc::new(SteelVal::StringV("foo".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_arity_too_few() {
        let args = vec![];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_bad_input() {
        let args = vec![SteelVal::NumV(1.0)];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cdr_normal_input() {
        let args = vec![SteelVal::VectorV(vector![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string())
        ])];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        let expected = Rc::new(SteelVal::VectorV(vector![SteelVal::StringV(
            "bar".to_string()
        )]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_empty_list() {
        let args = vec![SteelVal::VectorV(Vector::new())];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        assert!(res.is_err());
    }

    #[test]
    fn list_vec_arity() {
        let args = vec![];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        assert!(res.is_err());
    }

    #[test]
    fn list_vec_anything_but_null() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = Rc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_non_empty_vec() {
        let args = vec![SteelVal::VectorV(vector![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string())
        ])];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = Rc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_empty_vec() {
        let args = vec![SteelVal::VectorV(Vector::new())];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = Rc::new(SteelVal::BoolV(true));
        assert_eq!(res.unwrap(), expected);
    }
}
