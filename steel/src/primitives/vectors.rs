use crate::gc::Gc;
use crate::rerrs::{ErrorKind, SteelErr};
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use im_rc::Vector;

pub struct VectorOperations {}
impl VectorOperations {
    pub fn vec_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            Ok(SteelVal::VectorV(Gc::new(args.iter().cloned().collect())))
        })
    }

    pub fn vec_construct_iter<I: Iterator<Item = Result<SteelVal>>>(arg: I) -> Result<SteelVal> {
        let res: Result<Vector<SteelVal>> = arg.collect();
        Ok(SteelVal::VectorV(Gc::new(res?)))
    }

    pub fn vec_construct_iter_normal<I: Iterator<Item = SteelVal>>(arg: I) -> Result<SteelVal> {
        Ok(SteelVal::VectorV(Gc::new(
            arg.collect::<Vector<SteelVal>>(),
        )))
    }

    // TODO
    // mutation semantics are much more difficult than functional ones?
    // maybe for vectors use Rc<RefCell<SteelVal>> insides?
    // this would ensure that the insides can get mutated safely
    // COW would be cool though, because then I can ensure that if more than one
    // variable points to a location, then it changes only the reference that I want
    // to be changed
    //
    // Mutation functions have to have a different signature and run time
    // behavior, otherwise things don't work properly
    // pub fn vec_set_bang() -> SteelVal {
    //     SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
    //         if args.len() != 3 {
    //             stop!(ArityMismatch => "vector-set! takes 3 arguments");
    //         } else {
    //             // unimplemented!();
    //             // let vec_to_be_mut = Gc::clone(&args[0]);

    //             // let vec_to_be_mut = Gc::make_mut(&args[0]);

    //             let _idx = Gc::clone(&args[1]);
    //             let _new_value = Gc::clone(&args[2]);

    //             panic!("Internal Compiler Error - vector-set! not implemented")

    //             // unimplemented!()
    //         }
    //     })
    // }

    pub fn vec_append() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            let lsts: Vector<SteelVal> = unwrap_list_of_lists(args.iter().cloned().collect())?
                .into_iter()
                .flatten()
                .collect();
            Ok(SteelVal::VectorV(Gc::new(lsts)))
        })
    }

    pub fn vec_ref() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "vector-ref takes two arguments");
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(vec), Some(idx)) => {
                    if let (VectorV(vec), IntV(idx)) = (vec, idx) {
                        if idx < &0 {
                            stop!(TypeMismatch => "vector-ref expected a positive integer");
                        }

                        let idx: usize = *idx as usize;

                        if idx < vec.len() {
                            Ok(vec[idx].clone())
                        } else {
                            let e = format!("Index out of bounds - attempted to access index: {} with length: {}", idx, vec.len());
                            stop!(Generic => e);
                        }
                    } else {
                        stop!(TypeMismatch => format!("vector-ref expected a vector and a number, found: {} and {}", vec, idx))
                    }
                }
                _ => stop!(ArityMismatch => "vector-ref takes two arguments"),
            }
        })
    }

    pub fn vec_range() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "range takes two arguments");
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (IntV(lower), IntV(upper)) = (elem, lst) {
                        Ok(SteelVal::VectorV(Gc::new(
                            (*lower as usize..*upper as usize)
                                .into_iter()
                                .map(|x| SteelVal::IntV(x as isize))
                                .collect(),
                        )))
                    } else {
                        stop!(TypeMismatch => "range expected number")
                    }
                }
                _ => stop!(ArityMismatch => "range takes two arguments"),
            }
        })
    }

    pub fn vec_push() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "push takes two arguments");
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(l) = lst {
                        let mut l = l.unwrap();
                        l.push_back(elem.clone());
                        Ok(SteelVal::VectorV(Gc::new(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(elem.clone());
                        new.push_front(lst.clone());
                        Ok(SteelVal::VectorV(Gc::new(new)))
                    }
                }
                _ => stop!(ArityMismatch => "push takes two arguments"),
            }
        })
    }

    pub fn vec_cons() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 2 {
                stop!(ArityMismatch => "cons takes two arguments")
            }
            let mut args = args.iter();
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(l) = lst {
                        let mut l = l.unwrap();
                        l.push_front(elem.clone());
                        Ok(SteelVal::VectorV(Gc::new(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(lst.clone());
                        new.push_front(elem.clone());
                        Ok(SteelVal::VectorV(Gc::new(new)))
                    }
                }
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn vec_car() -> SteelVal {
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "car takes one argument");
            }
            if let Some(first) = args.iter().next() {
                match first {
                    SteelVal::VectorV(e) => {
                        let mut e = e.unwrap();
                        match e.pop_front() {
                            Some(e) => Ok(e),
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
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() != 1 {
                stop!(ArityMismatch => "cdr takes one argument");
            }
            if let Some(first) = args.iter().next() {
                match first {
                    SteelVal::VectorV(e) => {
                        let mut e = e.unwrap();
                        if !e.is_empty() {
                            e.pop_front();
                            Ok(SteelVal::VectorV(Gc::new(e)))
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
        SteelVal::FuncV(|args: &[SteelVal]| -> Result<SteelVal> {
            if args.len() == 1 {
                match &args[0] {
                    SteelVal::VectorV(v) => Ok(v.is_empty().into()),
                    _ => Ok(SteelVal::BoolV(false)),
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
        SteelVal::VectorV(lst) => Ok(lst.unwrap()),
        _ => stop!(TypeMismatch => "expected a list"),
    }
}

#[cfg(test)]
mod vector_prim_tests {
    use super::*;
    use crate::throw;
    use im_rc::vector;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<SteelVal> {
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(&args)
    }

    #[test]
    fn vec_construct_test() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)];
        let res = apply_function(VectorOperations::vec_construct(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3)
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_good_inputs() {
        let args = vec![
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ])),
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ])),
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ])),
        ];

        let res = apply_function(VectorOperations::vec_append(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3),
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3),
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3)
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_bad_inputs() {
        let args = vec![
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ])),
            SteelVal::StringV("foo".into()),
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ])),
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
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(1), SteelVal::IntV(1)];

        let res = apply_function(VectorOperations::vec_range(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_bad_input() {
        let args = vec![SteelVal::StringV("1".into()), SteelVal::NumV(1.0)];

        let res = apply_function(VectorOperations::vec_range(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_normal() {
        let args = vec![SteelVal::IntV(0), SteelVal::IntV(4)];

        let res = apply_function(VectorOperations::vec_range(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::IntV(0),
            SteelVal::IntV(1),
            SteelVal::IntV(2),
            SteelVal::IntV(3)
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_arity_too_few() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(VectorOperations::vec_push(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_push_test_good_input_pair() {
        let args = vec![
            SteelVal::StringV("baz".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into()),
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_test_good_input() {
        let args = vec![
            SteelVal::StringV("baz".into()),
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::StringV("foo".into()),
                SteelVal::StringV("bar".into())
            ])),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into())
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
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("foo".into()),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_cons_pair() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cons_elem_vector() {
        let args = vec![
            SteelVal::StringV("foo".into()),
            SteelVal::VectorV(Gc::new(vector![
                SteelVal::StringV("bar".into()),
                SteelVal::StringV("baz".into())
            ])),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
            SteelVal::StringV("baz".into())
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
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
        ];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_bad_input() {
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(VectorOperations::vec_car(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_car_normal_input() {
        let args = vec![SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]))];
        let res = apply_function(VectorOperations::vec_car(), args);
        let expected = SteelVal::StringV("foo".into());
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
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into()),
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
        let args = vec![SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]))];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        let expected = SteelVal::VectorV(Gc::new(vector![SteelVal::StringV("bar".into())]));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cdr_empty_list() {
        let args = vec![SteelVal::VectorV(Gc::new(Vector::new()))];
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
        let args = vec![SteelVal::StringV("foo".into())];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_non_empty_vec() {
        let args = vec![SteelVal::VectorV(Gc::new(vector![
            SteelVal::StringV("foo".into()),
            SteelVal::StringV("bar".into())
        ]))];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = SteelVal::BoolV(false);
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_empty_vec() {
        let args = vec![SteelVal::VectorV(Gc::new(Vector::new()))];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = SteelVal::BoolV(true);
        assert_eq!(res.unwrap(), expected);
    }
}
