use crate::env::{FALSE, TRUE};
use crate::gc::Gc;
use crate::rerrs::SteelErr;
use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
use im_rc::Vector;

// mod primitives;

pub struct VectorOperations {}
impl VectorOperations {
    pub fn vec_construct() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            Ok(Gc::new(SteelVal::VectorV(
                args.into_iter().map(Gc::clone).collect(),
            )))
        })
    }

    pub fn vec_construct_iter<I: Iterator<Item = Result<Gc<SteelVal>>>>(
        arg: I,
    ) -> Result<Gc<SteelVal>> {
        let res: Result<Vector<Gc<SteelVal>>> = arg.collect();
        Ok(Gc::new(SteelVal::VectorV(res?)))
    }

    pub fn vec_construct_iter_normal<I: Iterator<Item = Gc<SteelVal>>>(
        arg: I,
    ) -> Result<Gc<SteelVal>> {
        Ok(Gc::new(SteelVal::VectorV(
            arg.collect::<Vector<Gc<SteelVal>>>(),
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
    pub fn vec_set_bang() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 3 {
                stop!(ArityMismatch => "vector-set! takes 3 arguments");
            } else {
                // unimplemented!();
                // let vec_to_be_mut = Gc::clone(&args[0]);

                // let vec_to_be_mut = Gc::make_mut(&args[0]);

                let _idx = Gc::clone(&args[1]);
                let _new_value = Gc::clone(&args[2]);

                panic!("Internal Compiler Error - vector-set! not implemented")

                // unimplemented!()
            }
        })
    }

    pub fn vec_append() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            let lsts: Vector<Gc<SteelVal>> =
                unwrap_list_of_lists(args.into_iter().map(Gc::clone).collect())?
                    .into_iter()
                    .flatten()
                    .collect();
            Ok(Gc::new(SteelVal::VectorV(lsts)))
        })
    }

    pub fn vec_range() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "range takes two arguments");
            }
            let mut args = args.into_iter().map(Gc::clone);
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let (IntV(lower), IntV(upper)) = (elem.as_ref(), lst.as_ref()) {
                        Ok(Gc::new(SteelVal::VectorV(
                            (*lower as usize..*upper as usize)
                                .into_iter()
                                .map(|x| Gc::new(SteelVal::IntV(x as isize)))
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
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "push takes two arguments");
            }
            let mut args = args.into_iter().map(Gc::clone);
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(l) = lst.as_ref() {
                        let mut l = l.clone();
                        l.push_back(elem);
                        Ok(Gc::new(SteelVal::VectorV(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(elem);
                        new.push_front(lst);
                        Ok(Gc::new(SteelVal::VectorV(new)))
                    }
                }
                _ => stop!(ArityMismatch => "push takes two arguments"),
            }
        })
    }

    pub fn vec_cons() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 2 {
                stop!(ArityMismatch => "cons takes two arguments")
            }
            let mut args = args.into_iter().map(Gc::clone);
            match (args.next(), args.next()) {
                (Some(elem), Some(lst)) => {
                    if let SteelVal::VectorV(l) = lst.as_ref() {
                        let mut l = l.clone();
                        l.push_front(elem);
                        Ok(Gc::new(SteelVal::VectorV(l)))
                    } else {
                        let mut new = Vector::new();
                        new.push_front(lst);
                        new.push_front(elem);
                        Ok(Gc::new(SteelVal::VectorV(new)))
                    }
                }
                _ => stop!(ArityMismatch => "cons takes two arguments"),
            }
        })
    }

    pub fn vec_car() -> SteelVal {
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "car takes one argument");
            }
            if let Some(first) = args.into_iter().map(Gc::clone).next() {
                match first.as_ref() {
                    SteelVal::VectorV(e) => {
                        let mut e = e.clone();
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
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() != 1 {
                stop!(ArityMismatch => "cdr takes one argument");
            }
            if let Some(first) = args.into_iter().map(Gc::clone).next() {
                match first.as_ref() {
                    SteelVal::VectorV(e) => {
                        let mut e = e.clone();
                        if !e.is_empty() {
                            e.pop_front();
                            Ok(Gc::new(SteelVal::VectorV(e)))
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
        SteelVal::FuncV(|args: &[Gc<SteelVal>]| -> Result<Gc<SteelVal>> {
            if args.len() == 1 {
                match &args[0].as_ref() {
                    SteelVal::VectorV(v) => {
                        if v.is_empty() {
                            Ok(TRUE.with(|f| Gc::clone(f)))
                        } else {
                            Ok(FALSE.with(|f| Gc::clone(f)))
                        }
                    }
                    _ => Ok(FALSE.with(|f| Gc::clone(f))),
                }
            } else {
                stop!(ArityMismatch => "null? takes one argument");
            }
        })
    }
}

fn unwrap_list_of_lists(args: Vec<Gc<SteelVal>>) -> Result<Vec<Vector<Gc<SteelVal>>>> {
    args.iter().map(unwrap_single_list).collect()
}

fn unwrap_single_list(exp: &Gc<SteelVal>) -> Result<Vector<Gc<SteelVal>>> {
    match exp.as_ref() {
        SteelVal::VectorV(lst) => Ok(lst.clone()),
        _ => stop!(TypeMismatch => "expected a list"),
    }
}

#[cfg(test)]
mod vector_prim_tests {
    use super::*;
    use crate::throw;
    use im_rc::vector;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Gc<SteelVal>> {
        let args: Vec<Gc<SteelVal>> = args.into_iter().map(|x| Gc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(&args)
    }

    #[test]
    fn vec_construct_test() {
        let args = vec![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)];
        let res = apply_function(VectorOperations::vec_construct(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)]
                .into_iter()
                .map(Gc::new)
                .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_good_inputs() {
        let args = vec![
            SteelVal::VectorV(
                vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
            SteelVal::VectorV(
                vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
            SteelVal::VectorV(
                vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
        ];

        let res = apply_function(VectorOperations::vec_append(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3),
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3),
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_append_test_bad_inputs() {
        let args = vec![
            SteelVal::VectorV(
                vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
            SteelVal::StringV("foo".to_string()),
            SteelVal::VectorV(
                vector![SteelVal::IntV(1), SteelVal::IntV(2), SteelVal::IntV(3)]
                    .into_iter()
                    .map(Gc::new)
                    .collect(),
            ),
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
        let args = vec![SteelVal::StringV("1".to_string()), SteelVal::NumV(1.0)];

        let res = apply_function(VectorOperations::vec_range(), args);
        assert!(res.is_err());
    }

    #[test]
    fn vec_range_test_normal() {
        let args = vec![SteelVal::IntV(0), SteelVal::IntV(4)];

        let res = apply_function(VectorOperations::vec_range(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![
                SteelVal::IntV(0),
                SteelVal::IntV(1),
                SteelVal::IntV(2),
                SteelVal::IntV(3)
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
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
        let expected = Gc::new(SteelVal::VectorV(
            vector![
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string()),
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_push_test_good_input() {
        let args = vec![
            SteelVal::StringV("baz".to_string()),
            SteelVal::VectorV(
                vector![
                    SteelVal::StringV("foo".to_string()),
                    SteelVal::StringV("bar".to_string())
                ]
                .into_iter()
                .map(Gc::new)
                .collect(),
            ),
        ];
        let res = apply_function(VectorOperations::vec_push(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string())
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
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
        let expected = Gc::new(SteelVal::VectorV(
            vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string())
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn vec_cons_elem_vector() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::VectorV(
                vector![
                    SteelVal::StringV("bar".to_string()),
                    SteelVal::StringV("baz".to_string())
                ]
                .into_iter()
                .map(Gc::new)
                .collect(),
            ),
        ];
        let res = apply_function(VectorOperations::vec_cons(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string()),
                SteelVal::StringV("baz".to_string())
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        ));
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
        let args = vec![SteelVal::VectorV(
            vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string())
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        )];
        let res = apply_function(VectorOperations::vec_car(), args);
        let expected = Gc::new(SteelVal::StringV("foo".to_string()));
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
        let args = vec![SteelVal::VectorV(
            vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string())
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        )];
        let res = apply_function(VectorOperations::vec_cdr(), args);
        let expected = Gc::new(SteelVal::VectorV(
            vector![SteelVal::StringV("bar".to_string())]
                .into_iter()
                .map(Gc::new)
                .collect(),
        ));
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
        let expected = Gc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_non_empty_vec() {
        let args = vec![SteelVal::VectorV(
            vector![
                SteelVal::StringV("foo".to_string()),
                SteelVal::StringV("bar".to_string())
            ]
            .into_iter()
            .map(Gc::new)
            .collect(),
        )];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = Gc::new(SteelVal::BoolV(false));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn list_vec_empty_vec() {
        let args = vec![SteelVal::VectorV(Vector::new())];
        let res = apply_function(VectorOperations::list_vec_null(), args);
        let expected = Gc::new(SteelVal::BoolV(true));
        assert_eq!(res.unwrap(), expected);
    }
}
