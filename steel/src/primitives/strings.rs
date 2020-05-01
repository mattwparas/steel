// use crate::env::{FALSE, TRUE};
use crate::rerrs::SteelErr;
// use crate::rvals::SteelVal::*;
use crate::rvals::{Result, SteelVal};
use crate::stop;
// use im_rc::Vector;
use std::rc::Rc;

use crate::primitives::lists::ListOperations;

pub struct StringOperations {}
impl StringOperations {
    pub fn string_append() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 2 {
                if let (SteelVal::StringV(l), SteelVal::StringV(r)) =
                    (&args[0].as_ref(), &args[1].as_ref())
                {
                    let new_string = l.clone() + &r.clone();
                    return Ok(Rc::new(SteelVal::StringV(new_string)));
                } else {
                    stop!(TypeMismatch => "string-append expected two strings")
                }
            } else {
                stop!(ArityMismatch => "string-append takes two arguments")
            }
        })
    }

    pub fn string_to_list() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let chars: Vec<Rc<SteelVal>> =
                        s.chars().map(|x| Rc::new(SteelVal::CharV(x))).collect();
                    ListOperations::built_in_list_func()(chars)
                } else {
                    stop!(TypeMismatch => "string->list expected a string")
                }
            } else {
                stop!(ArityMismatch => "string->list takes one argument")
            }
        })
    }

    pub fn string_to_upper() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let upper = s.to_uppercase();
                    Ok(Rc::new(SteelVal::StringV(upper)))
                } else {
                    stop!(TypeMismatch => "string-upcase expected a string")
                }
            } else {
                stop!(ArityMismatch => "string-upcase takes one argument")
            }
        })
    }

    pub fn string_to_lower() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let lower = s.to_lowercase();
                    Ok(Rc::new(SteelVal::StringV(lower)))
                } else {
                    stop!(TypeMismatch => "string-lowercase expected a string")
                }
            } else {
                stop!(ArityMismatch => "string-lowercase takes one argument")
            }
        })
    }

    pub fn trim() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim();
                    Ok(Rc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim takes one argument")
            }
        })
    }

    pub fn trim_start() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim_start();
                    Ok(Rc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim-start expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim-start takes one argument")
            }
        })
    }

    pub fn trim_end() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let trimmed = s.trim_end();
                    Ok(Rc::new(SteelVal::StringV(trimmed.to_string())))
                } else {
                    stop!(TypeMismatch => "trim-end expected a string")
                }
            } else {
                stop!(ArityMismatch => "trim-end takes one argument")
            }
        })
    }

    pub fn split_whitespace() -> SteelVal {
        SteelVal::FuncV(|args: Vec<Rc<SteelVal>>| -> Result<Rc<SteelVal>> {
            if args.len() == 1 {
                if let SteelVal::StringV(s) = &args[0].as_ref() {
                    let split: Vec<Rc<SteelVal>> = s
                        .split_whitespace()
                        .map(|x| Rc::new(SteelVal::StringV(x.to_string())))
                        .collect();
                    ListOperations::built_in_list_func()(split)
                } else {
                    stop!(TypeMismatch => "split-whitespace expected a string")
                }
            } else {
                stop!(ArityMismatch => "split-whitespace takes one argument")
            }
        })
    }
}

#[cfg(test)]
mod string_operation_tests {
    use super::*;
    use crate::throw;

    fn apply_function(func: SteelVal, args: Vec<SteelVal>) -> Result<Rc<SteelVal>> {
        let args = args.into_iter().map(|x| Rc::new(x)).collect();
        func.func_or_else(throw!(BadSyntax => "string tests"))
            .unwrap()(args)
    }

    #[test]
    fn string_append_test_normal() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = Rc::new(SteelVal::StringV("foobar".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_append_test_arity_mismatch_too_few() {
        let args = vec![SteelVal::StringV("foo".to_string())];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = SteelErr::ArityMismatch("string-append takes two arguments".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_append_test_arity_mismatch_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
            SteelVal::StringV("baz".to_string()),
        ];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = SteelErr::ArityMismatch("string-append takes two arguments".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_append_test_takes_string() {
        let args = vec![SteelVal::CharV('a'), SteelVal::CharV('b')];
        let res = apply_function(StringOperations::string_append(), args);
        let expected = SteelErr::TypeMismatch("string-append expected two strings".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_upper_normal() {
        let args = vec![SteelVal::StringV("foobarbaz".to_string())];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = Rc::new(SteelVal::StringV("FOOBARBAZ".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_upper_spaces() {
        let args = vec![SteelVal::StringV("foo bar baz qux".to_string())];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = Rc::new(SteelVal::StringV("FOO BAR BAZ QUX".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_upper_arity_too_few() {
        let args = vec![];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = SteelErr::ArityMismatch("string-upcase takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_upper_arity_too_many() {
        let args = vec![
            SteelVal::StringV("foo".to_string()),
            SteelVal::StringV("bar".to_string()),
        ];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = SteelErr::ArityMismatch("string-upcase takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_upper_string_arg() {
        let args = vec![SteelVal::NumV(10.0)];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = SteelErr::TypeMismatch("string-upcase expected a string".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_lower_normal() {
        let args = vec![SteelVal::StringV("FOOBARBAZ".to_string())];
        let res = apply_function(StringOperations::string_to_lower(), args);
        let expected = Rc::new(SteelVal::StringV("foobarbaz".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_spaces() {
        let args = vec![SteelVal::StringV("FOO BAR BAZ QUX".to_string())];
        let res = apply_function(StringOperations::string_to_lower(), args);
        let expected = Rc::new(SteelVal::StringV("foo bar baz qux".to_string()));
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn string_to_lower_arity_too_few() {
        let args = vec![];
        let res = apply_function(StringOperations::string_to_lower(), args);
        let expected = SteelErr::ArityMismatch("string-lowercase takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_lower_arity_too_many() {
        let args = vec![
            SteelVal::StringV("FOO".to_string()),
            SteelVal::StringV("BAR".to_string()),
        ];
        let res = apply_function(StringOperations::string_to_lower(), args);
        let expected = SteelErr::ArityMismatch("string-lowercase takes one argument".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }

    #[test]
    fn string_to_lower_string_arg() {
        let args = vec![SteelVal::NumV(10.0)];
        let res = apply_function(StringOperations::string_to_upper(), args);
        let expected = SteelErr::TypeMismatch("string-upcase expected a string".to_string());
        assert_eq!(res.unwrap_err(), expected);
    }
}
